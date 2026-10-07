;; samizdat - a self-hosting agentic harness
;; Copyright (C) 2026 Dmitri Sotnikov
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.tui.core
  "The terminal UI: the loop, the pollers, and the handlers.

  This is the only namespace under `tui/` that knows ftxui exists. Everything
  else — the state fold, the widgets, the layout — is data and pure
  functions, which is why the suite covers them with no terminal. What is
  left here is wiring, and it is small on purpose.

  Strictly an HTTP client of the server, like the GUI: this process holds no
  engines, no database handle and no run state of its own. Everything it
  shows arrives through `samizdat.api.client` and everything it does goes
  back through a POST.

  THE LAYOUT IS RE-READ EVERY FRAME, and it comes from three places in
  order: a local file a person edits, the version the harness serves (the
  agent's, saved into the project), and the shipped template. That is the
  point of the file — a layout fixed at startup would make every claim about
  editing the UI while it runs a lie — and `samizdat.tui.layout` keeps it
  cheap: the file is re-read only when its mtime moves, and the served body
  is parsed once, when it arrives."
  (:require [clojure.string :as str]
            [ftxui.core :as ui]
            [ftxui.render :as ftxui-render]
            [samizdat.api.client :as client]
            [samizdat.api.sse :as sse]
            [samizdat.tui.commands :as cmd]
            [samizdat.tui.layout :as layout]
            [samizdat.tui.notify :as notify]
            [samizdat.tui.state :as st]
            [samizdat.tui.theme :as theme]
            [samizdat.tui.timeline :as tl]
            ;; Registers every :widget/* as a side effect of loading. Without
            ;; this require the layout resolves nothing and every panel draws
            ;; a "no widget" complaint.
            [samizdat.tui.widgets]))

(defn default-base-url
  "Where the server is. Defaults to the same port `jolt serve` does, so the
  TUI finds a default server with no configuration; `SAMIZDAT_URL` points it
  elsewhere, and `HARNESS_PORT` alone is enough when only the port moved."
  []
  (or (System/getenv "SAMIZDAT_URL")
      (str "http://127.0.0.1:" (or (System/getenv "HARNESS_PORT") "3985"))))

;; An ftxui atom, so a write from a poller thread redraws by itself. Plain
;; swap! from anywhere is safe: refresh! is thread-safe and every event ends
;; in a draw.
(defonce state (ui/atom (st/initial (default-base-url))))

;; The handlers below force a poll after acting, and the poller is defined
;; further down where it belongs beside the loop.
(declare poll-once!)

;; --- what the user can do ----------------------------------------------------

(defn- steer!
  "Send the compose box as an intervention. Not a chat message: samizdat runs
  autonomously and a person steers at a turn boundary, so this is the same
  seam the supervisor uses. On a run stopped with Esc it is what to do
  instead: the run resumes with it as its first directive."
  [text]
  (let [{:keys [base run-id branch-id]} @state]
    (when-let [payload (st/steer-payload text)]
      (cond
        (not run-id)
        (swap! state st/note-error "no run selected")

        (st/interrupted? @state)
        (do (swap! state #(-> % st/clear-input (st/note-notice "resuming…")))
            (future
              (let [r (client/resume! base run-id {:message payload})]
                (swap! state st/note-error (when-not (:ok r) (:error r)))
                (poll-once!))))

        :else
        (let [r (client/intervene! base run-id {:branch-id branch-id
                                                :kind "message"
                                                :payload payload})]
          (swap! state #(-> % (st/note-error (when-not (:ok r) (:error r)))
                            (cond-> (:ok r) st/clear-input))))))))

(defn- start!
  "Start a run on the compose box, as its problem statement.

  Only `:problem` goes up: everything else POST /v1/runs takes — the turn
  budget, the beam width, the model — is left to the server's own config,
  which is what an omitted key means. The GUI has a form for those because
  it has room for one; this is one line, and the statement is the part that
  cannot be defaulted.

  OFF THE UI THREAD, and that is not optional: the endpoint does not answer
  when the run row is written — beam/run! opens every branch first — so a
  start legitimately takes tens of seconds, which is why the client allows
  it longer than the server's own deadline (api.client/start-timeout-ms). A
  UI that stopped redrawing for that long is the frozen-but-alive
  failure this project keeps finding."
  [text]
  (let [{:keys [base]} @state]
    (if-let [problem (st/steer-payload text)]
      (do (swap! state st/note-notice "starting…")
          (future
            (swap! state st/apply-start
                   (client/start-run! base (merge {:problem problem}
                                                  (:next-llm @state))))
            (poll-once!)))
      (swap! state st/note-error "type a problem statement first"))))

;; `if-not run-id` rather than `when run-id`: both of these used to do
;; NOTHING AND SAY NOTHING with no run selected, which is indistinguishable
;; from a button that is not wired to anything — and that is exactly how it
;; was reported.
(defn- abort! []
  (let [{:keys [base run-id]} @state]
    (if-not run-id
      (swap! state st/note-error "no run selected")
      (let [r (client/abort! base run-id)]
        (swap! state st/note-error (when-not (:ok r) (:error r)))))))

(defn- resume! []
  (let [{:keys [base run-id]} @state]
    (if-not run-id
      (swap! state st/note-error "no run selected")
      (let [r (client/resume! base run-id)]
        (swap! state st/note-error (when-not (:ok r) (:error r)))))))

(defn- decide!
  "Answer a permission question. The branch is parked on this, so a failure
  has to be visible rather than swallowed — the run would otherwise sit
  there until the deadline with nobody knowing the click did nothing."
  [id decision]
  ;; A decision map — {:decision :allow :always true}, {:decision :deny
  ;; :note "…"} — or a bare keyword from an older caller.
  (let [r (client/decide! (:base @state) id (if (map? decision) decision {:decision decision}))]
    (swap! state st/cancel-reply)
    (swap! state st/note-error (when-not (:ok r) (:error r)))
    ;; Poll straight away rather than waiting out the interval: the whole
    ;; point of this dialog is that somebody is watching it.
    (future (poll-once!))))

(defn- submit-answers!
  "Send a questionnaire's answers — all of them, or (Esc half way) the ones
  given, the rest coming back to the model as unanswered."
  [id answers]
  (let [r (client/decide! (:base @state) id {:decision :answer :answers answers})]
    (swap! state st/note-error (when-not (:ok r) (:error r)))
    (future (poll-once!))))

(defn- answer!
  "Record an answer to a questionnaire, and submit once the last one is in.

  `swap!` rather than reading and `reset!`ting: the poller writes into this
  same atom every interval, and a reset would discard whichever poll landed
  between the read and the write."
  [id _i answers]
  (let [done? (volatile! false)]
    (swap! state (fn [s] (let [[next d] (st/answer-question s answers)]
                           (vreset! done? d)
                           next)))
    ;; The fold above is synchronous so the next key lands on the next
    ;; question; only the HTTP leaves the UI thread.
    (when @done?
      (future (submit-answers! id answers)))))

;; --- slash commands ------------------------------------------------------------

(defn- clip-line [s n]
  (let [t (str/replace (str s) #"\s+" " ")]
    (if (> (count t) n) (str (subs t 0 (dec n)) "…") t)))

(defn- say! [& lines] (swap! state st/note-local lines))

(defn- running? [s]
  (= "running" (str (get-in s [:detail :run :status]))))

(defn- live-switch!
  "/model and /effort, `value` or `role value`: live for the run on screen
  while it runs — for that role, or every role — otherwise kept for the next
  run started from here (which takes no role)."
  [kind llm-key arg]
  (let [{:keys [base run-id] :as s} @state
        words (str/split (str/trim arg) #"\s+")]
    (cond
      (and run-id (running? s))
      (let [r (client/intervene! base run-id {:kind kind :payload arg})]
        (if (:ok r)
          (say! (str kind " → " (if (= 2 (count words))
                                  (str (second words) " for the " (first words))
                                  (str (first words) " for every role"))
                     " on run " (clip-line run-id 9) ", from its next request"))
          (say! (str kind " switch refused: " (:error r)))))

      (= 2 (count words))
      (say! (str "a " kind " for one role switches a running run; no run is running here"))

      :else
      (do (swap! state assoc-in [:next-llm llm-key] (first words))
          (say! (str kind " → " (first words) " for the next run"))))))

(defn- command!
  "Do what a slash line says. Its output goes into the conversation."
  [text]
  (let [cs (:commands (layout/current))
        {:keys [error arg kind target] :as c} (cmd/parse text cs)
        {:keys [base run-id branch-id]} @state]
    (swap! state #(-> % (st/remember-input text) st/clear-input))
    (if error
      (say! error)
      (case (:do c)
        :help (apply say! (concat ["commands:"] (cmd/help-lines cs)
                                  (when-let [ks (seq (:keys (layout/current)))]
                                    (cons "keys:" (map #(str "  " %) ks)))))
        :quit (ui/exit!)
        :clear (swap! state st/clear-local)
        :follow (swap! state st/follow :conversation)
        :abort (abort!)
        :resume (resume!)
        :start (if arg (start! arg) (say! "/run needs the problem to work on"))
        :branch (if arg (swap! state st/select-branch arg) (say! "/branch needs a branch id"))
        :runs (if arg
                (if-let [r (first (filter #(str/starts-with? (str (:id %)) arg) (:runs @state)))]
                  (swap! state st/select-run (:id r))
                  (say! (str "no run starting " arg)))
                (apply say! "runs:" (for [r (take 20 (:runs @state))]
                                      (str "  " (clip-line (:id r) 9) "  " (:status r)
                                           "  " (clip-line (:problem r) 60)))))
        :model (if arg
                 (live-switch! "model" :model arg)
                 (future
                   (let [r (client/models base)
                         {:keys [current models]} (:body r)
                         next-m (get-in @state [:next-llm :model])]
                     (if (:ok r)
                       (apply say! (str "models (current " current
                                        (when next-m (str ", next run " next-m)) "):")
                              (for [m models] (str "  " (if (= m current) "▸ " "  ") m)))
                       (say! (str "could not list the models: " (:error r)))))))
        :effort (if arg
                  (live-switch! "effort" :reasoning_effort arg)
                  (say! "/effort needs a level: low, medium, high or max"))
        :mode (if arg
                (future
                  (let [r (client/set-approval-mode! base arg)]
                    (if (:ok r)
                      (do (say! (str "approval mode → " (get-in r [:body :mode])
                                     " for this session"))
                          (swap! state st/apply-project (client/project base)))
                      (say! (str "mode not set: " (:error r))))))
                (say! (str "approval mode: " (or (get-in @state [:project :approval_mode]) "unknown")
                           " — /mode attended (ask while a TUI watches), refuse, block, or yolo (every ask allowed)")))
        :intervene
        (cond
          (not run-id) (say! "no run on screen to direct")
          (and (= :arg target) (not arg)) (say! (str (:name c) " needs a branch id"))
          :else
          (let [r (client/intervene! base run-id
                                     {:kind kind
                                      :payload (if (= :arg target) "" (or arg ""))
                                      :branch-id (case target
                                                   :branch branch-id
                                                   :arg arg
                                                   nil)})]
            (say! (if (:ok r)
                    (str (:name c) " sent — " (or (get-in r [:body :note]) "queued"))
                    (str (:name c) " refused: " (:error r))))))
        (say! (str (:name c) " is not something this TUI can do"))))))

(defn- send!
  "Enter in the compose box. While a dialog holds it, the text is the deny
  note or the custom answer; a slash line is a command; anything else starts
  a run (none on screen) or steers the one that is."
  [action text]
  (let [{:keys [reply question-answers approvals question-cursor] :as s} @state
        kind (st/reply-kind s)
        id (or (:id reply) (:id (first approvals)))]
    (cond
      (= :deny-note kind)
      (do (decide! id {:decision :deny :note (str/trim (str text))})
          (swap! state st/clear-input))

      ;; A slash line is still a command while a question waits: /abort must
      ;; not become somebody's answer.
      (and (= :custom-answer kind) (not (cmd/parse text {})))
      (if (str/blank? (str text))
        (say! "type the answer, or Esc to go back")
        (do (swap! state #(-> % st/cancel-reply st/clear-input))
            (answer! id question-cursor (conj (vec question-answers) (str/trim (str text))))))

      (cmd/parse text {}) (command! text)

      ;; Sending is looking at the bottom again, as in dirge.
      :else (do (swap! state #(-> % (st/remember-input text) (st/follow :conversation)))
                (action text)))))

(defn- sync-mention!
  "Open, follow or close the @-picker for what the box holds now, fetching
  the files for a new query off the UI thread."
  []
  (let [q (st/mention-query (:input @state))]
    (when (not= q (get-in @state [:mention :query]))
      (swap! state st/mention-open q)
      (when q
        (let [{:keys [base]} @state
              limit (or (get-in (layout/current) [:file-picker :max]) 8)]
          (future (swap! state st/apply-mention-files q (client/project-files base q limit))))))))

(defn- held
  "What the compose box holds NOW, over what a widget handed up. A widget's
  handler closes over the state of the frame it was drawn in, and FTXUI reads
  the terminal 128 bytes at a time with a frame between reads, so a line
  typed or pasted in one go reached Enter as its first 128 characters — every
  run started from here had its problem cut there. The state is current: each
  key's on-change has folded into it by the time Enter is handled."
  [text]
  (or (not-empty (:input @state)) (str text)))

(def ^:private handlers
  "What the widgets can do, handed to them in the state. Widgets stay pure
  functions and a test drives a click by passing recording functions here."
  {:decide        decide!
   :answer        answer!
   :toggle        #(swap! state st/toggle-fold %)
   :select-run    #(swap! state st/select-run %)
   :select-branch #(swap! state st/select-branch %)
   :input         #(do (swap! state st/set-input %) (sync-mention!))
   :submit        #(send! steer! (st/expand-pastes @state (held %)))
   :start         #(send! start! (st/expand-pastes @state (held %)))
   :abort         abort!
   :resume        resume!
   :reply         (fn [kind id] (swap! state st/start-reply kind id))
   :toggle-option #(swap! state st/toggle-option %)
   :option        #(swap! state st/move-option %)
   :scroll        (fn [pane view] (swap! state st/scrolled pane view))
   :history-back    #(swap! state st/history-back)
   :history-forward #(swap! state st/history-forward)})

;; --- the feeds ----------------------------------------------------------------
;;
;; Two. The run being watched is PUSHED: its event stream (GET
;; /v1/runs/:id/events) says what changed, and only that is fetched. What is
;; not a run — the layout, the project, the run list — is polled, slowly.
;; When the stream is down the run is polled too, as it always was, so a
;; server without the stream or a dropped connection costs freshness, not
;; function.

(defn- poll-harness!
  "What the harness is, rather than what a run is doing."
  []
  (let [base (:base @state)]
    ;; The layout the harness holds, so a version the agent saved for itself
    ;; reaches the screen. Only on success: an outage must not blank the
    ;; arrangement that is already drawn.
    (let [r (client/layout base)]
      (when (:ok r) (layout/serve! (get-in r [:body :layout]))))
    ;; Cheap: the server caches the git side (gates.edn :git-snapshot-ttl-ms),
    ;; so this is not three shell-outs per poll.
    (swap! state st/apply-project (client/project base))
    (swap! state st/apply-runs (client/list-runs base))))

(defn- refresh-run!
  "Fetch what `wants` names for the selected run: :detail, :branch,
  :approvals, :steps, :runs.

  Questions first: a branch parked on a question is a branch doing nothing,
  so the dialog is the panel that must not wait behind a slow response."
  [wants]
  (let [base (:base @state)]
    (when (:runs wants) (swap! state st/apply-runs (client/list-runs base)))
    (when-let [rid (:run-id @state)]
      (when (:steps wants)
        (swap! state st/apply-steps (client/steps-since base rid (:steps-cursor @state))))
      (when (:approvals wants) (swap! state st/apply-approvals (client/approvals base rid)))
      (when (:detail wants) (swap! state st/apply-detail (client/run-detail base rid)))
      (when (:branch wants)
        (when-let [bid (:branch-id @state)]
          ;; From the newest turn held: the rest has not changed, and on a
          ;; long branch it is megabytes (karamazov-rf7d).
          (swap! state st/apply-branch
                 (client/branch-detail base rid bid
                                       (keys (get-in (layout/current) [:conversation :notes]))
                                       (st/turns-cursor @state)))
          ;; The prose, a turn at a time, for the newest turns only — the
          ;; branch listing drops it because it is the bulk.
          (doseq [n (st/prose-wanted @state (:prose-turns (layout/current) 12))]
            (swap! state st/apply-turn-text n (client/turn-detail base rid bid n))))))))

(def ^:private everything #{:steps :approvals :detail :branch})

(defn- poll-once!
  "One pass over every feed, folded into the state. F5, and after an action."
  []
  (poll-harness!)
  (refresh-run! everything))

;; What pushed events have asked to be fetched and nobody has fetched yet.
;; A burst of forty events in one turn is one fetch of each thing, not forty.
(defonce ^:private wanted (atom #{}))

(defn- on-pushed
  "One pushed event: fold it, and queue what it changed."
  [e]
  (let [w (volatile! #{})]
    (swap! state (fn [s] (let [[s' wants] (st/apply-event s e)]
                           (vreset! w wants)
                           s')))
    (when (seq @w) (swap! wanted into @w))
    ;; A moment a person who looked away wants to hear about.
    (let [settings (:notifications (layout/current))]
      (when-let [n (notify/for-event @state e settings)]
        (notify/notify! settings n ui/write-raw!)))))

(defonce ^:private workers (atom nil))

(defn- daemon! [name f]
  (doto (Thread. f name) (.setDaemon true) (.start)))

(defn- guarded
  "Run `f`, and note rather than throw: a worker that died would leave a UI
  that looks live and is frozen, which is the failure this project keeps
  finding."
  [f]
  (try (f) (catch Throwable e (swap! state st/note-error (ex-message e)))))

(defn- follow-loop
  "Follow the selected run's event stream; move to the new run when the
  selection changes."
  [running]
  (while @running
    (let [{:keys [base run-id]} @state]
      (if-not run-id
        (Thread/sleep 250)
        (do
          ;; A run newly followed is fetched whole once; from then on its
          ;; events say what changed.
          (swap! wanted into everything)
          (guarded
           #(sse/follow! (str base "/v1/runs/" run-id "/events?since=now")
                         {:on-event on-pushed
                          :on-status (fn [status] (swap! state st/stream-status status))
                          :on-error (fn [_] (swap! state st/stream-status nil))
                          :stop? (fn [] (or (not @running)
                                            (not= run-id (:run-id @state))))}))
          (swap! state st/stream-status nil))))))

(defn- refresh-loop
  "Fetch whatever pushed events asked for, a batch at a time."
  [running]
  (while @running
    (let [w (first (reset-vals! wanted #{}))]
      (if (seq w)
        (guarded #(refresh-run! w))
        (Thread/sleep 100)))))

(defn- poll-loop
  "Poll what is not pushed — and the run as well, while its stream is down.
  Backs off when the server is unreachable, so a TUI left open against a
  stopped server is not hammering it."
  [running]
  (loop [last-harness 0]
    (when @running
      (let [now (System/currentTimeMillis)
            {:keys [live? connected?]} @state
            harness? (or (not live?) (>= (- now last-harness) client/idle-interval-ms))]
        (guarded #(do (when harness? (poll-harness!))
                      (when-not live? (refresh-run! everything))))
        (Thread/sleep (if connected? client/base-interval-ms client/max-backoff-ms))
        (recur (if harness? now last-harness))))))

(defn start-polling!
  "Start the feeds on background threads until `stop-polling!`."
  []
  (when-not @workers
    (let [running (atom true)]
      (reset! workers {:running running})
      (daemon! "samizdat-tui-poller" #(poll-loop running))
      (daemon! "samizdat-tui-follower" #(follow-loop running))
      (daemon! "samizdat-tui-refresher" #(refresh-loop running))))
  nil)

(defn stop-polling! []
  (when-let [{:keys [running]} @workers]
    (reset! running false)
    (reset! workers nil))
  nil)

;; --- the frame ---------------------------------------------------------------

(defonce ^:private set-up-app (atom nil))

(def ^:private bracketed-paste-on "\u001b[?2004h")
(def ^:private bracketed-paste-off "\u001b[?2004l")

(defn- set-up-terminal!
  "Bracketed paste on, once per app from its first frame: the terminal marks
  a paste, so its newlines are line breaks rather than Enter (see
  `on-event`). FTXUI does not ask for it."
  []
  (when-let [app @ftxui-render/active-app]
    (when-not (identical? app @set-up-app)
      (ui/write-raw! bracketed-paste-on)
      (reset! set-up-app app))))

(defn root
  "One frame: read the layout, expand its widgets against the state, draw.

  Re-read every frame so a runtime edit to tui.edn takes effect on the next
  one. `layout/current` never throws and never returns something undrawable —
  a broken edit falls back to the shipped layout and reports itself in the
  status line rather than taking the screen."
  []
  (set-up-terminal!)
  ;; A paste closed since the last frame joins the box now, when every
  ;; on-change the box reported has landed (state/end-paste).
  (when (:paste-done @state) (swap! state st/apply-paste))
  (let [{:keys [layout error] th :theme :as spec} (layout/current)
        s (assoc @state :on handlers :layout-error error
                 ;; The settings a widget reads beyond the layout itself —
                 ;; the avatar's faces, say.
                 :settings (dissoc spec :layout :sources :error))]
    ;; Themed AFTER expansion, so the classes the widgets name are resolved
    ;; along with the ones the layout names — one stylesheet for both.
    (theme/apply-theme th (layout/expand layout s))))

(defn- conversation-entries
  "The conversation's entries as the widget draws them, for the keys that
  move through it."
  [s]
  (tl/entries s (:conversation (layout/current))))

(defn- on-event
  "Global keys. Everything else — clicks, arrows, text — belongs to whichever
  widget has the focus, so this consumes as little as it can.

  The dialog keys are the exception, and only while a dialog is up and
  nothing is typed: y/a/n/d over a permission (the buttons name them, and a
  label that names a key has to mean it), and the arrows, j/k, Space, Enter
  and digits over a questionnaire, which is answered without the focus ever
  leaving the compose box. `state/dialog-action` decides — over an
  open-ended question a `y` is a letter of the answer, and goes through."
  [{:keys [type key char control input] :as event}]
  (let [page! (fn [dir] (swap! state st/page :conversation dir) true)
        s @state]
    (cond
      ;; A paste, between the terminal's brackets: every key of it goes into
      ;; the paste and none to the box, a Return as a line break.
      (and (= :unknown type) (= "\u001b[200~" input))
      (do (swap! state st/begin-paste) true)
      (and (= :unknown type) (= "\u001b[201~" input))
      (do (swap! state st/end-paste) true)
      (st/pasting? s)
      (do (cond
            (= :character type) (swap! state st/paste-add char)
            (= :return key) (swap! state st/paste-add "\n")
            (= :tab key) (swap! state st/paste-add "\t"))
          true)

      ;; Ctrl+F: search what was sent. While it is open the keys are its.
      (st/searching? s)
      (do (cond
            (and (= :character type) (not control)) (swap! state st/search-type char)
            (= :backspace key) (swap! state st/search-backspace)
            (= :ctrl-f key) (swap! state st/search-next)
            (= :return key) (swap! state st/search-accept)
            (#{:escape :ctrl-c} key) (swap! state st/search-cancel))
          (not= :mouse type))
      (and (= :key type) (= :ctrl-f key) (nil? (:reply s)))
      (do (swap! state st/search-start) true)

      ;; The @-picker, while it has files to offer.
      (and (st/mention-active? s) (= :key type)
           (#{:tab :arrow-down :tab-reverse :arrow-up :return :escape} key))
      (do (swap! state (case key
                         (:tab :arrow-down) #(st/mention-move % 1)
                         (:tab-reverse :arrow-up) #(st/mention-move % -1)
                         :return st/mention-accept
                         :escape st/mention-cancel))
          true)

      ;; dirge's order: clear the draft, drop the reply, then quit — and with
      ;; a run going, only on a second press (state/ctrl-c-action).
      (and (= :key type) (= :ctrl-c key))
      (let [[act s'] (st/ctrl-c-action @state (System/currentTimeMillis))]
        (case act
          :clear (swap! state st/clear-input)
          :cancel-reply (swap! state #(-> % st/cancel-reply st/clear-input))
          :arm (swap! state (fn [s] (-> s (assoc :ctrl-c-at (:ctrl-c-at s'))
                                        (st/note-notice "a run is going — Ctrl+C again to quit"))))
          :quit (ui/exit!))
        true)
      ;; ftxui delivers Ctrl+Q as a :key, never as a :character with a
      ;; control flag — the old test for that could not match, and the quit
      ;; key the docs named did nothing.
      (and (= :key type) (= :ctrl-q key)) (do (ui/exit!) true)
      (and (= :key type) (= :f5 key)) (do (future (poll-once!)) true)

      ;; The dialog on screen, if any (state/dialog-action): y a n d over a
      ;; permission; arrows, j/k, Space, Enter and digits over a question;
      ;; Esc over either. Before the scroll keys, so Down over a question
      ;; moves its cursor rather than the conversation.
      (and (or (#{:escape :return :arrow-up :arrow-down} key)
               (and (= :character type) (not control)))
           (st/dialog-action @state {:char char :key key}))
      (let [[act a b] (st/dialog-action @state {:char char :key key})]
        (case act
          :decide (future (decide! a b))
          :reply (swap! state st/start-reply a b)
          :cancel-reply (swap! state #(-> % st/cancel-reply st/clear-input))
          :option (swap! state st/move-option a)
          :toggle (swap! state #(-> % (st/move-option a) (st/toggle-option a)))
          :answer (answer! a nil b)
          :answer-partial (future (submit-answers! a b))
          :notice (swap! state st/note-notice a))
        true)

      ;; Esc with nothing else to close: stop what the agent is doing (dirge's
      ;; interrupt). The run is left resumable — Enter on what to do instead
      ;; carries it on (steer!), and the resume button picks it up as it was.
      (and (= :key type) (= :escape key)
           (= "running" (str (get-in s [:detail :run :status]))))
      (let [{:keys [base run-id]} s]
        (swap! state st/note-notice "stopping — type what to do instead and press Enter to carry on")
        (future (let [r (client/interrupt! base run-id)]
                  (swap! state st/note-error (when-not (:ok r) (:error r)))
                  (poll-once!)))
        true)

      ;; The conversation: page through it, and back to following the bottom.
      ;; The wheel is not handled here: each pane scrolls under the mouse.
      (and (= :key type) (= :page-up key)) (page! -1)
      (and (= :key type) (= :page-down key)) (page! 1)
      ;; Down or End while scrolled up jumps back to the bottom (dirge);
      ;; while following, they are the input's.
      (and (= :key type) (#{:arrow-down :end} key) (st/scroll-top @state :conversation))
      (do (swap! state st/follow :conversation) true)
      ;; Tab completes a slash command's name.
      (and (= :key type) (= :tab key) (str/starts-with? (str (:input @state)) "/"))
      (do (swap! state #(st/set-input % (cmd/complete (:input %) (:commands (layout/current)))))
          true)
      ;; Ctrl+J, Alt+Enter or Shift+Enter breaks the line: Enter sends.
      (st/newline-key? event) (do (swap! state st/newline) true)
      ;; Ctrl+P / Ctrl+N walk back and forth through what was sent.
      (and (= :key type) (= :ctrl-p key)) (do (swap! state st/history-back) true)
      (and (= :key type) (= :ctrl-n key)) (do (swap! state st/history-forward) true)
      ;; Ctrl+O opens the newest folded result or thinking, and shuts it again.
      (and (= :key type) (= :ctrl-o key))
      (do (swap! state #(st/toggle-latest-fold
                         % (tl/fold-ids (conversation-entries %) (:conversation (layout/current)))))
          true)


      :else false)))

(defn- on-select
  "A mouse drag selected text — in the compose box, the conversation, any
  pane: put it on the clipboard. Through the terminal (OSC 52), since the
  terminal is the one thing between here and the clipboard in every setup,
  ssh included; one that refuses the sequence leaves Shift/Option-drag, the
  terminal's own selection, as it always was."
  [text]
  (ui/copy! text)
  (swap! state st/note-notice (str "copied " (count text) " characters")))

(defn run-ui!
  "Draw the TUI against the server at `base` until the user quits. Blocks."
  [base]
  (swap! state assoc :base base)
  ;; A first run leaves ~/.config/samizdat/tui.edn behind to edit.
  (try (layout/seed-global!) (catch Throwable _ nil))
  (start-polling!)
  (try
    ;; Mouse on: the folds in the conversation are ftxui collapsibles and
    ;; clicking one is how they open.
    ;; :force-ctrl-c false — Ctrl+C is ours. FTXUI otherwise quits on it even
    ;; when on-event consumed it, so `state/ctrl-c-action` (clear the draft,
    ;; quit only on a second press while a run is going) never got a say, and
    ;; in the one-process binary one Ctrl+C meant to clear a line stopped the
    ;; server and the run with it.
    (ui/run root :mode :fullscreen :mouse true :on-event on-event :on-select on-select
            :force-ctrl-c false)
    (finally
      (stop-polling!)
      ;; The terminal is FTXUI's again and then the shell's: hand bracketed
      ;; paste back off, or the next program's pastes arrive bracketed.
      (print bracketed-paste-off)
      (flush))))

(defn -main [& args]
  (let [base (or (first (remove str/blank? args)) (default-base-url))]
    (println "samizdat tui →" base)
    (run-ui! base)
    ;; Out, not merely returned: a request still in flight on a future kept
    ;; the process alive after the loop ended, the last frame on screen over
    ;; a terminal handed back to line mode — a TUI that looked open and
    ;; echoed every key instead of reading it.
    (shutdown-agents)
    (System/exit 0)))
