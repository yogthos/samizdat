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

(ns samizdat.tui.state
  "The view state: what the pollers fold into and what the widgets read.

  Every function here is pure, which is what leaves the run loop as wiring
  and puts the TUI's actual policy — cursors, what a disconnect does, what
  changing run resets — under test with no terminal and no server.

  The state is one flat map on purpose. A widget takes what it needs from it
  and the layout never has to name data, only widgets; that is what lets a
  user put a panel anywhere without anything being rewired."
  (:refer-clojure :exclude [newline])
  (:require [clojure.string :as str]
            [samizdat.api.sse :as sse]))

(def handler-keys
  "Every action a widget may ask the loop to take.

  Named here, in the toolkit-free half, so the suite can hold both ends of
  the seam: that `samizdat.tui.core` offers exactly these, and that some
  widget calls each one. Three of them — :abort, :resume and :select-branch
  — were offered and documented for a whole release with nothing on screen
  that called them, which no test could see because each half was correct on
  its own."
  #{:decide :answer :toggle :select-run :select-branch :input :submit :start
    :abort :resume :reply :toggle-option :option :scroll :history-back :history-forward})

(def max-trace
  "How many steps the UI holds. The server's ring is bounded and so is this:
  a TUI left running for a day would otherwise keep every step of every run
  it watched."
  500)

(defn initial [base]
  {:base base
   :connected? false
   :error nil
   ;; Whether :error is an outage, which the next poll that gets through
   ;; clears, rather than an action's refusal, which it must not.
   :error-outage? false
   ;; Beside :error rather than sharing it: the status line paints an error
   ;; red, and "starting…" is not a failure.
   :notice nil
   :layout-error nil
   ;; What the harness says it is working on: project, branch, dirty counts,
   ;; model. Nil until the first poll answers, so the footer and the GIT panel
   ;; both have to draw without it.
   :project nil
   :runs []
   :run-id nil
   :detail nil
   :branch-id nil
   :branch nil
   :trace []
   :steps-cursor 0
   :trace-dropped 0
   ;; The id of the last journal event the stream delivered: what a
   ;; reconnect sends as Last-Event-ID, so nothing is replayed twice.
   :journal-cursor 0
   ;; Whether the run's event stream is up. While it is, the run panels are
   ;; refreshed when an event says they changed rather than on a timer.
   :live? false
   ;; Each pane's scroll, as it last reported it: {pane {:top :max :rows}}.
   :scroll {}
   ;; What this TUI printed — command output, /help — drawn in the
   ;; conversation as the harness's voice. Local: nothing the server holds.
   :local-notes []
   ;; What was sent from the compose box, oldest first, and where Up / Down
   ;; (or Ctrl+P / Ctrl+N) have walked to in it (nil when not walking); the
   ;; line being typed when the walk began is :history-draft.
   :history []
   :history-at nil
   ;; The model and effort a /model or /effort with no run on screen set for
   ;; the next run started from here.
   :next-llm {}
   :turn-text {}
   :approvals []
   :approval-id nil
   :question-cursor 0
   :question-answers []
   ;; The row the keyboard is on in the question on screen: an option, or
   ;; one past the last, `type your own answer`.
   :question-option 0
   :expanded #{}
   :input ""})

;; --- folding what the pollers fetch -----------------------------------------

(defn- connected
  "A poll got through. It clears the error only when the error was an outage:
  one an action was answered with — a refused start, abort or steer — is not
  the poll's to take back. Clearing every error here wiped a refused start
  before a frame drew it, since start! polls straight after, so \"starting…\"
  blinked and nothing followed."
  [s]
  (cond-> (assoc s :connected? true)
    (:error-outage? s) (assoc :error nil :error-outage? false)))

(defn- disconnected [s {:keys [error]}]
  ;; Cursors and everything already drawn survive: the point of a cursor is
  ;; that an outage costs nothing, and a panel that blanked on a dropped
  ;; connection would lose the history that says what happened before it.
  (assoc s :connected? false :error (or error "no server") :error-outage? true))

(defn apply-steps
  "Fold a steps tail into the trace.

  `dropped` ACCUMULATES. The server reports what this client missed on that
  response; two polls that each missed some have missed the sum, and a panel
  shown only the latest would understate the hole in the trace."
  [s {:keys [ok body] :as r}]
  (if-not ok
    (disconnected s r)
    (let [steps (vec (:steps body))]
      (-> s
          connected
          (update :trace (fn [t] (let [t (into (vec t) steps)]
                                   (if (> (count t) max-trace)
                                     (subvec t (- (count t) max-trace))
                                     t))))
          (assoc :steps-cursor (or (:next body) (:steps-cursor s) 0))
          (update :trace-dropped + (or (:dropped body) 0))))))

(defn- supervisor? [b] (= "supervisor" (str (:role b))))

(defn- winner
  "The branch a finished run finished on: the one that is `done`."
  [branches]
  (:id (first (filter #(= "done" (str (:status %))) branches))))

(defn- active-branch
  "The branch to open a run on: the one that won, else one that is working,
  else the first.

  Opening a run and being shown nothing until you click is a worse first
  frame than opening it on the branch that is working. The supervisor's SUP
  is the last choice: it stays active after the run ends, and a run opened
  on it showed its passes where the answer should have been (karamazov-ttrn)."
  [branches]
  (or (winner branches)
      (:id (first (filter #(and (= "active" (str (:status %))) (not (supervisor? %)))
                          branches)))
      (:id (first (remove supervisor? branches)))
      (:id (first branches))))

(defn- finished? [detail]
  (contains? #{"completed" "exhausted" "failed" "aborted"}
             (str (get-in detail [:run :status]))))

(declare select-branch)

(defn apply-detail
  "Fold the run detail — the row, branches, artifacts, gates, board, files."
  [s {:keys [ok body] :as r}]
  (if-not ok
    (disconnected s r)
    (let [s (-> s connected (assoc :detail body))
          w (winner (:branches body))]
      (cond
        ;; Only when nothing is chosen. Re-picking on every poll would drag
        ;; the user off whichever branch they were reading the moment another
        ;; one became active.
        (nil? (:branch-id s))
        (cond-> (assoc s :branch-id (active-branch (:branches body)))
          (finished? body) (assoc :winner-shown (:run-id s)))

        ;; Except ONCE, when the run being watched finishes: the branch that
        ;; won is where its answer is. Run 74ddebb8 finished on B4 while the
        ;; TUI sat on an abandoned B1 and read as a failure (karamazov-ttrn).
        (and w (finished? body) (not= (:winner-shown s) (:run-id s)))
        (cond-> (assoc s :winner-shown (:run-id s))
          (not= w (:branch-id s)) (select-branch w))

        :else s))))

(defn turns-cursor
  "The newest turn row id held for the branch on screen: what to fetch the
  branch from next. nil with nothing held, which asks for all of it."
  [s]
  (some->> (get-in s [:branch :turns]) (keep :id) seq (reduce max)))

(defn apply-branch
  "Fold a branch answer. One answered from a cursor (:since) carries only the
  turns after it, and they are appended; the rest of it is current and
  replaces what was held. An answer for a branch that is no longer on screen
  is dropped — the fetch set out before the switch, and appending one
  branch's turns to another's would be a story nobody told."
  [s {:keys [ok body] :as r}]
  (cond
    (not ok) (disconnected s r)

    (and (some-> (get-in body [:branch :id]) str) (:branch-id s)
         (not= (str (get-in body [:branch :id])) (str (:branch-id s))))
    (connected s)

    (and (contains? body :since) (:branch s))
    (let [held (get-in s [:branch :turns])
          seen (set (keep :id held))]
      (-> s connected
          (assoc :branch (assoc (dissoc body :since)
                                :turns (into (vec held) (remove (comp seen :id)) (:turns body))))))

    :else (-> s connected (assoc :branch (dissoc body :since)))))

(defn apply-approvals
  "Fold the questions waiting on a person.

  The cursor and the answers collected so far are reset when the question at
  the head CHANGES — a questionnaire half answered must survive the poll that
  arrives in the middle of answering it, and a new question must not inherit
  the previous one's position."
  [s {:keys [ok body] :as r}]
  (if-not ok
    (disconnected s r)
    (let [as (vec (:approvals body))
          head (:id (first as))]
      (cond-> (assoc (connected s) :approvals as)
        (not= head (:approval-id s))
        (assoc :approval-id head :question-cursor 0 :question-answers []
               :question-selected #{} :question-option 0 :reply nil)))))

(defn answer-question
  "Record an answer and move to the next question, or say the set is done.

  Returns `[state done?]` — the caller sends the answers only when done?, so
  a half-answered questionnaire is never submitted."
  [s answers]
  (let [total (count (:questions (first (:approvals s))))
        next-i (count answers)]
    [(assoc s :question-answers (vec answers) :question-cursor (min next-i (dec total))
            :question-selected #{} :question-option 0 :reply nil)
     (>= next-i total)]))

(defn apply-turn-text
  "Fold one turn's full row — the prose the branch listing leaves out.

  A failure leaves what is already held alone. The text does not change
  after the turn is recorded, so a fetch that fails is a fetch to retry, not
  a reason to blank the words a reader is in the middle of."
  [s turn {:keys [ok body]}]
  (if ok
    (assoc-in s [:turn-text turn] body)
    s))

(defn prose-wanted
  "Which turns to fetch prose for: the ones in the newest-`n` WINDOW that are
  not already held.

  The window is chosen first and the held turns are dropped out of it. The
  other order — the newest n turns that are missing — reads the same and is
  not: with the window full it asks for the n turns BELOW it, and the poll
  after that for the n below those, so a four-hundred-turn branch fetched
  all four hundred bodies over thirty-three polls and held the whole 5.5MB
  the per-turn endpoint exists to avoid. Bounded means the set stops being
  asked for, not that each poll asks for a bounded number."
  [s n]
  (let [held (set (keys (:turn-text s)))]
    (->> (get-in s [:branch :turns])
         (map :turn)
         (sort)
         (take-last n)
         (remove held)
         vec)))

(defn apply-project
  "Fold GET /v1/harness/project.

  A failure leaves what is already held alone, like the turn text: the branch
  and the project name do not change because one poll missed, and blanking
  the footer on a dropped connection would take away the caption while
  leaving the panels it labels."
  [s {:keys [ok body]}]
  (if ok (assoc s :project body) s))

(defn apply-runs
  [s {:keys [ok body] :as r}]
  (if-not ok
    (disconnected s r)
    (-> s
        connected
        (assoc :runs (vec (:runs body)))
        ;; The server lists newest first, so the first is the one someone
        ;; opening the TUI almost always wants — but never over a choice
        ;; they already made.
        (update :run-id #(or % (:id (first (:runs body))))))))

;; --- what the user does ------------------------------------------------------

(defn select-run
  "Switch runs, dropping everything that belonged to the last one.

  Cursors especially. Carrying a cursor across a run change opened the new
  run's panels showing the old run's steps and then skipped everything
  before the stale number."
  [s run-id]
  (assoc s
         :run-id run-id
         :detail nil
         :branch-id nil
         :branch nil
         :trace []
         :steps-cursor 0
         :trace-dropped 0
         :journal-cursor 0
         ;; Turn numbers restart per branch, so prose kept across a switch
         ;; would caption the new run's turn 3 with the old run's words.
         :turn-text {}))

(defn select-branch [s branch-id]
  (assoc s :branch-id branch-id :branch nil :turn-text {}))

(defn current-question
  "The question on screen: the head questionnaire's, at the cursor. nil with
  none."
  [s]
  (let [qs (vec (:questions (first (:approvals s))))]
    (when (seq qs)
      (nth qs (min (or (:question-cursor s) 0) (dec (count qs)))))))

(defn reply-kind
  "What the compose box's next Enter is, while a dialog holds it: the deny
  note, a typed answer, or nil — a directive. An open-ended question (no
  options) takes its answer there without being asked: the box has the
  focus, and a second text box inside the dialog had to be clicked first."
  [s]
  (or (:kind (:reply s))
      (let [q (current-question s)]
        (when (and q (empty? (:options q))) :custom-answer))))

(defn- questionnaire-key
  "A key over a questionnaire with options (dirge's `question`): the cursor
  runs over the options and one more row, `type your own answer`."
  [s a q {:keys [char key]}]
  (let [opts (vec (:options q))
        n (count opts)
        at (min (or (:question-option s) 0) n)
        so-far (vec (:question-answers s))
        sel (set (:question-selected s))
        digit (some-> char str parse-long dec)
        pick (fn [i] [:answer (:id a) (conj so-far (str (nth opts i)))])]
    (cond
      (or (= :arrow-up key) (= "k" char)) [:option (max 0 (dec at))]
      (or (= :arrow-down key) (= "j" char)) [:option (min n (inc at))]
      (and digit (<= 0 digit) (< digit n)) (if (:multi q) [:toggle digit] (pick digit))
      (and (or (= :return key) (= " " char)) (= at n)) [:reply :custom-answer (:id a)]
      (and (:multi q) (= " " char)) [:toggle at]
      (and (:multi q) (= :return key))
      (if (seq sel)
        [:answer (:id a) (conj so-far (mapv #(str (nth opts %)) (sort sel)))]
        [:notice "select at least one option"])
      (or (= :return key) (= " " char)) (pick at)
      :else nil)))

(defn dialog-action
  "What a key means while a question is on screen, as an action for the loop:
  [:decide id decision-map], [:reply kind id], [:cancel-reply], [:option i]
  (move the questionnaire's cursor), [:toggle i], [:answer id answers],
  [:answer-partial id answers], [:notice text], or nil when the key is not
  the dialog's.

  Permission: y allow once, a allow always (this session — only when the
  question names the pattern it would allow), n or Esc deny, d deny with a
  note typed in the compose box. A questionnaire: Up/Down or k/j, Space,
  Enter and digits (`questionnaire-key`); Esc rejects it, or — with some
  questions answered — sends what was answered, as dirge does. Keys count
  only with nothing typed — a y in the middle of a directive is a letter,
  not a verdict."
  [s {:keys [char key] :as k}]
  (let [a (first (:approvals s))
        q (current-question s)]
    (cond
      (nil? a) nil
      (:reply s) (when (= :escape key) [:cancel-reply])
      ;; Half an answer typed: Esc throws the words away, not the questions.
      (and (= :escape key) (= :custom-answer (reply-kind s))
           (not (str/blank? (str (:input s)))))
      [:cancel-reply]
      (= :escape key) (cond
                        (seq (:question-answers s)) [:answer-partial (:id a) (vec (:question-answers s))]
                        (seq (:questions a)) [:decide (:id a) {:decision :deny :note "rejected"}]
                        :else [:decide (:id a) {:decision :deny}])
      (not (str/blank? (str (:input s)))) nil
      (seq (:questions a)) (when (seq (:options q)) (questionnaire-key s a q k))
      :else (case (str char)
              "y" [:decide (:id a) {:decision :allow}]
              "a" (when (:always a) [:decide (:id a) {:decision :allow :always true}])
              "n" [:decide (:id a) {:decision :deny}]
              "d" [:reply :deny-note (:id a)]
              nil))))

(defn move-option
  "Put the questionnaire's cursor on row `i`."
  [s i]
  (assoc s :question-option i))

(defn start-reply
  "Give the compose box to a dialog: its next Enter is the deny note or the
  custom answer, not a directive."
  [s kind id]
  (assoc s :reply {:kind kind :id id} :input ""))

(defn cancel-reply [s] (assoc s :reply nil))

(defn toggle-option
  "Tick or untick option `i` of a multi-select question."
  [s i]
  (update s :question-selected (fn [sel] (let [sel (set sel)]
                                           (if (contains? sel i) (disj sel i) (conj sel i))))))

(defn toggle-fold [s id]
  (update s :expanded (fn [e] (let [e (set e)]
                                (if (contains? e id) (disj e id) (conj e id))))))

(defn- enter-newline?
  "Whether `after` is `before` with exactly one newline inserted — what ftxui's
  multi-line input does on Enter, just before it fires on-enter."
  [before after]
  (and (= (inc (count before)) (count after))
       (let [i (count (take-while true? (map = before after)))]
         (and (= \newline (nth after i nil))
              (= before (str (subs after 0 i) (subs after (inc i))))))))

(defn set-input
  "The compose box's text. A change that is only Enter's newline is not
  kept: Enter sends, and a newline a person wants is Ctrl+J (`newline`)."
  [s text]
  (let [text (or text "")]
    (if (enter-newline? (str (:input s)) text)
      s
      (assoc s :input text))))

(defn newline
  "Ctrl+J: a line break in the compose box, on purpose."
  [s]
  (update s :input #(str % "\n")))

(defn clear-input [s]
  (assoc s :input "" :pastes {}))

;; --- Ctrl+F: searching what was sent (dirge) ---------------------------------
;;
;; While a search is open the box shows the match, and the keys belong to the
;; search: letters grow the query, Backspace shrinks it, Ctrl+F steps to the
;; next older match, Enter takes the match for editing, Esc puts back what was
;; being typed.

(defn- search-from
  "The newest history index at or below `from` whose line holds `query`."
  [history query from]
  (some #(when (str/includes? (nth history %) query) %)
        (range (min from (dec (count history))) -1 -1)))

(defn- search-show [s from]
  (let [{:keys [query]} (:search s)
        h (vec (:history s))
        i (when (seq query) (search-from h query from))]
    (cond
      i (-> s (assoc :input (nth h i)) (update :search assoc :hit i :miss false))
      (seq query) (assoc-in s [:search :miss] true)
      :else (-> s (assoc :input (get-in s [:search :draft]))
                (update :search assoc :hit nil :miss false)))))

(defn search-start [s]
  (assoc s :search {:query "" :draft (:input s) :hit nil :miss false}))

(defn searching? [s] (some? (:search s)))

(defn search-type [s ch]
  (-> s (update-in [:search :query] str ch)
      (search-show (count (:history s)))))

(defn search-backspace [s]
  (let [q (get-in s [:search :query])]
    (-> s (assoc-in [:search :query] (subs q 0 (max 0 (dec (count q)))))
        (search-show (count (:history s))))))

(defn search-next
  "The next older match, or the one shown when it is the oldest."
  [s]
  (let [{:keys [hit query]} (:search s)
        older (when (and hit (pos? hit)) (search-from (vec (:history s)) query (dec hit)))]
    (if older
      (-> s (assoc :input (nth (:history s) older)) (assoc-in [:search :hit] older))
      s)))

(defn search-accept [s] (dissoc s :search))

(defn search-cancel [s]
  (-> s (assoc :input (str (get-in s [:search :draft]))) (dissoc :search)))

;; --- @-mentions ------------------------------------------------------------------
;;
;; An @word at the end of the line opens a picker over the project's files —
;; the server lists them (GET /v1/harness/files), being the one bound to the
;; project. Tab/Down and Shift+Tab/Up move, Enter puts the path where the
;; @word was, Esc takes the @word away.

(defn mention-query
  "The word being typed after an @ at the end of `input`, or nil."
  [input]
  (second (re-find #"(?:^|\s)@([^\s@]*)$" (str input))))

(defn mention-open
  "Start (or restart) the picker for `query`; nil closes it."
  [s query]
  (if (nil? query)
    (dissoc s :mention)
    (assoc s :mention {:query query :files [] :at 0 :fetched false})))

(defn apply-mention-files
  "The server's answer for `query` — dropped when the line has moved on."
  [s query {:keys [ok body]}]
  (if (and ok (= query (get-in s [:mention :query])))
    (update s :mention assoc :files (vec (:files body)) :at 0 :fetched true)
    s))

(defn mention-active? [s] (boolean (seq (get-in s [:mention :files]))))

(defn mention-selected [s]
  (let [{:keys [files at]} (:mention s)] (nth files at nil)))

(defn mention-move [s d]
  (let [n (count (get-in s [:mention :files]))]
    (update-in s [:mention :at] #(max 0 (min (dec n) (+ (or % 0) d))))))

(defn- replace-mention [s with]
  (let [input (str (:input s))
        q (mention-query input)]
    (-> s
        (assoc :input (if q
                        (str (subs input 0 (- (count input) (count q) 1)) with)
                        input))
        (dissoc :mention))))

(defn mention-accept [s]
  (if-let [p (mention-selected s)] (replace-mention s p) s))

(defn mention-cancel [s] (replace-mention s ""))

;; --- pasting -------------------------------------------------------------------
;;
;; The TUI turns on the terminal's bracketed paste, so a paste arrives between
;; ESC[200~ and ESC[201~. Inside it Return is a line break, not a send — a
;; pasted stack trace used to go out at its first newline. A paste of
;; `paste-collapse-lines` lines or `paste-collapse-chars` characters is shown
;; as one placeholder and expanded when the line is sent (dirge's).

(def paste-collapse-lines 4)
(def paste-collapse-chars 4096)

(defn begin-paste [s] (assoc s :paste-buf ""))

(defn paste-add
  "Take `text` into the paste being received. The keys of a paste never
  reach the box one by one: FTXUI hands several to it before a frame, and a
  Return among them would be the box's Enter."
  [s text]
  (update s :paste-buf str text))

(defn pasting? [s] (contains? s :paste-buf))

(defn end-paste
  "Close the paste. It is NOT joined to the box here: FTXUI runs the box's
  on-change reports after the whole batch of keys a read delivered, so the
  state may not yet hold what was typed before the paste, and a report still
  to land would overwrite the join. `apply-paste` joins it at the next frame.
  A big paste becomes a placeholder, kept in :pastes for `expand-pastes`."
  [s]
  (let [pasted (str (:paste-buf s))
        lines (count (str/split-lines pasted))
        s (dissoc s :paste-buf)]
    (cond
      (= "" pasted) s
      (or (>= lines paste-collapse-lines) (>= (count pasted) paste-collapse-chars))
      (let [tag (str "[" lines " lines pasted #" (inc (count (:pastes s))) "]")]
        (-> s (assoc :paste-done tag) (assoc-in [:pastes tag] pasted)))
      :else (assoc s :paste-done pasted))))

(defn apply-paste
  "Join a closed paste to the end of the box. The box's cursor is the
  toolkit's, not the state's, so a paste lands after what is there."
  [s]
  (if-let [p (:paste-done s)]
    (-> s (update :input str p) (dissoc :paste-done))
    s))

(defn expand-pastes
  "`text` with every paste placeholder back to what was pasted."
  [s text]
  (reduce (fn [t [tag body]] (str/replace t tag body)) (str text) (:pastes s)))

(defn newline-key?
  "Whether a key event means a line break in the compose box: Ctrl+J, Alt+Enter
  (ESC then CR, as most terminals send it) or Shift+Enter where the terminal
  reports it (the kitty keyboard protocol's CSI 13;2u). Enter itself sends."
  [{:keys [type key input]}]
  (or (and (= :key type) (= :ctrl-j key))
      (and (= :unknown type)
           (contains? #{"\u001b\r" "\u001b\n" "\u001b[13;2u" "\u001b[27;2;13~"} (str input)))))

(def ctrl-c-window-ms
  "How long a first Ctrl+C, with a run going, waits for the second that quits."
  2000)

(defn- run-going? [s]
  (= "running" (str (get-in s [:detail :run :status]))))

(defn ctrl-c-action
  "What Ctrl+C means now, after dirge: [:clear] a draft, [:cancel-reply] a
  deny note or answer being written, [:quit] on an empty line — but with a
  run going, [:arm state'] first, and [:quit] only for a second press inside
  `ctrl-c-window-ms`. In the one-process binary quitting stops the server,
  and the run with it."
  [s now-ms]
  (cond
    (not (str/blank? (str (:input s)))) [:clear]
    (:reply s) [:cancel-reply]
    (not (run-going? s)) [:quit]
    (when-let [t (:ctrl-c-at s)] (< (- now-ms t) ctrl-c-window-ms)) [:quit]
    :else [:arm (assoc s :ctrl-c-at now-ms)]))

(defn note-error
  "Say what went wrong, and drop any notice it supersedes — a strip claiming
  \"starting…\" beside \"HTTP 503\" tells the reader nothing about which
  happened."
  [s msg]
  (cond-> (assoc s :error (when (not-empty (str msg)) (str msg)) :error-outage? false)
    (not-empty (str msg)) (assoc :notice nil)))

(defn note-notice
  "Say what is happening. Not an error, and not a place for one — but news
  newer than an earlier action's refusal, which would otherwise outrank it in
  the strip for as long as nothing else was done. An outage stays."
  [s msg]
  (cond-> (assoc s :notice (when (not-empty (str msg)) (str msg)))
    (and (not-empty (str msg)) (not (:error-outage? s))) (assoc :error nil)))

(defn steer-payload
  "What the compose box sends. Blank is not a directive."
  [text]
  (let [t (str/trim (str text))]
    (when (seq t) t)))

(defn enter-action
  "Which handler Enter in the compose box means right now.

  One box, two meanings, decided by whether there is a run to talk to. With
  a run selected the words are a directive for it. With none they are a
  PROBLEM STATEMENT, because a harness whose database is empty has nothing
  to steer and the statement is the only thing it can be given — the TUI
  could reach every run the server already had and could not make one, so
  the first thing a new user typed was answered with \"no run selected\" and
  dropped.

  A pure function rather than a branch inside the widget so the rule is
  testable, and named as a KEY so the widget still spells both handlers out
  literally — `every-handler-the-loop-offers-has-a-caller` reads those off
  the source.

  A run that has ENDED has nothing to steer either: a directive to it is
  refused, and what was typed was lost. Its status is read off the run
  detail; until that has loaded, a selected run is taken as live."
  [s]
  (let [status (some-> (get-in s [:detail :run :status]) str)]
    (if (and (:run-id s) (or (nil? status) (= "running" status))) :submit :start)))

(declare note-local)

(defn apply-start
  "Fold the answer to POST /v1/runs.

  A started run becomes the selected one, so the panels attach to what was
  just asked for rather than leaving the user to find it in the picker. A
  REFUSAL keeps the statement in the box: the server answers 503 when the
  beam does not come up inside its window, and clearing the box would make a
  retry mean retyping the paragraphs that were refused."
  [s {:keys [ok body error]}]
  (let [id (:run_id body)]
    (if (and ok (not-empty (str id)))
      (-> s
          (select-run (str id))
          clear-input
          ;; The error too: a start that worked supersedes whatever the last
          ;; attempt complained about, and leaving it up reads as this run
          ;; having failed.
          (assoc :error nil)
          (note-notice (str "started " (str id))))
      ;; Into the conversation as well: the status strip clips at 40
      ;; columns, which cut an endpoint refusal off at its URL.
      (let [why (or (not-empty (str error))
                    "the server accepted the request and returned no run id")]
        (-> s (note-error why) (note-local [(str "run not started: " why)]))))))

;; --- the pushed event stream -------------------------------------------------

(def ^:private refresh-for
  "What a journal event of each kind changed, beyond the branch it names.
  Anything unlisted changes the run detail — the gates, the artifacts, the
  board — which is the cheap default."
  {"run-finished" #{:detail :runs}
   "run-started"  #{:detail :runs}
   "run-failed"   #{:detail :runs}
   "run-error"    #{:detail :runs}})

(defn- step-entry
  "A pushed step in the shape the steps tail serves."
  [d]
  (select-keys d [:node :cell :transition :ms :failed :turn :branch_id]))

(defn- splice
  "`piece` into `held` at offset `at`: appended when it follows on, laid
  over what is held when it repeats some of it, and after the gap when one
  was lost — this is a preview the turn row replaces, not the record."
  [held piece at]
  (let [held (str held)
        at (or at (count held))]
    (str (subs held 0 (min at (count held))) piece)))

(defn- live-delta
  "A piece of the reply a branch is writing (samizdat.agent.infer publishes
  them while a model call streams)."
  [s {:keys [branch_id text reasoning] :as d}]
  (update-in s [:live branch_id]
             (fn [l]
               (let [l (or l {:text "" :reasoning ""})]
                 (cond-> l
                   ;; The start was missed (attached or reconnected mid-reply):
                   ;; what follows may be the inside of a tool call, and with
                   ;; its opening fence gone nothing can tell it from prose.
                   (and text (> (or (:text-at d) 0) (count (str (:text l)))))
                   (assoc :text-gap true)
                   text (update :text splice text (:text-at d))
                   reasoning (update :reasoning splice reasoning (:reasoning-at d)))))))

(defn apply-event
  "Fold one pushed event into the state. Returns [state wants]: `wants` is
  the set of things the event changed that the stream does not carry —
  :detail, :branch, :approvals, :runs — for the caller to fetch, coalesced."
  [s {:keys [id event data]}]
  (let [;; Off the wire an event's data is the JSON text it was sent as.
        data (if (string? data) (:data (sse/parse-data data)) data)
        s (cond-> s id (assoc :journal-cursor (or (parse-long (str id)) (:journal-cursor s))))]
    (case event
      "step" [(update s :trace (fn [t] (let [t (conj (vec t) (step-entry data))]
                                         (if (> (count t) max-trace)
                                           (subvec t (- (count t) max-trace))
                                           t))))
              #{}]
      "approval" [s #{:approvals}]
      "delta" [(live-delta s data) #{}]
      ;; A branch's turn row is in: it holds what the live preview showed.
      [(cond-> s (= "turn" event) (update :live dissoc (:branch_id data)))
       (cond-> (get refresh-for event #{:detail})
         (and (:branch_id data) (= (:branch_id data) (:branch-id s))) (conj :branch))])))

(defn stream-status
  "Note the event stream's state: `status` 200 is up, anything else down."
  [s status]
  (assoc s :live? (= 200 status)))

;; --- following the bottom ----------------------------------------------------
;;
;; A pane scrolls by rows (ftxui's :scroll). What it last reported is kept per
;; pane, {:top :max :rows}: :top the first row shown, nil following the bottom.

(defn scrolled
  "Keep what pane `pane` reported: its view and how far it can go."
  [s pane view]
  (assoc-in s [:scroll pane] view))

(defn scroll-top [s pane] (get-in s [:scroll pane :top]))

(defn follow
  "Follow the bottom of `pane` again."
  [s pane]
  (assoc-in s [:scroll pane :top] nil))

(defn page
  "Move `pane` a page up (`dir` -1) or down (1): the rows it shows, less two
  for context. Paging back to the end follows again, as dirge does."
  [s pane dir]
  (let [{:keys [top max rows]} (get-in s [:scroll pane])
        max (or max 0)
        to (clojure.core/max 0 (+ (or top max) (* dir (clojure.core/max 1 (- (or rows 3) 2)))))]
    (assoc-in s [:scroll pane :top] (when (< to max) to))))

(defn toggle-latest-fold
  "Ctrl+O, from dirge: open the newest fold in `ids` (in order), or shut it
  when it is the one already open."
  [s ids]
  (if-let [id (last ids)]
    (update s :expanded (fn [e] (let [e (set e)] (if (contains? e id) (disj e id) (conj e id)))))
    s))

;; --- what this TUI says, and what was typed ------------------------------------

(defn- now [] (str (java.time.Instant/now)))

(defn note-local
  "Lines this TUI prints into the conversation, in the harness's voice."
  [s lines]
  (let [at (now)]
    (update s :local-notes (fnil into [])
            (map-indexed (fn [i l] {:key (str "local-" at "-" i) :at at :text (str l)}) lines))))

(defn clear-local [s] (assoc s :local-notes []))

(def ^:private history-cap 500)

(defn remember-input
  "Keep what was sent, for Up / Ctrl+P. A repeat of the last line is not kept
  twice."
  [s text]
  (let [t (str/trim (str text))]
    (cond-> (-> s (assoc :history-at nil) (dissoc :history-draft))
      (and (seq t) (not= t (peek (:history s))))
      (update :history (fn [h] (let [h (conj (vec h) t)]
                                 (if (> (count h) history-cap)
                                   (subvec h (- (count h) history-cap))
                                   h)))))))

(defn history-back
  "Up on the box's top row, or Ctrl+P: the line before the one on show. The
  line being typed when the walk starts is kept, for coming back down to."
  [s]
  (let [h (:history s)]
    (if (empty? h)
      s
      (let [i (max 0 (dec (or (:history-at s) (count h))))]
        (cond-> (assoc s :history-at i :input (nth h i))
          (nil? (:history-at s)) (assoc :history-draft (:input s)))))))

(defn history-forward
  "Down on the box's bottom row, or Ctrl+N: the line after; past the newest,
  the line that was being typed."
  [s]
  (if-let [i (:history-at s)]
    (let [j (inc i)]
      (if (< j (count (:history s)))
        (assoc s :history-at j :input (nth (:history s) j))
        (-> s
            (assoc :history-at nil :input (or (:history-draft s) ""))
            (dissoc :history-draft))))
    s))
