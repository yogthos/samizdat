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
            [samizdat.api.client :as client]
            [samizdat.tui.layout :as layout]
            [samizdat.tui.state :as st]
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
  seam the supervisor uses."
  [text]
  (let [{:keys [base run-id branch-id]} @state]
    (when-let [payload (st/steer-payload text)]
      (if-not run-id
        (swap! state st/note-error "no run selected")
        (let [r (client/intervene! base run-id {:branch-id branch-id
                                                :kind "message"
                                                :payload payload})]
          (swap! state #(-> % (st/note-error (when-not (:ok r) (:error r)))
                            (cond-> (:ok r) st/clear-input))))))))

(defn- abort! []
  (let [{:keys [base run-id]} @state]
    (when run-id
      (let [r (client/abort! base run-id)]
        (swap! state st/note-error (when-not (:ok r) (:error r)))))))

(defn- resume! []
  (let [{:keys [base run-id]} @state]
    (when run-id
      (let [r (client/resume! base run-id)]
        (swap! state st/note-error (when-not (:ok r) (:error r)))))))

(defn- decide!
  "Answer a permission question. The branch is parked on this, so a failure
  has to be visible rather than swallowed — the run would otherwise sit
  there until the deadline with nobody knowing the click did nothing."
  [id decision]
  (let [r (client/decide! (:base @state) id {:decision decision})]
    (swap! state st/note-error (when-not (:ok r) (:error r)))
    ;; Poll straight away rather than waiting out the interval: the whole
    ;; point of this dialog is that somebody is watching it.
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
    (when @done?
      (let [r (client/decide! (:base @state) id {:decision :answer :answers answers})]
        (swap! state st/note-error (when-not (:ok r) (:error r)))
        (future (poll-once!))))))

(def ^:private handlers
  "What the widgets can do, handed to them in the state. Widgets stay pure
  functions and a test drives a click by passing recording functions here."
  {:decide        decide!
   :answer        answer!
   :toggle        #(swap! state st/toggle-fold %)
   :select-run    #(swap! state st/select-run %)
   :select-branch #(swap! state st/select-branch %)
   :input         #(swap! state st/set-input %)
   :submit        steer!
   :abort         abort!
   :resume        resume!})

;; --- the pollers -------------------------------------------------------------

(defn- poll-once!
  "One pass over every feed, folded into the state.

  Ordered cheapest-first and each fold is independent, so a slow branch
  detail does not hold up the step trace — the panel that moves most often
  is the one that must not wait."
  []
  (let [base (:base @state)]
    ;; The layout the harness holds, so a version the agent saved for itself
    ;; reaches the screen. Only on success: an outage must not blank the
    ;; arrangement that is already drawn.
    (let [r (client/layout base)]
      (when (:ok r) (layout/serve! (get-in r [:body :layout]))))
    (swap! state st/apply-runs (client/list-runs base))
    (when-let [rid (:run-id @state)]
      ;; The cursor is read here, after apply-runs may have selected a run —
      ;; reading it before would use the previous run's number on the first
      ;; pass after a switch.
      (swap! state st/apply-steps (client/steps-since base rid (:steps-cursor @state)))
      ;; Before the detail: a branch parked on a question is a branch doing
      ;; nothing, so the dialog is the panel that must not wait behind a
      ;; slow response.
      (swap! state st/apply-approvals (client/approvals base rid))
      (swap! state st/apply-detail (client/run-detail base rid))
      (when-let [bid (:branch-id @state)]
        (swap! state st/apply-branch (client/branch-detail base rid bid))
        ;; The prose, a turn at a time, for the newest turns only — the
        ;; branch listing drops it because it is the bulk. How many is a
        ;; userspace number, since how far back a reader wants to scroll is
        ;; their business and not the harness's.
        (doseq [n (st/prose-wanted @state (:prose-turns (layout/current) 12))]
          (swap! state st/apply-turn-text n (client/turn-detail base rid bid n)))))))

(defonce ^:private poller (atom nil))

(defn start-polling!
  "Tail the server on a background thread until `stop-polling!`.

  The interval backs off on failure through the same policy the GUI's poll
  loop uses, so a TUI left open against a stopped server is not hammering
  it. Never throws out of the loop: a poller that died would leave a UI that
  looks live and is frozen, which is the failure this project keeps finding."
  []
  (when-not @poller
    (let [running (atom true)
          t (Thread.
             (fn []
               (while @running
                 (try (poll-once!)
                      (catch Throwable e
                        (swap! state st/note-error (ex-message e))))
                 (Thread/sleep (if (:connected? @state)
                                 client/base-interval-ms
                                 client/max-backoff-ms))))
             "samizdat-tui-poller")]
      (.setDaemon t true)
      (reset! poller {:running running :thread t})
      (.start t)))
  nil)

(defn stop-polling! []
  (when-let [{:keys [running]} @poller]
    (reset! running false)
    (reset! poller nil))
  nil)

;; --- the frame ---------------------------------------------------------------

(defn root
  "One frame: read the layout, expand its widgets against the state, draw.

  Re-read every frame so a runtime edit to tui.edn takes effect on the next
  one. `layout/current` never throws and never returns something undrawable —
  a broken edit falls back to the shipped layout and reports itself in the
  status line rather than taking the screen."
  []
  (let [{:keys [layout error]} (layout/current)
        s (assoc @state :on handlers :layout-error error)]
    (layout/expand layout s)))

(defn- on-event
  "Global keys. Everything else — clicks, arrows, text — belongs to whichever
  widget has the focus, so this consumes as little as it can.

  `y`/`n` are the exception, and only while a permission dialog is up: the
  buttons are labelled `allow (y)` and `deny (n)`, and a label that names a
  key has to mean it. `state/pending-decision` decides whether there is such
  a dialog — over a questionnaire's answer box a `y` is a letter being
  typed, and it goes through untouched."
  [{:keys [type key char control]}]
  (cond
    (and (= :key type) (= :ctrl-c key)) (do (ui/exit!) true)
    (and (= :character type) control (= "q" char)) (do (ui/exit!) true)
    (and (= :key type) (= :f5 key)) (do (future (poll-once!)) true)

    (and (= :character type) (not control))
    (if-let [[id d] (st/pending-decision @state char)]
      (do (decide! id d) true)
      false)

    :else false))

(defn -main [& args]
  (let [base (or (first (remove str/blank? args)) (default-base-url))]
    (swap! state assoc :base base)
    (println "samizdat tui →" base)
    (start-polling!)
    (try
      ;; Mouse on: the folds in the conversation are ftxui collapsibles and
      ;; clicking one is how they open.
      (ui/run root :mode :fullscreen :mouse true :on-event on-event)
      (finally (stop-polling!)))))
