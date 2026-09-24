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
  (:require [clojure.string :as str]))

(def handler-keys
  "Every action a widget may ask the loop to take.

  Named here, in the toolkit-free half, so the suite can hold both ends of
  the seam: that `samizdat.tui.core` offers exactly these, and that some
  widget calls each one. Three of them — :abort, :resume and :select-branch
  — were offered and documented for a whole release with nothing on screen
  that called them, which no test could see because each half was correct on
  its own."
  #{:decide :answer :toggle :select-run :select-branch :input :submit :start
    :abort :resume :reply :toggle-option})

(def max-trace
  "How many steps the UI holds. The server's ring is bounded and so is this:
  a TUI left running for a day would otherwise keep every step of every run
  it watched."
  500)

(defn initial [base]
  {:base base
   :connected? false
   :error nil
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
   ;; Where the conversation is scrolled to: the key of the entry held in
   ;; view, or nil to follow the bottom as new entries arrive.
   :scroll-anchor nil
   ;; What this TUI printed — command output, /help — drawn in the
   ;; conversation as the harness's voice. Local: nothing the server holds.
   :local-notes []
   ;; What was sent from the compose box, oldest first, and where Ctrl+P /
   ;; Ctrl+N have walked to in it (nil when not walking).
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
   :expanded #{}
   :input ""})

;; --- folding what the pollers fetch -----------------------------------------

(defn- connected [s]
  (assoc s :connected? true :error nil))

(defn- disconnected [s {:keys [error]}]
  ;; Cursors and everything already drawn survive: the point of a cursor is
  ;; that an outage costs nothing, and a panel that blanked on a dropped
  ;; connection would lose the history that says what happened before it.
  (assoc s :connected? false :error (or error "no server")))

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

(defn- active-branch
  "The branch to open a run on: the one that is running, else the first.

  Opening a run and being shown nothing until you click is a worse first
  frame than opening it on the branch that is working."
  [branches]
  (or (:id (first (filter #(= "active" (str (:status %))) branches)))
      (:id (first branches))))

(defn apply-detail
  "Fold the run detail — the row, branches, artifacts, gates, board, files."
  [s {:keys [ok body] :as r}]
  (if-not ok
    (disconnected s r)
    (-> s
        connected
        (assoc :detail body)
        ;; Only when nothing is chosen. Re-picking on every poll would drag
        ;; the user off whichever branch they were reading the moment another
        ;; one became active.
        (update :branch-id #(or % (active-branch (:branches body)))))))

(defn apply-branch
  [s {:keys [ok body] :as r}]
  (if-not ok
    (disconnected s r)
    (-> s connected (assoc :branch body))))

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
               :question-selected #{} :reply nil)))))

(defn answer-question
  "Record an answer and move to the next question, or say the set is done.

  Returns `[state done?]` — the caller sends the answers only when done?, so
  a half-answered questionnaire is never submitted."
  [s answers]
  (let [total (count (:questions (first (:approvals s))))
        next-i (count answers)]
    [(assoc s :question-answers (vec answers) :question-cursor (min next-i (dec total))
            :question-selected #{} :reply nil)
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

(defn dialog-action
  "What a key means while a question is on screen, as an action for the loop:
  [:decide id decision-map], [:reply kind id], [:cancel-reply], or nil when
  the key is not the dialog's.

  Permission: y allow once, a allow always (this session — only when the
  question names the pattern it would allow), n or Esc deny, d deny with a
  note typed in the compose box. A questionnaire takes only Esc: reject it.
  Letters count only with nothing typed — a y in the middle of a directive
  is a letter, not a verdict."
  [s {:keys [char key]}]
  (let [a (first (:approvals s))]
    (cond
      (nil? a) nil
      (:reply s) (when (= :escape key) [:cancel-reply])
      (= :escape key) [:decide (:id a) (if (seq (:questions a))
                                         {:decision :deny :note "rejected"}
                                         {:decision :deny})]
      (seq (:questions a)) nil
      (not (str/blank? (str (:input s)))) nil
      :else (case (str char)
              "y" [:decide (:id a) {:decision :allow}]
              "a" (when (:always a) [:decide (:id a) {:decision :allow :always true}])
              "n" [:decide (:id a) {:decision :deny}]
              "d" [:reply :deny-note (:id a)]
              nil))))

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
  (assoc s :input ""))

(defn note-error
  "Say what went wrong, and drop any notice it supersedes — a strip claiming
  \"starting…\" beside \"HTTP 503\" tells the reader nothing about which
  happened."
  [s msg]
  (cond-> (assoc s :error (when (not-empty (str msg)) (str msg)))
    (not-empty (str msg)) (assoc :notice nil)))

(defn note-notice
  "Say what is happening. Not an error, and not a place for one."
  [s msg]
  (assoc s :notice (when (not-empty (str msg)) (str msg))))

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
  the source."
  [s]
  (if (:run-id s) :submit :start))

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
      (note-error s (or (not-empty (str error))
                        "the server accepted the request and returned no run id")))))

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

(defn apply-event
  "Fold one pushed event into the state. Returns [state wants]: `wants` is
  the set of things the event changed that the stream does not carry —
  :detail, :branch, :approvals, :runs — for the caller to fetch, coalesced."
  [s {:keys [id event data]}]
  (let [s (cond-> s id (assoc :journal-cursor (or (parse-long (str id)) (:journal-cursor s))))]
    (case event
      "step" [(update s :trace (fn [t] (let [t (conj (vec t) (step-entry data))]
                                         (if (> (count t) max-trace)
                                           (subvec t (- (count t) max-trace))
                                           t))))
              #{}]
      "approval" [s #{:approvals}]
      [s (cond-> (get refresh-for event #{:detail})
           (and (:branch_id data) (= (:branch_id data) (:branch-id s))) (conj :branch))])))

(defn stream-status
  "Note the event stream's state: `status` 200 is up, anything else down."
  [s status]
  (assoc s :live? (= 200 status)))

;; --- following the bottom ----------------------------------------------------

(defn follow
  "Follow the bottom of the conversation again."
  [s]
  (assoc s :scroll-anchor nil))

(defn scroll
  "Move the conversation `delta` entries (negative is up) over `ks`, the
  entries' keys in order. Following the bottom is the anchor being nil;
  scrolling back down to the last entry follows again, and an anchor holds
  its entry however many arrive below it — the view stays where the reader
  left it (dirge's rule)."
  [s ks delta]
  (let [n (count ks)
        at (or (some-> (:scroll-anchor s) (#(.indexOf ^java.util.List ks %)) (#(when (>= % 0) %)))
               (dec n))
        to (max 0 (min (dec n) (+ at delta)))]
    (if (or (zero? n) (>= to (dec n)))
      (follow s)
      (assoc s :scroll-anchor (nth ks to)))))

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
  "Keep what was sent, for Ctrl+P. A repeat of the last line is not kept
  twice."
  [s text]
  (let [t (str/trim (str text))]
    (cond-> (assoc s :history-at nil)
      (and (seq t) (not= t (peek (:history s))))
      (update :history (fn [h] (let [h (conj (vec h) t)]
                                 (if (> (count h) history-cap)
                                   (subvec h (- (count h) history-cap))
                                   h)))))))

(defn history-back
  "Ctrl+P: the line before the one on show."
  [s]
  (let [h (:history s)]
    (if (empty? h)
      s
      (let [i (max 0 (dec (or (:history-at s) (count h))))]
        (assoc s :history-at i :input (nth h i))))))

(defn history-forward
  "Ctrl+N: the line after; past the newest, an empty line."
  [s]
  (if-let [i (:history-at s)]
    (let [j (inc i)]
      (if (< j (count (:history s)))
        (assoc s :history-at j :input (nth (:history s) j))
        (assoc s :history-at nil :input "")))
    s))
