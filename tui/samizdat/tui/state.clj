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
  (:require [clojure.string :as str]))

(def max-trace
  "How many steps the UI holds. The server's ring is bounded and so is this:
  a TUI left running for a day would otherwise keep every step of every run
  it watched."
  500)

(defn initial [base]
  {:base base
   :connected? false
   :error nil
   :layout-error nil
   :runs []
   :run-id nil
   :detail nil
   :branch-id nil
   :branch nil
   :trace []
   :steps-cursor 0
   :trace-dropped 0
   :journal-cursor 0
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
        (assoc :approval-id head :question-cursor 0 :question-answers [])))))

(defn answer-question
  "Record an answer and move to the next question, or say the set is done.

  Returns `[state done?]` — the caller sends the answers only when done?, so
  a half-answered questionnaire is never submitted."
  [s answers]
  (let [total (count (:questions (first (:approvals s))))
        next-i (count answers)]
    [(assoc s :question-answers (vec answers) :question-cursor (min next-i (dec total)))
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
  "Which turns to fetch prose for: the newest `n` on the branch that are not
  already held.

  Bounded, and newest-first, because that is what a reader is following — a
  run of four hundred turns must not fetch four hundred bodies every poll,
  and the ones being read are at the bottom."
  [s n]
  (let [held (set (keys (:turn-text s)))]
    (->> (get-in s [:branch :turns])
         (map :turn)
         (remove held)
         (sort)
         (take-last n)
         vec)))

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

(defn toggle-fold [s id]
  (update s :expanded (fn [e] (let [e (set e)]
                                (if (contains? e id) (disj e id) (conj e id))))))

(defn set-input [s text]
  (assoc s :input (or text "")))

(defn clear-input [s]
  (assoc s :input ""))

(defn note-error [s msg]
  (assoc s :error (when (not-empty (str msg)) (str msg))))

(defn steer-payload
  "What the compose box sends. Blank is not a directive."
  [text]
  (let [t (str/trim (str text))]
    (when (seq t) t)))
