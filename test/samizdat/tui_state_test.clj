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

(ns samizdat.tui-state-test
  "The view state: what the pollers fold into, and what the widgets read.

  Pure, so the TUI's whole policy — cursors, what a disconnect does, what
  changing run resets — is testable with no terminal and no server. The run
  loop is then only wiring."
  (:require [clojure.test :refer [deftest testing is]]
            [samizdat.tui.state :as st]))

(deftest the-initial-state-names-the-server-and-nothing-else
  (let [s (st/initial "http://x:1")]
    (is (= "http://x:1" (:base s)))
    (is (nil? (:run-id s)))
    (is (= [] (:trace s)))
    (is (= #{} (:expanded s)))))

(deftest steps-append-to-the-trace-and-carry-the-cursor
  (let [s (-> (st/initial "b")
              (st/apply-steps {:ok true :body {:steps [{:seq 1 :node "start"}
                                                       {:seq 2 :node "infer"}]
                                               :next 2 :dropped 0}}))]
    (is (= ["start" "infer"] (mapv :node (:trace s))))
    (is (= 2 (:steps-cursor s)))
    (testing "a second batch appends rather than replacing"
      (let [s2 (st/apply-steps s {:ok true :body {:steps [{:seq 3 :node "parse"}]
                                                  :next 3 :dropped 0}})]
        (is (= ["start" "infer" "parse"] (mapv :node (:trace s2))))
        (is (= 3 (:steps-cursor s2)))))))

(deftest the-trace-the-ui-holds-is-bounded
  ;; The server's ring is bounded and so is this: a TUI left running for a
  ;; day would otherwise hold every step of every run it watched.
  (let [many (mapv (fn [i] {:seq (inc i) :node (str "n" i)}) (range (+ st/max-trace 50)))
        s (st/apply-steps (st/initial "b")
                          {:ok true :body {:steps many :next (count many) :dropped 0}})]
    (is (= st/max-trace (count (:trace s))))
    (is (= "n50" (:node (first (:trace s))))
        "the oldest are what go — 550 recorded, the first 50 dropped")))

(deftest a-dropped-count-is-remembered-not-just-the-latest
  ;; The server reports what THIS client missed. Two polls that each missed
  ;; some have missed the sum, and a panel that showed only the latest would
  ;; understate the hole.
  (let [s (-> (st/initial "b")
              (st/apply-steps {:ok true :body {:steps [] :next 0 :dropped 3}})
              (st/apply-steps {:ok true :body {:steps [] :next 0 :dropped 4}}))]
    (is (= 7 (:trace-dropped s)))))

(deftest a-failed-poll-marks-the-state-disconnected-and-keeps-the-cursor
  (let [s (-> (st/initial "b")
              (st/apply-steps {:ok true :body {:steps [{:seq 5 :node "x"}] :next 5}})
              (st/apply-steps {:ok false :error "connection refused"}))]
    (is (false? (:connected? s)))
    (is (= "connection refused" (:error s)))
    (is (= 5 (:steps-cursor s)) "so nothing is missed when the server returns")
    (is (= 1 (count (:trace s))) "and what was already drawn stays drawn")))

(deftest a-good-poll-clears-a-previous-error
  (let [s (-> (st/initial "b")
              (st/apply-steps {:ok false :error "boom"})
              (st/apply-steps {:ok true :body {:steps [] :next 0}}))]
    (is (true? (:connected? s)))
    (is (nil? (:error s)))))

(deftest selecting-a-run-resets-everything-that-belonged-to-the-last-one
  ;; The bug this exists to prevent: cursors and traces carried across a run
  ;; change, so the new run's panel opened showing the old run's steps and
  ;; then skipped everything before the stale cursor.
  (let [s (-> (st/initial "b")
              (st/apply-steps {:ok true :body {:steps [{:seq 9 :node "old"}] :next 9
                                               :dropped 2}})
              (assoc :branch-id "B1" :branch {:turns [{:turn 1}]}
                     :detail {:run {:id "r1"}})
              (st/select-run "r2"))]
    (is (= "r2" (:run-id s)))
    (is (= [] (:trace s)))
    (is (zero? (:steps-cursor s)))
    (is (zero? (:trace-dropped s)))
    (is (nil? (:branch s)))
    (is (nil? (:branch-id s)))
    (is (nil? (:detail s)))))

(deftest selecting-a-branch-drops-the-turns-of-the-last-one
  (let [s (-> (st/initial "b")
              (assoc :branch-id "B1" :branch {:turns [{:turn 1}]})
              (st/select-branch "B2"))]
    (is (= "B2" (:branch-id s)))
    (is (nil? (:branch s)))))

(deftest a-branch-is-picked-automatically-when-the-detail-lands
  ;; Opening a run and being shown nothing until you click a branch is a
  ;; worse first frame than opening it on the branch that is running.
  (let [s (st/apply-detail (st/initial "b")
                           {:ok true :body {:branches [{:id "B1" :status "culled"}
                                                       {:id "B2" :status "active"}]}})]
    (is (= "B2" (:branch-id s)) "the active one, not the first"))
  (testing "a choice already made is not overridden on every poll"
    (let [s (-> (st/initial "b")
                (assoc :branch-id "B1")
                (st/apply-detail {:ok true :body {:branches [{:id "B1"} {:id "B2" :status "active"}]}}))]
      (is (= "B1" (:branch-id s))))))

(deftest turn-text-lands-under-its-turn-number
  ;; The prose arrives one turn at a time, because the branch listing drops
  ;; it — so the state keeps a map the conversation reads through.
  (let [s (st/apply-turn-text (st/initial "b") 3
                              {:ok true :body {:turn 3 :assistant_text "said this"
                                               :reasoning_text "thought that"}})]
    (is (= "said this" (get-in s [:turn-text 3 :assistant_text])))
    (is (= "thought that" (get-in s [:turn-text 3 :reasoning_text])))))

(deftest a-failed-turn-text-fetch-does-not-blank-what-is-already-there
  (let [s (-> (st/initial "b")
              (st/apply-turn-text 3 {:ok true :body {:assistant_text "said this"}})
              (st/apply-turn-text 3 {:ok false :error "gone"}))]
    (is (= "said this" (get-in s [:turn-text 3 :assistant_text])))))

(deftest changing-run-drops-the-prose-of-the-last-one
  ;; Turn numbers restart per branch, so text kept across a switch would
  ;; caption the new run's turn 3 with the old run's words.
  (let [s (-> (st/initial "b")
              (st/apply-turn-text 3 {:ok true :body {:assistant_text "old run"}})
              (st/select-run "r2"))]
    (is (empty? (:turn-text s))))
  (let [s (-> (st/initial "b")
              (st/apply-turn-text 3 {:ok true :body {:assistant_text "old branch"}})
              (st/select-branch "B2"))]
    (is (empty? (:turn-text s)))))

(deftest the-turns-whose-prose-is-wanted-are-the-newest-on-screen
  ;; Bounded: a run of four hundred turns must not fetch four hundred bodies
  ;; every poll. The newest are what a reader is following.
  (let [turns (mapv (fn [n] {:turn n}) (range 1 21))
        s (assoc (st/initial "b") :branch {:turns turns})]
    (is (= [18 19 20] (st/prose-wanted s 3)))
    (testing "and text already held is not re-fetched"
      (let [s (assoc-in s [:turn-text 20] {:assistant_text "have it"})]
        ;; 18 and 19 — the rest of the WINDOW, not the next three turns
        ;; down. The window is chosen first and the held ones are dropped
        ;; out of it; picking the newest n that are missing instead walked
        ;; the whole branch n turns at a time.
        (is (= [18 19] (st/prose-wanted s 3))))))
  (testing "a branch with no turns wants nothing"
    (is (= [] (st/prose-wanted (st/initial "b") 3)))))

(deftest the-prose-window-does-not-walk-backwards-through-the-branch
  ;; The bug this pins: `remove held` before `take-last n` made every poll
  ;; ask for n turns FURTHER BACK, so a 400-turn branch fetched all 400
  ;; bodies over 33 polls and held the whole 5.5MB the per-turn endpoint
  ;; exists to avoid. Once the window is full it must ask for nothing.
  (let [turns (mapv (fn [n] {:turn n}) (range 1 401))
        s (assoc (st/initial "b") :branch {:turns turns})
        want (st/prose-wanted s 12)
        held (reduce (fn [acc n] (assoc-in acc [:turn-text n] {:assistant_text "x"}))
                     s want)]
    (is (= 12 (count want)))
    (is (= [389 400] [(first want) (last want)]) "the newest twelve")
    (is (= [] (st/prose-wanted held 12))
        "and with those held it wants nothing more — not the twelve below them")))

(deftest a-half-answered-questionnaire-survives-the-poll-that-lands-mid-answer
  ;; Polls arrive every second or so and a person takes longer than that to
  ;; read two questions. Resetting the cursor on every poll would make the
  ;; second question unreachable — you would be sent back to the first,
  ;; forever, while the branch sat parked.
  (let [q {:id "q1" :questions [{:question "a"} {:question "b"}]}
        s (-> (st/initial "b")
              (st/apply-approvals {:ok true :body {:approvals [q]}})
              (assoc :question-cursor 1 :question-answers ["first"])
              (st/apply-approvals {:ok true :body {:approvals [q]}}))]
    (is (= 1 (:question-cursor s)))
    (is (= ["first"] (:question-answers s))))
  (testing "but a different question starts clean"
    (let [s (-> (st/initial "b")
                (st/apply-approvals {:ok true :body {:approvals [{:id "q1"}]}})
                (assoc :question-cursor 1 :question-answers ["first"])
                (st/apply-approvals {:ok true :body {:approvals [{:id "q2"}]}}))]
      (is (zero? (:question-cursor s)))
      (is (= [] (:question-answers s))))))

(deftest answering-advances-until-the-last-question-is-done
  (let [s (assoc (st/initial "b")
                 :approvals [{:id "q1" :questions [{:question "a"} {:question "b"}]}])
        [s1 done1] (st/answer-question s ["yes"])]
    (is (false? done1) "one of two answered is not a set to submit")
    (is (= 1 (:question-cursor s1)))
    (let [[_ done2] (st/answer-question s1 ["yes" "no"])]
      (is (true? done2)))))

(deftest y-and-n-answer-the-permission-dialog-that-is-on-screen
  ;; The buttons are labelled "allow (y)" and "deny (n)", which was a promise
  ;; the key handler did not keep. Pure, so the key policy is covered here
  ;; rather than in the toolkit-bound suite.
  (let [s (assoc (st/initial "b") :approvals [{:id "a1" :kind "shell"}])]
    (is (= ["a1" :allow] (st/pending-decision s "y")))
    (is (= ["a1" :deny] (st/pending-decision s "n")))
    (is (nil? (st/pending-decision s "q")) "anything else belongs to whoever has focus"))
  (testing "and they do nothing when no dialog is up"
    (is (nil? (st/pending-decision (st/initial "b") "y"))))
  (testing "nor over a questionnaire, where a letter is something being typed"
    ;; The free-text box takes characters; y must reach it, not decide a
    ;; question it was never offered as an answer to.
    (let [s (assoc (st/initial "b")
                   :approvals [{:id "q1" :questions [{:question "name?"}]}])]
      (is (nil? (st/pending-decision s "y"))))))

(deftest folds-toggle-open-and-shut
  (let [s (-> (st/initial "b") (st/toggle-fold "3/result"))]
    (is (contains? (:expanded s) "3/result"))
    (is (not (contains? (:expanded (st/toggle-fold s "3/result")) "3/result")))))

(deftest the-run-list-lands-and-picks-the-newest-when-nothing-is-selected
  (let [s (st/apply-runs (st/initial "b")
                         {:ok true :body {:runs [{:id "r2"} {:id "r1"}]}})]
    (is (= 2 (count (:runs s))))
    (is (= "r2" (:run-id s)) "the first the server listed, which lists newest first"))
  (testing "and does not steal a selection the user made"
    (let [s (-> (st/initial "b")
                (assoc :run-id "r1")
                (st/apply-runs {:ok true :body {:runs [{:id "r2"} {:id "r1"}]}}))]
      (is (= "r1" (:run-id s))))))

;; --- starting a run from the compose box ------------------------------------

(deftest enter-starts-a-run-when-there-is-none-and-steers-when-there-is
  ;; The TUI could reach every run the server already had and could not make
  ;; one. Typing into the compose box with nothing selected reported "no run
  ;; selected" and dropped the text — so a freshly started harness with an
  ;; empty database had no path to its first run at all.
  (is (= :start (st/enter-action (st/initial "b")))
      "nothing to steer, so the words are a problem statement")
  (is (= :submit (st/enter-action (assoc (st/initial "b") :run-id "r1")))
      "with a run on screen the words are a directive for it"))

(deftest a-started-run-is-selected-and-the-box-is-emptied
  (let [s (-> (st/initial "b")
              (st/set-input "build a parser")
              (st/apply-start {:ok true :body {:run_id "r7"}}))]
    (is (= "r7" (:run-id s)) "the new run is what the panels now show")
    (is (= "" (:input s)) "and the statement is not left to be sent twice")
    (is (re-find #"r7" (str (:notice s))) "with the id said back")
    (is (nil? (:error s)))))

(deftest a-refused-start-keeps-the-statement
  ;; The server refuses with 503 when the beam does not come up. Clearing the
  ;; box would make the user retype several paragraphs to try again.
  (let [s (-> (st/initial "b")
              (st/set-input "build a parser")
              (st/apply-start {:ok false :error "HTTP 503"}))]
    (is (nil? (:run-id s)))
    (is (= "build a parser" (:input s)))
    (is (re-find #"503" (str (:error s)))))
  (testing "and so does a 2xx that carried no id"
    (let [s (st/apply-start (st/set-input (st/initial "b") "x")
                            {:ok true :body {}})]
      (is (nil? (:run-id s)))
      (is (= "x" (:input s)))
      (is (not-empty (str (:error s)))))))

(deftest a-notice-is-not-an-error
  ;; The status line paints :error red. "starting…" is not a failure and must
  ;; not read as one, so it travels in its own field.
  (let [s (st/note-notice (st/initial "b") "starting…")]
    (is (= "starting…" (:notice s)))
    (is (nil? (:error s))))
  (testing "and an error clears a stale notice"
    (let [s (-> (st/initial "b") (st/note-notice "starting…") (st/note-error "boom"))]
      (is (= "boom" (:error s)))
      (is (nil? (:notice s)) "or the strip would claim both at once"))))

(deftest starting-is-refused-before-it-is-sent-when-there-is-nothing-to-start
  (is (nil? (st/steer-payload "   ")))
  (is (= "go" (st/steer-payload "  go  "))))
