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
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
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

(deftest a-good-poll-does-not-erase-what-an-action-was-told
  ;; A refused start set its reason and then forced a poll; the poll's
  ;; `connected` cleared :error before a frame drew it, so "starting…"
  ;; blinked and nothing followed. Only an error the connection raised is
  ;; the connection's to clear.
  (let [refused (-> (st/initial "b")
                    (st/set-input "add fog")
                    (st/apply-start {:ok false :error "HTTP 503: model endpoint not answering"}))]
    (doseq [[what poll] [[:steps #(st/apply-steps % {:ok true :body {:steps [] :next 0}})]
                         [:runs #(st/apply-runs % {:ok true :body {:runs []}})]
                         [:detail #(st/apply-detail % {:ok true :body {:run {:status "failed"}}})]
                         [:branch #(st/apply-branch % {:ok true :body {:turns []}})]
                         [:approvals #(st/apply-approvals % {:ok true :body {:approvals []}})]]]
      (is (re-find #"503" (str (:error (poll refused)))) (str what " kept the refusal"))))
  (testing "while an outage is still cleared by the poll that gets through"
    (let [s (-> (st/initial "b")
                (st/note-error "HTTP 409: refused")
                (st/apply-runs {:ok false :error "connection refused"})
                (st/apply-runs {:ok true :body {:runs []}}))]
      (is (nil? (:error s))))))

(deftest a-refused-start-says-why-in-full
  ;; The status strip clips at 40 columns, which cut the reason off at the
  ;; endpoint's URL. The whole of it goes into the conversation.
  (let [s (st/apply-start (st/initial "b")
                          {:ok false :error "HTTP 503: model endpoint http://127.0.0.1:8080/v1 - connection refused"})]
    (is (some #(re-find #"connection refused" (:text %)) (:local-notes s)))))

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

(deftest the-dialog-keys-answer-the-permission-question-on-screen
  ;; dirge's keys: y allow once, a allow always (this session), n deny,
  ;; d deny with a note, Esc abort. The buttons name them, and a label that
  ;; names a key has to mean it. Pure, so the key policy is covered here.
  (let [s (assoc (st/initial "b") :approvals [{:id "a1" :kind "shell" :always "cargo *"}])]
    (is (= [:decide "a1" {:decision :allow}] (st/dialog-action s {:char "y"})))
    (is (= [:decide "a1" {:decision :allow :always true}] (st/dialog-action s {:char "a"})))
    (is (= [:decide "a1" {:decision :deny}] (st/dialog-action s {:char "n"})))
    (is (= [:decide "a1" {:decision :deny}] (st/dialog-action s {:key :escape})))
    (is (= [:reply :deny-note "a1"] (st/dialog-action s {:char "d"})))
    (is (nil? (st/dialog-action s {:char "q"})) "anything else belongs to whoever has focus")
    (testing "no always when the question offers no pattern to allow"
      (is (nil? (st/dialog-action (assoc-in s [:approvals 0 :always] nil) {:char "a"}))))
    (testing "not while something is being typed — a y in a directive is a letter"
      (is (nil? (st/dialog-action (assoc s :input "yes") {:char "y"}))))
    (testing "while writing the deny note, Esc goes back to the question"
      (is (= [:cancel-reply] (st/dialog-action (st/start-reply s :deny-note "a1") {:key :escape})))))
  (testing "they do nothing when no dialog is up"
    (is (nil? (st/dialog-action (st/initial "b") {:char "y"}))))
  (testing "over an open-ended question the letters are the answer being typed; Esc rejects"
    (let [s (assoc (st/initial "b") :approvals [{:id "q1" :questions [{:question "name?"}]}])]
      (is (nil? (st/dialog-action s {:char "y"})))
      (is (nil? (st/dialog-action s {:key :return})) "Enter is the compose box's: it sends the answer")
      (is (= [:decide "q1" {:decision :deny :note "rejected"}] (st/dialog-action s {:key :escape}))))))

;; --- the questionnaire from the keyboard (dirge's `question`) -----------------
;;
;; The compose box holds the focus from the first frame, so a questionnaire
;; drawn as a menu could be answered only with the mouse. dirge's keys: Up/Down
;; or k/j move, Space ticks (or picks), Enter picks or confirms, Esc rejects —
;; and a last row to type your own answer. Digits jump straight to an option.

(def ^:private two-qs
  {:id "q1" :questions [{:question "which store?" :options ["sqlite" "postgres"]}
                        {:question "which?" :multi true :options ["a" "b" "c"]}]})

(defn- asking [& {:as over}]
  (merge (assoc (st/initial "b") :approvals [two-qs]) over))

(deftest arrows-and-jk-move-through-the-options-and-the-own-answer-row
  (let [s (asking)]
    (is (= [:option 1] (st/dialog-action s {:key :arrow-down})))
    (is (= [:option 1] (st/dialog-action s {:char "j"})))
    (is (= [:option 0] (st/dialog-action (assoc s :question-option 1) {:char "k"})))
    (is (= [:option 2] (st/dialog-action (assoc s :question-option 1) {:key :arrow-down}))
        "the row after the options is `type your own answer`")
    (is (= [:option 2] (st/dialog-action (assoc s :question-option 2) {:key :arrow-down}))
        "clamped at the end, as dirge does")
    (is (= [:option 0] (st/dialog-action s {:key :arrow-up})))))

(deftest enter-picks-the-option-under-the-cursor
  (is (= [:answer "q1" ["postgres"]]
         (st/dialog-action (asking :question-option 1) {:key :return})))
  (is (= [:answer "q1" ["sqlite"]] (st/dialog-action (asking) {:char " "})) "Space picks too")
  (is (= [:answer "q1" ["postgres"]] (st/dialog-action (asking) {:char "2"})) "and a digit")
  (is (nil? (st/dialog-action (asking) {:char "7"})) "a digit with no option is nobody's")
  (testing "on the own-answer row it hands the compose box over"
    (is (= [:reply :custom-answer "q1"]
           (st/dialog-action (asking :question-option 2) {:key :return}))))
  (testing "carrying the earlier answers"
    (let [s (asking :question-cursor 1 :question-answers ["sqlite"] :question-selected #{0 2})]
      (is (= [:answer "q1" ["sqlite" ["a" "c"]]] (st/dialog-action s {:key :return}))
          "a multi-select confirms the ticked set, as one answer"))))

(deftest a-multi-select-question-ticks-with-space
  (let [s (asking :question-cursor 1 :question-answers ["sqlite"])]
    (is (= [:toggle 0] (st/dialog-action s {:char " "})))
    (is (= [:toggle 1] (st/dialog-action s {:char "2"})))
    (is (= [:notice "select at least one option"] (st/dialog-action s {:key :return}))
        "confirming nothing is refused, out loud")))

(deftest esc-rejects-or-submits-what-was-answered
  (is (= [:decide "q1" {:decision :deny :note "rejected"}] (st/dialog-action (asking) {:key :escape})))
  (is (= [:answer-partial "q1" ["sqlite"]]
         (st/dialog-action (asking :question-cursor 1 :question-answers ["sqlite"]) {:key :escape}))
      "half answered: the answers given go back, the rest as unanswered (dirge)"))

(deftest the-keys-step-aside-while-something-is-typed
  (is (nil? (st/dialog-action (asking :input "x") {:char "j"})))
  (is (nil? (st/dialog-action (asking :input "x") {:key :return})))
  (is (nil? (st/dialog-action (st/start-reply (asking) :custom-answer "q1") {:key :return}))))

(deftest an-open-ended-question-is-answered-in-the-compose-box
  (let [s (assoc (st/initial "b") :approvals [{:id "q9" :questions [{:question "name?" :options []}]}])]
    (is (= :custom-answer (st/reply-kind s)) "no options: what is typed is the answer")
    (is (nil? (st/reply-kind (asking))) "with options it is not, until asked for")
    (is (= [:cancel-reply] (st/dialog-action (assoc s :input "half") {:key :escape}))
        "Esc over half an answer throws the words away, not the question")
    (is (= :custom-answer (st/reply-kind (st/start-reply (asking) :custom-answer "q1"))))
    (is (= :deny-note (st/reply-kind (st/start-reply (asking) :deny-note "q1"))))))

(deftest a-new-question-starts-at-the-top
  (let [[s _] (st/answer-question (asking :question-option 2) ["sqlite"])]
    (is (zero? (:question-option s)) "the next question's cursor is not the last one's")))

(deftest a-reply-takes-the-compose-box-until-it-is-sent-or-cancelled
  (let [s (st/start-reply (st/initial "b") :custom-answer "q1")]
    (is (= {:kind :custom-answer :id "q1"} (:reply s)))
    (is (nil? (:reply (st/cancel-reply s))))))

(deftest multi-select-options-toggle
  (let [s (assoc (st/initial "b") :approvals [{:id "q1" :questions [{:question "which?" :multi true
                                                                        :options ["a" "b" "c"]}]}])
        s (-> s (st/toggle-option 0) (st/toggle-option 2))]
    (is (= #{0 2} (:question-selected s)))
    (is (= #{2} (:question-selected (st/toggle-option s 0))))
    (testing "a new question starts with nothing selected"
      (is (= #{} (:question-selected (st/apply-approvals s {:ok true :body {:approvals [{:id "q2"}]}})))))))

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
      "with a run on screen the words are a directive for it")
  (is (= :submit (st/enter-action (assoc (st/initial "b") :run-id "r1"
                                         :detail {:run {:status "running"}}))))
  (testing "a run that has ended cannot be steered: the words start the next one"
    ;; They went out as a directive, the server refused it, and what was typed
    ;; was gone — `describe the project` typed over a finished run.
    (doseq [status ["completed" "failed" "aborted" "exhausted"]]
      (is (= :start (st/enter-action (assoc (st/initial "b") :run-id "r1"
                                            :detail {:run {:status status}})))
          status))))

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

;; --- the pushed event stream (karamazov-tq7m.1) -------------------------------

(defn- ev [kind id data]
  {:id (some-> id str) :event kind :data data})

(deftest a-step-event-lands-in-the-trace-with-nothing-to-fetch
  (let [s (assoc (st/initial "b") :run-id "R")
        [s wants] (st/apply-event s {:event "step"
                                     :data {:kind "step" :run_id "R" :node "loop/assemble"
                                            :cell "c" :transition "ok" :ms 3}})]
    (is (= ["loop/assemble"] (mapv :node (:trace s))))
    (is (= #{} wants))))

(deftest a-journal-event-says-what-to-fetch-and-moves-the-cursor
  (let [s (assoc (st/initial "b") :run-id "R" :branch-id "B1")]
    (testing "a turn on the branch being read refreshes it and the run"
      (let [[s' w] (st/apply-event s (ev "turn" 41 {:id 41 :run_id "R" :branch_id "B1" :kind "turn"}))]
        (is (= #{:branch :detail} w))
        (is (= 41 (:journal-cursor s')) "the id a reconnect resumes after")))
    (testing "a turn on another branch refreshes only the run"
      (is (= #{:detail} (second (st/apply-event s (ev "turn" 42 {:branch_id "B2" :kind "turn"}))))))
    (testing "a question for a person refreshes the questions"
      (is (= #{:approvals} (second (st/apply-event s {:event "approval" :data {:kind "approval"}})))))
    (testing "the run ending refreshes the run list too"
      (is (= #{:detail :runs} (second (st/apply-event s (ev "run-finished" 50 {:kind "run-finished"}))))))))

(deftest the-stream-being-up-or-down-is-state
  (let [s (st/initial "b")]
    (is (false? (:live? s)))
    (is (true? (:live? (st/stream-status s 200))))
    (is (false? (:live? (st/stream-status (st/stream-status s 200) nil))))))

;; --- the compose box grows (karamazov-tq7m follow-up) ---------------------------

(deftest enter-in-the-box-sends-rather-than-breaking-the-line
  ;; The box is a multi-line ftxui input, which answers Enter by inserting a
  ;; newline AND firing on-enter. The newline is Enter's, not the person's:
  ;; it is not kept, so what is sent is what was typed.
  (let [s (st/set-input (st/initial "b") "fix the\nparser")]
    (is (= "fix the\nparser" (:input s)) "a newline Ctrl+J put there is kept")
    (is (= "fix the\nparser" (:input (st/set-input s "fix the\nparser\n"))) "Enter's at the end is not")
    (is (= "fix the\nparser" (:input (st/set-input s "fix\n the\nparser"))) "nor one in the middle")
    (is (= "fix the\nparsers" (:input (st/set-input s "fix the\nparsers"))) "typing is typing")))

(deftest a-newline-can-be-typed-on-purpose
  (is (= "one\n" (:input (st/newline (st/set-input (st/initial "b") "one"))))))

(deftest a-reply-being-written-is-folded-in-as-it-arrives
  (let [ev (fn [data] {:event "delta" :data (merge {:branch_id "B1"} data)})
        fold (fn [s e] (first (st/apply-event s e)))
        s (reduce fold (st/initial "b")
                  [(ev {:reasoning "hm" :reasoning-at 0})
                   (ev {:text "Hel" :text-at 0})
                   (ev {:text "lo" :text-at 3})])]
    (is (= {:text "Hello" :reasoning "hm"} (get-in s [:live "B1"])))
    (is (= #{} (second (st/apply-event s (ev {:text "!" :text-at 5}))))
        "nothing to fetch: the event carries it")
    (testing "a piece already held is not doubled"
      (is (= "Hello" (get-in (fold s (ev {:text "lo" :text-at 3})) [:live "B1" :text]))))
    (testing "the branch's next turn row takes over from it"
      (is (nil? (get-in (fold s {:id "9" :event "turn" :data {:branch_id "B1"}}) [:live "B1"]))))
    (testing "a piece after a gap marks the text as missing its start"
      ;; Attached mid-reply, the tail of a tool call arrived without the
      ;; fence that opens it, and was drawn as the agent's prose — raw JSON.
      (let [g (fold (st/initial "b") (ev {:text "\"}}\n```" :text-at 900}))]
        (is (true? (get-in g [:live "B1" :text-gap])))))))

(deftest a-pushed-event-is-read-as-it-comes-off-the-wire
  ;; The stream hands over each event's data as the JSON TEXT it was sent
  ;; as. Read as a map it never was, every event lost its branch: a turn on
  ;; the branch on screen asked for no branch fetch — the conversation only
  ;; caught up when something else refreshed it — and a reply being written
  ;; was filed under no branch at all.
  (let [s (assoc (st/initial "b") :run-id "R" :branch-id "B1")]
    (is (contains? (second (st/apply-event s {:id "41" :event "turn"
                                              :data "{\"id\":41,\"branch_id\":\"B1\",\"kind\":\"turn\"}"}))
                   :branch))
    (is (= "Hel" (get-in (first (st/apply-event s {:event "delta"
                                                   :data "{\"branch_id\":\"B1\",\"text\":\"Hel\",\"text-at\":0}"}))
                         [:live "B1" :text])))))

;; --- fetching a branch from where the reader is (karamazov-rf7d) -------------

(deftest a-branch-answered-from-a-cursor-is-appended
  (let [s (-> (st/initial "b") (assoc :branch-id "B1")
              (st/apply-branch {:ok true :body {:branch {:id "B1"}
                                                :turns [{:id 1 :turn 1} {:id 2 :turn 2}]}}))]
    (is (= 2 (st/turns-cursor s)) "the newest row held")
    (let [s' (st/apply-branch s {:ok true :body {:branch {:id "B1" :status "done"} :since 2
                                                 :turns [{:id 5 :turn 3}] :notes [{:id 9}]}})]
      (is (= [1 2 3] (map :turn (get-in s' [:branch :turns]))))
      (is (= "done" (get-in s' [:branch :branch :status])) "the rest of the answer is current")
      (is (= [{:id 9}] (get-in s' [:branch :notes])))
      (is (= 5 (st/turns-cursor s'))))
    (testing "a row already held is not held twice"
      (let [s' (st/apply-branch s {:ok true :body {:branch {:id "B1"} :since 1
                                                   :turns [{:id 2 :turn 2} {:id 3 :turn 3}]}})]
        (is (= [1 2 3] (map :id (get-in s' [:branch :turns]))))))
    (testing "an answer for a branch no longer on screen is dropped"
      (let [moved (st/select-branch s "B2")]
        (is (nil? (:branch (st/apply-branch moved {:ok true :body {:branch {:id "B1"} :since 2
                                                                    :turns [{:id 5 :turn 3}]}}))))
        (is (nil? (:branch (st/apply-branch moved {:ok true :body {:branch {:id "B1"}
                                                                    :turns [{:id 5 :turn 3}]}}))))))
    (testing "nothing held, the cursor is nil and the fetch is whole"
      (is (nil? (st/turns-cursor (st/initial "b")))))))

;; --- a finished run shows how it finished (karamazov-ttrn) -------------------

(deftest a-finished-run-opens-on-the-branch-that-won
  ;; Run 74ddebb8 finished on B4 and the TUI stayed on B1, an abandoned
  ;; branch whose last entry was the critic scoring it stalled — which read
  ;; as the run having failed. The supervisor's SUP stays active after the
  ;; run ends, and is never the answer.
  (let [branches [{:id "B1" :status "abandoned"} {:id "B4" :status "done"}
                  {:id "SUP" :status "active" :role "supervisor"}]
        finished {:ok true :body {:run {:status "completed"} :branches branches}}]
    (is (= "B4" (:branch-id (st/apply-detail (st/initial "b") finished))))
    (testing "a live run opens on a working branch, not the supervisor"
      (is (= "B2" (:branch-id (st/apply-detail
                               (st/initial "b")
                               {:ok true :body {:run {:status "running"}
                                                :branches [{:id "SUP" :status "active" :role "supervisor"}
                                                           {:id "B2" :status "active"}]}})))))
    (testing "watching a run finish moves to the winner once, and then leaves the choice alone"
      (let [watching (-> (st/initial "b") (assoc :run-id "R" :branch-id "B1" :branch {:turns []}))
            done (st/apply-detail watching finished)]
        (is (= "B4" (:branch-id done)))
        (is (nil? (:branch done)) "the old branch's turns are not shown under the winner's name")
        (is (= "B1" (:branch-id (st/apply-detail (st/select-branch done "B1") finished)))
            "a person who goes back to read B1 stays on B1")))))

;; --- Ctrl+C, after dirge --------------------------------------------------------
;;
;; It quit on the spot. In the one-process binary that stops the server, and
;; with it the run on screen — one stray Ctrl+C meant to clear a line ended an
;; hour of work. dirge's order: a draft is cleared, a reply is dropped, and
;; only an empty line quits — and here, with a run going, only a second press.

(deftest ctrl-c-clears-before-it-quits
  (let [running (assoc (st/initial "b") :run-id "r1" :detail {:run {:status "running"}})]
    (is (= [:clear] (st/ctrl-c-action (assoc running :input "half a thought") 1000)))
    (is (= [:cancel-reply] (st/ctrl-c-action (st/start-reply running :deny-note "a1") 1000)))
    (is (= [:quit] (st/ctrl-c-action (st/initial "b") 1000)) "idle and empty: quit")
    (let [[act s] (st/ctrl-c-action running 1000)]
      (is (= :arm act) "a run going: the first press only warns")
      (is (= [:quit] (st/ctrl-c-action s 2500)) "a second within the window quits")
      (is (= :arm (first (st/ctrl-c-action s 9000))) "one long after is a first press again"))))

(deftest alt-enter-and-shift-enter-break-the-line
  (is (st/newline-key? {:type :unknown :input "\u001b\r"}) "Alt+Enter, as most terminals send it")
  (is (st/newline-key? {:type :unknown :input "\u001b[13;2u"}) "Shift+Enter under the kitty protocol")
  (is (st/newline-key? {:type :key :key :ctrl-j}))
  (is (not (st/newline-key? {:type :key :key :return}))))

(deftest a-newer-notice-replaces-an-older-actions-error
  ;; "HTTP 409: approval not open" from a click long past sat in the strip
  ;; and hid "a run is going — Ctrl+C again to quit", the one notice that
  ;; must be read before the next key.
  (let [s (-> (st/initial "b") (st/note-error "HTTP 409: approval not open")
              (st/note-notice "a run is going"))]
    (is (nil? (:error s)))
    (is (= "a run is going" (:notice s))))
  (testing "but an outage stays: it is the poll's to clear, not a notice's"
    (let [s (-> (st/initial "b")
                (st/apply-runs {:ok false :error "no server"})
                (st/note-notice "starting…"))]
      (is (= "no server" (:error s))))))

;; --- Ctrl+F: search what was sent (dirge) ------------------------------------------

(deftest ctrl-f-searches-history-newest-first
  (let [s (-> (st/initial "b")
              (st/remember-input "run the tests")
              (st/remember-input "fix the parser")
              (st/remember-input "rerun the tests please")
              (st/set-input "draft"))
        s1 (-> s st/search-start (st/search-type "t") (st/search-type "e"))]
    (is (= "rerun the tests please" (:input s1)) "the newest line holding the query")
    (is (= "te" (get-in s1 [:search :query])))
    (let [s2 (st/search-next s1)]
      (is (= "run the tests" (:input s2)) "Ctrl+F again: the next older match")
      (is (= "run the tests" (:input (st/search-next s2))) "and it stays on the oldest"))
    (testing "no match keeps what is shown and says so"
      (let [s3 (st/search-type s1 "z")]
        (is (= "rerun the tests please" (:input s3)))
        (is (true? (get-in s3 [:search :miss])))))
    (testing "backspace widens the query again"
      (is (= "t" (get-in (st/search-backspace s1) [:search :query]))))
    (testing "Enter takes the match; Esc puts the draft back"
      (let [taken (st/search-accept s1)]
        (is (nil? (:search taken)))
        (is (= "rerun the tests please" (:input taken))))
      (let [back (st/search-cancel s1)]
        (is (nil? (:search back)))
        (is (= "draft" (:input back)))))))

;; --- pasting (bracketed paste) ------------------------------------------------------

(deftest a-large-paste-collapses-to-a-placeholder-and-expands-on-send
  ;; The paste is taken key by key into a buffer, closed, and only joined to
  ;; the box at the next frame: until then the box's own on-change reports
  ;; (which FTXUI runs after the whole batch of keys) may still be landing,
  ;; and a join made earlier was overwritten by them.
  (let [body (str/join "\n" (map #(str "line " %) (range 6)))
        paste (fn [s text] (-> s st/begin-paste (st/paste-add text) st/end-paste))
        s (paste (st/set-input (st/initial "b") "look at: ") body)]
    (is (= "look at: " (:input s)) "nothing joined yet")
    (let [s (st/set-input s "look at: typed")]
      (is (= "look at: typed[6 lines pasted #1]" (:input (st/apply-paste s)))
          "joined at the frame, after what the box reported last")
      (is (= (str "look at: typed" body) (st/expand-pastes (st/apply-paste s) "look at: typed[6 lines pasted #1]"))
          "and sent whole")
      (is (nil? (:paste-done (st/apply-paste s)))))
    (testing "a short paste goes in as it is"
      (is (= "a two\nlines" (:input (st/apply-paste (paste (st/set-input (st/initial "b") "a") " two\nlines"))))))
    (testing "a second big paste gets its own placeholder"
      (let [s (-> s st/apply-paste (paste body) st/apply-paste)]
        (is (re-find #"#2\]$" (:input s)))
        (is (= 2 (count (:pastes s))))))
    (testing "no paste, nothing to join"
      (is (= "x" (:input (st/apply-paste (st/set-input (st/initial "b") "x"))))))))

(deftest return-inside-a-paste-is-a-line-break-not-a-send
  (is (st/pasting? (st/begin-paste (st/initial "b"))))
  (is (not (st/pasting? (st/end-paste (st/begin-paste (st/initial "b")))))))

;; --- @-mentions: naming a file from the project (dirge's picker) ------------------

(deftest an-at-word-at-the-end-of-the-line-is-a-mention
  (is (= "src/co" (st/mention-query "look at @src/co")))
  (is (= "" (st/mention-query "@")))
  (is (nil? (st/mention-query "mail me at a@b.com")) "an @ inside a word is not one")
  (is (nil? (st/mention-query "look at @src/core.clj now")) "only the word being typed"))

(deftest the-mention-picker-moves-takes-and-drops
  (let [s (-> (st/initial "b") (st/set-input "fix @co") (st/mention-open "co")
              (st/apply-mention-files "co" {:ok true :body {:files ["src/core.clj" "test/core_test.clj"]}}))]
    (is (st/mention-active? s))
    (is (= "test/core_test.clj" (st/mention-selected (st/mention-move s 1))))
    (is (= "test/core_test.clj" (st/mention-selected (st/mention-move s 5))) "clamped")
    (is (= "src/core.clj" (st/mention-selected (st/mention-move s -1))))
    (is (= "fix src/core.clj" (:input (st/mention-accept s))) "the @word becomes the path")
    (is (nil? (:mention (st/mention-accept s))))
    (is (= "fix " (:input (st/mention-cancel s))) "Esc drops the @word")
    (testing "an answer for a query no longer being typed is dropped"
      (is (= ["src/core.clj" "test/core_test.clj"]
             (get-in (st/apply-mention-files s "c" {:ok true :body {:files ["x"]}}) [:mention :files]))))
    (testing "no files, nothing to pick"
      (is (not (st/mention-active? (st/apply-mention-files (st/mention-open s "zz") "zz"
                                                           {:ok true :body {:files []}})))))))
