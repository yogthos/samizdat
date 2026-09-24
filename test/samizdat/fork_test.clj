;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.fork-test
  "What a forked branch inherits (LR-1).

  Every child used to open on a fresh [system, problem] tape, so a fork threw
  away everything its parent had learned — while the crossover block's own
  prose claimed the child carried its parent's history. These pin the split:
  the CONVERSATION is inherited, every gate counter is not."
  (:require [clojure.test :refer [deftest is testing]]
            [samizdat.agent.beam :as beam]
            [samizdat.agent.state :as state]
            [samizdat.repl :as repl]
            [samizdat.tape :as tape]))

(defn- parent-branch
  "A parent mid-run: eight messages over three turns, and a gate record that
  says it was doing badly."
  []
  (-> (state/new-branch {:id "B1" :problem "solve it"
                         :messages [{:role "system" :content "SYS"}
                                    {:role "user" :content "## Problem"}]})
      (state/add-message "assistant" "turn 1 reasoning" {:turn 1})
      (state/add-message "user" "result 1" {:turn 1})
      (state/add-turn {:turn 1 :tool "eval" :category :success})
      (state/add-message "assistant" "turn 2 reasoning" {:turn 2})
      (state/add-message "user" "result 2" {:turn 2})
      (state/add-turn {:turn 2 :tool "shell" :category :failure :error "boom"})
      (state/add-message "assistant" "turn 3 reasoning" {:turn 3})
      (state/add-message "user" "result 3" {:turn 3})
      (state/add-turn {:turn 3 :tool "shell" :category :failure :error "boom"})
      (assoc :consecutive-failures 2
             :turns-since-progress 5
             :any-progress? true
             :abandoned ["the greedy approach"]
             :artifacts [{:claim "a bound" :claim-status :confirmed :turn 2}]
             :mechanics {:calls 9 :parse-errors 3})))

(deftest a-child-inherits-the-whole-conversation-by-default
  (let [p (parent-branch)
        c (state/fork-branch p {:id "B1.2" :turn 4})]
    (is (= (:messages p) (:messages c))
        "the conversation is what a fork is FOR — re-deriving it from the problem was the bug")
    (is (= "B1" (:parent-id c)))
    (is (= 8 (:forked-at c)) "the branch point, without which the tree edge is lossy")
    (is (= "solve it" (:problem c)))))

(deftest a-child-can-branch-an-older-turn
  (let [p (parent-branch)
        c (state/fork-branch p {:id "B1.2" :depth 4 :turn 4})]
    (is (= 4 (tape/depth (:messages c))))
    (is (= (take 4 (:messages p)) (:messages c))
        "the parent as it was before the mistake, not from zero")
    (is (= 8 (tape/depth (:messages p))) "and the parent is untouched")
    (is (= 4 (:forked-at c)))))

(deftest the-turn-log-follows-the-messages-it-covers
  (testing "a full inherit carries every turn"
    (let [c (state/fork-branch (parent-branch) {:id "B1.2" :turn 4})]
      (is (= [1 2 3] (mapv :turn (:turns c))))))
  (testing "a truncated inherit carries only the turns still on the tape"
    ;; Matched by the stamps add-message writes, not by position: a provider
    ;; error or a no-call turn appends messages without appending a turn row.
    (let [c (state/fork-branch (parent-branch) {:id "B1.2" :depth 4 :turn 4})]
      (is (= [1] (mapv :turn (:turns c)))
          "turn 1 is on the inherited tape; turns 2 and 3 were truncated away")))
  (testing "a fork with no inherited tape has no turn log"
    (let [c (state/fork-branch (parent-branch) {:id "B1.2" :depth 2 :turn 4})]
      (is (= [] (:turns c))))))

(deftest a-child-is-never-charged-its-parents-failures
  (let [p (parent-branch)
        c (state/fork-branch p {:id "B1.2" :turn 4})]
    (testing "every gate counter starts clean"
      (is (= 0 (:consecutive-failures c)))
      (is (= 0 (:consecutive-mechanics-failures c)))
      (is (= 0 (:turns-since-progress c)))
      (is (false? (:any-progress? c)))
      (is (= [] (:abandoned c)))
      (is (= [] (:artifacts c)))
      (is (= {:calls 0 :parse-errors 0 :auto-repairs 0
              :unknown-tools 0 :truncations 0 :multi-fences 0 :periodic 0}
             (:mechanics c))))
    (testing "and it gets a full phase budget rather than its parent's spent one"
      (is (= 4 (:phase-entered-turn c)))
      (is (= 4 (:created-at-turn c))))))

(deftest a-thesis-rides-along-when-one-is-given
  (let [c (state/fork-branch (parent-branch)
                             {:id "B1.2" :turn 4 :thesis {:goal "try Z3"}})]
    (is (= {:goal "try Z3"} (:thesis c))))
  (is (nil? (:thesis (state/fork-branch (parent-branch) {:id "B1.2" :turn 4})))))

(deftest a-stale-depth-degrades-rather-than-erroring
  (let [p (parent-branch)]
    (is (= (:messages p) (:messages (state/fork-branch p {:id "c" :depth 999 :turn 1})))
        "a depth past the parent's length cannot lengthen the tape")
    (is (= (:messages p) (:messages (state/fork-branch p {:id "c" :depth nil :turn 1}))))))

(deftest the-parent-is-a-value-and-cannot-see-its-children
  (let [p (parent-branch)
        c (-> (state/fork-branch p {:id "B1.2" :turn 4})
              (state/add-message "assistant" "child only" {:turn 4}))]
    (is (= 8 (tape/depth (:messages p))))
    (is (= 9 (tape/depth (:messages c))))
    (is (not-any? #(= "child only" (:content %)) (:messages p))
        "fork isolation is what makes branching cost nothing")))

;; --- per-branch eval sessions ----------------------------------------------
;;
;; RFC-006 listed `dispose-branch-engines!` as a no-op seam and named what
;; would fill it: "the coding tools open sessions (nREPL, subprocesses) that
;; will want per-branch disposal here."
;;
;; What filled it was the eval session, and moving it from per-run to
;; per-branch is an ISOLATION fix as much as a disposal one. One namespace per
;; run meant five competing branches shared one set of defs.

(deftest a-branch-eval-session-is-its-own
  (let [b1 (repl/new-session)
        b2 (repl/new-session)]
    (try
      (repl/eval-code "(def only-mine 1)" b1 nil)
      (is (:ok (repl/eval-code "only-mine" b1 nil)))
      (is (not (:ok (repl/eval-code "only-mine" b2 nil)))
          "a sibling could call a helper it never defined and whose definition
           is not in its own transcript — so it worked for reasons invisible
           to it, and stopped working on a replay that did not include B1")
      (finally (repl/close-session b1) (repl/close-session b2)))))

(deftest a-fork-inherits-its-parents-defs-and-then-diverges
  ;; A child inherits its parent's CONVERSATION, so it inherits a transcript
  ;; in which those defs were made. A child that can read `(def helper …)` in
  ;; its own history but cannot call it is being shown a lie about its state.
  (let [parent (repl/new-session)]
    (try
      (repl/eval-code "(def helper 41)" parent nil)
      (let [child (repl/fork-session parent)]
        (try
          (is (= "42" (:value (repl/eval-code "(inc helper)" child nil)))
              "the inherited transcript is true")

          (repl/eval-code "(def after-fork :child)" child nil)
          (is (not (:ok (repl/eval-code "after-fork" parent nil)))
              "copied, not shared — they are competing approaches")

          (repl/eval-code "(def helper 99)" child nil)
          (is (= "41" (:value (repl/eval-code "helper" parent nil)))
              "and rebinding an inherited name does not reach back")
          (finally (repl/close-session child))))
      (finally (repl/close-session parent)))))

(deftest forking-a-dead-parent-session-still-yields-a-session
  ;; A fork must never fail on the state of the thing it is forking from.
  (let [parent (repl/new-session)]
    (repl/close-session parent)
    (let [child (repl/fork-session parent)]
      (try (is (:ok (repl/eval-code "(+ 1 1)" child nil)))
           (finally (repl/close-session child)))))
  (let [child (repl/fork-session nil)]
    (try (is (:ok (repl/eval-code "(+ 1 1)" child nil)))
         (finally (repl/close-session child)))))

(deftest disposing-a-branch-closes-its-session
  (let [session (repl/new-session)]
    (repl/eval-code "(def x 1)" session nil)
    (is (some? (find-ns session)))
    (beam/dispose-branch-engines! {:id "B1" :repl-session session})
    (is (nil? (find-ns session)) "the branch's namespace does not outlive it")
    (is (nil? (beam/dispose-branch-engines! {:id "B1" :repl-session session}))
        "safe to call twice — the run-end teardown sweeps every branch again")
    (is (nil? (beam/dispose-branch-engines! {:id "B2"}))
        "and a branch that never opened one is not an error")))

(deftest a-fork-keeps-its-parents-role
  ;; A child inherits its parent's conversation, and the role that
  ;; conversation was opened under goes with it. Run 756b5572's answerer
  ;; forked into two branches with no role: unrestricted, with write tools,
  ;; and held to the implementor's `done` ("you changed no files").
  (let [parent (assoc (state/new-branch {:id "B1" :problem "p"}) :role :answerer)]
    (is (= :answerer (:role (state/fork-branch parent {:id "B1.2" :turn 5}))))
    (is (nil? (:role (state/fork-branch (dissoc parent :role) {:id "B1.3" :turn 5})))
        "and a parent with none gives none")))
