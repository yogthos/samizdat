;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.decompose-run-test
  "The decompose-on-stuck loop end to end: a stuck root is split, the sub-units
  land, and the parent assembles — driven by a role-dispatching mock."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [samizdat.agent.gitdiff :as gitdiff]
            [samizdat.llm.client :as llm]
            [samizdat.store.db :as db]
            [samizdat.store.journal :as journal]
            [samizdat.workflow :as workflow]))

(defn- done-call [answer]
  {:content (str "```tool-call\n{\"name\":\"done\",\"args\":{\"answer\":\"" answer "\"}}\n```")
   :finish-reason "stop"})

(defn- roles
  "Architect decomposes once; the root's first attempt gives up (stuck); the
  sub-units and the assembly ship a done."
  [_ _ messages & _]
  (let [c (str/join " " (map :content messages))]
    (cond
      (str/includes? c "architect diagnosing")
      {:content (str "{\"decision\":\"decompose\",\"reason\":\"two jobs\","
                     "\"subtasks\":[{\"name\":\"part-a\",\"description\":\"do part a\"},"
                     "{\"name\":\"part-b\",\"description\":\"do part b\"}]}")
       :finish-reason "stop"}

      ;; done answers must engage their unit's problem or the done-gate refuses
      ;; them (which would make the unit loop and look stuck).
      (str/includes? c "ASSEMBLY step") (done-call "assembled the big feature from its parts")
      (str/includes? c "do part a")     (done-call "handled part a")
      (str/includes? c "do part b")     (done-call "handled part b")

      :else                       ; root's first direct attempt — get stuck
      {:content "```tool-call\n{\"name\":\"give_up\",\"args\":{\"reason\":\"too big to do at once\"}}\n```"
       :finish-reason "stop"})))

(deftest decompose-splits-a-stuck-task-lands-the-pieces-and-assembles
  (with-redefs [llm/chat roles
                gitdiff/baseline (constantly "HEAD")
                ;; every attempt "changed files" so passed? tracks the worker's
                ;; verdict; the root give_up still fails (not done).
                gitdiff/changed-files (constantly ["src/piece.clj"])]
    (let [conn (db/open! ":memory:")
          r (workflow/run! {:conn conn :config {:run {:loop "decompose"}}
                            :llm-adapter :a :llm-config {:max-tokens 16384}
                            :problem "the big feature" :max-turns 6})]
      (is (= :completed (:status r)) "the root lands once its pieces + assembly pass")
      (testing "the answer records the decompose tree with landed pieces"
        (is (str/includes? (:answer r) "landed"))
        (is (str/includes? (:answer r) "T/part-a"))
        (is (str/includes? (:answer r) "T/part-b")))
      (testing "each unit ran on its own branch: root, both sub-units, the assembly"
        (let [b (set (map :branch_id (db/fetch conn ["SELECT DISTINCT branch_id FROM turns"])))]
          (is (contains? b "DT") "root direct attempt")
          (is (contains? b "DT_part_a"))
          (is (contains? b "DT_part_b"))
          (is (contains? b "DT-a") "the assembly attempt")))
      (testing "each unit's row records the attempt framing it opened on (v24)"
        (let [suffix #(str (:prompt_suffix (db/fetch-one conn ["SELECT prompt_suffix FROM branches WHERE id = ?" %])))]
          (is (str/includes? (suffix "DT-a") (workflow/prompt-text "assembly"))
              "the assembly attempt opened on the assembly prompt")
          (is (not (str/includes? (suffix "DT_part_a") (workflow/prompt-text "assembly")))
              "a sub-unit did not")
          (is (str/includes? (suffix "DT_part_a") (workflow/prompt-text "roles/implementor"))
              "it opened as an implementor"))))))

(defn- escalating-roles
  "Architect first calls the stuck unit 'one thing' (fresh-approach); the hinted
  retry still fails; on the SECOND diagnosis — which carries the 'already tried
  and also failed' evidence — it splits. Proves the cell wires force-split
  evidence through so a fresh-approach dead-end escalates to a real split
  (karamazov-dvz) rather than abandoning."
  [_ _ messages & _]
  (let [c (str/join " " (map :content messages))]
    (cond
      (and (str/includes? c "architect diagnosing")
           (str/includes? c "already tried and also failed"))
      {:content (str "{\"decision\":\"decompose\",\"reason\":\"split it\","
                     "\"subtasks\":[{\"name\":\"part-a\",\"description\":\"do part a\"},"
                     "{\"name\":\"part-b\",\"description\":\"do part b\"}]}")
       :finish-reason "stop"}

      (str/includes? c "architect diagnosing")
      {:content "{\"decision\":\"fresh_approach\",\"reason\":\"one thing\",\"hint\":\"try a different tactic\"}"
       :finish-reason "stop"}

      (str/includes? c "ASSEMBLY step") (done-call "assembled the big feature from its parts")
      (str/includes? c "do part a")     (done-call "handled part a")
      (str/includes? c "do part b")     (done-call "handled part b")

      :else                       ; the root's direct attempt AND its hinted retry both get stuck
      {:content "```tool-call\n{\"name\":\"give_up\",\"args\":{\"reason\":\"still stuck\"}}\n```"
       :finish-reason "stop"})))

(deftest fresh-approach-dead-end-escalates-to-a-real-split
  (with-redefs [llm/chat escalating-roles
                gitdiff/baseline (constantly "HEAD")
                gitdiff/changed-files (constantly ["src/piece.clj"])]
    (let [conn (db/open! ":memory:")
          r (workflow/run! {:conn conn :config {:run {:loop "decompose"}}
                            :llm-adapter :a :llm-config {:max-tokens 16384}
                            :problem "the big feature" :max-turns 6})]
      (is (= :completed (:status r)) "the unit lands by splitting after the fresh-approach retry failed")
      (let [b (set (map :branch_id (db/fetch conn ["SELECT DISTINCT branch_id FROM turns"])))]
        (is (contains? b "DT") "root direct attempt")
        (is (contains? b "DT-h") "the fresh-approach hinted retry ran")
        (is (contains? b "DT_part_a") "then it was split — sub-unit a")
        (is (contains? b "DT_part_b") "sub-unit b")
        (is (contains? b "DT-a") "and assembled")))))

(deftest decompose-reports-its-round-in-the-shared-outcome-vocabulary
  ;; karamazov-u5uy. The three implement strategies must report a round's
  ;; outcome in ONE vocabulary or the supervisor cannot read the round it is
  ;; supervising. decompose used to report only :verdict and :branch, so on
  ;; that strategy the digest counted nothing; now every unit of the tree is
  ;; an entry in the :implement-round note, keyed the way the fan-out keys
  ;; its workers.
  (with-redefs [llm/chat roles
                gitdiff/baseline (constantly "HEAD")
                gitdiff/changed-files (constantly ["src/piece.clj"])]
    (let [conn (db/open! ":memory:")
          r (workflow/run! {:conn conn :config {:run {:loop "decompose"}}
                            :llm-adapter :a :llm-config {:max-tokens 16384}
                            :problem "the big feature" :max-turns 6})
          note (journal/last-note conn (:run-id r) :implement-round)
          results (:results note)]
      (is (= "decompose" (:strategy note)))
      (testing "every unit of the tree is an owner, not just the root"
        (is (<= 3 (count results)) "the root and its two sub-units at least")
        (is (contains? (set (map :subtask results)) "T/part-a"))
        (is (contains? (set (map :subtask results)) "T/part-b")))
      (testing "statuses are the fan-out's, so the digest counts them unchanged"
        (is (every? #{"done" "abandoned"} (map :status results)))
        (is (some #(= "done" (:status %)) results))))))
