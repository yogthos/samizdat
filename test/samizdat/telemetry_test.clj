;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.telemetry-test
  "The run-health digest the supervisor introspects on — pure over journal rows."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [samizdat.agent.telemetry :as telemetry]))

(defn- row [branch turn tool cat]
  {:branch_id branch :turn turn :tool_name tool :category cat})

(deftest branch-health-counts-turns-mechanics-and-shipped
  (let [rows [(row "W0" 1 "read_file" "neutral")
              (row "W0" 2 "__no_call__" "mechanics")
              (row "W0" 3 "done" "success")
              (row "W1" 1 "__no_call__" "mechanics")
              (row "W1" 2 "__no_call__" "mechanics")]
        h (telemetry/branch-health rows)]
    (is (= 3 (get-in h ["W0" :turns])))
    (is (= 1 (get-in h ["W0" :mechanics])))
    (is (true? (get-in h ["W0" :shipped?])))
    (is (= 2 (get-in h ["W1" :mechanics])))
    (is (false? (get-in h ["W1" :shipped?])))
    (is (== 1.0 (get-in h ["W1" :mechanics-rate])))))

(deftest signals-flags-nothing-shipped
  (let [facts {:results [{:status :exhausted} {:status :abandoned}] :revision 0}]
    (is (some #(str/includes? % "NO IMPLEMENTOR SHIPPED")
              (telemetry/signals facts {})))))

(deftest signals-flags-thrash-reviewer-bounce-and-revising
  (let [facts {:results [{:status :done}] :review :revise :revision 2}
        health {"W0" {:turns 6 :mechanics 3 :mechanics-rate 0.5 :shipped? true}}
        sigs (telemetry/signals facts health)]
    (is (some #(str/includes? % "THRASH") sigs))
    (is (some #(str/includes? % "REVIEWER BOUNCED") sigs))
    (is (some #(str/includes? % "REVISING") sigs))))

(deftest signals-quiet-on-a-healthy-run
  (let [facts {:results [{:status :done} {:status :done}] :review :pass :revision 0}
        health {"W0" {:turns 3 :mechanics 0 :mechanics-rate 0.0 :shipped? true}}]
    (is (empty? (telemetry/signals facts health)))))

(deftest digest-renders-outcomes-decisions-and-signals
  (let [d (telemetry/digest {:results [{:status :done} {:status :exhausted}]
                             :review :pass :critic :ship :revision 1}
                            [(row "W0" 1 "done" "success")])]
    (is (str/includes? d "1/2 shipped"))
    (is (str/includes? d "Reviewer: pass"))
    (is (str/includes? d "Critic: ship"))
    (is (str/includes? d "revision 1"))))

;; --- failure exemplars: the digest leads with the problem, not the rate -----

(deftest failure-exemplars-carry-the-turn-and-the-words
  (let [rows [{:id 1 :turn 1 :branch_id "B1" :tool_name "shell"
               :category "success" :result "ok"}
              {:id 2 :turn 2 :branch_id "B1" :tool_name "__parse_error__"
               :category "mechanics"
               :parse_error "JSON error (end-of-file inside string)"}
              {:id 3 :turn 3 :branch_id "B1" :tool_name "__provider_error__"
               :category "neutral"
               :result "local error 500 — Context size has been exceeded"}
              {:id 4 :turn 4 :branch_id "B1" :tool_name "shell"
               :category "failure" :result "make: no rule to make target"}]
        out (telemetry/failure-exemplars rows {:per-kind 3 :chars 120})]
    (testing "each kind appears in its own words with a fetchable row id"
      (is (str/includes? (get-in out [:parse :lines]) "row 2"))
      (is (str/includes? (get-in out [:parse :lines]) "end-of-file inside string"))
      (is (str/includes? (get-in out [:provider :lines]) "row 3"))
      (is (str/includes? (get-in out [:provider :lines]) "Context size"))
      (is (str/includes? (get-in out [:tool :lines]) "row 4"))
      (is (str/includes? (get-in out [:tool :lines]) "make: no rule")))
    (testing "successes are not failures"
      (is (not-any? #(str/includes? (str (get-in out [% :lines])) "row 1")
                    [:parse :provider :tool])))))

(deftest failure-exemplars-cap-newest-last-and-count-the-rest
  (let [rows (for [i (range 10)]
               {:id i :turn i :branch_id "B1" :tool_name "__parse_error__"
                :category "mechanics" :parse_error (str "boom-" i)})
        out (telemetry/failure-exemplars rows {:per-kind 2 :chars 60})]
    (is (= 10 (get-in out [:parse :count]))
        "the count survives the cap, so the scale is never hidden")
    (is (str/includes? (get-in out [:parse :lines]) "boom-9") "newest kept")
    (is (not (str/includes? (get-in out [:parse :lines]) "boom-0"))
        "oldest dropped")))

(deftest a-clean-run-has-no-failures-section
  (is (nil? (telemetry/failure-exemplars
             [{:id 1 :turn 1 :branch_id "B1" :tool_name "shell"
               :category "success" :result "ok"}]
             {:per-kind 3 :chars 120}))))

(deftest the-digest-leads-with-the-failures
  (let [d (telemetry/digest {:results [{:status :exhausted}]
                             :review :revise :critic nil :revision 0}
                            [{:id 7 :turn 3 :branch_id "T0"
                              :tool_name "__parse_error__" :category "mechanics"
                              :parse_error "JSON error (missing entry in object)"}])]
    (is (str/includes? d "Failures this run"))
    (is (str/includes? d "row 7"))
    (is (str/includes? d "missing entry in object"))
    (is (str/includes? d "fetch_turn"))))

;; --- layer attribution (karamazov-i1u) --------------------------------------

(deftest failures-are-attributed-to-a-layer
  (testing "a base frame is the base — unreachable from a role loop"
    (is (= :base (telemetry/layer-of
                  "UNIQUE constraint failed: branches.id at samizdat.store.runs/open-branch!")))
    (is (= :base (telemetry/layer-of "samizdat.agent.loop/tool-step threw"))))
  (testing "a cell, manifest, prompt or policy is userspace — the tools reach it"
    (is (= :userspace (telemetry/layer-of "cells.board/next threw: boom")))
    (is (= :userspace (telemetry/layer-of "cells/feature.clj:112 nil pointer")))
    (is (= :userspace (telemetry/layer-of "the board manifest has no :review edge")))
    (is (= :userspace (telemetry/layer-of "gates.edn :storm-threshold is not a number"))))
  (testing "userspace wins a tie: a cell frame means the cell is what can
            actually be edited, whatever base code it called into"
    (is (= :userspace (telemetry/layer-of
                       "cells.board/next → samizdat.store.runs/open-branch! failed"))))
  (testing "silence rather than a guess when the text does not say"
    (is (nil? (telemetry/layer-of "something went wrong")))
    (is (nil? (telemetry/layer-of nil)))))

(deftest the-digest-tells-the-supervisor-which-layer-owns-a-crash
  (let [base (telemetry/digest
              {:results [] :revision 0
               :errors ["boom at samizdat.store.runs/open-branch!"]}
              [])
        user (telemetry/digest
              {:results [] :revision 0 :errors ["boom in cells.board/next"]}
              [])]
    (is (str/includes? base "layer: base"))
    (is (str/includes? base "record it with"))
    (is (str/includes? user "layer: userspace"))
    (is (str/includes? user "yours"))))

(deftest the-digest-shows-what-worked-not-only-what-broke
  ;; Metan App. D: a failures-only diet "strips out positive exemplars and
  ;; produces overfit constraints".
  (let [rows [{:id 1 :turn 1 :branch_id "T0" :tool_name "write_file"
               :category "success" :result "wrote src/mdlite/core.clj"}
              {:id 2 :turn 2 :branch_id "T0" :tool_name "shell"
               :category "failure" :result "1 test failed"}
              {:id 3 :turn 3 :branch_id "T0" :tool_name "read_file"
               :category "neutral" :result "..."}]
        out (telemetry/failure-exemplars rows {:per-kind 3 :chars 120 :wins 2})]
    (is (str/includes? (get-in out [:wins :lines]) "wrote src/mdlite/core.clj"))
    (is (= 1 (get-in out [:wins :count])))
    (is (not (str/includes? (str (get-in out [:wins :lines])) "row 3"))
        "a neutral read is not a win worth reporting"))
  (testing "a clean run still reports nothing — wins alone are not trouble"
    (is (nil? (telemetry/failure-exemplars
               [{:id 1 :turn 1 :branch_id "T0" :tool_name "write_file"
                 :category "success" :result "ok"}]
               {:per-kind 3 :chars 120 :wins 2}))))
  (testing "and the digest renders them"
    (let [d (telemetry/digest
             {:results [] :revision 0}
             [{:id 1 :turn 1 :branch_id "T0" :tool_name "write_file"
               :category "success" :result "wrote core.clj"}
              {:id 2 :turn 2 :branch_id "T0" :tool_name "shell"
               :category "failure" :result "boom"}])]
      (is (str/includes? d "what WORKED"))
      (is (str/includes? d "wrote core.clj")))))
