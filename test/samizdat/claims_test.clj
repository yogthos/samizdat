;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.claims-test
  "karamazov-ei6t.3 and .4: whether a claim about the world survives the run's
  own record. The harm these exist for is karamazov-ko5b."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [samizdat.agent.tools :as tools]
            [samizdat.claims :as claims]
            [samizdat.store.db :as db]
            [samizdat.store.journal :as journal]
            [samizdat.store.runs :as runs]))

(defn- turns [& specs]
  (mapv (fn [[tool cat]] {:tool_name tool :category cat}) specs))

(deftest the-record-refutes-a-claim-it-contradicts
  ;; karamazov-ko5b in miniature: ONE failure at turn 2 became "requires do
  ;; not work in this project", while the same run's turns 5 and 12-14
  ;; required four namespaces successfully.
  (let [record (into (turns ["eval" "failure"])
                     (repeat 14 {:tool_name "eval" :category "neutral"}))
        hits (claims/contradicted-by-record
              "the eval tool cannot load any flight namespace, do not retry"
              record ["eval" "shell" "grep"])]
    (is (= 1 (count hits)))
    (is (= "eval" (:tool (first hits))))
    (is (= 14 (:worked (first hits)))
        "the COUNT, not merely that it worked — 'it worked 14 times' is a
         different argument from 'it worked', and sym/query distincts its
         bindings so the join cannot supply it")))

(deftest what-the-mechanical-check-refuses-to-judge
  ;; Narrow on purpose. A check that fired on every disagreement would refuse
  ;; the ordinary case of a run learning something, and a check nobody trusts
  ;; gets turned off.
  (let [record (turns ["eval" "neutral"] ["shell" "neutral"])]
    (testing "a claim that is not a denial is not contradicted by anything"
      (is (empty? (claims/contradicted-by-record "eval works well here"
                                                 record ["eval"]))))
    (testing "a denial about a tool the record never shows working stands"
      (is (empty? (claims/contradicted-by-record "websearch is not available here"
                                                 record ["eval" "websearch"])))
      (is (empty? (claims/contradicted-by-record "python3 is not on the allow list"
                                                 record ["eval" "shell"]))
          "naming no tool in the vocabulary means there is nothing to judge"))
    (testing "ordinary steering is untouched, which is most directives"
      (is (empty? (claims/contradicted-by-record
                   "do not ship until the ring course test is re-pinned"
                   record ["eval" "shell"]))))))

(deftest a-directive-that-contradicts-the-run-is-refused-with-the-count
  ;; The wiring, in the tool where karamazov-ko5b actually landed — an
  ;; intervene directive, not a knowledge row.
  (let [c (db/open! ":memory:")
        rid (runs/start-run! c {:problem "p" :provider "x" :model "m"
                                :max-turns 5 :beam-width 1})]
    (dotimes [i 3]
      (journal/record-turn! c rid {:branch-id "SUP" :turn (inc i)
                                   :tool-name "eval" :category :neutral
                                   :args {} :result ":loaded"}))
    (testing "refused, and the refusal cites the evidence"
      (let [r (tools/run-tool {:tool-name "intervene" :conn c :run-id rid
                               :branch {:id "SUP"}
                               :args {:kind "message" :branch "T0"
                                      :text "the eval tool cannot load any namespace here, do not retry eval"}})]
        (is (true? (:policy-refusal? r))
            "a well-formed call the harness declined, not a malformed one")
        (is (str/includes? (str (:result r)) "3 time"))
        (is (nil? (seq (db/fetch c ["SELECT id FROM interventions"])))
            "and it did not land — a false directive costs turns in the
             direction of not trying the thing that works")))
    (testing "an ordinary steer still goes through"
      (let [r (tools/run-tool {:tool-name "intervene" :conn c :run-id rid
                               :branch {:id "SUP"}
                               :args {:kind "message" :branch "T0"
                                      :text "focus on the ring course test before shipping"}})]
        (is (true? (:progress? r)))
        (is (seq (db/fetch c ["SELECT id FROM interventions"])))))
    (db/close c)))
