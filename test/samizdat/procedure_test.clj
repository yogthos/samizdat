;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.procedure-test
  "The Procedural Graph's MECHANISM (karamazov-ylte.5, 2609.09153v1).

  Deliberately no seed graph here and none shipped. The paper measures a
  hand-written prior with no evolution at 58.93 against an unguided 87.50, and
  one edited without a validation gate at 53.57 — so a graph is only worth
  having once the gate that edits it exists. What IS safe to build now is the
  machinery, because it is validated by its own structural rules rather than
  by taste.

  The substrate is samizdat.symbolic: a (procedure, relation, procedure)
  triplet IS a fact, localization IS a join, and the structural checks the
  paper's PrepareCandidate performs ARE rules. One of them the paper cannot
  perform at all — B.5 concedes that matching action nodes to the tool
  catalogue is only a refiner-PROMPT requirement and its validator does not
  enforce it."
  (:require [clojure.test :refer [deftest testing is]]
            [samizdat.procedure :as proc]))

(def a-graph
  "A small course through the coding loop. Nodes are TOOLS, which is the
  altitude a manifest is not at: loop.edn's nodes are harness stages."
  {:nodes {:task {:type :action} :grep {:type :action} :read_file {:type :action}
           :plan {:type :action} :edit_file {:type :action} :shell {:type :action}
           :thesis {:type :action} :done {:type :terminal}}
   :edges [{:from :task :to :grep :rel :leads-to}
           {:from :grep :to :read_file :rel :leads-to}
           {:from :read_file :to :plan :rel :leads-to}
           {:from :plan :to :edit_file :rel :provides-input-for}
           {:from :edit_file :to :shell :rel :leads-to}
           {:from :shell :to :read_file :rel :leads-to
            :condition {:suite :red}
            :guidance "Read the file the stack frame names before editing."
            :pitfalls "Do not edit from the error text alone."}
           {:from :shell :to :thesis :rel :leads-to
            :condition {:suite :green}
            :guidance "Bank the claim; a green run nobody recorded is re-derived."}
           {:from :thesis :to :done :rel :converges-to}]})

;; --- localization -----------------------------------------------------------

(deftest the-active-node-is-the-last-tool-and-its-neighbourhood-is-a-join
  ;; Match(a_t-1, V) then N_h(u_t). The paper's h=2.
  (let [db (proc/load-graph a-graph)
        hop1 (proc/neighbourhood db :shell 1)
        hop2 (proc/neighbourhood db :shell 2)]
    (is (= #{:thesis :read_file} (set (map :to hop1))))
    (is (= #{:done :plan} (set (map :to (remove #(= :shell (:from %)) hop2))))
        "two hops reaches what follows the immediate options")
    (testing "and the edges carry their attributes, which is what guidance reads"
      (let [green (first (filter #(= :thesis (:to %)) hop1))]
        (is (= {:suite :green} (:condition green)))
        (is (re-find #"Bank the claim" (:guidance green)))))))

(deftest an-unknown-active-node-localizes-to-nothing-rather-than-guessing
  ;; §3.2 falls back to the FULL graph when Match fails. Table 3 measures that
  ;; fallback doing worse than no graph at all on ALFWorld (54.48 vs 72.58), so
  ;; here the miss is explicit and the caller decides — it never silently
  ;; serves the whole graph.
  (let [db (proc/load-graph a-graph)]
    (is (empty? (proc/neighbourhood db :websearch 2)))))

;; --- structural checks (PrepareCandidate) -----------------------------------

(deftest every-node-must-reach-a-terminal
  ;; The paper's check, and NOT mycelium's: validate-reachability! is BFS FROM
  ;; :start (every cell reachable), this is a path TO a terminal from every
  ;; node. Duals, neither a superset. A node you can enter and never leave is a
  ;; trap, and it is the one that matters for a graph the model is advised by.
  (let [trapped (update a-graph :edges conj {:from :plan :to :websearch :rel :leads-to})
        trapped (assoc-in trapped [:nodes :websearch] {:type :action})]
    (is (empty? (:traps (proc/check a-graph #{:task :grep :read_file :plan
                                              :edit_file :shell :thesis :done}))))
    (is (= [:websearch] (:traps (proc/check trapped #{:task :grep :read_file :plan
                                                      :edit_file :shell :thesis
                                                      :done :websearch}))))))

(deftest an-edge-endpoint-must-exist
  (let [dangling (update a-graph :edges conj {:from :shell :to :nowhere :rel :leads-to})]
    (is (= [[:shell :nowhere]] (:dangling (proc/check dangling #{}))))))

(deftest an-action-node-must-be-a-real-tool
  ;; The check 2609.09153v1 B.5 concedes it does NOT make: "matching
  ;; action-node names to the available tool list is a refiner-prompt
  ;; requirement; the generic structural validator does not independently
  ;; enforce tool-catalog membership." They ask the model nicely. Here it is a
  ;; rule, against the catalogue the loop can actually dispatch.
  (let [bogus (assoc-in a-graph [:nodes :not_a_tool] {:type :action})
        catalog #{:task :grep :read_file :plan :edit_file :shell :thesis :done}]
    (is (empty? (:unknown-tools (proc/check a-graph catalog))))
    (is (= [:not_a_tool] (:unknown-tools (proc/check bogus catalog))))
    (testing "a terminal is not an action and is not held to the catalogue"
      (is (empty? (:unknown-tools (proc/check a-graph catalog)))))))

(deftest the-catalogue-comes-from-the-tool-registry-not-a-gate-vocabulary
  ;; Measured the hard way: probing against the union of gates.edn's
  ;; :shipping / :storm-exempt / :storm-mutating (31 names) reported :task,
  ;; :plan and :websearch as not-a-tool. All three are real. Those vocabularies
  ;; are GATE vocabularies and deliberately partial; tools/tool-names is the
  ;; authority, and agent_test already walks vocab names against it.
  (let [cat (proc/tool-catalog)]
    (is (contains? cat :plan))
    (is (contains? cat :grep))
    (is (contains? cat :done))
    (is (not (contains? cat :not_a_tool)))))

;; --- the check the paper has no equivalent for ------------------------------

(deftest two-outgoing-edges-whose-conditions-overlap-are-ambiguous-guidance
  ;; symbolic/overlap? on the conditions. The paper's Phi has no notion of one
  ;; node's outgoing conditions being mutually satisfiable, so nothing there
  ;; can say "at this node the graph gives two different answers".
  (let [ambiguous (update a-graph :edges conj
                          {:from :shell :to :done :rel :leads-to
                           :condition {:phase :build}})]
    (is (empty? (:ambiguous (proc/check a-graph #{}))))
    (is (seq (:ambiguous (proc/check ambiguous #{})))
        "{:phase :build} is satisfiable together with {:suite :red}")))

(deftest an-edge-subsumed-by-a-sibling-is-dead-where-it-sits
  ;; symbolic/subsumes?. Every state matching the narrower condition already
  ;; matched the earlier one, so the second edge never gets its own say.
  (let [shadowed (update a-graph :edges conj
                         {:from :shell :to :done :rel :leads-to
                          :condition {:suite :red :fresh? true}})]
    (is (empty? (:shadowed (proc/check a-graph #{}))))
    (is (= [[:shell :done]] (:shadowed (proc/check shadowed #{}))))))

(deftest a-graph-that-passes-every-check-says-so-with-no-findings
  (is (true? (:ok? (proc/check a-graph (proc/tool-catalog))))))
