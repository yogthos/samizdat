;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.symbolic-differential-test
  "karamazov-ei6t.11. samizdat.symbolic against a brute-force oracle.

  WHY AN ORACLE AND NOT MORE CASES. symbolic decides real things — which gate
  fires, whether a procedural graph has a trap, how wide a beam may run — and
  a wrong answer there is SILENT. A gate that should have fired and did not
  looks exactly like a gate whose condition did not hold, which is the one
  failure shape hand-written cases are worst at catching, because the case you
  did not think of is the one that breaks.

  The technique is lemmalog's (tests/differential_test.rs: 450 random programs
  against a dead-simple fixpoint oracle, plus a parser fuzz), and it is worth
  copying because of what it caught there rather than because it is elegant:
  one soundness bug in cross-run negation, and two more found while building
  entity resolution — a scoped recompute that never processed same-stratum
  dependents, and an invalidation pass that ran before lower strata were
  materialized. Three real bugs, none of which a hand-written case found.

  WHAT IS DIFFERENT HERE, and it makes the oracle EASIER rather than harder:
  symbolic is a matcher, not a deductive database. `query` joins over ground
  facts with no derivation, no recursion and no negation, so a brute-force
  oracle is a nested loop over candidate bindings — obviously correct, and
  slow in a way that does not matter at these sizes."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [samizdat.symbolic :as sym]))

;;; ------------------------------------------------------------- the oracle

(defn- oracle-query
  "Every binding of the ?vars in `where` satisfied by `tuples`, computed the
  dumbest way that is obviously right: try each clause against each tuple,
  carrying an environment, and keep what survives.

  This is the specification. When it and `sym/query` disagree, the engine is
  what is wrong until proven otherwise."
  [tuples where]
  (letfn [(unify [env clause tuple]
            (when (= (count clause) (count tuple))
              (reduce (fn [e [pat val]]
                        (cond
                          (nil? e) (reduced nil)
                          (= '_ pat) e
                          (and (symbol? pat) (str/starts-with? (name pat) "?"))
                          (if (contains? e pat)
                            (if (= (get e pat) val) e (reduced nil))
                            (assoc e pat val))
                          :else (if (= pat val) e (reduced nil))))
                      env
                      (map vector clause tuple))))]
    (let [vars (vec (sort (distinct (filter #(and (symbol? %)
                                                  (str/starts-with? (name %) "?"))
                                            (mapcat rest where)))))
          envs (reduce (fn [envs clause]
                         (for [e envs t tuples
                               :let [e' (unify e clause t)]
                               :when e']
                           e'))
                       [{}]
                       where)]
      (vec (distinct (map #(select-keys % vars) envs))))))

;;; ------------------------------------------------------------- generators

(def ^:private rels [:called :worked :failed :edge])
(def ^:private atoms [:a :b :c "x" "y" 1 2])

(defn- rand-tuple []
  (let [r (rand-nth rels)
        arity (+ 1 (rand-int 3))]
    (into [r] (repeatedly arity #(rand-nth atoms)))))

(defn- rand-clause [tuples]
  ;; Shaped like a real tuple so joins actually match sometimes; a generator
  ;; that never produces a hit tests only that both sides answer [].
  (let [t (rand-nth tuples)]
    (into [(first t)]
          (map (fn [v] (case (rand-int 3)
                         0 v
                         1 (rand-nth ['?x '?y])
                         2 '_))
               (rest t)))))

(deftest query-agrees-with-a-brute-force-oracle
  (let [seeds (range 300)
        disagreements
        (doall
         (for [s seeds
               :let [_ (java.util.Random. s)
                     tuples (vec (distinct (repeatedly (+ 2 (rand-int 8)) rand-tuple)))
                     where (vec (repeatedly (+ 1 (rand-int 2)) #(rand-clause tuples)))
                     engine (try (set (sym/query (sym/facts tuples) where))
                                 (catch Throwable e {:threw (ex-message e)}))
                     spec (try (set (oracle-query tuples where))
                               (catch Throwable e {:threw (ex-message e)}))]
               :when (not= engine spec)]
           {:tuples tuples :where where :engine engine :oracle spec}))]
    (is (empty? disagreements)
        (str "engine and oracle disagree on " (count disagreements)
             " program(s); first: " (pr-str (first disagreements))))))

(deftest rewrite-always-terminates-or-says-why
  ;; The one place symbolic can LOOP rather than merely mislead. The agent can
  ;; author a cycling rule set, so the bound is load-bearing — and a throw
  ;; that names the rules is the difference between a bug report and a hang.
  (testing "a cycling rule set throws, naming the rules that cycled"
    (let [rs (sym/ruleset [{:name :a->b :when :a :then :b}
                           {:name :b->a :when :b :then :a}])
          e (try (sym/rewrite rs :a) nil (catch Throwable t (ex-data t)))]
      (is (= :rewrite-cycle (:error e)))
      (is (= #{:a->b :b->a} (set (:rules e)))
          "both halves of the cycle are named, not just the last one to fire")))
  (testing "a rule that rewrites a term to itself is a fixpoint, not a cycle"
    (let [rs (sym/ruleset [{:name :id :when '?x :then '?x}])]
      (is (= :a (sym/rewrite rs :a)))))
  (testing "and a terminating chain reaches its end"
    (let [rs (sym/ruleset [{:name :a->b :when :a :then :b}
                           {:name :b->c :when :b :then :c}])]
      (is (= :c (sym/rewrite rs :a))))))

;;; ------------------------------------------- authoring-time checks (ei6t.12)

(deftest a-cycling-ruleset-is-caught-before-it-is-installed
  ;; rewrite/ already bounds a cycle by throwing at its limit — but that is a
  ;; spent budget and a stack trace INSIDE a run, on a ruleset that was
  ;; accepted when it was written. The agent authors rulesets, so it should
  ;; get "these two rules undo each other" at save.
  ;;
  ;; ADAPTED, not ported: lemmalog refuses a program whose NEGATION cycles,
  ;; which needs derivation, negation-as-absence and strata. symbolic has
  ;; none of those — it derives nothing — so there is nothing to stratify.
  ;; What it has is rewrite's fixpoint, and that is where the same class of
  ;; bug lives here.
  (testing "a mutual pair is a finding, reported once"
    (let [c (sym/check-ruleset (sym/ruleset [{:name :a->b :when :a :then :b}
                                             {:name :b->a :when :b :then :a}]))]
      (is (false? (:ok? c)))
      (is (= [[:a->b :b->a]] (:cycles c))
          "once, not once from each end — a cycle is one problem")))
  (testing "a terminating chain is fine"
    (is (:ok? (sym/check-ruleset (sym/ruleset [{:name :a->b :when :a :then :b}
                                               {:name :b->c :when :b :then :c}])))))
  (testing "and a rule that rewrites a term to itself is a fixpoint, not a cycle"
    (is (:ok? (sym/check-ruleset (sym/ruleset [{:name :id :when '?x :then '?x}]))))))

;;; ------------------------------------------------------ lookahead (ei6t.13)

(deftest a-hypothetical-cannot-touch-what-it-asked-about
  ;; lemmalog restores its store byte-identically after a what_if, because a
  ;; hypothetical that leaves a trace answers the NEXT question against a
  ;; store nobody meant to change. Here a fact db is an immutable value, so
  ;; the restore is free — but the property is asserted rather than trusted,
  ;; because "it is immutable" is exactly the kind of thing that stays true
  ;; until someone adds a cache.
  (let [db (sym/facts [[:worked "eval"]])
        before (sym/query db [[:worked '?t]])
        hypo (sym/hypothetical db [[:worked "shell"]] [[:worked '?t]])
        after (sym/query db [[:worked '?t]])]
    (is (= 2 (count hypo)) "the assumption is visible inside the question")
    (is (= before after) "and gone outside it")
    (is (= 1 (count after)))))

(deftest a-ruleset-can-be-previewed-before-it-is-live
  (let [rs (sym/ruleset [{:name :a->b :when :a :then :b}])]
    (is (= [{:term :a :becomes :b :ok? true}
            {:term :z :becomes :z :ok? true}]
           (sym/would-rewrite rs [:a :z]))))
  (testing "a term the rules cannot settle is REPORTED, not thrown — a preview
            that dies is worse than one that names the term it could not answer"
    (let [rs (sym/ruleset [{:name :x :when :a :then :b}
                           {:name :y :when :b :then :a}])
          [r] (sym/would-rewrite rs [:a])]
      (is (false? (:ok? r)))
      (is (= :rewrite-cycle (:why r))))))
