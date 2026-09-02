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

(ns samizdat.symbolic-test
  "The pattern and rule engine (karamazov-41a.2).

  These tests ARE the specification of the pattern semantics, because the
  semantics are the part a rule author has to hold in their head: a map is an
  open-world subset, a vector is closed and positional, a repeated ?var means
  the same value in both places. Everything downstream — dispatch patterns,
  manifest invariants, policy rules — is written against exactly these rules."
  (:require [clojure.core.logic :as l]
            [clojure.core.logic.protocols :as lp]
            [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [jolt.fs :as fs]
            [samizdat.symbolic :as sym]))

;;; ---------------------------------------------------------------- patterns

(deftest a-map-pattern-matches-a-superset
  ;; THE central property. A workflow data map carries twenty keys a pattern
  ;; does not mention; if matching were ==, every pattern would have to
  ;; enumerate the whole map and would break the moment a cell added a key.
  (let [r (sym/rule '{:when {:phase :implement}})]
    (is (some? (sym/match r {:phase :implement})))
    (is (some? (sym/match r {:phase :implement :turn 3 :branch {:id "B1"}}))
        "extra keys are ignored")
    (is (nil? (sym/match r {:phase :review})) "a mentioned key must still match")
    (is (nil? (sym/match r {:turn 3})) "a mentioned key must be present")))

(deftest a-match-with-no-vars-binds-nothing-but-is-still-a-match
  ;; The nil/{} distinction: {} is a successful match that bound nothing.
  ;; Callers branch on it, so it must not read as failure.
  (let [r (sym/rule '{:when {:phase :implement}})]
    (is (= {} (sym/match r {:phase :implement})))
    (is (nil? (sym/match r {:phase :review})))))

(deftest a-var-binds-what-it-matched
  (let [r (sym/rule '{:when {:turn ?t :phase ?p}})]
    (is (= '{?t 7 ?p :implement} (sym/match r {:turn 7 :phase :implement :x 1})))))

(deftest a-repeated-var-means-the-same-value-in-both-places
  (let [r (sym/rule '{:when {:from ?x :to ?x}})]
    (is (= '{?x :done} (sym/match r {:from :done :to :done})))
    (is (nil? (sym/match r {:from :done :to :failed})))))

(deftest a-vector-pattern-is-positional-and-closed
  ;; Deliberately NOT open-world. A vector is a sequence whose length is part
  ;; of its meaning — a guard form, an edge pair — so an extra element is a
  ;; different term, not the same term with more detail.
  (let [r (sym/rule '{:when [:stall ?turn]})]
    (is (= '{?turn 12} (sym/match r [:stall 12])))
    (is (nil? (sym/match r [:stall 12 :extra])) "an extra element is a mismatch")
    (is (nil? (sym/match r [:stall])) "a missing element is a mismatch")
    (is (nil? (sym/match r [:other 12])))))

(deftest patterns-nest-and-stay-open-world-at-every-level
  (let [r (sym/rule '{:when {:branch {:id ?id}}})]
    (is (= '{?id "B1"} (sym/match r {:branch {:id "B1" :turn 4} :run "R1"}))
        "the inner map is a subset match too")
    (is (nil? (sym/match r {:branch {:turn 4}})))))

(deftest a-vector-inside-a-map-keeps-its-own-semantics
  (let [r (sym/rule '{:when {:edge [?from ?to]}})]
    (is (= '{?from :a ?to :b} (sym/match r {:edge [:a :b] :weight 2})))
    (is (nil? (sym/match r {:edge [:a :b :c]})))))

(deftest a-map-pattern-against-a-non-map-is-a-mismatch-not-a-throw
  ;; Rules run over whatever the harness hands them, which is not always the
  ;; shape the rule expected.
  (let [r (sym/rule '{:when {:a 1}})]
    (is (nil? (sym/match r 42)))
    (is (nil? (sym/match r nil)))
    (is (nil? (sym/match r [:a 1])))))

(deftest a-vector-pattern-also-matches-a-list-of-the-same-length
  ;; Sequential unification does not distinguish them, and EDN read back from
  ;; the store carries lists where the author wrote a vector. Recorded because
  ;; it is a real property of the language, not an accident to rely on quietly.
  (is (= '{?a 1 ?b 2} (sym/match (sym/rule '{:when [?a ?b]}) '(1 2))))
  (is (nil? (sym/match (sym/rule '{:when [?a ?b]}) '(1 2 3)))))

(deftest a-var-inside-a-set-or-list-pattern-is-a-compile-error
  ;; Sets and lists match as literals, so a ?var inside one could never bind:
  ;; the rule would compile, never fire, and report nothing. Silent
  ;; non-firing is the failure mode this whole engine is meant to remove, so
  ;; it is refused at compile time.
  (is (thrown? Exception (sym/rule '{:when {:tags #{?t}}})))
  (is (thrown? Exception (sym/rule '{:when {:form (?head ?arg)}})))
  (is (some? (sym/match (sym/rule '{:when {:tags #{:x}}}) {:tags #{:x} :n 1}))
      "a set with no vars is still a fine literal to match"))

(deftest a-var-in-a-map-key-is-a-compile-error-on-both-sides
  ;; :when because featurec cannot solve for a key; :then because substituting
  ;; an unbound var into key position would yield a map with a nil key.
  (is (thrown? Exception (sym/rule '{:when {?k 1}})))
  (is (thrown? Exception (sym/rule '{:when {:a 1} :then {?k 2}})))
  (is (= {"B1" 2} (:result (sym/first-match
                            [(sym/rule '{:when {:id ?id} :then {?id 2}})]
                            {:id "B1"})))
      "a BOUND var in key position substitutes normally"))

(deftest a-scalar-pattern-is-equality
  (is (= {} (sym/match (sym/rule '{:when :done}) :done)))
  (is (nil? (sym/match (sym/rule '{:when :done}) :failed)))
  (is (= {} (sym/match (sym/rule '{:when 42}) 42)))
  (is (nil? (sym/match (sym/rule '{:when 42}) 43))))

(deftest a-bare-var-matches-anything-and-binds-it
  (let [r (sym/rule '{:when ?whole})]
    (is (= '{?whole {:a 1}} (sym/match r {:a 1})))
    (is (= '{?whole nil} (sym/match r nil)) "including nil")))

(deftest nil-and-false-bind-without-reading-as-a-failed-match
  ;; The bug this pins: bindings threaded through `if-let`/`and` would drop a
  ;; legitimate false. A rule about a flag being off is a rule we want.
  (let [r (sym/rule '{:when {:ok ?v}})]
    (is (= '{?v false} (sym/match r {:ok false})))
    (is (= '{?v nil} (sym/match r {:ok nil})))))

;;; ------------------------------------------------------------------ guards

(deftest a-guard-filters-a-match-that-would-otherwise-succeed
  (let [r (sym/rule '{:when {:turn ?t} :if [:> ?t 10]})]
    (is (= '{?t 12} (sym/match r {:turn 12})))
    (is (nil? (sym/match r {:turn 3})) "the pattern matched; the guard rejected it")))

(deftest guards-compose
  (let [r (sym/rule '{:when {:turn ?t :phase ?p}
                      :if [:and [:> ?t 10] [:= ?p :implement]]})]
    (is (some? (sym/match r {:turn 12 :phase :implement})))
    (is (nil? (sym/match r {:turn 12 :phase :review})))
    (is (nil? (sym/match r {:turn 3 :phase :implement}))))
  (let [r (sym/rule '{:when {:s ?s} :if [:not [:empty? ?s]]})]
    (is (some? (sym/match r {:s "x"})))
    (is (nil? (sym/match r {:s ""})))))

(deftest a-guard-may-compare-two-bound-vars
  (let [r (sym/rule '{:when {:used ?u :cap ?c} :if [:> ?u ?c]})]
    (is (some? (sym/match r {:used 10 :cap 5})))
    (is (nil? (sym/match r {:used 2 :cap 5})))))

(deftest an-unregistered-guard-is-a-compile-error
  ;; THE SECURITY PROPERTY, and the reason this engine exists. The registry is
  ;; CLOSED: a guard names a predicate from a whitelist. If an unknown name
  ;; could reach a resolve or an eval, the engine would have reintroduced the
  ;; host eval that motivated replacing hand-written dispatch closures.
  (is (thrown? Exception (sym/rule '{:when {:t ?t} :if [:exec ?t]})))
  (is (thrown? Exception (sym/rule '{:when {:t ?t} :if [clojure.core/eval ?t]})))
  (is (thrown? Exception (sym/rule '{:when {:t ?t} :if [:and [:> ?t 1] [:boom ?t]]}))
      "including nested inside a composite guard"))

(deftest an-unregistered-guard-names-itself-and-what-is-available
  ;; A model that gets "unknown guard" learns nothing; one that gets the name
  ;; it used and the list it could have used fixes it on the next turn.
  (try (sym/rule '{:when {:t ?t} :if [:greater-than ?t 1]})
       (is false "should have thrown")
       (catch Exception e
         (let [d (ex-data e)]
           (is (= :greater-than (:guard d)))
           (is (contains? (set (:known d)) :>))))))

(deftest a-guard-of-the-wrong-arity-is-a-compile-error
  (is (thrown? Exception (sym/rule '{:when {:t ?t} :if [:> ?t]})))
  (is (thrown? Exception (sym/rule '{:when {:t ?t} :if [:> ?t 1 2]}))))

(deftest a-guard-over-an-unbound-var-is-a-compile-error
  ;; Otherwise it fails silently at runtime against an lvar and the rule just
  ;; never fires, which is the worst way for a rule to be wrong.
  (is (thrown? Exception (sym/rule '{:when {:turn ?t} :if [:> ?other 10]}))))

(deftest a-guard-on-the-wrong-type-rejects-rather-than-throws
  ;; Rules run over agent-authored data. A comparison against a string must
  ;; not take down the pass that is evaluating it.
  (let [r (sym/rule '{:when {:turn ?t} :if [:> ?t 10]})]
    (is (nil? (sym/match r {:turn "twelve"})))
    (is (nil? (sym/match r {:turn nil})))))

(deftest in-means-membership-and-never-index
  ;; clojure.core/contains? answers by INDEX for a vector, so [:in 0 [1 2 3]]
  ;; would be TRUE under it — never what a rule means by "is 0 one of these".
  (let [r (sym/rule '{:when {:xs ?xs} :if [:in 3 ?xs]})
        idx (sym/rule '{:when {:xs ?xs} :if [:in 0 ?xs]})]
    (is (some? (sym/match r {:xs [1 2 3]})))
    (is (nil? (sym/match idx {:xs [1 2 3]})) "0 is an index here, not a member")
    (is (some? (sym/match r {:xs #{3}})) "sets answer by key")
    (is (nil? (sym/match r {:xs nil})) "a nil collection holds nothing")))

(deftest an-empty-collection-pattern-still-means-something
  (is (= {} (sym/match (sym/rule '{:when {}}) {:a 1})) "{} matches any map")
  (is (nil? (sym/match (sym/rule '{:when {}}) 42)) "but only a map")
  (is (= {} (sym/match (sym/rule '{:when []}) [])))
  (is (nil? (sym/match (sym/rule '{:when []}) [1])) "[] is closed like any vector"))

(deftest a-set-anywhere-in-the-term-neither-hangs-nor-is-lost
  ;; REGRESSION, and the nastiest bug in this namespace's history.
  ;; core.logic 1.1.1 cannot REIFY a set: walk* calls walk-term, whose Object
  ;; case re-enters walk*, and IWalkTerm has no set case, so it recurses
  ;; forever. On the JVM that surfaces as a StackOverflowError in
  ;; Substitutions/walk; under jolt it is an unbounded HANG, which is worse
  ;; because nothing reports it. Samizdat data maps carry sets, so a rule
  ;; matching live data would have hung the harness.
  ;;
  ;; The engine now reifies only `true` and reads bindings out of the original
  ;; term. If these ever hang instead of failing, that is the regression.
  (is (= '{?xs #{3}} (sym/match (sym/rule '{:when {:xs ?xs}}) {:xs #{3}}))
      "a set bound to a var comes back whole")
  (is (= '{?v #{1 2}} (sym/match (sym/rule '{:when {:a {:b ?v}}})
                                 {:a {:b #{1 2}} :c #{9}}))
      "including nested, and with other sets alongside it")
  (is (some? (sym/match (sym/rule '{:when {:tags #{:x}}}) {:tags #{:x} :n 1}))
      "a set in the PATTERN still matches as a literal")
  (is (some? (sym/match (sym/rule '{:when {:xs ?xs} :if [:in 3 ?xs]}) {:xs #{3}}))
      "and a guard can look inside it")
  (is (= [:got #{7}]
         (:result (sym/first-match [(sym/rule '{:when {:xs ?xs} :then [:got ?xs]})]
                                   {:xs #{7}})))
      "and it survives substitution into :then"))

(deftest core-logic-can-reify-a-set-now
  ;; LOGIC-173, open upstream since 2019 and still unfixed in 1.1.1, the
  ;; newest release on Central. core.logic's IWalkTerm has no set case, so
  ;; walk* recurses through the Object case forever. samizdat.symbolic
  ;; extends it.
  ;;
  ;; TESTING A HANG IS AWKWARD, because the failure is not a wrong answer but
  ;; NO ANSWER: written the obvious way, a regression here would hang the
  ;; suite rather than fail it, and a hung suite reports nothing at all.
  ;; (samizdat.repl bounds eval on a future and that does rescue this
  ;; computation — but the test suite is not running through eval, so nothing
  ;; would rescue it here.)
  ;;
  ;; So the guard is structural and TOTAL: walk-term on a set must map the
  ;; ELEMENTS, where the Object fallback hands the whole set to f and returns
  ;; it unchanged (verified — it yields #{1} where this wants #{2}). That
  ;; discriminates the extension from its absence in a call that always
  ;; returns.
  (let [extended? (= #{2} (lp/walk-term #{1} (fn [x] (if (= x 1) 2 x))))]
    (is extended? "walk-term does not descend into a set — the extension is gone")
    ;; Only now is driving the real reifier safe: without the extension this
    ;; would hang the suite rather than fail it, so it is gated on the check
    ;; above rather than written as a peer assertion.
    (when extended?
      (is (= '(#{3}) (l/run 1 [q] (l/== q #{3})))
          "a ground set reifies")
      (is (= '({:xs #{1 2}}) (l/run 1 [q] (l/== q {:xs #{1 2}})))
          "including nested inside a map")
      (is (= '(#{7}) (l/run* [q] (l/fresh [a] (l/== q #{a}) (l/== a 7))))
          "and the reported LOGIC-173 case: a set holding an unbound lvar"))))

(deftest matching-a-set-bearing-term-does-not-depend-on-that-extension
  ;; The second, independent reason the hang cannot reach a run: match reifies
  ;; only `true` and reads bindings out of the original term, so this path is
  ;; safe even with the extension deleted. Asserted separately so that losing
  ;; one protection does not silently leave the other carrying everything.
  (is (= '{?xs #{1 2 3}} (sym/match (sym/rule '{:when {:xs ?xs}}) {:xs #{1 2 3}}))))

(deftest core-logic-stays-confined-to-this-namespace
  ;; THE RATCHET behind the set bug. samizdat.symbolic is careful to keep user
  ;; data out of reified position, but that care lives in ONE function. The
  ;; moment another namespace calls core.logic's run directly — 41a.6's everyg
  ;; over paths and 41a.8's fd are both reaching for it — the hang comes back,
  ;; and it comes back as a harness that silently stops rather than an error.
  ;;
  ;; If a later round genuinely needs core.logic elsewhere, that is a decision
  ;; to make deliberately: add the file here, and make sure whatever it reifies
  ;; cannot contain a set.
  (let [users (->> (fs/glob "src/samizdat" "**.clj")
                   (map str)
                   (filter #(str/includes? (slurp %) "core.logic"))
                   sort)]
    (is (= ["src/samizdat/symbolic.clj"] users)
        (str "core.logic is reachable from a namespace that has not thought "
             "about set reification: " (pr-str users)))))

(deftest a-set-with-a-var-is-refused-with-the-idiom-that-works
  ;; #{?t} is the natural way to write "any of these", so this refusal is one
  ;; the model WILL hit. A message that only says what is wrong sends it
  ;; looking for a structural pattern that cannot exist: set membership is a
  ;; guard, not a shape. Naming the working form is the whole value.
  (try (sym/rule '{:when {:tools #{?t}}})
       (is false "should have thrown")
       (catch Exception e
         (is (str/includes? (ex-message e) ":in")
             "the refusal names the guard that expresses membership")
         (is (some? (:instead (ex-data e)))
             "and carries a worked rule shape as data, for a renderer to use")))
  ;; The idiom it points at has to actually work.
  (is (some? (sym/match (sym/rule '{:when {:tools ?ts} :if [:in :read ?ts]})
                        {:tools #{:read :write}}))))

(deftest the-guard-catalog-is-enumerable
  ;; Discoverability: a supervisor writing a rule has to be able to ask what
  ;; guards exist without reading src/.
  (let [c (sym/guard-catalog)]
    (is (map? c))
    (is (contains? c :>))
    (is (every? string? (map :doc (vals c))) "each guard says what it does")
    (is (every? some? (map :arity (vals c))))))

;;; ------------------------------------------------------------------- rules

(deftest then-is-a-template-the-bindings-fill-in
  (let [r (sym/rule '{:when {:turn ?t :phase ?p} :then [:stalled-at ?t :in ?p]})]
    (is (= [:stalled-at 7 :in :implement]
           (:result (sym/first-match [r] {:turn 7 :phase :implement}))))))

(deftest then-substitutes-inside-nested-structure
  (let [r (sym/rule '{:when {:id ?id} :then {:target ?id :steps [?id ?id]}})]
    (is (= {:target "B1" :steps ["B1" "B1"]}
           (:result (sym/first-match [r] {:id "B1"}))))))

(deftest an-unbound-var-in-then-is-a-compile-error
  (is (thrown? Exception (sym/rule '{:when {:turn ?t} :then [:stalled ?branch]}))))

(deftest a-rule-carries-its-name-so-a-firing-can-say-which-one-it-was
  ;; What 41a.4 needs: a refusal that names the rule is a refusal the model
  ;; can comply with, instead of guessing at what it did wrong.
  (let [r (sym/rule '{:name :turn/over-budget
                      :when {:turn ?t} :if [:> ?t 10] :then :stop})]
    (is (= :turn/over-budget (:rule (sym/first-match [r] {:turn 12}))))))

(deftest a-rule-without-then-still-reports-that-it-matched
  ;; Verification rules are predicates: the fact that the pattern held IS the
  ;; finding, and there is nothing to rewrite.
  (let [r (sym/rule '{:name :always :when {:a 1}})
        hit (sym/first-match [r] {:a 1 :b 2})]
    (is (some? hit))
    (is (= :always (:rule hit)))
    (is (nil? (:result hit)))))

(deftest a-malformed-rule-is-a-compile-error
  (is (thrown? Exception (sym/rule '{:then :x})) "a rule needs a :when")
  (is (thrown? Exception (sym/rule [:not :a :map]))))

;;; -------------------------------------------------------------- strategies

(deftest first-match-wins
  (let [rs (sym/ruleset '[{:name :specific :when {:phase :implement :turn 1} :then :first-turn}
                          {:name :general :when {:phase :implement} :then :any-turn}])]
    (is (= :first-turn (:result (sym/first-match rs {:phase :implement :turn 1}))))
    (is (= :any-turn (:result (sym/first-match rs {:phase :implement :turn 2}))))
    (is (nil? (sym/first-match rs {:phase :review})))))

(deftest all-matches-returns-every-rule-that-fired-in-order
  (let [rs (sym/ruleset '[{:name :a :when {:phase :implement} :then :ra}
                          {:name :b :when {:turn ?t} :then :rb}
                          {:name :c :when {:phase :review} :then :rc}])
        hits (sym/all-matches rs {:phase :implement :turn 3})]
    (is (= [:a :b] (mapv :rule hits)))
    (is (= [:ra :rb] (mapv :result hits)))))

(deftest rewrite-runs-to-a-fixpoint
  (let [rs (sym/ruleset '[{:when [:inc ?n] :then ?n}])]
    (is (= 5 (sym/rewrite rs [:inc 5])) "one step")
    (is (= 5 (sym/rewrite rs [:inc [:inc 5]])) "and again on its own output")
    (is (= :untouched (sym/rewrite rs :untouched)) "no rule matches, term is returned")))

(deftest a-rule-that-rewrites-a-term-to-itself-has-converged
  ;; Not a loop — a fixpoint, which is precisely what rewriting runs to. The
  ;; rule keeps matching forever, so the terminating condition cannot be "no
  ;; rule matches"; it has to be "the term stopped changing".
  (let [rs (sym/ruleset '[{:name :identity :when [:a ?x] :then [:a ?x]}])]
    (is (= [:a 1] (sym/rewrite rs [:a 1])))))

(deftest rewrite-that-would-not-terminate-is-bounded
  ;; The real non-termination: a cycle between rules, where the term changes
  ;; every step and never repeats consecutively. The agent can write one, so
  ;; the engine must not hang the pass that runs it.
  (let [rs (sym/ruleset '[{:name :there :when [:a ?x] :then [:b ?x]}
                          {:name :back  :when [:b ?x] :then [:a ?x]}])]
    (is (thrown? Exception (sym/rewrite rs [:a 1])))
    (try (sym/rewrite rs [:a 1])
         (is false "should have thrown")
         (catch Exception e
           (let [d (ex-data e)]
             (is (some? (:limit d)) "the bound it hit")
             (is (= #{:there :back} (set (:rules d)))
                 "and which rules were doing the looping"))))))

;;; ----------------------------------------------------- no eval, ever

(deftest a-pattern-is-data-and-is-never-evaluated
  ;; The dispatch predicates this replaces are (fn [d] ...) forms handed to
  ;; the host eval. A pattern that contained such a form must match it as a
  ;; literal list, not run it.
  (let [r (sym/rule '{:when {:pred ?p}})
        bomb '(clojure.core/println "evaluated")]
    (is (= {'?p bomb} (sym/match r {:pred bomb}))
        "the form came back as a value")))

;;; ---------------------------------------------------------------- wildcard

(deftest an-underscore-matches-anything-and-binds-nothing
  ;; The catch-all a dispatch table ends with. It is a symbol rather than a
  ;; keyword because :_ is a keyword like any other — a LITERAL that a data
  ;; map never equals — and a catch-all that can never fire is the quietest
  ;; possible bug.
  (let [r (sym/rule '{:when {:parsed _}})]
    (is (= {} (sym/match r {:parsed nil})))
    (is (= {} (sym/match r {:parsed {:name "x"}})))
    (is (nil? (sym/match r {})) "the key must still be present"))
  (is (= {} (sym/match (sym/rule '{:when _}) {:anything 1})))
  (is (= {} (sym/match (sym/rule '{:when _}) nil)))
  (is (= '{?a 1} (sym/match (sym/rule '{:when [?a _ _]}) [1 2 3]))
      "each _ is its own hole; nothing is bound and nothing must agree"))

(deftest an-underscore-inside-a-set-or-list-is-refused-like-a-var
  (is (thrown-with-msg? Exception #"can never"
                        (sym/rule '{:when {:tools #{_}}})))
  (is (thrown-with-msg? Exception #"can never"
                        (sym/rule '{:when {:tools (_ 1)}}))))

(deftest a-key-mapped-to-nil-needs-the-key-present
  ;; {:parsed nil} is NOT (nil? (:parsed d)): the fn is true for an absent key
  ;; and the pattern is not. The loop manifest's :no-call branch was migrated
  ;; from the one to the other, and this is the difference it had to survive.
  (let [r (sym/rule '{:when {:parsed nil}})]
    (is (some? (sym/match r {:parsed nil})))
    (is (nil? (sym/match r {})))))

;;; ------------------------------------------------------------- specificity

(deftest subsumes-is-the-specificity-order
  ;; (subsumes? general specific): every term specific matches, general does.
  (is (sym/subsumes? '_ '{:a 1}))
  (is (sym/subsumes? '?x '{:a 1}))
  (is (sym/subsumes? '{:a 1} '{:a 1 :b 2}) "fewer keys is more general")
  (is (not (sym/subsumes? '{:a 1 :b 2} '{:a 1})))
  (is (sym/subsumes? '{:a ?x} '{:a 1}))
  (is (not (sym/subsumes? '{:a 1} '{:a ?x})))
  (is (sym/subsumes? '{:a {:b ?x}} '{:a {:b 1 :c 2}}) "nested, still open")
  (is (not (sym/subsumes? '{:a {:b 1}} '{:a ?x})) "a var may be bound to a non-map")
  (is (sym/subsumes? '[?x ?y] '[1 2]))
  (is (not (sym/subsumes? '[?x ?y] '[1 2 3])) "vectors are closed")
  (is (sym/subsumes? '{:a 1} '{:a 1}) "a pattern subsumes itself")
  (is (not (sym/subsumes? '{:a 1} '{:b 2})) "incomparable")
  (is (not (sym/subsumes? '{:a 1} '_)) "nothing but a hole subsumes a hole"))

(deftest a-repeated-var-is-more-specific-than-two-vars
  (is (sym/subsumes? '{:from ?x :to ?y} '{:from ?x :to ?x}))
  (is (not (sym/subsumes? '{:from ?x :to ?x} '{:from ?a :to ?b})))
  (is (sym/subsumes? '{:from ?x :to ?x} '{:from 1 :to 1}))
  (is (not (sym/subsumes? '{:from ?x :to ?x} '{:from 1 :to 2}))))

(def ^:private corpus-terms
  [{:a 1} {:a 1 :b 2} {:a {:b 1 :c 2}} {:a [1 2]} {:a nil} {:from 1 :to 1}
   {:from 1 :to 2} {:b 2} {} nil [1 2] [1 2 3] 7])

(def ^:private corpus-patterns
  '[_ ?x {:a 1} {:a 1 :b 2} {:a ?x} {:a {:b 1}} {:a {:b ?x}} {:a nil} [?x ?y]
    [?x _] {:from ?x :to ?x} {:from ?x :to ?y} {:b 2} {:b ?v}])

(deftest subsumes-agrees-with-match
  ;; THE property the shadowing refusal rests on. If the analysis says general
  ;; subsumes specific, then no term may match specific and miss general —
  ;; otherwise a branch would be refused as unreachable when it is not, which
  ;; is the refusal being wrong in the direction that blocks an edit.
  (doseq [g corpus-patterns
          s corpus-patterns
          :when (sym/subsumes? g s)
          t corpus-terms
          :when (some? (sym/match (sym/rule {:when s}) t))]
    (is (some? (sym/match (sym/rule {:when g}) t))
        (str (pr-str g) " subsumes " (pr-str s) " but misses " (pr-str t)))))

(deftest overlap-is-whether-one-term-can-match-both
  (is (sym/overlap? '{:a 1} '{:b 2}) "disjoint keys: one map can carry both")
  (is (not (sym/overlap? '{:a 1} '{:a 2})))
  (is (sym/overlap? '{:a ?x} '{:a 2}))
  (is (sym/overlap? '{:a {:b 1}} '{:a {:c 2}}))
  (is (not (sym/overlap? '{:a nil} '{:a {:c 2}})))
  (is (not (sym/overlap? '[?x] '[?x ?y])) "vectors are closed")
  (is (not (sym/overlap? '{:a ?x :b ?x} '{:a 1 :b 2})) "a repeated var must agree")
  (is (sym/overlap? '{:a ?x :b ?x} '{:a 1 :b 1}))
  (is (sym/overlap? '_ '{:a 1}))
  (is (sym/overlap? '{:a ?x} '{:a 1 :b ?x})
      "the same name in two patterns is two different variables"))

(deftest overlap-agrees-with-match
  ;; The other direction: if some term matches both patterns, overlap? must
  ;; say so, or an order-dependent pair would go unreported.
  (doseq [p corpus-patterns
          q corpus-patterns
          t corpus-terms
          :when (and (some? (sym/match (sym/rule {:when p}) t))
                     (some? (sym/match (sym/rule {:when q}) t)))]
    (is (sym/overlap? p q)
        (str (pr-str t) " matches both " (pr-str p) " and " (pr-str q)))))

;;; ------------------------------------------------------------------- facts

(deftest a-query-joins-facts-on-shared-vars
  ;; Facts are [rel & args]; a clause is the same shape with ?vars; a var
  ;; shared between clauses is a join. This is what a guard rule is: the
  ;; symbols a form executes, joined against the symbols that end a process.
  (let [db (sym/facts '[[:executed-symbol System/exit]
                        [:executed-symbol println]
                        [:terminator System/exit]
                        [:terminator .halt]])]
    (is (= '[{?s System/exit}]
           (sym/query db '[[:executed-symbol ?s] [:terminator ?s]])))
    (is (= [] (sym/query db '[[:executed-symbol ?s] [:terminator ?s] [:terminator .exit]]))
        "one clause nothing satisfies empties the whole conjunction")))

(deftest a-clause-may-carry-a-literal-and-a-hole
  (let [db (sym/facts '[[:call 1 spit "src/x.clj"]
                        [:call 2 slurp "src/y.clj"]
                        [:call 3 spit "resources/z.md"]])]
    (is (= '#{{?f 1 ?p "src/x.clj"} {?f 3 ?p "resources/z.md"}}
           (set (sym/query db '[[:call ?f spit ?p]]))))
    (is (= '#{{?p "src/x.clj"} {?p "src/y.clj"} {?p "resources/z.md"}}
           (set (sym/query db '[[:call _ _ ?p]]))))))

(deftest a-query-with-no-vars-answers-whether-the-facts-hold
  (let [db (sym/facts '[[:terminator System/exit]])]
    (is (= [{}] (sym/query db '[[:terminator System/exit]])))
    (is (= [] (sym/query db '[[:terminator println]])))))

(deftest a-fact-rule-fires-once-per-binding-and-a-guard-filters-it
  (let [db (sym/facts '[[:call 1 require] [:keyword-arg 1 :reload]
                        [:call 2 require] [:keyword-arg 2 :verbose]])
        rules (sym/fact-rules '[{:name :reload
                                 :where [[:call ?f require] [:keyword-arg ?f ?k]]
                                 :if [:in ?k #{:reload :reload-all}]}])]
    (is (= '[{:rule :reload :bindings {?f 1 ?k :reload}}] (sym/fire db rules)))
    (is (= [] (sym/fire (sym/facts '[[:call 1 require]]) rules)))))

(deftest a-fact-that-carries-a-set-comes-back-whole
  ;; LOGIC-173 once more, now through pldb: a set as a fact VALUE is walked
  ;; on the way back out, so the IWalkTerm extension is what makes this
  ;; return at all. A form's arguments are arbitrary read data.
  (let [db (sym/facts [[:arg 1 #{:reload}]])]
    (is (= [{'?v #{:reload}}] (sym/query db '[[:arg 1 ?v]])))))

(deftest a-malformed-fact-or-rule-is-refused-at-compile
  (is (thrown? Exception (sym/facts [["not-a-keyword" 1]])))
  (is (thrown? Exception (sym/facts [[:too-wide 1 2 3 4 5]])))
  (is (thrown? Exception (sym/facts [[:too-narrow]])))
  (is (thrown? Exception (sym/fact-rules '[{:name :r :where [[:x ?a]] :if [:> ?b 1]}]))
      "a guard over a var no clause binds")
  (is (thrown? Exception (sym/fact-rules '[{:name :r}])) "a rule with no :where"))
