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

(ns samizdat.symbolic
  "Patterns and rules over EDN. MECHANISM: pure, no driver, no tools, no
  opinion about what is being matched.

  WHY THIS EXISTS. Four places in the harness decide things with hand-written
  predicates, and mycelium's :dispatches carries `(fn [d] ...)` forms that the
  HOST EVALS — src/maestro/core.clj says so itself, and rests the safety case
  on validate/soak/rollback rather than on any sandbox. A pattern is data.
  There is nothing to eval, and a SET of patterns can be analysed for
  overlapping and unreachable branches, which a set of closures cannot.

  THE PATTERN LANGUAGE, which is the whole contract:

    a map      is an OPEN-WORLD SUBSET. {:phase :implement} matches any map
               carrying that key with that value, whatever else is in it. This
               is the one decision the rest follows from: a workflow data map
               has twenty keys a pattern does not mention, so == would force
               every pattern to enumerate the map and to break whenever a cell
               added a key. core.logic's featurec is exactly this relation.
    a vector   is CLOSED and POSITIONAL. Length is part of the term's meaning
               — a guard form, an edge pair — so an extra element is a
               different term, not the same term with more detail.
    ?x         is a logic variable. Repeated, it means the same value in both
               places, by unification rather than by a second comparison.
    anything   else is a literal, matched by equality.

  That is also the whole of the syntactic/semantic rule distinction other
  rewriting systems make explicit: here it falls out of the two collection
  cases, so there is no split to configure.

  A RULE is {:name n :when pattern :if guard :then template}. :when is
  required; the rest are optional, and a rule with no :then is a predicate
  whose firing is itself the finding — which is what verification rules want.

  GUARDS ARE A CLOSED REGISTRY. A guard is data, [:> ?turn 10], resolved
  against a whitelist. It must NOT reach a resolve or an eval: rebuilding
  arbitrary host evaluation inside the engine would give back exactly what
  this namespace exists to remove. Everything is checked when the rule is
  COMPILED — unknown guard, wrong arity, a ?var that no pattern binds — so a
  bad rule fails loudly at compile instead of silently never firing.

  Regex is deliberately absent from the registry. Rules are agent-authored,
  and a catastrophic backtracking pattern would hang the pass evaluating it;
  :includes?/:starts-with?/:ends-with? cover the cases that have come up.

  COST, measured 2026-08-31 under jolt on an M1 Max. A one-rule match against
  a realistic workflow data map is ~320us; a three-rule first-match is ~780us.
  Nearly all of it is featurec — about 160us per constrained key — and it is
  FLAT in the size of the map being matched (321us against a 2-key map, 326us
  against a 10-key one), so it is constraint setup and not scanning. A bare
  core.logic run costs 1.4us, so the logic engine itself is not the expense.
  Against a turn that makes a provider call this is nothing, and dispatch
  makes a handful of these per turn. A sweep that matches many rules over many
  nodes is where it would be felt: 100 nodes x 20 rules is ~0.6s. Worth
  knowing that matching needs no SEARCH — patterns and terms are both ground,
  so there is nothing to backtrack — which is why the constraint machinery is
  overhead here and earns its keep in the satisfiability rounds instead.

  PROVENANCE. The ideas here are standard term rewriting and long predate any
  one implementation: a rule as pattern/guard/result, ?-prefixed pattern
  variables, and rewriting to a fixpoint go back through core.match, Datalog,
  Stratego, Maude, Mathematica's /. and Prolog. The heavy lifting is
  core.logic's: featurec for partial-map unification, unification for repeated
  variables. /Users/yogthos/src/numeric-logic (expresso) was read as
  algorithmic inspiration only, and none of it is copied — it carries no
  license and this is GPL-3.0-or-later. Its two habits are deliberately NOT
  followed: guards do not macro-expand host code, and there is no pattern
  protocol of our own, because core.logic's IUnifyTerms already is one."
  (:require [clojure.core.logic :as l]
            [clojure.string :as str]))

;;; ------------------------------------------------------------------ syntax

(defn pvar?
  "Is `x` a pattern variable — a symbol written ?something?"
  [x]
  (and (symbol? x) (str/starts-with? (name x) "?")))

(defn- pattern-vars
  "Every ?var in a pattern or template, KEYS INCLUDED.

  Keys matter for templates: `:then {?k 2}` substitutes into key position, so
  an unbound ?k there would otherwise produce a map with a nil key instead of
  a compile error. In a :when pattern a ?var key is rejected outright by
  check-pattern!, so walking keys costs nothing there."
  [x]
  (cond
    (pvar? x) #{x}
    (map? x) (into #{} (mapcat pattern-vars) (concat (keys x) (vals x)))
    (or (vector? x) (set? x) (seq? x)) (into #{} (mapcat pattern-vars) x)
    :else #{}))

(defn- check-pattern!
  "Reject the two shapes that would compile to a rule which silently never
  fires — the worst way for a rule to be wrong, because nothing reports it.

  A map pattern's keys must be literal: featurec relates a map to KNOWN keys,
  so a ?var in key position cannot be solved for.

  Sets and lists match as literals (only maps and vectors have structural
  cases above), so a ?var inside one is never bound by matching. It would be
  collected as one of the rule's variables and then come back unbound."
  [pattern]
  (cond
    (map? pattern)
    (do (doseq [k (keys pattern)]
          (when (pvar? k)
            (throw (ex-info (str "map pattern key is a variable: " k
                                 " — featurec matches known keys")
                            {:error :var-in-key-position
                             :key k :pattern pattern}))))
        (run! check-pattern! (vals pattern)))

    (vector? pattern) (run! check-pattern! pattern)

    (or (set? pattern) (seq? pattern))
    (let [vs (pattern-vars pattern)]
      (when (seq vs)
        (throw (ex-info (str (if (set? pattern) "set" "list")
                             " pattern matches as a literal, so "
                             (str/join ", " (sort vs))
                             " can never bind — use a map or a vector")
                        {:error :var-in-literal-collection
                         :pattern pattern :vars (vec (sort vs))}))))

    :else nil))

;;; ------------------------------------------------- pattern -> core.logic goal

(defn- pattern-goal
  "Compile `pattern` to (fn [env term] goal), where env maps ?vars to lvars.

  env is taken at CALL time rather than baked in, because each match needs its
  own fresh lvars while the compiled structure is reused across matches."
  [pattern]
  (cond
    (pvar? pattern)
    (fn [env t] (l/== t (get env pattern)))

    (map? pattern)
    (let [ks (vec (keys pattern))
          subs (mapv #(pattern-goal (get pattern %)) ks)]
      (fn [env t]
        ;; featurec is the open-world half; recursing into a fresh lvar per key
        ;; keeps every nested level open-world too, rather than trusting one
        ;; featurec to describe a whole tree.
        (let [vs (mapv (fn [_] (l/lvar)) ks)]
          (l/and* (cons (l/featurec t (zipmap ks vs))
                        (map (fn [g v] (g env v)) subs vs))))))

    (vector? pattern)
    (let [subs (mapv pattern-goal pattern)]
      (fn [env t]
        ;; unifying against a vector of exactly n lvars is what makes this
        ;; closed: a term of any other length simply does not unify.
        (let [vs (mapv (fn [_] (l/lvar)) subs)]
          (l/and* (cons (l/== t vs)
                        (map (fn [g v] (g env v)) subs vs))))))

    :else
    (fn [_ t] (l/== t pattern))))

;;; ------------------------------------------------------------------ guards

(defn- member-of?
  "Membership, meaning the same thing for every collection kind: sets and maps
  answer by key, everything else by scanning. clojure.core/contains? would
  answer by INDEX for a vector, which is never what a rule means."
  [v coll]
  (cond
    (set? coll) (contains? coll v)
    (map? coll) (contains? coll v)
    (nil? coll) false
    :else (boolean (some #(= v %) (seq coll)))))

(def ^:private registry
  "The closed whitelist. `:fn` is applied to already-bound values."
  {:=            {:arity 2 :fn =            :doc "the two values are equal"}
   :not=         {:arity 2 :fn not=         :doc "the two values differ"}
   :>            {:arity 2 :fn >            :doc "numerically greater than"}
   :>=           {:arity 2 :fn >=           :doc "numerically greater or equal"}
   :<            {:arity 2 :fn <            :doc "numerically less than"}
   :<=           {:arity 2 :fn <=           :doc "numerically less or equal"}
   :in           {:arity 2 :fn member-of?   :doc "the value is a member of the collection"}
   :empty?       {:arity 1 :fn empty?       :doc "the collection or string is empty"}
   :nil?         {:arity 1 :fn nil?         :doc "the value is nil"}
   :some?        {:arity 1 :fn some?        :doc "the value is not nil"}
   :true?        {:arity 1 :fn true?        :doc "the value is exactly true"}
   :false?       {:arity 1 :fn false?       :doc "the value is exactly false"}
   :count=       {:arity 2 :fn (fn [c n] (= (count c) n)) :doc "the collection has exactly n elements"}
   :starts-with? {:arity 2 :fn (fn [s p] (str/starts-with? (str s) (str p))) :doc "the string starts with the prefix"}
   :ends-with?   {:arity 2 :fn (fn [s p] (str/ends-with? (str s) (str p)))   :doc "the string ends with the suffix"}
   :includes?    {:arity 2 :fn (fn [s p] (str/includes? (str s) (str p)))    :doc "the string contains the substring"}})

(def ^:private composites
  "Structural guards, handled by the compiler rather than by application."
  {:and {:arity :n :doc "every sub-guard holds"}
   :or  {:arity :n :doc "at least one sub-guard holds"}
   :not {:arity 1  :doc "the sub-guard does not hold"}})

(defn guard-catalog
  "Every guard a rule may name, with its arity and what it does.

  Discoverable on purpose: a supervisor authoring a rule has to be able to ask
  what exists without reading this file, and a capability it cannot enumerate
  is one it does not have."
  []
  (into (sorted-map)
        (map (fn [[k v]] [k (select-keys v [:arity :doc])]))
        (merge registry composites)))

(defn- guard-fn
  "Compile a guard form to (fn [bindings] boolean).

  Throws on an unknown name, the wrong arity, or a ?var the pattern does not
  bind. That last one matters most: an unbound ?var would compare against
  nothing at runtime and the rule would simply never fire, which is the
  hardest kind of wrong rule to notice."
  [form bound]
  (when-not (vector? form)
    (throw (ex-info (str "guard is not a vector: " (pr-str form))
                    {:error :malformed-guard :guard form})))
  (let [[op & args] form
        args (vec args)]
    (case op
      :and (let [fs (mapv #(guard-fn % bound) args)] (fn [b] (every? #(% b) fs)))
      :or  (let [fs (mapv #(guard-fn % bound) args)] (fn [b] (boolean (some #(% b) fs))))
      :not (do (when-not (= 1 (count args))
                 (throw (ex-info ":not takes one sub-guard"
                                 {:error :guard-arity :guard form
                                  :arity 1 :given (count args)})))
               (let [f (guard-fn (first args) bound)] (fn [b] (not (f b)))))
      (let [spec (get registry op)]
        (when-not spec
          (throw (ex-info (str "unknown guard: " (pr-str op))
                          {:error :unknown-guard
                           :guard op
                           :known (vec (sort (keys (guard-catalog))))})))
        (when-not (= (:arity spec) (count args))
          (throw (ex-info (str "guard " op " arity: wanted " (:arity spec)
                               ", got " (count args))
                          {:error :guard-arity :guard op
                           :arity (:arity spec) :given (count args)})))
        (doseq [a args]
          (when (and (pvar? a) (not (contains? bound a)))
            (throw (ex-info (str "guard " op " uses unbound " a)
                            {:error :unbound-var :guard op
                             :unbound a :bound (vec (sort bound))}))))
        (let [f (:fn spec)]
          (fn [b]
            (let [vs (map #(if (pvar? %) (get b %) %) args)]
              ;; Rules run over agent-authored data, so a guard meeting a type
              ;; it did not expect must REJECT rather than take down the pass
              ;; evaluating it. The registry holds only small total-ish
              ;; predicates, so the only realistic throw is a type error.
              (boolean (try (apply f vs) (catch Throwable _ false))))))))))

;;; ---------------------------------------------------------- substitution

(defn- substitute
  "Fill a :then template in from `bindings`. Structure is preserved; a ?var is
  replaced by its value, including when that value is nil or false."
  [template bindings]
  (cond
    (pvar? template) (get bindings template)
    (map? template) (into {} (map (fn [[k v]] [(substitute k bindings)
                                               (substitute v bindings)]))
                          template)
    (vector? template) (mapv #(substitute % bindings) template)
    (set? template) (into #{} (map #(substitute % bindings)) template)
    (seq? template) (map #(substitute % bindings) template)
    :else template))

;;; ------------------------------------------------------------------- rules

(defn rule
  "Compile one rule. Every error a rule can have is raised HERE rather than
  producing a rule that quietly never matches."
  [r]
  (when-not (map? r)
    (throw (ex-info (str "rule is not a map: " (pr-str r))
                    {:error :malformed-rule :rule r})))
  (when-not (contains? r :when)
    (throw (ex-info "rule has no :when"
                    {:error :missing-when :rule r})))
  (let [pattern (:when r)
        _ (check-pattern! pattern)
        vars (vec (sort (pattern-vars pattern)))
        bound (set vars)]
    (when (contains? r :then)
      (let [loose (remove bound (pattern-vars (:then r)))]
        (when (seq loose)
          (throw (ex-info (str ":then uses unbound " (str/join ", " (sort loose)))
                          {:error :unbound-var :rule (:name r)
                           :unbound (vec (sort loose)) :bound vars})))))
    {:name (:name r)
     :vars vars
     :when-pattern pattern
     :pattern (pattern-goal pattern)
     :guard (when (contains? r :if) (guard-fn (:if r) bound))
     :then (:then r)
     :has-then (contains? r :then)
     :source r}))

(defn ruleset
  "Compile a sequence of rules, in order. Order is meaning for first-match."
  [rs]
  (mapv rule rs))

(defn- extract
  "Read each ?var's value straight out of the ORIGINAL term.

  Only ever called after the goal has already succeeded, so it decides
  nothing: the shapes are known to line up and this just projects. Repeated
  vars write the same value twice, unification having already required them
  to be equal.

  This exists because core.logic CANNOT REIFY A SET. walk* calls walk-term,
  whose Object case re-enters walk*, and IWalkTerm has no set case — so a set
  recurses forever. On the JVM that is a StackOverflowError in
  Substitutions/walk; under jolt it is an unbounded hang, which is worse.
  Verified against core.logic 1.1.1 on both. Unification with a set is fine
  (sets are compared as opaque literals, which is exactly what this pattern
  language wants), so the fix is to never put user data in reified position:
  the goal answers yes or no, and the bindings come from here. See
  karamazov-41a."
  [pattern term acc]
  (cond
    (pvar? pattern) (assoc acc pattern term)

    (map? pattern)
    (if (map? term)
      (reduce (fn [a k] (extract (get pattern k) (get term k) a)) acc (keys pattern))
      acc)

    (vector? pattern)
    (if (and (sequential? term) (= (count pattern) (count term)))
      (let [tv (vec term)]
        (reduce (fn [a i] (extract (nth pattern i) (nth tv i) a)) acc
                (range (count pattern))))
      acc)

    :else acc))

(defn match
  "Bindings for `term` under a compiled rule, or nil if it does not match.

  An empty map is a SUCCESSFUL match that bound nothing — patterns with no
  ?vars are ordinary — so callers must test for nil, not for emptiness.

  The run reifies only `true`: see `extract` for why no user value may reach
  core.logic's reifier.

  The guard runs on the bindings in plain Clojure rather than as a goal: a
  guard is a deterministic test over values the pattern has already ground,
  so projecting it into the logic engine would buy nothing."
  [compiled term]
  (let [vars (:vars compiled)
        env (zipmap vars (map (fn [_] (l/lvar)) vars))
        matched? (seq (l/run 1 [q]
                        ((:pattern compiled) env term)
                        (l/== q true)))]
    (when matched?
      (let [b (extract (:when-pattern compiled) term {})
            g (:guard compiled)]
        (when (or (nil? g) (g b)) b)))))

(defn- hit [compiled bindings]
  {:rule (:name compiled)
   :bindings bindings
   :result (when (:has-then compiled) (substitute (:then compiled) bindings))})

(defn first-match
  "The first rule that matches, as {:rule :bindings :result}, or nil.

  First-match-wins, so a ruleset orders specific rules before general ones."
  [rules term]
  (some (fn [c] (when-let [b (match c term)] (hit c b))) rules))

(defn all-matches
  "Every rule that matches, in ruleset order."
  [rules term]
  (into [] (keep (fn [c] (when-let [b (match c term)] (hit c b)))) rules))

(def default-rewrite-limit 100)

(defn rewrite
  "Apply `rules` to `term` repeatedly until it stops changing.

  Terminates on a FIXPOINT — the term stopped changing — and not on \"no rule
  matched\", because a rule may legitimately rewrite a term to itself and
  would otherwise fire forever.

  Rewriting is TOP-LEVEL: subterms are not descended into. Nothing that uses
  this needs algebraic simplification, and traversal is a strategy that can be
  added over this without changing the engine.

  A cycle between rules changes the term every step and so never reaches a
  fixpoint. The agent can author one, so the limit is a hard bound and hitting
  it throws, naming the rules that were doing the looping."
  ([rules term] (rewrite rules term default-rewrite-limit))
  ([rules term limit]
   (loop [t term n 0 fired []]
     (if-let [h (first-match rules t)]
       (let [t' (:result h)]
         (cond
           (= t' t) t
           (>= n limit) (throw (ex-info (str "no fixpoint in " limit
                                             " steps; rules cycle")
                                        {:error :rewrite-cycle
                                         :limit limit :term term
                                         :rules (vec (distinct (conj fired (:rule h))))}))
           :else (recur t' (inc n) (conj fired (:rule h)))))
       t))))
