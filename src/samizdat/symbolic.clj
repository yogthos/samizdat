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
               A mentioned key must be PRESENT: {:k nil} matches a present
               nil and not an absent key, which is where it parts from
               (nil? (:k d)).
    a vector   is CLOSED and POSITIONAL. Length is part of the term's meaning
               — a guard form, an edge pair — so an extra element is a
               different term, not the same term with more detail.
    ?x         is a logic variable. Repeated, it means the same value in both
               places, by unification rather than by a second comparison.
    _          is a hole: it matches anything and binds nothing, and every
               occurrence is its own hole. A symbol and not a keyword, because
               :_ is a keyword like any other — a literal no data map equals,
               in a table whose catch-all would then never fire.
    anything   else is a literal, matched by equality.

  Two relations over patterns come with the language, because a SET of
  patterns can be analysed where a set of closures cannot: `subsumes?` is the
  specificity order (every term one matches, the other matches too) and
  `overlap?` asks whether one term could match both. Dispatch tables are
  checked with them for a branch nothing can reach and for an order that is
  the only thing deciding.

  That is also the whole of the syntactic/semantic rule distinction other
  rewriting systems make explicit: here it falls out of the two collection
  cases, so there is no split to configure.

  A RULE is {:name n :when pattern :if guard :then template}. :when is
  required; the rest are optional, and a rule with no :then is a predicate
  whose firing is itself the finding — which is what verification rules want.

  FACTS are the other half. A fact is [rel & args], {[:call 1 spit] ...}, and
  a fact rule is {:name n :where [[:call ?f ?head] [:kernel-writer ?head]]
  :if guard}: clauses shaped like facts, a ?var shared between clauses being
  a join. Patterns describe ONE term; facts describe a whole thing taken
  apart — the symbols a form executes, the edges of a graph — and a rule over
  them can say which one fired and on what. pldb holds the facts and
  core.logic runs the join; nothing here is a query engine of our own.

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
            [clojure.core.logic.pldb :as pldb]
            [clojure.core.logic.protocols :as lp]
            [clojure.string :as str]))

;;; ------------------------------------------------- making sets terms at all

;; core.logic CANNOT REIFY A SET, and the failure is not a graceful one:
;; walk* (logic.clj:235) hands the term to walk-term, whose Object case is
;; `(f v)`, and f re-enters walk* whenever tree-term? holds — so a set
;; recurses until the stack goes. On the JVM that is a StackOverflowError in
;; Substitutions/walk; under jolt it is an UNBOUNDED HANG, which is worse,
;; because a harness that stops without saying anything looks like a slow
;; provider. This is upstream and open: LOGIC-173, reported 2019, still
;; unfixed in 1.1.1, which is the newest release on Central.
;;
;; The gap is deliberate in origin — "Unification with sets no longer
;; supported: LOGIC-54 through 56" (CHANGES.md, 0.8-alpha1 to alpha2) — but
;; that removal was about UNIFYING sets, which needs an ordering a set does
;; not have. This extends WALKING one, which reification needs and which has
;; no such problem. Unification is untouched: sets still compare as opaque
;; values through the Object path, which is exactly what this pattern
;; language wants from them.
;;
;; Extending a third-party protocol is a global act, so the justification has
;; to be that it cannot break a correct program: there is no core.logic code
;; that depends on reifying a set, because reifying a set does not currently
;; return. We are defining behaviour where there was only a hang. samizdat
;; keeps core.logic confined to this namespace (see the ratchet in
;; symbolic-test), so anything else that reaches for it inherits this by
;; requiring us.
(extend-protocol lp/IWalkTerm
  clojure.lang.IPersistentSet
  (walk-term [v f] (into #{} (map #(lp/walk-term (f %) f)) v)))

;;; ------------------------------------------------------------------ syntax

(defn pvar?
  "Is `x` a pattern variable — a symbol written ?something?"
  [x]
  (and (symbol? x) (str/starts-with? (name x) "?")))

(defn wildcard?
  "Is `x` the hole `_` — matches anything, binds nothing?"
  [x]
  (= '_ x))

(defn- any-node?
  "Does `pred` hold anywhere in `x`, keys included?"
  [pred x]
  (boolean
   (or (pred x)
       (cond
         (map? x) (some #(any-node? pred %) (concat (keys x) (vals x)))
         (coll? x) (some #(any-node? pred %) x)
         :else false))))

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
    (let [vs (cond-> (vec (sort (pattern-vars pattern)))
               (any-node? wildcard? pattern) (conj '_))]
      (when (seq vs)
        ;; The advice has to name the idiom that DOES work, not just the one
        ;; that does not. #{?t} is the natural way to write "any of these", so
        ;; a refusal saying only "use a map or a vector" sends the author
        ;; looking for a structural pattern that cannot exist — membership is
        ;; a guard, not a shape.
        (throw (ex-info (str (if (set? pattern) "set" "list")
                             " pattern matches as a literal, so "
                             (str/join ", " vs)
                             " can never bind"
                             (if (set? pattern)
                               " — bind the set and test it with a guard: {:k ?v} :if [:in <item> ?v]"
                               " — use a vector to match positionally"))
                        {:error :var-in-literal-collection
                         :pattern pattern
                         :vars vs
                         :instead (if (set? pattern)
                                    '{:when {:k ?v} :if [:in <item> ?v]}
                                    '{:when [?a ?b]})}))))

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

    (wildcard? pattern)
    (fn [_ _] l/succeed)

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

  Reading the term rather than reifying the lvars is also what keeps user
  data out of core.logic's reifier entirely. The IWalkTerm extension at the
  top of this namespace means a set would now survive being reified, but the
  main path does not depend on that: it never reifies anything but `true`.
  Two independent reasons the LOGIC-173 hang cannot reach a run — and this
  one costs nothing, since reification would only rebuild structure the
  caller already has."
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

;;; ------------------------------------------------------------ specificity

(defn- skolemize
  "The most general TERM a pattern matches: every ?var becomes one constant
  per var, so a repeated var stays one value, and every _ its own.

  gensym'd symbols rather than fresh objects, so the result is still plain
  data — and a symbol no author writes, so it equals no literal in the
  pattern it is then compared against."
  [pattern]
  (let [table (atom {})]
    (letfn [(sk [x]
              (cond
                (pvar? x) (or (get @table x)
                              (let [c (gensym "skolem")]
                                (swap! table assoc x c)
                                c))
                (wildcard? x) (gensym "skolem")
                (map? x) (into {} (map (fn [[k v]] [k (sk v)])) x)
                (vector? x) (mapv sk x)
                :else x))]
      (sk pattern))))

(defn subsumes?
  "Does every term `specific` matches also match `general`?

  Decided by the matcher itself: the most general term `specific` matches is
  built (skolemize) and `general` is matched against it, so the answer
  cannot drift from what `match` does at run time. A refusal built on this
  needs exactly that — a branch refused as unreachable had better be
  unreachable by the code that dispatches."
  [general specific]
  (some? (match (rule {:when general}) (skolemize specific))))

(defn- rename-vars
  "`f` applied to every ?var in `pattern`, in the positions a var can occupy."
  [pattern f]
  (cond
    (pvar? pattern) (f pattern)
    (map? pattern) (into {} (map (fn [[k v]] [k (rename-vars v f)])) pattern)
    (vector? pattern) (mapv #(rename-vars % f) pattern)
    :else pattern))

(defn- walk-binding [s x]
  (if (and (pvar? x) (contains? s x)) (recur s (get s x)) x))

(defn- unify
  "Bindings under which patterns `a` and `b` describe one term, or nil.

  Maps are open on both sides, so only the keys they share have to agree;
  vectors have to agree in length and position; a hole agrees with anything.
  No occurs check: a cyclic binding would call two patterns compatible that
  no finite term satisfies, which errs toward reporting an overlap — and an
  overlap is reported, never refused."
  [s a b]
  (when s
    (let [a (walk-binding s a)
          b (walk-binding s b)]
      (cond
        (or (wildcard? a) (wildcard? b)) s
        (pvar? a) (if (= a b) s (assoc s a b))
        (pvar? b) (assoc s b a)

        (and (map? a) (map? b))
        (reduce (fn [s k] (or (unify s (get a k) (get b k)) (reduced nil)))
                s
                (filter #(contains? b %) (keys a)))

        (and (vector? a) (vector? b))
        (when (= (count a) (count b))
          (reduce (fn [s i] (or (unify s (nth a i) (nth b i)) (reduced nil)))
                  s
                  (range (count a))))

        :else (when (= a b) s)))))

(defn overlap?
  "Could one term match both `p` and `q`?

  The vars of `q` are renamed apart first: ?x in two patterns is two
  variables, not one shared one."
  [p q]
  (some? (unify {} p (rename-vars q #(symbol "q" (name %))))))

;;; ------------------------------------------------------------------- facts

;; pldb wants one var per relation, defined by a macro at compile time. The
;; relation name is carried instead as the FIRST, indexed argument of one
;; generic relation per arity, so a caller declares nothing and pldb's
;; internals are not touched: [:call 1 spit] is (fact2 :call 1 spit).
(pldb/db-rel fact1 ^:index r a)
(pldb/db-rel fact2 ^:index r a b)
(pldb/db-rel fact3 ^:index r a b c)
(pldb/db-rel fact4 ^:index r a b c d)

(def ^:private fact-rels [nil fact1 fact2 fact3 fact4])

(defn- fact-rel
  "The generic relation for a fact or clause of this shape, or a refusal."
  [tuple]
  (let [[rel & args] (when (sequential? tuple) tuple)
        f (get fact-rels (count args))]
    (when-not (and (keyword? rel) f)
      (throw (ex-info (str "fact is not [keyword & 1..4 args]: " (pr-str tuple))
                      {:error :malformed-fact :fact tuple})))
    f))

(defn facts
  "A fact database from `tuples`, each [rel & args]."
  [tuples]
  (reduce (fn [db [rel & args :as t]]
            (apply pldb/db-fact db (fact-rel t) rel args))
          pldb/empty-db
          tuples))

(defn- clause-goal
  "Compile one clause to (fn [env] goal); env maps ?vars to lvars, and each
  _ is a fresh lvar of its own."
  [clause]
  (let [f (fact-rel clause)
        [rel & args] clause]
    (fn [env]
      (apply f rel (map (fn [a] (cond (pvar? a) (get env a)
                                      (wildcard? a) (l/lvar)
                                      :else a))
                        args)))))

(defn- compile-where
  "{:vars [...] :run (fn [db] bindings)} for a vector of clauses."
  [where]
  (when-not (and (vector? where) (seq where))
    (throw (ex-info (str ":where is not a vector of clauses: " (pr-str where))
                    {:error :malformed-where :where where})))
  (let [vars (vec (sort (distinct (filter pvar? (mapcat rest where)))))
        goals (mapv clause-goal where)]
    {:vars vars
     :run (fn [db]
            (let [env (zipmap vars (map (fn [_] (l/lvar)) vars))]
              (pldb/with-db db
                (->> (l/run* [q]
                       (l/and* (conj (mapv #(% env) goals)
                                     (l/== q (mapv env vars)))))
                     (map #(zipmap vars %))
                     distinct
                     vec))))}))

(defn query
  "Every binding of the ?vars in `where` that the facts in `db` satisfy: a
  vector of maps — [{}] when a var-free where holds, [] when nothing does."
  [db where]
  ((:run (compile-where where)) db))

(defn fact-rules
  "Compile rules {:name n :where [clauses] :if guard} over facts, in order.
  Everything is checked here, as `rule` does for patterns: a guard over a
  var no clause binds is a compile error, not a rule that never fires."
  [rs]
  (mapv (fn [r]
          (when-not (and (map? r) (contains? r :where))
            (throw (ex-info (str "fact rule has no :where: " (pr-str r))
                            {:error :missing-where :rule r})))
          (let [{:keys [vars run]} (compile-where (:where r))]
            {:name (:name r)
             :vars vars
             :run run
             :guard (when (contains? r :if) (guard-fn (:if r) (set vars)))
             :source r}))
        rs))

(defn fire
  "Every rule that fires against `db`, once per binding, in rule order:
  [{:rule name :bindings {?x v}} ...]."
  [db rules]
  (into []
        (for [r rules
              b ((:run r) db)
              :when (or (nil? (:guard r)) ((:guard r) b))]
          {:rule (:name r) :bindings b})))
