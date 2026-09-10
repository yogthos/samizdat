;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.procedure
  "The Procedural Graph, as MECHANISM (2609.09153v1, karamazov-ylte.5).

  A directed attributed graph of (procedure, relation, procedure) triplets:
  nodes abstract the agent's own actions, edges say which action is admissible
  after which, and each edge carries condition / guidance / pitfalls. It is
  ADVISORY. The model is told what the neighbourhood says and still chooses —
  §3.2's \"biases the solver's next action without dictating it\".

  THE ALTITUDE, which is the whole reason this is a second graph rather than a
  change to the manifests. `loop.edn` is a graph the ENGINE traverses: its
  nodes are harness stages (assemble, infer, parse, dispatch, settle, arbiter,
  route), the model never picks an edge, and every turn of every task walks the
  same path. This one's nodes are TOOLS and its subject is a task across many
  turns. Localizing on `edit_file` finds nothing in loop.edn, because
  edit_file is not a stage.

  WHY samizdat.symbolic IS THE SUBSTRATE. A triplet is already the shape
  `facts` takes; the h-hop neighbourhood is a join rather than graph-walking
  code; the paper's PrepareCandidate checks are rules. And the conditions stay
  PATTERNS, never gates.edn-style `:when` forms — gates.clj compiles those with
  `eval`, and this is the object the agent rewrites most often, so it is
  exactly the wrong place to inherit host evaluation.

  NO GRAPH SHIPS WITH THIS NAMESPACE, and that is deliberate. The paper
  measures a hand-written prior with no evolution at 58.93 against an unguided
  87.50 on MultiChallenge, and one edited without a validation gate at 53.57 —
  both worse than no prior at all. A graph is worth having only once the gate
  that edits it exists (karamazov-ylte.4). This is the machinery; the graph is
  data, it belongs in resources, and it should be grown by the refiner from
  Start -> End rather than written by hand (Mode 5 beat Mode 4 by 9.3 F1).

  MEASURED COST, so nobody has to guess: a 2-hop localization on a small graph
  is ~4.4 ms, and precompiling the query does not help — the cost is the pldb
  join, not compilation. Negligible per turn against a provider call; NOT
  negligible for a sweep of many rules over many nodes, which is where the
  validation gate has to budget for it."
  (:require [clojure.string :as str]
            [samizdat.agent.tools :as tools]
            [samizdat.symbolic :as sym]))

;;; ------------------------------------------------------------------ facts

(defn facts
  "A graph definition as a symbolic fact database.

  `[:edge from to rel]` and `[:node id type]`. The attributes stay OUT of the
  facts: pldb unifies ground terms and prose is not one, so the edges are
  indexed here and their attributes fetched by endpoint."
  [{:keys [nodes edges]}]
  (sym/facts (concat (for [e edges] [:edge (:from e) (:to e) (:rel e)])
                     (for [[id {:keys [type]}] nodes] [:node id (or type :action)]))))

(defn- edges-from
  [graph u]
  (filterv #(= u (:from %)) (:edges graph)))

(defn neighbourhood
  "The h-hop directed neighbourhood of `u`, as full edge maps with attributes.

  Empty when `u` is not a node. §3.2 falls back to the FULL graph on a failed
  Match; Table 3 measures that fallback scoring BELOW no graph at all on
  ALFWorld (54.48 against 72.58), so the miss is returned as a miss and the
  caller decides. Silently serving the whole graph is the measured-worse
  option."
  ([db u] (neighbourhood db u 2))
  ([db u h]
   (let [graph (:graph (meta db))]
     (if graph
       (loop [frontier (hash-set u)
              seen (hash-set u)
              acc []
              depth 0]
         (if (or (>= depth h) (empty? frontier))
           acc
           (let [es (into [] (mapcat (fn [n] (edges-from graph n))) frontier)
                 nxt (into (hash-set) (comp (map :to) (remove seen)) es)]
             (recur nxt (into seen nxt) (into acc es) (inc depth)))))
       []))))

(defn load-graph
  "A fact db that remembers its definition, so `neighbourhood` can return
  attributes the facts deliberately do not carry."
  [graph]
  (with-meta (facts graph) {:graph graph}))

;;; -------------------------------------------------------- the tool catalogue

(defn tool-catalog
  "The action names a node may use: the tools the loop can actually dispatch.

  tools/tool-names, NOT a gates.edn vocabulary. Measured: probing against the
  union of :shipping / :storm-exempt / :storm-mutating reported :task, :plan
  and :websearch as not-a-tool, and all three are real — those sets are GATE
  vocabularies and deliberately partial. agent_test already walks vocab names
  against this same registry for the same reason."
  []
  (into #{} (map (comp keyword str)) (tools/tool-names)))

;;; ------------------------------------------------------- structural checks

(defn- reaches-terminal
  "Every node with a directed path to a terminal.

  A FIXPOINT, not a fact rule: symbolic's :where is a fixed vector of clauses
  and transitive closure needs recursion, which it has none of. Seed with the
  terminals and add any node with an edge into the set until nothing changes."
  [{:keys [nodes edges]}]
  (let [terminals (into #{} (keep (fn [[id {:keys [type]}]]
                                    (when (= :terminal type) id)))
                        nodes)]
    (loop [ok terminals]
      (let [nxt (into ok (comp (filter #(ok (:to %))) (map :from)) edges)]
        (if (= nxt ok) ok (recur nxt))))))

(defn check
  "Every structural finding about `graph`, as data.

  This is 2609.09153v1's PrepareCandidate plus two checks it does not have.
  Returns {:ok? :traps :dangling :unknown-tools :ambiguous :shadowed}.

  NOT mycelium's validator, and the difference is not cosmetic:
  `validate-reachability!` is BFS FROM :start — every cell reachable — while
  the check a procedural graph needs is a path TO a terminal from every node.
  They are duals and neither is a superset. A node you can enter and never
  leave is a trap, and a trap is what matters for a graph that advises rather
  than executes."
  [{:keys [nodes edges] :as graph} catalog]
  (let [ok (reaches-terminal graph)
        node-ids (set (keys nodes))
        by-source (group-by :from edges)
        ;; symbolic's pattern order, applied to a different table. Two outgoing
        ;; conditions that can both hold means the graph gives two answers at
        ;; one node; a condition subsumed by an EARLIER sibling's never gets
        ;; its own say. The paper's Phi has no notion of either.
        pairs (fn [f] (into [] (mapcat (fn [[_ es]]
                                         (for [[i a] (map-indexed vector es)
                                               [j b] (map-indexed vector es)
                                               :when (< i j)
                                               :let [ca (:condition a) cb (:condition b)]
                                               :when (and ca cb (f ca cb))]
                                           [(:from b) (:to b)])))
                            by-source))]
    (let [findings
          {:traps (vec (sort (remove ok node-ids)))
           :dangling (vec (for [e edges
                                :when (or (not (node-ids (:from e)))
                                          (not (node-ids (:to e))))]
                            [(:from e) (:to e)]))
           :unknown-tools (vec (sort (for [[id {:keys [type]}] nodes
                                           :when (and (= :action (or type :action))
                                                      (seq catalog)
                                                      (not (contains? catalog id)))]
                                       id)))
           ;; ambiguity: both satisfiable, neither ordering them
           :ambiguous (pairs (fn [a b] (and (sym/overlap? a b)
                                            (not (sym/subsumes? a b))
                                            (not (sym/subsumes? b a)))))
           ;; shadowing: the earlier sibling already matches everything
           ;; the later one does
           :shadowed (pairs (fn [a b] (sym/subsumes? a b)))}]
      (assoc findings :ok? (every? empty? (vals findings))))))

(defn render
  "The localized neighbourhood as the text a guidance step reads.

  The paper's B.5 serializer: the active node, then transitions grouped by
  hop with their condition, guidance and pitfalls. Plain text, because the
  reader is a model and the words it reads are policy — the WORDING belongs in
  resources/prompts once this is wired to anything (it is not, yet)."
  [edges u]
  (when (seq edges)
    (str/join "\n"
              (cons (str "Active node: " u)
                    (for [e edges]
                      (str "- " (:from e) " -> " (:to e)
                           (when-let [c (:condition e)] (str " when " (pr-str c)))
                           (when-let [g (:guidance e)] (str "\n    " g))
                           (when-let [p (:pitfalls e)] (str "\n    avoid: " p))))))))
