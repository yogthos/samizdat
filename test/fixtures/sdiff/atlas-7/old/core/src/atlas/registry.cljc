(ns atlas.registry
  "Semantic kernel providing compound-identity algebra.

   Each identity is a set of qualified keywords representing composed meaning.

   Registration model (like clojure.spec):
   - register! appends to an ordered log and keeps the map warm
   - compile! rebuilds the map from the log with aggregated compound-ids
   - check-consistency! validates the compiled registry

   Pipeline: load → compile! → check-consistency!"
  (:refer-clojure :exclude [exists? remove])
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.spec.alpha :as s]
            [taoensso.telemere :as tel]
            #?(:cljs [goog.string :as gstring])))

;; =============================================================================
;; Registry — dual storage: log (source of truth) + map (query index)
;; =============================================================================

(def registrations
  "Ordered log of all register! calls. Source of truth.
   Each entry: {:dev-id kw :type kw :aspects #{} :value {} :ns str}"
  (atom []))

(def registry
  "Derived map: compound-id → props. Rebuilt from log by compile!.
   After compile!, compound-ids include aggregated aspects from deps.
   Also kept warm by register! (with declared aspects only)."
  (atom {}))

(def dev-id-index
  "Derived index: dev-id → compound-id. O(1) lookup.
   Rebuilt by compile!. Also kept warm by register!."
  (atom {}))

(def ^:dynamic *registry-override*
  "When bound, `current-registry` returns this value instead of @registry.
   Used by the cloud MCP layer to run tools against a pulled version snapshot."
  nil)

(defn current-registry
  "Return the active registry map — either the dynamic override or the live atom."
  []
  (or *registry-override* @registry))

;; =============================================================================
;; Helpers
;; =============================================================================

#?(:clj (defn- format* [fmt & args]
          (apply format fmt args))
   :cljs (defn- format* [fmt & args]
           (apply gstring/format fmt args)))

(defn fetch
  "Fetch the value associated with a compound identity."
  [identity]
  (get @registry identity))

(defn exists?
  "Check if a compound identity exists in the registry and whether its value matches `value`.
   Returns {:exists bool :matches bool}."
  [compound-identity value]
  (let [found (get @registry compound-identity)]
    {:exists (contains? @registry compound-identity)
     :matches (= found value)}))

(defn remove
  "Remove a compound identity from the registry."
  [compound-identity]
  (swap! registry dissoc compound-identity))

(defn generate-dev-id
  "Generate a deterministic dev-id from a compound identity (aspect set).
   Sorts aspects alphabetically and creates a keyword in the 'auto' namespace."
  [aspect-set]
  (let [sorted-aspects (sort aspect-set)
        aspect-names (map (fn [kw]
                            (str (namespace kw) "--" (name kw)))
                          sorted-aspects)
        joined (str/join "--" aspect-names)]
    (keyword "auto" joined)))

;; =============================================================================
;; Registration — append to log + keep map warm
;; =============================================================================

(defn register!
  "Register a new compound identity with explicit entity type and aspects.

   Appends to the registration log and updates the map immediately
   (with declared aspects only). Call compile! after all registrations
   to rebuild with aggregated compound-ids.

   Arities:
   - [type aspects value]: Register with auto-generated dev-id
   - [dev-id type aspects value]: Register with explicit dev-id"
  ([type aspects value]
   (register! (generate-dev-id (conj aspects type)) type aspects value))
  ([dev-id type aspects value]
   (assert (qualified-keyword? type)
           (format* "Entity type must be a qualified keyword, got: %s" type))
   (assert (and (set? aspects) (every? qualified-keyword? aspects))
           (format* "Aspects must be a set of qualified kws got: %s" aspects))
   (let [compound-id (conj aspects type)
         value-with-meta (assoc value :atlas/dev-id dev-id :atlas/type type)]
     ;; Append to registration log
     (swap! registrations conj {:dev-id  dev-id
                                :type    type
                                :aspects aspects
                                :value   value
                                :ns      #?(:clj (str *ns*) :cljs nil)})
     ;; Keep map + index warm for backward compatibility
     (swap! dev-id-index assoc dev-id compound-id)
     (swap! registry assoc compound-id value-with-meta)
     compound-id)))

;; =============================================================================
;; Aggregation — pure functions over maps (used by compile!)
;; =============================================================================

(defn- resolve-refs
  "Get referenced dev-ids (keywords) from an entity's props for a given type-ref property.
   Filters to qualified keywords only."
  [props {:keys [property cardinality]}]
  (let [value (get props property)]
    (cond
      (nil? value) #{}
      (= cardinality :db.cardinality/many)
      (cond
        (set? value)        (into #{} (filter qualified-keyword?) value)
        (sequential? value) (into #{} (filter qualified-keyword?) value)
        (qualified-keyword? value) #{value}
        :else #{})
      :else
      (if (qualified-keyword? value) #{value} #{}))))

(defn- discover-type-refs
  "Read type-ref declarations from a base registry map (not the live atom).
   Returns a seq of {:source kw :property kw :cardinality kw :datalog-verb kw}."
  [base-map]
  (->> (vals base-map)
       (filter #(= :atlas/type-ref (:atlas/type %)))
       (keep (fn [props]
               (when (:type-ref/property props)
                 {:source       (:type-ref/source props)
                  :property     (:type-ref/property props)
                  :cardinality  (:type-ref/cardinality props)
                  :datalog-verb (:type-ref/datalog-verb props)})))
       vec))

(defn- discover-intrinsic-aspects
  "Union of every ontology's :ontology/intrinsic-aspects — the aspects that are
   properties of an entity itself and must never be inherited through any edge.
   Derived from the registry, so downstream ontologies declare their own; core
   never hardcodes the set."
  [base-map]
  (->> (vals base-map)
       (filter #(= :atlas/ontology (:atlas/type %)))
       (mapcat :ontology/intrinsic-aspects)
       (into #{})))

(defn- build-dep-graph
  "Build {dev-id -> #{dep-dev-ids}} by walking type-ref properties for each
   entity. Only includes dep-ids that exist in dev-id->entry (known entities).

   ALL type-refs contribute, regardless of :datalog-verb. Aspect propagation is
   filtered by which ASPECTS are intrinsic (see discover-intrinsic-aspects), not
   by which EDGE carried them — see :ontology/intrinsic-aspects for why the
   edge-based cut was wrong."
  [dev-id->entry base-map ref-properties]
  (reduce (fn [graph [dev-id {:keys [type aspects]}]]
            (let [cid   (conj aspects type)
                  props (get base-map cid)
                  deps  (into #{}
                              (comp (mapcat #(resolve-refs props %))
                                    (filter #(contains? dev-id->entry %)))
                              (filter #(contains? cid (:source %)) ref-properties))]
              (assoc graph dev-id deps)))
          {}
          dev-id->entry))

(defn- topo-sort*
  "Kahn's algorithm on {node -> #{deps}} graph.
   Returns nodes in dependency-first order (leaves before roots).
   Nodes referenced as deps but absent from graph keys are included."
  [graph]
  (let [all-nodes  (into #{} (concat (keys graph) (mapcat val graph)))
        in-degree  (into {} (map (fn [n] [n (count (get graph n #{}))]) all-nodes))
        rev-graph  (reduce (fn [acc [node deps]]
                             (reduce (fn [a d] (update a d (fnil conj #{}) node)) acc deps))
                           {}
                           graph)
        init-queue (filterv #(zero? (get in-degree %)) all-nodes)]
    (loop [queue     init-queue
           result    []
           in-degree in-degree]
      (if (empty? queue)
        result
        (let [node  (first queue)
              [in-deg' q']
              (reduce (fn [[indeg q] dep]
                        (let [new-deg (dec (get indeg dep 0))]
                          [(assoc indeg dep new-deg)
                           (if (zero? new-deg) (conj q dep) q)]))
                      [in-degree (subvec queue 1)]
                      (get rev-graph node #{}))]
          (recur q' (conj result node) in-deg'))))))

;; =============================================================================
;; Compile — rebuild map from log with aggregated compound-ids
;; =============================================================================

(defn compile!
  "Rebuild the registry map from the registration log.

   Two passes:
   1. Build base map from log (last-write-wins by dev-id, no orphans)
   2. Walk deps via type-refs to expand compound-ids with derived aspects

   After compile!, compound-ids include all aspects inherited from deps.
   The log preserves declared aspects — use declared-aspects to read them.

   Returns a summary map. Two DIFFERENT kinds of collapse are reported:

     :compile/shadowed   log entries superseded by a later registration of the
                         SAME dev-id. This is the normal dev loop — re-evaluating
                         a file re-registers its entities and the latest wins.
                         Expected and healthy; not a problem.

     :compile/collisions two DIFFERENT dev-ids that compiled to the SAME
                         compound-id. One silently shadows the other, and the
                         loser is unaddressable. ALWAYS a modelling error.

   Why collisions are detected *here* and not in register!: register! is
   append-only and per-call, so it cannot tell a re-eval from a collision — and
   it must stay that way, or every REPL reload would throw. By this point the
   log is already deduped by dev-id, so any compound-id claimed by more than
   one dev-id is a genuine collision by construction. It is also the last
   moment it is knowable: afterwards the shadowed entity is simply absent from
   the registry, so no invariant can ever see it.

   Options:
     :strict? — throw on collisions instead of warning (for CI)."
  ([] (compile! {}))
  ([{:keys [strict?]}]
   (let [entries @registrations
         ;; Last-write-wins by dev-id — THE DEV LOOP. Re-evaluating a file
         ;; re-registers its entities; the latest definition of each dev-id
         ;; wins. Anything collapsed here is a re-eval, never a collision.
         dev-id->entry (reduce (fn [acc {:keys [dev-id] :as entry}]
                                 (assoc acc dev-id entry))
                               {}
                               entries)

         ;; DEV-ID CONFLICTS — the mirror of the compound-id collision check
         ;; below. Atlas identity is a DOUBLE system: compound-id (what an
         ;; entity means) and dev-id (how you address it). Two entities sharing
         ;; a compound-id is a collision; two sharing a dev-id is a conflict.
         ;; Both destroy addressing, so both are reported.
         ;;
         ;; A dev-id re-registered with the SAME compound-id is the REPL
         ;; re-evaluation loop and stays silent — that is what last-write-wins
         ;; above exists for. Only a dev-id claiming DIFFERENT compound-ids is
         ;; reported: one address, two meanings, earlier meaning gone.
         ;;
         ;; Advisory, not an error: re-evaluating a file after EDITING an
         ;; entity's aspects produces exactly this shape, and last-write-wins is
         ;; then correct. Edit and duplicate are structurally indistinguishable
         ;; from the log alone, so this warns and lets the author judge.
         ;; (A real one: `register-entity-types!` registered its type marker
         ;; under the ontology's own dev-id, silently destroying the ontology
         ;; entity — and with it every mechanism declared on it.)
         dev-id-conflicts
         (->> entries
              (group-by :dev-id)
              (keep (fn [[dev-id es]]
                      (let [cids (distinct (map #(conj (:aspects %) (:type %)) es))]
                        (when (next cids)
                          {:dev-id dev-id
                           :claimed-compound-ids (vec cids)
                           :winner (last cids)}))))
              vec)

         ;; Pass 1: base map (declared compound-ids only)
         base-map (into {}
                        (map (fn [[_ {:keys [dev-id type aspects value]}]]
                               [(conj aspects type)
                                (assoc value :atlas/dev-id dev-id :atlas/type type)]))
                        dev-id->entry)

         ;; Discover type-refs, build dep graph, topo-sort
         ref-properties (discover-type-refs base-map)
         intrinsic      (discover-intrinsic-aspects base-map)
         dep-graph      (build-dep-graph dev-id->entry base-map ref-properties)
         topo-order     (topo-sort* dep-graph)

         ;; Pass 2: walk bottom-to-top — each node gets
         ;;   own ∪ (deps' all-aspects MINUS the intrinsic ones)
         ;;
         ;; An entity always keeps its OWN intrinsic aspects; it just never
         ;; acquires anyone else's. That is what makes a mutually-exclusive
         ;; enum (node-type) and a per-entity lifecycle (:status/*) survive
         ;; expansion, while characteristic aspects still propagate.
         all-aspects-by-id
         (reduce (fn [acc dev-id]
                   (let [own       (get-in dev-id->entry [dev-id :aspects] #{})
                         dep-ids   (get dep-graph dev-id #{})
                         ;; set/difference, not (remove intrinsic) — this ns
                         ;; defines its own `remove` (registry removal), which
                         ;; shadows clojure.core/remove.
                         inherited (set/difference
                                    (apply set/union (map #(get acc % #{}) dep-ids))
                                    intrinsic)]
                     (assoc acc dev-id (into own inherited))))
                 {}
                 topo-order)

         ;; Keep the pairs before `into {}` collapses them — a duplicate
         ;; compound-id is invisible once it has landed in the map.
         expanded-pairs
         (mapv (fn [[dev-id {:keys [type aspects value]}]]
                 (let [all-asp (get all-aspects-by-id dev-id aspects)
                       cid     (conj all-asp type)]
                   [cid (assoc value :atlas/dev-id dev-id :atlas/type type)]))
               dev-id->entry)

         expanded-map (into {} expanded-pairs)

         ;; COLLISIONS — distinct dev-ids compiling to one compound-id.
         ;; `into` keeps the last pair, so the last dev-id wins and the rest are
         ;; silently unaddressable.
         ;;
         ;; Checked on the EXPANDED cid because that is what becomes @registry,
         ;; and it catches a strict superset: a collision can also be INDUCED by
         ;; expansion, when two entities with different declared aspects inherit
         ;; their way onto the same expanded cid — invisible in base-map.
         ;;
         ;; Note a declared-cid collision can never be rescued by expansion:
         ;; build-dep-graph reads `(get base-map cid)` by DECLARED cid, so both
         ;; colliding dev-ids read the WINNER's props, resolve the winner's deps
         ;; and inherit identical aspects. The loser silently adopts the
         ;; winner's dependencies before disappearing — so a declared collision
         ;; corrupts dep resolution, not just entity count.
         collisions
         (->> expanded-pairs
              (group-by first)
              (keep (fn [[cid pairs]]
                      (let [dev-ids (mapv #(:atlas/dev-id (second %)) pairs)]
                        (when (next dev-ids)
                          {:compound-id cid
                           :claimed-by  dev-ids
                           :winner      (peek dev-ids)
                           :shadowed    (vec (butlast dev-ids))}))))
              vec)

         ;; Build dev-id → expanded compound-id index
         idx (into {}
                   (map (fn [[cid props]] [(:atlas/dev-id props) cid]))
                   expanded-map)

         aggregated (count (filter (fn [[dev-id all-asp]]
                                     (> (count all-asp)
                                        (count (get-in dev-id->entry [dev-id :aspects] #{}))))
                                   all-aspects-by-id))]
     ;; Diagnostics ride along as METADATA rather than being printed. The
     ;; registry value carries its own account of what was lost building it —
     ;; `(meta @registry)` — so a caller can query it, a test can assert on it,
     ;; and the UI can surface it, without compile! spraying stdout on every
     ;; REPL reload. (:compile/dev-id-conflicts is advisory and lives ONLY here;
     ;; an aspect edit followed by a re-eval produces the same shape as a real
     ;; duplicate, so it is never printed and never thrown.)
     (reset! registry (with-meta expanded-map
                        {:compile/collisions       collisions
                         :compile/dev-id-conflicts dev-id-conflicts}))
     (reset! dev-id-index idx)
     ;; A collision is always a modelling error: the shadowed entity is absent
     ;; from the registry, so nothing downstream can ever see it. This is the
     ;; last moment it is knowable — say so loudly rather than silently.
     (when (seq collisions)
       (let [msg (str "atlas: COMPOUND-ID COLLISION — "
                      (count collisions)
                      " compound-id(s) claimed by more than one dev-id."
                      " The shadowed entities are NOT in the registry.\n"
                      (str/join "\n"
                                (map (fn [{:keys [compound-id winner shadowed]}]
                                       (str "  " (pr-str compound-id)
                                            "\n    kept:     " winner
                                            "\n    shadowed: " (pr-str shadowed)))
                                     collisions)))]
         (if strict?
           (throw (ex-info msg {:collisions collisions}))
           (println msg))))
     {:compile/entities         (count dev-id->entry)
      :compile/from-entries     (count entries)
      :compile/shadowed         (- (count entries) (count dev-id->entry))
      :compile/collisions       collisions
      :compile/dev-id-conflicts dev-id-conflicts
      :compile/aggregated       aggregated
      :compile/type-refs        (mapv :property ref-properties)})))

;; =============================================================================
;; Log queries — read declared vs compiled aspects
;; =============================================================================

(defn declared-aspects
  "Get the declared aspects for a dev-id (from the log, before aggregation)."
  [dev-id]
  (let [entries @registrations]
    (:aspects (last (filter #(= dev-id (:dev-id %)) entries)))))

(defn derived-aspects
  "Get aspects that were derived by aggregation (compiled minus declared)."
  [dev-id]
  (when-let [compiled-cid (get @dev-id-index dev-id)]
    (let [declared (or (declared-aspects dev-id) #{})
          entry (last (filter #(= dev-id (:dev-id %)) @registrations))
          type (:type entry)]
      (disj (set/difference compiled-cid declared) type))))

;; =============================================================================
;; Consistency checking — post-load validation
;; =============================================================================

(defn check-consistency!
  "Validate the registry for consistency after compile!.

   Checks:
   1. Duplicate dev-ids in the log (same dev-id registered more than once)
   2. Compound-id collisions (different dev-ids producing the same compound-id)

   Returns {:consistent? bool :issues [...]}."
  []
  (let [entries @registrations
        compiled-registry @registry

        ;; 1. Duplicate dev-ids
        dev-id-entries (group-by :dev-id entries)
        duplicates (->> dev-id-entries
                        (filter (fn [[_ es]] (> (count es) 1)))
                        (mapv (fn [[dev-id es]]
                                {:issue    :duplicate-dev-id
                                 :dev-id   dev-id
                                 :count    (count es)
                                 :sources  (mapv :ns es)})))

        ;; 2. Compound-id collisions (different dev-ids → same expanded compound-id)
        cid-to-devids (reduce (fn [acc [cid props]]
                                (update acc cid (fnil conj #{}) (:atlas/dev-id props)))
                              {}
                              compiled-registry)
        collisions (->> cid-to-devids
                        (filter (fn [[_ dev-ids]] (> (count dev-ids) 1)))
                        (mapv (fn [[cid dev-ids]]
                                {:issue       :compound-id-collision
                                 :compound-id cid
                                 :dev-ids     (vec (sort dev-ids))})))

        issues (into duplicates collisions)]
    {:consistent? (empty? issues)
     :issues      issues
     :entities    (count compiled-registry)}))

;; =============================================================================
;; Reset — clear everything (tests, dev)
;; =============================================================================

(defn reset-all!
  "Clear registrations log, registry map, and dev-id index.
   Use in test fixtures and for full dev reset."
  []
  (reset! registrations [])
  (reset! registry {})
  (reset! dev-id-index {}))

(defn unregister-ns!
  "Remove all registrations contributed by ns-sym from the log and registry.
   Called from before-ns-unload hooks so incremental reloads keep @registrations
   accurate without needing a full reset-all!."
  [ns-sym]
  (let [ns-str     (str ns-sym)
        removed-ids (into #{} (comp (filter #(= ns-str (:ns %))) (map :dev-id)) @registrations)]
    (swap! registrations #(vec (clojure.core/remove (comp #{ns-str} :ns) %)))
    (swap! registry      #(apply dissoc % (map @dev-id-index removed-ids)))
    (swap! dev-id-index  #(apply dissoc % removed-ids))))

#?(:clj
   (defn load!
     "Require each namespace in ns-syms and intern a before-ns-unload hook so
      clj-reload can call unregister-ns! before unloading it.
      Pass :reload? true to force re-require (used in hard-reset path)."
     [ns-syms & {:keys [reload?]}]
     (doseq [ns-sym ns-syms]
       (if reload?
         (require ns-sym :reload)
         (require ns-sym))
       (intern (the-ns ns-sym) 'before-ns-unload
               (fn [] (unregister-ns! ns-sym))))))

;; =============================================================================
;; Entity Type Helpers
;; =============================================================================

(defn- registered-types*
  "Impl shared by the (deprecated) public `registered-types` and internal
   callers, so internal use doesn't trip the deprecation warning."
  []
  (->> @registry
       keys
       (filter #(contains? % :atlas/type))
       (map (fn [id] (first (disj id :atlas/type))))
       set))

(defn registered-types
  "Return all registered entity types (those with :atlas/type in their identity).
   :deprecated Use (atlas.ontology/all-ontologies) instead — queries :atlas/ontology
   compound-ids which are always present, unlike :atlas/type meta-registrations."
  {:deprecated "0.2.0"}
  []
  (registered-types*))

(defn entity-type
  "Extract the entity type from a compound identity.

  An entity type is a member that is registered with :atlas/type,
  or if none found, returns the first qualified keyword that could be a type.

  Returns the entity type keyword or nil."
  [identity]
  (let [types (registered-types*)
        type-in-id (first (set/intersection identity types))]
    type-in-id
    #_(or type-in-id
        ;; Fallback: return first keyword if no registered type found
          (first identity))))

(defn aspects
  "Extract all aspects (non-entity-type members) from identity.

  Returns a set of aspect keywords."
  [identity]
  (let [type (entity-type identity)]
    (when type
      (disj identity type))))

(defn with-entity-type
  "Create identity pattern with entity type + aspects.

  Example:
    (with-entity-type :atlas/execution-function
                      :tier/service
                      :domain/cart)
    => #{:atlas/execution-function :tier/service :domain/cart}"
  [entity-type & aspects]
  (apply conj #{entity-type} aspects))

;; =============================================================================
;; Post-Load Validation
;; =============================================================================

(defn validate-registry-types
  "Validate that all registered entities use known types."
  []
  (let [types (registered-types*)
        all-identities (vec (keys @registry))
        violations (for [id all-identities
                         :when (not (contains? id :atlas/type))
                         :let [type (entity-type id)]
                         :when (and type (not (contains? types type)))]
                     {:identity id
                      :type type
                      :dev-id (:atlas/dev-id (fetch id))
                      :message (format* "Entity uses unregistered type: %s" type)})]
    (if (seq violations)
      {:valid? false
       :violations violations
       :registered-types types}
      {:valid? true
       :registered-types types})))

(defn validate-with-specs
  "Optionally validate entities using clojure.spec if specs exist."
  []
  {:valid? true
   :note "Spec validation not yet implemented"})

(defn- dyn-spec
  "Create a dynamic spec that requires the given keys."
  [keys*]
  (eval `(s/keys :req ~keys*)))

(s/def :atlas/compound-id (s/coll-of qualified-keyword?))
(s/def :atlas/dev-id qualified-keyword?)

(defn validate-ontology-specs
  "Validate entities against their ontology definitions using dynamic specs."
  []
  (let [ontologies (into {} (filter (fn [[id _]] (contains? id :atlas/ontology)) @registry))
        ontology-specs (into {}
                             (for [[_compound-id props] ontologies
                                   :let [entity-type (:ontology/for props)
                                         required-keys (:ontology/keys props)]
                                   :when (and entity-type required-keys)]
                               [entity-type required-keys]))]
    (every? (fn [[compound-id props]]
              (or (contains? compound-id :atlas/ontology)
                  (contains? compound-id :atlas/type)
                  (let [etype (entity-type compound-id)
                        required-keys (get ontology-specs etype)]
                    (if required-keys
                      (if (s/valid? (dyn-spec required-keys) props)
                        true
                        (do (tel/log! {:level :warn}
                                      ["NOT VALID"
                                       {:dev-id (:atlas/dev-id props)
                                        :compound-id compound-id
                                        :message (s/explain-str (dyn-spec required-keys) props)}])
                            false))
                      true))))
            @registry)))

;; =============================================================================
;; Analytical Utilities — delegated to atlas.registry.analysis
;; =============================================================================
;;
;; JVM-only: these lazily resolve atlas.registry.analysis (which requires back
;; into atlas.registry / atlas.query) to avoid a load-time cycle. cljs has no
;; requiring-resolve and the browser UI does not use these, so they are elided.

#?(:clj
   (do
     (defn clusters           [] ((requiring-resolve 'atlas.registry.analysis/clusters)))
     (defn correlation-matrix [] ((requiring-resolve 'atlas.registry.analysis/correlation-matrix)))
     (defn missing-aspects    [identity-set] ((requiring-resolve 'atlas.registry.analysis/missing-aspects) identity-set))
     (defn find-anomalies     [] ((requiring-resolve 'atlas.registry.analysis/find-anomalies)))
     (defn to-graphviz        [] ((requiring-resolve 'atlas.registry.analysis/to-graphviz)))
     (defn to-mermaid         [] ((requiring-resolve 'atlas.registry.analysis/to-mermaid)))
     (defn summary            [] ((requiring-resolve 'atlas.registry.analysis/summary)))))
