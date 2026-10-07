(ns atlas.ide
  "IDE integration API - clean interface for editor tooling.
   Returns EDN that editors can parse and display."
  (:require [atlas.registry :as cid]
            [atlas.registry.lookup :as rt]
            [atlas.registry.serialise :as ser]
            [atlas.invariant :as ax]
            [atlas.docs :as docs]
            [atlas.ontology :as ot]
            [atlas.query :as query]
            [atlas.datalog :as datalog]
            [atlas.ide.history :as ide.history]
            [atlas.ide.trace :as ide.trace]
            [clojure.string :as str]
            [clojure.set :as set]))

;; =============================================================================
;; HELPERS
;; =============================================================================

(defn- ensure-keyword
  "Convert string to keyword, or return as-is if already a keyword.
   Handles colon-prefixed strings from MCP/JSON (e.g. \":fn/foo\" → :fn/foo)."
  [x]
  (cond
    (keyword? x) x
    (string? x)  (keyword (cond-> x (= \: (first x)) (subs 1)))
    :else        x))

(defn- sorted-vec
  "Return a sorted vector from a collection (defaults to ascending compare)."
  [coll]
  (->> (or coll [])
       sort
       vec))

(defn- definition-value
  "Normalize registry definition values for editor display."
  [value]
  (cond
    (set? value) (->> value (sort-by pr-str) vec)
    (map? value) (reduce-kv (fn [acc k v]
                              (assoc acc k (definition-value v)))
                            (empty value)
                            value)
    (sequential? value) (mapv definition-value value)
    :else value))

(defn- to-string 
  "Convert keyword to string while preserving its namespace and dropping the leading ':'."
  [x]
  (if (keyword? x)
    (if-let [ns (namespace x)]
      (str ns "/" (name x))
      (name x))
    (str x)))

(defn- fields-for [props]
  (or (:data-schema/fields props)
      (:atlas/fields props)
      []))

(defn- filter-non-serialisable
  "Remove non-serialisable keys from a map based on its compound identity.
   Returns the filtered map. Used to ensure atlas.ide never returns function values to Emacs."
  [compound-id m]
  (if (and (map? m) compound-id)
    (let [not-serialisable-keys (ot/not-serialisable-keys-for-identity compound-id)]
      (if (seq not-serialisable-keys)
        (reduce dissoc m not-serialisable-keys)
        m))
    m))

;; =============================================================================
;; QUERY API
;; =============================================================================

(defn list-entity-types
  "List all registered entity types from ontology definitions."
  []
  (->> (ot/all-ontologies)
       vals
       (map :ontology/for)
       sort
       vec))

(defn list-entity-types-with-counts
  "List all entity types present in the registry with entity counts.
   Discovers types from compound identities (keywords in atlas/* namespace),
   so works with both register!-built and raw EDN-loaded registries.
   Returns [{:entity-type/type :atlas/execution-function :entity-type/count 5} ...]"
  []
  (let [registry (cid/current-registry)]
    (->> registry
         (group-by (fn [[id _]] (first (filter #(= "atlas" (namespace %)) id))))
         (remove (fn [[type _]] (nil? type)))
         (map (fn [[type entries]]
                {:entity-type/type  type
                 :entity-type/count (count entries)}))
         (sort-by (comp str :entity-type/type))
         vec)))

(defn list-entities-of-type
  "List dev-ids of all entities whose compound identity contains entity-type.
   entity-type - keyword like :atlas/execution-function"
  [entity-type]
  (let [entity-type-kw (ensure-keyword entity-type)]
    (->> (cid/current-registry)
         (filter (fn [[id _props]] (contains? id entity-type-kw)))
         (map (fn [[_id props]] (:atlas/dev-id props)))
         (remove nil?)
         sort
         vec)))

(defn registered-entity?
  "Returns true if kw is a registered entity (has an identity in the registry)."
  [kw]
  (boolean (rt/identity-for (ensure-keyword kw))))

(defn entity-type-ontology-keys
  "Get ontology keys for a given entity type."
  [entity-type]
  (vec (or (ot/ontology-keys-for (ensure-keyword entity-type)) [])))

(defn entity-prop-keys
  "Get the navigable prop keys for an entity.
   Returns sorted keyword vector, excluding :atlas/dev-id and :atlas/type
   (those are metadata, not navigable semantic props).
   Returns nil if dev-id is not found in registry."
  [dev-id]
  (let [dev-id-kw (ensure-keyword dev-id)
        id        (rt/identity-for dev-id-kw)
        props     (rt/props-for dev-id-kw)]
    (when (and id props)
      (->> (filter-non-serialisable id props)
           keys
           (remove #{:atlas/dev-id :atlas/type})
           sort
           vec))))

(defn entity-prop-items
  "Get the items of a prop value, each annotated with :item/is-dev-id?.
   Supports single keywords, collections (vec/set), and maps (returns keys).
   Returns [{:item/value kw :item/is-dev-id? bool}] for interactive navigation."
  [dev-id prop-key]
  (let [dev-id-kw  (ensure-keyword dev-id)
        prop-kw    (ensure-keyword prop-key)
        props      (rt/props-for dev-id-kw)
        value      (get props prop-kw)
        annotate   (fn [v]
                     {:item/value      v
                      :item/is-dev-id? (boolean (and (keyword? v)
                                                     (rt/identity-for v)))})]
    (cond
      (or (sequential? value) (set? value))
      (mapv annotate (sort-by pr-str value))

      (map? value)
      (mapv (fn [[k _]] (annotate k)) (sort-by (comp pr-str first) value))

      (some? value)
      [(annotate value)]

      :else [])))

(defn list-all-entities
  "List all registered entities with their type classification.
   Types: endpoint, function, component, schema, protocol,
          business-pattern, constraint, failure-mode,
          value-proposition, user-role, user-experience, other"
  []
  (vec (sort-by str
                (for [[id props] (cid/current-registry)]
                  {:entity/dev-id (:atlas/dev-id props)
                   :entity/type (cond
                                  (contains? id :atlas/interface-endpoint) :endpoint
                                  (contains? id :atlas/execution-function) :function
                                  (contains? id :atlas/structure-component) :component
                                  (contains? id :atlas/schema) :schema
                                  (contains? id :atlas/interface-protocol) :protocol
                                  (contains? id :atlas/business-pattern) :business-pattern
                                  (contains? id :atlas/governance-constraint) :constraint
                                  (contains? id :atlas/risk-failure-mode) :failure-mode
                                  (contains? id :atlas/value-proposition) :value-proposition
                                  (contains? id :atlas/identity-role) :user-role
                                  (contains? id :atlas/experience-journey) :user-experience
                                  :else :other)}))))

(defn list-aspects
  "List all unique aspects used in the registry.

   Args:
     registry - (Optional) Registry map. Defaults to global registry."
  ([]
   (list-aspects (cid/current-registry)))
  ([registry]
   (let [;; Count aspect usage (cid/aspects strips the entity type per identity)
         counts (reduce (fn [acc identity]
                          (let [aspects (cid/aspects identity)]
                            (reduce (fn [m aspect]
                                      (update m aspect (fnil inc 0)))
                                    acc
                                    aspects)))
                        {}
                        (keys registry))
         ;; Namespace ordering for display
         ns-order {"authorization" 0
                   "capacity" 1
                   "compliance" 2
                   "domain" 3
                   "protocol" 4
                   "semantic-namespace" 5
                   "tier" 6
                   "temporal" 7}]
     (->> counts
          (map (fn [[aspect count]]
                 {:aspect/aspect aspect
                  :aspect/count count}))
          (sort-by (fn [{:aspect/keys [aspect]}]
                     (let [ns (namespace aspect)]
                       [(get ns-order ns 100) ns (name aspect)])))
          vec))))

(defn list-aspect-namespaces
  "List all aspect namespaces with usage counts.
   Dynamically excludes entity types using the registry."
  ([]
   (list-aspect-namespaces (cid/current-registry)))
  ([registry]
   (let [;; Count aspects by namespace
         ns-counts (reduce (fn [acc identity]
                             (let [aspects (cid/aspects identity)]
                               (reduce (fn [m aspect]
                                         (when-let [ns (namespace aspect)]
                                           (update m ns (fnil inc 0))))
                                       acc
                                       aspects)))
                           {}
                           (keys registry))]
     (->> ns-counts
          (map (fn [[ns count]]
                 {:namespace/name ns
                  :namespace/count count}))
          (sort-by :namespace/name)
          vec))))

(defn list-aspect-names-in-namespace
  "List all aspect names (without namespace prefix) in a given namespace.
   Returns only aspects, not entity types."
  ([ns-name]
   (list-aspect-names-in-namespace (cid/current-registry) ns-name))
  ([registry ns-name]
   (let [;; Collect all unique names in the namespace
         names (reduce (fn [acc identity]
                         (let [aspects (cid/aspects identity)]
                           (reduce (fn [s aspect]
                                     (if (= (namespace aspect) ns-name)
                                       (conj s (name aspect))
                                       s))
                                   acc
                                   aspects)))
                       #{}
                       (keys registry))]
     (vec (sort names)))))

(defn entities-with-aspect
  "Find all entities with given aspect."
  [aspect]
  (->> (rt/all-with-aspect (ensure-keyword aspect))
       (remove nil?)
       sort
       vec))

(defn entity-info
  [dev-id]
  (let [dev-id-kw (ensure-keyword dev-id)
        id (rt/identity-for dev-id-kw)
        props (rt/props-for dev-id-kw)]
    (when id
      (let [definition-keys (ot/definition-keys-for-identity id)
            not-serialisable-keys (ot/not-serialisable-keys-for-identity id)
            ;; Filter to only include ontology-defined keys, then remove non-serialisable ones
            definition-values (when (seq definition-keys)
                             (reduce (fn [acc k]
                                       ;; Only include if: (1) in props, (2) not in non-serialisable list
                                       (if (and (contains? props k)
                                               (not (contains? not-serialisable-keys k)))
                                         (assoc acc k (definition-value (get props k)))
                                         acc))
                                     (array-map)
                                     definition-keys))
            ;; Props the entity actually carries that no ontology DECLARES.
            ;; Undeclared props are legitimate by design — :ontology/keys is the
            ;; REQUIRED-key contract (see registry/validate-ontology-specs), not an
            ;; exhaustive list of what an entity may hold, and serialise keeps them
            ;; on purpose (:grain/outcomes, :data-schema/field-specs, …). Reporting
            ;; only the declared subset made real, pushed properties invisible to
            ;; entity-detail and every consumer built on it.
            declared    (set definition-keys)
            extra-props (reduce (fn [acc [k v]]
                                  (if (or (contains? declared k)
                                          (contains? not-serialisable-keys k)
                                          (not (ser/serialisable-value? v)))
                                    acc
                                    (assoc acc k (definition-value v))))
                                (array-map)
                                (sort-by key props))]
        {:entity/dev-id dev-id-kw
         :entity/identity (vec (sort id))
         :entity/aspects (vec (sort (disj id :atlas/execution-function
                                          :atlas/structure-component
                                          :atlas/interface-endpoint
                                          :atlas/schema)))
         :entity/definition-keys definition-keys
         :entity/definition-values definition-values
         :entity/extra-props extra-props
         :interface-endpoint/context (vec (sort (ot/context-for dev-id-kw)))
         :interface-endpoint/response (vec (sort (ot/response-for dev-id-kw)))
         :execution-function/deps (vec (sort (ot/deps-for dev-id-kw)))
         :atlas/fields (vec (sort (fields-for props)))}))))

(defn entities-info
  "Batch fetch entity-info for multiple dev-ids. Returns map of dev-id → info."
  [dev-ids]
  (into {} (keep (fn [id]
                   (when-let [info (entity-info id)]
                     [(:entity/dev-id info) info])))
        (map ensure-keyword dev-ids)))

;; =============================================================================
;; TRACE API — delegated to atlas.ide.trace
;; =============================================================================

(defn data-flow              [dev-id]   (ide.trace/data-flow dev-id))
(defn execution-order        []         (ide.trace/execution-order))
(defn dependents-of          [dev-id]   (ide.trace/dependents-of dev-id))
(defn dependencies-of        [dev-id]   (ide.trace/dependencies-of dev-id))
(defn recursive-dependencies-of      [dev-id] (ide.trace/recursive-dependencies-of dev-id))
(defn recursive-dependencies-summary [dev-id] (ide.trace/recursive-dependencies-summary dev-id))
(defn recursive-dependents-of        [dev-id] (ide.trace/recursive-dependents-of dev-id))
(defn recursive-dependents-summary   [dev-id] (ide.trace/recursive-dependents-summary dev-id))
(defn producers-of           [data-key] (ide.trace/producers-of data-key))
(defn consumers-of           [data-key] (ide.trace/consumers-of data-key))
(defn trace-data-flow        [data-key] (ide.trace/trace-data-flow data-key))
(defn impact-of-change       [entity-id] (ide.trace/impact-of-change entity-id))

;; =============================================================================
;; VALIDATION API
;; =============================================================================

(defn check-invariants
  "Run all invariant checks and return results."
  []
  (let [{:keys [valid? errors warnings]} (ax/check-all)]
    {:invariant/valid? valid?
     :invariant/error-count (count errors)
     :invariant/warning-count (count warnings)
     :invariant/errors (mapv #(select-keys % [:invariant :message]) errors)
     :invariant/warnings (mapv #(select-keys % [:invariant :message]) warnings)}))

(defn validate-entity
  "Check invariants relevant to a specific entity."
  [dev-id]
  (let [dev-id-kw (ensure-keyword dev-id)
        {:keys [errors warnings]} (ax/check-all)
        dev-id-str (str dev-id-kw)
        dev-id-name (name dev-id-kw)
        relevant (fn [v]
                   (let [text (str (:message v) (:details v))]
                     (or (str/includes? text dev-id-str)
                         (str/includes? text dev-id-name))))]
    {:validation/dev-id dev-id-kw
     :validation/errors (filterv relevant errors)
     :validation/warnings (filterv relevant warnings)}))

;; =============================================================================
;; DOCUMENTATION API
;; =============================================================================

(defn entity-doc
  "Get documentation for an entity."
  [dev-id]
  (docs/enrich-with-descriptions (ensure-keyword dev-id)))

(defn system-summary
  "Get system overview."
  []
  (docs/system-overview))

(defn generate-markdown
  "Generate full markdown documentation."
  []
  (docs/generate-markdown))

;; =============================================================================
;; COMPLETION API
;; =============================================================================

(defn complete-dev-id
  "Autocomplete dev-id from prefix. Returns vector of strings without ':' prefix."
  [prefix]
  (let [prefix-str (to-string prefix)
        all-ids (map to-string (ax/all-dev-ids))]
    (->> all-ids
         (filter #(str/starts-with? % prefix-str))
         sort
         vec)))

(defn complete-aspect 
  "Autocomplete aspect from prefix. Returns vector of strings without ':' prefix."
  [prefix]
  (let [prefix-str (to-string prefix)
        all-aspects (map (comp to-string :aspect/aspect) (list-aspects))]
    (->> all-aspects
         (filter #(str/starts-with? % prefix-str))
         sort
         vec)))

(defn complete-data-key 
  "Autocomplete data key from prefix. Returns vector of strings without ':' prefix."
  [prefix]
  (let [prefix-str (to-string prefix)
        all-keys (->> (cid/current-registry)
                      vals
                      (mapcat #(concat (:interface-endpoint/context %)
                                       (:interface-endpoint/response %)))
                      (remove nil?)
                      (map to-string)
                      distinct
                      sort)]
    (->> all-keys
         (filter #(str/starts-with? % prefix-str))
         sort
         vec)))

(defn domain-coupling
  "Analyze inter-domain dependencies."
  []
  (->> (query/domain-coupling (cid/current-registry) :atlas/dev-id :execution-function/deps)
       (map (fn [{:keys [domain depends-on entity-count]}]
              {:coupling/domain domain
               :coupling/depends-on (sorted-vec depends-on)
               :coupling/entity-count entity-count}))
       (sort-by :coupling/domain)
       vec))

(defn pii-surface
  "Find all entities handling PII."
  []
  (->> (query/pii-surface (cid/current-registry) :atlas/dev-id :interface-endpoint/context :interface-endpoint/response)
       (map (fn [{:keys [id audited? context response]}]
              {:pii/id id
               :pii/audited? audited?
               :pii/context (sorted-vec context)
               :pii/response (sorted-vec response)}))
       (sort-by :pii/id)
       vec))

(defn error-handler-coverage
  "Check error handler coverage."
  []
  (let [{:keys [handlers coverage]} (query/error-handler-coverage (cid/current-registry) :atlas/dev-id)]
    {:error-handler/handlers (->> handlers
                                  (map (fn [{:keys [id handles]}]
                                         {:error-handler/id id
                                          :error-handler/handles (sorted-vec handles)}))
                                  (sort-by :error-handler/id)
                                  vec)
     :error-handler/coverage (->> coverage
                                  (map (fn [{:keys [entity concern has-handler?]}]
                                         {:error-handler/entity entity
                                          :error-handler/concern concern
                                          :error-handler/has-handler? has-handler?}))
                                  (sort-by (juxt :error-handler/concern :error-handler/entity))
                                  vec)}))

;; =============================================================================
;; ONTOLOGY TOOLS API (from ontology-tools)
;; =============================================================================

(defn suggest-aspects
  "Suggest aspects for an entity based on similar entries."
  [dev-id]
  (let [id (rt/identity-for (keyword dev-id))]
    (let [{:keys [similar-entries suggested-aspects] :as raw} (ot/suggest-aspects id)]
      (-> raw
          (assoc :ontology/similar-entries (->> similar-entries
                                                (map sorted-vec)
                                                vec))
          (assoc :ontology/suggested-aspects (->> suggested-aspects sort vec))
          (dissoc :similar-entries :suggested-aspects)))))

;; WHY this is execution-function/deps related?
(defn inspect-entity
  "Quick inspection of an entity."
  [dev-id]
  (let [dev-id-kw (ensure-keyword dev-id)]
    (let [{:keys [semantic-identity value]} (ot/inspect dev-id-kw)
          ;; Filter non-serialisable keys before returning
          filtered-value (when value
                           (let [filtered (filter-non-serialisable semantic-identity value)]
                             (cond-> filtered
                               (:execution-function/deps filtered)
                               (update :execution-function/deps sorted-vec))))]
      {:inspection/semantic-identity (sorted-vec semantic-identity)
       :inspection/value filtered-value})))

(defn aspect-catalog
  "Show all aspects with usage stats."
  []
  (->> (ot/aspect-catalog)
       (map (fn [[ns items]]
              {:aspect-catalog/namespace ns
               :aspect-catalog/items (->> items
                                          (map (fn [{:keys [aspect usage-count examples]}]
                                                 {:aspect-catalog/aspect aspect
                                                  :aspect-catalog/usage-count usage-count
                                                  :aspect-catalog/examples (sorted-vec examples)}))
                                          (sort-by :aspect-catalog/aspect)
                                          vec)}))
       (sort-by :aspect-catalog/namespace)
       vec))

(defn validate-identity
  "Check if an identity is valid before registering."
  [identity-set]
  (ot/validate-before-register (set (map keyword identity-set))))

(defn list-templates
  "List available templates."
  []
  [{:template/name "foundation-component"
    :template/params [:domain :external? :async? :traced?]
    :template/description "Infrastructure components"}
   {:template/name "service-function"
    :template/params [:domain :operation :external? :pure? :pii?]
    :template/description "Business logic functions"}
   {:template/name "api-endpoint"
    :template/params [:domain :operation :auth-required? :rate-limited? :pii?]
    :template/description "HTTP API endpoints"}])

;; =============================================================================
;; SEMANTIC SIMILARITY API
;; =============================================================================

(defn semantic-similarity
  "Find entities with similar semantic profiles to given entity."
  [dev-id]
  (let [dev-id-kw (ensure-keyword dev-id)
        [target-identity _] (query/find-by-dev-id (cid/current-registry) dev-id-kw)
        results (query/semantic-similarity (cid/current-registry) target-identity 0.0)]
    (->> results
         (map (fn [{:keys [identity similarity shared]}]
                (let [dev-id-for-identity (get-in (cid/current-registry) [identity :atlas/dev-id])]
                  {:similarity/entity dev-id-for-identity
                   :similarity/score (double similarity)
                   :similarity/shared-aspects (sorted-vec shared)})))
         vec)))

;; =============================================================================
;; ARCHITECTURE VIEWS API
;; =============================================================================

(defn by-tier
  "Group all entities by architectural tier."
  []
  (let [result (query/by-tier (cid/current-registry) :atlas/dev-id)]
    (->> result
         (map (fn [[tier entities]]
                {:tier/name tier
                 :tier/entities (sorted-vec entities)
                 :tier/count (count entities)}))
         (sort-by :tier/name)
         vec)))

(defn architecture-view
  "Get architecture documentation: entities grouped by tier and domain."
  []
  (let [arch (docs/architecture-documentation)]
    {:architecture/title (:title arch)
     :architecture/tiers (->> (:tiers arch)
                              (map (fn [[tier data]]
                                     {:tier/name tier
                                      :tier/entities (sorted-vec (:entities data))
                                      :tier/count (:count data)}))
                              (sort-by :tier/name)
                              vec)
     :architecture/domains (->> (:domains arch)
                                (map (fn [[domain data]]
                                       {:domain/name domain
                                        :domain/entities (sorted-vec (:entities data))
                                        :domain/count (:count data)}))
                                (sort-by :domain/name)
                                vec)}))

(defn operations-view
  "Get operations documentation: external integrations, pure functions, OAuth deps."
  []
  (let [ops (docs/operations-documentation)]
    {:operations/title (:title ops)
     :operations/external-integrations
     {:entities (sorted-vec (get-in ops [:external-integrations :entities]))
      :description (get-in ops [:external-integrations :description])
      :concerns (vec (get-in ops [:external-integrations :operational-concerns]))}
     :operations/pure-functions
     {:entities (sorted-vec (get-in ops [:pure-functions :entities]))
      :description (get-in ops [:pure-functions :description])
      :concerns (vec (get-in ops [:pure-functions :operational-concerns]))}
     :operations/oauth-dependencies
     {:entities (sorted-vec (get-in ops [:oauth-dependencies :entities]))
      :description (get-in ops [:oauth-dependencies :description])
      :concerns (vec (get-in ops [:oauth-dependencies :operational-concerns]))}}))

;; =============================================================================
;; ASPECT IMPACT API
;; =============================================================================

(defn aspect-impact
  "Analyze what would be affected by changes to an aspect."
  [aspect]
  (let [aspect-kw (ensure-keyword aspect)
        result (ot/impact-analysis aspect-kw)]
    {:aspect-impact/aspect (:aspect result)
     :aspect-impact/total (or (:total-impact result) 0)
     :aspect-impact/by-tier (:by-tier result)
     :aspect-impact/by-type (:by-type result)
     :aspect-impact/examples (sorted-vec (:examples result))}))

;; =============================================================================
;; REFACTORING API
;; =============================================================================

(defn preview-refactor-aspect
  "Preview what would happen if an aspect is renamed (dry-run only)."
  [old-aspect new-aspect]
  (let [old-kw (ensure-keyword old-aspect)
        new-kw (ensure-keyword new-aspect)
        result (ot/refactor-aspect old-kw new-kw :dry-run? true)]
    {:refactor/old-aspect old-kw
     :refactor/new-aspect new-kw
     :refactor/affected-count (:affected-count result)
     :refactor/conflicts (vec (:conflicts result))
     :refactor/migrations (->> (:migrations result)
                               (map (fn [{:keys [dev-id old-id new-id will-collide?]}]
                                      {:refactor/dev-id dev-id
                                       :refactor/old-identity (sorted-vec old-id)
                                       :refactor/new-identity (sorted-vec new-id)
                                       :refactor/will-collide? will-collide?}))
                               vec)}))

;; =============================================================================
;; PROTOCOL API
;; =============================================================================

(defn list-protocols
  "List all registered protocols."
  []
  (->> (cid/current-registry)
       (filter (fn [[id _]] (contains? id :atlas/interface-protocol)))
       (map (fn [[_ props]]
              (let [dev-id (:atlas/dev-id props)]
                {:protocol/id dev-id
                 :interface-protocol/functions (vec (sort (:interface-protocol/functions props)))})))
       (sort-by :protocol/id)
       vec))

(defn protocol-info
  "Get detailed information about a protocol."
  [protocol-id]
  (let [protocol-id-kw (ensure-keyword protocol-id)
        props (rt/props-for protocol-id-kw)]
    (when props
      {:protocol/id protocol-id-kw
       :interface-protocol/functions (vec (sort (:interface-protocol/functions props)))
       :protocol/implementers (->> (cid/current-registry)
                                   (filter (fn [[id _]]
                                             (and (contains? id :atlas/structure-component)
                                                  (contains? id protocol-id-kw))))
                                   (map (fn [[_ v]] (:atlas/dev-id v)))
                                   sort
                                   vec)})))

(defn component-protocols
  "Get all protocols implemented by a component."
  [component-id]
  (let [component-id-kw (ensure-keyword component-id)
        compound-id (rt/identity-for component-id-kw)
        protocol-aspects (filter #(= "protocol" (namespace %)) compound-id)]
    (vec (sort protocol-aspects))))

(defn components-implementing
  "Find all components implementing a specific protocol."
  [protocol-id]
  (let [protocol-id-kw (ensure-keyword protocol-id)]
    (->> (cid/current-registry)
         (filter (fn [[id _]]
                   (and (contains? id :atlas/structure-component)
                        (contains? id protocol-id-kw))))
         (map (fn [[_ v]] (:atlas/dev-id v)))
         sort
         vec)))

;; =============================================================================
;; BUSINESS SEMANTICS API
;; =============================================================================

(defn- business-entity-type 
  "Detect the business entity type from compound identity."
  [id]
  (cond
    (contains? id :atlas/business-pattern) :business-pattern
    (contains? id :atlas/governance-constraint) :constraint
    (contains? id :atlas/risk-failure-mode) :failure-mode
    (contains? id :atlas/value-proposition) :value-proposition
    (contains? id :atlas/identity-role) :user-role
    (contains? id :atlas/experience-journey) :user-experience
    :else nil))

(defn list-business-entities
  "List all business-layer entities grouped by type."
  []
  (let [all-entities (list-all-entities)
        business-types [:business-pattern :constraint :failure-mode
                        :value-proposition :user-role :user-experience]
        grouped (reduce (fn [acc entity]
                          (let [type (:entity/type entity)]
                            (if (some #{type} business-types)
                              (update acc type (fnil conj []) entity)
                              acc)))
                        {}
                        all-entities)]
    {:business/patterns (sorted-vec (map :entity/dev-id (get grouped :business-pattern)))
     :business/constraints (sorted-vec (map :entity/dev-id (get grouped :constraint)))
     :business/failure-modes (sorted-vec (map :entity/dev-id (get grouped :failure-mode)))
     :business/values (sorted-vec (map :entity/dev-id (get grouped :value-proposition)))
     :business/roles (sorted-vec (map :entity/dev-id (get grouped :user-role)))
     :business/experiences (sorted-vec (map :entity/dev-id (get grouped :user-experience)))}))

(defn- extract-business-metadata
  "Extract type-specific metadata from business entity properties.
   Only extracts keys defined in the business entity ontology (serialisable keys only)."
  [compound-id props biz-type]
  ;; Extract metadata, then filter non-serialisable keys
  (let [metadata (case biz-type
                   :business-pattern
                   {:principle (:semantic-namespace/principle props)
                    :justification (:semantic-namespace/justification props)
                    :user-experience (:atlas/experience-journey props)
                    :business-value (:semantic-namespace/business-value props)
                    :alternative-rejected (:semantic-namespace/alternative-rejected props)
                    :why-rejected (:semantic-namespace/why-rejected props)}

                   :constraint
                   {:enforced-by (:semantic-namespace/enforced-by props)
                    :rationale (:semantic-namespace/rationale props)
                    :compliance-requirement (:semantic-namespace/compliance-requirement props)
                    :violation-response (:semantic-namespace/violation-response props)
                    :user-sees (:semantic-namespace/user-sees props)
                    :business-impact (:semantic-namespace/business-impact props)}

                   :failure-mode
                   {:triggered-by (:semantic-namespace/triggered-by props)
                    :detection (:semantic-namespace/detection props)
                    :user-experiences (:semantic-namespace/user-experiences props)
                    :recovery-path (:semantic-namespace/recovery-path props)
                    :recovery-steps (:semantic-namespace/recovery-steps props)
                    :data-loss (:semantic-namespace/data-loss props)
                    :business-impact (:semantic-namespace/business-impact props)
                    :frequency (:semantic-namespace/frequency props)
                    :preventable (:semantic-namespace/preventable props)}

                   :value-proposition
                   {:business-problem (:semantic-namespace/business-problem props)
                    :before-state (:semantic-namespace/before-state props)
                    :after-state (:semantic-namespace/after-state props)
                    :solution (:semantic-namespace/solution props)
                    :user-segment (:semantic-namespace/user-segment props)
                    :business-value (:semantic-namespace/business-value props)
                    :competitive-advantage (:semantic-namespace/competitive-advantage props)}

                   :user-role
                   {:description (:semantic-namespace/description props)
                    :cannot-access (:semantic-namespace/cannot-access props)
                    :responsibilities (:semantic-namespace/responsibilities props)
                    :expectations (:semantic-namespace/expectations props)
                    :data-access (:semantic-namespace/data-access props)
                    :granted-by (:semantic-namespace/granted-by props)
                    :security-requirement (:semantic-namespace/security-requirement props)}

                   :user-experience
                   {:user-journey (:semantic-namespace/user-journey props)
                    :time-to-complete (:semantic-namespace/time-to-complete props)
                    :friction-points (:semantic-namespace/friction-points props)
                    :user-sentiment (:semantic-namespace/user-sentiment props)
                    :failure-mode (:atlas/risk-failure-mode props)
                    :why-designed-this-way (:semantic-namespace/why-designed-this-way props)
                    :delivers-value (:semantic-namespace/delivers-value props)}

                   {})]
    ;; Filter out non-serialisable keys from metadata
    (filter-non-serialisable compound-id metadata)))

(defn business-entity-info
  "Get detailed information about a business entity including metadata and implementations."
  [dev-id]
  (let [dev-id-kw (ensure-keyword dev-id)
        [id props] (query/find-by-dev-id (cid/current-registry) dev-id-kw)]
    (when id
      (let [biz-type (business-entity-type id)]
        (cond-> {:entity/dev-id dev-id-kw
                 :entity/type biz-type
                 :entity/identity (vec (sort id))}
          biz-type (assoc :business/metadata (extract-business-metadata id props biz-type))
          biz-type (assoc :business/implements-in
                         (sorted-vec (entities-with-aspect dev-id-kw))))))))

(defn entities-implementing
  "Find all technical entities that implement/have a given business aspect."
  [business-aspect]
  (let [aspect-kw (ensure-keyword business-aspect)]
    (vec (sort (entities-with-aspect aspect-kw)))))

(defn business-aspects-of
  "Get all business aspects applied to a technical entity, grouped by type."
  [dev-id]
  (let [dev-id-kw (ensure-keyword dev-id)
        id (rt/identity-for dev-id-kw)
        ;; Filter aspects by business namespaces (pattern/, constraint/, etc.)
        business-namespaces #{"pattern" "constraint" "failure-mode"
                             "value" "role" "experience"}
        business-aspects (filter (fn [aspect]
                                   (when (keyword? aspect)
                                     (some #{(namespace aspect)} business-namespaces)))
                                 id)
        ;; Group by their type based on namespace
        grouped (reduce (fn [acc aspect]
                          (let [ns (namespace aspect)]
                            (case ns
                              "pattern" (update acc :patterns (fnil conj []) aspect)
                              "constraint" (update acc :constraints (fnil conj []) aspect)
                              "failure-mode" (update acc :failure-modes (fnil conj []) aspect)
                              "value" (update acc :values (fnil conj []) aspect)
                              "role" (update acc :roles (fnil conj []) aspect)
                              "experience" (update acc :experiences (fnil conj []) aspect)
                              acc)))
                        {}
                        business-aspects)]
    (cond-> {:entity/dev-id dev-id-kw}
      (:patterns grouped) (assoc :business/patterns (sorted-vec (:patterns grouped)))
      (:constraints grouped) (assoc :business/constraints (sorted-vec (:constraints grouped)))
      (:failure-modes grouped) (assoc :business/failure-modes (sorted-vec (:failure-modes grouped)))
      (:values grouped) (assoc :business/values (sorted-vec (:values grouped)))
      (:roles grouped) (assoc :business/roles (sorted-vec (:roles grouped)))
      (:experiences grouped) (assoc :business/experiences (sorted-vec (:experiences grouped))))))

;; =============================================================================
;; EXPLORER API (v2 dual-map filtering)
;; =============================================================================

(defn explorer-filter-entities
  "Filter entities matching AND/OR aspect criteria, sorted by semantic distance.
   - Entity matches if it has ALL aspects from aspects-and
   - OR entity matches if it has ANY aspect from aspects-or
   - Results sorted by Jaccard similarity to the query aspects (closest first)
   Returns vector of {:dev-id :type :similarity :shared :unique} maps.

   Uses cached datalog DB for efficient querying."
  [aspects-and aspects-or]
  (let [query-aspects (set/union (set aspects-and) (set aspects-or))]
    (when (seq query-aspects)
      (let [db (datalog/get-db)
            matching-dev-ids (datalog/query-explorer-filter db aspects-and aspects-or)]
        (->> matching-dev-ids
             (map (fn [dev-id]
                    (let [entity-aspects (datalog/query-entity-aspects db dev-id)
                          entity-type (datalog/query-entity-type db dev-id)
                          shared (set/intersection query-aspects entity-aspects)
                          unique-to-entity (set/difference entity-aspects query-aspects)
                          union (set/union query-aspects entity-aspects)
                          similarity (if (seq union)
                                       (/ (count shared) (count union))
                                       0.0)]
                      {:dev-id dev-id
                       :type (to-string entity-type)
                       :similarity (double similarity)
                       :shared (sorted-vec shared)
                       :unique (sorted-vec unique-to-entity)})))
             (filter :dev-id)
             ;; Sort by similarity (descending), then by dev-id for stability
             (sort-by (juxt (comp - :similarity) :dev-id))
             vec)))))

(defn similar-with-diff
  "Find similar entities to a compound identity, showing aspect differences.
   Returns entities sorted by similarity with shared and unique aspects highlighted.

   Uses cached datalog DB for efficient querying."
  [compound-identity]
  (let [compound-identity-set (if (set? compound-identity)
                                 compound-identity
                                 (set compound-identity))
        query-aspects (cid/aspects compound-identity-set)
        db (datalog/get-db)
        ;; Get dev-id for the query identity if it exists
        query-dev-id (:atlas/dev-id (cid/fetch compound-identity-set))
        all-dev-ids (datalog/query-all-entities db)]
    (->> all-dev-ids
         (keep (fn [dev-id]
                 (when (not= dev-id query-dev-id)
                   (let [entity-aspects (datalog/query-entity-aspects db dev-id)
                         shared (set/intersection query-aspects entity-aspects)
                         unique-to-query (set/difference query-aspects entity-aspects)
                         unique-to-entity (set/difference entity-aspects query-aspects)
                         union (set/union query-aspects entity-aspects)
                         similarity (if (seq union)
                                      (/ (count shared) (count union))
                                      0.0)]
                     (when (pos? similarity)
                       {:dev-id dev-id
                        :similarity (double similarity)
                        :shared (sorted-vec shared)
                        :unique-to-query (sorted-vec unique-to-query)
                        :unique-to-entity (sorted-vec unique-to-entity)})))))
         (sort-by (juxt (comp - :similarity) :dev-id))
         vec)))

;; =============================================================================
;; LLM EXPORT API
;; =============================================================================

(defn llm-context
  "Get full documentation context optimized for LLM consumption."
  []
  (docs/llm-documentation-context))

;; =============================================================================
;; HISTORY API — delegated to atlas.ide.history
;; =============================================================================

(defn history-init!          [] (ide.history/init!))
(defn history-versions       [] (ide.history/versions))
(defn history-entity-timeline [dev-id] (ide.history/entity-timeline dev-id))
(defn history-full-timeline  [dev-id] (ide.history/full-timeline dev-id))
(defn history-version-diff   [version] (ide.history/version-diff version))
(defn history-version-diff-full [version] (ide.history/version-diff-full version))
(defn history-version-summary [version] (ide.history/version-summary version))
(defn history-vocabulary-diff [v-old v-new] (ide.history/vocabulary-diff v-old v-new))
(defn history-edges-at       [version dev-id] (ide.history/edges-at version dev-id))
(defn history-dependents-of  [version dev-id] (ide.history/dependents-of version dev-id))
(defn history-edge-diff      [v-old v-new] (ide.history/edge-diff v-old v-new))
(defn history-edge-summary   [version] (ide.history/edge-summary version))
(defn history-snapshot!
  ([version-label] (ide.history/snapshot! version-label))
  ([version-label rename-map] (ide.history/snapshot! version-label rename-map)))
(defn history-get-conn       [] (ide.history/get-conn))

;; =============================================================================
;; ENTITY SCAFFOLD — ontology-driven
;; =============================================================================

(defn scaffold-entity
  "Generate a register! call for any entity type, driven by the ontology.
   Reads ontology keys to produce the right props skeleton. Works for
   built-in and custom entity types.

   Options:
     :dev-id-prefix — namespace prefix for the dev-id (default: derived from type)
     :aspects       — extra aspects to include (default: #{})

   Returns a string."
  ([entity-type]
   (scaffold-entity entity-type {}))
  ([entity-type {:keys [dev-id-prefix aspects]}]
   (let [entity-type-kw (ensure-keyword entity-type)
         ont-keys       (vec (ot/ontology-keys-for entity-type-kw))
         ;; Derive dev-id prefix from entity type name
         prefix         (or dev-id-prefix
                             (name entity-type-kw))
         ;; Build placeholder props from ontology keys
         prop-lines     (mapv (fn [k]
                                (let [nm (name k)]
                                  (cond
                                    (str/ends-with? nm "deps")
                                    (str k " #{}")

                                    (str/ends-with? nm "context")
                                    (str k " [" "]")

                                    (str/ends-with? nm "response")
                                    (str k " [" "]")

                                    (str/ends-with? nm "expectations")
                                    (str k " []")

                                    (str/ends-with? nm "mocks")
                                    (str k " {}")

                                    (str/ends-with? nm "fixture")
                                    (str k " {}")

                                    (str/ends-with? nm "fn")
                                    (str k " (fn [] nil)")

                                    (= nm "impl")
                                    (str k " (fn [ctx] ctx)")

                                    (= nm "target")
                                    (str k " :TARGET-DEV-ID")

                                    :else
                                    (str k " nil ;; TODO"))))
                              ont-keys)
         aspect-set     (or aspects #{})]
     (str/join
      "\n"
      [(str ";; " entity-type-kw " entity — fill in placeholders")
       (str "(registry/register!")
       (str " :" prefix "/NAME")
       (str " " entity-type-kw)
       (str " #{" entity-type-kw
            (when (seq aspect-set)
              (str " " (str/join " " (sort (map str aspect-set)))))
            "}")
       (if (seq prop-lines)
         (str " {"
              (str/join "\n  " prop-lines)
              "})")
         " {})")]))))

;; =============================================================================
;; TEST TEMPLATE GENERATION
;; =============================================================================

(defn exec-fn-test-template
  "Generate a test-case registration for a single execution-function.
   Uses direct deps only (no transitive walk). The generated code registers
   an :atlas/test-case entity that can be run via the runner and Kaocha.
   Returns a string."
  [dev-id]
  (let [dev-id-kw   (ensure-keyword dev-id)
        direct-deps (ot/deps-for dev-id-kw)
        entity-type (fn [id]
                      (when-let [identity (rt/identity-for id)]
                        (some #(when (= "atlas" (namespace %)) %) identity)))
        components  (vec (sort (filter #(= :atlas/structure-component (entity-type %))
                                       direct-deps)))
        dep-fns     (vec (sort (filter #(= :atlas/execution-function (entity-type %))
                                       direct-deps)))
        ctx-keys    (vec (sort (ot/context-for dev-id-kw)))
        ;; Derive a test dev-id from the fn name
        test-dev-id (keyword "test" (name dev-id-kw))
        sample      (fn [k]
                      (let [nm (name k)]
                        (cond
                          (str/includes? nm "id")    (str "\"<" nm ">\" ")
                          (str/includes? nm "token") (str "\"<" nm ">\" ")
                          (str/includes? nm "?")     "false"
                          (str/includes? nm "count") "0"
                          (str/ends-with? nm "s")    "[]"
                          :else                      (str "\"<" nm ">\" "))))]
    (str/join
     "\n"
     (remove
      nil?
      [(str ";; Test-case for " dev-id-kw)
       ";; Generated from atlas registry — edit fixture values and expectations"
       ""
       "(require '[atlas.registry :as registry])"
       "(require '[atlas.test.runner :as test-runner])"
       ""
       (str "(registry/register!")
       (str " " test-dev-id)
       " :atlas/test-case"
       (str " #{:atlas/test-case}  ;; add domain/test aspects as needed")
       ""
       (str " {;; ---- target ----"
            "\n  :test-case/target " dev-id-kw)
       ""
       "  ;; ---- fixture (initial context) ----"
       (str "  :test-case/fixture {"
            (str/join "\n                      "
                      (map (fn [k] (str k " " (sample k)))
                           ctx-keys))
            "}")
       ""
       ;; Mocks section
       (if (seq components)
         (str "  ;; ---- mocks (omit to start real components from ig-config) ----\n"
              "  ;; Replace with mock values, or remove to use real subsystem\n"
              "  :test-case/mocks {"
              (str/join "\n                     "
                        (map (fn [c] (str c " nil ;; TODO: mock or remove \n"))
                             components))
              "}")
         "  ;; No structure-component deps to mock")
       ""
       "  ;; ---- expectations ----"
       "  :test-case/expectations"
       (str "  [{:expectation/kind :result")
       (str "    :expectation/pred (fn [r] (contains? r " (first (ot/response-for dev-id-kw)) "))")
       (str "    :expectation/docs \"TODO: describe expected result\"}")
       ;; Trace expectations for dep fns
       (when (seq dep-fns)
         (str/join
          "\n"
          (map (fn [dep]
                 (str "   {:expectation/kind :trace\n"
                      "    :expectation/must-contain " dep "\n"
                      "    :expectation/docs \"Pipeline calls " dep "\"}"))
               dep-fns)))
       "   ]})"
       ""
       ";; ---- run ----"
       (str "(test-runner/run-test {:test/case-dev-id " test-dev-id "})")
       ""
       ";; With integrant config (starts real components minus mocks):"
       (str ";; (test-runner/run-test {:test/case-dev-id " test-dev-id)
       ";;                        :test/ig-config   ig-config"
       ";;                        :test/init-key-fn ig/init-key"
       ";;                        :test/halt-key-fn ig/halt-key!})"]))))

;; =============================================================================
;; SELF-REGISTRATION — IDE functions as execution-functions
;; =============================================================================

;; Intent: query (registry browsing)

(cid/register!
 :fn.ide/list-entity-types :atlas/execution-function
 #{:domain/ide :intent/query :meta/ide-list-entity-types}
 {:execution-function/context []
  :execution-function/response [:entity-type/list]
  :execution-function/deps #{}
  :atlas/docs "List all registered entity types (e.g. :atlas/execution-function, :atlas/data-schema). Use this to discover what kinds of entities exist in the registry."
  :atlas/impl (fn [_] (list-entity-types))})

(cid/register!
 :fn.ide/list-entity-types-with-counts :atlas/execution-function
 #{:domain/ide :intent/query :meta/ide-list-entity-types-with-counts}
 {:execution-function/context []
  :execution-function/response [:entity-type/type :entity-type/count]
  :execution-function/deps #{}
  :atlas/docs "List all entity types with how many entities of each type are registered. Gives a quick overview of registry composition."
  :atlas/impl (fn [_] (list-entity-types-with-counts))})

(cid/register!
 :fn.ide/list-entities-of-type :atlas/execution-function
 #{:domain/ide :intent/query :meta/ide-list-entities-of-type}
 {:execution-function/context [:entity-type/type]
  :execution-function/response [:entity/dev-id-list]
  :execution-function/deps #{}
  :atlas/docs "List all dev-ids of entities matching a given entity type (e.g. all :atlas/execution-function entities). Use this to enumerate entities of a specific kind."
  :atlas/impl #(list-entities-of-type (:entity-type/type %))})

(cid/register!
 :fn.ide/registered-entity? :atlas/execution-function
 #{:domain/ide :intent/query :meta/ide-registered-entity?}
 {:execution-function/context [:entity/dev-id]
  :execution-function/response [:entity/exists?]
  :execution-function/deps #{}
  :atlas/docs "Check whether an entity with the given dev-id exists in the registry. Returns boolean."
  :atlas/impl #(registered-entity? (:entity/dev-id %))})

(cid/register!
 :fn.ide/entity-type-ontology-keys :atlas/execution-function
 #{:domain/ide :intent/query :meta/ide-entity-type-ontology-keys}
 {:execution-function/context [:entity-type/type]
  :execution-function/response [:ontology/keys]
  :execution-function/deps #{}
  :atlas/docs "Get the ontology-defined prop keys for an entity type (e.g. :execution-function/context, :execution-function/deps for :atlas/execution-function). Use this to understand what props an entity type expects."
  :atlas/impl #(entity-type-ontology-keys (:entity-type/type %))})

(cid/register!
 :fn.ide/entity-prop-keys :atlas/execution-function
 #{:domain/ide :intent/query :meta/ide-entity-prop-keys}
 {:execution-function/context [:entity/dev-id]
  :execution-function/response [:entity/prop-keys]
  :execution-function/deps #{}
  :atlas/docs "Get all property keys on a specific entity. Use this to see what data is stored on an entity without fetching full values."
  :atlas/impl #(entity-prop-keys (:entity/dev-id %))})

(cid/register!
 :fn.ide/entity-prop-items :atlas/execution-function
 #{:domain/ide :intent/query :meta/ide-entity-prop-items}
 {:execution-function/context [:entity/dev-id :entity/prop-key]
  :execution-function/response [:entity/prop-items]
  :execution-function/deps #{}
  :atlas/docs "Get the value of a specific property key on an entity. Use this to drill into a single prop without fetching the entire entity."
  :atlas/impl #(entity-prop-items (:entity/dev-id %) (:entity/prop-key %))})

(cid/register!
 :fn.ide/list-all-entities :atlas/execution-function
 #{:domain/ide :intent/query :meta/ide-list-all-entities}
 {:execution-function/context []
  :execution-function/response [:entity/list]
  :execution-function/deps #{}
  :atlas/docs "List all entities in the registry with their dev-id, type, and aspects. Returns a compact summary of every registered entity."
  :atlas/impl (fn [_] (list-all-entities))})

(cid/register!
 :fn.ide/list-aspects :atlas/execution-function
 #{:domain/ide :intent/query :meta/ide-list-aspects}
 {:execution-function/context []
  :execution-function/response [:aspect/list]
  :execution-function/deps #{}
  :atlas/docs "List all aspects used in the registry with their usage counts. Use this to understand the semantic vocabulary of the system."
  :atlas/impl (fn [_] (list-aspects))})

(cid/register!
 :fn.ide/list-aspect-namespaces :atlas/execution-function
 #{:domain/ide :intent/query :meta/ide-list-aspect-namespaces}
 {:execution-function/context []
  :execution-function/response [:namespace/list]
  :execution-function/deps #{}
  :atlas/docs "List all aspect namespace prefixes (e.g. 'domain', 'tier', 'operation'). Use this to understand the taxonomy structure of the aspect vocabulary."
  :atlas/impl (fn [_] (list-aspect-namespaces))})

(cid/register!
 :fn.ide/list-aspect-names-in-namespace :atlas/execution-function
 #{:domain/ide :intent/query :meta/ide-list-aspect-names-in-namespace}
 {:execution-function/context [:namespace/name]
  :execution-function/response [:aspect/names]
  :execution-function/deps #{}
  :atlas/docs "List all aspect names within a given namespace (e.g. all aspects in the 'domain' namespace: auth, billing, etc.). Use this to explore a specific aspect category."
  :atlas/impl #(list-aspect-names-in-namespace (:namespace/name %))})

(cid/register!
 :fn.ide/entities-with-aspect :atlas/execution-function
 #{:domain/ide :intent/query :meta/ide-entities-with-aspect}
 {:execution-function/context [:query/aspect]
  :execution-function/response [:entity/dev-id-list]
  :execution-function/deps #{}
  :atlas/docs "Find all entities that have a given aspect in their compound identity. For example, find all entities tagged :domain/auth. Use this for semantic querying."
  :atlas/impl #(entities-with-aspect (:query/aspect %))})

(cid/register!
 :fn.ide/entity-info :atlas/execution-function
 #{:domain/ide :intent/query :meta/ide-entity-info}
 {:execution-function/context [:entity/dev-id]
  :execution-function/response [:entity/info]
  :execution-function/deps #{}
  :atlas/docs "Get comprehensive info for a single entity: dev-id, compound identity, type, aspects, and all props (with non-serializable values filtered). This is the primary entity inspection tool."
  :atlas/impl #(entity-info (:entity/dev-id %))})

(cid/register!
 :fn.ide/entities-info :atlas/execution-function
 #{:domain/ide :intent/query :meta/ide-entities-info}
 {:execution-function/context [:entity/dev-id-list]
  :execution-function/response [:entity/info-list]
  :execution-function/deps #{}
  :atlas/docs "Get comprehensive info for multiple entities at once (batch version of entity-info). Pass a list of dev-ids, get back a map of dev-id to entity info."
  :atlas/impl #(entities-info (:entity/dev-id-list %))})

(cid/register!
 :fn.ide/inspect-entity :atlas/execution-function
 #{:domain/ide :intent/query :meta/ide-inspect-entity}
 {:execution-function/context [:entity/dev-id]
  :execution-function/response [:entity/inspection]
  :execution-function/deps #{}
  :atlas/docs "Deep inspection of an entity including raw registry data, compound identity analysis, and structural details. More verbose than entity-info — use for debugging."
  :atlas/impl #(inspect-entity (:entity/dev-id %))})

(cid/register!
 :fn.ide/entity-doc :atlas/execution-function
 #{:domain/ide :intent/query :meta/ide-entity-doc}
 {:execution-function/context [:entity/dev-id]
  :execution-function/response [:entity/doc]
  :execution-function/deps #{}
  :atlas/docs "Get documentation for an entity, enriched with auto-generated descriptions based on its type, aspects, and relationships."
  :atlas/impl #(entity-doc (:entity/dev-id %))})

(cid/register!
 :fn.ide/suggest-aspects :atlas/execution-function
 #{:domain/ide :intent/query :meta/ide-suggest-aspects}
 {:execution-function/context [:entity/dev-id]
  :execution-function/response [:aspect/suggestions]
  :execution-function/deps #{}
  :atlas/docs "Suggest additional aspects that might apply to an entity based on patterns from similar entities. Use this to improve semantic coverage."
  :atlas/impl #(suggest-aspects (:entity/dev-id %))})

(cid/register!
 :fn.ide/semantic-similarity :atlas/execution-function
 #{:domain/ide :intent/query :meta/ide-semantic-similarity}
 {:execution-function/context [:entity/dev-id]
  :execution-function/response [:similarity/results]
  :execution-function/deps #{}
  :atlas/docs "Find entities semantically similar to a given entity based on shared aspects. Results are ranked by similarity score. Use this to discover related functionality."
  :atlas/impl #(semantic-similarity (:entity/dev-id %))})

(cid/register!
 :fn.ide/validate-identity :atlas/execution-function
 #{:domain/ide :intent/query :meta/ide-validate-identity}
 {:execution-function/context [:entity/identity-set]
  :execution-function/response [:validation/result]
  :execution-function/deps #{}
  :atlas/docs "Validate a proposed compound identity set: check it has exactly one entity type, all keywords are qualified, and aspects follow conventions."
  :atlas/impl #(validate-identity (:entity/identity-set %))})

;; Trace registrations are in atlas.ide.trace (loaded via require)

;; Intent: diagnose (system-wide analysis)

(cid/register!
 :fn.ide/check-invariants :atlas/execution-function
 #{:domain/ide :intent/diagnose :meta/ide-check-invariants}
 {:execution-function/context []
  :execution-function/response [:invariant/errors :invariant/warnings]
  :execution-function/deps #{}
  :atlas/docs "Run all registered invariant checks against the current registry. Returns error count, warning count, and detailed violation messages. Use this to verify architectural consistency."
  :atlas/impl (fn [_] (check-invariants))})

(cid/register!
 :fn.ide/validate-entity :atlas/execution-function
 #{:domain/ide :intent/diagnose :meta/ide-validate-entity}
 {:execution-function/context [:entity/dev-id]
  :execution-function/response [:validation/errors :validation/warnings]
  :execution-function/deps #{}
  :atlas/docs "Check invariants relevant to a specific entity. Filters all violations to show only those mentioning this entity. Use this to validate a single entity in isolation."
  :atlas/impl #(validate-entity (:entity/dev-id %))})

(cid/register!
 :fn.ide/domain-coupling :atlas/execution-function
 #{:domain/ide :intent/diagnose :meta/ide-domain-coupling}
 {:execution-function/context []
  :execution-function/response [:coupling/matrix]
  :execution-function/deps #{}
  :atlas/docs "Analyze inter-domain coupling: for each domain, show which other domains it depends on and how many entities it has. Use this to detect tight coupling between domains."
  :atlas/impl (fn [_] (domain-coupling))})

(cid/register!
 :fn.ide/pii-surface :atlas/execution-function
 #{:domain/ide :intent/diagnose :meta/ide-pii-surface}
 {:execution-function/context []
  :execution-function/response [:pii/entities]
  :execution-function/deps #{}
  :atlas/docs "Find all entities that handle PII (personally identifiable information). Detects entities with :data/pii or :effect/pii aspects. Use this for privacy audits and compliance reviews."
  :atlas/impl (fn [_] (pii-surface))})

(cid/register!
 :fn.ide/error-handler-coverage :atlas/execution-function
 #{:domain/ide :intent/diagnose :meta/ide-error-handler-coverage}
 {:execution-function/context []
  :execution-function/response [:coverage/results]
  :execution-function/deps #{}
  :atlas/docs "Analyze error handler coverage: which entities have error handling aspects and which don't. Use this to find gaps in error handling across the system."
  :atlas/impl (fn [_] (error-handler-coverage))})

;; Intent: explain (documentation & views)

(cid/register!
 :fn.ide/system-summary :atlas/execution-function
 #{:domain/ide :intent/explain :meta/ide-system-summary}
 {:execution-function/context []
  :execution-function/response [:system/summary]
  :execution-function/deps #{}
  :atlas/docs "Get a high-level system overview: entity counts by type, domain breakdown, and key architectural metrics. Use this as a starting point to understand a registry."
  :atlas/impl (fn [_] (system-summary))})

(cid/register!
 :fn.ide/generate-markdown :atlas/execution-function
 #{:domain/ide :intent/explain :meta/ide-generate-markdown}
 {:execution-function/context []
  :execution-function/response [:doc/markdown]
  :execution-function/deps #{}
  :atlas/docs "Generate full markdown documentation for the entire registry. Includes entity listings, aspect catalog, and architectural overview."
  :atlas/impl (fn [_] (generate-markdown))})

(cid/register!
 :fn.ide/aspect-catalog :atlas/execution-function
 #{:domain/ide :intent/explain :meta/ide-aspect-catalog}
 {:execution-function/context []
  :execution-function/response [:aspect/catalog]
  :execution-function/deps #{}
  :atlas/docs "Get a structured catalog of all aspects organized by namespace, with usage counts and example entities. Use this to understand the full semantic vocabulary."
  :atlas/impl (fn [_] (aspect-catalog))})

(cid/register!
 :fn.ide/list-templates :atlas/execution-function
 #{:domain/ide :intent/explain :meta/ide-list-templates}
 {:execution-function/context []
  :execution-function/response [:template/list]
  :execution-function/deps #{}
  :atlas/docs "List all registered entity templates (pre-configured aspect sets for common patterns). Use this to discover reusable registration patterns."
  :atlas/impl (fn [_] (list-templates))})

(cid/register!
 :fn.ide/by-tier :atlas/execution-function
 #{:domain/ide :intent/explain :meta/ide-by-tier}
 {:execution-function/context []
  :execution-function/response [:tier/groups]
  :execution-function/deps #{}
  :atlas/docs "Group all entities by their architectural tier (service, handler, gateway, etc.). Use this to see the layered architecture of the system."
  :atlas/impl (fn [_] (by-tier))})

(cid/register!
 :fn.ide/architecture-view :atlas/execution-function
 #{:domain/ide :intent/explain :meta/ide-architecture-view}
 {:execution-function/context []
  :execution-function/response [:architecture/view]
  :execution-function/deps #{}
  :atlas/docs "Get an architectural view of the system: domains, tiers, cross-cutting concerns, and their entity counts. Use this for high-level architecture diagrams."
  :atlas/impl (fn [_] (architecture-view))})

(cid/register!
 :fn.ide/operations-view :atlas/execution-function
 #{:domain/ide :intent/explain :meta/ide-operations-view}
 {:execution-function/context []
  :execution-function/response [:operations/view]
  :execution-function/deps #{}
  :atlas/docs "Get an operations-focused view: entities grouped by operation type (validate, process, query, etc.). Use this to understand the system's behavioral surface."
  :atlas/impl (fn [_] (operations-view))})

(cid/register!
 :fn.ide/llm-context :atlas/execution-function
 #{:domain/ide :intent/explain :meta/ide-llm-context}
 {:execution-function/context []
  :execution-function/response [:atlas/ontology :atlas/tools :atlas/types :atlas/registry]
  :execution-function/deps #{}
  :atlas/docs "Get a comprehensive context dump designed for LLM consumption: ontology rules, available tools, entity types, and full registry. Use this to bootstrap an LLM's understanding of the system."
  :atlas/impl (fn [_] (llm-context))})

;; Intent: refactor

(cid/register!
 :fn.ide/aspect-impact :atlas/execution-function
 #{:domain/ide :intent/refactor :meta/ide-aspect-impact}
 {:execution-function/context [:query/aspect]
  :execution-function/response [:aspect-impact/results]
  :execution-function/deps #{}
  :atlas/docs "Analyze the impact of removing or changing an aspect: how many entities use it, which domains are affected, and what compound identities would change. Use this before refactoring aspects."
  :atlas/impl #(aspect-impact (:query/aspect %))})

(cid/register!
 :fn.ide/preview-refactor-aspect :atlas/execution-function
 #{:domain/ide :intent/refactor :meta/ide-preview-refactor-aspect}
 {:execution-function/context [:refactor/old-aspect :refactor/new-aspect]
  :execution-function/response [:refactor/affected-count :refactor/conflicts :refactor/preview]
  :execution-function/deps #{}
  :atlas/docs "Preview renaming an aspect: shows affected entity count, potential compound-id conflicts, and a before/after preview. Use this to safely plan aspect renames."
  :atlas/impl #(preview-refactor-aspect (:refactor/old-aspect %) (:refactor/new-aspect %))})

;; Intent: query (protocols & components)

(cid/register!
 :fn.ide/list-protocols :atlas/execution-function
 #{:domain/ide :intent/query :meta/ide-list-protocols}
 {:execution-function/context []
  :execution-function/response [:protocol/list]
  :execution-function/deps #{}
  :atlas/docs "List all registered interface protocols. Returns dev-ids of entities with type :atlas/interface-protocol."
  :atlas/impl (fn [_] (list-protocols))})

(cid/register!
 :fn.ide/protocol-info :atlas/execution-function
 #{:domain/ide :intent/query :meta/ide-protocol-info}
 {:execution-function/context [:protocol/id]
  :execution-function/response [:protocol/info]
  :execution-function/deps #{}
  :atlas/docs "Get detailed information about a specific protocol: its methods, implementing components, and aspects."
  :atlas/impl #(protocol-info (:protocol/id %))})

(cid/register!
 :fn.ide/component-protocols :atlas/execution-function
 #{:domain/ide :intent/query :meta/ide-component-protocols}
 {:execution-function/context [:component/id]
  :execution-function/response [:protocol/list]
  :execution-function/deps #{}
  :atlas/docs "List all protocols that a given component implements. Use this to understand a component's interface surface."
  :atlas/impl #(component-protocols (:component/id %))})

(cid/register!
 :fn.ide/components-implementing :atlas/execution-function
 #{:domain/ide :intent/query :meta/ide-components-implementing}
 {:execution-function/context [:protocol/id]
  :execution-function/response [:component/list]
  :execution-function/deps #{}
  :atlas/docs "Find all components that implement a given protocol. Use this to discover available implementations."
  :atlas/impl #(components-implementing (:protocol/id %))})

;; Intent: query (business layer)

(cid/register!
 :fn.ide/list-business-entities :atlas/execution-function
 #{:domain/ide :intent/query :meta/ide-list-business-entities}
 {:execution-function/context []
  :execution-function/response [:business/entities]
  :execution-function/deps #{}
  :atlas/docs "List all business-layer entities (data schemas tagged as business entities). Use this to see the domain model."
  :atlas/impl (fn [_] (list-business-entities))})

(cid/register!
 :fn.ide/business-entity-info :atlas/execution-function
 #{:domain/ide :intent/query :meta/ide-business-entity-info}
 {:execution-function/context [:entity/dev-id]
  :execution-function/response [:business/info]
  :execution-function/deps #{}
  :atlas/docs "Get detailed info about a business entity: its fields, relationships, and domain aspects."
  :atlas/impl #(business-entity-info (:entity/dev-id %))})

(cid/register!
 :fn.ide/entities-implementing :atlas/execution-function
 #{:domain/ide :intent/query :meta/ide-entities-implementing}
 {:execution-function/context [:query/aspect]
  :execution-function/response [:entity/dev-id-list]
  :execution-function/deps #{}
  :atlas/docs "Find all entities that implement a given aspect (alias for entities-with-aspect, focused on business domain queries)."
  :atlas/impl #(entities-implementing (:query/aspect %))})

(cid/register!
 :fn.ide/business-aspects-of :atlas/execution-function
 #{:domain/ide :intent/query :meta/ide-business-aspects-of}
 {:execution-function/context [:entity/dev-id]
  :execution-function/response [:business/aspects]
  :execution-function/deps #{}
  :atlas/docs "Get the business-relevant aspects of an entity (filtering out infrastructure aspects like :meta/*, :atlas/*). Use this to see an entity's semantic meaning."
  :atlas/impl #(business-aspects-of (:entity/dev-id %))})

;; Intent: query (explorer)

(cid/register!
 :fn.ide/explorer-filter-entities :atlas/execution-function
 #{:domain/ide :intent/query :meta/ide-explorer-filter-entities}
 {:execution-function/context [:query/aspects-and :query/aspects-or]
  :execution-function/response [:explorer/results]
  :execution-function/deps #{}
  :atlas/docs "Filter entities by AND/OR aspect criteria with similarity scoring. Entities matching all AND-aspects are ranked highest, then those matching any OR-aspects. Use this for the visual explorer's faceted search."
  :atlas/impl #(explorer-filter-entities (:query/aspects-and %) (:query/aspects-or %))})

(cid/register!
 :fn.ide/similar-with-diff :atlas/execution-function
 #{:domain/ide :intent/query :meta/ide-similar-with-diff}
 {:execution-function/context [:entity/compound-identity]
  :execution-function/response [:similarity/results]
  :execution-function/deps #{}
  :atlas/docs "Find entities similar to a compound identity, showing which aspects are shared and which differ. Results sorted by similarity. Use this to find near-duplicates or related patterns."
  :atlas/impl #(similar-with-diff (:entity/compound-identity %))})

;; Intent: completion

(cid/register!
 :fn.ide/complete-dev-id :atlas/execution-function
 #{:domain/ide :intent/completion :meta/ide-complete-dev-id}
 {:execution-function/context [:completion/prefix]
  :execution-function/response [:completion/candidates]
  :execution-function/deps #{}
  :atlas/docs "Autocomplete a dev-id from a prefix string. Returns matching dev-ids sorted alphabetically. Use this for IDE autocomplete in dev-id input fields."
  :atlas/impl #(complete-dev-id (:completion/prefix %))})

(cid/register!
 :fn.ide/complete-aspect :atlas/execution-function
 #{:domain/ide :intent/completion :meta/ide-complete-aspect}
 {:execution-function/context [:completion/prefix]
  :execution-function/response [:completion/candidates]
  :execution-function/deps #{}
  :atlas/docs "Autocomplete an aspect from a prefix string. Returns matching aspect keywords sorted alphabetically. Use this for IDE autocomplete in aspect input fields."
  :atlas/impl #(complete-aspect (:completion/prefix %))})

(cid/register!
 :fn.ide/complete-data-key :atlas/execution-function
 #{:domain/ide :intent/completion :meta/ide-complete-data-key}
 {:execution-function/context [:completion/prefix]
  :execution-function/response [:completion/candidates]
  :execution-function/deps #{}
  :atlas/docs "Autocomplete a data key from a prefix string. Searches across all endpoint context/response keys. Use this for IDE autocomplete in data key input fields."
  :atlas/impl #(complete-data-key (:completion/prefix %))})

;; Intent: testing (template generation)

(cid/register!
 :fn.ide/exec-fn-test-template :atlas/execution-function
 #{:domain/ide :intent/testing :meta/ide-exec-fn-test-template}
 {:execution-function/context [:entity/dev-id]
  :execution-function/response [:template/code]
  :execution-function/deps #{}
  :atlas/docs "Generate a Clojure REPL test template for an execution-function. Shows components from integrant dev/*system*, dep impls from registry :atlas/impl, sample context, and the execute call. All sections are editable."
  :atlas/impl #(exec-fn-test-template (:entity/dev-id %))})

;; History registrations are in atlas.ide.history (loaded via require)
;; Trace registrations are in atlas.ide.trace (loaded via require)

;; =============================================================================
;; IDE TOOL ADAPTER — dispatch to registered :fn.ide/* functions by dev-id
;; =============================================================================

(defn handle-tool
  "Dispatch to a registered IDE tool by dev-id.

   Each :atlas/impl accepts a map keyed by :execution-function/context keywords.

   Input: {:tool/name :fn.ide/entity-info
           :tool/args {:entity/dev-id :fn/foo}}

   Returns: {:success? boolean
             :data {...} or :error {...}}"
  [{:tool/keys [name args]}]
  (let [dev-id (ensure-keyword name)]
    (if-let [handler (:atlas/impl (rt/props-for dev-id))]
      (try
        {:success? true
         :data (handler (or args {}))}
        (catch #?(:clj Exception :cljs js/Error) e
          {:success? false
           :error {:message #?(:clj (.getMessage e) :cljs (.-message e))
                   :tool dev-id}}))
      {:success? false
       :error {:message "Unknown IDE tool"
              :tool dev-id
              :available (vec (entities-with-aspect :domain/ide))}})))

(defn available-ide-tools
  "List all registered IDE tools with their context/response specs."
  []
  (->> (entities-with-aspect :domain/ide)
       (map (fn [dev-id]
              (let [props (rt/props-for dev-id)]
                {:tool/name dev-id
                 :tool/context (:execution-function/context props)
                 :tool/response (:execution-function/response props)})))
       vec))

;; =============================================================================
;; DEPRECATION — direct function calls are deprecated in favor of handle-tool
;; =============================================================================
;; Clients should use (handle-tool {:tool/name :fn.ide/entity-info :tool/args {...}})
;; instead of calling (entity-info ...) directly.

(doseq [v [#'list-entity-types #'list-entity-types-with-counts #'list-entities-of-type
            #'registered-entity? #'entity-type-ontology-keys #'entity-prop-keys
            #'entity-prop-items #'list-all-entities #'list-aspects #'list-aspect-namespaces
            #'list-aspect-names-in-namespace #'entities-with-aspect #'entity-info
            #'entities-info #'inspect-entity #'entity-doc #'suggest-aspects
            #'semantic-similarity #'validate-identity
            #'data-flow #'dependents-of #'dependencies-of #'recursive-dependencies-of
            #'recursive-dependencies-summary #'recursive-dependents-of
            #'recursive-dependents-summary #'producers-of #'consumers-of
            #'trace-data-flow #'impact-of-change #'execution-order
            #'check-invariants #'validate-entity #'domain-coupling #'pii-surface
            #'error-handler-coverage
            #'system-summary #'generate-markdown #'aspect-catalog #'list-templates
            #'by-tier #'architecture-view #'operations-view #'llm-context
            #'aspect-impact #'preview-refactor-aspect
            #'list-protocols #'protocol-info #'component-protocols #'components-implementing
            #'list-business-entities #'business-entity-info #'entities-implementing
            #'business-aspects-of
            #'explorer-filter-entities #'similar-with-diff
            #'complete-dev-id #'complete-aspect #'complete-data-key
            #'history-init! #'history-versions #'history-entity-timeline
            #'history-full-timeline #'history-version-diff #'history-version-diff-full
            #'history-version-summary #'history-vocabulary-diff #'history-edges-at
            #'history-dependents-of #'history-edge-diff #'history-edge-summary
            #'history-snapshot! #'history-get-conn]]
  (alter-meta! v assoc :deprecated "Use (handle-tool {:tool/name :fn.ide/... :tool/args {...}}) instead."))
