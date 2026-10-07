(ns atlas.docs
  "Generate documentation from semantic registry.
   Demonstrates that semantic structure enables self-documenting systems."
  (:require [atlas.registry :as cid]
            [atlas.registry.lookup :as rt]
            [atlas.ontology :as ontology]
            [atlas.invariant :as ax]
            [clojure.string :as str]
            [clojure.set :as set]))

;; =============================================================================
;; HELPERS
;; =============================================================================

(defn- aspects-of [dev-id]
  (when-let [id (rt/identity-for dev-id)]
    (disj id :atlas/execution-function :atlas/structure-component
          :atlas/interface-endpoint :atlas/schema)))

(defn- domain-of [dev-id]
  (when-let [id (rt/identity-for dev-id)]
    (first (filter #(= "domain" (namespace %)) id))))

(defn- tier-of [dev-id]
  (when-let [id (rt/identity-for dev-id)]
    (first (filter #(= "tier" (namespace %)) id))))

(defn- type-of [dev-id]
  (when-let [id (rt/identity-for dev-id)]
    (cond
      (contains? id :atlas/interface-endpoint) :endpoint
      (contains? id :atlas/execution-function) :function
      (contains? id :atlas/structure-component) :component
      (contains? id :atlas/schema) :schema
      :else :unknown)))

;; =============================================================================
;; SYSTEM OVERVIEW
;; =============================================================================

(defn system-overview []
  (let [components (rt/all-with-aspect :atlas/structure-component)
        functions (rt/all-with-aspect :atlas/execution-function)
        endpoints (rt/all-with-aspect :atlas/interface-endpoint)
        schemas (rt/all-with-aspect :atlas/schema)
        domains (->> (concat components functions endpoints)
                     (map domain-of)
                     (remove nil?)
                     distinct
                     sort)]
    {:title "System Overview"
     :summary (format "This system has %d components, %d functions, %d endpoints, and %d schemas across %d domains."
                      (count components) (count functions) (count endpoints) (count schemas) (count domains))
     :components (sort components)
     :functions (sort functions)
     :endpoints (sort endpoints)
     :schemas (sort schemas)
     :domains (vec domains)}))

;; =============================================================================
;; API DOCUMENTATION
;; =============================================================================

(defn endpoint-doc [dev-id]
  (let [props (rt/props-for dev-id)
        context (:interface-endpoint/context props)
        response (:interface-endpoint/response props)
        deps (:execution-function/deps props)]
    {:endpoint dev-id
     :description (format "API endpoint in domain %s" (domain-of dev-id))
     :inputs {:required (vec context)
              :description "These keys must be provided in the request"}
     :outputs {:entity/produces (vec response)
               :description "These keys will be in the response"}
     :implementation {:calls (vec deps)
                      :description "Internal functions invoked by this endpoint"}}))

(defn api-documentation []
  (let [endpoints (sort (rt/all-with-aspect :atlas/interface-endpoint))]
    {:title "API Documentation"
     :endpoints (mapv endpoint-doc endpoints)}))

;; =============================================================================
;; DATA FLOW DOCUMENTATION  
;; =============================================================================

(defn data-flow-doc [dev-id]
  (let [props (rt/props-for dev-id)
        context (:interface-endpoint/context props)
        response (:interface-endpoint/response props)
        trace (ontology/trace-data-flow dev-id)]
    {:function dev-id
     :needs context
     :entity/produces response
     :data-sources (for [{:keys [needs produced-by satisfied?]} trace]
                     {:key needs
                      :from (if (seq produced-by) (first produced-by) :endpoint-input)
                      :satisfied? (if (seq produced-by) satisfied? :assumed-input)})}))

(defn data-flow-documentation []
  (let [fns (sort (rt/all-with-aspect :atlas/execution-function))
        all-context (ax/all-context-keys)
        all-response (ax/all-response-keys)
        endpoint-inputs (ax/endpoint-context-keys)]
    {:title "Data Flow Documentation"
     :summary "How data moves through the system"
     :endpoint-inputs {:keys (vec (sort endpoint-inputs))
                       :description "These keys are expected from API callers"}
     :internal-data {:keys (vec (sort (set/difference all-context endpoint-inputs)))
                     :description "These keys are produced and consumed internally"}
     :final-outputs {:keys (vec (sort all-response))
                     :description "These keys are produced by the system"}
     :functions (mapv data-flow-doc fns)}))

;; =============================================================================
;; DEPENDENCY DOCUMENTATION
;; =============================================================================

(defn dependency-doc [dev-id]
  (let [deps (ontology/deps-for dev-id)
        dependents (->> (rt/all-with-aspect :atlas/execution-function)
                        (concat (rt/all-with-aspect :atlas/interface-endpoint))
                        (filter #(contains? (set (ontology/deps-for %)) dev-id)))]
    {:entity dev-id
     :type (type-of dev-id)
     :depends-on (vec (sort deps))
     :depended-by (vec (sort dependents))}))

(defn dependency-documentation []
  (let [all-ids (sort (concat (rt/all-with-aspect :atlas/structure-component)
                              (rt/all-with-aspect :atlas/execution-function)
                              (rt/all-with-aspect :atlas/interface-endpoint)))]
    {:title "Dependency Documentation"
     :summary "What depends on what"
     :entities (mapv dependency-doc all-ids)}))

;; =============================================================================
;; ARCHITECTURE DOCUMENTATION
;; =============================================================================

(defn architecture-documentation []
  (let [by-tier (group-by tier-of (concat (rt/all-with-aspect :atlas/structure-component)
                                          (rt/all-with-aspect :atlas/execution-function)
                                          (rt/all-with-aspect :atlas/interface-endpoint)))
        by-domain (group-by domain-of (concat (rt/all-with-aspect :atlas/structure-component)
                                              (rt/all-with-aspect :atlas/execution-function)
                                              (rt/all-with-aspect :atlas/interface-endpoint)))]
    {:title "Architecture Documentation"
     :tiers (into {} (map (fn [[tier entities]]
                            [tier {:entities (vec (sort entities))
                                   :count (count entities)}])
                          by-tier))
     :domains (into {} (map (fn [[domain entities]]
                              [domain {:entities (vec (sort entities))
                                       :count (count entities)}])
                            by-domain))}))

;; =============================================================================
;; OPERATIONS DOCUMENTATION
;; =============================================================================

(defn operations-documentation []
  (let [external (rt/all-with-aspect :integration/external)
        pure (filter #(rt/has-aspect? % :effect/pure) (rt/all-with-aspect :atlas/execution-function))
        oauth (rt/all-with-aspect :protocol/oauth)]
    {:title "Operations Documentation"
     :external-integrations {:entities (vec (sort external))
                             :description "These components/functions call external services"
                             :operational-concerns ["May timeout" "Need retry logic" "Monitor latency"]}
     :pure-functions {:entities (vec (sort pure))
                      :description "These functions have no side effects"
                      :operational-concerns ["Safe to cache" "Safe to retry" "No external dependencies"]}
     :oauth-dependencies {:entities (vec (sort oauth))
                          :description "These use OAuth protocol"
                          :operational-concerns ["Token refresh needed" "Auth failures possible"]}}))

;; =============================================================================
;; AXIOM / HEALTH DOCUMENTATION
;; =============================================================================

(defn health-documentation []
  (let [{:keys [violations errors warnings valid?]} (ax/check-all)]
    {:title "System Health"
     :valid? valid?
     :summary (if valid?
                "All architectural constraints satisfied"
                (format "%d errors, %d warnings found" (count errors) (count warnings)))
     :errors (mapv #(select-keys % [:invariant :message]) errors)
     :warnings (mapv #(select-keys % [:invariant :message]) warnings)}))

;; =============================================================================
;; MARKDOWN GENERATION
;; =============================================================================

(defn- md-section [title content]
  (str "## " title "\n\n" content "\n\n"))

(defn- md-list [items]
  (str/join "\n" (map #(str "- " %) items)))

(defn- md-table [headers rows]
  (let [header-row (str "| " (str/join " | " headers) " |")
        separator (str "| " (str/join " | " (repeat (count headers) "---")) " |")
        data-rows (map #(str "| " (str/join " | " %) " |") rows)]
    (str/join "\n" (concat [header-row separator] data-rows))))

(defn generate-markdown []
  (let [overview (system-overview)
        api (api-documentation)
        data-flow (data-flow-documentation)
        arch (architecture-documentation)
        ops (operations-documentation)
        health (health-documentation)]
    (str
     "# Calendar Availability System\n\n"
     "*Auto-generated from semantic registry*\n\n"

     (md-section "Overview" (:summary overview))

     (md-section "Domains" (md-list (map name (:domains overview))))

     (md-section "Components (Foundation Layer)"
                 (md-list (:components overview)))

     (md-section "Functions (Service Layer)"
                 (md-list (:functions overview)))

     (md-section "API Endpoints"
                 (str/join "\n\n"
                           (for [{:keys [endpoint inputs outputs]} (:endpoints api)]
                             (str "### " endpoint "\n\n"
                                  "**Inputs:** " (str/join ", " (map str (:required inputs))) "\n\n"
                                  "**Outputs:** " (str/join ", " (map str (:entity/produces outputs)))))))

     (md-section "Data Flow"
                 (str "**Endpoint Inputs:** " (str/join ", " (map str (:keys (:endpoint-inputs data-flow)))) "\n\n"
                      "**Internal Data:** " (str/join ", " (map str (:keys (:internal-data data-flow)))) "\n\n"
                      "**Final Outputs:** " (str/join ", " (map str (:keys (:final-outputs data-flow))))))

     (md-section "External Integrations"
                 (str (:description (:external-integrations ops)) "\n\n"
                      (md-list (map str (:entities (:external-integrations ops)))) "\n\n"
                      "**Operational Concerns:**\n"
                      (md-list (:operational-concerns (:external-integrations ops)))))

     (md-section "System Health"
                 (str (if (:valid? health) "âœ… " "â�Œ ") (:summary health)
                      (when (seq (:errors health))
                        (str "\n\n**Errors:**\n" (md-list (map :message (:errors health)))))
                      (when (seq (:warnings health))
                        (str "\n\n**Warnings:**\n" (md-list (map :message (:warnings health))))))))))

;; =============================================================================
;; LLM-FRIENDLY EXPORT
;; =============================================================================

(defn export-for-llm []
  "Export all semantic information in a format optimized for LLM consumption."
  {:system (system-overview)
   :api (api-documentation)
   :data-flow (data-flow-documentation)
   :dependencies (dependency-documentation)
   :architecture (architecture-documentation)
   :operations (operations-documentation)
   :health (health-documentation)
   :raw-registry (into {} (map (fn [[k v]] [(:atlas/dev-id v) {:identity (vec k) :props (dissoc v :atlas/dev-id)}])
                               (cid/current-registry)))})

(def aspect-glossary
  "Human-readable descriptions of semantic aspects"
  {:integration/external "Calls external services outside our system boundary. Implies network latency, potential failures, need for timeouts and retries."
   :effect/pure "No side effects - output depends only on input. Safe to cache, memoize, or retry."
   :protocol/oauth "Uses OAuth 2.0 authentication flow. Requires token refresh handling."
   :tier/foundation "Infrastructure layer - databases, external clients, base services."
   :tier/service "Business logic layer - functions that implement domain operations."
   :tier/api "API layer - entry points that receive external requests."
   :domain/google "Google integration domain - Calendar, OAuth, etc."
   :domain/users "User management domain - profiles, preferences, credentials."
   :domain/scheduling "Scheduling domain - availability, calendar operations."})

(defn enrich-with-descriptions
  "Add human descriptions based on semantic aspects.
   An LLM can use aspects to generate appropriate descriptions."
  [dev-id]
  (let [id (rt/identity-for dev-id)
        aspects (disj id :atlas/execution-function :atlas/structure-component
                      :atlas/interface-endpoint :atlas/schema)
        props (rt/props-for dev-id)]
    {:dev-id dev-id
     :type (type-of dev-id)
     :aspects (vec aspects)
     :aspect-meanings (into {} (for [a aspects :when (aspect-glossary a)]
                                 [a (aspect-glossary a)]))
     :context (:interface-endpoint/context props)
     :response (:interface-endpoint/response props)
     :deps (:execution-function/deps props)
     ;; Prompt for LLM: generate description based on above
     :llm-prompt (str "Generate a one-sentence description for a "
                      (name (type-of dev-id))
                      " with aspects " (pr-str aspects)
                      " that takes " (pr-str (:interface-endpoint/context props))
                      " and produces " (pr-str (:interface-endpoint/response props)))}))

(defn llm-documentation-context
  "Full context an LLM needs to generate comprehensive docs"
  []
  {:system-purpose "Calendar availability system that finds users available on a given date who speak a given language"
   :aspect-glossary aspect-glossary
   :entities (mapv enrich-with-descriptions (ax/all-dev-ids))
   :data-flow (data-flow-documentation)
   :health (health-documentation)
   :architectural-rules
   ["Components are foundation tier - infrastructure only"
    "Functions are service tier - business logic"
    "Endpoints are API tier - external entry points"
    "External integrations get automatic timeout wrappers"
    "Pure functions can be cached safely"
    "OAuth functions must depend on OAuth components"
    "All context keys must have a producer or be endpoint inputs"]})

(defn print-markdown []
  (println (generate-markdown)))
