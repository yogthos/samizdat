(ns atlas.invariant
  "Fixed invariants that understand dataflow markers and terminal outputs."
  (:require [atlas.registry :as cid]
            [atlas.registry.graph :as graph]
            [atlas.query :as q]
            [clojure.set :as set]))

;; =============================================================================
;; HELPERS
;; =============================================================================

(defn all-dev-ids []
  (map #(:atlas/dev-id (second %)) (cid/current-registry)))

(defn all-context-keys []
  (->> (cid/current-registry) vals (mapcat :interface-endpoint/context) (remove nil?) set))

(defn all-response-keys []
  (->> (cid/current-registry) vals (mapcat :interface-endpoint/response) (remove nil?) set))

(defn endpoint-context-keys
  "Keys that come from outside (endpoint inputs)."
  []
  (->> (cid/current-registry)
       (filter (fn [[id _]] (contains? id :atlas/interface-endpoint)))
       (mapcat (fn [[_ v]] (:interface-endpoint/context v)))
       set))

(defn endpoint-response-keys
  "Keys that go to the client (endpoint outputs) - terminal by design."
  []
  (->> (cid/current-registry)
       (filter (fn [[id _]] (contains? id :atlas/interface-endpoint)))
       (mapcat (fn [[_ v]] (:interface-endpoint/response v)))
       set))

(defn display-output-keys
  "Keys marked as :dataflow/display-output - terminal by design."
  []
  (->> (cid/current-registry)
       (filter (fn [[id _]] (contains? id :dataflow/display-output)))
       (map (fn [[id _]]
              ;; The key IS the dev-id for data-keys
              (:atlas/dev-id (cid/fetch id))))
       (remove nil?)
       set))

(defn external-input-keys
  "Keys marked as :dataflow/external-input - no internal producer expected."
  []
  (->> (cid/current-registry)
       (filter (fn [[id _]] (contains? id :dataflow/external-input)))
       (map (fn [[id _]] (:atlas/dev-id (cid/fetch id))))
       (remove nil?)
       set))

;; =============================================================================
;; FIXED DATA FLOW AXIOMS
;; =============================================================================

(defn invariant-context-satisfiable
  "Every context key must be either:
   - An endpoint input (comes from client)
   - Marked as :dataflow/external-input
   - Produced by some function"
  []
  (let [endpoint-inputs (endpoint-context-keys)
        external-inputs (external-input-keys)
        produced (all-response-keys)
        available (set/union endpoint-inputs external-inputs produced)
        all-needed (all-context-keys)
        unsatisfied (set/difference all-needed available)]
    (when (seq unsatisfied)
      {:invariant :context-satisfiable
       :violation :unsatisfied-context
       :missing unsatisfied
       :severity :error
       :message (str "These context keys are needed but never produced: " unsatisfied)})))

(defn invariant-no-orphan-responses
  "Response keys should be consumed OR be terminal outputs.
   
   A key is NOT orphan if any of:
   - Consumed by another function's context
   - Appears in an endpoint response (goes to client)
   - Marked as :dataflow/display-output (UI-bound)
   
   This catches truly dead code while allowing legitimate terminal outputs."
  []
  (let [produced (all-response-keys)
        consumed (all-context-keys)
        endpoint-outputs (endpoint-response-keys)
        display-outputs (display-output-keys)
        ;; Terminal = goes somewhere outside the internal dataflow
        terminal (set/union endpoint-outputs display-outputs)
        ;; Orphan = produced but neither consumed internally nor terminal
        orphans (-> produced
                    (set/difference consumed)
                    (set/difference terminal))]
    (when (seq orphans)
      {:invariant :no-orphan-responses
       :violation :orphan-outputs
       :orphans orphans
       :severity :warning
       :message (str "These response keys are produced but never consumed or displayed: " orphans)})))

;; NOTE: invariant-no-orphan-responses-strict has been replaced by
;; invariant-internal-fn-outputs-consumed in atlas.ontology.execution-function.
;; The new version uses ontology/response-for instead of hardcoded keys.

;; =============================================================================
;; GRAPH + COMPONENT INVARIANTS (RE-EXPORTS)
;; =============================================================================

(def invariant-deps-exist graph/invariant-deps-exist)
(def invariant-no-circular-deps graph/invariant-no-circular-deps)
(def graph-invariants graph/graph-invariants)

;; NOTE: invariant-all-fns-reachable has been moved to
;; atlas.ontology.interface-endpoint namespace.
;; It is registered automatically when the IE ontology is loaded:
;;   (require '[atlas.ontology.interface-endpoint :as ie])
;;   (ie/load!)

;; NOTE: Component invariants have been moved to atlas.ontology.structure-component:
;;   - invariant-components-are-foundation
;;   - invariant-protocol-exists
;;   - invariant-protocol-conformance
;; They are registered automatically when the SC ontology is loaded:
;;   (require '[atlas.ontology.structure-component :as sc])
;;   (sc/load!)

;; =============================================================================
;; TIER AXIOMS (unchanged)
;; =============================================================================

;; NOTE: invariant-endpoints-are-api-tier has been moved to
;; atlas.ontology.interface-endpoint namespace.
;; It is registered automatically when the IE ontology is loaded:
;;   (require '[atlas.ontology.interface-endpoint :as ie])
;;   (ie/load!)

;; =============================================================================
;; SEMANTIC CONSISTENCY AXIOMS
;; =============================================================================

;; NOTE: Execution-function specific invariants have been moved to
;; atlas.ontology.execution-function namespace:
;;   - invariant-pure-has-no-deps
;;   - invariant-external-is-async
;;
;; They are registered automatically when the EF ontology is loaded:
;;   (require '[atlas.ontology.execution-function :as ef])
;;   (ef/load!)

;; =============================================================================
;; ONTOLOGY INVARIANT DISCOVERY
;; =============================================================================
;;
;; Ontology modules register their invariants as :atlas/invariant entities
;; in the registry. They are discovered at check time by querying the registry.

;; =============================================================================
;; CHECK ALL
;; =============================================================================

(def core-invariants
  "Core invariants that are always checked (not ontology-specific)."
  [;; Structural
   invariant-deps-exist
   invariant-no-circular-deps
   ;; Dataflow
   invariant-context-satisfiable
   invariant-no-orphan-responses])

(defn all-invariants
  "All invariants including core and ontology-contributed invariants.
   Ontology invariants are discovered by querying the registry for :atlas/invariant entities."
  []
  (let [ontology-invs (->> (q/find-by-aspect (cid/current-registry) :atlas/invariant)
                           vals
                           (map :invariant/fn)
                           (remove nil?))]
    (into core-invariants ontology-invs)))

(defn- result-level [result]
  (or (:level result) (:severity result)))

(defn- normalize-result
  "Ensure invariant results carry both :level and :severity."
  [result]
  (cond-> result
    (and (:severity result) (not (:level result)))
    (assoc :level (:severity result))
    (and (:level result) (not (:severity result)))
    (assoc :severity (:level result))))

(defn check
  "Run provided invariants and return {:valid? :errors :warnings :violations}."
  [invariants]
  (let [results (keep #(%) invariants)
        normalized (map normalize-result results)
        errors (filter #(= :error (result-level %)) normalized)
        warnings (filter #(= :warning (result-level %)) normalized)]
    {:violations normalized
     :errors errors
     :warnings warnings
     :violations-flat normalized
     :errors-flat errors
     :warnings-flat warnings
     :valid? (empty? errors)}))

(defn check-all
  "Run all invariants, return {:valid? :errors :warnings :violations}."
  []
  (check (all-invariants)))

(defn check-against
  "Run all currently-registered invariants against a specific registry map.
   Invariant fns are collected from the live registry first, then executed
   with *registry-override* bound to reg — so each fn sees reg, not @registry."
  [reg]
  (let [invs (all-invariants)]
    (binding [cid/*registry-override* reg]
      (check invs))))

(defn report
  "Print human-readable invariant report."
  []
  (let [{:keys [errors warnings valid?]} (check-all)]
    (println "\n=== AXIOM VALIDATION REPORT ===\n")
    (if valid?
      (println "✓ All error-level invariants pass")
      (do
        (println "✗ ERRORS:")
        (doseq [e errors]
          (println "  -" (:invariant e) ":" (:message e)))))
    (when (seq warnings)
      (println "\n⚠ WARNINGS:")
      (doseq [w warnings]
        (println "  -" (:invariant w) ":" (:message w))))
    (println "\nTotal:" (+ (count errors) (count warnings))
             "issues (" (count errors) "errors," (count warnings) "warnings)")
    valid?))
