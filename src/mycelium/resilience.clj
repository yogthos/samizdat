(ns mycelium.resilience
  "Resilience policies for Mycelium cells.
   Native jolt reimplementation of the upstream resilience4j-backed namespace:
   same public API (wrap-handler, validate-resilience!), same policy config
   keys, same :mycelium/resilience-error data contract. Policies are small
   atom-based state machines instead of JVM library instances; failure types
   travel as ::type in ex-data rather than as Java exception classes, so no
   unwrapping of ExecutionException is ever needed — nothing here wraps.

   Each policy is built once by its make-* constructor (state lives in the
   closure, so a pre-compiled workflow shares breaker windows, bulkhead
   counters and limiter tokens across runs, matching upstream) and returns an
   executor: a function that runs a per-call thunk under the policy."
  (:require [clojure.set :as set]))

(defn- now-ms [] (System/currentTimeMillis))

(defn- policy-error [type msg]
  (ex-info msg {::type type}))

;; ===== Policy executors =====

(defn- make-timeout
  "Bounds the thunk to :timeout-ms by running it on its own thread. The
   runaway thread is abandoned, not cancelled — the FSM routes on the timeout
   regardless of what it eventually does."
  [{:keys [timeout-ms]}]
  (fn [thunk]
    (let [fut (future (try {:ok (thunk)} (catch Throwable t {:err t})))
          v   (deref fut timeout-ms ::timeout)]
      (cond
        (= ::timeout v) (throw (policy-error :timeout (str "did not complete within " timeout-ms "ms")))
        (:err v)        (throw (:err v))
        :else           (:ok v)))))

(defn- make-retry
  "Retries the thunk on any thrown error. :max-attempts counts the first call,
   matching resilience4j (max-attempts 5 with success on call 3 => 3 calls)."
  [{:keys [max-attempts wait-ms]}]
  (let [max-attempts (or max-attempts 3)
        wait-ms      (or wait-ms 500)]
    (fn [thunk]
      (loop [attempt 1]
        (let [r (try {:ok (thunk)} (catch Throwable t {:err t}))]
          (cond
            (nil? (:err r))          (:ok r)
            (< attempt max-attempts) (do (Thread/sleep wait-ms)
                                         (recur (inc attempt)))
            :else                    (throw (:err r))))))))

(defn- cb-record
  "Pure transition on circuit-breaker state after a call outcome."
  [{:keys [state window] :as cb} {:keys [failure-rate sliding-window-size minimum-calls]} failed?]
  (let [failure-rate        (or failure-rate 50)
        sliding-window-size (or sliding-window-size 100)
        minimum-calls       (or minimum-calls 10)]
    (case state
      :half-open (if failed?
                   {:state :open :window [] :opened-at (now-ms)}
                   {:state :closed :window [] :opened-at nil})
      ;; :closed (calls never record while :open — they fail fast)
      (let [window (vec (take-last sliding-window-size (conj window failed?)))
            rate   (* 100.0 (/ (count (filter true? window)) (count window)))]
        (if (and (>= (count window) minimum-calls)
                 (>= rate failure-rate))
          {:state :open :window [] :opened-at (now-ms)}
          (assoc cb :window window))))))

(defn- make-circuit-breaker
  "Count-based sliding-window breaker. Opens when the failure rate over the
   window reaches :failure-rate percent with at least :minimum-calls recorded;
   while open, calls fail fast without running the thunk; after
   :wait-in-open-ms one trial call runs half-open and closes or re-opens it."
  [{:keys [wait-in-open-ms] :as cfg}]
  (let [wait-in-open-ms (or wait-in-open-ms 60000)
        cb (atom {:state :closed :window [] :opened-at nil})]
    (fn [thunk]
      (let [{:keys [state opened-at]} @cb]
        (when (= :open state)
          (if (>= (- (now-ms) opened-at) wait-in-open-ms)
            (swap! cb assoc :state :half-open)
            (throw (policy-error :circuit-open "circuit breaker is open"))))
        (let [r (try {:ok (thunk)} (catch Throwable t {:err t}))]
          (swap! cb cb-record cfg (some? (:err r)))
          (if-let [e (:err r)] (throw e) (:ok r)))))))

(defn- make-bulkhead
  "Caps concurrent executions at :max-concurrent. A caller that cannot acquire
   a slot within :max-wait-ms (default 0 — fail immediately) is rejected."
  [{:keys [max-concurrent max-wait-ms]}]
  (let [max-concurrent (or max-concurrent 25)
        max-wait-ms    (or max-wait-ms 0)
        in-flight      (atom 0)
        try-acquire!   (fn []
                         (loop []
                           (let [n @in-flight]
                             (cond
                               (>= n max-concurrent)                  false
                               (compare-and-set! in-flight n (inc n)) true
                               :else                                  (recur)))))]
    (fn [thunk]
      (let [deadline (+ (now-ms) max-wait-ms)]
        (loop []
          (cond
            (try-acquire!)
            (try (thunk) (finally (swap! in-flight dec)))

            (< (now-ms) deadline)
            (do (Thread/sleep 5) (recur))

            :else
            (throw (policy-error :bulkhead-full
                                 (str "bulkhead full: " max-concurrent " concurrent calls")))))))))

(defn- make-rate-limiter
  "Token-per-period limiter: at most :limit-for-period calls per
   :limit-refresh-period-ms window; a caller waits up to :timeout-ms for the
   next window before being rejected."
  [{:keys [limit-for-period limit-refresh-period-ms timeout-ms]}]
  (let [limit      (or limit-for-period 50)
        period-ms  (or limit-refresh-period-ms 500)
        timeout-ms (or timeout-ms 5000)
        state      (atom {:period-start (now-ms) :count 0})
        try-acquire! (fn []
                       (let [{:keys [count]}
                             (swap! state (fn [{:keys [period-start] :as s}]
                                            (if (>= (- (now-ms) period-start) period-ms)
                                              {:period-start (now-ms) :count 1}
                                              (update s :count inc))))]
                         (<= count limit)))]
    (fn [thunk]
      (let [deadline (+ (now-ms) timeout-ms)]
        (loop []
          (cond
            (try-acquire!)        (thunk)
            (< (now-ms) deadline) (do (Thread/sleep 5) (recur))
            :else (throw (policy-error :rate-limited
                                       (str "rate limit exceeded: " limit " per " period-ms "ms")))))))))

;; ===== Error classification =====

(defn- classify-error
  "Classifies a policy failure into a :mycelium/resilience-error map."
  [cell-name e]
  (let [msg (ex-message e)]
    (case (::type (ex-data e))
      :timeout       {:type :timeout :cell cell-name :message msg}
      :circuit-open  {:type :circuit-open :cell cell-name :message msg}
      :bulkhead-full {:type :bulkhead-full :cell cell-name :message msg}
      :rate-limited  {:type :rate-limited :cell cell-name :message msg}
      {:type :unknown :cell cell-name :message msg
       :exception-type (str (type e))})))

;; ===== Handler wrapping =====

(def ^:private default-async-timeout-ms
  "Default timeout in ms for blocking on async handler promises."
  30000)

(defn- invoke-handler-sync
  "Invokes a handler synchronously. For async (4-arity) handlers, blocks on a promise.
   For sync (2-arity) handlers, calls directly.
   `async-timeout-ms` controls how long to wait for the async promise (default 30s)."
  [handler async? resources data async-timeout-ms]
  (if async?
    (let [p (promise)]
      (handler resources data
               (fn [result] (deliver p {:ok result}))
               (fn [error]  (deliver p {:error error})))
      (let [v (deref p (or async-timeout-ms default-async-timeout-ms)
                     {:error (ex-info "Async cell timed out in resilience wrapper" {})})]
        (if (:error v)
          (throw (if (instance? Throwable (:error v))
                   (:error v)
                   (ex-info (str (:error v)) {})))
          (:ok v))))
    (handler resources data)))

(defn wrap-handler
  "Wraps a cell handler with resilience policies.
   `cell-name` — the workflow cell name (for error reporting).
   `policies` — map of policy configs, e.g. {:timeout {:timeout-ms 5000}, :retry {:max-attempts 3}}.
   `opts` — optional map with :async? flag for async handlers.
   Returns a wrapped handler that catches resilience failures
   and returns data with :mycelium/resilience-error.
   Supports both sync (2-arity) and async (4-arity) handlers."
  ([handler cell-name policies]
   (wrap-handler handler cell-name policies {}))
  ([handler cell-name policies opts]
   (let [async? (:async? opts)
         async-timeout-ms (:async-timeout-ms policies)
         ;; Executors are built once here so their state is shared across runs
         ;; of a pre-compiled workflow. Applied innermost-first: circuit-breaker
         ;; → bulkhead → rate-limiter → retry (so the breaker sees every retry
         ;; attempt); :timeout bounds the whole chain, retries included.
         executors (->> [(when-let [cfg (:circuit-breaker policies)] (make-circuit-breaker cfg))
                         (when-let [cfg (:bulkhead policies)]        (make-bulkhead cfg))
                         (when-let [cfg (:rate-limiter policies)]    (make-rate-limiter cfg))
                         (when-let [cfg (:retry policies)]           (make-retry cfg))
                         (when-let [cfg (:timeout policies)]         (make-timeout cfg))]
                        (remove nil?))
         invoke (fn [resources data]
                  (try
                    ((reduce (fn [thunk executor] #(executor thunk))
                             #(invoke-handler-sync handler async? resources data async-timeout-ms)
                             executors))
                    (catch Throwable e
                      (assoc data :mycelium/resilience-error
                             (classify-error cell-name e)))))]
     (fn
       ([resources data]
        (invoke resources data))
       ([resources data callback _error-callback]
        (future (callback (invoke resources data)))
        nil)))))

;; ===== Validation =====

(def ^:private valid-policy-keys
  #{:timeout :retry :circuit-breaker :bulkhead :rate-limiter :async-timeout-ms})

(defn validate-resilience!
  "Validates the :resilience map in a workflow definition.
   Each key must be a cell name in the cells map, each value a map of policy configs."
  [resilience-map cells]
  (let [cell-names (set (keys cells))]
    (doseq [[cell-name policies] resilience-map]
      (when-not (contains? cell-names cell-name)
        (throw (ex-info (str "Resilience policy references cell " cell-name
                             " which is not in :cells")
                        {:cell-name cell-name :valid-cells cell-names})))
      (when-not (map? policies)
        (throw (ex-info (str "Resilience policies for " cell-name " must be a map")
                        {:cell-name cell-name :policies policies})))
      (let [unknown (set/difference (set (keys policies)) valid-policy-keys)]
        (when (seq unknown)
          (throw (ex-info (str "Unknown resilience policy keys for " cell-name ": " unknown)
                          {:cell-name cell-name :unknown-keys unknown
                           :valid-keys valid-policy-keys}))))
      (when-let [timeout (:timeout policies)]
        (when-not (and (:timeout-ms timeout) (pos? (:timeout-ms timeout)))
          (throw (ex-info (str "Resilience :timeout for " cell-name
                               " requires positive :timeout-ms")
                          {:cell-name cell-name :timeout timeout}))))
      (when-let [async-timeout (:async-timeout-ms policies)]
        (when-not (and (integer? async-timeout) (pos? async-timeout))
          (throw (ex-info (str "Resilience :async-timeout-ms for " cell-name
                               " must be a positive integer")
                          {:cell-name cell-name :async-timeout-ms async-timeout})))))))
