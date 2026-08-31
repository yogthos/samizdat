(ns mycelium.queue
  "Durable queue abstraction for asynchronous workflow execution.

  Provides a WorkQueue protocol that can be backed by in-memory storage
  (default, zero-config) or durable storage like Postgres (via Absurd or
  custom adapters).

  Default: `memory-queue` — in-memory, no durability, same process.
  For production durability, pass a custom implementation when creating
  workflows (e.g., an Absurd-backed adapter).

  jolt port note: upstream guarded a PriorityBlockingQueue with a
  ReentrantLock; here all queue state lives in one atom of pure data
  (tasks map + sorted ready-set) mutated through a compare-and-set loop,
  so every operation is atomic without host locks.")

;; ===== Protocol =====

(defprotocol WorkQueue
  (enqueue!
    [queue workflow-name data]
    [queue workflow-name data opts]
    "Enqueue a workflow run for asynchronous execution.

    `workflow-name` — keyword identifying the workflow definition.
    `data` — the initial data map for the workflow.
    `opts` — optional map:
      :run-at         — epoch-ms when the task becomes available (default: now)
      :max-attempts   — max retries before dead-letter (default: 1)

    Returns a UUID task-id.")

  (claim!
    [queue worker-id]
    "Atomically claim the next available task.

    Returns a task map with :task-id, :workflow-name, :data, :attempt, :worker-id,
    or nil if no tasks are available. Claim includes a lease — if the worker
    doesn't call heartbeat! or complete!/fail! within the lease timeout, the
    task becomes available for other workers.")

  (complete!
    [queue task-id worker-id result]
    "Mark a claimed task as completed successfully.
    Only succeeds if the worker-id matches the current claim holder
    and the lease has not expired. Otherwise it's a no-op.")

  (fail!
    [queue task-id worker-id error]
    "Mark a claimed task as failed.
    Only succeeds if the worker-id matches the current claim holder
    and the lease has not expired. Otherwise it's a no-op.
    If attempts < max-attempts, the task is re-queued for retry
    with exponential backoff (1s, 2s, 4s, ... capped at 60s).
    If attempts exhausted, the task is dead-lettered (removed from queue).
    `error` should be a Throwable or ex-info.")

  (heartbeat!
    [queue task-id worker-id]
    "Refresh the claim lease for a running task, preventing expiry.
    Only succeeds if the worker-id matches the current claim holder
    and the lease has not expired. Otherwise it's a no-op.")

  (claimed?
    [queue task-id worker-id]
    "Returns true if the task is currently claimed by worker-id
    and the lease has not expired. Used by workers to guard side
    effects (e.g., store writes) before completing/failing a task.")

  (queue-depth
    [queue]
    "Returns the number of pending tasks (including claimed but not yet
    completed/failed). Useful for monitoring.")

  (dead-lettered
    [queue]
    "Returns a vector of dead-lettered task entries.
    Each entry is a map with :task-id, :workflow-name, :data, :error, :failed-at.
    Entries are retained for inspection (callers decide when to purge)."))

;; ===== Helpers =====

(defn- now-ms []
  (System/currentTimeMillis))

(defn- new-uuid []
  (java.util.UUID/randomUUID))

(defn- backoff-ms
  "Exponential backoff for retries: 1s, 2s, 4s, 8s, ... capped at 60s."
  [attempt]
  (min (* 1000 (long (Math/pow 2 (dec attempt)))) 60000))

(defn- transact!
  "Applies pure transition f (state -> [state' ret]) to the state atom via a
   compare-and-set loop; returns ret. Every queue operation goes through here,
   so each is atomic over the whole queue state."
  [state f]
  (loop []
    (let [old @state
          [new ret] (f old)]
      (if (compare-and-set! state old new)
        ret
        (recur)))))

(defn- ready-entry [task]
  [(:run-at task) (:seq task) (:task-id task)])

(defn- enqueue-task [st task]
  (-> st
      (assoc-in [:tasks (:task-id task)] task)
      (update :ready conj (ready-entry task))))

(defn- retry-task
  "Re-queues task as :pending with the given attempt count and backoff."
  [st task attempt]
  (enqueue-task st (assoc task
                          :state :pending
                          :worker-id nil
                          :claimed-at nil
                          :claim-expires-at nil
                          :attempt attempt
                          :run-at (+ (now-ms) (backoff-ms attempt)))))

(defn- dead-letter-task [st task error max-dead-letters]
  (-> st
      (update :tasks dissoc (:task-id task))
      (update :dead-letters
              (fn [dls]
                (let [v (conj dls {:task-id       (:task-id task)
                                   :workflow-name (:workflow-name task)
                                   :data          (:data task)
                                   :error         error
                                   :failed-at     (now-ms)})]
                  (if (> (count v) max-dead-letters)
                    (subvec v (- (count v) max-dead-letters))
                    v))))))

(defn- drain-expired-claims
  "Expired leases go back to pending with backoff, or dead-letter when
   attempts are exhausted."
  [st max-dead-letters]
  (reduce
   (fn [st task]
     (let [next-attempt (inc (:attempt task))]
       (if (< next-attempt (:max-attempts task))
         (retry-task st task next-attempt)
         (dead-letter-task st task
                           (ex-info "Claim lease expired, max attempts exhausted" {})
                           max-dead-letters))))
   st
   (filter #(and (= :claimed (:state %))
                 (<= (:claim-expires-at %) (now-ms)))
           (vals (:tasks st)))))

(defn- claim-next
  "Walks the ready-set in [run-at seq] order: prunes entries whose task is
   gone or no longer pending, claims the first eligible one, stops at the
   first future run-at (the set is ordered, so nothing later is due either).
   Returns [st' claim-or-nil]."
  [st worker-id claim-timeout-ms]
  (let [now (now-ms)]
    (loop [st st]
      (if-let [[run-at _seq task-id :as entry] (first (:ready st))]
        (let [task (get-in st [:tasks task-id])]
          (cond
            ;; entry for a completed/dead-lettered/re-queued task — prune
            (or (nil? task) (not= entry (ready-entry task)))
            (recur (update st :ready disj entry))

            (> run-at now)
            [st nil]

            :else
            (let [claimed (assoc task
                                 :state :claimed
                                 :worker-id worker-id
                                 :claimed-at now
                                 :claim-expires-at (+ now claim-timeout-ms))]
              [(-> st
                   (update :ready disj entry)
                   (assoc-in [:tasks task-id] claimed))
               {:task-id       task-id
                :workflow-name (:workflow-name claimed)
                :data          (:data claimed)
                :attempt       (:attempt claimed)
                :worker-id     worker-id}])))
        [st nil]))))

(defn- holds-live-claim? [task worker-id]
  (and task
       (= :claimed (:state task))
       (= worker-id (:worker-id task))
       (< (now-ms) (:claim-expires-at task))))

(defn memory-queue
  "Creates an in-memory work queue.

  Tasks are ordered by :run-at (earliest first), then by insertion order.
  No durability — all state is lost on process restart.

  Suitable for development, testing, and single-process deployments.
  For production durability, provide a custom WorkQueue implementation.

  Implementation notes:
  - claim! runs an O(n) scan over all tasks to drain expired claims.
    Acceptable for modest queue sizes; a durable adapter backed by
    indexed rows would handle this more efficiently.
  - Long-running workflows MUST set :heartbeat-ms on the worker and
    :max-attempts > 1 to avoid premature dead-lettering (default
    claim-timeout-ms is 5 min, default max-attempts is 1).

  Options:
    :claim-timeout-ms — lease timeout in ms (default: 300000 = 5 min)
    :max-attempts     — default max retries (default: 1 = no retry)
    :max-dead-letters — max dead-letter entries retained (default: 10000)"
  ([]
   (memory-queue {}))
  ([{:keys [claim-timeout-ms max-attempts max-dead-letters]
     :or {claim-timeout-ms 300000
          max-attempts 1
          max-dead-letters 10000}}]
   (let [state (atom {:seq 0
                      :tasks {}
                      ;; [run-at seq task-id] triples ordered by run-at then
                      ;; insertion. The comparator must not reach task-id —
                      ;; jolt UUIDs don't compare — and [run-at seq] is unique
                      ;; per live entry (one entry per task, seq per enqueue).
                      :ready (sorted-set-by (fn [[ra sa _] [rb sb _]]
                                              (compare [ra sa] [rb sb])))
                      :dead-letters []})]
     (reify
       WorkQueue

       (enqueue! [this workflow-name data]
         (enqueue! this workflow-name data nil))

       (enqueue! [_ workflow-name data opts]
         (let [task-id (new-uuid)]
           (transact! state
                      (fn [st]
                        (let [task {:task-id          task-id
                                    :workflow-name    workflow-name
                                    :data             data
                                    :run-at           (or (:run-at opts) (now-ms))
                                    :max-attempts     (or (:max-attempts opts) max-attempts)
                                    :attempt          0
                                    :seq              (inc (:seq st))
                                    :state            :pending
                                    :worker-id        nil
                                    :claimed-at       nil
                                    :claim-expires-at nil}]
                          [(enqueue-task (update st :seq inc) task) task-id])))))

       (claim! [_ worker-id]
         (transact! state
                    (fn [st]
                      (-> st
                          (drain-expired-claims max-dead-letters)
                          (claim-next worker-id claim-timeout-ms)))))

       (complete! [_ task-id worker-id _result]
         (transact! state
                    (fn [st]
                      (if (holds-live-claim? (get-in st [:tasks task-id]) worker-id)
                        [(update st :tasks dissoc task-id) nil]
                        [st nil]))))

       (fail! [_ task-id worker-id error]
         (transact! state
                    (fn [st]
                      (let [task (get-in st [:tasks task-id])]
                        (if (holds-live-claim? task worker-id)
                          (let [attempts (inc (:attempt task))]
                            (if (< attempts (:max-attempts task))
                              [(retry-task st task attempts) nil]
                              [(dead-letter-task st task error max-dead-letters) nil]))
                          [st nil])))))

       (heartbeat! [_ task-id worker-id]
         (transact! state
                    (fn [st]
                      (let [task (get-in st [:tasks task-id])]
                        (if (holds-live-claim? task worker-id)
                          [(assoc-in st [:tasks task-id :claim-expires-at]
                                     (+ (now-ms) claim-timeout-ms))
                           nil]
                          [st nil])))))

       (claimed? [_ task-id worker-id]
         (holds-live-claim? (get-in @state [:tasks task-id]) worker-id))

       (queue-depth [_]
         (count (:tasks @state)))

       (dead-lettered [_]
         (vec (:dead-letters @state)))))))
