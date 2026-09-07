;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.events-flow-test
  "The event bus as a flow, and the supervisor as a reduce over it (RFC-013,
  karamazov-3cll.9). The bus keeps its two contracts by construction: a
  publisher never parks, and a slow watcher loses the oldest events rather
  than applying backpressure. The supervisor's stream is one task, a reduce
  over timed batches, cancelled with the run."
  (:require [clojure.test :refer [deftest testing is]]
            [ebb.core :as ebb]
            [samizdat.agent.oversight :as ov]
            [samizdat.cancel :as cancel]
            [samizdat.events :as events]))

(defn- now [] (System/currentTimeMillis))

(defn- mine [events] (filterv #(= :flow-test (:kind %)) events))

(deftest publishing-never-parks-on-a-subscriber-that-never-takes
  (let [sub (events/subscribe)]
    (try
      (let [t0 (now)]
        (dotimes [i 2000] (events/publish! {:kind :flow-test :i i}))
        (is (< (- (now) t0) 1000) "two thousand publishes into a full buffer, none of them waited"))
      (finally (events/unsubscribe! sub)))))

(deftest a-slow-subscriber-loses-the-oldest-not-the-publisher
  (let [sub (events/subscribe)]
    (try
      (dotimes [i 300] (events/publish! {:kind :flow-test :i i}))
      (let [got (mine (events/collect sub))]
        (is (<= (count got) 256) "the window is bounded")
        (is (>= (:i (first got)) 44) "the oldest were dropped")
        (is (= 299 (:i (last got))) "the newest survived"))
      (finally (events/unsubscribe! sub)))))

(deftest collecting-drains-what-is-buffered-and-no-more
  (let [sub (events/subscribe)]
    (try
      (dotimes [i 3] (events/publish! {:kind :flow-test :i i}))
      (is (= [0 1 2] (mapv :i (mine (events/collect sub)))))
      (is (empty? (mine (events/collect sub))) "drained")
      (finally (events/unsubscribe! sub)))))

(deftest a-batch-is-everything-that-arrived-in-one-interval
  (let [sub (events/subscribe)]
    (try
      (dotimes [i 3] (events/publish! {:kind :flow-test :i i}))
      (let [[tag batch] (cancel/with-deadline
                          (ebb/reduce (fn [_ b] (reduced b)) nil (events/batches sub 20))
                          2000)]
        (is (= :ok tag))
        (is (= [0 1 2] (mapv :i (mine batch)))))
      (finally (events/unsubscribe! sub)))))

(deftest a-quiet-interval-still-ticks
  ;; The supervisor's first look is immediate and its budget and spacing are
  ;; judged on every tick, events or not, so the flow emits an (empty) batch
  ;; per interval rather than parking until something arrives.
  (let [sub (events/subscribe)]
    (try
      (let [[tag batch] (cancel/with-deadline
                          (ebb/reduce (fn [_ b] (reduced b)) nil (events/batches sub 20))
                          1000)]
        (is (= :ok tag))
        (is (empty? (mine batch))))
      (finally (events/unsubscribe! sub)))))

(deftest the-stream-is-a-task-that-stops-when-cancelled
  (let [sub (events/subscribe)
        seen (atom [])
        stop (ov/start! {:enabled? true :poll-ms 20 :every-ms 1000000 :budget 0
                         :run-id "R-flow" :event-ch sub
                         :reflex-fn (fn [{:keys [arrived]}] (swap! seen conj (count arrived)) nil)}
                        (fn [_] nil))]
    (try
      (dotimes [_ 3] (events/publish! {:kind :step :run-id "R-flow" :branch-id "B1" :turn 1 :node :infer}))
      (Thread/sleep 150)
      (is (= 3 (reduce + @seen)) "every event of this run reached the reflex")
      (is (<= (count (remove zero? @seen)) 2) "in one batch, or two if the interval boundary fell between them")
      (stop)
      (Thread/sleep 30)
      (let [n (count @seen)]
        (dotimes [_ 3] (events/publish! {:kind :step :run-id "R-flow" :branch-id "B1" :turn 2 :node :infer}))
        (Thread/sleep 100)
        (is (= n (count @seen)) "nothing after the cancel"))
      (finally (events/unsubscribe! sub)))))
