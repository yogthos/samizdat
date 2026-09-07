;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.cancel-test
  "RFC-013's cancel point and pass-through rule (karamazov-3cll.4).

  A turn is a maestro FSM run inside an ebb task. Two things make it
  cancellable: the :pre interceptor every compiled manifest carries calls the
  cancel check before each cell, so a cancel requested between cells is
  observed at the next boundary; and every catch on the FSM path consults a
  compile-time predicate before treating a throwable as a cell error, so a
  Cancelled raised INSIDE a cell (a parked provider call) leaves the machine
  untouched instead of being routed to the error state and, from there, into
  a parked workflow. Mycelium and maestro learn the predicate as an opt and
  never name ebb."
  (:require [clojure.test :refer [deftest is testing]]
            [ebb.core :as ebb]
            [mycelium.cell :as cell]
            [mycelium.core :as myc]
            [samizdat.cancel :as cancel]
            [samizdat.llm.client :as client]
            [samizdat.llm.registry :as registry]
            [samizdat.manifests :as manifests]))

(defn- register! [id f]
  (cell/register-spec! id {:id id :doc "cancel test" :pure true :requires []
                           :handler f :schema {:input [:map] :output [:map]}}))

(def ^:private two-cells
  '{:cells {:start :cancel-test/a :then :cancel-test/b}
    :edges {:start :then :then :end}})

(defmacro ^:private with-cells [[a b] & body]
  `(do (register! :cancel-test/a ~a)
       (register! :cancel-test/b ~b)
       (try ~@body
            (finally (cell/remove-cell! :cancel-test/a)
                     (cell/remove-cell! :cancel-test/b)))))

(defn- start!
  "Invoke `task`. Returns {:cancel canceller :done promise-of [:ok v] | [:err e]}."
  [task]
  (let [done (promise)
        cancel (task (fn [v] (deliver done [:ok v]))
                     (fn [e] (deliver done [:err e])))]
    {:cancel cancel :done done}))

(defn- outcome [{:keys [done]}] (deref done 3000 [:timeout nil]))

(deftest the-check-is-a-no-op-off-a-task
  (is (nil? (cancel/check!)) "nothing to cancel on a plain thread"))

(deftest a-cancel-is-observed-at-the-next-cell-boundary
  ;; Cell a parks, then cancels its own task. The :pre check before cell b
  ;; throws Cancelled; b never runs; the FSM never reached its error state.
  (let [log (atom [])
        canceller (promise)]
    (with-cells [(fn [_ d]
                   (ebb/? (ebb/sleep 20))
                   (swap! log conj :a)
                   ((deref canceller 1000 (fn [])))
                   d)
                 (fn [_ d] (swap! log conj :b) d)]
      (let [wf (manifests/compile-definition two-cells)
            t (start! (ebb/sp (myc/run-compiled wf {} {})))]
        (deliver canceller (:cancel t))
        (let [[tag v] (outcome t)]
          (is (= :err tag))
          (is (ebb/cancelled? v) "the boundary check threw Cancelled")
          (is (= [:a] @log) "the next cell never ran"))))))

(deftest a-cancel-inside-a-cell-passes-through-every-catch
  ;; The provider call is a park inside a cell. A cancel there raises
  ;; Cancelled inside maestro's normalize-handler catch, which used to route
  ;; it to the error state and on to manifests' on-error.
  (let [log (atom [])]
    (with-cells [(fn [_ d] (ebb/? (ebb/sleep 2000)) (swap! log conj :a) d)
                 (fn [_ d] (swap! log conj :b) d)]
      (let [wf (manifests/compile-definition two-cells)
            t (start! (ebb/sp (myc/run-compiled wf {} {})))]
        (Thread/sleep 50)
        (let [t0 (System/currentTimeMillis)
              _ ((:cancel t))
              [tag v] (outcome t)]
          (is (= :err tag))
          (is (ebb/cancelled? v) "Cancelled, not an execution error")
          (is (not (map? v)) "and not a halted workflow returned as data")
          (is (< (- (System/currentTimeMillis) t0) 1000) "within the park, not after it")
          (is (empty? @log)))))))

(deftest an-ordinary-throw-still-routes-to-the-error-handler
  (with-cells [(fn [_ _] (throw (ex-info "boom" {})))
               (fn [_ d] d)]
    (let [wf (manifests/compile-definition two-cells)
          [tag v] (outcome (start! (ebb/sp (myc/run-compiled wf {} {}))))]
      (is (= :err tag))
      (is (not (ebb/cancelled? v)))
      (is (= "execution error" (ex-message v)) "maestro's error state, as before"))))

(deftest an-interrupted-exception-passes-through-too
  ;; via blk delivers what the thunk produced: an interrupted host call throws
  ;; InterruptedException, not Cancelled (the spike, rows b and c).
  (with-cells [(fn [_ _] (throw (InterruptedException. "read interrupted")))
               (fn [_ d] d)]
    (let [wf (manifests/compile-definition two-cells)
          [tag v] (outcome (start! (ebb/sp (myc/run-compiled wf {} {}))))]
      (is (= :err tag))
      (is (instance? InterruptedException v) "not wrapped as an execution error"))))

(deftest the-pass-through-is-a-compile-time-predicate
  ;; The seam mycelium and maestro expose: any predicate, no ebb in sight.
  (with-cells [(fn [_ _] (throw (IllegalStateException. "signal")))
               (fn [_ d] d)]
    (let [passing (myc/pre-compile two-cells {:rethrow? #(instance? IllegalStateException %)})
          routing (myc/pre-compile two-cells {})]
      (is (thrown? IllegalStateException (myc/run-compiled passing {} {}))
          "passes through untouched")
      (is (thrown-with-msg? Exception #"execution error" (myc/run-compiled routing {} {}))
          "routed to the error state without the predicate"))))

(deftest a-cancel-during-a-retry-sleep-returns-within-the-sleep
  ;; The provider ladder's backoff is a park, and the check runs before every
  ;; attempt. The first backoff is 2 s; a cancel 100 ms in must come back in
  ;; well under that with no second attempt made.
  (let [calls (atom 0)
        post-once (ns-resolve 'samizdat.llm.client 'post-once)]
    (with-redefs-fn {post-once (fn [& _] (swap! calls inc) {:outcome :retry :error "stub: transport"})}
      (fn []
        (let [adapter (registry/adapter-for :deepseek)
              config {:base-url "http://127.0.0.1:1" :api-key "x" :max-tokens 16
                      :temperature 0 :max-retries 3 :timeout-ms 1000}
              t (start! (ebb/sp (client/chat adapter config [{:role "user" :content "hi"}])))]
          (Thread/sleep 100)
          (let [t0 (System/currentTimeMillis)
                _ ((:cancel t))
                [tag v] (outcome t)]
            (is (= :err tag))
            (is (ebb/cancelled? v))
            (is (< (- (System/currentTimeMillis) t0) 1000) "the backoff was a park that saw the cancel")
            (is (= 1 @calls) "no further attempt after the cancel")))))))
