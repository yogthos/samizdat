;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.dev.ebb-spike
  "Measurement harness for karamazov-3cll.1 (RFC-013's open question).

  For every kind of wait samizdat has, start it, cancel it after `cancel-ms`,
  and record whether the waiter unblocked, how long after the cancel, what
  the caller saw, and whether the body ran on past the wait. Each wait is
  measured twice: under ebb (`via blk`, whose cancel interrupts the thread)
  and under a jolt `future` + `future-cancel`, which is what the code does
  today.

  Not a test: it prints a table. Run with

      jolt -A:dev -m samizdat.dev.ebb-spike

  Rows (a) and (d) are the question that decides the length of RFC-013's
  Cancelling state: a `:blocking` FFI recv (http-client, and the nrepl
  transport the project image uses). Both are held open by a local `nc`
  that accepts and never replies, so the only way out is the socket timeout
  or the cancel."
  (:require [ebb.core :as m]
            [jolt.http-client :as http]
            [jolt.process :as p]
            [nrepl.transport :as transport]
            [samizdat.engine.proc :as proc]))

(def ^:private socket-secs 5)
(def ^:private socket-ms (* 1000 socket-secs))
(def ^:private cancel-ms 500)
(def ^:private grace-ms (+ 2000 socket-ms))

(defn- now [] (System/currentTimeMillis))

(def ^:private pending ::pending)

(defn- outcome-of [x]
  (cond (identical? x pending) :still-blocked
        (m/cancelled? x) :cancelled
        (instance? Throwable x) (str (.getSimpleName (class x)) ": " (ex-message x))
        :else [:value (if (map? x) (select-keys x [:status :timeout :exit]) x)]))

;; --- one measurement, two modes ---------------------------------------------

(defn- settle
  "Cancel via `cancel!` after cancel-ms, then wait up to wait-ms on `done`, a
  promise of [outcome-value settled-at-ms]."
  [label mode cancel! done wait-ms after]
  (Thread/sleep cancel-ms)
  (let [t-cancel (now)
        _ (cancel!)
        r (deref done wait-ms nil)]
    {:row label :mode mode
     :settled? (some? r)
     :latency-ms (when r (- (second r) t-cancel))
     :outcome (outcome-of (if r (first r) pending))
     :ran-after? @after}))

(defn measure-task
  "Invoke an ebb task, cancel it, wait for it to settle."
  [label task wait-ms after]
  (let [done (promise)
        cancel (task (fn [v] (deliver done [v (now)]))
                     (fn [e] (deliver done [e (now)])))]
    (settle label "ebb" cancel done wait-ms after)))

(defn measure-future
  "Run a thunk in a jolt future, future-cancel it, wait for its finally."
  [label thunk wait-ms after]
  (let [done (promise)
        f (future (let [r (try (thunk) (catch Throwable e e))]
                    (deliver done [r (now)])))]
    (settle label "future" #(future-cancel f) done wait-ms after)))

(defn- with-after
  "A thunk that flags `after` once `f` has returned normally."
  [after f]
  (fn [] (let [r (f)] (reset! after true) r)))

(defn- both
  "The same wait under ebb via-blk and under a future. `body-of` takes the
  value `around` supplies (a port, or nil) and returns the blocking thunk;
  `around` wraps each mode's measurement, defaulting to a plain call."
  ([label wait-ms body-of] (both label wait-ms body-of (fn [f] (f nil))))
  ([label wait-ms body-of around]
   (mapv (fn [mode]
           (around (fn [arg]
                     (let [after (atom false)
                           thunk (with-after after (body-of arg))]
                       (if (= mode :ebb)
                         (measure-task label (m/via m/blk (thunk)) wait-ms after)
                         (measure-future label thunk wait-ms after))))))
         [:ebb :future])))

;; --- a listener that accepts and never replies ------------------------------

(def ^:private port* (atom 39930))

(defn- holding-port
  "Run (f port) with `nc` listening on a fresh port, its stdin held open by a
  sleeping pipe so it never sends EOF down the accepted connection."
  [f]
  (let [port (swap! port* inc)
        pr (p/process ["sh" "-c" (str "sleep 120 | nc -l 127.0.0.1 " port)] {})]
    (Thread/sleep 300)
    (try (f port)
         (finally (try (p/destroy-tree pr) (catch Throwable _ nil))))))

;; --- the rows ---------------------------------------------------------------

(defn rows []
  (concat
   (both "a. http-client recv" grace-ms
         (fn [port] #(http/get (str "http://127.0.0.1:" port "/")
                               {:socket-timeout socket-ms :conn-timeout 2000}))
         holding-port)
   (both "d. nrepl transport recv" grace-ms
         (fn [port] #(transport/recv (transport/connect "127.0.0.1" port
                                                        {:recv-timeout-secs socket-secs})))
         holding-port)
   (both "b. Thread/sleep" grace-ms (fn [_] #(Thread/sleep socket-ms)))
   [(let [after (atom false)]
      (measure-task "b. ebb sleep, on the fiber"
                    (m/sp (m/? (m/sleep socket-ms)) (reset! after true) :slept)
                    grace-ms after))]
   (both "c. promise deref" 3000 (fn [_] #(deref (promise))))
   (both "e. subprocess waitFor" grace-ms
         (fn [_] #(proc/run {:timeout-ms socket-ms} "sleep" (str socket-secs))))
   (let [after (atom false)]
     [(measure-task "f. tight loop, no check, via blk"
                    (m/via m/blk (loop [] (recur))) 2000 after)
      (measure-task "f. loop checking isInterrupted, via blk"
                    (m/via m/blk (loop [] (when-not (.isInterrupted (Thread/currentThread)) (recur))) :stopped)
                    2000 after)
      (measure-task "f. sp loop, (!) then a 1ms park"
                    (m/sp (loop [] (m/!) (m/? (m/sleep 1)) (recur))) 2000 after)
      (measure-task "g. (!) inside a called fn, nested in sp"
                    (m/sp (letfn [(inner [] (m/!) (m/? (m/sleep 5)))] (loop [] (inner) (recur))))
                    2000 after)])))

(defn row-sp-startup
  "How long invoking an sp task takes to return its canceller when the body
  does 300ms of CPU work before its first park. The beam starts one turn task
  per branch in a loop, so this is what serializes across branches."
  []
  (let [t0 (now)
        task (m/sp (let [end (+ (now) 300)] (loop [] (when (< (now) end) (recur))))
                   (m/? (m/sleep 1))
                   :done)
        done (promise)
        _ (task (fn [v] (deliver done v)) (fn [e] (deliver done e)))
        returned-after (- (now) t0)]
    (deref done 2000 nil)
    {:row "h. sp invoke returns (300ms CPU before first park)" :mode "ebb"
     :settled? true :latency-ms returned-after :outcome :canceller-returned :ran-after? false}))

(defn- timed-out-after
  "When (timeout child 500) settles, in ms from invocation."
  [label child]
  (let [t0 (now)
        done (promise)
        _ ((m/timeout child 500 ::timed-out)
           (fn [v] (deliver done [v (now)]))
           (fn [e] (deliver done [e (now)])))
        r (deref done grace-ms nil)]
    {:row label :mode "ebb"
     :settled? (some? r) :latency-ms (when r (- (second r) t0))
     :outcome (outcome-of (if r (first r) pending)) :ran-after? false}))

(defn row-timeout-waits
  "Does (timeout task 500) settle at 500ms, or only once the cancelled child
  has terminated? Once with an interruptible child (Thread/sleep) and once
  with one the interrupt cannot reach (an http recv held open by nc)."
  []
  [(timed-out-after "i. (timeout (via blk (Thread/sleep 5s)) 500) settles at"
                    (m/via m/blk (Thread/sleep socket-ms)))
   (holding-port
    (fn [port]
      (timed-out-after "i. (timeout (via blk (http recv 5s)) 500) settles at"
                       (m/via m/blk (http/get (str "http://127.0.0.1:" port "/")
                                              {:socket-timeout socket-ms :conn-timeout 2000})))))])

;; --- the table --------------------------------------------------------------

(defn- fmt [{:keys [row mode settled? latency-ms outcome ran-after?]}]
  (format "%-50s %-7s %-8s %7s  %-46s %s"
          row mode (if settled? "settled" "BLOCKED")
          (if latency-ms (str latency-ms "ms") "-")
          (pr-str outcome) (if ran-after? "ran-after" "")))

(defn -main [& _]
  (println "ebb spike: cancel at" cancel-ms "ms; socket timeout and sleeps" socket-secs "s")
  (println (fmt {:row "row" :mode "mode" :settled? true :latency-ms nil :outcome 'outcome}))
  (let [all (concat (rows) [(row-sp-startup)] (row-timeout-waits))]
    (doseq [r all] (println (fmt r)))
    (println)
    (prn (mapv #(dissoc % :mode) all)))
  (System/exit 0))
