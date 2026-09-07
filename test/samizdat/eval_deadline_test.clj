;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.eval-deadline-test
  "One cancellable deadline idiom for the three eval sites (RFC-013,
  karamazov-3cll.3): the harness eval, the mutation soak, and the project
  image. `cancel/with-deadline` starts a task, waits up to the budget, and on
  expiry cancels it and moves on — detach, because a runaway the cancel cannot
  reach must not hold the caller (the spike, row f).

  The image gets more than that. Its nREPL ships an `interrupt` op that stops
  even a tight loop at the next engine tick, so a runaway eval is interrupted
  and the image SURVIVES; the restart is kept only for an eval blocked in a
  foreign call, which the interrupt cannot reach."
  (:require [clojure.test :refer [deftest testing is]]
            [ebb.core :as ebb]
            [jolt.fs :as fs]
            [samizdat.cancel :as cancel]
            [samizdat.repl :as repl]
            [samizdat.repl.route :as route]))

(defn- now [] (System/currentTimeMillis))

(deftest with-deadline-returns-a-value-that-arrives-in-time
  (is (= [:ok 3] (cancel/with-deadline (ebb/via ebb/blk (+ 1 2)) 1000))))

(deftest with-deadline-returns-an-error-that-arrives-in-time
  (let [[tag e] (cancel/with-deadline (ebb/via ebb/blk (throw (ex-info "boom" {}))) 1000)]
    (is (= :err tag))
    (is (= "boom" (ex-message e)))))

(deftest with-deadline-cancels-a-late-task-and-does-not-wait-for-it
  ;; A blocking sleep of 5s under a 100ms budget: back within the budget, the
  ;; thread interrupted (the sleep is interruptible, rows b of the spike), and
  ;; whatever the task later produces is nobody's problem.
  (let [seen (atom nil)
        t0 (now)
        task (fn [s f] ((ebb/via ebb/blk (Thread/sleep 5000) :late)
                        (fn [v] (reset! seen [:ok v]) (s v))
                        (fn [e] (reset! seen [:err e]) (f e))))
        r (cancel/with-deadline task 100)]
    (is (= [:timeout] r))
    (is (< (- (now) t0) 1000) "detached at the budget, not at the sleep's end")
    (Thread/sleep 200)
    (is (= :err (first @seen)) "the interrupted sleep failed the task afterwards")
    (is (instance? InterruptedException (second @seen)))))

(deftest with-deadline-cancels-an-sp-at-its-next-check
  (let [t0 (now)
        r (cancel/with-deadline (cancel/spawn (fn [] (loop [] (cancel/check!) (ebb/? (ebb/sleep 10)) (recur))))
                                100)]
    (is (= [:timeout] r))
    (is (< (- (now) t0) 1000))))

(deftest the-harness-eval-is-bounded-by-the-same-idiom
  (let [sess (repl/new-session)]
    (try
      (let [t0 (now)
            r (repl/eval-code "(Thread/sleep 5000)" sess 200)]
        (is (false? (:ok r)))
        (is (= "timeout" (:error-type r)))
        (is (< (- (now) t0) 1500) "back at the budget"))
      (is (= "3" (:value (repl/eval-code "(+ 1 2)" sess 2000))) "the session still evaluates")
      (finally (repl/close-session sess)))))

(defn- project! []
  (let [root (str (fs/create-temp-dir))]
    (spit (str root "/deps.edn") (pr-str {:paths ["src"]}))
    root))

(deftest a-runaway-image-eval-is-interrupted-and-the-image-survives
  ;; Today a timed-out image eval releases the whole image. The nrepl interrupt
  ;; op stops the runaway at its next engine tick, the aborted eval replies,
  ;; and the same image answers the next eval: no restart, no lost defs.
  (let [root (project!)
        ctx {:root root :role :implementor}]
    (try
      (is (= "3" (:value (route/eval-for ctx "(def survived 3) survived" nil 25000))))
      (let [before (route/image-port ctx)
            t0 (now)
            t (route/eval-for ctx "(loop [] (recur))" nil 2000)]
        (is (:timeout? t))
        (is (= "timeout" (:error-type t)))
        (is (< (- (now) t0) 6000) "back near the budget, not after a restart")
        (is (= before (route/image-port ctx)) "the image was interrupted, not restarted")
        (is (= "3" (:value (route/eval-for ctx "survived" nil 25000)))
            "and its state survived with it"))
      (finally (route/release! root)))))
