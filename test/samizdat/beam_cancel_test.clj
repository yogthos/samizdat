;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.beam-cancel-test
  "The beam over ebb (RFC-013 section 2, karamazov-3cll.2): a turn is a task
  the beam starts with its own callbacks, keeps the canceller of, and cancels
  at the deadline; the branch forfeits while its cancelled turn has not yet
  terminated, and never runs beside it. Abort is the same cancel from outside."
  (:require [clojure.test :refer [deftest testing is]]
            [ebb.core :as ebb]
            [samizdat.agent.beam :as beam]
            [samizdat.agent.state :as state]
            [samizdat.api.control :as api-control]
            [samizdat.cancel :as cancel]
            [samizdat.store.db :as db]
            [samizdat.store.runs :as runs]))

(defn- now [] (System/currentTimeMillis))

(deftest starting-a-task-returns-before-its-body-runs
  ;; An sp runs its body on the caller until the first park (the spike, row
  ;; h). `spawn` yields first, so the canceller comes back at once and the
  ;; body runs on its own fiber: five branches start in parallel rather than
  ;; each waiting for the previous one's prefix.
  (let [t0 (now)
        done (promise)
        busy (fn [] (let [end (+ (now) 300)] (loop [] (when (< (now) end) (recur)))) :ran)
        cancel ((cancel/spawn busy) (fn [v] (deliver done v)) (fn [e] (deliver done e)))
        returned-after (- (now) t0)]
    (is (fn? cancel))
    (is (< returned-after 100) "the canceller came back before the 300ms prefix")
    (is (= :ran (deref done 2000 :timeout)) "and the body still ran to completion")))

(defn- ctx [deadline-ms]
  {:iterating-loop? true :turn-deadline-ms deadline-ms :cancelling (atom {})})

(deftest a-turn-past-the-deadline-is-cancelled-and-the-branch-forfeits-until-it-stops
  ;; The turn parks (a provider call would); the deadline cancels it; the park
  ;; sees the cancel at once, so by the next round the branch is free again.
  (let [calls (atom 0)
        b (state/new-branch {:id "B1" :problem "p"})]
    (with-redefs [beam/advance-branch (fn [_ b _]
                                        (if (= 1 (swap! calls inc))
                                          (do (ebb/? (ebb/sleep 2000)) (assoc b :slow true))
                                          (assoc b :fast true)))]
      (let [c (ctx 50)
            [r1] (beam/advance-all c [b] 1)]
        (is (= 1 (:timeouts r1)) "the slow turn forfeits")
        (is (contains? @(:cancelling c) "B1") "and the cancelled turn is tracked until it terminates")
        (Thread/sleep 100)
        (is (realized? (get @(:cancelling c) "B1"))
            "the park saw the cancel: terminated well inside the 2s it was sleeping for")
        (let [[r2] (beam/advance-all c [b] 2)]
          (is (true? (:fast r2)) "so the next round advances instead of forfeiting again")
          (is (= 2 @calls))
          (is (not (contains? @(:cancelling c) "B1")) "and the entry is released"))))))

(deftest a-turn-that-cannot-see-the-cancel-still-never-runs-beside-itself
  ;; karamazov-blt.18, restated over the promise: a turn blocked in a host
  ;; call the cancel cannot reach (the spike, rows a and d) keeps the branch
  ;; forfeiting until it terminates, and no second turn of that branch runs.
  (let [calls (atom 0)
        b (state/new-branch {:id "B1" :problem "p"})]
    (with-redefs [beam/advance-branch (fn [_ b _]
                                        (if (= 1 (swap! calls inc))
                                          (do (Thread/sleep 400) (assoc b :slow true))
                                          (assoc b :fast true)))]
      (let [c (ctx 50)
            [r1] (beam/advance-all c [b] 1)]
        (is (= 1 (:timeouts r1)))
        (let [[r2] (beam/advance-all c [b] 2)]
          (is (= 1 (:timeouts r2)) "still cancelling: forfeits again")
          (is (= 1 @calls) "no second turn ran beside the first"))
        (Thread/sleep 500)
        (let [[r3] (beam/advance-all c [b] 3)]
          (is (true? (:fast r3)) "once it terminated, the branch advances")
          (is (not (contains? @(:cancelling c) "B1"))))))))

(deftest a-cancelled-turns-result-is-discarded
  ;; The cancelled turn may still produce a value (the host call returned and
  ;; nothing checked). It must not become the branch: the beam forfeited it.
  (let [calls (atom 0)
        b (state/new-branch {:id "B1" :problem "p"})]
    (with-redefs [beam/advance-branch (fn [_ b _]
                                        (swap! calls inc)
                                        (Thread/sleep 200)
                                        (assoc b :late-value true))]
      (let [c (ctx 50)
            [r1] (beam/advance-all c [b] 1)]
        (is (nil? (:late-value r1)))
        (is (= 1 (:timeouts r1)))))))

(deftest a-branch-that-throws-is-abandoned-not-the-beam
  (let [b (state/new-branch {:id "B1" :problem "p"})]
    (with-redefs [beam/advance-branch (fn [_ _ _] (throw (ex-info "kaboom" {})))]
      (let [[r] (beam/advance-all (ctx 500) [b] 1)]
        (is (= :abandoned (:status r)))
        (is (re-find #"kaboom" (str (:inactive-reason r))))))))

(deftest abort-cancels-the-run-task
  ;; The abort flag becomes a cancel of the run task. A run parked in a
  ;; provider call (here, a sleep) comes back cancelled at once, not at the
  ;; top of the next round.
  (let [c (db/open! ":memory:")]
    (try
      (let [rid (runs/start-run! c {:problem "p"})
            ended (promise)]
        (with-redefs [beam/run! (fn [{:keys [on-start]}]
                                  (on-start rid)
                                  (try (ebb/? (ebb/sleep 5000))
                                       (catch Throwable e (deliver ended [:threw e]) (throw e)))
                                  (deliver ended [:finished])
                                  {:status :completed :run-id rid})]
          (let [r (api-control/start-run! {:conn c :config {:llm {:provider :local :model "m"}}}
                                          {:problem "p"})]
            (is (= rid (get-in r [:body :run_id])))
            (is (contains? @api-control/active rid))
            (let [t0 (now)
                  a (api-control/abort! c rid)
                  [how e] (deref ended 3000 [:timeout nil])]
              (is (= "aborting" (get-in a [:body :status])))
              (is (= :threw how) "the run task was cancelled while parked")
              (is (ebb/cancelled? e))
              (is (< (- (now) t0) 1000) "within the park, not after it"))
            (Thread/sleep 50)
            (is (not (contains? @api-control/active rid)) "deregistered when it ended"))))
      (finally (db/close c)))))
