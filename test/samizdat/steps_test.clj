;; samizdat - a self-hosting agentic harness
;; Copyright (C) 2026 Dmitri Sotnikov
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.steps-test
  "The per-run step ring: the manifest-state trace, made readable over HTTP.

  Steps are the implementer walking its state graph — :start, :measure,
  :infer, :parse, :dispatch — published on the bus by samizdat.events and,
  until this existed, held by nobody. The GUI and the TUI are both HTTP
  clients with no bus to subscribe to, so the trace was invisible to the
  only things that wanted to draw it.

  This is deliberately NOT the journal. A step is worth a frame of a live
  UI and nothing after that, so it is held in memory, bounded, and dropped
  with the run — the durable record of what a turn DID is the turns table,
  which is a different question and already answered."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [samizdat.api.runs :as api-runs]
            [samizdat.events :as events]
            [samizdat.steps :as steps]))

(use-fixtures :each (fn [f] (steps/reset!) (f) (steps/reset!)))

(defn- step [run-id node]
  {:kind :step :run-id run-id :branch-id "b1" :turn 1 :node node
   :cell :loop/assemble :transition :ok :ms 3})

(deftest a-cursor-tail-returns-only-what-is-new
  (steps/record! (step "r1" :start))
  (steps/record! (step "r1" :measure))
  (let [{:keys [steps cursor]} (steps/since "r1" 0 100)]
    (is (= [:start :measure] (mapv :node steps))
        "everything, from a zero cursor")
    (is (= 2 cursor) "the cursor is the last sequence number handed out")
    (testing "the same cursor again is empty, not a repeat"
      (is (= [] (:steps (steps/since "r1" cursor 100)))))
    (testing "and a step recorded after it arrives on the next poll"
      (steps/record! (step "r1" :infer))
      (let [nxt (steps/since "r1" cursor 100)]
        (is (= [:infer] (mapv :node (:steps nxt))))
        (is (= 3 (:cursor nxt)))))))

(deftest each-run-has-its-own-ring
  ;; A shared ring would fold a beam's branches into one another's traces —
  ;; every run on the server is publishing to the same bus.
  (steps/record! (step "r1" :start))
  (steps/record! (step "r2" :infer))
  (is (= [:start] (mapv :node (:steps (steps/since "r1" 0 100)))))
  (is (= [:infer] (mapv :node (:steps (steps/since "r2" 0 100)))))
  (testing "a run nothing was recorded for is empty, not an error"
    (is (= {:steps [] :cursor 0 :dropped 0} (steps/since "r3" 0 100)))))

(deftest the-ring-drops-the-oldest-and-says-so
  ;; The contract the bus already keeps: a slow watcher loses the oldest
  ;; rather than applying backpressure. What it must not do is lose them
  ;; SILENTLY — a UI that missed steps should be able to say so instead of
  ;; drawing a trace with a hole in it.
  (let [cap (steps/capacity)]
    (dotimes [i (+ cap 5)]
      (steps/record! (step "r1" (keyword (str "n" i)))))
    (let [{:keys [steps dropped cursor]} (steps/since "r1" 0 (* 2 cap))]
      (is (= cap (count steps)) "never more than the cap is retained")
      (is (= (+ cap 5) cursor) "the cursor still counts everything recorded")
      (is (= 5 dropped) "and the tail reports what fell off before it")
      (is (= (keyword (str "n" 5)) (:node (first steps)))
          "the oldest survivor is the first one not evicted"))))

(deftest a-limit-bounds-one-response-without-losing-the-rest
  (dotimes [i 10] (steps/record! (step "r1" (keyword (str "n" i)))))
  (let [{:keys [steps cursor]} (steps/since "r1" 0 4)]
    (is (= 4 (count steps)))
    (is (= 4 cursor) "the cursor advances only over what was actually handed over")
    (is (= [:n4 :n5 :n6 :n7] (mapv :node (:steps (steps/since "r1" cursor 4))))
        "so the next poll continues where this one stopped")))

(deftest forgetting-a-run-frees-its-ring
  (steps/record! (step "r1" :start))
  (steps/forget! "r1")
  (is (= [] (:steps (steps/since "r1" 0 100)))))

(deftest the-registry-itself-is-bounded
  ;; forget! is called when a run ends, and a process that never sees a run
  ;; end must still not grow without bound. The oldest run's ring is the one
  ;; that goes.
  (let [n (steps/max-runs)]
    (dotimes [i (+ n 3)]
      (steps/record! (step (str "run" i) :start)))
    (is (= n (steps/run-count)))
    (is (= [] (:steps (steps/since "run0" 0 100)))
        "the first run seen is the first evicted")
    (is (= [:start] (mapv :node (:steps (steps/since (str "run" (+ n 2)) 0 100))))
        "the newest is still there")))

(deftest only-step-events-are-recorded
  ;; The subscription is to the whole bus, which carries every journal append
  ;; too. A ring that took those would evict the trace it exists to hold.
  (steps/record! {:kind :turn :run-id "r1" :data {:tool "bash"}})
  (steps/record! (step "r1" :start))
  (is (= [:start] (mapv :node (:steps (steps/since "r1" 0 100))))))

(deftest a-step-with-no-run-id-is-dropped-rather-than-filed-under-nil
  ;; events/tracer derefs a promise for the run id, and the turn manifest is
  ;; compiled BEFORE the run row exists — so a step really can arrive with no
  ;; run behind it. Filing those under nil would make one ring that every
  ;; client polling a real run misses anyway.
  (steps/record! (step nil :start))
  (is (zero? (steps/run-count))))

(deftest the-tail-endpoint-answers-in-the-shape-a-poller-already-speaks
  ;; Same contract as journal-tail: `next` is what the client sends back, so
  ;; the TUI runs one poll loop over two feeds rather than two designs.
  (steps/record! (step "r1" :start))
  (steps/record! (step "r1" :infer))
  (let [r (api-runs/steps-tail "r1" 0 200)]
    (is (= "r1" (:run_id r)))
    (is (= ["start" "infer"] (mapv :node (:steps r)))
        "node and cell are keywords in the ring and strings on the wire — a
         JSON client cannot read a keyword back")
    (is (= ["loop/assemble" "loop/assemble"] (mapv :cell (:steps r)))
        "the cell is its printed name, with no leading colon — str on a
         keyword keeps the colon, and a panel would draw \":loop/assemble\"")
    (is (= 2 (:next r)))
    (is (= 2 (:count r)))
    (is (zero? (:dropped r))))
  (testing "a run with no ring is an empty tail, not a 404 — a client may
            legitimately poll a run that has not stepped yet"
    (is (= {:run_id "nope" :steps [] :next 0 :count 0 :dropped 0}
           (api-runs/steps-tail "nope" 0 200)))))

(deftest the-bus-subscription-feeds-the-ring
  ;; The seam that matters: publishing a step anywhere in the process must
  ;; reach the ring, or the endpoint serves an empty trace on a live run.
  (let [sub (steps/subscribe!)]
    (try
      (events/publish! (step "r1" :dispatch))
      (steps/drain! sub)
      (is (= [:dispatch] (mapv :node (:steps (steps/since "r1" 0 100)))))
      (finally (steps/unsubscribe! sub)))))

(defn- eventually
  "Wait up to `ms` for `pred`, polling. A fixed sleep would either be flaky
  on a loaded machine or slow on an idle one; this is neither."
  [ms pred]
  (let [deadline (+ (System/currentTimeMillis) ms)]
    (loop []
      (cond (pred) true
            (> (System/currentTimeMillis) deadline) false
            :else (do (Thread/sleep 10) (recur))))))

(deftest the-pump-carries-a-published-step-to-the-ring-on-its-own
  ;; What start! wires. Without it the ring is a buffer nothing fills, and
  ;; the endpoint answers an empty trace on a run that is stepping.
  (try
    (steps/start-pump!)
    (events/publish! (step "r1" :arbiter))
    (is (eventually 3000 #(seq (:steps (steps/since "r1" 0 100))))
        "the pump drains the bus without anyone asking it to")
    (is (= [:arbiter] (mapv :node (:steps (steps/since "r1" 0 100)))))
    (finally (steps/stop-pump!))))

(deftest starting-the-pump-twice-runs-one-pump
  ;; start! can be called after a restart! that failed halfway, and two pumps
  ;; on one bus means each drains half the events into the same rings — a
  ;; trace with gaps that only appears under a restart.
  (try
    (steps/start-pump!)
    (steps/start-pump!)
    (is (steps/pumping?))
    (finally (steps/stop-pump!)))
  (is (not (steps/pumping?)) "and stopping once stops it")
  (testing "stopping a pump that is not running is a no-op, not a throw —
            stop! runs best-effort over every resource"
    (is (nil? (steps/stop-pump!)))))
