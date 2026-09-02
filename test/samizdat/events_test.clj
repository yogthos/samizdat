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

(ns samizdat.events-test
  "The implementer's STEPS on the event bus (RFC-012).

  The bus itself is not new — samizdat.events has always been here and every
  journal append publishes to it. What it carried was turn-level records,
  after the fact. These tests are about the step events: mycelium hands each
  completed cell to :on-trace, and that is the implementer advancing through
  its state graph, live, for a supervisor to watch."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [mycelium.cell :as cell]
            [mycelium.core :as myc]
            [samizdat.cells :as cells]
            [samizdat.events :as events]
            [samizdat.manifests :as manifests]))

(deftest the-graph-publishes-a-step-for-every-cell-it-completes
  ;; The seam mycelium always had and samizdat never passed. Driving a real
  ;; compiled workflow rather than calling publish! by hand, because what is
  ;; being tested is that :on-trace is WIRED — a test that published its own
  ;; events would pass with the callback still dropped on the floor.
  (cells/load-cells!)
  (cell/defcell :events-test/a
    {:doc "first" :pure true :input [:map] :output [:map [:a :int]]}
    (fn [_ d] (assoc d :a 1)))
  (cell/defcell :events-test/b
    {:doc "second" :pure true :input [:map [:a :int]] :output [:map [:b :int]]}
    (fn [_ d] (assoc d :b 2)))
  (let [ch (events/subscribe)]
    (try
      (let [wf (manifests/compile-definition
                '{:cells {:start :events-test/a :second :events-test/b}
                  :edges {:start :second :second :end}}
                {:on-trace (events/tracer "R1")})]
        (myc/run-compiled wf {} {})
        (Thread/sleep 50)
        (let [steps (filterv #(= :step (:kind %)) (events/collect ch))]
          (is (= 2 (count steps)) "one step per cell that completed")
          (is (= [:start :second] (mapv :node steps)) "in the order they ran")
          (is (= [:events-test/a :events-test/b] (mapv :cell steps)))
          (is (every? #(= "R1" (:run-id %)) steps)
              "each says which run it belongs to — the bus is process-wide")))
      (finally
        (events/unsubscribe! ch)
        (cell/remove-cell! :events-test/a)
        (cell/remove-cell! :events-test/b)))))

(deftest a-step-carries-the-shape-and-not-the-payload
  ;; THE SIZE CONSTRAINT. A mycelium trace entry holds the whole data map, so
  ;; publishing entries raw would put copies of the branch on a bus with a
  ;; 256-deep sliding buffer. Same projection as samizdat.park's path.
  (let [branch {:id "B1" :messages (vec (repeat 40 {:content (apply str (repeat 300 "x"))}))}
        e (events/step "R1" {:cell :infer :cell-id :llm/infer :transition :ok
                             :duration-ms 12.5
                             :data {:branch branch :turn 7}})]
    (is (= :step (:kind e)) "distinguishable from the journal appends beside it")
    (is (= "B1" (:branch-id e)) "the branch is identified")
    (is (= 7 (:turn e)))
    (is (= :llm/infer (:cell e)))
    (is (nil? (:data e)) "but its contents do not ride along")
    (is (< (count (pr-str e)) 220)
        (str "a step is " (count (pr-str e)) " chars — the branch leaked in"))))

(deftest a-failed-step-says-so
  (let [e (events/step "R1" {:cell :dispatch :cell-id :tool/dispatch
                             :data {:turn 1} :error {:cell-id :tool/dispatch}})]
    (is (:failed e))
    (is (not (:failed (events/step "R1" {:cell :x :data {}}))))))

(deftest the-run-id-may-arrive-after-the-compile
  ;; The turn manifest is compiled BEFORE the run row exists — the row records
  ;; a width the compile decides — and :on-trace is only accepted at compile.
  ;; So the tracer takes something derefable and the driver delivers the id
  ;; once it has one.
  (let [run-id* (atom nil)
        ch (events/subscribe)]
    (try
      (let [t (events/tracer run-id*)]
        (reset! run-id* "R9")
        (t {:cell :a :cell-id :x/a :data {:turn 1}})
        (Thread/sleep 50)
        (is (= ["R9"] (mapv :run-id (filterv #(= :step (:kind %))
                                             (events/collect ch))))))
      (finally (events/unsubscribe! ch)))))

(deftest publishing-never-throws-into-the-implementers-thread
  ;; :on-trace runs synchronously inside the turn. A bus that could break the
  ;; run it observes would be worse than no bus.
  (with-redefs [events/publish! (fn [& _] (throw (ex-info "bus is broken" {})))]
    (let [t (events/tracer "R1")]
      (is (nil? (t {:cell :x}))
          "the tracer let a broken bus kill the turn"))))
