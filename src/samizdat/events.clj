;; samizdat - a claim-first verification harness
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

(ns samizdat.events
  "The live event bus.

  Every journal append publishes here. The durable copy is the `events` table;
  this exists so a client can watch a run without polling, and so nothing in
  the loop has to know whether anyone is watching.

  A subscriber that stops reading must never stall the loop, so taps use a
  sliding buffer: a slow watcher loses events rather than applying
  backpressure. That is the right trade because the durable journal is the
  source of truth and a client that fell behind re-reads it by cursor."
  (:require [clojure.core.async :as async]))

(def buffer-size 256)

(defonce ^:private hub
  (let [ch (async/chan (async/sliding-buffer buffer-size))]
    {:ch ch :mult (async/mult ch)}))

(defn publish!
  "Non-blocking. Returns immediately whether or not anyone is listening."
  [event]
  (async/put! (:ch hub) event)
  nil)

(defn subscribe
  "A channel receiving every event published from now on. Close it with
  `unsubscribe!` when done, or it keeps consuming a tap slot."
  ([] (subscribe buffer-size))
  ([n]
   (let [ch (async/chan (async/sliding-buffer n))]
     (async/tap (:mult hub) ch)
     ch)))

(defn unsubscribe! [ch]
  (async/untap (:mult hub) ch)
  (async/close! ch)
  nil)

(defn step
  "One mycelium trace entry as a STEP event: the implementer advancing through
  its state graph, for the supervisor to watch (RFC-012).

  Until this existed the bus carried only journal appends — turn-level, after
  the fact — so a supervisor wanting to know what the implementer was doing
  mid-turn had to re-derive it. mycelium hands every completed cell to
  `:on-trace`; this is that, published.

  `:data` is deliberately dropped. A trace entry holds the WHOLE data map at
  that cell, branch and message history included, so a bus of raw entries is a
  bus of copies of the branch — and this one has a sliding buffer, so it would
  be 256 of them. The shape of the step is what a watcher needs; a value it
  actually wants is one `fetch_turn` away."
  [run-id entry]
  (let [d (:data entry)]
    (cond-> {:kind :step
             :run-id run-id
             :branch-id (get-in d [:branch :id])
             :turn (:turn d)
             :node (:cell entry)
             :cell (:cell-id entry)
             :transition (:transition entry)
             :ms (:duration-ms entry)}
      (:error entry) (assoc :failed true))))

(defn tracer
  "The `:on-trace` callback to hand mycelium, publishing a step per cell.

  `run-id*` is derefable rather than a value because the turn manifest is
  compiled BEFORE the run row exists — the row records a width the compile
  decides — so the id is not known at the only moment mycelium will accept a
  callback.

  Never throws. :on-trace runs synchronously inside the implementer's turn, so
  a bus that could fail would be a bus that can break the run it observes."
  [run-id*]
  (fn [entry]
    (try (publish! (step (if (instance? clojure.lang.IDeref run-id*)
                           @run-id* run-id*)
                         entry))
         (catch Throwable _ nil))))

(defn collect
  "Drain whatever is currently buffered on `ch`. For tests and for a polling
  client that would rather not block."
  [ch]
  (loop [acc []]
    (if-let [v (async/poll! ch)]
      (recur (conj acc v))
      acc)))
