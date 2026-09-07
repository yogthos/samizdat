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

(ns samizdat.events
  "The live event bus.

  Every journal append publishes here, and so does every cell the implementer
  completes (`tracer`). The durable copy is the `events` table; this exists so
  a client can watch a run without polling, and so nothing in the loop has to
  know whether anyone is watching.

  Two contracts, held by construction rather than by a buffer size (RFC-013):

  - A PUBLISHER NEVER PARKS. `publish!` appends to each subscriber's window
    and returns; there is no channel to fill and no consumer to wait for.
  - A SLOW WATCHER LOSES THE OLDEST. Each subscription is a sliding window of
    `buffer-size` events, so a watcher that stops reading loses events rather
    than applying backpressure. That is the right trade because the durable
    journal is the source of truth and a client that fell behind re-reads it
    by cursor. (ebb's `relieve` with a sliding semigroup says the same thing;
    the window is that, spelled out, because a watcher also wants to drain it
    on demand without a flow.)

  A consumer that is a task reads the bus as a FLOW: `batches` hands over
  everything that arrived in each poll interval, empty intervals included, so
  a supervisor's reduce ticks on a quiet run too and is cancelled with the
  run it watches."
  (:require [ebb.core :as ebb]))

(def buffer-size 256)

(defonce ^:private subscribers (atom #{}))

(defn- slide
  "`buf` with `event` appended and the oldest dropped past `cap`. A fresh
  vector when full rather than a subvec, whose base would keep every event
  ever published."
  [buf cap event]
  (if (< (count buf) cap)
    (conj buf event)
    (conj (vec (rest buf)) event)))

(defn publish!
  "Non-blocking. Returns immediately whether or not anyone is listening."
  [event]
  (doseq [sub @subscribers]
    (swap! sub (fn [{:keys [buf cap] :as s}] (assoc s :buf (slide buf cap event)))))
  nil)

(defn subscribe
  "A subscription receiving every event published from now on: a sliding
  window of the newest `n`. Close it with `unsubscribe!` when done, or it
  keeps receiving."
  ([] (subscribe buffer-size))
  ([n]
   (let [sub (atom {:buf [] :cap n})]
     (swap! subscribers conj sub)
     sub)))

(defn unsubscribe! [sub]
  (swap! subscribers disj sub)
  nil)

(defn collect
  "Drain whatever `sub` holds right now, without blocking: the events that
  arrived since the last drain, oldest first."
  [sub]
  (loop []
    (let [s @sub]
      (if (compare-and-set! sub s (assoc s :buf []))
        (:buf s)
        (recur)))))

(defn batches
  "The subscription as a flow: one batch per `poll-ms` interval, each batch
  everything that arrived since the last, an empty vector when nothing did.
  With no subscription it still ticks, with nil, so a consumer that has no
  bus (a driver without one, a test) keeps its clock. Consume it with
  `ebb/reduce`; cancelling the reduce ends the flow at its next park."
  [sub poll-ms]
  (ebb/ap
   (let [_ (ebb/?> (ebb/seed (repeat nil)))]
     (ebb/? (ebb/sleep poll-ms))
     (when sub (collect sub)))))

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
