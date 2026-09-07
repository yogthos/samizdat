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

(ns samizdat.cancel
  "The seam between the state machine and the scheduler (RFC-013).

  Mycelium is the machine: it compiles a manifest and runs it as a
  synchronous loop wherever it is called. Ebb is the scheduler: it decides
  which fiber that call runs on and what can stop it. The two meet in three
  compile-time options on `manifests/compile-definition`, and this namespace
  is what samizdat hands in for two of them:

    :pre       `pre-check` — maestro runs it before every step, outside every
               catch, so a cancel requested between cells is observed at the
               next boundary and a Cancelled thrown there leaves the loop
               clean. Every cell boundary is a cancel point, and since the
               journal writes are cells, a cancel never splits one.
    :rethrow?  `control-signal?` — every catch on the FSM path consults it
               before treating a throwable as a cell error, so a Cancelled
               raised INSIDE a cell (a parked provider call) is not routed to
               the error state and from there into a parked workflow.

  Mechanism only. Whether a turn is cancelled, and when, is the beam's
  decision (`agent/beam`), and the deadline behind it is `gates.edn`.

  Two measured facts shape the helpers (the spike, RFC-013):

  - `via blk` delivers whatever the thunk produced after an interrupt: an
    InterruptedException, a SocketTimeoutException, or a plain value. It never
    substitutes Cancelled. So `control-signal?` accepts InterruptedException,
    and `after-blocking!` follows every blocking host call.
  - jolt's interrupt does not reach a `:blocking` FFI read, so a cancel is
    observed at the next check after the read returns. `check!` is that
    check; it costs nothing off a task."
  (:require [ebb.core :as ebb]))

(defn check!
  "Throw Cancelled if the running task has been cancelled; nil otherwise,
  including on a plain thread where there is nothing to cancel."
  []
  (ebb/!)
  nil)

(def after-blocking!
  "Call immediately after a blocking host call returns (a `via blk`, a
  socket read, a subprocess wait): whatever it returned, a cancel requested
  while it ran becomes Cancelled here rather than a value the code goes on
  to act on."
  check!)

(defn control-signal?
  "Whether `e` is a cancellation rather than a failure: ebb's Cancelled, or
  the InterruptedException an interrupted host call throws. Every catch on
  the FSM path rethrows these untouched (the :rethrow? opt)."
  [e]
  (boolean (or (ebb/cancelled? e) (instance? InterruptedException e))))

(defn pre-check
  "The :pre interceptor: check for a cancel, then hand the state through
  unchanged. Shaped as mycelium's `(fn [fsm-state resources] -> fsm-state)`."
  [fsm-state _resources]
  (check!)
  fsm-state)

(defn sleep!
  "Park for `ms`. On a task the park sees a cancel at once; off a task it is
  a plain wait. Replaces Thread/sleep wherever the harness waits on purpose,
  which is what lets a retry backoff or a poll interval be cancelled."
  [ms]
  (ebb/? (ebb/sleep (long ms)))
  nil)
