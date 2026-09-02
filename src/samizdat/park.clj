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

(ns samizdat.park
  "A workflow that failed, PARKED rather than ended: the state machine is data,
  so the place it stopped is a value that can be read, fixed, and re-entered.

  The alternative is what the harness did before. A cell that failed killed its
  branch, the supervisor found out on its next poll of the journal, and by then
  there was nothing left to act on — it could edit a cell for the NEXT run and
  that was all. For a mismatch in a shared cell every branch died the same way
  within seconds, so the run was over before the supervisor looked. That made
  strict schema checking a bad trade: it stopped bad data at the cost of the
  run, which is not self-healing.

  Parking removes the clock. mycelium already carries the machinery —
  :mycelium/halt with :mycelium/resume names the state to re-enter, and
  resume-compiled takes a merge-data argument its own docstring describes as
  human-provided input. The supervisor is that input.

  THIS NAMESPACE IS THE READING, and reading is the hard half. The raw
  :mycelium/trace snapshots the WHOLE DATA MAP at every cell — branch, message
  history and all — so handing it over is not information, it is the context
  window spent on one failure. What a supervisor needs to act is much smaller:
  which cell, what it was told, which way the graph actually went, and what the
  data had at that point. `brief` computes exactly that and nothing else.

  Mechanism only: it assembles the facts. The words that carry them to the
  model are prompts/parked.md."
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [mycelium.core :as myc]))

(def ^:private mycelium-keys
  "Bookkeeping mycelium threads through the data map. Never the branch's own,
  and never worth a line in a brief."
  #{:mycelium/trace :mycelium/halt :mycelium/resume :mycelium/warnings
    :mycelium/schema-error :mycelium/error :mycelium/join-traces
    :mycelium/params :mycelium/child-trace :mycelium/timeout})

(defn parked?
  "Whether `data` is a halted workflow that can be re-entered."
  [data]
  (boolean (and (map? data) (:mycelium/resume data))))

(defn resume-state
  "The state id a resume re-enters at — the cell that failed, so a fix is
  retried rather than skipped."
  [data]
  (:mycelium/resume data))

(defn- data-keys
  "The branch-visible keys of a trace entry's data snapshot."
  [entry]
  (set/difference (set (keys (:data entry))) mycelium-keys))

(defn path
  "How the run reached the failure, one entry per cell that ran.

  `:adds` is the DELTA — what this step put on the data map that was not there
  before — rather than everything available. That is the difference between a
  line a supervisor can read and a line that restates the whole map at every
  step, and it is what makes a silent step visible: a cell that adds nothing
  took a route that produced nothing, which is usually the answer.

  Bounded by the trace itself, which the loop already caps (:loop/route keeps
  the last 20 entries, :beam/tick the last 5) — so this is the tail of a long
  run, and says so rather than pretending to be the whole history."
  [data]
  (let [entries (vec (:mycelium/trace data))]
    (vec (map-indexed
          (fn [i e]
            (let [before (if (pos? i) (data-keys (nth entries (dec i))) #{})]
              (cond-> {:node (:cell e)
                       :cell (:cell-id e)
                       :transition (:transition e)
                       :adds (vec (sort (set/difference (data-keys e) before)))}
                (:error e) (assoc :failed true))))
          entries))))

(defn failure
  "What went wrong, as the supervisor needs it rather than as mycelium stores
  it: the error type, the cell, the message, and — for a schema mismatch — the
  key-diff, which is the part that names the fix rather than the symptom.

  Reads through myc/workflow-error so a schema error keeps its own type
  instead of flattening into a generic handler failure."
  [data]
  (when-let [e (myc/workflow-error data)]
    (cond-> {:type (:error-type e)
             :message (:message e)}
      (:cell-id e) (assoc :cell (:cell-id e))
      (:cell e) (assoc :node (:cell e))
      (:key-diff e) (assoc :missing (vec (sort (:missing (:key-diff e))))
                           :extra (vec (sort (:extra (:key-diff e))))))))

(defn brief
  "Everything a supervisor needs to decide what to fix, and nothing else.

  Deliberately omits the DATA VALUES. The keys present at the failure are the
  actionable part — a missing key names its producer, and the producer is what
  gets edited — while the values are a branch's whole message history and would
  bury the one fact that matters. `fetch_turn` is how a supervisor reads a
  value it actually needs."
  [{:keys [workflow version run-id branch-id data]}]
  {:workflow workflow
   :version version
   :run-id run-id
   :branch-id branch-id
   :resume-state (resume-state data)
   :failure (failure data)
   :path (path data)
   :available (vec (sort (set/difference (set (keys data)) mycelium-keys)))})

(defn strip
  "The STATE, with the diagnosis of how it stopped removed — what gets stored
  and what a resume re-enters with.

  Two things go. The trace, because each entry carries a full snapshot of the
  data at that cell, so a twenty-entry trace is twenty copies of the branch and
  its message history; `path` has already reduced it to the deltas worth
  keeping and those live in the parked row's own columns.

  And THE ERROR ITSELF, which matters more than it looks. resume-compiled
  clears :mycelium/halt and :mycelium/resume but nothing else, so a
  :mycelium/schema-error left on the map rides through the resumed run and
  myc/workflow-error still finds it at the end — every successful resume would
  report as a failure, and every driver's (myc/error? data) branch would fire
  on a run that worked. The diagnosis belongs to the parked row, not to the
  state being re-entered."
  [data]
  (dissoc data
          :mycelium/trace :mycelium/join-traces :mycelium/child-trace
          :mycelium/schema-error :mycelium/error))

(defn render-path
  "The path as lines, for the brief template. `->` between a node and the
  transition it left by, so the route taken through the graph is readable as a
  route rather than as a table."
  [path]
  (str/join
   "\n"
   (for [{:keys [node cell transition adds failed]} path]
     (str (if failed "  ✗ " "  · ")
          (name (or node :?)) " (" (subs (str cell) 1) ")"
          (when transition (str " -" (name transition) "->"))
          (if (seq adds)
            (str "  +" (str/join " +" (map #(subs (str %) 1) adds)))
            "  (added nothing)")))))
