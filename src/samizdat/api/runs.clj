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

(ns samizdat.api.runs
  "The read model over the journal.

  Every one of these is a query against tables the loop appends to as it goes,
  so they work identically for a live run and a finished one and need no
  cooperation from the loop. That is what makes a UI a client rather than a
  special case.

  The tail endpoint is a cursor over `events` rather than a stream, because a
  cursor works over any HTTP server and a stream does not — see PLAN.md on the
  vendored adapter."
  (:require ;; the java.time.* host shim, before data.json — see samizdat.store.journal
            [jolt.time]
            [clojure.data.json :as json]
            [samizdat.agent.gates :as gates]
            [samizdat.store.db :as db]
            [samizdat.store.interventions :as interventions]
            [samizdat.store.journal :as journal]
            [samizdat.store.runs :as runs]
            [samizdat.store.tasks :as tasks]
            [samizdat.steps :as steps]))

(defn- parse-json [s]
  (when s (try (json/read-str s :key-fn keyword) (catch Throwable _ s))))

(defn- kw-name
  "A keyword as the name a reader expects — `loop/assemble`, not
  `:loop/assemble`. `name` drops the namespace and `str` keeps the colon, so
  neither is right on its own for a namespaced keyword going over the wire."
  [k]
  (cond
    (nil? k) nil
    (keyword? k) (subs (str k) 1)
    :else (str k)))

(defn list-runs [conn limit]
  {:runs (mapv (fn [r]
                 {:id (:id r) :problem (:problem r) :status (:status r)
                  :model (:model r) :beam_width (:beam_width r)
                  :token_budget (:token_budget r)
                  :started_at (:started_at r) :ended_at (:ended_at r)})
               ;; provenance R3-12: a negative limit went into SQL LIMIT, where
               ;; -1 means no limit — a tighter-looking ask that answered
               ;; with the whole table.
               (runs/list-runs conn (max 0 (or limit 50))))})

(def stall-threshold-ms
  "How long a running run may say nothing before a reader should doubt it.

  Above the 900000ms turn deadline by a factor of two, because silence under
  that is a branch legitimately waiting on a provider call or a Lean tactic
  and reporting it would be crying wolf. Past twice the deadline every branch
  in the beam has had its turn forfeited and the round should have moved on,
  so continued silence means nobody is going to update this row."
  1800000)

(defn- seeded?
  "Whether this run was seeded from a prior one.

  Read off the journal rather than a column: beam/run! applies the seed's
  consequences in memory and the row never learned about it, but the
  `run-seeded` event is written before any branch opens, so the fact is
  already durable."
  [conn run-id]
  (some? (db/fetch-one conn ["SELECT id FROM events
                              WHERE run_id = ? AND kind = 'run-seeded' LIMIT 1"
                             run-id])))

(defn get-run [conn run-id]
  (when-let [r (runs/get-run conn run-id)]
    (let [branches (runs/branches conn run-id)]
      {:run (-> r
              (update :prompt_digest str)
              ;; A status of 'running' is a claim the loop makes once and never
              ;; revisits, so on its own it cannot distinguish a working run
              ;; from a dead one. These two let a client tell.
              (assoc :last_progress_at (runs/last-progress-at conn run-id)
                     :stalled (runs/stalled? conn run-id stall-threshold-ms)
                     ;; beam_width is the repopulation FLOOR — repopulate only
                     ;; fires below it and branch-out grows past it — so the
                     ;; number a caller set is not the number of concurrent
                     ;; provider calls they get. A run started at width 5 was
                     ;; observed at 9 active branches. Report both, and the
                     ;; ceiling that actually bounds it.
                     :active_branches (count (filter #(= "active" (:status %))
                                                     branches))
                     :max_branches (gates/threshold :max-total-branches)
                     ;; Seeding forces sharing on regardless of config
                     ;; (beam.clj), and nothing recorded that, so /health
                     ;; reported the config value while a seeded run shared
                     ;; freely — it once said sharing was off during a run that
                     ;; had served 91 shared artifacts.
                     :share_artifacts (seeded? conn run-id)
                     ;; What the run has spent, beside the budget the row
                     ;; carries, so an operator watching a metered provider
                     ;; can see the one against the other (karamazov-aqsr.3).
                     :usage (journal/run-usage conn run-id)))
       ;; Reuses the rows already read for the active count above.
       :branches (mapv #(update % :thesis parse-json) branches)
       :artifacts (mapv #(update % :witness parse-json)
                        (journal/artifacts conn run-id))
       :gates (journal/gate-tally conn run-id)
       :interventions (interventions/history conn run-id)
       ;; The board and the tree, for the panels that show what is being
       ;; worked on and what has changed under it. Both are queries over
       ;; tables the loop already appends to, so they need no cooperation
       ;; from the run and work the same on a finished one.
       :tasks (tasks/board conn {:run-id run-id})
       :modified (journal/run-writes conn run-id (gates/threshold :modified-files-shown))})))

(defn journal-tail
  "Everything after `since`. The `next` cursor is what the client sends back,
  so a poller never has to reason about timestamps or ordering."
  [conn run-id since limit]
  (let [events (journal/events-since conn run-id (or since 0)
                                     ;; provenance R3-12: as list-runs — a negative
                                     ;; limit is LIMIT -1, i.e. no limit.
                                     (max 0 (or limit 200)))]
    {:run_id run-id
     :events (mapv #(update % :data parse-json) events)
     :next (or (:id (last events)) (or since 0))
     :count (count events)}))

(defn turn-detail
  "One turn, whole — including the text `branch-detail` deliberately drops.

  `branch-turns` leaves out assistant_text and reasoning_text because they
  are the bulk: on one real run, 5.5MB against 62KB of results, enough that
  the branch panel exceeded its socket timeout and never rendered. But the
  model's prose is the substance of what a reader is following, so it has to
  arrive somehow — one turn at a time, for the turns actually on screen.

  This is `fetch_turn`'s query with an HTTP door on it."
  [conn run-id branch-id turn]
  (when-let [t (journal/branch-turn conn run-id branch-id turn)]
    (update t :args parse-json)))

(defn steps-tail
  "The live manifest-state trace after `since` — the implementer walking its
  state graph, for a front end to draw.

  Deliberately the same shape as `journal-tail`, down to `next` and `count`,
  so a client runs one poll loop over both feeds instead of two designs. It
  takes no `conn`: steps are held in memory by samizdat.steps and are not a
  durable record (see that namespace on why).

  `dropped` is what this client missed to the ring's bound — 0 while it keeps
  up. It is reported rather than hidden so a UI can say the trace has a hole
  in it instead of drawing one that looks continuous."
  [run-id since limit]
  (let [{:keys [steps cursor dropped]} (steps/since run-id (or since 0)
                                                    (max 0 (or limit 200)))]
    {:run_id run-id
     ;; Keywords on the bus, strings on the wire: :node, :cell and
     ;; :transition are what a client renders, and a JSON reader hands back a
     ;; string either way. `str` on a keyword keeps the colon, so a cell went
     ;; over as ":loop/assemble" and a panel drew it that way; `kw-name`
     ;; prints the name, namespace and all.
     :steps (mapv (fn [s] (-> s
                              (update :node kw-name)
                              (update :cell kw-name)
                              (update :transition kw-name)))
                  steps)
     :next cursor
     :count (count steps)
     :dropped dropped}))

(defn branch-detail [conn run-id branch-id]
  (when-let [b (runs/get-branch conn run-id branch-id)]
    {:branch (update b :thesis parse-json)
     :turns (journal/branch-turns conn run-id branch-id)
     ;; Gates that fired but whose predictions never settled — the run's own
     ;; account of advice that went unheeded, surfaced where the turns it
     ;; targeted are read.
     :unsettled-gates (journal/unsettled-gates conn run-id branch-id)
     :artifacts (mapv #(update % :witness parse-json)
                      (journal/artifacts conn run-id branch-id))}))
