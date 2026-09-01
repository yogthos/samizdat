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

(ns samizdat.store.interventions
  "The human directive queue.

  A directive applies at the next branch boundary, never mid-turn: a branch
  inside a provider call or a Lean tactic is not in a state anyone should
  mutate. That means a UI has to show pending versus applied honestly rather
  than pretending a click took effect, which is why `status` and `disposition`
  are stored rather than inferred.

  Abort is the exception and does not come through here. It goes to the
  supervisor, because a run that is wedged is exactly the run that will never
  reach another boundary to drain a queue at."
  (:require [clojure.data.json :as json]
            ;; db.jdbc registers the java.sql shim clojure.jdbc compiles against and
            ;; points connection construction at the native driver; it has to load
            ;; before jdbc.core.
            [db.jdbc]
            [jdbc.core :as jdbc]
            [samizdat.store.db :as db]
            [samizdat.store.journal :as journal]))

(def kinds
  "What a human may ask for. Each is applied by the arbiter or the scheduler at
  a boundary; nothing here mutates a branch directly."
  {"message" "Inject text into a branch's next turn."
   "review" "Tell a branch to cross-check and ship what it has."
   "cull" "Stop a branch. Refused if it is the last one running."
   "fork" "Open a sibling branch on a stated thesis."
   "retract" "Un-confirm an artifact that was not what it claimed. Payload: {\"artifact_id\": N, \"reason\": \"...\"}."
   "extend" "Raise the run's turn cap."
   "pause" "Stop scheduling new turns; in-flight turns finish."
   "resume" "Resume scheduling."})

(defn submit!
  [conn run-id {:keys [branch-id kind payload issued-by]}]
  (when-not (contains? kinds kind)
    (throw (ex-info (str "Unknown intervention kind: " kind)
                    {:kind kind :known (sort (keys kinds))})))
  (let [id (db/with-writer
             (db/execute! conn
                            ["INSERT INTO interventions (run_id, branch_id, kind, payload,
                                                         issued_by, status, created_at)
                              VALUES (?, ?, ?, ?, ?, 'pending', ?)"
                             run-id branch-id kind
                             (if (string? payload) payload (json/write-str (or payload {})))
                             (or issued-by "human") (db/now)])
             (db/last-insert-id conn))]
    (journal/note! conn run-id :intervention-submitted
                   {:branch-id branch-id :data {:id id :kind kind}})
    id))

(defn pending
  "Directives waiting for a boundary. Scoped to a branch when given one, plus
  the run-wide ones that apply to every branch."
  ([conn run-id]
   (db/fetch conn ["SELECT * FROM interventions
                      WHERE run_id = ? AND status = 'pending' ORDER BY id" run-id]))
  ([conn run-id branch-id]
   (db/fetch conn ["SELECT * FROM interventions
                      WHERE run_id = ? AND status = 'pending'
                        AND (branch_id = ? OR branch_id IS NULL) ORDER BY id"
                     run-id branch-id])))

(defn paused?
  "Whether this run is currently paused: the most recently APPLIED pause or
  resume was a pause.

  Derived rather than stored, so there is one answer and it survives a process
  restart — a `runs` column would be a second copy that a crash between the
  directive and the column write could leave disagreeing with the record the
  run is judged by. Only `applied` rows count: a pause the scheduler refused
  never took effect, and a pending one has not reached a boundary yet."
  [conn run-id]
  (= "pause" (:kind (db/fetch-one
                     conn
                     ["SELECT kind FROM interventions
                         WHERE run_id = ? AND status = 'applied'
                           AND kind IN ('pause','resume')
                         ORDER BY id DESC LIMIT 1"
                      run-id]))))

(defn resolve!
  "Record what the arbiter or scheduler did with a directive. `status` is
  applied or rejected, and `disposition` says why — a directive refused because
  it would cull the last branch is a different thing from one that landed, and
  the UI has to be able to tell them apart.

  Decided by the ROW (provenance R2-4): only a still-pending directive of THIS run
  resolves — a double resolve must not overwrite the first disposition, and a
  resolve under the wrong run-id must not journal into that run. Returns rows
  written."
  [conn run-id id status disposition turn]
  (let [n (db/with-writer
            (db/execute! conn
                         ["UPDATE interventions SET status = ?, disposition = ?, applied_at_turn = ?
                            WHERE id = ? AND run_id = ? AND status = 'pending'"
                          (name status) disposition turn id run-id])
            (db/change-count conn))]
    (when (pos? n)
      (journal/note! conn run-id :intervention-resolved
                     {:turn turn :data {:id id :status status :disposition disposition}}))
    n))

(defn history [conn run-id]
  (db/fetch conn ["SELECT * FROM interventions WHERE run_id = ? ORDER BY id" run-id]))
