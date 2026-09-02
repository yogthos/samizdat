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
  "The directive queue: the ONE write path into a running run (RFC-012).

  Three writers share it — a person at the control API, the supervisor's
  reasoning pass through the `intervene` tool, and the supervisor's reflex —
  and `issued_by` says which. A directive applies at the next boundary, never
  mid-turn: a branch inside a provider call or a tool is not in a state anyone
  should mutate. That means a UI has to show pending versus applied honestly
  rather than pretending a click took effect, which is why `status` and
  `disposition` are stored rather than inferred.

  THREE BOUNDARIES OWN THE KINDS, and each drain leaves alone what it does not
  own rather than rejecting it: a branch's own steer boundary takes what is
  about that branch, the beam's round top takes what is about the run, and a
  workflow's own directives stage takes what is about the workflow's next
  round. A kind rejected at the wrong boundary is a directive that never
  reached the right one, which is what happened to every scheduler kind before
  karamazov-blt.10 and would have happened to every workflow kind here.

  Abort is the exception and does not come through here. It goes to the
  supervisor, because a run that is wedged is exactly the run that will never
  reach another boundary to drain a queue at."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            ;; db.jdbc registers the java.sql shim clojure.jdbc compiles against and
            ;; points connection construction at the native driver; it has to load
            ;; before jdbc.core.
            [db.jdbc]
            [jdbc.core :as jdbc]
            [samizdat.store.db :as db]
            [samizdat.store.journal :as journal]))

(def branch-kinds
  "Applied at a branch's own steer boundary."
  #{"message" "review"})

(def scheduler-kinds
  "Applied at the beam's round top: facts about the run or its set of
  branches."
  #{"cull" "fork" "retract" "extend" "pause" "resume"})

(def workflow-kinds
  "Applied at a workflow's own directives stage — the feature loop's, between
  its verify and its route — because they decide that workflow's NEXT ROUND
  and nothing at the turn or scheduler level has anywhere to put them."
  #{"switch" "budget" "stop"})

(def kinds
  "Every directive kind that may be queued: the NAMES, which are mechanism —
  each has a drain that applies it. What each one does, in words a person and
  the supervisor are shown, is `wordlists.edn :directive-kinds`, read by the
  surfaces that show them (the intervene tool, the control API), because
  every sentence the model reads has to be editable without a rebuild."
  (into #{} cat [branch-kinds scheduler-kinds workflow-kinds]))

(defn payload
  "A directive's JSON payload as a map, or {} — a payload that will not parse
  is a malformed request, not a reason to take a boundary down."
  [d]
  (let [p (:payload d)]
    (cond
      (map? p) p
      :else (or (try (let [v (json/read-str (str p) :key-fn keyword)]
                       (when (map? v) v))
                     (catch Throwable _ nil))
                {}))))

(defn text-of
  "The words a directive carries: the `text` of its payload, or the payload
  itself when it is a bare string (`message` and `fork` arrive that way from
  the intervene tool)."
  [d]
  (let [p (:payload d)]
    (if (and (string? p) (not (str/starts-with? (str/trim p) "{")))
      p
      (some-> (payload d) :text str))))

(defn turns-asked
  "How many turns a directive asks for, or nil.

  One parser for both shapes a directive arrives in. The control API sends
  {\"turns\": N} (or `by` / `max_turns`); the intervene tool sends every
  structural kind as {\"text\": \"...\"}, and reading only the first shape
  meant the supervisor's `extend` was refused every time it was tried — with
  a reason naming a payload the tool cannot produce."
  [d]
  (let [p (payload d)
        raw (or (:turns p) (:by p) (:max_turns p)
                (some-> (:text p) str str/trim not-empty))
        n (cond (number? raw) (long raw)
                (string? raw) (parse-long raw)
                :else nil)]
    (when (and n (pos? n)) n)))

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
