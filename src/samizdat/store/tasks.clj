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

(ns samizdat.store.tasks
  "The task board: work grounded in durable rows, not context state.

  dirge's issues schema generalized. A task can have a parent, an epic is a
  type of task, so the hierarchy is as deep as the model wants it. run_id NULL
  is the passive backlog; a set run_id claims the task onto that run's active
  board. Because the board is regenerated from these rows rather than carried
  in any message history, compaction cannot lose it and a resumed run finds
  it exactly where it was.

  contract and tests are what make a task a delegable unit: the spec the work
  must satisfy and the tests that define delivery. A subagent handed a task id
  has, in the row itself, a clear definition of what it must produce."
  (:require [clojure.string :as str]
            [samizdat.store.db :as db]))

(def ^:private status-aliases
  ;; dirge's vocabulary. Models say todo/wip/completed/wontfix; the board must
  ;; not fork into synonym lanes.
  {"open" "open" "todo" "open" "backlog" "open" "pending" "open"
   "in_progress" "in_progress" "wip" "in_progress" "doing" "in_progress"
   "blocked" "blocked"
   "done" "done" "completed" "done" "finished" "done"
   "cancelled" "cancelled" "canceled" "cancelled" "wontfix" "cancelled"})

(def ^:private priority-aliases
  ;; Bare 0-4 are accepted alongside p0-p4: a model naturally writes
  ;; priority 2, not "p2", and the docs say "0-4 or P0-P4".
  {"high" "high" "p0" "high" "p1" "high" "0" "high" "1" "high" "urgent" "high"
   "normal" "normal" "p2" "normal" "2" "normal" "medium" "normal"
   "low" "low" "p3" "low" "p4" "low" "3" "low" "4" "low" "minor" "low"})

(def terminal? #{"done" "cancelled"})

(defn normalize-status [s]
  (or (status-aliases (str/lower-case (str/trim (str s))))
      (throw (ex-info (str "unknown task status: " s)
                      {:status s :valid (sort (distinct (vals status-aliases)))}))))

(defn normalize-priority [p]
  (or (priority-aliases (str/lower-case (str/trim (str p))))
      (throw (ex-info (str "unknown task priority: " p)
                      {:priority p :valid (sort (distinct (vals priority-aliases)))}))))

(defn get-task [conn id]
  (db/fetch-one conn ["SELECT * FROM tasks WHERE id = ?" id]))

(defn- new-id
  "A short, human-repeatable handle. Six hex chars is 16M ids; the insert
  retries on the rare collision rather than paying for global uniqueness."
  []
  (str "sz-" (subs (str/replace (str (random-uuid)) "-" "") 0 6)))

(defn create!
  "Insert a task and return its id. Unset fields take the dirge defaults:
  type task, status open, priority normal, no parent, backlog (no run)."
  [conn {:keys [title body type status priority parent-id run-id contract tests
                stub-file stubs]}]
  (when (str/blank? (str title))
    (throw (ex-info "a task needs a title" {})))
  (when (and parent-id (nil? (get-task conn parent-id)))
    (throw (ex-info (str "parent task " parent-id " does not exist")
                    {:parent-id parent-id})))
  (let [status (normalize-status (or status "open"))
        priority (normalize-priority (or priority "normal"))
        now (db/now)]
    (loop [attempt 1]
      (let [id (new-id)
            r (try
                (db/with-writer
                  (db/execute! conn
                               ["INSERT INTO tasks (id, title, body, type, status, priority,
                                                    parent_id, run_id, contract, tests,
                                                    stub_file, stubs,
                                                    created_at, updated_at, closed_at)
                                 VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
                                id (str title) (or body "") (or type "task")
                                status priority parent-id run-id
                                (or contract "") (or tests "")
                                ;; The checkable half of the delegation spec:
                                ;; which names, in which file. `contract` says
                                ;; what to build and is for the model; these
                                ;; two are how the ship gate asks whether it
                                ;; was built (v21).
                                (or stub-file "")
                                (str/join "," (remove str/blank? (map str (or stubs []))))
                                now now (when (terminal? status) now)]))
                id
                (catch Throwable e
                  (if (and (db/id-collision? e) (< attempt 5))
                    ::retry
                    (throw e))))]
        (if (= ::retry r) (recur (inc attempt)) r)))))

(defn update!
  "Update the given fields, bump updated_at, and keep closed_at honest: a
  transition into a terminal status stamps it, a transition out clears it.

  The write names ONLY the fields the caller passed (provenance R2-1): the old
  full-row rewrite from a (possibly stale) read silently erased whatever a
  concurrent writer had landed in between — a claim, another run's edit — the
  a#4 race class one def over from the guarded claim!. An updated_at guard
  was tried and rejected: db/now has millisecond precision, so a claim in the
  same millisecond as the create it raced passed the guard. A field-scoped
  write cannot clobber what it never names."
  [conn id {:keys [title body type status priority parent-id run-id contract tests plan]}]
  (let [t (get-task conn id)]
    (when-not t
      (throw (ex-info (str "no task " id) {:id id})))
    (let [status (some-> status normalize-status)
          priority (some-> priority normalize-priority)
          closed-at (when status
                      (cond
                        (terminal? status) (or (:closed_at t) (db/now))
                        :else nil))
          ;; Column -> value for exactly the fields given. status drags
          ;; closed_at along so the stamp stays honest; nil still means
          ;; "not passed" everywhere (a field cannot be cleared to NULL via
          ;; update!, same contract as before).
          cols (cond-> {}
                 title (assoc :title title)
                 body (assoc :body body)
                 type (assoc :type type)
                 status (assoc :status status :closed_at closed-at)
                 ;; `open` MEANS claimable (RFC-008: branch_id NULL is the
                 ;; claimable state). Setting status open while the holder
                 ;; stayed attributed produced a row that read open but that
                 ;; claim!'s guard refused to everyone (karamazov-blt.34).
                 (= "open" status) (assoc :branch_id nil)
                 priority (assoc :priority priority)
                 parent-id (assoc :parent_id parent-id)
                 run-id (assoc :run_id run-id)
                 ;; The approved implementation plan, persisted as the
                 ;; task's contract (karamazov-vale).
                 plan (assoc :plan plan)
                 contract (assoc :contract contract)
                 tests (assoc :tests tests))]
      (db/with-writer
        (db/execute! conn
                     (into [(str "UPDATE tasks SET "
                                 (str/join ", " (map #(str (name %) " = ?") (keys cols)))
                                 ", updated_at = ? WHERE id = ?")]
                           (concat (vals cols) [(db/now) id])))))
    (get-task conn id)))

(defn claim!
  "Assign a task to a BRANCH of a run and mark it in_progress. Returns the
  updated task, or nil when somebody else already holds it.

  First-writer-wins decided by the ROW: the UPDATE itself guards on the claim
  being free, because a read-then-write pair is two lock acquisitions and two
  branches whose reads both saw the unclaimed row could both write (provenance A-4,
  docs/provenance.md).

  THE HOLDER IS A BRANCH, not a run, and that distinction is the whole point of
  migration v12. The guard used to be `(run_id IS NULL OR run_id = ?)` with only
  run_id set, which is exclusive between runs and a NO-OP within one — so in a
  team workflow, where several implementors fan out over one feature as branches
  of a single run, two workers both claimed the same task and both believed they
  held it. That is precisely the case the board exists to arbitrate.

  Re-claiming what you already hold is idempotent, so a branch does not have to
  remember whether it called this."
  [conn id run-id branch-id]
  (db/with-writer
    (db/execute! conn
                 ["UPDATE tasks SET run_id = ?, branch_id = ?, status = 'in_progress',
                                    updated_at = ?, closed_at = NULL
                   WHERE id = ?
                     AND (branch_id IS NULL OR (run_id = ? AND branch_id = ?))
                     AND closed_at IS NULL
                     AND status NOT IN ('done','cancelled')"
                  run-id branch-id (db/now) id run-id branch-id]))
  (let [t (get-task conn id)]
    (when (and t (= run-id (:run_id t)) (= branch-id (:branch_id t))
               (= "in_progress" (:status t)))
      t)))

(defn release!
  "Let go of a task without closing it — back to the board, claimable again.

  What a switch does to the task being set down. Without it a task a branch
  abandoned stays attributed to that branch forever, which reads as work in
  progress that nobody is doing: the worst state for a shared board, because it
  is indistinguishable from work that is progressing."
  [conn id branch-id]
  (db/with-writer
    (db/execute! conn
                 ["UPDATE tasks SET branch_id = NULL, status = 'open',
                                    updated_at = ?
                   WHERE id = ? AND branch_id = ? AND closed_at IS NULL"
                  (db/now) id branch-id]))
  (get-task conn id))

(defn close!
  "Mark a task done (or cancelled)."
  ([conn id] (close! conn id "done"))
  ([conn id status]
   (let [status (normalize-status status)]
     (when-not (terminal? status)
       (throw (ex-info (str "close! wants a terminal status, got " status) {})))
     (update! conn id {:status status}))))

(defn attempted!
  "Record one more attempt on `id`, and return the new count.

  The number the recursion escalates on. It was kept in memory, so a resumed
  run re-litigated every unit from zero and 'is this making progress' could
  only be asked of a live branch, never of the task (v21). Incremented in the
  UPDATE rather than read-then-written, for the reason `claim!` guards in the
  row: two branches attempting one task would both read the same count."
  [conn id]
  (db/with-writer
    (db/execute! conn ["UPDATE tasks SET attempts = attempts + 1, updated_at = ?
                        WHERE id = ?" (db/now) id]))
  (:attempts (first (db/fetch conn ["SELECT attempts FROM tasks WHERE id = ?" id]))))

(defn children-of [conn id]
  (db/fetch conn ["SELECT * FROM tasks WHERE parent_id = ? ORDER BY created_at, id" id]))

(def ^:private board-order
  "dirge's board ordering: what is moving first, then what matters, then what
  moved recently."
  " ORDER BY CASE status WHEN 'in_progress' THEN 0 WHEN 'blocked' THEN 1 ELSE 2 END,
             CASE priority WHEN 'high' THEN 0 WHEN 'normal' THEN 1 ELSE 2 END,
             updated_at DESC, id DESC")

(defn board
  "Active (non-terminal) tasks. With a :run-id, the run's own tasks plus the
  unclaimed backlog — another run's claimed work is its own business."
  [conn {:keys [run-id]}]
  (if run-id
    (db/fetch conn [(str "SELECT * FROM tasks
                          WHERE status NOT IN ('done','cancelled')
                            AND (run_id = ? OR run_id IS NULL)" board-order)
                    run-id])
    (db/fetch conn [(str "SELECT * FROM tasks
                          WHERE status NOT IN ('done','cancelled')" board-order)])))

(defn backlog
  "Unclaimed, non-terminal tasks."
  [conn]
  (db/fetch conn [(str "SELECT * FROM tasks
                        WHERE run_id IS NULL
                          AND status NOT IN ('done','cancelled')" board-order)]))

(defn held-by
  "The non-terminal task a branch currently holds on this run, or nil.

  The claim a resume must restore: the row survives the crash with its
  branch_id set, but the rebuilt branch used to come back with no :task —
  telling the model 'No task claimed', letting it claim a SECOND task (the
  one-task rule reads the branch), and leaving the old row in_progress and
  attributed to it forever, which is RFC-008's named worst state for a
  shared board (karamazov-blt.21)."
  [conn run-id branch-id]
  (db/fetch-one conn ["SELECT * FROM tasks
                        WHERE run_id = ? AND branch_id = ?
                          AND status NOT IN ('done','cancelled')
                        LIMIT 1"
                      run-id branch-id]))
