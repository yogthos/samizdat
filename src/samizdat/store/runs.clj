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

(ns samizdat.store.runs
  "Run and branch lifecycle.

  A run has an identity and a lifecycle independent of the HTTP request that
  started it, so `POST /v1/chat/completions` can block on one for OpenAI
  compatibility while `POST /v1/runs` starts one and returns. A branch is an
  entity with a durable id rather than a value threaded through the loop,
  because an intervention has to be able to name one."
  (:require [clojure.data.json :as json]
            ;; log is called from start-run!'s retention sweep. jolt resolves
            ;; ns aliases lazily, so leaving this out loads fine and throws on
            ;; the first run start that actually prunes — which made enabling
            ;; :retention :run-record-days break every subsequent run start
            ;; (karamazov-blt.31).
            [clojure.tools.logging :as log]
            [jdbc.core :as jdbc]
            [samizdat.store.db :as db]
            [samizdat.store.journal :as journal]
            [samizdat.lexicon :as lexicon]))

(defn start-run!
  "Open a run and return its id."
  [conn {:keys [problem provider model max-turns beam-width prompt-digest]}]
  (let [id (str (random-uuid))]
    ;; The retention sweep (provenance R2-11), on run START rather than at
    ;; finish: a client tailing a just-finished run still reads its
    ;; :run-finished entry to learn the run ended.
    ;;
    ;; Two windows, and they are different KINDS of decision. Events are a
    ;; tail buffer and go on their own; the run RECORD is what a resume
    ;; replays from and what makes a crashed run inspectable, so it is kept
    ;; forever unless an operator says otherwise. Both are gates.edn
    ;; :retention — the events window was (* 24 3600) here, a number nobody
    ;; could change without a rebuild.
    (let [{:keys [events-hours run-record-days]} (lexicon/policy :retention)
          ago (fn [seconds]
                (str (.minusSeconds (java.time.Instant/now) (long seconds))))]
      (when events-hours
        (journal/prune-finished! conn (ago (* events-hours 3600))))
      (when run-record-days
        (let [n (journal/prune-run-record! conn (ago (* run-record-days 86400)))]
          (when (pos? n)
            (log/info "retention: dropped the record of" n "run(s) older than"
                      run-record-days "days")))))
    (db/with-writer
      (db/execute! conn
                     ["INSERT INTO runs (id, problem, status, provider, model, max_turns,
                                         beam_width, prompt_digest, started_at)
                       VALUES (?, ?, 'running', ?, ?, ?, ?, ?, ?)"
                      ;; The columns are NOT NULL DEFAULT '', and a DEFAULT does
                      ;; not apply to an explicitly-inserted NULL, so these
                      ;; coerce rather than relying on the schema.
                      id problem (if provider (name provider) "") (or model "")
                      (or max-turns 0) (or beam-width 1) (or prompt-digest "")
                      (db/now)]))
    (journal/note! conn id :run-started {:data {:problem problem :model model}})
    id))

(defn finish-run!
  "Terminal only from 'running', decided by the ROW rather than the caller
  (provenance R2-4): abort!'s transient window could overwrite a run that completed
  between its registry read and this write. Returns rows written; 0 means the
  run was already terminal and nothing changed, the journal says nothing."
  [conn run-id status final-answer]
  (let [n (db/with-writer
            (db/execute! conn
                         ["UPDATE runs SET status = ?, final_answer = ?, ended_at = ?
                            WHERE id = ? AND status = 'running'"
                          (name status) final-answer (db/now) run-id])
            (db/change-count conn))]
    (when (pos? n)
      (journal/note! conn run-id :run-finished {:data {:status status}}))
    n))

(defn reconcile-orphans!
  "Mark every run still claiming to be running as interrupted. Returns how many.

  `status = 'running'` is written once when the beam opens and revisited only
  when it finishes, so a process that dies mid-run leaves the row asserting
  forever: it shows up in the run list and in any 'is anything active' check,
  and nothing in the row itself says otherwise. gen-18 and gen-11 both sat that
  way for days, and the second was filed as its own bug before anyone noticed
  it was the same defect — which is the argument for reconciling rather than
  correcting rows by hand each time.

  Sound only AT STARTUP, and that is the whole trick. In general a row cannot
  tell a live run from a dead one. But the beam only ever runs in this process,
  so at the moment this process comes up, nothing is running by construction
  and every such row is a leftover.

  `interrupted` rather than `failed` or `completed`: the run neither errored nor
  finished, and recording either would be a lie about what happened. `ended_at`
  is set because a run with no end time makes every duration computed from it
  wrong."
  [conn]
  (let [orphans (db/fetch conn ["SELECT id FROM runs WHERE status = 'running'"])]
    (doseq [{:keys [id]} orphans]
      (db/with-writer
        (db/execute! conn
                     ["UPDATE runs SET status = 'interrupted', ended_at = ?
                        WHERE id = ? AND status = 'running'"
                      (db/now) id]))
      (journal/note! conn id :run-finished
                     {:data {:status "interrupted"
                             :reason "no process was running it when the server started"}}))
    (count orphans)))

(defn mark-running!
  "Put a run back into 'running' and clear its end time.

  For resume. Before crashed runs were reconciled, a resumable row still read
  'running' from its original start, so nothing had to set this and nothing
  did. Reconciling to 'interrupted' made the omission visible: gen-19 resumed,
  took turns, and its row still said interrupted.

  Not cosmetic. `stalled?` only reports on a run whose status is 'running', so
  a resumed run that wedges would be invisible to the one check built to notice
  precisely that — and ended_at has to go with it, or the run carries an end
  time earlier than half its turns."
  [conn run-id]
  (db/with-writer
    (db/execute! conn
                 ["UPDATE runs SET status = 'running', ended_at = NULL
                    WHERE id = ? AND status NOT IN ('completed','aborted')"
                  run-id])
    (db/change-count conn)))

(defn get-run [conn run-id]
  (db/fetch-one conn ["SELECT * FROM runs WHERE id = ?" run-id]))

(defn last-progress-at
  "When this run last wrote anything to its journal, or nil if it never has.

  Every meaningful step emits an event, so this is the run's heartbeat without
  a heartbeat column: the id index gives it in one indexed row lookup, and
  ordering by id rather than created_at avoids trusting a clock for
  monotonicity."
  [conn run-id]
  (:created_at (db/fetch-one conn ["SELECT created_at FROM events WHERE run_id = ?
                                    ORDER BY id DESC LIMIT 1" run-id])))

(defn stalled?
  "True when a run still claims to be running but has been silent longer than
  `threshold-ms`.

  gen-11 crashed mid-round and sat at status 'running' with no events for the
  rest of the night, and nothing could tell that from a healthy slow round —
  both look like a row saying `running`. Silence alone is not enough (a Lean
  tactic legitimately buys minutes of it), so the threshold belongs above the
  turn deadline; silence plus a status nobody will ever update is the signal.

  Only ever reports on a RUNNING run. A finished one is quiet because it is
  over, which is not the same fault and must not raise the same alarm."
  [conn run-id threshold-ms]
  (let [r (get-run conn run-id)]
    (boolean
     (and r
          (= "running" (:status r))
          (when-let [t (or (last-progress-at conn run-id) (:started_at r))]
            (try
              (> (- (System/currentTimeMillis)
                    (.toEpochMilli (java.time.Instant/parse t)))
                 threshold-ms)
              ;; An unparseable timestamp is a storage fault, not evidence the
              ;; run is wedged. Say nothing rather than cry wolf.
              (catch Throwable _ false)))))))

(defn list-runs
  ([conn] (list-runs conn 50))
  ([conn limit]
   (db/fetch conn ["SELECT * FROM runs ORDER BY started_at DESC LIMIT ?" limit])))

(defn open-branch!
  "`:problem` is the branch's OWN problem when it differs from the run's — a
  decompose unit's contract, a team worker's sub-task — and nil for a branch
  working the run-level problem. It is what a resume rebuilds the branch's
  opening messages from (karamazov-blt.23)."
  [conn run-id {:keys [branch-id parent-id created-at-turn problem]}]
  ;; IDEMPOTENT, because a resumed run re-opens branch ids it already has.
  ;; Branch ids are round-scoped by construction (T0, T0v1, T0r1), so after a
  ;; crash and resume the board claims the same task to the same id and the
  ;; plain INSERT died on the (run_id, id) primary key — taking the board
  ;; stage down with it and surfacing to the supervisor as a crash it then
  ;; spent half a run chasing (karamazov-otd, found live on run e0c7662f).
  ;;
  ;; OR IGNORE rather than an upsert, deliberately: the existing row is this
  ;; same branch's record, and the one thing it holds that we must not
  ;; overwrite is how it ENDED. close-branch! refuses to rewrite a closed
  ;; branch's status for exactly that reason (provenance R2-4); re-opening
  ;; must not do through the back door what closing refuses to do directly.
  ;; So a rejoin keeps the row and says so in the journal.
  (let [n (db/with-writer
            (db/execute! conn
                         ["INSERT OR IGNORE INTO branches (id, run_id, parent_id, status, created_at_turn, problem)
                           VALUES (?, ?, ?, 'active', ?, ?)"
                          branch-id run-id parent-id (or created-at-turn 0) problem]))]
    (journal/note! conn run-id (if (pos? n) :branch-opened :branch-rejoined)
                   {:branch-id branch-id :data {:parent parent-id}}))
  branch-id)

(defn close-branch!
  "Only from 'active' (provenance R2-4): a late close used to overwrite a closed
  branch's status and inactive_reason — the column the cull-honesty work
  exists to keep truthful. Returns rows written."
  [conn run-id branch-id status reason]
  (let [n (db/with-writer
            (db/execute! conn
                         ["UPDATE branches SET status = ?, inactive_reason = ?
                            WHERE run_id = ? AND id = ? AND status = 'active'"
                          (name status) reason run-id branch-id])
            (db/change-count conn))]
    (when (pos? n)
      (journal/note! conn run-id :branch-closed
                     {:branch-id branch-id :data {:status status :reason reason}}))
    n))

(defn extend-budget!
  "Raise a run's max_turns. Only ever called with an explicitly requested
  value: the resume path keeps the original budget unless one is passed, so
  a crash cannot re-grant turns — extension is a human act, journaled as one."
  [conn run-id max-turns]
  (db/with-writer
    (db/execute! conn ["UPDATE runs SET max_turns = ? WHERE id = ?"
                       max-turns run-id]))
  (journal/note! conn run-id :budget-extended {:data {:max-turns max-turns}}))

(defn reopen-branch!
  "An exhausted branch back to active — the budget-extension path. Branches
  closed for cause (culled, abandoned, done) are never reopened; exhaustion
  is the one closing reason that names the budget rather than the branch."
  [conn run-id branch-id]
  (db/with-writer
    (db/execute! conn ["UPDATE branches SET status = 'active', inactive_reason = NULL
                        WHERE run_id = ? AND id = ? AND status = 'exhausted'"
                       run-id branch-id]))
  (journal/note! conn run-id :branch-reopened {:branch-id branch-id}))

(defn set-thesis!
  "The branch's current structural plan. Overwriting is allowed — committing to
  a different route is a legitimate move — and the change is journalled."
  [conn run-id branch-id thesis]
  (db/with-writer
    (db/execute! conn
                   ["UPDATE branches SET thesis = ? WHERE run_id = ? AND id = ?"
                    (json/write-str thesis) run-id branch-id]))
  (journal/note! conn run-id :thesis {:branch-id branch-id :data thesis}))

(defn branches [conn run-id]
  (db/fetch conn ["SELECT * FROM branches WHERE run_id = ? ORDER BY id" run-id]))

(defn get-branch [conn run-id branch-id]
  (db/fetch-one conn ["SELECT * FROM branches WHERE run_id = ? AND id = ?"
                        run-id branch-id]))
