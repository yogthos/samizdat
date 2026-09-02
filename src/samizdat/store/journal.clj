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

(ns samizdat.store.journal
  "Append-only writes for a run, and the queries that read them back.

  Everything is written when it happens rather than assembled at the end. Two
  things depend on that. A run killed mid-flight stays fully inspectable, which
  is what makes SQLite worth having over an in-memory result. And the read API
  serves a live run and a finished one with the same query, so a UI needs no
  cooperation from the loop.

  Every append also emits an event carrying a monotonic cursor, which is what
  `GET /v1/runs/:id/journal?since=N` reads. The loop calls these; nothing calls
  the loop."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.tools.logging :as log]
            ;; db.jdbc registers the java.sql shim clojure.jdbc compiles against and
            ;; points connection construction at the native driver; it has to load
            ;; before jdbc.core.
            [db.jdbc]
            [jdbc.core :as jdbc]
            [samizdat.events :as events]
            [samizdat.session :as session]
            [samizdat.store.db :as db]))

(defn- js
  "A value as JSON text for a journal column.

  Falls back to pr-str when the value holds something data.json cannot write.
  The journal is a RECORD of the run; it must not be able to destroy the work
  it is recording. gen-31 lost two branches — both on the run's actual target —
  to a Don't-know-how-to-write-JSON-of-class-java.lang.Character thrown from
  here: a tool's args held a String where a collection was expected, it seq'd
  into Characters, and the exception propagated out of the turn writer to the
  beam, which abandoned the branch. That specific cause is fixed in the XML
  parameter parser; this is so the NEXT unserialisable value nobody predicted
  costs a readable args column instead of a line of mathematics.

  Degrading rather than dropping, because an args column that silently went
  empty would be the same class of lie as a false cull reason: the record would
  look like a turn that passed no arguments."
  [v]
  (if (string? v)
    v
    (try (json/write-str v)
         (catch Throwable e
           (log/warn "journal: falling back to pr-str for a value data.json"
                     "could not write:" (ex-message e))
           (pr-str v)))))

(defn- emit!
  "Record an event and publish it. The row is the durable copy the tail
  endpoint reads; the publish is for anything watching live."
  [conn run-id kind {:keys [branch-id turn data]}]
  (let [now (db/now)]
    (db/with-writer
      (db/execute! conn
                     ["INSERT INTO events (run_id, branch_id, turn, kind, data, created_at)
                       VALUES (?, ?, ?, ?, ?, ?)"
                      run-id branch-id turn (name kind) (js (or data {})) now])
      (let [id (db/last-insert-id conn)]
        (events/publish! {:id id :run-id run-id :branch-id branch-id
                          :turn turn :kind kind :data data :created-at now})
        id))))

;; --- turns ------------------------------------------------------------------

(defn record-turn!
  "One model turn: what it called, with what, and what came back.

  `category` is :success, :failure, :neutral or :mechanics — the last for a
  turn that produced no usable tool call, kept distinct so the record can tell
  a branch that was wrong from one that could not format a fence. It is what
  the cull and
  progress guards read. It is recorded rather than derived later because the
  tool that produced it knows, and a reconstruction would be guessing."
  [conn run-id {:keys [branch-id turn tool-name args result category
                       parse-error auto-repaired assistant-text reasoning-text
                       usage policy-refusal?]}]
  (db/with-writer
    (db/execute! conn
                   ["INSERT INTO turns (run_id, branch_id, turn, tool_name, args, result,
                                        category, parse_error, auto_repaired,
                                        assistant_text, reasoning_text, created_at,
                                        prompt_tokens, completion_tokens, total_tokens,
                                        cache_hit_tokens, cache_miss_tokens,
                                        policy_refusal)
                     VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
                    run-id branch-id turn (str tool-name) (js (or args {}))
                    (str result) (some-> category name) parse-error
                    (if auto-repaired 1 0)
                    assistant-text reasoning-text (db/now)
                    ;; nil, not 0, when the turn had no response to cost —
                    ;; see migration v4. `usage` is absent on the
                    ;; provider-error path by construction.
                    (:prompt-tokens usage) (:completion-tokens usage)
                    (:total-tokens usage)
                    (:cache-hit-tokens usage) (:cache-miss-tokens usage)
                    (if policy-refusal? 1 0)]))
  (emit! conn run-id :turn {:branch-id branch-id :turn turn
                            :data {:tool tool-name :category category}}))

(defn run-usage
  "What a run has spent so far, summed over its turn rows: {:turns
  :prompt-tokens :completion-tokens :total-tokens}. A row with no usage — a
  provider error — counts as a turn and costs nothing. This is what the beam
  holds a run's token budget against (karamazov-aqsr.3)."
  [conn run-id]
  (let [r (first (db/fetch conn ["SELECT count(*) AS turns,
                                         coalesce(sum(prompt_tokens), 0) AS prompt_tokens,
                                         coalesce(sum(completion_tokens), 0) AS completion_tokens,
                                         coalesce(sum(total_tokens), 0) AS total_tokens
                                    FROM turns WHERE run_id = ?" run-id]))]
    {:turns (:turns r)
     :prompt-tokens (:prompt_tokens r)
     :completion-tokens (:completion_tokens r)
     :total-tokens (:total_tokens r)}))

(defn turns
  "Every turn of a run, whole rows. `assistant_text` comes back with them, so
  this is what resume replays from — and it is why nothing that merely
  DISPLAYS turns should call it. See `branch-turns`."
  [conn run-id]
  (db/fetch conn ["SELECT * FROM turns WHERE run_id = ? ORDER BY id" run-id]))

(defn branch-turns
  "One branch's turns, carrying only what a reader renders.

  Both halves matter. Filtering by branch in SQL uses the
  (run_id, branch_id, turn) index instead of dragging the whole run into
  memory to throw most of it away; dropping assistant_text and
  reasoning_text drops the bulk, which on one real run was 5.5MB against
  62KB of results. The branch panel used to fetch all of it, spend over two
  minutes doing so, and exceed the client's socket timeout — so the branch
  never rendered at all."
  [conn run-id branch-id]
  (db/fetch conn
            ["SELECT id, run_id, branch_id, turn, tool_name, args, result,
                     category, parse_error, auto_repaired, created_at
                FROM turns
               WHERE run_id = ? AND branch_id = ?
               ORDER BY turn, id"
             run-id branch-id]))

;; --- artifacts --------------------------------------------------------------

(defn- path-of
  "The `path` argument of a recorded turn, or nil. Args are stored as the JSON
  the model sent, so this reads them back the same way."
  [row]
  (try (some-> (json/read-str (str (:args row)) :key-fn keyword) :path str not-empty)
       (catch Throwable _ nil)))

(defn sibling-writes
  "What OTHER branches on this run have done to the tree: one entry per path,
  naming every branch that has changed it and the turn it was last touched,
  most recently touched first, capped at `limit`.

  Per PATH rather than per write, and naming every collaborator rather than
  the latest: the question a worker has is `who else is in this file`, and a
  list of the last writer alone answers a different one. Live, three branches
  changed src/kit/core.clj and reporting only the most recent would have named
  one of them.

  THE MAILBOX CARRIES WHAT A SIBLING SAID; THIS CARRIES WHAT IT DID. Team
  workers share one working tree by design — the parts of a feature belong in
  the same files — and the only thing that told a worker about its siblings was
  mail they chose to send. Live, three workers wrote src/kit/core.clj fifteen
  times between them, full-file `write_file` overwrites interleaved with
  surgical `edit_file`s, two of them landing on the same turn. The tree came
  out coherent because the last writer happened to hold a complete picture,
  which is not a mechanism.

  Derived from the journal rather than reported by the branches, so it cannot
  drift from what actually happened and costs no turn to produce."
  [conn run-id branch-id limit]
  (->> (db/fetch conn ["SELECT branch_id, turn, tool_name, args FROM turns
                      WHERE run_id = ? AND branch_id <> ?
                        AND tool_name IN ('write_file', 'edit_file')
                        AND category = 'success'
                      ORDER BY turn DESC, id DESC"
                    run-id (str branch-id)])
       (keep (fn [r] (when-let [p (path-of r)]
                       {:branch (:branch_id r) :turn (:turn r) :path p})))
       (reduce (fn [acc {:keys [path branch turn]}]
                 (let [i (first (keep-indexed #(when (= path (:path %2)) %1) acc))]
                   (if i
                     (update-in acc [i :branches] (fn [bs] (if (some #{branch} bs) bs (conj bs branch))))
                     (conj acc {:path path :turn turn :branches [branch]}))))
               [])
       (take limit)
       vec))

(defn changed-since-read
  "Whether a sibling changed `path` after this branch last READ it, and who.

  Returns {:branch :turn :tool} for the most recent such change, or nil — nil
  also when this branch has never read the path, because a branch writing a
  file it has not looked at is a different problem and this one has nothing to
  say about it.

  This is the moment the shared tree actually bites: `write_file` replaces a
  whole file, so a worker writing from its own picture of what belongs there
  silently drops everything a sibling added since it last looked. The notice
  is a NOTICE — the write goes through. Workers sharing a tree are
  collaborating, and a harness that refuses the write decides for them which
  version wins, which is exactly the judgement it does not have."
  [conn run-id branch-id path]
  (let [reads (db/fetch conn ["SELECT turn, args FROM turns
                            WHERE run_id = ? AND branch_id = ?
                              AND tool_name IN ('read_file', 'write_file', 'edit_file')
                            ORDER BY turn DESC, id DESC"
                           run-id (str branch-id)])
        last-seen (some (fn [r] (when (= path (path-of r)) (:turn r))) reads)]
    (when last-seen
      (->> (db/fetch conn ["SELECT branch_id, turn, tool_name, args FROM turns
                          WHERE run_id = ? AND branch_id <> ? AND turn >= ?
                            AND tool_name IN ('write_file', 'edit_file')
                            AND category = 'success'
                          ORDER BY turn DESC, id DESC"
                        run-id (str branch-id) (long last-seen)])
           (keep (fn [r] (when (= path (path-of r))
                           {:branch (:branch_id r) :turn (:turn r)
                            :tool (:tool_name r)})))
           first))))

(defn record-artifact!
  "A machine-checked result.

  `claim-status` is the confirmed / refuted / ambiguous / existential /
  empirical / unfaithful split. Two of those earn their keep by being neither
  a yes nor a no: existential, where a SAT verdict over free variables says a
  solution exists and does not hand you one, and empirical, where a
  computation produced a number and decided nothing. The done gate refuses to
  let either substantiate an answer on its own."
  [conn run-id {:keys [branch-id turn kind claim code verdict witness
                       claim-status tier]}]
  (db/with-writer
    (db/execute! conn
                   ["INSERT INTO artifacts (run_id, branch_id, turn, kind, claim, code,
                                            verdict, witness, claim_status, tier, created_at)
                     VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
                    run-id branch-id turn (name kind) claim (str code)
                    (some-> verdict name) (when witness (js witness))
                    (name claim-status) (name (or tier :fast)) (db/now)]))
  (emit! conn run-id :artifact {:branch-id branch-id :turn turn
                                :data {:kind kind :claim claim
                                       :claim-status claim-status}}))

(defn artifacts
  ([conn run-id]
   (db/fetch conn ["SELECT * FROM artifacts WHERE run_id = ? ORDER BY id" run-id]))
  ([conn run-id branch-id]
   (db/fetch conn ["SELECT * FROM artifacts WHERE run_id = ? AND branch_id = ? ORDER BY id"
                     run-id branch-id])))

(defn confirmed-artifacts [conn run-id branch-id]
  (db/fetch conn ["SELECT * FROM artifacts
                     WHERE run_id = ? AND branch_id = ? AND claim_status = 'confirmed'
                     ORDER BY id" run-id branch-id]))

(defn ledger
  "What the run has settled, as state rather than narrative.

  Two lists: what is established and what is ruled out. Read from `artifacts`
  rather than `shared_artifacts` for two reasons — the authoritative table
  already carries `claim_status` where the shared pool does not, and the
  ledger has to be COMPLETE. The shared block samples the top five by FTS
  relevance, which is right when the payload is a large encoding and wrong
  here: the whole value of a ledger is that a branch can trust the absence of
  a line.

  `refuted` is the half nothing carried before. 127 of them exist across the
  project's history and loop/shareable? admits only `:confirmed`, so a branch
  was told what siblings had proved and never what they had disproved — and a
  refutation prunes a direction where a confirmation only adds a fact.

  `unfaithful` is deliberately excluded despite being numerous: the encoding
  did not establish the claim, so carrying it would spread an assertion
  nothing verified. `empirical` and `existential` are excluded for now as an
  explicit choice rather than an oversight — a measurement is not a settled
  fact and would need its own section to avoid being read as one.

  Own-branch rows are INCLUDED, unlike `corroborating-artifacts`. Re-reading
  your own established facts as a list is the point; the alternative is
  scanning an eighty-turn transcript for them.

  `:inherited` is what a seed carried in. `seed-from-run!` copies a prior
  run's confirmed artifacts into `shared_artifacts`, NOT into `artifacts`, so
  a seeded run's ledger read `established 0` while the run held eleven
  verified lemmas — telling the branch the opposite of the truth and leaving
  the problem statement's hand-written summary as the only route to them,
  which is the fragility seeding exists to remove.

  Cheap: gen-20's entire confirmed set is 1,495 characters of claim text."
  [conn run-id]
  (let [rows (db/fetch conn
                       ["SELECT id, branch_id, turn, kind, tier, claim, claim_status
                         FROM artifacts
                         WHERE run_id = ?
                           AND claim_status IN ('confirmed', 'refuted', 'sketch')
                         ORDER BY id" run-id])
        by-status (group-by :claim_status rows)]
    {:established (vec (get by-status "confirmed" []))
     :ruled-out (vec (get by-status "refuted" []))
     ;; Plans, not results: a sketch elaborates and cites real lemmas but
     ;; proves nothing, so it rides in its own list under its own prefix
     ;; rather than joining either half of what the run has SETTLED.
     :sketches (vec (get by-status "sketch" []))
     ;; Seeded rows only. A live branch's shared artifacts are already in
     ;; `artifacts` above, so including them here would double-count.
     :inherited (vec (db/fetch conn
                               ["SELECT id, branch_id, turn, kind, tier, claim
                                 FROM shared_artifacts
                                 WHERE run_id = ? AND branch_id LIKE 'seed:%'
                                 ORDER BY id" run-id]))}))

(defn branch-turn
  "One turn of one branch, whole.

  What `fetch_turn` serves: compaction unloads a branch's early turns to one
  line each, and this is how a line gets opened again. Scoped to the branch
  because the digest is of the branch's OWN history — a sibling's turn is not
  what `t8` referred to, and cross-branch reading is what the shared-artifact
  block and the settled-state ledger are for."
  [conn run-id branch-id turn]
  (db/fetch-one conn ["SELECT * FROM turns
                       WHERE run_id = ? AND branch_id = ? AND turn = ?
                       ORDER BY id LIMIT 1"
                      run-id branch-id turn]))

(defn shared-artifact-by-id
  "One shared-pool row of this run, whole, including its encoding.

  A separate table from `artifacts` and therefore a separate id space, which
  is why the ledger renders seeded entries as `s#N` and this run's own as
  `a#N`. Run-scoped for the same reason as `artifact-by-id`."
  [conn run-id id]
  (db/fetch-one conn ["SELECT * FROM shared_artifacts WHERE run_id = ? AND id = ?"
                      run-id id]))

(defn artifact-by-id
  "One artifact of this run, whole, including its encoding.

  Scoped to the run on purpose: cross-run reach would let a branch cite
  something the run never established, which the done gate's evidence rungs
  exist to prevent."
  [conn run-id id]
  (db/fetch-one conn ["SELECT * FROM artifacts WHERE run_id = ? AND id = ?"
                      run-id id]))

(defn corroborating-artifacts
  "Everything the rest of the run confirmed or measured.

  A fork opens with its parent's confirmed claims quoted into its first
  message, and every branch sees the shared-artifact block. Both are context
  and neither is an artifact, so the done gate's coverage rung — which read
  only the branch's own list — refused answers that cited what the harness had
  just handed the branch (vf-b9c). Same run, same database, same engines: a
  sibling's confirmed claim is support.

  Own-branch rows are excluded; the caller already holds those."
  [conn run-id branch-id]
  (db/fetch conn ["SELECT * FROM artifacts
                     WHERE run_id = ? AND branch_id <> ?
                       AND claim_status IN ('confirmed', 'empirical')
                     ORDER BY id" run-id branch-id]))

;; --- gate firings -----------------------------------------------------------

(defn- observe-session!
  "Feed the live session tally — and the branch's own, when the caller can
  name it. Wrapped because a counter must never be able to cost a turn: the
  journal's own contract is that it cannot destroy the work it records, and a
  tally is strictly less important than the journal."
  ([path] (observe-session! path nil nil))
  ([path run-id branch-id]
   (try (session/observe! path (when (and run-id branch-id) [run-id branch-id]))
        (catch Throwable _ nil))))

(defn record-gate!
  "A gate fired, with what it expects to happen next.

  The prediction is the point. A gate that cannot say what should change is a
  gate whose effect nobody can check, and settling these deterministically from
  later turns is what turns a steering guess into something falsifiable."
  [conn run-id {:keys [branch-id turn gate priority message prediction window]}]
  ;; Returns the gate_firings row id, NOT the event id. The caller holds this
  ;; to settle the prediction later, and returning the event id instead means
  ;; every settle updates a row that does not exist, leaving the whole tally
  ;; permanently open. That is what the first live run showed.
  (let [id (db/with-writer
             (db/execute! conn
                            ["INSERT INTO gate_firings (run_id, branch_id, turn, gate, priority,
                                                        message, prediction, window, created_at)
                              VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
                             run-id branch-id turn (name gate) (or priority 0)
                             (str message) (str prediction) (or window 0) (db/now)])
             (db/last-insert-id conn))]
    (emit! conn run-id :gate {:branch-id branch-id :turn turn
                              :data {:gate gate :prediction prediction}})
    (observe-session! [:gates (keyword (name gate)) :fired] run-id branch-id)
    id))

(defn settle-gate!
  "Record whether a firing's prediction came true."
  [conn firing-id outcome settled-turn]
  (let [row (db/fetch-one conn ["SELECT gate, run_id, branch_id FROM gate_firings WHERE id = ?"
                                firing-id])]
    (db/with-writer
      (db/execute! conn
                   ["UPDATE gate_firings SET outcome = ?, settled_at_turn = ? WHERE id = ?"
                    (name outcome) settled-turn firing-id]))
    ;; The settlement, not just the firing: a gate that fires and is never
    ;; obeyed is the pattern worth surfacing, and it is invisible from firings
    ;; alone.
    (when-let [g (:gate row)]
      (observe-session! [:gates (keyword g) (keyword (name outcome))]
                        (:run_id row) (:branch_id row)))))

(defn unsettled-gates [conn run-id branch-id]
  (db/fetch conn ["SELECT * FROM gate_firings
                     WHERE run_id = ? AND branch_id = ? AND outcome IS NULL
                     ORDER BY id" run-id branch-id]))

(defn gate-firings [conn run-id]
  (db/fetch conn ["SELECT * FROM gate_firings WHERE run_id = ? ORDER BY id" run-id]))

(defn gate-tally
  "Per gate: how often it fired, and how its predictions settled. A gate that
  never fires across a benchmark sweep is either dead or guarding something the
  probe set should be provoking; a gate whose predictions never settle is not
  steering anything.

  `met_late` is its own column rather than folded into either side: a gate
  with a high late rate is one whose advice works and whose WINDOW is wrong,
  which is a different repair from a gate nobody obeys."
  [conn run-id]
  (db/fetch conn
              ["SELECT gate,
                       count(*) AS fired,
                       sum(CASE WHEN outcome = 'met' THEN 1 ELSE 0 END) AS met,
                       sum(CASE WHEN outcome = 'met-late' THEN 1 ELSE 0 END) AS met_late,
                       sum(CASE WHEN outcome = 'unmet' THEN 1 ELSE 0 END) AS unmet,
                       sum(CASE WHEN outcome IS NULL THEN 1 ELSE 0 END) AS open
                FROM gate_firings WHERE run_id = ? GROUP BY gate ORDER BY fired DESC"
               run-id]))

;; --- events -----------------------------------------------------------------

(defn events-since
  "Everything after `cursor`. One indexed range scan, which is all a polling
  UI needs and works over any HTTP server."
  ([conn run-id cursor] (events-since conn run-id cursor 500))
  ([conn run-id cursor limit]
   (db/fetch conn ["SELECT * FROM events WHERE run_id = ? AND id > ? ORDER BY id LIMIT ?"
                     run-id (or cursor 0) limit])))

(defn note!
  "A free-form journal entry, for anything without a table of its own."
  [conn run-id kind data]
  (emit! conn run-id kind data))

(defn last-note
  "The data of the most recent `kind` note on this run, parsed back from JSON,
  or nil.

  One stage reading another's conclusion out of the journal rather than out of
  a data map is deliberate: the supervision STREAM runs beside the run and
  hands its output to nobody, so the journal is the only place a pipeline
  stage can meet it (karamazov-poe). Best effort on the parse — a note the
  reader cannot make sense of is not worth failing a stage over."
  [conn run-id kind]
  (when-let [row (first (db/fetch conn
                                  ["SELECT data FROM events
                                     WHERE run_id = ? AND kind = ?
                                     ORDER BY id DESC LIMIT 1"
                                   run-id (name kind)]))]
    (try (json/read-str (str (:data row)) :key-fn keyword)
         (catch Throwable _ nil))))

(defn notes
  "Every note of `kind` on this run, oldest first, each parsed back from
  JSON; one that will not parse is skipped rather than failing the read.

  `last-note`'s plural, for the kinds that accumulate — a run's :stage-error
  notes are the crashes its stages survived, and the supervisor stream wants
  all of them, not the latest (RFC-012 F1: the crashes used to be shown to a
  supervisor stage that no longer exists, so the stream reads them here)."
  [conn run-id kind]
  (into []
        (keep (fn [row]
                (try (json/read-str (str (:data row)) :key-fn keyword)
                     (catch Throwable _ nil))))
        (db/fetch conn ["SELECT data FROM events
                          WHERE run_id = ? AND kind = ?
                          ORDER BY id"
                        run-id (name kind)])))

(def ^:private record-tables
  "The tables holding a run's account of itself, and how each one names its
  run. Ordered so an FTS mirror goes before the rows it indexes — deleted the
  other way round, the subquery that finds the rowids has nothing left to find
  and the index keeps ranking against nothing."
  [["DELETE FROM failures_fts WHERE rowid IN
       (SELECT id FROM failures WHERE run_id IN (%s))" :fts]
   ["DELETE FROM shared_artifacts_fts WHERE rowid IN
       (SELECT id FROM shared_artifacts WHERE run_id IN (%s))" :fts]
   ["DELETE FROM turns WHERE run_id IN (%s)" :rows]
   ["DELETE FROM artifacts WHERE run_id IN (%s)" :rows]
   ["DELETE FROM shared_artifacts WHERE run_id IN (%s)" :rows]
   ["DELETE FROM failures WHERE run_id IN (%s)" :rows]
   ["DELETE FROM gate_firings WHERE run_id IN (%s)" :rows]
   ["DELETE FROM messages WHERE run_id IN (%s)" :rows]
   ["DELETE FROM interventions WHERE run_id IN (%s)" :rows]
   ["DELETE FROM events WHERE run_id IN (%s)" :rows]])

(def ^:private finished-before
  "Runs that ended before the cutoff. `status != 'running'` AND a non-null
  `ended_at`, the same pair prune-finished! uses: a row that says it is
  running is either live or a leftover reconcile-orphans! has not seen yet,
  and neither is safe to strip."
  "SELECT id FROM runs WHERE status != 'running' AND ended_at IS NOT NULL
     AND ended_at < ?")

(defn prune-run-record!
  "Delete the RECORD of runs that ended before `cutoff`: turns, artifacts,
  failures, gate firings, branch messages, interventions and events.

  The `runs` and `branches` rows survive. What is left is an index — this run
  existed, it ended this way, at this time — without the bulk, which is a
  strictly better thing to have than a deleted row when somebody asks what
  happened six months ago.

  DESTRUCTIVE, and off unless an operator turns it on. RFC-009's central
  property is that a resume rebuilds branch state by replay and that a crashed
  run stays inspectable; this ends both for the runs it touches. It exists
  because `events/prune-finished!` addressed only half of provenance R2-11 and
  the other four tables grew forever in the one shared file — but a default
  that quietly discarded the record would be the wrong reading of the same
  finding.

  Returns the number of runs whose record was removed."
  [conn cutoff]
  (let [ids (mapv :id (db/fetch conn [finished-before cutoff]))]
    (when (seq ids)
      (let [placeholders (str/join ", " (repeat (count ids) "?"))]
        (db/with-writer
          (doseq [[sql _] record-tables]
            (db/execute! conn (into [(format sql placeholders)] ids))))))
    (count ids)))

(defn prune-finished!
  "Delete the events rows of runs that ended before `cutoff` (an ISO-8601
  timestamp). Events are a tail buffer for the live run — their only readers
  are the live tail and last-progress-at — and every kind that matters is
  also written to a durable table (turns, artifacts, failures,
  gate_firings). Nothing pruned them, so the one shared DB file grew
  without bound (provenance R2-11). The sweep runs on run start, not at finish:
  a tailing client still reads a just-finished run's events to see the
  :run-finished entry — that is how it learns the run ended — so events
  ride out a retention window (gates.edn :retention :events-hours) and go
  after that."
  [conn cutoff]
  (db/with-writer
    (db/execute! conn
                 ["DELETE FROM events WHERE run_id IN
                    (SELECT id FROM runs
                      WHERE status != 'running' AND ended_at IS NOT NULL
                        AND ended_at < ?)"
                  cutoff])))
