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

(ns samizdat.store.migrations
  "Schema as numbered, idempotent migrations against PRAGMA user_version.

  Every migration is a VECTOR OF SINGLE STATEMENTS, never one multi-statement
  string. db.sqlite/query calls sqlite3_prepare_v2 with a null tail pointer, so
  a string holding several statements executes only the first and reports no
  error at all. migrations-test asserts the statement count of each migration
  against the objects it should have created, so that failure mode cannot
  return quietly.

  Add a migration by appending to `migrations`; never edit one that shipped.
  Each carries a comment saying what it is for, which is the reason dirge's
  schema is still legible eleven migrations in.")

(def ^:private v1
  ;; The run journal. Everything the loop learns is appended here as it
  ;; happens, not assembled at the end, so a crashed run stays inspectable and
  ;; the read API can serve a live run and a finished one with the same query.
  ["CREATE TABLE IF NOT EXISTS runs (
      id            TEXT PRIMARY KEY,
      problem       TEXT NOT NULL,
      status        TEXT NOT NULL DEFAULT 'running',
      provider      TEXT NOT NULL DEFAULT '',
      model         TEXT NOT NULL DEFAULT '',
      max_turns     INTEGER NOT NULL DEFAULT 0,
      beam_width    INTEGER NOT NULL DEFAULT 0,
      prompt_digest TEXT NOT NULL DEFAULT '',
      final_answer  TEXT,
      started_at    TEXT NOT NULL,
      ended_at      TEXT
    )"

   ;; A branch is an entity with a durable id rather than a value threaded
   ;; through the loop, because an intervention has to be able to name one.
   "CREATE TABLE IF NOT EXISTS branches (
      id              TEXT NOT NULL,
      run_id          TEXT NOT NULL REFERENCES runs(id),
      parent_id       TEXT,
      status          TEXT NOT NULL DEFAULT 'active',
      inactive_reason TEXT,
      thesis          TEXT,
      created_at_turn INTEGER NOT NULL DEFAULT 0,
      PRIMARY KEY (run_id, id)
    )"

   "CREATE TABLE IF NOT EXISTS turns (
      id         INTEGER PRIMARY KEY AUTOINCREMENT,
      run_id     TEXT NOT NULL REFERENCES runs(id),
      branch_id  TEXT NOT NULL,
      turn       INTEGER NOT NULL,
      tool_name  TEXT NOT NULL DEFAULT '',
      args       TEXT NOT NULL DEFAULT '',
      result     TEXT NOT NULL DEFAULT '',
      category   TEXT,
      parse_error   TEXT,
      auto_repaired INTEGER NOT NULL DEFAULT 0,
      created_at TEXT NOT NULL
    )"

   "CREATE INDEX IF NOT EXISTS idx_turns_run ON turns(run_id, branch_id, turn)"

   ;; claim_status is the confirmed / refuted / ambiguous / existential split.
   ;; The existential bucket is why this column exists: a SAT verdict over free
   ;; variables says a solution exists and does not hand you one, and the
   ;; done gate refuses to let it substantiate a concrete answer.
   "CREATE TABLE IF NOT EXISTS artifacts (
      id           INTEGER PRIMARY KEY AUTOINCREMENT,
      run_id       TEXT NOT NULL REFERENCES runs(id),
      branch_id    TEXT NOT NULL,
      turn         INTEGER NOT NULL,
      kind         TEXT NOT NULL,
      claim        TEXT NOT NULL,
      code         TEXT NOT NULL DEFAULT '',
      verdict      TEXT,
      witness      TEXT,
      claim_status TEXT NOT NULL,
      tier         TEXT NOT NULL DEFAULT 'fast',
      created_at   TEXT NOT NULL
    )"

   "CREATE INDEX IF NOT EXISTS idx_artifacts_run ON artifacts(run_id, branch_id)"

   ;; The cross-branch failure log. In the TypeScript harness this is a vector
   ;; re-rendered into every branch's context each turn, so it grows without
   ;; bound; backed by FTS5 it becomes a query for the failures most like what
   ;; this branch is about to try.
   "CREATE TABLE IF NOT EXISTS failures (
      id         INTEGER PRIMARY KEY AUTOINCREMENT,
      run_id     TEXT NOT NULL REFERENCES runs(id),
      branch_id  TEXT NOT NULL,
      turn       INTEGER NOT NULL,
      tool_name  TEXT NOT NULL DEFAULT '',
      claim      TEXT NOT NULL DEFAULT '',
      reason     TEXT NOT NULL DEFAULT '',
      created_at TEXT NOT NULL
    )"

   ;; Standalone FTS5, not external-content: the indexed text is a projection
   ;; (claim plus reason), and external-content deletes require the exact
   ;; indexed values back, which a projection cannot promise. Sync is
   ;; app-managed in store.failures, no triggers.
   "CREATE VIRTUAL TABLE IF NOT EXISTS failures_fts USING fts5(claim, reason)"

   ;; Decision observability: a gate firing records what it expected to happen
   ;; next, and a later turn settles that prediction from the journal with no
   ;; LLM in the path. A gate whose predictions never settle is not steering.
   "CREATE TABLE IF NOT EXISTS gate_firings (
      id          INTEGER PRIMARY KEY AUTOINCREMENT,
      run_id      TEXT NOT NULL REFERENCES runs(id),
      branch_id   TEXT NOT NULL,
      turn        INTEGER NOT NULL,
      gate        TEXT NOT NULL,
      priority    INTEGER NOT NULL DEFAULT 0,
      message     TEXT NOT NULL DEFAULT '',
      prediction  TEXT NOT NULL DEFAULT '',
      window      INTEGER NOT NULL DEFAULT 0,
      outcome     TEXT,
      settled_at_turn INTEGER,
      created_at  TEXT NOT NULL
    )"

   "CREATE INDEX IF NOT EXISTS idx_gate_firings_run ON gate_firings(run_id, gate)"

   ;; Human directives. Applied at the next branch boundary, never mid-turn:
   ;; a branch inside a provider call or a Lean tactic is not in a state anyone
   ;; should mutate. status carries pending / applied / rejected so a UI can
   ;; show the difference honestly instead of pretending a click took effect.
   "CREATE TABLE IF NOT EXISTS interventions (
      id          INTEGER PRIMARY KEY AUTOINCREMENT,
      run_id      TEXT NOT NULL REFERENCES runs(id),
      branch_id   TEXT,
      kind        TEXT NOT NULL,
      payload     TEXT NOT NULL DEFAULT '',
      issued_by   TEXT NOT NULL DEFAULT 'human',
      status      TEXT NOT NULL DEFAULT 'pending',
      disposition TEXT,
      created_at  TEXT NOT NULL,
      applied_at_turn INTEGER
    )"

   "CREATE INDEX IF NOT EXISTS idx_interventions_pending
      ON interventions(run_id, status)"

   ;; The event cursor the tail endpoint reads. One row per appended event, so
   ;; GET /v1/runs/:id/journal?since=N is a single indexed range scan and the
   ;; UI needs no cooperation from the loop.
   "CREATE TABLE IF NOT EXISTS events (
      id         INTEGER PRIMARY KEY AUTOINCREMENT,
      run_id     TEXT NOT NULL REFERENCES runs(id),
      branch_id  TEXT,
      turn       INTEGER,
      kind       TEXT NOT NULL,
      data       TEXT NOT NULL DEFAULT '',
      created_at TEXT NOT NULL
    )"

   "CREATE INDEX IF NOT EXISTS idx_events_cursor ON events(run_id, id)"])

(def ^:private v3
  ;; The run-scoped shared confirmed-artifact log, twin of the failure log.
  ;; Branches already share what was disproven; this shares what an engine
  ;; CONFIRMED, with provenance (branch, engine, tier) inline. Only
  ;; engine-confirmed artifacts enter — never self-reports, which is the
  ;; difference from UCLA's harness. Off by default because shared lemmas can
  ;; cost beam diversity; sweep-widths runs it both ways to find out.
  ["CREATE TABLE IF NOT EXISTS shared_artifacts (
      id         INTEGER PRIMARY KEY AUTOINCREMENT,
      run_id     TEXT NOT NULL REFERENCES runs(id),
      branch_id  TEXT NOT NULL,
      turn       INTEGER NOT NULL,
      kind       TEXT NOT NULL DEFAULT '',
      tier       TEXT NOT NULL DEFAULT 'fast',
      claim      TEXT NOT NULL DEFAULT '',
      code       TEXT NOT NULL DEFAULT '',
      created_at TEXT NOT NULL
    )"
   ;; Standalone FTS5 like failures_fts: the indexed text is a projection
   ;; (claim alone here), and sync is app-managed in store.artifacts.
   "CREATE VIRTUAL TABLE IF NOT EXISTS shared_artifacts_fts USING fts5(claim)"])

(def ^:private v2
  ;; What the model actually said.
  ;;
  ;; v1 stored the tool call and the result but not the prose around it, so a
  ;; turn that produced no tool call at all recorded only that fact. Nine of
  ;; twenty turns in a Lean run came back "__no_call__" and there was no way to
  ;; ask why, because the one artefact that would answer it was the one thing
  ;; not kept. A harness whose whole thesis is that decisions must be
  ;; inspectable after the fact should not throw away the decision.
  ;;
  ;; Nullable, because the provider-error path has no response to record.
  ["ALTER TABLE turns ADD COLUMN assistant_text TEXT"
   ;; The reasoning block, split out rather than left inline. Reasoning models
   ;; put most of their output here and it dwarfs the answer, so a UI wants to
   ;; fold it and a query that greps the response should not have to wade
   ;; through it. Empty for models that do not emit one.
   "ALTER TABLE turns ADD COLUMN reasoning_text TEXT"])

(def ^:private v4
  ;; What each turn cost.
  ;;
  ;; The adapter parsed usage and client/chat returned it, and the agent loop
  ;; dropped it: nothing outside the bench harness and the raw passthrough API
  ;; ever read it, and turns had no columns to put it in. So the one number a
  ;; harness whose operating rule is "a generation is hours of provider spend"
  ;; most needs was the number it never kept.
  ;;
  ;; Nullable throughout, deliberately. The provider-error path has no response
  ;; and therefore no usage, and a zero there would claim the call was free —
  ;; summing it would under-report the run rather than admit the gap.
  ["ALTER TABLE turns ADD COLUMN prompt_tokens INTEGER"
   "ALTER TABLE turns ADD COLUMN completion_tokens INTEGER"
   "ALTER TABLE turns ADD COLUMN total_tokens INTEGER"
   ;; The prefix-cache split, when the provider reports one. Each branch
   ;; carries its own growing message list, so a beam of five holds five
   ;; diverging prefixes; whether that is cheap is the question these answer.
   "ALTER TABLE turns ADD COLUMN cache_hit_tokens INTEGER"
   "ALTER TABLE turns ADD COLUMN cache_miss_tokens INTEGER"])

(def ^:private v5
  ;; One bit per turn: whether the tool call was declined by harness phase
  ;; policy (vf-b25/vf-eaw). The cull record has to be able to tell a
  ;; declined call — a perfectly well-formed one the harness refused — from a
  ;; malformed fence, or the reason string lies in the permanent record
  ;; (gen-30 B3.2 was culled with exactly that false reason).
  ["ALTER TABLE turns ADD COLUMN policy_refusal INTEGER"])

(def ^:private v6
  ;; The task board. dirge's issues schema with two generalizations: epic_id
  ;; becomes a self-referential parent_id plus a type column (an epic is a
  ;; TYPE of task, so epics nest and the model decides how many levels it
  ;; wants), and session scoping becomes run scoping (run_id NULL = passive
  ;; backlog, set = claimed onto that run's active board). contract and tests
  ;; are what make a task a delegable unit: the spec the work must satisfy
  ;; and the tests that define delivery, per the decomposition design.
  ["CREATE TABLE IF NOT EXISTS tasks (
      id          TEXT PRIMARY KEY,
      title       TEXT NOT NULL,
      body        TEXT NOT NULL DEFAULT '',
      type        TEXT NOT NULL DEFAULT 'task',
      status      TEXT NOT NULL DEFAULT 'open',
      priority    TEXT NOT NULL DEFAULT 'normal',
      parent_id   TEXT REFERENCES tasks(id),
      run_id      TEXT,
      contract    TEXT NOT NULL DEFAULT '',
      tests       TEXT NOT NULL DEFAULT '',
      created_at  TEXT NOT NULL,
      updated_at  TEXT NOT NULL,
      closed_at   TEXT
    )"
   "CREATE INDEX IF NOT EXISTS idx_tasks_status ON tasks(status)"
   "CREATE INDEX IF NOT EXISTS idx_tasks_parent ON tasks(parent_id)"])

(def ^:private v7
  ;; Workflow definitions: the agentic loop as durable, versioned data. Every
  ;; save is a new version — never an UPDATE — so a bad edit is rolled back by
  ;; pointing at the previous row and the whole edit history stays readable.
  ;; Runs journal which (name, version) drove them.
  ["CREATE TABLE IF NOT EXISTS workflows (
      name       TEXT NOT NULL,
      version    INTEGER NOT NULL,
      edn        TEXT NOT NULL,
      created_at TEXT NOT NULL,
      PRIMARY KEY (name, version)
    )"])

(def ^:private v8
  ;; Session permission grants. When a command that would ask is approved by a
  ;; human, the grant persists here scoped to its run and is consulted ahead of
  ;; the base rules on later commands. Human-only writes: nothing the model
  ;; emits reaches this table (the model has no edge into grants — see the
  ;; security model, docs/RFCS/RFC-003-security-model.md). A grant can never override a hard deny.
  ["CREATE TABLE IF NOT EXISTS grants (
      id         INTEGER PRIMARY KEY AUTOINCREMENT,
      run_id     TEXT NOT NULL,
      pattern    TEXT NOT NULL,
      created_at TEXT NOT NULL
    )"
   "CREATE INDEX IF NOT EXISTS idx_grants_run ON grants(run_id)"])

(def ^:private v9
  ;; Long-term knowledge: facts a run commits to durable rows and recalls by
  ;; search later. Unlike turns/artifacts (the run's own record of what it
  ;; did), a knowledge row is a claim extracted as worth keeping — content is
  ;; the searchable text, kind separates notes from other kinds later. Recall
  ;; is a LIKE scan, so content is the index and needs no extra one.
  ;;
  ;; A ROW IS A VERSION OF A BELIEF, NOT THE BELIEF. `restate!` used to
  ;; UPDATE content in place, and you cannot retract what you overwrote: once
  ;; the previous wording was gone nothing could ask what we believed or what
  ;; made us believe it, so a premise that turned out false left no trace and
  ;; everything downstream of it kept standing (karamazov-oov). That is the
  ;; 238-turn run — every re-read CONFIRMED the code was fine, which is
  ;; exactly why it read again.
  ;;
  ;; `lineage_id` is the subject; `current` says which version speaks for it
  ;; now. A FLAG rather than MAX(version), because max-version cannot express
  ;; "retracted with no replacement" and that is the shape a disproven belief
  ;; has — the run needed its premise withdrawn, not edited into another one.
  ;; A lineage with no current row is a retraction.
  ;;
  ;; `cause` is why the row was written. The model can only act on what it
  ;; knows about, so "this is false" is a dead end where "this is false, and
  ;; here is what made us believe it" is a lead. Nullable like
  ;; userspace.rationale: most writes have nothing to say about their origin,
  ;; and inventing text would make the history lie.
  ["CREATE TABLE IF NOT EXISTS knowledge (
      id             TEXT PRIMARY KEY,
      content        TEXT NOT NULL,
      kind           TEXT NOT NULL DEFAULT 'note',
      created_at     TEXT NOT NULL,
      lineage_id     TEXT,
      current        INTEGER NOT NULL DEFAULT 1,
      cause          TEXT,
      supersedes     TEXT,
      retired_at     TEXT,
      retired_reason TEXT
    )"
   "CREATE INDEX IF NOT EXISTS idx_knowledge_lineage ON knowledge(lineage_id)"
   "CREATE INDEX IF NOT EXISTS idx_knowledge_current ON knowledge(current)"])

(def ^:private v10
  ;; Agent mailbox: durable messages between branches working one feature.
  ;; to_branch NULL means broadcast to the whole run. read_at NULL marks the
  ;; message unread — surfacing it in context does not consume it; the inbox
  ;; tool is what stamps read_at. Inbox queries filter on run_id + read_at,
  ;; so the natural index covers them.
  ["CREATE TABLE IF NOT EXISTS messages (
      id          TEXT PRIMARY KEY,
      run_id      TEXT NOT NULL,
      from_branch TEXT NOT NULL,
      to_branch   TEXT,
      body        TEXT NOT NULL,
      created_at  TEXT NOT NULL,
      read_at     TEXT
    )"

   "CREATE INDEX IF NOT EXISTS idx_messages_run ON messages(run_id, read_at)"])

(def ^:private v11
  ;; THE USERSPACE LAYER, per project.
  ;;
  ;; The harness ships a userspace TEMPLATE in resources/ — the cells, the
  ;; manifests, the policy tables, the prompts. A project seeds its own copy
  ;; from that template on first use and then evolves it: every edit the
  ;; supervisor makes is a new version HERE, in this project's database, and
  ;; the shipped files are never written. That is what lets two projects
  ;; running the same harness diverge in how they work, which is the whole
  ;; point of the split — src/ is capability, userspace is the loop that
  ;; assembles capabilities, and the loop belongs to the project.
  ;;
  ;; One table, four kinds ('cell', 'manifest', 'policy', 'prompt'), because
  ;; the lifecycle is identical for all of them: seed from the template, load
  ;; the latest version, append a version on edit, roll back by pointing at an
  ;; older row. Append-only for the same reason the workflows table was — the
  ;; edit history of a system that rewrites itself is the most valuable thing
  ;; in the database.
  ;;
  ;; `body` is TEXT for every kind: Clojure source for a cell, EDN for a
  ;; manifest or a policy table, markdown for a prompt. The kind says how to
  ;; read it; nothing here parses it.
  ["CREATE TABLE IF NOT EXISTS userspace (
      kind       TEXT NOT NULL,
      name       TEXT NOT NULL,
      version    INTEGER NOT NULL,
      body       TEXT NOT NULL,
      created_at TEXT NOT NULL,
      PRIMARY KEY (kind, name, version)
    )"

   ;; Manifests were the one layer that already worked this way. Fold them in
   ;; rather than leaving two mechanisms: the workflows table keeps its rows
   ;; (nothing is dropped, and a rollback to a pre-migration version still
   ;; resolves). store/workflows.clj was a thin shim over this table and has
   ;; since been retired; callers use samizdat.store.userspace directly
   ;; so samizdat.workflow and the manifest tool keep their call sites.
   "INSERT OR IGNORE INTO userspace (kind, name, version, body, created_at)
      SELECT 'manifest', name, version, edn, created_at FROM workflows"

   ;; Reads are always 'the newest version of this name' — the index the
   ;; loader hits on every compile.
   "CREATE INDEX IF NOT EXISTS idx_userspace_latest
      ON userspace(kind, name, version DESC)"])

(def ^:private v12
  ;; WHO holds a task, not just which run.
  ;;
  ;; claim! guarded on `(run_id IS NULL OR run_id = ?)` and set run_id, which
  ;; makes the claim exclusive BETWEEN runs and a no-op WITHIN one. In a team
  ;; workflow the competing agents are branches of a single run — several
  ;; implementors fanned out over one feature — so two workers both claimed the
  ;; same task and both believed they held it. The docstring cited provenance A-4 (two beam
  ;; branches whose reads both saw the unclaimed row) and had fixed the
  ;; read-then-write race while leaving the granularity wrong for exactly the
  ;; case it named.
  ;;
  ;; run_id keeps its meaning — which board a task is on, NULL for the backlog —
  ;; and branch_id says who is working it. Both are needed: the board is shared
  ;; by the run, the work is done by a branch.
  ["ALTER TABLE tasks ADD COLUMN branch_id TEXT"

   ;; Existing claimed rows predate the distinction. Left with branch_id NULL,
   ;; which reads as "on this run's board, nobody holding it" — claimable,
   ;; which is the safe reading: the alternative is a task no branch can ever
   ;; take because it is attributed to a branch that no longer exists.
   "CREATE INDEX IF NOT EXISTS idx_tasks_holder ON tasks(run_id, branch_id)"])

(def ^:private v13
  ;; An index for long-term memory.
  ;;
  ;; `recall` was a LIKE scan and the docstring said so — "content is the whole
  ;; searchable payload, so no extra index" — which is fine at a hundred rows
  ;; and is not a plan. Knowledge is the one table that deliberately OUTLIVES
  ;; every run, so it is the one whose row count only goes up, and a substring
  ;; scan degrades exactly as the memory becomes worth having.
  ;;
  ;; Standalone FTS5 like failures_fts and shared_artifacts_fts: the indexed
  ;; text is a projection, external-content deletes would require the exact
  ;; indexed values back, and sync is app-managed in store.knowledge with no
  ;; triggers. Same shape, same reasons, third time.
  ;;
  ;; Backfilled here rather than lazily, so a project that has been running for
  ;; months does not have a search that silently covers only what it learned
  ;; after the upgrade — the failure mode of a lazy backfill on a recall path
  ;; is a confident empty answer.
  ["CREATE VIRTUAL TABLE IF NOT EXISTS knowledge_fts USING fts5(content)"
   "INSERT INTO knowledge_fts (rowid, content) SELECT rowid, content FROM knowledge"])

(def ^:private v14
  ;; Memory that learns from being used.
  ;;
  ;; A knowledge row was content + kind + a timestamp: everything ever
  ;; remembered, equally, forever. Recall could rank by text relevance (v13)
  ;; but not by whether a memory had ever been WORTH recalling — so the note
  ;; that saved three runs and the note nobody ever read again looked the same,
  ;; and the supervisor reading them had no way to tell which was which.
  ;;
  ;; The axes are dirge's (src/extras/salience.rs), which are in turn the
  ;; converged LangMem/MemoryOS taxonomy:
  ;;
  ;;   salience    — how important, decayed by disuse and reinforced by use
  ;;   confidence  — how likely to be TRUE, which is a different question
  ;;   use_count / last_used_at — being looked up IS the relevance signal
  ;;   success_count / failure_count — did acting on it actually work
  ;;   pinned      — never decayed, never evicted
  ;;
  ;; salience and confidence are separate on purpose: a fact can be important
  ;; but contested, or trivial but certain, and collapsing them loses exactly
  ;; the distinction a supervisor needs when two memories disagree.
  ;;
  ;; run_id records which run formed the memory, so a claim can be traced back
  ;; to the evidence that produced it.
  ["ALTER TABLE knowledge ADD COLUMN salience REAL NOT NULL DEFAULT 0.5"
   "ALTER TABLE knowledge ADD COLUMN confidence REAL NOT NULL DEFAULT 0.6"
   "ALTER TABLE knowledge ADD COLUMN use_count INTEGER NOT NULL DEFAULT 0"
   "ALTER TABLE knowledge ADD COLUMN last_used_at TEXT"
   "ALTER TABLE knowledge ADD COLUMN success_count INTEGER NOT NULL DEFAULT 0"
   "ALTER TABLE knowledge ADD COLUMN failure_count INTEGER NOT NULL DEFAULT 0"
   "ALTER TABLE knowledge ADD COLUMN pinned INTEGER NOT NULL DEFAULT 0"
   "ALTER TABLE knowledge ADD COLUMN run_id TEXT"
   "CREATE INDEX IF NOT EXISTS idx_knowledge_salience ON knowledge(salience DESC)"])

(def ^:private v15
  ;; Corroboration: how many DISTINCT runs have seen this.
  ;;
  ;; A memory could already say how often acting on it worked, and could not
  ;; say how many independent runs produced it in the first place. Those are
  ;; different questions and the second is the one that separates a pattern
  ;; from a bad afternoon: a single run can go wrong for reasons that have
  ;; nothing to do with the loop — a flaky provider, an unlucky task — and a
  ;; supervisor that retunes the harness on one run's evidence is fitting the
  ;; noise.
  ;;
  ;; The mechanic is backpass's (VISION.md, `Evidence is the only currency`):
  ;; a new instruction needs corroboration from at least two distinct
  ;; sessions, one session never counts twice however often it is re-analysed,
  ;; and corroboration accumulates in a ledger across runs rather than
  ;; resetting each time.
  ;;
  ;; `last_run_id` is what makes `distinct` real: a second distillation within
  ;; the same run confirms nothing, and without it a long run would corroborate
  ;; its own findings by repetition.
  ["ALTER TABLE knowledge ADD COLUMN corroborations INTEGER NOT NULL DEFAULT 1"
   "ALTER TABLE knowledge ADD COLUMN last_run_id TEXT"])

(def ^:private v16
  ;; Identity as a COLUMN, which is the whole reason this is a database.
  ;;
  ;; Distillation had to answer `have I written this pattern down before`, and
  ;; answered it by scanning every row of a kind and prefix-matching a marker
  ;; embedded in the content text. That is what backpass has to do — its memory
  ;; is a markdown file, an instruction has no id, so `is this the same gap` is
  ;; a Sorensen-Dice similarity question at a tuned threshold, with a side-car
  ;; JSON ledger keyed by hashed phrasings to hold the count.
  ;;
  ;; None of that is necessary here. A row has a key. Reproducing text-identity
  ;; matching on top of a table with a primary key inherits a constraint we do
  ;; not have, and it is fragile in the way text matching always is: the
  ;; verdict marker embedded the change description, so `beam width 5 -> 2` and
  ;; `beam-width 5→2` were two different levers with two different records.
  ;;
  ;; `pattern_key` is that identity. NULL for an ordinary memory somebody typed
  ;; — those have no pattern, they are just facts — so the index is sparse and
  ;; the column costs nothing on the common row.
  ["ALTER TABLE knowledge ADD COLUMN pattern_key TEXT"
   "CREATE INDEX IF NOT EXISTS idx_knowledge_pattern ON knowledge(pattern_key)"])

(def ^:private v17
  ;; WHOSE COPY IS THIS. A project seeds its own row for every shipped
  ;; template on first read, and that row was authoritative from then on — so
  ;; a harness upgrade could never reach a project again. Live: a project
  ;; seeded gates.edn on its first read, a threshold added afterwards was
  ;; missing from that project's table, and the rule reading it threw rather
  ;; than finding the key absent. Because entries seed lazily at first USE, a
  ;; long-lived project ends up on a sediment of whatever harness version
  ;; happened to touch each one first.
  ;;
  ;; The fix needs to tell `the factory copy, untouched` from `the
  ;; supervisor's own work`, and the version number cannot: `seed!` writes
  ;; version 1, but so does a `save!` of a name that was never seeded. Getting
  ;; that wrong overwrites the supervisor's work with the template, which is
  ;; the one thing userspace exists to prevent — so it is a column, not an
  ;; inference.
  ;;
  ;; Backfilled to 'factory' for a SOLE version-1 row, which is what every
  ;; seeded row looks like today; anything with a version above it is the
  ;; project's own and defaults to 'project'. Agent-authored names have no
  ;; shipped template, so refresh never reaches them either way.
  ["ALTER TABLE userspace ADD COLUMN source TEXT NOT NULL DEFAULT 'project'"
   "UPDATE userspace SET source = 'factory'
     WHERE version = 1
       AND NOT EXISTS (SELECT 1 FROM userspace u2
                        WHERE u2.kind = userspace.kind
                          AND u2.name = userspace.name
                          AND u2.version > 1)"])

(def ^:private v18
  ;; A branch's OWN problem. Sub-workflow branches do not work the run-level
  ;; problem: a decompose unit's branch opens on its unit CONTRACT, a team
  ;; worker on its sub-task. Nothing durable recorded that, so a resume
  ;; rebuilt every branch on the top-level feature text with no role framing —
  ;; re-aiming every worker at the wrong job (karamazov-blt.23). NULL means
  ;; "the run's problem", which is what every branch before this column meant.
  ["ALTER TABLE branches ADD COLUMN problem TEXT"])

(def ^:private v19
  ;; WHY an edit was made, and what it has survived.
  ;;
  ;; Run c2260271: one supervisor landed prompt tuning as v3, and thirteen
  ;; minutes later the next supervisor of the same run reverted it to v2 —
  ;; the history showed bodies and timestamps but never a reason, so a
  ;; successor confronted with an unfamiliar delta had no way to judge it and
  ;; restored what it recognized. Self-tuning without a rationale column is
  ;; self-oscillation (karamazov-c58).
  ;;
  ;; `rationale` is the commit message of self-modification: nullable, because
  ;; seeding and mechanical writes have nothing to say, and inventing text
  ;; would make the history lie. The mutation tools are what demand it.
  ;;
  ;; success/failure counts are the version's STANDING: how many runs ended
  ;; shipped or not while this row was the current version of its name. A
  ;; tuning that has survived green runs has earned something a fresh
  ;; supervisor should weigh before reverting it — the same argument that gave
  ;; knowledge rows outcome columns in v14.
  ["ALTER TABLE userspace ADD COLUMN rationale TEXT"
   "ALTER TABLE userspace ADD COLUMN success_count INTEGER NOT NULL DEFAULT 0"
   "ALTER TABLE userspace ADD COLUMN failure_count INTEGER NOT NULL DEFAULT 0"])

(def migrations
  "Ordered. Index 0 is migration 1; PRAGMA user_version holds the count applied."
  [v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19])
