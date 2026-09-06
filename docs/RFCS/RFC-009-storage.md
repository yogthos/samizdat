# RFC-009 — Storage and the run record

**Status:** implemented.

## Purpose

Specifies the durable record: what is written, when, by whom, and the property
everything else depends on — that a resumed run rebuilds by **replay** rather
than by trusting a snapshot.

## Scope

**This layer decides** nothing. It owns rows and SQL.

**It must not know** what a claim means, what makes an artifact good, or why a
gate fired. It stores what it is handed.

**It hands** rows to readers and events to whoever is listening.

## Model

One SQLite database per project at `<root>/.samizdat/samizdat.sqlite3`,
root-relative, so a project *is* a directory (RFC-001). `db/open!` creates the
parent directory; `config/db-location` decides the path (env, existing,
legacy root file, or the default) and start! logs a legacy file rather than
moving it.

```
runs ──┬── branches ──┬── turns          every model call and tool result
       │              ├── artifacts ──── shared_artifacts  (the cross-branch pool)
       │              ├── failures        FTS-ranked, shared by the whole run
       │              └── gate_firings    with settlement
       ├── events                         a tail buffer for the live view
       ├── interventions                  human directives, with resolution
       └── messages                       branch-to-branch mail

tasks         the board (RFC-008)         run_id + branch_id
knowledge     long-term memory, LIKE-searchable
grants        session permission grants (RFC-003) — human-only writes
workflows     manifests, pre-v11; now a shim over userspace
userspace     the project's own loop (RFC-001)
```

### The journal is the record, not a summary

Everything the loop learns is appended **as it happens**, not assembled at the
end. Two consequences that shape every other layer:

- a crashed run stays inspectable
- the read API serves a live run and a finished one with the same query

`state.clj` holds the working copy of a branch during a turn; **everything in it
is also journalled**, so a resume rebuilds it by replay. The engine session and
the message history are the only parts that cannot be reconstructed.

### Append-only where it matters

| table | discipline | why |
|---|---|---|
| `turns`, `artifacts`, `failures`, `gate_firings` | append | the run's account of itself |
| `userspace` | append, one row per version | the edit history of a system that rewrites itself |
| `events` | append, **pruned** | a tail buffer, not a record — every kind that matters is also in a durable table |
| `branches`, `runs`, `tasks`, `interventions` | mutable status, **guarded** | lifecycle UPDATEs carry status guards, because an ungated one let `abort!` rewrite a finished run (`provenance R2-4`, `CR1-3`) |

### Migrations

Numbered, idempotent, against `PRAGMA user_version`. Each is a **vector of
single statements**, never one multi-statement string: the FFI binding calls
`sqlite3_prepare_v2` with a null tail pointer, so a string holding several
statements executes only the first and reports no error at all. A test asserts
the shape so that failure mode cannot return quietly.

Never edit a migration that shipped. Append.

## API

### `samizdat.store.db`

| fn | contract |
|---|---|
| `(open! path)` | Connect and migrate. |
| `(connect path)` / `(close conn)` / `(migrate! conn)` | The pieces. |
| `(fetch conn q)` / `(fetch-one conn q)` / `(execute! conn q [opts])` | Query. `q` is `[sql & params]`. |
| `(with-writer & body)` | Serialise writes. SQLite has one writer; this is where that is expressed. |
| `(with-conn & body)` | Scoped connection. |
| `(now)` | ISO-8601 timestamp — one definition, so rows sort. |
| `(schema-version conn)` / `(table-names conn)` | Introspection, for the migration tests. |
| `(id-collision? e)` | Whether an exception is a UNIQUE collision. **Only** a collision is an id problem; anything else (disk, lock) must surface as itself, or a retry loop reports a disk error as an id collision (`provenance R2-15`). |
| `(fts5-available? conn)` | Probed explicitly, because whether the loaded libsqlite3 has FTS5 is a different question from whether the CLI does, and the failure mode is a migration that throws at startup. |
| `(last-insert-id conn)` / `(change-count conn)` | Post-write. |

### `samizdat.store.journal`

| fn | contract |
|---|---|
| `(record-turn! conn run-id {…})` | One turn: tool, args, result, category, parse flags, assistant and reasoning text, usage, policy-refusal flag. |
| `(record-artifact! conn run-id {…})` | A claim with its code, verdict, witness, claim-status and tier. |
| `(record-gate! conn run-id {…})` | A firing with its prediction and window. |
| `(settle-gate! conn firing-id outcome settled-turn)` | Close a prediction. |
| `(note! conn run-id kind data)` | A free-form entry, for anything without a table of its own. |
| `(turns conn run-id)` / `(branch-turns …)` / `(branch-turn …)` | Read. |
| `(artifacts conn run-id [branch-id])` / `(confirmed-artifacts …)` / `(artifact-by-id …)` / `(shared-artifact-by-id …)` / `(corroborating-artifacts …)` | The artifact surface. |
| `(ledger conn run-id)` | The settled-state block: what is established **and what is ruled out**. |
| `(gate-firings conn run-id)` / `(gate-tally conn run-id)` / `(unsettled-gates …)` | Steer observability (RFC-007). |
| `(events-since conn run-id cursor [limit])` | One indexed range scan — all a polling UI needs. |
| `(prune-finished! conn cutoff)` | Delete the events of runs that ended before `cutoff`. |

**Contract — the journal must not be able to destroy the work it records.** A
value `data.json` cannot write falls back to `pr-str` rather than throwing.
gen-31 lost two branches — both on the run's actual target — to
`Don't know how to write JSON of class java.lang.Character` thrown out of the
turn writer: a non-mathematical event ending mathematical work.

### `samizdat.events` — the live bus

| fn | contract |
|---|---|
| `(publish! event)` | **Non-blocking.** Returns immediately whether or not anyone is listening. |
| `(subscribe [n])` | A channel receiving everything published from now on. |
| `(unsubscribe! ch)` | Close it, or it keeps consuming a tap slot. |
| `(collect ch)` | Drain what is buffered. |

**Contract — a slow subscriber loses events rather than applying
backpressure.** Taps use a sliding buffer (256). That is the right trade because
the durable journal is the source of truth and a client that fell behind re-reads
by cursor. A subscriber that stops reading must never stall the loop.

## Protocol

```
every journal append ──→ events/publish!    so a client can watch without polling
                                            and nothing in the loop knows whether
                                            anyone is watching

system/start!
  └─ runs/reconcile-orphans!   nothing can be running yet, so any row that says
                               it is, is a leftover from a process that died.
                               This is the ONLY moment that inference is sound.
```

## Invariants

| invariant | enforced by |
|---|---|
| A resume rebuilds branch state by replay. | Everything a gate reads is journalled; `resume/rebuild-branch`. |
| A migration's statements all execute. | `store-test/every-migration-statement-runs` asserts the vector shape. |
| Migrations are idempotent. | `store-test/migrations-are-idempotent`. |
| A journal failure cannot kill a turn. | The `pr-str` fallback; `store-test`. |
| Only a UNIQUE collision is treated as an id problem. | `id-collision?`; `store-test`. |
| A slow event subscriber cannot stall the loop. | Sliding buffer. |
| Lifecycle UPDATEs are status-guarded. | Per-statement `WHERE`; `provenance R2-4`. |
| A run that dies is marked failed. | `beam/run-rounds`' catch (RFC-006). |

## Known gaps

- ~~Retention is partial: only events prune.~~ Closed:
  `journal/prune-run-record!` sweeps the detail tables of runs older than
  `gates.edn :retention :run-record-days`, at run start, FTS mirrors
  included; the run row itself stays as an index. The knob defaults to nil —
  the record is kept forever unless an operator decides otherwise, because
  pruning ends replay and inspectability for the pruned run.
- `workflows` remains after migration v11 copied its rows into `userspace`.
  Nothing reads it and it is not dropped, so a rollback to a pre-v11 binary
  still resolves.
- `knowledge` recall is a `LIKE` scan; content is the index. Fine at current
  volumes, and not a plan.
