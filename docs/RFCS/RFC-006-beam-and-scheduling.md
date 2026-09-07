# RFC-006 — The beam and scheduling

**Status:** implemented.

## Purpose

Specifies how many branches advance one problem in parallel: the round protocol,
the branch lifecycle, and the split between the scheduling *capability* in
`src/` and the scheduling *policy* in userspace.

## Scope

**The base decides nothing about which branch is good.** It advances branches,
fans out under a deadline, disposes sessions, and closes rows.

**The round is userspace** — `manifests/beam.edn` plus `cells/beam.clj`. Which
branches live, which fork, when the run ends, and in what order those questions
are asked is a project's to change (RFC-002).

**It must not know** what a turn does internally; it drives the turn manifest
and reads `:branch` back out.

## Model

### The round

```
:start  round-open   ─┬─→ :aborted   abort flag set (checked FIRST)
                      ├─→ :completed a branch shipped and policy says stop
                      ├─→ :exhausted nobody active, or turn > max-turns
                      └─→ :continue
                            │
        directives ─→ advance ─→ score ─→ cull ─→ settle ─→ repopulate
                                                              │
                            ┌─────────── tick ←─── spawn ←────┘
                            │  turn+1, trace capped
                            └─→ back to :start
```

Data map: `{:branches [] :turn n}` plus per-round products the tick cell drops.

### Why a barrier and not a pipeline

Every active branch advances once, then culling, forking and the done-check run
against the **settled** set. A pipeline would be faster in wall clock, but a
branch deciding whether to fork needs the failure log as of the whole beam's
last round, not as of whenever a sibling happened to finish.

### Ordering constraints

| order | protects | enforced |
|---|---|---|
| directives before advance | a human's instruction lands on a turn boundary; a branch mid-turn holds a ledger it read before the change | docstring |
| score before cull | retention reads critic scores; stale ones decide a live branch's fate on last round's evidence | **`:constraints`** |
| cull before spawn | a branch culled this round must not also spend the branch budget on children | docstring |
| settle before repopulate | a freed slot must be visible for the same round to refill it | **`:constraints`** |

### Branch lifecycle

```
open ──→ active ──┬──→ done       :final-answer set; may or may not end the run
                  ├──→ culled     retention rule, or a human directive
                  ├──→ abandoned  threw, or superseded by a winner
                  └──→ exhausted  turn cap
```

A branch is a map, not an object. Everything a gate needs is in it, and
everything in it is also journalled, so a resumed run rebuilds by **replay**
rather than by trusting a snapshot.

### Forking

A child inherits its parent's **conversation** — the messages up to
`gates.edn :fork-inherit :depth` (nil = all) and the turn-log slice those
messages cover. It inherits **no** gate counter: consecutive failures, the
mechanics tally, the stall clock, the phase, artifacts and the abandoned log all
start clean, because those record how the *parent* was doing and culling a
newborn for its parent's failures is what the reprieve machinery exists to
prevent.

`:forked-at` records the branch point. Without it an `{:at N}` fork's tree edge
is lossy — the depth is the only thing that says where the child left the
parent's line.

## API

### `samizdat.agent.beam` — capability

| fn | contract |
|---|---|
| `(run! {…})` | A beam to completion. Opens the run, seeds branches, drives the scheduler manifest. |
| `(run-rounds ctx branches start-turn)` | The driver: compile the round manifest, hand it the branches, own the crash record and teardown. Returns `{:status :run-id :branches …}`. |
| `(advance-branch ctx b turn)` | One branch through its turn manifest. **Throws** without `:turn-workflow` — there is no second composition of a turn. |
| `(advance-all ctx branches turn)` | Concurrent, each under a deadline. A branch that throws is abandoned; one that hangs forfeits its turn. **Neither touches the counters retention reads** — the branch never got an answer to be wrong about. |
| `(ensure-scored ctx branches turn)` | Critic scores, at most one sub-LLM call per branch per `:critic-every`. A failed scoring leaves the previous scores: stale information beats invented information. |
| `(cull-or-keep ctx branch survivors sibling-scores)` | *(lives in `cells.beam`)* the retention cascade. |
| `(repopulate ctx branches total turn)` | *(lives in `cells.beam`)* mark the strongest earning survivor to reseed. |
| `(spawn-children! ctx parent existing turn)` | `[children parent']` under the total cap. |
| `(drain-directives! ctx branches directives turn)` | Apply human directives, resolve each in the record. |
| `(record-inactive! ctx branches)` | Write the ending of every non-active branch and release what it held. |
| `(select-done-branch ctx candidates)` | Rank shipped branches by `state/rank-finished` — a `phases.edn` rubric, no model in the path. Journals the comparison when more than one qualified. |
| `(finish-now? ctx done-branch branches)` | Whether a shipped branch ends the run. `config :run :stop-on-first-done?`. |
| `(turn-deadline-ms)` | `gates.edn :turn-deadline-ms`, `HARNESS_TURN_DEADLINE_MS` overriding. |
| `(summary …)` | The run report. |

### `cells.beam` — policy

`:beam/round-open` `:beam/directives` `:beam/advance` `:beam/score`
`:beam/cull` `:beam/settle` `:beam/repopulate` `:beam/spawn` `:beam/tick`
`:beam/abort` `:beam/complete` `:beam/exhaust`, plus the public
`cull-or-keep` and `repopulate` the cascade lives in.

## Protocol

### What the driver owns, and why a manifest cannot

**The crash record.** A run that dies must say so in the journal it is judged
by. `run-rounds` catches, records, and rethrows — best effort, because a failure
to journal the failure must not replace it with a different one.

Two subtleties the manifest introduced:

- mycelium **wraps** a cell's throw, so every branch failure would reach
  `run!`'s callers as one opaque "execution error". `unwrap-round-error` peels
  to the innermost cell error — nested manifests wrap once per level, so one
  layer is not enough — and rethrows what the cell actually threw.
- the wrapper carries the **entire compiled FSM** in its `ex-data`, so
  `pr-str`-ing it into the journal would write a row the size of the workflow.
  The record keeps the failing **node** instead.

**Teardown.** Must not depend on the round's cooperation — a thrown manifest
hands nothing back. The driver keeps `:live-branches`, an atom the advance and
tick cells refresh. Leaky, documented as such, and tested.

### Retention cascade

Evaluated **in order**, against the count of branches that would still be
running after the decisions already made — so whether *this* branch survives
depends on what happened to the ones before it. The last branch standing is
never culled.

```
mechanics exhausted (cull-mechanics-multiple × threshold)  → cull, naming
                                                             whether the calls
                                                             were malformed or
                                                             policy-refused
not (failures ≥ threshold ∧ nothing banked recently ∧ survivors) → keep
failures ≥ cull-hard-multiple × threshold                  → cull, reprieve spent
inside a reframe                                            → spare, journal
critic scored viability ≤ 1                                 → cull, dead end
turn-count < juvenile-grace                                 → spare, speak
no critic scores                                            → cull, scalar rule
dominated on every critic objective                         → cull
otherwise                                                   → spare, journal
```

The scalar rule is the **trigger**; the critic's Pareto frontier is the
**verdict**. Pareto is the weakest defensible cull criterion — a frontier never
discards anything some rational preference could still want — and its known cost
is permissiveness, which is why every reprieve is a loan with a clock.

## Invariants

| invariant | enforced by |
|---|---|
| An abort stops the run without its cooperation. | `round-open` reads the flag first; `beam-test`. |
| The last branch standing is never culled. | `survivors` in the cascade; `agent-test`. |
| A branch's fate is decided on fresh scores. | `:constraints` `score → cull`. |
| A stopped branch is recorded before its slot is refilled. | `:constraints` `settle → repopulate`. |
| A fork inherits no gate counter. | `state/fork-branch`; `fork-test`. |
| A crash is journalled before it propagates. | `run-rounds`' catch; `beam-test`. |
| Teardown sees the round's own progress. | `:live-branches`; `beam-test`. |
| The scheduler is never scheduled as a turn. | `iterating?` is false for `beam.edn`; `beam-test`. |
| A non-iterating manifest runs at width 1. | `run!` forces it — five copies of a whole-run workflow multiplies the job rather than exploring one. |

## Known gaps

- **Beam width is not justified.** Nobody has measured five branches against
  one branch at five times the turn budget. `samizdat.bench.beam` is named as
  the comparison and **does not exist** — the directory is absent.
- ~~`dispose-branch-engines!` is a no-op seam.~~ Closed: it disposes the
  per-branch eval session.
- ~~`drain-directives!` rejects pause/resume/extend/fork as unwired.~~ Closed
  (2026-08 audit): all eight advertised kinds land. `pause`/`resume` are also
  applied by `await-resume!` itself, since the beam parks there upstream of
  the directives cell (blt.9); the per-turn drain leaves scheduler kinds
  pending for the beam instead of eating them mid-round (blt.10); a human
  `cull` closes the row and stays in the record (blt.11); `extend` rides the
  branch (`:extended-turns`), persists to the runs row, and reaches the turn
  slice's cap (blt.12).

Two invariants the audit added to the table's spirit: a branch is never
advanced beside its own turn — a turn past the deadline is cancelled and the
branch forfeits until that turn has terminated (`advance-all`'s `:cancelling`
registry of termination promises; RFC-013 restates blt.18 this way and
`samizdat.model.turn-journal-test` proves it over every schedule) — and the
cull cell counts only ACTIVE branches as survivors and never re-judges an
inactive one (blt.17).
