# RFC-010 — Adaptation: how the harness gets better

**Status:** implemented.

## Purpose

The harness runs a loop it can rewrite. This RFC specifies how it decides
*what* to rewrite: the signals it collects while running, the two memories it
keeps them in, the two roles that act on them, and the selection pressure that
makes a change stick or go away again.

The short version: **variation is cheap, measurement is the hard part, and
selection is what makes the difference between a system that learns and one
that accumulates edits.**

## Scope

**This layer decides** whether the loop is running well, whether a change to it
helped, and what is worth carrying to the next run.

**It must not decide** whether the WORK is correct. That is the ship gate and
the tests (RFC-008). A run can score well on every signal here and produce
something wrong; a supervisor optimising these numbers alone would trivially
maximise them by having branches take no risks. Fitness measures how well the
loop is RUNNING, not whether it is running toward the right thing.

**It hands** a session block to the supervisor, directives to the interventions
queue (RFC-006), and rows to `knowledge`.

## Model

### The evolutionary frame, and where each piece lives

| | mechanism | where |
|---|---|---|
| **variation** | the supervisor edits a cell, manifest, prompt or threshold | userspace (RFC-001) |
| **fitness** | one scalar per turn from the live tally | `session/fitness-of` |
| **selection** | experiment → verdict → revert or keep | `session/experiment!`, `verdict` |
| **heredity of variation** | userspace is versioned, append-only | RFC-001 |
| **heredity of selection** | verdicts distilled to memory, with the outcome as their record | `knowledge/distill-verdicts!` |

The last row is the one that is easy to leave out and fatal to leave out.
Without it an experiment dies with the process, so a lever that was tried and
made things worse is forgotten and tried again. **Variation and measurement
without inheritance is not evolution; it is thrashing with statistics.**

### Two memories

They answer different questions and are kept in different places for that
reason.

```
SHORT TERM  samizdat.session      in memory, per process, dies with it
            what is happening right now — turns, tools by outcome, call
            mechanics, gate settlements, ship verification, provider trouble

                    │  distilled at run end
                    ▼

LONG TERM   knowledge table       durable, per project, ranked
            what this project has learned — facts, rules, episodes, and the
            record of whether acting on each one helped
```

Short-term memory is process-wide and deliberately **not** per-run: a pattern
that only shows up across three runs is exactly the pattern a single-run digest
cannot see. It is fed from the seams everything already crosses — the journal's
turn and gate writes, the ship gate, the provider client — and every hook is
wrapped, because a counter must never be able to cost a turn.

Long-term memory ranks by **effective salience**:

```
salience  (by kind, reinforced by use, decayed by disuse)
+ recent-use bonus  (used within the last N RUNS — the clock is runs, not days)
+ effectiveness   (log-damped, signed, capped — did acting on it work?)
+ corroboration   (log-damped, capped — how many distinct runs re-observed it?)
+ confidence      (centred, weighted low — is it likely TRUE?)
```

The clock the store ages by is runs, not days: a run is an opportunity for a
memory to be needed, a day is not. `idle_runs` counts the run ends a memory
existed through without being used or re-observed; `age!` advances it once
per run end, and decay begins only past the window. The working set
(`current = 1`) is bounded per kind: over the cap, the lowest-standing rows
are retired with the reason `evicted`, still readable by id and in their
lineage — demotion, never deletion. Episodes seen in enough distinct runs are
surfaced to the supervisor as candidates for a rule; the store never promotes
one itself.

Salience and confidence are separate axes on purpose: a fact can be important
but contested, or trivial but certain, and collapsing them loses the
distinction a supervisor needs when two memories disagree. Effectiveness is the
axis that makes memory a loop rather than a list — everything else measures
whether a memory gets READ; only `outcome` measures whether reading it HELPED.

Distillation writes a finding **once**. A recurring pattern is not new
knowledge, it is the same knowledge confirmed, so recurrence shows up as a
record and a corroboration count rather than as volume. Identity is a
`pattern_key` COLUMN, not a similarity match on the text: a row has a key, and
reproducing text-identity matching on top of a table with a primary key
inherits a constraint we do not have.

### Two roles, and the asymmetry between them

This is the distinction most easily lost, and losing it produces a supervisor
that reworded a prompt because a provider was down.

|  | **Steering** | **Tuning** |
|---|---|---|
| runs as | a watcher thread, continuously | the supervisor role, after the work |
| target | this branch, now | the loop itself |
| instrument | a directive through the interventions queue | a cell, manifest, prompt or threshold |
| evidence bar | **what one run shows** | **corroborated across runs** |
| cost of being wrong | one turn | every run from here on |

The evidence bar follows from the cost. A nudge is wrong for one turn and the
branch reads the next one. A userspace edit is wrong for every run until
somebody changes it back — and a single run goes wrong for reasons that have
nothing to do with the loop: a flaky provider, an unlucky task, a model having
a bad day. So the watcher, which can only see one run, is restricted to the
reversible instrument. **It steers; it never tunes.**

The watcher exists because the supervisor role is a manifest node: it runs
between rounds, in sequence, and only in workflows that wire it. A run losing
every turn to empty provider replies reaches no round boundary quickly, and the
node never gets a look. The watcher is the other half — it watches the turns go
by and says something while the pattern is still forming, the way a person
watching a harness does.

It intervenes through the same queue a human uses. RFC-006's rule — a directive
lands on a turn boundary, because a branch mid-turn holds a ledger it read
before the change — is not a rule the harness's own observer gets to skip.

## API

### `samizdat.session` — short-term memory

| fn | contract |
|---|---|
| `(reset!)` | A fresh tally. `system/start!` calls it. |
| `(observe! path)` | Bump one counter. Total and forgiving: an unknown path creates itself, so anywhere in the loop can contribute cheaply. |
| `(observe-turn! {…})` | One turn: its tool, its category, its parse signals. One call, because a partial record would look like a clean turn. |
| `(snapshot)` | The tally. Marks and experiments excluded — they hold tallies of their own. |
| `(fitness-of tally)` / `(fitness)` | One scalar per turn. `nil` for an empty tally: no turns is the ABSENCE of a measurement, and `0.0` would read as neutral. |
| `(experiment! name {:change :hypothesis})` | Bind a change to what it is expected to do. Throws `:samizdat.session/too-many-open` past the cap. |
| `(verdict name)` / `(verdict name p)` | `:better` `:worse` `:unchanged` `:too-early` `:confounded`, with per-turn fitness before and after. Both sides are scored under ONE policy — the before-fitness stamped at `experiment!` is recomputed, not carried — and the verdict also carries `:health-blind` (the same two tallies with the provider's terms zeroed), `:regraded` (the stamped number differed, so the run moved its own scale), and `:branches` (how many measured branches went backwards). |
| `(reverted! name kept?)` | Settle a verdict once acted on. |
| `(unsettled-losses)` | Measured-and-found-wanting changes nobody has acted on. |
| `(findings)` | Patterns crossing `gates.edn :session-findings`. Reports successes too. |
| `(render mark)` | The block the supervisor reads. |

### `samizdat.memory` — the ranking model

`(base-salience kind)` `(effectiveness w f)` `(confidence-bonus c)`
`(effective-salience row)` `(reinforced s)` `(decayed s)` `(rank rows)`.
All pure; every constant is `gates.edn :memory`.

### `samizdat.watch` — the steering thread

| fn | contract |
|---|---|
| `(start! ctx)` | Begin watching. Returns an idempotent stop fn; the driver calls it in a `finally`. A ctx with no conn gets a no-op. |
| `(pass! ctx seen)` | One observation, exposed so a test can drive it a step at a time. |

### `samizdat.store.knowledge` — long-term memory

`remember!` `recall` `standing` `touch!` `record-outcome!` `corroborate!`
`corroborated?` `by-pattern` `distill!` `distill-verdicts!` `forget!`
`age!` `curate!` `evict!` `graduation-candidates`.

## Protocol

```
every turn ──→ session/observe-turn!         short-term memory accrues
journal gate write ──→ session/observe!      firings AND settlements
ship gate ──→ session/observe!               ran / green / red / skipped
provider failure ──→ session/observe!        by REASON, carried from detection

watcher thread (every :poll-ms)
  └─ session/findings → severity filter → not-already-raised → cap
       └─ interventions/submit! {:kind "message" :issued-by "watch"}
            └─ drained at the next turn boundary, like a human's

supervisor role (between rounds, in workflows that wire it)
  └─ reads: session/render, knowledge/standing and graduation-candidates,
            userspace drift (store.userspace/drift), experiment verdicts
  └─ acts:  experiment! → edit userspace → verdict → revert or keep

run end (both drivers, knowledge/distil-session!)
  ├─ knowledge/age!              one more run every untouched memory went unused in
  ├─ knowledge/distill!          findings  → episodic memory, corroborated
  ├─ knowledge/distill-verdicts! verdicts  → procedural memory, outcome recorded
  ├─ knowledge/distil-project!   shell facts → semantic memory, one per command as it RAN
  ├─ knowledge/curate!           decay past the window, in runs
  └─ knowledge/evict!            retire the lowest-standing rows over a kind's cap
```

## Invariants

| invariant | enforced by |
|---|---|
| A counter can never cost a turn. | Every hook wrapped; `journal/observe-session!`, `loop/observe-turn!`. |
| The watcher never edits userspace. | It submits `message` only; `watch-test/the-watcher-steers-and-never-tunes`. |
| A directive from the watcher lands at a turn boundary. | It uses `interventions/submit!`; the drivers drain it (RFC-006). |
| At most one measured-but-unsettled change is in flight. | `session/experiment!` throws past `:max-open-experiments`; `session-test`. |
| A losing change is raised until it is settled. | `unsettled-losses` in the rendered block; `session-test`. |
| A pattern is corroborated only by DISTINCT runs. | `last_run_id` guard in `corroborate!`; `knowledge-test`. |
| A finding becomes one memory, however often it recurs. | `pattern_key` column; `knowledge-test/one-lever-worded-two-ways-is-one-record`. |
| Recall reinforces; being shown by default does not. | `recall` calls `touch!`, `standing` does not; `knowledge-test`. |
| A memory is not aged by the run that wrote it. | `age!` ages only rows created before the run started; `knowledge-test/a-memory-written-this-run-does-not-decay-when-the-run-ends`. |
| Eviction retires; it never deletes. | `evict!` goes through `retire!`; `knowledge-test/over-the-cap-the-lowest-standing-memories-are-retired-not-deleted`. |
| The store surfaces graduation candidates; it never promotes one. | `graduation-candidates` is a read; `oversight-pass.md` hands the decision to the supervisor. |
| An unfinished experiment teaches nothing. | `distill-verdicts!` skips `:too-early`; `session-test`. |
| Nor does one the world ruined. | `distill-verdicts!` skips `:confounded`; `session-test/a-confounded-experiment-teaches-nothing-and-is-not-written`. |
| A verdict the provider decided says so instead of picking a direction. | The health-blind reading is scored beside the full one and disagreement is `:confounded`; `session-test/a-window-the-provider-ruined-is-confounded-not-worse` and its mirror. |
| Both halves of a verdict are on one scale. | `verdict` recomputes the before side under the policy in force now, and reports `:regraded` when that differs from the stamped one; `session-test/both-halves-of-a-verdict-are-scored-on-one-scale`. |
| An aggregate never hides a branch the change broke. | `:branches` counts the measured branches that regressed; `session-test/a-verdict-counts-the-branches-that-went-backwards`. |
| Fitness is `nil`, never `0.0`, with no turns. | `fitness-of`'s `(when (pos? turns) …)`; `session-test`. |

## What this does not do

Two limits worth stating rather than discovering.

**Fitness is a proxy.** It measures turns spent on work rather than on
overhead. A change that games it while making the work worse is a bad change
however the score moves, and the supervisor prompt says so. The ship gate and
the tests are what measure correctness.

**Nothing here is held out.** Fitness is still computed over the same turns the
change was made on, so the comparison is between two stretches of different
work rather than between two configurations on the same work. `:confounded`
and `:regraded` name two ways that comparison can be something other than a
measurement; they do not make it a controlled one. The frozen battery replayed
against old and new userspace is karamazov-7mo.4, and until it exists a verdict
is evidence rather than a result.

**There is no human gate.** backpass, which shares this design's framing, never
writes without accept/reject. samizdat's substitute is the mutation protocol —
compile, soak, rollback (RFC-002) — plus versioned userspace, so a bad edit is
a revert rather than a broken harness. That is a different bet, deliberately
taken, and it depends on the validation being real.
