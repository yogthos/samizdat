# RFC-012 — The implementer and the supervisor

**Status:** implemented. The audit below is of the code as it was found; each
finding carries what was done about it (karamazov-ts3o).

## Purpose

Specifies the two streams the harness runs, what each is responsible for, and
the one pattern every supervisory mechanism must follow. It exists because
there are currently five of them, on three clock domains, with no shared
contract — and two have already been observed overwriting each other.

RFC-007 specifies *what may be said* to a branch and RFC-010 specifies *which
signals are collected and how selection uses them*. Neither says **who is
allowed to act, when, and through what**. That is this RFC.

## Scope

**This layer decides** the division of labour: which stream owns the task,
which owns the loop's health, when each may act, and what each may touch.

**It must not decide** what a steer says (RFC-007), which signals exist
(RFC-010), how branches are scheduled (RFC-006), or what a manifest means
(RFC-002).

## Model

Two streams, running in parallel, coupled only by events and by the mutation
protocol.

```
  IMPLEMENTER STREAM                    SUPERVISOR STREAM
  manifest-driven, owns the task        manifest-driven, owns the loop

  turn: assemble → infer → parse        phase 1  WATCH
        → dispatch → journal            per turn, while the turn runs.
        → settle → arbiter → route      Is it progressing? Is it stuck?
           │                            Nudge, through the one write path.
           ├── events ────────────────► 
           │                            phase 2  EVALUATE
           │                            at the turn boundary. What went well
           │                            and what did not, from the metrics
           ◄──── directives ────────    the turn produced. Adjust the
           ◄──── manifest edits ────    manifests so the next turn does
                                        better. This is the selection step.
```

### The implementer

Goes through the workflow its manifests describe and solves the task it was
given, in the project it is working on. It is steered by its manifests, and by
nothing else it can see: a directive arrives as a message at a turn boundary,
and a manifest edit arrives as a different graph next turn. The implementer
never knows it is being supervised, which is what keeps the two separable.

### The supervisor

A parallel stream whose subject is **the implementer's loop, not the
implementer's task**. It does not judge whether the code is right — that is the
ship gate and the tests (RFC-008). It judges whether the loop is making
progress and is not blocked, and it has two phases:

**Phase 1 — watch.** Runs while the turn runs. Sees the turn's events as they
happen, notices a pattern forming, and nudges. The distinction that matters:
it must be able to act on a turn that has not finished, because a run losing
every turn to empty provider replies never reaches a boundary.

**Phase 2 — evaluate.** Runs at the turn boundary. Reads the positive and
negative metrics the turn produced, decides what went well and what did not,
and may adjust the manifests so the next turn succeeds more often. This is the
genetic-algorithm step: variation is the manifest edit, and the measurement is
what makes it selection rather than drift.

### Why the supervisor is not a node

A node runs when the graph reaches it, which is exactly when a stuck turn does
not. `samizdat.watch`'s docstring already makes this argument against
`:feature/supervise`, and it is the general rule: **anything that must act on a
turn that is going wrong cannot live inside that turn's graph.**

## Audit: what exists today

Five mechanisms did supervisory work when this was written. Three clock
domains, two intervention paths, no shared contract.

| # | mechanism | kind | clock | acts through |
|---|---|---|---|---|
| 1 | `gate/arbiter` | **node** in every loop manifest | per turn | one message into the branch (RFC-007) |
| 2 | `samizdat.watch` | background thread | 5s poll | the interventions queue |
| 3 | `oversight.edn` + `agent/oversight.clj` | **parallel state machine**, branch `SUP` | 120s wall clock | mutation tools |
| 4 | `:feature/supervise` | **node** in feature.edn, branch `S<revision>` | per round | `:implement-strategy`, `:feature/turn-budget`, `:feature/stop` |
| 5 | beam `score → cull → settle → repopulate → spawn` | nodes in beam.edn | per round | branch selection |

`samizdat.agent.supervisor` is **not** one of these despite its name: it is
pure stall-detection predicates (`over-studying?`) that gates.edn rules call,
so it belongs to mechanism 1.

What it converged to:

| # | mechanism | is now | clock | acts through |
|---|---|---|---|---|
| 1 | `gate/arbiter` | the implementer's own boundary — where every steer, including a queued directive, is rendered and recorded | per turn | the branch, as the one renderer |
| 2 | `samizdat.watch` | gone; `oversight/reflex!`, phase 1 of the stream | every poll, on the run's events | the interventions queue, as `watch` |
| 3 | `oversight.edn` + `agent/oversight.clj` | the one supervisor, branch `SUP`, both phases | phase 2 on the turn boundary, spaced by `:every-ms` | the queue (`intervene`) and the mutation tools |
| 4 | `:feature/supervise` | not a supervisor: the boundary where the stream's outer-loop directives (`switch`, `budget`, `stop`) land | per round | the round's data map, deterministically |
| 5 | beam cull | selection on the same number evaluation reads (F3) | per round | branch selection |

### What the audit found

**F1 — Two of the five are nodes inside the implementer's graph.** *(Fixed.)*
Mechanisms 1 and 4 ran only when the graph reached them. `watch` existed
precisely because that is too late, so the harness already contained both the
mistake and its refutation.

The resolution is different for the two. The **arbiter** stays a node,
because it is not a supervisor: the model above puts it inside the
implementer's turn (`settle → arbiter → route`), and what it does is render
the one steer a boundary gets — including a queued directive, which reaches
the branch through its `:human-directive` gate. It does not need to act on a
turn that is going wrong; the stream does. **`:feature/supervise`** stops
being a supervisor. It no longer runs the supervisor role, opens no branch,
and makes no model call: it drains the outer-loop directives waiting at the
round boundary (`switch`, `budget`, `stop`, from `interventions/workflow-kinds`)
and applies them to the round's data map, resolving each applied or rejected
with a reason. What the stage used to be handed — the round's outcome, the
soft cap, the stage crashes — the stream now reads off the journal
(`:route`, `:review`, `:critique` and `:stage-error` notes) in
`:oversight/gather`, and the cap and a crash are each reason enough to spend
a model call. The stream's brief carries the three levers.

**F2 — Phase 2 is not triggered by the turn.** *(Fixed.)* The model says
evaluation happens at the turn boundary. Mechanism 3 fired on a 120-second
wall clock unrelated to turns; 4 and 5 fire per *round*. Nothing evaluated a
turn when the turn ended.

The stream drains the event bus once per poll and both phases read that
drain. The reflex looks whenever anything of the run arrived — any event, not
only a traced step, since role sub-loops are compiled without the tracer and
journal turns instead. The reasoning pass is due when a `:turn` record of the
run has arrived since the last pass and `:every-ms` have elapsed
(`oversight/due?`): the clock is a spacing now, not a trigger, and a run that
is idle buys no passes. The first pass is still immediate. A stream started
without a bus (a test, a driver with none) keeps the clock, explicitly.

**F3 — Evaluation and selection are separate mechanisms with separate
metrics.** *(Fixed; see below.)* Mechanism 5 selected branches on critic
scores and failure counts; mechanism 3 tuned manifests on the telemetry
digest and session marks. The model describes these as one phase. A branch
could be culled for a trajectory the manifest tuner never saw, and a manifest
tuned on evidence the culler never read.

The number they share is **session fitness per branch** (karamazov-ts3o.2):
`samizdat.session` keeps the same counters cut per branch, and
`branch-fitness` scores them with the same `fitness-of` and weights the
supervisor's experiments are judged by. The cull carries it onto the
retention frontier as a measured objective beside the critic's judged ones —
a triggered branch is dominated only by a sibling at least as good on every
critic objective *and* at least as fit, and with no critic at all the fittest
line is not culled while nobody is measurably doing better — and every cull
reason cites it. The supervisor's digest lists it per branch, so a tuning
decision and a cull decision read the same scale.

**F4 — Two supervisors have collided.** *(Fixed.)* `:oversight/reason`'s
docstring records it: the stream opened `SUP` and `:feature/supervise` opened
`S<revision>`, and "run 498450e1's S0 holds 26 turn rows numbered up to 14,
the stream and the stage overwriting each other's turn numbers, and a record
that cannot say which supervisor said what is a record of neither."

Renaming the stream's branch stopped the collision; F1's fix removes the
second supervisor. One supervisor per run, one identity (`SUP`), one context —
the stream's carried branch — and the roles/supervisor prompt no longer
describes two.

**F5 — Two intervention paths.** *(Fixed.)* Mechanism 2 went through the
interventions queue and honoured RFC-006's boundary rule; mechanism 1 wrote a
message straight onto the branch. Both were defensible alone. Together they
meant there was no single answer to "what has been said to this branch and by
whom".

They are one path. Every supervisory write — the reflex's, the reasoning
pass's through `intervene`, a person's — is a queued directive; every
directive is drained into the branch's `:pending-directive` at its boundary
and rendered by the arbiter's `:human-directive` gate, which records the
firing like any other steer. So the gate-firings table is the one ledger of
what was said to a branch. What was wrong with it: the gate's message said
"a human has intervened" whatever issued the directive, so the ledger
attributed the harness's own steering to the operator. It names the issuer
now. Two things found on the way: the `intervene` tool's `extend` was refused
every time (it sends `{text: N}`, the drains read `:turns`; one parser,
`interventions/turns-asked`, reads both), and each drain rejected any kind it
did not own, which would have eaten the new workflow-level kinds at whichever
worker finished a turn first — both drains now leave `workflow-kinds` for the
workflow's own stage, symmetric with the per-turn drain leaving the
scheduler's kinds (karamazov-blt.10).

**F6 — The event bus exists and carried the wrong grain.** *(Largely fixed;
see below.)* `samizdat.events` has always been here — core.async, sliding
buffer, `publish!`/`subscribe`/`collect` — and every journal append publishes
to it. But it carried only turn-level records, after the fact, and nothing
subscribed. Meanwhile mycelium's `pre-compile` takes `:on-trace` — "callback
(fn [trace-entry]) called after each cell completes", the implementer
advancing through its state graph — and samizdat passed it nowhere, so the
supervisor re-derived what it wanted instead: `samizdat.watch` recomputed
`session/findings` on a timer.

The apparent reason a clock is still needed is **absence**: a hung provider
call or a looping cell emits no event, so nothing fires. But mycelium's
per-cell `:timeouts` turn a stall into a *transition*, and no manifest declares
one. With `:on-trace` wired and timeouts declared on the cells that can hang,
`samizdat.watch` has no remaining justification — every pattern it detects is
derivable from the stream, and the case a stream cannot express becomes a
stream event.

One constraint survives the change: `on-trace` fires **synchronously** in the
implementer's thread, so a supervisor doing work there stalls the turn it is
watching. The callback must enqueue and return. `watch`'s thread provides that
decoupling for free today, and an event stream has to provide it deliberately.

### What is already right

Worth stating, because the fix is reorganisation and not replacement:

- **The interventions queue is the correct write path.** One queue, drained at
  the boundary, with `issued-by` distinguishing `human` from `watch`. Every
  supervisory write should go through it.
- **`samizdat.session` is the phase-2 memory.** Its marks answer "did the
  change I made last round help?", which is the measurement selection needs.
- **Standing is already recorded and surfaced.** `userspace/record-run-outcome!`
  bumps a version's green/failed counts as a run ends, and `base/version-line`
  shows them, so a supervisor deciding whether to keep a predecessor's tuning
  sees what it has survived.
- **The supervisor already has its own manifest.** `oversight.edn` is a state
  machine with its own branch and its own cells — the second stream exists in
  the right shape, on the wrong clock.
- **Parking makes the streams properly independent** (RFC-002, karamazov-6y7.7):
  a failed workflow becomes a value the supervisor reads, fixes and resumes, so
  the two streams no longer race.

## Protocol

The pattern every supervisory mechanism must follow:

1. **Observe through events, not polling.** The implementer emits turn events;
   the supervisor consumes them. Phase 1 sees them as they arrive; phase 2 runs
   on the boundary event.
2. **Act through exactly two channels, never a third.** A *directive* through
   the interventions queue, drained at a boundary. A *manifest or cell edit*
   through the mutation protocol, validated and soaked before it is live.
   Writing to a branch directly is not available.
3. **Never judge the task.** A supervisory decision is about turns, blocks and
   rates. Whether the code is correct belongs to the ship gate.
4. **Measure what you changed.** Every edit carries a rationale and a mark, and
   the next evaluation is shown the counters since that mark. An edit that
   cannot be measured is drift.
5. **One supervisor per run, with one identity.** Branch `SUP`. A second writer
   on a supervisor branch is F4.

## Invariants

| invariant | enforced by |
|---|---|
| A directive lands on a turn boundary, never mid-turn. | `loop/drain-directives!`; RFC-006. |
| A supervisory edit cannot go live unvalidated. | The mutation protocol (RFC-002): validate, soak, commit or roll back. |
| A refused edit is not charged to the branch. | `base/rejected` is `:mechanics` (karamazov-gn64). |
| A version's standing follows it. | `userspace/record-run-outcome!`, shown by `base/version-line`. |
| The supervisor cannot switch off the checks it is judged by. | `:schema-validation` tunes the runtime cost only; the chain check is unconditional. |

## Known gaps

- A directive of a workflow-level kind on a run whose workflow has no
  directives stage (a plain `loop.edn` beam run) is left pending by both
  drains and stays pending when the run ends — the "intervention that never
  resolves" shape of karamazov-blt.38, now reachable by three more kinds. The
  control API refuses directives against ended runs, so it cannot grow after
  the fact, but nothing marks it undelivered.
- A `switch`, `budget` or `stop` lands at the feature loop's NEXT round
  boundary. The stream cannot get between a round's verify and its route, so
  a decision made on round N's outcome takes effect after round N+1's
  implement stage. That is the boundary rule working as specified, and it is
  a round late by construction.
- The GA's selection metric and the supervisor's evaluation metric are the
  same number now, session fitness per branch (F3), but the weights behind
  it are one policy table tuned for the supervisor's experiments; whether
  the cull wants the same weights is unmeasured.
- A parked supervisor stream is a deadlock: if the failing cell is one
  `oversight.edn` traverses, the supervisor parks too and nobody can fix
  anything (karamazov-6y7.7).
