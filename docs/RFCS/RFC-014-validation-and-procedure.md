# RFC-014 — Validation and procedure: measuring a change to the loop

**Status:** partially implemented. The replay substrate, the battery's
expectations, the held-out gate in production (stage 1 on every cell, manifest
and policy save; stage 2 over the arena's arms — karamazov-7mo.4 / ylte.4) and
the procedural graph's mechanism are built and tested. A project's CASES are
its own data, added with the `battery` tool; the graph is not built.

## Purpose

RFC-010 specifies how the harness decides what to rewrite and concedes, in its
own words, the hole this layer fills:

> **Nothing here is held out.** Fitness is still computed over the same turns
> the change was made on, so the comparison is between two stretches of
> different work rather than between two configurations on the same work.

This layer makes that comparison a measurement. It also specifies the second
graph — a procedural prior over the agent's own actions — because the same
evidence that says a change needs a gate says a prior without one is worse than
no prior at all.

The external source is *Procedural Graphs: Self-Evolving Execution Structures
for LLM Agents* (2609.09153v1), read in full; `research/2609.09153v1/`. Its
numbers are quoted where they decided something here.

## Scope

**This layer decides** whether a candidate change to userspace may go live, and
what the agent is told about which action may follow which.

**It must not decide** whether the WORK is correct — that is the ship gate and
the tests (RFC-008) — nor what a steer says (RFC-007), nor which signals exist
(RFC-010), nor who may act (RFC-012).

**It hands** a verdict to the mutation protocol (RFC-002), a refusal reason to
the interventions record that RFC-012's supervisor reads, and a localized
neighbourhood to whatever renders guidance.

## The ordering constraint

This is the layer's central finding and it is not a preference. The paper
measures five construction strategies on MultiChallenge against an unguided
baseline of **87.50**:

| configuration | score | |
|---|---|---|
| unguided baseline | 87.50 | |
| hand-written prior, no evolution | **58.93** | −28.6 |
| prior + one offline edit, no gate | **53.57** | −33.9 |
| prior + evolution **with a validation gate** | **92.86** | +5.4 |

A prior nobody validates edits to is the second row. A prior edited without a
gate is the third. **The gate comes first**, and that is why RFC-010's
karamazov-7mo.4 moved from "largest of the four, do it last" to the
prerequisite everything else waits behind.

`samizdat.procedure` therefore ships **no graph**, and says so in its
docstring.

## Model

```
  A CANDIDATE EDIT                          THE ADVISORY GRAPH
  (RFC-002's mutation protocol)             (consulted, never traversed)

  validate   does it still compile?         nodes are TOOLS
     ↓                                      edges are admissible transitions
  soak       does it run without            each edge carries
             throwing? (10 s, effects         condition / guidance / pitfalls
             stubbed)                             │
     ↓                                            │ localize on the last
  BATTERY    does it REGRESS?  ◄────────────┐     │ dispatched tool, take
     ↓       replay held-out cases,         │     │ the h-hop neighbourhood
  commit     per-target, accept on tie      │     ▼
                                            └── structural checks:
                                                traps, dangling, non-tools,
                                                ambiguity, dead edges
```

### Replay

`samizdat.replay` freezes a finished run as a case: the model's side of the
conversation, per branch, in order, off `turns.assistant_text`. `complete-fn`
serves it back in the shape `infer/complete-fn` produces.

**Free in tokens, not in wall-clock.** A replay pays no provider and its cost
is bounded and repeatable, which is what lets the gate run one per case. It is
not instant: every tool the recorded conversation calls genuinely executes, so
a replayed run reads real files, runs real shells and takes real seconds. A
measured 125-turn replay spent 0 provider tokens — against 684,076 for the same
run before the drivers actually carried the injected `complete` — and still
took minutes.

**One cursor per branch, and an unknown branch is refused.** A beam is several
conversations. `case-complete-fn` dispatches on the tape's `:id` and keeps a
cursor for each, because one cursor across them interleaves the recording and
every branch reads another's next line. A branch the recording does not have is
refused by name rather than served the nearest one: driving a real replay, the
feature loop escalated `T0` through five revisions and fanned out to four
workers, ten branches the recording had never seen, and substituting produced a
run that looked like a clean replay and was not one.

**Only the model's side is recorded.** The tape the harness assembled, the
gates that fired, where it routed — those are exactly what an edit is allowed
to change, and recording them would pin the thing under test. One number about
the harness's side is kept beside each reply: the digest of the request it
answered (`turns.request_hash`, `infer/request-digest` over the wire
fingerprint). It pins nothing; it lets the replay say whether the tape it is
handed is the one the reply answered.

**The blind spot, stated so it is never rediscovered as a surprise — and
measured.** The recorded reply is fixed, so under replay a changed *prompt*
cannot change the model's behaviour. Replay measures what the harness DOES with
a given conversation. Whether different words would produce a better
conversation is the live sweep's question. This is why the validation design is
a hybrid and not a cost compromise. Since karamazov-luqc.2 (after ZCode's
workflow engine, which keys every replayed step on a hash of its input) the
point at which the conversation stops being *given* is named: the first reply
served against a tape whose digest is not the recorded one comes back with
`:replay {:diverged {:turn k :recorded :rendered}}`, `loop/call-model` journals
it as a `:replay-diverged` note, and `battery/check` carries it as
`:diverged-at`, so a verdict says "harness side only from turn k" instead of
scoring turns k+1… on a conversation that never happened. The reply is still
served, because the harness side is still what replay measures; a case that
wants faithfulness itself as a target asserts `[:replay-faithful]`.

**Running past the recording is a result, not a gap.** A candidate that takes
more turns than the recording has exhausts the fixture, and that comes back as
`:replay/exhausted` with the branch and the count. Returning nil, or repeating
the last reply, would score a candidate on a conversation that never happened.

### The battery

`samizdat.battery` holds a closed vocabulary of assertions over a finished
run's journal, and the accept rule.

**Per target, never a scalar.** A validation score of 0.62 against 0.64 says a
candidate is worse and cannot say what it broke, so the refusal teaches nothing
and the next round re-derives the same edit. Every expectation carries a name
and its own verdict, and a refusal reads `battery: 1/2 — broke routes to ship`.

**The vocabulary is closed, and an unknown verb throws.** This gate decides
whether a self-modification goes live; a case carrying an arbitrary form would
put an eval seam in the one place that must not have one — the same argument
`samizdat.symbolic` makes for its guard registry. A case whose assertion nobody
implemented would otherwise pass forever, and the battery would grow a hole
shaped like coverage.

**Ties are accepted**, following Algorithm 1 line 17: it lets the loop drift
laterally through neutral edits into a better basin. A gate demanding
improvement would refuse every edit that fixed something the battery does not
measure, which is most edits, since the battery is small by design.

**Only flips count as regressions.** A target already failing before the edit
is not this edit's doing, and refusing on it would turn a validation gate into
a freeze.

### The procedural graph

`samizdat.procedure` is the mechanism for a second graph, at an altitude the
manifests are not at.

|  | manifest (`loop.edn`) | procedural graph |
|---|---|---|
| who walks it | the engine | nobody — it is read |
| nodes | harness stages | the agent's own tools |
| span | one turn | a task, across turns |
| a wrong edge costs | the turn breaks | one guidance string the model may ignore |

`Match(a_{t-1}, V)` is a lookup on the last dispatched tool; the neighbourhood
is a join over `samizdat.symbolic` facts, because a `(procedure, relation,
procedure)` triplet already *is* the shape `facts` takes.

**Conditions are patterns, never `:when` forms.** `gates.clj` compiles those
with `eval`. This is the object the agent rewrites most often, so it is exactly
the wrong place to inherit host evaluation.

**A failed match returns empty**, deliberately departing from §3.2, which falls
back to the full graph. Table 3 measures that fallback scoring *below no graph
at all* on ALFWorld — 54.48 against a 72.58 baseline, at 5.3× the tokens — so
silently serving the whole graph is the measured-worse option and the caller
decides.

### The gate in production (karamazov-7mo.4 / ylte.4)

`samizdat.heldout` is what runs the battery. Every `cell save`, `manifest
save`/`patch` and `policy save` validates in the turn as before, then hands
the edit to `heldout/defer!`: the battery is replayed with the candidate in
place and without it on a background lane, and an edit under which a target
that passed fails is refused with the targets named
(`prompts/heldout-refused.md`). **Off the turn** (karamazov-nha1): a refusal is
confirmed by a second replay, so one case's refusal took 733 s against a
900 s turn deadline while the gate ran inside the tool call. The tool now
answers at once that the edit is pending and not live; the verdict reaches
the branch that made it as a `message` directive on a later turn
(`prompts/heldout-decided.md`) and is journalled as `:heldout-verdict`. Edits
are measured and committed one at a time, in the order they were made.

**One child process per case.** A replay must read the candidate as the
project's userspace from every branch fiber, and a dynamic binding does not
cross a fiber while a global override would leak the candidate into the live
run proposing it. So the candidate is materialized: the case's fixture — the
recorded run's `:git-baseline`, unpacked with `git archive` — gets a copy of
the project's `.samizdat/` with the candidate file written in, and
`samizdat.heldout.child` starts the harness there on an in-memory database,
replays the recording through `beam/run!`, and runs `battery/check`. Side
calls a cell makes on its own (critic, judges) were never recorded and are
refused identically on both sides.

**The same computation every time** (karamazov-x0dx). A recording that forked
replayed its branches concurrently, and which turn landed first was timing:
one baseline read 11/14 and 13/14 minutes apart. The child runs
`beam/run!` with `:serial-turns? true` — each round's turns run one after
another in branch order — and `:oversight? false`, since the supervisor's
passes fire on a clock and the recording cannot answer them.

**The rule is non-compensatory, per target.** No target that passed at
baseline may fail on the candidate, and both sides must cover the same
targets. Ties are accepted. A case that does not run at baseline is set aside
by name; one that ran and no longer runs is a regression.

**One replay per edit, not two.** Measurements are cached by the project and
the CONTENT of its userspace, so the candidate's measurement is the baseline
of the state a commit of it produces.

**The battery may grow and may not be weakened — now enforced.**
`battery_cases` (v37) is the authority: a case is written once under its id.
A case file edited under `.samizdat/battery/` runs as stored and the edit is
named; a deleted one still runs. The `battery` tool adds a case from a
finished run (`heldout/draft!`, which pins its baseline under
`refs/samizdat/battery/`) and has no remove.

**Recorded beside every number.** `heldout_checks` holds one row per target
per edit: before, after, the verdict, and the fixture sha, harness revision,
model and scorer it was measured on.

**What stage 1 does not judge.** Prompt edits: the replies are fixed. That is
stage 2 — `heldout/live-verdict` over the arena's baseline and candidate
arms, per task, in RRSI's order: floor (median fitness no lower than the best
kept score less the baseline's own noise band), cost (a gain pays for its
tokens by `:fitness :cost-rule`; inside the band only a cost cut or a declared
structural change is admissible), guards (no acceptance criterion the baseline
always met is lost, no suite goes red). The arena prints it with
`ARENA_ACCEPT=<baseline>,<candidate>`.

## API

| fn | contract |
|---|---|
| `replay/record` | A run's model-side conversation as a case, with the run id and time it was taken, and under `:sent` the digest each reply answered. |
| `replay/tape-digest` | What a tape would send, digested the way a live turn's `request_hash` is — the two are comparable. |
| `replay/complete-fn` | Case + branch → a `complete`. Stateful; one per branch, since two branches are two conversations. Carries `:wire` like a live call, and `:replay {:diverged …}` on the first reply served against a tape it did not answer. |
| `battery/check` | Expectations against a finished run → `{:ok? :passed :total :targets :diverged-at}`. |
| `battery/accept?` | Whether a candidate may commit. `>=`, ties included; refuses outright when the two results cover different numbers of targets. |
| `battery/regressions` | Targets that passed before and fail after — the flips only. |
| `battery/vocabulary` | Every assertion verb a case may use. |
| `procedure/load-graph` | A graph definition as queryable facts that remember their definition. |
| `procedure/neighbourhood` | The h-hop directed neighbourhood with attributes; empty on a miss. |
| `procedure/check` | `{:ok? :traps :dangling :unknown-tools :ambiguous :shadowed}`. |
| `procedure/tool-catalog` | The action names a node may use, from the tool registry. |

## Protocol

```
mutation (RFC-002)
  validate → soak → BATTERY → commit
                       │
                       └─ refusal → :mutation-rolled-back {:reason :attempt}
                                      └─ read by oversight/gather, rendered
                                         into the supervisor's brief
```

The battery runs **last** of the three because it is the dearest — a replay per
case — and there is nothing to learn from paying it for an edit the soak
already rejected. `soak-fn` and `battery-fn` are injected, so this namespace
need not reach up into the workflow layer for a way to run a replay, and a test
can drive the order.

**Opt-in.** A project with no battery behaves exactly as it did before.
Otherwise a project without recorded cases could not tune itself at all, which
is a worse failure than the one the gate prevents.

## Invariants

| invariant | enforced by |
|---|---|
| A case records the model's side only. | `replay/record` selects `assistant_text`; `replay-test/a-case-is-the-runs-replies-in-order-per-branch`. |
| Replay never invents a reply. | `:replay/exhausted`; `replay-test/a-replay-that-runs-past-its-recording-says-so-rather-than-inventing`. |
| Replay is deterministic and spends no provider tokens. | `replay-test/replay-costs-nothing-and-is-deterministic`. |
| A branch the recording lacks is refused, never substituted. | `:replay/unknown-branch`; `replay-test/a-branch-the-recording-does-not-have-is-refused-not-substituted`. |
| One cursor per branch. | `replay-test/one-complete-keeps-a-cursor-per-branch`. |
| An injected `complete` reaches the turn through a real driver. | `replay-test/a-drivers-injected-complete-actually-reaches-the-turn`, which runs `beam/run!` with `llm/chat` redefined to throw. |
| An injected `complete` does not change the live path. | `replay-test/without-an-injected-complete-nothing-changes`. |
| An unknown assertion refuses rather than passing. | `verb :default` throws; `battery-test/an-unknown-assertion-is-refused-at-check-time-not-ignored`. |
| A case cannot carry executable code. | The vocabulary is a closed multimethod; `battery-test/a-case-cannot-smuggle-a-form`. |
| A tie commits. | `battery/accept?` is `>=`; `battery-test/accept-on-tie`. |
| Two results over different target sets are incomparable, not a pass. | `battery/accept?` requires equal `:total`; found on real data, where a 14/14 baseline against a 14/15 candidate read as `14 >= 14` and committed. |
| A pre-existing failure cannot block an edit. | `battery/regressions` filters on `was-ok`; `battery-test/a-regression-names-the-targets-that-broke`. |
| The battery runs after the soak, never before. | `battery-test/the-gate-sits-between-soak-and-commit`. |
| A refusal names what broke. | `mutation/battery-reason`; `battery-test/a-regression-rolls-back-and-names-what-broke`. |
| Every node can reach a terminal. | `procedure/check` `:traps`; `procedure-test/every-node-must-reach-a-terminal`. |
| An action node is a dispatchable tool. | `:unknown-tools` against `tools/tool-names`; `procedure-test/an-action-node-must-be-a-real-tool`. |
| A failed match does not serve the whole graph. | `procedure-test/an-unknown-active-node-localizes-to-nothing-rather-than-guessing`. |

## What this does not do

**A project with no cases is not gated**: the tools consult the battery on
every save, and with no cases it passes nothing and refuses nothing.

**A battery is per project.** Userspace is per project, so the cases that
gate an edit are that project's own runs: an edit tested on endless-flight's
cases is tested on endless-flight, and samizdat working on its own repo keeps
its own battery in its own `.samizdat/`. Promoting a project's edit into the
shipped templates is where both subjects have to be replayed, and that is not
automated.

**"May add, may not weaken or delete" is enforced by the table, not the
files** (`battery_cases`); a person with the database can still delete a row,
and that is deliberate.

**No graph ships.** Building one by hand is the 58.93 row. It should be grown
by a refiner from `Start → End` against the gate — scratch-with-evolution beat
scratch-without by 9.3 F1, and beat the hand-written prior too.

**Guidance is not wired.** Nothing calls `neighbourhood` in a run. The intended
seam is a node between `:start` and `:infer` in `loop.edn`, beside the
compaction ladder, since guidance shapes the request and must precede `:infer`.

**Cost is known and unbudgeted.** A 2-hop localization measures ~4.4 ms, and
precompiling does not help — the cost is the pldb join. Negligible per turn
against a provider call; not negligible for a validation sweep of many rules
over many nodes.
