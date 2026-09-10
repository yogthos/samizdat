# RFC-014 — Validation and procedure: measuring a change to the loop

**Status:** partially implemented. The replay substrate, the battery's
expectations, the gate inside the mutation protocol, and the procedural graph's
mechanism are built and tested (karamazov-ylte). The battery's CASES and the
graph itself are not; both are data, and where they come from is specified
below.

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

**Only the model's side is recorded.** The tape the harness assembled, the
gates that fired, where it routed — those are exactly what an edit is allowed
to change, and recording them would pin the thing under test.

**The blind spot, stated so it is never rediscovered as a surprise.** The
recorded reply is fixed, so under replay a changed *prompt* cannot change the
model's behaviour. Replay measures what the harness DOES with a given
conversation. Whether different words would produce a better conversation is
the live sweep's question. This is why the validation design is a hybrid and
not a cost compromise.

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

## API

| fn | contract |
|---|---|
| `replay/record` | A run's model-side conversation as a case, with the run id and time it was taken. |
| `replay/complete-fn` | Case + branch → a `complete`. Stateful; one per branch, since two branches are two conversations. |
| `battery/check` | Expectations against a finished run → `{:ok? :passed :total :targets}`. |
| `battery/accept?` | Whether a candidate may commit. `>=`, ties included. |
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
| Replay is deterministic and costs nothing. | `replay-test/replay-costs-nothing-and-is-deterministic`. |
| An injected `complete` does not change the live path. | `replay-test/without-an-injected-complete-nothing-changes`. |
| An unknown assertion refuses rather than passing. | `verb :default` throws; `battery-test/an-unknown-assertion-is-refused-at-check-time-not-ignored`. |
| A case cannot carry executable code. | The vocabulary is a closed multimethod; `battery-test/a-case-cannot-smuggle-a-form`. |
| A tie commits. | `battery/accept?` is `>=`; `battery-test/accept-on-tie`. |
| A pre-existing failure cannot block an edit. | `battery/regressions` filters on `was-ok`; `battery-test/a-regression-names-the-targets-that-broke`. |
| The battery runs after the soak, never before. | `battery-test/the-gate-sits-between-soak-and-commit`. |
| A refusal names what broke. | `mutation/battery-reason`; `battery-test/a-regression-rolls-back-and-names-what-broke`. |
| Every node can reach a terminal. | `procedure/check` `:traps`; `procedure-test/every-node-must-reach-a-terminal`. |
| An action node is a dispatchable tool. | `:unknown-tools` against `tools/tool-names`; `procedure-test/an-action-node-must-be-a-real-tool`. |
| A failed match does not serve the whole graph. | `procedure-test/an-unknown-active-node-localizes-to-nothing-rather-than-guessing`. |

## What this does not do

**The battery has no cases yet**, and until it does the gate is inert wherever
nobody wires `battery-fn`. Cases come from two subjects, and both are required:
recorded `endless-flight` runs and recorded runs of samizdat working on its own
repo. A battery of one subject would pass an edit that breaks the other, and
self-modification is the project's reason for existing.

**"May add, may not weaken or delete" is stated, not enforced.** The rule that
a running agent may add a case from an observed failure and may never weaken
one is policy with nothing behind it yet.

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
