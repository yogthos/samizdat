# RFC-007 — The steer system

**Status:** implemented.

## Purpose

Specifies how the harness talks to a branch: what may be said, how exactly one
message is chosen per boundary, what each message predicts, and how that
prediction is settled without a model in the path.

## Scope

**This layer decides** which single message a branch receives at a turn
boundary, and records what it expected to change.

**It must not decide** anything about the *content* of the work. A gate observes
branch state and says a sentence; it never verifies, never ships, never edits.

**It hands** a `decision` map to the loop, which applies it to the branch.

## Model

### A gate is data

```clojure
{:gate       :stuck                    ; identity
 :priority   30                        ; lower wins arbitration
 :when       (form reading ctx)        ; precondition, re-evaluated every tick
 :message-file "stuck"                 ; prompts/stuck.md
 :message-suffix "…"                   ; optional inline template
 :message-form (form)                  ; or a computed message
 :prediction {:kind … :window n}       ; what should change, and by when
 :budget     :max-stuck-hints          ; a threshold key; how often it may fire
 :effect     :begin-reframe            ; optional branch-state effect
 :doc        "…"}
```

Preconditions are **re-evaluated, not latched**. That is the behaviour-tree
property worth taking from Kelley (arXiv 2404.07439): a condition that stopped
holding should stop firing, and a one-shot counter cannot express that.

Every gate **must** declare a prediction, because a gate that cannot say what
should change is one whose effect nobody can check.

### Exactly one steer per boundary

The TypeScript harness injected its milestone, emergency-review and stuck
prompts from three independent conditionals, so a branch could receive three
messages before a single turn, each pushing a different direction. dirge found
up to five. This replaces the chain with one arbiter choosing in strict
priority — the behaviour-tree fallback node.

Not zero-or-one *per gate*: exactly one *per boundary*.

### Phases

```clojure
{:initial-phase :explore
 :phases {:explore {:cap-key :explore-cap :next :build :withholds #{}}
          :build   {:withholds #{}}}
 :transitions {[:result :verified-green?] [:mark-green :clear-reframe]}
 :finished-key [forms…]}
```

`:withholds` is the per-phase tool policy, consulted **before** dispatch. Empty
today — the proof harness's explore/build policy left with its tool surface —
but the table is read, so a coding loop's policy plugs back in as a data edit.

`:transitions` maps a `get-in` path into the turn envelope to named effects.
Effect names dispatch in `loop.clj` to state functions, because **a table cannot
mutate a branch**.

### The storm guard

Every steer above keys on failure or on budget. A model repeating the same
*successful* call, or alternating A-B-A-B between two calls, tripped nothing
and burned the run (karamazov-ekk; dirge's `storm.rs` was built for the same
hole). The guard splits along the standing rule:

- **Mechanism** — `samizdat.agent.storm`: pure functions over a per-branch
  window of `(tool, canonical-args)` signatures. Canonicalization (sorted
  keys, string keys, `1` ≡ `1.0`) means signature equality is call identity,
  not serialization accident.
- **Decision** — two `phases.edn` `:refusals` rules (`:storm`,
  `:storm-oscillation`) withhold the Nth identical call and the alternation
  extension, through the same `phase-refusal` seam as every other refusal:
  `:mechanics` + `:policy-refusal? true`, because a withheld call says
  nothing about the branch's line of inquiry. It **withholds** rather than
  warns — the begin-reframe experience is that gates which merely suggest
  went 0-for-4.
- **Policy** — the `:storm-*` thresholds and the `:storm-exempt` /
  `:storm-mutating` vocabularies in `gates.edn`. dirge hardcodes its 6/3
  window; here the agent can retune or disable the guard at runtime.
- **Escalation** — consecutive withholds count `:storm-strikes`; the `:storm`
  gate (this table, priority 2.7) then *forces* an honest `give_up`, the
  last-call mechanism reused. Withheld signatures also join the `:abandoned`
  reflexion log, so stuck/safe-state steers quote them back as dead ends.

Exempt tools (the read-only surface, plus `done`/`give_up`) are never
withheld: repeated reads are the studying gate's business, and storm blocking
a `done` that last-call is forcing would be one guard punishing what another
demands — a conflict dirge documents hitting three times.

### The winner rubric

`:finished-key` is a vector of EDN forms compiled at load into a ranking tuple,
best-first: non-relaxation, slow-tier-seen, engine-diversity, confirmed-count,
id. A branch that proved the asked claim beats one that proved a weakening; a
cross-checked confirmation beats a one-shot; independent engines compose.

UCLA's FirstProof ranked prose candidates with an LLM judge because nothing
about their candidates was mechanical. Ours are engine-audited, so the ranking
is data and no model sits in the path.

## API

### `samizdat.agent.gates`

| fn | contract |
|---|---|
| `(config)` | The whole policy table, cached. |
| `(reload-config!)` | Re-read and **bump the generation**, so derived tables recompile. |
| `(gen)` | The generation. Anything compiled from the config caches against it. |
| `(threshold k)` | `(get-in (config) [k :value])`. |
| `(tool-vocab k)` | A named tool vocabulary (`:verification`, `:shipping`, `:file-write`, `:settle-called`). |
| `(gates)` | The compiled steer table. **A function, not a value** — memoized against `gen`, so a reload genuinely re-compiles. As a top-level `def` it was compiled once at namespace load and editing a gate's `:when` needed a restart, in the one namespace whose whole premise is that the policy is data. |
| `(by-name k)` | One compiled gate. |
| `(budget-exceeded? gate branch)` | Whether it has fired as often as it may. |
| `(crossed-fractions branch max-turns)` | Turn-budget notice thresholds now passed. |
| `(describe)` | The table for docs and `/v1/harness/gates`. |

**Compilation contract.** A `:when` form is compiled into `(fn [ctx] form)` with
the context keys bound as plain locals — `directive`, `done-block`, `branch`,
`max-turns`, `branch-count`, `safe-state-coverage`. A form referencing anything
else fails to compile at load, which is the fail-fast. Inside the compiled fn
the accessors are ordinary calls, so `(threshold k)` reads the config at **fire**
time: tuning a number stays runtime-editable, and only the form *structure*
compiles.

`*ns*` is bound explicitly during compilation, because `eval` resolves free
symbols against whatever namespace is current — and since the table is compiled
on first *use* rather than at namespace load, the caller could be anything.

### `samizdat.agent.arbiter`

| fn | contract |
|---|---|
| `(eligible ctx)` | Every gate whose precondition holds and whose budget is unspent, in priority order. Exposed so a test can assert what was passed over, not only what was chosen. |
| `(decide ctx)` | `nil`, or `{:gate :priority :message :prediction :tool :effect :passed-over}`. `:passed-over` names the gates that also held — what makes the tally's co-occurrence column readable, since a gate that only ever fires alone tells you something different from one perpetually outranked. |
| `(settle prediction {:keys [current-turn tools-called branch-before branch-after]})` | `:met`, `:unmet`, or `nil` for "still open". Deterministic. |
| `(force-tool-for decision)` / `(prefill-for decision)` | How a steer is made unavoidable (RFC-005). |
| `(settle-called-names)` / `settled-by-rule` | The gates settled by vocabulary and by rule; `gate-config-is-coherent` asserts every gate is covered by one or the other. |

### `samizdat.agent.phases`

`(table)` `(initial-phase)` `(phase p)` `(next-phase p)` `(withholds p)`
`(transitions)` `(finished-key-forms)` `(reload!)` `(gen)`.

## Protocol

```
loop/steer-step
  ├─ settle-predictions!            close what this turn resolved, FIRST —
  │                                 so a gate cannot be credited with an
  │                                 outcome that preceded it
  ├─ drain-directives!              a human directive outranks every machine gate
  ├─ arbiter/decide ctx             at most one
  ├─ apply-effects                  the gate's :effect → a state fn
  ├─ state/add-message              the one steer, stamped with its turn
  └─ record the open prediction     {:id :gate :prediction :window :turn}
```

Settling is journalled per firing, which is what makes the gate tally worth
reading: a gate whose predictions never settle is not steering anything, and
that is measurable rather than arguable.

## Invariants

| invariant | enforced by |
|---|---|
| Exactly one steer per boundary. | `decide` takes `first`; `agent-test/exactly-one-steer-per-boundary`. |
| Every gate declares a prediction. | `gate-config-is-coherent`. |
| Every gate can be settled. | `gate-config-is-coherent` walks `settle-called-names` ∪ `settled-by-rule` — a gate missing from `settle`'s `case` is not merely unhandled, its prediction can *never* come true (`provenance R3-7`). |
| `stuck-threshold` < `cull-threshold`. | `gate-config-is-coherent`. Both were 3, so the gate saying *change your approach* arrived on the turn the branch became eligible to be killed for not having changed it — and every move it predicts costs at least one turn. The README recorded the consequence (stuck fired and was obeyed zero times) without naming the cause. |
| No cost ceiling is capability-tunable. | `gate-config-is-coherent`. Scaling one up for a struggling run means spending more on the run already in trouble. |
| Every tool a vocabulary names is registered. | `agent-test` walks them against `run-tool`. |
| A reload re-compiles the table. | `util/generation-cache` against `gen`. |
| Every `:context-budget` key is read. | `agent-test/every-context-budget-key-is-actually-read`. |

## Known gaps

- ~~`:withholds` is empty, so the phase machinery is wired and inert.~~
  Partly closed: `:withholds` stays empty, but phases.edn now carries a
  populated `:refusals` table (per-branch conditions — the task-required rule
  among them), and `phase-refusal` consults both. A refusal is `:mechanics`
  with `:policy-refusal? true`, so it feeds the refusal counter, never the
  cull counter (blt.15).
- A prediction's `:window` is in turns, so a gate whose advice takes longer than
  its window to follow settles `:unmet` regardless of whether it worked.
- ~~`:transitions` carries one entry; the artifact-status trigger is
  unused.~~ Closed: both entries are live, and a typo'd effect name warns
  loudly instead of no-oping (blt.38).
- `settle` can return `:met-late` (acted on, one turn later than the window
  asked); it counts as met wherever the tally is read.
