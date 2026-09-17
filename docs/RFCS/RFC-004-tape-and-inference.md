# RFC-004 — Tape and inference

**Status:** implemented. Ported from llm-repl (MIT, © 2026 Michael Whitford);
`src/samizdat/tape.clj` carries the notice.

## Purpose

A chat completion is a pure function of `messages[]`. This RFC specifies the
consequences samizdat takes from that: the message array as an immutable value,
one inference step with the model call injected, and four drivers that differ
only in how they apply it.

## Scope

**This layer decides** nothing. It advances, copies and probes a tape on
request.

**It must not know** what a branch is, what a run is, which provider is
configured, or whether a tool exists. `samizdat.tape` is pure; `samizdat.agent.infer`
touches the network only through a function it is handed.

**It hands** a tape to the provider layer (RFC-005) and outcomes back to
whatever asked. The decision to ask is a cell's (RFC-002).

Explicitly **out of scope: running tools.** See the safety note below.

## Model

```
tape = {:id       branch id, and the endpoint's prefix-cache key
        :messages [{:role :content …}]     the accumulator
        :turns    [turn records]           what compaction summarises with
        :prefill    request knob, one turn
        :force-tool request knob, one turn}
```

The array is a reduction accumulator, which makes three operations cheap:

| operation | is | cost |
|---|---|---|
| advance | append the reply | one completion |
| fork | copy, optionally truncated | free — structural sharing |
| probe | apply and discard | one completion, no state change |

One step, four drivers:

```
step        complete ⊕ tape → tape'                    the committed turn
bounce      apply to a FIXED tape, read, discard       one probe
trampoline  map bounce over N inputs, same prefix      N independent probes
ab          vary `complete`, hold the tape             N configs, one question
```

### Why a probe cannot run a tool

samizdat's full turn is *not* pure: `:tool/dispatch` shells out and writes
files. So the probe drivers stop at inference and parse, and that is
**structural rather than a rule** — `samizdat.agent.infer` has no tool seam in
it at all. A probe reports the call the model *would* issue; running it is the
committed path's business.

## API

### `samizdat.tape` — the value (pure)

| fn | contract |
|---|---|
| `(message role content [meta])` | A message. `meta` merges; `{:turn n}` is the provenance compaction reads. |
| `(append messages role content [meta])`, `(append-user …)`, `(append-assistant …)` | Return a vector, so a `nil` or seq tape normalises. |
| `(depth messages)` | Message count — the unit a fork depth is expressed in (2 per exchange). |
| `(truncate-at messages n)` | The tape as it was after its first `n` messages. `n` nil, negative or past the end returns it unchanged, so a stale depth cannot lengthen a tape. |
| `(window-index messages k)` | Index where the last-`k` verbatim window begins, counted in **assistant turns**; extended one earlier when the k-th-from-last reply is preceded by the user turn that prompted it. `nil` when nothing has aged out. |
| `(due-indices messages k [roles])` | Ascending indices that have aged out, whose role is rewritable, and which are neither compacted, declined nor `:pinned?`. **The caller owns its frame** — this knows nothing about which leading messages are load-bearing. |
| `(next-to-compact messages k [roles])` | First due index, or `nil`. |
| `(within-band? replacement original floor)` | `|new| ≤ max(|original|, floor)`, blank always outside. |
| `(compact-at messages i replacement [{:keys [floor roles]}])` | Replace in place. Three outcomes, all of which **change the array**: accept (`:compacted?`, original kept as `:original`), decline (`:declined?`, content untouched), no-op (absent, wrong role, already settled). |
| `(fold-split messages k)` | `{:head :tail}` for a boundary fold — `:tail` is the verbatim window. |
| `(fold-input head)` / `(fold-message session-id summary)` | The compactor's input; the single message carrying a folded session. |
| `(apply-fold messages k session-id summary)` | `{:messages :folded?}` under a **strictly-shorter** contract; rejects safely, leaving the tape unfolded. |

`default-floor` is 120 characters. `default-roles` is `#{"assistant"}`.

### `samizdat.agent.infer` — the step and the drivers

| fn | contract |
|---|---|
| `(of-branch branch)` | Project a branch onto its tape. Carries **exactly** what a call depends on and nothing else — that is what makes the step drivable from a literal. |
| `(into-branch branch tape)` | Write messages back and clear `:prefill`/`:force-tool`. Cleared here, not where they were set: one steer forecloses prose on **one** turn. |
| `(render tape)` | The wire messages — compaction applied, budget from `gates.edn :context-budget`. The tape's own array is untouched. |
| `(complete-fn ctx [{:keys [journal?]}])` | **The one effect**, as a value: `tape → {:ok true :response r}` or `{:ok false :error s}`. Retries once at a **doubled** budget when the reply hit the cap before emitting a call. `journal? false` for a probe — a retry inside a probe is not a turn the run took. |
| `(absorb tape response [turn])` | **Pure.** `{:tape :parsed :signals :said}`. Reattaches a prefilled opener; clears the knobs. |
| `(step complete tape)` | `{:tape :call :parsed :signals :said}`. Pure given `complete`. A provider failure returns `:call {:ok false}` with the tape **unchanged** — a failed call costs the turn, not the history. |
| `(bounce complete tape)` | `{:depth :parsed :said :call}` at the tape's original depth, or `{:depth :error}`. Never throws. |
| `(trampoline complete tape inputs)` | `{:depth :bounces [{:input …}]}`. Per-bounce errors as data: one failed probe does not sink the scan. Each input forks the same prefix, so they never accumulate into one another. |
| `(ab complete-for tape variants [input])` | `{:depth :variants {vk outcome}}`. `complete-for` is `variant → complete`, so each arm gets its own effect seam. **Sequential on purpose** — local endpoints contend on KV slots, and these results get compared. |
| `(log-probe! ctx branch-id kind {:keys [arms errors]})` | A journal receipt for spend that never reached a tape. |

## Protocol

```
cell (:llm/infer)
  └─ loop/call-model ctx branch
       └─ infer/complete-fn ctx        → complete
            └─ (complete (of-branch branch))
                 └─ infer/render tape          [out] wire messages
                      └─ llm/chat adapter cfg messages {…:cache-key id}   RFC-005

cell (:llm/parse)
  └─ loop/absorb-response branch response turn
       ├─ infer/absorb (of-branch branch) response turn   PURE: the tape half
       └─ state/record-mechanics signals                  the BRANCH half

cell (:probe/next-move)                                   RFC-002
  └─ infer/trampoline (complete-fn ctx {:journal? false}) tape candidates
```

The split in `absorb-response` is deliberate: the tape half is pure and a probe
drives it; the mechanics tally is bookkeeping about the *branch*, and a bounce
that parsed badly is not a branch that called badly.

## Invariants

| invariant | enforced by |
|---|---|
| **Nothing early in the message array changes between turns.** | The rule below. Three designs have had to obey it. |
| A probe leaves the tape at its original depth. | `bounce` discards `step`'s tape; `kanban-test`, `infer-test`. |
| A probe runs no tool. | Structural: no tool seam in the namespace. |
| A failed provider call does not advance the tape. | `step`'s `if-not (:ok call)`. |
| Compaction preserves roles, order and count. | `compact-at` replaces in place; `llm-test`. |
| A message is compacted at most once. | `:compacted?`/`:declined?` leave the due set permanently. |
| A pinned message is never unloaded. | `due-indices` skips `:pinned?`. |
| A fork's turn log covers only its inherited messages. | `state/fork-branch` filters by `:turn` stamp. |

### The caching rule

Prefix caching rests on the first invariant, and it is the one this layer exists
to protect. Violating it does not fail a test — it silently doubles cost.

| design | where it puts mutable content | cost |
|---|---|---|
| compaction, before | appended to the **problem** message | rewrote index 1 on every compaction: cache invalid from there, every turn |
| compaction, now | each aged-out message, in place, once | prefix before the newest rewrite is byte-stable |
| compaction frontier, before | advanced one exchange per turn | the verbatim window behind it re-prefilled every call: a quarter of every request on run 5e78b96a |
| compaction frontier, now | advances `:compaction-batch` exchanges at once | batch−1 turns in every batch send a byte-identical history |
| current task | appended once on claim, `:pinned?`, never rewritten | free — an append lands where the cache boundary already is |
| per-turn context block | appended | free |

A block held at a fixed early position and rewritten when its subject changes
invalidates every cached token behind it; one carrying anything per-turn means
the cache never warms at all.

**Measured, not assumed.** Every committed call fingerprints the wire
messages it sent (`infer/wire-fingerprint`, one `[hash chars]` per prepared
message) and compares them with the branch's previous render
(`infer/prefix-stats`). The turn row records how much of the request was
byte-identical from the front, whether only the tail moved or history behind
it was rewritten, and the tool a native `tool_choice` named (migration v30);
compaction notes carry the branch and turn they fired on. `journal/cache-misses`
groups the low-hit turns by those causes and `introspect` shows the grouping,
so a drop in the hit rate is attributable to a fold, to a forced call, or to
the provider, rather than argued about (karamazov-o4wm.1). A rewrite also
leaves a `:prefix-rewrite` note naming the message that changed, its role
and its size before and after, which is how the first live run of these
columns showed that every turn past `:keep-pairs` was a rewrite with no
compaction note beside it — the frontier moving, not a fold — and why the
frontier now moves in batches (karamazov-pdes). The operator sees
the same numbers: `GET /v1/runs/:id` carries the grouping beside the hit rate
and each branch's newest measured request (`journal/branch-context`, how full
that branch is now), and the TUI draws them per branch and per turn
(karamazov-pdes).

One deliberate, bounded exception: `strip-stale-ledgers` (RFC-005) blanks the
previous turn's settled-state block on the way to the wire, so the byte-stable
prefix ends one ledger position earlier than the newest message. The cost is
re-prefilling roughly one turn's tail per call; the alternative — every stale
ledger riding along — grows the context by a full ledger per turn.

### The opening block

A branch's first user turn is `prompts/problem.md`: the problem, and — when
`gates.edn :orient-inject` is on and the tree has them — a block saying
where the names the problem mentions are defined (`samizdat.agent.orient`,
karamazov-fp21.3). The driver computes it once per run from the tree at
start, stores the rendered text on the run row (`runs.opening_context`,
v31), journals what it found (`:orient-inject`), and hands it to every fresh
tape; a resume reads the stored text back rather than recomputing it, so
the prefix a rebuilt branch opens on is the one it ran under. A branch a
cell opens on its own problem (board, team, decompose) carries none. It is
an arena arm: `:enabled? false` is the control.

## Known gaps

**F1 — the boundary fold has no production caller.** `apply-fold`, `fold-split`,
`fold-input` and `fold-message` are exercised only by tests. They are advertised
to the agent in `manual.edn`, so an agent can legitimately call `apply-fold`
with a summary it writes itself — which is coherent, and is why they are kept.
But the harness does not fold at a session boundary, and the obvious place for
it (`artifacts/seed-from-run!`, which seeds a new run from an old one) carries
artifacts and not tapes. Wiring it needs a compactor that produces the summary;
samizdat's compactor is deterministic and produces digest lines, not prose.

The compaction *scheduler* upstream (`compact-next`, `needs-compaction?`,
`backlog-count`, `declined-count`) was **removed** for the same reason inverted:
it exists because llm-repl asks a model for one summary at a time, and a
deterministic compactor that rewrites every due message in one pass has nothing
to schedule.

**F2 — `:probe/ab-model` is wired but inert by default.** `gates.edn :probe
:variants` is empty, which is the honest default: the arm costs one completion
per variant and nothing consumes the comparison yet.
