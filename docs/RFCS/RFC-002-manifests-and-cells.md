# RFC-002 — Cells and manifests

**Status:** implemented.

## Purpose

Specifies the unit of work (a cell), the thing that wires units into a loop (a
manifest), and the protocol by which an edit to either is validated before it
becomes live.

## Scope

**This layer decides everything about what the harness does** — which step runs
next, which branch lives, when a run ends. That is why it is userspace (RFC-001)
and not `src/`.

**A cell must not know** which manifest wired it, what ran before it, or what
will run after. It transforms the data map and declares what it touched.

**A manifest must not contain logic.** Dispatch predicates read explicit keys
that a cell computed; the moment a predicate computes something, the routing has
stopped being visible in the manifest.

## Model

### The cell contract

```clojure
(cell/defcell :ns/name
  {:doc     "what this step does"
   :pure    true            ; XOR
   :effects [:net :db :fs :proc]}
  (fn [ctx data] data'))
```

| | |
|---|---|
| `ctx` | run-scoped resources: `:conn :run-id :config :llm-adapter :llm-config :root :max-turns :abort`. Never mutated. |
| `data` | the workflow's value, threaded node to node. A cell returns it changed. |
| `:pure`/`:effects` | **load-bearing, not documentation.** The mutation soak stubs effectful cells to identity so a dry-run does no IO; a cell declaring neither is rejected, because the safety the marks exist for would be void. |

Effect vocabulary: `:net` (a provider or network call), `:db`, `:fs`, `:proc`
(spawns a process).

Naming is load-bearing: `:llm/*`, `:tool/*`, `:journal/*`, `:gate/*` are what
glob-scoped interceptors match on.

### The manifest schema

```clojure
{:description  "what this workflow is for — the supervisor reads this to choose"
 :cells        {node-kw cell-id}
 :edges        {node-kw next-node          ; unconditional
                node-kw {branch-kw node}}  ; dispatched
 :dispatches   {node-kw [[branch-kw pattern] …]}         ; ordered, first wins
                                           ; [branch-kw pattern guard], or a (fn [data] pred) form
 :constraints  [{:type :must-follow :if node :then node}]
 :subworkflows {cell-id manifest-name}     ; optional: a nested manifest as one node
 :prompt       "name"}                     ; optional: prompt appended to the base
```

`:start` is the entry node and `:end` terminates. A dispatch entry is a
**pattern** over the data map (`samizdat.symbolic`): a map is an open-world
subset — `{:verdict :done}` matches any data map carrying that key with that
value, and a named key must be present — `_` matches anything, `?x` binds, and
a third element is a guard from `samizdat.symbolic/guard-catalog`. Entries are
tried in order and the first match wins. Because the table is data, compile
checks it: a branch an earlier pattern makes unreachable is refused, and two
branches that overlap with neither more specific are reported by `manifest
save` and `manifest show` as order-dependent. A `(fn [data] pred)` form is
still accepted where a pattern cannot say it; it is evaluated at **compile**
time and is opaque to the analysis.

### The two levels

```
manifests/beam.edn         the ROUND    advance · score · cull · settle ·
                                        repopulate · spawn · tick · back edge
  └─ manifests/loop.edn    the TURN     assemble · infer · parse · dispatch ·
     (per-turn slice)                   journal · settle · arbiter · route
```

`turn-manifest` **derives** the per-turn slice from a whole-run manifest by
redirecting every edge that would return to `:start` or reach a `:loop/finish`
node into `:end`. One file therefore serves both drivers and an edit reaches
both — rather than two files that must be kept in agreement, which is how the
two drivers drifted apart before karamazov-ioo.20.

`iterating?` classifies a manifest: a pass is one **turn** the beam may schedule
against siblings iff the slice contains `:llm/infer` **and** an edge returns to
`:start`. Both conditions are needed — `orchestrator` returns to its start node,
but that node is an entire nested run, so treating it as a turn would put a
multi-minute job under the per-turn deadline and run five at once.

## API

### `samizdat.manifests` and `samizdat.workflow`

Reading, validating and compiling a manifest lives in **`samizdat.manifests`**
(mechanism only: mycelium + the cell loader + the userspace seam, no driver),
so the self-modification tools validate an edit EXACTLY the way the loader
will — they cannot require `samizdat.workflow`, which reaches the tool
dispatcher through the branch loop, and each once re-implemented a slice of
the pipeline that drifted (2026-08 audit, blt.2/.3/.4/.6). `samizdat.workflow`
delegates the moved names, so every caller below keeps working; it retains
the drivers (`load-loop!`, `compile-turn-loop`, `run-turn`, `run!`) and the
role plumbing. Two functions worth knowing apart:

| fn | contract |
|---|---|
| `(manifests/compile-loop definition)` | The loader's full pipeline: registry reload, sub-workflow registration through userspace, ctx-key requires, derived constraints, mycelium pre-compile. |
| `(manifests/compile-definition definition)` | The same WITHOUT the registry reload — what the mutation protocol validates through, because reloading would replace the candidate it just installed. |

### `samizdat.workflow`

| fn | contract |
|---|---|
| `(read-definition edn-text)` | Parse. Dispatch predicates stay as forms. |
| `(compile-loop definition)` | Load cells, register sub-workflows, then mycelium's full static check: structure, dispatch coverage, reachability, constraints. **Throws** on any violation. Logs and returns compile warnings. |
| `(load-loop! conn [name])` | Seed the factory template, load the project's latest version, compile. `{:name :version :definition :compiled}`. |
| `(turn-manifest definition)` | The per-turn slice. |
| `(compile-turn-loop conn name)` | Both forms plus `:iterating?`. What the beam drives. |
| `(compiled-manifest name)` | A factory manifest compiled fresh — the seam a role's sub-loop uses. |
| `(run-turn ctx branch turn [manifest-name])` | **The one composition of a turn.** Compiles the slice and runs one branch through it, so it cannot drift from production. |
| `(run! {:keys [conn config llm-adapter llm-config problem max-turns]})` | One branch to completion under the stored loop. |
| `(catalog conn)` / `(render-catalog conn)` | Every selectable workflow with its `:description` — the menu the supervisor chooses from. |
| `(iterating? definition)`, `(finish-nodes …)`, `(start-node)` | Classification. |
| `(role-ctx ctx role)` | ctx with the adapter and model swapped to `config :run :role-models`. |
| `(workflow-prompt definition)` / `(prompt-text name)` | A manifest's prompt suffix. |

### `samizdat.cells`

| fn | contract |
|---|---|
| `(load-cells!)` | **The project's** cells: seed templates into the store, read back, `load-string` into the live image. `.samizdat/cells` files seed alongside. Unbound, reads the templates. |
| `(load-cells! dirs)` | A literal source scan of `dirs` plus shipped resources, **no store**. The seam a test loading a temp directory needs; deliberately not the production path. |
| `(loaded)` | `{cell-id {:source name}}`. |
| `(loaded-file-content)` | Last-good on-disk content, for the file-based rollback. |
| `shipped-cells` | Template resource names, **enumerated not globbed** — a classpath has no directory listing, so a glob finds nothing inside a built binary. Pinned against the directory by a test. |

Loading is **transactional**: on any error the registry is restored and the error
rethrown, so a broken edit never half-loads.

### `samizdat.mutation`

| fn | contract |
|---|---|
| `(propose-cell! {:keys [name body loop-def extra-defs soak-input compile-fn conn run-id]})` | Validate a candidate and commit it as a new version **only if it survives**. `:loop-def` is the ACTIVE stored loop (soaked); `:extra-defs` is every other shipped+stored manifest (compiled, not soaked) — a cell wired only into the beam or a team loop is invisible to the active loop, and validating one definition let an edit that broke every other workflow commit (blt.2). `{:status :committed :version n}`, `{:status :live-unsaved :reason :unbound}` when no store is bound, or `{:status :rolled-back :reason s}`. |
| `(apply-cell-edit! {:keys [dirs loop-def soak-input compile-fn conn run-id]})` | The legacy file-based protocol: reload, validate, soak, commit or restore the file. Refuses a store-mode image (`:reason :store-mode-image`) — its checkpoint is file paths, and the production loader's is store names (blt.7). |

## Protocol

### The mutation protocol

```
propose → load-string the candidate into the live image
        → validate: does the loop still compile?
        → soak: dry-run with effectful cells stubbed to identity, under a timeout
        → commit as a new version   |   reject, registry restored
```

**Nothing is written until the candidate survives**, so a bad edit never enters
the project's version history. The *attempt* is journalled with its reason — the
store holds versions that were live, the journal holds every attempt and its
verdict.

The soak is bounded (10s) so a cell that loops cannot hang the protocol, and the
registry is restored afterwards so the stubs never leak.

### Compilation

```
workflow/compile-loop
  ├─ cells/load-cells!              every compile, because the registry is
  │                                 global mutable state and a non-empty
  │                                 registry is not proof THIS loop's cells
  │                                 are present
  ├─ register-subworkflows!          nested manifests become workflow-cells
  └─ myc/pre-compile                 structure · dispatch coverage ·
                                     reachability · constraints
```

## Invariants

| invariant | enforced by |
|---|---|
| A manifest that cannot run cannot be saved. | `manifest save` compiles before storing. |
| A cell edit that breaks the loop never goes live. | `propose-cell!`'s validate + soak. |
| A cell that declares no effects is rejected. | `mutation/validate` reads mycelium's `:undeclared-effects` warning. |
| Declared constraints are compile-time errors. | mycelium `:constraints`; `beam-test` asserts a violating edit is refused. |
| Every registered cell is reachable from some manifest. | `beam-test/every-shipped-cell-is-reachable-from-some-manifest`. |
| Every shipped manifest compiles and is in the catalogue. | `beam-test/every-shipped-manifest-compiles-and-is-selectable`. |
| The shipped cell list matches the directory. | `cells-test/shipped-cells-match-what-ships`. |
| A shipped cell's requires are reachable without `load-string`. | `cells-test`, against `samizdat.cell-prelude`. |

### Declared constraints today

| manifest | constraint | protects |
|---|---|---|
| `loop` | `dispatch → journal` | a dispatched call is always recorded |
| `loop` | `journal → arbiter` | a recorded turn always faces a gate |
| `loop` | `settle → arbiter` | a gate is credited only with outcomes after it fired |
| `beam` | `score → cull` | retention reads fresh critic scores, not last round's |
| `beam` | `settle → repopulate` | a branch is written down before its slot is refilled |

## Known gaps

- **Every declared invariant is enforced.** Every ordering rule a manifest
  claims is declared in its `:invariants`, each saying what it `:protects`
  and whether it is `:enforced`; the enforced ones are DERIVED into the
  `:constraints` the compiler checks (`must-follow`, `must-precede`), and
  `beam_test` pins that every enforced invariant reaches the compiler and
  every unenforced one says why. The last unenforced one, settle before fire,
  ordered two steps inside `:gate/arbiter` where a path-based checker had
  nothing to look at; the cell is now `:gate/settle` then `:gate/arbiter`,
  the arbiter requires the `:settled` key only settle writes, and every
  turn-shaped manifest declares `:must-precede :settle :before :arbiter`
  (karamazov-aqsr.2). A stored manifest that wires the arbiter without the
  settle node is refused by the schema chain at its next compile, naming the
  path — the same refusal a manifest routing around `:loop/assemble` gets.
- A cell's `ctx` keys are checked at compile against `manifests/ctx-keys`
  (`check-requires!`), and `manifests/preconditions` reports per node what it
  requires from the driver and from upstream and what holds on every path in;
  a schema-chain refusal names the path that reaches the node lacking the key.
  What is not checked is the driver's side at run time, beyond `beam_test`
  asserting that the production ctx carries every declared key.
