---
name: mycelium
description: Use when you are about to edit a cell, write a manifest, or change how the loop works — how to structure cells and workflows (effects, edges, dispatch) and the safe reload_cells/manifest/introspect flow.
---

# Structuring the loop: cells and manifests (mycelium)

Load this before you edit a cell or a manifest. Your agentic loop is a
**mycelium workflow**: a graph of **cells** (small data transforms) wired by a
**manifest** (edges + dispatch). This is how to change it well.

## The shape

- A **cell** is a function `(fn [ctx data] -> data')` registered by id.
- **`ctx`** (mycelium calls it resources) is the run's infrastructure —
  `:conn :run-id :llm-adapter :llm-config :root :max-turns`. Read-only.
- **`data`** is the workflow state flowing through the graph — for the loop it
  carries `:branch` and `:turn` plus per-turn products (`:call :parsed :signals
  :said :result :tool :verdict`). Cells add to it.
- A **manifest** (EDN) names the cells, the edges between them, and the
  dispatch predicates that pick an edge. `resources/manifests/loop.edn` is the
  default loop; the workflows table can hold many named ones.

## Writing a cell

Cells live in `resources/cells/*.clj` and are loaded at runtime. Declare its
effects — `:pure true` for a data-only transform, or `:effects [...]` naming
what it touches (`:net :db :fs :proc`). The manifest's static checks use this.

```clojure
(cell/defcell :loop/route
  {:doc "Decide the turn's verdict from the branch." :pure true}
  (fn [ctx {:keys [branch turn] :as data}]
    (assoc data :verdict (if (state/active? branch) :continue :done))))

(cell/defcell :llm/infer
  {:doc "One model call." :effects [:net :db]}
  (fn [ctx {:keys [branch] :as data}]
    (assoc data :call (turn/call-model ctx branch))))
```

Rules that matter:

1. **Return the data map, adding keys** — `(assoc data :new k)`. Downstream
   cells see everything upstream cells added. (Generic mycelium lets you return
   only the new keys; samizdat's loop cells thread the whole map with `assoc`
   because the branch and turn ride along — follow the existing cells.)
2. **Never return nil.** A cell must return a map. If it has nothing to add,
   return `data` unchanged.
3. **Declare effects honestly.** An undeclared effect is a compile warning; a
   pure cell that actually calls the model or writes the db is a lie the soak
   cannot catch.
4. **Errors are data, not exceptions.** Set a key (`:call {:ok false ...}`) and
   let a dispatch predicate route it, the way `:parse` routes `:provider-error`.
5. **One cell, one transform.** A cell that does two things wants to be two
   cells wired in sequence.

## The manifest

```clojure
{:cells {:start :loop/assemble   ; node-name -> cell-id
         :route :loop/route
         :finish :loop/finish}
 :edges {:start :route           ; node -> next node
         :route {:continue :start ; or a dispatch map: decision -> node
                 :done     :finish}
         :finish :end}            ; :end terminates
 :dispatches {:route [[:continue (fn [d] (= :continue (:verdict d)))]
                      [:done     (fn [d] (= :done (:verdict d)))]]}
 :constraints [{:type :must-follow :if :dispatch :then :journal}]}
```

- **Edges** are either one successor node, or a map of `decision -> node`.
- **Dispatch predicates** only READ a key the cell already computed
  (`:verdict`, `:critic/decision`). Keep them trivial — the decision logic
  belongs in the cell, the predicate just reads it, so the routing stays
  visible in the manifest. Predicates are EDN forms evaluated at compile time.
- **Constraints** make an invariant a compile-time error (e.g. a dispatched
  tool call must always be journalled).
- A node named in `:cells` must be reachable and its dispatch must cover every
  edge, or the compile fails — before anything runs.

## A manifest can carry its own prompt

A manifest may declare `:prompt "name"` — a prompt resource
(`resources/prompts/name.md`) appended to the base system prompt for that
workflow, so the start injects the workflow's own instructions. This is how a
workflow gets a ROLE: `manifests/review.edn` is the ordinary loop plus
`:prompt "review"`, so the same cells do a code review instead of building a
feature. The base prompt (tools, REPL habit) stays; the manifest adds the task
framing on top. Select a workflow with config `:run :loop`.

## Hooks are cells

There is no separate plugin-hook system because you do not need one — every
place another harness exposes a hook is already a cell you can swap per
manifest:

- prepare-the-next-turn → the `:start` cell (`:loop/assemble`).
- decide-whether-to-stop → the `:route` cell (`:loop/route`) and its dispatch.
- inject-a-follow-up-message → any cell that adds to `:branch` messages (the
  critic does exactly this to send a branch back).

A "plugin" is a cell in `resources/cells/` (or a project's `.samizdat/cells/`)
plus a manifest that wires it in. Because you control the whole state graph,
hooking into any point is adding a node and an edge — nothing more.

## Changing it safely

You have three tools; use them in this order.

- **`reload_cells`** — after you EDIT a cell file in `resources/cells/`. It
  checkpoints, reloads, re-compiles the loop, and soaks (dry-runs) the change.
  Pass or fail, a bad edit is rolled back and the file restored. This is how a
  cell's behavior changes.
- **`manifest`** — to change the WIRING or add a whole alternative loop.
  `manifest patch {name, ops, rationale}` names the change — rename a node,
  add one and wire it, retarget an edge — and applies it to the stored text,
  so the rest of the file, comments included, stays as it was. `manifest save
  {name, edn}` stores a whole new manifest. Both compile the result the way
  the loader will before storing it, so a manifest that can't run can't be
  stored. `manifest refs {name, cell}` shows every place a node is named
  before you touch it; `manifest diff {name}` shows what the last version
  changed. Which manifest a run uses is config (`:run :loop`); patch the
  active one to tune it, or save a new name to propose an alternative.
- **`introspect`** — see the wiring and this run's health before and after.

## Adding a step to the loop (worked example)

The `critic` manifest adds one node to the default loop: a judge on the `:done`
path. The recipe generalizes — add a node, point an edge at it, dispatch out.

1. Write the cell (`resources/cells/critic.clj`, id `:gate/critic`) that sets a
   decision key: `(assoc data :critic/decision :ship)` or `:revise`.
2. Patch the manifest in one call — add the node with its out-edges and a
   dispatch on the key, and retarget the `:done` edge at it. The batch
   compiles as one, so the node is never stored unreachable:

   ```
   manifest patch {name "loop", rationale "judge a done before it ships",
     ops [{:op "add-cell" :name "critic" :id "gate/critic"
           :edges "{:ship :distil :revise :start}"
           :dispatches "[[:ship {:critic/decision :ship}] [:revise {:critic/decision :revise}]]"}
          {:op "set-edge" :from "route" :label "done" :to "critic"}]}
   ```

   Dispatch entries are patterns over the data map — `{:critic/decision
   :ship}` matches when that key has that value, `_` matches anything, first
   match wins. A `(fn [d] ...)` form is accepted where a pattern cannot say
   it.
3. It compiles or it is refused with the reason; `manifest diff {name "loop"}`
   shows what landed. Then run under it.

## Common mistakes

- Returning `nil` from a cell, or a value that is not a map → downstream break.
- Putting decision logic in a dispatch predicate instead of the cell → the
  routing stops being readable in the manifest.
- Editing a cell and not calling `reload_cells` → the running loop still uses
  the old version (the file change alone does nothing until you reload).
- A new node that nothing routes to, or whose dispatch misses an edge → the
  compile refuses it. Read the error; it names the gap.
