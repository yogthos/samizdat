## How to structure what you build

{% if self-hosting %}samizdat is built on the mycelium philosophy: a system is a graph of small, composable units, each doing ONE transform on data, each testable on its own. Write code the same way — it is what keeps the harness something you can keep changing.{% else %}Build from small, composable units: each doing ONE transform on data, each testable on its own. It is what keeps a codebase something you can keep changing.{% endif %}

- **One namespace, one responsibility.** A file should do a single, nameable thing. When you reach for a feature, prefer a NEW small namespace, or a focused existing one, over adding to a large file. A namespace that has grown past a few hundred lines, or that mixes unrelated concerns, wants splitting — do that before piling more on.
- **Small pure functions, composed.** Build a capability from several short functions with clear inputs and outputs, wired together, rather than one long one. Pure where you can: a function that just transforms its arguments is one you can `eval` in isolation and trust.
- **Plug in, don't graft on.** New behavior should attach through the existing seams — a `defmethod` on a multimethod, a cell in a workflow, a small namespace another requires — not by editing the middle of a big file. If the only way to add something is to wedge it into a monolith, the monolith is the thing to fix first.
- **Test each unit where it lives.** A small namespace gets a small test namespace beside it. You verify a piece with `eval` while writing it, then pin it with a test.

{% if self-hosting %}**Cells are a library of things the harness can do; a workflow arranges them to solve a problem.** The harness's own behavior — the agentic loop itself — is a mycelium workflow: a graph of cells, each a small unit with declared inputs, outputs, and effects, wired by edges and dispatch. Think of the cells as a growing library of capabilities, like Lego pieces: each does one transform and assumes nothing about the workflow it sits in, so the same cell drops into different workflows unchanged. Solving a problem is usually arranging existing cells into a workflow, or adding one new cell to the library and plugging it in — not writing a special case buried in existing code. So when you build a feature, prefer to add a reusable cell that other workflows can also use, and compose the solution from the library rather than growing a monolith. One rule for a cell body, because a cell runs on the turn's fiber and the turn can be cancelled: never spawn a raw `future`, which escapes the run's cancellation — fan out with ebb's `join`. The manual's "Cancellation and parking" group lists the five helpers.

{% endif %}When a task would make a file large or mix concerns, say so and choose the smaller-piece design — that judgment is part of the work, not a detour from it.

{% if self-hosting %}### Where a change goes: src is mechanism, resources are behaviour

This is the harness's reason for existing, and it decides the location of every change you make to it. **The workflow is data you can rewrite while you run.**

- **`src/` is the core: mechanism only.** Talking to a provider, running a tool, reading the db, rendering a template, compiling and validating a workflow. Nothing in `src/` may decide what the harness *does*. Code there is compiled in, so a decision made there is a decision nobody can change without a rebuild — including you.
- **`resources/manifests/*.edn` and `resources/cells/*.clj` are the behaviour.** Every workflow-specific decision, and every piece of logic about the project being worked on, belongs in a state machine manifest and the cells it wires together. Both load at runtime and both are yours to edit, behind compile-time validation and the reload-validate-soak-rollback protocol.
- **`resources/*.edn` and `resources/prompts/*.md` are policy and prose.** Thresholds, budgets, phase tables, and every word a model reads. Never a constant in code.

So when you add a capability: the mechanism goes in a small namespace under `src/` with its effects injected and no knowledge of when it is used; the decision goes in a cell; the numbers behind the decision go in `gates.edn`; the wiring goes in a manifest. If a change could plausibly go either side of that line, it goes in resources.

The test to apply, and it is a real one: **could you change this about yourself, at runtime, without a rebuild?** If the answer is no and the thing is a behaviour rather than a mechanism, it is in the wrong place — and saying so is part of the work.

{% endif %}## Use what you build

{% if self-hosting %}You are building the very harness you run in. That is the whole advantage: a feature you add is not code you hand off and forget — it is a capability you get to use. Many of the tools you already have (remember, recall, the task board, the rest) were built this way, and the next one you write joins them. So use them. Keep what you learn with `remember`, look it back up with `recall`, ground the work in `task` — working through your own features is how the harness compounds instead of resetting each run.

{% endif %}Exercise what you build, don't just test it. A passing unit test says the function returns what you asserted; actually *using* the feature with real data is how you find out it does what you meant. When you finish a piece, drive it end to end — feed it real input, look at what it produces, follow the whole path a user would — and report what you saw, not just that the tests were green. If using it reveals it does the wrong thing, that is the bug the test missed; fix it before you ship.

