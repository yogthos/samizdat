# RFCs

One specification per layer. Each states what the layer is *for*, where its
boundary is, the API it offers, the protocol by which it talks to its
neighbours, and the invariants it guarantees — with what enforces each one.

These are design documents, not a change log. Past defects live in
`docs/provenance.md`; open work lives in beads.

| RFC | Layer | Owns |
|---|---|---|
| [001](RFC-001-core-layer.md) | Base and userspace | The seam that makes the loop per-project and editable at runtime |
| [002](RFC-002-manifests-and-cells.md) | Cells and manifests | What a step is, what a workflow is, how an edit is validated |
| [003](RFC-003-security-model.md) | Security | What contains the model, and what deliberately does not |
| [004](RFC-004-tape-and-inference.md) | Tape and inference | The message array as a value; one step, four drivers |
| [005](RFC-005-provider-layer.md) | Provider | One retry ladder, one message shape, one fence parser |
| [006](RFC-006-beam-and-scheduling.md) | Beam | Many branches on one problem; who lives, who forks |
| [007](RFC-007-steer-system.md) | Steer | What the harness says to a branch, and whether it worked |
| [008](RFC-008-tools-and-tasks.md) | Tools | The agent's capability surface and the work board |
| [009](RFC-009-storage.md) | Storage | The durable record a resume rebuilds from |
| [010](RFC-010-adaptation.md) | Adaptation | How the harness gets better: two memories, two roles, one selection pressure |
| [011](RFC-011-board-workflow.md) | Board workflow | Owned tasks worked one at a time, a critic on each task's own diff |
| [012](RFC-012-implementer-and-supervisor.md) | Implementer and supervisor | Two streams: who owns the task, who owns the loop, and the one pattern every supervisory mechanism follows |

## How to read a layer boundary

Every RFC answers the same three questions in its **Scope** section, and the
answers are what keep the layers from bleeding:

- **What this layer decides.** If the answer includes anything about *what the
  harness should do*, the layer is userspace and its contents belong in
  `resources/`. If it only says *how to do a thing on request*, it is base.
- **What it must not know.** A provider adapter must not know what a turn is. A
  cell must not know which manifest wired it. The beam must not know what makes
  a branch good.
- **What it hands to whom.** Named data on a named seam, so a reader can follow
  one value end to end.

## Conventions

`[in]` and `[out]` mark the direction of a seam. A **contract** is a promise the
caller may rely on; an **invariant** is a property the layer guarantees and
names the mechanism that enforces it. Where nothing enforces an invariant, it
says so — an unenforced invariant is a convention, and calling it anything else
is how it stops being true.
