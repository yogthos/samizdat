# RFC-011 — The board workflow

**Status:** implemented.

## Purpose

Specifies the **board** workflow — the default implement strategy: a queue of
owned tasks, each claimed by exactly one implementor, worked to a terminal
verdict, and reviewed by a critic on the diff **that task** produced before it
may close. It replaces the parallel fan-out (`team`) as the default way several
agents share a feature.

The change of shape comes from the todomvc dogfood run (a3ba69bb). The fan-out
put four workers into one tree at once, on parts invented by a planner LLM call
before anyone had read the code — and what that bought was workers negotiating
over the same files (hardcoded test ports, half-written namespaces), a
planner's reasoning preamble mistaken for the task list (karamazov-6a3), and a
round-level review whose findings arrived as one blob attributed to nobody.
What demonstrably worked in the same run was the task board: exclusive claims,
pinned statements, honest release on failure. The board workflow makes that
the *workflow* rather than the bookkeeping beside one.

Three commitments, each answering a failure observed live:

- **Every change has one owner.** Work happens only under a claimed task, and
  a claim is exclusive per branch. Two agents can never both be "doing" a task,
  and a defect found later is attributable to the task that introduced it.
- **The split is made by whoever owns the work, after reading the code.** A
  task that is really several is split by its owner at claim time into child
  tasks (`prompts/task-claimed.md` asks the question; `task create` with
  `parentId` is the mechanism). There is no up-front planner call whose output
  nobody owns.
- **Review is per change, not per round.** When an owner finishes, a critic
  reads the diff between the task's own baseline and the tree now. Findings go
  back to *that* task; a pass closes *that* task.

## Scope

**This layer decides** which task is worked next, who owns it, whether its
change may close, and when the board run is finished. It is userspace —
`resources/manifests/board.edn` + `resources/cells/board.clj` — editable at
runtime behind the mutation protocol like every workflow (RFC-002).

**It must not know** how a turn works (it composes the `worker` manifest for
that), how the judge scores a diff (it calls `samizdat.agent.judge`), or how
claims are made exclusive (`samizdat.store.tasks`, RFC-008). It also must not
know whether it is the whole run or one stage of a larger one — that arrives
as data (`:board/nested?`).

**It hands to whom:** run as a loop (`:run :loop "board"`), it ends the run
through the same `:status`/`:answer` contract as every manifest. Nested in the
feature loop (`:feature/board`), it returns `:board/landed` / `:board/left`
summaries and a `:results` vector in the fan-out's vocabulary, so the feature
loop's review, verify, supervise, and route stages work unchanged whichever
strategy implemented the round.

## Model

```
plan ─> next ──────────────────────────> finish   (:empty — board clear, or only
         │ ^ ^                                      tasks this run gave up on, or
  (:task)│ │ │(:epic-review)                        the runaway guard)
         v │ └──────────> epic-review ──> next      (RFC epic, children all done:
      triage                                         validate whole diff vs the RFC;
      │ │ └─(:skip)──────────────────> work ─> review   pass closes it, a gap
      │ │                                ^        │      spawns a fix child)
      │ │(:plan|:rfc, mode-aware)        └────────┘ (:revise, same owner)
      v v
    design ─> dcritic ─(:ok)─> approve ─(:go)───────> work
                ^  │(:revise)     │(:decompose)
                └──┘              v
                              decompose ─(:decomposed)─> next   (children on the board)
                                        ─(:single)─────> work   (RFC named no items)
```

The pre-construction PLAN PHASE (`triage`→`design`→`dcritic`→`approve`, and for a
multi-part task `decompose` and later `epic-review`) sits between claiming a task
and building it — the one place the implementer had no gate (karamazov-vale,
karamazov-dq1r). See "The plan phase" below.

| node | cell | what it decides |
|---|---|---|
| `plan` | `:board/plan` | Ensure the board has work. An existing board is left alone (a revise round works what is open). Else `config :run :subtasks` if the caller supplied a split, else **one** task from the run's problem — never an invented split. On a nested revise round with an empty board, the review findings themselves become a task. |
| `next` | `:board/next` | Claim the next **workable** task — open, a **leaf** (no open children), oldest first — to a fresh branch `T<n>` (`T<n>v<round>` nested), pin its statement, and stamp `gitdiff/baseline` for it. Before that, if an RFC epic's children have all landed it routes to `epic-review` (`:epic-review`) — the whole change is validated before anything else. `:empty` when nothing is workable *by this run*: tasks it already gave up on are excluded, and the `:board-max-tasks` runaway guard counts. Closes any non-RFC parent whose children are all done (an RFC epic closes only through `epic-review`). |
| `triage` | `:board/triage` | Decide how the task enters construction (karamazov-vale, karamazov-dq1r). Deterministic, no model call: `:skip` (phase off, or short and no list) straight to `work`; `:rfc` (multi-part — an explicit list, or over `:rfc-min-words`, and not already an RFC child) to the RFC tier; `:plan` (ordinary) to a lightweight plan. |
| `design` | `:board/design` | A bounded owner sub-loop (`:board-design-turns`) that reads code and declares a plan without building. Mode-aware: under the RFC brief it declares a full RFC (a mermaid call-graph, work items, acceptance criteria) which becomes the plan text; under the lightweight brief, a Goal/Files/Tests. |
| `dcritic` | `:board/design-review` | The PLAN critic — `judge/review-plan`, the same two-pass verify the diff critic uses, on the plan against the requirement. `:ok` to `approve`; `:revise` back to `design`, bounded by `:max-design-attempts`, fail-open. A blank plan is sent back once, then fails open. |
| `approve` | `:board/approve` | The gate before construction. Headless the critic already decided, so this persists the plan as the task's contract (`tasks.plan`) and passes through; attended (`:approval :mode :block`) it also asks the person. `:go` to `work` for a lightweight plan, `:decompose` for an RFC (stamped `plan_kind` "rfc"), `:rework` back to `design`. |
| `decompose` | `:board/decompose` | Break an approved RFC into the child tasks its "## Work items" named, under the epic, and release the epic's claim so the board works the children (they skip their own RFC). `:decomposed` to `next`; `:single` to `work` when the RFC named no items. Records the epic's pre-construction baseline for `epic-review`. |
| `epic-review` | `:board/epic-review` | The END-OF-PHASE critic: once an RFC epic's children have all landed, validate the whole diff (from the epic baseline) against the RFC — `judge/review` with the RFC as the requirement, plus the code-quality metrics over everything touched. `:pass` closes the epic; a blocking gap spawns one fix child under it, bounded by `:board-review-attempts` and fail-open. This is what makes the RFC the acceptance contract. |
| `work` | `:board/work` | Run the implementor sub-loop (`worker` manifest, `:implementor` role model) on the claimed task until a terminal verdict. A re-attempt runs on `<bid>r<attempt>` with the critic's findings appended to the task's problem. |
| `review` | `:board/review` | The critic on **this task's diff**: deterministic checks first, then the judge (`judge/critic-prompt` over the diff since `:board/baseline`). `:pass` closes the task; `:revise` sends it back to the same owner with the findings; `:give-up` (owner never landed, or `:board-review-attempts` spent) **releases** the task back to the board open. Fail-open: a judge that errors or throws ships — a broken gate must not end the run. |
| `finish` | `:board/finish` | Nested (the default — the board as the feature loop's implement stage) it only summarizes: what landed and what is left goes UP, to the supervisor, whose job is to figure out why a task did not land and adjust the loop — another round with the findings, a strategy SWITCH, an `EXTEND: <n>` budget raise. Standalone, `:completed` only when something landed **and** nothing is left; anything else ends `:abandoned`, honestly — but a standalone board has no supervisor, which is exactly why the feature loop is the default. |

### The plan phase and the RFC tier

Every gate the implementer had fired *after* construction. The plan phase puts a
critic at the elaboration boundary, and triage keeps it off work that does not
need it (karamazov-vale). Triage is three-way (karamazov-dq1r): a trivial task
skips it; an ordinary single change gets a lightweight Goal/Files/Tests plan; a
multi-part task — an explicit list, or over `:rfc-min-words` — gets a full **RFC**
instead.

The RFC is a design document with a mermaid call-graph, a work-item breakdown and
acceptance criteria (prompts/rfc-brief.md). It is persisted on the epic
(`tasks.plan`, `plan_kind` "rfc"), **decomposed** into child tasks the board works
in turn, and — the point — validated at the end: when the children have all
landed, `epic-review` judges the whole diff against the RFC before the epic may
close. `tasks.plan` was write-only before this; now construction reads it (a
child is handed its epic's RFC to build within) and `epic-review` reads it as the
contract. A child derived from an RFC does not open its own RFC (`rfc-child?`), so
the recursion terminates.

### The code-quality gate

`:board/review` and `:feature/critique` — and `epic-review` — measure the changed
code with `samizdat.metrics` (cyclomatic complexity, erosion, clone-only
verbosity; SlopCodeBench) and merge the result into the critic's findings. A
breach past the advisory ceiling rides along as a `[medium]`; past the block
ceiling it is a `[high]` and refuses the ship, like any blocking finding. The
limits are gates.edn `:code-quality`; the prose is prompts/metrics-findings.md.

### The data seam

All board state travels in the data map under `:board/*` — no atoms, no
side-channel:

| key | meaning |
|---|---|
| `:board/task` `:board/branch-id` `:board/problem` | the claimed task and its owner branch |
| `:board/baseline` | the git ref stamped at claim; what review diffs against |
| `:board/attempts` `:board/findings` | the review bounce count and its findings for the re-attempt |
| `:board/outcome` `:board/answer` | the owner's terminal verdict and final answer |
| `:board/decision` | the review's `:pass` / `:revise` / `:give-up` |
| `:board/worked` | tasks finished with (not attempts) — what the runaway guard counts |
| `:board/landed` `:board/left` | `{:task :answer}` per closed task / per task left open |
| `:board/nested?` `:board/round` `:board/guidance` | set by `:feature/board`: summarize instead of finishing, round-scope the branch ids, and seed the findings task |

### Composition with the feature loop

The feature loop's implement-strategy ladder is `["board" "team" "decompose"]`
(`cells/feature.clj`). The board is the default; the supervisor switches with
`SWITCH: board|team|decompose`, and a strategy that keeps failing its soft-cap
rounds auto-advances. Only the fan-out routes through the planner
(`:team/plan`) — the board's split belongs to task owners and decompose splits
on stuck, so a planner call in front of them was a provider call per round
producing a split both ignored.

Nested rounds are additive: tasks closed in round *n* stay closed, so a revise
round works what is left rather than the feature again.

### The behavior-tree variant (`board-bt.edn`)

Same cells, same contract, one structural difference: every action returns to
a `:board/sense` node that re-derives the loop's position from the blackboard
and the tasks table each tick, querying preconditions in reverse — unjudged
outcome → review; revise verdict or fresh claim → work; refused claim → done;
workable task → claim (Kelley arXiv 2404.07439 Appendix A.2, the implicit
sequence). In a static world the two manifests are identical; the difference
appears when the world shifts under the loop (a stale claim released, a task
closed elsewhere, a crash resumed), where the plain machine continues from
where it *thinks* it is and this one from where the board actually stands.
The review-before-done ordering that board.edn enforces as an edge lives here
in `:board/sense`'s cond order — enforcement moved from structure to data —
and the root done-check deliberately sits *below* review, because "nothing
closes unreviewed" outranks reactivity. Selected per-run with
`:run :loop "board-bt"` or, as the feature loop's implement stage, with
`:run :board-manifest "board-bt"` (`HARNESS_BOARD_MANIFEST`). An A/B variant
for the loop-guard epic (karamazov-fut): prefer `board` until it has been
measured against it.

## Policy (gates.edn)

| key | what it bounds |
|---|---|
| `:board-max-tasks` | how many tasks one run works before stopping to report — the structural bound on a board whose owners may keep splitting. A runaway guard, not a plan. |
| `:board-owner-turns` | how many turns one owner may spend on one task before it comes back `:exhausted`, which the review reads as give-up: the task stays open, unowned, and the board moves on. Never wider than the run's own `:max-turns`. Added after run e1b765e7 (2026-09-07), where an owner's cap was the run's and one part ran past 140 turns while the last part never started (karamazov-ghti). |
| `:board-review-attempts` | how many times the critic may bounce one task before the loop leaves it open and moves on. |

## Invariants

- **No unreviewed close.** Every path from `work` leads through `review`, and
  only `review`'s `:pass` arm calls `tasks/close!`. Enforced: the manifest's
  `:must-follow :work :review` invariant is compile-checked (RFC-002), and no
  other board cell closes a task.
- **No unowned work.** `work` runs only on a task `next` just claimed, and a
  claim is exclusive per branch. Enforced: the `:must-precede :next :work`
  manifest invariant, plus the tasks store's claim guard (migration v12,
  RFC-008).
- **Review reads the task's own change.** The baseline is stamped at claim
  time, before the owner's first turn, and review diffs against that exact
  ref. Enforced by construction — `:board/baseline` is written by `next` and
  read by `review`; nothing rewrites it in between.
- **A task nobody landed stays open — as the supervisor's input, not as an
  ending.** `:give-up` releases rather than closes, and `finish` refuses
  `:completed` while anything is left. Open tasks are not a verdict: in the
  feature loop they are what the next round works and what the supervisor
  diagnoses and adjusts around (SWITCH, EXTEND, re-split) — the run keeps
  solving until the supervisor explicitly STOPs or the opt-in runaway guard
  trips. Abandonment is the exceptional ending, reached only when adjustment
  itself has been tried and failed. Enforced in the two cells; pinned by
  `board-test/a-task-its-owner-could-not-finish-does-not-close`.
- **Termination.** Each `next` either consumes a workable task this run has
  not given up on or ends the loop, `:board/worked` is monotonic, and the
  runaway guard caps it. Not separately enforced beyond the cells' logic;
  pinned by the board tests running under a finite mock.

## Known gaps

- Tasks are worked **sequentially** within one board run. The claim mechanics
  would support several owner branches at once (that is exactly what the
  fan-out does over the same store); the board deliberately starts serial
  because simultaneity in one tree was the cost the dogfood run paid. If
  parallel boards return, they should come back as N single-owner board runs
  over one shared board, not as N owners inside one run.
- The critic reviews a diff, not a rebuilt tree: it does not run the tests
  (that is the feature loop's verify gate, GATE 2). A standalone board run
  with no `:verify-cmd` therefore closes tasks on review alone — same standing
  as the `critic` manifest.
