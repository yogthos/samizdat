# RFC-008 — Tools and the task board

**Status:** implemented.

## Purpose

Specifies the agent's capability surface: how a tool is defined, what it must
return, how the return is read by the guards, and how work is grounded in a
durable board rather than in the conversation.

## Scope

**This layer decides** how to do a requested thing, and reports what happened.

**It must not decide** whether the thing was worth doing, or what to do next.
A tool returns a category; the *meaning* of that category to a cull rule is
RFC-007's business.

**It hands** a result envelope to the turn, and rows to the store.

## Model

### The tool contract

```clojure
(defmethod base/run-tool "name" [{:keys [branch conn run-id args] :as ctx}]
  {:result    "what the model sees"          ; required
   :category  :success|:failure|:neutral|:mechanics
   :progress? boolean
   :branch    updated-branch                 ; required
   :artifact  {…}      ; optional, recorded to artifacts
   :failure   {…}      ; optional, recorded to the shared failure log
   :done?     boolean}) ; optional, ends the run
```

A multimethod rather than a `case`, because that is what lets a tool be
redefined against a running process and picked up on the next branch turn.

**`:category` and `:progress?` are separate on purpose.** A call can succeed and
advance nothing, and a model making varied, well-formed, useless calls trips no
error-keyed guard while burning the whole run.

**The four categories, and why `:mechanics` exists:**

| category | means | read by |
|---|---|---|
| `:success` | the thing worked | resets the failure counter |
| `:failure` | the branch's line of inquiry was tested and found wanting | the cull rule |
| `:neutral` | bookkeeping; nothing was established | nothing |
| `:mechanics` | the call was made *wrong* — malformed args, unknown tool, a policy refusal | its own looser counter |

`:mechanics` is the distinction that took five attempts to get right. A
malformed call produced no claim and tested nothing, so there is no evidence in
it about the branch's line of inquiry — charging it to the counter that decides
whether a branch lives is the `vf-jki` mistake, and it appeared in five separate
places (fences, `expectedVerdict`, `proof_start`, engine outages, argument
shape). It is still *counted*, because a branch looping on malformed calls is
real spend; it just stops being read as substance.

### Helpers

| fn | produces |
|---|---|
| `(ok branch result & extra)` | `:neutral`, `:progress? false` |
| `(fail branch result & extra)` | `:failure` |
| `(malformed branch result)` | `:mechanics` |
| `(unavailable branch capability e)` | `ok` — an external capability that could not be reached is not the branch's fault |
| `(missing ctx & ks)` | The complaint for absent required args, **with a call skeleton**. A bare list of names taught nothing: gen-20 B1 called `proof_start` without its arguments five times, three producing the byte-identical message, and was culled for it. |
| `(phase-refusal ctx)` | A refusal carrying `:policy-refusal? true`, so a cull record can tell a declined call from a malformed fence |
| `(arg ctx k)` / `(tool-names)` | Argument access; the registered surface |

### The task board

```
tasks(id, title, body, type, status, priority, parent_id,
      run_id, branch_id, contract, tests, created_at, updated_at, closed_at)
```

| field | meaning |
|---|---|
| `status` | `open` `in_progress` `blocked` `done` `cancelled`, with synonyms normalised — models say todo/wip/completed and the board must not fork into synonym lanes |
| `priority` | `high` `normal` `low`, accepting `p0`–`p4` and bare `0`–`4` |
| `parent_id` | an epic is a task with children; the hierarchy is as deep as the model wants |
| `run_id` | which board. `NULL` is the passive backlog |
| `branch_id` | **who holds it.** `NULL` means claimable |
| `contract` / `tests` | the delegation spec: what the work must satisfy, and what defines delivery |

**The delegation spec is CODE, and the harness checks it.** A parent that
splits a task writes the STUBS each piece must fill — the signature with a
docstring saying what it owes, and a body that only throws — writes its own
composition against them, and sketches the tests that pin each piece. The
`split` tool then reads the tree and declines the delegation unless every named
stub is there, is still hollow, and is claimed by exactly one piece; on
acceptance `contract` is the stub source itself rather than a restatement of
the ask. This is what makes the two layers independent: the parent reasons
about the API and how the pieces compose, the child reasons about the
implementation behind one signature, and the only thing they share is the
boundary. It is also what makes both halves checkable rather than judged —
`samizdat.agent.stubs/filled?` answers whether a child delivered.

The suite is **red** from the split until the last piece lands. That is
tests-first at the scale of a delegation, not a broken tree: a child's green is
its OWN tests, and the parent's assembly is green when its tests and every
child's tests pass together. The parent may adjust what the pieces delivered at
assembly — seeing them compose is the first time anyone can tell whether the
boundary was right (`prompts/assembly.md`).

**The parent PARKS, and that is a state rather than an absence.** A successful
split ends the branch's turn-taking: the branch goes `:parked`
(`state/parked?`) and the row it holds goes `blocked`, still naming it. Three
things follow from the row, and each of them used to be wrong. The claim is not
up for grabs, so nothing hands the parent's task to another owner while the
work it asked for is being built. The parent takes no more turns, so it cannot
spend them implementing the very stubs it just delegated — a part whose
contract is already met would ship having done nothing, which is the same state
`verify` refuses at split time and which was one turn away and unchecked
afterwards. And the parent is not finished, so nothing closes its row when the
last piece lands: it owes the composition.

Waking it belongs to whoever composed the loop, because only that layer knows
when the pieces are in. `cells/decompose` resumes the parked branch itself —
same id, same tape, same turn counter, with the assembly framing and what each
piece delivered appended as a user turn, so the agent that drew the boundary is
the agent that composes it. The board unblocks the row when the last piece
closes and hands it out again as ordinary workable work, which is weaker: it
opens a fresh owner with the contract pinned rather than the one that designed
the split. `stubs` on the children is what tells a delegating parent from a
grouping epic, which the board closes on its parts as before.

**The holder is a branch, not a run.** On a team workflow the competing
implementors are branches of one run, so a per-run claim is exclusive between
runs and a no-op within one — which is precisely the case the board exists to
arbitrate.

### One task at a time

A branch carries `:task {:id :title}`. Claiming a second is refused, naming
`task switch {id, reason}` as the way out; a switch **releases** the task being
set down, so it returns to the board rather than staying attributed to a branch
that has stopped — the worst state for a shared board, because it is
indistinguishable from work that is progressing.

## API

### `samizdat.store.tasks`

| fn | contract |
|---|---|
| `(create! conn {:keys [title body type status priority parent-id run-id contract tests]})` | Returns the id (`sz-` + 6 hex). Throws on a blank title or an absent parent. |
| `(get-task conn id)` / `(children-of conn id)` | Read. |
| `(update! conn id fields)` | Names **only** the fields the caller passed, so a concurrent claim is not clobbered (`provenance R2-1`). |
| `(claim! conn id run-id branch-id)` | First-writer-wins **decided by the UPDATE**, because a read-then-write pair is two lock acquisitions. Returns the task, or `nil` when somebody else holds it. Re-claiming what you hold is idempotent. |
| `(release! conn id branch-id)` | Back to the board, `open`, holder cleared. Only the holder may release. |
| `(close! conn id [status])` | `done` or `cancelled`; refuses a non-terminal status. |
| `(board conn {:keys [run-id]})` | Non-terminal tasks: with a run, its own plus the unclaimed backlog — another run's claimed work is its own business. Ordered by what is moving, then what matters, then what moved recently. |
| `(backlog conn)` | Unclaimed and non-terminal. |
| `(normalize-status s)` / `(normalize-priority p)` | Throw naming the valid values. |

### The tool surface

| group | tools |
|---|---|
| REPL | `eval` `doc` `complete` |
| files | `read_file` `write_file` `edit_file` `grep` |
| shell | `shell` |
| shipping | `done` `give_up` `thesis` `branch_theses` |
| board | `task` `split` |
| coordination | `message` |
| memory | `remember` `recall` `forget` `outcome` |
| record | `fetch_turn` `fetch_artifact` |
| self-modification | `cells` `cell` `reload_cells` `manifest` `policy` `prompt` `introspect` `manual` |
| adaptation | `experiment` `verdict` |
| skills | `skill` |
| lsp | `lsp` |

Every dispatched tool **must** be documented in `prompts/system.md`, matched as
a word at the start of a documentation line — a tool the model is not told about
is unreachable, and `str/includes?` matching once counted `cell` as documented
because the prompt contains `cells`.

### Reads that return an answer

`read_digest({paths, question, anchors?})` sends the files to a reader and
returns only its bullets. The reader is the `:reader` role's model when
`:run :role-models` assigns one and the branch's own otherwise; either way the
files never enter the branch's context. With `anchors: true` every line the
reader sees carries its `line:hash`, so a bullet that cites one is a `patch`
address — the digest can hand back edit coordinates, which a plain summary
cannot. Usage lands on the journal as a `:digest` note.

The steering is a `phases.edn` refusal, not advice: an untargeted `read_file`
(no offset, no limit) of a file over `gates.edn :digest :min-lines` is refused
with a message naming the digest and the paged read. A read that names a
section always passes. The compaction fold's summary call runs on the
`:summarizer` role the same way. Pattern and numbers after Spotify's shunt;
karamazov-b76m.

## Protocol

```
loop/tool-step
  ├─ tools/phase-refusal ctx     BEFORE dispatch — a refused call never reaches
  │                              a tool, and is journalled like any other turn
  ├─ tools/run-tool ctx          the method
  ├─ state/record-outcome        counters, per :category and :progress?
  ├─ state/add-turn              the turn record (tool, category, error)
  ├─ state/add-artifact          when the result carries one
  └─ apply-transitions           phases.edn signal → effect
```

Task claim, and what the branch gets:

```
task claim {id}
  ├─ holding? → refused, naming task switch
  ├─ tasks/claim! conn id run-id branch-id
  ├─ branch :task ← {:id :title}
  └─ append the task statement, :pinned? true   ← never compacted away (RFC-004)

per turn: context-block prepends "Current task: …" or "No task claimed."
          — appended at the END, so it costs nothing (RFC-004 caching rule)
```

## Invariants

| invariant | enforced by |
|---|---|
| Every tool returns `:branch`. | Convention; the `:default` method and helpers all do. **Not** mechanically checked. |
| A malformed call is `:mechanics`, never `:failure`. | The helpers; `agent-test`. |
| Two branches cannot hold one task. | `claim!`'s UPDATE guard on `branch_id`; `kanban-test`. |
| Only the holder can release. | `release!`'s `WHERE branch_id = ?`; `kanban-test`. |
| A branch holds at most one task. | The `holding` check in the tool; `kanban-test`. |
| Closing the current task frees the slot. | The tool clears `:task`; `kanban-test`. |
| Every dispatched tool is documented. | `prompt-test/every-tool-is-documented`. |
| Every tool a gate vocabulary names is registered. | `agent-test`. |

## Known gaps

- ~~The board is encouraged, not enforced.~~ Closed: phases.edn's `:refusals`
  table carries the work-needs-a-task rule, and a refusal is a policy
  refusal (`:mechanics` + `:policy-refusal?`), not a failure (blt.15). The
  claim also survives a resume (blt.21), a switch to the held task is a
  no-op rather than a silent release, only the holder closes held work, and
  setting a task's status to `open` clears its holder (blt.34).
- The result envelope has no schema, but the dispatch wrapper backstops the
  two observed failure shapes (a bare string, a map with no `:branch`) and
  redacts the envelope structurally.
- Pre-v12 claimed rows migrated with `branch_id NULL`, which reads as "on this
  board, nobody holding it" — claimable. The safe reading: the alternative is a
  task no branch can take because it is attributed to one that no longer exists.
