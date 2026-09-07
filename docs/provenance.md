# Provenance

An index of the numbered review findings that code comments cite. This is NOT a
specification — the specifications are `docs/RFCS/`, one per layer. It exists
for one reason: about fifty comments in the code say a guard exists because of a
specific past failure, and a reader has to be able to look that failure up.


How the four in-tree libraries differ from their upstreams is a different
record: `docs/divergences.md`, checked against `docs/divergences.edn` by
`samizdat.divergences-test`.

The citations read `(provenance R3-11)`, `(provenance A-4)`. The findings they
name are below, one line each. The full original write-ups are in git
history (`git log --diff-filter=D -- docs/`) if the one-line summary is not
enough.

Tags: `A-n` = the 2026-05 review, `CR1-n` = 2026-08 pass 1, `R2-n` = pass 2,
`R3-n` = pass 3.

### A — 2026-05

| tag | finding |
|---|---|
| A-1 | Shell policy bypass: a command could reach the shell without a permission decision. |
| A-2 | The human-approval flow had no write path — `grant!` was unreachable, so an `ask` could never be answered and a run blocked forever. |
| A-3 | The full suite errored on six tests from file-descriptor exhaustion; the `test` task raises `ulimit -n` because of this. |
| A-4 | `tasks/claim!` contradicted its own contract: a read-then-write pair let two branches both claim a task. Fixed by guarding inside the UPDATE — and see RFC-002 F3, which found the fix's granularity was still wrong. |
| A-5 | `lisp/scan` misreported `:balanced` for an escaped quote at end-of-input. |

### CR1 — 2026-08, pass 1

| tag | finding |
|---|---|
| CR1-1 | `base/missing` returned a bare string where callers expected a result map, NPE-ing the loop. |
| CR1-2 | Dead guard: the "usage cap wearing a rate limit's headers" branch could never fire, because the provider's asked-for wait was clamped before the check that needed it unclamped. |
| CR1-3 | An `active`-registry race stranded stale entries and let `abort!` rewrite finished runs. |
| CR1-4 | The LSP client let concurrent requests steal each other's frames. |
| CR1-5 | Eval-session namespaces were never removed — unbounded growth on a long-lived serve process. |
| CR1-6 | GUI poller lifecycle races and stale-run bleed. |

### R2 — 2026-08, pass 2

| tag | finding |
|---|---|
| R2-1 | `tasks/update!` re-opened the claim-race class one definition away from the fixed `claim!`. |
| R2-2 | The `message` tool sent the branch **map** as `:from`, storing 55–73 KB map dumps in `from_branch` and breaking the inbox's sender exclusion. |
| R2-3 | `migrate!` autocommitted every statement, so a crash mid-`ALTER` permanently bricked startup. |
| R2-4 | `abort!` had a transient window, and lifecycle `UPDATE`s ran with no status guards. |
| R2-6 | GUI: the inspector scroll reset to top on every state swap rather than on selection change. |
| R2-7 | GUI: a single-flight CAS silently dropped a newly selected branch's fetch — a permanent "loading…" on idle runs. |
| R2-8 | GUI: one failed run-list fetch disconnected the run being tailed. |
| R2-9 | LSP `client-for` had a get-then-create race that orphaned a duplicate clojure-lsp process. |
| R2-10 | LSP `diagnostics`: a concurrent same-uri erase returned a false "clean". |
| R2-11 | No retention anywhere — every high-volume table grew forever in one shared database file. |
| R2-13 | Five of thirty-four `gates.edn` keys had no reader. Now none do; see RFC-001 F2 for why checking this is harder than it looks. |
| R2-14 | `claim!` could resurrect a terminal unclaimed task. |
| R2-15 | Id-retry loops and FTS catches discarded the real failure cause, so a disk or lock error was reported as an id collision. |
| R2-17 | The LSP reader routed frames outside its `try`; one malformed frame killed the client permanently. |

### R3 — 2026-08, pass 3

| tag | finding |
|---|---|
| R3-1 | **Model-reachable arbitrary command execution on the verify path, with the unscrubbed parent environment.** Fixed: the child gets `secrets/scrubbed-process-env` and its output passes the redaction boundary. Compare RFC-003 F1, which is the same threat on a path that got neither guard. |
| R3-3 | `ring_chez` answered HTTP/1.1 but silently truncated chunked request bodies. |
| R3-4 | Unbounded request size, no read timeout, thread-per-connection. |
| R3-5 | Multibyte UTF-8 split across `recv` boundaries corrupted request bodies. |
| R3-6 | The arbiter and the state machine still referenced the removed proof-era tool surface. |
| R3-8 | Timeout kill escalation reached only the root process; trapped children survived. |
| R3-9 | Dead gate key `:sketch-duplicate-threshold` was still served by `/v1/harness/gates`. |
| R3-11 | `cells/resource-dir` was never called, so an AOT binary launched outside the project root silently registered zero cells. |
| R3-12 | Server/adapter conformance nits. |
| R3-13 | `control/watch` hardcoded branch `"B1"`. |
| R3-14 | `prefill-support?` had zero production callers; `chat-body` consulted a private twin, so the answer a caller could query and the answer acted on could drift. |

## What these replaced

Five review records, an audit, and a security document. The review records are
above; the audit's question is answered by RFC-001 and the security document
became RFC-003. Full text: `git log --diff-filter=D -- docs/`.
