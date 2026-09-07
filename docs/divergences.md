# Divergences from upstream

samizdat carries four libraries in-tree, and each differs from its upstream.
This is the record of how, written once, here, instead of in headers that
drift. `docs/divergences.edn` is the machine-readable half and
`test/samizdat/divergences_test.clj` checks the two against each other on
every run: every entry there must be anchored below with an HTML comment,
every anchor below must have an entry, and every entry either names a test
that pins the behaviour or says why none can. The pattern is ebb's
`doc/conformance.md` and jolt's `known-divergences.edn`.

What it cannot do is notice a divergence nobody wrote down. That would need
the upstream as an oracle, and none of them run under jolt.

| library | upstream | in tree | licence |
|---|---|---|---|
| mycelium | [mycelium-clj/mycelium](https://github.com/mycelium-clj/mycelium), sha not recorded when vendored | `src/mycelium/` | as upstream |
| maestro | [yogthos/maestro](https://github.com/yogthos/maestro) `src/maestro/core.cljc` | `src/maestro/` | as upstream |
| parinferish | [oakes/parinferish](https://github.com/oakes/parinferish) `src/parinferish/core.cljc` | `src/parinferish/` | public domain, not ours to relicense |
| ring-chez-adapter | jolt-lang/ring-chez-adapter `@07f14d9` | `src/ring_chez/` | EPL-2.0, not ours to relicense |

## ring-chez-adapter

<!-- divergence: ring-chez-connection-off-the-accept-loop -->
**Each connection is served off the accept loop.** Upstream serves one
connection at a time on the accept thread, so a long request blocked `/health`
and every other request until it finished. The vendored copy hands each
accepted connection off the loop, marked inline in `adapter.clj`. Worth
offering upstream; vendored until then. Probed by the smoke task's `/slow`
check; no unit test isolates the accept loop.

## maestro

<!-- divergence: maestro-dispatch-predicates-compile-with-host-eval -->
**Dispatch predicates compile with the host eval.** Upstream evaluates EDN
dispatch predicates under sci. Here sci is gone; safety against a bad
agent-authored predicate comes from the mutation protocol's validate, soak and
rollback rather than from interpreter sandboxing. Pinned by
`maestro.core-test/compile-eagerly-validates-dispatches`.

<!-- divergence: maestro-async-executor-on-a-jolt-future -->
**The async executor is a jolt future.** Upstream drives an async FSM with
promesa. Here the executor loops on its own thread and parks on a per-step
promise, so it is stack-safe for synchronous callbacks and correct for
callbacks fired from other threads; `run` on an async FSM returns that future
and deref rethrows. No samizdat manifest reaches it; RFC-013 replaces it with
an ebb task under `karamazov-3cll.8`. Pinned by
`maestro.core-test/async-cross-thread`.

<!-- divergence: maestro-cljc-flattened -->
**The `.cljc` is flattened.** Reader conditionals dropped; jolt shims
`System/nanoTime`. A property of the file, not a behaviour.

## parinferish

<!-- divergence: parinferish-last-index-of-takes-a-string -->
**`last-index-of` takes a string needle.** `count-last-line` passes `"\n"`
rather than `\newline`: jolt's `clojure.string/last-index-of` takes a string
where the JVM's also accepts a char. The line is reached by any multi-line
string token, so unported it threw out of `parse` on exactly the truncated
input the repair rung exists for. Marked `PORT CHANGE` inline. Exercised
through `samizdat.lisp/balance`'s parinfer rung; no test isolates the needle.

## mycelium

<!-- divergence: mycelium-resilience-is-native -->
**Resilience policies are native.** Upstream backs them with resilience4j.
Here each policy is a small atom-based state machine with the same public API
(`wrap-handler`, `validate-resilience!`), the same config keys and the same
`:mycelium/resilience-error` contract; failure types travel as `::type` in
ex-data rather than as Java exception classes, so nothing unwraps an
`ExecutionException`. Pinned by
`mycelium.resilience-test/composed-timeout-and-retry-test`.

<!-- divergence: mycelium-queue-is-an-atom -->
**The work queue is one atom.** Upstream guards a `PriorityBlockingQueue`
with a `ReentrantLock`. Here all queue state lives in one atom of pure data
(tasks map plus sorted ready set) mutated through a compare-and-set loop, so
every operation is atomic without host locks. Pinned by
`mycelium.queue-test/memory-queue-claim-test`.

<!-- divergence: mycelium-dispatches-are-patterns -->
**Dispatch entries may be patterns.** A dispatch may be a pattern over the
data map (`{:verdict :done}`, `_`) rather than a predicate function, resolved
through `samizdat.symbolic.dispatch`, so routing stays readable data in the
manifest and a table of patterns can be checked for a branch nothing can
reach. `mycelium.workflow`, `manifest`, `validation` and `dev` require the
symbolic layer for it, which upstream has no notion of. Pinned by
`samizdat.symbolic.dispatch-test/the-pattern-form-agrees-with-the-fn-form-it-replaced`.

<!-- divergence: mycelium-registry-snapshot-for-rollback -->
**The cell registry can be snapshotted and restored.** The mutation
protocol's rollback needs the registry returned to exactly the state a load
started from, which upstream has no reason to offer. Pinned by
`samizdat.mutation-test/a-rollback-records-what-was-actually-tried`.

<!-- divergence: mycelium-time-shim-before-malli -->
**`jolt.time` is required before `malli.transform`.** malli's date formatter
needs the java.time shim at namespace load, the same fix as selmer in
`samizdat.prompt`. Load order; the namespace does not load without it.
