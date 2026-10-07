;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.cell-prelude
  "Preload for the namespaces the shipped cells depend on but nothing else in
  src requires.

  Cells run in SCI (samizdat.sandbox.sci), and the allowlist there is DATA —
  it requires each namespace it exposes when it first builds a context, not
  at compile time, because several of those namespaces require the sandbox.
  A namespace only a cell reaches — samizdat.agent.planner, telemetry, judge —
  would therefore be absent from a `jolt build` image, which embeds the
  entry's require closure and nothing else, and the first cell load there
  would fail to find it. It used to be worse: cells were load-stringed, and on
  a `-dirty` jolt build that nested compile path poisoned the AOT cache
  (karamazov-fv6).

  Requiring those namespaces HERE, from a normal src namespace on the compile
  graph (core -> workflow -> cells -> this), puts them in the image. A
  preload, not a dependency — nothing here is called; the `require` is the
  whole point. Add a namespace here whenever a new shipped cell reaches for
  one that nothing in src already pulls in."
  (:require [samizdat.agent.compaction]
            [samizdat.agent.decompose]
            ;; cells/oversight.clj reads the run's own health to decide
            ;; whether the harness needs looking at.
            [samizdat.agent.gates]
            [samizdat.agent.gitdiff]
            [samizdat.agent.judge]
            [samizdat.agent.planner]
            [samizdat.agent.reflect]
            [samizdat.agent.telemetry]
            ;; cells/board.clj and cells/feature.clj read the run's metrics;
            ;; nothing in src required the namespace, so a built binary would
            ;; have lacked it (cells-test caught it, 2026-09-16).
            [samizdat.metrics]))

;; decompose was the one shipped-cell dependency nothing in src reached, so it
;; was never compiled into a `jolt build` image and cells/decompose.clj's
;; load-string fell through to "Could not locate samizdat/agent/decompose on
;; the source roots". Invisible until now for two reasons: a built binary did
;; not boot outside the project root at all, and inside it the source tree was
;; sitting there for the fallback to find. cells-test walks every shipped
;; cell's requires against this list so the next one is caught at test time.
