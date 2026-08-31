;; samizdat - a self-hosting agentic harness
;; Copyright (C) 2026 Dmitri Sotnikov
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.test-runner
  "jolt -M:test — run every test namespace and exit non-zero on failure.

  Also callable from a connected editor as (samizdat.test-runner/run) so the
  suite runs against the live process without paying startup again."
  (:require [clojure.test :as t]
            [maestro.core-test]
            [mycelium.cell-test]
            [mycelium.coercion-test]
            [mycelium.compose-test]
            [mycelium.constraints-test]
            [mycelium.core-test]
            [mycelium.default-transition-test]
            [mycelium.defcell-test]
            [mycelium.dev-test]
            [mycelium.effects-test]
            [mycelium.error-groups-test]
            [mycelium.error-handler-test]
            [mycelium.error-messages-test]
            [mycelium.error-taxonomy-test]
            [mycelium.execution-tracing-test]
            [mycelium.fragment-test]
            [mycelium.generate-stubs-test]
            [mycelium.halt-resume-test]
            [mycelium.infer-schema-test]
            [mycelium.input-schema-test]
            [mycelium.integration-test]
            [mycelium.interceptor-test]
            [mycelium.invoke-cell-test]
            [mycelium.join-test]
            [mycelium.lite-schema-test]
            [mycelium.manifest-test]
            [mycelium.middleware-test]
            [mycelium.orchestrate-test]
            [mycelium.propagate-keys-test]
            [mycelium.queue-integration-test]
            [mycelium.queue-test]
            [mycelium.registry-test]
            [mycelium.resilience-test]
            [mycelium.schema-error-test]
            [mycelium.schema-test]
            [mycelium.store-test]
            [mycelium.system-test]
            [mycelium.timeout-test]
            [mycelium.transform-test]
            [mycelium.validate-warn-test]
            [mycelium.validation-test]
            [mycelium.workflow-test]
            [samizdat.agent-test]
            [samizdat.base-test]
            [samizdat.boundary-test]
            [samizdat.compaction-test]
            [samizdat.collab-test]
            [samizdat.select-test]
            [samizdat.session-test]
            [samizdat.reflect-test]
            [samizdat.reflex-test]
            [samizdat.tasks-test]
            [samizdat.kernel-write-test]
            [samizdat.knowledge-test]
            [samizdat.messages-test]
            [samizdat.prompt-test]
            [samizdat.workflow-test]
            [samizdat.manifest-test]
            [samizdat.judge-test]
            [samizdat.team-test]
            [samizdat.board-test]
            [samizdat.planner-test]
            [samizdat.decompose-test]
            [samizdat.decompose-run-test]
            [samizdat.verify-test]
            [samizdat.feature-test]
            [samizdat.telemetry-test]
            [samizdat.supervisor-test]
            [samizdat.gitdiff-test]
            [samizdat.skills-test]
            [samizdat.source-test]
            [samizdat.security.secrets-test]
            [samizdat.config-test]
            [samizdat.files-test]
            [samizdat.edit-test]
            [samizdat.eval-mode-test]
            [samizdat.grep-test]
            [samizdat.hashline-test]
            [samizdat.control-test]
            [samizdat.util-test]
            [samizdat.lisp-test]
            [samizdat.lsp-test]
            [samizdat.cells-test]
            [samizdat.park-test]
            [samizdat.events-test]
            [samizdat.cell-schema-test]
            [samizdat.mutation-test]
            [samizdat.ratelimit-test]
            [samizdat.repl-confinement-test]
            [samizdat.retire-test]
            [samizdat.schemacheck-test]
            [samizdat.repl-guard-test]
            [samizdat.repl-test]
            [samizdat.roles-test]
            [samizdat.sandbox-test]
            [samizdat.websearch-test]
            [samizdat.toolerr-test]
            [samizdat.tape-test]
            [samizdat.image-test]
            [samizdat.infer-test]
            [samizdat.fork-test]
            [samizdat.probe-test]
            [samizdat.manual-test]
            [samizdat.userspace-test]
            [samizdat.beam-test]
            [samizdat.kanban-test]
            [samizdat.security.policy-test]
            [samizdat.llm-test]
            [samizdat.prompt-test]
            [samizdat.server-test]
            [samizdat.adapter-test]
            [samizdat.proc-test]
            [samizdat.board-bt-test]
            [samizdat.finalization-test]
            [samizdat.replroots-test]
            [samizdat.oversight-test]
            [samizdat.mechanics-test]
            [samizdat.repair-test]
            [samizdat.storm-test]
            [samizdat.tournament-test]
            [samizdat.trajectory-test]
            [samizdat.store-test]
            [samizdat.gui-api-test]
            [samizdat.gui-ops-test]
            [samizdat.gui-graph-test]
            [samizdat.gui-style-test]
            [samizdat.gui-input-test]
            [samizdat.gui-mathtext-test]
            [samizdat.gui-newrun-test]))

(def namespaces
  '[maestro.core-test
    mycelium.cell-test
    mycelium.coercion-test
    mycelium.compose-test
    mycelium.constraints-test
    mycelium.core-test
    mycelium.default-transition-test
    mycelium.defcell-test
    mycelium.dev-test
    mycelium.effects-test
    mycelium.error-groups-test
    mycelium.error-handler-test
    mycelium.error-messages-test
    mycelium.error-taxonomy-test
    mycelium.execution-tracing-test
    mycelium.fragment-test
    mycelium.generate-stubs-test
    mycelium.halt-resume-test
    mycelium.infer-schema-test
    mycelium.input-schema-test
    mycelium.integration-test
    mycelium.interceptor-test
    mycelium.invoke-cell-test
    mycelium.join-test
    mycelium.lite-schema-test
    mycelium.manifest-test
    mycelium.middleware-test
    mycelium.orchestrate-test
    mycelium.propagate-keys-test
    mycelium.queue-integration-test
    mycelium.queue-test
    mycelium.registry-test
    mycelium.resilience-test
    mycelium.schema-error-test
    mycelium.schema-test
    mycelium.store-test
    mycelium.system-test
    mycelium.timeout-test
    mycelium.transform-test
    mycelium.validate-warn-test
    mycelium.validation-test
    mycelium.workflow-test
    samizdat.board-bt-test
    samizdat.finalization-test
    samizdat.replroots-test
    samizdat.oversight-test
    samizdat.mechanics-test
    samizdat.repair-test
    samizdat.storm-test
    samizdat.tournament-test
    samizdat.trajectory-test
    samizdat.store-test
    samizdat.llm-test
    samizdat.agent-test
    samizdat.base-test
    samizdat.boundary-test
    samizdat.compaction-test
    samizdat.collab-test
    samizdat.select-test
    samizdat.session-test
    samizdat.reflect-test
    samizdat.reflex-test
    samizdat.tasks-test
    samizdat.kernel-write-test
    samizdat.knowledge-test
    samizdat.messages-test
    samizdat.prompt-test
    samizdat.workflow-test
    samizdat.manifest-test
    samizdat.judge-test
    samizdat.team-test
    samizdat.board-test
    samizdat.planner-test
    samizdat.decompose-test
    samizdat.decompose-run-test
    samizdat.verify-test
    samizdat.feature-test
    samizdat.telemetry-test
    samizdat.supervisor-test
    samizdat.gitdiff-test
    samizdat.skills-test
    samizdat.source-test
    samizdat.security.secrets-test
    samizdat.config-test
    samizdat.files-test
    samizdat.edit-test
    samizdat.eval-mode-test
    samizdat.grep-test
    samizdat.hashline-test
    samizdat.control-test
    samizdat.util-test
    samizdat.lisp-test
    samizdat.lsp-test
    samizdat.cells-test
    samizdat.park-test
    samizdat.events-test
    samizdat.cell-schema-test
    samizdat.mutation-test
    samizdat.ratelimit-test
    samizdat.repl-confinement-test
    samizdat.retire-test
    samizdat.schemacheck-test
    samizdat.repl-guard-test
    samizdat.repl-test
    samizdat.roles-test
    samizdat.sandbox-test
    samizdat.websearch-test
    samizdat.toolerr-test
    samizdat.tape-test
    samizdat.image-test
    samizdat.infer-test
    samizdat.fork-test
    samizdat.probe-test
    samizdat.manual-test
    samizdat.userspace-test
    samizdat.beam-test
    samizdat.kanban-test
    samizdat.security.policy-test
    samizdat.prompt-test
    samizdat.server-test
    samizdat.adapter-test
    samizdat.proc-test
    samizdat.gui-api-test
    samizdat.gui-ops-test
    samizdat.gui-graph-test
    samizdat.gui-style-test
    samizdat.gui-input-test
    samizdat.gui-mathtext-test
    samizdat.gui-newrun-test])

(defn run []
  (apply t/run-tests namespaces))

(defn -main [& _]
  (let [{:keys [fail error] :as summary} (run)]
    (println)
    (println summary)
    (System/exit (if (pos? (+ (or fail 0) (or error 0))) 1 0))))
