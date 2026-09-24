;; samizdat - a claim-first verification harness
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

(ns samizdat.capabilities
  "Every namespace the shipped manual (resources/manual.edn) names, required,
  so all of them are in the binary.

  `jolt build` compiles what samizdat.main's require graph reaches, and
  nothing else: only resources/ is embedded, so a namespace nobody requires is
  not in the binary and the manual's runtime `require` of it fails there. It
  failed the whole `manual` tool — run 756b5572's supervisor spent its pass on
  \"samizdat.agent.tournament/run: its namespace could not be loaded\".
  samizdat.core requires this; manual-test holds it to the manual."
  (:require [mycelium.patch]
            [samizdat.agent.acceptance]
            [samizdat.agent.compaction]
            [samizdat.agent.files]
            [samizdat.agent.gates]
            [samizdat.agent.infer]
            [samizdat.agent.instructions]
            [samizdat.agent.judge]
            [samizdat.agent.live]
            [samizdat.agent.orient]
            [samizdat.agent.outline]
            [samizdat.agent.phases]
            [samizdat.agent.select]
            [samizdat.agent.state]
            [samizdat.agent.storm]
            [samizdat.agent.stubs]
            [samizdat.agent.telemetry]
            [samizdat.agent.tools.ask]
            [samizdat.agent.tools.digest]
            [samizdat.agent.tools.ship]
            [samizdat.agent.tools.split]
            [samizdat.agent.tournament]
            [samizdat.agent.trajectory]
            [samizdat.agent.webfetch]
            [samizdat.api.stream]
            [samizdat.approval]
            [samizdat.battery]
            [samizdat.cancel]
            [samizdat.cells]
            [samizdat.config]
            [samizdat.escapes]
            [samizdat.events]
            [samizdat.export]
            [samizdat.hashline]
            [samizdat.layers]
            [samizdat.lisp]
            [samizdat.llm.client]
            [samizdat.llm.grammar]
            [samizdat.manifests]
            [samizdat.metrics]
            [samizdat.mutation]
            [samizdat.procedure]
            [samizdat.prompt]
            [samizdat.repl.guard]
            [samizdat.repl.route]
            [samizdat.replay]
            [samizdat.security.exposure]
            [samizdat.security.policy]
            [samizdat.security.sandbox]
            [samizdat.session]
            [samizdat.store.interventions]
            [samizdat.store.journal]
            [samizdat.symbolic]
            [samizdat.symbolic.dispatch]
            [samizdat.tape]
            [samizdat.userspace]
            [samizdat.workflow]))
