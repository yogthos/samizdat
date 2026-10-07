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

(ns samizdat.agent.tools.shell
  "The shell tool.

  One call into samizdat.security.policy, which owns the permission engine,
  the scrubbed environment, and output redaction. See
  samizdat.agent.tools.base for the shared result helpers and the run-tool
  multimethod."
  (:require [samizdat.agent.files :as files]
            [samizdat.agent.tools.base :as base]
            [samizdat.security.confine :as confine]
            [samizdat.security.policy :as policy]))

(defmethod base/run-tool "shell" [{:keys [branch] :as ctx}]
  ;; Every command faces the permission engine, runs under a scrubbed
  ;; environment, and its output is redacted before it returns — one call into
  ;; samizdat.security.policy, which owns all three. A denied or unapproved
  ;; command never spawns. The result's :category is what the cull guard
  ;; reads: a deny is :mechanics with :policy-refusal? (the branch did
  ;; nothing wrong, the harness declined — the refusal counter's business,
  ;; not the cull counter's), an :ask is :neutral (waiting on a human), and
  ;; a real command failure is :failure.
  (if-let [m (base/missing ctx :command)]
    (base/malformed branch m)
    ;; What the shell may read without asking: the project, the reference
    ;; roots the operator declared — read_file's boundary — and the build
    ;; caches, where a dependency's source is and which the shell can write
    ;; anyway (karamazov-3vu1.3).
    (let [r (policy/run-shell (assoc ctx :read-roots
                                     (concat [(or (:root ctx) ".")]
                                             (files/ctx-reference-roots ctx)
                                             (confine/build-caches (System/getenv "HOME")))))]
      (assoc r :branch branch
             ;; A policy refusal is journalled as declined, like a phase
             ;; refusal, so the record can tell it from a command that ran
             ;; and failed.
             :policy-refusal? (contains? #{:deny :ask} (get-in r [:policy :effect]))))))
