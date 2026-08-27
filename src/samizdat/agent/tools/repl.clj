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

(ns samizdat.agent.tools.repl
  "The REPL tools: eval, doc, complete.

  Evaluating Clojure in the live image is the primary feedback loop; these
  three methods only dispatch it. See samizdat.agent.tools.base for the
  shared result helpers and the run-tool multimethod.

  THE REDACTION BOUNDARY APPLIES HERE TOO (RFC-003 F1). `eval` runs in the
  harness process, so unlike the shell it can read the environment, the
  resolved config and the secret store directly — strictly more capability
  than the path that gets a scrubbed env and a redacted result. It had
  neither, and the security model asserted that no path from the environment
  reaches model space unredacted while its graph omitted this tool entirely.

  What redacting here does and does not buy: it closes the ACCIDENTAL leak,
  which is the realistic one — a model prints a config map while debugging and
  a provider key lands in the branch messages and the journal permanently.
  It does not stop deliberate exfiltration, because a model that wants the
  value out can encode it, and in-process execution cannot be contained from
  inside the process. That case is out of scope by design and RFC-003 says so
  rather than leaving it silently unhandled."
  (:require [clojure.string :as str]
            [samizdat.agent.tools.base :as base]
            [samizdat.repl :as repl]
            [samizdat.security.secrets :as secrets]))

(defn- scrubbed
  "A model-bound eval payload with known secret values and credential-shaped
  strings replaced. Same function the shell path uses, so the two boundaries
  cannot drift in what they consider a secret."
  [s]
  (secrets/redact (str s) (secrets/known-values (into {} (System/getenv)))))

(defmethod base/run-tool "eval" [{:keys [branch repl-session] :as ctx}]
  ;; The BRANCH's session when it has one, the run's otherwise. Per-branch is
  ;; what keeps two competing branches from seeing each other's defs; the run
  ;; session remains the answer for a single-branch driver and for a resume
  ;; that rebuilt a branch without one.
  ;; Evaluate Clojure in the live harness image. REPL-first development: the
  ;; agent tries a form, sees the value and output, and iterates before
  ;; committing it to a file. :neutral — evaluating establishes nothing on its
  ;; own; a define-and-test is exploration, and progress is the file it leads
  ;; to. Defs persist across evals within a run (the session is per-run).
  (if-let [m (base/missing ctx :code)]
    (base/malformed branch m)
    (let [timeout (some-> (base/arg ctx :timeout-ms) str str/trim not-empty parse-long)
          session (or (:repl-session branch) repl-session)
          r (repl/eval-code (str (base/arg ctx :code)) session timeout)]
      (if (:ok r)
        (base/ok branch (scrubbed (str "=> " (:value r)
                                       (when (seq (:out r)) (str "\n" (:out r))))))
        (assoc (base/fail branch (scrubbed (str "Eval error: " (:error r)
                                                (when (seq (:out r))
                                                  (str "\n" (:out r))))))
               ;; Same flag run-shell carries: a timed-out eval burned its
               ;; whole budget, and the loop weights it accordingly.
               :timeout? (= "timeout" (:error-type r)))))))

(defmethod base/run-tool "doc" [{:keys [branch] :as ctx}]
  (if-let [m (base/missing ctx :symbol)]
    (base/malformed branch m)
    (let [d (repl/doc-sym (str (base/arg ctx :symbol)))]
      (if (:not-found d)
        (base/malformed branch (str "No var " (base/arg ctx :symbol) " is loaded."))
        (base/ok branch (str (:name d) "\n" (pr-str (:arglists d)) "\n\n" (:doc d)))))))

(defmethod base/run-tool "complete" [{:keys [branch] :as ctx}]
  (if-let [m (base/missing ctx :prefix)]
    (base/malformed branch m)
    (let [ms (repl/complete (str (base/arg ctx :prefix)))]
      (base/ok branch (if (seq ms)
                   (str/join "\n" (take 50 ms))
                   (str "No symbols match " (base/arg ctx :prefix) "."))))))
