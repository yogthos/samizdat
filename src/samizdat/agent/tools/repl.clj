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
            [samizdat.agent.source :as source]
            [samizdat.agent.tools.base :as base]
            [samizdat.prompt :as prompt]
            [samizdat.repl :as repl]
            [samizdat.repl.route :as route]
            [samizdat.security.secrets :as secrets]))

(defn- scrubbed
  "A model-bound eval payload with known secret values and credential-shaped
  strings replaced. Same function the shell path uses, so the two boundaries
  cannot drift in what they consider a secret."
  [s]
  (secrets/redact (str s) (secrets/known-values (into {} (System/getenv)))))

(defn- eval-vetted
  "Evaluate source that has already passed the syntax gate. `note` is the
  repair sentence when the harness closed a truncation, so the model is told
  its code was completed rather than silently corrected — an invisible repair
  teaches nothing and the next form drops the same closer."
  [{:keys [branch repl-session] :as ctx} code note]
  (let [timeout (some-> (base/arg ctx :timeout-ms) str str/trim not-empty parse-long)
        session (or (:repl-session branch) repl-session)
        prefix (if note (str "[harness] " note "\n") "")
        ;; WHICH IMAGE, decided per role by the operator's config. Everything
        ;; below is unchanged: route/eval-for answers in the same shape
        ;; repl/eval-code always did, so the redaction boundary, the syntax
        ;; gate and the timeout flag all still apply exactly as they did.
        r (route/eval-for ctx code session timeout)]
    (if (:ok r)
      (base/ok branch (scrubbed (str prefix "=> " (:value r)
                                     (when (seq (:out r)) (str "\n" (:out r))))))
      (assoc (base/fail branch
                        (scrubbed (str prefix "Eval error: " (:error r)
                                       (when (seq (:out r)) (str "\n" (:out r)))
                                       ;; WHERE it ran, every time it fails.
                                       ;; A branch that cannot tell a stale
                                       ;; session from a broken file reads
                                       ;; every eval failure as a defect in
                                       ;; its own code and goes looking in the
                                       ;; file — which is exactly what run
                                       ;; f2014821 did for ten turns until a
                                       ;; supervisor pass told it otherwise
                                       ;; (karamazov-60c).
                                       (when (:where r)
                                         (str "\n\n"
                                              (prompt/render "eval-error"
                                                             {:where (:where r)}))))))
             ;; Same flag run-shell carries: a timed-out eval burned its
             ;; whole budget, and the loop weights it accordingly.
             :timeout? (= "timeout" (:error-type r))))))

(defmethod base/run-tool "eval" [{:keys [branch] :as ctx}]
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
    ;; THE SAME GATE THE FILE TOOLS USE. An eval form is wholly authored in
    ;; this call, so nothing pre-existing can be re-parented and a dropped
    ;; closer is repaired exactly as it would be in a write_file — which is
    ;; what this path did NOT do, for no reason anyone had decided. Run
    ;; 5e8b5973 lost 2 of its first 17 turns to `Eval error: Unmatched
    ;; delimiter: )`, a message with no line, no column and no repair, for text
    ;; the harness already knew how to fix. Unrepairable source never reaches
    ;; the reader at all: it is a malformed CALL, not a failed evaluation, so
    ;; it is :mechanics with a line and a column rather than :failure with the
    ;; reader's guess at where things went wrong.
    (let [{:keys [code problem note]} (source/vet (str (base/arg ctx :code))
                                                  {:whole? true})]
      (if problem
        ;; WHICH KIND of problem, so the advice matches it. The template used
        ;; to append a delimiter hint to every refusal, including reasons that
        ;; have nothing to do with delimiters: run ace34d83 refused a bad
        ;; string escape six times and told the model about parens each time,
        ;; which is where T0 spent its last six turns.
        (base/malformed branch
                        (prompt/render "eval-syntax"
                                       {:syntax note
                                        (name (:reason problem)) true
                                        :delimiter (not= :does-not-read
                                                         (:reason problem))}))
        (eval-vetted ctx code note)))))

(defmethod base/run-tool "doc" [{:keys [branch] :as ctx}]
  (if-let [m (base/missing ctx :symbol)]
    (base/malformed branch m)
    (let [d (route/doc-for ctx (str (base/arg ctx :symbol)))]
      (if (:not-found d)
        (base/malformed branch (str "No var " (base/arg ctx :symbol) " is loaded."))
        (base/ok branch (str (:name d) "\n" (pr-str (:arglists d)) "\n\n" (:doc d)))))))

(defmethod base/run-tool "complete" [{:keys [branch] :as ctx}]
  (if-let [m (base/missing ctx :prefix)]
    (base/malformed branch m)
    (let [ms (route/complete-for ctx (str (base/arg ctx :prefix)))]
      (base/ok branch (if (seq ms)
                   (str/join "\n" (take 50 ms))
                   (str "No symbols match " (base/arg ctx :prefix) "."))))))
