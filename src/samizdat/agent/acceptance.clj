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
(ns samizdat.agent.acceptance
  "The acceptance checker: criteria the OPERATOR writes before the run,
  checked over the tree as it stands, which the run cannot edit
  (karamazov-a6mj.2).

  WHY IT EXISTS. Gate 2 runs the suite the run itself writes and may weaken
  (karamazov-fgsb measured 2 of 22 runs doing so); the critic is an LLM over
  the diff and catches an omitted requirement a whole round late
  (karamazov-dsfx). Thinkingbox's model of completion is a test case = a task
  + a CHECKER the author wrote first, run over the post-run world state,
  invisible to the agent, testing the OUTCOME and never the path. This is
  that object for a harness that runs real tools on a real tree: a shell
  command whose exit code is the verdict, or a narrow yes/no question for the
  critic role, one verdict per criterion.

  MECHANISM ONLY. Nothing here knows when it runs or what a failure means:
  `done` runs the :check criteria at the ship gate (model-free, like every
  other rung there) and :feature/verify runs both kinds as Gate 2. The shell
  and the judge are injected seams, so the whole decision is pure.

  WHERE THE SPEC LIVES decides whether it is a gate: `:run :acceptance` in
  the project's .samizdat/config.edn, which the file tools and the shell
  policy already refuse to let a run write (karamazov-kvw, files/run-config?).
  A spec the run could edit would be Gate 2 all over again."
  (:require [clojure.string :as str]
            [samizdat.lexicon :as lexicon]
            [samizdat.prompt :as prompt]))

;; --- the spec ---------------------------------------------------------------

(defn- malformed
  "The exception for a criterion that does not parse. Returned, not thrown, so
  every call site reads `(throw (malformed …))` — which is also what lets the
  base-test prose ratchet see these strings for the operator-facing
  exception text they are."
  [entry why]
  (ex-info (str "malformed acceptance criterion: " why) {:criterion entry}))

(defn normalize
  "`spec` — the value of :run :acceptance — as a vector of criteria
  `{:name :kind :text}`, `:kind` :check (a shell command, pass = exit 0) or
  :judge (a narrow yes/no question for the critic). nil or [] is no criteria.

  A malformed entry THROWS rather than being dropped: a criterion the checker
  silently skipped is a requirement nobody checks, which is the defect this
  namespace exists to prevent. Loud at config time beats quiet at ship time."
  [spec]
  (cond
    (nil? spec) []
    (not (sequential? spec)) (throw (malformed spec "the spec must be a vector of criteria"))
    :else
    (into []
          (map (fn [{:keys [name check judge] :as entry}]
                 (when-not (map? entry) (throw (malformed entry "each criterion is a map")))
                 (when (str/blank? (str name)) (throw (malformed entry "a criterion needs a :name")))
                 (cond
                   (and check judge) (throw (malformed entry "one of :check or :judge, not both"))
                   (not (str/blank? (str check))) {:name (str name) :kind :check :text (str check)}
                   (not (str/blank? (str judge))) {:name (str name) :kind :judge :text (str judge)}
                   :else (throw (malformed entry "a criterion needs a :check command or a :judge question")))))
          spec)))

;; --- running it -------------------------------------------------------------

(defn- run-one
  "One criterion's verdict. `:passed?` is true, false, or nil for a criterion
  that was not decided — not run (its kind was not asked for), or a judge
  that gave no verdict or threw. nil is fail-OPEN: like the critic, a judge
  that cannot answer must not refuse a ship by itself; it is recorded so a
  reader can see the gate went undecided."
  [{:keys [name kind text]} {:keys [kinds run-check judge]}]
  (let [base {:name name :kind kind}]
    (cond
      (not (contains? kinds kind))
      (assoc base :passed? nil :output "not run")

      (= :check kind)
      (let [attempt (fn [] (try (run-check text)
                                (catch Throwable e {:green? false :output (str "check failed to run: " (ex-message e))})))
            blocked? (fn [{:keys [green? output]}]
                       (and (not green?)
                            (some #(str/includes? (str output) %)
                                  (lexicon/wordlist :environment-blocked))))
            ;; A check the ENVIRONMENT refused — no window server, say — is
            ;; tried once more, and if it is still refused it is undecided,
            ;; not failed: the work was not what stopped it, and sending the
            ;; round back to fix it asks an owner to repair the host
            ;; (run eb64f59d, karamazov-na2k.24).
            first-try (attempt)
            r (if (blocked? first-try) (attempt) first-try)
            {:keys [green? timeout? output]} r]
        (if (blocked? r)
          (assoc base :passed? nil :blocked true
                 :output (str "blocked by the environment\n" output))
          (assoc base :passed? (boolean green?)
                 :output (if timeout?
                           (str "timed out" (when-not (str/blank? (str output)) (str "\n" output)))
                           (str output)))))

      :else
      (let [{:keys [yes? reply]}
            (try (judge text)
                 (catch Throwable e {:yes? nil :reply (str "judge failed: " (ex-message e))}))]
        (assoc base :passed? (when (boolean? yes?) yes?)
               :output (str reply))))))

(defn check
  "Run `criteria` (from `normalize`) and return one result per criterion, in
  order: `{:name :kind :passed? :output}`.

    :kinds      which kinds to run, default #{:check :judge}; a criterion of a
                kind not asked for comes back :passed? nil, \"not run\"
    :run-check  (fn [cmd] -> {:green? :timeout? :output}) — verify/run-verify
                bound to a root, in production
    :judge      (fn [question] -> {:yes? true|false|nil :reply}) — the critic
                role, in production; may be absent when :judge is not in :kinds

  A reduce, not a mapv: the judge parks (llm/chat), and a park inside a lazy
  body or mapv is a hang on a fiber (RFC-013)."
  [criteria {:keys [kinds] :as opts}]
  (let [opts (assoc opts :kinds (or kinds #{:check :judge}))]
    (reduce (fn [acc c] (conj acc (run-one c opts))) [] criteria)))

(defn failed
  "The criteria that were decided and did not pass."
  [results]
  (filterv #(false? (:passed? %)) results))

(defn unknown
  "The criteria that were not decided — not run, or a judge with no verdict."
  [results]
  (filterv #(nil? (:passed? %)) results))

(defn all-passed?
  "Whether nothing decided against the ship: no criterion is false. An
  undecided criterion does not block (see `run-one`)."
  [results]
  (empty? (failed results)))

;; --- what the branch reads --------------------------------------------------

(defn- tail [s n]
  (->> (str/split-lines (str s)) (remove str/blank?) (take-last n) (str/join "\n")))

(defn table
  "One line per criterion — PASS / FAIL / ? — for a journal note, a verify
  note, or a person reading a run."
  [results]
  (str/join "\n"
            (map (fn [{:keys [name passed?]}]
                   (str (case passed? true "PASS" false "FAIL" "?   ") "  " name))
                 results)))

(defn refusal
  "The message a branch reads when acceptance criteria failed: each failing
  criterion by name with the failure's own words, and what was met so the
  branch does not redo it. prompts/acceptance-failed.md. nil when nothing
  failed."
  ([results] (refusal results 25))
  ([results n]
   (when-let [f (seq (failed results))]
     (prompt/render "acceptance-failed"
                    {:failed (mapv (fn [r] (update r :output #(tail % n))) f)
                     :passed (not-empty (mapv :name (filter #(true? (:passed? %)) results)))}))))

(defn configured-commands
  "The shell commands `config` names for a run: :run :verify-cmd and every
  :run :acceptance :check. A malformed :acceptance contributes none — the
  acceptance stage reports that itself. Operator text in config.edn, which no
  tool the agent holds can write: what verify/seal-run! lets a cell run, and
  what a branch's flow label does not stop (samizdat.security.policy)."
  [config]
  (let [checks (try (->> (get-in config [:run :acceptance])
                         normalize
                         (keep #(when (= :check (:kind %)) (:text %))))
                    (catch Throwable _ nil))]
    (into #{} (remove str/blank?) (cons (some-> (get-in config [:run :verify-cmd]) str) checks))))
