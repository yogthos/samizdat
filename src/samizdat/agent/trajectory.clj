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

(ns samizdat.agent.trajectory
  "Continuous trajectory scoring (karamazov-fut), after llm-as-a-verifier's
  ProgressTracker: score a branch's steps-so-far on a 20-letter scale with K
  repeats averaged, so a run carries a NUMBER for 'is this converging'
  instead of only the counters the stall gates read. The paper's finding:
  a completed trajectory's score climbs; a doomed one stays flat and low —
  a flat-low curve is the machine-readable signature of a loop.

  DELIBERATELY NOT WIRED TO ANY DECISION YET. The empirical protocol
  (karamazov-0j8) validates the signal first: score finished baseline runs
  offline via `score-run` and check that completed and exhausted runs
  actually separate, before any gate or cull reads the number. The
  :abandon-below threshold ships in gates.edn from day one so the eventual
  wiring is a data edit, but nothing consults it here.

  Mechanism only: the judge is an injected function (an LLM in production, a
  script in tests); criteria, repeats, stride, and threshold are gates.edn
  policy (gates/trajectory-policy); the words are
  resources/prompts/trajectory-judge.md. The judge sees only steps so far —
  it cannot peek ahead — and is told to trust observed output, never the
  agent's narration."
  (:require [clojure.string :as str]
            [samizdat.agent.gates :as gates]
            [samizdat.llm.client :as llm]
            [samizdat.prompt :as prompt]
            [samizdat.store.journal :as journal]
            [samizdat.store.runs :as runs]))

(defn parse-letter
  "The judge's letter, as a value in [0,1]: A = 0 (no progress), T = 1 (goal
  accomplished and verified). Read from the LAST non-blank line, standalone
  letters only, so the judge's prose above cannot collide with the scale.
  nil for an unusable reply — nil and 0.0 mean opposite things, and a broken
  judge must never read as 'no progress'."
  [reply]
  (when (string? reply)
    (when-let [line (->> (str/split-lines reply)
                         (map str/trim)
                         (remove str/blank?)
                         last)]
      (let [hits (->> (re-seq #"[A-Za-z]+" line)
                      (filter #(and (= 1 (count %))
                                    (<= 0 (- (int (first %)) (int \A)) 19))))]
        (when-let [l (last hits)]
          (/ (double (- (int (first l)) (int \A))) 19.0))))))

(defn- snippet [s n]
  (let [s (str s)]
    (if (> (count s) n) (str (subs s 0 n) "…") s)))

(defn steps-digest
  "The steps-so-far block the judge reads: one line per turn — tool, an args
  snippet, and the outcome. Compact on purpose: the judge is scoring the
  SHAPE of the trajectory, and full results would drown it (and the token
  budget) in the very output it is told to distrust the narration of."
  [rows]
  (str/join "\n"
            (map (fn [{:keys [turn tool_name args category result]}]
                   (str "t" turn " " tool_name " " (snippet args 80)
                        " -> " category ": " (snippet result 120)))
                 rows)))

(defn- judge-prompt [{:keys [problem criteria steps]}]
  (prompt/render "trajectory-judge"
                 {:problem problem
                  :criteria (str/join "\n" (map #(str "- " %) criteria))
                  :steps steps}))

(defn score-rows
  "Score a trajectory: every :stride-th step is judged on the rows up to and
  including it, :repeats times, letters averaged. `ask` is the judge —
  prompt string in, reply string out. Returns [{:turn n :score s|nil}] in
  order; a nil score means the judge produced nothing parseable at that
  point, which is a fact about the judge and must never count as progress or
  its absence."
  [ask rows {:keys [problem repeats stride criteria]}]
  (let [rows (vec rows)]
    (mapv (fn [i]
            (let [upto (subvec rows 0 (inc i))
                  p (judge-prompt {:problem problem :criteria criteria
                                   :steps (steps-digest upto)})
                  vals (keep parse-letter (doall (repeatedly repeats #(ask p))))]
              {:turn (:turn (rows i))
               :score (when (seq vals)
                        (/ (reduce + 0.0 vals) (count vals)))}))
          (range (dec stride) (count rows) stride))))

(defn score-run
  "Score one finished (or live) branch's trajectory from the journal — the
  offline validation entry point. ctx needs :conn, :llm-adapter,
  :llm-config; policy comes from gates.edn. Run it over a completed and an
  exhausted run and compare the curves before letting anything act on them."
  [{:keys [conn llm-adapter llm-config]} run-id branch-id]
  (let [rows (journal/branch-turns conn run-id branch-id)
        run (runs/get-run conn run-id)
        ask (fn [p] (:content (llm/chat llm-adapter llm-config
                                        [{:role "user" :content p}])))]
    (score-rows ask rows (assoc (gates/trajectory-policy)
                                :problem (:problem run)))))
