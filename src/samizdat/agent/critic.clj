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

(ns samizdat.agent.critic
  "An LLM critic that scores a live branch on explicit objectives, and the
  Pareto machinery the scheduler runs over those scores.

  The cull rule this feeds was a scalar — three consecutive failures, no
  recent confirmation, dead — and three live runs in a row it killed a
  branch the beam arguably wanted: one whose mathematics was right and
  whose Lean proofs merely kept failing. The scores here are judgment
  calls, and are treated as exactly that: the parser fails closed (an
  unusable answer is NO information, never a guessed vector), the
  scheduler falls back to the scalar rule when no scores exist, and every
  score is journaled so a retention decision is auditable after the run.

  Domination — worse-or-equal on every objective, strictly worse on at
  least one — is deliberately the weakest defensible cull criterion. A
  Pareto frontier never discards anything some rational preference could
  still want, so a struggling branch with any unique strength survives.
  The known cost is permissiveness, which is why the reprieve it grants is
  a loan with a clock (the hard floor in the scheduler), not an exemption."
  (:require [clojure.string :as str]
            [samizdat.agent.gates :as gates]
            [samizdat.agent.state :as state]
            [samizdat.llm.message :as message]
            [samizdat.llm.client :as llm]
            [samizdat.prompt :as prompt]
            [samizdat.store.journal :as journal]
            [samizdat.util :as util]))

;; The score prompt is resources/prompts/critic.md (tier 2a), rendered by
;; selmer — the shared samizdat.prompt seam every gate message reads through.

(def objectives
  "All maximize, 1..5. Tier 1b: the list is gates.edn :critic-objectives —
  data, so the rubric is retunable at runtime. What each objective MEANS is
  documented beside the value in gates.edn.

  :progress      engine-confirmed work accumulated over the branch's life
  :momentum      whether the RECENT turns are productive or flailing
  :distinctness  how different this approach is from the sibling theses —
                 the beam's diversity lives here
  :viability     whether the current line has anywhere left to go

  Memoized against the gates generation rather than realized at namespace
  load, so `reload-config!` actually moves it."
  (util/generation-cache gates/gen #(vec (gates/threshold :critic-objectives))))

(defn parse-scores
  "Line-anchored `SCORE <objective>: <1-5>` lines out of a critic response.

  Same discipline as the verdict parser: reasoning is stripped first, prose
  mentioning a score is not a score, several lines for one objective means
  drafts and the last wins, and anything short of all four objectives in
  range returns nil — no information, fail closed."
  [text]
  (let [objs (objectives)
        {:keys [min max]} (gates/threshold :critic-scale)
        ;; Built from the rubric rather than written beside it. The names and
        ;; the range are gates.edn, and a pattern that named them again would
        ;; be a second copy that a retune silently leaves behind.
        ;; \\d+ with a NUMERIC bounds check, not a character class built
        ;; from the range: [1-10] parses as "1, hyphen, 0", so any retune of
        ;; :critic-scale past 9 silently failed every parse and the critic
        ;; went dark (karamazov-blt.38).
        pattern (re-pattern (str "(?i)SCORE\\s+("
                                 (str/join "|" (map name objs))
                                 ")\\s*:\\s*(\\d+)\\s*"))
        t (message/strip-think-blocks text)
        m (into {}
                (keep (fn [line]
                        (when-let [[_ obj v] (re-matches pattern (str/trim line))]
                          (let [n (parse-long v)]
                            (when (and n (<= min n max))
                              [(keyword (str/lower-case obj)) n])))))
                (str/split-lines t))]
    (when (= (count objs) (count m)) m)))

(def survival-objectives
  "The objectives a CULL decision reads: everything except :progress.

  :progress is cumulative, so it rises with age and nothing else. Comparing
  on it lets any mature branch dominate any young one indefinitely — the age
  bias that the juvenile grace period only postpones. It is also the wrong
  question: the artifacts a branch already confirmed are in the log and
  cannot be lost by culling it, so survival should turn on where the line is
  GOING (momentum, viability) and on what it uniquely covers (distinctness).
  A branch that banked a great deal and then stopped moving is exactly the
  one worth reclaiming budget from.

  DERIVED from the rubric — :critic-objectives minus the cumulative ones —
  rather than listed again, which is what it was: a second vector that a
  retune of the rubric silently left behind."
  (util/generation-cache
   gates/gen
   #(let [cumulative (set (gates/threshold :critic-cumulative-objectives))]
      (vec (remove cumulative (objectives))))))

(def fitness-objective
  "The MEASURED objective on the retention frontier (RFC-012 F3): the branch's
  session fitness, `samizdat.session/branch-fitness`, carried in the score
  map under this key beside the critic's judged ones. A key rather than a
  separate argument so `dominated?` reads it like any other objective."
  :fitness)

(defn dominated?
  "True when some sibling is at least as good on every objective the two
  share and strictly better on one. Equal vectors do not dominate.

  The objectives are the survival objectives plus `fitness-objective`, and
  an objective counts only when BOTH vectors carry it: fitness is measured
  per branch and may be unknown for either side (a resumed run, a branch
  that has not taken a turn), and an unknown must neither protect nor
  condemn — the comparison falls back to what both sides do have, which for
  two critic vectors is what it always was. So the branch with the best
  measured trajectory is never Pareto-culled by a sibling the critic merely
  prefers, and a branch the critic likes but that measurably fails still
  can be."
  [scores sibling-scores]
  (let [objectives (conj (vec (survival-objectives)) fitness-objective)]
    (boolean
     (some (fn [o]
             (let [shared (filter #(and (contains? o %) (contains? scores %)) objectives)]
               (and (seq shared)
                    (every? #(>= (get o %) (get scores %)) shared)
                    (some #(> (get o %) (get scores %)) shared))))
           sibling-scores))))

(defn- summary
  "The deterministic facts the critic judges from. Compact on purpose: the
  critic is a per-branch recurring cost."
  [branch siblings]
  (let [confirmed (state/confirmed-artifacts branch)
        measured (state/empirical-artifacts branch)
        recent-user (->> (:messages branch)
                         (filter #(= "user" (:role %)))
                         (take-last 2)
                         (map #(let [c (str (:content %))]
                                 (let [cap (:critic-claim-chars
                                            (gates/threshold :context-budget))]
                                   (subs c 0 (min cap (count c)))))))]
    (str "BRANCH " (:id branch) "\n"
         "Thesis: " (or (get-in branch [:thesis :goal]) "(none registered)") "\n"
         "Turns taken: " (state/turn-count branch)
         "; consecutive failed verifications: "
         (or (:consecutive-failures branch) 0) "\n"
         "Confirmed artifacts: " (count confirmed)
         (when (seq confirmed)
           (str "; the most recent:\n"
                (str/join "\n" (for [a (take-last 3 confirmed)]
                                 (str "  - " (:claim a))))))
         ;; Measurements are listed because leaving them out made a branch
         ;; three hours into a parameter sweep read as a branch that had done
         ;; nothing, and the beam culled it accordingly (vf-0of).
         "\nMeasurements banked: " (count measured)
         (when (seq measured)
           (str "; the most recent:\n"
                (str/join "\n" (for [a (take-last 3 measured)]
                                 (str "  - " (:claim a))))))
         "\n\nSibling theses (the diversity this branch is judged against):\n"
         (if (seq siblings)
           (str/join "\n" (for [s siblings]
                            (str "  - [" (:id s) "] "
                                 (or (get-in s [:thesis :goal]) "(none)"))))
           "  (none — this is the only branch)")
         "\n\nWhat the harness last told the branch:\n"
         (str/join "\n---\n" recent-user))))

(defn score!
  "Score `branch` against its siblings: {:scores {...} :turn turn}, or nil
  when the critic could not answer usably. nil is no information — the
  caller falls back to the scalar rule rather than inventing a vector. A
  usable score is journaled so retention decisions are auditable."
  [{:keys [llm-adapter llm-config conn run-id]} branch siblings turn]
  (let [p (prompt/render "critic" {:summary (summary branch siblings)})
        scores (try
                 (parse-scores
                  (:content (llm/chat llm-adapter llm-config
                                      ;; BOTH halves of the critic's prompt come
                                      ;; from resources. The system half was a
                                      ;; str in this file, which made the
                                      ;; critic the one seam where half the
                                      ;; prompt was editable and half was not.
                                      [{:role "system"
                                        :content (prompt/prompt "critic-system")}
                                       {:role "user" :content p}]
                                      {:temperature 0.0})))
                 (catch Throwable _ nil))]
    (when scores
      (when (and conn run-id)
        (journal/note! conn run-id :critic-score
                       {:branch-id (:id branch) :turn turn :data scores}))
      {:scores scores :turn turn})))
