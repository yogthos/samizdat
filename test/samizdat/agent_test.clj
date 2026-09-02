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

(ns samizdat.agent-test
  "Phase 3: the control layer, offline.

  These assertions hold at n=1, which is the point. PR 739 measured a roughly
  2x run-to-run noise floor on identical configurations, so any claim of the
  form \"this reduces turns\" is unmeasurable at an affordable sample size,
  while \"the mechanism fired when it should and stayed silent otherwise\" is
  checkable deterministically."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest testing is are]]
            [samizdat.agent.arbiter :as arbiter]
            [samizdat.agent.beam :as beam]
            [samizdat.agent.critic :as critic]
            [samizdat.agent.gates :as gates]
            [samizdat.agent.loop :as aloop]
            [samizdat.workflow :as wf]
            [samizdat.agent.phases :as phases]
            [samizdat.agent.resume :as resume]
            [samizdat.agent.state :as state]
            [samizdat.agent.tools :as tools]
            [samizdat.agent.tools.base :as tools-base]
            [samizdat.agent.tools.ship :as ship]
            [samizdat.agent.verify :as verify]
            [samizdat.lexicon :as lexicon]
            [samizdat.cells :as cells]
            [samizdat.agent.gitdiff :as gitdiff]
            [samizdat.llm.client :as llm]
            [clojure.data.json :as json]
            [samizdat.store.artifacts :as artifacts]
            [samizdat.store.db :as db]
            [samizdat.store.failures :as failures]
            [samizdat.store.interventions :as interventions]
            [samizdat.store.journal :as journal]
            [samizdat.store.runs :as runs]))

;; The retention and repopulation cascades moved into cells/beam.clj — they are
;; POLICY, and policy is userspace (samizdat-adw). These assertions did not
;; change: same arguments, same expectations, resolved out of the cell's
;; namespace instead of the harness's. Resolved at CALL time rather than at
;; load, because the cell namespace does not exist until the loader has run.
(defn- cell-fn [sym]
  ;; find-ns first: ns-resolve THROWS on an absent namespace rather than
  ;; returning nil, so an `or` never reaches its fallback.
  (when-not (find-ns 'cells.beam) (cells/load-cells!))
  (or (ns-resolve 'cells.beam sym)
      (throw (ex-info (str "cells.beam/" sym " did not load") {}))))

(defn- cull-or-keep [& args] (apply (cell-fn 'cull-or-keep) args))
(defn- repopulate [& args] (apply (cell-fn 'repopulate) args))

;; --- gates and the arbiter --------------------------------------------------

(defn- branch-with [& {:as overrides}]
  (merge (state/new-branch {:id "B1" :problem "p"}) overrides))

(deftest exactly-one-steer-per-boundary
  (testing "several gates can hold at once and exactly one is emitted"
    (let [b (branch-with :consecutive-failures 3
                         :any-progress? true
                         :turns-since-progress 9
                         :artifacts [{:claim "every element of S satisfies P" :claim-status :confirmed :turn 1}]
                         :turns (vec (repeat 5 {})))
          ctx {:branch b :max-turns 40}
          d (arbiter/decide ctx)]
      (is (>= (count (arbiter/eligible ctx)) 3)
          "the scenario has to actually have competing gates or it proves nothing")
      (is (some? d))
      (is (= :emergency-review (:gate d)) "the highest-priority eligible gate wins")
      (is (seq (:passed-over d)) "what was outranked is recorded, not discarded")
      (is (string? (:message d)))
      (is (string? (:prediction d)) "every gate declares what it expects next")))

  (testing "a human directive outranks every machine gate"
    ;; dirge PR 717 as a design property rather than a bug fix.
    (let [b (branch-with :consecutive-failures 5
                         :artifacts [{:claim "every element of S satisfies P" :claim-status :confirmed :turn 1}])
          d (arbiter/decide {:branch b :max-turns 40
                             :directive {:payload "stop and ship what you have"}})]
      (is (= :human-directive (:gate d)))
      (is (str/includes? (:message d) "stop and ship what you have"))))

  (testing "a fresh branch is not nudged"
    (is (nil? (arbiter/decide {:branch (branch-with) :max-turns 40})))))

(deftest gates-stay-silent-when-they-should
  (testing "the stall gate arms only after the branch has made progress"
    ;; Exploration is never nudged; the prologue cap covers the other case.
    (let [never-progressed (branch-with :any-progress? false :turns-since-progress 20)]
      (is (not-any? #{:progress-stalled} (map :gate (arbiter/eligible
                                                     {:branch never-progressed
                                                      :max-turns 40}))))))

  (testing "the prologue cap covers the branch that produced nothing at all"
    ;; The case every other guard is principled-blind to: no failures for the
    ;; stuck gate, no progress event to arm the stall gate.
    ;;
    ;; In :build, because that is where a branch this old actually is — the
    ;; explore cap (5) forces the transition well before the prologue cap (8),
    ;; so the fixture would otherwise be a state the loop never produces. The
    ;; phase is now part of the precondition; see the gate's own note and
    ;; prologue-cap-is-silent-while-a-branch-is-re-planning.
    (let [b (state/enter-phase
             (branch-with :any-progress? false :turns (vec (repeat 8 {}))) 6)]
      (is (= :prologue-cap (:gate (arbiter/decide {:branch b :max-turns 40}))))))

  (testing "a spent budget stops a gate re-firing"
    (let [b (branch-with :artifacts [{:claim "every element of S satisfies P" :claim-status :confirmed :turn 1}]
                         :gate-history [{:gate :milestone :turn 1}])]
      (is (not-any? #{:milestone} (map :gate (arbiter/eligible {:branch b
                                                                :max-turns 40}))))))

  (testing "the emergency review is guarded like every other steer"
    ;; A live knights-3 run re-fired it on three consecutive boundaries, all
    ;; predictions unmet: its precondition persists while the branch is busy
    ;; complying. It was the one steer gate with no re-fire guard.
    (let [b (branch-with :consecutive-failures 3
                         :turns (vec (repeat 4 {}))
                         :artifacts [{:claim "every element of S satisfies P" :claim-status :confirmed :turn 3}])]
      (is (some #{:emergency-review} (map :gate (arbiter/eligible {:branch b
                                                                   :max-turns 40})))
          "fires while at the cull threshold holding a recent confirmation")
      (let [spent (assoc b :gate-history [{:gate :emergency-review :turn 4}])]
        (is (not-any? #{:emergency-review}
                      (map :gate (arbiter/eligible {:branch spent :max-turns 40})))
            "and once is all it gets")))))

(deftest wind-down-steers-the-branch-to-ship
  (testing "fires at and past the wind-down fraction of the turn cap"
    (let [ctx-at (fn [turns]
                   {:branch (branch-with :turns (vec (repeat turns {})))
                    :max-turns 40})]
      (is (not-any? #{:wind-down} (map :gate (arbiter/eligible (ctx-at 33))))
          "silent below the fraction")
      (let [d (arbiter/decide (ctx-at 34))] ; 34/40 = 0.85
        (is (= :wind-down (:gate d)) "fires exactly at the fraction")
        (is (string? (:message d)))
        (is (str/includes? (:message d) "at turn 34 of 40")
            "the steer says where the branch stands")
        (is (string? (:prediction d)) "every gate declares what it expects next")
        (is (some #{:turn-budget :prologue-cap} (:passed-over d))
            "the ship steer outranks the plain notices")))
    (testing "fires once per branch; a spent re-fire guard keeps it silent"
      (let [b (branch-with :turns (vec (repeat 36 {}))
                           :gate-history [{:gate :wind-down :turn 34}])]
        (is (not-any? #{:wind-down}
                      (map :gate (arbiter/eligible {:branch b :max-turns 40})))))))

  (testing "silent on a branch that already shipped"
    (let [b (branch-with :turns (vec (repeat 36 {}))
                         :status :done :final-answer "x")]
      (is (not-any? #{:wind-down}
                    (map :gate (arbiter/eligible {:branch b :max-turns 40}))))))

  (testing "done-blocked outranks it when both hold"
    (let [b (branch-with :turns (vec (repeat 36 {}))
                         :artifacts [{:claim "every element of S satisfies P" :claim-status :confirmed :turn 1}])
          d (arbiter/decide {:branch b :max-turns 40
                             :done-block "`done` refused.\n\nNo confirmed artifact."})]
      (is (= :done-blocked (:gate d)) "the correctness rung wins, not the budget steer")
      (is (some #{:wind-down} (:passed-over d))))))

(deftest last-call-forces-a-ship-attempt-in-the-final-turns
  ;; karamazov-pab: a live team run had every worker exhaust its cap with no
  ;; done or give_up — wind-down asked, the model ignored it. This rung forces
  ;; the attempt in the final turns.
  (testing "in the last turns it outranks wind-down and forces a done call"
    (let [b (branch-with :turns (vec (repeat 39 {})))   ; 39/40 — within the last 2
          d (arbiter/decide {:branch b :max-turns 40})]
      (is (= :last-call (:gate d)) "the forced ship beats the soft wind-down")
      (is (some #{:wind-down} (:passed-over d)))
      (is (= "```tool-call\n{\"name\": \"done\"" (arbiter/prefill-for d))
          "it prefills a done call, so the model cannot explore its last turns away")))
  (testing "silent before the window — wind-down still handles the earlier stretch"
    (let [b (branch-with :turns (vec (repeat 34 {})))]  ; 34/40 — past 0.85, not last-2
      (is (not-any? #{:last-call}
                    (map :gate (arbiter/eligible {:branch b :max-turns 40}))))))
  (testing "silent on a branch that already shipped"
    (let [b (branch-with :turns (vec (repeat 39 {})) :status :done :final-answer "x")]
      (is (not-any? #{:last-call}
                    (map :gate (arbiter/eligible {:branch b :max-turns 40}))))))

  (testing "it forces the done via native tool_choice, not a prefill (karamazov-9se)"
    (let [d (arbiter/decide {:branch (branch-with :turns (vec (repeat 39 {}))) :max-turns 40})]
      (is (= "done" (:name (arbiter/force-tool-for d)))
          "the forced tool is done, sent as a native function")
      (is (nil? (arbiter/force-tool-for {:gate :wind-down}))
          "a gate that names no tool forces nothing"))))

(deftest gate-config-is-coherent
  (testing "every gate with a budget names a threshold that exists"
    (doseq [g (gates/gates) :when (:budget g)]
      (is (some? (gates/threshold (:budget g)))
          (str (:gate g) " names a budget with no entry in gates.edn"))))

  (testing "the stuck gate fires strictly before the branch is cullable"
    ;; vf-31m. Both were 3, so the gate that says change your approach landed
    ;; on the turn the branch became killable for not having changed it, and
    ;; every move it predicts costs at least a turn. Asserted here rather than
    ;; left to the two numbers agreeing by habit.
    (is (< (gates/threshold :stuck-threshold) (gates/threshold :cull-threshold))))

  (testing "priorities are unique, or the arbiter's choice is arbitrary"
    (let [ps (map :priority (gates/gates))]
      (is (= (count ps) (count (distinct ps))))))

  (testing "every gate can be settled — a vocabulary entry or a rule of its own"
    ;; The budget check above has a sibling this lacked: a gate added to
    ;; gates.edn :gates with no :tool-vocab :settle-called entry and no
    ;; explicit clause in arbiter/settle used to hand `nil` to `some` as a
    ;; predicate and throw on its first firing. `settle` now defaults to #{},
    ;; so the failure mode is a prediction that can only expire — quieter and
    ;; still wrong. Name it here instead.
    (let [vocab (gates/tool-vocab :settle-called)]
      (doseq [g (gates/gates)]
        (is (or (contains? vocab (:gate g))
                (contains? arbiter/settled-by-rule (:gate g)))
            (str (:gate g) " has no :settle-called vocabulary and no rule in"
                 " arbiter/settle — its prediction can only ever expire")))))

  (testing "arbiter/settled-by-rule names only gates that exist"
    (let [known (set (map :gate (gates/gates)))]
      (doseq [g arbiter/settled-by-rule]
        (is (contains? known g)
            (str g " is claimed as rule-settled but is not a gate")))))

  (testing "the capability tier may not tune a verification or progress guard"
    ;; PR 740's rule: a signal may only tune a guard that fires on the same
    ;; thing the signal measures. The tier observes tool-call mechanics only.
    (doseq [k [:cull-threshold :stuck-threshold :progress-stall-threshold
               :prologue-cap]]
      (is (false? (get-in (gates/config) [k :capability-tunable?]))
          (str k " must not be capability-tunable"))))

  (testing "no cost ceiling is capability-tunable"
    ;; Scaling one up for a struggling run means spending more on the run
    ;; already in trouble.
    (doseq [[k v] (gates/config) :when (= :cost-ceiling (:kind v))]
      (is (false? (:capability-tunable? v)) (str k " is a cost ceiling")))))

(deftest every-gate-that-fires-can-actually-be-met
  ;; vf-9bo. settle dispatches on gate in a case whose fallthrough is false, so
  ;; a gate missing from that case is not merely unhandled — its prediction can
  ;; never come true. It fires, expires, and is recorded :unmet, which reads
  ;; off the gate table as a gate the model ignores when the harness never
  ;; checked. safe-state sat at 0 met / 2 unmet across gen-22/23/24 for exactly
  ;; this reason.
  ;;
  ;; The same shape as the failures charged to the cull counter in vf-jki: a
  ;; tally blaming the branch for something nobody measured. This asserts the
  ;; dispatch covers the table, so adding a gate without a settle rule fails
  ;; here rather than silently three generations later.
  (let [b (branch-with)
        every-tool (vec (tools/tool-names))]
    (doseq [{:keys [gate]} (gates/gates)]
      (is (= :met (arbiter/settle {:gate gate :turn 1 :window 3}
                                  {:current-turn 2
                                   :tools-called every-tool
                                   :branch-before b
                                   :branch-after (branch-with
                                                  :artifacts
                                                  [{:claim "every element of S satisfies P"
                                                    :claim-status :confirmed}])}))
           (str "gate " gate " has no way to be met; it can only expire unmet")))))

(deftest settle-and-tool-vocab-names-are-registered-tools
  ;; provenance R3-6/#7. The proof-era tool names (verify_template, review, audit,
  ;; sketch, retract_rule, proof_*, octave_eval) outlived the tool surface
  ;; that served them. A settle name the loop can never dispatch is a gate
  ;; whose settle silently narrows — and vf-9bo's every-tool probe above
  ;; cannot catch that, because a clause with one live name among three dead
  ;; ones still settles :met. This walks the vocabularies themselves — since
  ;; the tier-1a migration they live in gates.edn as :tool-vocab — and
  ;; asserts every name is a run-tool the loop can actually dispatch.
  (let [registered (set (tools/tool-names))
        names-fn (resolve 'samizdat.agent.arbiter/settle-called-names)]
    (is (some? names-fn)
        "settle-called-names exists — the settle vocabulary is walkable data")
    (when names-fn
      (doseq [n (@names-fn)]
        (is (contains? registered n)
            (str "settle reads `" n "` — no run-tool method dispatches it"))))
    (doseq [vocab [:verification :shipping :file-write]
            n (gates/tool-vocab vocab)]
      (is (contains? registered n)
          (str "tool-vocab " vocab " names `" n "` — no run-tool method dispatches it")))))

(deftest tool-vocab-is-gates-edn-data
  ;; Tier 1a: the vocabularies the gates read (settle's compliance tools,
  ;; verification, shipping, file-write) moved out of src defs into
  ;; resources/gates.edn, so a vocabulary changes at runtime without a
  ;; rebuild. Pin the move: the keys exist, carry the live vocabulary, and
  ;; settle's table still keys by gate.
  (is (= #{"eval" "shell"} (gates/tool-vocab :verification)))
  (is (contains? (gates/tool-vocab :shipping) "write_file"))
  ;; EVERY tool that writes a file must be in both, or a branch using it
  ;; scores as having shipped nothing and gets nudged for work it is doing.
  ;; `patch` was added to the loop and to neither, which is exactly the shape
  ;; of the omission this now pins: assert the RULE, not a frozen set, so the
  ;; next write tool fails here rather than silently going uncounted.
  (doseq [t ["write_file" "edit_file" "patch"]]
    (is (contains? (gates/tool-vocab :file-write) t)
        (str t " writes files but is missing from :file-write"))
    (is (contains? (gates/tool-vocab :shipping) t)
        (str t " writes files but is missing from :shipping")))
  (let [settle-called (gates/tool-vocab :settle-called)]
    (is (map? settle-called))
    (is (= #{"done"} (:milestone settle-called)))
    (is (= #{"thesis" "done" "give_up"} (:safe-state settle-called)))))

(deftest policy-scalars-are-gates-edn-data
  ;; Tier 1b: the policy numbers that lived as hard-coded multipliers and defs
  ;; in src — the beam's mechanics-cull multiple, the safe-state multiple, the
  ;; thesis fork cap, the reflection cadence, the critic's objective list,
  ;; decompose's budgets — moved to gates.edn beside the thresholds they
  ;; parameterize, so a policy number changes at runtime without a rebuild.
  (is (= 2 (gates/threshold :cull-mechanics-multiple)))
  (is (= 2 (gates/threshold :safe-state-multiple)))
  (is (= 4 (gates/threshold :max-branch-theses)))
  (is (= [:progress :momentum :distinctness :viability]
         (gates/threshold :critic-objectives)))
  (is (= 3 (gates/threshold :decompose-max-depth)))
  (is (= 4 (gates/threshold :decompose-max-parts))))

(deftest wordlists-are-resource-data
  ;; Tier 1c: the curated wordlists — the relevance filter's stopwords
  ;; (state.clj advances-thesis?) and the answer gate's framing stopwords +
  ;; tool-version pattern (ship.clj answer-tokens) — moved out of src defs
  ;; into resources/wordlists.edn, so a list is retuned at runtime without a
  ;; rebuild. A separate loader from gates.clj because state.clj sits below
  ;; gates in the require graph and cannot read its accessor.
  (is (set? (lexicon/wordlist :claim-relevance)))
  (is (contains? (lexicon/wordlist :claim-relevance) "the"))
  (is (set? (lexicon/wordlist :answer-framing)))
  (is (contains? (lexicon/wordlist :answer-framing) "mathlib"))
  (is (string? (lexicon/wordlist :tool-version)))
  (is (re-find (re-pattern (lexicon/wordlist :tool-version)) "Python 3.11"))
  (is (nil? (re-find (re-pattern (lexicon/wordlist :tool-version))
                     "witness 3"))))

(deftest a-reframed-branch-settles-stuck-with-a-live-verification
  ;; The stuck gate's compliance clause: a verification the harness ACCEPTED
  ;; while the reframe stood is compliance by construction — the withheld
  ;; claim is still refused, so the call that got through was on a different
  ;; one. It has been dead since the proof tools left: verification-tools
  ;; named nine unregistered tools, so the some? never found a call. On the
  ;; coding loop the engines are eval and shell; the clause must fire on one.
  (let [b (assoc (branch-with) :reframe-entered-turn 4)]
    (is (= :met (arbiter/settle {:gate :stuck :turn 4 :window 3}
                                {:current-turn 5
                                 :tools-called ["eval"]
                                 :branch-before b
                                 :branch-after b})))))

(deftest gates-config-carries-no-keys-for-removed-machinery
  ;; provenance R3-9: :sketch-duplicate-threshold documented the sketch diversity
  ;; gate (vf-eaw) whose tool left with the proof harness. The code it points
  ;; at hardcodes 0.6 and its query is test-only — the key is doc-rot, the
  ;; same class as the four keys pass 2 deleted.
  (gates/reload-config!)
  (is (nil? (get (gates/config) :sketch-duplicate-threshold))
      ":sketch-duplicate-threshold still served by /v1/harness/gates"))

;; --- the done gate ----------------------------------------------------------

(deftest answer-must-be-covered-by-evidence
  (testing "a number in the answer that appears in no artifact is uncovered"
    ;; The deterministic claim-evidence gate. An answer asserting something no
    ;; artifact mentions is a fabricated verification report (dirge PR 749).
    (let [artifacts [{:claim "the set has size 23" :code "(assert (= n 23))" :witness nil}]]
      (is (empty? (tools/uncovered-tokens "the answer is 23" artifacts)))
      (is (= ["24"] (tools/uncovered-tokens "the answer is 24" artifacts)))))

  (testing "stopwords and short words are not evidence claims"
    (is (empty? (tools/uncovered-tokens "the answer is that it exists"
                                        [{:claim "every element of S satisfies P" :code "" :witness nil}]))))

  (testing "grammar that slipped through the inflections is not an assertion"
    ;; A live refusal listed `does`, `follow`, `from` and `having` beside the
    ;; genuine catches, telling the branch to verify or remove the word "from".
    ;; `follows`, `have` and `has` were already stopwords; their other forms
    ;; were not, and `from` and `does` were missing outright. All four are
    ;; framing by the list's own test — none can carry a specific claim — so
    ;; adding them costs the gate nothing.
    (let [artifacts [{:claim "the minimum is 2" :code "" :witness nil}]]
      (is (empty? (tools/uncovered-tokens
                   "this does follow from having the minimum 2" artifacts)))
      (is (= ["residue"] (tools/uncovered-tokens
                          "this does follow from having a residue" artifacts))
          "and the substantive term is still caught")))

  (testing "the witness counts as evidence, not only the claim text"
    (is (empty? (tools/uncovered-tokens "a is knave"
                                        [{:claim "solved" :code ""
                                          :witness [{:A "knave"}]}])))))

(deftest coverage-does-not-refuse-the-words-an-honest-partial-answer-needs
  ;; vf-w2k. B4 of run 0d0c3560 called `done` eight times over twenty turns
  ;; with a PASSING audit, and was refused six times for asserting `stated`,
  ;; `facts`, `against`, `together`, `asked`, `settled`, `establish`,
  ;; `evidence`, `found`, `showed`. None of those is an assertion.
  ;;
  ;; Worse, the relevance rung added the same morning TELLS a branch to state
  ;; which questions it did not settle — and the thing you failed to establish
  ;; is, by construction, absent from your evidence. The two rungs could not
  ;; both be satisfied.
  (let [artifacts [{:claim "enumerating all 2^12 assignments gives 8 flows of cost 6"
                    :code "" :witness nil}
                   {:claim "the residue at the top-left square is +1" :code "" :witness nil}]]

    (testing "framing vocabulary is not an assertion"
      (is (empty? (tools/uncovered-tokens
                   (str "Taken together, what the evidence establishes is 8 flows."
                        " The general question was asked but remains unsettled.")
                   artifacts))))

    (testing "the problem's own vocabulary is never a fabrication"
      ;; The rung exists to catch an answer asserting a number or a name that
      ;; appears nowhere in the evidence — a fabricated verification report.
      ;; A term the HARNESS put in front of the model cannot be fabricated by
      ;; it. B4 of run 0d0c3560 was refused for `polynomial-time`,
      ;; `canonical-selection-rule` and `recovery-guarantee`, every one of them
      ;; lifted from the problem it had been asked to solve, and every one
      ;; unavoidable in saying which parts of that problem it had not reached.
      (let [problem (str "Is there a canonical selection rule, computable in"
                         " polynomial time? No recovery guarantee exists for"
                         " 2D unwrapping in any form.")
            flagged (set (tools/uncovered-tokens
                          (str "No canonical-selection-rule is given and"
                               " polynomial-time computability and the"
                               " recovery-guarantee are not reached.")
                          artifacts problem))]
        (is (empty? flagged) (str "still flagged: " (pr-str flagged))))
      (testing "a hyphenated compound of framing words is still framing"
        ;; `lean-verified` and `engine-confirmed` are single tokens, so both
        ;; halves being stopwords did not save them. They are provenance —
        ;; the one thing an artifact can never mention, since an artifact is
        ;; about the problem and says nothing about the engine that ran it.
        (let [flagged (set (tools/uncovered-tokens
                            "what is lean-verified and engine-confirmed is stated above"
                            artifacts))]
          (is (empty? flagged) (str "still flagged: " (pr-str flagged))))
        (is ((set (tools/uncovered-tokens "the optimal-flow set is large" artifacts))
             "optimal-flow")
            "but a compound with a substantive half is not exempt"))

      (testing "but a term from outside the problem AND the evidence is caught"
        (is ((set (tools/uncovered-tokens
                   "the vortices unbind at the transition"
                   artifacts "a canonical selection rule in polynomial time"))
             "vortices"))))

    (testing "naming what you did NOT settle needs the audit's own restatement"
      ;; The hard half, and the one stopwords cannot reach. `polynomial-time`
      ;; is a substantive term and it is absent from the evidence for the only
      ;; reason that matters: nobody established it. The relevance rung asks
      ;; the branch to say so. The audit — which has passed, and whose job was
      ;; to state what the evidence does and does not cover — is where that
      ;; sentence is licensed from.
      (let [answer "polynomial-time computability is not settled here"]
        (is (seq (tools/uncovered-tokens answer artifacts))
            "with nothing but artifacts, the honest sentence is refused")
        (is (empty? (tools/uncovered-tokens
                     answer artifacts
                     (str "8 flows of cost 6 on that graph; polynomial-time"
                          " computability of the tie-break is not established")))
            "and the passing audit's ESTABLISHED line licenses it")))

    (testing "an inflection of a word in the evidence is covered"
      ;; str/includes? on the raw haystack sees none of these.
      (let [flagged (set (tools/uncovered-tokens
                          "the residues and the enumeration give 8 flows"
                          artifacts))]
        (is (not (flagged "residues")) "evidence says `residue`")
        (is (not (flagged "enumeration")) "evidence says `enumerating`")))

    (testing "a hyphen is not a different word"
      (is (not ((set (tools/uncovered-tokens "the top-left residue is +1" artifacts))
                "top-left"))))

    (testing "a number in no artifact is still refused — that is what this is for"
      (is (= ["9"] (tools/uncovered-tokens "there are 9 flows" artifacts)))
      (is ((set (tools/uncovered-tokens "the threshold is 0.31" artifacts)) "0.31")))

    (testing "a substantive word in no artifact is still refused"
      (is ((set (tools/uncovered-tokens "a vortex is present" artifacts)) "vortex")))

    (testing "the passing audit's own restatement covers WORDS but never numbers"
      ;; The audit is a gate that already passed on the merits and its
      ;; ESTABLISHED line is the harness's account of what the evidence shows,
      ;; so an answer may echo its prose. It may not inherit its arithmetic:
      ;; a number still has to come from an artifact, which is the whole
      ;; reason this rung exists.
      (let [established (str "the lexicographic tie-break selects a unique flow"
                             " whenever the optimal set is finite, and 42 of them exist")]
        (is (empty? (tools/uncovered-tokens
                     "the lexicographic tie-break selects a unique flow"
                     artifacts established)))
        (is (= ["42"] (tools/uncovered-tokens
                       "there are 42 flows" artifacts established))
            "a number the audit restated but no engine confirmed is not covered")))))

;; --- the answer has to be an answer to THIS problem -------------------------
;;
;; vf-eq9. The phase-unwrapping run shipped, as its answer to "when can 2D
;; phase unwrapping be done exactly, and by a polynomial-time algorithm?", a
;; true statement about four oriented edges of a 4-cycle. No phase field, no
;; noise parameter, no torus, no threshold, no algorithm.
;;
;; Both existing gates passed it, correctly by their own criteria. The audit
;; answered GAPS: none, because it compares the answer to the THESIS and the
;; thesis had itself drifted. The coverage check found every substantive token
;; in a confirmed artifact, because the answer WAS a confirmed artifact,
;; verbatim. Nothing in the harness asked whether it answers the question.

(def ^:private unwrap-problem
  (str "When can two-dimensional phase unwrapping be done exactly, and by a"
       " polynomial-time algorithm? Locate the noise threshold sigma at which"
       " exact recovery of the wrapped field on the torus becomes impossible,"
       " and say whether the unwrapper everyone ships attains it. That"
       " unwrapper minimises a weighted sum over integer flows on the dual"
       " grid, which is a minimum cost flow and runs in polynomial time."))

(defmacro with-db [[binding] & body]
  `(let [~binding (db/open! ":memory:")]
     (try ~@body (finally (db/close ~binding)))))

(deftest gate-firing-id-is-settleable
  ;; record-gate! returned the EVENT id rather than the gate_firings id, so
  ;; every settle updated a row that did not exist and the whole tally stayed
  ;; permanently open. Caught by reading a live run's journal.
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p"})
          _ (runs/open-branch! c rid {:branch-id "B1"})
          id (journal/record-gate! c rid {:branch-id "B1" :turn 1 :gate :milestone
                                          :prediction "calls review" :window 2})]
      (is (= 1 (count (journal/unsettled-gates c rid "B1"))))
      (journal/settle-gate! c id :met 2)
      (is (empty? (journal/unsettled-gates c rid "B1")))
      (is (= 1 (:met (first (journal/gate-tally c rid))))))))

(deftest the-fork-invite-floor-gates-repopulation
  ;; provenance R2-13: :fork-invite-floor documented minimum critic scores for a
  ;; fork invitation and nothing read it — the scheduler invited the
  ;; strongest survivor even when every survivor sat below the floor,
  ;; spending budget on lines not yet earning it.
  (let [floor (gates/threshold :fork-invite-floor)
        weak [{:id "B1" :status :active :critic {:scores {:viability 1 :progress 1}}}]
        strong [{:id "B1" :status :active :critic {:scores {:viability 1 :progress 1}}}
                {:id "B2" :status :active :critic {:scores {:viability 9 :progress 8}}}]]
    (is (map? floor) "the floor is configured")
    (is (every? (fn [[obj minimum]]
                  (< (get-in (first weak) [:critic :scores obj] 0) minimum))
                floor)
        "sanity: the weak fixture is below the configured floor")
    (is (empty? (filter :fork-invited
                        (repopulate {:beam-width 3} weak 2 5)))
        "no survivor above the floor: nobody is invited to reseed")
    (is (= ["B2"] (mapv :id (filter :fork-invited
                                    (repopulate {:beam-width 3} strong 2 5))))
        "a survivor above the floor is still invited")))

;; --- the explore/build phase machine (vf-b25, vf-eaw) ----------------------

(deftest a-new-branch-starts-in-explore
  (let [b (state/new-branch {:id "B1" :problem "p" :created-at-turn 3})]
    (is (= :explore (:phase b)))
    (is (= 3 (:phase-entered-turn b))
        "the phase clock starts at branch creation, so a forked branch gets
         a full explore budget instead of inheriting its parent's spent one")))

(deftest enter-phase-stamps-the-phase-once
  (let [b (state/enter-phase (state/new-branch {:id "B1" :problem "p"}) 3)]
    (is (= :build (:phase b)))
    (is (= 3 (:phase-entered-turn b)))
    (is (= 3 (:phase-entered-turn (state/enter-phase b 9)))
        "already in build: the phase did not begin again, so the stamp does
         not move")))

(deftest explore-cap-expires-after-cap-full-turns
  (let [b (state/new-branch {:id "B1" :problem "p" :created-at-turn 0})]
    (is (not (state/explore-cap-expired? b 5 5))
        "turns 1-5 are the five explore turns the cap allows")
    (is (state/explore-cap-expired? b 5 6))
    (is (not (state/explore-cap-expired? (state/enter-phase b 3) 5 20))
        "only the explore phase is capped")))

(deftest steer-prose-lives-in-prompt-files
  ;; Tier 2d: the scheduler's steer prose — the explore→build valve message,
  ;; the juvenile-grace spare, the Pareto reprieve, the crossover context —
  ;; moved from loop/beam string literals to resources/prompts/, the same
  ;; seam every gate message reads through. End-to-end where the public
  ;; seam is cheap: an unsubstituted {{...}} placeholder reaching the model
  ;; is the failure mode the pins exclude.
  (with-redefs [gates/threshold (fn [_] 5)]
    (let [b (state/new-branch {:id "B1" :problem "p" :created-at-turn 0})
          out (aloop/phase-valve b 6)
          msg (-> out :messages peek :content)]
      (is (str/includes? msg "BUILD phase"))
      (is (str/includes? msg "green test run"))
      (is (not (str/includes? msg "{{")))))
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p"})
          ctx {:conn c :run-id rid}
          juvenile (-> (branch-with :id "B1.2" :consecutive-failures 3
                                    :created-at-turn 15
                                    :critic {:scores {:progress 1 :momentum 2
                                                      :distinctness 5 :viability 4}
                                             :turn 16})
                       (assoc :turns (vec (repeat 3 {}))))
          spared (cull-or-keep ctx juvenile 2
                                      [{:progress 5 :momentum 5
                                        :distinctness 5 :viability 4}])
          jm (-> spared :messages peek :content)]
      (is (state/active? spared))
      (is (str/includes? (str/replace jm #"\s+" " ")
                         "A branch this new is not culled for it"))
      (is (not (str/includes? jm "{{"))))
    (let [rid (runs/start-run! c {:problem "p"})
          ctx {:conn c :run-id rid}
          reprieved (-> (branch-with :id "B2" :consecutive-failures 3
                                     :created-at-turn 0
                                     :critic {:scores {:progress 2 :momentum 2
                                                       :distinctness 4 :viability 3}
                                              :turn 20})
                        (assoc :turns (vec (repeat 20 {}))))
          kept (cull-or-keep ctx reprieved 2 [])
          rm (-> kept :messages peek :content)]
      (is (state/active? kept) "no dominating sibling, so the reprieve holds")
      (is (str/includes? rm "reprieve ends unconditionally at"))
      (is (str/includes? rm (str (* (gates/threshold :cull-hard-multiple)
                                    (gates/threshold :cull-threshold))))
          "the hard floor is interpolated from the config numbers")
      (is (not (str/includes? rm "{{"))))
    (let [file (slurp (clojure.java.io/resource "prompts/crossover.md"))]
      (is (str/includes? file "**Confirmed by other lineages in this run**"))
      (is (str/includes? file "{{artifacts}}")))))

;; --- the forced reframe (vf-9wx, vf-31m, vf-49o) -----------------------------
;;
;; The harness's only answer to repeated failure was to kill the branch. These
;; tests pin the cheaper alternative that runs first: the approach that keeps
;; failing is WITHHELD, the branch is given room to propose a different one,
;; and the cull stays as the backstop.

(deftest the-stuck-gate-fires-with-room-left-to-obey-it
  ;; vf-31m. stuck-threshold and cull-threshold were both 3, so the hint that
  ;; says change your approach arrived on the turn the branch became eligible
  ;; to be killed for not having changed it. Every move it predicts — retract,
  ;; decompose, change technique — costs at least one turn.
  (is (< (gates/threshold :stuck-threshold) (gates/threshold :cull-threshold))
      "the advice has to arrive before the execution or it cannot be obeyed")
  (let [at-stuck (branch-with :consecutive-failures (gates/threshold :stuck-threshold)
                              :turns (vec (repeat 10 {})))]
    (is (some #{:stuck} (map :gate (arbiter/eligible {:branch at-stuck :max-turns 40})))
        "the gate fires at its own threshold")
    (is (= :active (:status (cull-or-keep {:turn 10} at-stuck 2 [])))
        "and the branch is not yet cullable when it does")))

(deftest a-lookup-miss-is-not-a-mathematical-failure
  ;; gen-31 B1, turn 25 (2026-08-18). B1 was the only branch attempting the
  ;; run's actual target. It failed verify_lean on a wrong lemma name, then
  ;; fetched a#777 using the wrong id namespace and was told so — and those two
  ;; together tripped the stuck gate, which withheld TARGET 1 step 4, the whole
  ;; point of the run, from the only branch working on it.
  ;;
  ;; fetch_artifact is documented as deliberately :neutral because "a lookup
  ;; establishes nothing". That cuts both ways: a lookup that finds nothing
  ;; refutes nothing. A bad id is a call made wrong, which is what :mechanics
  ;; counts, and it must not reach the counter that culls branches and now also
  ;; withholds their claims. Same principle as vf-jki.
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p"})
          b (state/new-branch {:id "B1" :problem "p"})]
      (runs/open-branch! c rid {:branch-id "B1"})
      (let [r (tools/run-tool {:branch b :conn c :run-id rid
                               :tool-name "fetch_artifact" :args {:id "a#777"}})]
        (is (= :mechanics (:category r))
            "a miss is a call made wrong, not evidence about mathematics")
        (is (str/includes? (:result r) "a#12")
            "and it still says how ids are spelled"))
      (let [r (tools/run-tool {:branch b :conn c :run-id rid
                               :tool-name "fetch_turn" :args {:id "t999"}})]
        (is (= :mechanics (:category r)) "same for a turn that is not there")))))

(deftest fetch-turn-reads-another-branch-by-name
  ;; The run-health digest hands the supervisor "turn 3 (T0, ...)"; without
  ;; the :branch arg the reader could not open the very record the report
  ;; names — a digest pointing at turns its reader cannot reach.
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p"})
          sup (state/new-branch {:id "S0" :problem "p"})]
      (runs/open-branch! c rid {:branch-id "S0"})
      (runs/open-branch! c rid {:branch-id "T0"})
      (journal/record-turn! c rid {:branch-id "T0" :turn 3 :tool-name "shell"
                                   :args "{\"command\": \"make\"}"
                                   :result "make: no rule to make target"
                                   :category "failure"})
      (testing "an explicit branch opens the named record"
        (let [r (tools/run-tool {:branch sup :conn c :run-id rid
                                 :tool-name "fetch_turn"
                                 :args {:turn 3 :branch "T0"}})]
          (is (= :neutral (:category r)))
          (is (str/includes? (str (:result r)) "make: no rule"))
          (is (str/includes? (str (:result r)) "[T0]")
              "labelled, so the reader knows whose turn it is looking at")))
      (testing "the default stays own-branch"
        (let [r (tools/run-tool {:branch sup :conn c :run-id rid
                                 :tool-name "fetch_turn" :args {:turn 3}})]
          (is (= :mechanics (:category r)) "S0 has no turn 3 of its own"))))))

;; --- the ship gate's test rung: done is not terminal until tests pass --------

(deftest done-is-a-hard-gate-on-a-green-test-run
  (let [answer "gated the remember tool so a done with an empty diff is refused"
        ship (fn [ctx] (tools/run-tool (merge {:branch (state/new-branch {:id "B1" :problem "gate the remember tool"})
                                               :tool-name "done" :turn 3
                                               :root "/tmp" :git-baseline "HEAD"
                                               :args {:answer answer}}
                                              ctx)))]
    (testing "a red run refuses done and feeds the failure back — the branch stays unshipped"
      (with-redefs [verify/run-verify (fn [_ _ _] {:green? false :output "FAIL: 1 assertion failed"})
                    gitdiff/changed-files (fn [_ _] ["src/x.clj" "test/x_test.clj"])]
        (let [r (ship {:config {:run {:verify-cmd "jolt -M:test"}}})]
          (is (not (:done? r)) "done was refused")
          (is (nil? (:final-answer (:branch r))) "nothing shipped")
          (is (str/includes? (:result r) "not green"))
          (is (str/includes? (:result r) "assertion failed") "the real failure output is fed back"))))

    (testing "a green run with a test among the changes accepts done"
      (with-redefs [verify/run-verify (fn [_ _ _] {:green? true :output ""})
                    gitdiff/changed-files (fn [_ _] ["src/x.clj" "test/x_test.clj"])]
        (let [r (ship {:config {:run {:verify-cmd "jolt -M:test"}}})]
          (is (:done? r) "done accepted")
          (is (= answer (:final-answer (:branch r)))))))

    (testing "TDD: a green run that added no test is refused"
      (with-redefs [verify/run-verify (fn [_ _ _] {:green? true :output ""})
                    gitdiff/changed-files (fn [_ _] ["src/x.clj"])]
        (let [r (ship {:config {:run {:verify-cmd "jolt -M:test"}}})]
          (is (not (:done? r)))
          (is (str/includes? (str/lower-case (:result r)) "test")))))

    (testing "no :verify-cmd configured => the test rung does not apply (backward compatible)"
      (let [called (atom false)]
        (with-redefs [verify/run-verify (fn [_ _ _] (reset! called true) {:green? false :output "x"})]
          (let [r (ship {:config {:run {}}})]
            (is (:done? r) "ships without a verify command, as before")
            (is (not @called) "and the verify command was never run")))))))

(deftest the-reframe-reprieve-is-a-loan-with-a-clock
  ;; vf-31m. A branch dropped into a reframe carries the failures that caused
  ;; it, so without this it is culled mid-reframe for the very approach it was
  ;; just told to abandon. The reprieve is bounded: Pareto's known weakness is
  ;; permissiveness and a zombie beam is the failure mode.
  (let [grace (gates/threshold :reframe-grace)
        failing (-> (branch-with :consecutive-failures (gates/threshold :cull-threshold))
                    (assoc :turns (vec (repeat 10 {})))
                    (state/enter-reframe 10 "the greedy exchange terminates"))]
    (is (= :active (:status (cull-or-keep {:turn 11} failing 2 [])))
        "spared while it is re-planning")
    (let [expired (cull-or-keep {:turn (+ 10 grace)} failing 2 [])]
      (is (= :culled (:status expired)) "the loan comes due")
      (is (str/includes? (:inactive-reason expired) "reframe")
          "and the record says the branch had already been given its chance"))
    (testing "the hard floor ends it regardless"
      ;; A branch still failing at twice the cull threshold is not reframing.
      (let [floored (assoc failing :consecutive-failures
                           (* (gates/threshold :cull-hard-multiple)
                              (gates/threshold :cull-threshold)))
            r (cull-or-keep {:turn 11} floored 2 [])]
        (is (= :culled (:status r)))
        (is (str/includes? (:inactive-reason r) "reframe")
            "the reason names what actually happened; the cull reasons are the
             run's post-hoc explanation of itself and are read later as evidence")))
    (testing "a branch with no reframe is culled exactly as before"
      (is (= :culled (:status (cull-or-keep
                               {:turn 11}
                               (dissoc failing :reframe-claim :reframe-entered-turn)
                               2 [])))))))

(deftest prologue-cap-is-silent-while-a-branch-is-re-planning
  ;; vf-9wx note 1. A banked sketch is deliberately :neutral with progress?
  ;; false — a plan is not progress — so a branch dropped back into explore at
  ;; turn 40 accrues nothing while it re-plans and would be told "you are 41
  ;; turns in with nothing verified", which is true and useless. The guard is
  ;; ordered so the branch that wasted its explore budget gets LESS rope, not
  ;; more: sketch immediately and you get the full build allowance before the
  ;; nudge; burn the whole prologue and you get what is left.
  (let [replanning (branch-with :any-progress? false :turns (vec (repeat 12 {})))
        building (state/enter-phase replanning 6)]
    (is (= :explore (:phase replanning)) "the fixture is what it claims to be")
    (is (not-any? #{:prologue-cap}
                  (map :gate (arbiter/eligible {:branch replanning :max-turns 40})))
        "not scolded for having verified nothing while re-planning")
    (is (some #{:prologue-cap}
              (map :gate (arbiter/eligible {:branch building :max-turns 40})))
        "but the gate still covers the branch it was written for")))

(deftest the-stuck-gate-says-what-it-is-withholding
  ;; vf-49o. A branch that is refused without being told why re-submits the
  ;; same call; the audit gate's refusals redirected work precisely because
  ;; they named what was wrong.
  (let [b (branch-with :consecutive-failures (gates/threshold :stuck-threshold)
                       :last-failed-claim "the greedy exchange terminates in n steps"
                       :turns (vec (repeat 10 {})))
        d (arbiter/decide {:branch b :max-turns 40})]
    (is (= :stuck (:gate d)))
    (is (str/includes? (:message d) "greedy exchange")
        "the withheld approach is quoted, not merely alluded to")))

(deftest explore-cap-expiry-forces-build-and-says-so
  ;; The release valve: a branch that cannot get a skeleton to elaborate must
  ;; not be locked out of verification for the whole run. At the cap the
  ;; prologue is declared over, and the branch is told why.
  (let [c (db/connect ":memory:")
        _ (db/migrate! c)
        rid (runs/start-run! c {:problem "p" :beam-width 1})
        b (state/new-branch {:id "B1" :problem "p"})
        cap (gates/threshold :explore-cap)]
    (runs/open-branch! c rid {:branch-id "B1" :created-at-turn 0})
    (with-redefs [llm/chat (fn [& _]
                             {:content (str "```tool-call\n"
                                            (json/write-str
                                             {:name "thesis"
                                              :args {:goal "settle Q-1"
                                                     :technique "scalarization"
                                                     :subClaims ["the box bound holds"]}})
                                            "\n```")
                              :finish-reason "stop"})]
      (let [after (wf/run-turn {:conn c :run-id rid :max-turns 40
                                   :llm-adapter :a :llm-config {:max-tokens 16384}}
                                  b (inc cap))]
        (is (= :build (:phase after)))
        (is (= (inc cap) (:phase-entered-turn after)))
        (is (some #(and (= "user" (:role %))
                        (str/includes? (:content %) "prologue is over"))
                  (:messages after))
            "the branch is told the prologue is over and why")))))

(deftest a-root-branch-starts-its-phase-clock-at-zero
  ;; loop.clj creates the root branch without :created-at-turn, so the phase
  ;; clock has to survive that: a nil :phase-entered-turn is a trap for the
  ;; next reader — vf-9wx, the GUI — and the fallback in
  ;; explore-cap-expired? exists to paper over exactly this.
  (let [b (state/new-branch {:id "B1" :problem "p"})]
    (is (= 0 (:phase-entered-turn b)))))

;; --- residual objectives ----------------------------------------------------

(deftest residual-reports-what-is-outstanding
  ;; A run cut short should say what is unfinished rather than making a resume
  ;; re-derive scope from the transcript (dirge PR 738).
  (let [b (branch-with :thesis {:goal "prove P" :subClaims ["lemma A" "lemma B"]}
                       :artifacts [{:claim "lemma A" :claim-status :confirmed}])
        r (state/residual b)]
    (is (= ["lemma A"] (:proved r)))
    (is (= ["lemma B"] (:outstanding r)))
    (is (str/includes? (state/render-residual r) "lemma B"))))

;; --- progress must mean progress -------------------------------------------

(deftest confirmation-alone-is-not-progress
  ;; Found by a zebra run that exhausted its turns. At turn 11 the model
  ;; verified "clpfd is available and supports a basic finite-domain
  ;; constraint", an engine said yes, and the harness recorded a confirmed
  ;; artifact and fired the milestone gate. Nothing about the puzzle had been
  ;; established. A guard that credits every confirmation cannot see a branch
  ;; verifying its own tooling.
  (let [b (branch-with :thesis {:goal "identify who drinks water and who owns the zebra"
                                :subClaims ["a clpfd model covers all fifteen clues"
                                            "the model yields a unique assignment"]})]
    (testing "a claim about the tooling does not advance the plan"
      (is (not (state/advances-thesis? b "clpfd is available and supports a basic finite-domain constraint")))
      (is (not (state/advances-thesis? b "the prolog session works"))))

    (testing "a claim about the actual problem does"
      (is (state/advances-thesis? b "the fifteen clues force a unique assignment"))
      (is (state/advances-thesis? b "the Norwegian drinks water and the Japanese owns the zebra")))

    (testing "the stall counter keeps climbing through a hollow confirmation"
      (let [after (state/record-outcome (assoc b :turns-since-progress 2)
                                        {:category :success :progress? true
                                         :claim "clpfd is available"})]
        (is (= 3 (:turns-since-progress after)))
        (is (not (:any-progress? after)))))

    (testing "and resets on a real one"
      (let [after (state/record-outcome (assoc b :turns-since-progress 2)
                                        {:category :success :progress? true
                                         :claim "the fifteen clues force a unique assignment"})]
        (is (= 0 (:turns-since-progress after)))
        (is (:any-progress? after)))))

  (testing "with no thesis registered, the problem statement stands in"
    ;; vf-8fl. Returning true unconditionally was defensible while the only
    ;; consumer was the stall counter. It is not defensible now: this is the
    ;; harness's only relevance signal, and a branch that never called `thesis`
    ;; had no guard at all. Observed live — B3 of run 0d0c3560 confirmed
    ;; "Diagnostic: between(-1,1,X) succeeds for X = -1,0,1." at turn 4, was
    ;; congratulated by the milestone gate for it, and exported it to all three
    ;; siblings through the shared pool.
    (let [b (branch-with :problem (str "Characterise how non-unique the L1"
                                       " minimum-cost-flow unwrapping is, and"
                                       " exhibit a canonical tie-breaking rule."))]
      (is (not (state/advances-thesis? b "Diagnostic: between(-1,1,X) succeeds for X = -1,0,1.")))
      (is (state/advances-thesis? b "the two-path gadget has exactly two optimal flows")
          (str "exploration that engages the problem is still credited, which"
               " is the whole reason this was permissive to begin with"))))

  (testing "a problem with no vocabulary of its own credits everything"
    ;; Nothing to be irrelevant to. Keeps the permissive behaviour wherever
    ;; there is genuinely nothing to measure against.
    (is (state/advances-thesis? (branch-with) "anything at all"))
    (is (state/advances-thesis? (state/new-branch {:id "B1"}) "anything at all"))))

;; --- safe state -------------------------------------------------------------

(deftest safe-state-coverage-gate
  ;; DS1's third rung. dirge's version restored from a snapshot store only
  ;; after proving against git that the store covered every file changed
  ;; since green. In this harness the journal IS the replay log — resume
  ;; rebuilds branches from it — and it is append-only, so the green point
  ;; is the turn cursor at the green verify and coverage reduces to the
  ;; log still reaching that cursor.
  (testing "no green point means nothing to fall back to"
    (is (false? (:ok (state/snapshot-covers? (branch-with)))))
    (is (str/includes? (:reason (state/snapshot-covers? (branch-with)))
                       "no green verify")))

  (testing "covered: the turn log still reaches the green point"
    (let [b (state/mark-green (branch-with :turns (vec (repeat 3 {}))))
          now (assoc b :turns (vec (repeat 6 {})))]
      (is (:ok (state/snapshot-covers? now)))
      (is (= 3 (:rewinding (state/snapshot-covers? now)))
          "three turns sit between the branch and its confirmed point")))

  (testing "DECLINES when the turn log no longer reaches the green point"
    (let [b (state/mark-green (branch-with :turns (vec (repeat 5 {}))))
          pruned (assoc b :turns (vec (repeat 2 {})))]
      (is (false? (:ok (state/snapshot-covers? pruned))))
      (is (str/includes? (:reason (state/snapshot-covers? pruned))
                         "no longer reaches"))))

  (testing "the rung is harder to trip than a cull"
     (let [b (state/mark-green (branch-with :turns (vec (repeat 2 {}))))
           ;; tier 1b: the multiple is gates.edn :safe-state-multiple
           [cull multiple] [3 (gates/threshold :safe-state-multiple)]]
       (is (not (state/safe-state-due? (assoc b :consecutive-failures 3) cull multiple)))
       (is (state/safe-state-due? (assoc b :consecutive-failures 6) cull multiple))))

  (testing "and never trips without a green point at all"
    (is (not (state/safe-state-due? (branch-with :consecutive-failures 99) 3
                                    (gates/threshold :safe-state-multiple))))))

(deftest green-verify-marks-the-green-point
  ;; The green point is a fact about the WORKING TREE — the suite was observed
  ;; passing — so it keys on the verify signal and not on any claim. That is
  ;; why it did not move to the artifact trigger when :clear-reframe did: the
  ;; two entries ask different questions and neither subsumes the other.
    (with-redefs [tools/run-tool (fn [{:keys [branch]}]
                                 {:branch branch
                                  :result "Answer accepted."
                                  :verified-green? true})]
    (let [branch (assoc (branch-with) :turns (vec (repeat 4 {})))
          r (aloop/tool-step {} branch 5 {:name "done" :args {:answer "all green"}})]
      (is (= 5 (:green-snapshot (:branch r)))
          "the cursor is the turn count at the green verify")
      (is (:ok (state/snapshot-covers? (:branch r))))))

  (testing "a red or skipped verify does not move the green point"
    (with-redefs [tools/run-tool (fn [{:keys [branch]}]
                                   {:branch branch
                                    :result "Answer accepted."
                                    :verified-green? false})]
      (let [branch (assoc (branch-with) :turns (vec (repeat 4 {})))
            r (aloop/tool-step {} branch 5 {:name "done" :args {:answer "still red"}})]
        (is (nil? (:green-snapshot (:branch r))))))))

;; --- forking ----------------------------------------------------------------

(deftest branch-theses-splits-the-first-from-the-rest
  (let [b (branch-with)
        r (tools/run-tool {:branch b :turn 1 :tool-name "branch_theses"
                           :args {:theses [{:goal "route A" :technique "induction"}
                                           {:goal "route B" :technique "algebra"}
                                           {:goal "route C" :technique "search"}]}})]
    (testing "the first commits this branch, the rest become pending siblings"
      (is (= "route A" (get-in r [:branch :thesis :goal])))
      (is (= ["route B" "route C"] (mapv :goal (get-in r [:branch :pending-branch-theses])))))

    (testing "a tool never creates a branch itself"
      (is (nil? (:children r)) "the scheduler owns the branch table"))

    (testing "malformed proposals are refused rather than partially applied"
      ;; :mechanics since vf-v6x. A proposal the harness cannot parse says
      ;; nothing about the branch's mathematics, and this exact complaint was
      ;; 16 of the campaign's failure-turns.
      (are [theses] (= :mechanics (:category (tools/run-tool
                                            {:branch b :turn 1 :tool-name "branch_theses"
                                             :args {:theses theses}})))
        []
        "not a list"
        [{:no-goal true}]
        (vec (repeat 9 {:goal "too many"}))))))

(deftest cull-protects-a-recently-productive-branch
  ;; Incremental strategies look like verify N, fail at N+1, verify N+1.
  ;; Culling them throws away the most productive branch in the beam.
  ;; Ten turns: past the juvenile grace period, so this exercises the
  ;; ordinary cull path rather than the newborn protection.
  (let [failing (-> (branch-with :consecutive-failures 3)
                    (assoc :turns (vec (repeat 10 {}))))
        productive (assoc failing :artifacts [{:claim "every element of S satisfies P" :claim-status :confirmed
                                               :turn 5}])]
    (is (= :culled (:status (cull-or-keep {} failing 2 []))))
    (is (= :active (:status (cull-or-keep {} productive 2 []))))
    (testing "a stale confirmation does not protect it forever"
      (let [stale (assoc failing :artifacts [{:claim "every element of S satisfies P" :claim-status :confirmed
                                              :turn 0}])]
        (is (= :culled (:status (cull-or-keep {} stale 2 []))))))
    (testing "the last branch standing is never culled"
      ;; Found by the width sweep: the width-1 arm was culled at turn 9 of 12
      ;; and the run ended there, which reads as evidence against narrow beams
      ;; and is actually a rule fired outside the situation it was written for.
      (is (= :active (:status (cull-or-keep {} failing 0 [])))))
    (testing "a recent measurement protects it too"
      ;; vf-0of. A branch locating something empirically confirms nothing by
      ;; construction, so the confirmation-only trigger culled exactly the
      ;; branch whose thesis was the measurement.
      (let [measuring (assoc failing :artifacts [{:claim "the rate at sigma = 0.7 is 0.72"
                                                  :claim-status :empirical :turn 5}])]
        (is (= :active (:status (cull-or-keep {} measuring 2 []))))))))

(deftest a-run-can-keep-exploring-after-a-branch-ships
  ;; Winner-takes-all ends a research run at the first qualifying answer.
  ;; With the flag off, a shipped branch goes inactive holding its answer
  ;; and the rest keep working; the best is chosen at the end.
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p" :beam-width 2})
          shipped (assoc (branch-with :id "B1") :final-answer "first answer")
          working (branch-with :id "B2")]
      (runs/open-branch! c rid {:branch-id "B1"})
      (runs/open-branch! c rid {:branch-id "B2"})
      (testing "stop-on-first-done? true ends the run"
        (is (some? (#'beam/finish-now? {:config {:run {:stop-on-first-done? true}}}
                                       shipped [shipped working]))))
      (testing "false keeps going while another branch is alive"
        (is (nil? (#'beam/finish-now? {:config {:run {:stop-on-first-done? false}}}
                                      shipped [shipped working]))))
      (testing "false still ends once nobody is left to explore"
        (is (some? (#'beam/finish-now?
                    {:config {:run {:stop-on-first-done? false}}}
                    shipped [shipped (assoc working :status :culled)])))))))

(deftest a-shipped-branch-is-recorded-as-finished
  ;; vf-7hz. `done` sets {:status :done :final-answer ...}, and both places
  ;; that wrote a branch's ending missed exactly that pair: the post-cull
  ;; loop skipped anything holding a final answer, and the run-end loop only
  ;; wrote branches still `active?`. So a branch that SHIPPED — the one
  ;; outcome the run exists to produce — kept status 'active' in the record
  ;; and never emitted branch-closed.
  ;;
  ;; Gen-10 of the covering campaign completed with all nine surviving
  ;; branches shipped and all nine still reading 'active'. The GUI folds
  ;; branch status from branch-closed events, so a finished run drew nine
  ;; live branches, and the working indicator keys on :active as well.
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p" :beam-width 3})
          status-of (fn [id] (-> (db/fetch c ["SELECT status, inactive_reason
                                               FROM branches
                                               WHERE run_id = ? AND id = ?" rid id])
                                 first))]
      (doseq [id ["B1" "B2" "B3"]]
        (runs/open-branch! c rid {:branch-id id :created-at-turn 0}))
      (#'beam/record-inactive!
       {:conn c :run-id rid}
       [(assoc (branch-with :id "B1") :status :done :final-answer "the answer")
        (assoc (branch-with :id "B2") :status :culled
               :inactive-reason "culled after 3 consecutive failures")
        (branch-with :id "B3")])
      (testing "a shipped branch is written as done, not left active"
        (is (= "done" (:status (status-of "B1")))))
      (testing "an ordinary cull still records why"
        (is (= "culled" (:status (status-of "B2"))))
        (is (str/includes? (:inactive_reason (status-of "B2")) "consecutive")))
      (testing "a branch still working is untouched"
        (is (= "active" (:status (status-of "B3")))))
      (testing "shipping emits branch-closed, which is what the GUI folds"
        (let [closed (filter #(= "branch-closed" (:kind %))
                             (journal/events-since c rid 0))]
          (is (= #{"B1" "B2"} (set (map :branch_id closed)))
              "exactly the two that stopped, and no event for the live one"))))))

;; --- the critic and Pareto retention ----------------------------------------

(deftest critic-score-parsing
  (testing "all four objectives, line-anchored, reasoning stripped"
    (is (= {:progress 4 :momentum 3 :distinctness 5 :viability 4}
           (critic/parse-scores
            (str "<think>SCORE progress: 1 maybe</think>\n"
                 "SCORE progress: 4\nSCORE momentum: 3\n"
                 "SCORE distinctness: 5\nSCORE viability: 4")))))
  (testing "a missing objective fails closed"
    (is (nil? (critic/parse-scores "SCORE progress: 4\nSCORE momentum: 3"))))
  (testing "out of range is not a score"
    (is (nil? (critic/parse-scores
               (str "SCORE progress: 7\nSCORE momentum: 3\n"
                    "SCORE distinctness: 5\nSCORE viability: 4")))))
  (testing "drafts: the last line wins"
    (is (= 2 (:progress (critic/parse-scores
                         (str "SCORE progress: 4\nSCORE momentum: 3\n"
                              "SCORE distinctness: 5\nSCORE viability: 4\n"
                              "SCORE progress: 2")))))))

(deftest branch-out-waits-before-asking-again
  ;; Gen-9: the rung fired on turns 14, 15 and 16 — one fork, then two
  ;; nagging re-fires — and the branch's whole fork budget was gone by turn
  ;; 16 of a 65-turn run. A gate that re-fires while the branch is still
  ;; acting on it spends the budget on repetition, which is what re-fire
  ;; guards exist for.
  (let [fit (fn [now last-fired]
              (branch-with :artifacts [{:claim "every element of S satisfies P" :claim-status :confirmed :turn 5}]
                           :turns (vec (repeat now {}))
                           :gate-history (into [{:gate :milestone :turn 1}]
                                               (when last-fired
                                                 [{:gate :branch-out :turn last-fired}]))))]
    (testing "silent immediately after firing"
      (is (not-any? #{:branch-out}
                    (map :gate (arbiter/eligible {:branch (fit 12 11) :max-turns 40
                                                  :branch-count 3})))))
    (testing "eligible again once the cooldown has passed"
      (is (some #{:branch-out}
                (map :gate (arbiter/eligible
                            {:branch (fit (+ 11 (gates/threshold :branch-out-cooldown)) 11)
                             :max-turns 40 :branch-count 3})))))
    (testing "the budget allows a productive branch several forks over a run"
      (is (>= (gates/threshold :max-branch-outs) 5)))))

(deftest domination-ignores-accumulated-progress
  ;; Survival is about where a line is going, not what it has banked. The
  ;; artifacts a branch already confirmed are in the log and cannot be lost
  ;; by culling it, while `progress` is cumulative and therefore rises with
  ;; age — comparing on it lets any mature branch dominate any young one
  ;; indefinitely, which is the age bias that outlives the grace period.
  (let [young {:progress 1 :momentum 4 :distinctness 4 :viability 4}
        mature {:progress 5 :momentum 4 :distinctness 4 :viability 4}]
    (is (not (critic/dominated? young [mature]))
        "more banked work alone does not dominate")
    (is (critic/dominated? {:progress 5 :momentum 2 :distinctness 2 :viability 2}
                           [{:progress 1 :momentum 4 :distinctness 4 :viability 4}])
        "a branch going nowhere is dominated however much it banked")))

(deftest critic-domination
  (let [a {:progress 4 :momentum 4 :distinctness 3 :viability 4}
        b {:progress 3 :momentum 3 :distinctness 3 :viability 4}
        c {:progress 1 :momentum 1 :distinctness 5 :viability 2}]
    (is (critic/dominated? b [a]) "worse somewhere, better nowhere: dominated")
    (is (not (critic/dominated? a [b])))
    (is (not (critic/dominated? c [a b])) "a unique strength survives")
    (is (not (critic/dominated? a [a])) "an equal vector does not dominate")))

(deftest critic-scoring-is-fail-closed
  (let [b (branch-with :thesis {:goal "g" :technique "t" :subClaims []})]
    (with-redefs [llm/chat (fn [& _]
                             {:content (str "SCORE progress: 4\nSCORE momentum: 3\n"
                                            "SCORE distinctness: 5\nSCORE viability: 4")})]
      (is (= {:progress 4 :momentum 3 :distinctness 5 :viability 4}
             (:scores (critic/score! {} b [] 7)))))
    (with-redefs [llm/chat (fn [& _] {:content "the branch seems fine to me"})]
      (is (nil? (critic/score! {} b [] 7))
          "an unusable critic answer is no information, not a random vector"))
    (with-redefs [llm/chat (fn [& _] (throw (ex-info "provider down" {})))]
      (is (nil? (critic/score! {} b [] 7))
          "a dead provider must not take the beam down"))))

(deftest critic-score-prompt-is-a-prompt-file
  ;; Tier 2a: the critic's score prompt moved from src prose to
  ;; resources/prompts/critic.md — runtime-editable, and no longer
  ;; proof-domain ("parallel proof attempts") on a coding surface.
  (let [captured (atom nil)
        b (branch-with :thesis {:goal "g" :technique "t" :subClaims []})]
    (with-redefs [llm/chat (fn [_ _ msgs _]
                             (reset! captured (-> msgs peek :content))
                             {:content (str "SCORE progress: 4\nSCORE momentum: 3\n"
                                            "SCORE distinctness: 5\nSCORE viability: 4")})]
      (critic/score! {} b [] 7)
      (let [p (or @captured "")]
        (is (str/includes? p "research director"))
        (is (str/includes? p "BRANCH") "the deterministic summary is substituted in")
        (is (str/includes? p "SCORE progress: <n>")
            "the SCORE line format the parser is coupled to survives the move")
        (is (not (str/includes? p "proof attempts"))
            "the coding loop is not a proof surface")))))

(deftest a-juvenile-branch-is-not-culled-against-its-elders
  ;; Gen-9 forked for the first time and every child died within twelve
  ;; turns. The reason was structural, not intellectual: progress and
  ;; momentum are age-correlated, so a branch born at turn 15 scores 1 on
  ;; progress by definition and its fifteen-turn-old parent dominates it on
  ;; every objective one turn later. Selection that runs before an offspring
  ;; can express itself is not selection. A branch gets a grace period of
  ;; its own turns before the cull rule applies to it at all.
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p"})
          ctx {:conn c :run-id rid}
          mature {:progress 5 :momentum 5 :distinctness 5 :viability 4}
          newborn (fn [turns]
                    (-> (branch-with :id "B1.2" :consecutive-failures 3
                                     :created-at-turn 15
                                     :critic {:scores {:progress 1 :momentum 2
                                                       :distinctness 5 :viability 4}
                                              :turn 16})
                        (assoc :turns (vec (repeat turns {})))))]
      (testing "a branch inside its grace period survives a dominating elder"
        (let [b (cull-or-keep ctx (newborn 3) 2 [mature])]
          (is (state/active? b))
          (is (= 1 (count (filter #(= "cull-spared" (:kind %))
                                  (journal/events-since c rid 0)))))))
      (testing "past the grace period the ordinary rules resume"
        (is (= :culled (:status (cull-or-keep
                                 ctx
                                 (newborn (inc (gates/threshold :juvenile-grace)))
                                 2 [mature])))))
      (testing "grace does not save a branch the critic calls a dead end"
        (let [doomed (assoc-in (newborn 3) [:critic :scores :viability] 1)]
          (is (= :culled (:status (cull-or-keep ctx doomed 2 [])))))))))

(deftest pareto-retention-spares-non-dominated-branches
  ;; The scalar rule is the TRIGGER; domination is the verdict. Three runs in
  ;; a row culled a branch at 3 consecutive failures — including one whose
  ;; mathematics was right and whose Lean proofs merely kept failing. A
  ;; failing branch that no sibling dominates keeps exploring, on a clock.
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p"})
          ctx {:conn c :run-id rid}
          failing (fn [fails scores]
                    (-> (branch-with :id "BF" :consecutive-failures fails
                                     :critic {:scores scores :turn 6})
                        (assoc :turns (vec (repeat 8 {})))))]
      (testing "failing and dominated: culled"
        (is (= :culled (:status (cull-or-keep
                                 ctx
                                 (failing 3 {:progress 2 :momentum 2
                                             :distinctness 2 :viability 3})
                                 1
                                 [{:progress 3 :momentum 4
                                   :distinctness 3 :viability 4}])))))
      (testing "failing but non-dominated: spared, journaled, and told"
        (let [b (cull-or-keep
                 ctx
                 (failing 3 {:progress 2 :momentum 2
                             :distinctness 5 :viability 3})
                 1
                 [{:progress 3 :momentum 4 :distinctness 2 :viability 4}])]
          (is (state/active? b))
          (is (some #(and (= "user" (:role %))
                          (str/includes? (:content %) "no sibling dominates"))
                    (:messages b)))
          (is (= 1 (count (filter #(= "cull-spared" (:kind %))
                                  (journal/events-since c rid 0)))))))
      (testing "the critic's own dead-end verdict culls"
        (is (= :culled (:status (cull-or-keep
                                 ctx
                                 (failing 3 {:progress 2 :momentum 2
                                             :distinctness 5 :viability 1})
                                 1 [])))))
      (testing "the hard floor: double the threshold ends the reprieve"
        (is (= :culled (:status (cull-or-keep
                                 ctx
                                 (failing 6 {:progress 2 :momentum 2
                                             :distinctness 5 :viability 5})
                                 1 []))))))))

(deftest the-beam-repopulates-when-it-falls-below-width
  ;; The blocker for a genuine frontier: culls remove width permanently and
  ;; the only route back up was a fork gated on a confirmation AND a
  ;; cooldown, so every run in the campaign decayed toward one line — five
  ;; runs, five collapses to a single branch. A population that only ever
  ;; shrinks is not a frontier. When the beam drops below its target width
  ;; and the cap allows, the strongest survivor is told to reseed it.
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p"})
          ctx {:conn c :run-id rid :beam-width 3}
          scored (fn [id sc turns]
                   (-> (branch-with :id id :critic {:scores sc :turn 6})
                       (assoc :turns (vec (repeat turns {})))))
          strong (scored "B1" {:progress 4 :momentum 4 :distinctness 3 :viability 5} 12)
          weak (scored "B2" {:progress 1 :momentum 2 :distinctness 2 :viability 2} 12)
          dead (assoc (scored "B3" {:progress 1 :momentum 1 :distinctness 1 :viability 1} 12)
                      :status :culled)]
      (testing "below target width, the strongest survivor is marked to reseed"
        (let [bs (repopulate ctx [strong weak dead] 3 20)
              b1 (first (filter #(= "B1" (:id %)) bs))
              b2 (first (filter #(= "B2" (:id %)) bs))]
          (is (= 20 (:fork-invited b1)))
          ;; The mark, not a message: the ask itself is the :repopulate gate,
          ;; so it carries a prediction and settles. See
          ;; repopulation-is-a-gate-with-a-prediction-not-an-invitation.
          (is (= 20 (:repopulate-due b1)))
          (is (= (count (:messages strong)) (count (:messages b1)))
              "the scheduler does not speak on a boundary the arbiter owns")
          (is (nil? (:fork-invited b2)) "one ask, to the strongest")
          (is (= 1 (count (filter #(= "repopulate" (:kind %))
                                  (journal/events-since c rid 0)))))))
      (testing "at or above target width it stays quiet"
        (is (nil? (:fork-invited
                   (first (repopulate ctx [strong weak
                                                  (scored "B4" {:progress 2 :momentum 2
                                                                :distinctness 2 :viability 2} 12)]
                                             3 20))))))
      (testing "no room under the cap, no ask"
        (is (nil? (:fork-invited
                   (first (repopulate ctx [strong dead]
                                             (gates/threshold :max-total-branches) 20))))))
      (testing "a recent ask is not repeated"
        (is (= 18 (:fork-invited
                   (first (repopulate ctx [(assoc strong :fork-invited 18) dead]
                                             3 20)))))))))

(deftest every-gate-renders-its-message
  ;; The progress-stalled gate referenced resources/prompts/progress-stalled.md,
  ;; which did not exist. `slurp` of a nil resource throws, so the gate killed
  ;; whatever branch it fired on, and because a dying branch is abandoned
  ;; rather than surfaced the gate simply never appeared in any tally. It had
  ;; never worked. Rendering every gate's message is the general guard: a gate
  ;; that cannot produce its text is not a quiet gate, it is a broken one.
  (let [ctx {:branch (branch-with :turns (vec (repeat 5 {}))
                                  :turns-since-progress 4
                                  :green-snapshot 2)
             :max-turns 40
             :done-block "blocked because ..."
             :directive {:payload "a human said so"}
             :safe-state-coverage {:ok true}}]
    (doseq [g (gates/gates)]
      (testing (str (:gate g))
        (let [msg ((:message g) ctx)]
          (is (string? msg))
          (is (not (str/blank? msg))
              (str (:gate g) " produced an empty message")))
        (is (string? ((:prediction g) ctx)))))))

(deftest provenance-words-are-not-treated-as-claims
  ;; The gate is aimed at fabricated specifics: a number or a name in the
  ;; answer that no artifact supports. Words describing HOW something was
  ;; checked can never appear in an artifact, because an artifact's claim and
  ;; code are about the problem. Flagging them refused a correct answer three
  ;; times in one run and pushed the model toward stripping its explanation.
  (let [artifacts [{:claim "for every n, sum of first n odds = n^2"
                    :code "theorem t (n : Nat) : ..." :witness nil}]]
    (is (empty? (tools/uncovered-tokens
                 "For every n, the sum of the first n odd numbers equals n^2. This
                  universal statement is kernel-checked by two Lean 4 + Mathlib
                  theorems, proved by induction on the successor."
                 artifacts))
        "provenance prose must not be read as unsupported assertion"))

  (testing "the guard still catches a fabricated number"
    (is (= ["24"] (tools/uncovered-tokens "the answer is 24"
                                          [{:claim "the answer is 23" :code "" :witness nil}])))))

(deftest a-tool-version-is-not-a-numeric-claim
  ;; "Lean 4" asserts the number 4 under a naive tokenizer, and numbers are the
  ;; part of this gate that must not be relaxed. Stripped as a name-plus-version
  ;; phrase rather than by exempting the digit.
  (is (empty? (tools/answer-tokens "verified with Lean 4 and Mathlib")))
  (is (= ["4"] (tools/answer-tokens "the answer is 4"))
      "a bare number is still a claim"))

;; --- done-eligible ranking ---------------------------------------------------

(defn- finished-branch
  "A minimal done-eligible branch: :final-answer plus whatever evidence the
  test under exercise needs. Explicit per test, so each one states only the
  axes it is about."
  [id & {:as evidence}]
  (merge (state/new-branch {:id id :problem "p"})
         {:final-answer (str "answer " id)}
         evidence))

;; --- resume ------------------------------------------------------------------

(deftest resumability-is-a-one-way-door
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p" :max-turns 10 :beam-width 1})]
      (is (resume/resumable? c rid) "a running run resumes")
      (runs/finish-run! c rid :completed "42")
      (is (not (resume/resumable? c rid)) "a completed run shipped; the answer is the record"))
    (let [rid (runs/start-run! c {:problem "p" :max-turns 10 :beam-width 1})]
      (runs/finish-run! c rid :aborted nil)
      (is (not (resume/resumable? c rid)) "abort is a person saying stop; resume is not them changing their mind"))
    (let [rid (runs/start-run! c {:problem "p" :max-turns 10 :beam-width 1})]
      (runs/finish-run! c rid :failed nil)
      (is (resume/resumable? c rid) "an exhausted process that never tore down may continue"))
    (is (not (resume/resumable? c "no-such-run")))))

;; --- forking twice must not collide -----------------------------------------

(deftest child-ids-skip-suffixes-the-parent-already-used
  ;; Killed gen-11 and gen-12. Child ids were parent + "." + (batch index + 2),
  ;; which has no memory of an earlier fork, so a branch that forked twice
  ;; reissued its first child's id and the INSERT hit
  ;; `UNIQUE constraint failed: branches.run_id, branches.id`, taking the run
  ;; down. Repopulation makes this the common case rather than a corner: it
  ;; asks the strongest survivor to branch again, and the strongest survivor is
  ;; the one that has already branched.
  (is (= ["B2.2" "B2.3"] (#'beam/child-ids #{} "B2" 2))
      "a first fork still starts at .2")
  (is (= ["B2.4" "B2.5"] (#'beam/child-ids #{"B2.2" "B2.3"} "B2" 2))
      "a second fork continues past the children already spawned")
  (is (= ["B2.3" "B2.5"] (#'beam/child-ids #{"B2.2" "B2.4"} "B2" 2))
      "gaps are reusable: ids are unique keys, not a spawn ordering")
  (is (= ["B1.2.2"] (#'beam/child-ids #{"B1.2"} "B1.2" 1))
      "the parent's own id is not a child id and must not be skipped over"))

;; --- the other three engines face the same two layers -----------------------

(defn- fresh-run []
  (let [c (db/connect ":memory:")
        _ (db/migrate! c)
        rid (runs/start-run! c {:problem "p" :beam-width 1})]
    (runs/open-branch! c rid {:branch-id "B1" :created-at-turn 0})
    [c rid (state/new-branch {:id "B1" :problem "p"})]))

(deftest a-measured-number-covers-an-answer-that-cites-it
  ;; The coverage gate exists to catch fabricated numbers. A number Octave
  ;; computed and the harness recorded is not fabricated, so it covers — which
  ;; is what lets a branch state its measurement in the answer at all.
  (let [measured {:claim "the recovery rate at sigma = 0.7 is 0.72"
                  :claim-status :empirical :witness {:value 0.72} :code "rate"}]
    (is (empty? (tools/uncovered-tokens "the recovery rate is 0.72" [measured])))
    (is (= ["0.91"] (tools/uncovered-tokens "the recovery rate is 0.91" [measured])))))

;; --- a truncated turn is retried, not spent ---------------------------------

(deftest a-response-truncated-before-any-tool-call-is-retried-at-a-doubled-budget
  ;; fence/signals already separates :truncated from :no-fence and says why:
  ;; a reply that hit the cap mid-thought never reached the fence, and "the fix
  ;; is more tokens, not more steering." The branch loop steered anyway, and
  ;; spent the turn. gen-12 opened with three of these in one round, gen-11 ran
  ;; a 12% no-call rate against gen-10's 4%.
  (let [calls (atom [])]
    (with-redefs [llm/chat (fn [_ _ _ & [opts]]
                             (swap! calls conj (:max-tokens opts))
                             (if (= 1 (count @calls))
                               {:content "thinking..." :finish-reason "length"}
                               {:content "```tool-call\n{\"name\":\"thesis\"}\n```"
                                :finish-reason "stop"}))]
      (let [ctx {:llm-adapter :a :llm-config {:max-tokens 16384}}
            r (#'aloop/call-model ctx {:messages []})]
        (is (true? (:ok r)))
        (is (= 2 (count @calls)) "the truncated call is retried rather than spent")
        (is (= [16384 32768] @calls)
            "and the retry doubles the budget instead of repeating it")
        (is (str/includes? (:content (:response r)) "tool-call")
            "the retry's response is the one returned")))))

(deftest a-truncated-response-that-still-carried-a-call-is-not-retried
  ;; Truncation only matters when it cost the tool call. A reply that emitted
  ;; its fence and then ran out of room is a complete turn.
  (let [calls (atom 0)]
    (with-redefs [llm/chat (fn [& _]
                             (swap! calls inc)
                             {:content "```tool-call\n{\"name\":\"thesis\"}\n```\nand then"
                              :finish-reason "length"})]
      (#'aloop/call-model {:llm-adapter :a :llm-config {:max-tokens 16384}} {:messages []})
      (is (= 1 @calls)))))

(deftest a-model-that-never-calls-a-tool-stops-after-the-doubled-attempt
  ;; The escalation is bounded: one retry, then the turn is spent as before.
  ;; An unbounded loop here would burn a branch's whole budget on one turn.
  (let [calls (atom 0)]
    (with-redefs [llm/chat (fn [& _]
                             (swap! calls inc)
                             {:content "still thinking" :finish-reason "length"})]
      (#'aloop/call-model {:llm-adapter :a :llm-config {:max-tokens 16384}} {:messages []})
      (is (= 2 @calls)))))

;; --- a turn that emitted no call is prefilled into the fence ----------------

(deftest a-turn-that-emitted-no-tool-call-prefills-the-next-one
  ;; gen-22 B1 spent 24 of its 44 turns on __no_call__ — more than half the
  ;; branch. It was told "[harness] No ```tool-call block in your response"
  ;; twenty-four times, which is the measurement: asking a model that just
  ;; wrote 109,360 characters without a fence to please emit one does not
  ;; work. Turn 42 is the shape of it — a full page of sound reasoning ending
  ;; "let me confirm the composition theorem a#712's exact statement", and
  ;; then nothing.
  ;;
  ;; arbiter/prefill-for already argues the general case: across gen-19 and
  ;; gen-20 the gates that changed behaviour were the ones that WITHHELD, and
  ;; ending the request mid-fence is the withholding form of an instruction —
  ;; the model cannot answer in prose because it is already inside a tool
  ;; call. That mechanism was reachable only from a gate decision, so it never
  ;; reached the branch with the most to gain from it.
  (let [c (db/connect ":memory:")
        _ (db/migrate! c)
        rid (runs/start-run! c {:problem "p" :beam-width 1})
        b (state/new-branch {:id "B1" :problem "p"})]
    (runs/open-branch! c rid {:branch-id "B1" :created-at-turn 0})
    (with-redefs [llm/chat (fn [& _] {:content "Let me confirm a#712 first."
                                      :finish-reason "stop"})]
      (let [after (wf/run-turn {:conn c :run-id rid :max-turns 40
                                   :llm-adapter :a :llm-config {:max-tokens 16384}}
                                  b 1)]
        (is (= "```tool-call\n" (:prefill after))
            "the next request ends mid-fence, so prose is not an available reply")
        (is (not (str/includes? (:prefill after) "\"name\""))
            "bare: which tool to call is the branch's decision, not the harness's")))))

(deftest a-turn-that-called-a-tool-leaves-no-prefill-behind
  ;; The complement, and the one that would go wrong quietly: a branch that is
  ;; working normally must not be forced into a fence, or it can never write
  ;; the reasoning that earns the next call.
  (let [c (db/connect ":memory:")
        _ (db/migrate! c)
        rid (runs/start-run! c {:problem "p" :beam-width 1})
        b (state/new-branch {:id "B1" :problem "p"})]
    (runs/open-branch! c rid {:branch-id "B1" :created-at-turn 0})
    (with-redefs [llm/chat (fn [& _]
                             {:content (str "```tool-call\n"
                                            (json/write-str
                                             {:name "thesis"
                                              :args {:goal "settle Q-1"
                                                     :technique "scalarization"
                                                     :subClaims ["the box bound holds"]}})
                                            "\n```")
                              :finish-reason "stop"})]
      (let [after (wf/run-turn {:conn c :run-id rid :max-turns 40
                                   :llm-adapter :a :llm-config {:max-tokens 16384}}
                                  b 1)]
        (is (nil? (:prefill after)))))))

(deftest a-prefilled-turn-that-still-emits-no-call-keeps-the-fence
  ;; One prefill is not a guarantee. A model can open the fence and then fail
  ;; to close it, and the recovery from that is the same recovery — not a
  ;; branch that silently reverts to prose on the turn after.
  (let [c (db/connect ":memory:")
        _ (db/migrate! c)
        rid (runs/start-run! c {:problem "p" :beam-width 1})
        b (assoc (state/new-branch {:id "B1" :problem "p"})
                 :prefill "```tool-call\n")]
    (runs/open-branch! c rid {:branch-id "B1" :created-at-turn 0})
    (with-redefs [llm/chat (fn [& _] {:content "{\"name\": " :finish-reason "length"})]
      (let [after (wf/run-turn {:conn c :run-id rid :max-turns 40
                                   :llm-adapter :a :llm-config {:max-tokens 16384}}
                                  b 1)]
        (is (= "```tool-call\n" (:prefill after)))))))

(deftest turns-that-run-clean-work-off-the-cull-budget
  ;; gen-18 B1 made three malformed tool calls, then three clean Octave runs
  ;; that produced dual potentials — and was culled on a counter last
  ;; incremented three turns earlier. `consecutive-failures` drives the cull
  ;; gate, so it has to mean consecutive. The guard against well-formed but
  ;; useless calls is `turns-since-progress`, which a neutral turn still
  ;; increments, so nothing is lost by letting a clean turn work the failure
  ;; count down.
  (let [b (reduce (fn [b _] (state/record-outcome b {:category :failure}))
                  (state/new-branch {:id "B1" :problem "p"})
                  (range 3))]
    (is (= 3 (:consecutive-failures b)))
    (let [b1 (state/record-outcome b {:category :neutral :progress? false})
          b2 (state/record-outcome b1 {:category :neutral :progress? false})]
      (is (= 2 (:consecutive-failures b1)) "one clean turn decays it")
      (is (= 1 (:consecutive-failures b2)) "and another")
      (is (= 5 (:turns-since-progress b2))
          "while the useless-call guard keeps counting")
      (is (= 0 (:consecutive-failures
                (state/record-outcome b2 {:category :neutral :progress? false})))
          "and it floors at zero rather than going negative"))
    (testing "sustained failure still accumulates"
      (let [b (reduce (fn [b c] (state/record-outcome b {:category c}))
                      b [:neutral :failure :failure :neutral :failure :failure])]
        (is (>= (:consecutive-failures b) 3))))))

(deftest a-malformed-fence-is-a-mechanics-fault-not-a-verification-failure
  ;; gen-20 B2 was culled at turn 6 having called `thesis` and `lean_search`
  ;; and nothing else. Its other four turns produced no ```tool-call block at
  ;; all, and each was recorded :failure — the same counter the cull gate
  ;; reads. The reason it died with says "the critic scored the line a dead
  ;; end", but the critic had no line to score: the branch never made a
  ;; substantive claim. A fifth of the beam went to a formatting problem.
  ;;
  ;; loop.clj already draws this distinction one branch up, for a provider
  ;; error: "not the branch's fault and must not count against it as a
  ;; verification failure." A fence the model malformed is the same kind of
  ;; thing — a mechanics fault, which the branch already tracks separately.
  (let [b2 (reduce (fn [b c] (state/record-outcome b {:category c :progress? false}))
                   (state/new-branch {:id "B2" :problem "p"})
                   ;; B2's actual sequence, turns 1-6.
                   [:neutral :mechanics :neutral :mechanics :mechanics :mechanics])]
    (is (zero? (:consecutive-failures b2))
        "four malformed fences must not read as verification failures")
    (is (= 3 (:consecutive-mechanics-failures b2))
        "but they are counted, on their own tally — consecutive, so the
         lean_search between the first and the rest resets it")
    (is (= 6 (:turns-since-progress b2))
        "and the useless-turn guard still counts every one of them")
    (is (< (:consecutive-mechanics-failures b2)
           (* 2 (gates/threshold :cull-threshold)))
        "and B2, the branch this is named for, now survives its turn 6")
    (testing "a well-formed call clears the mechanics tally — it proved it can"
      (doseq [c [:success :failure :neutral]]
        (is (zero? (:consecutive-mechanics-failures
                    (state/record-outcome b2 {:category c :progress? false})))
            (str "a " c " turn is a well-formed call and should clear it"))))
    (testing "a real verification failure still counts, after mechanics noise"
      (let [b (state/record-outcome b2 {:category :failure :progress? false})]
        (is (= 1 (:consecutive-failures b))
            "and starts from 1, not compounded by the malformed turns")))))

(deftest a-policy-refusal-is-mechanics-but-not-a-malformed-fence
  ;; vf-b25/vf-eaw follow-up. A phase or sketch-diversity refusal is a
  ;; mechanics turn — nothing to do with the mathematics — but the branch
  ;; emitted a perfectly well-formed call and was declined. Counting it only
  ;; as a malformed fence would make the cull reason a lie, so it gets its
  ;; own counter with the same clear rule.
  (testing "six policy refusals count on their own counter, not just mechanics"
    (let [b (reduce (fn [b _] (state/record-outcome b {:category :mechanics
                                                       :policy-refusal? true}))
                    (state/new-branch {:id "B1" :problem "p"})
                    (range 6))]
      (is (= 6 (:consecutive-mechanics-failures b)))
      (is (= 6 (:consecutive-policy-refusals b)))))
  (testing "a genuinely malformed fence does not move the policy counter"
    (let [b (reduce (fn [b _] (state/record-outcome b {:category :mechanics}))
                    (state/new-branch {:id "B1" :problem "p"})
                    (range 6))]
      (is (= 6 (:consecutive-mechanics-failures b)))
      (is (zero? (:consecutive-policy-refusals b)))))
  (testing "any real tool call clears both, exactly like the mechanics tally"
    (let [b (state/record-outcome
             (reduce (fn [b _] (state/record-outcome b {:category :mechanics
                                                       :policy-refusal? true}))
                     (state/new-branch {:id "B1" :problem "p"})
                     (range 6))
             {:category :neutral :progress? false})]
      (is (zero? (:consecutive-policy-refusals b)))
      (is (zero? (:consecutive-mechanics-failures b))))))

(deftest a-gate-that-names-one-tool-forces-its-fence
  ;; gen-19 settled gate predictions 4 met to 22 unmet, gen-20 9 to 27. The
  ;; pattern across both is that a gate which WITHHOLDS something changes
  ;; behaviour and a gate which SUGGESTS one does not — the audit gate's
  ;; refusals redirected work, milestone and tier-escalation went 0-for-4.
  ;;
  ;; A prefill is the withholding version of a suggestion: the request ends
  ;; mid-fence, so the model cannot answer in prose. Applied only where a gate
  ;; already fired, not on every turn — prefilling makes the response BEGIN
  ;; with the call, which is right when the harness is already steering and
  ;; wrong as a blanket default.
  (testing "a gate whose prediction names exactly one tool carries it"
    (doseq [g [:branch-out :repopulate]]
      (is (= "branch_theses" (:tool (gates/by-name g)))
          (str g " predicts 'the branch calls branch_theses' and should say so"))))
  (testing "a gate naming a choice or a behaviour carries none"
    ;; milestone predicts "review or done", stuck predicts a change of
    ;; technique. Forcing either would be the harness picking, not steering.
    (doseq [g [:milestone :stuck :progress-stalled :turn-budget]]
      (is (nil? (:tool (gates/by-name g)))
          (str g " names no single tool and must not force one"))))
  (testing "the prefill is the opening fence, plus the name when there is one"
    (is (= "```tool-call\n{\"name\": \"branch_theses\""
           (arbiter/prefill-for {:tool "branch_theses"})))
    (is (= "```tool-call\n" (arbiter/prefill-for {:tool nil}))
        "a gate with no named tool still forecloses prose")
    (is (nil? (arbiter/prefill-for nil))
        "and no gate at all means no prefill")))

(deftest a-branch-that-cannot-emit-a-tool-call-is-still-bounded
  ;; The counter has to be separated, not softened. `mechanics` feeds only the
  ;; capability tier and gates nothing; `turns-since-progress` feeds only
  ;; progress-stalled, a nudge gate that gen-19 settled 0-for-2. So if a
  ;; malformed fence simply stopped counting, a branch emitting nothing but
  ;; garbage would hold a beam slot until the turn budget ran out.
  ;;
  ;; Deliberately looser than the verification threshold: three bad fences is
  ;; a model having a bad turn, and the branch has already shown it can call a
  ;; tool. Twice that is a branch that cannot work the protocol.
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p"})
          ctx {:conn c :run-id rid}
          threshold (gates/threshold :cull-threshold)
          babbling (fn [n]
                     (-> (branch-with :id "BM" :consecutive-mechanics-failures n)
                         (assoc :turns (vec (repeat 8 {})))))]
      (is (state/active? (cull-or-keep ctx (babbling threshold) 2 []))
          "at the verification threshold a mechanics-only branch keeps going")
      (let [dead (cull-or-keep ctx (babbling (* 2 threshold)) 2 [])]
        (is (= :culled (:status dead)))
        (is (re-find #"(?i)tool call|fence|malformed" (:inactive-reason dead))
            (str "the reason must name the real cause, not a dead-end line: "
                 (:inactive-reason dead))))
      (testing "and the last branch standing is never culled for it either"
        (is (state/active? (cull-or-keep ctx (babbling (* 4 threshold)) 0 [])))))))

(deftest a-branch-culled-on-policy-refusals-is-not-blamed-for-malformed-fences
  ;; Six build-phase `sketch` refusals are six perfectly well-formed calls the
  ;; harness declined — but the mechanics cull string said "could not emit a
  ;; well-formed fence", which would be a lie in the permanent record. gen-30
  ;; B3.2 was culled with exactly that false reason when the real cause was a
  ;; harness parse bug, and the reason was believed. The cull says what
  ;; actually happened: the policy, and the phase it happened in.
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p"})
          ctx {:conn c :run-id rid}
          threshold (gates/threshold :cull-threshold)
          b (-> (branch-with :id "BM"
                             :consecutive-mechanics-failures (* 2 threshold)
                             :consecutive-policy-refusals (* 2 threshold)
                             :phase :build)
                (assoc :turns (vec (repeat 8 {}))))
          dead (cull-or-keep ctx b 2 [])]
      (is (= :culled (:status dead)))
      (is (not (re-find #"(?i)well-formed fence|malformed" (:inactive-reason dead)))
          "a declined call is not a protocol failure")
      (is (re-find #"(?i)policy" (:inactive-reason dead))
          "the reason names the policy")
      (is (re-find #"(?i)build" (:inactive-reason dead))
          "and the phase it happened in"))))

(deftest a-mixed-streak-culls-with-both-counts
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p"})
          ctx {:conn c :run-id rid}
          threshold (gates/threshold :cull-threshold)
          b (-> (branch-with :id "BM"
                             :consecutive-mechanics-failures (* 2 threshold)
                             :consecutive-policy-refusals 2)
                (assoc :turns (vec (repeat 8 {}))))
          dead (cull-or-keep ctx b 2 [])]
      (is (= :culled (:status dead)))
      (is (re-find #"(?i)2 .*policy" (:inactive-reason dead)))
      (is (re-find #"(?i)4 .*fence|4 .*malformed" (:inactive-reason dead))
          "the two kinds are counted separately, so the record stays true"))))

(deftest the-reflection-gate-is-retired-and-stays-retired
  ;; RETIRED, on evidence: it fired in every run of this campaign and was met
  ;; in none of them — 0 for 9 across two model tiers. Two reasons, and the
  ;; second is why rewording it would not have helped.
  ;;
  ;; It fired ON A CADENCE rather than because anything was wrong, so most of
  ;; its firings interrupted a branch that was fine. And it asked the
  ;; IMPLEMENTER to inspect and reshape its own loop — which is the
  ;; supervisor's job, done now by the oversight stream with the right role,
  ;; the right context and the evidence to judge a change afterwards. A gate
  ;; asking the wrong role to do someone else's work cannot be fixed by better
  ;; wording.
  (is (nil? (gates/by-name :reflection))
      "if this fails, something re-added the gate — read karamazov-634 first")
  (is (nil? (gates/threshold :reflection-cadence)))
  (testing "the reflection POLICY is a different thing and stays: it bounds
            how much of a turn the task reflector sees"
    (is (some? (lexicon/policy :reflection)))))

(deftest the-studying-gate-catches-inspect-without-shipping
  (let [studying (branch-with :turns (vec (concat [{:tool "write_file"}]
                                                  (repeat 10 {:tool "read_file"}))))
        exploring (branch-with :turns (vec (repeat 12 {:tool "read_file"})))]
    (testing "fires when a branch that shipped lapses into inspection"
      (is (some #{:studying} (map :gate (arbiter/eligible {:branch studying :max-turns 40})))))
    (testing "silent on opening exploration — nothing shipped yet to lapse from"
      (is (not-any? #{:studying} (map :gate (arbiter/eligible {:branch exploring :max-turns 40})))))
    (testing "settles the moment the branch ships"
      (is (= :met (arbiter/settle {:gate :studying :turn 1 :window 3}
                                  {:current-turn 2 :tools-called ["edit_file"]
                                   :branch-before (branch-with) :branch-after (branch-with)}))))))

(deftest the-reflexion-log-accumulates-and-the-stuck-gate-surfaces-it
  (testing "reflexion-log appends newest-last, bounded at five"
    (let [b (reduce (fn [b c] (assoc b :abandoned (state/abandoned-log b c)))
                    {} ["a" "b" "c" "d" "e" "f"])]
      (is (= ["b" "c" "d" "e" "f"] (:abandoned b)) "the oldest drops")))
  (testing "the stuck gate names the earlier abandoned approaches so a branch diverges"
    (let [b (branch-with :consecutive-failures 5 :last-failed-claim "z"
                         :abandoned ["x" "y" "z"])
          msg ((:message (gates/by-name :stuck)) {:branch b})]
      (is (re-find #"already tried and abandoned" msg))
      (is (str/includes? msg "x"))
      (is (str/includes? msg "y"))
      (is (str/includes? msg "z") "the withheld claim is still shown too"))))


(deftest pilot-gates-are-config-data
  ;; Tier 3a: gates moved from closures in gates.clj to :gates entries in
  ;; gates.edn with EDN :when forms — the steer policy as data, the same
  ;; direction as the manifest dispatches. The forms are compiled once at load
  ;; into the closure shape the arbiter reads, and call the same accessors the
  ;; closures did: (threshold k) reads the config atom at fire time, so tuning
  ;; stays runtime-editable.
  ;;
  ;; :reflection was the other pilot and has since been retired on evidence
  ;; (0 for 9); :orienting stands in, being the same shape — an EDN :when that
  ;; reads a threshold, and a :message-form rather than a plain file.
  (let [orient (gates/by-name :orienting)
        pro (gates/by-name :prologue-cap)]
    (is (= 9 (:priority pro)))
    (is (fn? (:when orient)) "the EDN form compiled into a predicate fn")
    (is (fn? (:when pro)))
    (let [floor (gates/threshold :orient-turns)
          reading (fn [n] (assoc (branch-with) :turns (vec (repeat n {:tool "shell"}))))]
      (is ((:when orient) {:branch (reading floor)}) "fires at the floor")
      (is (not ((:when orient) {:branch (reading (dec floor))}))
          "and not before it — orientation below the floor is free"))
    (let [pro-b (-> (branch-with :phase :build :any-progress? false)
                    (assoc :turns (vec (repeat 8 {}))))]
      (is ((:when pro) {:branch pro-b}))
      (is (not ((:when pro) {:branch (assoc pro-b :phase :explore)}))
          "explore is deliberately exempt — a reframe sends one back there")
      (is (not ((:when pro) {:branch (assoc pro-b :any-progress? true)}))))
    (is (str/includes? ((:message pro) {:branch (assoc (branch-with)
                                                       :turns (vec (repeat 8 {})))})
                       "8 turns in")
        "the turn count is interpolated")))

(deftest plain-gates-are-config-data
  ;; Tier 3b tranche 1: the seven gates whose messages are a prompts/ file
  ;; plus {{turn-count}}/{{max-turns}} interpolation moved from closures to
  ;; gates.edn :gates entries — no new compiler machinery, the tier-3a path.
  ;; Behavior is pinned by the existing suite (coherence, settle, message
  ;; tests); this pins the move itself. The five message-builder gates
  ;; (done-blocked, human-directive, safe-state, stuck, studying,
  ;; progress-stalled) remain closures pending message-form compilation.
  (let [data (set (map :gate (:gates (gates/config))))]
    (doseq [g [:emergency-review :milestone :repopulate :branch-out
               :last-call :wind-down :turn-budget]]
      (is (contains? data g) (str g " is a gates.edn entry")))
    (is (= 4 (:priority (gates/by-name :emergency-review))))
    (is (= 2.5 (:priority (gates/by-name :last-call))) "fractional slots survive")
    (is (= "branch_theses" (:tool (gates/by-name :branch-out))))
    (is (= "done" (:tool (gates/by-name :last-call))))
    (let [b (assoc (branch-with) :turns (vec (repeat 30 {})))
          msg ((:message (gates/by-name :turn-budget)) {:branch b :max-turns 40})]
      (is (str/includes? msg "used 30 of 40 turns")))
    (let [b (assoc (branch-with) :turns (vec (repeat 36 {})))
          msg ((:message (gates/by-name :wind-down)) {:branch b :max-turns 40})]
      (is (str/includes? msg "at turn 36 of 40")))
    (is (not (str/includes? ((:message (gates/by-name :emergency-review)) {})
                            "{{")))
    ;; forceable-tools is data too: the arbiter's force schemas come from
    ;; gates.edn, not a src def.
    (is (contains? (gates/config) :forceable-tools))
    (is (= "done" (:name (get-in (gates/config) [:forceable-tools "done"]))))))

(deftest message-builder-gates-are-config-data
  ;; Tier 3b tranche 2: the six gates whose messages build from branch/ctx
  ;; state moved to gates.edn with :message-form — an EDN form compiled like
  ;; :when, seeing the same gate-context keys plus prompt/threshold/state.
  ;; With these, the entire 15-gate table is data; gates.clj holds only the
  ;; compiler.
  (let [data (set (map :gate (:gates (gates/config))))]
    (doseq [g [:human-directive :done-blocked :safe-state :stuck
               :progress-stalled :studying]]
      (is (contains? data g) (str g " is a gates.edn entry")))
    (is (= (count (gates/gates)) (count (:gates (gates/config))))
        "every gate comes from data — no closure half remains")
    (is (= "done-block passthrough"
           ((:message (gates/by-name :done-blocked))
            {:done-block "done-block passthrough"})))
    (is (str/includes? ((:message (gates/by-name :human-directive))
                        {:directive {:payload "STOP THE RUN"}})
                       "**A human has intervened"))
    (let [b (branch-with :turns-since-progress 7 :any-progress? true)]
      (is (str/includes? ((:message (gates/by-name :progress-stalled)) {:branch b})
                         "Nothing has advanced in 7 turns.")))
    (let [b (branch-with :last-failed-claim "claim z" :abandoned ["x" "y" "z"])]
      (is (str/includes? ((:message (gates/by-name :stuck)) {:branch b})
                         "**Withheld**"))
      (is (str/includes? ((:message (gates/by-name :stuck)) {:branch b})
                         "- x")))))

(deftest share-policy-is-gates-edn-data
  ;; drg-4026 #5: the shared-pool entry condition (which claim statuses
  ;; count, whether relevance gates export) moved from hard-coded clauses in
  ;; shareable? to gates.edn :share — project policy, runtime-tunable, the
  ;; same shape as every other policy scalar.
  (is (= {:statuses #{:confirmed} :require-relevance? true}
         (gates/threshold :share)))
  (let [b (assoc (branch-with) :thesis {:goal "count the odd numbers"})
        relevant {:claim-status :confirmed
                  :claim "the set of odd numbers has size 23"}
        irrelevant {:claim-status :confirmed :claim "the graph is 4-colorable"}
        sketch {:claim-status :sketch
                :claim "the set of odd numbers has size 23"}]
    (is (aloop/shareable? b relevant true))
    (is (not (aloop/shareable? b irrelevant true)) "relevance still gates export")
    (is (not (aloop/shareable? b sketch true)) "only :confirmed enters the pool")
    (is (not (aloop/shareable? b relevant false)) "the run's off switch still wins")
    (with-redefs [gates/threshold (fn [_] {:statuses #{:confirmed}
                                           :require-relevance? false})]
      (is (aloop/shareable? b irrelevant true)
          "relevance off in config turns the filter off — read at fire time"))))

(deftest gate-effects-are-gate-data
  ;; drg-4026 #4: the two gates that change branch state (not just speak) had
  ;; their effects keyed by hard-coded gate names in the steer-step. The
  ;; effect is now a key on the gate's own data (gates.edn :effect), carried
  ;; through arbiter/decide, and applied by loop/apply-effects dispatching on
  ;; :effect — so adding or renaming a state-changing gate is a data edit,
  ;; and loop.clj owns only the effect implementations (data cannot mutate
  ;; the branch).
  (is (= :begin-reframe (:effect (gates/by-name :stuck))))
  (is (= :notified-fractions (:effect (gates/by-name :turn-budget))))
  (is (every? (complement :effect)
              (remove #(contains? #{:stuck :turn-budget} (:gate %)) (gates/gates)))
      "no other gate claims a branch effect")
  ;; decide carries :effect from the chosen gate's data
  (with-redefs [gates/gates (constantly [{:gate :test-gate :priority 0 :effect :e
                              :when (fn [_] true)
                              :message (fn [_] "m")
                              :prediction (fn [_] nil)}])]
    (is (= :e (:effect (arbiter/decide {})))))
  ;; apply-effects dispatches on :effect, not the gate's name
  (let [b (branch-with)]
    (is (= 7 (:reframe-entered-turn
              (aloop/apply-effects {:gate :stuck :effect :begin-reframe} 7 40 b))))
    (is (nil? (:reframe-entered-turn (aloop/apply-effects {:gate :stuck} 7 40 b)))
        "a stuck-shaped decision without the effect key does not reframe")
    (is (contains? (aloop/apply-effects {:gate :turn-budget
                                         :effect :notified-fractions} 30 40 b)
                   :notified-fractions))
    (is (= b (aloop/apply-effects nil 7 40 b))
        "no decision, no effect")))

(deftest ship-gate-rungs-are-gates-edn-data
  ;; drg-4026 #44: the lexical ship rungs — an answer must exist, its figures
  ;; must come from the evidence, it must engage the problem — moved from a
  ;; cond in ship.clj to gates.edn :ship-gates, compiled at load into
  ;; predicate/message closures fired on the computed evidence. Adding a rung
  ;; is a data edit; the evidence computation stays in src.
  (let [rungs (gates/threshold :ship-gates)]
    (is (= 4 (count rungs)))
    (is (= [:answer-exists :figure-coverage :engages-problem :completeness]
           (mapv :name rungs))))
  ;; Asserts the SKELETON, not the sentence: two live runs did the work,
  ;; passed their tests, closed their task, then spent every remaining turn
  ;; calling `done` empty and ended :exhausted with nothing shipped. The rung
  ;; was saying only the argument's name — the exact failure base/missing
  ;; exists to prevent — so what matters is that it now shows the call.
  (let [msg (ship/ship-gate-block {:answer nil})]
    (is (str/includes? msg "answer"))
    (is (str/includes? msg "```tool-call")
        "a missing-argument complaint has to show the call it wanted")
    (is (str/includes? msg "\"name\": \"done\"")))
  (let [figures-msg (ship/ship-gate-block {:answer "the count is 42 and 7"
                                           :problem "count things"
                                           :evidence [{:claim "count is 40"}]
                                           :uncovered-numbers ["42" "7"]})]
    (is (str/includes? (str figures-msg) "figures no artifact supports"))
    (is (str/includes? (str figures-msg) "`42`")))
  (is (nil? (ship/ship-gate-block {:answer "the count is 40"
                                   :problem "count things"
                                   :evidence [{:claim "count is 40"}]
                                   :uncovered-numbers []})))
  ;; engages-problem still gates: an answer sharing no term with the problem
  (is (string? (ship/ship-gate-block {:answer "xyzzy plugh"
                                      :problem "count the odd numbers"
                                      :evidence [{:claim "odd numbers"}]
                                      :uncovered-numbers []}))))

(deftest phase-machine-is-resource-data
  ;; drg-4026 #34: the explore/build machine — which phase starts, which is
  ;; capped and by what threshold key, what follows, what each withholds —
  ;; moved from hard-coded :explore/:build literals in state.clj to
  ;; phases.edn. state.clj reads structure only; the cap VALUE stays a
  ;; gates.edn scalar the loop passes in.
  (is (= :explore (phases/initial-phase)))
  (is (= :build (phases/next-phase :explore)))
  (is (= :explore-cap (:cap-key (phases/phase :explore))))
  (is (nil? (phases/next-phase :build)) "build has no clock")
  (is (= :explore (:phase (state/new-branch {:id "B" :problem "p"}))))
  (let [b {:phase :explore :phase-entered-turn 0 :turns (range 12)}]
    (is (state/explore-cap-expired? b 10 12))
    (is (not (state/explore-cap-expired? (assoc b :phase :build) 10 12)))
    (is (= :build (:phase (state/enter-phase b 12))))))

(deftest phase-refusal-reads-the-phase-table
  ;; drg-4026 #34: phase-refusal consults the table's :withholds — the seam
  ;; the audit called inert. Still empty (the withheld proof tools left), and
  ;; a refusal from it must carry :policy-refusal? true so the cull record can
  ;; tell a declined call from a malformed fence.
  (with-redefs [phases/refusals (fn [] [])]
    (is (nil? (tools-base/phase-refusal {:branch {:phase :explore}
                                         :tool-name "eval"})))
    (with-redefs [phases/withholds (fn [_] #{"eval"})]
      (let [r (tools-base/phase-refusal {:branch {:phase :explore}
                                         :tool-name "eval"})]
        (is (map? r))
        (is (true? (:policy-refusal? r)))
        (is (str/includes? (str (:result r)) "explore"))))))

(deftest the-board-is-enforced-not-merely-encouraged
  ;; RFC-008 recorded that the board was "encouraged, not enforced": the
  ;; context block said "No task claimed" and the prompt said work starts with
  ;; a task, but nothing refused a call from a branch holding none. RFC-007
  ;; named `phases.edn :withholds` as the mechanism and noted it was empty —
  ;; and it could not have expressed this rule anyway, because it is handed a
  ;; PHASE and holding a task is a fact about the BRANCH.
  (let [unclaimed {:id "b1" :phase :explore}
        holding   {:id "b1" :phase :explore :task {:id "sz-abc" :title "t"}}]
    (testing "a branch with no task cannot change the working tree"
      (doseq [t ["write_file" "edit_file"]]
        (let [r (tools-base/phase-refusal {:branch unclaimed :tool-name t})]
          (is (map? r) (str t " was allowed from an unclaimed branch"))
          (is (true? (:policy-refusal? r))
              "a declined call is not evidence about the branch's line of inquiry")
          (is (= :work-needs-a-task (:refusal-rule r)))
          (is (str/includes? (str (:result r)) "task")
              "the refusal says what to do about it"))))

    (testing "holding one, it can"
      (doseq [t ["write_file" "edit_file"]]
        (is (nil? (tools-base/phase-refusal {:branch holding :tool-name t})))))

    (testing "investigating never needs a task — a branch must be able to find
              out what to claim before it can claim it"
      (doseq [t ["read_file" "grep" "lsp" "shell" "task" "message"]]
        (is (nil? (tools-base/phase-refusal {:branch unclaimed :tool-name t}))
            (str t " was refused, which is a deadlock rather than a policy"))))

    (testing "`eval` is the exception, and the deadlock argument still holds"
      ;; eval used to sit in the list above. It now requires a PLAN — not a
      ;; task — because a REPL session must begin by naming the files it
      ;; intends to change (karamazov-70b: a run spent 238 turns exploring
      ;; without ever having to say where it thought the bug was).
      ;;
      ;; This is not the deadlock the case above rules out. Every tool you
      ;; ORIENT with stays free: read_file, grep, lsp and shell are all
      ;; unrefused for a branch with neither task nor plan, and reading the
      ;; failing assertion plus the code it calls is how you decide which file
      ;; is lying. What is gated is EXPLORING, which is the step that comes
      ;; after you have a hypothesis.
      (let [planned (assoc unclaimed :repl-plan {:files ["src/a.clj"]})]
        (is (some? (tools-base/phase-refusal {:branch unclaimed :tool-name "eval"}))
            "no plan: the REPL is closed")
        (is (= :repl-needs-a-plan
               (:refusal-rule (tools-base/phase-refusal
                               {:branch unclaimed :tool-name "eval"}))))
        (is (nil? (tools-base/phase-refusal {:branch planned :tool-name "eval"}))
            "a branch that said what it is changing may explore freely")
        (is (nil? (tools-base/phase-refusal {:branch planned :tool-name "plan"}))
            "and may always re-plan")))

    (testing "finishing never needs a task — discarding completed work over a
              missing row is the worst available trade, and ending a run is
              where a bad refusal is least recoverable"
      (doseq [t ["done" "give_up"]]
        (is (nil? (tools-base/phase-refusal {:branch unclaimed :tool-name t})))))))

(deftest a-refusal-rule-is-data-and-can-be-turned-off
  ;; The whole point of the table: a project that does not want a board should
  ;; not have to carry one, and should not need a rebuild to say so.
  (with-redefs [phases/refusals (fn [] [])]
    (is (nil? (tools-base/phase-refusal {:branch {:id "b" :phase :explore}
                                         :tool-name "write_file"}))))
  ;; And a rule with a different condition fires on that condition instead.
  (with-redefs [phases/refusals
                (fn [] [{:rule :never-on-tuesdays
                         :when (fn [ctx] (= "b-doomed" (:id (:branch ctx))))
                         :tools #{"write_file"}
                         :message-file "task-required"}])]
    (is (some? (tools-base/phase-refusal {:branch {:id "b-doomed"} :tool-name "write_file"})))
    (is (nil? (tools-base/phase-refusal {:branch {:id "b-fine"} :tool-name "write_file"})))))

(deftest a-confirmed-artifact-ends-a-reframe
  ;; RFC-007 recorded that :transitions carried one entry and that an
  ;; artifact-status trigger was available and unused. Wiring it needed the
  ;; table to be able to match a VALUE: :claim-status is truthy for :confirmed,
  ;; :empirical and :sketch alike, so the truthy test the table had would have
  ;; fired the confirmed effects on an unverified plan. A status is a
  ;; vocabulary, not a flag.
  ;;
  ;; What it is keyed to is clear-reframe's own long-standing definition —
  ;; "the branch banked something the withheld approach could not have
  ;; produced" — which is a statement about confirming a claim, not about a
  ;; green test run. Keying it here makes it general: ANY tool that confirms a
  ;; claim ends a reframe, not only a green ship-verify.
  (let [reframed (state/begin-reframe (branch-with) 3 "the withheld claim")]
    (is (some? (:reframe-claim reframed)) "the branch is inside a reframe")

    (testing "a confirmed artifact lifts it"
      (is (nil? (:reframe-claim
                 (aloop/apply-transitions {} {:claim-status :confirmed} reframed)))))

    (testing "an unverified plan does not — this is the whole reason the table
              had to learn to match a value rather than test for truth"
      (doseq [status [:sketch :empirical :refuted]]
        (is (some? (:reframe-claim
                    (aloop/apply-transitions {} {:claim-status status} reframed)))
            (str status " lifted a reframe"))))

    (testing "and no artifact at all does not"
      (is (some? (:reframe-claim (aloop/apply-transitions {} nil reframed)))))

    (testing "confirming a claim is not the same as a green working tree"
      (is (nil? (:green-snapshot
                 (aloop/apply-transitions {} {:claim-status :confirmed}
                                          (assoc reframed :turns [{} {}]))))))))

(deftest a-transition-entry-can-test-for-truth-or-match-a-value
  ;; Both forms, at the seam rather than through a whole turn, so the table's
  ;; contract is pinned independently of which effects happen to be wired.
  (with-redefs [phases/transitions (fn [] {[:result :flagged?] [:mark-green]})]
    (is (= [:mark-green] (aloop/transition-effects {:result {:flagged? true}})))
    (is (= [] (vec (aloop/transition-effects {:result {:flagged? false}})))))
  (with-redefs [phases/transitions (fn [] {[:artifact :kind] {:test [:clear-reframe]}})]
    (is (= [:clear-reframe] (aloop/transition-effects {:artifact {:kind :test}})))
    (is (= [] (vec (aloop/transition-effects {:artifact {:kind :lemma}})))
        "a value-keyed entry fires on its value and no other")))

(deftest result-transitions-are-resource-data
  ;; drg-4026 #3: the claim-first state machine as a declarative table —
  ;; tool-result signals map to branch effects, applied generically, not
  ;; cond-> clauses in the executor.
  ;;
  ;; Two live rows now, asking different questions. The green point is about
  ;; the WORKING TREE and keys on the verify signal; ending a reframe is about
  ;; BANKING A CLAIM and keys on a confirmed artifact. :clear-reframe used to
  ;; ride the verify signal, which made a green test run the only thing that
  ;; could end a reframe — narrower than what clear-reframe means.
  (is (= [:mark-green] (get (phases/transitions) [:result :verified-green?])))
  (is (= {:confirmed [:clear-reframe]}
         (get (phases/transitions) [:artifact :claim-status])))
  (let [b (assoc (branch-with) :reframe-entered-turn 3 :reframe-claim "c")]
    (let [out (aloop/apply-transitions {:verified-green? true} nil b)]
      (is (= (count (:turns out)) (:green-snapshot out)))
      (is (= 3 (:reframe-entered-turn out))
          "a green tree is not by itself a banked claim"))
    (let [out (aloop/apply-transitions {} {:claim-status :confirmed} b)]
      (is (nil? (:reframe-entered-turn out)))
      (is (nil? (:green-snapshot out))))
    (is (= b (aloop/apply-transitions {:verified-green? false} nil b))
        "no signal, no transition")))

(deftest winner-rubric-is-resource-data
  ;; drg-4026 #30: the finished-key rubric moved from a tuple literal in
  ;; state.clj to phases.edn forms compiled at load. Retuning it is a data
  ;; edit.
  ;;
  ;; THE NON-RELAXATION COMPONENT IS GONE (karamazov-83p), and this test is
  ;; why it survived so long. It read (:relaxation? (:last-audit branch)), and
  ;; :last-audit was seeded nil by new-branch and written by NOTHING in the
  ;; harness — so the component was a constant in production and never
  ;; separated two branches. The test passed because it manufactured the key
  ;; by hand. A test that supplies an input production cannot produce is
  ;; testing the function and not the feature, and it is exactly how a dead
  ;; ranking rule keeps a green tick.
  (is (= 4 (count (phases/finished-key-forms))))
  (let [b (assoc (branch-with)
                 :artifacts [{:claim-status :confirmed :kind :z3 :turn 1}])]
    (is (= [0 1 1 "B1"] (state/finished-key b)))
    (testing "every remaining component reads something the harness writes"
      (is (= [1 1 1 "B1"] (state/finished-key (assoc b :tiers-seen #{:slow})))
          "tiers-seen is set by record-outcome")))
  (testing "and a branch with more confirmed artifacts still outranks one with
            fewer, which is the rubric's real job"
    (let [more (assoc (branch-with)
                      :artifacts [{:claim-status :confirmed :kind :z3 :turn 1}
                                  {:claim-status :confirmed :kind :prolog :turn 2}])
          less (assoc (branch-with)
                      :artifacts [{:claim-status :confirmed :kind :z3 :turn 1}])]
      (is (= [more less] (state/rank-finished [less more]))))))

(deftest every-context-budget-key-is-actually-read
  ;; A knob that is documented, parsed, and read by nothing is worse than no
  ;; knob: `:run :loop` was exactly that for a whole phase — configured in
  ;; three places and consulted by no live code — so the critic, team, feature
  ;; and decompose loops could not run outside the suite. These numbers decide
  ;; how much the model sees, which makes a dead one invisible in the same way.
  ;; src/samizdat, not src: the vendored trees under src/ would widen the
  ;; haystack, and a key that only "appears" in a mycelium docstring is not a
  ;; key samizdat reads.
  (let [src (->> (file-seq (java.io.File. "src/samizdat"))
                 (filter #(.isFile %))
                 (filter #(str/ends-with? (.getName %) ".clj"))
                 (map slurp)
                 (str/join "\n"))
        declared (keys (gates/threshold :context-budget))]
    (is (seq declared))
    (doseq [k declared]
      ;; Either spelling counts: the explicit keyword, or the bare name as a
      ;; {:keys [...]} destructuring binding. Matching only the keyword failed
      ;; on the first key that was read idiomatically, and a test that dictates
      ;; destructuring style to avoid a false positive is the wrong trade.
      (is (or (str/includes? src (str k))
              (re-find (re-pattern (str ":keys \\[[^\\]]*\\b"
                                        (java.util.regex.Pattern/quote (name k))
                                        "\\b"))
                       src))
          (str k " is declared in gates.edn :context-budget but nothing in src"
               " reads it — either wire it or drop it")))))

(deftest a-prediction-tells-late-compliance-from-none
  ;; RFC-007: "a prediction's :window is in turns, so a gate whose advice takes
  ;; longer than its window to follow settles :unmet regardless of whether it
  ;; worked." Two outcomes could not tell apart the gate nobody obeys and the
  ;; gate whose advice is sound but slow — and those want opposite repairs:
  ;; reword the first, widen the second's window.
  (let [p {:gate :milestone :turn 5 :window 2}
        at (fn [turn called] {:current-turn turn :tools-called called
                              :branch-before {} :branch-after {}})]
    (testing "inside the window is prompt compliance"
      (is (= :met (arbiter/settle p (at 6 ["done"])))))

    (testing "after the window but inside the grace is LATE compliance,
              which used to be indistinguishable from never"
      (is (= :met-late (arbiter/settle p (at 9 ["done"])))))

    (testing "silence past the window is still open — the grace is what makes
              late compliance observable at all"
      (is (nil? (arbiter/settle p (at 8 [])))))

    (testing "silence past the grace is :unmet, which now means what it says"
      (is (= :unmet (arbiter/settle p (at 12 [])))))))

(deftest the-grace-is-a-threshold-not-a-constant
  (is (pos? (gates/threshold :prediction-grace-turns))
      "a project whose turns are slower wants a wider grace, and that is a
       retune rather than a rebuild")
  (let [p {:gate :milestone :turn 0 :window 1}]
    (with-redefs [gates/threshold (fn [k] (if (= k :prediction-grace-turns) 0
                                              (#'gates/threshold k)))]
      (is (= :unmet (arbiter/settle p {:current-turn 1 :tools-called []
                                       :branch-before {} :branch-after {}}))
          "a zero grace restores the old two-outcome behaviour exactly"))))

(deftest progress-stalled-settles-on-what-it-armed-on
  ;; The gate fires when :turns-since-progress crosses a threshold, and that
  ;; counter is reset by any tool reporting :progress? true — write_file and
  ;; edit_file among them. It used to settle on ARTIFACTS, and the only
  ;; artifact a coding run produces is a green test through the ship gate. So
  ;; it armed on one definition of progress and was graded on a stricter one.
  ;;
  ;; Measured across four live runs against a real project: eight firings,
  ;; eight `unmet`, and zero artifacts produced in any of them — the outcome
  ;; could not have been anything else. One run fired the gate at turn 30 and
  ;; wrote files at 31 and 32; the ledger recorded the branch as ignoring it.
  ;; That false zero feeds session findings, which feed the supervisor, which
  ;; is the role that retunes the loop on the evidence.
  (let [firing {:gate :progress-stalled :turn 1 :window 3}
        settle (fn [before after]
                 (arbiter/settle firing {:current-turn 2 :tools-called ["write_file"]
                                         :branch-before before :branch-after after}))]
    (testing "a reset progress counter settles met, with no artifact in sight"
      (is (= :met (settle (branch-with :turns-since-progress 9 :any-progress? true)
                          (branch-with :turns-since-progress 0 :any-progress? true)))))
    (testing "a still-climbing counter does not"
      (is (nil? (settle (branch-with :turns-since-progress 9)
                        (branch-with :turns-since-progress 10)))))
    (testing "an artifact still settles it — a green test is the strongest form"
      (is (= :met (settle (branch-with :turns-since-progress 9)
                          (branch-with :turns-since-progress 10
                                       :artifacts [{:claim "tests pass"
                                                    :claim-status :confirmed :turn 2}])))))
    (testing "and it still expires when the branch really does nothing"
      (is (= :unmet (arbiter/settle firing
                                    {:current-turn 20 :tools-called ["read_file"]
                                     :branch-before (branch-with :turns-since-progress 9)
                                     :branch-after (branch-with :turns-since-progress 28)}))))))

(deftest every-nudge-that-fires-on-a-cadence-is-bounded
  ;; A gate whose :when is a condition stops firing when the condition clears.
  ;; One that fires on a CADENCE has no such brake, so an unbudgeted cadence
  ;; gate nags for the whole run: reflection fired 11 times across four live
  ;; runs with zero compliance, each firing a tax on the branch's context paid
  ;; to ask again for something already declined ten times.
  (doseq [g (gates/gates)
          :when (str/includes? (str (:when g)) "cadence")]
    (is (some? (:budget g))
        (str (:gate g) " fires on a cadence and has no :budget — nothing bounds it"))))

(deftest a-production-refusal-feeds-the-refusal-counter-not-the-cull-counter
  ;; karamazov-blt.15. The synthetic-input test above proves the COUNTER works;
  ;; this proves a production path actually produces the pair it needs.
  ;; Refusals used to go out as :failure, so a task-less branch refused N
  ;; times by the work-needs-a-task rule died as "N consecutive failures" —
  ;; the vf-jki lie in a sixth place.
  (let [b (state/new-branch {:id "B" :problem "p"})
        r (tools-base/phase-refusal {:branch b :tool-name "write_file"})]
    (is (some? r) "the task-required rule refuses work from a task-less branch")
    (is (= :mechanics (:category r)))
    (is (true? (:policy-refusal? r)))
    (let [b' (state/record-outcome b {:category (:category r)
                                      :policy-refusal? (:policy-refusal? r)})]
      (is (= 1 (:consecutive-policy-refusals b')))
      (is (zero? (:consecutive-failures b'))
          "a declined call is not evidence about the branch's line of inquiry"))))

(deftest a-shell-deny-is-a-refusal-not-a-failure
  ;; The hard-deny arm returned :failure while the tool's own comment claimed
  ;; :neutral; every deny charged the counter that kills branches
  ;; (karamazov-blt.15).
  (let [b (state/new-branch {:id "B" :problem "p"})
        r (tools-base/run-tool {:tool-name "shell" :branch b :root "/tmp"
                          :args {:command "rm -rf /"}})]
    (is (= :mechanics (:category r)))
    (is (true? (:policy-refusal? r)))))

(deftest budget-arithmetic-runs-in-global-turns
  ;; karamazov-blt.16. max-turns, artifact stamps and gate-history stamps are
  ;; all GLOBAL turns; (count :turns) is the branch's own experience. Mixing
  ;; them meant a fork born at round 18 of 25 read as turn ~0 — never told to
  ;; ship — while its parent's ten-round-old artifact read as "recent"
  ;; forever, making it cull-exempt.
  (let [fork (assoc (state/new-branch {:id "F" :problem "p"}) :current-turn 18)]
    (is (= 18 (state/turn-count fork))
        "the loop's per-turn stamp wins over the log length")
    (is (zero? (state/own-turn-count fork))
        "while the branch's own experience stays separate (juvenile-grace's unit)")
    (let [b (update fork :artifacts conj {:claim-status :confirmed :turn 5})]
      (is (false? (state/banked-in-last b 6))
          "an artifact banked at global turn 5 is not recent at turn 18")
      (is (true? (state/banked-in-last (assoc b :current-turn 9) 6))
          "and IS recent when the run is actually at turn 9")))
  (is (= 7 (:current-turn (aloop/phase-valve (state/new-branch {:id "B" :problem "p"}) 7)))
      "phase-valve is where the stamp lands, at the top of every turn"))

;; --- settle is its own step (karamazov-aqsr.2) -------------------------------

(deftest settle-step-closes-what-the-turn-resolved-and-says-how-many
  ;; Settling used to be the first thing steer-step did, which made
  ;; settle-before-fire a convention inside one cell. It is a node now, and
  ;; its product — the branch with its predictions closed, and the count —
  ;; is what :gate/arbiter's schema requires, so the order is compiled.
  (let [c (db/open! ":memory:")]
    (try
      (db/migrate! c)
      (let [rid (runs/start-run! c {:problem "p" :beam-width 1})
            _ (runs/open-branch! c rid {:branch-id "B1" :created-at-turn 0})
            fid (journal/record-gate! c rid {:branch-id "B1" :turn 1
                                             :gate :stuck :priority 1
                                             :message "m" :prediction "p"
                                             :window 3})
            before (state/new-branch {:id "B1" :problem "p"})
            open {:id fid :gate :stuck :prediction "p" :window 3 :turn 1}
            b (assoc before :open-predictions [open])]
        (testing "a prediction whose window has passed is closed and counted"
          (let [{:keys [branch closed]}
                (aloop/settle-step {:conn c} before b 10 {:parsed {:name "read_file"}})]
            (is (= 1 closed))
            (is (empty? (:open-predictions branch)))))
        (testing "one still inside its window stays open, and the count says so"
          (let [{:keys [branch closed]}
                (aloop/settle-step {:conn c} before b 2 {:parsed {:name "read_file"}})]
            (is (= 0 closed))
            (is (= [open] (:open-predictions branch))))))
      (finally (db/close c)))))
