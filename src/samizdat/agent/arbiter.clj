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

(ns samizdat.agent.arbiter
  "One arbiter per boundary, emitting at most one message.

  The TypeScript harness injects its milestone prompt, its emergency-review
  prompt and its stuck hint from three independent conditionals, so a branch
  can receive three messages before a single turn, each pushing a different
  direction. dirge found up to five and replaced the chain with one arbiter
  choosing in strict priority. That is the behavior-tree fallback node, and it
  is the single highest-value structural change available here.

  Two properties follow and both are asserted in the tests.

  Exactly one steer per boundary, always. Not zero-or-one per gate.

  A gate that fires records what it expects to happen next, and a later turn
  settles that prediction from the journal with no model in the path. A gate
  whose predictions never settle is not steering anything, and that is
  measurable rather than arguable.

  MECHANISM, with nothing of its own to say. Every input to the choice is gate
  DATA: which gates exist and their :when forms, their :priority, and the
  budget key each spends against. This namespace filters on those, sorts on
  those, and takes the first. So a project retunes what the harness says to a
  branch, and when, by editing gates.edn — not by editing this."
  (:require [samizdat.agent.gates :as gates]
            [samizdat.agent.state :as state]
            [samizdat.util :as util]))

(defn eligible
  "Every gate whose precondition holds and whose budget is not spent, in
  priority order. Exposed so a test can assert what the arbiter passed over,
  not only what it chose."
  [ctx]
  (->> (gates/gates)
       (filter (fn [g] (and ((:when g) ctx)
                            (not (gates/budget-exceeded? g (:branch ctx))))))
       (sort-by :priority)))

(defn decide
  "Pick at most one steer for this boundary.

  Returns nil when nothing applies, or {:gate :message :prediction :window
  :priority :passed-over}. `:passed-over` names the gates that also held, which
  is what makes the gate tally's co-occurrence column readable — a gate that
  only ever fires alone tells you something different from one that is
  perpetually outranked."
  [ctx]
  (let [candidates (eligible ctx)]
    (when-let [chosen (first candidates)]
       {:gate (:gate chosen)
        :priority (:priority chosen)
        :message ((:message chosen) ctx)
        :prediction ((:prediction chosen) ctx)
        ;; The tool this gate's prediction names, when it names exactly one.
        ;; Carried so the steer can be prefilled instead of merely asked for.
        :tool (:tool chosen)
        ;; drg-4026 #4: the gate's branch-state effect, if any — a key on the
        ;; gate's own data, applied by loop/apply-effects.
        :effect (:effect chosen)
        :window (:window chosen)
        :passed-over (mapv :gate (rest candidates))})))

(defn prefill-for
  "The partial assistant text that forecloses a prose answer on the steered
  turn, or nil when no gate fired.

  Gate predictions settled 4 met to 22 unmet in gen-19 and 9 to 27 in gen-20.
  Across both runs the gates that changed behaviour were the ones that
  WITHHELD something — the audit's refusals redirected work — while the ones
  that merely suggested did not: milestone and tier-escalation each went
  0-for-4. Ending the request mid-fence is the withholding form of a
  suggestion; the model cannot reply in prose because it is already inside a
  tool call.

  A gate that names one tool gets that name too, which is the cheapest fix for
  a prediction like \"the branch runs a slow-tier check\" — not a callable
  thing. A gate naming a choice gets the bare fence: forcing one of two would
  be the harness deciding rather than steering."
  [decision]
  (when decision
    (if-let [t (:tool decision)]
      (str "```tool-call\n{\"name\": \"" t "\"")
      "```tool-call\n")))

(def ^:private forceable
  "Schemas for the tools a gate may FORCE via native tool_choice, from
  gates.edn :forceable-tools (tier 3b: data). Only the terminal tools —
  forcing a mid-task tool would be the harness deciding rather than
  steering. Minimal: enough for the provider to accept the function.

  Read through the config generation rather than snapshotted at namespace
  load, so reload-config! reaches it like every other derived table."
  (util/generation-cache gates/gen #(:forceable-tools (gates/config))))

(defn force-tool-for
  "The native-tool spec to FORCE for a decision naming a forceable tool, or nil.

  The provider-agnostic force (native tool_choice), which supersedes the prefill
  for tool-naming gates: a prefill only lands where the provider continues a
  trailing assistant message (DeepSeek /beta), and a gate that must land a done
  on GLM cannot rely on that. See samizdat.llm.adapter.openai."
  [decision]
  (when-let [t (:tool decision)]
    (get (forceable) t)))

;; --- settling predictions ---------------------------------------------------

(defn- progressed?
  "Whether the branch advanced over this turn.

  THE GATE MUST SETTLE ON WHAT IT ARMED ON. `progress-stalled` fires when
  `:turns-since-progress` crosses a threshold, and that counter is reset by
  any tool call reporting `:progress? true` — which on a coding run means
  `write_file` and `edit_file`, because changing the tree is real work. It
  used to settle on artifacts alone, and the only artifact a coding run
  produces is a green test run through the ship gate. So the gate armed on one
  definition of progress and was graded on a stricter one.

  Measured across four live runs against a real project: `progress-stalled`
  fired eight times and settled `unmet` eight times, and could not have
  settled anything else — zero artifacts were produced in any of them. Turn 30
  of one run fired the gate and turns 31 and 32 wrote files; the ledger
  recorded the branch as ignoring it. That false zero is not cosmetic: it
  feeds session findings, which feed the supervisor, which is the role that
  retunes the loop. A GA mechanic reading a fabricated failure signal selects
  against noise.

  Artifacts still count — a green test is the strongest form of this — but a
  reset counter counts too, and it is the one a coding branch can actually
  reach."
  [before after]
  (or (> (count (state/confirmed-artifacts after))
         (count (state/confirmed-artifacts before)))
      (> (count (:artifacts after)) (count (:artifacts before)))
      (< (or (:turns-since-progress after) 0)
         (or (:turns-since-progress before) 0))))

;; The tool names each gate's settle reads as compliance (provenance R3-6). Tier
;; 1a: the table lives in gates.edn as :tool-vocab :settle-called — data,
;; not inline strings in a case, so a test can walk it and assert every name
;; resolves to a registered run-tool: a settle name the loop can never
;; dispatch is a gate whose prediction silently narrows to :unmet. The
;; proof-era names (verify_template, review, audit, retract_rule, add_rule,
;; sketch) came out with their tool surface; what remains is the live
;; vocabulary.

(defn settle-called-names
  "Every tool name any settle rule reads as compliance, deduped. Exposed for
  the vocabulary test (provenance R3-6)."
  []
  (distinct (mapcat seq (vals (gates/tool-vocab :settle-called)))))

(def settled-by-rule
  "The gates `settle` decides with a rule of its own rather than by asking
  which tool was called — so they are the ones legitimately absent from the
  :settle-called vocabulary. Kept beside the `case` below and asserted
  against it by the coherence test, so a gate that falls out of both is
  caught at test time rather than by settling :unmet forever."
  #{:stuck :prologue-cap :progress-stalled :file-thrash :human-directive})

(defn settle
  "Decide whether an open prediction came true, given what the branch did.

  Deterministic and cheap: the questions are all of the form 'did the branch
  call one of these tools' or 'did it produce something new'. No model is
  consulted, because a judge asked to grade the harness's own steering is a
  judge with an opinion about the harness.

  Returns :met, :met-late, :unmet, or nil for still-open.

  THREE outcomes, not two, because two could not tell apart the gate nobody
  obeys and the gate whose advice is sound but slow — and those want opposite
  responses. A prediction's :window is in turns; anything a gate asks for
  costs at least a turn to attempt and another to verify, so advice followed
  on the third turn of a two-turn window used to settle :unmet and read
  exactly like advice ignored. The window is now the promptness bar and the
  grace (gates.edn :prediction-grace-turns) is the did-it-happen-at-all bar."
  [{:keys [gate turn window]} {:keys [current-turn tools-called branch-before branch-after]}]
  (let [settle-called (gates/tool-vocab :settle-called)
        elapsed (- current-turn turn)
        late? (> elapsed window)
        expired? (>= elapsed (+ window (gates/threshold :prediction-grace-turns)))
        ;; `#{}` rather than nil for a gate the vocabulary does not name:
        ;; `(some nil tools-called)` calls nil as a predicate and throws, so a
        ;; gate added to gates.edn :gates without a :settle-called entry took
        ;; the settle path down with it on its first firing. Adding a gate is
        ;; supposed to be a data edit; a missing vocabulary now settles :unmet
        ;; at the window instead, and the coherence test names the omission.
        tools-met? (fn [g] (boolean (some (get settle-called g #{}) tools-called)))]
    (cond
      (case gate
        ;; Now that the gate withholds rather than suggests (vf-49o), what
        ;; counts as compliance is what the branch DOES about the refused
        ;; approach: a new plan, a restated thesis — or a result the withheld
        ;; approach could not have produced, whatever tool produced it. The
        ;; last clause matters because the reframe is claim-scoped, and
        ;; complying looks exactly like verifying something else successfully.
        :stuck (or (tools-met? :stuck)
                   (progressed? branch-before branch-after)
                   ;; A verification the harness ACCEPTED while the reframe
                   ;; stood. The withheld claim is refused for as long as the
                   ;; reframe lasts, so a call that got through is on a
                   ;; different one by construction — which is precisely what
                   ;; the refusal asked for, and it banks nothing, so neither
                   ;; progressed? nor any named tool can see it.
                   ;;
                   ;; Without this the gate records a miss against its own
                   ;; success. Watched it happen on a live knights-3 run
                   ;; (2026-08-18): B2 was withheld "A is a knave, B is a
                   ;; knight, and C is a knave", dropped it, opened a proof on
                   ;; something else, and the firing settled unmet. Since the
                   ;; met-rate is the only evidence any of this works, a
                   ;; blind spot there would have re-measured the same
                   ;; misleading zero vf-31m exists to correct.
                   ;;
                   ;; The MECHANICS counter, not the policy one. Every call
                   ;; the harness declines increments it and every call it
                   ;; accepts clears it, so it covers refusals the phase policy
                   ;; never sees. Keying on the policy counter alone produced a
                   ;; false met on gen-31 B2.3 (2026-08-18): the branch's next
                   ;; turn was declined with "That statement is already proved
                   ;; ... in different words", which is :mechanics but not a
                   ;; policy refusal, and the gate was credited with compliance
                   ;; that had not happened. A settling that can be wrong in
                   ;; THIS direction is worse than one that is blind, because
                   ;; the met-rate is the only evidence the reframe works.
                   ;; Known soft spot: eval and shell carry no claim and are
                   ;; never refused, so they read as compliance on their own.
                    (and (:reframe-entered-turn branch-after)
                         (some (set tools-called) (gates/tool-vocab :verification))
                         (zero? (or (:consecutive-mechanics-failures branch-after) 0))))
        (:prologue-cap :progress-stalled) (progressed? branch-before branch-after)
        ;; Met when the streak actually broke: the branch touched a different
        ;; file, a no-file call intervened, or the task closed. Reading the
        ;; streak itself is the only settle that cannot be gamed by naming a
        ;; tool — the nudge asks for divergence, and divergence is what the
        ;; counter measures (karamazov-g86).
        :file-thrash (< (get-in branch-after [:file-touch :streak] 0)
                        (gates/threshold :file-thrash-threshold))
        :human-directive true
        (tools-met? gate))
      (if late? :met-late :met)

      expired? :unmet
      :else nil)))
