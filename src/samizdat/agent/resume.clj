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

(ns samizdat.agent.resume
  "Resume a crashed run by replaying its journal.

  UCLA's recovery is event-sourced replay plus a persisted run-start anchor,
  so a restart does not re-grant the budget. The anchor here is the turns
  table and the runs row: a resumed run continues at (last recorded turn)+1
  against the run's ORIGINAL max_turns, so a crash at hour N does not buy N
  more hours. state.clj promises that everything a gate needs is journalled;
  this namespace is what makes the promise true.

  Fidelity contract, per field:

  REPLAYS EXACTLY, from tables verbatim:
  - branch status, inactive-reason, created-at-turn, and thesis (branches row).
  - artifacts with claim-status / verdict / witness (artifacts rows).
  - failures are re-read fresh by the loop from the shared log, as always.
  - message history from the turns table: assistant_text is what the model
    said, result is what the harness answered, over the system prompt and the
    run's recorded problem (loop/initial-messages). Steer messages that fired
    pre-crash are not in the journal, so they are not replayed; that is good
    enough for the model to continue and is the accepted fidelity gap.

  REPLAYS CONSERVATIVELY, recomputed so no guard re-fires on its own past:
  - consecutive-failures, turns-since-progress, any-progress? from the turns
    category column through state/record-outcome, the same function the live
    loop uses. progress? is approximated as (= category :success) because the
    tool's own progress? flag is not journalled.
  - gate-history and open-predictions from gate_firings rows, so a gate that
    fired pre-crash counts against its budget after resume and unsettled
    predictions still settle at the next boundary.
  - notified-fractions from the rebuilt turn count via gates/crossed-fractions,
    so the turn-budget notice does not re-fire on fractions already crossed.
  - tiers-seen from rebuilt artifact tiers.
  - mechanics from the turns parse_error / auto_repaired columns; fence
    signals that are not journalled (truncation, multi-fence) reset to zero.
  - consecutive-policy-refusals replayed from the turns policy_refusal
    column, through record-outcome exactly as the live loop counts it.
  - phase from the banked sketch artifacts, phase-entered-turn from the first
    sketch's turn. A cap-forced build with no sketch resumes as :explore and
    re-fires the cap check — the re-entry can only grant extra explore turns,
    never fewer.

  DOES NOT REPLAY (known limits, accepted):
  - Lean: a resumed branch gets no Lean session and its Lean tools report the
    session unavailable. Lean sessions are process memory with a heavy startup
    cost; a crash cannot resurrect them.
  - The safe-state green snapshot is branch memory, not journalled (mark-green
    assocs into the branch map only), so a resumed branch starts a fresh empty
    Prolog session and the safe-state rung has no fallback until its next
    confirmation. In-process safe-state aborts are unaffected.
  - last-review / last-audit are branch memory; the done gate re-requires
    review/audit after a resume rather than trusting pre-crash state.
  - The claim registry is run-scoped memory; resume starts it empty, safe
    because the worst case is one duplicate slow verification.
  - :shared-served (which shared artifacts a branch was already told about)
    is branch memory; resume starts it empty, so each artifact-branch pair
    can journal one duplicate shared-artifact-hit after a resume.
  - :premises-served (which retrieved premises a branch was already handed,
    vf-3wg) is branch memory for the same reason; a resumed branch may be
    re-served one premise block.
  - the forced reframe (vf-9wx): :reframe-claim and :reframe-entered-turn are
    branch memory, so a branch that crashed mid-reframe resumes with neither
    the withheld approach nor the reprieve that goes with it. Deriving them
    would be easy and wrong — the last stuck firing's turn is in gate_firings
    and the last failed claim is in the turns args, but the two need not
    belong to the same reframe, and reinstating a withholding the harness
    cannot justify blocks work the branch is entitled to do. Erring toward the
    permissive direction is the right way to be wrong about a gate that
    REFUSES things.

    It largely self-heals: consecutive-failures replays, so the stuck gate
    re-fires at the branch's next boundary and re-enters the reframe with a
    claim it can justify. The residual gap is a branch whose max-stuck-hints
    budget is already spent, which loses the reprieve and can be culled for
    the failures that caused the reframe. Bounded by :reframe-grace, and the
    cull was always the documented backstop.
  - :critic scores and :fork-invited markers are branch memory; a resumed
    branch is re-scored at its next boundary, and may be re-invited to fork
    sooner than the cooldown would otherwise allow.
  - the per-turn :error that repeating-failure? compares is not replayed, so a
    branch that was looping on one identical failure gets one un-escalated
    answer after a resume before the harness can see the loop again. The turns
    table does hold the result text, but it holds the ESCALATED copy, which
    would not compare equal to the next clean one — replaying it would break
    the detection it was meant to restore."
  (:require [clojure.data.json :as json]
            [samizdat.agent.beam :as beam]
            [samizdat.agent.gates :as gates]
            [samizdat.agent.gitdiff :as gitdiff]
            [samizdat.agent.loop :as branch-loop]
            [samizdat.agent.state :as state]
            [samizdat.agent.storm :as storm]
            [samizdat.agent.tools.tasks :as task-tool]
            [samizdat.repl :as repl]
            [samizdat.store.journal :as journal]
            [samizdat.store.runs :as runs]
            [samizdat.store.tasks :as tasks]
            [samizdat.workflow :as workflow]))

(defn- parse-json [s]
  (when s
    (try (json/read-str s :key-fn keyword) (catch Throwable _ s))))

(defn- artifact-map
  "An artifacts row back into the map the tools conj onto the branch."
  [a]
  {:turn (:turn a)
   :kind (keyword (:kind a))
   :claim (:claim a)
   :code (:code a)
   :verdict (some-> (:verdict a) keyword)
   :witness (parse-json (:witness a))
   :claim-status (keyword (:claim_status a))
   :tier (keyword (:tier a))})

(defn- messages-from-turns
  "The message history a continuing model needs: what it said, and what the
  harness answered, over the system prompt and the problem."
  [problem turns]
  (reduce (fn [msgs t]
            (cond-> msgs
              (seq (:assistant_text t))
              (conj {:role "assistant" :content (:assistant_text t)})
              (seq (:result t))
              (conj {:role "user" :content (:result t)})))
          (branch-loop/initial-messages problem)
          turns))

(defn- rebuild-branch
  "Reconstruct one branch's working map from the journal rows for its run.

  The green snapshot is not journalled, so a resumed branch always starts
  the safe-state rule from its 'otherwise' arm."
  [run branch-row turns artifacts firings max-turns storm-pol]
  (let [branch-id (:id branch-row)
        ;; The branch's OWN problem where it has one — a decompose unit's
        ;; contract, a team worker's sub-task — else the run's. Rebuilding
        ;; every branch on the run-level problem re-aimed every worker at the
        ;; top-level feature text (karamazov-blt.23).
        problem (or (not-empty (str (:problem branch-row))) (:problem run))
        branch-turns (get turns branch-id [])
        ;; The phase is rebuilt from the banked sketch artifacts: a sketch on
        ;; record means the branch left explore, and its turn is the phase
        ;; entry. A cap-forced build with no sketch resumes as :explore and
        ;; re-fires the cap check on the next turn — conservative, and the
        ;; re-entry can only grant extra explore turns, never fewer.
        artifact-maps (mapv artifact-map (get artifacts branch-id []))
        first-sketch (some #(when (= :sketch (:claim-status %)) (:turn %))
                           artifact-maps)
        base (-> (state/new-branch {:id branch-id
                                    :parent-id (:parent_id branch-row)
                                    :problem problem
                                    :created-at-turn (:created_at_turn branch-row)
                                    :messages (messages-from-turns problem
                                                                   branch-turns)})
                 (assoc :status (keyword (:status branch-row))
                        :inactive-reason (:inactive_reason branch-row)
                        :thesis (parse-json (:thesis branch-row))
                        :artifacts artifact-maps
                        :phase (if first-sketch :build :explore)
                        :phase-entered-turn (or first-sketch
                                                (:created_at_turn branch-row))
                        :gate-history (mapv (fn [f]
                                              {:gate (keyword (:gate f))
                                               :turn (:turn f)})
                                            (get firings branch-id []))
                        :open-predictions (mapv (fn [f]
                                                  {:id (:id f)
                                                   :gate (keyword (:gate f))
                                                   :prediction (:prediction f)
                                                   :window (:window f)
                                                   :turn (:turn f)})
                                                (remove #(:outcome %)
                                                        (get firings branch-id [])))))
        ;; The counters, replayed through the same functions the live loop
        ;; uses — WITH the live loop's call discipline, not one of our own
        ;; (karamazov-blt.22): a provider-error row is journalled but the live
        ;; loop applies NEITHER add-turn nor record-outcome to it (the branch
        ;; never got an answer to be wrong about — replaying it as :neutral
        ;; DECREMENTED consecutive-failures and reset the mechanics
        ;; counters), and a no-call/parse-error row records the outcome but
        ;; appends no :turns entry. progress? is approximated from the
        ;; category because the tool's own progress? flag is not journalled.
        branch (reduce (fn [b t]
                         (let [cat (some-> (:category t) keyword)
                               tool (:tool_name t)]
                           (cond
                             (= "__provider_error__" tool)
                             b

                             (contains? #{"__no_call__" "__parse_error__"} tool)
                             (state/record-outcome b {:category cat
                                                      :progress? false})

                             :else
                             (-> b
                                 (state/add-turn {:turn (:turn t)
                                                  :tool tool
                                                  :category cat})
                                 (state/record-outcome {:category cat
                                                        :progress? (= :success cat)
                                                        :policy-refusal? (pos? (or (:policy_refusal t) 0))})))))
                       base
                       branch-turns)
        branch (assoc branch
                      :tiers-seen (set (keep :tier (:artifacts branch)))
                      ;; The storm window rebuilds from the journal's verbatim
                      ;; args, so a resumed branch keeps its repeat protection
                      ;; — unlike repeating-failure?, which the docstring
                      ;; above documents comes back blind. Strikes are not
                      ;; rebuilt (a refusal row is not distinguishable from a
                      ;; malformed one), so the give_up escalation restarts;
                      ;; the withhold itself does not.
                      :storm-window (storm/window-from-turns
                                     branch-turns storm-pol)
                      :mechanics {:calls (count branch-turns)
                                  :parse-errors (count (filter :parse_error branch-turns))
                                  :auto-repairs (count (filter #(pos? (:auto_repaired %))
                                                               branch-turns))
                                  :unknown-tools 0 :truncations 0 :multi-fences 0}
                      ;; The fractions already crossed are told, so the
                      ;; turn-budget notice does not re-fire on them.
                      :notified-fractions (gates/crossed-fractions branch max-turns))]
    branch))

(defn resumable?
  "A run can be resumed when it exists and has not reached a terminal state.

  An aborted run STAYS aborted: abort is a person saying stop, and a resume is
  not a person changing their mind. A completed run shipped; the answer is the
  record. Only a run still in flight — status 'running', or 'failed' from an
  exhausted process that never got to tear down — is resumable."
  [conn run-id]
  (when-let [r (runs/get-run conn run-id)]
    (not (contains? #{"completed" "aborted"} (:status r)))))

(defn resume!
  "Rebuild a run's branches from the journal and continue the beam's round
  loop at the round after the last recorded turn, under the run's ORIGINAL
  max-turns.

  `:max-turns` is the one exception to the anchor rule, and it is explicit:
  when passed AND larger than the recorded budget, the runs row is raised to
  it and branches closed as `exhausted` reopen — they closed because the
  budget ran out, not for cause, so more budget un-closes them. Culled,
  abandoned, and done branches stay closed. The extension is journaled
  (budget-extended, branch-reopened) and the rows are updated BEFORE the
  branches are read back, so a crash mid-extension replays correctly. A
  crash still cannot re-grant budget; only a caller asking for more can.

  Pending interventions are already in their table; the existing
  pending-directives drain picks them up at the first resumed boundary — this
  function does not reimplement that path.

  Returns the beam/run-rounds result. Throws when the run is not resumable."
  [{:keys [conn config llm-adapter llm-config run-id abort max-turns]}]
  (let [run (runs/get-run conn run-id)]
    (when-not (resumable? conn run-id)
      (throw (ex-info (str "run " run-id " is not resumable (status "
                           (:status run) ")")
                      {:run-id run-id :status (:status run)})))
    ;; The row said 'interrupted' (or 'failed'); it is about to be running
    ;; again, and stalled? only watches runs whose status says so.
    (runs/mark-running! conn run-id)
    (when (and max-turns (> max-turns (:max_turns run)))
      (runs/extend-budget! conn run-id max-turns)
      (doseq [b (runs/branches conn run-id)
              :when (= "exhausted" (:status b))]
        (runs/reopen-branch! conn run-id (:id b))))
    (let [max-turns (max (or max-turns 0) (:max_turns run))
          width (:beam_width run)
          turn-rows (journal/turns conn run-id)
          turns (group-by :branch_id turn-rows)
          artifacts (group-by :branch_id (journal/artifacts conn run-id))
          firings (group-by :branch_id (journal/gate-firings conn run-id))
          ;; Same three keys run! sets. A resumed run works on the same tree
          ;; and needs the same file root; the eval session is genuinely new,
          ;; because the old process's namespace died with it and nothing in
          ;; the journal can rebuild an in-image binding.
          root (or (get-in config [:run :root]) (System/getProperty "user.dir"))
          ;; The same compiled per-turn manifest run! drives, recompiled here:
          ;; a resume enters run-rounds directly, so without this it would
          ;; silently fall back to the bare composition and finish a critic or
          ;; feature run on the factory loop.
          loop-nm (workflow/active-loop-name config)
          {turn-wf :compiled iterating? :iterating?}
          (workflow/compile-turn-loop conn loop-nm)
          ctx {:conn conn :run-id run-id :config config :problem (:problem run)
               :llm-adapter llm-adapter :llm-config llm-config
               :max-turns max-turns :beam? (> width 1) :beam-width width
               ;; The budget the run STARTED under, like max-turns: a resume
               ;; continues the same bound, it does not re-grant it.
               :token-budget (:token_budget run)
               :root root
               :turn-workflow turn-wf
               :iterating-loop? iterating?
               ;; Baselined at the RESUME, so a critic reviewing the resumed
               ;; run sees what the resumption changed. What the dead process
               ;; changed is already committed to the tree it starts from.
               :git-baseline (gitdiff/baseline root)
               :repl-session (repl/new-session)
               :abort abort}
          ;; :verify-cmd rides on the policy so the rebuilt window skips
          ;; verify calls exactly the way the live path never notes them.
          storm-pol (assoc (gates/storm-policy)
                           :verify-cmd (get-in config [:run :verify-cmd]))
          branches (mapv (fn [row]
                           (let [b (rebuild-branch run row turns artifacts
                                                   firings max-turns storm-pol)
                                 ;; The task claim survives the crash on its
                                 ;; ROW; without restoring it here the branch
                                 ;; came back reading "No task claimed", could
                                 ;; claim a second task, and left the old one
                                 ;; in_progress and attributed to it forever
                                 ;; (karamazov-blt.21). The pinned statement
                                 ;; is re-appended through the same renderer
                                 ;; the claim used.
                                 held (tasks/held-by conn run-id (:id b))]
                             (cond-> b
                               held (assoc :task {:id (:id held)
                                                  :title (:title held)})
                               held (task-tool/task-statement held))))
                         (runs/branches conn run-id))
          ;; The anchor: rounds completed are the max turn in the journal, so
          ;; the loop continues one past it. max-turns is the ORIGINAL budget.
          start-turn (inc (reduce max 0 (map :turn turn-rows)))]
      (beam/run-rounds ctx branches start-turn))))
