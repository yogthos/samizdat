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

(ns samizdat.agent.loop
  "WHAT A TURN IS MADE OF. The steps — assemble, call, absorb, dispatch,
  journal, arbitrate, steer — as public functions with nothing composing them.

  What a turn IS lives in the loop manifest, whose cells call these. This
  namespace held a `run-turn` that composed them itself, which meant there were
  two definitions of a turn and an edit to the manifest reached only one; see
  samizdat.workflow/run-turn, which is now the only composition.

  Nothing here assumes it is running alone — every write carries a branch id —
  because the beam schedules many branches through the same steps.

  THE ORDER IS LOAD-BEARING, and the manifest is where it is now written down.
  The tool runs before the arbiter, so a gate sees the state the turn produced
  rather than the state it started from. Predictions settle before new gates
  fire, so a gate cannot be credited with an outcome that preceded it. Those
  constraints are the manifest's to keep; what this namespace guarantees is
  that each step does one thing and says what it touched."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.logging :as log]
            [samizdat.agent.arbiter :as arbiter]
            [samizdat.agent.files :as files]
            [samizdat.agent.gates :as gates]
            [samizdat.agent.infer :as infer]
            [samizdat.config :as config]
            [samizdat.agent.phases :as phases]
            [samizdat.agent.roles :as roles]
            [samizdat.agent.state :as state]
            [samizdat.agent.storm :as storm]
            [samizdat.agent.thinking :as thinking]
            [samizdat.agent.tools :as tools]
            [samizdat.agent.skills :as skills]
            [samizdat.llm.message :as message]
            [samizdat.prompt :as prompt]
            [samizdat.session :as session]
            [samizdat.store.artifacts :as artifacts]
            [samizdat.store.failures :as failures]
            [samizdat.store.interventions :as interventions]
            [samizdat.store.journal :as journal]
            [samizdat.store.knowledge :as knowledge]
            [samizdat.store.messages :as messages]
            [samizdat.store.runs :as runs]
            [samizdat.userspace :as userspace]))

(defn max-result-chars
  "How much of one tool result the branch is shown, from gates.edn
  :context-budget. A project reading generated files needs a different number
  from one reading hand-written namespaces, which is why it is not a constant."
  []
  (:tool-result-chars (gates/threshold :context-budget)))

(defn system-prompt-for
  "The system prompt as ROLE sees it: the tool catalogue filtered to that
  role's surface (resources/roles.edn).

  Roles used to be the implementer's world plus a suffix, so a supervisor was
  handed 31 tools written for somebody building the project and its own prompt
  had to argue it back out of them — a whole paragraph explaining that its file
  tools cannot reach the harness source, which was there because this prompt
  had just told it it has file tools. Constructing the catalogue instead means
  the argument is unnecessary: a role is not shown a tool it may not call, and
  calling one anyway is refused rather than discouraged.

  The catalogue is still HAND WRITTEN prose in system.md; only WHICH entries
  appear is computed. A nil role keeps all of it, which is what a workflow
  that names no role has always had.

  The TEXT comes through the :system chain (prompt-chain.edn, LR-7), so a
  project can replace the shipped prompt outright or suppress it entirely.
  First-present-wins: a level replaces, never concatenates. A suppressed base
  is legitimate — a workflow's own :prompt then IS the instruction set — so
  this renders empty rather than falling back to the shipped file.

  SCOPED TO THE PROJECT as well as to the role. `self-hosting` gates the
  sections about this harness's own architecture, and `reference-paths` names
  the read-only trees this project declared — both facts about the run's
  target, and both previously rendered as if every run worked on samizdat with
  nothing beside it."
  [role]
  (let [root (userspace/project-root)
        ;; WHICH IMAGE, or none. The prompt's opening sentence and its whole
        ;; REPL-first section are claims about where `eval` runs, and both were
        ;; unconditional. `repl.clj`'s docstring records what that costs when
        ;; the claim is false — the prompt promising a workflow the harness
        ;; cannot deliver. Under :project the model is in a SEPARATE image
        ;; rooted at the project, not "the same image the harness runs in", so
        ;; :project needs the rewording as much as :off needs the suppression.
        eval-mode (config/eval-mode root)
        ;; PER ROLE, not per run: `:project` is a posture, and the supervisor
        ;; keeps the harness image inside it. Telling a supervisor it is in a
        ;; separate project image while its evals land in the harness would be
        ;; the same false claim this is meant to remove.
        image (config/eval-image eval-mode role)]
    (roles/scope-catalogue
     (prompt/render-str (or (prompt/layer :system) "")
       {:templates ""
        :skills (skills/render-catalog)
        :self-hosting (userspace/self-hosting?)
        :repl (not= :off image)
        :harness-image (= :harness image)
        ;; From the project's own config file rather than the merged run
        ;; config, which prompt assembly is not handed. Same file, same key —
        ;; `.samizdat/config.edn` is the only place these are ever set, and it
        ;; is the operator's rather than the agent's.
        :reference-paths (seq (files/reference-roots
                               (get-in (config/project-config root)
                                       [:run :reference-paths])
                               root))})
     role eval-mode)))

(defn system-prompt
  "The whole system prompt, unscoped — every tool the harness has.

  The tool documentation IS hand written, because a prompt is prose and
  generated prose reads like it. `samizdat.prompt-test` asserts every name in
  `tools/tool-names` appears here, so a new tool cannot be added without being
  documented — that is what kept the whole Lean surface unreachable."
  []
  (system-prompt-for nil))

(defn judge-exemptions
  "The DO-NOT-FLAG list shipped to the audit and review judges. A var rather
  than a slurp inline so the digest can be attributable to it; re-read per
  digest, which is per run."
  []
  ;; Through the prompt seam, so a project can tune what its judges must not
  ;; flag without editing the harness for everyone.
  (prompt/prompt "judge-exemptions"))

(defn prompt-digest
  "A cheap fingerprint of the prompt and gate set a run used. AHE component
  observability: a pass-rate change should be attributable to a file."
  []
  (str (hash [(system-prompt) (gates/config) (judge-exemptions)])))

(defn shareable?
  "Whether a just-produced artifact belongs in the run's shared pool.

  The policy is gates.edn :share (drg-4026 #5) — which claim statuses count
  and whether relevance gates export — read at fire time, so a project
  retunes its sharing entry condition without a rebuild. The on/off flag
  stays in the run config: it is the diversity trade-off's off switch."
  [branch artifact share?]
  (let [{:keys [statuses require-relevance?]} (gates/threshold :share)]
    (boolean (and share?
                  (contains? statuses (:claim-status artifact))
                  (or (not require-relevance?)
                      (state/advances-thesis? branch (:claim artifact)))))))

(defn- truncate
  "Clip a tool result to the budget, ending with what the model can DO about
  it. See gates.edn :tool-clip for why the marker carries the sizes and the
  instruction rather than the bare word `truncated`."
  [s]
  (let [s (str s)
        cap (max-result-chars)]
    (if (> (count s) cap)
      (str (subs s 0 cap)
           (prompt/render-str (:message (gates/threshold :tool-clip))
                              {:shown cap :total (count s)}))
      s)))

(defn initial-messages
  "The branch's opening messages. A workflow may hand a `prompt-suffix` — extra
  system guidance appended for that workflow — which is how a manifest injects
  its own instructions at the start (a review workflow adds review guidance on
  top of the base prompt, keeping the whole tool surface). nil/blank leaves the
  base prompt untouched."
  ([problem] (initial-messages problem nil nil))
  ([problem prompt-suffix] (initial-messages problem prompt-suffix nil))
  ([problem prompt-suffix role]
   [{:role "system" :content (cond-> (system-prompt-for role)
                               (not (str/blank? prompt-suffix))
                               (str "\n\n" prompt-suffix))}
    ;; The opening user turn is prose the model reads and a project may want
    ;; worded differently — prompts/problem.md, not a `str` here.
    {:role "user" :content (prompt/render "problem" {:problem problem})}]))

(defn- shared-tree
  "The other branches' work on this run's tree, for the context block.

  nil when nothing else has written — a solo run is every run of the factory
  loop, and a heading over an empty list is a per-turn tax on the common case."
  [conn run-id branch-id]
  (when (and conn run-id branch-id)
    (let [files (journal/sibling-writes
                 conn run-id branch-id
                 (:tree-lines (gates/threshold :context-budget)))]
      (when (seq files)
        (prompt/render "shared-tree"
                       {:files (mapv (fn [f]
                                       (assoc f :branches (str/join ", " (:branches f))))
                                     files)})))))

(defn- context-block
  "What the harness adds to the branch's view before its next turn: the
  failures most like what it just tried, and — when sharing is on — the
  artifacts other branches confirmed that look most like it. Both FTS-ranked
  rather than the whole log, so the block stays small and stays relevant.

  Own-branch entries are excluded from both: a branch re-reading its own
  lemmas is noise, and the value of sharing is exactly the cross-branch hit.
  A shared artifact is journaled the FIRST time it enters this branch's
  context and never again: the block re-renders every turn, so per-serving
  events counted turns (86 events for a 15-row pool on one 28-turn run), not
  distinct sharing. Whether sharing earns the beam its width stays a question
  the journal can answer, now directly. Returns {:block :branch}; the branch
  carries the :shared-served ids the dedup reads."
  [conn run-id branch last-claim share?]
  (let [others #(remove (fn [e] (= (:branch_id e) (:id branch))) %)
        fhits (others (if (str/blank? last-claim)
                        (failures/recent conn run-id 5)
                        (failures/similar conn run-id last-claim 5)))
        ;; Fetched WIDE and then ranked down, not fetched at the display size.
        ;; Ranking a top-5 that is already all seeds just reorders seeds: a
        ;; completed run contributes its whole pool at turn 0 while the live
        ;; run's starts empty, and gen-20 served 67 seeded artifacts to 24 of
        ;; its own. Taking a wider slice is what lets an in-run lemma reach
        ;; the block at all; prefer-in-run then decides the order.
        shared-shown 5
        ahits (when share?
                (->> (if (str/blank? last-claim)
                       (artifacts/recent conn run-id (* 3 shared-shown))
                       (artifacts/similar conn run-id last-claim (* 3 shared-shown)))
                     others
                     artifacts/prefer-in-run
                     (take shared-shown)
                     vec))
        fresh (remove (comp (or (:shared-served branch) #{}) :id) ahits)]
    (doseq [a fresh]
      (journal/note! conn run-id :shared-artifact-hit
                     {:branch-id (:id branch)
                      :data {:claim (:claim a) :source-branch (:branch_id a)}}))
    (let [blocks (keep identity [;; WHAT THIS BRANCH IS WORKING ON, first in the
                                 ;; block. Restated every turn rather than held
                                 ;; at a fixed position near the top of the
                                 ;; array: the block is appended at the END,
                                 ;; which is where the prefix-cache boundary
                                 ;; already is, so this costs nothing per turn —
                                 ;; whereas a block maintained early in the
                                 ;; array invalidates every cached token behind
                                 ;; it each time the task changes. The task's
                                 ;; full statement is pinned into the tape once
                                 ;; on claim (tools/tasks); this is the
                                 ;; reminder, and the end is where a model
                                 ;; attends most.
                                 (if-let [t (:task branch)]
                                   (prompt/render "task-current"
                                     {:id (:id t) :title (:title t)})
                                   (prompt/prompt "task-none"))
                                 ;; The run's settled state, first and complete:
                                 ;; what is established and — the half nothing
                                 ;; carried before — what is RULED OUT. Read
                                 ;; from the artifacts table every turn, so it
                                 ;; cannot drift from the record, and cheap:
                                 ;; gen-20's whole confirmed set is under 400
                                 ;; tokens of claim text. Unlike the blocks
                                 ;; below it is not FTS-sampled, because the
                                 ;; value of a ledger is that a branch can
                                 ;; trust the absence of a line.
                                 (artifacts/render-ledger
                                  (journal/ledger conn run-id))
                                 ;; Breadcrumb index: kept memories surfaced as
                                 ;; ids + previews only, relevance-ranked by the
                                 ;; branch's last-claim, recent when blank. nil
                                 ;; on an empty store, so keep identity drops it.
                                 (knowledge/breadcrumb-index conn last-claim)
                                 ;; Unread mail from other branches on this run,
                                 ;; a bounded preview; nil when the inbox is
                                 ;; empty. Surfacing does not consume — the
                                 ;; message tool's inbox action marks read.
                                 (messages/render-inbox
                                  conn run-id (:id branch)
                                  (:inbox-lines (gates/threshold :context-budget)))
                                 ;; And what the siblings DID, which the
                                 ;; mailbox cannot say: it carries what a
                                 ;; branch chose to announce, and a worker
                                 ;; sharing a tree needs the ground truth.
                                 ;; nil for a solo run, so the block's keep
                                 ;; identity drops it and nothing changes for
                                 ;; the loops that have one branch.
                                 (shared-tree conn run-id (:id branch))
                                 (failures/render fhits)
                                 (artifacts/render ahits)])]
      {:block (when (seq blocks) (str/join "\n\n" blocks))
       :branch (update branch :shared-served (fnil into #{}) (map :id fresh))})))

;; --- one turn ---------------------------------------------------------------

(defn call-model
  "One model call for `branch`, through the injected inference seam.

  The mechanism moved to samizdat.agent.infer, where the tape is a value and
  `complete` is an argument — this is the branch-shaped wrapper the cells and
  the beam call. Same behaviour as before: one retry at a doubled budget when
  the response hit the token cap before emitting a tool call, and a provider
  failure returned as {:ok false :error} rather than thrown."
  [ctx branch]
  ;; The branch's own reasoning effort, not the run's. Once the runaway
  ;; breaker has fired on this branch, thinking is off for the rest of its
  ;; task — read HERE at request time rather than written into the run config,
  ;; because the config belongs to the run and this decision belongs to one
  ;; branch (samizdat.agent.thinking).
  (let [off (:off-value (gates/threshold :thinking-budget))
        ctx (update ctx :llm-config
                    (fn [c]
                      (assoc c :reasoning-effort
                             (thinking/effort-for branch (:reasoning-effort c) off))))]
    ((infer/complete-fn ctx) (infer/of-branch branch))))

(defn- settle-predictions!
  "Close out any prediction whose window has passed or whose expectation the
  branch just met. Deterministic; no model in the path."
  [conn branch turn tools-called before after]
  (let [{kept true closed false}
        (group-by (fn [p]
                    (nil? (arbiter/settle p {:current-turn turn
                                             :tools-called tools-called
                                             :branch-before before
                                             :branch-after after})))
                  (:open-predictions after))]
    (doseq [p closed]
      (journal/settle-gate! conn (:id p)
                            (arbiter/settle p {:current-turn turn
                                               :tools-called tools-called
                                               :branch-before before
                                               :branch-after after})
                            turn))
    ;; Stamp the MET settlements onto gate-history: that is the episode
    ;; boundary a budget re-arms at (karamazov-gez). Without it a gate spends
    ;; its whole run's allowance on the first stall and is silent for every
    ;; stall after, however much worse.
    (let [met-gates (into #{}
                          (comp (filter (fn [p]
                                          (contains?
                                           #{:met :met-late}
                                           (arbiter/settle p {:current-turn turn
                                                              :tools-called tools-called
                                                              :branch-before before
                                                              :branch-after after}))))
                                (map :gate))
                          closed)]
      (cond-> (assoc after :open-predictions (vec kept))
        (seq met-gates)
        (update :gate-history
                (fnil into [])
                (map (fn [g] {:gate g :turn turn :settled :met}) met-gates))))))

(defn phase-valve
  "The release valve for the explore prologue (vf-b25): a branch that cannot
  get a skeleton to elaborate must not be locked out of verification for the
  whole run, so at the cap the prologue is declared over and the branch told
  why. The message lands before the model call so the next response actually
  sees it."
  [branch turn]
  (cond-> (assoc branch :current-turn turn)
    ;; :current-turn is the branch's knowledge of the GLOBAL turn, stamped at
    ;; the top of every turn (both drivers come through here). It is what
    ;; state/turn-count serves, so budget arithmetic — last-call, wind-down,
    ;; crossed-fractions, banked-in-last — runs in the same unit as max-turns
    ;; and the artifact/gate stamps. (count :turns) undercounted it: no-call
    ;; and provider-error turns append no :turns entry, and a FORK's log
    ;; starts nearly empty, so a fork born at round 18 of 25 read as turn ~0,
    ;; was never told to ship, and its parent's old artifacts read as
    ;; \"recent\" forever (karamazov-blt.16).
    (state/explore-cap-expired? branch (gates/threshold :explore-cap) turn)
    (-> (state/enter-phase turn)
        (state/add-message
         "user"
           ;; "prologue" only for a branch that has never left explore. Once a
           ;; reframe can send one back (vf-9wx) the same message on a re-entry
           ;; would be describing something that is not happening.
           ;; Tier 2d: the prose is prompts/explore-cap.md — runtime-editable,
           ;; the same seam every gate message reads through.
           (str "[harness] "
                (prompt/render "explore-cap"
                  {:lead (if (:reframe-entered-turn branch)
                           "Your re-planning budget is spent: "
                           "The explore prologue is over: ")
                   :cap (gates/threshold :explore-cap)}))
         {:turn turn}))))

(defn provider-error-step
  "A provider failure is not the branch's fault and must not count against it
  as a verification failure.

  It IS counted against the run, though, which is different and was missing.
  `:provider` existed in the session tally and nothing ever wrote to it — dead
  structure, and the exact trap the fence parser's own comment warns about: a
  signal that is never fed reads identically to a behaviour that never
  happens. A run losing half its turns to empty replies scored as neutral,
  because a provider error is journalled `:neutral` (correctly — the BRANCH
  did nothing wrong) and nothing else looked at it.

  Counted by REASON, because the responses differ. An empty reply means the
  model spent its whole budget thinking and wants more tokens or reasoning
  turned off; a refused connection wants waiting. Telling a supervisor only
  that `the provider failed` gives it nothing to act on."
  ([ctx branch turn error] (provider-error-step ctx branch turn error nil))
  ([{:keys [conn run-id]} branch turn error reason]
   (session/observe! [:provider (or reason :call-failed)]
                     (when (and run-id (:id branch)) [run-id (:id branch)]))
   (log/warn "branch" (:id branch) "turn" turn "model call failed:" error)
   (journal/record-turn! conn run-id
                         {:branch-id (:id branch) :turn turn
                          :tool-name "__provider_error__" :result error
                          :category "neutral"})
   (if (= :context-overflow reason)
     ;; The prompt outgrew the window. 'Try again' is exactly wrong here —
     ;; the failure is upstream of the model seeing anything, the next
     ;; assemble would be just as oversized, and APPENDING a message grows
     ;; the very thing that overflowed. Squeeze the branch's compaction
     ;; budget instead (karamazov-d41): the next render fits, and the model
     ;; continues none the wiser, which is how compaction always works.
     (state/squeeze-context branch)
     (state/add-message branch "user"
                        (str "[harness] The provider call failed: " error
                             " Try again.")
                        {:turn turn}))))

(defn absorb-response
  "Fold the model's response into the branch.

  Two layers, deliberately separate. The TAPE half — parse the fence, append
  what the assistant actually said, clear the per-turn knobs — is
  `infer/absorb`, a pure function of a tape value that a probe drives without
  a branch anywhere in sight. The BRANCH half is the mechanics tally, which is
  bookkeeping about the branch rather than about the conversation, and which a
  probe deliberately does not touch: a bounce that parsed badly is not a
  branch that called badly."
  ([branch response] (absorb-response branch response nil))
  ([branch response turn]
   (let [{:keys [tape parsed signals said]}
         (infer/absorb (infer/of-branch branch) response turn)
         ;; PROACTIVE, not reactive (karamazov-3y5). The only thing that used
         ;; to tell a branch its prompt had grown too big was a failed
         ;; request: the overflow came back, THEN the budget was squeezed.
         ;; The provider reports the size of every request it accepted, so
         ;; the wall is visible one turn before it is hit — squeeze on the
         ;; approach and the overflow never happens. Still harness-side and
         ;; invisible to the model, exactly like compaction always is; the
         ;; model-facing half of vis's hint waits on a fold tool to name,
         ;; because telling a model it is near a ceiling it has no lever to
         ;; move is noise.
         pressure (state/context-pressure
                   (get-in response [:usage :prompt-tokens])
                   (gates/threshold :context-pressure))]
     {:parsed parsed
      :signals signals
      :said said
      :pressure pressure
      :branch (cond-> (-> (infer/into-branch branch tape)
                          (state/record-mechanics signals))
                (contains? #{:urgent :over} pressure) state/squeeze-context)})))

(defn no-call-step
  "No usable call. Say exactly what was wrong; a bare \"try again\" produces
  another identical attempt."
  [{:keys [conn run-id]} branch turn {:keys [parsed signals said response]}]
  ;; A reply that is nothing but a copy of the harness's own compaction
  ;; marker. On a long branch almost every message is an [unloaded] digest
  ;; standing in for a past turn, and a model reading its own history that
  ;; way starts writing digests instead of calls — eight in a row on a live
  ;; supervisor (karamazov-068). It needs its OWN complaint: told merely to
  ;; emit a tool call, it emits another digest.
  (let [imitation? (and (message/unloaded? said)
                        (not (:truncated signals)))
        ;; RUNAWAY REASONING, which is a different failure from a budget that
        ;; was merely too small and wants the opposite advice: more tokens
        ;; will not help a model that deliberates without converging, and the
        ;; harness has watched this happen without being able to stop it
        ;; (:provider-empty-replies). Both signals required — cut off at the
        ;; limit AND a trace past its own derived budget.
        tb (gates/threshold :thinking-budget)
        runaway? (thinking/runaway?
                  {:truncated? (:truncated signals)
                   :parsed parsed
                   :reasoning (:reasoning response)}
                  (thinking/derived-cap (:thinking-grant response) tb)
                  (:chars-per-token tb))
        msg (cond
              runaway?
              (prompt/prompt "thinking-runaway")
              (:truncated signals)
              (str "[harness] Your response hit the token limit before you"
                   " emitted a tool call. Think less and call a tool.")
              imitation?
              (prompt/prompt "no-call-imitation")
              (nil? parsed)
              (str "[harness] No ```tool-call block in your response."
                   " Every turn must end with exactly one.")
              :else
              (str "[harness] Your tool-call block did not parse: "
                   (:parse-error parsed)))]
    ;; The response matters most on THIS path. A turn that produced no usable
    ;; call records nothing else about what the model did, and without the
    ;; text there is no way to tell a model that rambled from one that emitted
    ;; the wrong fence from one that answered in prose.
    ;; `mechanics`, not `failure`. The branch produced no claim, so there is
    ;; nothing here to hold against its line of inquiry — the same reasoning
    ;; as the provider-error path. The count is still kept and still bounds
    ;; the branch; see record-outcome.
    (journal/record-turn! conn run-id
                          {:branch-id (:id branch) :turn turn
                           :tool-name (or (:name parsed) "__no_call__")
                           :result msg :category "mechanics"
                           :parse-error (:parse-error parsed)
                           :auto-repaired (:auto-repaired? parsed)
                           :assistant-text said
                           :reasoning-text (:reasoning response)
                           ;; A turn that produced no usable call still cost
                           ;; tokens, and those are the ones worth counting.
                           :usage (:usage response)})
    (-> branch
        (state/record-outcome {:category :mechanics :progress? false})
        (cond-> runaway? thinking/recovery)
        (state/add-message "user" msg {:turn turn})
        ;; And make the next request end mid-fence, so prose is not an
        ;; available reply. Telling the model to emit a fence is the
        ;; suggesting form; this is the withholding form, which is the one
        ;; that has ever worked — see arbiter/prefill-for. Bare, with no tool
        ;; named: nothing is being steered — the branch had a plan and failed
        ;; to act on it, and picking its next call for it would replace a
        ;; mechanics failure with the harness doing the reasoning.
        ;;
        ;; EXCEPT on an imitation, where the prefill is half the trap: the
        ;; model opens inside a fence, looks at a context of digest lines,
        ;; and the likeliest continuation is another digest. Withholding
        ;; prose is the right instinct against rambling and the wrong one
        ;; here, so this turn gets a clean slate to reason in
        ;; (karamazov-068).
        (as-> b (if imitation? (dissoc b :prefill) (assoc b :prefill "```tool-call\n"))))))

(defn transition-effects
  "The effect names a turn envelope triggers, per phases.edn `:transitions`.

  A key is a get-in path into the envelope. The value says what the path has
  to hold:

    [effects…]        the path holds anything truthy
    {value [effects…]} the path holds exactly `value`

  The second form is what the table could not previously say, and it is what
  an artifact trigger needs: `:claim-status` is truthy for `:confirmed`,
  `:empirical` and `:sketch` alike, so a truthy test on it would fire the
  confirmed branch's effects on an unverified plan. A status is a
  vocabulary, not a flag, and a table that can only ask `is it set` cannot
  key on one."
  [envelope]
  (mapcat (fn [[path outcome]]
            (let [v (get-in envelope path)]
              (if (map? outcome)
                (get outcome v)
                (when v outcome))))
          (phases/transitions)))

(defn apply-transitions
  "Apply the result-signal transitions the turn's result carries (drg-4026
  #3) — the claim-first state machine as a declarative table (phases.edn
  :transitions) instead of cond-> clauses in the executor. Effect names
  dispatch here to state fns, because a table cannot mutate a branch."
  [result artifact branch]
  (reduce (fn [b effect]
            (case effect
              :mark-green    (state/mark-green b)
              :clear-reframe (state/clear-reframe b)
              ;; phases.edn is runtime-editable, so a typo'd effect name has
              ;; to SAY something — a silent no-op reads as the transition
              ;; working (blt.38).
              (do (log/warn "phases.edn :transitions names an effect this loop"
                            "does not implement:" effect
                            "— known: :mark-green :clear-reframe")
                  b)))
          branch
          (transition-effects {:result result :artifact artifact})))

(defn- note-storm
  "The storm guard's per-call bookkeeping (karamazov-ekk): note the
  dispatched call in the branch's window, count consecutive withholds as
  strikes for the :storm gate, and put a withheld signature on the reflexion
  log so the stuck/safe-state steers quote it back as a dead end. The
  withhold itself already happened (or did not) in tools/phase-refusal; the
  policy is gates.edn data; the detection is samizdat.agent.storm. A
  withheld call is never noted — it did not run, and keeping the originals
  in the window is what keeps the repeat withheld until the branch actually
  changes course. Tracked dispatched calls reset the strikes; exempt calls
  leave them alone — a read between two withheld attempts is still the same
  storm."
  [branch {:keys [tool sig paths result refused? verify?]} policy]
  (let [tracked? (and (storm/tracked? policy tool)
                      ;; A verify call is invisible to the guard end to end:
                      ;; never counted, never withheld (see storm/verify-call?).
                      (not verify?))
        storm-refused? (contains? #{:storm :storm-oscillation}
                                  (:refusal-rule result))
        failed? (= :failure (:category result))
        digest-chars (:error-digest-chars policy)
        line (str "withheld repeat: " sig)]
    (cond-> branch
      (and tracked? (not refused?))
      (-> (update :storm-window storm/note-call
                  {:sig sig
                   :mutating? (storm/mutating? policy tool)
                   :timeout? (boolean (:timeout? result))
                   :failed? failed?
                   ;; The digest a deliberate retry inherits (retry-diagnosis)
                   :error (when failed?
                            (let [s (str (:result result))]
                              (if (and digest-chars (> (count s) digest-chars))
                                (subs s 0 digest-chars)
                                s)))}
                  policy)
          (assoc :storm-strikes 0))

      ;; The same-file streak counts every dispatched call, exempt tools
      ;; included — re-reading and re-editing one file are the same thrash.
      ;; A refused call touched nothing and leaves the streak alone.
      (not refused?)
      (update :file-touch storm/note-file-touch paths)

      ;; A landed WRITE discharges the file from the repl session's plan. Only
      ;; a write — reading a file you promised to change is not changing it,
      ;; and the whole contract is that exploration ends in a file.
      (and (not refused?)
           (contains? (gates/tool-vocab :file-write) tool)
           (= :success (:category result)))
      (as-> b (reduce state/note-write b paths))

      storm-refused?
      (update :storm-strikes (fnil inc 0))

      (and storm-refused?
           (not (some #{line} (:abandoned branch))))
      (assoc :abandoned (state/abandoned-log branch line)))))

(defn tool-step
  "Dispatch the parsed call: phase policy first, then the tool, then the
  branch bookkeeping the outcome demands. Returns {:branch :result :tool}."
  [ctx branch turn parsed]
  (let [tool (:name parsed)
        sig (storm/signature tool (:args parsed))
        ;; Read BEFORE this call is noted: has this exact call failed before?
        ;; If it fails again — even differently — the retry inherits the
        ;; previous diagnosis below (J-Space's rule: never a blank retry).
        prev-fail (storm/last-failure-of (:storm-window branch) sig)
        ;; Phase policy is consulted before dispatch: a refused call never
        ;; reaches a tool, and the refusal is journalled like any other turn
        ;; (vf-b25, vf-eaw). One place owns the refusals — tools/phase-refusal.
        refusal (tools/phase-refusal
                 (assoc ctx :branch branch :turn turn
                        :tool-name tool :args (:args parsed)))
        result (or refusal
                   (tools/run-tool (assoc ctx :branch branch :turn turn
                                          :tool-name tool :args (:args parsed))))
        storm-policy (gates/storm-policy)
        branch (-> (:branch result)
                    ;; The tool and the claim ride along so the branch can
                    ;; remember what it was grinding when it failed — which is
                    ;; what the stuck gate withholds (vf-9wx).
                    (state/record-outcome
                     (assoc result :tool tool
                            :claim (get-in parsed [:args :claim])
                            ;; A timeout is the most expensive failure there
                            ;; is; the streak gates read the counter it
                            ;; weights (gates.edn :timeout-failure-weight).
                            :weight (when (:timeout? result)
                                      (gates/threshold :timeout-failure-weight))))
                   (state/add-turn {:turn turn :tool tool
                                    :category (:category result)
                                    ;; Kept for failures AND malformed calls,
                                    ;; only so repeating-failure? can see a
                                    ;; loop. The turns table holds the
                                    ;; authoritative result.
                                    :error (when (#{:failure :mechanics}
                                                  (:category result))
                                             (str (:result result)))})
                   (note-storm {:tool tool
                                :sig sig
                                :paths (storm/touched-paths (:args parsed))
                                :result result
                                :refused? (some? refusal)
                                :verify? (storm/verify-call?
                                          tool (:args parsed)
                                          (get-in ctx [:config :run :verify-cmd]))}
                               storm-policy))
        ;; 29 of gen-20's 57 failures were four identical (tool, message)
        ;; pairs, and the harness answered the fifth exactly as it answered
        ;; the first. Say something different instead.
        result (if (state/repeating-failure? branch tool (str (:result result)))
                 (update result :result
                         #(str % "\n\n[harness] This exact call has now"
                               " failed this exact way more than once."
                               " Repeating it will fail again. Change"
                               " the call, or change technique — a"
                               " different tool, a smaller claim, or a"
                               " different encoding of the same one."))
                 result)
        ;; The same call failing DIFFERENTLY is the case repeating-failure?
        ;; cannot see (it needs the identical error), and a blank retry is
        ;; the loop J-Space names: the retry must inherit the diagnosis
        ;; (karamazov-g86). Both failures are put side by side; two
        ;; different failures from one call usually mean the call itself is
        ;; wrong.
        result (if (and prev-fail
                        (= :failure (:category result))
                        (not= (str (:error prev-fail))
                              (subs (str (:result result))
                                    0 (min (count (str (:result result)))
                                           (count (str (:error prev-fail)))))))
                 (update result :result
                         #(str % "\n\n"
                               (prompt/render "retry-diagnosis"
                                              {:previous (:error prev-fail)})))
                 result)
         branch (if-let [a (:artifact result)]
                   (state/add-artifact branch (assoc a :turn turn))
                   branch)
         ;; A green ship-verify is the green point the safe-state rung
         ;; rewinds to, and green work also ends a reframe: the withheld
         ;; approach could not have produced it (vf-9wx). The signal→effect
         ;; table itself is phases.edn :transitions data (drg-4026 #3).
         ;;
;; The table now carries BOTH triggers, and they are different questions.
         ;; :mark-green keys on the verify signal, because the green point the
         ;; safe-state rung rewinds to is a fact about the WORKING TREE — that
         ;; the suite was observed passing — and not about any claim.
         ;; :clear-reframe keys on a CONFIRMED ARTIFACT, because that is what
         ;; clear-reframe has always meant: the branch banked something the
         ;; withheld approach could not have produced. Any tool that confirms
         ;; a claim ends a reframe now, not only a green ship-verify.
         branch (apply-transitions result (:artifact result) branch)]
    ;; A green verify marks the green point the safe-state rung falls back
    ;; to. The snapshot is the turn cursor: the journal is the store
    ;; checkpoint — append-only, and what resume replays from — so the
    ;; cursor is all the rung needs to name a rewindable state.
    {:branch branch :result result :tool tool}))

(defn- observe-turn!
  "Feed the live session tally with what this turn did and how the reply
  parsed — for the process and for this branch, whose own tally is the
  number the cull and the supervisor share (RFC-012 F3). Never allowed to
  throw: a counter must not be able to cost a turn."
  [run-id branch tool result signals]
  (try
    (session/observe-turn! {:tool tool
                            :category (:category result)
                            :signals signals
                            :branch (when (and run-id (:id branch))
                                      [run-id (:id branch)])})
    (catch Throwable _ nil)))

(defn journal-step!
  "The durable record of the turn: the turn row, any artifact (and its entry
  into the shared pool when it qualifies), any failure, any thesis. Side
  effects only; returns nil."
  [{:keys [conn run-id] :as ctx} branch turn {:keys [parsed result tool said response signals]}]
  (observe-turn! run-id branch tool result (or signals
                                 ;; A turn that never reached a tool still has
                                 ;; something to say: the parse flags are how
                                 ;; the harness's OWN failure modes get counted,
                                 ;; and those are the ones a supervisor is least
                                 ;; able to infer from outcomes.
                                 {:parse-error (= "__parse_error__" (:name parsed))
                                  :auto-repaired (:auto-repaired? parsed)}))
  (journal/record-turn! conn run-id
                        {:branch-id (:id branch) :turn turn
                         :tool-name tool :args (:args parsed)
                         :result (truncate (:result result))
                         :category (name (:category result))
                         :policy-refusal? (:policy-refusal? result)
                         :auto-repaired (:auto-repaired? parsed)
                         :assistant-text said
                         :reasoning-text (:reasoning response)
                         :usage (:usage response)})
  (when-let [a (:artifact result)]
    (journal/record-artifact! conn run-id
                              (assoc a :branch-id (:id branch) :turn turn))
    ;; Only confirmed, on-topic artifacts enter the shared pool — see
    ;; shareable?. The flag is the diversity trade-off's off switch.
    (when (shareable? branch a (get-in ctx [:config :run :share-artifacts?]))
      (artifacts/record! conn run-id
                         {:branch-id (:id branch) :turn turn
                          :kind (:kind a) :tier (:tier a)
                          :claim (:claim a) :code (:code a)})))
  (when-let [f (:failure result)]
    (failures/record! conn run-id
                      (assoc f :branch-id (:id branch) :turn turn
                             :tool-name tool)))
  (when-let [t (:thesis result)]
    (runs/set-thesis! conn run-id (:id branch) t))
  nil)

(defn- drain-directives!
  "Apply the directives waiting at this branch's boundary — a person's, the
  supervisor's or the reflex's; the queue is the one write path and
  `issued_by` says whose (RFC-012).

  TWO DRIVERS, ONE QUEUE, and which drain owns a directive depends on the
  run's shape (karamazov-blt.10):

  On a BEAM run this drain takes only what is addressed to THIS branch —
  a branch-scoped `message`/`review` lands sooner here than at the next
  round top. Everything else (run-wide messages, cull/fork/retract/pause/
  resume/extend) is LEFT PENDING for `:beam/directives`: this used to eat
  and reject the scheduler kinds at whichever branch's boundary came first,
  which — since a round's wall-clock lives inside `:beam/advance` — was
  nearly always before the beam drain ever saw them. A human's pause was
  resolved `:rejected` by a branch.

  On a SINGLE-BRANCH run there is no beam drain, so everything lands here:
  `message`/`review` become the :pending-directive the arbiter puts at
  priority zero, `extend` raises the branch's cap and persists the run row
  (karamazov-blt.12 — the old arm assumed control/extend! had run, which is
  REPL-only, and left the row pending forever), and the scheduler-only kinds
  are rejected with a reason rather than accepted silently.

  The WORKFLOW kinds (`interventions/workflow-kinds`) are left pending in
  both shapes: they decide a workflow's next round, and the workflow's own
  directives stage is the boundary that applies them. Eating them here
  meant a `switch` landed at whichever worker finished a turn first and was
  refused as unknown, rounds before the stage that wanted it.

  Shares the interventions queue with the HTTP control surface, so a REPL
  steer and a UI steer are the same event."
  [{:keys [conn run-id beam?] :as ctx} branch turn]
  (if-not (and conn run-id)
    branch
    (reduce
     (fn [b d]
       (let [scoped-here? (some? (:branch_id d))]
         (case (:kind d)
           ("message" "review")
           (if (and beam? (not scoped-here?))
             b ;; run-wide: the beam broadcasts it to every branch at the round top
             (do (interventions/resolve! conn run-id (:id d) :applied nil turn)
                 ;; :payload-text = the parsed human words; the raw column is
                 ;; a JSON blob the gate would render verbatim (blt.38).
                 (assoc b :pending-directive
                        (assoc d :payload-text (interventions/text-of d)))))

           "extend"
           (if beam?
             b ;; run-level: the beam drain applies and persists it
             (if-let [n (interventions/turns-asked d)]
               (let [b' (update b :extended-turns (fnil + 0) n)]
                 (interventions/resolve! conn run-id (:id d) :applied nil turn)
                 ;; The row is what a crash-resume reads its budget from.
                 (runs/extend-budget! conn run-id
                                      (+ (:max-turns ctx) (:extended-turns b')))
                 b')
               (do (interventions/resolve! conn run-id (:id d) :rejected
                                           (prompt/render "directive-rejected"
                                                          {:extend-no-turns true})
                                           turn)
                   b)))

           (cond
             ;; the workflow's own boundary owns these, in either shape
             (contains? interventions/workflow-kinds (:kind d)) b
             ;; scheduler kinds: the beam drain owns them
             beam? b
             :else
             (do (interventions/resolve! conn run-id (:id d) :rejected
                                         (str (:kind d) " applies to the beam scheduler,"
                                              " not a single-branch run")
                                         turn)
                 b)))))
     branch
     (interventions/pending conn run-id (:id branch)))))

(defn apply-effects
  "Apply the fired gate's branch-state effects (drg-4026 #4).

  A gate is data and cannot mutate the branch, so the small set of effect
  IMPLEMENTATIONS lives here; WHICH gate carries which effect is the gate's
  own :effect key in gates.edn, carried through the decision. Naming the
  effect rather than the gate means adding or renaming a state-changing
  gate is a data edit — loop.clj is not a dispatch table of gate names."
  [decision turn max-turns branch]
  (cond-> branch
    (= :notified-fractions (:effect decision))
    (assoc :notified-fractions (gates/crossed-fractions branch max-turns))

    (= :begin-reframe (:effect decision))
    (state/begin-reframe turn
                         (:last-failed-claim branch))))

(defn settle-step
  "Close out the predictions this turn resolved, BEFORE anything chooses a
  new steer: settling compares the branch as it entered the turn against the
  branch now, so a resolution closes against the gate that asked for it and
  not the one about to. Its own step, and its own node in every turn-shaped
  manifest, so that order is a constraint the compiler checks rather than a
  convention inside one cell (karamazov-aqsr.2). Returns {:branch :closed},
  the count being how many predictions this turn closed."
  [{:keys [conn]} before branch turn {:keys [parsed]}]
  (let [open (count (:open-predictions branch))
        branch (settle-predictions! conn branch turn [(:name parsed)] before branch)]
    {:branch branch
     :closed (- open (count (:open-predictions branch)))}))

(defn steer-step
  "Pending human directives drain, then the single boundary: at most one
  steer, chosen in priority (a human directive outranks every machine gate),
  plus the context block. Runs on a branch settle-step has already closed
  this turn's predictions on. Returns the branch ready for its next turn (or
  carrying the final answer when the turn shipped)."
  [{:keys [conn run-id max-turns] :as ctx} branch turn {:keys [parsed result]}]
  (let [tool (:name parsed)
        ;; Not on a done turn: the done path renders no steer, so a directive
        ;; drained here was resolved "applied" and never shown (blt.38). Left
        ;; pending, it reaches whoever can still act — another branch, or the
        ;; queue's history as honestly undelivered.
        branch (if (:done? result) branch (drain-directives! ctx branch turn))
        ;; The cap the gates reason against includes whatever `extend`
        ;; directives have granted this branch — otherwise last-call and the
        ;; turn-budget notices keep firing against the spent original cap
        ;; (karamazov-blt.12).
        max-turns (+ max-turns (or (:extended-turns branch) 0))]
    (if (:done? result)
      ;; :tool as well as :turn. Compaction's prune pass replaces an old tool
      ;; result with one line keyed BY TOOL — a shell result's useful line is
      ;; its command, a grep's is its match count, a read's is its size — and
      ;; a message that does not say which tool produced it gets the generic
      ;; preview instead, which is the one shape that carries nothing.
      (state/add-message branch "user" (truncate (:result result))
                         {:turn turn :tool tool})
      ;; Coverage answers whether the safe-state rung's fallback is honest:
      ;; the green cursor still points into a turn log the journal can
      ;; replay up to.
      (let [coverage (state/snapshot-covers? branch)
            decision (arbiter/decide
                      {:branch branch
                       :max-turns max-turns
                       ;; How wide the beam already is, so the reproduction
                       ;; rung knows whether the run can afford offspring.
                       :branch-count (or (:branch-count ctx) 1)
                       :done-block (:done-block result)
                       :directive (or (:pending-directive branch)
                                      (:directive ctx))
                       :safe-state-coverage coverage})
            {ctx-block :block branch :branch}
            (context-block conn run-id branch
                           (get-in parsed [:args :claim])
                           (get-in ctx [:config :run :share-artifacts?]))
            body (str (truncate (:result result))
                      (when ctx-block (str "\n\n" ctx-block))
                      (when decision (str "\n\n---\n\n" (:message decision))))
            ;; Recorded exactly once. The row id is what a later turn settles,
            ;; so writing it twice would leave one firing permanently open.
            firing-id (when decision
                        (journal/record-gate!
                         conn run-id
                         {:branch-id (:id branch) :turn turn
                          :gate (:gate decision)
                          :priority (:priority decision)
                          :message (:message decision)
                          :prediction (:prediction decision)
                          :window (:window decision)}))]
        (when decision
          (log/debug "branch" (:id branch) "turn" turn
                     "gate" (:gate decision)
                     "passed over" (:passed-over decision)))
         ;; drg-4026 #4: any branch-state effect the fired gate carries is
         ;; keyed by the gate's own :effect (gates.edn data), applied by
         ;; apply-effects — not by gate names hard-coded here.
         (apply-effects decision turn max-turns
           (cond-> (-> branch
                     (dissoc :pending-directive)
                     (state/add-message "user" body {:turn turn}))
           decision (update :gate-history (fnil conj [])
                            {:gate (:gate decision) :turn turn})
           decision (update :open-predictions (fnil conj [])
                            {:id firing-id
                             :gate (:gate decision)
                             :prediction (:prediction decision)
                             :window (:window decision)
                             :turn turn})
           ;; Consumed by the NEXT call-model and cleared there, so a steer
           ;; forecloses prose on exactly the turn it steers and no later one. A
           ;; gate naming a forceable tool sets BOTH a prefill and a force-tool
           ;; spec; the adapter uses the prefill where the provider continues a
           ;; trailing assistant message (DeepSeek /beta) and falls back to native
           ;; tool_choice only where it does not (GLM) — tool_choice is rejected
           ;; by some providers' thinking mode, so it is the fallback, not the
           ;; default. A bare steer just prefills the fence.
           decision (assoc :force-tool (arbiter/force-tool-for decision)
                           :prefill (arbiter/prefill-for decision))))))))
