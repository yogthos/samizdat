;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; PROBE cells — the policy half of LR-2 and LR-3.
;;
;; samizdat.agent.infer owns the mechanism: bounce one input off a fixed tape,
;; trampoline many, or fan one across config variants. None of it decides
;; anything. What to probe, how to judge the answers, and what to do with the
;; winner is policy, and policy lives here in resources so the supervisor can
;; rewrite it at runtime without a rebuild.
;;
;; WHY A PROBE IS SAFE. A committed turn runs a tool: it writes files and
;; shells out. A probe stops at inference and parse, and that is structural
;; rather than a rule — infer has no tool seam in it at all. So a cell here can
;; ask "what would this branch do next?" as many times as its budget allows
;; and nothing on disk changes.
;;
;; WHAT IT COSTS. One completion per candidate. On a local endpoint every
;; bounce reuses the same warm prefix (:cache_prompt, LR-5), so a scan of five
;; candidates is five completions and one prefill, not five of each. Against a
;; metered provider it is five calls, which is why the width is data
;; (gates.edn :probe) and defaults to something small.
(ns cells.probe
  (:require [clojure.string :as str]
            [mycelium.cell :as cell]
            [samizdat.agent.gates :as gates]
            [samizdat.agent.infer :as infer]
            [samizdat.agent.phases :as phases]
            [samizdat.agent.state :as state]
            [samizdat.agent.tools.base :as base]
            [samizdat.config :as config]
            [samizdat.llm.registry :as registry]
            [samizdat.prompt :as prompt]))

(defn- candidates
  "The framings to probe, one per non-blank, non-comment line of
  prompts/probe-candidates.md. Editing that file changes what the harness
  considers, with no rebuild and no code change."
  []
  (->> (str/split-lines (prompt/prompt "probe-candidates"))
       (map str/trim)
       (remove str/blank?)
       (remove #(str/starts-with? % "#"))
       vec))

(defn- usable?
  "Whether a bounce produced something worth acting on: a well-formed call to a
  tool this phase actually allows.

  A probe that parsed badly says nothing about the branch — it is not a turn
  the branch took — so it is simply not a candidate. A call the phase would
  refuse is not usable either: acting on it would spend the real turn on a
  refusal, which is the failure the probe exists to avoid."
  [branch {:keys [parsed error]}]
  (boolean (and (nil? error)
                parsed
                (:name parsed)
                (not= "__parse_error__" (:name parsed))
                (not (contains? (phases/withholds (:phase branch))
                                (:name parsed)))
                ;; The REFUSALS table too, not just the (empty) withholds —
                ;; the probe used to pay inference to steer the branch into
                ;; exactly the refusal it exists to avoid (a task-less branch
                ;; steered into write_file, then declined by the
                ;; work-needs-a-task rule) (karamazov-blt.36).
                (nil? (base/phase-refusal {:branch branch
                                           :tool-name (:name parsed)})))))

(defn- pick
  "The winning bounce, or nil.

  Deterministic and cheap on purpose — no model sits in this judgement. A
  usable call that does NOT repeat this branch's last failure beats one that
  does; among equals the earlier candidate wins, so the candidate file's order
  is the tie-break and an operator expresses a preference by moving a line."
  [branch bounces]
  (let [usable (filter #(usable? branch %) bounces)
        fresh (remove #(= (:last-failed-tool branch) (:name (:parsed %))) usable)]
    (first (concat fresh usable))))

(defn- role-ctx
  "`ctx` with its adapter and model swapped to the one assigned to `role` under
  config :run :role-models. A role with no entry keeps the run's own model.

  Mirrors samizdat.workflow/role-ctx rather than requiring it: a shipped cell
  is load-stringed from inside the loop compile, and reaching back into the
  loop driver from there is the cycle samizdat.agent.tools.manifest documents
  avoiding for the same reason."
  [ctx role]
  (if-let [spec (get-in (:config ctx) [:run :role-models role])]
    (let [provider (or (some-> (:provider spec) name str/lower-case keyword)
                       (:provider (:llm-config ctx)))]
      (assoc ctx
             :llm-adapter (registry/adapter-for provider)
             :llm-config (config/provider-llm provider (dissoc spec :provider))))
    ctx))

(cell/defcell :probe/next-move
  {:doc "Scan candidate framings of the next step against the branch's CURRENT
        tape without committing any of them, and hand the winner forward as a
        steer for the real turn.

        Runs only when the branch is actually stuck — `gates.edn :probe`
        :on-mechanics-failures — because a branch producing well-formed calls
        needs no help and probing it is pure spend. Adds :probe to the data
        map; a manifest routes on it or ignores it."
   :effects [:net :db]
   :requires []
   :input  [:map [:branch :map] [:turn :int]]
   ;; OPTIONAL because the early-out is the common case: a branch that is not
   ;; stuck returns `data` untouched, and a probe that found no winner adds
   ;; :probe without touching :branch. Declaring either as guaranteed would
   ;; make every skipped probe a schema warning.
   :output [:map [:probe {:optional true} :any]
            [:branch {:optional true} :map]]}
  (fn [ctx {:keys [branch turn] :as data}]
    (let [{:keys [width on-mechanics-failures]} (gates/threshold :probe)]
      (if (or (not (pos? (or width 0)))
              (< (:consecutive-mechanics-failures branch 0)
                 (or on-mechanics-failures 1)))
        data
        ;; journal? false: a retry inside a probe is not a turn the run took,
        ;; and recording it would put spend that never reached the tape into
        ;; the turn record. The probe leaves its own receipt instead.
        (let [complete (infer/complete-fn ctx {:journal? false})
              inputs (vec (take width (candidates)))
              {:keys [bounces]} (infer/trampoline complete
                                                  (infer/of-branch branch)
                                                  inputs)
              winner (pick branch bounces)]
          (infer/log-probe! ctx (:id branch) :next-move
                            {:arms (count bounces)
                             :errors (count (filter :error bounces))})
          (cond-> (assoc data :probe {:arms (count bounces)
                                      :chosen (:input winner)
                                      :tool (:name (:parsed winner))})
            winner
            (assoc :branch
                   (state/add-message
                    branch "user"
                    (str "[harness] "
                         (prompt/render "probe-steer"
                           {:framing (:input winner)
                            :tool (:name (:parsed winner))}))
                    {:turn turn}))))))))

(cell/defcell :probe/ab-model
  {:doc "Ask the same question of several configured models off ONE tape and
        keep every answer as data, without any of them becoming a turn.

        The arms are `gates.edn :probe` :variants — role keys resolved through
        config :run :role-models, so which models get compared is config, not
        code. Adds :ab to the data map; nothing routes on it yet, which is
        deliberate: the comparison is worth recording before it is worth
        acting on."
   :effects [:net :db]
   :requires []
   :input  [:map [:branch :map]]
   ;; Optional for the same reason next-move's is: no configured variants
   ;; means the cell returns `data` and adds nothing.
   :output [:map [:ab {:optional true} :any]]}
  (fn [ctx {:keys [branch] :as data}]
    (let [{:keys [variants]} (gates/threshold :probe)]
      (if-not (seq variants)
        data
        (let [complete-for (fn [role]
                             (infer/complete-fn (role-ctx ctx role)
                                                {:journal? false}))
              out (infer/ab complete-for (infer/of-branch branch) variants)]
          (infer/log-probe! ctx (:id branch) :ab-model
                            {:arms (count (:variants out))
                             :errors (count (filter :error (vals (:variants out))))})
          (assoc data :ab (into {}
                                (map (fn [[k v]] [k (select-keys v [:said :error])]))
                                (:variants out))))))))
