;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; THE BEAM'S ROUND, as cells.
;;
;; The inner TURN has been a manifest since karamazov-ioo.20 — one branch, one
;; model call, one tool call, one arbiter decision. The outer ROUND was not: it
;; was a `loop`/`recur` inside samizdat.agent.beam, so the sequence that
;; decides which branches live, which fork, and when the run ends could not be
;; changed without rebuilding the harness.
;;
;; That sequence is the clearest case of userspace there is. It does not talk
;; to a provider or open a file; it ARRANGES the pieces that do. So it lives
;; here, in this project's own copy, where a project that wants to score before
;; it culls, or cull before it drains directives, or skip repopulation
;; entirely, can say so by editing a manifest.
;;
;; WHAT STAYS IN THE BASE, and why: driving one branch through its turn
;; manifest, fanning out over branches under a deadline, disposing engine
;; sessions, closing branch and run rows, ranking finished branches on a rubric
;; that is already phases.edn data. Those are capabilities. Nothing in them
;; decides a branch's fate.
;;
;; THE ORDER IS LOAD-BEARING and the manifest is where it is now written down:
;;
;;   directives before advance   a human's instruction lands on a turn boundary,
;;                               not mid-turn, so a branch is never holding a
;;                               ledger it read before the change
;;   score before cull           retention reads critic scores, so they must be
;;                               fresh for the post-turn state
;;   cull before spawn           a branch culled this round must not also spend
;;                               the branch budget on children
;;   settle before repopulate    the freed slot has to be visible for the same
;;                               round to refill it
;;
;; Change the order in manifests/beam.edn and you change the policy. That is
;; the point; the reasons above are why you should be deliberate about it.
(ns cells.beam
  (:require [clojure.string :as str]
            [mycelium.cell :as cell]
            [samizdat.agent.beam :as beam]
            [samizdat.agent.critic :as critic]
            [samizdat.agent.gates :as gates]
            [samizdat.prompt :as prompt]
            [samizdat.agent.state :as state]
            [samizdat.session :as session]
            [samizdat.store.failures :as failures]
            [samizdat.store.interventions :as interventions]
            [samizdat.store.journal :as journal]
            [samizdat.store.runs :as runs]))

;; --- the retention policy ----------------------------------------------------
;;
;; WHICH BRANCHES LIVE. This is the harness's most consequential judgement and
;; the one most specific to a project: what counts as a dead end in a codebase
;; with a fast test suite is not what counts in one where a single check takes
;; twenty minutes. It reads thresholds from gates.edn and speaks through
;; prompts/, and now the CASCADE itself — which conditions cull, in what order,
;; with what reason — is here where a project can retune it.
;;
;; Every reason string in here ends up in the permanent record and is read
;; later as evidence, so a reason that names the wrong cause is worse than a
;; vague one. That is why several branches below say what the conditions WERE
;; rather than guessing which of them mattered.

(defn cull-or-keep
  "Apply the retention rule to a branch that just failed.

  A branch that banked something recently is never culled — incremental
  strategies naturally look like verify size N, fail at N+1, verify N+1, and
  culling them throws away the most productive branch in the beam. The
  emergency-review gate is what talks to it instead, and the arbiter has
  already had its say by the time this runs.

  A recent MEASUREMENT counts here as well as a confirmation. A branch
  locating something empirically confirms nothing by construction, and the run
  that motivated this culled exactly such a branch at turn 12 with most of its
  simulation already done (vf-0of). The gates that ask a branch to ship still
  read confirmations only.

  The scalar rule (consecutive failures, no recent confirmation) is the
  TRIGGER; the critic's Pareto frontier is the verdict. A triggered branch
  is culled when a living sibling dominates it on every critic objective,
  when the critic itself scored the line a dead end, or when no scores
  exist (the critic is advisory — absent, the scalar rule stands). A
  triggered branch that is NOT dominated keeps its distinct strengths and
  survives, journaled and on a clock: at cull-hard-multiple times the
  threshold the reprieve ends unconditionally, because Pareto's known
  weakness is permissiveness and a zombie beam is the failure mode.

  A branch inside a REFRAME is spared the failure rule outright (vf-31m). It
  was told to abandon its approach and it carries the failures that caused the
  reframe, so without this it dies for exactly the thing it was just told to
  stop doing — the harness advising and executing on the same turn, which is
  what the stuck gate's 0-met record was really measuring. Bounded like the
  Pareto reprieve and for the same reason: :reframe-grace turns, and
  cull-hard-multiple ends it early. Every cull on the failure path says so
  when the branch had already been handed a reframe, because the reasons are
  the run's post-hoc account of itself and are read later as evidence.

  `survivors` is how many other branches would still be running. Culling
  exists to reallocate the beam's budget to branches doing better; when
  there is nobody to reallocate to, culling is just an early exit with turns
  left on the clock. The width sweep found this the direct way: the width-1
  arm was culled at turn 9 of 12 and the run ended there. The last branch
  standing is never culled; the stuck and emergency-review gates keep
  talking to it instead."
  [{:keys [conn run-id turn]} branch survivors sibling-scores]
  (let [threshold (gates/threshold :cull-threshold)
        fails (or (:consecutive-failures branch) 0)
        mech (or (:consecutive-mechanics-failures branch) 0)
        pol (or (:consecutive-policy-refusals branch) 0)
        grace (gates/threshold :reframe-grace)
        ;; Ever handed a reframe, versus still inside its window. The first
        ;; belongs in the cull record and the second decides the reprieve.
        reframed? (some? (:reframe-entered-turn branch))
        reframing? (state/reframe-active? branch turn grace)
        cull (fn [why] (assoc branch :status :culled :inactive-reason why))
        ;; The failure-path cull. A branch that was told to change approach and
        ;; died failing anyway must say so: the cull reasons are the run's
        ;; post-hoc explanation of itself and are read later as evidence, and
        ;; "consecutive failures" alone hides the fact that the harness had
        ;; already intervened and the intervention did not take (vf-31m).
        cull-fail (fn [why]
                    (cull (str why
                               (when reframed?
                                 (str "; the branch had already been handed a"
                                      " reframe and kept failing")))))
        scores (get-in branch [:critic :scores])
        hard-floor (* (gates/threshold :cull-hard-multiple) threshold)]
    (cond
      ;; A branch that cannot emit a well-formed tool call is bounded, but on
      ;; its own looser threshold and with its own reason. Three bad fences is
      ;; a model having a bad turn; twice that is a branch that cannot work the
      ;; protocol, and saying so beats the dead-end line it used to die with.
      (and (>= mech (* (gates/threshold :cull-mechanics-multiple) threshold))
           (pos? survivors))
      (cull (cond
              ;; The mechanics counter also counts policy refusals, so the
              ;; reason has to say which actually happened. gen-30 B3.2 was
              ;; culled with "could not emit a well-formed fence" when the
              ;; real cause was a harness parse bug, and the reason was
              ;; believed; a declined sketch or verification is a well-formed
              ;; call the harness refused, and naming it as a protocol
              ;; failure would be the same lie in the permanent record.
              ;; Which policy declined them is not tracked per refusal, so
              ;; the reason states the conditions rather than guessing between
              ;; them. Naming one would be the same class of lie.
              (and (pos? pol) (= pol mech))
              (str "culled after " mech " consecutive turns with no usable"
                   " tool call; every call was declined by harness policy —"
                   " the branch was in the "
                   (str/upper-case (name (or (:phase branch) :build))) " phase"
                   (when reframing? " with its approach withheld")
                   " and did not change what it was asking for")

              (pos? pol)
              (str "culled after " mech " consecutive turns with no usable"
                   " tool call; " pol " were declined by harness policy and "
                   (- mech pol) " could not emit a well-formed fence")

              :else
              (str "culled after " mech " consecutive turns with no usable tool"
                   " call; the branch could not emit a well-formed fence")))

      (not (and (>= fails threshold)
                (not (state/banked-in-last branch
                                           (gates/threshold :cull-recent-window)))
                (pos? survivors)))
      branch

      ;; Ahead of the reframe reprieve, deliberately: a branch still failing
      ;; at twice the cull threshold is not reframing, and the reprieve is a
      ;; loan with a clock rather than an exemption.
      (>= fails hard-floor)
      (cull-fail (str "culled after " fails
                      " consecutive failures; the Pareto reprieve was spent"))

      ;; The reframe reprieve (vf-31m). A branch dropped into a reframe carries
      ;; the failures that caused it, so without this it is culled for exactly
      ;; the approach it was just told to abandon — the harness advising and
      ;; executing on the same turn, which is the bug the stuck gate's 0-met
      ;; record was really measuring. Nothing is said here: the refusal is
      ;; already talking to the branch every time it retries the old approach.
      reframing?
      (do (when (and conn run-id)
            (journal/note! conn run-id :cull-spared
                           {:branch-id (:id branch)
                            :data {:scores scores :failures fails
                                   :reframe? true
                                   :reframe-claim (:reframe-claim branch)}}))
          branch)

      ;; A dead end is a dead end at any age; the critic's own verdict is
      ;; the one judgement that does not depend on how long the branch has
      ;; had to accumulate anything.
      (and scores (<= (:viability scores) 1))
      (cull-fail (str "culled after " fails
                      " consecutive failures; the critic scored the line a dead end"))

      ;; Juvenile grace. Progress and momentum are age-correlated, so a
      ;; newborn is dominated by its own parent one turn after being forked.
      ;; Let it express itself first.
      ;; The branch's OWN experience, not its position in the run: a fork
      ;; born at round 18 is 0 turns old (blt.16).
      (< (state/own-turn-count branch) (gates/threshold :juvenile-grace))
      (do (when (and conn run-id)
            (journal/note! conn run-id :cull-spared
                           {:branch-id (:id branch)
                            :data {:scores scores :failures fails :juvenile? true}}))
            (state/add-message
             branch "user"
             ;; Tier 2d: the spare's prose is prompts/juvenile-grace.md.
             (prompt/render "juvenile-grace" {:failures fails})))

      (nil? scores)
      (cull-fail (str "culled after " fails
                      " consecutive failures with no recent confirmed work"))

      (critic/dominated? scores sibling-scores)
      (cull-fail (str "culled after " fails
                      " consecutive failures; dominated by a sibling on every"
                      " critic objective"))

      :else
      (do (when (and conn run-id)
            (journal/note! conn run-id :cull-spared
                           {:branch-id (:id branch)
                            :data {:scores scores :failures fails}}))
            (state/add-message
             branch "user"
             ;; Tier 2d: the reprieve's prose is prompts/cull-reprieve.md.
             (prompt/render "cull-reprieve" {:failures fails :hard-floor hard-floor}))))))

;; --- the repopulation policy -------------------------------------------------

(defn repopulate
  "Refill the beam when it has fallen below its target width.

  This is the blocker that kept the frontier from being a frontier. Culling
  removes width permanently, while the only route back up — the branch-out
  rung — is gated on a confirmation AND a cooldown. So the population
  monotonically decayed: five campaign runs, five collapses to a single
  surviving line, whatever the cap allowed. A genetic algorithm maintains
  its population; death without replacement is just attrition.

  When fewer branches are alive than the run asked for and the cap has
  room, the strongest survivor is told to reseed. Deliberately NOT gated on
  a fresh confirmation: refilling an empty slot is a different act from
  asking a busy branch for more, and the alternative is a beam that spends
  the rest of the run at width one. The per-branch cooldown still applies,
  so one death does not produce a stampede of asks at the same branch.

  This MARKS the branch; it does not speak to it. The ask itself is the
  :repopulate gate, which reads the mark. It used to append the message here,
  which made it an invitation rather than a mechanism: no prediction, nothing
  to settle, no row in the gate tally. gen-17 sent 12 and 9 were declined, and
  that was invisible until someone counted branch-opened events by hand — the
  same argument that turned branch-out into a gate. Speaking from here also
  put a second harness voice on a boundary that had already had its one steer.

  The precondition needs facts only the scheduler has (how many are alive, the
  target width, which survivor is strongest), which is why the split is mark
  here, ask there."
  [{:keys [conn run-id beam-width]} branches total-count turn]
  (let [cap (gates/threshold :max-total-branches)
        cooldown (gates/threshold :fork-invite-cooldown)
        floor (gates/threshold :fork-invite-floor)
        earning? (fn [b]
                   ;; The floor (provenance R2-13, wired): a survivor below the
                   ;; minimum critic scores is not invited to reseed — growth
                   ;; does not spend budget on lines not yet earning it.
                   (every? (fn [[obj minimum]]
                             (>= (get-in b [:critic :scores obj] 0) minimum))
                           floor))
        alive (filterv state/active? branches)
        target (or beam-width 1)
        strength (fn [b]
                   (let [sc (get-in b [:critic :scores])]
                     [(reduce + 0 (vals (select-keys sc (critic/survival-objectives))))
                      (count (state/confirmed-artifacts b))]))
        candidate (when (and (< (count alive) target) (< total-count cap))
                    (->> alive
                         (remove #(when-let [t (:fork-invited %)]
                                    (< (- turn t) cooldown)))
                         (filter earning?)
                         (sort-by strength)
                         reverse
                         first))]
    (if-not candidate
      branches
      (do (when (and conn run-id)
            (journal/note! conn run-id :repopulate
                           {:branch-id (:id candidate) :turn turn
                            :data {:alive (count alive) :target target}}))
          (mapv #(if (= (:id %) (:id candidate))
                   (assoc % :fork-invited turn
                            :repopulate-due turn
                            :repopulate-alive (count alive)
                            :repopulate-target target)
                   %)
                branches)))))

;; --- the top of the round ----------------------------------------------------

(cell/defcell :beam/round-open
  {:doc "Decide whether this round happens at all, and record why not.

        Four verdicts. :aborted — the abort flag is set; checked FIRST and at
        the top, because a stop must not need the run's cooperation. :completed
        — a branch shipped and the campaign policy says that ends the run.
        :exhausted — nobody is left to explore, or the turn cap is spent.
        :continue — do the round.

        Reads the abort flag, so not pure."
   :effects [:db]
   :requires [:abort]
   ;; The ROUND's shape. Every beam cell reads and writes the same working
   ;; set, so these declarations are the round's data flow written down: what
   ;; each stage needs to have happened before it, and what it leaves for the
   ;; next. The order the manifest wires them in is the order these inputs
   ;; force.
   :input  [:map [:branches :any] [:turn :int]]
   ;; :verdict as well, and it is the one that matters: beam.edn's :start
   ;; dispatches the entire round on it, four ways. Omitting it left the
   ;; scheduler's single routing key the one thing the chain could not see.
   :output [:map [:active :any] [:done-branch :any]
            [:multi-candidate? :boolean] [:verdict :keyword]]}
  (fn [{:keys [abort] :as ctx} {:keys [branches turn] :as data}]
    ;; A paused run waits HERE, at the top of the round, before anything is
    ;; scheduled — which is what `pause` promises: no new turns, and whatever
    ;; is in flight finishes (it already has, a round ago). The wait polls the
    ;; abort flag too, so a paused run is still stoppable; a pause that could
    ;; not be aborted out of would be a wedge with a friendly name.
    (beam/await-resume! ctx)
    (let [cap (beam/round-max-turns ctx data)
          active (filterv state/active? branches)
          candidates (filterv :final-answer branches)
          done (beam/finish-now? ctx (beam/select-done-branch ctx candidates) branches)]
      (assoc data
             :active active
             :done-branch done
             :multi-candidate? (< 1 (count candidates))
             :verdict (cond
                        (and abort @abort) :aborted
                        done :completed
                        (or (empty? active) (> turn cap)) :exhausted
                        :else :continue)))))

;; --- the round --------------------------------------------------------------

(cell/defcell :beam/directives
  {:doc "Apply pending human directives at the boundary and resolve each in the
        interventions table. First in the round on purpose: a directive that
        landed mid-turn would rewrite state under a branch that had already
        read it.

        Three of the eight kinds are about the RUN rather than a branch, and
        this is where they land in the data map: `:paused?` (the next round
        waits at :beam/round-open, so turns in flight finish), and
        `:max-turns` (an `extend` raises the cap the round-open exhaustion
        check reads). Both persist across the back edge because :beam/tick
        drops only the per-round products."
   :effects [:db]
   :requires []
   :input  [:map [:active :any] [:turn :int] [:branches :any]]
   ;; :paused-by-directive? and a raised :max-turns are conditional — most
   ;; rounds drain no directives at all.
   :output [:map [:active :any] [:branches :any]
            [:paused-by-directive? {:optional true} :any]
            [:max-turns {:optional true} :any]]}
  (fn [ctx {:keys [active turn] :as data}]
    (let [{:keys [conn run-id]} ctx
          pending (interventions/pending conn run-id)
          {:keys [branches max-turns paused?]}
          (beam/drain-directives! ctx active pending turn)
          ;; A branch the human just culled is inactive NOW, and this cell is
          ;; the only one that knows: it never enters :advanced, so the
          ;; settle/cull bookkeeping downstream never sees it. Close its row
          ;; and release what it held here, and fold the mark into :branches
          ;; so settle's inactive set (computed from :branches) carries it
          ;; into the round's record instead of tick dropping it — the open
          ;; zombie row of karamazov-blt.11.
          now-inactive (filterv (complement state/active?) branches)
          still-active (filterv state/active? branches)
          marked (into {} (map (juxt :id identity)) branches)]
      (beam/record-inactive! ctx now-inactive)
      (let [data' (cond-> (assoc data
                                 :active still-active
                                 :branches (mapv #(get marked (:id %) %)
                                                 (:branches data)))
                    (some? paused?) (assoc :paused-by-directive? paused?)
                    max-turns (update :max-turns
                                      (fnil + (beam/round-max-turns ctx data))
                                      max-turns))]
        ;; An extension is a fact about the RUN, so it survives a crash: the
        ;; row is what a resume reads its budget from, and the drain's old
        ;; comment ("a resume re-reads its budget from the control API
        ;; anyway") was simply false (karamazov-blt.12).
        (when max-turns
          (runs/extend-budget! conn run-id (:max-turns data')))
        data'))))

(cell/defcell :beam/advance
  {:doc "One turn for every active branch, concurrently, each under a hard
        deadline. A branch that throws is abandoned rather than taking the beam
        down with it; a branch that hangs loses only its own turn."
   :effects [:net :db :fs :proc]
   :requires [:live-branches]
   :input  [:map [:active :any] [:branches :any] [:turn :int]]
   :output [:map [:advanced :any]]}
  (fn [ctx {:keys [active branches turn] :as data}]
    ;; The turn slice and its gates read (:max-turns ctx); an `extend` lives
    ;; in the round's data map. Without this, every turn past the ORIGINAL
    ;; cap routed :exhausted -> :memory/distil — one LLM reflection per
    ;; branch per extended round — while the beam kept scheduling
    ;; (karamazov-blt.12).
    (let [advanced (beam/advance-all (assoc ctx
                                            :branch-count (count branches)
                                            :max-turns (beam/round-max-turns ctx data))
                                     (filterv state/active? active)
                                     turn)]
      ;; The driver's teardown needs the branches as they stand, and a manifest
      ;; run does not hand intermediate state back on a throw. The atom is the
      ;; driver's window into the round; see beam/run-rounds' finally.
      (when-let [live (:live-branches ctx)] (reset! live advanced))
      (assoc data :advanced advanced))))

(cell/defcell :beam/score
  {:doc "Refresh critic scores for every active branch, at most one sub-LLM
        call per branch per :critic-every window. Before the retention pass,
        because that pass reads them and stale scores would decide a live
        branch's fate on last round's evidence."
   :effects [:net :db]
   :requires []
   :input  [:map [:advanced :any] [:turn :int]]
   :output [:map [:advanced :any]]}
  (fn [ctx {:keys [advanced turn] :as data}]
    (assoc data :advanced (beam/ensure-scored ctx advanced turn))))

(cell/defcell :beam/cull
  {:doc "The retention pass: every branch that just failed faces the cull rule,
        left to right, against the count of branches that would still be
        running after the decisions already made.

        Evaluated in order rather than in parallel for that reason — the last
        branch standing is never culled, so whether THIS branch survives
        depends on what happened to the ones before it."
   :effects [:db]
   :requires []
   :input  [:map [:advanced :any] [:turn :int]]
   :output [:map [:culled :any]]}
  (fn [ctx {:keys [advanced turn] :as data}]
    ;; Only ACTIVE branches face the rule, and only they count as survivors.
    ;; `advanced` also holds branches that went done/abandoned during this
    ;; round's advance: counting them seeded `alive` high, so the LAST active
    ;; branch saw phantom survivors and was cullable — the beam emptied and
    ;; the run exhausted, violating the last-branch invariant. And running the
    ;; cascade over an already-inactive branch rewrote its ending (:abandoned
    ;; relabelled :culled with a fabricated mechanics reason) — the false-
    ;; reason class this cell's own comments exist to prevent
    ;; (karamazov-blt.17).
    (let [culled (first
                  (reduce (fn [[acc alive] b]
                            (if-not (state/active? b)
                              [(conj acc b) alive]
                              (let [sibs (keep #(when (and (state/active? %)
                                                           (not= (:id %) (:id b)))
                                                  (get-in % [:critic :scores]))
                                               advanced)
                                    b' (cull-or-keep (assoc ctx :turn turn)
                                                     b (dec alive) sibs)]
                                [(conj acc b') (if (state/active? b') alive (dec alive))])))
                          [[] (count (filter state/active? advanced))]
                          advanced))]
      ;; SHARPENING vs EXPANSION (papers/2608.17981v1 §4.5.4, after Yue et al.
      ;; on pass@k). A beam can fail two ways and they want opposite fixes:
      ;; no branch ever held the answer (expansion — widen, diversify), or one
      ;; did and the harness threw it away (sharpening — fix the rubric and the
      ;; cull thresholds). End-to-end success confuses them.
      ;;
      ;; A branch culled while holding CONFIRMED artifacts is the cheap
      ;; in-run signal for the second: it had banked machine-checked evidence
      ;; and was killed anyway. Counted here, where the decision is actually
      ;; made, so the diagnosis is available without a bench run.
      (doseq [b culled
              :when (and (not (state/active? b))
                         (seq (state/confirmed-artifacts b)))]
        (session/observe! [:beam :culled-with-evidence]))
      (assoc data :culled culled))))

(cell/defcell :beam/settle
  {:doc "Write the ending of every branch that is no longer active and release
        what it held, then fold this round's survivors back together with the
        branches that were already inactive.

        Before repopulation, so a slot freed this round is visible to the
        refill that happens in the same round."
   :effects [:db]
   :requires []
   :input  [:map [:branches :any] [:culled :any]]
   :output [:map [:inactive :any] [:all-now :any]]}
  (fn [ctx {:keys [branches culled] :as data}]
    (beam/record-inactive! ctx culled)
    (let [inactive (filterv (complement state/active?) branches)]
      (assoc data
             :inactive inactive
             :all-now (into (vec inactive) culled)))))

(cell/defcell :beam/repopulate
  {:doc "Refill the beam when it has fallen below its target width by MARKING
        the strongest earning survivor. The ask itself is the :repopulate gate,
        which reads the mark — so the invitation has a prediction and shows up
        in the gate tally rather than being an untracked second harness voice."
   :effects [:db]
   :requires []
   :input  [:map [:culled :any] [:all-now :any] [:turn :int]]
   :output [:map [:culled :any]]}
  (fn [ctx {:keys [culled all-now turn] :as data}]
    (assoc data :culled (repopulate ctx culled (count all-now) turn))))

(cell/defcell :beam/spawn
  {:doc "Turn each branch's pending theses into sibling branches, under the
        total cap. After the cull, so a branch that died this round does not
        spend the budget on children."
   :effects [:db]
   :requires []
   :input  [:map [:culled :any] [:all-now :any] [:turn :int]]
   :output [:map [:children :any] [:updated :any]]}
  (fn [ctx {:keys [culled all-now turn] :as data}]
    (let [[children updated]
          (reduce (fn [[acc bs] b]
                    (if (and (state/active? b) (seq (:pending-branch-theses b)))
                      (let [[kids parent] (beam/spawn-children!
                                           ctx b (+ (count all-now) (count acc)) turn)]
                        [(into acc kids) (conj bs parent)])
                      [acc (conj bs b)]))
                  [[] []]
                  culled)]
      (assoc data :children children :updated updated))))

(def ^:private round-products
  "The keys a round produces and the next one must not inherit.

  CLEARED TO NIL, NOT DISSOC'D. mycelium propagates keys as
  `(merge input (handler input))`, so a handler cannot remove anything — the
  input map is merged back over the top and the dissoc is undone. This cell's
  own docstring claimed it dropped these for six months; it never did, and the
  data map it was written to bound held every branch's whole message history
  five times over, once per round, for the life of the run.

  Nil is what a handler CAN say, because handler output wins on a key it
  names. The key survives and the value it was holding is released, which is
  the half that mattered — and a stale round product now reads as absent to
  the cell that would have inherited it."
  [:active :advanced :culled :inactive :all-now :updated :children
   :done-branch :multi-candidate? :verdict])

(cell/defcell :beam/tick
  {:doc "Close the round: reassemble the branch set, advance the turn, and
        release the per-round products so the data map does not grow without
        bound.

        The trace is capped hard here. Every mycelium trace entry snapshots the
        whole data map, and this map holds every branch's entire message
        history — an uncapped trace would be quadratic in the run.

        Declared EFFECTFUL although it touches no table: it reset!s the
        driver's :live-branches teardown atom, and the :pure mark is
        load-bearing — the mutation soak runs pure cells for real, so a soak
        of this manifest would have clobbered the record the driver's finally
        reads (karamazov-blt.35)."
   :effects [:db]
   :requires [:live-branches]
   :input  [:map [:inactive :any] [:updated :any] [:children :any]]
   ;; It also CLEARS the round products, merging them back to nil, which
   ;; mycelium does not model — it tracks what a cell adds and never what it
   ;; drops. Safe here only because :tick edges back to :start, which the
   ;; chain walk has already visited, so no downstream cell is told a cleared
   ;; key is still available. The same caveat :loop/route carries.
   :output [:map [:branches :any] [:turn :int]]}
  (fn [ctx {:keys [inactive updated children] :as data}]
    (let [next-branches (into (into (vec inactive) updated) children)]
      (when-let [live (:live-branches ctx)] (reset! live next-branches))
      (-> data
          (assoc :branches next-branches)
          (update :turn inc)
          (merge (zipmap round-products (repeat nil)))
          (update :mycelium/trace #(vec (take-last 5 %)))))))

;; --- the three endings -------------------------------------------------------

(cell/defcell :beam/abort
  {:doc "The run was stopped from outside. Every still-active branch is closed
        as abandoned; no answer is claimed."
   :effects [:db]
   :requires [:conn :run-id]
   :input  [:map [:branches :any] [:active :any]]
   :output [:map [:status :keyword] [:result :any]]}
  (fn [{:keys [conn run-id]} {:keys [branches active] :as data}]
    (doseq [b active]
      (runs/close-branch! conn run-id (:id b) :abandoned "aborted"))
    (assoc data :status :aborted :result {:status :aborted :run-id run-id
                                          :branches branches})))

(cell/defcell :beam/complete
  {:doc "A branch shipped and the campaign policy says that ends the run. The
        others are closed naming what superseded them — 'outranked by' when
        more than one branch had shipped and the rubric chose, 'superseded by'
        when only one had."
   :effects [:db]
   :requires [:conn :run-id]
   ;; :done-branch and :multi-candidate? come from :beam/round-open, which is
   ;; the only node that edges here — this ending exists because that node
   ;; found a finished branch.
   :input  [:map [:branches :any] [:done-branch :any]
            [:multi-candidate? :boolean]]
   ;; :result is the beam's actual product — run-rounds reads it and throws
   ;; "the beam scheduler ended without a result" without it. All three
   ;; endings write it and all three now say so.
   :output [:map [:status :keyword] [:result :any]]}
  (fn [{:keys [conn run-id]} {:keys [branches done-branch multi-candidate?] :as data}]
    (doseq [b branches
            :when (and (state/active? b) (not= (:id b) (:id done-branch)))]
      (runs/close-branch! conn run-id (:id b) :abandoned
                          (str (if multi-candidate? "outranked by " "superseded by ")
                               (:id done-branch)
                               (when-not multi-candidate? " done()"))))
    (runs/finish-run! conn run-id :completed (:final-answer done-branch))
    (assoc data :status :completed
           :result {:status :completed :answer (:final-answer done-branch)
                    :run-id run-id :branches branches})))

(cell/defcell :beam/exhaust
  {:doc "Nobody is left to explore, or the turn cap is spent. Every active
        branch is closed as exhausted and each branch's RESIDUAL is journalled:
        what it believed it was close to when the budget ran out, so a resume
        does not re-derive scope from the transcript."
   :effects [:db]
   :requires [:conn :max-turns :run-id]
   :input  [:map [:branches :any] [:active :any]]
   ;; A last look for a finished branch, so this ending may still report
   ;; :completed with a winner — hence :done-branch on the way out.
   :output [:map [:status :keyword] [:result :any]
            [:done-branch {:optional true} :any]]}
  (fn [{:keys [conn run-id max-turns] :as ctx} {:keys [branches active] :as data}]
    ;; A branch may have SHIPPED rounds ago while :stop-on-first-done? kept
    ;; the beam exploring. The cap expiring is not a failure then: the banked
    ;; answer ends the run, ranked by the same rubric :beam/complete uses.
    ;; finish-run! :failed nil here discarded it (karamazov-blt.20).
    (let [winner (beam/select-done-branch ctx (filterv :final-answer branches))]
      (doseq [b active]
        (runs/close-branch! conn run-id (:id b) :exhausted
                            (str "turn cap of " max-turns " reached")))
      (if winner
        (do (runs/finish-run! conn run-id :completed (:final-answer winner))
            (assoc data :status :completed :done-branch winner
                   :result {:status :completed :answer (:final-answer winner)
                            :run-id run-id :branches branches}))
        (let [residuals (keep state/residual branches)
              report (state/build-residual-report
                      {:branches branches
                       :failures (failures/recent conn run-id 10)
                       :gate-tally (journal/gate-tally conn run-id)
                       :max-turns max-turns})]
          (doseq [r residuals]
            (journal/note! conn run-id :residual {:branch-id (:branch r) :data r}))
          (journal/note! conn run-id :residual-report {:data report})
          ;; :exhausted, NOT :failed. A thrown error also records :failed
          ;; (beam/run!'s catch), so recording an honest end-of-budget the same
          ;; way made "the harness broke" and "the work did not finish in the
          ;; turns it had" the same row — and the second is an ordinary
          ;; outcome that a resume is built to continue (karamazov-emw).
          (runs/finish-run! conn run-id :exhausted nil)
          (assoc data :status :exhausted
                 :result {:status :exhausted :run-id run-id :branches branches
                          :residuals (vec residuals) :report report
                          :report-text (state/render-residual-report report)}))))))
