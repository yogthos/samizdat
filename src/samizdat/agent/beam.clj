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

(ns samizdat.agent.beam
  "The beam's CAPABILITIES: what it takes to run many branches at once, with
  none of the judgement about which of them should live.

  The round itself is manifests/beam.edn, and the decisions inside it are
  cells/beam.clj — both userspace, both this project's own copy. What is left
  here is the set of pieces that round is assembled from, and this docstring's
  job is to say why each one is a piece rather than a policy that got away.

  MECHANISM, deliberately:

  - `advance-branch` / `advance-all` — drive one branch through its turn
    manifest, and fan that out over the active set under a deadline. The
    barrier is structural, not a preference: a branch deciding whether to fork
    needs the failure log as of the whole beam's last round, not as of whenever
    a sibling happened to finish. A branch that THROWS is abandoned and a
    branch that HANGS forfeits its turn — both are fault handling, not a
    verdict on the branch's line of inquiry, which is why neither touches the
    failure counters the retention policy reads.
  - `drain-directives!` — apply a human's instruction at a turn boundary and
    resolve it in the record. It EXECUTES a decision rather than making one.
    Its one refusal — a cull that would empty the run — is a safety guard on an
    irreversible action, of the same kind as refusing to delete the last
    backup; the alternative is guessing that a person meant to end their run.
  - `select-done-branch` — rank the branches that shipped. The rubric is
    already phases.edn data (`state/finished-key`); this just applies it and
    journals the comparison.
  - `child-ids` — allocate unused ids for a parent's children. Naming, not
    deciding.
  - `record-inactive!`, `dispose-branch-engines!` — write an ending and release
    what the branch held.
  - `run-rounds` / `run!` — the driver: compile the round manifest, hand it the
    branches, own the crash record and the teardown. See `run-rounds` for why
    those two cannot live in the manifest.

  Each branch carries its own message history and turn log. The only thing they
  share is the failure log, and that sharing is the point: an approach one
  branch disproved should not be retried by another. It is FTS-ranked rather
  than broadcast whole, so a branch is shown the failures most like what it
  just tried instead of everything everyone ever got wrong.

  The width is not treated as justified. The original never measured five
  branches against one branch at five times the turn budget, and
  `samizdat.bench.beam` is the comparison."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.tools.logging :as log]
            [mycelium.core :as myc]
            [samizdat.agent.critic :as critic]
            [samizdat.agent.gates :as gates]
            [samizdat.agent.gitdiff :as gitdiff]
            [samizdat.agent.loop :as branch-loop]
            [samizdat.agent.select :as select]
            [samizdat.events :as events]
            [samizdat.agent.state :as state]
            [samizdat.prompt :as prompt]
            [samizdat.session :as session]
            [samizdat.watch :as watch]
            [samizdat.lexicon :as lexicon]
            [samizdat.agent.oversight :as oversight]
            [samizdat.repl :as repl]
            [samizdat.repl.route :as route]
            [samizdat.store.artifacts :as artifacts]
            [samizdat.store.failures :as failures]
            [samizdat.store.interventions :as interventions]
            [samizdat.store.journal :as journal]
            [samizdat.store.knowledge :as knowledge]
            [samizdat.store.runs :as runs]
            [samizdat.userspace :as userspace]
            [samizdat.workflow :as workflow])
  (:refer-clojure :exclude [run!]))

(defn- crossover-block
  "What OTHER lineages have proved, for a newly forked child's opening
  context.

  This is the recombination half. A child that inherits only its parent's
  history is mutation: it deepens one line. Opening it holding the results
  its aunts and uncles confirmed lets a fork COMBINE lineages — the branch
  that proved a bound in Prolog and the branch that proved a structure in
  Lean can have a child that uses both. Own-lineage results are excluded
  because the child already carries them in its inherited history."
  [conn run-id parent-id]
  (let [{:keys [statuses limit]} (gates/threshold :crossover)
        others (remove #(= parent-id (:branch_id %))
                       (journal/artifacts conn run-id))
        confirmed (filter #(contains? statuses (:claim_status %)) others)]
    (when (seq confirmed)
      ;; Tier 2d: the header prose is prompts/crossover.md — runtime-editable,
      ;; the same seam every gate message reads through.
      (str "\n\n"
           (prompt/render "crossover"
             {:artifacts (str/join "\n"
                                   (for [a (take-last limit confirmed)]
                                     (str "- [" (:branch_id a) " " (:kind a) "] "
                                          (:claim a))))})))))

(defn- seed-branch
  "The child's opening state: its parent's conversation when there is a parent
  to inherit from and the policy says take it, else a fresh tape.

  `gates.edn :fork-inherit` owns the decision, so switching a run back to
  fresh-tape forks — or forking from an older turn — is a data edit and not a
  rebuild. See `state/fork-branch` for what is and is not inherited."
  [{:keys [problem]} parent id thesis turn]
  (let [{:keys [inherit? depth]} (gates/threshold :fork-inherit)]
    (if (and parent inherit?)
      (state/fork-branch parent {:id id :depth depth :turn turn
                                 :thesis thesis :problem problem})
      (cond-> (state/new-branch
               {:id id :parent-id (:id parent) :problem problem
                :created-at-turn turn
                :messages (branch-loop/initial-messages problem)})
        thesis (assoc :thesis thesis)))))

(defn- open-branch!
  [{:keys [conn run-id] :as ctx} id parent thesis turn]
  (let [parent-id (:id parent)
        ;; One eval namespace per BRANCH. A child forks its parent's, so the
        ;; conversation it inherits stays true — it can call what it can read
        ;; itself defining — while anything either defines afterwards stays
        ;; its own. dispose-branch-engines! closes it when the branch ends.
        b (assoc (seed-branch ctx parent id thesis turn)
                 :repl-session (if parent
                                 (repl/fork-session (:repl-session parent))
                                 (repl/new-session)))]
    (runs/open-branch! conn run-id {:branch-id id :parent-id parent-id
                                    :created-at-turn turn})
    (if thesis
      (do (runs/set-thesis! conn run-id id thesis)
          (-> b
              (assoc :thesis thesis)
              ;; The nudge is prompts/fork-thesis.md — runtime-editable, the
              ;; same seam every other harness message reads through, and it
              ;; has to say something different to a child that inherited its
              ;; parent's history than to one starting the problem over.
              (state/add-message
               "user"
               (prompt/render "fork-thesis"
                 {:parent parent-id
                  :goal (:goal thesis)
                  :technique (:technique thesis)
                  :crossover (crossover-block conn run-id parent-id)
                  ;; :forked-at is stamped only by state/fork-branch, so its
                  ;; presence IS the answer to "did this child inherit?".
                  :inherited (some? (:forked-at b))}))))
      b)))

(defn ensure-scored
  "Fresh critic scores for every active branch, at most one sub-LLM call per
  branch per :critic-every window. A scoring that fails leaves the previous
  scores in place — stale information beats invented information."
  [ctx branches turn]
  (mapv (fn [b]
          (if (and (state/active? b)
                   (or (nil? (get-in b [:critic :turn]))
                       (>= (- turn (get-in b [:critic :turn]))
                           (gates/threshold :critic-every))))
            (let [siblings (filterv #(and (state/active? %)
                                          (not= (:id %) (:id b)))
                                    branches)]
              (if-let [s (critic/score! ctx b siblings turn)]
                (assoc b :critic s)
                b))
            b))
        branches))

(defn- child-ids
  "`n` unused child ids for `parent-id`, given the ids already `taken`.

  Numbering has to survive a parent forking more than once. It used to be the
  index within the spawn batch plus two, which reissues `.2` the second time a
  branch branches — and the INSERT then fails the branches primary key and
  takes the whole run down with it. Both gen-11 and gen-12 died exactly there,
  and neither had anything unusual about it: repopulation asks the strongest
  survivor to branch again, and the strongest survivor is by definition one
  that has already branched.

  Gaps are filled rather than stepped over. A child id is a unique key, not a
  record of spawn order, and the turn a branch was created is already stored
  on the row for anyone who wants the ordering."
  [taken parent-id n]
  (loop [ix 2, acc []]
    (if (<= n (count acc))
      acc
      (let [id (str parent-id "." ix)]
        (recur (inc ix) (cond-> acc (not (taken id)) (conj id)))))))

(defn spawn-children!
  "Turn a branch's pending theses into sibling branches, under the total cap.

  Returns [children updated-parent]. The cap is a cost ceiling: every branch is
  another concurrent provider call per turn plus another engine process, so
  when it binds the parent is told rather than the request silently shrinking."
  [ctx parent existing-count turn]
  (let [pending (:pending-branch-theses parent)
        cap (gates/threshold :max-total-branches)
        room (max 0 (- cap existing-count))
        take-n (min room (count pending))
        spawning (vec (take take-n pending))
        parent (assoc parent :pending-branch-theses nil)]
    (cond
      (empty? pending) [[] parent]

      (zero? room)
      [[] (state/add-message
           parent "user"
           ;; The prose is prompts/branch-cap.md — the same runtime-editable
           ;; seam every other harness message reads through.
           (str "[harness] " (prompt/render "branch-cap"
                               {:none true :asked (count pending) :cap cap})))]

      :else
      ;; Read the taken ids from the branches table rather than counting on
      ;; the parent value, which a resumed run rebuilds from the journal and
      ;; which is not the authority on what has been inserted anyway.
      (let [taken (set (map :id (runs/branches (:conn ctx) (:run-id ctx))))
            ids (child-ids taken (:id parent) (count spawning))
            ;; mapv, not map: these INSERT. A lazy seq of side effects is only
            ;; correct for as long as every caller keeps realising it.
            ;; The parent VALUE, not just its id: a child inherits its
            ;; conversation from it (LR-1, gates.edn :fork-inherit).
            children (mapv (fn [id t] (open-branch! ctx id parent t turn))
                           ids spawning)
            parent (cond-> parent
                     (< take-n (count pending))
                     (state/add-message
                      "user"
                      (str "[harness] " (prompt/render "branch-cap"
                                          {:asked (count pending) :cap cap
                                           :allowed take-n}))))]
        [(vec children) parent]))))

(defn turn-deadline-ms
  "The hard ceiling on one branch turn, from gates.edn :turn-deadline-ms.

  A policy number, so it lives with the other policy numbers rather than as a
  constant here — a project whose checks are slower raises it for itself. The
  env override stays for an operator who needs it for one run without editing
  the project's policy; the reasoning behind the value is in gates.edn."
  []
  (or (some-> (System/getenv "HARNESS_TURN_DEADLINE_MS") parse-long)
      (gates/threshold :turn-deadline-ms)))

(defn round-max-turns
  "The turn cap this round compares against: the run's, plus whatever `extend`
  directives have raised it by.

  A function rather than a ctx read, because `extend` has to be able to move
  it and ctx is immutable for the life of the run."
  [ctx data]
  (or (:max-turns data) (:max-turns ctx)))

(defn await-resume!
  "Block while the run is paused, returning as soon as it resumes or aborts.

  Re-reads `interventions/paused?` on every pass rather than trusting the
  round's data map: the resume is a row a human writes from ANOTHER PROCESS
  while this loop is inside the wait, so a snapshot taken before the wait
  began is exactly the value that can never change. Reading the flag once
  would be an infinite loop with a poll in it.

  Polls rather than waits on a condition for the same reason — there is no
  in-process event to signal on. The interval is `gates.edn :pause-poll-ms`.
  The abort flag is checked on every pass, because a paused run that could not
  be aborted out of would be a wedge with a friendly name.

  APPLIES pending pause/resume rows itself, every pass. The `:beam/directives`
  cell that normally applies them sits DOWNSTREAM of the node this blocks in,
  so a `resume` submitted while paused stayed `pending` forever and `paused?`
  — which counts applied rows only — kept reading the pause: the pause was a
  one-way door to abort (karamazov-blt.9). Draining the two run-level kinds
  here is the same boundary the directives cell applies them at — the top of
  a round, with nothing in flight.

  Returns the number of times it waited, so a test can tell waiting from not
  waiting without watching the clock."
  [{:keys [conn run-id abort] :as _ctx}]
  (if-not (and conn run-id)
    0
    (loop [waited 0]
      (if (and abort @abort)
        waited
        (do
          (doseq [d (interventions/pending conn run-id)
                  :when (#{"pause" "resume"} (:kind d))]
            (interventions/resolve! conn run-id (:id d) :applied nil nil))
          (if (interventions/paused? conn run-id)
            (do (Thread/sleep (gates/threshold :pause-poll-ms))
                (recur (inc waited)))
            waited))))))

(defn- rejected
  "Record a refused directive with its reason.

  The reason is prose a person reads in the run view, so it is
  prompts/directive-rejected.md and not a string here — the same seam the
  cull refusal already used, which is what made the ones written inline look
  like an oversight rather than a choice. One template keyed by reason rather
  than five files, because they are one sentence each and belong together."
  [conn run-id d turn reason-ctx]
  (interventions/resolve! conn run-id (:id d) :rejected
                          (prompt/render "directive-rejected" reason-ctx)
                          turn))

(defn- directive-payload
  "A directive's JSON payload as a map, or {} — a payload that will not parse
  is a malformed request, not a reason to take the round down."
  [d]
  (or (try (json/read-str (str (:payload d)) :key-fn keyword)
           (catch Throwable _ nil))
      {}))

(defn drain-directives!
  "Apply pending human directives at the boundary, and record what happened to
  each.

  MECHANISM, not policy: this executes a decision a person already made. The
  `:status :culled` it writes is the human's verdict being carried out, which
  is why it does not go through the retention cascade and why its reason says
  'culled by a human' rather than naming a rule.

  A directive is never silently dropped. `cull` on the last running branch — or
  an untargeted one that would take every branch with it — is refused with a
  reason rather than obeyed. That is a guard on an irreversible action rather
  than a judgement about the branches: obeying it would end the run in a way
  the person almost certainly did not intend, and rather than guessing we say
  so.

  Returns `{:branches :max-turns :paused?}`. It used to return the branch
  vector alone, which was why four of the eight kinds `interventions/kinds`
  advertises to a human were rejected here as `not wired to a scheduler action
  yet`: `pause`, `resume` and `extend` are facts about the RUN and had nowhere
  to go, and rejecting them explicitly beat accepting them silently but left
  the control API promising something the scheduler would not do. `:max-turns`
  and `:paused?` are nil when no directive touched them, so the caller keeps
  whatever it had."
  [{:keys [conn run-id]} branches directives turn]
  (reduce
   (fn [{:keys [branches] :as acc} d]
     (let [kind (:kind d)
           bs branches
           target (:branch_id d)
           matches? (fn [b] (or (nil? target) (= target (:id b))))
           alive (count (filter state/active? bs))]
       (case kind
         "cull"
         ;; What the directive would actually kill, not just how many are
         ;; alive. An UNTARGETED cull matches every branch, so on a beam of
         ;; three it emptied the run outright — the guard below only ever
         ;; caught the alive=1 case, and the outcome it exists to prevent
         ;; (ending the run in a way the person almost certainly did not
         ;; intend) arrived by the other door. Refuse whenever nothing would
         ;; be left running, and say which case it was.
         (let [doomed (count (filter #(and (matches? %) (state/active? %)) bs))]
           (if (>= doomed alive)
             (do (interventions/resolve!
                  conn run-id (:id d) :rejected
                  (prompt/render "directive-refused"
                    {:last-branch (<= alive 1) :alive alive})
                  turn)
                 acc)
             (do (interventions/resolve! conn run-id (:id d) :applied nil turn)
                 (assoc acc :branches
                        (mapv #(if (and (matches? %) (state/active? %))
                                 (assoc % :status :culled
                                        :inactive-reason "culled by a human")
                                 %)
                              bs)))))

         "retract"
         ;; Applied here rather than at submit time so it lands on a turn
         ;; boundary like every other directive — a branch mid-turn is holding
         ;; a ledger it read before the change, and rewriting under it would
         ;; make the two disagree for exactly one turn.
         (let [payload (directive-payload d)
               aid (or (:artifact_id payload) (:artifact-id payload))]
           (if (and aid (artifacts/retract! conn run-id aid
                                            (or (:reason payload) "retracted by a human")))
             (interventions/resolve! conn run-id (:id d) :applied nil turn)
             (rejected conn run-id d turn
                       {:retract-unknown true :artifact-id (pr-str aid)}))
           acc)

         ("message" "review")
         (do (interventions/resolve! conn run-id (:id d) :applied nil turn)
             ;; Delivered as a directive on the branch, which the arbiter puts
             ;; at priority zero — above every machine gate. :payload-text is
             ;; the parsed human words; the raw column is a JSON blob and the
             ;; gate rendered it verbatim (blt.38).
             (let [d' (assoc d :payload-text
                             (let [payload (directive-payload d)]
                               (or (:text payload)
                                   (when (string? payload) payload))))]
               (assoc acc :branches
                      (mapv #(if (matches? %) (assoc % :pending-directive d') %) bs))))

         "fork"
         ;; A fork needs no scheduler machinery of its own: `:beam/spawn`
         ;; already turns a branch's `:pending-branch-theses` into siblings
         ;; under the total cap, and it runs after the cull in the same round.
         ;; So a human's fork is the same object a branch's own
         ;; `branch_theses` call produces, and it inherits the cap, the
         ;; parent's conversation and the `:forked-at` stamp for free.
         (let [payload (directive-payload d)
               thesis (or (:thesis payload) (:goal payload))
               parents (filter #(and (matches? %) (state/active? %)) bs)]
           (if-not (and (seq (str thesis)) (seq parents))
             (do (rejected conn run-id d turn
                           (if (seq parents)
                             {:fork-no-thesis true}
                             {:fork-no-branch true}))
                 acc)
             (do (interventions/resolve! conn run-id (:id d) :applied nil turn)
                 ;; The FIRST matching parent only. An untargeted fork across
                 ;; five branches would spend the whole branch budget on one
                 ;; click, and the cap would then silently decide which
                 ;; children survived — a person asking for a fork is asking
                 ;; for a fork.
                 (let [parent (first parents)]
                   (assoc acc :branches
                          (mapv #(if (= (:id %) (:id parent))
                                   (update % :pending-branch-theses
                                           (fnil conj []) {:goal (str thesis)
                                                           :from-human? true})
                                   %)
                                bs))))))

         "extend"
         ;; Raise the turn cap for the rest of the run. Carried in the round's
         ;; data map rather than written to the run row: the cap is what THIS
         ;; scheduler loop compares against, a resume re-reads its budget from
         ;; the control API anyway, and a row that disagreed with the live
         ;; value would be the worse of the two to have.
         (let [payload (directive-payload d)
               by (or (:turns payload) (:by payload) (:max_turns payload))
               n (when (number? by) (long by))]
           (if-not (and n (pos? n))
             (do (rejected conn run-id d turn {:extend-no-turns true})
                 acc)
             (do (interventions/resolve! conn run-id (:id d) :applied nil turn)
                 (update acc :max-turns (fnil + 0) n))))

         ("pause" "resume")
         ;; Run-level and last-writer-wins: two pauses are one pause, and a
         ;; resume in the same drain as a pause means the person changed their
         ;; mind before the round started. The scheduler reads this at the top
         ;; of the next round, so in-flight turns finish — which is what
         ;; `interventions/kinds` promises.
         (do (interventions/resolve! conn run-id (:id d) :applied nil turn)
             (assoc acc :paused? (= "pause" kind)))

         (do (rejected conn run-id d turn {:unknown-kind true :kind kind})
             acc))))
   {:branches branches :max-turns nil :paused? nil}
   directives))

(defn advance-branch
  "One turn for one branch.

  The seam where the two drivers became one. The beam used to call
  branch-loop/run-turn directly while the manifest driver ran the same steps
  as mycelium cells, and nothing in the production path ever reached the
  manifest — `:run :loop` was documented, parsed, and read by no live code, so
  the critic, feature, team and decompose loops existed only under the test
  suite. The beam now drives the run's compiled PER-TURN manifest slice
  (workflow/turn-manifest), which for the factory loop composes exactly the
  steps run-turn composed.

  There is no fallback any more, and that is the point. This used to drop to
  branch-loop/run-turn when the ctx carried no workflow, which meant a SECOND
  composition of a turn existed in compiled code — the very shape whose last
  appearance is described above. Its stated justification (the benches) had
  outlived the benches: there is no bench directory, and both production paths
  always set :turn-workflow, so the fallback was dead in production and alive
  only for the tests. A path production never exercises is exactly how a
  duplicate rots unnoticed. `workflow/run-turn` is the one composition now, and
  it is the manifest.

  A structural failure is this branch's problem, not the beam's: it abandons
  the branch with the reason, the same shape as a throw. The manifest driver
  threw here and ended the whole run, which is right for a single-branch
  driver and wrong for one branch of five."
  [ctx b turn]
  (if-let [wf (:turn-workflow ctx)]
    (let [data (workflow/note-schema-warnings!
                ctx (myc/run-compiled wf ctx {:branch b :turn turn}))
          fail (fn [why]
                 (log/warn "branch" (:id b) "turn" turn "loop workflow failed:" why)
                 (assoc b :status :abandoned
                        :inactive-reason (str "loop workflow failed: " why)))]
      (cond
        (myc/error? data) (fail (pr-str (myc/workflow-error data)))
        ;; A turn that came back carrying no branch is a manifest whose slice
        ;; dropped the key — an edit the compile cannot catch, since :branch is
        ;; data rather than structure. Abandoning ONE branch with a legible
        ;; reason beats returning nil into the scheduler, where it would
        ;; surface several rounds later as an NPE on somebody else's turn.
        (nil? (:branch data)) (fail "the turn returned no :branch")
        :else (:branch data)))
    ;; Loud rather than a quiet second path: every caller that reaches here in
    ;; production sets this, so its absence is a wiring bug and not a mode.
    (throw (ex-info "the beam was handed no :turn-workflow — a turn is defined by a manifest, and the scheduler cannot advance a branch without one"
                    {:branch (:id b) :turn turn}))))

(defn advance-all
  "One turn for every active branch, concurrently, each under a hard deadline.

  MECHANISM. Both failure paths here are fault handling and neither is a
  verdict: a branch that throws is abandoned rather than taking the beam down
  with it, and a branch that hangs forfeits its turn. Crucially neither touches
  the counters the retention policy reads — the branch did not get an answer to
  be wrong about, so charging it a verification failure would be the vf-jki
  mistake in a new place. Phase 1 proved five concurrent swipl
  sessions hold, which is what makes this real parallelism rather than a loop
  wearing futures.

  The deadline is skipped for a NON-ITERATING manifest (team, feature,
  decompose): there a `turn` is the branch's entire job rather than one model
  call, so the turn deadline would abandon the run partway through the work it
  was asked to do. Those runs stop by the abort flag, which is the stop path
  that never depended on cooperation anyway."
  [ctx branches turn]
  (let [deadline (when (get ctx :iterating-loop? true)
                   (or (:turn-deadline-ms ctx) (turn-deadline-ms)))
        ;; {branch-id future} of turns that blew their deadline and are STILL
        ;; executing. A forfeited turn's thread cannot be interrupted (and
        ;; killing it mid-journal-write would be worse), but it shares the
        ;; branch's eval session and journals under its id — so advancing the
        ;; same branch again while it runs interleaved two turns of one branch
        ;; and made the journal diverge from the live state
        ;; (karamazov-blt.18). The branch forfeits again instead, until the
        ;; dangling call completes; the wait is bounded by the provider socket
        ;; timeout and the tool timeouts inside the turn.
        in-flight (:in-flight ctx)
        forfeit (fn [b]
                  (-> b
                      (state/add-message
                       "user"
                       (str "[harness] " (prompt/render "turn-deadline"
                                           {:seconds (quot (or deadline 0) 1000)})))
                      (update :timeouts (fnil inc 0))))
        pending (mapv (fn [b]
                        (let [dangling (when in-flight (get @in-flight (:id b)))]
                          (cond
                            (and dangling (not (realized? dangling)))
                            [b ::still-dangling]

                            :else
                            (do (when dangling (swap! in-flight dissoc (:id b)))
                                [b (future
                                     (try
                                       (advance-branch ctx b turn)
                                       (catch Throwable e
                                         (log/warn "branch" (:id b) "died on turn" turn
                                                   ":" (ex-message e))
                                         (assoc b :status :abandoned
                                                :inactive-reason
                                                (str "branch error: " (ex-message e))))))]))))
                      branches)]
    (mapv (fn [[b fut]]
            (if (= ::still-dangling fut)
              (do (log/warn "branch" (:id b) "still executing a forfeited turn;"
                            "skipping turn" turn "to keep its turns serial")
                  (forfeit b))
              (let [r (if deadline (deref fut deadline ::timeout) @fut)]
                (if (= ::timeout r)
                  (do (log/warn "branch" (:id b) "exceeded the turn deadline on turn" turn)
                      ;; Not a verification failure: the branch did not get an
                      ;; answer to be wrong about. It loses the turn and is told
                      ;; so; the dangling call is REMEMBERED so the next round
                      ;; does not run beside it.
                      (when in-flight (swap! in-flight assoc (:id b) fut))
                      (forfeit b))
                  r))))
          pending)))

(defn dispose-branch-engines!
  "Release one branch's external sessions.

  The proof engines this existed for are gone and it was a no-op seam for a
  while — RFC-006 listed it as a gap and named what would fill it. What
  filled it is the eval session, which is now per BRANCH rather than per run.

  That change is an isolation fix as much as a disposal one. One namespace
  per run meant five competing branches shared one set of defs: a helper B1
  defined was callable from B2, which had never defined it and whose own
  history did not mention it — so B2 could work for reasons invisible in its
  transcript, and stop working on a replay that did not include B1. The beam's
  premise is that branches are independent lines of inquiry.

  clojure-lsp is deliberately NOT disposed here: its clients are keyed by
  ROOT, every branch of a run shares one root, and one server per root is
  correct. `system/stop!` sweeps those.

  Safe to call twice by contract — `repl/close-session` is idempotent on an
  unknown name — and the run-end teardown still sweeps every branch in case
  one never reached this path."
  [b]
  (when-let [session (:repl-session b)]
    (try (repl/close-session session)
         (catch Throwable e
           (log/warn "closing branch" (:id b) "eval session failed:"
                     (ex-message e)))))
  nil)

(defn record-inactive!
  "Write the ending of every branch in `branches` that is no longer active.

  One place, keyed on `state/active?` alone, because splitting this by
  outcome is what let a shipped branch fall between two loops: `done` leaves
  {:status :done :final-answer ...}, which is neither `active?` (so the
  run-end loop skipped it) nor answer-free (so the cull loop skipped it),
  and the branch stayed 'active' in the record for the rest of time. The
  status written is the branch's own, so a cull reads culled and a ship
  reads done.

  Engines are released here rather than at run end. A Lean session holds
  ~0.83GB, so a branch that stopped at turn 10 of 80 sat on that for the
  other 70 turns. The run-end `finally` still covers everything, including
  branches still alive; this only stops a finished one holding memory it
  can no longer use."
  [{:keys [conn run-id]} branches]
  (doseq [b branches :when (not (state/active? b))]
    (runs/close-branch! conn run-id (:id b) (:status b) (:inactive-reason b))
    (dispose-branch-engines! b)))

(defn finish-now?
  "Should a shipped branch end the run? Returns the winning branch, or nil to
  keep exploring.

  Winner-takes-all is right for a question with one answer and wrong for a
  research campaign: the first branch to clear the bar terminates every other
  line, so the run returns the cheapest qualifying result rather than the best
  one. With `:stop-on-first-done?` false a shipped branch goes inactive
  holding its answer and the rest keep working; the run ends when nobody is
  left to explore, and select-done-branch ranks every finished branch on the
  evidence it carries."
  [ctx done-branch branches]
  (when done-branch
    (if (get-in ctx [:config :run :stop-on-first-done?] true)
      done-branch
      (when-not (some #(and (state/active? %) (not (:final-answer %))) branches)
        done-branch))))

(defn select-done-branch
  "The winner among branches that landed :final-answer this round.

  The choice is mechanical — state/rank-finished over the engine-audited
  evidence each branch carries — so no model sits in the path where UCLA
  needed an LLM selector. When more than one branch is eligible the choice
  is journalled with each candidate's id and ranking key plus the winner,
  so it is auditable from the run record. A single candidate is today's
  behavior and journals nothing."
  [{:keys [conn run-id]} candidates]
  (let [winner (first (state/rank-finished candidates))]
    (when (and conn run-id (< 1 (count candidates)))
      (journal/note! conn run-id :candidate-selection
                     {:data {:candidates (mapv (fn [b]
                                                 {:branch-id (:id b)
                                                  :key (state/finished-key b)})
                                               candidates)
                             :winner (:id winner)}}))
    winner))

(def beam-manifest-name
  "The scheduler manifest a run drives. Config :run :beam overrides it, the
  same way :run :loop chooses the per-branch turn manifest — the two are
  different axes, and a project may well want to pair a custom scheduler with
  the factory turn or the other way round."
  "beam")

(defn- beam-manifest
  "The compiled round manifest for this run.

  Loaded through the userspace store like every other manifest, so a project
  that has evolved its scheduler drives its own version. Compiled fresh per
  run: a cell edit committed mid-campaign takes effect on the next run, which
  is the same contract the turn manifest has."
  [conn config]
  (:compiled (workflow/load-loop! conn (or (get-in config [:run :beam])
                                          beam-manifest-name))))

(defn- unwrap-round-error
  "The exception a cell actually threw, out of mycelium's execution wrapper.

  Two reasons this matters, and both bit on the first manifest-driven run.
  A caller of `run!` used to see the branch's own exception; wrapped, every
  failure became the same opaque \"execution error\", which is a worse report
  and breaks anything matching on the cause. And mycelium's wrapper carries the
  ENTIRE compiled FSM in its ex-data — pr-str'ing that into the journal writes
  a row the size of the workflow for every crash.

  So: unwrap for both the record and the rethrow, and record only the small
  keys. The failing NODE is worth keeping, because \"which step of the round
  died\" is the first thing anyone asks."
  [e]
  ;; Unwrapped to the INNERMOST cell error, not one layer: a nested workflow
  ;; (the round drives a turn manifest per branch) wraps once per level, and
  ;; peeling a single layer still reports "execution error" from the level
  ;; above.
  (loop [cur e, node nil, depth 0]
    (let [d (ex-data cur)
          inner (:error d)
          node (or (:last-state-id d) node)]
      (if (and (instance? Throwable inner) (< depth 8))
        (recur inner node (inc depth))
        {:throwable cur :node node}))))

(defn- oversight-stream
  "Start the supervisor's parallel stream over this run.

  MECHANISM ONLY. When a pass happens is `oversight/start!`; what a pass IS is
  manifests/oversight.edn; whether it happens at all and how often is the
  `:oversight` policy in gates.edn. This function knows none of it — it hands
  the driver a thunk and gets a stop function back.

  The stream carries its branch across passes, which is what gives the
  supervisor one continuous memory of the run instead of a cold re-read every
  time (see cells/oversight.clj :oversight/reason)."
  [{:keys [conn run-id llm-adapter] :as ctx}]
  (let [p (lexicon/policy :oversight)]
    (if-not (and conn run-id llm-adapter (:enabled? p))
      (constantly nil)
      (oversight/start!
       (assoc ctx :enabled? true
              :every-ms (:every-ms p) :budget (:budget p) :poll-ms (:poll-ms p))
       (fn [pass-ctx]
         (let [out (myc/run-compiled (workflow/compiled-manifest "oversight")
                                     pass-ctx {:oversight/carry (:carry pass-ctx)})]
           {;; The carry: the supervisor's branch, so the next pass continues
            ;; the same conversation rather than starting one. A QUIET pass
            ;; produces no branch, and returning its nil wiped the carry — so
            ;; a healthy stretch erased the supervisor's memory of the run and
            ;; the next reasoning pass started cold, which is the one thing
            ;; the stream exists to avoid.
            :carry (or (:oversight/branch out) (:carry pass-ctx))
            ;; And a quiet pass made no model call, so it must not spend the
            ;; budget that exists to bound model calls (karamazov-808).
            :spent? (boolean (:oversight/worth-a-look? out))}))))))

(defn run-rounds
  "Drive the beam's scheduler manifest from round `start-turn`.

  THE ROUND IS NOT A LOOP HERE ANY MORE. It is manifests/beam.edn — advance,
  score, cull, settle, repopulate, spawn, tick, with a back edge — and this
  function is the driver: it compiles that manifest, hands it the branches and
  the run context, and owns the two things a manifest cannot own.

  The first is the CRASH RECORD. gen-11 threw inside the round and the
  exception went to the process's stdout — a tty — and nowhere else. The row
  stayed 'running' with ended_at NULL, so the API, the GUI and every query
  agreed the run was alive for the nine hours it had been dead. A crash that
  leaves no trace in the record is indistinguishable from a slow round, which
  makes the whole journal untrustworthy as a liveness signal. Recorded then
  rethrown, and the recording is best-effort: a failure to journal the failure
  must not replace it with a different one.

  The second is TEARDOWN, which must not depend on the round's cooperation —
  the RAX-manager principle. `live-branches` is the driver's window into a run
  that may be mid-round when it dies; the advance and tick cells keep it
  current, because a thrown manifest hands nothing back.

  `max-turns` comes from the ctx, which a resume builds from the runs row — the
  ORIGINAL budget — so starting at turn N+1 of M is what keeps a crash from
  re-granting the N turns before it. Returns {:status :run-id :branches …}."
  [{:keys [conn run-id config repl-session] :as ctx} branches start-turn]
  (let [live-branches (atom branches)
        ;; The watcher starts with the round and stops in the finally below,
        ;; however the run ends. It observes and submits directives through the
        ;; interventions queue; it never touches a branch, so it cannot race
        ;; the round it is watching.
        ctx (assoc ctx :live-branches live-branches)
        _ (session/mark-run! run-id)
        ctx (assoc ctx :stop-watch (watch/start! ctx))
        ;; THE SUPERVISOR STREAM, beside the run rather than inside it. The
        ;; watcher above is the reflex — rule-based, cheap, steering only. This
        ;; is the deliberate one: it runs the supervisor role over the run's
        ;; health and may tune the harness as well as steer the run.
        ;;
        ;; Started here for the same reason the watcher is: a supervisor wired
        ;; as a node in the workflow it supervises only runs where that
        ;; workflow puts it, and `:feature/supervise` sits after the implement
        ;; stage RETURNS. Runs fps5 and fps6 both ended having never reached
        ;; it, because the implementer stalled and never returned — the
        ;; watchdog was downstream of the thing it watches for.
        ctx (assoc ctx :stop-oversight (oversight-stream ctx))]
    (try
      (let [data (myc/run-compiled (beam-manifest conn config) ctx
                                   {:branches branches :turn start-turn})]
        (when (myc/error? data)
          ;; A structural failure in the scheduler is a harness bug, not a run
          ;; outcome; surface it rather than reporting a half-closed run.
          (throw (ex-info "the beam scheduler failed structurally"
                          {:run-id run-id :error (myc/workflow-error data)})))
        (or (:result data)
            (throw (ex-info "the beam scheduler ended without a result"
                            {:run-id run-id :status (:status data)}))))
      (catch Throwable e
        (let [{:keys [throwable node]} (unwrap-round-error e)]
          (try
            (journal/note! conn run-id :run-error
                           {:data {:error (ex-message throwable)
                                   ;; jolt's Throwable has an empty stack trace,
                                   ;; so the type and the failing node are all
                                   ;; there is. NOT the wrapper's ex-data: it
                                   ;; holds the whole compiled FSM.
                                   :type (some-> (:via (Throwable->map throwable))
                                                 first :type str)
                                   :node (some-> node str)
                                   :ex-data (some-> (ex-data throwable) pr-str)}})
            (runs/finish-run! conn run-id :failed nil)
            (catch Throwable _ nil))
          ;; Rethrow what the cell threw, not the wrapper: the callers of run!
          ;; were written against the branch's own exception.
          (throw throwable)))
      (finally
        ;; The watcher stops with the run, however the run ended.
        (when-let [stop (:stop-watch ctx)] (stop))
        ;; The supervisor stream stops with the run, however the run ended.
        (when-let [stop (:stop-oversight ctx)] (stop))
        ;; SHORT-TERM BECOMES LONG-TERM. The session tally dies with the
        ;; process; a pattern that held across the run is a candidate for
        ;; something the next run should start out knowing, and this is the
        ;; only moment at which the whole run has happened and the tally is
        ;; still there to read.
        ;;
        ;; Best effort and last: a failure to remember must never be able to
        ;; turn a finished run into a failed one.
        (try
          (let [{:keys [findings verdicts]}
                (knowledge/distil-session! conn {:run-id run-id
                                                 :findings (session/findings
                                                            ;; THIS RUN's window, not the
                                                            ;; whole-process tally: counters
                                                            ;; never reset between runs, so
                                                            ;; run 1's parse-error rate kept
                                                            ;; "corroborating" a finding at
                                                            ;; the end of clean runs 2..n,
                                                            ;; each with a distinct run-id
                                                            ;; that defeated the guard
                                                            ;; (karamazov-blt.24).
                                                            (session/run-window run-id))
                                                 :experiments (session/experiments)})]
            (when (or (seq findings) (seq verdicts))
              (log/info "distilled" (count findings) "finding(s) and"
                        (count verdicts) "verdict(s) into memory")))
          (catch Throwable e
            (log/warn "distilling the session failed:" (ex-message e))))
        ;; The proof engines' scheduler- and tool-opened session registries
        ;; are gone with the engines (karamazov-w7w); what a branch holds now
        ;; — its eval session — is disposed per branch, no matter how the
        ;; run ended.
        (doseq [b @live-branches] (dispose-branch-engines! b))
        ;; The run's eval namespace does not outlive the run
        ;; (provenance CR1-6). Best effort, and only for a session this
        ;; ctx actually carries: a resume that entered here without one has
        ;; nothing to close.
        (when repl-session
          (try (repl/close-session repl-session)
               (catch Throwable e
                 (log/warn "closing the run's eval session failed:" (ex-message e)))))
        ;; And the project image, which is a PROCESS rather than a namespace:
        ;; closing the session leaves it running, holding a port and a sandbox
        ;; for the life of the harness.
        (try (route/release! (:root ctx))
             (catch Throwable e
               (log/warn "stopping the project image failed:" (ex-message e))))))))

(defn run!
  "Run a beam to completion.

  Returns {:status :answer :run-id :branches :residuals}. The first branch to
  land a `done` wins and the rest are abandoned, since paying for four more
  provider calls after the answer exists is pure waste."
  [{:keys [conn config llm-adapter llm-config problem max-turns beam-width
           abort on-start seed-run quarantine] :as opts}]
  (let [max-turns (or max-turns (get-in config [:run :max-turns]) 40)
        ;; Which loop drives this run, compiled to its per-turn slice. Before
        ;; on-start, and so before POST /v1/runs returns, because the run row
        ;; records the width this decides and a compile failure must refuse
        ;; the request rather than surface as a dead run. It costs one cell
        ;; load and one manifest compile — well under the open-branch! cost
        ;; the comment below is about.
        ;; A run that named no workflow gets one chosen for it from the
        ;; catalogue (samizdat.agent.select). Before the compile, because the
        ;; choice decides which manifest is compiled; after nothing else,
        ;; because it is one small model call and a run must not fail to start
        ;; over it — `pick!` answers nil on every uncertainty and the
        ;; precedence in `active-loop-name` falls back to the factory loop.
        selected (select/pick! {:conn conn :llm-adapter llm-adapter
                                :llm-config llm-config}
                               problem)
        loop-nm (workflow/active-loop-name config selected)
        ;; THE IMPLEMENTER'S STREAM (RFC-012). Every cell that completes is
        ;; published as a step, onto the same bus the journal already uses.
        ;; The id is an atom because this compile happens BEFORE the run row
        ;; exists — the row records a width this compile decides — and
        ;; :on-trace is only accepted here.
        run-id* (atom nil)
        {loop-version :version turn-wf :compiled iterating? :iterating?}
        (workflow/compile-turn-loop conn loop-nm
                                    {:on-trace (events/tracer run-id*)})
        ;; A non-iterating manifest (team, feature, decompose) is a whole-run
        ;; workflow: one "turn" is the branch's entire job, and it fans out
        ;; internally. Running five of those concurrently would multiply the
        ;; whole job rather than explore five lines of one, so the beam is
        ;; width 1 there regardless of what was asked for.
        requested-width (or beam-width (get-in config [:run :beam-width]) 5)
        width (if iterating? requested-width 1)
        ;; Seeding forces sharing on for this run regardless of the config
        ;; flag: seeds enter through the shared log's context blocks, and
        ;; seeds nobody reads would be dead rows.
        config (cond-> config
                 seed-run (assoc-in [:run :share-artifacts?] true))
        run-id (runs/start-run! conn {:problem problem
                                      :provider (:provider llm-config)
                                      :model (:model llm-config)
                                      :max-turns max-turns
                                      :beam-width width
                                      :prompt-digest (branch-loop/prompt-digest)})
        ;; The tracer's steps can now say which run they belong to; the bus is
        ;; process-wide and the watcher filters on it.
        _ (reset! run-id* run-id)
        ;; Seeded before any branch opens, so the first context block a
        ;; branch ever sees can already carry inherited lemmas.
        ;; `quarantine` drops named claims from the inheritance: a row still
        ;; marked confirmed that the harness has since learned was not.
        _ (when seed-run (artifacts/seed-from-run! conn run-id seed-run
                                                   {:quarantine quarantine}))
        ;; Every session ever opened, including forked children, so the
        ;; supervisor can tear them all down regardless of how the run ended.
        ;; The three ctx keys the manifest driver set and this one did not.
        ;; Every consumer defends with `(or root ".")` or a nil check, so the
        ;; omission was silent: `:run :root` was documented and ignored, every
        ;; run's file tools resolved against the serve process's cwd, and
        ;; `eval` fell through to repl/default-session — one process-wide
        ;; namespace, never closed, shared by every run on the box. That is
        ;; the leak provenance CR1-6 fixed on the other driver only.
        root (or (get-in config [:run :root]) (System/getProperty "user.dir"))
        ;; Make the project's own namespaces requirable from `eval` before any
        ;; branch takes a turn. The system prompt's whole first section is
        ;; REPL-first against the project under work, and without this that
        ;; instruction is unreachable the moment :run :root is not the harness.
        _ (repl/ensure-project-roots! root)
        ctx {:conn conn :run-id run-id :config config :problem problem
             :llm-adapter llm-adapter :llm-config llm-config
             :max-turns max-turns :beam? (> width 1) :beam-width width
             :root root
             ;; The compiled per-turn manifest advance-branch drives, and
             ;; whether it is a per-turn loop at all (which decides the turn
             ;; deadline; see advance-all).
             :turn-workflow turn-wf
             :iterating-loop? iterating?
             ;; What this run changed, for the ship gate's focused verify and
             ;; for a finalization critic reading the run's own diff.
             :git-baseline (gitdiff/baseline root)
             ;; One eval namespace per RUN: defs the agent makes with `eval`
             ;; persist across its turns and die with the run. run-rounds
             ;; closes it in the same finally that disposes the sessions.
             :repl-session (repl/new-session)
             ;; {branch-id future} of forfeited turns still executing, so
             ;; advance-all never runs a branch beside its own dangling turn
             ;; (karamazov-blt.18).
             :in-flight (atom {})
             :abort abort}]
    ;; Before the branches, not after. api.control/start-run! blocks until this
    ;; fires, so this line is how long POST /v1/runs takes — and open-branch!
    ;; spawns a Prolog session per branch, so putting it after made the endpoint
    ;; cost the whole beam's startup: 47ms idle, 21095ms under load at width 1,
    ;; proportionally worse wider. A client bound tighter than that reported a
    ;; failure for a run already committed and running (vf-36o).
    ;;
    ;; The run row exists by here, which is what the id addresses. A caller that
    ;; fetches run-detail immediately sees zero branches for a moment; the
    ;; journal poller handles that, and it is the honest picture — the branches
    ;; genuinely do not exist yet.
    (when on-start (on-start run-id))
    ;; Which loop drove this run, durably: an agent reading a surprising run
    ;; back needs to know which version of itself produced it.
    (journal/note! conn run-id :loop-workflow
                   {:data {:name loop-nm :version loop-version
                           :iterating? iterating?
                           ;; How this run came to be driven by that manifest:
                           ;; the caller pinned it, selection chose it, or
                           ;; nothing did and it is the factory default. A run
                           ;; read back later is otherwise indistinguishable
                           ;; from one somebody configured by hand.
                           :chosen-by (cond (get-in config [:run :loop]) "config"
                                            (= selected loop-nm) "selection"
                                            :else "default")
                           :beam-width width
                           :requested-beam-width requested-width}})
    (when (not= width requested-width)
      (log/info "loop" loop-nm "is a whole-run workflow; beam width forced to 1"
                "(asked for" (str requested-width ")")))
    (let [initial (mapv #(open-branch! ctx (str "B" (inc %)) nil nil 0) (range width))
          result (try (run-rounds ctx initial 1)
                      (catch Throwable e
                        ;; A crash is an outcome too: a workflow that crashes
                        ;; five times showing "no runs" in the selection
                        ;; history taught the chooser nothing (blt.38).
                        (try (knowledge/record-workflow-outcome!
                              conn {:workflow loop-nm :run-id run-id
                                    :shipped? false})
                             (userspace/record-run-outcome! false)
                             (catch Throwable _ nil))
                        (throw e)))]
      ;; HOW THIS WORKFLOW WENT, for the next run's choice. A run only ever
      ;; sees its own attempt, so `direct attempts on this project keep getting
      ;; stuck` is not something any single run can notice — it has to be
      ;; written down. samizdat.agent.select reads it back.
      ;;
      ;; Here rather than in run-rounds' finally because this is the only place
      ;; that knows both which manifest drove the run and whether it shipped.
      (knowledge/record-workflow-outcome!
       conn {:workflow loop-nm :run-id run-id
             :shipped? (boolean (:answer result))})
      ;; The same ending, stamped onto the project-authored userspace versions
      ;; that were current for it — the standing the versions listing shows a
      ;; later supervisor weighing an unfamiliar edit (karamazov-c58).
      (userspace/record-run-outcome! (boolean (:answer result)))
      result)))

(defn summary
  "One line per branch, for logs and the run response."
  [{:keys [branches residuals]}]
  (str/join "\n" (concat (map state/describe branches)
                         (keep state/render-residual residuals))))
