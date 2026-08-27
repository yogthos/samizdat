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

(ns samizdat.agent.state
  "Branch and run state held in memory during a turn.

  SQLite is the durable record; this is the working copy the loop reads
  between appends. Anything a gate needs to decide has to be here, and
  everything here is also journalled, so a resumed run rebuilds it by replay
  rather than by trusting a snapshot.

  A branch is a map, not an object. The engine session and the message history
  are the only parts that cannot be reconstructed from the journal, and the
  session carries its own replay log (see engine/prolog.clj)."
  (:require [clojure.set]
            [clojure.string :as str]
            [samizdat.agent.phases :as phases]
            [samizdat.lexicon :as lexicon]
            [samizdat.prompt :as prompt]
            [samizdat.tape :as tape]
            [samizdat.util :as util]))

(defn new-branch
  [{:keys [id parent-id problem prolog messages created-at-turn]}]
  {:id id
   :parent-id parent-id
   :problem problem
   :status :active
   :inactive-reason nil
   :created-at-turn (or created-at-turn 0)
   :prolog prolog
   :messages (or messages [])
   :turns []
   ;; Artifacts this branch produced, newest last. Mirrors the artifacts table.
   :artifacts []
   ;; Consecutive failed verifications; reset by any success.
   :consecutive-failures 0
   ;; Consecutive turns that produced no usable tool call, cleared by any
   ;; well-formed one. Kept apart from :consecutive-failures because a
   ;; malformed fence says nothing about whether the branch's line of inquiry
   ;; is any good, and the cull gate reads that counter as if it did.
   :consecutive-mechanics-failures 0
   ;; The subset of the mechanics tally that were calls the harness declined
   ;; on phase policy (vf-b25/vf-eaw) — perfectly well-formed calls, so the
   ;; cull record must be able to tell them from malformed fences, or the
   ;; reason string lies in the permanent record (gen-30 B3.2 died with
   ;; exactly that false reason).
   :consecutive-policy-refusals 0
   ;; Turns since the last progress event. See gates/progress-stalled?.
   :turns-since-progress 0
   ;; Whether this branch has ever produced anything at all. The stall counter
   ;; arms only after the first progress event, so a branch that produced
   ;; nothing needs a separate bound — dirge PR 739's exploration prologue.
   :any-progress? false
   :thesis nil
   :last-review nil
   :last-audit nil
   ;; Tool-call mechanics only, for the capability tier. Never verification or
   ;; progress signals: a signal may tune a guard that fires on the same thing
   ;; the signal measures (dirge PR 740).
   :mechanics {:calls 0 :parse-errors 0 :auto-repairs 0
               :unknown-tools 0 :truncations 0 :multi-fences 0}
  ;; The draft/commit split (vf-b25): :explore until the cap, :build
  ;; after — the phase-valve message tells the branch why. Withholding
  ;; here is the harness's one reliably-working gate: explore cannot
  ;; become an endless planning loop. The phase names themselves are
  ;; phases.edn data (drg-4026 #34).
  :phase (phases/initial-phase)
   ;; The turn the CURRENT phase began, not when the branch did. Starts at
   ;; branch creation so a forked branch gets a full explore budget instead
   ;; of inheriting its parent's spent one; reenter-explore moves it.
   :phase-entered-turn (or created-at-turn 0)
   ;; The claim of the last verification that FAILED, and the tool it failed
   ;; on. What the stuck gate names when it withholds an approach (vf-9wx),
   ;; and how it decides whether a Lean sketch is a move this branch can
   ;; actually make.
   :last-failed-claim nil
   :last-failed-tool nil
   ;; Reflexion log (karamazov-ioo.625): every claim the harness has withheld
   ;; or reframed, newest last, bounded to the last 5. :last-failed-claim is
   ;; cleared by any success; this is not — a branch that abandoned A, made
   ;; progress, and is now stuck should still be told A is a dead end.
   :abandoned []
   ;; The forced reframe: the approach the harness has told this branch to
   ;; abandon, and the turn it said so. While these are set and inside
   ;; :reframe-grace, re-verifying that approach is refused and the branch is
   ;; not culled for the failures that caused the reframe.
   :reframe-claim nil
   :reframe-entered-turn nil
   ;; Gate firings awaiting settlement, as {:id :gate :prediction :window :turn}
   :open-predictions []
   ;; Verification tiers seen. :fast is a one-shot check, :slow is a
   ;; cross-checked template or a review-plus-audit pass.
   :tiers-seen #{}
   ;; The ONE task this branch is working on, as {:id :title}, or nil. Set by
   ;; `task claim`, cleared when that task closes. One rather than many so that
   ;; "until it is done" means something: a branch holding three tasks is a
   ;; branch that has told you nothing about what it is doing.
   :task nil
   :final-answer nil})

(defn fork-branch
  "A child of `parent` carrying its CONVERSATION — the fork llm-repl's tape
  makes cheap, and the one samizdat was not taking.

  Every child used to open on `initial-messages problem`: a fresh two-message
  tape, so a fork discarded everything its parent had learned and re-derived
  it from the problem statement. The parent's own docstring for the crossover
  block already assumed otherwise ('the child already carries them in its
  inherited history'). Now it does.

  What is INHERITED is the conversation: the messages up to `depth` (nil ≡ all
  of them), and the slice of the turn log those messages cover, so compaction
  and the turn-window predicates still have a history to read. What is NOT
  inherited is every gate counter — consecutive failures, the mechanics tally,
  the stall clock, the phase, the artifacts, the abandoned log. A child gets a
  clean slate as a BRANCH while carrying the conversation, because the
  counters are a record of how the PARENT was doing and culling a newborn for
  its parent's failures is the mistake the reprieve machinery exists to
  prevent.

  `:turns` is RE-DERIVED from the inherited tape rather than copied, the way
  llm-repl re-derives its turn count: truncating a copy drops assistant turns,
  and the tape is the ground truth the counter has to agree with. `:forked-at`
  records the branch point, without which an `{:at N}` fork's tree edge is
  lossy — the depth is the only thing that says where the child left the
  parent's line."
  [parent {:keys [id depth turn thesis problem]}]
  (let [messages (tape/truncate-at (:messages parent) depth)
        ;; The turn records the inherited messages actually cover — matched by
        ;; the stamps add-message writes, not by position. A turn whose
        ;; messages were truncated away is not this child's history.
        kept-turns (let [stamped (set (keep :turn messages))]
                     (if (seq stamped)
                       (vec (filter #(contains? stamped (:turn %)) (:turns parent)))
                       []))]
    (assoc (new-branch {:id id
                        :parent-id (:id parent)
                        :problem (or problem (:problem parent))
                        :created-at-turn turn
                        :messages messages})
           :turns kept-turns
           :forked-at (count messages)
           :thesis thesis)))

(defn active? [branch] (= :active (:status branch)))

(defn turn-count
  "How far into the RUN this branch is, in GLOBAL turns — the unit
  `max-turns`, artifact `:turn` stamps and gate-history stamps are all
  expressed in. Served from `:current-turn`, which the loop stamps at the top
  of every turn; the length of the branch's own log is the fallback for a
  branch no driver has touched (a fresh test branch), and the WRONG answer
  for budget arithmetic on a live one — a fork's log starts nearly empty and
  no-call turns append nothing (karamazov-blt.16)."
  [branch]
  (or (:current-turn branch) (count (:turns branch))))

(defn own-turn-count
  "How many turns THIS branch has itself taken — its experience, not its
  position in the run's budget. What juvenile-grace and the prologue cap
  mean by a turn: a fork born at round 18 is 0 turns OLD while being 18
  turns IN."
  [branch]
  (count (:turns branch)))

(defn confirmed-artifacts [branch]
  (filter #(= :confirmed (:claim-status %)) (:artifacts branch)))

(defn has-confirmed? [branch]
  (boolean (seq (confirmed-artifacts branch))))

(defn empirical-artifacts
  "Measurements this branch banked: a value an engine computed, recorded for
  what it is. Never confirmations — nothing was decided — but not nothing
  either, which is what they counted for before (vf-0of)."
  [branch]
  (filter #(= :empirical (:claim-status %)) (:artifacts branch)))

(defn confirmed-in-last
  "Whether a confirmed artifact landed within the last `n` turns. Incremental
  strategies naturally look like verify size N, fail at N+1, verify N+1, and
  culling them throws away the most valuable branch."
  [branch n]
  ;; Cutoff in GLOBAL turns — the unit the artifact stamps are in. Local
  ;; (count :turns) made a fork read its parent's ten-round-old artifact as
  ;; recent forever (karamazov-blt.16).
  (let [cutoff (- (turn-count branch) n)]
    (boolean (some #(and (= :confirmed (:claim-status %))
                         (>= (:turn %) cutoff))
                   (:artifacts branch)))))

(defn banked-in-last
  "Whether the branch banked anything in the last `n` turns — a confirmation or
  a measurement.

  What the cull trigger reads, where `confirmed-in-last` used to. A branch
  halfway through a parameter sweep has confirmed nothing by construction, and
  culling it for that is the same mistake as culling the incremental prover:
  the work is going somewhere and the beam cannot see it. The gates that ask a
  branch to SHIP still read `confirmed-in-last`, because a measurement is not
  something to ship."
  [branch n]
  ;; Same unit note as confirmed-in-last: the cutoff and the stamps must both
  ;; be global turns.
  (let [cutoff (- (turn-count branch) n)]
    (boolean (some #(and (#{:confirmed :empirical} (:claim-status %))
                         (>= (:turn %) cutoff))
                   (:artifacts branch)))))

;; --- the winner rubric as data (drg-4026 #30) --------------------------------
;;
;; The rubric's component FORMS live in phases.edn :finished-key and compile
;; here at load — the same compile-once pattern as the steer gates (tier 3):
;; the forms see `branch` as a local, the fns they call resolve in THIS
;; namespace at compile, and the branch VALUE arrives at fire time. Retuning
;; the rubric is a data edit; the compile is the fail-fast for a form that
;; references something that does not exist.

(def ^:private key-fns
  ;; Memoized against the phase table's generation, not compiled once at
  ;; namespace load: system/start! calls phases/reload! precisely so a rubric
  ;; edit takes effect, and a top-level def made that call a no-op for the
  ;; rubric while working for every plain lookup beside it.
  (util/generation-cache
   phases/gen
   ;; `*ns*` bound for the same reason as gates/compile-form: compiled on
   ;; first use now, and `confirmed-artifacts` resolves here and nowhere the
   ;; first caller might be.
   #(binding [*ns* (the-ns 'samizdat.agent.state)]
      (mapv (fn [form] (eval `(fn [~'branch] ~form)))
            (phases/finished-key-forms)))))

(defn finished-key
  "The ranking tuple for a done-eligible branch, best-first component order.

  UCLA's FirstProof selector ranked prose candidates with an LLM judge —
  rigor, then self-consistency, then citation reliability, prefer-the-
  stronger-claim on ties — because nothing about their candidates was
  mechanical. Ours are engine-audited, so the ranking is data and no model
  sits in the path. The components live in phases.edn (drg-4026 #30);
  most important first:

  [non-relaxation slow-seen engine-diversity confirmed-count id]

  - non-relaxation: 1 unless the last audit declared the evidence a
    relaxation of the thesis. A branch that proved the asked claim beats one
    that proved a weakening — UCLA's prefer-the-stronger-claim tie-break,
    mechanical here because the audit already judged it. A nil last-audit
    counts as non-relaxation; only an explicit RELAXATION: yes lowers it.
  - slow-seen: 1 when :slow is in :tiers-seen. A cross-checked template or
    an independent review is stronger evidence than a one-shot check.
    :tiers-seen is the authoritative signal because every slow path records
    it — verify_template stamps the set AND the artifact, review re-
    confirms without producing a new artifact and stamps only the set.
  - engine-diversity: distinct engine kinds among confirmed artifacts.
    Independent engines compose (consensus/engine-agreement's counting
    rule): one Prolog + one Z3 confirmation is stronger than two Z3s.
  - confirmed-count: more engine-confirmed artifacts beats fewer.
  - id: ascending, a stable arbitrary tie-break so the ranking never
    depends on vector order."
  [branch]
  (mapv #(% branch) (key-fns)))

(defn compare-finished-keys
  "Compare two ranking tuples, best first. Component-wise in key order, so a
  relaxation never outranks a direct proof no matter how many artifacts it
  carries; the first component that differs decides.

  Generic over the tuple's SHAPE, which is the point: the rubric is
  phases.edn data (drg-4026 #30), and a version of this that destructured
  five named components and negated four of them was data-driven in name
  only — dropping a component from the rubric threw on `(- nil)` and adding
  one silently left it out of the ranking. The direction is read off the
  values instead: a numeric component is bigger-is-better (every rubric
  component counts evidence), anything else is an ascending tie-break (the
  id). Both are what the five-component rubric meant.

  Not a chain of `(or (compare …) …)`: `compare` returns 0 on a tie and 0 is
  truthy, so the chain never falls through to the next component."
  [a b]
  (reduce (fn [_ [x y]]
            (let [c (if (and (number? x) (number? y))
                      (compare y x)
                      (compare x y))]
              (if (zero? c) 0 (reduced c))))
          0
          (map vector a b)))

(defn rank-finished
  "Rank done-eligible branches best first, by `finished-key`.

  Expects branches holding :final-answer (the caller has filtered); the
  ranking reads only the evidence they carry, never the order they arrived
  in."
  [branches]
  (sort-by finished-key compare-finished-keys branches))

(defn record-mechanics
  "Fold one turn's fence signals into the branch's mechanics counters."
  [branch signals]
  (-> branch
      (update-in [:mechanics :calls] inc)
      (cond->
       (:parse-error signals) (update-in [:mechanics :parse-errors] inc)
       (:auto-repaired signals) (update-in [:mechanics :auto-repairs] inc)
       (:truncated signals) (update-in [:mechanics :truncations] inc)
       (:multiple-fences signals) (update-in [:mechanics :multi-fences] inc))))

;; Tier 1c: the list is wordlists.edn :claim-relevance — data, retunable at
;; runtime. A separate loader from gates.clj because this namespace sits
;; below gates in the require graph.
(def ^:private claim-stopwords
  "The relevance filter's stopwords, re-read when the lexicon reloads.

  This was a top-level `def`, evaluated once at namespace load — so a
  supervisor editing the list saw nothing change until the process
  restarted, in the namespace whose whole premise is that the list is data.
  ship.clj had already hit this and fixed it with `generation-cache`; state
  had the same bug and not the same fix."
  (util/generation-cache lexicon/gen #(lexicon/wordlist :claim-relevance)))

(defn- singularize
  "Strip one trailing `s`. `flow` and `flows` are the same noun, and a
  set-intersection that cannot see that misses the commonest way two people
  write the same mathematical term — it cost the relevance guard a true match
  the first time it was asked a real question.

  Applied AFTER the stopword filter, so a stopword's stem is never resurrected
  (`supports` is on the list; `support` is not). Never to a word ending in
  `ss`, and never below the lexicon's `:min-stem-length`, so nothing is
  stemmed into a collision."
  [w]
  (if (and (>= (count w) (lexicon/tuning :claim-matching :min-stem-length))
           (str/ends-with? w "s")
           (not (str/ends-with? w "ss")))
    (subs w 0 (dec (count w)))
    w))

(defn- claim-tokens [s]
  (->> (str/split (str/lower-case (or s "")) #"[^a-z0-9]+")
       (remove str/blank?)
       (remove (claim-stopwords))
       (filter #(>= (count %) (lexicon/tuning :claim-matching :min-token-length)))
       (map singularize)
       set))

(defn advances-thesis?
  "Whether a confirmed claim actually moves the registered plan forward.

  Found by a zebra run that exhausted its turns: at turn 11 the model verified
  `clpfd is available and supports a basic finite-domain constraint`, an engine
  said yes, the harness recorded a confirmed artifact, and the milestone gate
  congratulated it. Nothing about the puzzle had been established. A guard that
  treats every confirmation as progress cannot see a branch verifying its own
  tooling, and that is precisely the successful-but-useless turn the progress
  monitor exists for.

  With no thesis registered the PROBLEM stands in for one. That case used to
  return true unconditionally, which was defensible while the stall counter was
  the only consumer and stopped being so once this became the harness's only
  relevance signal: a branch that never called `thesis` had no guard at all.
  Run 0d0c3560's B3 confirmed `Diagnostic: between(-1,1,X) succeeds for
  X = -1,0,1.` at turn 4, was congratulated for it by the milestone gate, and
  exported it to all three siblings through the shared pool (vf-8fl).

  Only when there is nothing to measure against at all — no thesis, and a
  problem with no substantive vocabulary of its own — does everything count.
  Refusing to credit exploration would be worse than crediting too much, and
  the bar is very low: any one shared substantive word clears it."
  [branch claim]
  (let [sub-claims (get-in branch [:thesis :subClaims])
        goal (get-in branch [:thesis :goal])
        targets (remove str/blank? (conj (vec sub-claims) goal))
        targets (if (seq targets) targets (remove str/blank? [(:problem branch)]))
        ct (claim-tokens claim)]
    (if (every? empty? (map claim-tokens targets))
      true
      (boolean (some #(seq (clojure.set/intersection ct (claim-tokens %)))
                     targets)))))

(defn has-relevant-confirmed?
  "Whether the branch has confirmed anything that engages what it is working
  on. What the gates whose signal is \"you proved something, now act on it\"
  should read: `has-confirmed?` cannot tell a lemma from a check of the
  harness's own tooling, and both of those gates spend something real — a
  milestone nudge, or a fork."
  [branch]
  (boolean (some #(advances-thesis? branch (:claim %))
                 (confirmed-artifacts branch))))

(defn enter-phase
  "Advance the branch to the phase the table says follows its current one,
  stamping the turn the new phase began. A no-op when the current phase has
  no successor — the stamp means when THIS phase began. The successor is
  phases.edn data (drg-4026 #34); the name is generic because the machine,
  not this fn, decides what follows what."
  [branch turn]
  (if-let [next (phases/next-phase (:phase branch))]
    (assoc branch :phase next :phase-entered-turn turn)
    branch))

(defn explore-cap-expired?
  "Whether the branch's current phase is capped and has spent more than
  `cap` turns in it. The release valve: a branch that cannot get a skeleton
  to elaborate must not be locked out of verification for the whole run.
  Which phases are capped is phases.edn data (drg-4026 #34); build has no
  clock, and a re-entry restarts the count."
  [branch cap current-turn]
  (and (:cap-key (phases/phase (:phase branch)))
       (> (- current-turn (or (:phase-entered-turn branch)
                              (:created-at-turn branch)))
          cap)))

(defn enter-reframe
  "Withhold `claim` from this branch and start the reframe clock.

  vf-9wx. The harness's only answer to repeated failure was to kill the
  branch, which is a fine backstop and a poor first move: a branch that has
  been grinding one approach for three turns usually needs a different
  approach, not a funeral. The gate that fires here WITHHOLDS rather than
  suggests, because that is the one thing this harness has repeatedly measured
  as working — gen-27 ignored seventeen advisory nudges, while the audit
  gate's refusals had B4 rewriting its encoding three times.

  Claim-scoped rather than tool-scoped, which is what makes it work on every
  engine. The obvious implementation is to reuse the phase machine — drop the
  branch into :explore, where verification is unavailable and a plan is the
  only move — but :explore withholds only the LEAN tools, for the good reason
  that the way out of it is a Lean sketch (vf-2vi). On a Prolog or Z3 problem
  that withholds nothing and the gate is a no-op precisely where it is most
  needed: the odd-covering campaign is entirely Z3 and Prolog. Refusing the
  failing CLAIM bites on every engine, and its substitute — verify something
  else — is available to all of them."
  [branch turn claim]
  (assoc branch :reframe-claim claim :reframe-entered-turn turn))

(defn clear-reframe
  "End the reframe. The branch banked something, which the refused approach
  could not have produced, so the restriction and the reprieve both lift."
  [branch]
  (dissoc branch :reframe-claim :reframe-entered-turn))

(defn reframe-active?
  "Whether the branch is inside its reframe window.

  Keyed on the clock rather than on the claim, so a branch reframed with no
  identifiable claim to withhold still gets its turns to change course — it
  was told to change technique either way, and that costs turns either way.

  A nil `turn` means the caller is not tracking turns (a unit test, or a
  context that has no turn to give); the reframe then reads as active on the
  strength of the stamp alone."
  [branch turn grace]
  (boolean (and (:reframe-entered-turn branch)
                (or (nil? turn)
                    (< (- turn (:reframe-entered-turn branch)) grace)))))

(defn begin-reframe
  "Enter a reframe: record the withheld claim and start the reframe clock.

  The claim-scoped refusal is the whole mechanism now. The proof harness's
  extra step — dropping a Lean branch back into :explore to force a new plan —
  left with its tool surface (provenance R3-6); the phase machine's live work is
  the explore cap, the release valve that keeps a branch from camping in
  explore forever."
  [branch turn claim]
  (enter-reframe branch turn claim))

(defn abandoned-log
  "The claims this branch has had withheld and reframed away from, newest
  last, bounded at 5. This is the reflexion log the stuck gate reads: a branch
  grinding on its third dead end has usually forgotten the first two, and
  re-deriving them costs turns the beam could spend elsewhere.
  See begin-reframe, which appends here as it records :last-failed-claim."
  [branch claim]
  (-> (or (:abandoned branch) [])
      (conj claim)
      (->> (take-last 5))
      vec))

(defn record-outcome
  "Apply a turn's outcome to the counters the gates read.

  `progress?` is deliberately narrower than `success?`. A tool call can succeed
  and advance nothing — a model making varied, well-formed, useless calls trips
  no error-keyed guard and just burns the run, which is the case dirge PR 738's
  progress monitor exists for. And a confirmation that has nothing to do with
  the branch's own plan is the same failure wearing a success's clothes; see
  `advances-thesis?`.

  `consecutive-failures` drives the cull gate, so it has to mean consecutive.
  It used to be cleared only by :success — in practice only by banking an
  artifact — so a branch that hit a rough patch and then worked cleanly for
  several turns still carried the old count into the gate. gen-18 B1 made
  three malformed tool calls, recovered, ran three clean Octave sessions that
  produced dual potentials, and was culled on a counter last incremented three
  turns earlier. A clean turn now works one off the tally. Sustained failure
  still accumulates faster than recovery clears it, and the guard against
  well-formed but useless calls is `turns-since-progress`, which a neutral
  turn still increments — so nothing is given away here.

  `:mechanics` is a fourth category, for a turn that produced no usable tool
  call. It does NOT touch `consecutive-failures`: gen-20 B2 was culled at turn
  6 having called `thesis` and `lean_search` and nothing else, its four other
  turns having emitted no ```tool-call block, and the reason it died with said
  the critic had scored its line a dead end — when the branch had never made a
  claim for the critic to score. loop.clj draws exactly this distinction one
  branch up for a provider error; a fence the model malformed is the same kind
  of fault, and the branch already tracks mechanics separately.

  It gets its own tally rather than simply not counting, because separating
  the counter is the point and softening it would be a different bug: the
  `mechanics` map feeds only the capability tier, and `turns-since-progress`
  feeds only progress-stalled, so with neither counter moving, a branch
  emitting nothing but garbage would hold a beam slot to the turn budget. Any
  well-formed call clears the tally — the branch has demonstrated it can work
  the protocol, whatever the call then did."
  [branch {:keys [category progress? claim tool policy-refusal?]}]
  (let [real-progress? (and progress?
                            (or (nil? claim) (advances-thesis? branch claim)))]
    (cond-> branch
      (= :failure category) (update :consecutive-failures inc)
      ;; What the stuck gate withholds, and what it withholds it from. Only
      ;; failures set it: a branch that failed on A and then succeeded on B
      ;; has not been told to abandon anything. Left alone on a claimless
      ;; failure rather than cleared, because the last claim that DID fail is
      ;; still the better answer to "what is this branch grinding".
       ;; A failing call that carried a claim records it, so the stuck gate
       ;; knows what the branch was grinding on when it failed. The proof
       ;; harness's Lean exclusion (an unsolved goal is evidence about the
       ;; proof, not the statement) left with its tool surface (provenance R3-6);
       ;; on the coding loop a failing claim is fair game to withhold.
       (and (= :failure category) (seq (str claim)))
       (assoc :last-failed-claim claim
              :abandoned (vec (take-last 5 (conj (or (:abandoned branch) []) claim))))
      (= :failure category) (assoc :last-failed-tool tool)
      ;; Cleared by a success, so the gate can never withhold something the
      ;; branch has already got past. Without this, a branch that failed on A,
      ;; succeeded, and then failed twice on CLAIMLESS calls (octave_eval is
      ;; the one verification tool with no claim) would be refused A — an
      ;; approach it is not even working on. A false withholding blocks
      ;; legitimate work and says nothing about why.
      (= :success category)
      (assoc :last-failed-claim nil :last-failed-tool nil)
      (= :success category) (assoc :consecutive-failures 0)
      (= :neutral category) (update :consecutive-failures #(max 0 (dec (or % 0))))
      (= :mechanics category)
      (update :consecutive-mechanics-failures (fnil inc 0))
      (and (= :mechanics category) policy-refusal?)
      (update :consecutive-policy-refusals (fnil inc 0))
      (contains? #{:failure :success :neutral} category)
      (assoc :consecutive-mechanics-failures 0)
      (contains? #{:failure :success :neutral} category)
      (assoc :consecutive-policy-refusals 0)
      real-progress? (assoc :turns-since-progress 0 :any-progress? true)
      (not real-progress?) (update :turns-since-progress inc))))

(defn add-artifact
  "Bank an artifact on the branch, stamping its tier into :tiers-seen.

  The stamp closes a live/replay disagreement. Nothing on the live path ever
  wrote :tiers-seen — the proof tools that did left with the proof harness —
  while resume/rebuild-branch reconstructs it as `(set (keep :tier artifacts))`.
  So the winner rubric's slow-seen component read 0 for a running branch and 1
  for the same branch after a crash and resume, which is exactly the kind of
  snapshot-versus-replay drift this namespace's docstring says cannot happen.
  Deriving it here from the artifact makes the two agree by construction."
  [branch artifact]
  (cond-> (update branch :artifacts conj artifact)
    (:tier artifact) (update :tiers-seen (fnil conj #{}) (:tier artifact))))

(defn squeeze-context
  "Tighten this branch's compaction budget one notch (karamazov-d41).

  Set by the loop when the provider says the prompt outgrew its context
  window. The squeeze level scales the gates.edn :context-budget compaction
  numbers down (infer/render applies gates.edn :context-squeeze), so the
  NEXT assemble fits where this one did not — recovery is harness-side and
  invisible to the model, exactly like compaction always is. Never unwound:
  a branch that hit the wall once will grow back into it, and the level is
  the durable record that it did."
  [branch]
  (update branch :context-squeeze (fnil inc 0)))

(defn add-turn
  "Record one turn on the branch. `entry` carries :turn, :tool, :category, and
  for a failure the :error it produced — the last so `repeating-failure?` can
  tell a branch stuck in a loop from one making fresh mistakes."
  [branch entry]
  (update branch :turns conj entry))

(defn unpin-task-statement
  "Release `task-id`'s pinned statement back to the compaction pool.

  A claim pins its statement so the branch's CURRENT task is never unloaded
  (RFC-004); a task that is closed or set down is not current, and its
  statement used to stay pinned forever — a branch that switched twice
  carried three permanent 'your task is…' blocks, the earlier two wrong
  (karamazov-swd). Un-pinning is a METADATA flip: llm.message/prepare
  projects role and content only, so no wire byte changes and the prefix
  cache is untouched; the statement then ages out of the verbatim window and
  compacts to a digest line through the normal one-attempt in-place rewrite."
  [branch task-id]
  (update branch :messages
          (fn [ms]
            (mapv #(if (and (:pinned? %) (= task-id (:task-id %)))
                     (dissoc % :pinned?)
                     %)
                  ms))))

(defn repeating-failure?
  "Whether this branch's LAST turn was already this exact (tool, error) failure.

  29 of gen-20's 57 failures were four identical (tool, message) pairs, and the
  harness answered the fifth the way it answered the first. B1 — which had
  independently rediscovered the greedy characterisation, the best idea in the
  run — died calling `proof_start` wrong, being told, and calling it wrong
  again.

  Exact comparison rather than text similarity, deliberately. Both halves are
  already recorded, it needs no threshold to tune, and it cannot fire on an
  honest retry: a branch that changed anything about the call produces a
  different error, and a branch that succeeded in between is not looping.

  Called AFTER the turn is recorded, so it asks whether the last two turns are
  the same failure — one failure is a mistake, two identical ones are a loop."
  [branch tool error]
  (let [turns (:turns branch)
        ;; :mechanics as well as :failure. A branch repeating an identical
        ;; malformed call is looping in exactly the sense this function was
        ;; written for, and matching only :failure made the harness answer the
        ;; fifth the way it answered the first — gen-31 B3 was told "Missing
        ;; required argument(s): query" five times while a parser bug ate the
        ;; argument it had supplied. Categories that reached an engine and
        ;; those that never left the harness both loop the same way.
        same? (fn [t] (and t
                           (#{:failure :mechanics} (:category t))
                           (= tool (:tool t))
                           (= error (:error t))))]
    (boolean (and (>= (count turns) 2)
                  (same? (peek turns))
                  (same? (peek (pop turns)))))))

(defn add-message
  "Append a message. `meta` is optional per-message provenance merged onto it —
  `{:turn n}` is the one that earns its keep: compaction needs to know which
  turn a message belongs to in order to replace it with that turn's digest,
  and the positional guess it used before is not sound (a provider error or a
  no-call turn appends messages without appending a turn row, so the k-th
  message is not the k-th turn). Stamping at creation, where the turn number
  is actually known, makes the correspondence a fact rather than an inference.

  Absent for the many call sites with no turn in scope; compaction falls back
  to summarising a message from its own content, which is never a lie about
  which turn it was."
  ([branch role content] (add-message branch role content nil))
  ([branch role content meta]
   (update branch :messages conj
           (merge {:role role :content content} (not-empty meta)))))

;; turn-count and own-turn-count are defined beside `active?`, above their
;; budget-arithmetic callers (confirmed-in-last / banked-in-last).

(defn describe
  "One line for logs and for the run summary."
  [branch]
  (str (:id branch) " " (name (:status branch))
       " turns=" (turn-count branch)
       " artifacts=" (count (:artifacts branch))
       " confirmed=" (count (confirmed-artifacts branch))
       (let [m (count (empirical-artifacts branch))]
         (when (pos? m) (str " measured=" m)))
       (when-let [r (:inactive-reason branch)] (str " (" r ")"))))

(defn residual
  "What this branch left outstanding. A run cut short by the turn cap should
  say what is unfinished rather than making a resume re-derive scope from the
  transcript — dirge PR 738's residual objectives."
  [branch]
  (let [{:keys [goal subClaims]} (:thesis branch)
        proved (set (map :claim (confirmed-artifacts branch)))
        outstanding (remove proved subClaims)]
    (when goal
      {:branch (:id branch)
       :goal goal
       :proved (vec (filter proved subClaims))
       :outstanding (vec outstanding)
       :best (some-> (last (confirmed-artifacts branch)) :claim)})))

(defn render-residual [r]
  (when r
    (str "- " (:branch r) " was proving: " (:goal r) "\n"
         (when (seq (:proved r))
           (str "    proved: " (str/join "; " (:proved r)) "\n"))
         (when (seq (:outstanding r))
           (str "    outstanding: " (str/join "; " (:outstanding r)) "\n"))
         (when (:best r) (str "    best confirmed: " (:best r) "\n")))))

(defn- artifact-substantiates
  "What an artifact's claim-status lets it substantiate. Only :confirmed
  artifacts may be presented as established; the existential, measured and
  ambiguous buckets get their own clearly-labeled sections, and anything else
  (refuted, unknown) substantiates nothing and never renders."
  [a]
  (cond
    (= :confirmed (:claim-status a)) :established
    (= :existential (:claim-status a)) :existential
    (= :empirical (:claim-status a)) :measured
    (= :ambiguous (:claim-status a)) :ambiguous
    :else :neither))

(defn build-residual-report
  "The honest progress report for a run that exhausted without shipping.

  Never ship nothing, never ship a lie — UCLA Track B's honesty mandate, made
  mechanical by the artifact requirement: only artifacts with :confirmed
  status may appear as established, and every other artifact is labeled for
  exactly what it does and does not substantiate. The report says on its face
  that it is a progress report, not a solution.

  Pure: the final branch states, the failure-log entries and the gate tally
  arrive as data, so this is testable with no model and no store."
  [{:keys [branches failures gate-tally]}]
  {:label (str "PROGRESS REPORT — not a solution. Nothing below is"
               " established unless an engine confirmed it.")
   :branches
   (mapv (fn [b]
           (let [{:keys [goal subClaims]} (:thesis b)
                 confirmed (confirmed-artifacts b)
                 proved (set (map :claim confirmed))
                 grouped (group-by artifact-substantiates (:artifacts b))
                 provenance #(mapv (fn [a] (select-keys a [:claim :kind :tier :turn])) %)
                 audit (:last-audit b)]
             (cond-> {:branch (:id b)
                      :goal goal
                      :outstanding (vec (remove proved subClaims))
                      :proved (vec (filter proved subClaims))
                      :established (provenance (get grouped :established))
                      :existential (provenance (get grouped :existential))
                      :measured (provenance (get grouped :measured))
                      :ambiguous (provenance (get grouped :ambiguous))}
               ;; Drift is only reportable when the audit actually restated
               ;; what the evidence establishes; an audit with no ESTABLISHED
               ;; line has nothing to compare against the goal.
               (:established audit)
               (assoc :drift {:goal goal
                              :established (:established audit)
                              :relaxation? (:relaxation? audit)}))))
         branches)
   :failures (vec failures)
   :gate-tally (vec gate-tally)})

(defn- claim-lines
  "`- [kind/tier] claim` per artifact, or nil for an empty section — the
  mechanical half of the residual report. What each section MEANS is prose,
  and prose lives in prompts/residual-report.md."
  [artifacts]
  (when (seq artifacts)
    (str/join "\n" (for [a artifacts]
                     (str "- [" (:kind a) "/" (:tier a) "] " (:claim a))))))

(defn render-residual-report
  "Markdown-ish text for the API content slot. The established section is the
  load-bearing one; existential, ambiguous, drift and run-level sections are
  labeled for exactly what they are.

  The labels are the point of this function and every one of them was a
  string literal here. They are what tells a reader that `measured` is not
  `established` and that an existential witness is not an instance — the
  distinctions the whole residual report exists to make — and a project that
  works on something other than proofs will want all of them said
  differently. They are in prompts/residual-report.md now; this assembles the
  lists and hands them over."
  [r]
  (when r
    (prompt/render
     "residual-report"
     {:label (:label r)
      :branches
      (for [b (:branches r)]
        {:branch (:branch b)
         :goal (:goal b)
         :outstanding (when (seq (:outstanding b))
                        (str/join "\n" (map #(str "- " %) (:outstanding b))))
         :established (claim-lines (:established b))
         :existential (claim-lines (:existential b))
         :measured (claim-lines (:measured b))
         :ambiguous (claim-lines (:ambiguous b))
         :drift (boolean (:drift b))
         :drift-established (:established (:drift b))
         :drift-goal (:goal (:drift b))
         ;; Whether the audit found a WEAKENING is the one judgement in this
         ;; report. It reaches the template as a flag and the template says
         ;; both readings, so a project can reword either without touching a
         ;; conditional in compiled code.
         :drift-relaxation (boolean (:relaxation? (:drift b)))})
      :failures (when (seq (:failures r))
                  (str/join "\n" (for [f (:failures r)]
                                   (str "- [" (:branch_id f) " t" (:turn f) " "
                                        (:tool_name f) "] " (:claim f)
                                        "\n  → " (:reason f)))))
      :gate-tally (when (seq (:gate-tally r))
                    (str/join "\n" (for [g (:gate-tally r)]
                                     ;; met-late IS met — acting a turn later
                                     ;; is acting. Omitting it rendered a gate
                                     ;; that settled met-late 3x as "3 fired,
                                     ;; 0 met" (blt.38).
                                     (str "- " (:gate g) ": " (:fired g) " fired, "
                                          (+ (or (:met g) 0) (or (:met-late g) 0)) " met, "
                                          (or (:unmet g) 0) " unmet, "
                                          (or (:open g) 0) " open"))))})))

;; --- safe state -------------------------------------------------------------
;;
;; DS1's third failure rung, which dirge implemented as a git-backed restore.
;; The branch analogue is the turn cursor at the moment of the last
;; confirmation: the journal is the replay log resume already rebuilds
;; branches from, and it is append-only, so the turns up to the cursor are
;; by construction a state the branch was in.

(defn mark-green
  "Record the branch state at a confirmation. The snapshot is the turn
  cursor — an index into the journal's replay log, not a copy of anything."
  [branch]
  (assoc branch :green-snapshot (count (:turns branch))))

(defn snapshot-covers?
  "Whether falling back to the green point produces a state that ever existed.

  This is the coverage gate, and it is the part that matters. dirge's version
  declined when a `sed -i` mutated a file outside the snapshot store, because
  restoring around it yielded a state that never was. This surface has no
  un-journalled mutation path — every effect a branch can produce rides a
  recorded tool turn — so coverage reduces to the turn log still reaching
  the green cursor: replaying the first N turns is exactly the branch as it
  stood at the confirmation.

  Declining is the safe answer. A partially-restored session is worse than no
  restore, because it is a state nobody reasoned about."
  [branch]
  (let [green (:green-snapshot branch)
        now   (count (:turns branch))]
    (cond
      (nil? green)
      {:ok false :reason "no green verify to fall back to"}

      (> green now)
      {:ok false :reason "the turn log no longer reaches the green point — the journal was pruned or rewritten, and replaying past it would produce a session that never existed"}

      :else
      {:ok true :rewinding (- now green)})))

(defn safe-state-due?
  "Twice the cull threshold's worth of consecutive failures, with a green point
  to fall back to. Deliberately harder to trigger than a cull: this rung spends
  a hard-capped abort and rebuilds a process. The multiple is passed by the
  caller (gates.edn :safe-state-multiple), keeping this namespace free of the
  config layer — the same split as supervisor's shipping vocabulary (tier 1a)."
  [branch cull-threshold multiple]
  (and (:green-snapshot branch)
       (>= (:consecutive-failures branch) (* multiple cull-threshold))))
