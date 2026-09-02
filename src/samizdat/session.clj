;; samizdat - a self-hosting agentic harness
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

(ns samizdat.session
  "SHORT-TERM MEMORY: what has happened in this process, live and in memory.

  The journal is the durable record and answers `what happened in run X`. This
  answers a different question — `what is happening, right now, across
  everything this process has done` — and it answers it without a query,
  because the supervisor needs it on every turn and a supervisor that has to go
  and measure before it can think will mostly not bother.

  WHY THIS EXISTS. The supervisor's job is to notice what is going wrong and
  change it. It could already read the journal, and in practice it diagnosed
  from a digest of outcomes: which stages shipped, what the reviewer said. That
  is enough to see THAT a run is going badly and nearly useless for seeing
  WHERE — a branch losing a third of its turns to unparseable tool calls and a
  branch losing them to a failing test look identical at the outcome level, and
  they want opposite repairs.

  It also could not answer the question that matters most: **did the change I
  made last round help?** Marks are for exactly that. The supervisor stamps a
  mark when it intervenes, and next turn it is shown the counters SINCE that
  mark — so an intervention is judged against a measured delta rather than
  against a feeling. That is the whole point of keeping this: the harness
  should not have to guess where its problems are, or whether its own fixes
  worked.

  Long-term memory is distilled FROM this (`distill`): a pattern that held
  across a session is a candidate for something worth remembering, and the
  session tally is the evidence for it.

  Process-wide and deliberately not per-run: a session outlives a run, and a
  pattern that only shows up across three runs is exactly the pattern a
  single-run digest cannot see. Reset by `system/start!`."
  (:require [clojure.string :as str]
            [samizdat.lexicon :as lexicon]
            [samizdat.prompt :as prompt]))

(defn- empty-tally []
  {:started-at (str (java.time.Instant/now))
   ;; name -> {:change :hypothesis :before :before-fitness}. The selection
   ;; record: what was changed, what it was expected to do, and the tally it
   ;; is measured against.
   :experiments {}
   :turns 0
   ;; tool name -> category -> count. Both axes matter: WHICH tool and HOW it
   ;; went. A run failing on `eval` and a run failing on `shell` are different
   ;; diagnoses.
   :tools {}
   ;; The mechanics of talking to the model: fences that did not parse, replies
   ;; with no call, repairs the harness had to make, replies cut off by the
   ;; token cap. These are the harness's own failure modes and the ones a
   ;; supervisor is least able to infer from outcomes.
   :signals {}
   ;; gate -> {:fired :met :met-late :unmet}. Whether steering is working.
   :gates {}
   ;; The ship gate: how often it ran, and what it found.
   :verify {}
   ;; The beam's own selection quality — see :selection-losing-evidence.
   :beam {}
   ;; Provider trouble: failed calls, retries, empty replies.
   :provider {}
   ;; Named points in time, each holding the tally as it was.
   :marks {}})

(defonce ^:private tally (atom (empty-tally)))

(defn reset!
  "Start a fresh session. Called by `system/start!`."
  []
  (clojure.core/reset! tally (empty-tally))
  nil)

(defn snapshot
  "The tally as it stands. A plain map — safe to hold, diff, or render.

  Marks, experiments and the per-branch tallies are excluded: the first two
  hold whole tallies of their own, and including them would make a snapshot
  grow with every intervention and a diff compare a tally against a tally
  containing itself; the branch tallies are the same counts again, cut per
  branch, and belong to `branch-tally` rather than to the process-wide view."
  []
  (dissoc @tally :marks :experiments :branches))

;; --- per-branch tallies (RFC-012 F3, karamazov-ts3o.2) ------------------------
;;
;; The same counters, cut per branch, so that the number the beam culls on and
;; the number the supervisor evaluates on are ONE number: `fitness-of` over a
;; branch's own tally, with the same weights. Keyed by [run-id branch-id],
;; because branch ids repeat across runs and a serve process outlives many.
;; Fed by the same observe calls, when the caller can say which branch — a
;; count with no branch still lands in the process-wide tally and nowhere
;; else, so nothing that could be counted before is counted less now.

(defn- count-at
  "`f` applied at the root of the tally and, when `branch` is given, at that
  branch's tally too."
  [t branch f]
  (cond-> (f t)
    branch (update-in [:branches branch] #(f (or % {})))))

(defn observe!
  "Record one thing that happened. `path` is a key path into the tally and the
  count at it goes up by one — for the process and, when `branch` ([run-id
  branch-id]) is given, for that branch.

  Deliberately total and forgiving: an unknown path just creates itself. A
  counter that threw on an unrecognised event would make adding a new signal a
  change to this namespace, and the whole value of the tally is that anywhere
  in the loop can contribute to it cheaply."
  ([path] (observe! path nil))
  ([path branch]
   (swap! tally count-at branch #(update-in % (vec path) (fnil inc 0)))
   nil))

(defn- count-turn
  [t {:keys [tool category signals]}]
  (cond-> (update t :turns (fnil inc 0))
    (and tool category)
    (update-in [:tools (str tool) (keyword (name category))] (fnil inc 0))

    :always
    (update :signals
            (fn [s]
              (reduce (fn [acc [k v]] (if v (update acc k (fnil inc 0)) acc))
                      (or s {})
                      (select-keys signals
                                   [:no-fence :truncated :parse-error
                                    :auto-repaired :multiple-fences]))))))

(defn observe-turn!
  "One completed turn: the tool it called and how that went, plus whatever the
  fence had to say about the reply — and, when `:branch` ([run-id branch-id])
  is given, the same for that branch's own tally.

  One call rather than several, because this is on the hot path of every turn
  and a partial record — the tool counted, the signals lost — would be worse
  than none: it would look like a clean turn."
  [{:keys [branch] :as turn}]
  (swap! tally count-at branch #(count-turn % turn))
  nil)

(defn branch-tally
  "The tally of one branch — the same shape as `snapshot`, so everything that
  reads a tally reads this — or nil when nothing has been counted for it."
  [run-id branch-id]
  (get-in @tally [:branches [run-id branch-id]]))

(defn forget-run!
  "Drop a finished run's branch tallies, so a long-lived serve process does not
  keep one per branch it has ever driven. The process-wide counts stay: that
  is the cross-run view, and dilution is its point."
  [run-id]
  (swap! tally update :branches
         (fn [bs] (into {} (remove (fn [[[r _] _]] (= r run-id))) bs)))
  nil)

(defn mark!
  "Stamp a named point in the session. Anything measured after it can be read
  back with `since`.

  This is what turns the tally from a dashboard into an experiment: the
  supervisor marks when it changes something, and the next turn's delta is the
  evidence for whether the change helped. A mark overwrites its predecessor of
  the same name, so a supervisor re-marking each round always compares against
  its most recent intervention rather than its first."
  [name]
  (swap! tally assoc-in [:marks (str name)] (snapshot))
  nil)

(defn- deep-diff
  "b minus a, for the nested integer counters. Keys absent from `a` count from
  zero; keys whose delta is zero are dropped, because a report of everything
  that did NOT change is how a signal gets lost in its own noise."
  [a b]
  (cond
    (and (number? a) (number? b)) (let [d (- b a)] (when-not (zero? d) d))
    (and (map? b) (or (nil? a) (map? a)))
    (let [m (into {} (keep (fn [[k v]]
                             (when-let [d (deep-diff (get a k) v)] [k d])))
                  b)]
      (when (seq m) m))
    (nil? a) b
    :else nil))

(defn since
  "What has happened since the mark `name`, or nil when there is no such mark.

  Only what CHANGED. A supervisor reading this wants the answer to `did my
  change move anything`, and a full tally with three altered numbers buried in
  it does not answer that."
  [name]
  (when-let [m (get-in @tally [:marks (str name)])]
    (deep-diff m (snapshot))))

(defn marks
  "The marks that have been stamped, newest content aside — just the names, so
  a caller can tell what it is able to ask about."
  []
  (vec (sort (keys (:marks @tally)))))

;; --- distillation -----------------------------------------------------------

(defn fitness-of
  "How well the loop ran, per turn, over a tally-shaped map.

  Takes a SNAPSHOT or a DELTA, which is the same shape, and that is the whole
  reason a delta reports counts rather than rates: the same function scores
  `the session so far` and `the stretch since the supervisor changed something`,
  so the two are comparable without anyone converting between them.

  Per turn rather than per run, or a long bad run would outscore a short good
  one. Returns nil for an empty tally — no turns is not a score of zero, it is
  the absence of a measurement, and a supervisor told `0.0` would read it as
  neutral rather than as unknown."
  ([tally] (fitness-of tally (lexicon/policy :fitness)))
  ([tally p]
   (let [turns (or (:turns tally) 0)]
     (when (pos? turns)
       (let [w (:weights p)
             sig (:signals tally)
             tool-counts (reduce (fn [acc [_ cats]]
                                   (merge-with + acc (select-keys cats
                                                                  [:success :failure :mechanics])))
                                 {} (:tools tally))
             gate-counts (reduce (fn [acc [_ o]]
                                   (merge-with + acc (select-keys o [:met :met-late :unmet])))
                                 {} (:gates tally))
             v (:verify tally)
             prov (:provider tally)
             score (+ (* (or (:empty-reply prov) 0) (:provider-empty w))
                      (* (+ (or (:call-failed prov) 0) (or (:usage-cap prov) 0))
                         (:provider-failed w))
                      (* (or (:retried prov) 0) (:provider-retry w))
                      (* (or (:success tool-counts) 0) (:tool-success w))
                      (* (or (:failure tool-counts) 0) (:tool-failure w))
                      (* (or (:mechanics tool-counts) 0) (:tool-mechanics w))
                      (* (or (:parse-error sig) 0) (:parse-error w))
                      (* (or (:no-fence sig) 0) (:no-fence w))
                      (* (or (:truncated sig) 0) (:truncated w))
                      (* (or (:auto-repaired sig) 0) (:auto-repaired w))
                      (* (or (:green v) 0) (:verify-green w))
                      (* (or (:red v) 0) (:verify-red w))
                      (* (or (:skipped v) 0) (:verify-skipped w))
                      (* (or (:met gate-counts) 0) (:gate-met w))
                      (* (or (:met-late gate-counts) 0) (:gate-met-late w))
                      (* (or (:unmet gate-counts) 0) (:gate-unmet w)))]
         (/ score (double turns)))))))

(defn fitness
  "The session's fitness as it stands."
  []
  (fitness-of (snapshot)))

(defn branch-fitness
  "One branch's fitness, per turn, by the SAME function and weights the session
  and every experiment are scored with — the number selection and evaluation
  share (RFC-012 F3). nil when nothing has been counted for the branch: not a
  score of zero but the absence of one, and a cull or a digest reading nil
  reads it as unknown."
  [run-id branch-id]
  (some-> (branch-tally run-id branch-id) fitness-of))

(defn branch-fitnesses
  "Every measured branch of a run: {branch-id fitness}, only the ones with a
  fitness to report."
  [run-id]
  (into (sorted-map)
        (keep (fn [[[r b] t]]
                (when (= r run-id)
                  (when-let [f (fitness-of t)] [b f]))))
        (:branches @tally)))

;; --- experiments: a change, its hypothesis, and what happened ---------------

(declare experiments unsettled-losses)

(defn experiment!
  "Record that the supervisor is CHANGING something, and start measuring.

  This is the selection step, and it is the one the loop was missing. A
  supervisor could already edit a cell and could already read a tally; what it
  could not do was bind the two together, so a change was never actually
  judged — it was made, and then the next round's numbers were whatever they
  were.

  An experiment stamps the tally, the fitness at that moment, what was changed,
  and what the supervisor expected. The hypothesis matters as much as the
  measurement: a change with no stated expectation cannot be wrong, and a
  change that cannot be wrong teaches nothing whichever way the numbers go."
  [name {:keys [change hypothesis]}]
  (let [before (snapshot)
        open (count (filter #(and (nil? (:settled %))
                                  (not= :too-early (:verdict %)))
                            (experiments)))
        cap (:max-open-experiments (lexicon/policy :fitness))]
    ;; ENFORCED, not asked. The supervisor prompt has always said `one change
    ;; per round`, and a rule the model can decline is not a rule — a second
    ;; change measured alongside the first tells you nothing about either, so
    ;; letting them stack quietly destroys the measurement the whole mechanism
    ;; is for. Refused as data, so the caller reports it rather than throwing.
    (when (>= open cap)
      (throw (ex-info "too many changes in flight"
                      ;; Fully qualified, not ::alias-resolved. A reader
                      ;; outside this namespace — base-test reads every src
                      ;; file as forms — cannot resolve an alias it does not
                      ;; have, and the whole lint died on one keyword.
                      {:type :samizdat.session/too-many-open :open open :cap cap
                       :unsettled (mapv :name (unsettled-losses))}))))
  (let [before (snapshot)]
    (swap! tally assoc-in [:experiments (str name)]
           {:change (str change)
            :hypothesis (str hypothesis)
            :at (str (java.time.Instant/now))
            :before before
            :before-fitness (fitness-of before)})
    (mark! name)
    nil))

(defn verdict
  "What happened after an experiment, or nil when there is no such experiment.

  Compares the fitness of the stretch SINCE the change against the fitness of
  everything before it. Both are per-turn, so the comparison holds even when
  the two stretches are different lengths — which they always are.

  `:too-early` is a real verdict and is reported as one. A supervisor that
  reads three turns of noise as a result will keep changing things on the
  strength of nothing, which is the failure mode this whole mechanism exists to
  prevent."
  [name]
  (when-let [e (get-in @tally [:experiments (str name)])]
    (let [p (lexicon/policy :fitness)
          delta (deep-diff (:before e) (snapshot))
          turns (or (:turns delta) 0)
          after (fitness-of delta p)
          before (:before-fitness e)
          move (when (and after before) (- after before))]
      (merge (select-keys e [:change :hypothesis :at])
             {:turns-since turns
              :before before
              :after after
              :delta move
              :verdict
              (cond
                (< turns (:min-turns-for-verdict p)) :too-early
                (nil? move) :too-early
                (>= move (:meaningful-delta p)) :better
                (<= move (- (:meaningful-delta p))) :worse
                :else :unchanged)}))))

(defn reverted!
  "Record that the supervisor acted on a verdict — reverted the change, or
  kept it deliberately.

  An experiment that returned `worse` and was never acted on is the failure
  mode this whole mechanism exists to prevent: the measurement happened, the
  answer was clear, and the change stayed anyway. Marking it settles it and
  stops the block nagging; NOT marking it is what makes the nagging escalate."
  [name kept?]
  (swap! tally update-in [:experiments (str name)]
         (fn [e] (when e (assoc e :settled (if kept? :kept :reverted)))))
  nil)

(defn experiments
  "Every experiment this session, with its current verdict. What the supervisor
  has tried and what happened — the record that makes a second attempt at the
  same idea a decision rather than an accident."
  []
  (vec (keep (fn [[nm e]] (assoc (verdict nm) :name nm :settled (:settled e)))
             (:experiments @tally))))

(defn unsettled-losses
  "Experiments that measured `worse` or `unchanged` and have not been acted on.

  The one thing a supervisor under selection pressure must not be allowed to
  quietly skip. A change that was measured and found wanting, and then left in
  place, is worse than one nobody measured: the loop now carries a
  modification that the evidence says is not helping, and the next
  supervisor inherits it with no way to tell it was ever questioned."
  []
  (vec (filter #(and (nil? (:settled %))
                     (contains? #{:worse :unchanged} (:verdict %)))
               (experiments))))

(defn- detail
  "What a finding SAYS, from the policy beside the threshold that fires it.

  Here rather than inline because the sentence and the number are one decision:
  a supervisor retuning when `calls-not-parsing` fires will usually also want
  to say something different about it, and splitting those across a resource
  and a compiled string means changing half of it."
  [p kind ctx]
  (prompt/render-str (get-in p [:details kind] (name kind)) ctx))

(defn- rate [n total] (if (pos? total) (/ (double n) total) 0.0))

(defn run-window
  "The tally for THIS RUN, or the whole session when the run was never marked.

  Rates over an unbounded window under-react to recent change, and that is not
  hypothetical: a live run starved of tokens produced an empty provider reply,
  the counter recorded it correctly, and no finding fired — one bad turn in
  thirty-six cumulative is under every threshold. The session had been healthy
  for two runs and the arithmetic said so.

  The RUN is the natural window, because `this run is going badly` is the
  actionable statement and `this process has been fine on average` is not. The
  full-session tally stays for the supervisor's cross-run view, where dilution
  is the point rather than the problem."
  [run-id]
  (let [k (str "run:" run-id)]
    (if (get-in @tally [:marks k])
      ;; The mark exists; a nil diff means NOTHING happened in the run — an
      ;; empty window, not license to fall back to the whole-process tally.
      ;; Conflating the two re-served run 1's patterns at the end of a
      ;; perfectly quiet run (the karamazov-blt.24 edge).
      (or (since k) {})
      (snapshot))))

(defn mark-run!
  "Stamp the start of a run, so findings can be evaluated over it."
  [run-id]
  (mark! (str "run:" run-id)))

(defn findings
  "Patterns in the session worth someone's attention, as data.

  This is the bridge from short-term to long-term memory: a pattern that held
  across a whole session is a candidate for something worth REMEMBERING, and
  the tally is the evidence for it. What counts as a pattern is
  `gates.edn :session-findings` — thresholds, not judgement, because the
  judgement is the supervisor's and hard-coding it here would take the decision
  away from the role that has the context to make it.

  Returns `[{:kind :severity :detail :evidence}]`, empty when the session looks
  healthy. Reports SUCCESSES as well as failures: a supervisor told only what
  is broken will keep changing things that are working."
  ([] (findings (snapshot)))
  ([snap]
   (let [p (lexicon/policy :session-findings)
         turns (max 1 (or (:turns snap) 0))
         sig (:signals snap)
         mech (+ (or (:parse-error sig) 0) (or (:no-fence sig) 0))
         mech-rate (rate mech turns)
         repair-rate (rate (or (:auto-repaired sig) 0) turns)
         trunc-rate (rate (or (:truncated sig) 0) turns)
         tool-fail (fn [[nm cats]]
                     (let [f (+ (or (:failure cats) 0) (or (:mechanics cats) 0))
                           n (reduce + 0 (vals cats))]
                       (when (and (>= n (:min-tool-calls p))
                                  (>= (rate f n) (:tool-failure-rate p)))
                         {:kind :tool-failing :severity :high
                          :detail (detail p :tool-failing {:tool nm})
                          :evidence {:tool nm :calls n :failed f}})))
         gate-dead (fn [[g outcomes]]
                     (let [fired (or (:fired outcomes) 0)
                           met (+ (or (:met outcomes) 0) (or (:met-late outcomes) 0))]
                       (when (and (>= fired (:min-gate-firings p)) (zero? met))
                         {:kind :gate-ignored :severity :medium
                          :detail (detail p :gate-ignored {:gate (name g) :fired fired})
                          :evidence {:gate g :fired fired}})))]
     (vec
      (remove
       nil?
       (concat
        [(when (>= mech-rate (:mechanics-rate p))
           {:kind :calls-not-parsing :severity :high
            :detail (detail p :calls-not-parsing {})
            :evidence {:turns turns :unusable mech :rate mech-rate}})
         (when (>= repair-rate (:repair-rate p))
           {:kind :calls-need-repair :severity :medium
            :detail (detail p :calls-need-repair {})
            :evidence {:turns turns :repaired (:auto-repaired sig) :rate repair-rate}})
         (when (>= trunc-rate (:truncation-rate p))
           {:kind :replies-truncated :severity :high
            :detail (detail p :replies-truncated {})
            :evidence {:turns turns :truncated (:truncated sig) :rate trunc-rate}})
         (let [v (:verify snap)
               skipped (or (:skipped v) 0) ran (or (:ran v) 0)]
           (when (and (pos? (+ skipped ran)) (>= (rate skipped (+ skipped ran))
                                                 (:verify-skip-rate p)))
             {:kind :shipping-unverified :severity :high
              :detail (detail p :shipping-unverified {})
              :evidence {:ran ran :skipped skipped}}))
         (let [prov (:provider snap)
               empties (or (:empty-reply prov) 0)]
           (when (>= (rate empties turns) (:provider-trouble-rate p))
             {:kind :provider-empty-replies :severity :high
              :detail (detail p :provider-empty-replies {})
              :evidence {:turns turns :empty empties}}))
         (let [prov (:provider snap)
               bad (+ (or (:call-failed prov) 0) (or (:usage-cap prov) 0)
                      (or (:retried prov) 0))]
           (when (>= (rate bad turns) (:provider-trouble-rate p))
             {:kind :provider-unreliable :severity :high
              :detail (detail p :provider-unreliable {})
              :evidence (into {:turns turns} (filter val (or prov {})))}))
         (let [n (or (:culled-with-evidence (:beam snap)) 0)]
           (when (>= n (:min-culled-with-evidence p))
             {:kind :selection-losing-evidence :severity :high
              :detail (detail p :selection-losing-evidence {})
              :evidence {:culled-with-evidence n}}))
         (let [green (or (:green (:verify snap)) 0)]
           (when (>= green (:min-green-runs p))
             {:kind :verification-working :severity :good
              :detail (detail p :verification-working {})
              :evidence {:green green}}))
         ;; ACROSS ALL GATES, not one of them. `gate-dead` needs a single gate
         ;; to fire :min-gate-firings times, and a branch ignoring five
         ;; different gates twice each trips none of them. That is the shape a
         ;; live run actually took: 38 turns, five gates, eight firings, not
         ;; one file written, and no finding — because the harness counted the
         ;; nags separately and the branch was ignoring all of them equally.
         ;; The distinction matters to the reader: one dead gate is a bad
         ;; gate, and every gate dead is a bad loop.
         (let [gs (vals (:gates snap))
               fired (reduce + 0 (map #(or (:fired %) 0) gs))
               met (reduce + 0 (map #(+ (or (:met %) 0) (or (:met-late %) 0)) gs))
               ;; A threshold the project's policy table does not carry turns
               ;; the rule OFF. It is not hypothetical: a project seeds its
               ;; own copy of gates.edn on first read and that copy is
               ;; authoritative afterwards, so a rule added to the harness
               ;; later finds keys missing on every project older than it.
               ;; This block renders into the supervisor's turn, and throwing
               ;; there costs the supervisor its whole view of the session to
               ;; report one pattern.
               floor (:min-total-gate-firings p)
               ceiling (:gate-met-rate p)]
           (when (and floor ceiling
                      (>= fired floor)
                      (<= (rate met fired) ceiling))
             {:kind :steering-ignored :severity :high
              :detail (detail p :steering-ignored {:fired fired :met met})
              :evidence {:fired fired :met met :turns turns}}))]
        (keep tool-fail (:tools snap))
        (keep gate-dead (:gates snap))))))))

(defn- counts-line [m]
  (->> m (sort-by key) (map (fn [[k v]] (str (name k) " " v))) (str/join ", ")))

(defn- group-line [m]
  (str/join " | " (for [[k v] (sort-by key m)] (str (name k) " (" (counts-line v) ")"))))

(defn render
  "The session block a supervisor reads.

  Three parts, and the middle one is the reason this exists. The TALLY says
  where the turns are going. The DELTA says what has changed since the
  supervisor last intervened — the only way to tell whether its own fix
  helped, and the thing it previously had to guess at. The FINDINGS say which
  of it crossed a threshold worth acting on.

  Empty when nothing has happened yet, rather than a page of zeroes: a block
  that says nothing should take up no room. The wording is
  prompts/session-block.md, because what the supervisor is told is as much
  policy as when it is told it."
  ([] (render nil))
  ([mark]
   (let [snap (snapshot)
         fs (findings snap)
         f (fitness-of snap)
         exps (experiments)]
     (when (pos? (or (:turns snap) 0))
       (prompt/render
        "session-block"
        {:turns (:turns snap)
         :fitness (when f (format "%.2f" f))
         :tools (when (seq (:tools snap)) (group-line (:tools snap)))
         :signals (when (seq (:signals snap)) (counts-line (:signals snap)))
         :verify (when (seq (:verify snap)) (counts-line (:verify snap)))
         :gates (when (seq (:gates snap)) (group-line (:gates snap)))
         :experiments
         (when (seq exps)
           (str/join "\n"
                     (for [{:keys [name change hypothesis verdict before after
                                   turns-since settled]} exps]
                       (str "- " name " [" (clojure.core/name verdict) "]"
                            (case settled
                              :reverted " (reverted)"
                              :kept " (kept deliberately)"
                              "")
                            (when (and before after)
                              (format " fitness %.2f -> %.2f over %d turns"
                                      before after turns-since))
                            "\n    changed: " change
                            "\n    expected: " hypothesis))))
         :unsettled (let [n (count (unsettled-losses))] (when (pos? n) n))
         :findings (when (seq fs)
                     (str/join "\n"
                               (for [{:keys [kind severity detail evidence]} fs]
                                 (str "- [" (clojure.core/name severity) "] "
                                      (clojure.core/name kind) ": "
                                      detail " " (pr-str evidence)))))})))))
