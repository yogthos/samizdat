;; The supervisor's stream, as cells. See manifests/oversight.edn.
;;
;; One pass: gather -> (reason -> apply | quiet). The gate between them is
;; `worth-a-look?`, and it is deliberately cheap and conservative, because a
;; pass costs a model call and most moments in a run do not need one.
(ns cells.oversight
  (:require [clojure.string :as str]
            [clojure.tools.logging :as log]
            [mycelium.cell :as cell]
            [samizdat.agent.loop :as turn]
            [samizdat.agent.state :as state]
            [samizdat.agent.telemetry :as telemetry]
            [samizdat.agent.gates :as gates]
            [samizdat.prompt :as prompt]
            [samizdat.session :as session]
            [samizdat.store.journal :as journal]
            [samizdat.store.knowledge :as knowledge]
            [samizdat.store.runs :as runs]
            [samizdat.store.userspace :as store-us]
            [samizdat.userspace :as userspace]
            [samizdat.workflow :as wf]
            [mycelium.core :as myc]))

(defn- safely
  "A supervisor stage that throws leaves the run alone and the stream alive.

  LOGGED, always. The first version swallowed silently, and a stream whose
  failures are invisible cannot be told apart from one that never started —
  which is exactly the confusion it caused the first time it ran.

  `fallback` is a value, or a FUNCTION of the exception when the failure
  itself belongs in the data map — a log line tells the operator, and the
  record has to tell the next pass."
  [what f fallback]
  (try (f)
       (catch Throwable e
         (log/warn "oversight" what "failed:" (ex-message e))
         (if (fn? fallback) (fallback e) fallback))))

(defn- clip
  "First `n` characters, safely. Collapsing whitespace shortens the string, so
  indexing the ORIGINAL length into the COLLAPSED one overruns it — a crash
  that could only happen once a pass actually succeeded, which is the worst
  time to find it."
  [s n]
  (let [t (str/replace (str s) #"\s+" " ")]
    (subs t 0 (min n (count t)))))

;; --- gather -----------------------------------------------------------------

(defn worth-a-look?
  "Whether this moment deserves a model call.

  PURE, and the whole cost control of the stream. Five things make a pass
  worth its price, and none of them is 'time has passed':

  - the run is being STEERED AND IGNORING IT. A gate firing unmet is the
    harness saying something the branch did not act on, which is the signal
    that the harness's own words are wrong — the supervisor's actual job.
  - the run is PRODUCING NOTHING. Turns are being spent with no artifact and
    no file written.
  - something CRASHED. A stage error is a harness bug the loop survived, and
    it will happen again on the next run if nobody looks.
  - the outer loop is AT ITS SOFT CAP. The feature loop keeps solving past
    it on its own ladder, and the cap is its notice that whether to switch,
    re-budget or stop is now a decision — the supervisor's, and there is one
    supervisor to hand it to (RFC-012 F1).
  - NOBODY SHIPPED. The sharpest form of the second one: a round where every
    owner abandoned, gave up or crashed is producing nothing, and says so in
    its own outcomes rather than by the proxy of turns since a write
    (karamazov-u5uy). No floor, because zero out of a positive total is not
    a matter of degree.

  A healthy run that is shipping gets no supervision, which is correct: there
  is nothing to tune and saying so costs a turn of somebody's budget."
  [{:keys [unmet-gates idle-turns errors at-cap? nothing-shipped?]}
   {:keys [unmet-floor idle-floor]}]
  (boolean (or (>= (or unmet-gates 0) unmet-floor)
               (>= (or idle-turns 0) idle-floor)
               (seq errors)
               at-cap?
               nothing-shipped?)))

(defn at-cap?
  "Whether the outer loop's last round was at or past its soft cap, read from
  the :route note the feature loop leaves each round."
  [round]
  (boolean (and round (:soft-cap round)
                (>= (or (:revision round) 0) (:soft-cap round)))))

(defn round-results
  "The implement round's per-owner outcomes, read off the journal.

  Every implement strategy — board, team and decompose — writes the round it
  finished as an :implement-round note in the FAN-OUT's vocabulary, and this
  is the one place that vocabulary is read back. The status makes the trip as
  a string, because a journal note is JSON and JSON has no keywords, so it is
  put back as a keyword here: the digest counts `(= :done (:status %))` and a
  silent string would count as zero shipped forever, which is the shape of
  the bug this fixes (karamazov-u5uy)."
  [note]
  (mapv (fn [r] (update r :status #(some-> % name keyword)))
        (:results note)))

(defn nothing-shipped?
  "Whether an implement round produced nothing at all. Guarded on a non-empty
  round: no round is not the same fact as a failed round, and only the second
  is worth a model call."
  [results]
  (boolean (and (seq results)
                (not-any? #(= :done (:status %)) results))))

(defn- crash-line
  "A :stage-error note as the one-line form the digest's layer classifier
  reads — the same shape the feature loop's stage guard accumulates."
  [{:keys [stage node error]}]
  (str stage (when (seq (str node)) (str "/" node)) ": " error))

(cell/defcell :oversight/gather
  {:doc "Read the run's health from the JOURNAL rather than from a stage's data
        map. That is what makes this a stream: it depends on nothing having
        been handed to it, so a stalled implementer that hands nothing to
        anybody cannot starve it."
   :effects [:db]
   :requires [:conn :run-id]
   ;; Nothing. That is the docstring's claim made checkable: the stream reads
   ;; the journal, so it depends on nothing having been handed to it, and a
   ;; cell declaring no input is a cell no stalled stage can starve.
   :input  [:map]
   ;; PER-TRANSITION, because the two edges leave with genuinely different
   ;; maps. The :quiet side is also the fallback's shape — a gather that threw
   ;; still routes, carrying only the decision not to look further — so
   ;; declaring the telemetry on both would promise :oversight/quiet keys that
   ;; a failed gather never wrote.
   :output [:per-transition
            {:reason [:map [:oversight/turns :any] [:oversight/firings :any]
                      [:oversight/findings :any] [:oversight/unmet :any]
                      [:oversight/idle :any] [:oversight/round :any]
                      [:oversight/crashes :any] [:oversight/results :any]
                      [:oversight/self-graded :any]
                      [:oversight/worth-a-look? :boolean]]
             :quiet  [:map [:oversight/worth-a-look? :boolean]]}]}
  (fn [{:keys [conn run-id]} data]
    (safely :gather
     (fn []
       (let [turns (journal/turns conn run-id)
             firings (journal/gate-firings conn run-id)
             unmet (count (filter #(= "unmet" (str (:outcome %))) firings))
             ;; Turns since anything was written. The stream's cheapest and
             ;; most reliable distress signal — every stalled run in this
             ;; project's history shows it.
             writes (gates/tool-vocab :file-write)
             since (count (take-while #(not (contains? writes (str (:tool_name %))))
                                      (reverse turns)))
             findings (session/findings (session/run-window run-id))
             ;; THE ROUND, off the journal. The feature loop's stage used to
             ;; hand a supervisor of its own the round's facts in a data map;
             ;; the one supervisor reads them where the loop writes them
             ;; (RFC-012 F1): the last :route note is the round's outcome and
             ;; its cap, and every :stage-error note is a crash the loop
             ;; survived and nobody else will look at.
             round (journal/last-note conn run-id :route)
             ;; THE ROUND'S OUTCOMES, per owner. The strategies used to hand
             ;; these to a supervisor stage of their own in a data map; that
             ;; stage is gone, so they are journalled and read here like
             ;; everything else the stream needs (karamazov-u5uy). Without
             ;; this the digest counted an empty vector on EVERY strategy and
             ;; every brief read `Implementors: 0/0 shipped`.
             results (round-results (journal/last-note conn run-id :implement-round))
             ;; Every evaluator edit this run made, so a run that regraded
             ;; itself says so in its own brief and its own record
             ;; (karamazov-7mo M10). Named, never refused.
             self-graded (into [] (comp (mapcat :keys) (distinct))
                               (journal/notes conn run-id :self-graded))
             crashes (journal/notes conn run-id :stage-error)]
         (assoc data
                :oversight/turns turns
                :oversight/firings firings
                :oversight/findings findings
                :oversight/unmet unmet
                :oversight/idle since
                :oversight/round round
                :oversight/results results
                :oversight/self-graded self-graded
                :oversight/crashes crashes
                :oversight/worth-a-look?
                (worth-a-look? {:unmet-gates unmet :idle-turns since
                                :errors (seq (concat (filter :error findings) crashes))
                                :at-cap? (at-cap? round)
                                :nothing-shipped? (nothing-shipped? results)}
                               {:unmet-floor (gates/threshold :oversight-unmet-floor)
                                :idle-floor (gates/threshold :oversight-idle-floor)}))))
     (assoc data :oversight/worth-a-look? false))))

;; --- reason -----------------------------------------------------------------

(defn resume-branch
  "The carried branch, ready for another pass.

  Keeps the MESSAGES — the supervisor's memory of what it already noticed and
  already tried — and clears the terminal state. A branch that concluded once
  is finished forever otherwise: run b2ffb2ad's supervisor called `done` on
  pass one and its next four passes resumed a completed branch and returned
  instantly, so it spoke once and went quiet for the rest of the run.

  Concluding is not the same as having nothing left to say. A pass ends; the
  stream does not."
  [b]
  (-> b (dissoc :final-answer :verdict :done? :status) (assoc :advisory? true)))

(cell/defcell :oversight/reason
  {:doc "One turn of the supervisor ROLE, in the stream's OWN branch.

        The branch id is stable for the whole run (`SUP`), not minted per pass,
        so the supervisor accumulates a memory of what it already noticed and
        already tried — it can say 'I changed that last time and it did not
        help', which a supervisor opened cold per look never could.

        THE ONE SUPERVISOR (RFC-012 F4). `:feature/supervise` used to open
        `S<revision>` and run this same role from a second context, and before
        that this stream opened `S0`, which is what the stage opens on revision
        zero: run 498450e1's S0 holds 26 turn rows numbered up to 14, the
        stream and the stage overwriting each other's turn numbers, and a
        record that cannot say which supervisor said what is a record of
        neither. The stage runs no role now; its say is what this pass sends
        through `intervene`."
   :effects [:net :db]
   :requires [:conn :run-id :config]
   ;; The telemetry gather produced, which only reaches here on the :reason
   ;; edge — the manifest's :must-follow constraint and this input are the two
   ;; halves of the same claim.
   :input  [:map [:oversight/turns :any] [:oversight/firings :any]
            [:oversight/unmet :any] [:oversight/idle :any]
            ;; Required, like the other four gather always writes on this
            ;; edge: the round's per-owner outcomes are what the digest counts
            ;; for `N/M shipped` and :nobody-shipped, and a supervisor brief
            ;; that silently reports 0/0 because nobody declared the key is
            ;; the bug this closes (karamazov-u5uy).
            [:oversight/results :any]
            [:oversight/self-graded {:optional true} :any]
            [:oversight/round {:optional true} :any]
            [:oversight/crashes {:optional true} :any]
            [:oversight/carry {:optional true} :any]]
   ;; :oversight/branch only on the path that ran — the fallback records a
   ;; verdict and an explanation, and there is no branch to carry.
   :output [:map [:oversight/verdict :keyword] [:oversight/answer :any]
            [:oversight/branch {:optional true} :any]]}
  (fn [{:keys [conn run-id] :as ctx} data]
    (safely :reason
     (fn []
       (let [round (:oversight/round data)
             ;; The mark the supervisor's deltas are measured from, stamped
             ;; BEFORE it is shown anything, so `since` covers the interval
             ;; from its last look to now — the interval its last change was
             ;; in force for (RFC-012 protocol rule 4). Named per run so two
             ;; runs in one process do not read each other's deltas.
             mark (str "supervisor:" run-id)
             live (session/render mark)
             _ (session/mark! mark)
             dig (telemetry/digest {:idle-turns (:oversight/idle data)
                                    :unmet-gates (:oversight/unmet data)
                                    ;; The round's per-owner outcomes, which
                                    ;; is what :nobody-shipped counts and what
                                    ;; the brief's `N/M shipped` line reports.
                                    :results (:oversight/results data)
                                    :self-graded (:oversight/self-graded data)
                                    ;; What this project has already
                                    ;; prescribed for itself (M9).
                                    :prescription (userspace/prescription-mass)
                                    ;; Each branch's session fitness: the
                                    ;; number the cull reads, shown to the
                                    ;; role that tunes (RFC-012 F3).
                                    :fitness (session/branch-fitnesses run-id)
                                    ;; The round, as the feature loop left it
                                    ;; — what its own supervisor stage used
                                    ;; to be handed (F1).
                                    :revision (:revision round)
                                    :soft-cap (:soft-cap round)
                                    :at-cap? (at-cap? round)
                                    :hollow? (:hollow round)
                                    :tests-passed? (:tests-passed round)
                                    :review (some-> (journal/last-note conn run-id :review)
                                                    :decision keyword)
                                    :critic (some-> (journal/last-note conn run-id :critique)
                                                    :decision)
                                    :errors (mapv crash-line (:oversight/crashes data))}
                                   (:oversight/turns data)
                                   (:oversight/firings data))
             prob (prompt/render "oversight-pass"
                                 {:digest dig
                                  :since (not-empty (str live))
                                  :learned (seq (knowledge/standing conn))
                                  ;; Episodes seen in enough distinct runs
                                  ;; to ask whether they are rules. The
                                  ;; store surfaces; the supervisor judges
                                  ;; (karamazov-h27r).
                                  :candidates (seq (knowledge/graduation-candidates conn))
                                  ;; Which surfaces the tuning keeps
                                  ;; touching, and which edits keep being
                                  ;; undone — nil when nothing moved, so
                                  ;; the block takes no room (karamazov-00qw).
                                  :drift (let [{:keys [window-runs top-names]} (gates/threshold :drift)
                                               surfaces (store-us/drift
                                                         conn {:since (runs/nth-recent-start conn window-runs)
                                                               :top-names top-names})]
                                           (when (seq surfaces)
                                             (prompt/render "drift" {:window window-runs
                                                                     :surfaces surfaces})))
                                  :catalog (safely :catalog #(wf/render-catalog conn) "")})
             ;; ONE branch for the run, carried by the stream. Opened once;
             ;; re-opening an existing id is a no-op that returns the row.
             bid "SUP"
             suffix (wf/prompt-text "roles/supervisor")
             _ (runs/open-branch! conn run-id {:branch-id bid :role :supervisor
                                               :prompt-suffix suffix})
             ;; The stream's memory arrives in DATA, not ctx: ctx is the
             ;; run-scoped resources every driver provides, and the carry is
             ;; this pass's value. Putting it in ctx would have meant claiming
             ;; the beam driver provides it, which it does not.
             b (or (some-> (:oversight/carry data) resume-branch)
                   (assoc (state/new-branch
                           {:id bid :problem prob
                            :messages (turn/initial-messages prob suffix :supervisor)})
                          :advisory? true :role :supervisor))
             out (myc/run-compiled (wf/compiled-manifest "supervisor")
                                   (wf/role-ctx ctx :supervisor)
                                   {:branch b :turn 1})]
         (assoc data
                :oversight/answer (get-in out [:branch :final-answer])
                ;; How the pass ENDED, kept beside what it said. A pass that
                ;; ran out of turns has no answer and neither does one that
                ;; crashed; without this the record shows the same
                ;; `notes: null` for both, which is what made run b2ffb2ad's
                ;; four blank passes take a live run to explain
                ;; (karamazov-r5a).
                :oversight/verdict (:verdict out)
                :oversight/branch (:branch out))))
     (fn [e] (assoc data :oversight/verdict :error
                    :oversight/answer (str "the pass failed: " (ex-message e)))))))

;; --- apply ------------------------------------------------------------------

(cell/defcell :oversight/apply
  {:doc "Record what the pass concluded.

        The supervisor ACTS THROUGH ITS TOOLS — `intervene` to steer the run,
        the mutation protocol to tune the harness — so by the time control
        reaches here the acting has already happened. What is left is the
        record, which is not a formality: a decision that appears nowhere is
        indistinguishable from a pass that never ran.

        The NEXT PASS does not read this — it inherits the branch's messages,
        which carry more than a clipped note ever could. Its reader is the
        operator, in the run view."
   :effects [:db]
   :requires [:conn :run-id]
   :input  [:map [:oversight/verdict :keyword] [:oversight/answer :any]
            [:oversight/idle {:optional true} :any]
            [:oversight/unmet {:optional true} :any]]
   ;; Returns `data`: the record is the effect, and the row is the product.
   :output [:map]}
  (fn [{:keys [conn run-id]} data]
    (safely :apply
     (fn []
       (journal/note! conn run-id :oversight
                      {:data {:idle (:oversight/idle data)
                              :unmet (:oversight/unmet data)
                              ;; A blank note means one of several things and
                              ;; the verdict is which: `:done` said nothing,
                              ;; `:exhausted` ran out of turns, `:error` threw.
                              :verdict (some-> (:oversight/verdict data) name)
                              :notes (some-> (:oversight/answer data)
                                             (clip (gates/threshold :oversight-note-chars)))}})
       data)
     data)))

(cell/defcell :oversight/quiet
  {:doc "The run is fine. No model call — the correct outcome for most passes,
        and the reason the stream is affordable at all.

        It still leaves a HEARTBEAT. Saying nothing and not running look
        identical from outside otherwise, and telling those two apart is the
        whole of knowing whether the harness is watching itself. It is one
        cheap row against a run's thousands."
   :effects [:db]
   :requires [:conn :run-id]
   ;; Both OPTIONAL, which the chain check is what established. The :quiet
   ;; edge is also the one a gather that THREW takes, and that path wrote
   ;; neither key — so the heartbeat row is empty in exactly the case where
   ;; something went wrong. It still fires, which is the point of it.
   :input  [:map [:oversight/idle {:optional true} :any]
            [:oversight/unmet {:optional true} :any]]
   :output [:map]}
  (fn [{:keys [conn run-id]} data]
    (safely :quiet
     (fn []
       (journal/note! conn run-id :oversight-quiet
                      {:data {:idle (:oversight/idle data)
                              :unmet (:oversight/unmet data)}})
       data)
     data)))
