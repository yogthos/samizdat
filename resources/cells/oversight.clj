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

  PURE, and the whole cost control of the stream. Three things make a pass
  worth its price, and none of them is 'time has passed':

  - the run is being STEERED AND IGNORING IT. A gate firing unmet is the
    harness saying something the branch did not act on, which is the signal
    that the harness's own words are wrong — the supervisor's actual job.
  - the run is PRODUCING NOTHING. Turns are being spent with no artifact and
    no file written.
  - something CRASHED. A stage error is a harness bug the loop survived, and
    it will happen again on the next run if nobody looks.

  A healthy run that is shipping gets no supervision, which is correct: there
  is nothing to tune and saying so costs a turn of somebody's budget."
  [{:keys [unmet-gates idle-turns errors]} {:keys [unmet-floor idle-floor]}]
  (boolean (or (>= (or unmet-gates 0) unmet-floor)
               (>= (or idle-turns 0) idle-floor)
               (seq errors))))

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
                      [:oversight/idle :any] [:oversight/worth-a-look? :boolean]]
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
             findings (session/findings (session/run-window run-id))]
         (assoc data
                :oversight/turns turns
                :oversight/firings firings
                :oversight/findings findings
                :oversight/unmet unmet
                :oversight/idle since
                :oversight/worth-a-look?
                (worth-a-look? {:unmet-gates unmet :idle-turns since
                                :errors (seq (filter :error findings))}
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
        already tried. `:feature/supervise` opens `S<revision>` — a new context
        every time — which is why the supervisor there re-derives the same
        diagnosis on every look and can never say 'I changed that last time and
        it did not help'.

        `SUP` and not `S0`, which is what this used to open and what
        `:feature/supervise` opens on revision zero. Two writers on one branch
        id: run 498450e1's S0 holds 26 turn rows numbered up to 14, the stream
        and the stage overwriting each other's turn numbers, and a record that
        cannot say which supervisor said what is a record of neither."
   :effects [:net :db]
   :requires [:conn :run-id :config]
   ;; The telemetry gather produced, which only reaches here on the :reason
   ;; edge — the manifest's :must-follow constraint and this input are the two
   ;; halves of the same claim.
   :input  [:map [:oversight/turns :any] [:oversight/firings :any]
            [:oversight/unmet :any] [:oversight/idle :any]
            [:oversight/carry {:optional true} :any]]
   ;; :oversight/branch only on the path that ran — the fallback records a
   ;; verdict and an explanation, and there is no branch to carry.
   :output [:map [:oversight/verdict :keyword] [:oversight/answer :any]
            [:oversight/branch {:optional true} :any]]}
  (fn [{:keys [conn run-id] :as ctx} data]
    (safely :reason
     (fn []
       (let [dig (telemetry/digest {:idle-turns (:oversight/idle data)
                                    :unmet-gates (:oversight/unmet data)}
                                   (:oversight/turns data)
                                   (:oversight/firings data))
             prob (prompt/render "oversight-pass"
                                 {:digest dig
                                  :learned (seq (knowledge/standing conn))
                                  :catalog (safely :catalog #(wf/render-catalog conn) "")})
             ;; ONE branch for the run, carried by the stream. Opened once;
             ;; re-opening an existing id is a no-op that returns the row.
             bid "SUP"
             _ (runs/open-branch! conn run-id {:branch-id bid})
             ;; The stream's memory arrives in DATA, not ctx: ctx is the
             ;; run-scoped resources every driver provides, and the carry is
             ;; this pass's value. Putting it in ctx would have meant claiming
             ;; the beam driver provides it, which it does not.
             b (or (some-> (:oversight/carry data) resume-branch)
                   (assoc (state/new-branch
                           {:id bid :problem prob
                            :messages (turn/initial-messages
                                       prob (wf/prompt-text "roles/supervisor") :supervisor)})
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
        which carry more than a clipped note ever could. Its readers are the
        operator and `:feature/supervise`, which quotes the stream's last
        conclusion rather than deriving a second one of its own."
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
