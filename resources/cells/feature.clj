;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; The FEATURE loop's role cells (feature.edn wires them). The feature manifest
;; is the outer state machine; these cells delegate its stages to roles:
;;
;;   :feature/review    the REVIEWER role — run reviewer.edn on the implementors'
;;                      finished work; PASS or REVISE.
;;   :feature/critique  the CRITIC role — gate the result with the same judge the
;;                      finalization critic uses, without its branch surgery.
;;   :feature/supervise the SUPERVISOR — watch the role loops and adjust the
;;                      outer loop (round one: force another round on a hollow
;;                      implement result).
;;   :feature/route     ship, or send back to implement with findings as
;;                      guidance, bounded by :run :max-revisions.
;;
;; The implement stage itself is :team/fan-out (cells/team.clj) — the horizontal
;; team of implementor workers lives inside this loop as one stage.
(ns cells.feature
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [mycelium.cell :as cell]
            [mycelium.core :as myc]
            [samizdat.agent.gitdiff :as gitdiff]
            [samizdat.agent.judge :as judge]
            [samizdat.agent.loop :as turn]
            [samizdat.agent.state :as state]
            [samizdat.agent.telemetry :as telemetry]
            [samizdat.agent.tools :as tools]
            [samizdat.engine.proc :as proc]
            [samizdat.llm.client :as llm]
            [samizdat.store.journal :as journal]
            [samizdat.session :as session]
            [samizdat.store.knowledge :as knowledge]
            [samizdat.store.runs :as runs]
            [samizdat.workflow :as wf]))

(defn- revision [data] (or (:feature/revisions data) 0))

(def ^:private implement-ladder
  "The escalation ladder of implement strategies. When one keeps failing its
  soft-cap rounds and the supervisor has not switched, the loop AUTO-ADVANCES to
  the next — so iteration through different approaches happens even when the
  supervisor is passive or can't decide. The supervisor can still switch earlier
  (or to any rung) with a SWITCH: line; this is the floor, not the ceiling."
  ["board" "team" "decompose"])

(defn- next-strategy [current]
  (second (drop-while #(not= % current) implement-ladder)))

(defn- rounds-on-strategy
  "How many rounds this strategy has been tried (from :feature/tried), counting
  the round just finished."
  [data strategy]
  (inc (count (filter #(= strategy (:strategy %)) (:feature/tried data)))))

(defn- hollow?
  "Ground truth: true when the run KNOWS it changed no files — a done with an
  empty working tree is not a feature, however confidently the workers announced
  it. When git is unavailable the answer is unknown, and we do not block what we
  cannot verify (false)."
  [{:keys [root git-baseline]}]
  (let [files (gitdiff/changed-files root git-baseline)]
    (boolean (and files (empty? files)))))

(defn- root-error
  "The exception a cell actually threw, out of mycelium's execution wrapper —
  the same unwrapping beam/unwrap-round-error does, for the same reason: every
  nested-workflow failure otherwise records as the same opaque
  \"execution error\", which told nobody what broke (observed live on the
  board stage). Returns {:message :node}."
  [e]
  (loop [cur e, node nil, depth 0]
    (let [d (ex-data cur)
          inner (:error d)
          node (or (:current-state-id d) (:last-state-id d) node)]
      (if (and (instance? Throwable inner) (< depth 8))
        (recur inner node (inc depth))
        {:message (ex-message cur) :node node}))))

(defn- safely
  "Run a stage body, but never let it take the whole run down. A stage that
  throws records the error and falls through to `fallback` (a safe default for
  that stage) with the error accumulated on :feature/errors — so the run reaches
  the SUPERVISOR, which sees the crash in its telemetry and can plan a fix,
  rather than the workflow dying structurally. The supervisor cannot supervise a
  loop that is already dead."
  [conn run-id stage data body fallback]
  (try (body)
       (catch Throwable e
         (let [{:keys [message node]} (root-error e)
               msg (str (name stage) (when node (str "/" node)) ": " message)]
           (journal/note! conn run-id :stage-error
                          {:data {:stage stage :node (some-> node str) :error message}})
           (-> (fallback data)
               (update :feature/errors (fnil conj []) msg))))))

(defn- run-role
  "Run a role sub-loop (compiled) on a fresh branch `bid` with problem `prob` and
  role-prompt `suffix`. Returns {:verdict :answer}.

  The branch is marked ADVISORY: its done delivers a verdict about the run,
  not shippable work, so the ship gate's evidence rungs (figures need
  artifacts; the tests must be green) do not apply — they gated the verdict
  on the very condition it was reporting, and every advisory role exhausted
  its budget unable to conclude (karamazov-t86)."
  ([ctx compiled bid prob suffix] (run-role ctx compiled bid prob suffix nil))
  ([{:keys [conn run-id] :as ctx} compiled bid prob suffix role]
  (runs/open-branch! conn run-id {:branch-id bid})
  (let [b (assoc (state/new-branch {:id bid :problem prob
                                    ;; ROLE-SCOPED: the tool catalogue this
                                    ;; role is shown is filtered to what it
                                    ;; may call, so its own prompt no longer
                                    ;; has to argue it out of the rest.
                                    :messages (turn/initial-messages prob suffix role)})
                 :advisory? true
                 ;; The branch carries its role, which is what the
                 ;; :outside-role-surface refusal reads. A branch with no role
                 ;; is unrestricted, so a workflow that names none is unchanged.
                 :role role)
        out (myc/run-compiled compiled ctx {:branch b :turn 1})]
    {:verdict (:verdict out) :answer (get-in out [:branch :final-answer])})))

(defn- run-role-as
  "run-role with the ROLE named first — the call sites read better that way and
  the role is the thing that must not be forgotten."
  [role ctx compiled bid prob suffix]
  (run-role ctx compiled bid prob suffix role))

(defn- review-decision
  "PASS/REVISE from the reviewer's verdict + answer. A reviewer that could not
  finish (not :done) or said nothing fails OPEN to :pass — a review backstop
  that can wedge the loop is worse than none. Otherwise the first line decides."
  [verdict answer]
  (if (or (not= :done verdict) (str/blank? (str answer)))
    :pass
    (let [first-line (-> (str answer) str/split-lines first str str/upper-case)]
      (if (str/includes? first-line "REVISE") :revise :pass))))

(cell/defcell :feature/redispatch
  {:doc "The revise round's re-entry point: a pass-through node whose EDGES
        re-dispatch the implement strategy. A node of its own, never a route
        back to :start — the beam driver's turn slice redirects any edge that
        returns to the entry node into :end, so a whole-run manifest that
        routes revise to :start has each revision run as a separate turn with
        a FRESH data map: :feature/revisions resets, branch ids collide, and
        the round bookkeeping is gone. Observed live (run 3b8d2af5); the
        single-branch driver the tests use carries data across that edge, so
        only a beam-driven run sees it."
   :pure true
   :requires []
   ;; A pass-through: the EDGES are the whole cell, so it requires nothing
   ;; beyond the branch and promises nothing new. The dispatch it re-runs
   ;; reads :implement-strategy, which the supervisor may have just changed.
   :input  [:map [:branch :map]]
   :output [:map]}
  (fn [_ data] data))

(cell/defcell :feature/board
  {:doc "The IMPLEMENT stage, board strategy (the default): run the board loop
        as this round's implementation — a queue of owned tasks, each claimed by
        one implementor, worked to a finish, and reviewed by a critic on the
        diff THAT task produced before it closes.

        Nested, so the board summarizes and returns rather than finishing the
        run: the feature loop still has its reviewer, its tests and its
        supervisor to run on the round as a whole. A revise round re-enters with
        the findings, and the board picks up whatever is still open — the tasks
        that landed stay closed, so a second round is the work that is left
        rather than the work again."
   :effects [:net :db]
   :requires [:config :conn :run-id :root]
   :input  [:map [:branch :map]
            [:feature/turn-budget {:optional true} :any]]
   ;; :results in the FAN-OUT's vocabulary, deliberately — the docstring says
   ;; so, and it is what lets every downstream stage work unchanged whichever
   ;; strategy implemented the round. The three shapes agreeing on :results is
   ;; the contract; declaring it here is what makes that checkable.
   ;;
   ;; The safely fallback only touches :branch, so :board/* and :results are
   ;; optional: a stage that crashed still routes on to the supervisor.
   :output [:map [:branch :map]
            [:board/landed {:optional true} :any]
            [:board/left {:optional true} :any]
            [:results {:optional true} :any]]}
  (fn [{:keys [conn run-id] :as ctx} {:keys [branch] :as data}]
    (safely conn run-id :board data
      (fn []
        (let [;; The supervisor's EXTEND lever: owners in this round run under
              ;; the extended budget. On the ctx, because the worker loop reads
              ;; its cap from ctx :max-turns.
              ctx (if-let [budget (:feature/turn-budget data)]
                    (assoc ctx :max-turns budget)
                    ctx)
              ;; Which board manifest implements the round is config, so the
              ;; BT variant (board-bt) can be A/B'd against the plain board
              ;; without touching this cell (karamazov-fut).
              board-manifest (or (get-in ctx [:config :run :board-manifest])
                                 "board")
              out (myc/run-compiled (wf/compiled-manifest board-manifest) ctx
                                    {:branch branch :turn 1
                                     :board/nested? true
                                     ;; round-scoped branch ids, and the
                                     ;; findings become tasks when the last
                                     ;; round closed everything it opened
                                     :board/round (revision data)
                                     :board/guidance (:revise/guidance data)})]
          (assoc data
                 :board/landed (:board/landed out)
                 :board/left (:board/left out)
                 ;; The shape the supervisor's telemetry digest reads
                 ;; (`:nobody-shipped` counts :done statuses in :results) —
                 ;; the board's outcome in the fan-out's result vocabulary, so
                 ;; every downstream stage works unchanged whichever strategy
                 ;; implemented the round.
                 :results (vec (concat
                                (map (fn [{:keys [task answer]}]
                                       {:status :done :subtask task :answer answer})
                                     (:board/landed out))
                                (map (fn [{:keys [task answer]}]
                                       {:status :abandoned :subtask task :answer answer})
                                     (:board/left out))))
                 :branch (assoc branch :final-answer (:answer out)))))
      (fn [d] (assoc d :branch (assoc (:branch d) :final-answer nil))))))

(cell/defcell :feature/review
  {:doc "The reviewer role: run reviewer.edn on the implementors' finished work
        (on its own branch R<rev>) and read back PASS or REVISE. Fail-open to
        :pass on a reviewer error/abstention."
   :effects [:net :db]
   :requires [:conn :run-id]
   :input  [:map [:branch :map]
            [:board/landed {:optional true} :any]
            [:board/left {:optional true} :any]]
   ;; Both keys on every path, fallback included — that is what fail-open
   ;; means here, and a downstream :feature/verify reads :review/decision
   ;; unconditionally.
   :output [:map [:review/decision :keyword] [:review/findings :any]]}
  (fn [{:keys [conn run-id] :as ctx} {:keys [branch] :as data}]
    (safely conn run-id :review data
      (fn []
        (if (or (:board/landed data) (:board/left data))
          ;; A board round: every task that closed was already reviewed by the
          ;; critic ON ITS OWN DIFF before it could close (RFC-011), so a
          ;; second 40-turn reviewer pass over the same work re-reviews what
          ;; was reviewed — and on a red tree it could not even deliver its
          ;; verdict (karamazov-t86). The round-level judge (critique) and the
          ;; tests (verify) still gate the round.
          (do (journal/note! conn run-id :review
                             {:data {:decision :pass :verdict :per-task}})
              (assoc data :review/decision :pass :review/findings ""))
        (let [prob (str "Review this feature's work.\n\nFeature:\n" (:problem branch)
                        "\n\nThe implementors reported:\n" (:final-answer branch))
              {:keys [verdict answer]}
              (try (run-role-as :reviewer (wf/role-ctx ctx :reviewer) (wf/compiled-manifest "reviewer")
                             (str "R" (revision data)) prob
                             (wf/prompt-text "roles/reviewer"))
                   (catch Throwable e {:verdict :error :answer (ex-message e)}))
              decision (review-decision verdict answer)]
          (journal/note! conn run-id :review {:data {:decision decision :verdict verdict}})
          (assoc data :review/decision decision :review/findings (str answer)))))
      ;; fail-open: a broken review does not block shipping
      (fn [d] (assoc d :review/decision :pass :review/findings "")))))

(defn- parse-args [r]
  (update r :args #(try (json/read-str (str %) :key-fn keyword)
                        (catch Throwable _ {}))))

(cell/defcell :feature/critique
  {:doc "The critic role: gate the feature result with the finalization judge —
        deterministic checks, then an LLM verdict on the answer + the run's diff
        — but WITHOUT the single-branch critic's branch surgery. Sets
        :critic/decision :ship or :revise. Fail-open (a judge that errors ships)."
   :effects [:net :db]
   :requires [:conn :git-baseline :root :run-id]
   :input  [:map [:branch :map]]
   ;; :critique/findings, not :critic/findings — the decision key is
   ;; :critic/* and the findings key is :critique/*, which is a trap worth
   ;; naming rather than tidying: :feature/route reads :critic/decision and
   ;; the digest reads :critique/findings.
   :output [:map [:critic/decision :keyword] [:critique/findings :any]]}
  (fn [{:keys [conn run-id root git-baseline] :as ctx}
       {:keys [branch] :as data}]
    (safely conn run-id :critique data
      (fn []
        (let [{:keys [llm-adapter llm-config]} (wf/role-ctx ctx :critic)
              answer (:final-answer branch)
              rows (map parse-args (journal/turns conn run-id))
              ;; Ground truth first: a done with no diff is not a completed
              ;; feature, whatever the workers claimed. This bounces before the
              ;; LLM judge is even paid for.
              det (or (when (hollow? ctx)
                        "no files were changed — the implementors called done but the working tree is unchanged, so nothing was actually built")
                      (judge/deterministic-block answer rows (tools/tool-names)))
              decision
              (if det
                :revise
                (let [diff (gitdiff/diff root git-baseline)
                      evidence (judge/evidence rows)
                      prompt (judge/critic-prompt {:rules (turn/system-prompt)
                                                   :transcript (str answer)
                                                   :evidence evidence
                                                   :diff diff
                                                   :answer answer})
                      reply (try (:content (llm/chat llm-adapter llm-config
                                                     [{:role "user" :content prompt}]))
                                 (catch Throwable _ nil))
                      verdict (if reply (judge/parse-verdict reply) :complete)
                      blocking (when reply (judge/blocking-findings reply))]
                  (if (and (= :complete verdict) (not blocking)) :ship :revise)))]
          (journal/note! conn run-id :critique
                         {:data {:decision decision :deterministic (boolean det)}})
          (assoc data :critic/decision decision :critique/findings (or det ""))))
      ;; fail-open: a broken critic ships rather than wedging the loop
      (fn [d] (assoc d :critic/decision :ship :critique/findings "")))))

(defn- parse-switch
  "A supervisor may switch this run's implement approach with a `SWITCH: <name>`
  line — the mid-run self-healing lever. Only the switchable implement strategies
  are honoured: `board` (the default — one owner per task, reviewed per task),
  `decompose` (the decompose-on-stuck loop) or `team`/`fanout` (the parallel
  fan-out). Returns the normalized strategy or nil."
  [answer]
  (when-let [m (re-find #"(?im)^\s*SWITCH:\s*([A-Za-z-]+)" (str answer))]
    (let [s (str/lower-case (second m))]
      (cond (= s "board") "board"
            (= s "decompose") "decompose"
            (#{"team" "fanout" "fan-out"} s) "team"
            :else nil))))

(defn- parse-extend
  "A supervisor may raise the per-owner turn budget for the following rounds
  with an `EXTEND: <n>` line — the lever for the failure mode where owners keep
  exhausting mid-task (orient, start the fix, run out). Clamped: an extension
  below the current default is not an extension, and an unbounded one is a
  spend policy no single supervisor turn should be able to set. Returns n or
  nil."
  [answer]
  (when-let [m (re-find #"(?im)^\s*EXTEND:\s*(\d{1,4})" (str answer))]
    (let [n (parse-long (second m))]
      (when (and n (pos? n)) (min n 200)))))

(defn- supervise-directive
  "The within-run directive from the supervisor's verdict + answer: STOP (ship
  and end), REVISE (force another round), or CONTINUE (default). A supervisor
  that could not finish or said nothing fails SAFE to :continue — it must not be
  able to wedge or hijack the loop by crashing."
  [verdict answer]
  (if (or (not= :done verdict) (str/blank? (str answer)))
    :continue
    (let [first-line (-> (str answer) str/split-lines first str str/upper-case)]
      (cond (str/includes? first-line "STOP")   :stop
            (str/includes? first-line "REVISE") :revise
            :else :continue))))

(defn- tail [s n]
  (->> (str/split-lines (str s)) (remove str/blank?) (take-last n) (str/join "\n")))

(cell/defcell :feature/verify
  {:doc "Gate 2 — run the tests. The completion criteria are two gates: gate 1 is
        that a diff exists and the review passes (hollow? + reviewer + critic);
        gate 2, here, is that the tests actually pass. Runs config :run
        :verify-cmd in the project root and passes only on exit 0. Short-circuits
        (does not pay for a test run) when gate 1 already failed — a hollow diff
        or a revise verdict means the loop is going back anyway. No :verify-cmd
        configured -> not applicable, passes."
   :effects [:proc :db]
   :requires [:config :conn :git-baseline :root :run-id]
   ;; Reads gate 1's verdicts to decide whether to pay for a test run at all,
   ;; so both are required — a manifest wiring verify without a review and a
   ;; critique in front of it would short-circuit on nils and pass by default,
   ;; which is the wrong direction for a gate.
   :input  [:map [:review/decision :keyword] [:critic/decision :keyword]]
   ;; Both keys on every branch of the cond, the note carrying WHY on the
   ;; paths where nothing ran.
   :output [:map [:verify/passed? :boolean] [:verify/note :any]]}
  (fn [{:keys [conn run-id root config] :as ctx} data]
    (let [cmd (get-in config [:run :verify-cmd])]
      (cond
        (hollow? ctx)
        (assoc data :verify/passed? false :verify/note "not run — no diff to test")

        (or (= :revise (:review/decision data)) (= :revise (:critic/decision data)))
        (assoc data :verify/passed? false :verify/note "not run — review already sent it back")

        (str/blank? (str cmd))
        (assoc data :verify/passed? true :verify/note "no :verify-cmd configured")

        :else
        (let [r (proc/run {:timeout-ms (or (get-in config [:run :verify-timeout-ms]) 600000)}
                          "sh" "-c" (str "cd " root " && " cmd))
              passed? (and (not (:timeout r)) (zero? (or (:exit r) 1)))]
          (journal/note! conn run-id :verify
                         {:data {:passed passed? :exit (:exit r) :timeout (:timeout r)}})
          (assoc data :verify/passed? passed?
                 :verify/note (cond (:timeout r) "tests TIMED OUT"
                                    passed? "tests passed"
                                    :else (str "tests FAILED (exit " (:exit r) ")\n"
                                               (tail (str (:out r) "\n" (:err r)) 25)))))))))

(cell/defcell :feature/supervise
  {:doc "The ROUTING supervisor: one look at the round, one decision about what
        the outer loop does next — CONTINUE / REVISE / STOP, plus the SWITCH
        and EXTEND levers. A pipeline decision, which is why it stays a
        pipeline node.

        IT NO LONGER OWNS DIAGNOSIS. The supervision STREAM
        (manifests/oversight.edn) watches the whole run continuously on its
        own branch and tunes the harness through the mutation protocol; this
        stage sees one round, at one moment, and a second supervisor deriving
        its own diagnosis from a different context reaches a different
        conclusion about the same run and neither knows about the other
        (karamazov-poe). So the stream's last conclusion is QUOTED into this
        stage's brief, and this stage is asked to route rather than to
        re-diagnose.

        Fails SAFE to :continue so it can never wedge the loop."
   :effects [:net :db]
   :requires [:config :conn :run-id]
   ;; The round it is routing on: what was implemented, and how the two gates
   ;; judged it. Those are the brief.
   ;; :results is OPTIONAL, and the chain check is what established why. Two
   ;; of the three implement strategies write it — :team/fan-out always,
   ;; :feature/board deliberately, in the fan-out's vocabulary so every
   ;; downstream stage works unchanged — and :decompose/run does not. So on
   ;; the decompose path this cell supervises a round it cannot see the
   ;; shipping outcome of. It degrades rather than lies: telemetry guards the
   ;; count with (pos? total), so the :nobody-shipped signal stays silent
   ;; instead of firing falsely. Filed as karamazov-u5uy.
   :input  [:map [:review/decision :keyword]
            [:critic/decision :keyword] [:verify/passed? :boolean]
            [:results {:optional true} :any]
            [:feature/errors {:optional true} :any]
            [:feature/tried {:optional true} :any]]
   ;; Only :supervisor/notes is unconditional. The LEVERS are conditional by
   ;; construction — a round where the supervisor neither switched strategy,
   ;; nor extended the budget, nor escalated, nor stopped writes none of them,
   ;; and that is the common case rather than an edge one. The safely fallback
   ;; writes nothing at all.
   :output [:map [:supervisor/notes {:optional true} :any]
            [:implement-strategy {:optional true} :any]
            [:feature/turn-budget {:optional true} :any]
            [:feature/escalate {:optional true} :boolean]
            [:feature/stop {:optional true} :boolean]]}
  (fn [{:keys [conn run-id config] :as ctx} {:keys [results] :as data}]
    (safely conn run-id :supervise data
      (fn []
        (let [soft-cap (or (get-in config [:run :max-revisions]) 6)
              rev (revision data)
              dig (telemetry/digest {:results results
                                     :review (:review/decision data)
                                     :critic (:critic/decision data)
                                     :revision rev
                                     ;; stage crashes are the first thing the
                                     ;; supervisor should see and plan around.
                                     :errors (:feature/errors data)
                                     ;; ground truth: did the working tree
                                     ;; actually change, or was done hollow?
                                     :hollow? (hollow? ctx)
                                     ;; gate 2 — did the tests pass, and if not why.
                                     :tests-passed? (:verify/passed? data)
                                     :verify-note (:verify/note data)
                                     ;; the soft cap is a notification, not a
                                     ;; verdict: at it, the supervisor decides.
                                     :at-cap? (>= rev soft-cap)
                                     :soft-cap soft-cap}
                                    (journal/turns conn run-id)
                                    ;; Per-branch gate outcomes: which steering
                                    ;; this run is doing and which a branch has
                                    ;; been ignoring. The rollup the session
                                    ;; tally keeps is per GATE, which hides a
                                    ;; gate one branch obeys and another does
                                    ;; not (karamazov-b9v).
                                    (journal/gate-firings conn run-id))
              ;; The mark the supervisor's deltas are measured from. Stamped
              ;; BEFORE it is shown anything, so `since` covers the interval
              ;; from its last intervention to now — the interval its last
              ;; change was in force for. Named per run so two runs in one
              ;; process do not read each other's deltas.
              mark (str "supervisor:" run-id)
              live (session/render mark)
              _ (session/mark! mark)
              ;; The stream's last word, so the two supervisors on this run
              ;; agree or disagree in the open rather than in parallel.
              stream (journal/last-note conn run-id :oversight)
              prob (str "Decide what the outer loop does next with this round: "
                        "CONTINUE, REVISE, or STOP, and whether to SWITCH the "
                        "implement strategy or EXTEND the turn budget.\n\n"
                        (if (and stream (seq (str (:notes stream))))
                          (str "## What the supervision stream already concluded\n"
                               "A supervisor has been watching this run continuously on its own "
                               "branch, with the whole run in view rather than one round. Its "
                               "latest conclusion:\n\n> "
                               (str/replace (str (:notes stream)) #"\n" "\n> ")
                               "\n\nStart from that. If you disagree, say what you saw that it "
                               "did not — do not re-derive the same diagnosis from scratch, and "
                               "do not tune the harness here: that is the stream's job and it "
                               "has more context to do it with.\n\n")
                          (str "No supervision-stream conclusion is on record for this run yet. "
                               "Route on what the digest below shows.\n\n"))
                        ;; Claim a crash only when one happened. The
                        ;; unconditional "a STAGE CRASHED signal is a harness
                        ;; bug — diagnose it" sentence sent a supervisor with a
                        ;; healthy run chasing a phantom crash for a whole
                        ;; branch budget (run e1491f04). Authored BY the
                        ;; harness's own supervisor via `cell save` — the first
                        ;; protocol-compliant self-edit — and folded into the
                        ;; canonical cell here.
                        (when-let [errs (seq (:feature/errors data))]
                          (str "A STAGE CRASHED signal appeared this run — a "
                               "harness bug the loop just survived — diagnose "
                               "it and, if you can, fix it at the source with "
                               "your tools.\n\nStage errors:\n- "
                               (str/join "\n- " (map str errs)) "\n\n"))
                        ;; Measured, not inferred. The digest says what the
                        ;; stages produced; this says where the turns actually
                        ;; went and whether the last change moved anything.
                        (when (seq (str live)) (str live "\n\n"))
                        dig
                        (when-let [tried (seq (:feature/tried data))]
                          (str "\n\n## Approaches already tried (do NOT repeat a losing one)\n"
                               (str/join "\n"
                                         (for [{:keys [round strategy outcome]} tried]
                                           (str "- round " round ": " strategy " → " outcome)))
                               "\nIf an approach keeps failing, try a DIFFERENT one — SWITCH the "
                               "implement strategy, tune a prompt, re-decompose — not the same thing again."))
                        ;; What this project has LEARNED, ranked by standing.
                        ;; The supervisor is the role that acts on experience,
                        ;; and it cannot act on what it is not shown — a memory
                        ;; store nobody reads is a diary, not a loop.
                        (when-let [ms (seq (knowledge/standing conn))]
                          (str "\n\n## What this project has learned\n"
                               "Ranked by standing: kind, salience, and the record of"
                               " whether acting on it worked. Report back with"
                               " `outcome {id, worked}` when you act on one — that is"
                               " the only signal that separates a memory worth keeping"
                               " from one that merely gets read.\n"
                               (str/join "\n"
                                         (for [m ms]
                                           (str "- " (:id m) " [" (:kind m) "]"
                                                (format " s%.2f" (double (or (:salience m) 0.5)))
                                                (let [w (or (:success_count m) 0)
                                                      f (or (:failure_count m) 0)]
                                                  (when (pos? (+ w f)) (str " " w "✓/" f "✗")))
                                                ;; One run is an observation, not
                                                ;; a pattern. Marked so acting on
                                                ;; a single sighting is a
                                                ;; deliberate choice.
                                                (let [c (or (:corroborations m) 1)]
                                                  (if (knowledge/corroborated? m)
                                                    (str " [seen in " c " runs]")
                                                    " [ONE RUN ONLY — not yet corroborated]"))
                                                " " (:content m))))))
                        "\n\n## Workflows you can switch to, tune, or add to\n"
                        "When the current approach keeps failing — e.g. the "
                        "implementors cannot do the task in one shot — a DIFFERENT "
                        "workflow may fit better. You can point future runs at one of "
                        "these, tune one, or author a new one with the manifest/cells "
                        "tools:\n"
                        ;; auxiliary context — a catalog hiccup must never skip
                        ;; the supervisor itself.
                        (try (wf/render-catalog conn) (catch Throwable _ "")))
              {:keys [verdict answer]}
              (try (run-role-as :supervisor (wf/role-ctx ctx :supervisor) (wf/compiled-manifest "supervisor")
                             (str "S" (revision data)) prob
                             (wf/prompt-text "roles/supervisor"))
                   (catch Throwable e {:verdict :error :answer (ex-message e)}))
              directive (supervise-directive verdict answer)
              ;; The mid-run self-healing levers: switch the implement approach,
              ;; and/or extend the per-owner turn budget for the rounds that
              ;; follow. Either implies keep-solving, so both force a revise.
              switch (parse-switch answer)
              extend (parse-extend answer)]
          (journal/note! conn run-id :supervise
                         {:data {:directive directive :verdict verdict
                                 :switch switch :extend extend}})
          (cond-> (assoc data :supervisor/notes (str answer))
            switch                (assoc :implement-strategy switch :feature/escalate true)
            extend                (assoc :feature/turn-budget extend :feature/escalate true)
            (= directive :revise) (assoc :feature/escalate true)
            (= directive :stop)   (assoc :feature/stop true))))
      ;; fail-safe: a broken supervisor lets the loop proceed unchanged
      (fn [d] d))))

(cell/defcell :feature/route
  {:doc "Decide the feature's fate. The default is to KEEP SOLVING: unless the
        work is real and verified, send it back to implement with the findings
        as guidance — the loop is an open-ended problem solver, not a one-shot.
        It SHIPS (completed) only on real, verified work (reviewer pass + critic
        ship + the working tree actually changed). It ABANDONS only when the
        SUPERVISOR gives up (STOP) after failing to find a solution — the loop is
        fully supervisor-driven, with no numeric cap that ends it. A run may opt
        into a hard runaway guard (:run :max-revisions-hard) as an unattended
        safety net, but by default there is none. Abandoning is honest, not a
        hollow ship: the run reports it did not solve the task."
   :effects [:db]
   :requires [:config :conn :run-id]
   :input  [:map [:branch :map]
            [:review/decision :keyword] [:critic/decision :keyword]
            [:verify/passed? :boolean]
            [:feature/revisions {:optional true} :int]
            [:feature/stop {:optional true} :boolean]
            [:feature/escalate {:optional true} :boolean]]
   ;; PER-TRANSITION, and this is the one that earns it. :verdict is written
   ;; on :ship and only on :ship — which is exactly right, because :ship is
   ;; the edge to :finish and :revise goes back to :redispatch. Declaring
   ;; :verdict unconditionally here would tell :loop/finish it may rely on a
   ;; key the revise round never writes.
   :output [:per-transition
            {:ship   [:map [:feature/decision :keyword] [:verdict :keyword]
                      [:branch :map]]
             :revise [:map [:feature/decision :keyword]
                      [:feature/revisions :int] [:revise/guidance :any]]}]}
  (fn [{:keys [conn run-id config] :as ctx} data]
    (let [rev (revision data)
          soft-cap (or (get-in config [:run :max-revisions]) 6)
          ;; Fully supervisor-driven by default: there is NO numeric abandon, so
          ;; the loop keeps solving until the supervisor decides to STOP. A hard
          ;; runaway guard exists only if a run explicitly opts into one
          ;; (:run :max-revisions-hard) — a safety net for unattended runs, not
          ;; the normal terminator.
          hard-cap (get-in config [:run :max-revisions-hard])
          hollow (hollow? ctx)
          ;; BOTH gates green to ship completed. Gate 1: a diff exists and it
          ;; passed review (reviewer + critic). Gate 2: the tests passed.
          pass? (and (= :pass (:review/decision data))
                     (= :ship (:critic/decision data))
                     (not (:feature/escalate data))
                     (not hollow)
                     (:verify/passed? data))
          ;; Abandon only in the extreme: the supervisor gave up (STOP), or the
          ;; runaway guard tripped. The SOFT cap does NOT abandon — it only
          ;; notifies the supervisor (via telemetry) so it decides for itself.
          give-up? (:feature/stop data)
          runaway? (and hard-cap (>= rev hard-cap))
          decision (cond pass? :ship
                         (or give-up? runaway?) :abandon
                         :else :revise)
          ;; Deterministic escalation: if the failing strategy has had its
          ;; soft-cap rounds and the supervisor didn't switch, advance the ladder
          ;; so the next round tries a DIFFERENT approach on its own.
          strategy (or (:implement-strategy data) "board")
          auto-next (when (and (= decision :revise)
                               (>= (rounds-on-strategy data strategy) soft-cap))
                      (next-strategy strategy))]
      (journal/note! conn run-id :route
                     {:data {:decision decision :revision rev :soft-cap soft-cap
                             :hard-cap hard-cap :hollow hollow :strategy strategy
                             :auto-switch auto-next
                             :tests-passed (:verify/passed? data)
                             :gave-up (boolean give-up?) :runaway runaway?}})
      (case decision
        :ship
        ;; The DECLARATION of done lives here, behind both gates — review
        ;; pass + critic ship + a real diff + green tests — not in the
        ;; fan-out join, which used to mark it unconditionally
        ;; (karamazov-blt.19).
        ;;
        ;; The ANSWER is written here, because under the beam driver the turn
        ;; slice cuts the :finish node out of a whole-run manifest and the
        ;; beam's own ending reads the branch's :final-answer — a ship whose
        ;; final round happened to land nothing carried nil there, and two
        ;; live runs (3b8d2af5, e1491f04) had their green, shipped feature
        ;; recorded as finish-run! :failed, teaching record-workflow-outcome!
        ;; that the loop never ships.
        (assoc data :feature/decision :ship :verdict :done
               :branch (let [b (:branch data)]
                         (assoc b :status :done
                                :final-answer
                                (or (not-empty (str (:final-answer b)))
                                    (str "Feature shipped after " rev " revision round(s):"
                                         " the review passed, the critic shipped it, and"
                                         " the tests passed"
                                         (when-let [note (:verify/note data)]
                                           (str " (" note ")"))
                                         ".")))))   ; -> finish, :completed

        :abandon
        ;; Honest end, not a hollow completed. Any partial work stays on disk for
        ;; a human or the next run to pick up; the run just does not claim done.
        (assoc data :feature/decision :ship :verdict :abandoned
               :branch (assoc (:branch data)
                              :status :abandoned :final-answer nil
                              :inactive-reason
                              (if give-up?
                                "supervisor could not find a solution"
                                (str "runaway guard tripped after " rev " revisions"))))

        :revise
        ;; keep solving — another implement round with the findings as guidance.
        (-> data
            (assoc :feature/decision :revise
                   :feature/revisions (inc rev)
                   :feature/escalate false
                   ;; carry the strategy forward, auto-advancing the ladder when
                   ;; the current one has run its soft-cap rounds.
                   :implement-strategy (or auto-next strategy)
                   ;; the record of what was tried and how it failed, so the
                   ;; supervisor picks something DIFFERENT next round rather than
                   ;; repeating a losing approach.
                   :feature/tried (conj (or (:feature/tried data) [])
                                        {:round rev
                                         :strategy (or (:implement-strategy data) "board")
                                         :outcome (cond hollow "changed no files"
                                                        (= :revise (:review/decision data)) "review bounced it"
                                                        (false? (:verify/passed? data)) "tests failed"
                                                        :else "not verified")})
                   :revise/guidance
                   (str/trim
                    (str (when hollow
                           "The implementors called done but changed no files — actually edit the code this round.\n\n")
                         (when (= :revise (:review/decision data))
                           (str "Reviewer asked for changes:\n"
                                (:review/findings data) "\n\n"))
                         (when (seq (:critique/findings data))
                           (str "Critic flagged:\n" (:critique/findings data) "\n\n"))
                         ;; the tests are ground truth — a failure here is the
                         ;; most actionable guidance the implementors can get.
                         (when (and (some? (:verify/passed? data))
                                    (not (:verify/passed? data))
                                    (not hollow)
                                    (not= :revise (:review/decision data)))
                           (str "The tests did not pass:\n" (:verify/note data))))))
            (dissoc :results :review/decision :critic/decision
                    :review/findings :critique/findings :verify/passed? :verify/note))))))
