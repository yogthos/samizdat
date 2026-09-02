;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; The BOARD loop: collaboration through OWNED TASKS rather than simultaneous
;; workers. This is what the fan-out (cells/team.clj) turned into after the
;; todomvc dogfood run, where the fan-out's costs showed up plainly:
;;
;;   - the split was invented by a planner LLM call and belonged to nobody: a
;;     reasoning preamble was parsed as the task list and four workers spent a
;;     budget each on fragments of the planner's musing (karamazov-6a3);
;;   - four workers in one tree spent their turns negotiating over the same
;;     files, discovering each other's hardcoded ports and half-written
;;     namespaces, and re-deriving the same facts in parallel;
;;   - nothing was reviewed until the whole round ended, so a defect found at
;;     the end was attributed to a round rather than to the change that caused
;;     it, and the review's guidance came back as one blob for four workers.
;;
;; What actually worked there was the BOARD: one task per branch, exclusive
;; claim, statement pinned, closed by its holder. So the board becomes the
;; workflow rather than the bookkeeping beside it:
;;
;;   next   take the next workable task — a leaf, oldest first — and give it
;;          ONE owner, on its own branch, with a git baseline taken at claim
;;   work   that owner runs the implementor loop until it finishes or gives up.
;;          A task that is really several is split BY ITS OWNER at claim time
;;          (prompts/task-claimed.md asks the question); the children land on
;;          the board and are worked in turn, each with its own owner
;;   review  a critic reads THE DIFF THAT TASK PRODUCED — its own baseline, not
;;          the run's — and either closes it or sends it back to the same task
;;          with the findings
;;
;; Work is still shared and still concurrentable at the task level (the claim
;; is per-branch and exclusive, migration v12); what is gone is several agents
;; editing one tree with no owner for any of it.
(ns cells.board
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [mycelium.cell :as cell]
            [mycelium.core :as myc]
            [samizdat.agent.gates :as gates]
            [samizdat.agent.gitdiff :as gitdiff]
            [samizdat.agent.judge :as judge]
            [samizdat.agent.loop :as turn]
            [samizdat.agent.skills :as skills]
            [samizdat.agent.state :as state]
            [samizdat.agent.tools :as tools]
            [samizdat.llm.client :as llm]
            [samizdat.prompt :as prompt]
            [samizdat.store.db :as db]
            [samizdat.store.journal :as journal]
            [samizdat.store.runs :as runs]
            [samizdat.store.tasks :as tasks]
            [samizdat.workflow :as wf]))

(defn- max-review-attempts
  "How many times a critic may send one task back to its owner before the loop
  moves on and leaves it open. A task nobody can land must not hold the board.
  gates.edn :board-review-attempts."
  []
  (gates/threshold :board-review-attempts))

(defn- max-tasks
  "The runaway guard on an open-ended board: owners may split their tasks into
  more tasks, so nothing else bounds the loop. gates.edn :board-max-tasks."
  []
  (gates/threshold :board-max-tasks))

(defn- title-of
  "A board line, not a problem statement. The full text lives in :body."
  [s]
  (let [t (str/trim (str/replace (str s) #"\s+" " "))]
    (if (> (count t) 100) (str (subs t 0 100) "…") t)))

(defn- open? [t] (not (contains? #{"done" "cancelled"} (:status t))))

(defn- board-tree?
  "Whether a task belongs to the board's own tree: its parent chain ends at a
  run-scoped root of type \"feature\" — a root board/plan opened. Role branches
  create run-scoped tasks too (the supervisor's housekeeping, a reviewer's
  notes-to-self; the task tool practically requires them) and those are not
  the board's to hand to a feature implementor, which is how a live run
  assigned 'Diagnose STAGE CRASHED harness bug' to an owner meant to be
  building todo handlers (run e1491f04)."
  [conn run-id t]
  (loop [cur t, depth 0]
    (cond
      (nil? cur) false
      (> depth 16) false
      (nil? (:parent_id cur)) (and (= "feature" (:type cur))
                                   (= run-id (:run_id cur)))
      :else (recur (tasks/get-task conn (:parent_id cur)) (inc depth)))))

(defn- release-stale-claims!
  "Release every claim on this run held by a branch that is no longer active.
  A claim is exclusive while its holder works; a holder that exhausted or
  crashed keeps nothing, or the task it held is stranded — claimed forever,
  invisible to an unclaimed-only board, the run's own work lost to it."
  [conn run-id]
  (doseq [t (tasks/board conn {:run-id run-id})
          :when (and (= "in_progress" (:status t)) (:branch_id t))
          :let [b (db/fetch-one conn ["SELECT status FROM branches
                                       WHERE run_id = ? AND id = ?"
                                      run-id (:branch_id t)])]
          :when (not= "active" (:status b))]
    (journal/note! conn run-id :board-release
                   {:data {:task (:id t) :from (:branch_id t)
                           :why (if b "branch finished" "no such branch")}})
    (try (tasks/release! conn (:id t) (:branch_id t)) (catch Throwable _ nil))))

(defn- workable
  "The tasks the board may hand an owner right now: UNCLAIMED (status open —
  what another branch holds is that branch's business), in the board's OWN
  TREE (see board-tree?) or the unclaimed human backlog, and LEAVES — a task
  whose children are still open is a container for work that has its own
  owners. Oldest first, so a split is worked in the order it was written."
  [conn run-id]
  (let [rows (tasks/board conn {:run-id run-id})
        open-child? (fn [t] (some open? (tasks/children-of conn (:id t))))]
    (->> rows
         (filter #(= "open" (:status %)))
         (filter #(or (nil? (:run_id %)) (board-tree? conn run-id %)))
         (remove open-child?)
         (sort-by (juxt :created_at :id))
         vec)))

(defn- closable-parents!
  "Close every task whose children are all done. A parent is finished when its
  parts are; leaving it open would keep the board reporting work that no
  longer exists."
  [conn run-id]
  (doseq [t (tasks/board conn {:run-id run-id})
          :let [kids (tasks/children-of conn (:id t))]
          :when (and (seq kids) (not-any? open? kids) (open? t))]
    (tasks/close! conn (:id t))))

(cell/defcell :board/plan
  {:doc "Make sure the board has work. An existing board is left alone — a
        revise round picks up what is still open rather than duplicating it.
        Otherwise the work comes from config :run :subtasks when the caller
        supplied a split, else ONE task from the run's problem.

        No planner call either way: a task that is really several is split by
        its OWNER at claim time, once it has read the code
        (prompts/task-claimed.md). Splitting before anyone has looked at the
        tree is how a planner's reasoning preamble became four workers' task
        list (karamazov-6a3)."
   :effects [:db]
   :requires [:config :conn :run-id]
   ;; :board/guidance arrives only on a revise round from the outer feature
   ;; loop, so it is optional; the board's own rounds carry it forward.
   :input  [:map [:branch :map]
            [:subtasks {:optional true} :any]
            [:board/guidance {:optional true} :any]]
   :output [:map [:board/planned :int]]}
  (fn [{:keys [conn config run-id]} {:keys [branch] :as data}]
    (let [existing (workable conn run-id)
          ;; WHAT DEFINES DELIVERY, on every task the board opens.
          ;;
          ;; The field existed, the schema had it, task-claimed.md renders it
          ;; under "TESTS — what defines delivery", the task tool documents it
          ;; as half the delegation spec, and the same template tells an owner
          ;; to set it on any subtask IT creates. The board set :title, :body,
          ;; :contract and :type and never this one, so the block — which is
          ;; `{% if tests %}` — silently rendered as nothing on every task the
          ;; harness ever handed anybody.
          ;;
          ;; That matters because of WHERE it lands. A claimed task's statement
          ;; is pinned in the owner's context and does not age out (RFC-004),
          ;; so this is the one place a delivery requirement sits beside the
          ;; work for the whole task. The same instruction exists in
          ;; roles/implementor and the repl-workflow skill — measured at
          ;; character 34,720 of a 39,595-character system message, behind
          ;; 30KB of tool catalogue. Run f2014821 wrote five namespaces and
          ;; zero tests across 124 turns with both of those in its prompt.
          ;;
          ;; Prose, so a project working in a language the seam paragraph does
          ;; not fit can reword it without a rebuild.
          tests (wf/prompt-text "task-tests")
          guidance (str (:board/guidance data))
          given (->> (or (:subtasks data) (get-in config [:run :subtasks]))
                     (remove #(str/blank? (str %)))
                     vec)
          [from specs]
          (cond
            (seq existing) [:existing nil]

            ;; A review sent the round back and every task is closed: the
            ;; findings ARE the work now, so they get a row and an owner like
            ;; any other work. Without this a revise round would find an empty
            ;; board, land nothing, and bounce again on the same findings.
            (not (str/blank? guidance))
            [:findings [{:title (title-of (str "Address the review findings: " guidance))
                         :body guidance
                         :contract guidance
                         :tests tests
                         :type "feature"}]]

            (seq given)
            [:subtasks (mapv (fn [s] {:title (title-of s) :body (str s)
                                      :contract (str s) :tests tests
                                      :type "feature"})
                             given)]

            :else
            (let [prob (str (:problem branch))]
              [:problem [{:title (title-of prob) :body prob :contract prob
                          :tests tests :type "feature"}]]))]
      (if (nil? specs)
        (assoc data :board/planned (count existing))
        (let [ids (mapv #(tasks/create! conn (assoc % :run-id run-id)) specs)]
          (journal/note! conn run-id :board {:data {:opened ids :from from}})
          (assoc data :board/planned (count ids)))))))

(cell/defcell :board/next
  {:doc "Take the next workable task and give it one owner: claim it to a fresh
        branch, pin its statement, and stamp a git baseline so the review can
        see what THIS task changed. Verdict :empty when the board is clear, when
        everything left is a task this run already gave up on, or when the
        runaway guard trips."
   :effects [:db]
   :requires [:conn :run-id :root]
   ;; Everything it reads is its own bookkeeping from an earlier lap, so all
   ;; of it is optional: the first pass through the board has none of it.
   ;; :branch is REQUIRED and was missed: the handler destructures it and
   ;; writes (assoc branch :task ...) on the claim path, so without one it
   ;; would build a fresh {:task ...} map in the branch's place — silently,
   ;; which is the nil-six-cells-downstream failure this epic exists to refuse.
   :input  [:map [:branch :map]
            [:board/left {:optional true} :any]
            [:board/worked {:optional true} :int]
            [:board/round {:optional true} :int]]
   ;; PER-TRANSITION. Claiming a task stamps the whole working set — the id,
   ;; the branch, the statement, the baseline the review diffs against — and
   ;; deliberately CLEARS the previous task's outcome. An empty board writes
   ;; only the verdict, so declaring the working set on both edges would tell
   ;; :board/finish it may rely on a task that was never claimed.
   :output [:per-transition
            {:task  [:map [:board/verdict :keyword] [:board/task :any]
                     [:board/branch-id :any] [:board/problem :any]
                     [:board/attempts :int] [:board/findings :any]
                     [:board/outcome :any] [:board/decision :any]
                     [:board/answer :any] [:board/baseline :any]
                     [:branch :map]]
             :empty [:map [:board/verdict :keyword]]}]}
  (fn [{:keys [conn run-id root]} {:keys [branch] :as data}]
    (release-stale-claims! conn run-id)
    (closable-parents! conn run-id)
    ;; A given-up task is RELEASED back to the board — that is what makes it an
    ;; honest record of work still to do — which means the board would hand it
    ;; straight back to this same loop, forever. What this run has already
    ;; failed at is not workable BY THIS RUN.
    (let [tried (set (map :task (:board/left data)))
          worked (or (:board/worked data) 0)
          queue (when (< worked (max-tasks))
                  (remove #(contains? tried (:id %)) (workable conn run-id)))]
    (if-let [t (first queue)]
      ;; Round-scoped, like the fan-out's W<i>v<rev>: the feature loop runs the
      ;; board again on a revise round, and a second T0 would append this
      ;; round's turns to the last round's branch.
      (let [n worked
            round (or (:board/round data) 0)
            ;; The id NAMES THE TASK. It used to be the owner index alone, so
            ;; every task that owner ever worked shared one id — run 8710067f
            ;; ran turns 1-153 under "T0" across two tasks with two separate
            ;; fresh contexts, and nothing downstream could tell them apart.
            bid (state/branch-id-for n round (:title t))
            prob (str (or (not-empty (str (:body t))) (:title t)))
            claimed (do (runs/open-branch! conn run-id {:branch-id bid :problem prob})
                        (tasks/claim! conn (:id t) run-id bid))]
        (journal/note! conn run-id :board-task
                       {:branch-id bid :data {:task (:id t) :title (:title t)}})
        (assoc data
               :board/task (:id t)
               :board/branch-id bid
               :board/problem prob
               :board/attempts 0
               ;; the previous task's review has nothing to say about this one
               :board/findings nil
               ;; The previous task's outcome and verdict are cleared with it.
               ;; The plain manifest never reads them again (its edges carry
               ;; the position), but the BT variant's :board/sense re-derives
               ;; position from these keys every tick, and a stale outcome
               ;; would read as work awaiting review (karamazov-fut).
               :board/outcome nil
               :board/decision nil
               :board/answer nil
               ;; The baseline for THIS task, taken now: the review reads the
               ;; diff its owner produced, not the run's accumulated one.
               :board/baseline (gitdiff/baseline root)
               :board/verdict :task
               :branch (assoc branch :task {:id (:id t) :title (:title t)})))
      (assoc data :board/verdict :empty)))))

(defn surface-block
  "THE SURFACE a task sits in: the overarching goal and the sibling parts, one
  line each with who is on them. nil when there are no siblings — a heading
  over an empty list is a per-turn tax on the common case.

  A task owner used to get its own contract and nothing about the whole, so it
  could not tell which interfaces were shared, what another owner was already
  building, or what its part was FOR. Metan (research/2608.24735v1) ablated
  exactly this channel and put the plain conditioning string passed between
  layers at ~72% of what recursion buys — the cheapest thing there is, and
  this had none of it. A live worker went off-task onto a superficially
  similar recalled fix for want of it (karamazov-b3z).

  DELIBERATELY THIN: titles and status, never contracts or bodies. The parent
  holds the surface and each child holds its own implementation; handing a
  child its siblings' contracts would put us back to everybody reading
  everything, which is the thing the layering exists to avoid."
  [{:keys [goal siblings mine]}]
  (let [others (remove #(= (str (:id %)) (str mine)) siblings)]
    (when (seq others)
      (str "## Where this fits\n\n"
           (when (seq (str goal))
             (str "The whole this serves: " goal "\n\n"))
           "Other parts of it, so you know what NOT to build and which "
           "interfaces are shared:\n"
           (str/join "\n"
                     (for [t others]
                       (str "  - " (:title t) " [" (:status t) "]")))
           "\n\nThose are somebody else's to build. If your part needs one of "
           "them, define the interface and keep going — do not build theirs."))))

(defn- owner-prompt
  "The implementor's prompt suffix: its role identity and the repl-workflow
  skill (the file on disk is the deliverable, not the eval)."
  []
  (str/join "\n\n"
            (remove str/blank?
                    [(wf/prompt-text "roles/implementor")
                     (skills/load-skill "repl-workflow")])))

(cell/defcell :board/work
  {:doc "Run the implementor loop on the claimed task, on its own branch, until
        it finishes or gives up. On a re-attempt the critic's findings are
        appended to the task's problem, so the owner works the same task again
        knowing what was wrong with the last try."
   :effects [:net :db]
   :requires [:config :conn :run-id]
   ;; The claimed task, and OPTIONAL only because the two board manifests
   ;; guarantee it by different means. board.edn edges :next -> :work, so the
   ;; claim structurally precedes the work and its :must-precede invariant
   ;; says so. board-bt.edn dispatches :sense -> :work directly, and the
   ;; guarantee there is DYNAMIC: sense answers :work-due only when a claim is
   ;; already on the blackboard. mycelium walks edges, so it can check the
   ;; first and not the second — requiring these keys refuses board-bt for a
   ;; claim its dispatcher will not route without.
   :input  [:map [:board/task {:optional true} :any]
            [:board/problem {:optional true} :any]
            [:board/attempts {:optional true} :int]
            [:board/findings {:optional true} :any]]
   :output [:map [:board/branch-id :any] [:board/task :any]
            [:board/outcome :any] [:board/decision :any]
            [:board/answer :any]]}
  (fn [{:keys [conn run-id] :as ctx} {:keys [board/task board/branch-id] :as data}]
    (let [attempt (or (:board/attempts data) 0)
          bid (if (pos? attempt) (str branch-id "r" attempt) branch-id)
          findings (:board/findings data)
          prob (cond-> (str (:board/problem data))
                 (not (str/blank? (str findings)))
                 (str "\n\nA review of your last attempt sent this back."
                      " Address it:\n" findings))
          t (tasks/get-task conn task)
          ictx (wf/role-ctx ctx :implementor)
          out (try
                (when (pos? attempt)
                  (runs/open-branch! conn run-id {:branch-id bid :problem prob})
                  (tasks/claim! conn task run-id bid))
                (let [b (-> (state/new-branch
                             {:id bid :problem prob
                              ;; ROLE-SCOPED, and role-ENFORCED. The board is
                              ;; the default implement stage and it passed
                              ;; neither: the owner was shown every tool the
                              ;; harness has (roles/scope-catalogue never ran)
                              ;; and could call any of them (:outside-role-surface
                              ;; needs a :role on the branch to refuse). The
                              ;; machinery roles.edn describes was wired into
                              ;; feature.clj's advisory roles only, so the one
                              ;; role that writes code was the one role without
                              ;; a scoped world.
                              :messages (turn/initial-messages prob (owner-prompt)
                                                               :implementor)})
                            (assoc :task {:id task :title (:title t)}
                                   :role :implementor)
                            (state/add-message
                             "user"
                             (str "[harness] "
                                  (prompt/render "task-claimed"
                                                 {:id task :title (:title t)
                                                  :contract (:contract t)
                                                  :tests (:tests t)
                                                  ;; WHERE THIS FITS. The
                                                  ;; parent holds the surface;
                                                  ;; the child gets its own
                                                  ;; contract plus a one-line
                                                  ;; map of the rest, never
                                                  ;; the siblings' contracts
                                                  ;; (karamazov-b3z).
                                                  :surface
                                                  (surface-block
                                                   {:goal (:problem data)
                                                    :mine task
                                                    :siblings (tasks/board conn {:run-id run-id})})}))
                             {:pinned? true :task-id task}))]
                  (myc/run-compiled (wf/worker-compiled) ictx {:branch b :turn 1}))
                (catch Throwable e
                  {:verdict :error
                   :branch {:id bid :final-answer (str "owner failed: " (ex-message e))}}))
          ;; What the branch ACTUALLY held at the end. The claim prompt tells a
          ;; composite task's owner to split and SWITCH to the first child, and
          ;; reviewing the task the board handed out then judges the untouched
          ;; parent while the child the owner worked stays claimed to a dead
          ;; branch (karamazov-bf2). The row is the truth; follow it.
          held (try (tasks/held-by conn run-id bid) (catch Throwable _ nil))
          switched? (and held (not= (:id held) task))]
      (when switched?
        (journal/note! conn run-id :board-switch
                       {:branch-id bid :data {:from task :to (:id held)}}))
      (assoc data
             :board/branch-id bid
             :board/task (if switched? (:id held) task)
             :board/outcome (:verdict out)
             ;; A fresh outcome is unjudged by definition; clearing the last
             ;; review's verdict here is what lets :board/sense (the BT
             ;; variant) read outcome-with-no-decision as review-due.
             :board/decision nil
             :board/answer (get-in out [:branch :final-answer])
             :branch (:branch out)))))

(cell/defcell :board/review
  {:doc "The critic reads the diff THIS task produced and either closes it or
        sends it back to the same owner with the findings. A task whose owner
        did not finish is never closed — it goes back to the board open, which
        is the honest record of what is left."
   :effects [:net :db]
   :requires [:conn :root :run-id :llm-adapter :llm-config]
   ;; :board/outcome and :board/baseline are what make this a review OF a
   ;; change rather than of a claim — the diff is taken against the baseline
   ;; :board/next stamped at claim time. Optional for the same reason
   ;; :board/work's are: board-bt reaches this node from its sense dispatcher,
   ;; which routes :review-due only when an unjudged outcome exists, and that
   ;; is a precondition query rather than an edge mycelium can walk.
   :input  [:map [:board/task {:optional true} :any]
            [:board/outcome {:optional true} :any]
            [:board/answer {:optional true} :any]
            [:board/baseline {:optional true} :any]
            [:board/branch-id {:optional true} :any]
            [:board/attempts {:optional true} :int]]
   :output [:map [:board/decision :keyword] [:board/attempts :int]
            [:board/findings :any] [:board/worked :int]
            [:board/landed :any] [:board/left :any]]}
  (fn [{:keys [conn run-id root] :as ctx} {:keys [board/task] :as data}]
    (let [{:keys [llm-adapter llm-config]} (wf/role-ctx ctx :critic)
          attempts (inc (or (:board/attempts data) 0))
          landed? (= :done (:board/outcome data))
          answer (str (:board/answer data))
          diff (gitdiff/diff root (:board/baseline data))
          ;; PARSED args, exactly as the critic loop hands them over: the
          ;; judge's evidence predicates read (get-in row [:args :command]),
          ;; and rows carrying raw JSON strings made "no test was run" true of
          ;; every run — a green, verified task got deterministically bounced
          ;; twice and left open (run c2260271).
          rows (map (fn [r]
                      (update r :args
                              #(try (json/read-str (str %) :key-fn keyword)
                                    (catch Throwable _ {}))))
                    (journal/turns conn run-id))
          ;; The cheap deterministic checks first — an answer claiming a test
          ;; ran when none did needs no judge and no provider call. A judge that
          ;; THROWS must not take the board down with it: the review is a gate
          ;; on the work, not part of doing it, and a broken gate that ends the
          ;; run is worse than no gate (the reasoning behind feature.clj's
          ;; `safely`, which this cell sits inside when nested).
          det (when landed?
                (try (judge/deterministic-block answer rows (tools/tool-names))
                     (catch Throwable _ nil)))
          reply (when (and landed? (not det))
                  (try (:content (llm/chat llm-adapter llm-config
                                           [{:role "user"
                                             :content (judge/critic-prompt
                                                       {:rules (turn/system-prompt)
                                                        :transcript answer
                                                        :evidence (judge/evidence rows)
                                                        :diff diff
                                                        :answer answer})}]))
                       (catch Throwable _ nil)))
          verdict (cond (not landed?) :unfinished
                        det :deterministic
                        (nil? reply) :complete ; fail-open: a broken judge ships
                        :else (try (judge/parse-verdict reply)
                                   (catch Throwable _ :complete)))
          blocking (when reply (try (judge/blocking-findings reply)
                                    (catch Throwable _ nil)))
          pass? (and landed?
                     (nil? det)
                     (= :complete verdict)
                     (not blocking))
          ;; Out of attempts: stop paying for the same task. It stays OPEN —
          ;; a task the board still shows is a truer record than one closed
          ;; because the loop got tired of it.
          spent? (>= attempts (max-review-attempts))
          decision (cond pass? :pass
                         (or (not landed?) spent?) :give-up
                         :else :revise)]
      (journal/note! conn run-id :board-review
                     {:data {:task task :attempt attempts :verdict verdict
                             :decision decision :landed (boolean landed?)}})
      (when pass? (tasks/close! conn task))
      (when (= :give-up decision)
        ;; back to the board, unattributed, so the next round or a human sees
        ;; it as work still to do rather than as somebody's abandoned claim
        (try (tasks/release! conn task (:board/branch-id data)) (catch Throwable _ nil)))
      (assoc data
             :board/decision decision
             :board/attempts attempts
             :board/findings (when (= :revise decision)
                               (or det
                                   blocking
                                   (judge/critique-message verdict (judge/findings reply))))
             ;; counts TASKS finished with, not attempts — a re-attempt is the
             ;; same task, and the runaway guard is about board size
             :board/worked (cond-> (or (:board/worked data) 0)
                             (not= :revise decision) inc)
             :board/landed (cond-> (or (:board/landed data) [])
                             pass? (conj {:task task :answer answer}))
             :board/left (cond-> (or (:board/left data) [])
                           (= :give-up decision) (conj {:task task :answer answer}))))))

(cell/defcell :board/sense
  {:doc "The BT variant's one decision point (karamazov-fut, Kelley arXiv
        2404.07439 Appendix A.2, the implicit sequence): re-derive the board
        loop's position from the blackboard and the tasks table EVERY tick,
        instead of latching it in the state machine's edges. Preconditions
        are queried in reverse — the most downstream applicable action wins —
        so an action whose precondition stopped holding stops firing, and
        'keep doing step A because a counter says so' is structurally
        impossible.

        The order IS the policy:
          1. an outcome with no verdict  -> review   (unjudged work exists)
          2. a revise verdict            -> work     (same owner, findings)
          3. a fresh unworked claim      -> work
          4. the board refused a claim   -> finish   (:board/next said :empty)
          5. a workable task exists      -> claim
          6. otherwise                   -> finish

        The root postcondition (board clear) deliberately sits BELOW review:
        checking done-ness first would finish the round past the final task's
        unreviewed diff, and 'nothing reaches done without a critic reading
        that change' outranks reactivity. Reading order 1 depends on
        :board/next and :board/work clearing the previous task's outcome and
        decision — the blackboard hygiene those cells now do for this cell."
   :effects [:db]
   :requires [:conn :run-id]
   ;; Everything OPTIONAL, and that is the behaviour-tree design rather than
   ;; laxity: this cell re-derives the position from the blackboard on EVERY
   ;; tick instead of latching it in edges, so it has to cope with any of
   ;; these being absent — the first tick, where none of them exist yet, most
   ;; of all.
   :input  [:map [:board/outcome {:optional true} :any]
            [:board/decision {:optional true} :any]
            [:board/verdict {:optional true} :any]]
   :output [:map [:board/sense :keyword]]}
  (fn [{:keys [conn run-id]} data]
    (let [outcome (:board/outcome data)
          decision (:board/decision data)
          state (cond
                  (and outcome (nil? decision)) :review-due
                  (= :revise decision) :work-due
                  (and (= :task (:board/verdict data)) (nil? outcome)) :work-due
                  (= :empty (:board/verdict data)) :done
                  (seq (workable conn run-id)) :claim-due
                  :else :done)]
      (assoc data :board/sense state))))

(cell/defcell :board/finish
  {:doc "End on what the board says: every task closed is a completed run whose
        answer is what the owners landed; anything left open is an honest
        partial, because the work the run was given is still on the board.

        NESTED (:board/nested? — the board running as the feature loop's
        implement stage) it only summarizes: the run belongs to the outer loop,
        which still has its review, its tests and its supervisor to run, and a
        stage that closed the run row would end the feature at its own stage."
   :effects [:db]
   :requires [:conn :run-id]
   ;; Both optional: a board that never claimed anything reaches :finish
   ;; straight from :next's :empty edge, with neither list written.
   :input  [:map [:branch :map]
            [:board/landed {:optional true} :any]
            [:board/left {:optional true} :any]]
   :output [:map [:status :keyword] [:answer :any]]}
  (fn [{:keys [conn run-id]} {:keys [branch] :as data}]
    (closable-parents! conn run-id)
    (let [landed (or (:board/landed data) [])
          left (or (:board/left data) [])
          still-open (workable conn run-id)
          summary (str (count landed) " task(s) landed"
                       (when (seq left) (str ", " (count left) " left open"))
                       ":\n"
                       (str/join "\n" (map (fn [{:keys [task answer]}]
                                             (str "- " task ": " answer))
                                           landed)))
          done? (and (seq landed) (empty? left) (empty? still-open))
          status (if done? :completed :abandoned)
          branch' (assoc branch :final-answer (when (seq landed) summary))]
      (journal/note! conn run-id :board-finish
                     {:data {:landed (count landed) :left (count left)
                             :open (count still-open)
                             :nested (boolean (:board/nested? data))}})
      (when-not (:board/nested? data)
        (runs/close-branch! conn run-id (:id branch')
                            (if done? :done :abandoned) nil)
        (runs/finish-run! conn run-id status (:final-answer branch')))
      (assoc data :status status :answer (:final-answer branch')
             :verdict (if done? :done :abandoned)
             :branch branch'))))
