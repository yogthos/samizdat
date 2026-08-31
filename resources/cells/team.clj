;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Multi-agent fan-out. Not wired into any single-branch loop; the `team`
;; manifest routes through it. Given a vector of sub-tasks, it runs a WORKER
;; sub-loop per sub-task — in parallel, each its own branch on the SHARED run,
;; so the workers share one WORKING TREE, the run's mailbox (message tool), and
;; its artifact/failure pool. It joins their answers into the manager branch's
;; final answer.
;;
;; SHARING THE TREE IS THE DESIGN, not a hazard to be isolated away: the parts
;; of a feature belong in the same files, and two workers in one file is them
;; collaborating. What they lacked was any way to SEE each other. The mailbox
;; carries what a worker chose to announce; nothing carried what it did. Live,
;; three workers wrote src/kit/core.clj fifteen times between them, full-file
;; `write_file` overwrites interleaved with surgical `edit_file`s, two landing
;; on the same turn — and the tree came out coherent only because the last
;; writer happened to hold a complete picture.
;;
;; So there are now three channels, and only the first is self-reported:
;;   mailbox        what a peer says it is doing (message tool)
;;   shared tree    which files peers have actually changed, from the journal,
;;                  in every worker's context block every turn
;;   stale write    when a write lands on a file a peer moved since this
;;                  worker last read it, the result says who and when
;; The last two are ground truth and cost no turn to produce.
;;
;; This is the dataflow fan-out shape (futures + deref, workers run to
;; completion): coordination is between-turn, not a live actor. The
;; escapement-style parked-conversation actor (per-turn peer steering) is
;; karamazov-oy1, a later, larger step.
(ns cells.team
  (:require [clojure.string :as str]
            [mycelium.cell :as cell]
            [mycelium.core :as myc]
            [samizdat.agent.loop :as turn]
            [samizdat.agent.planner :as planner]
            [samizdat.agent.skills :as skills]
            [samizdat.agent.state :as state]
            [samizdat.llm.client :as llm]
            [samizdat.store.journal :as journal]
            [samizdat.prompt :as prompt]
            [samizdat.store.runs :as runs]
            [samizdat.store.tasks :as tasks]
            [samizdat.workflow :as wf]))

(defn- summarize [results]
  (str "Team of " (count results) " workers:\n"
       (str/join "\n"
                 (for [{:keys [worker subtask status answer]} results]
                   (str "- W" worker " [" (name (or status :unknown)) "] " subtask
                        (when answer (str "\n    → " answer)))))))

(defn- roster
  "The team-context prompt suffix for worker `idx`: which worker it is, its part,
  and every peer's part (so it knows who to coordinate with), followed by the
  shared team-worker guide. Only meaningful for a real team — a solo worker gets
  no suffix (see worker-prompt)."
  [idx tasks]
  (str "## Your team\n\n"
       "You are worker W" idx " of " (count tasks) ". Your part: " (nth tasks idx)
       "\n\nThe team and their parts:\n"
       (str/join "\n"
                 (map-indexed (fn [i t] (str "- W" i (when (= i idx) " (you)") ": " t))
                              tasks))
       "\n\n"
       (or (wf/prompt-text "team-worker") "")))

(defn- worker-prompt
  "The prompt suffix for implementor worker `idx`: its implementor role identity,
  the repl-workflow skill (implementors get it in-context — REPL development is
  their core method, and it is where they must be told the file on disk is the
  deliverable, not the eval), plus a peer roster + coordination guide when it is
  one of several (>1 task). A solo worker still gets the role identity + skill."
  [idx tasks]
  (let [role (wf/prompt-text "roles/implementor")
        repl (skills/load-skill "repl-workflow")
        base [role repl]]
    (str/join "\n\n"
              (remove str/blank?
                      (if (> (count tasks) 1) (conj base (roster idx tasks)) base)))))

(defn- run-worker
  "Run one worker sub-loop as branch `bid` on the shared run: `prob` is the
  branch's problem (a sub-task, possibly with revise guidance appended), `st` is
  the bare sub-task kept for the result label, `suffix` the role prompt. A throw
  becomes an :error result rather than taking the whole fan-out down — one
  worker's crash is not the team's."
  [{:keys [conn run-id] :as ctx} worker bid idx st prob suffix task-id]
  (try
    ;; The sub-task is the branch's OWN problem, durably — what a resume
    ;; rebuilds this branch's opening messages from (blt.23).
    (runs/open-branch! conn run-id {:branch-id bid :problem prob})
    (let [b (assoc (state/new-branch
                    {:id bid :problem prob
                     ;; Scoped and enforced, as the board's owners are — a
                     ;; fan-out worker is an implementor by another name.
                     :messages (turn/initial-messages prob suffix :implementor)})
                   :role :implementor)
          ;; The worker opens HOLDING its part of the board. Claimed here rather
          ;; than left to the worker to claim itself, because the split already
          ;; decided who does what — asking each worker to go and claim the row
          ;; the planner made for it is a turn spent on bookkeeping, and a
          ;; worker that forgot would work untracked.
          ;;
          ;; The claim is per-BRANCH (migration v12). Under the old per-run
          ;; guard every worker on this fan-out would have claimed successfully
          ;; and all of them would have believed they held it.
          claimed (when task-id (tasks/claim! conn task-id run-id bid))
          b (if claimed
              (-> b
                  (assoc :task {:id (:id claimed) :title (:title claimed)})
                  (state/add-message
                   "user"
                   (str "[harness] " (prompt/render "task-claimed"
                                       {:id (:id claimed) :title (:title claimed)
                                        :contract (:contract claimed)
                                        :tests (:tests claimed)}))
                   {:pinned? true :task-id (:id claimed)}))
              b)
          out (myc/run-compiled worker ctx {:branch b :turn 1})
          done? (= :done (:verdict out))]
      ;; The board reflects what happened: a part that shipped is closed, a
      ;; part that did not goes back so the supervisor can re-task it rather
      ;; than it sitting attributed to a worker that has stopped.
      (when claimed
        (if done?
          (tasks/close! conn (:id claimed))
          (tasks/release! conn (:id claimed) bid)))
      {:worker idx :subtask st :branch bid :task (:id claimed)
       :status (:verdict out)
       :answer (get-in out [:branch :final-answer])})
    (catch Throwable e
      (when task-id (try (tasks/release! conn task-id bid) (catch Throwable _ nil)))
      {:worker idx :subtask st :branch bid :task task-id :status :error
       :answer (str "worker failed: " (ex-message e))})))

(defn- ok?
  "A worker result that landed a shippable answer. Anything else — :abandoned
  (gave up), :exhausted (turn cap), :error (crash) — is a part the supervisor
  may re-task."
  [r]
  (= :done (:status r)))

(cell/defcell :team/plan
  {:doc "Split the manager branch's problem into independent sub-tasks for the
        team to fan out over. If sub-tasks were already provided (config
        :run :subtasks), pass through — an explicit split wins. Otherwise one
        LLM call proposes at most :max-subtasks parts; fail-soft to a single
        worker on the whole problem when the call fails or yields no list."
   :effects [:net :db]
   :requires [:config :conn :run-id]
   ;; :subtasks optional in and guaranteed out — that IS the cell. It arrives
   ;; already set (config :run :subtasks) and passes through, or it does not
   ;; and one call produces it; either way what leaves has it.
   :input  [:map [:branch :map] [:subtasks {:optional true} :any]]
   :output [:map [:subtasks :any]]}
  (fn [{:keys [conn run-id config] :as ctx}
       {:keys [branch subtasks] :as data}]
    (if (seq subtasks)
      data
      (let [{:keys [llm-adapter llm-config]} (wf/role-ctx ctx :planner)
            max-parts (or (get-in config [:run :max-subtasks])
                          (planner/default-max-parts))
            reply (try (:content (llm/chat llm-adapter llm-config
                                           [{:role "user"
                                             :content (planner/plan-prompt
                                                       (:problem branch) max-parts)}]))
                       (catch Throwable _ nil))
            parts (planner/parse-plan reply max-parts)
            tasks (or parts [(:problem branch)])]
        (journal/note! conn run-id :plan
                       {:data {:planned (count tasks) :split (boolean parts)}})
        (assoc data :subtasks tasks)))))

(cell/defcell :team/fan-out
  {:doc "Run a worker sub-loop per sub-task, in parallel, each its own branch on
        the shared run, sharing one working tree and coordinating through the
        mailbox, the shared-tree block and the stale-write notice). Join their
        answers into the manager branch and finish. A dataflow join, not a live
        actor: workers run to completion."
   :effects [:net :db]
   :requires [:config :conn :run-id]
   ;; :subtasks stays optional even though :team/plan always produces it —
   ;; the handler falls back to the whole problem, and a manifest is free to
   ;; wire this cell without a planner in front of it.
   ;; :revise/guidance and :feature/revisions arrive only on a feature loop's
   ;; second and later rounds, which is why neither is required.
   :input  [:map [:branch :map]
            [:subtasks {:optional true} :any]
            [:revise/guidance {:optional true} :any]
            [:feature/revisions {:optional true} :int]]
   :output [:map [:subtasks :any] [:results :any]
            [:team/epic :any] [:team/task-ids :any] [:branch :map]]}
  (fn [{:keys [conn run-id] :as ctx} {:keys [branch subtasks] :as data}]
    (let [tasks (vec (if (seq subtasks) subtasks [(:problem branch)]))
          worker (wf/worker-compiled)
          ;; Implementors may run on their own assigned model (config :run
          ;; :role-models :implementor) — a cheap one, say, while the reviewer
          ;; and supervisor run on a stronger one.
          ictx (wf/role-ctx ctx :implementor)
          ;; When the feature loop sends a round back, :revise/guidance carries
          ;; the reviewer/critic findings and :feature/revisions bumps, so retry
          ;; branches (W<i>v<rev>) do not collide with the earlier round's.
          guidance (:revise/guidance data)
          rev (or (:feature/revisions data) 0)
          prob-of (fn [s] (if (str/blank? (str guidance))
                            s
                            (str s "\n\nA prior review sent this back. Address:\n"
                                 guidance)))
          bid-of (fn [i] (str "W" i (when (pos? rev) (str "v" rev))))
          ;; One board row per part, created BEFORE the fan-out so the split is
          ;; visible as work rather than only as five branches. The manager and
          ;; the supervisor read the same board the workers hold, which is what
          ;; makes "which parts are still open" a query instead of an inference
          ;; from branch statuses.
          ;;
          ;; parent-id ties them to the feature: the board shows the shape of
          ;; the split, not five unrelated tasks.
          ;; Titles are BOUNDED. A task title is a board line, and the
          ;; problem statement can be pages — an unbounded title makes the
          ;; board unreadable at exactly the moment it matters most, which is
          ;; when several parts are in flight. The full text lives in :body and
          ;; :contract, which is what a worker reads.
          title-of (fn [s] (let [t (str/trim (str/replace (str s) #"\s+" " "))]
                             (if (> (count t) 100) (str (subs t 0 100) "…") t)))
          ;; Created ONCE. A revise round re-enters this cell with the same
          ;; parts; re-running create! each round left one epic and one full
          ;; set of duplicate rows per revision, with the prior rounds'
          ;; released rows still open — making "which parts are still open",
          ;; the board's whole purpose, unanswerable (karamazov-blt.36). The
          ;; ids ride the data map; a retry re-claims the SAME row the first
          ;; attempt released.
          parent (or (:team/epic data)
                     (when (seq subtasks)
                       (tasks/create! conn {:title (str "Feature: " (title-of (:problem branch)))
                                            :body (str (:problem branch))
                                            :type "epic" :run-id run-id})))
          task-ids (or (not-empty (:team/task-ids data))
                       (mapv (fn [s]
                               (tasks/create! conn {:title (title-of s)
                                                    :body (str s)
                                                    :parent-id parent
                                                    :run-id run-id
                                                    :contract (str s)}))
                             tasks))
          results (->> (map-indexed vector tasks)
                       (mapv (fn [[i s]]
                               (future (run-worker ictx worker (bid-of i) i s
                                                   (prob-of s) (worker-prompt i tasks)
                                                   (nth task-ids i nil)))))
                       (mapv deref))]
      (journal/note! conn run-id :team
                     {:data {:workers (count results) :revision rev
                             :epic parent
                             :tasks (vec (remove nil? task-ids))
                             :done (count (filter ok? results))}})
      ;; The JOIN lands on the branch — the feature loop's review/critique/
      ;; verify gates read it — but no verdict and no :done here: marking the
      ;; run done unconditionally at this point meant a team where every
      ;; worker failed still finished :completed with a summary of failures
      ;; as its answer (karamazov-blt.19), upstream of the false-completion
      ;; memories in karamazov-mjb. In team.edn the verdict is
      ;; :team/supervise's (after its retries); in feature.edn it is
      ;; :feature/route's, behind both gates.
      (assoc data
             :subtasks tasks
             :results (vec results)
             :team/epic parent
             :team/task-ids task-ids
             :branch (assoc branch :final-answer (summarize results))))))

(cell/defcell :team/supervise
  {:doc "Watch the fan-out's results and re-task the parts that did not land: a
        worker that gave up, hit the turn cap, or crashed gets one more run on a
        fresh branch (W<idx>r1). The retry replaces the original only if it does
        better. A bounded re-task, not an open loop — the supervisor's job is to
        catch a stalled part, not to grind. Re-joins the answers after."
   :effects [:net :db]
   :requires [:conn :run-id]
   ;; :results is REQUIRED and is the point — this cell exists to re-task the
   ;; parts that did not land, so a manifest wiring it without a fan-out in
   ;; front has nothing to supervise.
   :input  [:map [:branch :map] [:results :any] [:subtasks :any]]
   ;; :verdict either way, decided AFTER the retries from what actually
   ;; landed. It is what :loop/finish routes on, and declaring it here is part
   ;; of what lets that input become required.
   :output [:map [:results :any] [:verdict :keyword] [:branch :map]]}
  (fn [{:keys [conn run-id] :as ctx} {:keys [branch results subtasks] :as data}]
    (let [tasks (vec subtasks)
          worker (wf/worker-compiled)
          ictx (wf/role-ctx ctx :implementor)
          retried (mapv (fn [r]
                          (if (ok? r)
                            r
                            (let [i (:worker r)
                                  st (:subtask r)
                                  ;; The retry re-claims the SAME board row the
                                  ;; first attempt released, so a re-tasked part
                                  ;; keeps its identity and its contract rather
                                  ;; than becoming a second task for the same
                                  ;; work.
                                  r2 (run-worker ictx worker (str "W" i "r1") i
                                                 st st (worker-prompt i tasks)
                                                 (:task r))]
                              (if (ok? r2) r2 r))))
                        results)
          fixed (count (filter (fn [[a b]] (and (not (ok? a)) (ok? b)))
                               (map vector results retried)))]
      (journal/note! conn run-id :supervise
                     {:data {:retried (count (remove ok? results)) :fixed fixed}})
      ;; The verdict, decided AFTER the retries, from what actually landed. A
      ;; team where nothing landed — retries included — ends :abandoned with
      ;; no answer, so :loop/finish records an abandoned run rather than a
      ;; completed one (karamazov-blt.19).
      (if (some ok? retried)
        (assoc data
               :results retried
               :verdict :done
               :branch (assoc branch :status :done
                              :final-answer (summarize retried)))
        (assoc data
               :results retried
               :verdict :abandoned
               ;; :final-answer cleared explicitly — the fan-out's join put
               ;; the failure summary there for the reviewer's benefit, and
               ;; :loop/finish reads a non-blank answer as a completion.
               :branch (assoc branch :status :abandoned
                              :final-answer nil
                              :inactive-reason
                              "every worker failed, retries included"))))))
