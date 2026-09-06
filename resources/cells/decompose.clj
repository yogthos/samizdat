;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Decompose-on-stuck orchestration (karamazov-ioo.15). The `decompose` manifest
;; routes through :decompose/run, which drives samizdat.agent.decompose/solve
;; with the REAL ops:
;;   attempt  run an implementor worker on the unit, on its own branch, then
;;            check ground truth — did this attempt actually change files.
;;   recover  the architect LLM call on a stuck unit (decompose vs fresh-approach).
;;   fan      run the sub-unit solves (sequential for now; the reference defaults
;;            to one-at-a-time too).
;; A landed unit changed the tree and its worker shipped; a stuck one is split
;; and its children solved first, then the parent re-attempted as the assembly.
(ns cells.decompose
  (:require [clojure.string :as str]
            [mycelium.cell :as cell]
            [mycelium.core :as myc]
            [samizdat.agent.decompose :as dec]
            [samizdat.agent.gitdiff :as gitdiff]
            [samizdat.agent.loop :as turn]
            [samizdat.agent.skills :as skills]
            [samizdat.agent.state :as state]
            [samizdat.llm.client :as llm]
            [samizdat.agent.tools.tasks :as task-tool]
            [samizdat.store.journal :as journal]
            [samizdat.store.tasks :as tasks]
            [samizdat.store.runs :as runs]
            [samizdat.workflow :as wf]))

(defn- branch-id [node]
  (str "D" (str/replace (str (:id node)) #"[^A-Za-z0-9]" "_")
       (cond (:assembly node) "-a" (:hint node) "-h" :else "")))

(defn- title-of
  "A task title from a unit's problem text: one line, bounded. Same shape the
  board and the team use — a title is an index entry, and the contract is what
  a worker actually reads."
  [s]
  (let [t (str/trim (str/replace (str s) #"\s+" " "))]
    (if (> (count t) 100) (str (subs t 0 100) "…") t)))

(defn- attempt-suffix
  "The prompt an implementor gets for one unit: its role, the REPL/TDD workflow,
  and — on a fresh-approach retry or an assembly — the extra steer."
  [node]
  (str/join "\n\n"
            (remove str/blank?
                    [(wf/prompt-text "roles/implementor")
                     (skills/load-skill "repl-workflow")
                     (when (:hint node)
                       (str "## A prior attempt got stuck. Try this different approach:\n"
                            (:hint node)))
                     (when (:assembly node)
                       (wf/prompt-text "assembly"))
                     (when (:assembly node)
                       (str/join "\n" (map #(str "- " %) (:child-answers node))))])))

(defn- attempt-node
  "Build the unit directly: run an implementor worker on its own branch, then the
  ground-truth check — did THIS attempt change files (a fresh baseline per
  attempt, so an earlier unit's edits don't count). :passed? = the worker shipped
  and the tree changed. When git can't tell, a shipped worker is trusted."
  [{:keys [conn run-id root] :as ctx} worker node]
  (let [bid (branch-id node)
        base (gitdiff/baseline root)
        prob (:problem node)
        ;; EVERY UNIT HOLDS A TASK, and that is what makes the rest work. The
        ;; `split` tool hangs the pieces off the task the agent holds; the ship
        ;; gate reads the contract's tests and stubs off it; the attempt count
        ;; lives on it. A unit that held nothing was also told "No task
        ;; claimed" on every turn — the instruction that ate the turns in run
        ;; 938b4eb8, pointing at a board the decompose path never wrote to.
        ;; A delegated piece arrives with its row; the root mints one.
        task-id (or (:task-id node)
                    (tasks/create! conn {:title (title-of prob) :body (str prob)
                                         :contract (str (:contract node))
                                         ;; An architect-made unit hangs off
                                         ;; the row its parent holds, so the
                                         ;; fallback path records the same
                                         ;; nesting the split path does.
                                         :parent-id (:parent-task node)
                                         :run-id run-id}))
        held (tasks/claim! conn task-id run-id bid)
        attempts (tasks/attempted! conn task-id)]
    (try
      ;; The unit's contract is the branch's OWN problem, durably — what a
      ;; resume rebuilds this branch's opening messages from (blt.23).
      (runs/open-branch! conn run-id {:branch-id bid :problem prob})
      (let [b (cond-> (assoc (state/new-branch
                              {:id bid :problem prob
                               ;; Scoped and enforced, as the board's owners are.
                               :messages (turn/initial-messages prob (attempt-suffix node)
                                                                :implementor)})
                             :role :implementor)
                ;; Opens HOLDING its piece, so the contract and the tests it
                ;; must satisfy are pinned in its context rather than restated
                ;; once at turn zero and never again.
                held (assoc :task {:id (:id held) :title (:title held)})
                held (task-tool/task-statement held))
            ;; The attempt's own baseline reaches the worker's ship gate, so the
            ;; done tool's test rung diffs against exactly what THIS attempt
            ;; changed (a green suite with no diff of its own is not a ship).
            out (myc/run-compiled worker
                                  (assoc (wf/role-ctx ctx :implementor) :git-baseline base)
                                  {:branch b :turn 1})
            done? (= :done (:verdict out))
            changed (gitdiff/changed-files root base)
            ;; DID IT DELEGATE? The agent splits by writing stubs and calling
            ;; the split tool, which verified them against the tree and turned
            ;; them into child rows under this unit's task. Walking them is how
            ;; the recursion finds the pieces — the tool reports nothing out of
            ;; band, so a split that survived a crash is still found here.
            ;;
            ;; ONLY ROWS THE SPLIT TOOL WROTE COUNT, and `stubs` is the
            ;; evidence: it is the one column split sets and nothing else does
            ;; (board and team write contract and tests, never this). Reading
            ;; every child as a delegation let an agent bypass the whole
            ;; verification with `task create {parentId}` — a child with no
            ;; stubs behind it, which is exactly the unchecked hand-off the
            ;; split tool exists to refuse. Found by run 3b3ce405, where the
            ;; agent did create a loose task and only the missing parent kept
            ;; it from being read as a split.
            kids (filterv #(seq (str/trim (str (:stubs %))))
                          (tasks/children-of conn task-id))]
        (if (seq kids)
          {:task-id task-id
           :split (mapv (fn [k]
                          {:id (str (:id node) "/" (:title k))
                           :name (:title k)
                           :problem (:body k)
                           :contract (:contract k)
                           :task-id (:id k)
                           :parent (:id node)})
                        kids)
           :attempts attempts}
          (let [passed? (and done? (or (nil? changed) (seq changed)))]
            {:passed? passed?
             :task-id task-id
             :answer (get-in out [:branch :final-answer])
             :attempts attempts
             :failure (when-not passed?
                        (if done? "the worker shipped but changed no files"
                            "the worker did not finish"))})))
      (catch Throwable e
        {:passed? false :attempts attempts :task-id task-id
         :failure (str "attempt crashed: " (ex-message e))}))))

(defn- recover-node
  "The architect call on a stuck unit: decompose vs fresh-approach, from the
  evidence. Fails soft to nil (no recovery -> the unit fails, honestly)."
  [{:keys [conn run-id] :as ctx} node evidence]
  (let [{:keys [llm-adapter llm-config]} (wf/role-ctx ctx :architect)
        reply (try (:content (llm/chat llm-adapter llm-config
                                       [{:role "user"
                                         :content (dec/architect-prompt node evidence)}]))
                   (catch Throwable _ nil))
        decision (dec/parse-decision reply (:depth evidence))]
    (journal/note! conn run-id :architect
                   {:data {:node (:id node) :depth (:depth evidence)
                           :decision (:kind decision)
                           :subtasks (mapv :name (:subtasks decision))}})
    decision))

(defn- summarize [result]
  (letfn [(line [r ind]
            (str (apply str (repeat ind "  "))
                 "- " (name (:status r)) " " (get-in r [:node :id])
                 (when-let [a (:answer r)] (str " — " (subs (str a) 0 (min 80 (count (str a))))))
                 (apply str (for [c (:children r)] (str "\n" (line c (inc ind)))))))]
    (str "Decompose-on-stuck result:\n" (line result 0))))

(defn- unit-results
  "The decompose tree flattened into the FAN-OUT's per-owner vocabulary, one
  entry per unit, so a decompose round describes itself the way a board or
  team round does (karamazov-u5uy).

  EVERY unit, not just the root. The root is one attempt among several — it
  is `:landed` only once its children landed and the assembly passed — so
  counting it alone would report a round that landed three of four pieces as
  having shipped nothing. The tree is the same one `summarize` walks."
  [result]
  (letfn [(walk [r]
            (cons {:status (if (= :landed (:status r)) :done :abandoned)
                   :subtask (get-in r [:node :id])
                   :answer (:answer r)}
                  (mapcat walk (:children r))))]
    (vec (walk result))))

(cell/defcell :decompose/run
  {:doc "Solve the branch's problem by decompose-on-stuck: attempt it directly;
        when a unit is stuck, split it (architect) and solve the sub-units first,
        then assemble. Landed => the manager branch ships the tree; failed =>
        abandoned honestly."
   :effects [:net :db]
   :requires [:conn :run-id]
   :input  [:map [:branch :map]]
   ;; :verdict is the key :loop/finish routes on. This cell is one of the
   ;; four routers that produce it outside :loop/route — declaring it here is
   ;; part of what lets :loop/finish require it (karamazov-6y7.3).
   :output [:map [:verdict :keyword] [:branch :map]]}
  (fn [{:keys [conn run-id] :as ctx} {:keys [branch] :as data}]
    (let [worker (wf/worker-compiled)
          root {:id "T" :problem (:problem branch)}
          ops {:attempt (fn [node] (attempt-node ctx worker node))
               :recover (fn [node evidence] (recover-node ctx node evidence))
               :fan (fn [thunks] (mapv #(%) thunks))}
          result (dec/solve root 0 ops)
          landed? (= :landed (:status result))]
      (journal/note! conn run-id :decompose
                     {:data {:status (:status result)
                             :children (count (:children result))}})
      (journal/note! conn run-id :implement-round
                     {:data {:strategy "decompose"
                             :revision (:feature/revisions data 0)
                             :results (unit-results result)}})
      (assoc data
             :verdict (if landed? :done :abandoned)
             :branch (assoc branch
                            :status (if landed? :done :abandoned)
                            :final-answer (when landed? (summarize result))
                            :inactive-reason (when-not landed? (:reason result)))))))
