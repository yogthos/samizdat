;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Decompose-on-stuck orchestration (karamazov-ioo.15). The `decompose` manifest
;; routes through :decompose/run, which drives samizdat.agent.decompose/solve
;; with the REAL ops:
;;   attempt  run an implementor worker on the unit, on its own branch, then
;;            check ground truth — did this attempt actually change files.
;;   recover  the architect LLM call on a stuck unit (decompose vs fresh-approach).
;;   fan      run the sub-unit solves, one at a time — see fan-out for why that
;;            is a decision and not a placeholder.
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

(def ^:private attempt-suffixes
  "The branch-id suffixes a unit's attempts run under: the direct build, the
  hinted retry, the assembly. Enumerated because `attempt-node` has to be able
  to NAME this unit's earlier attempts in order to take its task back off
  them."
  ["" "-h" "-a"])

(defn- unit-branch-id
  "The branch id every attempt of this unit shares."
  [node]
  (str "D" (str/replace (str (:id node)) #"[^A-Za-z0-9]" "_")))

(defn- branch-id [node]
  (str (unit-branch-id node)
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

(defn- wake-message
  "What the parked parent is told when its pieces come back: the assembly
  framing and one line per piece. A USER turn on its own tape, not a system
  prompt on a new one — the agent being woken is the agent that designed the
  boundary, and it is being told the news, not introduced to the job."
  [node]
  (str (wf/prompt-text "assembly") "\n\n"
       (str/join "\n" (map #(str "- " %) (:child-answers node)))))

(defn- attempt-node
  "Build the unit directly: run an implementor worker on its own branch, then the
  ground-truth check — did THIS attempt change files (a fresh baseline per
  attempt, so an earlier unit's edits don't count). :passed? = the worker shipped
  and the tree changed. When git can't tell, a shipped worker is trusted.

  A unit that DELEGATES parks instead of passing or failing: `split` leaves the
  branch :parked, and this reports {:split [...]} carrying the parked branch
  itself, so the recursion can hand it straight back once the pieces land.

  `:resume` is that hand-back. The attempt then continues the parked branch —
  same id, same tape, same turn counter — with the wake message appended,
  rather than opening a stranger on a fresh tape who has to rediscover why the
  boundary was drawn where it was. The architect FALLBACK does not resume: a
  unit split from outside got stuck, and its tape is a record of being stuck."
  [{:keys [conn run-id root] :as ctx} worker node]
  (let [resume (:resume node)
        parked (:branch resume)
        bid (or (:id parked) (branch-id node))
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
        ;; TAKE THE ROW BACK FROM THIS UNIT'S EARLIER ATTEMPT. Every attempt
        ;; runs on its own branch and claim! is first-writer-wins BY BRANCH
        ;; (v12), so the hinted retry and the assembly asked for a row the
        ;; direct build was still holding and got nil — opening with no task
        ;; at all. That is both halves of the delegation gone at once: the
        ;; per-turn block falls back to "No task claimed" (the nag that ate
        ;; run 938b4eb8), and ship.clj reads the contract off held-by, so the
        ;; attempt that composes every piece was judged with no contracted
        ;; tests and no unfilled-stub check.
        ;;
        ;; BY NAME, and only this unit's own attempts. release! guards on
        ;; branch_id, so naming a branch that does not hold the row is a
        ;; no-op and no sibling's claim can be touched by this — which is
        ;; what keeps it from being the general claim-stealing that v12
        ;; exists to forbid.
        _ (doseq [b (map #(str (unit-branch-id node) %) attempt-suffixes)
                  :when (not= b bid)]
            (tasks/release! conn task-id b))
        held (tasks/claim! conn task-id run-id bid)
        attempts (tasks/attempted! conn task-id)]
    (try
      ;; The unit's contract is the branch's OWN problem, durably — what a
      ;; resume rebuilds this branch's opening messages from (blt.23).
      (let [suffix (attempt-suffix node)
            ;; The row records the suffix beside the problem (v24), so a
            ;; resume or an export opens the unit on its own attempt framing.
            ;; INSERT OR IGNORE, so waking a parked branch rejoins its row
            ;; rather than rewriting how it ended.
            _ (runs/open-branch! conn run-id {:branch-id bid :problem prob :role :implementor
                                              :prompt-suffix suffix})
            b (cond-> (or (some-> parked
                                  (assoc :status :active)
                                  (dissoc :delegated :inactive-reason)
                                  (state/add-message "user" (wake-message node)))
                          (assoc (state/new-branch
                                  {:id bid :problem prob
                                   ;; Scoped and enforced, as the board's owners are.
                                   :messages (turn/initial-messages prob suffix :implementor)})
                                 :role :implementor))
                ;; Opens HOLDING its piece, so the contract and the tests it
                ;; must satisfy are pinned in its context rather than restated
                ;; once at turn zero and never again.
                held (assoc :task {:id (:id held) :title (:title held)})
                held (task-tool/task-statement held))
            ;; The attempt's own baseline reaches the worker's ship gate, so the
            ;; done tool's test rung diffs against exactly what THIS attempt
            ;; changed (a green suite with no diff of its own is not a ship).
            ;;
            ;; A woken branch picks its turn counter up where it parked, so a
            ;; unit that delegated at turn 3 of 25 assembles with the 22 it has
            ;; left rather than a fresh budget it did not earn.
            out (myc/run-compiled worker
                                  (assoc (wf/role-ctx ctx :implementor) :git-baseline base)
                                  {:branch b :turn (or (:turn resume) 1)})
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
            ;;
            ;; AND NOT THE PIECES THIS ATTEMPT IS ALREADY COMPOSING. `assemble`
            ;; names the rows it dispatched, because otherwise the assembly
            ;; attempt re-read them and reported a split instead of a result —
            ;; so `assemble` saw no :passed? and every agent-made split ended
            ;; "assembly did not land" whatever the pieces did. The architect
            ;; fallback hid it: its children are described rather than stubbed,
            ;; so the filter above already excluded them, and the fallback is
            ;; what every end-to-end test drove.
            kids (filterv #(and (seq (str/trim (str (:stubs %))))
                                (not (contains? (:assembled node) (:id %))))
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
           ;; THE PARKED BRANCH ITSELF, so the recursion can wake this exact
           ;; agent when the pieces land instead of opening a stranger with
           ;; the same name. Only when it really parked: a split found by the
           ;; board walk after a crash has no branch to hand back, and the
           ;; assembly then opens fresh, which is the old behaviour and the
           ;; right fallback.
           :branch (when (state/parked? (:branch out)) (:branch out))
           :turn (:turn out)
           :attempts attempts}
          (let [passed? (boolean (and done? (or (nil? changed) (seq changed))))]
            ;; A landed unit's row is CLOSED, the way team.clj closes a
            ;; worker's. Nothing else does it — `done` ships the branch and
            ;; leaves the board alone — so a decompose tree that landed every
            ;; piece used to end with every row still in_progress, which is
            ;; RFC-008's named worst state for a shared board: work nobody is
            ;; doing, indistinguishable from work that is progressing.
            (when passed? (tasks/close! conn task-id))
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

(defn- fan-out
  "Run the sub-unit solves, one at a time, in order.

  A `reduce` AND NOT A `mapv`, which is what this was. Each thunk is a whole
  sub-unit solve — an implementor worker branch, so `llm/chat` and a park at
  every provider call — and jolt's `mapv` is `(vec (apply map f colls))`, so
  its function runs while the lazy seq's counted lock is held, and a fiber
  cannot leave the CPU there (karamazov-p3jo, RFC-013 ADR-001). The whole
  decompose loop therefore died on its first sub-unit under ebb while the
  suite stayed green, because a test drives it from a plain thread where a
  park is only a block. base-test's ratchet could not see it either: the lazy
  body was `#(%)`, which names no parking call lexically.

  SEQUENTIAL ON PURPOSE, not merely for now. `attempt-node` takes a fresh git
  baseline per attempt and asks whether THIS attempt changed files, so
  siblings running at the same time would each be credited with the others'
  edits — a child that shipped without writing anything would pass on its
  sibling's diff, which is the exact false pass the per-attempt baseline
  exists to catch. A parallel fan wants ground truth scoped to the piece's own
  contract first; `cells/team.clj` is the join to copy when it is."
  [thunks]
  (reduce (fn [acc thunk] (conj acc (thunk))) [] thunks))

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
               :fan fan-out}
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
