;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.agent.decompose
  "Decompose-on-stuck (karamazov-ioo.15). A task is attempted DIRECTLY first;
  only when it cannot pass its contract after a bounded number of tries is it
  split. 'Too big' is discovered as 'can't pass its tests', not estimated up
  front — the evidence-based approach code-graph-agent uses, and the one that
  turns 'a weak model can't do this' into 'it can do these three small pieces'.

  When a unit is stuck, an ARCHITECT call chooses:
    - :decompose      the unit does several distinct things — split into 2..N
                      single-responsibility sub-units (each becomes its own
                      task with a signature + description; its implementor writes
                      its own tests, TDD). The parent then becomes a thin
                      assembly that composes them, verified against its OWN,
                      preserved tests.
    - :fresh-approach the unit does one thing but the implementation keeps
                      missing — a hint to try a different angle, no split.

  Pure here — the architect prompt and the decision parsing; the orchestration
  (attempt, recurse, assemble, depth cap) lives in cells/decompose.clj. Same
  split as planner.clj vs cells/team.clj."
  (:require ;; the java.time.* host shim, before data.json — see samizdat.store.journal
            [jolt.time]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [samizdat.agent.gates :as gates]
            [samizdat.prompt :as prompt]))

;; Tier 1b: both budgets are gates.edn data (:decompose-max-depth,
;; :decompose-max-parts) — cost ceilings, since each level of recursion
;; multiplies sub-agents.

(defn max-depth
  "How deep the split recursion may go before a stuck unit is a hard failure
  rather than split again. Kept shallow: each level multiplies sub-agents, and a
  unit that still won't pass its tests three levels down is not a size problem.

  A FUNCTION: a top-level def froze the gates.edn value at namespace load —
  the exact bug generation-cache fixed everywhere else — so a retune (or the
  project's own policy after bind) never took effect (blt.38)."
  []
  (gates/threshold :decompose-max-depth))

(defn default-max-parts [] (gates/threshold :decompose-max-parts))

(defn architect-prompt
  "Ask the architect to diagnose a stuck unit and choose to split it or hint a
  fresh approach. Fed the EVIDENCE — the contract, the tests, the last attempt,
  the failure, how many tries and how deep — so the choice is grounded in why it
  failed, not a guess about its size. The skeleton is
  resources/prompts/architect.md (tier 2c), rendered by selmer; an absent
  section is an empty ctx value and collapses cleanly."
  [{:keys [problem contract tests]}
   {:keys [attempts last-answer last-failure depth fresh-failed force-split]}]
  ;; Values only. Every heading and every sentence of framing that used to be
  ;; assembled here is in prompts/architect.md now, behind its own {% if %} —
  ;; an architect prompt half in a template and half in `str` calls is a
  ;; prompt a supervisor can only edit half of.
  (prompt/render "architect"
    {:attempts (or attempts "several")
     :problem (str problem)
     :contract (not-empty (str/trim (str contract)))
     :tests (not-empty (str/trim (str tests)))
     :last-answer (not-empty (str/trim (str last-answer)))
     :last-failure (not-empty (str/trim (str last-failure)))
     :force-split (boolean (or fresh-failed force-split))
     :max-parts (default-max-parts)
     :last-round (>= (or depth 0) (dec (max-depth)))}))

(defn- normalize-subtask [m]
  (let [name (some-> (or (get m "name") (get m :name)) str str/trim not-empty)
        desc (some-> (or (get m "description") (get m :description)) str str/trim not-empty)]
    (when (and name desc) {:name name :description desc})))

(defn child-node
  "A sub-unit node from the parent and an architect subtask spec. Its id encodes
  lineage; its problem is the subtask's one-paragraph contract.

  `:parent-task` carries the PARENT'S task id, not its node id, so the row the
  child mints hangs off the row the parent holds. Without it the fallback
  path's task tree was flat — every architect-made unit an orphan — while the
  split path's was properly nested, so the same run recorded its work two
  different ways depending on which path produced a unit (run 3b3ce405)."
  [parent {:keys [name description]}]
  {:id (str (:id parent) "/" name)
   :name name
   :problem description
   :parent (:id parent)
   :parent-task (:task-id parent)})

(defn- can-split?
  "Whether a unit at this depth may still be decomposed. Below the budget a stuck
  unit is split smaller; at or past it, splitting would only spawn children the
  budget forbids, so the unit fails honestly instead."
  [depth max-d]
  (< depth max-d))

(defn generic-split
  "The last-resort split, used when the architect will not (or cannot) give a
  usable decomposition but the unit is still stuck and we can still go smaller.
  Almost every stuck unit here is 'make a change and prove it', so split it into
  the change and the test that pins it. Imperfect, but it keeps the invariant the
  whole design rests on: a stuck unit is decomposed further, never abandoned
  while there is still room to split."
  [node]
  {:kind :decompose
   :reason "generic split (architect gave no usable decomposition)"
   :subtasks [{:name "impl"
               :description (str "Make the smallest change that satisfies this unit, then stop:\n"
                                 (:problem node))}
              {:name "spec"
               :description (str "Add a focused test that proves the behaviour this unit requires:\n"
                                 (:problem node))}]})

(declare solve)

(defn- assemble
  "Solve `children` (recursively, so a stuck sub-unit splits again), then — if
  they all land — re-attempt the parent as the assembly that composes them,
  judged against the parent's OWN tests.

  The parent may ADJUST what the pieces delivered at this step. Seeing them
  compose is the first time anyone can tell whether the boundary was right, so
  assembly is where the design gets checked rather than only where the glue
  goes (prompts/assembly.md). What it must not do is discard a piece that met
  its contract.

  THIS IS THE WAIT, and `resume` is what makes it a wait rather than a
  replacement. A parent that delegated is parked, not finished; passing its
  parked branch back means the agent that designed the boundary is the agent
  that composes it, on the tape where it drew the boundary. Nil on the
  architect path — a unit split from outside was stuck when it stopped, and
  resuming it would resume the confusion; that one starts fresh."
  [node depth {:keys [attempt fan] :as ops} children resume]
  (let [results (fan (mapv (fn [c] #(solve c (inc depth) ops)) children))]
    (if-not (every? #(= :landed (:status %)) results)
      {:status :failed :reason "a sub-unit did not land" :node node :children results}
      ;; `:assembled` names the rows the pieces are on, so the assembly
      ;; attempt composes them instead of rediscovering them as a delegation
      ;; it has just made. Empty on the architect path, whose children are
      ;; described rather than stubbed and are filtered out anyway.
      (let [asm (attempt (cond-> (assoc node :assembly true
                                        :child-answers (mapv :answer results)
                                        :assembled (into #{} (keep :task-id children)))
                           resume (assoc :resume resume)))]
        (cond
          (:passed? asm)
          {:status :landed :answer (:answer asm) :node node :children results}

          ;; A second-generation split. `solve` has no path to honour one — the
          ;; pieces are already built and this node is the composition — so say
          ;; that rather than reporting it as an ordinary miss.
          (seq (:split asm))
          {:status :failed :reason "assembly split again"
           :node node :children results}

          :else
          {:status :failed :reason "assembly did not land" :node node :children results})))))

(defn- decompose-node
  "Assemble from an ARCHITECT's decision rather than the agent's own split.

  This is the recovery path: a unit that got stuck, could not be talked into a
  different approach, and is being broken up from outside. Its children are
  described rather than stubbed — the architect has no tools and cannot write
  code — so they carry a paragraph where a delegated piece carries a signature.
  That is a weaker contract, and it is why this is the fallback and the agent's
  own `split` is the ordinary path."
  [node depth ops decision]
  (assemble node depth ops (mapv #(child-node node %) (:subtasks decision)) nil))

(defn solve
  "Recursive decompose-on-stuck for one node. Pure control flow over injected
  ops, so the recursion is testable without a model or a worker; cells/decompose
  provides the real ops.

  ops:
    :attempt   (fn [node] -> {:passed? bool :answer .. :failure ..}) — build the
               unit directly (worker loop) and verify it against its tests.
    :recover   (fn [node evidence] -> decision|nil) — the architect call on a
               stuck unit (evidence carries :last-answer :last-failure :depth,
               and :fresh-failed/:force-split when a hinted retry already failed).
    :fan       (fn [thunks] -> results) — run sub-unit solves (parallel or not).
    :max-depth  optional override of the split-recursion budget (default max-depth).

  The governing rule (karamazov-dvz): a stuck unit is NEVER abandoned while it
  can still be split. Fresh-approach is a first, cheap try; when it fails the
  unit is decomposed — forced through the architect, or a generic split as a last
  resort — recursively, until pieces land or the depth floor is genuinely hit.

  Returns {:status :landed|:failed :answer .. :node .. :children [..]}. A landed
  node is a stable subassembly: it passed its own tests, so a parent that
  composes it never re-litigates it."
  [node depth {:keys [attempt recover fan max-depth] :as ops}]
  (let [max-d (or max-depth (samizdat.agent.decompose/max-depth))
        r (attempt node)
        ;; The attempt is what learns this unit's task id — the root's row is
        ;; minted when it is first tried — so the node only knows it afterwards.
        ;; Threading it back is what lets child-node point a sub-unit's row at
        ;; its parent's.
        node (cond-> node (:task-id r) (assoc :task-id (:task-id r)))]
    (cond
      ;; THE AGENT SPLIT ITS OWN TASK. It wrote the stubs, the harness verified
      ;; them against the tree, and the child tasks exist — so there is nothing
      ;; to diagnose and no architect to ask. This is the recursion's ordinary
      ;; path, not its recovery path: a unit that split was never stuck.
      ;; The parked branch travels with the split, so the assembly wakes this
      ;; agent rather than opening a new one under its name. `attempt` reports
      ;; no branch when the split was recovered from the board after a crash,
      ;; and the assembly then opens fresh.
      (seq (:split r))
      (assemble node depth ops (:split r)
                (when (:branch r) {:branch (:branch r) :turn (:turn r)}))

      (:passed? r) {:status :landed :answer (:answer r) :node node}

      :else
      (let [ev {:last-answer (:answer r) :last-failure (:failure r) :depth depth
                ;; From the TASK ROW (v21), not a counter in this process. A
                ;; resumed run picks up the tally its predecessor left, so a
                ;; unit on its fourth try is diagnosed as one — the architect
                ;; is told how many times this has been attempted, and that
                ;; number used to reset to zero on every crash.
                :attempts (:attempts r)}
            decision (recover node ev)]
        (case (:kind decision)
          ;; The architect wants a split. Honour it if the budget allows; at the
          ;; floor there is no room for children, so it fails honestly.
          :decompose
          (if (can-split? depth max-d)
            (decompose-node node depth ops decision)
            {:status :failed :reason "depth exhausted" :node node :answer (:answer r)})

          ;; 'One thing, wrong strategy' — try the hint once. If it lands, done.
          ;; If it still misses, the unit is NOT abandoned: escalate to a split
          ;; (re-ask the architect, now told the hint failed; a generic split if
          ;; it still refuses). Only at the depth floor is a failed retry the end.
          :fresh-approach
          (let [r2 (attempt (assoc node :hint (:hint decision)))]
            (cond
              (:passed? r2)
              {:status :landed :answer (:answer r2) :node node}

              (not (can-split? depth max-d))
              {:status :failed :reason "depth exhausted; fresh approach did not land"
               :node node :answer (:answer r2)}

              :else
              (let [ev2 (assoc ev :last-answer (:answer r2) :last-failure (:failure r2)
                               :fresh-failed true :force-split true)
                    forced (recover node ev2)
                    split (if (= :decompose (:kind forced)) forced (generic-split node))]
                (decompose-node node depth ops split))))

          ;; No usable decision. Don't abandon while we can still go smaller —
          ;; generic split; at the floor, honest failure.
          (if (can-split? depth max-d)
            (decompose-node node depth ops (generic-split node))
            {:status :failed :reason "no recovery at depth floor"
             :node node :answer (:answer r)}))))))

(defn parse-decision
  "The architect's decision from its reply. Returns
    {:kind :decompose :reason .. :subtasks [{:name :description} ...]}
    {:kind :fresh-approach :reason .. :hint ..}
  or nil when the reply carries no usable JSON decision (the caller then treats
  a nil as 'no recovery' and fails the unit — better than guessing). `depth` >=
  max-depth-1 forces :fresh-approach even if the model asked to split, so the
  depth budget is honoured whatever the model returns."
  ([reply] (parse-decision reply 0))
  ([reply depth]
   (let [m (try (json/read-str (or (re-find #"(?s)\{.*\}" (str reply)) ""))
                (catch Throwable _ nil))]
     (when (map? m)
       (let [decision (some-> (get m "decision") str str/lower-case str/trim)
             reason (some-> (get m "reason") str)
             subtasks (->> (get m "subtasks") (keep normalize-subtask) vec)
             hint (some-> (get m "hint") str str/trim not-empty)
             ;; the depth guard: at the budget's edge a split is not allowed, so
             ;; a decompose degrades to a fresh approach.
             want-split? (and (= decision "decompose")
                              (seq subtasks)
                              (< depth (dec (max-depth))))]
         (cond
           want-split? {:kind :decompose :reason reason :subtasks subtasks}
           (or hint (= decision "fresh_approach"))
           {:kind :fresh-approach :reason reason
            :hint (or hint "Try a different implementation strategy.")}
           ;; a decompose we cannot honour (too deep, or no subtasks) still means
           ;; "one thing, wrong strategy" — degrade to a fresh approach.
           (= decision "decompose") {:kind :fresh-approach :reason reason
                                     :hint "Try a different implementation strategy."}
           :else nil))))))
