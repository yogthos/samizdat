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
            [samizdat.store.journal :as journal]
            [samizdat.store.runs :as runs]
            [samizdat.workflow :as wf]))

(defn- branch-id [node]
  (str "D" (str/replace (str (:id node)) #"[^A-Za-z0-9]" "_")
       (cond (:assembly node) "-a" (:hint node) "-h" :else "")))

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
                       (str "## This is an ASSEMBLY step\n"
                            "The sub-units below are already built and tested. Your job is the "
                            "small piece that composes them to satisfy this unit — do not rebuild "
                            "them.\n\nSub-units delivered:\n"
                            (str/join "\n" (map #(str "- " %) (:child-answers node)))))])))

(defn- attempt-node
  "Build the unit directly: run an implementor worker on its own branch, then the
  ground-truth check — did THIS attempt change files (a fresh baseline per
  attempt, so an earlier unit's edits don't count). :passed? = the worker shipped
  and the tree changed. When git can't tell, a shipped worker is trusted."
  [{:keys [conn run-id root] :as ctx} worker node]
  (let [bid (branch-id node)
        base (gitdiff/baseline root)
        prob (:problem node)]
    (try
      ;; The unit's contract is the branch's OWN problem, durably — what a
      ;; resume rebuilds this branch's opening messages from (blt.23).
      (runs/open-branch! conn run-id {:branch-id bid :problem prob})
      (let [b (assoc (state/new-branch
                      {:id bid :problem prob
                       ;; Scoped and enforced, as the board's owners are.
                       :messages (turn/initial-messages prob (attempt-suffix node)
                                                        :implementor)})
                     :role :implementor)
            ;; The attempt's own baseline reaches the worker's ship gate, so the
            ;; done tool's test rung diffs against exactly what THIS attempt
            ;; changed (a green suite with no diff of its own is not a ship).
            out (myc/run-compiled worker
                                  (assoc (wf/role-ctx ctx :implementor) :git-baseline base)
                                  {:branch b :turn 1})
            done? (= :done (:verdict out))
            changed (gitdiff/changed-files root base)
            passed? (and done? (or (nil? changed) (seq changed)))]
        {:passed? passed?
         :answer (get-in out [:branch :final-answer])
         :failure (when-not passed?
                    (if done? "the worker shipped but changed no files"
                        "the worker did not finish"))})
      (catch Throwable e
        {:passed? false :failure (str "attempt crashed: " (ex-message e))}))))

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
      (assoc data
             :verdict (if landed? :done :abandoned)
             :branch (assoc branch
                            :status (if landed? :done :abandoned)
                            :final-answer (when landed? (summarize result))
                            :inactive-reason (when-not landed? (:reason result)))))))
