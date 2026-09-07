;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.model.task-claim-test
  "Two branches of one run claim the same task; exactly one may believe it
  holds it. store/tasks.clj's claim! has been wrong about this twice:

    provenance A-4   a read-then-write pair: two branches whose reads both
                     saw the row free both wrote, and both held it.
    migration v12    the guard moved into the UPDATE but keyed on run_id, which
                     is exclusive between runs and a no-op WITHIN one; a team
                     fan-out is several implementors as branches of one run,
                     and two of them claimed the same task.

  Three claim protocols over the same two programs, each a branch of run :r:

    :read-then-write   read the row, then write if it looked free
    :run-guard         UPDATE guarded on run_id; verify by run_id
    :branch-guard      UPDATE guarded on (run_id, branch_id); verify by both

  The property: exactly one branch believes it holds the task, over every
  interleaving. The single-writer contract (db/with-writer) makes each UPDATE
  atomic, which the model reflects by making each action one step."
  (:require [clojure.test :refer [deftest is testing]]
            [samizdat.model.interleave :as il]))

(def ^:private s0 {:holder nil :saw-free #{} :believes #{}})

(defn- program [variant b]
  (case variant
    :read-then-write [[b :read] [b :write]]
    (:run-guard :branch-guard) [[b :update] [b :verify]]))

(defn- programs [variant] [(program variant :b1) (program variant :b2)])

(defn- mine [b] {:run :r :branch b})

(defn- step-for [variant]
  (fn [{:keys [holder] :as s} [b op]]
    (case [variant op]
      [:read-then-write :read]   (cond-> s (nil? holder) (update :saw-free conj b))
      [:read-then-write :write]  (if (contains? (:saw-free s) b)
                                   (-> s (assoc :holder (mine b)) (update :believes conj b))
                                   s)
      [:run-guard :update]       (cond-> s (or (nil? holder) (= :r (:run holder)))
                                   (assoc :holder (mine b)))
      [:run-guard :verify]       (cond-> s (= :r (:run holder)) (update :believes conj b))
      [:branch-guard :update]    (cond-> s (or (nil? holder) (= (mine b) holder))
                                   (assoc :holder (mine b)))
      [:branch-guard :verify]    (cond-> s (= (mine b) holder) (update :believes conj b)))))

(defn- one-holder? [s] (= 1 (count (:believes s))))

(defn- violations [variant]
  (il/violations (step-for variant) s0 one-holder? (programs variant)))

(deftest the-model-covers-every-interleaving
  (is (= 6 (count (il/schedules (programs :branch-guard)))) "4 actions in programs of 2 and 2"))

(deftest a-read-then-write-claim-lets-both-branches-hold-the-task
  (testing "provenance A-4"
    (is (pos? (count (violations :read-then-write))))))

(deftest a-run-keyed-guard-is-a-no-op-within-one-run
  (testing "pre-v12: every schedule ends with both branches believing"
    (let [bad (violations :run-guard)]
      (is (= (count bad) (count (il/schedules (programs :run-guard))))))))

(deftest a-branch-keyed-guard-admits-exactly-one-holder
  (is (empty? (violations :branch-guard))
      "claim! today: the UPDATE guards on (run_id, branch_id) and the verify reads both"))
