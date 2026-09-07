;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.model.turn-journal-test
  "Two consecutive turns of ONE branch must never interleave their journal
  writes. That is karamazov-blt.18: a turn past its deadline kept running, the
  next round advanced the same branch beside it, and the journal diverged from
  the live state. RFC-013 restates it as 'a branch is never advanced beside its
  own turn' and enforces it with a termination promise.

  Three programs: turn 1 (two journal writes, each preceded by the cancel check
  the :pre interceptor gives every cell), the scheduler (the deadline, then the
  next round), and turn 2 (its own writes). Four schedulers over the same
  programs, differing in what the deadline does and what lets turn 2 start:

    :zombie     no cancel, turn 2 starts unconditionally   — the blt.18 bug
    :in-flight  no cancel, turn 2 waits for turn 1 to END  — today's guard
    :cancel     cancel, turn 2 starts unconditionally      — cancel alone
    :promise    cancel, turn 2 waits for END or TERMINATED — RFC-013

  Enumerated over every interleaving. The point of :cancel is that it still
  fails: a cancel is observed at the turn's NEXT check, and turn 2 can be
  writing before turn 1 has reached one. That is why the promise exists."
  (:require [clojure.test :refer [deftest is testing]]
            [samizdat.model.interleave :as il]))

(def ^:private s0
  {:t1 :running :t2 :idle :journal [] :cancelled? false :late-writes 0})

(def ^:private turn-1 [:t1/check :t1/write :t1/check :t1/write :t1/end])
(def ^:private scheduler [:s/deadline :s/round])
(def ^:private turn-2 [:t2/write :t2/write :t2/end])
(def ^:private programs [turn-1 scheduler turn-2])

(def ^:private variants
  {:zombie    {:cancel? false :start-when #{:done}}
   :in-flight {:cancel? false :start-when #{:done} :guard? true}
   :cancel    {:cancel? true  :start-when #{:done}}
   :promise   {:cancel? true  :start-when #{:done :terminated} :guard? true}})

(defn- step-for [{:keys [cancel? start-when guard?]}]
  (fn [s a]
    (case a
      ;; the cell boundary: with cancellation, a cancelled turn ends here
      :t1/check   (if (and cancel? (:cancelled? s) (= :running (:t1 s)))
                    (assoc s :t1 :terminated)
                    s)
      :t1/write   (if (= :running (:t1 s))
                    (cond-> (update s :journal conj :t1)
                      (:cancelled? s) (update :late-writes inc))
                    s)
      :t1/end     (if (= :running (:t1 s)) (assoc s :t1 :done) s)
      ;; the deadline: forfeit, and (when the scheduler cancels) cancel
      :s/deadline (assoc s :cancelled? true)
      ;; the next round: does the branch advance beside its previous turn?
      :s/round    (if (or (not guard?) (contains? start-when (:t1 s)))
                    (assoc s :t2 :running)
                    s)
      :t2/write   (if (= :running (:t2 s)) (update s :journal conj :t2) s)
      :t2/end     (if (= :running (:t2 s)) (assoc s :t2 :done) s))))

(defn- serial?
  "Every write of turn 1 precedes every write of turn 2."
  [{:keys [journal]}]
  (= journal (concat (filter #{:t1} journal) (filter #{:t2} journal))))

(defn- violations [variant]
  (il/violations (step-for (variants variant)) s0 serial? programs))

(defn- late-writes
  "Across every schedule, how many turn-1 writes landed after the deadline:
  the work a forfeited turn still does under each scheduler."
  [variant]
  (reduce + (map (fn [sched] (:late-writes (il/run-schedule (step-for (variants variant)) s0 sched)))
                 (il/schedules programs))))

(deftest the-model-covers-every-interleaving
  (is (= 2520 (count (il/schedules programs)))
      "10 actions in three programs of 5, 2 and 3: 10!/(5!2!3!) schedules"))

(deftest advancing-a-branch-beside-its-own-turn-interleaves-the-journal
  (testing "the blt.18 bug: turn 2 starts while turn 1 is still writing"
    (is (pos? (count (violations :zombie)))))
  (testing "today's :in-flight guard: turn 2 waits for turn 1 to end"
    (is (empty? (violations :in-flight)))))

(deftest a-cancel-alone-is-not-enough
  ;; The cancel is observed at the turn's NEXT check. Between the deadline and
  ;; that check the turn is still a running turn, and a round that trusts the
  ;; cancel starts turn 2 beside it.
  (is (pos? (count (violations :cancel)))
      "cancel without tracking termination still interleaves the journal"))

(deftest the-termination-promise-keeps-the-journal-serial
  (is (empty? (violations :promise))
      "RFC-013: turn 2 starts only once turn 1 has ended or terminated"))

(deftest cancelling-retires-the-forfeited-turn-sooner
  ;; Today's guard keeps the journal serial by letting the zombie run to
  ;; completion. The promise keeps it serial AND stops the turn at its next
  ;; check, so fewer writes land after the deadline across all schedules.
  (is (< (late-writes :promise) (late-writes :in-flight))
      "a forfeited turn does less after the deadline when it is cancelled"))
