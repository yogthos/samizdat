;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.model.ratelimit-teardown-test
  "The provider retry ladder against a run's abort and teardown.

  llm/ratelimit.clj's first rule is 'FAIL FAST, NEVER SLEEP HERE': the ladder
  above owns waiting, checks the abort flag between attempts, and a sleep down
  in the latch 'would hold a thread past an abort and race the run's
  teardown'. The rule is right about the latch and silent about the ladder's
  OWN sleep, which cannot be cancelled either: an abort that lands while the
  ladder sleeps is not seen until the sleep ends, and by then the run may have
  been torn down under the attempt that follows.

  Two programs: the ladder (attempt, check the flag, sleep, attempt again) and
  the aborter (set abort, tear down). Two ladders over the same programs:

    :today  the sleep is Thread/sleep and only the check reads the flag
    :ebb    the sleep is a park that sees the cancel, and the check runs before
            every attempt (RFC-013: (!) before each attempt)

  The property: no attempt after teardown, over every interleaving."
  (:require [clojure.test :refer [deftest is testing]]
            [samizdat.model.interleave :as il]))

(def ^:private s0 {:abort? false :torn? false :ladder :running :late-attempts 0})

(def ^:private ladder  [:l/attempt :l/check :l/sleep :l/attempt :l/end])
(def ^:private aborter [:a/abort :a/teardown])
(def ^:private programs [ladder aborter])

(defn- ended [s] (assoc s :ladder :ended))

(defn- step-for [variant]
  (fn [s a]
    (case a
      :l/attempt  (cond
                    (not= :running (:ladder s)) s
                    ;; RFC-013: (!) before every attempt
                    (and (= :ebb variant) (:abort? s)) (ended s)
                    (:torn? s) (update s :late-attempts inc)
                    :else s)
      :l/check    (if (and (= :running (:ladder s)) (:abort? s)) (ended s) s)
      ;; today the sleep cannot be cancelled; under ebb the park sees it
      :l/sleep    (if (and (= :ebb variant) (= :running (:ladder s)) (:abort? s)) (ended s) s)
      :l/end      (if (= :running (:ladder s)) (ended s) s)
      :a/abort    (assoc s :abort? true)
      :a/teardown (assoc s :torn? true))))

(defn- no-late-attempt? [s] (zero? (:late-attempts s)))

(defn- violations [variant]
  (il/violations (step-for variant) s0 no-late-attempt? programs))

(deftest the-model-covers-every-interleaving
  (is (= 21 (count (il/schedules programs))) "7 actions in programs of 5 and 2"))

(deftest a-sleeping-ladder-can-attempt-after-teardown-today
  (testing "the check between attempts does not see an abort that lands during the sleep"
    (let [bad (violations :today)]
      (is (pos? (count bad)))
      (is (some (fn [[sched _]]
                  (= sched [:l/attempt :l/check :a/abort :a/teardown :l/sleep :l/attempt :l/end]))
                bad)
          "the schedule ratelimit.clj warns about: abort and teardown inside the sleep"))))

(deftest a-parking-ladder-never-attempts-after-teardown
  (is (empty? (violations :ebb))
      "RFC-013: the sleep is a park that sees the cancel, and (!) precedes every attempt"))
