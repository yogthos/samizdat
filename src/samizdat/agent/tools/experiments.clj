;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.agent.tools.experiments
  "The `experiment` and `verdict` tools: how the supervisor applies selection
  pressure to the loop and finds out whether it worked.

  The supervisor could already change the loop, and the harness could already
  measure itself. What was missing was the binding between the two — a change
  was MADE, and then the next round's numbers were whatever they were, with
  nothing connecting them. That is variation without selection, which is drift.

  An experiment names a change, states what it was expected to do, and stamps
  the tally it will be judged against. The verdict compares per-turn fitness
  before and after and answers better / worse / unchanged / too early.

  The hypothesis is not decoration. A change with no stated expectation cannot
  be wrong, and a change that cannot be wrong teaches nothing whichever way the
  numbers move."
  (:require [clojure.string :as str]
            [samizdat.agent.tools.base :as base]
            [samizdat.prompt :as prompt]
            [samizdat.session :as session]))

(defn- msg [ctx] (prompt/render "experiment-tool" ctx))

(def ^:private usage (delay (msg {:usage true})))

(defmethod base/run-tool "experiment" [{:keys [branch] :as ctx}]
  (let [name (some-> (base/arg ctx :name) str str/trim not-empty)
        change (some-> (base/arg ctx :change) str str/trim not-empty)
        hypothesis (some-> (base/arg ctx :hypothesis) str str/trim not-empty)]
    (if-not (and name change hypothesis)
      (base/malformed branch (str (base/missing ctx :name :change :hypothesis)
                                  "\n\n" @usage))
      (try
        (session/experiment! name {:change change :hypothesis hypothesis})
        (base/ok branch (msg {:started true :name name
                              :change change :hypothesis hypothesis})
                 :progress? true)
        (catch clojure.lang.ExceptionInfo e
          (if (= :samizdat.session/too-many-open (:type (ex-data e)))
            ;; A refusal, not a crash: the supervisor made a well-formed call
            ;; and the harness declined it, which is a policy refusal like any
            ;; other and must not read as its mistake.
            (let [{:keys [open cap unsettled]} (ex-data e)]
              (base/refusal branch (msg {:too-many true :open open :cap cap
                                         :unsettled (when (seq unsettled)
                                                      (str/join ", " unsettled))})))
            (throw e)))))))

(defmethod base/run-tool "verdict" [{:keys [branch] :as ctx}]
  (let [name (some-> (base/arg ctx :name) str str/trim not-empty)
        action (some-> (base/arg ctx :action) str str/trim str/lower-case not-empty)]
    (cond
      (not name)
      (base/malformed branch (str (base/missing ctx :name) "\n\n" @usage))

      ;; Settling is how a supervisor answers the nag. Without it
      ;; `unsettled-losses` would raise the same change every turn forever,
      ;; which trains a reader to skip the block — the opposite of the point.
      action
      (if-not (contains? #{"reverted" "kept"} action)
        (base/malformed branch (str "`action` is `reverted` or `kept`.\n\n" @usage))
        (if-not (session/verdict name)
          (base/fail branch (msg {:no-experiment true :name name}))
          (do (session/reverted! name (= "kept" action))
              (base/ok branch (msg {:settled true :name name :action action
                                    :why (base/arg ctx :why)})
                       :progress? true))))

      :else
      (if-let [v (session/verdict name)]
        (base/ok branch
                 (msg {:reported true :name name
                       :verdict (clojure.core/name (:verdict v))
                       :change (:change v) :hypothesis (:hypothesis v)
                       :numbers (when (and (:before v) (:after v))
                                  (format "fitness %.2f -> %.2f over %d turns"
                                          (:before v) (:after v) (:turns-since v)))
                       :better (= :better (:verdict v))
                       :worse (= :worse (:verdict v))
                       :unchanged (= :unchanged (:verdict v))
                       :too-early (= :too-early (:verdict v))
                       ;; The second reading, and only when it is the reason
                       ;; for the verdict: on any other verdict it agrees with
                       ;; the first and is noise.
                       :confounded
                       (when (= :confounded (:verdict v))
                         (let [b (:health-blind v)]
                           {:verdict (clojure.core/name (:verdict b))
                            :numbers (when (and (:before b) (:after b))
                                       (format "%.2f -> %.2f" (:before b) (:after b)))}))
                       :regraded
                       (when (:regraded v)
                         (when-let [stamped (:before-as-stamped v)]
                           (format "%.2f" stamped)))
                       :branches
                       (when-let [br (:branches v)]
                         (when (pos? (:regressed br))
                           {:regressed (:regressed br) :measured (:measured br)}))}))
        (base/fail branch (msg {:no-experiment true :name name}))))))
