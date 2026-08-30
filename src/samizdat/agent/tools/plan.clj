;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.agent.tools.plan
  "The `plan` tool: the entry condition of a REPL session.

  A branch says which files it is about to create or edit and which tests it
  will write, BEFORE it starts exploring. That declaration is a hypothesis
  about where the problem is, and having to state one is the whole point.

  Run bd56a286 is why this exists. A strong model spent 238 turns in the REPL
  hunting a defect that was in its own tests — every re-read of the
  implementation confirmed the implementation was fine, which is precisely why
  it read again. It never had to say where it thought the defect was, so it
  never noticed it had assumed. Naming a file forces the question; naming a
  different file later is how the answer gets corrected.

  The wording lives in prompts/plan-tool.md; the flow lives in the `repl`
  manifest."
  (:require [samizdat.agent.state :as state]
            [samizdat.agent.tools.base :as base]
            [samizdat.prompt :as prompt]))

(defn- msg [ctx] (prompt/render "plan-tool" ctx))

(defmethod base/run-tool "plan" [{:keys [branch] :as ctx}]
  (let [coerce (fn [k]
                 (let [v (or (base/arg ctx k) (get (:args ctx) (name k)))]
                   (cond (nil? v) []
                         (coll? v) (vec (remove empty? (map str v)))
                         :else [(str v)])))
        files (coerce :files)
        tests (coerce :tests)
        goal (some-> (base/arg ctx :goal) str not-empty)]
    (if (empty? (concat files tests))
      ;; An empty plan is the state this tool exists to rule out, so it is a
      ;; malformed call rather than an accepted no-op.
      (base/malformed branch (msg {:needs-files true}))
      (let [b (state/declare-plan branch {:files files :tests tests :goal goal})]
        (assoc (base/ok branch (msg {:declared true
                                     :files (clojure.string/join ", "
                                                                 (:files (state/plan b)))
                                     :goal goal}))
               :branch b)))))
