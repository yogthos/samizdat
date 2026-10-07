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
  manifest. On a branch tagged `:planning?` (the board's design step) the
  call is also the branch's END — see state/finish-planning."
  (:require [samizdat.agent.state :as state]
            [samizdat.agent.tools.base :as base]
            [samizdat.prompt :as prompt]))

(defn- msg [ctx] (prompt/render "plan-tool" ctx))

(defmethod base/run-tool "plan" [{:keys [branch] :as ctx}]
  (let [coerce (fn [k]
                 (let [v (or (base/arg ctx k) (get (:args ctx) (name k)))
                       v (base/listed v)]
                   (cond (nil? v) []
                         (coll? v) (vec (remove empty? (map str v)))
                         :else [(str v)])))
        files (coerce :files)
        tests (coerce :tests)
        ;; Requirement sentences, not paths: what `done` will have to account
        ;; for item by item (karamazov-dsfx). Kept apart from :files so the
        ;; path check below does not refuse a sentence it was given here.
        checklist (mapv (fn [x]
                          ;; Run 6e3eda8a sent [{"c1": "..."}]: a one-entry map
                          ;; is a labelled item, a map with :text its text.
                          (if (map? x)
                            (or (some #(get x %) [:text "text" :item "item"])
                                (let [[[k v]] (seq x)] (str (name k) ": " v)))
                            x))
                        (base/listed (or (base/arg ctx :checklist) (get (:args ctx) "checklist"))))
        checklist (vec (remove empty? (map str (if (sequential? checklist) checklist [checklist]))))
        goal (some-> (base/arg ctx :goal) str not-empty)
        ;; An RFC is free prose (Purpose/Model/Work items/Acceptance, with a
        ;; mermaid call-graph), not a path — the design-rfc step asks for it,
        ;; the diff critic reads it as the contract. Optional: an ordinary
        ;; lightweight plan carries none.
        rfc (some-> (base/arg ctx :rfc) str not-empty)
        ;; A declared entry is a PATH, and a path has no whitespace. Run
        ;; e1b765e7's owner declared "test/flight/ghost_test.clj — ghost HUD
        ;; text (GHOST <score>), …": declare-plan folds :tests into the files
        ;; owed, the sentence became a file it could never write, and every
        ;; `done` from turn 117 was refused for it after the real files had
        ;; landed (karamazov-j9ow). Refused on the turn it is declared,
        ;; naming the entry, rather than withheld a hundred turns later.
        ;; List syntax left in an entry ([, ], a quote) is the same failure
        ;; by another road: a list the call half-encoded.
        not-paths (filterv #(re-find #"[\s\[\]\"]" %) (concat files tests))]
    (cond
      (seq not-paths)
      (base/malformed branch (msg {:not-a-path (first not-paths)}))

      ;; Empty, over a plan that still owes files: WITHDRAW what was never
      ;; written. done refuses owed files and tells the branch to re-plan,
      ;; so without this a branch that found no change was needed could not
      ;; finish at all (run 52eba2b2).
      (and (empty? (concat files tests)) (seq (state/unwritten branch)))
      (let [b (state/withdraw-unwritten branch goal)]
        (assoc (base/ok branch (msg {:withdrawn true
                                     :files (clojure.string/join ", " (:files (state/plan b)))}))
               :branch b))

      (empty? (concat files tests))
      ;; An empty plan is the state this tool exists to rule out, so it is a
      ;; malformed call rather than an accepted no-op.
      (base/malformed branch (msg {:needs-files true}))

      :else
      (let [b (-> (state/declare-plan branch {:files files :tests tests :goal goal :rfc rfc})
                  (state/declare-checklist checklist))
            ;; On a PLANNING branch the declaration is the deliverable, so it
            ;; ends the branch — the board's design step reads the plan off the
            ;; finished branch. Nothing else ended it: the step ran to its cap
            ;; and was then forced to a `done` it could never pass
            ;; (karamazov-ee72). A building branch is untouched — its plan
            ;; opens the repl session it then has to land.
            planning? (state/planning? b)
            b (if planning? (state/finish-planning b) b)]
        (assoc (base/ok branch (msg {:declared true
                                     :files (clojure.string/join ", "
                                                                 (:files (state/plan b)))
                                     :goal goal :rfc (boolean rfc)
                                     :checklist (count (state/checklist b))
                                     :planning planning?}))
               :branch b)))))
