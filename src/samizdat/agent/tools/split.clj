;; samizdat - a self-hosting agentic harness
;; Copyright (C) 2026 Dmitri Sotnikov
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.agent.tools.split
  "Hand work down: declare that this task is more than one thing, and say
  which piece each child owns.

  THE CALL IS NOT THE WORK. Before calling this, the agent writes the stubs
  into the tree — the signatures with docstrings saying what each one owes —
  and sketches the tests that will pin them. This tool then VERIFIES that it
  did, mechanically, and only then opens the child tasks. That is the whole
  point of making it a tool rather than a paragraph of JSON: a split used to
  be a description of an intention, and the harness had no way to tell an
  agent that had designed the pieces from one that had described them.

  What the verification buys is a boundary that holds in both directions. The
  parent reasons about the API and how the pieces compose; each child reasons
  about the implementation behind one signature and shares nothing else with
  its siblings. A child knows it is done when its stubs are filled and its
  tests pass, and neither of those is a judgement call.

  The suite is RED from here until the last child lands, and that is the
  intended state — it is tests-first at the scale of a delegation. See
  cells/decompose.clj for what a child's green means, which is its OWN tests
  rather than the tree's.

  Refusals are `base/rejected`: well-formed arguments the harness validated
  and declined, not charged to the branch. The words live in
  prompts/split-tool.md."
  (:require [clojure.string :as str]
            [samizdat.agent.files :as files]
            [samizdat.agent.gates :as gates]
            [samizdat.agent.stubs :as stubs]
            [samizdat.agent.tools.base :as base]
            [samizdat.prompt :as prompt]
            [samizdat.store.tasks :as tasks]))

(defn- msg [ctx] (prompt/render "split-tool" ctx))

(defn- read-under
  "The text of `path` under `root`, or nil when it does not resolve or is not
  there. One primitive for confinement (files/resolve-under-root), because a
  tool that rolled its own was the escape this project already had."
  [root path]
  (when (and root (not (str/blank? (str path))))
    (when-let [abs (files/resolve-under-root root (str path))]
      (let [f (java.io.File. ^String abs)]
        (when (.isFile f) (slurp f))))))

(defn- part-of
  "One piece as data, however the model spelled its keys."
  [m]
  (let [g (fn [& ks] (some #(let [v (or (get m %) (get m (name %)))]
                              (when (and (some? v)
                                         (not (and (string? v) (str/blank? v))))
                                v))
                           ks))
        coll (fn [v] (cond (nil? v) [] (coll? v) (vec (map str v)) :else [(str v)]))]
    {:name (some-> (g :name) str str/trim)
     :description (some-> (g :description :desc) str str/trim)
     :file (some-> (g :file :path) str str/trim)
     :stubs (coll (g :stubs :fns :functions))
     :tests (some-> (g :tests :test :test-file) str str/trim)}))

(defn verify
  "Everything wrong with this split, as DATA, in the order a reader would want
  it. Empty means the split holds.

  Pure over the two readers it is handed (`source` and `exists?`), so the
  whole rule set is testable without a tree — and so the confinement decision
  stays in one place rather than being made again here.

  Each entry is {:kind :part :detail} plus the kind as a true flag, because
  selmer has no `==` and a template cannot compare a value: `{% if p.no-stubs %}`
  reads better than the alternative anyway. prompts/split-tool.md renders them.
  Deliberately data rather than sentences: the same reasoning as every other
  refusal in this harness, and base-test enforces it."
  [parts {:keys [source exists? min-parts max-parts]}]
  (let [named (remove #(str/blank? (:name %)) parts)
        seen (atom {})]
    (vec
     (concat
      (when (< (count named) min-parts)
        [{:kind :too-few :detail (str (count named))}])
      (when (> (count named) max-parts)
        [{:kind :too-many :detail (str (count named))}])
      (for [p parts
            :let [nm (or (not-empty (:name p)) "?")]
            problem
            (concat
             (when (str/blank? (:description p)) [{:kind :no-description}])
             (when (str/blank? (:file p)) [{:kind :no-file}])
             (when (empty? (:stubs p)) [{:kind :no-stubs}])
             (when (str/blank? (:tests p)) [{:kind :no-tests}])
             (when (and (not (str/blank? (:tests p)))
                        (not (exists? (:tests p))))
               [{:kind :tests-absent :detail (:tests p)}])
             (when (and (seq (:stubs p)) (not (str/blank? (:file p))))
               (let [src (source (:file p))]
                 (if (nil? src)
                   [{:kind :file-absent :detail (:file p)}]
                   (concat
                    ;; Written? The stub has to be IN THE TREE, because that
                    ;; is the artefact the child inherits and the parent's own
                    ;; composition calls.
                    (for [n (stubs/missing src (:stubs p))]
                      {:kind :stub-absent :detail n})
                    ;; Still hollow? Delegating something already implemented
                    ;; hands a child a contract that is already met, and it
                    ;; would ship having done nothing.
                    (for [n (:stubs p)
                          :when (stubs/filled? src n)]
                      {:kind :stub-already-filled :detail n})
                    ;; Owned once? Two children on one function is the
                    ;; duplicated-work failure the claims registry exists to
                    ;; stop, arriving one level earlier.
                    (for [n (:stubs p)
                          :let [prev (get @seen n)]
                          :when (do (swap! seen update n #(or % nm)) prev)]
                      {:kind :stub-owned-twice :detail n}))))))]
        (assoc problem :part nm (:kind problem) true))))))

(defmethod base/run-tool "split"
  [{:keys [branch conn run-id root] :as ctx}]
  (let [raw (or (base/arg ctx :parts) (base/arg ctx :subtasks))
        parts (mapv part-of (if (coll? raw) raw []))
        reason (some-> (base/arg ctx :reason) str str/trim)
        {:keys [min-parts max-parts]} (gates/threshold :split-parts)
        problems (verify parts {:source #(read-under root %)
                                :exists? #(some? (read-under root %))
                                :min-parts min-parts :max-parts max-parts})]
    (cond
      (empty? parts)
      (base/malformed branch (or (base/missing ctx :parts)
                                 (msg {:no-parts true :max-parts max-parts})))

      (seq problems)
      ;; ALL OR NOTHING. A half-created split leaves rows nobody owns and a
      ;; parent that believes it delegated less than it did, which is worse
      ;; than no split at all — so nothing is written until every piece holds.
      (base/rejected branch (msg {:problems problems
                                  :min-parts min-parts :max-parts max-parts}))

      :else
      ;; THE PIECES HANG OFF THE TASK THE AGENT HOLDS. An agent splits the task
      ;; it is working, so that task is their parent — which is what makes the
      ;; tree walkable in both directions and what lets the caller find the
      ;; pieces afterwards without the tool having to report them out of band.
      ;; Only when the agent holds nothing is a row minted to hang them from,
      ;; because a set of orphan pieces is a split nobody owns.
      (let [held (when (and conn run-id (:id branch))
                   (tasks/held-by conn run-id (:id branch)))
            parent (or (:id held)
                       (tasks/create! conn {:title (str "Split: " (or reason "this task"))
                                            :body (str reason)
                                            :type "epic" :run-id run-id}))
            ids (mapv (fn [p]
                        (tasks/create!
                         conn
                         {:title (:name p)
                          :body (:description p)
                          :parent-id parent
                          :run-id run-id
                          ;; The contract IS the stub source the parent wrote,
                          ;; not a restatement of the ask. task-statement pins
                          ;; it into the child's tape, so the signature it must
                          ;; fill sits beside the work for the whole task.
                          :contract (str/join "\n\n"
                                              (keep #(some-> (stubs/definition
                                                              (read-under root (:file p)) %)
                                                             pr-str)
                                                    (:stubs p)))
                          ;; The PATH, not a copy of the tests. They live in
                          ;; the tree — that is the whole design — so a copy
                          ;; pinned here would drift the moment the parent
                          ;; adjusts them at assembly, and it is the path that
                          ;; lets the harness run exactly this piece's tests
                          ;; against a tree that is red by design.
                          :tests (:tests p)
                          ;; The same delegation, addressably: which names in
                          ;; which file, so the child's ship gate can ask
                          ;; whether they are implemented rather than trusting
                          ;; that green tests imply it (v21).
                          :stub-file (:file p)
                          :stubs (:stubs p)}))
                      parts)]
        (base/ok branch (msg {:created (mapv (fn [p id] {:id id :name (:name p)
                                                         :stubs (str/join ", " (:stubs p))})
                                             parts ids)
                              :parent parent})
                 :split/parent parent
                 :split/task-ids ids)))))
