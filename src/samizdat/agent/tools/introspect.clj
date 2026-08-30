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

(ns samizdat.agent.tools.introspect
  "Self-reflection, read-only: see the loop you are running in.

  Two renderings, both bounded:

    IMAGE   which image `eval` lands in for this role, and what the sandbox
            around it allows — so a supervisor reading a failed eval can tell
            a policy refusal from a broken form.

    WIRING  the loop graph from the manifest — every node, its cell, the
            cell's effects, and its outgoing edge or dispatch table. The
            same data reload_cells validates an edit against.

    HEALTH  this run so far, from the turns table — the last few turns
            (turn, tool, category) and simple tallies.

  A separate namespace requiring only base + the read seams, so the tool
  surface grows by a plug-in file rather than by editing the aggregator.
  Render fns are exposed (not private) so a test can call them directly."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [mycelium.cell :as cell]
            [samizdat.agent.tools.base :as base]
            [samizdat.cells :as cells]
            [samizdat.config :as config]
            [samizdat.repl.route :as route]
            [samizdat.security.sandbox :as sandbox]
            [samizdat.manual :as manual]
            [samizdat.prompt :as prompt]
            [samizdat.manifests :as manifests]
            [samizdat.store.journal :as journal]))

(defn active-manifest
  "The manifest that is ACTUALLY driving this run, as {:name :version
  :definition}. The beam puts compile-turn-loop's result in ctx
  :turn-workflow — the version-true wiring — and a caller without one (a
  test, a bare render) falls back to the configured name through the
  userspace seam. Reading the factory loop.edn here meant the
  self-observation tool showed a project that evolved its loop the FACTORY
  wiring, and always called it 'loop' whatever was running
  (karamazov-blt.4)."
  [ctx]
  (if-let [tw (:turn-workflow ctx)]
    (if (:definition tw)
      (select-keys tw [:name :version :definition])
      (let [nm (or (get-in ctx [:config :run :loop]) "loop")]
        {:name nm
         :definition (edn/read-string (manifests/manifest-body! nm))}))
    (let [nm (or (get-in ctx [:config :run :loop]) "loop")]
      {:name nm
       :definition (edn/read-string (manifests/manifest-body! nm))})))

(defn loop-def
  "The active loop's workflow definition — :cells (node -> cell-id), :edges
  (node -> next node or dispatch map), :dispatches."
  ([] (loop-def nil))
  ([ctx] (:definition (active-manifest ctx))))

(defn cell-effects
  "A cell's effects, as the cells tool renders them: 'pure', the sorted
  effect set, or 'undeclared'. 'not-loaded' when the registry has no such
  cell, so a stale manifest degrades to a note rather than a throw."
  [id]
  (if-let [spec (cell/get-cell id)]
    (cond (cell/pure? spec) "pure"
          (seq (cell/effects spec))
          (str/join "," (map name (sort (cell/effects spec))))
          :else "undeclared")
    "not-loaded"))

(defn edge-str
  "One node's outgoing wiring: a single successor name, or the dispatch
  table rendered as decision->node, pipe-separated."
  [edge]
  (if (map? edge)
    (str/join " | " (for [[d n] (sort-by key edge)]
                      (str (name d) "->" (name n))))
    (name edge)))

(defn render-wiring
  "The whole path a turn takes: for each node in the manifest, its cell,
  that cell's effects, and where the node routes next. Bounded by the size
  of the manifest itself."
  ([] (render-wiring (loop-def)))
  ([def]
   (str/join "\n"
             (for [[node cell-id] (sort-by key (:cells def))]
               ;; The FULL cell id, namespace included — :llm/* vs :tool/* is
               ;; load-bearing (interceptors glob on it), and (name :llm/parse)
               ;; and (name :fence/parse) rendered identically.
               (str (name node) " = " (subs (str cell-id) 1)
                    "  [" (cell-effects cell-id) "]"
                    "  -> " (edge-str (get (:edges def) node)))))))

(defn render-recent
  "The last n turns as one line each: turn, tool, category. The compact
  tail of the run, not the whole log."
  ([rows] (render-recent rows 8))
  ([rows n]
   (if (empty? rows)
     "(no turns recorded yet)"
     (str/join "\n"
               (for [r (take-last n rows)]
                 (str (:turn r) "  " (:tool_name r "?")
                      "  " (some-> (:category r) name)))))))

(defn render-health
  "A compact snapshot of a run: tallies over every turn row, then the
  recent tail. rows are turn maps with :turn :tool_name :category
  :parse_error; max-turns may be absent, in which case only the count is
  shown."
  ([rows] (render-health rows nil))
  ([rows max-turns]
   (let [n (count rows)
         parse-errors (count (filter :parse_error rows))
         failures (count (filter #(= "failure" (some-> (:category %) name)) rows))
         head (str "turns: " n (when max-turns (str " of " max-turns))
                   " | failed calls: " failures
                   " | parse errors: " parse-errors)]
     (str head "\n\nrecent:\n" (render-recent rows)))))

(defn- render-eval-image
  "Which image this run's `eval` lands in, and how confined it is.

  WITHOUT THIS A SUPERVISOR CANNOT TELL A DENIAL FROM A DEFECT. It reads a
  worker's failed eval with no way to know whether the sandbox refused the
  call or the code is broken, and the standing rule is that everything the
  supervisor might act on has to be enumerable at runtime with a description.
  The wording is prompts/eval-image.md, like every other sentence the model
  reads."
  [ctx]
  (let [image (route/image-of ctx)
        root (:root ctx)]
    (prompt/render "eval-image"
                   {:image (clojure.core/name image)
                    :root root
                    :role (or (:role ctx) "(unset)")
                    :harness (= :harness image)
                    :project (= :project image)
                    :off (= :off image)
                    :backend (clojure.core/name
                              (sandbox/backend-for (config/eval-sandbox root)
                                                   (System/getProperty "os.name")))})))

(defmethod base/run-tool "introspect" [{:keys [branch conn run-id max-turns] :as ctx}]
  (let [{:keys [name version definition]} (active-manifest ctx)]
    (base/ok branch
             (str "=== LOOP WIRING (" name
                  (when version (str " v" version)) ") ===\n\n"
                  (render-wiring definition)
                  "\n\n=== EVAL IMAGE ===\n\n"
                  (render-eval-image ctx)
                  "\n\n=== RUN HEALTH ===\n\n"
                  (if conn
                    (render-health
                     (map #(select-keys % [:turn :tool_name :category :parse_error])
                          (journal/turns conn run-id))
                     max-turns)
                    "(no run database in this context — wiring only)")))))

(defmethod base/run-tool "manual" [{:keys [branch] :as ctx}]
  ;; The harness's own command surface, for a branch developing at the REPL
  ;; inside it. Curated in resources/manual.edn (LR-6), so what the agent is
  ;; told it can do is editable at runtime — including by the agent.
  ;;
  ;; With a `name` argument, the full docstring for that one entry; without,
  ;; the whole surface as summaries. A name that is not in the manual says so
  ;; and lists the groups, rather than returning nothing.
  (let [wanted (some-> (base/arg ctx :name) str str/trim not-empty)]
    (try
      (if wanted
        (if-let [e (manual/find-entry wanted)]
          (base/ok branch (str (:name e) " " (pr-str (:arglists e)) "\n\n"
                               (:summary e) "\n\n"
                               (or (:doc e) "(no docstring)")))
          (base/malformed
           branch
           (str "`" wanted "` is not in the manual. Groups: "
                (str/join ", " (map :group (manual/groups)))
                ". Call `manual` with no arguments for the whole surface, or"
                " `doc` for any var whether it is curated or not.")))
        (base/ok branch
                 (str "=== THE HARNESS'S OWN COMMAND SURFACE ===\n"
                      "Call these from `eval`. `manual` with a `name` gives one"
                      " entry's full docstring.\n\n"
                      (manual/render))))
      (catch Throwable e
        ;; A broken manual.edn is a real failure and must say so: a manual that
        ;; renders nothing reads as "there is nothing here".
        (base/malformed branch
                        (str "the manual could not be compiled — resources/manual.edn"
                             " names something that does not resolve: "
                             (ex-message e)))))))
