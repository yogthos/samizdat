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

(ns samizdat.agent.tools.mutate
  "The self-modification tool: after the agent edits a cell in resources/cells,
  `reload_cells` runs the mutation protocol (checkpoint -> reload -> validate
  -> soak -> commit or rollback). A good edit goes live on the next turn; a bad
  one is rolled back and the reason returned, so the agent can fix it. This is
  how the harness safely changes its own behavior at runtime.

  A separate namespace requiring only base, so it plugs into the tool surface
  without dragging the mutation machinery into the aggregator."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [mycelium.cell :as cell]
            [samizdat.agent.state :as state]
            [samizdat.agent.tools.base :as base]
            [samizdat.cells :as cells]
            [samizdat.manifests :as manifests]
            [samizdat.mutation :as mutation]
            [samizdat.prompt :as prompt]
            [samizdat.store.userspace]
            [samizdat.userspace :as userspace]))

(defmethod base/run-tool "cells" [{:keys [branch]}]
  ;; What the loop is made of: the cells currently loaded, their effects, and
  ;; the file each came from — so the agent knows what it can edit and where.
  (base/ok branch
           (str/join "\n"
                     (for [[id {:keys [source]}] (sort-by key (cells/loaded))]
                       (let [spec (cell/get-cell id)
                             fx (cond (cell/pure? spec) "pure"
                                      (seq (cell/effects spec))
                                      (str/join "," (map name (sort (cell/effects spec))))
                                      :else "undeclared")]
                         (str id "  [" fx "]  " source))))))

(defn- active-name
  "Which manifest this run drives — config :run :loop, else the default.
  Mirrors workflow/active-loop-name, which this tool cannot require (the
  workflow ns reaches the tool dispatcher through the branch loop)."
  [ctx]
  (or (get-in ctx [:config :run :loop]) "loop"))

(defn- current-loop-def
  "The ACTIVE loop's workflow definition, through the userspace seam — the
  project's stored version, not the factory resource. Validating and soaking
  against the shipped loop.edn meant an edit was checked against wiring the
  run was not driving: a bad edit to the evolved loop could commit, and a
  valid one could be refused (karamazov-blt.2)."
  [ctx]
  (edn/read-string (userspace/body! :manifest (active-name ctx))))

(defn- extra-defs
  "Every OTHER manifest this project can run — shipped and stored — as
  {name definition}, for compile-only validation. A cell wired into the beam
  or a team loop is not referenced by the active loop at all, so validating
  the one definition let an edit that broke every other workflow commit
  untouched (karamazov-blt.2)."
  [active]
  (into {}
        (for [nm (distinct (concat manifests/shipped-manifests
                                   (map :name (userspace/names :manifest))))
              :when (not= nm active)
              :let [body (manifests/manifest-body nm)]
              :when body
              :let [d (try (edn/read-string body) (catch Throwable _ nil))]
              :when (seq (:cells d))]
          [nm d])))

(defn- soak-input
  "A synthetic starting state the soak dry-run terminates from: a branch that
  is already done, so the loop routes straight to finish without needing a
  real model call."
  []
  {:branch (assoc (state/new-branch {:id "soak" :problem "soak"})
                  :status :done :final-answer "soak")
   :turn 1})

(defmethod base/run-tool "reload_cells" [{:keys [branch] :as ctx}]
  ;; Reload the project's cells from the userspace store (plus .samizdat/cells
  ;; seeds) — the PRODUCTION loader — then prove the active loop still
  ;; compiles the way the run loader will. The old body ran the dir-based
  ;; apply-cell-edit!, which read templates/disk instead of the store: calling
  ;; it regressed store-evolved cells to the templates for the rest of the
  ;; run, and its rollback wrote store keys into the cwd as files
  ;; (karamazov-blt.7).
  (let [snapshot (cell/registry-snapshot)]
    (try
      (cells/load-cells!)
      (manifests/compile-definition (current-loop-def ctx))
      (base/ok branch
               (prompt/render "cell-tool" {:reloaded true
                                           :name (active-name ctx)})
               :progress? true)
      (catch Throwable e
        (cell/registry-restore! snapshot)
        ;; The registry is back to what it was and nothing is live, so this is
        ;; a rejected edit rather than a branch failure — see base/rejected.
        (base/rejected branch
                       (str (prompt/render "cell-tool" {:reload-failed true})
                            "\n\n" (ex-message e)))))))

;; --- the project's own cells (userspace) -------------------------------------

(def ^:private cell-usage
  "Actions: list, show {name, version?}, save {name, clj | file, rationale}, versions {name}, revert {name, version, rationale}. A cell is one step of the loop, as Clojure. Save validates by compiling the loop and dry-running it before it stores, and stores a new VERSION in this project — the shipped template is never written. For a large body, write it to a file first (write_file), then save {name, file}: a fix left as a file and never saved does not exist. rationale: one sentence on why — the history shows it to the next supervisor deciding whether your change stays.")

(defn- render-versions [name]
  (let [rows (userspace/versions :cell name)]
    (if (seq rows)
      (str/join "\n" (map base/version-line rows))
      (str "No stored versions of '" name "' in this project."
           " It is still the shipped template."))))

(defmethod base/run-tool "cell" [{:keys [branch conn run-id] :as ctx}]
  ;; The project-scoped half of self-modification. `cells` lists what is
  ;; loaded; this edits it. Every save is a new version in THIS project's
  ;; userspace store, seeded from the harness's template on first read — so a
  ;; loop this project evolves is its own, and no other project sees it.
  (let [action (some-> (base/arg ctx :action) str str/trim str/lower-case not-empty)
        name (some-> (base/arg ctx :name) str str/trim not-empty)]
    (try
      (case action
        nil
        (base/malformed branch (str "`cell` needs an `action`. " cell-usage))

        "list"
        (let [rows (userspace/names :cell)]
          (base/ok branch
                   (if (seq rows)
                     (str/join "\n" (for [{:keys [name version versions]} rows]
                                      (str name "  v" version " (" versions
                                           (if (= 1 versions) " version)" " versions)"))))
                     (str "This project has stored no cell versions yet — it is"
                          " running the shipped templates. Any save starts its"
                          " own copy."))))

        "show"
        (if-not name
          (base/malformed branch (base/missing ctx :name))
          (let [v (some-> (base/arg ctx :version) str str/trim not-empty parse-long)
                body (if v
                       (some-> (userspace/conn)
                               (samizdat.store.userspace/load-version :cell name v)
                               :body)
                       (userspace/body :cell name))]
            (if body
              (base/ok branch (str name (when v (str " v" v)) ":\n\n" body))
              (base/malformed branch (str "No cell '" name "'"
                                          (when v (str " v" v)) "."
                                          " `cell list` shows this project's;"
                                          " `cells` shows what is loaded.")))))

        "versions"
        (if-not name
          (base/malformed branch (base/missing ctx :name))
          (base/ok branch (render-versions name)))

        "save"
        (let [{body :body err :error} (base/save-body ctx :clj)
              why (base/rationale ctx)]
          (cond
            (not name) (base/malformed branch (base/missing ctx :name))
            err (base/malformed branch err)
            (str/blank? (str body)) (base/malformed branch (base/missing ctx :clj))
            (nil? why) (base/malformed branch (base/missing ctx :rationale))
            :else
            (let [active (active-name ctx)
                  r (mutation/propose-cell!
                     {:name name :body (str body) :rationale why
                      :loop-def (current-loop-def ctx)
                      :extra-defs (extra-defs active)
                      ;; The loader's own static pipeline (sub-workflows,
                      ;; ctx-key requires, derived constraints), WITHOUT its
                      ;; registry reload — which would replace the candidate
                      ;; just installed (karamazov-blt.2).
                      :compile-fn manifests/compile-definition
                      :soak-input (soak-input)
                      :conn conn :run-id run-id})]
              (case (:status r)
                :committed
                (base/ok branch
                         (str "Saved cell '" name "' as v" (:version r)
                              " in this project — it compiled, it dry-ran, and it"
                              " is live on your next turn. The shipped template is"
                              " unchanged; other projects still start from it.")
                         :progress? true)

                ;; Validate and soak passed and the edit is live in this
                ;; image, but nothing was persisted — telling the model it
                ;; was "saved as v" here was a lie about durability
                ;; (karamazov-blt.8).
                :live-unsaved
                (base/ok branch
                         (prompt/render "cell-tool" {:live-unsaved true
                                                     :name name}))

                ;; Rolled back: validate or the soak refused it, the registry
                ;; is restored, and nothing was stored. A correctable edit,
                ;; not a branch failure — see base/rejected.
                (base/rejected branch
                               (str "Cell '" name "' was NOT saved; the loop is"
                                    " unchanged and nothing entered this project's"
                                    " history.\n\n" (:reason r)
                                    "\n\nFix it and save again."))))))

        "revert"
        (let [v (some-> (base/arg ctx :version) str str/trim not-empty parse-long)
              why (base/rationale ctx)]
          (cond
            (not name) (base/malformed branch (base/missing ctx :name))
            (nil? v) (base/malformed branch (base/missing ctx :version))
            (nil? why) (base/malformed branch (base/missing ctx :rationale))
            :else
            (if-let [nv (userspace/revert! :cell name v why)]
              (base/ok branch
                       (str "Reverted cell '" name "' to the body of v" v
                            ", stored as v" nv ". Reverting is itself an edit, so"
                            " the version you left behind is still readable."
                            " Call reload_cells to make it live.")
                       :progress? true)
              (base/malformed branch (str "No v" v " of cell '" name
                                          "' in this project. " (render-versions name))))))

        (base/malformed branch (str "Unknown cell action `" action "`. " cell-usage)))
      (catch Throwable e
        (base/fail branch (str "`cell " action "` refused: " (ex-message e)
                               "\n\n" cell-usage))))))
