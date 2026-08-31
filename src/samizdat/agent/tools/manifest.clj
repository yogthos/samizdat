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

(ns samizdat.agent.tools.manifest
  "Manifest management. The agentic loop is a named, versioned workflow in the
  workflows table, and there can be many of them — the factory `loop` beside a
  more sophisticated one. This tool lists them, shows one, and saves a tuned or
  brand-new manifest. A save must COMPILE the way the loader will before it is
  stored, so a manifest that cannot run cannot be saved.

  Which manifest a run drives is chosen by config (:run :loop / HARNESS_LOOP /
  a project's .samizdat/config.edn). Tuning the active manifest is picked up on
  the next run, because the loader loads the latest stored version. Validation
  goes through mycelium + the cell registry directly rather than the workflow
  loader, to keep this tool out of the loop-driver's require graph."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [samizdat.agent.tools.base :as base]
            [samizdat.manifests :as manifests]
            [samizdat.store.userspace :as us]))

(defn- validate!
  "Compile the definition EXACTLY the way load-loop! will: cells loaded,
  composed sub-loops registered through the userspace seam, ctx-key requires
  checked, and the :constraints derived from the enforced invariants. Throws
  on any error. The tool used to run a bare pre-compile that skipped the
  last two, so a manifest that could not run could still be saved — and then
  threw out of load-loop! at the next run start (karamazov-blt.6)."
  [edn-text]
  (manifests/compile-loop (manifests/read-definition edn-text))
  true)

(def ^:private usage
  "Actions: list, show {name, version?}, save {name, edn | file, rationale}. A manifest is the loop as data — a :cells map, :edges, and dispatch predicates. Save validates by compiling before it stores; the run that uses it is chosen by config :run :loop. rationale: one sentence on why — the history shows it to the next supervisor deciding whether your change stays.")

(defn- render-list
  "Every manifest this project can run: the stored rows PLUS the shipped
  templates not yet seeded. Store-only listing hid a factory manifest until
  its first run seeded it — `manifest show worker` before any worker run
  answered \"No manifest worker\", so the agent could not read the thing it
  is invited to tune (karamazov-blt.4)."
  [conn]
  (let [rows (us/names conn :manifest)
        stored (set (map :name rows))
        unseeded (for [nm manifests/shipped-manifests
                       :when (and (not (stored nm))
                                  (io/resource (manifests/manifest-resource nm)))]
                   (str nm "  [factory template, unseeded]"))]
    (str/join "\n"
              (concat
               (for [{:keys [name version versions]} rows]
                 (str name "  v" version " (" versions
                      (if (= 1 versions) " version)" " versions)")
                      (when (io/resource (manifests/manifest-resource name))
                        "  [factory]")))
               unseeded))))

(defmethod base/run-tool "manifest" [{:keys [branch conn] :as ctx}]
  (let [action (some-> (base/arg ctx :action) str str/trim str/lower-case not-empty)]
    (try
      (case action
        nil
        (base/malformed branch (str "`manifest` needs an `action`. " usage))

        "list"
        (base/ok branch (render-list conn))

        "show"
        (let [name (base/arg ctx :name)
              v (some-> (base/arg ctx :version) str str/trim not-empty parse-long)]
          (cond
            ;; base/missing yields a complaint STRING for malformed to wrap —
            ;; bare, it dropped :category/:branch from the result map
            ;; (provenance CR1-1).
            (str/blank? (str name)) (base/malformed branch (base/missing ctx :name))
            :else
            (if-let [row (if v
                           (us/load-version conn :manifest name v)
                           (us/load-latest conn :manifest name))]
              (base/ok branch (str name " v" (:version row) ":\n\n" (:body row)))
              ;; Not stored — fall back to the userspace seam, which serves
              ;; the factory template (and seeds it as v1 when a project is
              ;; bound), so a shipped manifest is readable before any run
              ;; drives it (karamazov-blt.4). Version pinning has nothing to
              ;; pin to here, so only the latest form takes this path.
              (if-let [body (when-not v (manifests/manifest-body name))]
                (base/ok branch (str name " (factory template):\n\n" body))
                (base/malformed branch (str "No manifest " name
                                            (when v (str " v" v)) "."))))))

        "save"
        (let [name (base/arg ctx :name)
              {edn-text :body err :error} (base/save-body ctx :edn)
              why (base/rationale ctx)]
          (cond
            (str/blank? (str name)) (base/malformed branch (base/missing ctx :name))
            err (base/malformed branch err)
            (str/blank? (str edn-text)) (base/malformed branch (base/missing ctx :edn))
            (nil? why) (base/malformed branch (base/missing ctx :rationale))
            :else
            ;; The validation refusal is caught HERE rather than by the outer
            ;; handler, so "this manifest does not compile" — a correctable
            ;; edit, and the loop this tool exists to invite — stays distinct
            ;; from a db write that failed, which is a real failure. The outer
            ;; catch swallowed both and billed the branch for each.
            (if-let [complaint (try (validate! edn-text) nil
                                    (catch Throwable e (ex-message e)))]
              ;; The complaint plus `usage`, which already says a save
              ;; validates before it stores — no new sentence in src/.
              (base/rejected branch
                             (str "`manifest save` refused: " complaint
                                  "\n\n" usage))
              (let [v (us/save! conn :manifest name edn-text "project" why)]
                (base/ok branch
                         (str "Saved manifest '" name "' v" v " — it compiles."
                              " A run configured for '" name "' (config :run :loop)"
                              " will use it; tuning the active manifest is picked"
                              " up on the next run.")
                         :progress? true)))))

        (base/malformed branch (str "Unknown manifest action `" action "`. " usage)))
      (catch Throwable e
        (base/fail branch (str "`manifest " action "` refused: " (ex-message e)
                               "\n\n" usage))))))
