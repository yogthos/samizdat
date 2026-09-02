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

(ns samizdat.agent.tools.prompts
  "The `prompt` tool: the agent edits the words it runs under.

  RFC-001 lists `:prompt` as a userspace kind — seeded from the shipped
  template, versioned append-only, owned by the project — and every other
  userspace kind had a tool. `cells`/`cell`/`reload_cells` reach `:cell`,
  `manifest` reaches `:manifest`, and nothing reached `:prompt`. So every word
  the model reads was userspace in principle and unreachable in practice,
  which is the standing rule failing on its own terms: *could the agent change
  this about itself, at runtime, without a rebuild?* The answer was no, not
  because prompts were compiled, but because no seam was exposed.

  This closes it, including the system prompt and the gate messages. A
  supervisor that can retune the words its branches read can tune the loop
  precisely rather than only structurally — a gate that fires at the right
  moment and says the wrong thing is a real failure mode, and rewiring the
  manifest is the wrong instrument for it.

  Deliberately NOT validated the way a cell is. A cell is code and a manifest
  is a graph, so both have a compile that can refuse them; a prompt is prose
  and there is nothing to compile. What it gets instead is the render check —
  selmer has to parse it — because an unbalanced `{% if %}` fails at the
  moment the prompt is used, which for a gate message is mid-run and for the
  system prompt is at the top of every branch."
  (:require [clojure.string :as str]
            [samizdat.agent.tools.base :as base]
            [samizdat.prompt :as prompt]
            [samizdat.store.userspace :as us]
            [samizdat.userspace :as userspace]))

(def ^:private usage
  "Actions: list, show {name, version?}, save {name, body | file, rationale}, versions {name}, revert {name, version, rationale}. A prompt is prose the model reads — the system prompt, a gate's message, a role's instructions. Editing one changes what the harness says, not what it does. rationale: one sentence on why — the history shows it to the next supervisor deciding whether your change stays.")

(defn- render-check
  "Whether selmer can parse `body`, as `nil` or a complaint.

  The only check a prompt can be given, and it is worth having: a template
  with an unbalanced `{% if %}` throws where it is USED, which for a gate
  message is in the middle of a run and for the system prompt is at the top of
  every branch. Better to refuse the save."
  [body]
  (try (prompt/render-str body {}) nil
       (catch Throwable e
         (str "this does not render: " (ex-message e)))))

(defn- shipped? [name]
  (some? (userspace/template :prompt name)))

(defn- render-list []
  (let [stored (into {} (map (juxt :name identity)) (userspace/names :prompt))
        shipped (sort prompt/shipped-prompts)
        all (sort (into (set shipped) (keys stored)))]
    (if (empty? all)
      "No prompts."
      (str/join "\n"
                (for [n all
                      :let [{:keys [version versions]} (get stored n)]]
                  (str n
                       (if version
                         (str "  v" version " (" versions
                              (if (= 1 versions) " version)" " versions)"))
                         "  [template]")
                       (when (and version (shipped? n)) "  [edited]")))))))

(defn- msg
  "One of this tool's messages, from prompts/prompt-tool.md. The tool that
  makes prose editable should not be the one holding its own prose in
  compiled code."
  [ctx]
  (prompt/render "prompt-tool" (assoc ctx :usage usage)))

(defmethod base/run-tool "prompt" [{:keys [branch] :as ctx}]
  (let [action (some-> (base/arg ctx :action) str str/trim str/lower-case not-empty)
        name (some-> (base/arg ctx :name) str str/trim not-empty)]
    (try
      (case action
        nil
        (base/malformed branch (msg {:needs-action true}))

        "list"
        (base/ok branch (render-list))

        "show"
        (if-not name
          (base/malformed branch (base/missing ctx :name))
          (let [v (some-> (base/arg ctx :version) str str/trim not-empty parse-long)
                body (if v
                       (some-> (userspace/conn) (us/load-version :prompt name v) :body)
                       (userspace/body :prompt name))]
            (if body
              (base/ok branch (str name (when v (str " v" v)) ":\n\n" body))
              (base/malformed branch (msg {:no-prompt true :name name :version v})))))

        "versions"
        (if-not name
          (base/malformed branch (base/missing ctx :name))
          (let [rows (userspace/versions :prompt name)]
            (base/ok branch
                     (if (seq rows)
                       (str/join "\n" (map base/version-line rows))
                       (msg {:no-versions true :name name
                             :shipped (shipped? name)})))))

        "save"
        (let [{body :body err :error} (base/save-body ctx :body)
              why (base/rationale ctx)]
          (cond
            (not name) (base/malformed branch (base/missing ctx :name))
            err (base/malformed branch err)
            (nil? body) (base/malformed branch (base/missing ctx :body))
            (nil? why) (base/malformed branch (base/missing ctx :rationale))
            :else
            (if-let [complaint (render-check (str body))]
              ;; The render check is this surface's whole validation, so a
              ;; body that fails it is a rejected edit — nothing was stored.
              (base/rejected branch (msg {:bad-render true :name name
                                          :complaint complaint}))
              (if-let [v (userspace/save! :prompt name (str body) why)]
                (base/ok branch (msg {:saved true :name name :version v})
                         :progress? true)
                (base/fail branch (msg {:unbound true :name name}))))))

        "revert"
        (let [v (some-> (base/arg ctx :version) str str/trim not-empty parse-long)
              why (base/rationale ctx)]
          (cond
            (not name) (base/malformed branch (base/missing ctx :name))
            (nil? v) (base/malformed branch (base/missing ctx :version))
            (nil? why) (base/malformed branch (base/missing ctx :rationale))
            :else
            (if-let [nv (userspace/revert! :prompt name v why)]
              (base/ok branch (msg {:reverted true :name name :from v :version nv})
                       :progress? true)
              (base/malformed branch (msg {:no-revert true :name name :version v})))))

        (base/malformed branch (msg {:unknown-action true :action action})))
      (catch Throwable e
        (base/fail branch (str "prompt " action " failed: " (ex-message e)))))))
