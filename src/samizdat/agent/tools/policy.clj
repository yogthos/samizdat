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

(ns samizdat.agent.tools.policy
  "The :policy kind of userspace, as a tool: gates.edn, phases.edn,
  wordlists.edn, manual.edn, prompt-chain.edn — versioned per project like
  every other piece of userspace.

  This closes the gap karamazov-blt.5 named: RFC-010 lists 'move a threshold'
  among the supervisor's four tuning instruments and the supervisor prompt
  says so too, but no tool wrote the :policy kind — the only route was raw
  `eval` of userspace/save! plus knowing to call gates/reload-config!, none of
  it discoverable. Per the recorded design principle, a capability with no
  discovery path does not exist from the supervisor's point of view.

  Save is validated the way the mutation protocol validates a cell, scaled to
  what a policy table is: the body must PARSE as EDN, and after it is stored
  the affected caches are reloaded and their derived tables recompiled —
  gates' :when forms, the phase machine, the manual's var resolution. A save
  the recompile rejects is ROLLED BACK (revert to the prior version, or
  re-save the shipped template when there is none) and the previous policy is
  live again, so a typo in the 1500-line gates.edn cannot take the harness
  down between turns."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [samizdat.agent.gates :as gates]
            [samizdat.agent.phases :as phases]
            [samizdat.agent.tools.base :as base]
            [samizdat.lexicon :as lexicon]
            [samizdat.manual :as manual]
            [samizdat.agent.roles :as roles]
            [samizdat.prompt :as prompt]
            [samizdat.store.userspace]
            [samizdat.userspace :as userspace]))

(def shipped-policies
  "The policy tables that ship, ENUMERATED not globbed — same reason as every
  other resource list (a classpath has no directory listing)."
  ["gates" "manual" "phases" "prompt-chain" "roles" "wordlists"])

(defn- msg [ctx] (prompt/render "policy-tool" ctx))

(defn- usage [] (msg {:usage true}))

(defn reload-and-verify!
  "Reload whatever caches serve `name`, and force the derived tables to
  recompile so a semantically broken body fails HERE, inside the save's
  rollback, rather than mid-run at the next threshold read. Throws on any
  failure; nil on success.

  Tables with no compiled cache (prompt-chain) are covered by the userspace
  read-cache invalidation that every save already does."
  [name]
  (case name
    "gates"     (do (gates/reload-config!)
                    ;; Compiling the steer table evaluates every gate's :when
                    ;; form — the real validation, not just the EDN parse.
                    (gates/gates)
                    (gates/describe))
    "phases"    (do (phases/reload!)
                    (phases/table)
                    (phases/transitions))
    "wordlists" (do (lexicon/reload!)
                    ;; Force the wordlist regexes to compile.
                    (lexicon/wordlist :usage-cap-signals))
    "manual"    ;; Every :name in the manual must resolve; render walks them.
                (manual/render)
    "roles"     ;; Every role must still declare a surface, and every tool it
                ;; names must be one the loop can dispatch — a surface naming
                ;; a tool that does not exist silently narrows that role.
                (let [registered (roles/all-tool-names)]
                  (doseq [r (roles/names)
                          :let [surface (roles/surface r)]]
                    (when-not (or (= :all surface) (set? surface))
                      (throw (ex-info (str "role " r " declares no tool surface")
                                      {:role r})))
                    (when (and registered (set? surface))
                      (when-let [unknown (seq (remove registered surface))]
                        (throw (ex-info (str "role " r " names unregistered tools: "
                                             (str/join ", " unknown))
                                        {:role r :unknown unknown}))))))
    nil)
  nil)

(defn- known? [name] (some #{name} shipped-policies))

(defn- render-list []
  (let [stored (into {} (map (juxt :name identity)) (userspace/names :policy))]
    (str/join "\n"
              (for [nm (sort (distinct (concat shipped-policies (keys stored))))]
                (if-let [row (get stored nm)]
                  (str nm "  v" (:version row) " (" (:versions row)
                       (if (= 1 (:versions row)) " version)" " versions)"))
                  (str nm "  [" (msg {:still-template true}) "]"))))))

(defn- rollback!
  "Make the pre-save policy live again: revert to the version before `v`, or
  re-save the shipped template when this save created the first version.
  Reloads afterwards — the pre-save state compiled once, so it compiles now."
  [name v]
  (let [why (msg {:rollback-rationale true})]
    (if (> v 1)
      (userspace/revert! :policy name (dec v) why)
      (userspace/save! :policy name (userspace/template :policy name) why)))
  (reload-and-verify! name))

(defmethod base/run-tool "policy" [{:keys [branch] :as ctx}]
  (let [action (some-> (base/arg ctx :action) str str/trim str/lower-case not-empty)
        name (some-> (base/arg ctx :name) str str/trim not-empty)]
    (try
      (case action
        nil
        (base/malformed branch (usage))

        "list"
        (base/ok branch (render-list))

        "show"
        (cond
          (not name) (base/malformed branch (base/missing ctx :name))
          (not (or (known? name) (seq (userspace/versions :policy name))))
          (base/malformed branch (msg {:no-policy true :name name
                                       :names (str/join ", " shipped-policies)}))
          :else
          (let [v (some-> (base/arg ctx :version) str str/trim not-empty parse-long)
                body (if v
                       (some-> (userspace/conn)
                               (samizdat.store.userspace/load-version :policy name v)
                               :body)
                       (userspace/body :policy name))]
            (if body
              (base/ok branch (str name (when v (str " v" v)) ":\n\n" body))
              (base/malformed branch (msg {:no-policy true :name name
                                           :names (str/join ", " shipped-policies)})))))

        "versions"
        (if-not name
          (base/malformed branch (base/missing ctx :name))
          (let [rows (userspace/versions :policy name)]
            (base/ok branch
                     (if (seq rows)
                       (str/join "\n" (map base/version-line rows))
                       (msg {:no-versions true :name name})))))

        "save"
        (let [{body :body err :error} (base/save-body ctx :edn)
              why (base/rationale ctx)]
          (cond
            (not name) (base/malformed branch (base/missing ctx :name))
            err (base/malformed branch err)
            (str/blank? (str body)) (base/malformed branch (base/missing ctx :edn))
            (nil? why) (base/malformed branch (base/missing ctx :rationale))
            (not (known? name))
            (base/malformed branch (msg {:no-policy true :name name
                                         :names (str/join ", " shipped-policies)}))
            :else
            (let [parsed (try {:ok (edn/read-string (str body))}
                              (catch Throwable e {:error (ex-message e)}))]
              (if (:error parsed)
                ;; A body that does not read is a rejected edit, not a branch
                ;; failure: nothing was stored and the complaint says where.
                (base/rejected branch (msg {:bad-edn true :name name
                                            :complaint (:error parsed)}))
                ;; Warm the cache so the seed exists and the version we might
                ;; roll back to is real, then store and recompile.
                (do (userspace/body :policy name)
                    (let [v (userspace/save! :policy name (str body) why)]
                      (if (nil? v)
                        (base/fail branch (msg {:unbound true :name name}))
                        (try
                          (reload-and-verify! name)
                          (base/ok branch (msg {:saved true :name name :version v})
                                   :progress? true)
                          (catch Throwable e
                            (rollback! name v)
                            ;; Rolled back to the prior version, so the harness
                            ;; is where it started — a rejected edit.
                            (base/rejected branch
                                           (msg {:rolled-back true :name name
                                                 :complaint (ex-message e)})))))))))))

        "revert"
        (let [v (some-> (base/arg ctx :version) str str/trim not-empty parse-long)
              why (base/rationale ctx)]
          (cond
            (not name) (base/malformed branch (base/missing ctx :name))
            (nil? v) (base/malformed branch (base/missing ctx :version))
            (nil? why) (base/malformed branch (base/missing ctx :rationale))
            :else
            (if-let [v' (userspace/revert! :policy name v why)]
              (do (reload-and-verify! name)
                  (base/ok branch (msg {:reverted true :name name
                                        :from v :version v'})
                           :progress? true))
              (base/malformed branch (msg {:no-revert true :name name :version v})))))

        (base/malformed branch (usage)))
      (catch Throwable e
        (base/fail branch (str "`policy " action "` failed: " (ex-message e)
                               "\n\n" (usage)))))))
