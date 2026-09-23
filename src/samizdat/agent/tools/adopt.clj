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

(ns samizdat.agent.tools.adopt
  "The `adopt` tool: the supervisor answers what is on offer to its project.

  A project's workflow is its own from the first run, so what a later release
  ships — a new role, a changed template — and what the project stored before
  its workflow lived in files are OFFERED, not applied (userspace/offers).
  This is how the offer is answered: take it (checked like any edit, then
  written and versioned with the reason) or decline it (remembered with the
  reason, so it is not offered again)."
  (:require [clojure.string :as str]
            [samizdat.agent.tools.base :as base]
            [samizdat.prompt :as prompt]
            [samizdat.userspace :as userspace]))

(def ^:private usage
  "Actions: list, show {kind, name}, take {kind, name, rationale}, decline {kind, name, rationale}. kind is manifest, prompt, policy or cell. rationale: one sentence on why — the next supervisor reads it.")

(defn- msg [ctx]
  (prompt/render "adopt-tool" (assoc ctx :usage usage)))

(def ^:private kinds {"manifest" :manifest "prompt" :prompt "policy" :policy "cell" :cell})

(defn- offer-ctx [o]
  {:kind (name (:kind o)) :name (:name o) :offer (name (:offer o))
   :edited (:edited? o) :version (:version o) :path (:path o)})

(defn- current-text [o]
  (some-> (userspace/project-path (:kind o) (:name o))
          java.io.File. (#(when (.isFile %) (slurp %)))))

(defmethod base/run-tool "adopt" [{:keys [branch] :as ctx}]
  (let [action (some-> (base/arg ctx :action) str str/trim str/lower-case not-empty)
        kind-s (some-> (base/arg ctx :kind) str str/trim str/lower-case not-empty)
        kind (get kinds kind-s)
        nm (some-> (base/arg ctx :name) str str/trim not-empty)
        named (fn [f]
                (cond
                  (not (and kind-s nm)) (base/malformed branch (base/missing ctx :kind :name))
                  (not kind) (base/malformed branch (msg {:bad-kind true :kind kind-s}))
                  :else (f)))]
    (try
      (case action
        nil (base/malformed branch (msg {:needs-action true}))

        "list"
        (base/ok branch (msg {:listing true :offers (not-empty (mapv offer-ctx (userspace/offers)))}))

        "show"
        (named
         #(if-let [o (some (fn [o] (when (and (= kind (:kind o)) (= nm (:name o))) o))
                           (userspace/offers))]
            (base/ok branch (msg (assoc (offer-ctx o) :showing true
                                        :text (userspace/offer-text o)
                                        :current (when-not (= :new (:offer o))
                                                   (current-text o)))))
            (base/malformed branch (msg {:no-offer true :kind kind-s :name nm}))))

        "take"
        (named
         #(if-let [why (base/rationale ctx)]
            (let [r (userspace/adopt! kind nm why)]
              (cond
                (:no-offer r) (base/malformed branch (msg {:no-offer true :kind kind-s :name nm}))
                (:problem r)
                (let [p (:problem r)]
                  (base/rejected branch (msg (assoc (offer-ctx (:offer r)) :refused true
                                                    :stage (name (or (:stage p) :check))
                                                    :line (:line p) :column (:column p)
                                                    :message (:message p)))))
                :else
                (base/ok branch (msg (assoc (offer-ctx (:adopted r)) :adopted true
                                            :version (:version r)))
                         :progress? true)))
            (base/malformed branch (base/missing ctx :rationale))))

        "decline"
        (named
         #(if-let [why (base/rationale ctx)]
            (if-let [o (userspace/decline! kind nm why)]
              (base/ok branch (msg (assoc (offer-ctx o) :declined true)) :progress? true)
              (base/malformed branch (msg {:no-offer true :kind kind-s :name nm})))
            (base/malformed branch (base/missing ctx :rationale))))

        (base/malformed branch (msg {:unknown-action true :action action})))
      (catch Throwable e
        (base/fail branch (str "adopt " action " failed: " (ex-message e)))))))
