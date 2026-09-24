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

(ns samizdat.agent.live
  "What a person changed about a RUNNING run's model: which model answers it,
  and how hard it thinks.

  A run's llm config is fixed when it starts (api.control/run-llm-config).
  This is the layer over it that a live switch writes — the `model` and
  `effort` intervention kinds — and loop/call-model and workflow/role-ctx
  read on every request, so the change lands on the next turn with no
  boundary to wait for. A switch names a ROLE (the critic, the supervisor,
  the implementors …) or none, which is every role; a role's own switch
  stands over a run-wide one. Forgotten when the run ends."
  (:refer-clojure :exclude [get set!])
  (:require [samizdat.config :as config]
            [samizdat.llm.registry :as registry]))

;; {run-id {role {:provider :model :reasoning-effort}}}, the role :all for a
;; switch that named none.
(defonce ^:private overrides (atom {}))

(defn set!
  "Merge `m` ({:model …}, {:provider …}, {:reasoning-effort …}) into
  `run-id`'s overrides for `role` — :all when it names none."
  [run-id role m]
  (swap! overrides update-in [run-id (or role :all)] merge m)
  nil)

(defn get
  "`run-id`'s overrides, {role {…}}, or nil."
  [run-id]
  (clojure.core/get @overrides run-id))

(defn- over
  "`llm` with override `o` applied: a provider it names replaces the config
  with that provider's (samizdat.config/provider-llm), the rest merges."
  [llm o]
  (cond
    (empty? o) llm
    (and (:provider o) (not= (:provider o) (:provider llm)))
    (config/provider-llm (:provider o) (dissoc (merge (select-keys llm [:reasoning-effort :timeout-ms]) o)
                                               :provider))
    :else (merge llm o)))

(defn apply-to
  "`llm-config` with `run-id`'s overrides for `role` over it: the run-wide
  switch first, then the role's own, so a role named in a switch keeps it
  when a later switch names nobody."
  ([llm-config run-id] (apply-to llm-config run-id nil))
  ([llm-config run-id role]
   (let [o (get run-id)]
     (cond-> llm-config
       (:all o) (over (:all o))
       (and role (clojure.core/get o role)) (over (clojure.core/get o role))))))

(defn in-ctx
  "`ctx` with its llm config — and its adapter, when the provider moved —
  switched for `ctx`'s :role on its run."
  [{:keys [run-id role llm-config] :as ctx}]
  (if-not (get run-id)
    ctx
    (let [c (apply-to llm-config run-id role)]
      (cond-> (assoc ctx :llm-config c)
        (not= (:provider c) (:provider llm-config))
        (assoc :llm-adapter (registry/adapter-for (:provider c)))))))

(defn forget-run! [run-id]
  (swap! overrides dissoc run-id)
  nil)
