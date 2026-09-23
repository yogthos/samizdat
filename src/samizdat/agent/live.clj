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
  `effort` intervention kinds — and loop/call-model reads on every request,
  so the change lands on the next turn of every branch with no boundary to
  wait for. Forgotten when the run ends."
  (:refer-clojure :exclude [get]))

;; {run-id {:model "…" :reasoning-effort "…"}}
(defonce ^:private overrides (atom {}))

(defn set!
  "Merge `m` ({:model …} and/or {:reasoning-effort …}) into `run-id`'s
  overrides."
  [run-id m]
  (swap! overrides update run-id merge m)
  nil)

(defn get
  "`run-id`'s overrides, or nil."
  [run-id]
  (clojure.core/get @overrides run-id))

(defn apply-to
  "`llm-config` with `run-id`'s overrides over it."
  [llm-config run-id]
  (if-let [o (get run-id)] (merge llm-config o) llm-config))

(defn forget-run! [run-id]
  (swap! overrides dissoc run-id)
  nil)
