;; samizdat - a claim-first verification harness
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

(ns samizdat.llm.registry
  "Provider keyword to adapter.

  The only place that knows the full set. Adding a provider means writing an
  adapter and adding one line here; nothing in the loop or the client changes."
  (:require [clojure.string :as str]
            [samizdat.llm.adapter :as adapter]
            [samizdat.llm.adapter.ollama :as ollama]
            [samizdat.llm.adapter.openai :as openai]))

(def adapters
  {:deepseek openai/deepseek
   :glm openai/glm
   :openai openai/openai
   :local openai/local
   :ollama ollama/ollama})

(defn providers
  "Every provider an adapter exists for."
  []
  (keys adapters))

(defn adapter-for
  "The adapter for a provider keyword. Throws naming what is available rather
  than returning nil, since a nil adapter fails much later and less clearly."
  [provider]
  (or (get adapters provider)
      (throw (ex-info (str "No adapter for provider " provider
                           ". Known: " (str/join ", " (sort (map name (keys adapters)))))
                      {:provider provider :known (keys adapters)}))))

(defn describe
  "One line per adapter, for /v1/models and for logging."
  []
  (for [[k a] (sort-by key adapters)]
    {:provider k :name (adapter/display-name a)}))
