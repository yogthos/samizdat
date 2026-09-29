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

(ns samizdat.agent.tools.battery
  "`battery` — the held-out cases every cell, manifest and policy edit is
  replayed against before it may go live (samizdat.heldout). `list` shows the
  battery; `add {run_id}` freezes a finished run of this project as a case.
  There is no remove: the battery may grow and may not be weakened."
  (:require [clojure.string :as str]
            [samizdat.agent.tools.base :as base]
            [samizdat.heldout :as heldout]
            [samizdat.prompt :as prompt]))

(defn- msg [vars] (prompt/render "battery-tool" vars))

(defmethod base/run-tool "battery" [{:keys [branch conn] :as ctx}]
  (let [action (some-> (base/arg ctx :action) str str/trim str/lower-case not-empty)]
    (case action
      "list"
      (let [{:keys [cases altered]} (heldout/cases conn)]
        (base/ok branch
                 (msg {:listed true
                       :none (empty? cases)
                       :cases (for [c cases]
                                {:id (:id c) :subject (:subject c)
                                 :targets (count (:expect c))
                                 :targets-s (if (= 1 (count (:expect c))) "" "s")
                                 :loop (or (:loop c) "")
                                 :fixture (str (get-in c [:fixture :sha]))})
                       :altered (str/join ", " altered)})))

      "add"
      (if-let [run-id (some-> (base/arg ctx :run_id) str str/trim not-empty)]
        (try
          (let [{:keys [id path targets]} (heldout/draft! conn run-id)]
            (base/ok branch (msg {:added true :id id :path path :targets targets
                                        :targets-s (if (= 1 targets) "" "s")})
                     :progress? true))
          (catch Throwable e
            (base/rejected branch (msg {:add-failed true :run-id run-id
                                        :reason (or (ex-message e) (str e))}))))
        (base/malformed branch (base/missing ctx :run_id)))

      (base/malformed branch (msg {:usage true})))))
