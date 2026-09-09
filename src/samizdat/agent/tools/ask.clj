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

(ns samizdat.agent.tools.ask
  "`ask_human` — put a question to a person and wait for the answer.

  The counterpart to the permission gate, over the same queue in
  samizdat.approval: that one asks `may I run this`, this one asks `which of
  these did you want`. Both are bounded by gates.edn `:approval`, and both
  are OFF unless a project turned them on.

  Off by default matters more here than there. A run is autonomous, and a
  tool that could park one indefinitely is a way for the model to stop a
  campaign dead — so with `:mode :refuse` this refuses and says why, and the
  model goes back to deciding for itself, which is what it was going to have
  to do anyway.

  :neutral, never :success. Asking establishes nothing: reporting progress
  would clear the branch's consecutive-failure count and buy it turns for
  having had a question, which is the well-formed-but-useless call the
  progress guards exist to catch. The wording is prompts/ask-tool.md."
  (:require [clojure.string :as str]
            [samizdat.agent.tools.base :as base]
            [samizdat.approval :as approval]
            [samizdat.prompt :as prompt]))

(defn- msg [ctx] (prompt/render "ask-tool" ctx))

(defn- normalize
  "The questions as a vector of maps, whatever shape the model sent.

  A model that sends one question rather than a list, or a bare string
  instead of a map, has asked a perfectly clear question — refusing it on
  shape would spend a turn teaching JSON rather than answering."
  [qs]
  (let [qs (cond (nil? qs) []
                 (sequential? qs) qs
                 :else [qs])]
    (->> qs
         (keep (fn [q]
                 (cond
                   (map? q) (let [text (or (:question q) (:text q) (:prompt q))]
                              (when (not-empty (str text))
                                {:question (str text)
                                 :options (mapv str (:options q))
                                 :multi (boolean (:multi q))}))
                   (not-empty (str q)) {:question (str q) :options []}
                   :else nil)))
         vec)))

(defn- answered
  "What the branch reads back. The questions are echoed beside the answers:
  a turn later the model has only this string, and an answer without its
  question is not something it can act on."
  [questions answers]
  (str/join "\n"
            (map-indexed (fn [i q]
                           (str (:question q) " → "
                                (let [a (nth answers i nil)]
                                  (cond
                                    (sequential? a) (str/join ", " a)
                                    (some? a) (str a)
                                    :else "(no answer)"))))
                         questions)))

(defmethod base/run-tool "ask_human" [{:keys [branch run-id branch-id] :as ctx}]
  (let [questions (normalize (base/arg ctx :questions))
        {:keys [mode wait-ms on-timeout]} (approval/policy)]
    (cond
      (empty? questions)
      (base/malformed branch (msg {:needs-questions true}))

      (not= :block mode)
      ;; Not a failure and not a refusal of the branch's reasoning: there is
      ;; simply nobody on the other end. Say so plainly and let it decide.
      (base/malformed branch (msg {:nobody-configured true}))

      :else
      (let [id (approval/request! {:run-id run-id :branch-id (or branch-id (:id branch))
                                   :kind :question :questions questions})
            answer (approval/await! id wait-ms {:decision (or on-timeout :deny)})]
        (if (:timed-out answer)
          (base/malformed branch (msg {:unanswered true
                                       :seconds (int (/ (or wait-ms 0) 1000))}))
          (base/ok branch (answered questions (vec (:answers answer)))))))))
