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

(ns samizdat.api.openai
  "The OpenAI-compatible surface.

  A caller pointing an OpenAI client at this gets a chat completion whose
  content is the verified answer. The harness trace rides along in a
  non-standard `harness` field, which clients ignore and humans want.

  `raw: true` bypasses the loop entirely and forwards to the provider. That is
  the control arm: the same question, same model, no verification, and it is
  worth keeping precisely because it is the comparison anyone will ask for."
  (:require [clojure.string :as str]
            [samizdat.agent.beam :as beam]
            [samizdat.api.control :as api-control]
            [samizdat.llm.client :as llm]
            [samizdat.llm.registry :as registry]
            [samizdat.store.journal :as journal]))

(defn- last-user-content [messages]
  (->> messages
       (filter #(= "user" (or (:role %) (get % "role"))))
       last
       (#(or (:content %) (get % "content")))))

(defn- completion-envelope [model content extra]
  (merge
   {:id (str "chatcmpl-" (random-uuid))
    :object "chat.completion"
    :created (quot (System/currentTimeMillis) 1000)
    :model model
    :choices [{:index 0
               :message {:role "assistant" :content content}
               :finish_reason "stop"}]}
   extra))

(defn- render-answer
  "What the caller sees. The answer, then the evidence that earned it, because
  an unverifiable answer from a verification harness is worth less than one
  whose artifacts are listed."
  [answer artifacts]
  (let [confirmed (filter #(= "confirmed" (:claim_status %)) artifacts)
        measured (filter #(= "empirical" (:claim_status %)) artifacts)]
    (str answer
         (when (seq confirmed)
           (str "\n\n---\n\nVerified along the way:\n"
                (str/join "\n" (for [a confirmed]
                                 (str "- [" (:kind a) "/" (:tier a) "] " (:claim a))))))
         ;; Listed apart from the verified block and labeled, because the whole
         ;; value of that block is that everything in it was decided by an
         ;; engine. A measurement was not.
         (when (seq measured)
           (str "\n\nMeasured along the way — computations at the parameters"
                " stated, not proofs:\n"
                (str/join "\n" (for [a measured]
                                 (str "- [" (:kind a) "] " (:claim a)))))))))

(defn chat-completion
  "Run the harness on the last user message and answer in OpenAI's shape."
  [{:keys [conn config]} body]
  (let [llm-config (:llm config)
        adapter (registry/adapter-for (:provider llm-config))
        model (:model llm-config)
        messages (or (:messages body) (get body "messages"))
        problem (last-user-content messages)]
    (cond
      (str/blank? problem)
      {:status 400
       :body {:error {:message "no user message in `messages`" :type "invalid_request_error"}}}

      ;; The bypass. Same model, same question, no verification.
      (or (:raw body) (get body "raw"))
      (let [r (llm/chat adapter llm-config messages)]
        {:status 200
         :body (completion-envelope model (:content r)
                                    {:usage (:usage r) :harness {:mode "raw"}})})

      :else
      ;; Registered in api.control/active like a run started by POST /v1/runs,
      ;; so POST /v1/runs/:id/abort works on it — without the abort atom and
      ;; the registration this run was unabortable for its whole (potentially
      ;; hours-long) life (karamazov-blt.13). The request thread still blocks:
      ;; that IS the OpenAI-compat contract this endpoint exists for.
      (let [abort (atom false)
            run-id* (atom nil)
            r (try
                (beam/run! {:conn conn :config config
                            :llm-adapter adapter :llm-config llm-config
                            :problem problem
                            :abort abort
                            :on-start (fn [rid]
                                        (reset! run-id* rid)
                                        (swap! api-control/active assoc rid {:abort abort}))
                            :max-turns (or (:max_turns body) (:max-turns body)
                                           (get-in config [:run :max-turns]))
                            :beam-width (or (:beam_width body) (:beam-width body)
                                            (get-in config [:run :beam-width]))
                            :token-budget (or (:token_budget body) (:token-budget body)
                                              (get-in config [:run :token-budget]))})
                (finally
                  (when-let [rid @run-id*]
                    (swap! api-control/active dissoc rid))))
            artifacts (journal/artifacts conn (:run-id r))
            answered (= :completed (:status r))]
        {:status 200
         :body (completion-envelope
                model
                (if answered
                  (render-answer (:answer r) artifacts)
                  ;; An exhausted run ships its progress report, not a failure
                  ;; string: never ship nothing, never ship a lie. Other
                  ;; unanswered statuses (abort, error) keep the plain line.
                  (or (:report-text r)
                      (str "The harness did not reach a verified answer ("
                           (name (:status r)) ").\n\n"
                           (or (beam/summary r) ""))))
                {:harness {:mode "agent"
                           :run_id (:run-id r)
                           :status (name (:status r))
                           :report (:report r)
                           :branches (mapv (fn [b]
                                             {:id (:id b) :status (name (:status b))
                                              :confirmed (count (filter #(= :confirmed (:claim-status %))
                                                                        (:artifacts b)))})
                                           (:branches r))}})}))))
