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
  progress guards exist to catch. The wording is prompts/ask-tool.md.

  THE SIMULATED USER (karamazov-a6mj.3). With nobody attached, `decide it
  yourself` was the only answer, so every underspecified task was resolved by
  guessing and a run could never be asked to ask. When the operator supplies
  `:run :user-context` — the ground truth a user would know — and no person
  is configured, the question goes to the :user role instead: a model
  answering ONLY from that context, copying entities verbatim, saying `I
  don't know` where the context is silent, never doing the assistant's work
  (thinkingbox's user-LLM, prompts/user-simulator.md). A person, when one is
  configured (:approval :mode :block), always outranks it. The branch is told
  a simulated user answered; the journal records the exchange under
  :simulated-user and bills the call as a side call."
  (:require [clojure.string :as str]
            [clojure.tools.logging :as log]
            [samizdat.agent.tools.base :as base]
            [samizdat.approval :as approval]
            [samizdat.config :as config]
            [samizdat.llm.client :as llm]
            [samizdat.llm.message :as message]
            [samizdat.llm.registry :as registry]
            [samizdat.prompt :as prompt]
            [samizdat.store.journal :as journal]))

(defn- msg [ctx] (prompt/render "ask-tool" ctx))

;; --- the simulated user -----------------------------------------------------

(defn- transcript
  "The branch's conversation as the user model reads it: the visible turns,
  role-labelled, system messages and tool results left out. The user was
  not shown the tool traffic and should not answer from it."
  [messages]
  (->> messages
       (filter #(contains? #{"user" "assistant"} (str (:role %))))
       (map #(str (:role %) ": " (message/strip-think-blocks (str (:content %)))))
       (str/join "\n\n")))

(defn user-prompt
  "The one user message the :user role answers — prompts/user-simulator.md
  over the operator's context, the transcript and the questions. Pure, so
  the rules the template states are testable."
  [{:keys [context transcript questions]}]
  (prompt/render "user-simulator"
                 {:context (str context)
                  :transcript (if (str/blank? (str transcript)) "(nothing yet)" (str transcript))
                  :questions (mapv (fn [q] (update q :options #(not-empty (vec %)))) questions)
                  :several (> (count questions) 1)}))

(defn- user-model
  "The adapter and config the simulated user runs on: the :user role's when
  config :roles assigns one (config/role-llm, the same resolver
  as every role), else the branch's own."
  [{:keys [config llm-adapter llm-config]}]
  (if-let [llm (config/role-llm config llm-config :user)]
    {:adapter (registry/adapter-for (:provider llm)) :config llm :role :user}
    {:adapter llm-adapter :config llm-config :role :branch}))

(defn- simulate!
  "Put `questions` to the simulated user. Returns the answer text, or throws
  — the caller decides what an unreachable user means for the branch."
  [{:keys [branch conn run-id] :as ctx} context questions]
  (let [{:keys [adapter config role]} (user-model ctx)
        prompt (user-prompt {:context context
                             :transcript (transcript (:messages branch))
                             :questions questions})
        reply (llm/chat adapter config [{:role "user" :content prompt}])
        answer (str/trim (message/strip-think-blocks (str (:content reply))))]
    (when (and conn run-id)
      (journal/note! conn run-id :simulated-user
                     {:branch-id (:id branch)
                      :data {:questions (mapv #(select-keys % [:question :options]) questions)
                             :answer answer :role role :model (:model config)}})
      (journal/record-side-call! conn run-id {:branch-id (:id branch) :turn (:turn branch)
                                              :kind :simulated-user :role role
                                              :model (:model config) :usage (:usage reply)}))
    answer))

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
        {:keys [mode wait-ms on-timeout]} (approval/policy)
        context (not-empty (str/trim (str (get-in ctx [:config :run :user-context]))))]
    (cond
      (empty? questions)
      (base/malformed branch (msg {:needs-questions true}))

      ;; Nobody configured, but the operator said what the user knows: the
      ;; simulated user answers. `ok`, like a person's answer, and labelled
      ;; as simulated so the branch cannot report "the user confirmed X"
      ;; about a context the operator wrote down.
      (and (not= :block mode) context)
      (try
        (let [answer (simulate! ctx context questions)]
          ;; One reply to all the questions, the way a person answers a
          ;; message with several — `answered` pairs answers by position and
          ;; would show the rest as unanswered.
          (base/ok branch (str (str/join "\n" (map :question questions))
                               "\n→ " answer
                               (msg {:simulated true}))))
        (catch Throwable e
          (log/warn "ask_human: the simulated user failed:" (ex-message e))
          ;; unavailable, on base's reasoning: an outside capability that
          ;; could not be reached is not the branch's fault. The branch is
          ;; back where it would be with no context at all.
          (base/ok branch (msg {:simulator-down true :error (ex-message e)}))))

      (not= :block mode)
      ;; Not a failure and not a refusal of the branch's reasoning: there is
      ;; simply nobody on the other end. Say so plainly and let it decide.
      (base/malformed branch (msg {:nobody-configured true}))

      :else
      (let [id (approval/request! {:run-id run-id :branch-id (or branch-id (:id branch))
                                   :kind :question :questions questions})
            answer (approval/await! id wait-ms {:decision (or on-timeout :deny)})]
        (cond
          (:timed-out answer)
          (base/malformed branch (msg {:unanswered true
                                       :seconds (int (/ (or wait-ms 0) 1000))}))
          ;; The person saw the question and chose not to answer it — told
          ;; as such, not as a set of empty answers.
          (= :deny (:decision answer))
          (base/ok branch (msg {:declined true :note (:note answer)}))
          :else
          (base/ok branch (answered questions (vec (:answers answer)))))))))
