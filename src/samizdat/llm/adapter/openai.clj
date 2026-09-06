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

(ns samizdat.llm.adapter.openai
  "The OpenAI chat-completions family.

  One adapter covers OpenAI, DeepSeek, Zhipu GLM, and any local llama-server
  or vLLM endpoint, because they all speak the same wire format. The
  differences that actually exist are carried as fields on the adapter record
  rather than as separate namespaces, since a subclass whose only content is a
  base URL is not an abstraction.

  The one real variation is where the reasoning stream lives. DeepSeek and GLM
  return `reasoning_content` alongside `content`; others return nothing. The
  field name is configurable and the client folds it into <think> framing so
  the fence parser sees one string either way."
  (:require ;; the java.time.* host shim, before data.json — see samizdat.store.journal
            [jolt.time]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [samizdat.lexicon :as lexicon]
            [samizdat.llm.adapter :as adapter]
            [samizdat.util :as util]))
(def ^:private usage-cap-re
  "Memoized against the lexicon's generation, so an added gateway phrasing
  takes effect on reload rather than at the next restart."
  (util/generation-cache lexicon/gen
                         #(re-pattern (lexicon/wordlist :usage-cap-signals))))


(defn- tool-call->fence
  "A native OpenAI tool_call turned into the harness's text-fence convention, so
  a forced (tool_choice) response parses through exactly the same path a normal
  fenced response does. `arguments` is a JSON string; it is parsed and re-embedded
  as `args` so the downstream fence parser reads one object."
  [tc]
  (let [name (get-in tc [:function :name])
        args (try (json/read-str (str (get-in tc [:function :arguments])))
                  (catch Throwable _ {}))]
    (str "```tool-call\n"
         (json/write-str {:name name :args args})
         "\n```")))

(defn- supports-prefill?
  "Which members of the family continue a flagged trailing assistant message,
  on the endpoint actually configured.

  DeepSeek does, but ONLY from its beta base URL: on /v1 the same request is
  rejected with 'prefix is only available when using beta api', so a prefill
  sent to the wrong endpoint fails the call outright rather than degrading.
  Both halves are checked here so a /v1 config simply does not prefill and
  behaves exactly as it does today.

  Plain OpenAI and a stock local endpoint have no equivalent. Opted into by id
  rather than assumed for the family."
  [provider-id config]
  (and (= :deepseek provider-id)
       (str/includes? (str (:base-url config)) "/beta")))

(defn llama-cpp-endpoint?
  "Whether this config points at a llama.cpp server.

  Two ways to be one, and the second is why this exists. `:local` as a
  provider id is how an operator DECLARES it. `:llama-cpp?` is what
  `client/probe-llama-cpp` DISCOVERED by asking `/props` at startup — so a
  llama-server configured under `:openai`, which RFC-005 recorded as silently
  losing its prefix pinning, gets it anyway.

  The declared form still counts on its own, because a probe needs the server
  to be up and the config to be right about the URL, and neither is
  guaranteed at the moment an operator is setting one up."
  [provider-id config]
  (or (= :local provider-id) (boolean (:llama-cpp? config))))

(defn- local-cache-wire
  "The extra body keys a llama.cpp server honours, or an empty map.

  `cache_prompt` asks the server to reuse the longest common prefix it already
  holds instead of re-prefilling. That is the difference between an inherited
  fork or a fan of probes off one tape costing a completion each and costing a
  full prefill each — see docs/RFCS/RFC-005-provider-layer.md.

  `id_slot` PINS a conversation to a physical KV slot, and is emitted only
  from an explicit `:slots` table in the provider config ({cache-key -> int}).
  Absent, the server picks by prefix similarity and LRU, which is the right
  default: a slot count is a property of how the server was launched, and
  inventing an index would evict another conversation's warm prefix to serve
  a guess. A bad pin costs a re-prefill, never a wrong answer.

  `chat_template_kwargs {enable_thinking false}` turns Qwen-family reasoning
  OFF. llama.cpp has it ON by default and `/no_think` in the PROMPT does not
  disable it — the template decides, not the text — so without this knob a
  local model can spend its whole output budget thinking and return a reply
  with neither content nor a tool call. This layer already treats that reply
  as an error rather than an empty answer (see `client`), which is the right
  reading and does nothing to prevent it; this is what prevents it. From
  llm-repl's `llamacpp` backend, which documents the same trap.

  Opt-out via `:thinking? true` in the provider config, because a reasoning
  model asked to reason is a legitimate configuration — just not the default
  for a harness whose turns must end in a tool call.

  Gated on the endpoint being llama.cpp, so every hosted provider's body is
  byte-identical to what it was. The cache design is llm-repl's `llama-wire`;
  only the seam differs."
  [provider-id config cache-key]
  (if-not (llama-cpp-endpoint? provider-id config)
    {}
    (cond-> {}
      (not (:thinking? config))
      (assoc :chat_template_kwargs {:enable_thinking false})

      (some? cache-key)
      (assoc :cache_prompt true)

      (and cache-key (get (:slots config) cache-key))
      (assoc :id_slot (get (:slots config) cache-key)))))

(defn- reasoning-wire
  "The reasoning fields for a resolved `effort` on `provider-id`.

  `effort` is non-nil here — the caller only reaches this when the run stated
  something. Any ordinary tier (`low`/`high`/`max`) is the same top-level
  `reasoning_effort` string every OpenAI-compatible member of the family
  honours (verified live 2026-09-06: GLM and DeepSeek both act on it).

  The OFF value `\"none\"` is the runaway breaker asking for no reasoning, and
  the wire for that is per-provider because the providers differ:

  - `:deepseek` — `{:thinking {:type \"disabled\"}}`. Its documented, reliable
    disable (3/3 zero reasoning tokens live); `reasoning_effort` is dropped so
    the two knobs cannot disagree.
  - `:glm` — `{:reasoning_effort \"low\"}`. GLM-5.3 CANNOT be turned off
    (z.ai's own migration note; `thinking disabled` measured 0/0/15 reasoning
    tokens, i.e. no effect), so `low` is the least it will think.
  - everything else — `{:reasoning_effort \"none\"}`, unchanged from before.

  This is a provider WIRE fact (how the API spells 'do not think'), not a model
  trait, which is why it lives keyed by provider — the same split dirge draws
  with its DisableWire enum. The fuller per-provider effort table is
  karamazov-07d."
  [provider-id effort]
  (if (= "none" effort)
    (case provider-id
      :deepseek {:thinking {:type "disabled"}}
      :glm      {:reasoning_effort "low"}
      {:reasoning_effort "none"})
    {:reasoning_effort effort}))

(defrecord OpenAIAdapter [provider-id label reasoning-key max-tokens-key]
  adapter/Adapter
  (id [_] provider-id)
  (display-name [_] label)

  (chat-url [_ config] (str (:base-url config) "/chat/completions"))

  ;; NOT simply base-url + /models. DeepSeek's /beta is a chat-completions
  ;; variant only — it serves prefix completion, and /beta/models is a 404 —
  ;; so a run configured for prefilling reported "listed no models" at startup
  ;; and downgraded a real check to a warning. The listing lives on the stable
  ;; path either way.
  (models-url [_ config]
    (str (str/replace (str (:base-url config)) #"/beta/?$" "/v1") "/models"))

  (auth-headers [_ config]
    (if-let [k (:api-key config)]
      {"Authorization" (str "Bearer " k)}
      {}))

  (chat-body [this config {:keys [messages max-tokens temperature prefill force-tool
                                  cache-key reasoning-effort]}]
   ;; The gate is the protocol method on THIS adapter (provenance R3-14), so the
   ;; answer a caller can query and the answer chat-body acts on are one
   ;; path and cannot drift apart.
   (let [use-prefill? (and prefill (adapter/prefill-support? this config))
         ;; The per-call effort wins over the run's default, so a side call
         ;; (a critic, a reflector, the workflow chooser) can ask for less
         ;; thinking than the run is configured for — GLM-5.3 defaults to
         ;; `max`, which spent 24-31s on sub-1k-token prompts. nil means the
         ;; run stated nothing, and the model does whatever it does by default.
         effort (or reasoning-effort (:reasoning-effort config))
         ;; Force a specific finishing tool with native tool_choice — the way
         ;; to make a prefill-less provider (GLM) call `done`/`give_up`. Only
         ;; as a FALLBACK: where a prefill will force the call it is preferred,
         ;; because tool_choice is incompatible with DeepSeek's thinking mode.
         forcing? (and force-tool (not use-prefill?))]
    (cond-> {:model (:model config)
             :messages (if use-prefill?
                         ;; `:prefix true` is what makes the provider CONTINUE
                         ;; this message rather than reply after it. Without
                         ;; the flag a trailing assistant turn is just history,
                         ;; and the model answers below it in prose — the exact
                         ;; failure the prefill exists to prevent.
                         (conj (vec messages)
                               {:role "assistant" :content prefill :prefix true})
                         messages)}
      max-tokens (assoc max-tokens-key max-tokens)
      temperature (assoc :temperature temperature)
      ;; Only when the run stated something. Whether a model thinks was
      ;; previously a property of which one happened to be configured rather
      ;; than something a run stated; this makes it explicit and recorded. The
      ;; OFF value takes each provider's documented disable — see
      ;; `reasoning-wire`.
      (some? effort)
      (merge (reasoning-wire provider-id effort))

      ;; Only the forced tool is exposed.
      forcing?
      (assoc :tools [{:type "function" :function force-tool}]
             :tool_choice {:type "function"
                           :function {:name (:name force-tool)}})

      ;; DeepSeek REJECTS tool_choice while thinking is on ("Thinking mode does
      ;; not support this tool_choice", a 400) — measured live 2026-09-06 — so
      ;; a native forced call on the /v1 endpoint, the one path where a prefill
      ;; cannot force it instead, must turn thinking off for that one request.
      ;; Overrides any effort wire set above, since the two would disagree.
      (and forcing? (= :deepseek provider-id))
      (-> (dissoc :reasoning_effort)
          (assoc :thinking {:type "disabled"}))

      ;; Local llama-server prefix-cache reuse. Merged LAST and only for
      ;; :local, so no hosted provider's body changes.
      :always
      (merge (local-cache-wire provider-id config cache-key)))))

  (prefill-support? [_ config] (supports-prefill? provider-id config))

  (parse-chat [_ body]
    (when-let [choice (first (:choices body))]
      (let [msg (:message choice)]
        {;; A forced tool_choice response carries the call in tool_calls, not
         ;; content — fold it into the fence convention so downstream is blind to
         ;; how the call was produced.
         :content (if-let [tc (first (:tool_calls msg))]
                    (tool-call->fence tc)
                    (:content msg))
         :reasoning (get msg reasoning-key)
         :finish-reason (or (:finish_reason choice) "stop")
         :usage (when-let [u (:usage body)]
                  ;; The cache split is conditional on the provider reporting
                  ;; it, and ABSENT rather than zero when it does not: zero
                  ;; would assert every token missed the cache, which is a
                  ;; different and false claim. The point of keeping these is
                  ;; to reason about cache behaviour across a wide beam, where
                  ;; each branch carries its own diverging prefix, and a
                  ;; fabricated zero would poison exactly that question.
                  (cond-> {:prompt-tokens (or (:prompt_tokens u) 0)
                           :completion-tokens (or (:completion_tokens u) 0)
                           :total-tokens (or (:total_tokens u) 0)}
                    (:prompt_cache_hit_tokens u)
                    (assoc :cache-hit-tokens (:prompt_cache_hit_tokens u))
                    (:prompt_cache_miss_tokens u)
                    (assoc :cache-miss-tokens (:prompt_cache_miss_tokens u))
                    ;; OpenAI reports the hit count nested instead.
                    (get-in u [:prompt_tokens_details :cached_tokens])
                    (assoc :cache-hit-tokens
                           (get-in u [:prompt_tokens_details :cached_tokens]))))})))

  (parse-models [_ body] (mapv :id (:data body)))

  (error-message [_ body]
    (when-let [e (:error body)]
      (str (or (:message e) "unknown error")
           (when-let [c (:code e)] (str " (code " c ")")))))

  (usage-cap? [_ _status body]
    ;; A 429 that means "you are out of credit" must not be retried; a 429 that
    ;; means "slow down" must be. Providers signal the first in the error text
    ;; rather than the status, so this is a text match, and it is deliberately
    ;; narrow: misreading a rate limit as a cap costs the run.
    (let [msg (str (get-in body [:error :message])
                   " " (get-in body [:error :type])
                   " " (get-in body [:error :code]))]
      ;; The vocabulary is wordlists.edn :usage-cap-signals — a fact about
      ;; PROVIDERS, who change their error text without asking, so somebody
      ;; hitting a new gateway's wording can add it and keep working.
      (boolean (re-find (usage-cap-re) msg)))))

(defn openai-family
  "Build an adapter for an OpenAI-compatible endpoint.

  `reasoning-key` names the field carrying a separate reasoning stream, or nil
  when the provider has none. `max-tokens-key` exists because newer OpenAI
  models renamed `max_tokens` to `max_completion_tokens` and reject the old
  one."
  [{:keys [id label reasoning-key max-tokens-key]
    :or {max-tokens-key :max_tokens}}]
  (->OpenAIAdapter id (or label (str/capitalize (name id)))
                   (or reasoning-key :__no_reasoning_field__)
                   max-tokens-key))

(def deepseek
  (openai-family {:id :deepseek :label "DeepSeek" :reasoning-key :reasoning_content}))

(def glm
  (openai-family {:id :glm :label "GLM" :reasoning-key :reasoning_content}))

(def openai
  (openai-family {:id :openai :label "OpenAI"}))

;; A local llama-server / vLLM / LM Studio endpoint. Same wire format, no key.
;;
;; `:reasoning_content` because what a local endpoint SERVES is a model, not a
;; provider: llama-server hands back reasoning_content for a reasoning model
;; exactly as DeepSeek does, and this adapter carried the sentinel, so every
;; local reasoning stream was dropped. Absent for a model that does not reason,
;; which reads as nil — the behaviour this had before.
(def local
  (openai-family {:id :local :label "local" :reasoning-key :reasoning_content}))
