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
            [samizdat.config :as config]
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
  a native call parses through exactly the same path a fenced one does.
  `arguments` is a JSON string, re-embedded as `args` when it parses — and
  embedded AS WRITTEN when it does not, so the fence parser's repair ladder
  gets its chance and a failure is reported to the model as the parse error
  it is, rather than becoming an empty `{}` the tool then calls missing."
  [tc]
  (let [name (get-in tc [:function :name])
        raw (str (get-in tc [:function :arguments]))
        args (try (json/read-str raw) (catch Throwable _ ::unparsed))]
    (str "```tool-call\n"
         (if (= ::unparsed args)
           (str "{\"name\": " (json/write-str name) ", \"args\": " raw "}")
           (json/write-str {:name name :args args}))
         "\n```")))

(defn- native-content
  "What a reply said, with every native call it made appended as a fence:
  the words a model wrote beside its call are kept (glm-5.3 writes a
  sentence and a call), and several calls stay several fences, for the fence
  rule to pick from and count rather than for this to drop quietly."
  [msg]
  (let [calls (:tool_calls msg)
        said (str/trim (str (:content msg)))]
    (if (seq calls)
      (str/join "\n\n" (cond->> (map tool-call->fence calls)
                           (seq said) (cons said)))
      (:content msg))))

(defn- supports-prefill?
  "Whether this endpoint continues a flagged trailing assistant message:
  config :llm :features says (samizdat.config), not the provider id and not
  the URL. It used to be 'DeepSeek, and only on /beta', which was true and
  incomplete: llama.cpp with --jinja continues one too, and the no-call clamp
  that relies on it was a no-op there (karamazov-srw9). The DeepSeek URL rule
  now lives where the features are declared."
  [_provider-id config]
  (config/supports? config :prefill))

(defn- local-cache-wire
  "The llama.cpp-flavoured knobs, each behind the feature that says the
  endpoint has it (config :llm :features; samizdat.config documents them):

  `cache_prompt` (:cache-prompt) asks the server to reuse the longest common
  prefix it already holds instead of re-prefilling. `id_slot` PINS a
  conversation to a physical KV slot, and is emitted only from an explicit
  `:slots` table in the provider config ({cache-key -> int}): a slot count is
  a property of how the server was launched, and inventing an index would
  evict another conversation's warm prefix to serve a guess.

  `chat_template_kwargs {enable_thinking false}` (:thinking-toggle) turns
  Qwen-family reasoning OFF — `/no_think` in the PROMPT does not, the
  template decides — unless the config opts into thinking with `:thinking?
  true`, which a reasoning model asked to reason is.

  `reasoning_budget_tokens` (:reasoning-budget) is a per-CALL cap on
  thinking, honoured by injecting the end-of-thinking tag at the cut; only
  when a caller stated one (karamazov-w7n4).

  Gated per feature rather than per provider id, so a hosted endpoint's body
  is byte-identical to what it was and a llama-server configured under any
  id gets the knobs once the probe has identified it. The cache design is
  llm-repl's `llama-wire`; only the seam differs."
  [_provider-id config cache-key reasoning-budget]
  (cond-> {}
    (and (config/supports? config :thinking-toggle) (not (:thinking? config)))
    (assoc :chat_template_kwargs {:enable_thinking false})

    (and (config/supports? config :cache-prompt) (some? cache-key))
    (assoc :cache_prompt true)

    (and (config/supports? config :cache-prompt) cache-key
         (get (:slots config) cache-key))
    (assoc :id_slot (get (:slots config) cache-key))

    (and (config/supports? config :reasoning-budget) (some? reasoning-budget))
    (assoc :reasoning_budget_tokens (long reasoning-budget))))

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
                                  cache-key reasoning-effort grammar reasoning-budget tools]}]
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
         ;; A GBNF grammar (samizdat.llm.grammar) forces or restricts the
         ;; call AT SAMPLING — cache-safe, since it never touches the prompt,
         ;; and template-free (karamazov-fp21.1). Only an endpoint whose
         ;; features say :grammar sees the field: a strict hosted server 422s
         ;; the whole request over a key it does not know.
         grammar* (when (and grammar (config/supports? config :grammar))
                    grammar)
         ;; Force a specific finishing tool with native tool_choice — the way
         ;; to make a prefill-less provider (GLM) call `done`/`give_up`. Only
         ;; as a FALLBACK: where a prefill will force the call it is preferred,
         ;; because tool_choice is incompatible with DeepSeek's thinking mode;
         ;; and where a grammar forces it, the tools array is not sent either,
         ;; because the template would render it into the prefix.
         forcing? (and force-tool (not use-prefill?) (not grammar*))
         ;; The whole tool surface, natively, on every turn the endpoint takes
         ;; it (config :native-tools, measured) — the way DeepSeek's and
         ;; Zhipu's own harnesses call their models (samizdat.llm.toolspec).
         ;; Never beside a prefill: DeepSeek answers `Function call should
         ;; not be used with prefix` (400, measured 2026-09-24). Never beside
         ;; a grammar, which forces the call at sampling on an endpoint that
         ;; renders tools into the prefix.
         native (when (and (seq tools) (config/supports? config :native-tools)
                           (not use-prefill?) (not grammar*))
                  (mapv (fn [t] {:type "function" :function t}) tools))]
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

      grammar* (assoc :grammar grammar*)

      native (assoc :tools native)

      ;; A force names its tool. With the surface already native the array
      ;; stays whole, so the forced turn's prefix is the same bytes as every
      ;; other turn's; otherwise only the forced tool is exposed.
      forcing?
      (assoc :tools (cond
                      (nil? native) [{:type "function" :function force-tool}]
                      (some #(= (:name force-tool) (get-in % [:function :name])) native) native
                      :else (conj native {:type "function" :function force-tool}))
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
      (merge (local-cache-wire provider-id config cache-key reasoning-budget)))))

  (prefill-support? [_ config] (supports-prefill? provider-id config))

  (parse-chat [_ body]
    (when-let [choice (first (:choices body))]
      (let [msg (:message choice)]
        {;; A native call arrives in tool_calls, not content — folded into the
         ;; fence convention so downstream is blind to how the call was made.
         :content (native-content msg)
         :reasoning (get msg reasoning-key)
         :finish-reason (or (:finish_reason choice) "stop")
         ;; The model that ANSWERED, as the provider names it, or nil when
         ;; the body does not say. Kept apart from the one requested and
         ;; never defaulted to it: a retired or aliased id is served by a
         ;; different model behind a normal 200 with no warning field
         ;; anywhere (z.ai answers glm-4.6 with glm-5.3-flash, DeepSeek
         ;; answers deepseek-chat with deepseek-v4-flash — escapement,
         ;; 2026-09-07), and echoing the request here would hide exactly
         ;; that (karamazov-a28w).
         :model (some-> (:model body) str not-empty)
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
