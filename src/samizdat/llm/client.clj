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

(ns samizdat.llm.client
  "The provider-independent half of talking to a model.

  Everything here applies identically whichever adapter is in play, which is
  the point: a retry ladder that differs by provider is a retry ladder nobody
  can reason about.

  Three behaviours are worth naming.

  A 429 is not one thing. `Retry-After` and the `x-ratelimit-reset-*` headers
  say when the window reopens, and waiting exactly that long beats doubling a
  guess — dirge PR 719. A 429 that means the account is out of credit is a
  wall, not a window, and retrying it burns the run's budget against something
  that will not move, so the adapter gets to say which it is — dirge PR 689.

  A reply with neither content nor reasoning is an error, not an empty answer.
  It usually means the model spent its whole budget thinking, and reporting it
  as a successful empty turn would send the loop round again with nothing.

  Prior assistant turns lose their think blocks on the way out. See
  samizdat.llm.message."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.tools.logging :as log]
            [jolt.http-client :as http]
            [samizdat.lexicon :as lexicon]
            [samizdat.llm.adapter :as adapter]
            [samizdat.llm.message :as message]
            [samizdat.util :as util]
            [samizdat.session :as session]))

(def default-max-retries 2)
(def default-timeout-ms 300000)

(def default-conn-timeout-ms
  "A bound on the TCP handshake alone, separate from the per-read timeout.

  http-client honours this as of v0.0.3; before that it was silently ignored
  (setting O_NONBLOCK needs variadic fcntl, and on Apple arm64 a fixed-arity
  binding corrupts the stack-passed argument), so a connect to a host that
  drops SYNs was bounded only by the kernel's retry limit — about 75 seconds
  on macOS. Every call here passes one now: a provider that is unreachable
  should cost a branch its turn, not its budget, and should not hold up
  startup at all. Fifteen seconds is far past any real handshake and far
  short of the kernel's."
  15000)
;; Never sleep longer than this on a provider's say-so. A header asking for
;; ten minutes should not silently become a ten-minute stall.
(def max-backoff-ms 60000)

(def max-in-run-retry-wait-ms
  "The longest a single retry may wait before the call is abandoned. A 429
  whose reset is farther out than this is a usage cap wearing a rate-limit's
  status: retrying just re-hits it and burns the run's budget, so past this
  bound a retryable error is treated as fatal."
  300000)

;; --- error classification ---------------------------------------------------

(defn retry-after-ms
  "How long the provider asked us to wait, from the response headers, or nil.

  Handles `retry-after` in seconds and the `x-ratelimit-reset-*` family that
  several OpenAI-compatible providers send instead. UNCLAMPED: the in-run cap
  check in `chat` must see the provider's real ask — clamping here hid a usage
  cap wearing a rate-limit's headers under a 60s ceiling that could never
  cross the 300s window (provenance CR1-2). The ceiling on what we
  actually sleep is applied in backoff-ms."
  [headers]
  (let [h (fn [k] (get headers k))
        secs (some-> (or (h "retry-after") (h "Retry-After")) str/trim parse-long)
        reset (some-> (or (h "x-ratelimit-reset-requests")
                          (h "x-ratelimit-reset-tokens"))
                      str/trim
                      (str/replace #"[sm]$" "")
                      parse-long)]
    (when-let [s (or secs reset)]
      (* 1000 (max 0 s)))))

(def ^:private context-overflow-re
  ;; wordlists.edn :context-overflow — how endpoints word a window overflow.
  ;; Data, because the wording is theirs to change and the supervisor's to
  ;; track (karamazov-d41).
  (util/generation-cache lexicon/gen
                         #(re-pattern (lexicon/wordlist :context-overflow))))

(defn context-overflow?
  "Whether this response body says the prompt outgrew the context window.

  A 500 wearing this message is DETERMINISTIC — the same oversized prompt
  fails identically every time — so retrying it on backoff burns wall-clock
  to learn nothing. Observed live on the Qwen baseline (run b8a2b72c,
  llama-server -c 32768): a branch past the wall re-sent the same prompt
  three times with up to 32s of backoff per turn (karamazov-d41). Matched on
  the RAW body so an undecodable error page still classifies."
  [raw-body]
  (boolean (re-find (context-overflow-re) (str raw-body))))

(defn classify
  "Decide what to do about a non-2xx response.

  :retry — transient, try again. :fatal — do not retry, the answer will not
  change. Anything unrecognized is fatal, because retrying an error we do not
  understand spends budget to learn nothing."
  [adapter status body]
  (cond
    (and (= 429 status) (adapter/usage-cap? adapter status body)) :fatal
    (= 429 status) :retry
    (>= status 500) :retry
    (= 408 status) :retry
    :else :fatal))

(defn- backoff-ms
  "2s, 8s, 32s, with up to +25% jitter. Overridden by whatever the provider
  asked for. The jitter is what keeps a beam of branches that all hit the same
  429 from retrying in lockstep and re-colliding on the next window."
  [attempt headers]
  ;; The clamp lives HERE, on what we actually sleep — not on the provider's
  ;; ask, which the cap check in `chat` needs unclamped (same review).
  (min max-backoff-ms
       (or (retry-after-ms headers)
           (long (* (+ 1.0 (rand 0.25))
                    (* 2000 (Math/pow 4 attempt)))))))

;; --- one call ---------------------------------------------------------------

(defn- decode [body]
  (try (json/read-str body :key-fn keyword) (catch Throwable _ nil)))

(defn- post-once [adapter config request]
  (let [url (adapter/chat-url adapter config)
        payload (json/write-str (adapter/chat-body adapter config request))
        started (System/currentTimeMillis)
        resp (http/post url {:headers (merge (adapter/auth-headers adapter config)
                                             {"Content-Type" "application/json"})
                             :body payload
                             :socket-timeout (:timeout-ms config default-timeout-ms)
                             :conn-timeout (:conn-timeout-ms config
                                                             default-conn-timeout-ms)
                             :throw-exceptions false})
        elapsed (- (System/currentTimeMillis) started)
        status (:status resp)
        decoded (decode (:body resp))]
    (log/debug (adapter/display-name adapter) "responded" status "in" elapsed "ms")
    (if (<= 200 status 299)
      (if-let [err (adapter/error-message adapter decoded)]
        ;; Some providers return 200 with an error object in the body.
        {:outcome :fatal :error (str (adapter/display-name adapter) " API error: " err)}
        (if-let [parsed (adapter/parse-chat adapter decoded)]
          (let [merged (message/merge-reasoning (:content parsed) (:reasoning parsed))]
            (if (str/blank? merged)
              ;; The REASON travels from where it is detected. Re-deriving it
              ;; downstream by matching this sentence would make the counter a
              ;; measurement of the wording — reword the message and the
              ;; provider-trouble finding silently stops firing.
              {:outcome :fatal
               :reason :empty-reply
               :error (str (adapter/display-name adapter)
                           " returned neither content nor reasoning. This usually means"
                           " the model spent its entire output budget thinking; raise"
                           " :max-tokens or shorten the context.")}
              {:outcome :ok
               :response {:content merged
                          ;; Carried as well as folded. agent/loop stores this
                          ;; as turns.reasoning_text, and dropping it here left
                          ;; that column empty for every run ever recorded —
                          ;; which reads as "nothing reasoned" rather than as
                          ;; "nobody kept it".
                          :reasoning (:reasoning parsed)
                          :finish-reason (:finish-reason parsed)
                          :usage (:usage parsed)
                          :elapsed-ms elapsed}}))
          {:outcome :fatal
           :error (str (adapter/display-name adapter)
                       " reply had no completion in it: "
                       (subs (str (:body resp)) 0 (min 300 (count (str (:body resp))))))}))
      ;; A context overflow outranks the status-code ladder: it is the one
      ;; 5xx that is deterministic, and the reason travels from where it is
      ;; detected so the loop can respond by compacting rather than retrying
      ;; (karamazov-d41).
      (let [overflow? (context-overflow? (:body resp))]
        (cond-> {:outcome (if overflow? :fatal (classify adapter status decoded))
                 :headers (:headers resp)
                 :error (str (adapter/display-name adapter) " error " status
                             (when-let [m (adapter/error-message adapter decoded)] (str " — " m))
                             (when-not decoded
                               (str " — " (subs (str (:body resp))
                                                0 (min 300 (count (str (:body resp))))))))}
          overflow? (assoc :reason :context-overflow))))))

;; --- the public surface -----------------------------------------------------

(defn chat
  "Send `messages` and return {:content :finish-reason :usage :elapsed-ms}.

  Throws ex-info with :provider and :attempts when every attempt failed. The
  loop is bounded in attempts and each attempt is bounded in wall clock, so a
  stuck provider costs a known amount rather than the run."
  ([adapter config messages] (chat adapter config messages nil))
  ([adapter config messages {:keys [max-tokens temperature max-retries prefill force-tool
                                    cache-key]}]
   (let [request {:messages (message/prepare messages)
                  :max-tokens (or max-tokens (:max-tokens config))
                  :temperature (or temperature (:temperature config))
                  ;; Passed through as given; the adapter decides whether it
                  ;; can honour it, and one that cannot must ignore it rather
                  ;; than approximate it.
                  :prefill prefill
                  ;; Native tool-choice forcing (a gate that names a tool): the
                  ;; adapter sends this tool as a native OpenAI function with
                  ;; tool_choice, forcing the call on providers that don't honour
                  ;; assistant prefill (GLM). See samizdat.agent.arbiter.
                  :force-tool force-tool
                  ;; The stable conversation key an endpoint pins its prefix
                  ;; cache to — a branch id. Adapters that have nowhere to put
                  ;; it MUST ignore it; only the local one emits anything.
                  :cache-key cache-key}
         retries (or max-retries (:max-retries config) default-max-retries)]
     (loop [attempt 0, errors []]
       (let [result (try
                      (post-once adapter config request)
                      (catch Throwable e
                        ;; A transport failure — connection reset, TLS error,
                        ;; socket timeout — is the case retrying exists for.
                        {:outcome :retry :error (str "transport: " (ex-message e))}))
             errors (conj errors (:error result))]
         (cond
           (= :ok (:outcome result))
           (:response result)

           (or (= :fatal (:outcome result)) (>= attempt retries))
           (throw (ex-info (str (adapter/display-name adapter) " call failed: "
                                (last errors))
                           {:provider (adapter/id adapter)
                            :attempts (inc attempt)
                            ;; A KEYWORD reason beside the prose, carried from
                            ;; where the trouble was DETECTED. An empty reply
                            ;; and a refused connection want different
                            ;; responses — more tokens versus wait and retry —
                            ;; and deriving that from a sentence downstream is
                            ;; how a counter ends up measuring the wording.
                            :reason (or (:reason result) :call-failed)
                            :errors errors}))

           ;; A retryable error whose own reset is beyond the in-run window is
           ;; a cap in a rate-limit's clothing: waiting it out would blow the
           ;; run, and retrying sooner just re-hits it. Stop now.
           (when-let [server-wait (retry-after-ms (:headers result))]
             (> server-wait max-in-run-retry-wait-ms))
           (throw (ex-info (str (adapter/display-name adapter)
                                " asked to wait "
                                (retry-after-ms (:headers result))
                                "ms, beyond the retry window — treating as a cap: "
                                (last errors))
                           {:provider (adapter/id adapter)
                            :attempts (inc attempt)
                            :reason :usage-cap
                            :errors errors}))

           :else
           (let [wait (backoff-ms attempt (:headers result))]
             ;; Retries are counted even when the call eventually succeeds. A
             ;; run that got there on the third attempt every time is a run in
             ;; trouble, and the outcome alone cannot say so.
             (session/observe! [:provider :retried])
             (log/warn (adapter/display-name adapter) "attempt" (inc attempt)
                       "failed, retrying in" wait "ms:" (:error result))
             (Thread/sleep wait)
             (recur (inc attempt) errors))))))))

(defn probe-llama-cpp
  "Ask an endpoint whether it is a llama.cpp server, and how many KV slots it
  was launched with. Returns `{:llama-cpp? true :total-slots n}` or nil.

  IDENTIFY, DO NOT GUESS — and do not send hopefully either. RFC-005 recorded
  that `:local` was decided by which config key the endpoint sat under, so a
  llama-server configured as `:openai` silently got no prefix pinning. The
  obvious repair is to send `cache_prompt` everywhere and let servers ignore
  what they do not know, and that repair is wrong: an OpenAI-compatible server
  that validates its body strictly rejects the WHOLE REQUEST over an unknown
  field. dirge measured it (dirge-07ew) — Cerebras answered 422
  `property 'body.prompt_cache_key' is unsupported`, Groq and Volcano Engine's
  DeepSeek answer the same way — so a field sent hopefully is a session that
  cannot make a single request. A probe asks; it does not hope.

  `/props` is llama.cpp's own endpoint and `total_slots` is the field only it
  serves. Anything else — a 404, a hosted provider's error page, a connection
  refused — is `nil`, meaning `not llama.cpp`, which is the safe answer in
  every direction.

  `total_slots` is worth having on its own: RFC-005 said an explicit `:slots`
  table was the only option because `a slot count is a property of how the
  server was launched`. It is, and this is the server saying so."
  [config]
  (try
    (let [base (str/replace (str (:base-url config)) #"/v1/?$" "")
          resp (http/get (str base "/props")
                         {:socket-timeout 5000
                          :conn-timeout (:conn-timeout-ms config
                                                          default-conn-timeout-ms)
                          :throw-exceptions false})]
      (when (<= 200 (:status resp) 299)
        (let [body (decode (:body resp))]
          (when-let [slots (:total_slots body)]
            {:llama-cpp? true :total-slots slots}))))
    (catch Throwable _
      ;; Unreachable, not-llama.cpp and malformed are the same answer here, and
      ;; none of them is a reason not to start: the harness must come up
      ;; against an endpoint that is merely slow to boot.
      nil)))

(defn list-models
  "Model ids the endpoint advertises, or [] when it has no such endpoint."
  [adapter config]
  (if-let [url (adapter/models-url adapter config)]
    ;; Bounded like every other call: this is the boot-time reachability
    ;; probe (core/warm-tls!), and a harness whose provider is unreachable
    ;; must still come up rather than sit in a connect nobody bounded.
    (let [resp (http/get url {:headers (adapter/auth-headers adapter config)
                              :socket-timeout 30000
                              :conn-timeout (:conn-timeout-ms config
                                                              default-conn-timeout-ms)
                              :throw-exceptions false})]
      (if (<= 200 (:status resp) 299)
        (adapter/parse-models adapter (decode (:body resp)))
        []))
    []))
