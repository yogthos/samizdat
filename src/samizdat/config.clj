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

(ns samizdat.config
  "Runtime configuration, read from the environment once at startup.

  Provider selection mirrors the TypeScript harness: an explicit
  HARNESS_PROVIDER wins, otherwise the first provider whose API key is present.
  In-process GGUF inference is not carried over — point HARNESS_BASE_URL at any
  OpenAI-compatible endpoint (including llama-server) instead."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.walk :as walk]))

(defn deep-merge
  "Merge maps left to right, recursing when BOTH values are maps; any other
  collision is won by the later value. The layering primitive for config:
  defaults < project .samizdat/config.edn < explicit overrides."
  [& ms]
  (apply merge-with (fn [a b] (if (and (map? a) (map? b))
                               (deep-merge a b)
                               b))
         ms))

(defn project-config
  "The project-local config layer: <root>/.samizdat/config.edn as an EDN map.
  {} when absent, unreadable, or not a map — a broken project file must never
  stop the harness. Precedence sits between the built-in defaults and the
  caller's overrides, so a checkout pins its port/model/db without env or code
  and an explicit override still wins. Mirrors the project-local CELLS layer
  (samizdat.cells/default-dirs)."
  [root]
  (try (let [v (edn/read-string (slurp (str root "/.samizdat/config.edn")))]
         (if (map? v) v {}))
       (catch Exception _ {})))

;; --- the eval toggle --------------------------------------------------------

(def eval-defaults
  "Which image the REPL runs in, absent an operator saying otherwise.

  `:project` RATHER THAN `:harness`, deliberately. The mode names an image:

    :off      no REPL at all — the tools are withheld and the REPL-first
              sections of the system prompt are suppressed with them.
    :project  a `jolt nrepl-server` subprocess rooted at the PROJECT, under an
              OS sandbox. What a role building a project should be talking to.
    :harness  the live harness image, in-process. The supervisor's, and only
              behind the mutation protocol.

  Defaulting to `:harness` would have left karamazov-zrq open for everyone who
  did not read a release note — a P0 whose escape was observed live is not
  closed by making the fix opt-in. So the DANGEROUS mode is the one an operator
  opts into, not the safe one.

  `:sandbox :auto` resolves to the platform's backend — seatbelt on macOS,
  bubblewrap on Linux where it is installed — `:none` skips it, and `:bwrap`
  asks for bubblewrap by name and fails closed without it (karamazov-zrq.8).
  `:none` is legitimate rather than a footgun: inside a container, or on a host
  without a backend, the subprocess split alone still ends in-process access to
  the harness and still fixes the classpath and cwd bugs. The OS layer hardens
  that; it is not what makes it correct."
  {:mode :project :sandbox :auto})

(def ^:private eval-modes #{:off :project :harness})
(def ^:private eval-sandboxes #{:auto :none :bwrap})

(defn eval-settings
  "The `:eval` block of a project config, normalised to
  `{:mode … :sandbox …}`.

  PURE, and a total function of whatever the file happened to contain. It
  follows `project-config`'s rule — a broken project file must never stop the
  harness — with the direction that rule implies for a security control: an
  unreadable setting falls back to the DEFAULT, never to the open image. A
  string `\"project\"` is not the keyword `:project` and is not guessed at,
  because guessing is how an operator who meant `:off` silently gets a REPL."
  [cfg]
  (let [m (:eval cfg)
        m (if (map? m) m {})]
    {:mode    (get eval-modes (:mode m) (:mode eval-defaults))
     :sandbox (get eval-sandboxes (:sandbox m) (:sandbox eval-defaults))}))

(defn eval-mode
  "The eval mode for the project rooted at `root`. nil root — a test, a bare
  REPL — gets the default."
  [root]
  (:mode (eval-settings (when root (project-config root)))))

(defn eval-sandbox
  "The sandbox backend setting for the project at `root` (`:auto`, `:none` or
  `:bwrap`)."
  [root]
  (:sandbox (eval-settings (when root (project-config root)))))

(def harness-image-roles
  "The roles that keep the LIVE harness image under `:mode :project`.

  Only the supervisor, because only the supervisor's job is the harness: it
  reads the run's health and changes manifests, cells, prompts and policy, and
  a project image rooted at somebody else's repo cannot see any of that. Its
  kernel-source writes are what the mutation protocol is for.

  Not in roles.edn. A run that could add itself to this set by editing
  userspace would be granting itself the harness image, which is the escape
  this whole bead is about. A role nobody has heard of gets `:project` — the
  safe direction, and the one that makes adding a role harmless."
  #{:supervisor "supervisor"})

(defn eval-image
  "Which image `role` evaluates in under `mode`: `:off`, `:project` or
  `:harness`.

  `:mode :project` is a posture for the RUN, not a single answer for every
  role — the supervisor stays in the harness image inside it. Resolving this
  in one pure function keeps the prompt and the router from disagreeing: the
  prompt telling a supervisor it is in a separate project image, while its
  evals actually land in the harness, is the same false claim this bead is
  otherwise about removing."
  [mode role]
  (case mode
    :off :off
    :harness :harness
    (if (contains? harness-image-roles role) :harness :project)))

(def ^:private providers
  {;; /beta rather than /v1, for prefix completion. A gate that names one tool
   ;; steers by ending the request mid-fence rather than by asking, which
   ;; DeepSeek serves only from the beta endpoint — on /v1 the same request is
   ;; rejected outright ("prefix is only available when using beta api").
   ;; Verified that /beta serves ordinary completions identically, so this is
   ;; not a trade: nothing else about the run changes. The adapter checks the
   ;; URL anyway and simply does not prefill against /v1, so overriding
   ;; HARNESS_BASE_URL back is safe.
   :deepseek {;; The model's context window, which the compaction ladder reads:
              ;; every rung is a FRACTION of this, so without it the whole
              ;; ladder is inert and folds never happen. Per provider because
              ;; it is a property of the model, and overridable with
              ;; HARNESS_CONTEXT_WINDOW because a table cannot keep up with
              ;; what endpoints serve.
              ;;
              ;; A wrong value is not a correctness bug, it moves WHEN folding
              ;; starts: too small folds early and spends summarizer calls,
              ;; too large folds late and risks an overflow the ladder existed
              ;; to prevent.
              :context-window 128000
              :base-url "https://api.deepseek.com/beta"
              :key-env  "DEEPSEEK_API_KEY"
              ;; deepseek-v4-flash is the development and test model: cheap
              ;; enough to run the beam repeatedly. deepseek-v4-pro is the
              ;; second arm. Note the TypeScript default, deepseek-reasoner,
              ;; is no longer served by the API.
              :model    "deepseek-v4-flash"}
   ;; The coding endpoint, not the general /api/paas/v4: it is the one dirge
   ;; drives GLM through in practice, tuned for agentic coding traffic. Same
   ;; OpenAI-compatible chat-completions surface, so the openai-family adapter
   ;; handles it unchanged.
   :glm      {:context-window 128000
              :base-url "https://open.bigmodel.cn/api/coding/paas/v4"
              :key-env  "ZHIPU_API_KEY"
              :model    "glm-5.3"
              ;; GLM benefits from a low temperature on coding tasks (dirge
              ;; pins 0.2); the loop leaves it unset for other providers.
              :temperature 0.2}
   :openai   {:context-window 128000
              :base-url "https://api.openai.com/v1"
              :key-env  "OPENAI_API_KEY"
              :model    "gpt-4o"}
   ;; A local llama-server / vLLM / LM Studio OpenAI-compatible endpoint.
   :local    {;; A local endpoint is launched with whatever -c it was given, so
              ;; the conservative value is right until HARNESS_CONTEXT_WINDOW
              ;; or the llama.cpp probe says otherwise.
              :context-window 32768
              :base-url "http://127.0.0.1:8080/v1"
              :key-env  nil
              :model    "local-model"}
   ;; Ollama's NATIVE api, so no /v1 suffix. See llm/adapter/ollama.clj for
   ;; why the native surface rather than Ollama's OpenAI-compatible one.
   :ollama   {:context-window 32768
              :base-url "http://127.0.0.1:11434"
              :key-env  nil
              :model    "qwen3"}})

(def providers-for-test
  "The static provider table, exposed for tests — a live load-config picks a
  provider from the environment, which a test cannot pin without touching env."
  providers)

(defn provider-temperature
  "The temperature a provider runs at: its own default, or the 0.7 family
  fallback. The one place the precedence lives, so config and tests agree."
  [provider]
  (or (:temperature (providers provider)) 0.7))

(defn- env [k] (let [v (jolt.host/getenv k)] (when-not (str/blank? v) v)))

(defn- env-long [k] (some-> (env k) parse-long))

(defn- detect-provider []
  (or (some-> (env "HARNESS_PROVIDER") str/lower-case keyword)
      (first (for [p [:deepseek :glm :openai]
                   :let [ke (:key-env (providers p))]
                   :when (env ke)]
               p))
      :local))

(defn load-config
  "Build the config map. `overrides` is merged last so tests and REPL sessions
  can point at a fake provider or an in-memory database without touching env."
  ([] (load-config nil))
  ([overrides]
   (let [provider (detect-provider)
         defaults (or (providers provider)
                      (throw (ex-info (str "Unknown HARNESS_PROVIDER: " provider)
                                      {:provider provider
                                       :known (keys providers)})))
         ;; The project layer layers between defaults and overrides. Root: the
         ;; caller's :run :root if given, then HARNESS_ROOT, else the process
         ;; working dir. The env rung exists because a SERVED harness has no
         ;; other way to name the project it works on: every other run knob has
         ;; an override, and without this one `jolt serve` can only ever build
         ;; whatever directory it was launched from.
         root (or (get-in overrides [:run :root])
                  (env "HARNESS_ROOT")
                  (System/getProperty "user.dir"))
         project (project-config root)]
     (deep-merge
      ;; 3985 rather than a common port: 3000 is the busiest address on a
      ;; developer machine, and a harness that silently fails to bind (or
      ;; binds where something else already lives) is worse than one on an
      ;; address nothing else wants.
      {:http     {:port (or (env-long "HARNESS_PORT") 3985)}
       :nrepl    {:port (or (env-long "HARNESS_NREPL_PORT")
                            (env-long "JOLT_NREPL_PORT")
                            7888)}
       :db       {:path (or (env "HARNESS_DB") "samizdat.sqlite3")}
       :llm      {:provider    provider
                  :base-url    (or (env "HARNESS_BASE_URL") (:base-url defaults))
                  :api-key     (some-> (:key-env defaults) env)
                  :model       (or (env "HARNESS_MODEL") (:model defaults))
                  ;; Sent only when set — see llm/adapter/openai. Left unset,
                  ;; each model does whatever it does by default, which for
                  ;; deepseek-v4-pro is to think and for deepseek-v4-flash is
                  ;; not to. A run that cares should say so; POST /v1/runs
                  ;; takes reasoning_effort per run and overrides this.
                  :reasoning-effort (env "HARNESS_REASONING_EFFORT")
                  :max-tokens  (or (env-long "HARNESS_MAX_TOKENS") 16384)
                  ;; What the compaction ladder measures pressure against. Its
                  ;; rungs are fractions of this; absent, samizdat.agent.compaction
                  ;; routes :none and no fold ever happens.
                  :context-window (or (env-long "HARNESS_CONTEXT_WINDOW")
                                      (:context-window defaults))
                  ;; A provider default (GLM pins 0.2 for coding) wins over the
                  ;; family default of 0.7; HARNESS_TEMPERATURE overrides both.
                  :temperature (or (some-> (env "HARNESS_TEMPERATURE") parse-double)
                                   (provider-temperature provider))
                  ;; Per-read inactivity bound (SO_RCVTIMEO on the socket).
                  :timeout-ms  (or (env-long "HARNESS_TIMEOUT_MS") 300000)
                  ;; Bound on the TCP handshake alone. Honoured as of
                  ;; http-client v0.0.3; before that a connect to a host that
                  ;; drops SYNs ran to the kernel's retry limit (~75s), which
                  ;; is a whole branch turn spent before the first byte.
                  :conn-timeout-ms (or (env-long "HARNESS_CONN_TIMEOUT_MS")
                                       15000)
                  ;; Total wall-clock bound on one response, across all reads.
                  ;; A peer that trickles a byte every few seconds resets the
                  ;; per-read timer forever, so :timeout-ms alone does not bound
                  ;; the call. Deliberately BELOW the turn deadline (900000) so
                  ;; the HTTP layer gives up first, with a typed exception that
                  ;; unwinds the thread and closes the socket. If the scheduler's
                  ;; deadline fires first it only abandons the branch's turn --
                  ;; the thread stays parked in the read and leaks.
                  :max-response-ms (or (env-long "HARNESS_MAX_RESPONSE_MS") 600000)}
       ;; Generous by default: self-building is the primary use, and a
       ;; REPL-first feature run spends many turns prototyping before it ships.
       ;; Compaction keeps context bounded regardless of turn count (older
       ;; turns become one-line digests), and the turn-budget gate nudges
       ;; toward shipping as the cap nears. A blocking HTTP caller that wants a
       ;; tighter bound sets HARNESS_MAX_TURNS.
       :run      {;; Carried into :run so beam/workflow read the same root
                  ;; project-config was layered from. Without it the env rung
                  ;; would pick the project's .samizdat/config.edn and then run
                  ;; against the working dir anyway.
                  :root       root
                  :max-turns  (or (env-long "HARNESS_MAX_TURNS") 1000)
                  :beam-width (or (env-long "HARNESS_BEAM_WIDTH") 5)
                  ;; Tokens the whole run may spend, summed over every turn's
                  ;; total_tokens; nil is unbounded. The beam ends the run
                  ;; :exhausted when it is crossed (karamazov-aqsr.3).
                  :token-budget (env-long "HARNESS_TOKEN_BUDGET")
                  ;; Which loop manifest drives a run. The workflows table holds
                  ;; many named, versioned manifests; this picks one by name (its
                  ;; latest version). nil means the factory "loop". A project can
                  ;; pin its own via .samizdat/config.edn, and the agent can add
                  ;; or tune manifests at runtime with the `manifest` tool.
                  :loop       (env "HARNESS_LOOP")
                  ;; Which board manifest the feature loop's implement stage
                  ;; runs. nil means "board"; "board-bt" is the behavior-tree
                  ;; variant being A/B'd (karamazov-fut). Per-project via
                  ;; .samizdat/config.edn like :loop.
                  :board-manifest (env "HARNESS_BOARD_MANIFEST")
                  ;; The ship gate's test rung, ON by default. `done` is a hard
                  ;; gate on a green test (b1a4b88) — but verify-on? needs a
                  ;; :verify-cmd or this flag, and neither had a default, so the
                  ;; headline gate was inert on every run that did not ship a
                  ;; .samizdat/config.edn. Focused rather than the whole suite:
                  ;; it runs only the test namespaces the branch touched, so a
                  ;; project with no configured command still pays seconds, and
                  ;; a branch that changed no test file is refused by the TDD
                  ;; rung before anything is spawned.
                  :verify-focused? (not= "0" (or (env "HARNESS_VERIFY_FOCUSED") "1"))
                  ;; The TDD half: a change with no test file in it is refused.
                  ;; Read with a default of true at the use site already; named
                  ;; here so it is visible and switchable.
                  :require-test? (not= "0" (or (env "HARNESS_REQUIRE_TEST") "1"))
                  ;; Cross-branch sharing of engine-confirmed artifacts. Off by
                  ;; default: shared lemmas may cost the beam its diversity, and
                  ;; whether they earn it is exactly what sweep-widths measures.
                  :share-artifacts? (= "1" (env "HARNESS_SHARE_ARTIFACTS"))
                  ;; Winner-takes-all: the first verified `done` ends the run.
                  ;; Right for a question with one answer, wrong for a research
                  ;; campaign, where it returns the cheapest qualifying result
                  ;; and terminates every other line. Off means a shipped
                  ;; branch goes inactive holding its answer while the rest
                  ;; keep exploring, and the best is ranked at the end.
                  :stop-on-first-done? (not= "0" (or (env "HARNESS_STOP_ON_FIRST_DONE")
                                                     "1"))}}
      project
      overrides))))

(defn provider-llm
  "The :llm config for a SPECIFIC provider — its base URL, model, temperature,
  and API key (from the provider's key-env in the environment) from the built-in
  providers table, with `overrides` merged last. Independent of which provider
  the run detected as its default, so a role can be assigned a different model
  than the rest of the run (per-role model assignment). The shared per-response
  timeouts still come from HARNESS_* env. Throws on an unknown provider."
  [provider overrides]
  (let [defaults (or (providers provider)
                     (throw (ex-info (str "unknown provider for a role: " provider)
                                     {:provider provider :known (keys providers)})))]
    (merge
     {:provider    provider
      :base-url    (:base-url defaults)
      :api-key     (some-> (:key-env defaults) env)
      :model       (:model defaults)
      :temperature (provider-temperature provider)
      :max-tokens  (or (env-long "HARNESS_MAX_TOKENS") 16384)
      :timeout-ms  (or (env-long "HARNESS_TIMEOUT_MS") 300000)
      :conn-timeout-ms (or (env-long "HARNESS_CONN_TIMEOUT_MS") 15000)
      :max-response-ms (or (env-long "HARNESS_MAX_RESPONSE_MS") 600000)}
     overrides)))

(defn redacted
  "The config with every :api-key masked, WHEREVER it sits, for logging and
  for /health.

  A walk rather than a path: [:llm :api-key] is not the only place a key
  lives — a role spec under :run :role-models may carry its own :api-key
  override (role-ctx merges it into the provider config), and the path
  version served exactly that one cleartext (karamazov-blt.29). Masking by
  key name means the next nested key is masked without anyone remembering
  this function exists."
  [config]
  (walk/postwalk
   (fn [x]
     (if (and (map? x) (some? (:api-key x)))
       (assoc x :api-key "***")
       x))
   config))
