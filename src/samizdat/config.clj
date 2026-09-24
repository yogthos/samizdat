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
  "Runtime configuration: the environment and the config.edn layers, read
  once at startup.

  Providers are DECLARED under :providers by alias and ASSIGNED to roles
  under :roles, :default being the run's own model (see 'declared providers
  and role assignment' below). With no :roles :default the run falls back to
  HARNESS_PROVIDER, then the first built-in whose API key is present, then
  :local — any OpenAI-compatible endpoint, llama-server included."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [samizdat.layers :as layers]))

(def deep-merge
  "Merge maps left to right, recursing when BOTH values are maps; any other
  collision is won by the later value. The layering primitive for config:
  defaults < the config.edn file layers < explicit overrides. Lives in
  samizdat.layers, which assembles the file layers with it."
  layers/deep-merge)

(defn- env [k] (let [v (jolt.host/getenv k)] (when-not (str/blank? v) v)))

(defn- env-long [k] (some-> (env k) parse-long))

(defn- read-config-file
  "`path` as an EDN map, or {} when absent, unreadable, or not a map — a
  broken config file must never stop the harness. Shared by both file layers
  so they cannot disagree about what a bad file means."
  [path]
  (try (let [v (edn/read-string (slurp (str path)))]
         (if (map? v) v {}))
       (catch Exception _ {})))

(defn project-config
  "The project-local config layer: <root>/.samizdat/config.edn as an EDN map.
  {} when absent, unreadable, or not a map. Precedence sits between the global
  layer and the caller's overrides, so a checkout pins its port/model/db
  without env or code and an explicit override still wins. Mirrors the
  project-local CELLS layer (samizdat.cells/default-dirs)."
  [root]
  (read-config-file (str root "/.samizdat/config.edn")))

(defn- layer-opts
  "Where the config.edn layers are for the project at `root`."
  [root]
  {:root root :global-dir (layers/global-dir)})

(defn global-config-path
  "Where the machine-wide layer lives: <config-home>/samizdat/config.edn, or
  nil when there is no config home to look in."
  []
  (some-> (layers/global-dir) (str "/config.edn")))

(defn global-config
  "The machine-wide config layer as an EDN map — a user's defaults, so they
  are not re-declared per checkout. {} when there is no file, same posture as
  project-config. Same map shape as the project file, so a key means the same
  thing wherever it is set."
  []
  (if-let [p (global-config-path)] (read-config-file p) {}))

(defn file-config
  "Every config.edn file layer as one map, through samizdat.layers: the file
  SAMIZDAT_CONFIG_FILE names, over the project's .samizdat/config.edn, over
  the global file. A project overrides a machine default key by key and
  inherits the rest. Everything that reads config from disk comes through
  here — load-config, the eval settings, the reference paths — so no reader
  can see one layer and not another.

  A layer that does not read is dropped on its own; the others still count,
  and `config-sources` names the broken one."
  [root]
  (or (:value (layers/resolve "config" (layer-opts root))) {}))

(defn- file-layers
  "Each config.edn layer as {:layer :path :value}, highest first."
  [root]
  (or (:layers (layers/resolve "config" (layer-opts root))) []))

(defn config-sources
  "Which config files this process reads, in precedence order (lowest first),
  whether each exists, and why one was dropped (:error). For the boot log and
  /health, so a surprising value can be traced to the file that set it rather
  than guessed at. The global layer is always listed, with a nil path when
  there is no config home; the env layer only when its variable is set."
  [root]
  (let [cands (layers/candidates "config" (layer-opts root))
        errors (into {} (map (juxt :path :error))
                     (:errors (layers/resolve "config" (layer-opts root))))
        present? (fn [p] (boolean (and p (.exists (java.io.File. (str p))))))
        row (fn [{:keys [layer path]}]
              (cond-> {:layer layer :path path :present? (present? path)}
                (get errors path) (assoc :error (get errors path))))
        by-layer (into {} (map (juxt :layer identity)) cands)]
    (vec (concat
          [(row (or (:global by-layer) {:layer :global :path nil}))
           (row (or (:project by-layer) {:layer :project
                                         :path (str root "/.samizdat/config.edn")}))]
          (when-let [e (:env by-layer)] [(row e)])))))

(defn db-location
  "Where the project's database is, as {:path :from}. `env-db` is HARNESS_DB
  or nil.

    :env      the operator named a path; it wins whatever is on disk
    :project  <root>/.samizdat/samizdat.sqlite3 exists
    :legacy   only the pre-1g6b <root>/samizdat.sqlite3 exists — opened where
              it is, and the boot log says so, rather than moved: a file the
              user did not ask to have moved stays where they left it
    :default  neither exists; a fresh project gets the .samizdat/ path

  Root-relative, not cwd-relative: the db belongs to the project being
  worked on, and a served harness with HARNESS_ROOT set elsewhere used to put
  the project's whole history in whatever directory it happened to be
  launched from."
  [root env-db]
  (let [under (str root "/.samizdat/samizdat.sqlite3")
        legacy (str root "/samizdat.sqlite3")
        exists? (fn [p] (.exists (java.io.File. p)))]
    (cond
      env-db           {:path env-db :from :env}
      (exists? under)  {:path under :from :project}
      (exists? legacy) {:path legacy :from :legacy}
      :else            {:path under :from :default})))

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
  (:mode (eval-settings (when root (file-config root)))))

(defn eval-sandbox
  "The sandbox backend setting for the project at `root` (`:auto`, `:none` or
  `:bwrap`)."
  [root]
  (:sandbox (eval-settings (when root (file-config root)))))

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
              ;; What the endpoint was MEASURED to do (2026-09-06): the beta
              ;; URL continues a flagged assistant prefix; `thinking {type
              ;; disabled}` reliably yields no reasoning; a native tool_choice
              ;; is honoured, but only with thinking off (the adapter knows).
              :features #{:prefill :native-tool-choice :thinking-toggle :reasoning-effort :stream}
              :key-env  "DEEPSEEK_API_KEY"
              ;; deepseek-v4-flash is the development and test model: cheap
              ;; enough to run the beam repeatedly. deepseek-v4-pro is the
              ;; second arm. Both think by default (high effort); the
              ;; TypeScript default, deepseek-reasoner, is no longer served by
              ;; the API. Both serve a 1M context — :context-window below is a
              ;; compaction-ladder budget, not the model's window (karamazov-fass).
              :model    "deepseek-v4-flash"}
   ;; The coding endpoint, not the general /api/paas/v4: it is the one dirge
   ;; drives GLM through in practice, tuned for agentic coding traffic. Same
   ;; OpenAI-compatible chat-completions surface, so the openai-family adapter
   ;; handles it unchanged.
   :glm      {:context-window 128000
              :base-url "https://open.bigmodel.cn/api/coding/paas/v4"
              ;; Measured 2026-09-06: ignores a trailing assistant prefix
              ;; entirely, cannot be told not to think (effort low is the
              ;; least), honours tool_choice {type function} despite its docs.
              :features #{:native-tool-choice :reasoning-effort :stream}
              :key-env  "ZHIPU_API_KEY"
              :model    "glm-5.3"
              ;; GLM benefits from a low temperature on coding tasks (dirge
              ;; pins 0.2); the loop leaves it unset for other providers.
              :temperature 0.2}
   :openai   {:context-window 128000
              :base-url "https://api.openai.com/v1"
              :features #{:native-tool-choice :reasoning-effort :stream}
              :key-env  "OPENAI_API_KEY"
              :model    "gpt-4o"}
   ;; A local llama-server / vLLM / LM Studio OpenAI-compatible endpoint.
   :local    {;; A local endpoint is launched with whatever -c it was given, so
              ;; the conservative value is right until HARNESS_CONTEXT_WINDOW
              ;; or the llama.cpp probe says otherwise.
              :context-window 32768
              :base-url "http://127.0.0.1:8080/v1"
              :key-env  nil
              :model    "local-model"
              ;; A bare OpenAI-compatible endpoint until the startup probe
              ;; says which server it is; `llama-cpp-features` is what a
              ;; llama.cpp answer adds (apply-discovery).
              :features #{:native-tool-choice :reasoning-effort :stream}}
   ;; Ollama's NATIVE api, so no /v1 suffix. See llm/adapter/ollama.clj for
   ;; why the native surface rather than Ollama's OpenAI-compatible one.
   :ollama   {:context-window 32768
              :base-url "http://127.0.0.1:11434"
              :key-env  nil
              :model    "qwen3"
              :features #{}}})

;; --- provider FEATURES -------------------------------------------------------
;;
;; What an endpoint can do, as data, so nothing downstream guesses it from a
;; URL or a provider id (karamazov-srw9 found the guess wrong: the local
;; adapter said no prefill while the server continued one; the no-call clamp
;; is a prefill; eight no-call turns on Bonsai got a clamp that did nothing).
;; Every preset DECLARES a set; system/start! ADDS what the probe discovers;
;; a config file may name its own, which replaces the set. The adapter emits
;; only the knobs a feature allows, and infer/force-mechanism picks how a
;; steered turn is forced from gates.edn :force-mechanism's order against it.
;;
;;   :prefill             continues a trailing assistant message (the fence
;;                        force; the reply content may or may not repeat it)
;;   :native-tool-choice  tools + tool_choice {type function}
;;   :grammar             a GBNF `grammar` field applied at sampling (llama.cpp)
;;   :cache-prompt        `cache_prompt` / `id_slot` prefix-cache reuse (llama.cpp)
;;   :reasoning-budget    `reasoning_budget_tokens` per call (llama.cpp)
;;   :thinking-toggle     thinking can be turned OFF on request (DeepSeek's
;;                        thinking {type disabled}, llama.cpp's
;;                        chat_template_kwargs {enable_thinking false})
;;   :reasoning-effort    a top-level reasoning_effort is honoured
;;   :stream              `stream: true` is answered as server-sent chunks, so
;;                        a reply can be watched as it is written

(def llama-cpp-features
  "What a llama.cpp server adds once /props has identified it. Measured on
  stock b10809 and on PrismML's fork at mainline 10687 with --jinja
  (2026-09-17/18): grammar at sampling, prefix-cache reuse, a per-call
  reasoning budget, thinking off through the template, and a trailing
  assistant message continued (thinking on or off; the content repeats the
  prefill, which the parser's reattach already tolerates)."
  #{:grammar :cache-prompt :reasoning-budget :thinking-toggle :prefill})

(defn features-of
  "The features a provider preset declares. Throws on an unknown provider,
  like every other read of the table."
  [provider]
  (or (:features (providers provider))
      (throw (ex-info (str "Unknown provider: " provider)
                      {:provider provider :known (keys providers)}))))

(defn- resolve-features
  "`llm` with the one feature rule that depends on the URL applied to
  whatever features it carries: DeepSeek serves prefix completion only from
  its beta URL and answers 'prefix is only available when using beta api'
  on /v1, so a config pointed there must not claim :prefill, whichever
  layer claimed it. Kept from the days the adapter guessed this from the
  URL; now it edits the declaration instead."
  [provider llm]
  (cond-> llm
    (and (= :deepseek provider)
         (not (str/includes? (str (:base-url llm)) "/beta")))
    (update :features (fnil disj #{}) :prefill)))

(defn- provider-features
  "The declared features for `provider` at `base-url` (see resolve-features
  for the URL rule)."
  [provider base-url]
  (:features (resolve-features provider {:features (features-of provider)
                                         :base-url base-url})))

(defn apply-discovery
  "`llm` with what the startup probe learned merged in: the probe's own facts
  (`:llama-cpp?`, `:total-slots`, `:model-id`) and, for a llama.cpp server,
  `llama-cpp-features` added to the declared set. nil `probed` is the
  endpoint saying nothing, which changes nothing."
  [llm probed]
  (cond-> llm
    probed (merge probed)
    (:llama-cpp? probed) (update :features (fnil into #{}) llama-cpp-features)))

(defn supports?
  "Whether this `llm` config's endpoint has `feature` — the one question a
  cell or a gate asks before choosing a mechanism. No features declared is
  no features: an endpoint nobody described can do nothing special."
  [llm feature]
  (contains? (or (:features llm) #{}) feature))


(def providers-for-test
  "The static provider table, exposed for tests — a live load-config picks a
  provider from the environment, which a test cannot pin without touching env."
  providers)

(defn provider-temperature
  "The temperature a provider runs at: its own default, or the 0.7 family
  fallback. The one place the precedence lives, so config and tests agree."
  [provider]
  (or (:temperature (providers provider)) 0.7))

(defn- detect-provider []
  (or (some-> (env "HARNESS_PROVIDER") str/lower-case keyword)
      (first (for [p [:deepseek :glm :openai]
                   :let [ke (:key-env (providers p))]
                   :when (env ke)]
               p))
      :local))

;; --- declared providers and role assignment ----------------------------------
;;
;; A config file DECLARES endpoints under :providers, by alias, and says which
;; alias serves each role under :roles — the shape dirge's config.json has:
;;
;;   {:providers {:bonsai {:type :local :base-url "http://127.0.0.1:8080/v1"
;;                         :thinking? true :gen-floor-tps 15}
;;                :glm    {:model "glm-5.3"}             ; alias = built-in, no :type
;;                :flash  {:type :deepseek :model "deepseek-v4-flash"}
;;                :vllm   {:type :openai :base-url "https://gpu:8000/v1"
;;                         :api-key "${VLLM_KEY}" :headers {"X-Org" "${ORG}"}}}
;;    :roles {:default :bonsai :supervisor :glm :reader :flash}}
;;
;; :type picks the adapter and the built-in preset that fills whatever the
;; entry leaves out; it defaults to the alias when the alias names a built-in.
;; :api-key-env names the variable holding the key; :api-key is a literal or
;; exactly "${VAR}", and so is a :headers value. Anything else on an entry is
;; an :llm knob (:model, :context-window, :thinking?, :max-tokens, ...).
;;
;; Only the providers something SELECTS are resolved, so a machine-wide file
;; can declare an endpoint whose key this machine does not have.

(def ^:private declaration-keys
  "Entry keys that describe the declaration rather than the endpoint's :llm."
  [:type :api-key-env])

(defn- as-key
  "A provider or role name as a lower-case keyword, whichever spelling a layer
  used: `\"glm\"` is what somebody writes after reading /health."
  [x]
  (some-> x name str/lower-case not-empty keyword))

(defn- expand-var
  "`s` with an exact `${VAR}` replaced from the environment. An unset
  variable is an error naming it: a key someone asked for by name and did not
  get is a misconfiguration, and a request without it fails far less clearly."
  [where s]
  (if-let [[_ v] (and (string? s) (re-matches #"\$\{([A-Za-z_][A-Za-z0-9_]*)\}" s))]
    (or (env v)
        (throw (ex-info (str where " names ${" v "}, which is not set") {:var v})))
    s))

(defn- declared-entry [config alias]
  (some (fn [[k v]] (when (= (as-key k) alias) v)) (:providers config)))

(defn provider-names
  "Every provider `config` can select: the built-ins and its declared aliases."
  [config]
  (distinct (concat (keys providers) (keep as-key (keys (:providers config))))))

(defn- resolve-entry
  "The provider `alias` names in `config`, as {:type :alias :llm}: its adapter
  type and the :llm knobs its declaration sets, keys resolved. Throws naming
  the alias when nothing declares it or its type has no adapter."
  [config alias]
  (let [alias (as-key alias)
        entry (declared-entry config alias)
        type (as-key (or (:type entry) (when (providers alias) alias)))
        known (sort (map name (keys providers)))]
    (cond
      (and (nil? entry) (nil? (providers alias)))
      (throw (ex-info (str "unknown provider " (some-> alias name)
                           "; declared: " (str/join ", " (sort (keep #(some-> % as-key name) (keys (:providers config)))))
                           "; built-in: " (str/join ", " known))
                      {:provider alias :known (provider-names config)}))

      (nil? type)
      (throw (ex-info (str "provider " (name alias) " needs a :type, one of "
                           (str/join ", " known))
                      {:provider alias}))

      (nil? (providers type))
      (throw (ex-info (str "provider " (name alias) " has :type " (name type)
                           ", which no adapter serves; one of " (str/join ", " known))
                      {:provider alias :type type}))

      :else
      (let [where (str "provider " (name alias))]
        {:type type
         :alias alias
         :llm (cond-> (apply dissoc (or entry {}) declaration-keys)
                (and (:api-key-env entry) (not (:api-key entry)))
                (assoc :api-key (env (:api-key-env entry)))
                (:api-key entry) (update :api-key #(expand-var where %))
                (:headers entry) (update :headers
                                         #(into {} (for [[k v] %] [k (expand-var where v)]))))}))))

(def ^:private file-keys
  "The top-level keys a config file may set. :run stays open below this:
  cells read their own :run keys, and a list of them here would be a
  decision about behaviour made in src."
  #{:http :nrepl :db :eval :run :providers :roles})

(defn- refuse-unrecognized!
  "Throw naming the file and the key when a config layer sets a top-level
  key nothing reads. Refused rather than ignored: a key that silently means
  nothing is a setting somebody believes is in effect."
  [layers]
  (doseq [{:keys [path value]} layers
          k (keys value)
          :when (not (contains? file-keys k))]
    (throw (ex-info (str path ": unrecognized key " (pr-str k))
                    {:path path :key k :known file-keys}))))

(declare role-llm)

(defn- check-roles!
  "`config`, once every role it assigns resolves. A typo in :roles would
  otherwise surface only when that role first runs, minutes into a run."
  [config]
  (doseq [role (keys (:roles config))]
    (try (role-llm config (:llm config) role)
         (catch Exception e
           (throw (ex-info (str ":roles " (name role) ": " (ex-message e))
                           (assoc (ex-data e) :role role) e)))))
  config)

(defn load-config
  "Build the config map. `overrides` is merged last so tests and REPL sessions
  can point at a fake provider or an in-memory database without touching env."
  ([] (load-config nil))
  ([overrides]
   (let [;; The project layer layers between defaults and overrides. Root: the
         ;; caller's :run :root if given, then HARNESS_ROOT, else the process
         ;; working dir. The env rung exists because a SERVED harness has no
         ;; other way to name the project it works on: every other run knob has
         ;; an override, and without this one `jolt serve` can only ever build
         ;; whatever directory it was launched from.
         root (or (get-in overrides [:run :root])
                  (env "HARNESS_ROOT")
                  (System/getProperty "user.dir"))
         ;; global < project, as one map (file-config). Env stays INSIDE the
         ;; defaults layer below both files, which is where it always was:
         ;; a file has beaten HARNESS_* since the project layer existed, and
         ;; moving env above the files would silently change every checkout
         ;; that pins a value in .samizdat/config.edn.
         files (file-config root)
         layers (file-layers root)
         _ (refuse-unrecognized! layers)
         ;; The choice belongs to ONE layer: the highest that names a
         ;; :roles :default.
         chosen (or (some #(get-in % [:roles :default])
                          (cons overrides (map :value layers)))
                    (detect-provider))
         declared (deep-merge (select-keys files [:providers])
                              (select-keys overrides [:providers]))
         entry (resolve-entry declared chosen)
         provider (:type entry)
         defaults (providers provider)
         db (db-location root (env "HARNESS_DB"))]
     (-> (deep-merge
      ;; 3985 rather than a common port: 3000 is the busiest address on a
      ;; developer machine, and a harness that silently fails to bind (or
      ;; binds where something else already lives) is worse than one on an
      ;; address nothing else wants.
      {:http     {:port (or (env-long "HARNESS_PORT") 3985)
                  ;; The server's worker pool. Every open event stream (a
                  ;; front end following a run) holds one, and so does a
                  ;; POST /v1/runs for the length of its workflow choice.
                  :worker-threads 32}
       :nrepl    {:port (or (env-long "HARNESS_NREPL_PORT")
                            (env-long "JOLT_NREPL_PORT")
                            7888)}
       ;; :from is carried so start! can say WHERE it opened the db and why
       ;; — a legacy root file in particular is worth one log line.
       :db       {:path (:path db) :from (:from db)}
       :llm      {:provider    provider
                  :base-url    (or (env "HARNESS_BASE_URL") (:base-url defaults))
                  ;; What this endpoint can do, from the preset (see the
                  ;; features section above); the probe adds to it at
                  ;; start, a file may replace it.
                  :features    (provider-features provider
                                                  (or (env "HARNESS_BASE_URL")
                                                      (:base-url defaults)))
                  :api-key     (some-> (:key-env defaults) env)
                  :model       (or (env "HARNESS_MODEL") (:model defaults))
                  ;; Sent only when set — see llm/adapter/openai. Left unset,
                  ;; each model does whatever it does by default; both v4 models
                  ;; THINK by default at `high` effort (verified live
                  ;; 2026-09-06, and DeepSeek's docs), so a run that wants them
                  ;; quiet must say so. POST /v1/runs takes reasoning_effort per
                  ;; run and overrides this; the runaway breaker sets it to the
                  ;; off-value, which the adapter turns into each provider's
                  ;; documented disable wire (deepseek: thinking off; glm-5.3
                  ;; cannot disable, so: effort low).
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
                  ;; The OPERATOR's definition of done (karamazov-a6mj.2): a
                  ;; vector of {:name "..." :check "<shell cmd>"} (pass = exit
                  ;; 0 in the project root) or {:name "..." :judge "<one
                  ;; narrow yes/no question>"} (put to the critic role). No
                  ;; default and no env form: it is per project and belongs
                  ;; in .samizdat/config.edn, the one file under the root the
                  ;; run cannot write, which is what makes it a gate the run
                  ;; cannot weaken. `done` checks the :check criteria and
                  ;; :feature/verify checks both kinds; system/start!
                  ;; refuses a malformed spec. samizdat.agent.acceptance.
                  :acceptance nil
                  ;; What the USER knows that the problem statement does not
                  ;; say (karamazov-a6mj.3): the ground truth a person would
                  ;; answer from. When set and no person is attached
                  ;; (gates.edn :approval :mode :refuse), ask_human is
                  ;; answered by the :user role from this text alone —
                  ;; verbatim entities, "I don't know" where it is silent —
                  ;; so an underspecified task is resolved by asking rather
                  ;; than guessing, and asking is testable. A person, when
                  ;; configured, always outranks it. Per project or per run;
                  ;; no env form. samizdat.agent.tools.ask.
                  :user-context nil
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
      ;; The selected provider's declaration over its built-in preset. An
      ;; override's :llm (a test, a REPL) tunes the result.
      {:llm (:llm entry)}
      files
      overrides)
      (update :llm assoc :provider provider :provider-name (:alias entry))
      ;; The one feature rule that depends on the RESOLVED url, after every
      ;; layer has had its say: DeepSeek off /beta answers a prefill with a
      ;; 400, whatever a file claimed.
      (update :llm #(resolve-features provider %))
      (check-roles!)))))

(defn provider-llm
  "The :llm config for a SPECIFIC provider — a built-in, or an alias `config`
  declares under :providers — with `overrides` merged last. Independent of
  which provider the run's default is, so a role can run on a different
  model than the rest of the run. The shared per-response timeouts still
  come from HARNESS_* env. Throws on an unknown provider."
  [config provider overrides]
  (let [{:keys [type alias llm]} (resolve-entry config provider)
         defaults (providers type)
         base-url (or (:base-url llm) (:base-url defaults))]
     (resolve-features
      type
      (merge
       {:base-url    (:base-url defaults)
        :features    (provider-features type base-url)
        :api-key     (some-> (:key-env defaults) env)
        :model       (:model defaults)
        :context-window (:context-window defaults)
        :temperature (provider-temperature type)
        :max-tokens  (or (env-long "HARNESS_MAX_TOKENS") 16384)
        :timeout-ms  (or (env-long "HARNESS_TIMEOUT_MS") 300000)
        :conn-timeout-ms (or (env-long "HARNESS_CONN_TIMEOUT_MS") 15000)
        :max-response-ms (or (env-long "HARNESS_MAX_RESPONSE_MS") 600000)}
       llm
       (dissoc overrides :provider)
       {:provider type :provider-name alias}))))

(defn role-llm
  "The :llm config for `role` when config :roles assigns it a provider alias
  — `{:reader :flash}` — or nil when it runs on the caller's own model. An
  alias that IS the default's is no assignment: the default carries what the
  startup probe learned about it, a fresh resolution would not. The one
  resolver behind workflow/role-ctx (a role's whole sub-loop) and
  read_digest (one call), so 'which model does this role run on' has one
  answer (karamazov-b76m)."
  [config default-llm role]
  (when-let [alias (as-key (get-in config [:roles role]))]
    (when-not (= alias (as-key (or (:provider-name default-llm) (:provider default-llm))))
      (provider-llm config alias {}))))

(defn redacted
  "The config with every :api-key masked, WHEREVER it sits, for logging and
  for /health.

  A walk rather than a path: [:llm :api-key] is not the only place a key
  lives — every :providers entry may carry its own, and the path version
  once served a nested one cleartext (karamazov-blt.29). Masking by
  key name means the next nested key is masked without anyone remembering
  this function exists."
  [config]
  (walk/postwalk
   (fn [x]
     (cond-> x
       (and (map? x) (some? (:api-key x))) (assoc :api-key "***")
       ;; A header is as likely as the key to carry a secret.
       (and (map? x) (map? (:headers x)))
       (update :headers #(into {} (for [[k _] %] [k "***"])))))
   config))
