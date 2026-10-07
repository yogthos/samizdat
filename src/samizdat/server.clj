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

(ns samizdat.server
  "The HTTP surface.

  Routes are a table of ruuter route maps, matched best-match: a literal
  segment beats a parameter, so the table's order does not matter.

  This namespace is pure logic: redefining `handler` against a running process
  takes effect on the next request. See samizdat.system."
  (:require ;; the java.time.* host shim, before data.json — see samizdat.store.journal
            [jolt.time]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.tools.logging :as log]
            [samizdat.agent.gates :as gates]
            [samizdat.agent.gitdiff :as gitdiff]
            [samizdat.api.control :as control]
            [samizdat.api.openai :as openai]
            [samizdat.api.runs :as api-runs]
            [samizdat.api.stream :as stream]
            [samizdat.approval :as approval]
            [samizdat.config :as config]
            [samizdat.engine.proc :as proc]
            [samizdat.llm.client :as llm-client]
            [samizdat.store.db :as db]
            [samizdat.security.token :as token]
            [samizdat.system :as system]
            [samizdat.userspace :as userspace]
            [ruuter.core :as ruuter]))

(defn json-response
  ([body] (json-response 200 body))
  ([status body]
   {:status status
    :headers {"Content-Type" "application/json"}
    :body (json/write-str body)}))

(defn- body-json [req]
  (let [b (:body req)]
    (when b
      (try (json/read-str (if (string? b) b (slurp b)) :key-fn keyword)
           (catch Throwable _ nil)))))

(defn- url-decode
  "provenance R3-12: query values arrive %XX-encoded with + for space. Decode to
  bytes and build the string once from UTF-8, so a multibyte char spread
  across escapes reassembles whole. A % that does not head a valid escape
  passes through raw — the client already sent it, and a bad query string
  should not become a 500."
  [s]
  (if-not (or (str/includes? s "%") (str/includes? s "+"))
    s
    (let [hex? (fn [c] (try (Integer/parseInt (str c) 16) true
                            (catch Throwable _ false)))
          out (java.io.ByteArrayOutputStream.)]
      (loop [i 0]
        (if (>= i (count s))
          (String. (.toByteArray out) "UTF-8")
          (let [c (nth s i)]
            (cond
              (= c \+) (do (.write out 32) (recur (inc i)))
              (and (= c \%)
                   (< (+ i 2) (count s))
                   (hex? (nth s (inc i)))
                   (hex? (nth s (+ i 2))))
              (do (.write out (Integer/parseInt (subs s (inc i) (+ i 3)) 16))
                  (recur (+ i 3)))
              :else (do (doseq [b (.getBytes (str c) "UTF-8")] (.write out b))
                        (recur (inc i))))))))))

(defn- query-param [req k]
  (when-let [qs (:query-string req)]
    (some->> (str/split qs #"&")
             (keep #(let [[a v] (str/split % #"=" 2)] (when (= a k) v)))
             first
             url-decode)))

(defn- long-param [req k] (some-> (query-param req k) parse-long))

(defn- ctx [] {:conn (system/conn) :config (system/config)})

;; --- handlers ---------------------------------------------------------------

(defonce ^:private started-at (str (java.time.Instant/now)))

(def harness-identity
  "Which harness THIS process is: the checkout's commit and whether its tree
  had uncommitted changes, read once, on the first /health — so a checkout
  that moves on afterwards shows as a revision the process is not running —
  and when the process started. A stale
  `serve` answered /health like a current one, so a client could not tell it
  was talking to old code until a run failed (karamazov-uk77). Nil fields
  where there is no checkout to read, as from a built binary."
  (let [identity* (delay
                   (let [dir (System/getProperty "user.dir")
                         git (fn [& args]
                               (let [r (apply proc/run {:timeout-ms 5000} "git" "-C" dir args)]
                                 (when (and (not (:timeout r)) (zero? (long (or (:exit r) 1))))
                                   (str/trim (str (:out r))))))
                         rev (not-empty (git "rev-parse" "HEAD"))]
                     {:revision rev
                      :dirty (when rev (boolean (seq (git "status" "--porcelain" "--untracked-files=no"))))}))]
    (fn [] (assoc @identity* :started_at started-at))))

(defn- health [_req]
  (let [cfg (system/config)]
    (json-response
     {:status "ok"
      :schema_version (db/schema-version (system/conn))
      :active_runs (count @control/active)
      ;; Which code is answering (karamazov-uk77).
      :harness (harness-identity)
      ;; DEFAULTS, said plainly. :run/share-artifacts? in particular is not
      ;; what a given run is doing — beam/run! forces it on for any seeded run
      ;; — and reporting it flat once said sharing was off during a run that
      ;; had already served 91 shared artifacts. The per-run truth is on the
      ;; run detail endpoint as share_artifacts.
      :config_defaults (config/redacted (select-keys cfg [:llm :run :db]))
      ;; Which files those defaults were layered from, lowest first.
      :config_sources (config/config-sources (get-in cfg [:run :root]))
      ;; Which provider serves which role, and what each declared alias is.
      :providers (config/redacted (:providers cfg))
      :roles (:roles cfg)
      ;; Kept under the old key as well: this is a published endpoint and the
      ;; GUI reads it. Removing it is a separate change from correcting it.
      :config (config/redacted (select-keys cfg [:llm :run :db]))})))

(defn- models [_req]
  (let [{:keys [model provider]} (:llm (system/config))]
    (json-response {:object "list"
                    :data [{:id model :object "model" :owned_by (name provider)}]})))

(defn- chat-completions [req]
  (let [r (openai/chat-completion (ctx) (body-json req))]
    (json-response (or (:status r) 200) (:body r))))

(defn- harness-models
  "What the provider actually serves, for the UI's model picker.

  Deliberately NOT /v1/models, which is the OpenAI-compatibility endpoint and
  answers a different question — what this harness serves as a model — and
  would start lying if it listed upstream's catalogue instead.

  `current` is what a run gets when it names nothing. A provider with no
  listing endpoint, or one that is unreachable, yields an empty list rather
  than an error: the picker then offers only the configured default, which is
  exactly today's behaviour and still a working form."
  [_req]
  (let [{:keys [model] :as llm} (:llm (system/config))
        served (try (llm-client/list-models (system/adapter) llm)
                    (catch Throwable e
                      (log/warn "model listing failed:" (ex-message e))
                      []))]
    (json-response {:current model
                    :models (vec (distinct (cons model served)))})))

(defn- gate-table [_req]
  (json-response {:gates (gates/describe) :thresholds (gates/config)}))

(defn layout-body
  "What this project has AUTHORED of the terminal UI's settings, as EDN text,
  or nil.

  tui.edn is a layered settings file (samizdat.layers), and the front end
  assembles it itself: its own env file and project file, then this, then a
  person's ~/.config/samizdat/tui.edn, then the file it ships. So this serves
  the project's layer and nothing below it — the project's .samizdat/tui.edn
  under this process's root, else a version the agent saved. Serving the
  shipped file when nothing was authored would put the defaults ABOVE the
  person's global file and silently override it.

  Served because a front end may run on another machine than the project it
  watches. Text, not parsed: a client that is going to read it anyway gains
  nothing from a round trip through JSON."
  []
  {:layout (or (when-let [r (userspace/project-root)]
                 (let [f (java.io.File. (str r "/.samizdat/tui.edn"))]
                   (when (.isFile f) (slurp f))))
               (let [b (userspace/body :policy "tui")]
                 (when (not= b (userspace/template :policy "tui")) b)))})

(defn- layout-table [_req]
  (json-response (layout-body)))

;; {:at ms :root path :snapshot m} — see gates.edn :git-snapshot-ttl-ms for why
;; a cache exists at all.
(defonce ^:private git-cache (atom nil))

(defn cached-snapshot
  "`gitdiff/snapshot` for `root`, at most once per :git-snapshot-ttl-ms.

  Keyed on the root as well as the clock, so a harness whose project moved
  does not serve the previous one's branch for the rest of the window."
  [root]
  (let [ttl (or (gates/threshold :git-snapshot-ttl-ms) 0)
        now (System/currentTimeMillis)
        c @git-cache]
    (if (and c (= root (:root c)) (< (- now (:at c)) ttl))
      (:snapshot c)
      (let [s (gitdiff/snapshot root)]
        (reset! git-cache {:at now :root root :snapshot s})
        s))))

(defn project-body
  "What a front end needs to caption itself: which project, which branch, how
  dirty, which model.

  Served rather than read locally for the same reason the layout is: the TUI
  holds no filesystem knowledge of the project and no database handle, so a
  TUI pointed at a harness on another machine — or merely started from
  another directory — would otherwise caption the wrong repo with perfect
  confidence. Only this process knows what it is working on.

  Every field is nullable and the endpoint never fails: a harness outside a
  git tree still has a project name, and a front end that cannot draw a
  branch should still draw the rest of its footer."
  []
  (let [cfg (system/config)
        root (get-in cfg [:run :root])
        snap (cached-snapshot root)]
    {:project (some-> root (str/split #"/") last not-empty)
     :root root
     :branch (:branch snap)
     :staged (:staged snap)
     :unstaged (:unstaged snap)
     :untracked (:untracked snap)
     :last_commit (:last-commit snap)
     :provider (some-> (get-in cfg [:llm :provider]) name)
     ;; The alias config.edn declared (bonsai, deepseek-flash): what the
     ;; footer names and what /model takes. :provider is its adapter type.
     :provider_name (some-> (get-in cfg [:llm :provider-name]) name)
     :model (get-in cfg [:llm :model])
     :context_window (get-in cfg [:llm :context-window])
     ;; What happens when a run needs a person: refuse, block (ask), or a
     ;; simulated user. The footer shows it, and /mode changes it.
     :approval_mode (some-> (:mode (approval/policy)) name)}))

(defn- project-table [_req]
  (json-response (project-body)))

;; {:at ms :root path :files v} — the same window as the git snapshot: a
;; person typing an @-mention asks on every key.
(defonce ^:private files-cache (atom nil))

(defn cached-files
  "`gitdiff/project-files` for `root`, at most once per :git-snapshot-ttl-ms."
  [root]
  (let [ttl (or (gates/threshold :git-snapshot-ttl-ms) 0)
        now (System/currentTimeMillis)
        c @files-cache]
    (if (and c (= root (:root c)) (< (- now (:at c)) ttl))
      (:files c)
      (let [fs (gitdiff/project-files root)]
        (reset! files-cache {:at now :root root :files fs})
        fs))))

(defn files-body
  "The project's files whose path holds `q` (any case), for an @-mention:
  a match in the file's own name before one only in its directories, then
  shorter paths first. At most `limit`, capped by gates.edn
  :project-files-shown; :total says how many matched."
  [q limit]
  (let [q (str/lower-case (str q))
        fs (or (cached-files (get-in (system/config) [:run :root])) [])
        base #(str/lower-case (last (str/split % #"/")))
        hits (->> fs
                  (filter #(str/includes? (str/lower-case %) q))
                  (sort-by (juxt #(if (str/includes? (base %) q) 0 1) count identity)))
        cap (min (or limit Long/MAX_VALUE) (or (gates/threshold :project-files-shown) 50))]
    {:files (vec (take cap hits)) :total (count hits)}))

(defn- files-table [req]
  (json-response (files-body (query-param req "q") (long-param req "limit"))))

;; --- routing ----------------------------------------------------------------
;;
;; ruuter route maps: {:method :path :response}. A path segment starting with
;; ':' binds, and the bindings arrive under the request's :params. Matching is
;; best-match — a literal segment beats a parameter — so the order of the
;; table does not matter.
;;
;; A handler defined above is named as #(handler %), not #'handler: ruuter
;; calls :response only when it is a fn?, which a var is not, and calling
;; through the name still reads the var on every request, so a REPL
;; redefinition lands without restarting the server.

(def ^:private slow-ms-cap 10000)

(defn- clamp-slow-ms
  "/slow exists so the smoke probe can prove /health still answers while a
  handler is busy; its ms parameter is a dial for \"briefly busy\", not a lease
  on a connection thread, so it is clamped (provenance R3-4)."
  [ms]
  (min (max (or ms 1000) 0) slow-ms-cap))

(defn- slow
  "Sleeps, so the smoke probe can prove /health still answers while a handler is
  busy. That is the property the server's worker pool buys and
  the reason a multi-minute beam can share a process with a UI."
  [req]
  (let [ms (clamp-slow-ms (long-param req "ms"))]
    (Thread/sleep ms)
    (json-response {:slept_ms ms})))

(defn- not-found [req]
  (json-response 404 {:error {:message (str "Not found: "
                                            (str/upper-case (name (:request-method req)))
                                            " " (:uri req))
                              :type "not_found"}}))

(def routes
  [{:method :get :path "/health" :response #(health %)}
   {:method :get :path "/slow" :response #(slow %)}
   {:method :get :path "/v1/models" :response #(models %)}
   {:method :post :path "/v1/chat/completions" :response #(chat-completions %)}
   {:method :get :path "/v1/harness/gates" :response #(gate-table %)}
   ;; The terminal UI's own arrangement, so a front end that holds no
   ;; database handle can still see the version the agent saved.
   {:method :get :path "/v1/harness/layout" :response #(layout-table %)}
   {:method :get :path "/v1/harness/models" :response #(harness-models %)}
   ;; Which project, which branch, how dirty, which model — the footer and the
   ;; GIT panel. Served because only this process is bound to the project.
   {:method :get :path "/v1/harness/project" :response #(project-table %)}
   ;; The files an @-mention in the compose box can name (?q=, ?limit=).
   {:method :get :path "/v1/harness/files" :response #(files-table %)}
   ;; The approval mode for this server session: {"mode": "block"} to have
   ;; the runs ask a person, "refuse" to not, null for the project's own.
   {:method :post :path "/v1/harness/approval-mode" :response
    (fn [req] (let [m (:mode (body-json req))]
                (if-let [now (approval/set-mode! m)]
                  (json-response {:mode (name now)})
                  (json-response 400 {:error {:message (str "not a mode: " (pr-str m)
                                                            "; one of "
                                                            (str/join ", " (map name (sort approval/modes))))}}))))}
   {:method :get :path "/v1/runs" :response
    (fn [req] (json-response (api-runs/list-runs (system/conn) (long-param req "limit"))))}
   ;; `(or (:status r) 200)`, the same shape resume uses: a handler that refuses
   ;; says so with a status, and success carries none. Answering 200 with an
   ;; error body let a caller checking only the code read a refusal as success.
   {:method :post :path "/v1/runs" :response
    (fn [req] (let [r (control/start-run! (ctx) (body-json req))]
                (json-response (or (:status r) 200) (:body r))))}
   {:method :get :path "/v1/runs/:id" :response
    (fn [req] (if-let [r (api-runs/get-run (system/conn) (get-in req [:params :id])
                                           (system/config))]
                (json-response r)
                (json-response 404 {:error {:message "no such run"}})))}
   {:method :get :path "/v1/runs/:id/journal" :response
    (fn [req] (json-response (api-runs/journal-tail (system/conn)
                                                    (get-in req [:params :id])
                                                    (long-param req "since")
                                                    (long-param req "limit"))))}
   ;; The run PUSHED: every journal event after the cursor (Last-Event-ID, or
   ;; ?since=), then each one as it lands, plus the steps and approvals that
   ;; are never journalled. See samizdat.api.stream.
   {:method :get :path "/v1/runs/:id/events" :response
    (fn [req] (stream/response (system/conn) (get-in req [:params :id])
                               (stream/cursor req)))}
   {:method :get :path "/v1/events" :response
    (fn [req] (stream/response (system/conn) nil (stream/cursor req)))}
   ;; The live manifest-state trace. No conn: steps are held in memory, not
   ;; journalled — see samizdat.steps.
   {:method :get :path "/v1/runs/:id/steps" :response
    (fn [req] (json-response (api-runs/steps-tail (get-in req [:params :id])
                                                  (long-param req "since")
                                                  (long-param req "limit"))))}
   ;; One turn, whole. The branch listing drops the model's prose because it
   ;; is the bulk; this is how a reader gets it back, a turn at a time.
   {:method :get :path "/v1/runs/:id/branches/:branch/turns/:turn" :response
    (fn [req] (let [{:keys [id branch turn]} (:params req)]
                (if-let [t (api-runs/turn-detail (system/conn) id branch
                                                 (parse-long (str turn)))]
                  (json-response t)
                  (json-response 404 {:error {:message "no such turn"}}))))}
   {:method :get :path "/v1/runs/:id/branches/:branch" :response
    (fn [req] (let [{:keys [id branch]} (:params req)]
                (if-let [b (api-runs/branch-detail (system/conn) id branch
                                                   (some-> (query-param req "notes")
                                                           (str/split #",")
                                                           (->> (remove str/blank?)))
                                                   (long-param req "since"))]
                  (json-response b)
                  (json-response 404 {:error {:message "no such branch"}}))))}
   {:method :post :path "/v1/runs/:id/interventions" :response
    (fn [req] (let [r (control/intervene! (system/conn) (system/config)
                                          (get-in req [:params :id])
                                          (body-json req))]
                (json-response (or (:status r) 200) (:body r))))}
   {:method :post :path "/v1/runs/:id/abort" :response
    (fn [req] (let [r (control/abort! (system/conn)
                                      (get-in req [:params :id]))]
                (json-response (or (:status r) 200) (:body r))))}
   ;; Stop what the run is doing and leave it resumable (Esc in the TUI).
   {:method :post :path "/v1/runs/:id/interrupt" :response
    (fn [req] (let [r (control/interrupt! (system/conn) (get-in req [:params :id]))]
                (json-response (or (:status r) 200) (:body r))))}
   {:method :post :path "/v1/runs/:id/resume" :response
    (fn [req] (let [r (control/resume! {:conn (system/conn)
                                        :config (system/config)}
                                       (get-in req [:params :id])
                                       (body-json req))]
                (json-response (or (:status r) 200) (:body r))))}
   ;; Questions waiting on a person: the permission gate and ask_human, which
   ;; share one queue because they differ only in what they carry.
   {:method :get :path "/v1/runs/:id/approvals" :response
    (fn [req] (json-response {:approvals (approval/pending
                                          (get-in req [:params :id]))}))}
   {:method :get :path "/v1/approvals" :response
    (fn [_] (json-response {:approvals (approval/pending nil)}))}
   {:method :post :path "/v1/approvals/:aid" :response
    (fn [req]
      (let [{:keys [decision note answers always]} (body-json req)
            d (keyword (or decision "deny"))]
        (if (approval/decide! (get-in req [:params :aid])
                              (cond-> {:decision d}
                                note (assoc :note note)
                                answers (assoc :answers answers)
                                ;; allow for the rest of this session, not once
                                (true? always) (assoc :always true)))
          (json-response {:status "decided" :decision (name d)})
          ;; Gone rather than never-existed: the ordinary cause is a second
          ;; operator answering a question the first already settled, or a
          ;; wait that expired. 409 says which, in the house style the other
          ;; handlers use — a short noun phrase, not a sentence.
          (json-response 409 {:error {:message "approval not open"}}))))}
   {:method :get :path "/v1/interventions/kinds" :response (fn [_] (json-response (control/kinds)))}
   ;; Anything unmatched, a known path under the wrong method included.
   {:path :not-found :response #(not-found %)}])

(defn- loopback-host?
  "Whether `host` — a Host header, or an Origin's authority — names this
  machine's loopback, with or without a port."
  [host]
  (let [h (str/lower-case (str host))
        h (cond (str/starts-with? h "[") (subs h 0 (inc (or (str/index-of h "]") (dec (count h)))))
                (= 1 (count (filter #{\:} h))) (first (str/split h #":"))
                :else h)]
    (contains? #{"localhost" "127.0.0.1" "[::1]" "::1"} h)))

(defn- header [req k] (get (:headers req) k))

(defn refusal
  "Why `req` may not be served, as a response, or nil when it may.

  The server binds loopback, which leaves the operator's own BROWSER as a
  way in: any page they have open can send a
  text/plain POST to 127.0.0.1 without a preflight, and a page on a name that
  rebinds to 127.0.0.1 reads the answers as same-origin. body-json parsed
  whatever arrived, so `POST /v1/runs` from a web page started a run on the
  page's prompt (karamazov-3vu1.1). Three checks close it without a secret:
  the Host must be loopback, an Origin (every browser sends one cross-origin)
  must be loopback, and a request with a body must declare it JSON — the one
  type a page cannot send without a preflight nothing here answers.

  And while this process's server holds a token (samizdat.security.token),
  every request but /health carries it. On Linux a process the agent runs
  can reach loopback, and the token is in a file it cannot read
  (karamazov-3vu1.11). A request with no headers is not exempt: HTTP/1.0
  lets a raw socket send exactly that."
  [req]
  (let [host (header req "host")
        origin (header req "origin")
        ct (some-> (header req "content-type") str/lower-case)
        body? (let [b (:body req)] (and b (not (and (string? b) (str/blank? b)))))]
    (cond
      (and host (not (loopback-host? host)))
      (json-response 403 {:error {:message (str "host not served: " host)}})

      (and origin (not (loopback-host? (str/replace origin #"^[a-z]+://" ""))))
      (json-response 403 {:error {:message (str "cross-origin request refused: " origin)}})

      (and body? (seq (:headers req))
           (not (#{:get :head} (:request-method req)))
           (not (some-> ct (str/starts-with? "application/json"))))
      (json-response 415 {:error {:message "body must be JSON"}})

      (and (token/current) (not= "/health" (:uri req))
           (not (token/valid? (header req "authorization"))))
      (json-response 401 {:error {:message "bearer token required"}}))))

(defn handler [req]
  (try
    ;; The table by name on every request, so a redefined one is what routes.
    ;; ruuter compiles a table once and caches it by value, which is why the
    ;; table is a def and not built here.
    (or (refusal req)
        (ruuter/route routes req))
    (catch Throwable e
      (json-response 500 {:error {:message (ex-message e) :type "internal_error"}}))))
