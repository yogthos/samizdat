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

  Routes are matched against a vector of [method path-or-pattern handler]
  rather than through a router library. There are a dozen of them, and a
  dependency that needs its :clj reader branches switched on costs more to load
  than it saves.

  This namespace is pure logic: redefining `handler` against a running process
  takes effect on the next request. See samizdat.system."
  (:require ;; the java.time.* host shim, before data.json — see samizdat.store.journal
            [jolt.time]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.tools.logging :as log]
            [samizdat.agent.gates :as gates]
            [samizdat.api.control :as control]
            [samizdat.api.openai :as openai]
            [samizdat.api.runs :as api-runs]
            [samizdat.approval :as approval]
            [samizdat.config :as config]
            [samizdat.llm.client :as llm-client]
            [samizdat.store.db :as db]
            [samizdat.system :as system]
            [samizdat.userspace :as userspace]))

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

(defn- health [_req]
  (let [cfg (system/config)]
    (json-response
     {:status "ok"
      :schema_version (db/schema-version (system/conn))
      :active_runs (count @control/active)
      ;; DEFAULTS, said plainly. :run/share-artifacts? in particular is not
      ;; what a given run is doing — beam/run! forces it on for any seeded run
      ;; — and reporting it flat once said sharing was off during a run that
      ;; had already served 91 shared artifacts. The per-run truth is on the
      ;; run detail endpoint as share_artifacts.
      :config_defaults (config/redacted (select-keys cfg [:llm :run :db]))
      ;; Which files those defaults were layered from, lowest first.
      :config_sources (config/config-sources (get-in cfg [:run :root]))
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
  "The project's terminal-UI layout, as the EDN text of its `tui` policy.

  Served rather than read by the front end because only this process is
  BOUND to the project: `userspace/body` reads the stored row here and falls
  back to the shipped template, where the same call in the TUI's process —
  which holds no database handle by design — can only ever see the template.
  Without this the agent could save a new version of its own UI and nothing
  would ever draw it.

  Text, not parsed: the server has no business understanding a layout, and a
  client that is going to `edn/read-string` it anyway gains nothing from a
  round trip through JSON."
  []
  {:layout (userspace/body :policy "tui")})

(defn- layout-table [_req]
  (json-response (layout-body)))

;; --- routing ----------------------------------------------------------------
;;
;; A route is [method pattern handler]. A pattern segment starting with ':'
;; binds; the bindings arrive under :path-params.

(def ^:private slow-ms-cap 10000)

(defn- clamp-slow-ms
  "/slow exists so the smoke probe can prove /health still answers while a
  handler is busy; its ms parameter is a dial for \"briefly busy\", not a lease
  on a connection thread, so it is clamped (provenance R3-4)."
  [ms]
  (min (max (or ms 1000) 0) slow-ms-cap))

(defn- slow
  "Sleeps, so the smoke probe can prove /health still answers while a handler is
  busy. That is the property the vendored thread-per-connection change buys and
  the reason a multi-minute beam can share a process with a UI."
  [req]
  (let [ms (clamp-slow-ms (long-param req "ms"))]
    (Thread/sleep ms)
    (json-response {:slept_ms ms})))

(def routes
  [[:get "/health" #'health]
   [:get "/slow" #'slow]
   [:get "/v1/models" #'models]
   [:post "/v1/chat/completions" #'chat-completions]
   [:get "/v1/harness/gates" #'gate-table]
   ;; The terminal UI's own arrangement, so a front end that holds no
   ;; database handle can still see the version the agent saved.
   [:get "/v1/harness/layout" #'layout-table]
   [:get "/v1/harness/models" #'harness-models]
   [:get "/v1/runs" (fn [req] (json-response (api-runs/list-runs (system/conn)
                                                                 (long-param req "limit"))))]
   ;; `(or (:status r) 200)`, the same shape resume uses: a handler that refuses
   ;; says so with a status, and success carries none. Answering 200 with an
   ;; error body let a caller checking only the code read a refusal as success.
   [:post "/v1/runs" (fn [req] (let [r (control/start-run! (ctx) (body-json req))]
                                 (json-response (or (:status r) 200) (:body r))))]
   [:get "/v1/runs/:id" (fn [req]
                          (if-let [r (api-runs/get-run (system/conn)
                                                       (get-in req [:path-params :id]))]
                            (json-response r)
                            (json-response 404 {:error {:message "no such run"}})))]
   [:get "/v1/runs/:id/journal"
    (fn [req] (json-response (api-runs/journal-tail (system/conn)
                                                    (get-in req [:path-params :id])
                                                    (long-param req "since")
                                                    (long-param req "limit"))))]
   ;; The live manifest-state trace. No conn: steps are held in memory, not
   ;; journalled — see samizdat.steps.
   [:get "/v1/runs/:id/steps"
    (fn [req] (json-response (api-runs/steps-tail (get-in req [:path-params :id])
                                                  (long-param req "since")
                                                  (long-param req "limit"))))]
   ;; One turn, whole. The branch listing drops the model's prose because it
   ;; is the bulk; this is how a reader gets it back, a turn at a time.
   [:get "/v1/runs/:id/branches/:branch/turns/:turn"
    (fn [req] (let [{:keys [id branch turn]} (:path-params req)]
                (if-let [t (api-runs/turn-detail (system/conn) id branch
                                                 (parse-long (str turn)))]
                  (json-response t)
                  (json-response 404 {:error {:message "no such turn"}}))))]
   [:get "/v1/runs/:id/branches/:branch"
    (fn [req] (let [{:keys [id branch]} (:path-params req)]
                (if-let [b (api-runs/branch-detail (system/conn) id branch)]
                  (json-response b)
                  (json-response 404 {:error {:message "no such branch"}}))))]
    [:post "/v1/runs/:id/interventions"
     (fn [req] (let [r (control/intervene! (system/conn)
                                           (get-in req [:path-params :id])
                                           (body-json req))]
                 (json-response (or (:status r) 200) (:body r))))]
   [:post "/v1/runs/:id/abort"
    (fn [req] (let [r (control/abort! (system/conn)
                                      (get-in req [:path-params :id]))]
                (json-response (or (:status r) 200) (:body r))))]
   [:post "/v1/runs/:id/resume"
    (fn [req] (let [r (control/resume! {:conn (system/conn)
                                        :config (system/config)}
                                       (get-in req [:path-params :id])
                                       (body-json req))]
                (json-response (or (:status r) 200) (:body r))))]
   ;; Questions waiting on a person: the permission gate and ask_human, which
   ;; share one queue because they differ only in what they carry.
   [:get "/v1/runs/:id/approvals"
    (fn [req] (json-response {:approvals (approval/pending
                                          (get-in req [:path-params :id]))}))]
   [:get "/v1/approvals"
    (fn [_] (json-response {:approvals (approval/pending nil)}))]
   [:post "/v1/approvals/:aid"
    (fn [req]
      (let [{:keys [decision note answers]} (body-json req)
            d (keyword (or decision "deny"))]
        (if (approval/decide! (get-in req [:path-params :aid])
                              (cond-> {:decision d}
                                note (assoc :note note)
                                answers (assoc :answers answers)))
          (json-response {:status "decided" :decision (name d)})
          ;; Gone rather than never-existed: the ordinary cause is a second
          ;; operator answering a question the first already settled, or a
          ;; wait that expired. 409 says which, in the house style the other
          ;; handlers use — a short noun phrase, not a sentence.
          (json-response 409 {:error {:message "approval not open"}}))))]
   [:get "/v1/interventions/kinds" (fn [_] (json-response (control/kinds)))]])

(defn- match-path [pattern uri]
  (let [ps (str/split (str/replace pattern #"^/" "") #"/")
        us (str/split (str/replace (or uri "") #"^/" "") #"/")]
    (when (= (count ps) (count us))
      (reduce (fn [acc [p u]]
                (cond
                  (str/starts-with? p ":") (assoc acc (keyword (subs p 1)) u)
                  (= p u) acc
                  :else (reduced nil)))
              {} (map vector ps us)))))

(defn- match [{:keys [request-method uri]}]
  (some (fn [[m pattern h]]
          (when (= m request-method)
            (when-let [params (match-path pattern uri)]
              [h params])))
        routes))

(defn handler [req]
  (try
    (if-let [[h params] (match req)]
      (h (assoc req :path-params params))
      (json-response 404 {:error {:message (str "Not found: "
                                                (str/upper-case (name (:request-method req)))
                                                " " (:uri req))
                                  :type "not_found"}}))
    (catch Throwable e
      (json-response 500 {:error {:message (ex-message e) :type "internal_error"}}))))
