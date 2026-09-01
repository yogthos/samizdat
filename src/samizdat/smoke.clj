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

(ns samizdat.smoke
  "Phase 0 platform probes. Every check here corresponds to a stated risk in
  PLAN.md, and the point is to find out now rather than in the phase that
  depends on it.

      jolt -M:smoke

  Exits non-zero if any required probe fails. Lean is optional and reports
  as skipped when the toolchain is absent, since only Phase 5 needs it."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            ;; db.jdbc registers the java.sql shim clojure.jdbc compiles against and
            ;; points connection construction at the native driver; it has to load
            ;; before jdbc.core.
            [db.jdbc]
            [jdbc.core :as jdbc]
            [jolt.http-client :as http]
            [jolt.process :as p]
            [samizdat.config :as config]
            [samizdat.llm.client :as llm]
            [samizdat.llm.registry :as registry]
            [samizdat.store.db :as db]
            [samizdat.system :as system]))

(def ^:private results (atom []))

(defn- record! [name status detail]
  (swap! results conj {:name name :status status :detail detail})
  (println (format "  %-7s %-34s %s"
                   (case status :pass "pass" :fail "FAIL" :skip "skip")
                   name
                   (or detail ""))))

(defmacro probe
  "Run `body`; it should return [status detail] or throw."
  [name & body]
  `(try
     (let [[st# detail#] (do ~@body)]
       (record! ~name st# detail#))
     (catch Throwable e#
       (record! ~name :fail (str (ex-message e#))))))

;; --- storage ----------------------------------------------------------------

(defn- sqlite-check []
  (let [c (db/open! ":memory:")]
    (try
      (let [v (db/schema-version c)
            tables (db/table-names c)]
        (if (and (pos? v) (every? (set tables) ["runs" "branches" "turns" "artifacts"
                                                "failures" "gate_firings"
                                                "interventions" "events"]))
          [:pass (str "user_version " v ", " (count tables) " tables")]
          [:fail (str "user_version " v ", tables " (pr-str tables))]))
      (finally (db/close c)))))

(defn- fts5-check []
  (let [c (db/connect ":memory:")]
    (try
      (if (db/fts5-available? c)
        (do (jdbc/execute! c "CREATE VIRTUAL TABLE t USING fts5(claim, reason)")
            (jdbc/execute! c ["INSERT INTO t VALUES (?, ?)"
                              "sidon set of size 24" "z3 returned unsat"])
            (let [rows (jdbc/fetch c ["SELECT claim FROM t WHERE t MATCH ?" "sidon"])]
              (if (= 1 (count rows))
                [:pass "match returned the row"]
                [:fail (str "match returned " (count rows) " rows")])))
        [:fail "libsqlite3 loaded by the FFI binding has no FTS5"])
      (finally (db/close c)))))

(defn- sqlite-concurrency [n]
  ;; Not a claim that concurrent writers are safe — a claim that the single
  ;; writer this design uses survives concurrent callers.
  (let [c (db/connect ":memory:")]
    (try
      (jdbc/execute! c "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT)")
      (let [lock (Object.)
            fs (mapv (fn [i]
                       (future
                         (dotimes [j 20]
                           (locking lock
                             (jdbc/execute! c ["INSERT INTO t (v) VALUES (?)"
                                               (str i "-" j)])))))
                     (range n))]
        (run! deref fs)
        (let [n* (-> (jdbc/fetch-one c "SELECT count(*) AS n FROM t") :n)]
          (if (= n* (* n 20))
            [:pass (str n* " rows from " n " writers")]
            [:fail (str "expected " (* n 20) " rows, got " n*)])))
      (finally (db/close c)))))

;; --- network ----------------------------------------------------------------

(defn- provider-check [{:keys [provider api-key model] :as llm-cfg}]
  ;; Through the adapter, not base-url + "/models": the models endpoint is
  ;; provider-specific (DeepSeek serves chat under /beta but models at the
  ;; root), and the adapter is the one place that knows. Naive concatenation
  ;; had this probe failing 404 against a perfectly healthy provider.
  (if (str/blank? api-key)
    [:skip "no API key in the environment"]
    (let [models (llm/list-models (registry/adapter-for provider) llm-cfg)]
      (cond
        (empty? models)        [:fail "provider listed no models"]
        (some #{model} models) [:pass (str model " listed")]
        :else                  [:fail (str model " NOT listed; available: "
                                          (str/join ", " (take 5 models)))]))))

(defn- long-request-check [{:keys [base-url api-key model timeout-ms]}]
  ;; A five-minute TLS read is the shape of every real provider call here, and
  ;; clj-http-lite over jolt.ffi sockets has not been exercised at that
  ;; duration. One real completion is the cheapest honest probe.
  (if (str/blank? api-key)
    [:skip "no API key in the environment"]
    (let [start (System/currentTimeMillis)
          resp (http/post (str base-url "/chat/completions")
                          {:headers {"Authorization" (str "Bearer " api-key)}
                           :content-type :json
                           :socket-timeout timeout-ms
                           :body (json/write-str
                                  {:model model
                                   :max_tokens 1200
                                   :messages [{:role "user"
                                               :content "Count from 1 to 300, one number per line, nothing else."}]})})
          ms (- (System/currentTimeMillis) start)]
      (if (= 200 (:status resp))
        [:pass (str ms "ms, " (count (:body resp)) " bytes")]
        [:fail (str "status " (:status resp))]))))

(defn- nrepl-load-order-check []
  ;; Regression guard for the load-order bug in samizdat.core's ns form.
  ;; Requiring jolt.nrepl before jolt.http-client leaves the process unable to
  ;; complete a TLS handshake at all. It has to run in a subprocess, because
  ;; by the time this namespace is evaluated both are already loaded here in
  ;; whatever order the smoke run used.
  (let [code (str "(require 'samizdat.core 'samizdat.system)"
                  "(require (quote samizdat.server))(samizdat.system/start! (var samizdat.server/handler) {:http {:port 3993} :db {:path \":memory:\"}})"
                  "(samizdat.core/warm-tls! (samizdat.system/config))"
                  "(require 'jolt.nrepl)"
                  "(require '[jolt.http-client :as h])"
                  "(println :probe (try (:status (h/get \"https://example.com\"))"
                  "                     (catch Throwable e :tls-broken)))"
                  "(samizdat.system/stop!)")
        {:keys [out timeout]} (p/sh {:timeout-ms 180000} "jolt" "-e" code)]
    (cond
      timeout [:fail "the subprocess did not finish"]
      (str/includes? (str out) ":probe 200")
      [:pass "https survives the nREPL load in the real startup order"]
      (str/includes? (str out) ":tls-broken")
      [:fail (str "TLS is broken once jolt.nrepl loads. The warm-up in"
                  " samizdat.core/warm-tls! must complete a real https handshake"
                  " BEFORE nREPL is required.")]
      :else [:fail (str "unexpected output: " (str/trim (str out)))])))

;; --- server -----------------------------------------------------------------

(defn- server-concurrency-check []
  ;; The vendored adapter change: upstream serves one connection at a time on
  ;; the accept thread, so a multi-minute beam would block /health. /slow
  ;; sleeps three seconds; /health must answer well inside that.
  (let [port 3987]
    (system/start! (requiring-resolve 'samizdat.server/handler)
                   {:http {:port port} :db {:path ":memory:"}})
    (try
      (let [base (str "http://127.0.0.1:" port)
            slow (future (http/get (str base "/slow?ms=3000") {:socket-timeout 10000}))
            _ (Thread/sleep 300)
            start (System/currentTimeMillis)
            health (http/get (str base "/health") {:socket-timeout 10000})
            ms (- (System/currentTimeMillis) start)]
        @slow
        (cond
          (not= 200 (:status health)) [:fail (str "/health returned " (:status health))]
          (> ms 2000) [:fail (str "/health waited " ms "ms behind /slow — server is serialized")]
          :else [:pass (str "/health answered in " ms "ms while /slow was running")]))
      (finally (system/stop!)))))

;; --- driver -----------------------------------------------------------------

(defn run []
  (reset! results [])
  (let [cfg (config/load-config)
        engines (:engines cfg)]
    (println "samizdat phase 0 probes\n")

    (println "\nstorage")
    (probe "sqlite migrate" (sqlite-check))
    (probe "sqlite fts5" (fts5-check))
    (probe "sqlite 5 writers" (sqlite-concurrency 5))

    (println "\nnetwork")
    (probe "provider reachable" (provider-check (:llm cfg)))
    (probe "long completion" (long-request-check (:llm cfg)))
    (probe "https after nrepl load" (nrepl-load-order-check))

    (println "\nserver")
    (probe "concurrent requests" (server-concurrency-check))

    (let [failed (filter #(= :fail (:status %)) @results)
          skipped (filter #(= :skip (:status %)) @results)]
      (println)
      (println (format "%d passed, %d failed, %d skipped"
                       (count (filter #(= :pass (:status %)) @results))
                       (count failed)
                       (count skipped)))
      (empty? failed))))

(defn -main [& _]
  (if (run)
    (System/exit 0)
    (System/exit 1)))
