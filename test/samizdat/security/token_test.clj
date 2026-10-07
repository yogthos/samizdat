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

(ns samizdat.security.token-test
  "The HTTP API's bearer token and the nREPL's gate (karamazov-3vu1.11): on a
  host whose sandbox cannot hide loopback, a process the agent runs can reach
  both, so the API asks for a token kept where that process cannot read, and
  the nREPL — which asks for nothing — does not start unless the operator
  says so."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [jolt.fs :as fs]
            [samizdat.api.client :as client]
            [samizdat.api.sse :as sse]
            [samizdat.core :as core]
            [samizdat.security.confine :as confine]
            [samizdat.security.token :as token]
            [samizdat.server :as server]
            [samizdat.system :as system]))

(defmacro with-token-dir [[d] & body]
  `(let [~d (str (fs/create-temp-dir))]
     (with-redefs [token/dir (constantly ~d)]
       (try ~@body (finally (token/revoke-all!))))))

(deftest a-token-is-issued-to-a-file-only-the-owner-reads
  (with-token-dir [d]
    (let [t (token/issue! 4111)]
      (is (re-matches #"[0-9a-f]{64}" t))
      (is (= t (token/read-for 4111)))
      (is (str/starts-with? (token/path-for 4111) d))
      (is (= #{"OWNER_READ" "OWNER_WRITE"}
             (set (map str (fs/posix-file-permissions (token/path-for 4111))))))
      (is (= t (token/current))))
    (testing "and revoked with its server"
      (token/revoke! 4111)
      (is (nil? (token/read-for 4111)))
      (is (not (fs/exists? (token/path-for 4111))))
      (is (nil? (token/current))))))

(deftest the-token-lives-where-the-sandbox-cannot-read
  (with-redefs [token/home (constantly "/home/dev")]
    (is (some #(str/starts-with? (#'token/default-dir) (str % "/"))
              (confine/secret-regions "/home/dev" "/work/samizdat")))))

(defn- call [uri headers]
  (server/handler {:request-method :get :uri uri
                   :headers (merge {"host" "127.0.0.1:3985"} headers)}))

(deftest the-api-asks-for-the-token
  (with-token-dir [_]
    (with-redefs [system/conn (constantly nil) system/config (constantly {})]
      (let [t (token/issue! 3985)]
        (is (= 401 (:status (call "/v1/interventions/kinds" {}))))
        (is (= 401 (:status (call "/v1/interventions/kinds" {"authorization" "Bearer nope"}))))
        (is (= 200 (:status (call "/v1/interventions/kinds" {"authorization" (str "Bearer " t)}))))
        (testing "health says whether the server is up, and nothing it guards"
          (is (not= 401 (:status (call "/health" {})))))))))

(deftest with-no-token-issued-nothing-is-asked
  ;; A handler under test, with no server: there is nothing to reach.
  (with-redefs [system/conn (constantly nil) system/config (constantly {})]
    (token/revoke-all!)
    (is (= 200 (:status (call "/v1/interventions/kinds" {}))))))

(deftest the-client-sends-the-token-for-the-port-it-calls
  (with-token-dir [_]
    (let [t (token/issue! 4222)]
      (is (= {"Authorization" (str "Bearer " t)}
             (client/auth-headers "http://127.0.0.1:4222")))
      (is (= {} (client/auth-headers "http://127.0.0.1:4333")) "no token for that port")
      (is (= {} (client/auth-headers "https://example.com:4222")) "and none to a host that is not this one"))))

(deftest the-nrepl-starts-only-where-loopback-is-hidden
  (is (core/nrepl-allowed? {:os "Mac OS X" :backend :seatbelt :nrepl {}}))
  (is (not (core/nrepl-allowed? {:os "Linux" :backend :bwrap :nrepl {}})))
  (is (not (core/nrepl-allowed? {:os "Linux" :backend :none :nrepl {}})))
  (is (core/nrepl-allowed? {:os "Linux" :backend :bwrap :nrepl {:unconfined :allow}})
      "unless the operator says so")
  (is (not (core/nrepl-allowed? {:os "Linux" :backend :bwrap :nrepl {:unconfined "allow"}}))
      "in exactly those words"))

(deftest the-event-stream-sends-it-too
  (with-token-dir [_]
    (let [t (token/issue! 4555)]
      (is (str/includes? (#'sse/request-text {:host "127.0.0.1" :port 4555 :path "/v1/x"} nil)
                         (str "Authorization: Bearer " t "\r\n")))
      (is (not (str/includes? (#'sse/request-text {:host "127.0.0.1" :port 4556 :path "/v1/x"} nil)
                              "Authorization"))))))
