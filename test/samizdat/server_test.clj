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

(ns samizdat.server-test
  "The vendored ring adapter's request reader, and the listen socket's
  close-on-exec."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [jolt.process :as p]
            [ring-chez.adapter :as adapter]
            [samizdat.api.control :as control]
            [samizdat.server :as server]
            [samizdat.store.db :as db]
            [samizdat.userspace :as userspace]))

(defn- request [body]
  (str "POST /v1/runs HTTP/1.1\r\n"
       "Content-Type: application/json\r\n"
       "Content-Length: " (alength (.getBytes body "UTF-8")) "\r\n"
       "\r\n"
       body))

(deftest slow-clamps-its-sleep
  ;; /slow exists so the smoke probe can prove /health still answers while a
  ;; handler is busy; its ms parameter is a dial for "briefly busy", not a
  ;; lease on a connection thread, so it is clamped (provenance R3-4). Resolved at
  ;; runtime so the missing var reads as a failing assertion, not a dead file.
  (let [clamp (resolve 'samizdat.server/clamp-slow-ms)]
    (is (some? clamp) "clamp-slow-ms exists")
    (when clamp
      (testing "the dial has both ends"
        (is (= 1000 (@clamp nil)) "no parameter means the default")
        (is (= 250 (@clamp 250)) "an in-range value passes through")
        (is (= 10000 (@clamp 999999999)) "the ceiling holds")
        (is (= 0 (@clamp -5)) "a negative asks to sleep nothing")))))

(deftest the-harness-serves-the-layout-the-agent-saved
  ;; The seam that makes "the agent can rearrange its own UI" true. Only the
  ;; server is BOUND to the project, so only the server can read the stored
  ;; `tui` policy; the TUI holds no database handle by design and asks. Before
  ;; this endpoint existed, a saved version was written and nothing ever drew
  ;; it — the front end's own userspace read could only reach the shipped
  ;; template, and then cached that for the life of the process.
  (let [tmp (str (java.io.File/createTempFile "layout" ".sqlite3"))
        conn (db/open! tmp)
        prev (userspace/bind! conn)]
    (try
      (testing "with nothing saved it is the shipped template, seeded"
        (let [body (:layout (server/layout-body))]
          (is (string? body))
          (is (= (:layout (edn/read-string body))
                 (:layout (edn/read-string (userspace/template :policy "tui"))))
              "which is what a fresh project draws")))
      (testing "and after the agent saves one, it is the agent's"
        (userspace/save! :policy "tui"
                         (pr-str {:prose-turns 3 :layout [:vbox [:widget/status {}]]}))
        (let [spec (edn/read-string (:layout (server/layout-body)))]
          (is (= [:vbox [:widget/status {}]] (:layout spec)))
          (is (= 3 (:prose-turns spec)))))
      (finally
        (userspace/bind! prev)
        (db/close conn)))))

(deftest content-length-is-octets-not-characters
  ;; A 3-byte em-dash decodes to one char. Judging completeness by char count
  ;; left the reader waiting for two bytes that had already arrived, so every
  ;; POST whose body carried multibyte UTF-8 hung until the client gave up.
  (testing "a multibyte body is complete when its octet count matches"
    (is (#'adapter/request-complete?
         (request "{\"note\": \"an em-dash — here\"}"))))
  (testing "an ascii body is complete"
    (is (#'adapter/request-complete? (request "{\"a\": 1}"))))
  (testing "a short body is incomplete"
    (let [r (request "{\"a\": 1}")]
      (is (not (#'adapter/request-complete? (subs r 0 (- (count r) 3)))))))
  (testing "unterminated headers are incomplete"
    (is (not (#'adapter/request-complete?
              "POST / HTTP/1.1\r\nContent-Length: 5\r\n")))))

(deftest a-subprocess-does-not-inherit-the-listen-socket
  ;; The listening socket is a raw fd from socket(2), and every process the
  ;; harness spawns — the Lean repl through `lake env`, prolog, octave — forks
  ;; from the server. Without close-on-exec each child holds a duplicate of it,
  ;; which lsof showed directly: jolt, lake and repl all on fd 4, same kernel
  ;; object, TCP 127.0.0.1:3985 (LISTEN).
  ;;
  ;; The port then stays bound for as long as ANY holder lives. Kill the server
  ;; while a Lean session lingers — which is what an abandoned run leaves behind,
  ;; since destroy-tree is a shutdown hook rather than a guarantee — and the
  ;; restart fails with address-in-use against a server that is already gone.
  ;;
  ;; Two assertions, because they fail for different reasons and CI proved it:
  ;; the flag was set on Linux and the rebind STILL failed, which is a separate
  ;; bug — close() does not wake a thread blocked in accept() there, so the
  ;; socket outlived stop-server. stop-server calls shutdown() first now.
  ;;
  ;; Rebinding is worth asserting anyway because it is the consequence that
  ;; bites. SO_REUSEADDR lets a new socket past a TIME_WAIT, but not past a
  ;; live listener, so it fails whenever anything still holds one.
  (let [port 39187
        handler (fn [_] {:status 200 :headers {} :body "ok"})
        server (adapter/run-server handler {:port port})
        ;; Spawned while the server is up, so it forks with the fd open.
        child (p/process ["sleep" "20"] {})]
    (try
      ;; Asserted separately from the consequence, because the two fail for
      ;; different reasons and the first version could not tell them apart:
      ;; it passed on macOS and failed on Linux CI with nothing to say about
      ;; whether the flag had been set at all.
      (is (adapter/cloexec? (:socket server))
          "the listen fd is not marked FD_CLOEXEC — the mechanism itself failed")
      (adapter/stop-server server)
      (let [again (try {:ok true :server (adapter/run-server handler {:port port})}
                       (catch Throwable e {:ok false :error (ex-message e)}))]
        (is (:ok again)
            (str "port " port " is still held after the server stopped — a child "
                 "inherited the listen fd: " (:error again)))
        (when-let [s (:server again)] (adapter/stop-server s)))
      (finally
        (try (p/destroy-tree child) (catch Throwable _ nil))))))

(deftest an-error-carries-a-status-code-not-just-an-error-body
  ;; The API's own convention is a real status plus {:error {:message ...}} —
  ;; that is what "no such run", "no such branch", the 404 fallback, the 500
  ;; handler and resume's 409 all do. Two endpoints deviated and answered 200
  ;; with an error body, so a caller checking the status code alone read a
  ;; refusal as a success:
  ;;
  ;;   $ curl -X POST .../v1/runs/<finished>/abort -w '%{http_code}'
  ;;   {"error":"no active run ..."}
  ;;   200
  ;;
  ;; 409 rather than 404 for abort, matching resume: the run exists, it is just
  ;; not in a state that can be aborted. 503 for a start that did not come up,
  ;; because the request was fine and the server could not service it.
  (testing "aborting a run that is not active is a 409 with the house error shape"
    (let [r (control/abort! nil "no-such-run")]
      (is (= 409 (:status r)))
      (is (string? (get-in r [:body :error :message])))
      (is (= "no-such-run" (get-in r [:body :run_id])))))
  (testing "success and refusal share one envelope, so the route needs no special case"
    ;; Both wrap in :body and only a refusal sets :status, which is what lets
    ;; every route read (json-response (or (:status r) 200) (:body r)).
    ;;
    ;; The envelope is not decoration. A success body carries :status "aborting"
    ;; — the RUN's state — so a route reading (:status r) off a bare map would
    ;; have handed the HTTP layer the string "aborting" as its status code.
    ;; Wrapping keeps the two :status meanings from ever meeting.
    (let [ok (control/abort! nil "no-such-run")]
      (is (map? (:body ok)) "a refusal has a :body")
      (is (= 409 (:status ok)) "and an HTTP status beside it"))
    (is (= "aborting" (:status {:run_id "r" :status "aborting"}))
        "whereas the run's own :status lives inside the body and stays a string")))

(deftest a-run-can-name-its-own-model-and-thinking-level
  ;; Switching arms meant restarting the server, because the model came from
  ;; HARNESS_MODEL at startup and start-run! read it off the global config. A
  ;; restart kills whatever run is in flight — hours of provider spend — so
  ;; comparing deepseek-v4-flash against deepseek-v4-pro was gated on the box
  ;; being idle. It is per-run now, and recorded on the run row, so the arm is
  ;; provenance rather than something to remember about the environment.
  (let [base {:model "deepseek-v4-flash" :provider :deepseek :max-tokens 16384}]
    (testing "nothing asked for leaves the configured arm alone"
      (is (= base (control/run-llm-config base {:problem "p"}))))

    (testing "the body's model wins"
      (is (= "deepseek-v4-pro"
             (:model (control/run-llm-config base {:model "deepseek-v4-pro"})))))

    (testing "underscored keys work too, as everywhere else on this API"
      ;; JSON bodies arrive underscored; the first call made against this API
      ;; asked for beam_width 2 and silently got the config default of 5.
      (is (= "high" (:reasoning-effort
                     (control/run-llm-config base {:reasoning_effort "high"}))))
      (is (= "deepseek-v4-pro"
             (:model (control/run-llm-config base {"model" "deepseek-v4-pro"})))))

    (testing "a blank model is not a model"
      ;; An empty select in the UI posts "", and merging that would ask the
      ;; provider to serve a model with no name.
      (is (= "deepseek-v4-flash" (:model (control/run-llm-config base {:model ""}))))
      (is (= "deepseek-v4-flash"
             (:model (control/run-llm-config base {:model "   "})))))

    (testing "everything else on the config survives"
      (let [r (control/run-llm-config base {:model "deepseek-v4-pro"
                                            :reasoning_effort "high"})]
        (is (= :deepseek (:provider r)))
        (is (= 16384 (:max-tokens r)))))))

(deftest refusals-carry-their-own-reason-phrase
  ;; provenance R3-12: status-text knew 409 and 503 not, and the status line fell
  ;; back to "OK" — a refusal that read as a success on the wire. Both are
  ;; statuses this API actually sends (abort/resume 409, start 503).
  (is (str/starts-with? (#'adapter/response->string
                         {:status 409 :headers {} :body "x"})
                        "HTTP/1.1 409 Conflict"))
  (is (str/starts-with? (#'adapter/response->string
                         {:status 503 :headers {} :body "x"})
                        "HTTP/1.1 503 Service Unavailable")))

(deftest a-failed-send-throws-rather-than-truncating
  ;; provenance R3-12: send-all stopped silently when c-send answered <= 0, so the
  ;; client read a body that ended exactly where the socket died while
  ;; Content-Length promised more — a well-formed lie. Throwing hands the
  ;; connection to serve-conn's error path instead.
  (let [fd (adapter/c-socket 2 1 0)]
    (when (neg? fd) (throw (ex-info "socket() failed in test setup" {})))
    (try
      (#'adapter/c-close fd)
      (is (thrown? Throwable (#'adapter/send-all fd "x"))
          "send on a dead fd must not return as if it wrote")
      (finally (#'adapter/c-close fd)))))

(deftest query-params-are-percent-decoded
  ;; provenance R3-12: values arrived raw from the query string, so %XX stayed %XX
  ;; and + stayed +. The API's own params are numeric, but a steer or a UI
  ;; search that ever carries text should not have to know the adapter's
  ;; omission.
  (let [q (fn [qs k] (#'server/query-param {:query-string qs} k))]
    (testing "plain values pass through"
      (is (= "250" (q "ms=250" "ms"))))
    (testing "+ is space"
      (is (= "a b" (q "ms=a+b" "ms"))))
    (testing "%XX decodes, byte-exact across a UTF-8 sequence"
      (is (= "€" (q "ms=%E2%82%AC" "ms")))
      (is (= "50%" (q "ms=50%25" "ms"))))
    (testing "a lone % the client never escaped survives"
      (is (= "50%" (q "ms=50%" "ms"))))))
