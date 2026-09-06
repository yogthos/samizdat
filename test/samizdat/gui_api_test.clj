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

(ns samizdat.gui-api-test
  "The GUI's HTTP client and poll fold. samizdat.gui.api is deliberately
  toolkit-free (http-client + json only), which is what lets the headless
  suite cover it without ever loading GTK."
  (:require ;; the java.time.* host shim, before data.json — see samizdat.store.journal
            [jolt.time]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [jolt.http-client :as http]
            [samizdat.gui.api :as api]))

(defn- ok [body] {:status 200 :body (json/write-str body)})

(deftest client-hits-the-documented-endpoints
  (let [calls (atom [])]
    (testing "GET endpoints and shapes"
      (with-redefs [http/get (fn [url & _]
                               (swap! calls conj url)
                               (ok {:runs [{:id "r1" :status "running"}]}))]
        (let [r (api/list-runs "http://x:1")]
          (is (= "http://x:1/v1/runs" (last @calls)))
          (is (:ok r))
          (is (= "r1" (-> r :body :runs first :id)))))
      (with-redefs [http/get (fn [url & _]
                               (swap! calls conj url)
                               (ok {:run_id "r1" :events [{:id 7 :kind "turn"}]}))]
        (api/journal-since "http://x:1" "r1" 42 100)
        (is (= "http://x:1/v1/runs/r1/journal?since=42&limit=100" (last @calls)))))
    (testing "interventions carry branch, kind, payload"
      (with-redefs [http/post (fn [url opts]
                                (swap! calls conj [url (:body opts)])
                                (ok {:id 1 :status "pending"}))]
        (api/intervene! "http://x:1" "r1" {:branch-id "B1" :kind "message"
                                           :payload "try modulus 25"})
        (let [[url body] (last @calls)]
          (is (= "http://x:1/v1/runs/r1/interventions" url))
          (is (str/includes? body "B1"))
          (is (str/includes? body "try modulus 25")))))
    (testing "resume includes max_turns only when extending"
      (with-redefs [http/post (fn [url opts]
                                (swap! calls conj [url (:body opts)])
                                (ok {:status "resuming"}))]
        (api/resume! "http://x:1" "r1")
        (is (not (str/includes? (second (last @calls)) "max_turns")))
        (api/resume! "http://x:1" "r1" 400)
        (is (str/includes? (second (last @calls)) "400"))))
    (testing "starting a run POSTs the body and hands back the new id"
      (with-redefs [http/post (fn [url opts]
                                (swap! calls conj [url (:body opts)])
                                (ok {:run_id "r9" :status "running"}))]
        (let [r (api/start-run! "http://x:1" {:problem "does an odd covering exist?"
                                              :max_turns 300})
              [url body] (last @calls)]
          (is (= "http://x:1/v1/runs" url))
          (is (str/includes? body "odd covering"))
          (is (str/includes? body "300"))
          (is (= "r9" (-> r :body :run_id))
              "the caller needs the id to attach the poller"))))
    (testing "starting a run waits longer than the server's own start window"
      ;; POST /v1/runs does not answer until the beam has opened every branch:
      ;; api.control/start-run! blocks on (deref promised 30000), and beam/run!
      ;; only calls on-start after (mapv open-branch! (range width)). So the
      ;; server may legitimately take up to 30s, and a client that gives up
      ;; sooner reports a failure for a run that is actually starting — the
      ;; row is already written, so the user is told it failed AND left with a
      ;; live run burning provider spend. Observed at the default 10s.
      (with-redefs [http/post (fn [url opts]
                                (swap! calls conj [url opts])
                                (ok {:run_id "r9" :status "running"}))]
        (api/start-run! "http://x:1" {:problem "p"})
        (let [[_ opts] (last @calls)]
          (is (> (:socket-timeout opts) 30000)
              "must outlast the 30s the server is allowed to take"))))
    (testing "a run the server refused to start is an error, not a run"
      ;; start-run! answers 200 with an {:error ...} body when the beam does
      ;; not come up inside 30s, so :ok alone does not mean a run exists.
      (with-redefs [http/post (fn [_ _] (ok {:error "the run did not start within 30s"}))]
        (let [r (api/start-run! "http://x:1" {:problem "p"})]
          (is (false? (:ok r)))
          (is (str/includes? (:error r) "did not start")))))
    (testing "a dead server is a value, not a throw"
      (with-redefs [http/get (fn [& _] (throw (ex-info "connection refused" {})))]
        (let [r (api/list-runs "http://x:1")]
          (is (false? (:ok r)))
          (is (str/includes? (:error r) "refused")))))))

(deftest poll-fold-advances-cursor-and-backs-off
  (testing "events advance the cursor and reset the interval"
    (let [s (api/poll-step {:cursor 10 :interval-ms 24000}
                           {:ok true :body {:events [{:id 11} {:id 12}]}})]
      (is (= 12 (:cursor s)))
      (is (= api/base-interval-ms (:interval-ms s)))
      (is (true? (:connected? s)))
      (is (= [{:id 11} {:id 12}] (:events s)))))
  (testing "an empty batch keeps the cursor"
    (is (= 10 (:cursor (api/poll-step {:cursor 10 :interval-ms 1500}
                                      {:ok true :body {:events []}})))))
  (testing "failure doubles the interval up to the cap and marks disconnected"
    (let [s1 (api/poll-step {:cursor 10 :interval-ms api/base-interval-ms}
                            {:ok false :error "refused"})
          s2 (api/poll-step s1 {:ok false :error "refused"})]
      (is (false? (:connected? s1)))
      (is (= 10 (:cursor s2)) "the cursor survives an outage")
      (is (> (:interval-ms s2) (:interval-ms s1)))
      (is (<= (:interval-ms (nth (iterate #(api/poll-step % {:ok false}) s2) 10))
              api/max-backoff-ms)))))

(deftest a-batch-that-lands-after-stop-is-not-delivered
  ;; provenance CR1-5: an in-flight fetch completing after stop! still
  ;; delivered its callbacks, folding the OLD run's events into whatever the
  ;; UI was now showing. The loop rechecks @running between fetch and delivery.
  (let [delivered (atom 0)
        gate (promise)]
    (with-redefs [api/journal-since (fn [& _] @gate)]
      (let [{:keys [stop!]} (api/start-poller! {:base "x" :run-id "r"
                                                :on-events (fn [_] (swap! delivered inc))
                                                :on-status (fn [_])})]
        (stop!)
        (deliver gate {:ok true :body {:events [{:id 1}]}})
        (Thread/sleep 200)
        (is (zero? @delivered) "a batch that landed after stop! is dropped")))))

(deftest a-throwing-callback-does-not-kill-the-poller
  ;; Same review: one throw from a callback killed the future silently and the
  ;; header kept saying "tailing" over a frozen graph. Callbacks are guarded.
  (let [statuses (atom 0)]
    (with-redefs [api/journal-since (fn [& _] {:ok true :body {:events [{:id 1}]}})]
      (let [{:keys [stop!]} (api/start-poller! {:base "x" :run-id "r"
                                                :on-events (fn [_] (throw (ex-info "boom" {})))
                                                :on-status (fn [_] (swap! statuses inc))})]
        (Thread/sleep 300)
        (stop!)
        (is (pos? @statuses) "on-status still ran after on-events threw")))))
