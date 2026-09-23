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

(ns samizdat.event-stream-test
  "A run's events pushed to a front end as server-sent events
  (karamazov-tq7m.1): the wire parser, the server's pump, and the two ends
  talking over a real socket."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [samizdat.api.sse :as sse]
            [samizdat.api.stream :as stream]
            [samizdat.approval :as approval]
            [samizdat.events :as events]
            [samizdat.net :as net]
            [samizdat.store.db :as db]
            [samizdat.store.journal :as journal]
            [samizdat.store.runs :as runs]))

(defn- bytes-of [s] (.getBytes (str s) "UTF-8"))

(defn- feed-all
  "Feed `pieces` (strings) one at a time; every event that came out."
  [pieces]
  (loop [st (sse/reader), ps pieces, out []]
    (if-let [p (first ps)]
      (let [{:keys [state events]} (sse/feed st (bytes-of p))]
        (recur state (rest ps) (into out events)))
      {:state st :events out})))

(def ^:private head
  "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\nTransfer-Encoding: chunked\r\n\r\n")

(defn- chunk [s]
  (let [b (bytes-of s)]
    (str (Integer/toHexString (alength b)) "\r\n" s "\r\n")))

;; --- the wire ----------------------------------------------------------------

(deftest events-come-out-whole-however-the-bytes-are-split
  (let [wire (str head
                  (chunk "id: 5\r\nevent: turn\r\ndata: {\"a\":1}\r\n\r\n")
                  (chunk ": ping\r\n\r\n")
                  (chunk "event: step\r\ndata: one\r\ndata: two\r\n\r\n"))
        whole (:events (feed-all [wire]))]
    (is (= [{:id "5" :event "turn" :data "{\"a\":1}"}
            {:event "step" :data "one\ntwo"}]
           whole)
        "a comment is not an event; data lines join with newlines")
    (testing "split at every byte boundary, the same events"
      (is (= whole (:events (feed-all (map str wire))))))))

(deftest a-multibyte-character-split-across-chunks-decodes-once
  ;; Chunk sizes are octets. An em-dash cut across two chunks must reach the
  ;; event whole, never as replacement characters.
  (let [line "data: a — b\r\n\r\n"
        b (bytes-of line)
        cut 9                           ; inside the 3-byte em-dash
        c1 (java.util.Arrays/copyOfRange b 0 cut)
        c2 (java.util.Arrays/copyOfRange b cut (alength b))
        st0 (:state (sse/feed (sse/reader) (bytes-of head)))
        r1 (sse/feed st0 (byte-array (concat (bytes-of (str (Integer/toHexString (alength c1)) "\r\n"))
                                             c1 (bytes-of "\r\n"))))
        r2 (sse/feed (:state r1) (byte-array (concat (bytes-of (str (Integer/toHexString (alength c2)) "\r\n"))
                                                     c2 (bytes-of "\r\n"))))]
    (is (= [{:data "a — b"}] (into (:events r1) (:events r2))))))

(deftest a-refused-stream-says-so
  (let [{:keys [state]} (feed-all ["HTTP/1.1 404 Not Found\r\nContent-Length: 0\r\n\r\n"])]
    (is (= 404 (:status state)))
    (is (:done? state))))

;; --- the server's pump -------------------------------------------------------

(defn- pumping
  "Run the pump for `run-id` from `since` on a thread, collecting what it
  emits. Returns [emitted stop!]."
  [conn run-id since]
  (let [out (atom [])
        stop (atom false)
        f (future (stream/pump! {:conn conn :run-id run-id :since since
                                 :emit! (fn [e] (swap! out conj e) true)
                                 :stop? #(deref stop)
                                 :poll-ms 10 :heartbeat-ms 60000 :page 2}))]
    [out (fn [] (reset! stop true) (deref f 2000 :timeout))]))

(defn- wait-for [pred]
  (loop [n 0]
    (cond (pred) true
          (> n 200) false
          :else (do (Thread/sleep 10) (recur (inc n))))))

(defn- kinds [out] (mapv :event (remove :comment @out)))

(deftest the-pump-replays-then-follows-in-order
  (let [conn (db/open! ":memory:")
        rid (runs/start-run! conn {:problem "p"})
        other (runs/start-run! conn {:problem "q"})
        _ (journal/note! conn rid :first {:data {:n 1}})
        _ (journal/note! conn rid :second {:data {:n 2}})
        _ (journal/note! conn rid :third {:data {:n 3}})
        since (:id (first (filter #(= "first" (:kind %)) (journal/events-since conn rid 0))))
        [out stop!] (pumping conn rid since)]
    (try
      (is (wait-for #(= 2 (count (remove :comment @out)))))
      (is (= ["second" "third"] (take-last 2 (kinds out)))
          "everything after the cursor, a page at a time, oldest first")
      (journal/note! conn other :elsewhere {})
      (journal/note! conn rid :fourth {:data {:n 4}})
      (is (wait-for #(some #{"fourth"} (kinds out))))
      (is (not (some #{"elsewhere"} (kinds out))) "another run's events are not this stream's")
      (let [e (last (remove :comment @out))]
        (is (string? (:id e)) "an id the client sends back as Last-Event-ID")
        (is (= {:n 4} (get-in (sse/parse-data (:data e)) [:data :data]))
            "the row, as GET /v1/runs/:id/journal serves it"))
      (testing "an event that is not journalled rides the bus straight through"
        (events/publish! {:kind :step :run-id rid :node :loop/assemble})
        (is (wait-for #(some #{"step"} (kinds out))))
        (is (= "loop/assemble"
               (get-in (sse/parse-data (:data (last (remove :comment @out)))) [:data :node]))
            "a namespaced keyword keeps its namespace and loses its colon"))
      (finally (stop!) (db/close conn)))))

(deftest the-pump-stops-when-the-client-is-gone
  (let [conn (db/open! ":memory:")
        rid (runs/start-run! conn {:problem "p"})
        before (count @@#'events/subscribers)
        f (future (stream/pump! {:conn conn :run-id rid :since 0
                                 :emit! (fn [_] false)
                                 :stop? (constantly false)
                                 :poll-ms 10 :heartbeat-ms 20 :page 10}))]
    (is (not= :timeout (deref f 2000 :timeout)) "a refused write ends the pump")
    (is (= before (count @@#'events/subscribers)) "and its bus subscription with it")
    (db/close conn)))

(deftest a-question-for-a-person-is-an-event
  (let [seen (events/subscribe)]
    (try
      (let [id (approval/request! {:run-id "R" :kind :shell :input "rm x"})]
        (approval/decide! id {:decision :deny})
        (is (= [["R" "pending"] ["R" "decided"]]
               (mapv (juxt :run-id (comp :status :data))
                     (filter #(= :approval (:kind %)) (events/collect seen))))))
      (finally (events/unsubscribe! seen)))))

(deftest where-a-stream-resumes
  (is (= 7 (stream/cursor {:headers {"last-event-id" "7"} :query-string "since=3"}))
      "the header a reconnect sends wins")
  (is (= 3 (stream/cursor {:query-string "a=1&since=3"})))
  (is (= :now (stream/cursor {:query-string "since=now"})))
  (is (= 0 (stream/cursor {}))))

;; --- both ends, over a socket ------------------------------------------------

(deftest a-client-follows-a-run-and-resumes-where-it-left-off
  (let [conn (db/open! ":memory:")
        rid (runs/start-run! conn {:problem "p"})
        port (with-open [s (java.net.ServerSocket. 0)] (.getLocalPort s))
        handler (fn [req] (stream/response conn rid (stream/cursor req)
                                           {:poll-ms 10 :heartbeat-ms 200 :page 50}))
        server (net/run-server handler {:port port})
        got (atom [])
        stop (atom false)
        follow (fn [last-id]
                 (future (sse/follow! (str "http://127.0.0.1:" port "/events")
                                      {:last-event-id last-id
                                       :on-event #(swap! got conj %)
                                       :stop? #(deref stop)
                                       :retry-ms 20})))]
    (try
      (journal/note! conn rid :one {})
      (let [f (follow nil)]
        (is (wait-for #(some #{"one"} (map :event @got))))
        (journal/note! conn rid :two {})
        (is (wait-for #(some #{"two"} (map :event @got))) "pushed, not polled for")
        (reset! stop true)
        (is (not= :timeout (deref f 3000 :timeout))))
      (testing "reconnecting with the last id resumes after it"
        (let [last-id (:id (last @got))]
          (reset! got [])
          (reset! stop false)
          (journal/note! conn rid :three {})
          (let [f (follow last-id)]
            (is (wait-for #(some #{"three"} (map :event @got))))
            (is (not-any? #{"one" "two"} (map :event @got)))
            (reset! stop true)
            (deref f 3000 :timeout))))
      (finally (net/stop-server server) (db/close conn)))))

(deftest the-server-routes-both-streams
  (require 'samizdat.server)
  (let [match @(resolve 'samizdat.server/match)]
    (is (= {:id "abc"} (second (match {:request-method :get :uri "/v1/runs/abc/events"}))))
    (is (some? (match {:request-method :get :uri "/v1/events"})))))

(deftest the-approval-mode-can-be-set-for-the-session
  (try
    (is (= :block (approval/set-mode! "block")))
    (is (= :block (:mode (approval/policy))))
    (is (nil? (approval/set-mode! "sometimes")) "not a mode")
    (is (= :block (:mode (approval/policy))) "and the one in force stands")
    (finally (approval/set-mode! nil)))
  (is (not= :block (:mode (approval/policy))) "cleared, the project's own is back"))
