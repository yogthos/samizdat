;; samizdat - a claim-first verification harness
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

(ns samizdat.adapter-test
  "The vendored ring adapter's connection handling, driven end to end through a
  raw socket client: chunked-body refusal (provenance R3-3), the request-size cap
  and read timeout (#4), and byte-exact body decoding across packet splits
  (#5). The client carries its own 5s SO_RCVTIMEO so a broken server FAILS
  these tests instead of hanging the suite."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [jolt.ffi :as ffi]
            [ring-chez.adapter :as adapter]))

;; connect(2) is the one call the adapter never makes; its siblings
;; (socket/send/recv/close/setsockopt) are public vars on the adapter.
(ffi/defcfn t-connect "connect" [:int :pointer :int] :int)

(def ^:private af-inet 2)
(def ^:private sock-stream 1)
(def ^:private macos?
  (str/includes? (str/lower-case (or (System/getProperty "os.name") "")) "mac"))
(def ^:private sol-socket (if macos? 0xffff 1))
(def ^:private so-rcvtimeo (if macos? 0x1006 20))
(def ^:private bufsize 65536)

;; sockaddr_in for 127.0.0.1:port, laid out exactly like the adapter's own
;; make-sockaddr (macOS wants sin_len in byte 0, Linux starts at the family).
(defn- sockaddr [port]
  (let [sa (ffi/alloc 16)]
    (dotimes [i 16] (ffi/write sa :uint8 0 i))
    (if macos?
      (do (ffi/write sa :uint8 16 0) (ffi/write sa :uint8 af-inet 1))
      (ffi/write sa :uint8 af-inet 0))
    (ffi/write sa :uint8 (bit-and (bit-shift-right port 8) 0xff) 2)
    (ffi/write sa :uint8 (bit-and port 0xff) 3)
    (ffi/write sa :uint8 127 4) (ffi/write sa :uint8 0 5)
    (ffi/write sa :uint8 0 6)   (ffi/write sa :uint8 1 7)
    sa))

(defn- connect!
  "Open a client socket to the loopback adapter, guarded by a 5s receive
  timeout so a server that never answers cannot wedge the suite."
  [port]
  (let [fd (adapter/c-socket af-inet sock-stream 0)]
    (when (neg? fd) (throw (ex-info "client socket() failed" {})))
    (let [sa (sockaddr port)]
      (when (neg? (t-connect fd sa 16))
        (ffi/free sa) (adapter/c-close fd)
        (throw (ex-info "connect() failed" {})))
      (ffi/free sa))
    (let [tv (ffi/alloc 16)]
      (ffi/write tv :int64 5 0)
      (ffi/write tv :int64 0 8)
      (adapter/c-setsockopt fd sol-socket so-rcvtimeo tv 16)
      (ffi/free tv))
    fd))

(defn- load-payload
  "Encode s into a fresh pointer; [ptr octet-count]. Caller frees."
  [s]
  (let [p (ffi/alloc (max 1 (* 4 (count s))))
        n (ffi/write-bytes p s)]
    [p n]))

(defn- send-part
  "Send `len` octets of pointer `p` starting at `start` — byte-granular, so a
  test can split a payload inside a multibyte character."
  [fd p start len]
  (loop [off start, left len]
    (when (pos? left)
      (let [sent (adapter/c-send fd (+ p off) left 0)]
        (when (pos? sent) (recur (+ off sent) (- left sent)))))))

(defn- send-string [fd s]
  (let [[p n] (load-payload s)]
    (try (send-part fd p 0 n) (finally (ffi/free p)))))

(defn- read-response
  "Read until the server hangs up. {:text .. :closed? ..} — closed? is true
  only on a clean EOF (recv 0), false when our own 5s guard fired (recv -1).
  That distinction is how the stall test proves the SERVER closed the
  connection rather than the client giving up first."
  [fd]
  (let [buf (ffi/alloc bufsize)]
    (try
      (loop [acc ""]
        (let [n (adapter/c-recv fd buf bufsize 0)]
          (cond
            (zero? n) {:text acc :closed? true}
            (neg? n) {:text acc :closed? false}
            :else (recur (str acc (ffi/read-bytes buf n))))))
      (finally (ffi/free buf)))))

(defn- free-port
  "A port the OS says is free right now.

  This used to be `(+ 40000 (rand-int 20000))` — a guess, bound with no check
  that anything else held it. It was latent for as long as nothing else in the
  suite took ephemeral ports; karamazov-zrq's project images do, Linux hands
  those out of a range that overlaps 40000-59999, and CI failed with
  `bind() failed on port 52106`. Asking the OS is both narrower and correct.

  Still a race in principle — the port is free when we close it and taken by
  the time the server binds — which is why the caller retries."
  []
  (with-open [s (java.net.ServerSocket. 0)]
    (.getLocalPort s)))

(defn- with-server
  "Run (f port) against an adapter whose handler records every request it
  sees into `captured`.

  Retries the bind, because a free port can be taken between asking and using
  it and a test that fails on that is testing the scheduler, not the adapter."
  [opts captured f]
  (let [handler (fn [req]
                  (swap! captured conj req)
                  {:status 200 :headers {"Content-Type" "text/plain"} :body "ok"})
        [port server] (loop [attempt 0]
                        (let [p (free-port)
                              srv (try (adapter/run-server handler (assoc opts :port p))
                                       (catch Exception e
                                         (when (>= attempt 4) (throw e))
                                         nil))]
                          (if srv [p srv] (recur (inc attempt)))))]
    (try (f port) (finally (adapter/stop-server server)))))

(defn- lengthed-post [body]
  (str "POST /v1/runs HTTP/1.1\r\nContent-Type: application/json\r\n"
       "Content-Length: " (alength (.getBytes body "UTF-8")) "\r\n\r\n"
       body))

(deftest chunked-request-bodies-are-refused-not-truncated
  ;; provenance R3-3: with no Content-Length header, content-length answered 0, so
  ;; a chunked body looked complete the instant its headers arrived and the
  ;; handler ran on whatever fragment happened to land in the first recv. A
  ;; 1.1 server that does not speak chunked must refuse it (411), never serve
  ;; a silent truncation.
  (let [captured (atom [])]
    (with-server {} captured
      (fn [port]
        (let [fd (connect! port)]
          (try
            (send-string fd (str "POST /v1/runs HTTP/1.1\r\n"
                                 "Content-Type: application/json\r\n"
                                 "Transfer-Encoding: chunked\r\n\r\n"
                                 "5\r\nhello\r\n0\r\n\r\n"))
            (let [r (read-response fd)]
              (is (str/starts-with? (:text r) "HTTP/1.1 411")
                  (str "got: " (pr-str (:text r))))
              (is (= [] @captured)
                  "the handler must never see a chunked request"))
            (finally (adapter/c-close fd))))))))

(deftest a-request-over-the-cap-is-refused-not-buffered-forever
  ;; provenance R3-4: the read loop appended whatever arrived with no ceiling, so a
  ;; Content-Length claim of any size was buffered in full — and read to the
  ;; end — before anyone looked at it.
  (let [captured (atom [])]
    (with-server {:max-request-bytes 1024} captured
      (fn [port]
        (let [fd (connect! port)]
          (try
            (send-string fd (str "POST /v1/runs HTTP/1.1\r\nContent-Length: 100000\r\n\r\n"
                                 (apply str (repeat 1100 "x"))))
            (let [r (read-response fd)]
              (is (str/starts-with? (:text r) "HTTP/1.1 413")
                  (str "got: " (pr-str (:text r))))
              (is (= [] @captured)
                  "an oversized request never reaches the handler"))
            (finally (adapter/c-close fd))))))))

(deftest a-request-that-stalls-is-closed-not-held
  ;; provenance R3-4: recv blocked forever with no SO_RCVTIMEO, so a client that
  ;; sent its headers and vanished held its connection thread for the life of
  ;; the server. With a server-side read timeout the stalled connection is
  ;; closed on the server's schedule — :closed? here is true only when the
  ;; hangup came from the other end, not from our own 5s guard.
  (let [captured (atom [])]
    (with-server {:read-timeout-ms 400} captured
      (fn [port]
        (let [fd (connect! port)]
          (try
            (send-string fd "POST /v1/runs HTTP/1.1\r\nContent-Length: 10\r\n\r\nabc")
            (let [r (read-response fd)]
              (is (:closed? r) "the server must close a stalled connection")
              (is (= [] @captured)
                  "a stalled request never reaches the handler"))
            (finally (adapter/c-close fd))))))))

(deftest a-body-split-across-packets-decodes-exactly-once
  ;; provenance R3-5: the accumulator decoded every recv separately and str'd the
  ;; pieces together, so a multibyte UTF-8 char landing across two packets
  ;; reached the handler as U+FFFD replacement chars. Octets must accumulate
  ;; raw and the string decode once, after the body is complete.
  (let [captured (atom [])
        body (str "{\"note\": \"" (apply str (repeat 8 "—")) "\"}")
        headers (str "POST /v1/runs HTTP/1.1\r\nContent-Type: application/json\r\n"
                     "Content-Length: " (alength (.getBytes body "UTF-8")) "\r\n\r\n")
        ;; headers and the body's ASCII prefix encode 1 byte per char, so the
        ;; encoded offset of the first em-dash equals its character index;
        ;; +1 lands the split INSIDE the 3-byte char (E2 | 80 94).
        split (+ (count headers) (count "{\"note\": \"") 1)]
    (with-server {} captured
      (fn [port]
        (let [fd (connect! port)]
          (try
            (let [[p n] (load-payload (str headers body))]
              (try
                (send-part fd p 0 split)
                (Thread/sleep 150)      ; two packets, not one coalesced send
                (send-part fd p split (- n split))
                (let [r (read-response fd)]
                  (is (str/starts-with? (:text r) "HTTP/1.1 200")
                      (str "got: " (pr-str (:text r)))))
                (finally (ffi/free p))))
            (finally (adapter/c-close fd))))
        (testing "the handler saw the body byte-exact"
          (is (= 1 (count @captured)) "the request was served")
          (when-let [req (first @captured)]
            (is (= body (slurp (:body req)))
                "no U+FFFD from a packet-split multibyte char")))))))

(deftest a-truncated-request-is-dropped-not-served-partial
  ;; The old reader returned whatever it had accumulated when the peer
  ;; vanished mid-body, so a client that died after half its Content-Length
  ;; still had its fragment handed to the handler as though it were the whole
  ;; request.
  (let [captured (atom [])]
    (with-server {} captured
      (fn [port]
        (let [fd (connect! port)]
          (try
            (send-string fd (str "POST /v1/runs HTTP/1.1\r\nContent-Length: 100\r\n\r\n"
                                 (apply str (repeat 40 "x"))))
            (Thread/sleep 200)         ; let the server recv the fragment
            (adapter/c-close fd)       ; vanish mid-body
            (Thread/sleep 200)         ; let the server notice the EOF
            (is (= [] @captured)
                "an incomplete request must not reach the handler")
            (finally (adapter/c-close fd))))))))
