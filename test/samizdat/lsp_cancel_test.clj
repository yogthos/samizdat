;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.lsp-cancel-test
  "The LSP client's request wait over the deadline idiom (RFC-013,
  karamazov-3cll.8). A request parked on the reader's promise used to block
  the turn's carrier for up to the 20 s read timeout, and a cancel landed only
  when it expired; the wait now runs under `cancel/with-deadline` over
  `via blk`, so a cancelled turn gets its thread back at once and the pending
  entry is released on every exit."
  (:require [clojure.test :refer [deftest is]]
            [ebb.core :as ebb]
            [samizdat.cancel :as cancel]
            [samizdat.lsp.client :as client]))

(defn- silent-client
  "A client whose server never answers: the reader is up, the stream is open,
  and nothing ever arrives."
  []
  (let [resp-writer (java.io.PipedOutputStream.)]
    {:in (java.io.BufferedInputStream. (java.io.PipedInputStream. resp-writer))
     :out (java.io.ByteArrayOutputStream.)
     :root "/tmp/samizdat-lsp-cancel-root"
     :next-id (atom 0)
     :opened (atom #{})
     :diagnostics (atom {})
     :diag-n (atom 0)
     :pending (atom {})
     :writer resp-writer}))

(deftest a-cancelled-request-returns-at-the-cancel-not-the-read-timeout
  (let [c (silent-client)]
    (try
      (#'client/start-reader! c)
      (let [{:keys [cancel done]} (cancel/start! (cancel/spawn #(#'client/request! c "textDocument/hover" {})))]
        (while (nil? (get @(:pending c) 1)) (Thread/sleep 5))
        (let [t0 (System/currentTimeMillis)]
          (cancel)
          (let [[tag e] (deref done 5000 [:unsettled nil])]
            (is (= :err tag))
            (is (ebb/cancelled? e) "Cancelled, not the 20 s read timeout")
            (is (< (- (System/currentTimeMillis) t0) 2000) "at the cancel")
            (is (empty? @(:pending c)) "the pending entry is released on the way out"))))
      (finally (.close (:writer c))))))

(deftest the-read-timeout-still-bounds-a-request
  (let [c (silent-client)]
    (try
      (#'client/start-reader! c)
      (with-redefs-fn {(ns-resolve 'samizdat.lsp.client 'read-timeout-ms) 300}
        (fn []
          (let [t0 (System/currentTimeMillis)
                e (try (#'client/request! c "textDocument/hover" {}) nil
                       (catch Throwable e e))]
            (is (some? e))
            (is (re-find #"no response" (ex-message e)) "a silent server is still a timeout, not a hang")
            (is (< 250 (- (System/currentTimeMillis) t0) 3000))
            (is (empty? @(:pending c))))))
      (finally (.close (:writer c))))))
