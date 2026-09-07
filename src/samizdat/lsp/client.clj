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

(ns samizdat.lsp.client
  "A narrow LSP client over a persistent clojure-lsp subprocess.

  clojure-lsp is spoken to as `clojure-lsp listen`: LSP JSON-RPC messages,
  each framed with a Content-Length header, over the child's stdin/stdout.
  One server is started per project root and kept alive across turns —
  startup analyses the project, so per-request spawning would be far too
  slow.

  One dedicated reader thread per client reads every frame and routes it by
  id: a response delivers the promise its request is parked on, and
  server-pushed notifications go to the diagnostics store. This is what
  makes the client safe across concurrent callers — each request reading the
  shared stream itself let two callers steal each other's frames
  (provenance CR1-4). Each request is bounded by a timeout so a
  wedged server costs a known amount rather than the run."
  (:require ;; the java.time.* host shim, before data.json — see samizdat.store.journal
            [jolt.time]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [ebb.core :as ebb]
            [jolt.process :as jp]
            [samizdat.cancel :as cancel]
            [samizdat.engine.proc :as proc]
            [samizdat.security.secrets :as secrets])
  (:import [java.io BufferedInputStream OutputStream]))

;; root -> {:proc :out :in :next-id :opened :diagnostics :pending}
(defonce ^:private clients (atom {}))

(def ^:private read-timeout-ms 20000)

(defn available?
  "Whether clojure-lsp can be executed at all."
  []
  (proc/available? "clojure-lsp"))

;; ---- framing --------------------------------------------------------------

(defn- send-frame!
  "Write one Content-Length-framed JSON-RPC message. Synchronised on the
  stream so two callers cannot interleave a frame."
  [{:keys [^OutputStream out]} msg]
  (let [body (.getBytes ^String (json/write-str msg) "UTF-8")
        header (.getBytes (str "Content-Length: " (alength body) "\r\n\r\n") "UTF-8")]
    (locking out
      (.write out header) (.write out body) (.flush out))))

(defn- read-frame
  "Read one message: the Content-Length header block, then that many bytes
  of JSON. Blocking; callers bound it with a timeout. Returns nil at EOF."
  [{:keys [^BufferedInputStream in]}]
  (let [header (loop [sb (StringBuilder.)]
                 (let [c (.read in)]
                   (cond
                     (neg? c) nil
                     (str/ends-with? (.append sb (char c)) "\r\n\r\n") (.toString sb)
                     :else (recur sb))))
        m (some->> header (re-find #"(?i)Content-Length:\s*(\d+)"))]
    (when m
      (let [n (Integer/parseInt (second m))
            buf (byte-array n)]
        (loop [off 0]
          (when (< off n)
            (let [r (.read in buf off (- n off))]
              (when-not (neg? r) (recur (+ off r))))))
        (json/read-str (String. buf "UTF-8") :key-fn keyword)))))

(declare store-diagnostics!)

(defn- start-reader!
  "The one thread that reads this client's stream, for the client's life.

  Every frame is routed by id: a response delivers the promise its request!
  is parked on, and anything else (server-pushed notifications) goes to
  store-diagnostics!. A response whose waiter timed out is simply no longer
  pending, so it falls through to the notification path and is dropped —
  nobody else can consume it by mistake.

  Any exit — EOF, a read failure, or a throw while routing a frame (which
  used to kill the loop with waiters still parked, provenance R2-17) — releases
  every pending waiter with ::closed and evicts the client from the
  registry by its :root, so a later call starts a fresh server instead of
  reusing a corpse and waiting out the full timeout."
  [{:keys [pending] :as client}]
  (future
    (loop [alive? true]
      (let [msg (try (read-frame client)
                     (catch Throwable _ nil))
            ok? (when (and msg alive?)
                  (try
                    (if-let [[_ p] (find @pending (:id msg))]
                      (deliver p msg)
                      (store-diagnostics! client msg))
                    true
                    (catch Throwable _ false)))]
        (when (and msg ok?)
          (recur true))))
    ;; any exit: release every waiter, evict the corpse (provenance R2-17)
    (doseq [[_ p] @pending] (deliver p ::closed))
    (when-let [root (:root client)]
      (swap! clients dissoc root)))
  nil)

;; ---- request / notify -----------------------------------------------------

(defn- store-diagnostics! [client msg]
  (when (= "textDocument/publishDiagnostics" (:method msg))
    (let [uri (get-in msg [:params :uri])
          diags (get-in msg [:params :diagnostics])
          ;; Version the push so a waiter can tell a push that answers ITS
          ;; request from one that predates it (provenance R2-10).
          n (swap! (:diag-n client) inc)]
      (swap! (:diagnostics client) assoc uri {:n n :diags diags}))))

(defn- request!
  "Send a request and park on its id until the reader delivers the response
  (or the timeout / a server close intervenes). Returns the :result, throws
  on an :error, a timeout, or the server closing.

  The wait is a park on the caller's fiber, not a blocking deref on its
  carrier (RFC-013): the deref runs under `via blk` with the read timeout as
  its deadline, so a cancelled turn gets its thread back at once — a promise
  deref is interruptible — rather than when the timeout expires. The pending
  entry is released on every exit, a cancel included."
  [client method params]
  (let [id (swap! (:next-id client) inc)
        p (promise)]
    (swap! (:pending client) assoc id p)
    (try
      (send-frame! client {:jsonrpc "2.0" :id id :method method :params params})
      (let [[tag msg] (cancel/with-deadline (ebb/via ebb/blk (deref p)) read-timeout-ms)]
        (cond
          (= :timeout tag) (throw (ex-info (str "lsp: no response to " method) {:method method}))
          (= :err tag) (throw msg)
          (= ::closed msg) (throw (ex-info (str "lsp: server closed during " method) {:method method}))
          :else (if-let [e (:error msg)]
                  (throw (ex-info (str "lsp error: " (:message e)) {:error e}))
                  (:result msg))))
      (finally (swap! (:pending client) dissoc id)))))

(defn- notify! [client method params]
  (send-frame! client {:jsonrpc "2.0" :method method :params params}))

;; ---- lifecycle ------------------------------------------------------------

(defn- uri [path] (str "file://" (.getAbsolutePath (io/file path))))

(defn spawn-spec
  "What start! spawns, as data, so a test can hold the seam without a server.

  The environment is the SCRUBBED one — the same seam the shell tool, verify
  and gitdiff spawn through. clojure-lsp was the one subprocess inheriting the
  full parent environment, and it re-spawns children of its own (classpath
  resolution) that inherit whatever it got (karamazov-blt.27)."
  []
  {:cmd ["clojure-lsp" "listen"]
   :opts {:env (secrets/scrubbed-process-env)}})

(defn start!
  "Spawn clojure-lsp for `root` and run the initialize/initialized
  handshake. Returns the client map."
  [root]
  (let [{:keys [cmd opts]} (spawn-spec)
        p (jp/process cmd opts)
        ^java.lang.Process osproc (:proc p)
        client {:proc p
                :root root
                :out (.getOutputStream osproc)
                :in (BufferedInputStream. (.getInputStream osproc))
                :next-id (atom 0)
                :opened (atom #{})
                :diagnostics (atom {})
                :diag-n (atom 0)
                :pending (atom {})}]
    ;; The reader must be running before the first request parks on it.
    (start-reader! client)
    (request! client "initialize"
              {:processId nil :rootUri (uri root) :capabilities {}})
    (notify! client "initialized" {})
    client))

(defn client-for
  "The live client for `root`, started on first use. nil when clojure-lsp
  is not installed. The miss-then-start is one critical section (review2
  #9): two callers racing on a fresh root used to both start!, leaking the
  loser's clojure-lsp process and remembering only one of them."
  [root]
  (when (available?)
    (locking clients
      (or (get @clients root)
          (let [c (start! root)]
            (swap! clients assoc root c)
            c)))))

(defn shutdown!
  "Stop the server for `root`, if any."
  [root]
  (when-let [c (get @clients root)]
    (try (notify! c "exit" {}) (catch Throwable _ nil))
    (try (.destroyForcibly ^java.lang.Process (:proc (:proc c))) (catch Throwable _ nil))
    (swap! clients dissoc root)))

(defn shutdown-all!
  "Stop every spawned server. system/stop! calls this so process teardown
  does not strand clojure-lsp children the tool surface started lazily."
  []
  (locking clients
    (doseq [root (vec (keys @clients))]
      (shutdown! root))))

;; ---- document + narrow ops ------------------------------------------------

(defn- ensure-open! [client path]
  (when-not (contains? @(:opened client) path)
    (notify! client "textDocument/didOpen"
             {:textDocument {:uri (uri path) :languageId "clojure"
                             :version 1 :text (slurp path)}})
    (swap! (:opened client) conj path)))

(defn- at [client path line character method]
  (ensure-open! client path)
  (request! client method
            {:textDocument {:uri (uri path)}
             :position {:line line :character character}}))

(defn definition [client path line character]
  (at client path line character "textDocument/definition"))

(defn references [client path line character]
  (ensure-open! client path)
  (request! client "textDocument/references"
            {:textDocument {:uri (uri path)}
             :position {:line line :character character}
             :context {:includeDeclaration true}}))

(defn hover [client path line character]
  (at client path line character "textDocument/hover"))

(defn diagnostics
  "The problems clojure-lsp reports for `path`. didOpen/didChange trigger a
  publishDiagnostics push; the reader stores each push versioned per uri, so
  this takes the version at entry and waits for a push newer than it
  (provenance R2-10) — another caller on the same file cannot erase what this one
  is waiting for, and this one cannot return a push that predates its own
  request. A push that never arrives is an error, not a silent [] `clean`."
  [client path]
  (let [u (uri path)
        n0 @(:diag-n client)]
    (ensure-open! client path)
    ;; ask the server to (re-)analyse the doc as it stands on disk
    (notify! client "textDocument/didChange"
             {:textDocument {:uri u :version 2}
              :contentChanges [{:text (slurp path)}]})
    (loop [waited 0]
      (let [e (get @(:diagnostics client) u)]
        (cond
          (and e (> (:n e) n0)) (:diags e)
          (>= waited 5000) (throw (ex-info (str "lsp: no diagnostics push for " u)
                                           {:uri u}))
          :else (do (Thread/sleep 250)
                    (recur (+ waited 250))))))))
