;; Vendored from jolt-lang/ring-chez-adapter @07f14d9 and NOT ours to relicense,
;; so this file carries no veriframe copyright notice. Upstream is EPL-2.0,
;; which is what this project uses, so the vendored copy is redistributable on
;; the same terms as the rest of the tree.
;;
;; It had no licence at all when this copy was taken, which meant all rights
;; reserved and no permission to redistribute it. That is why the check happened.
;;
;; The VERIFRAME-marked change below is the only local modification.

(ns ring-chez.adapter
  "A Ring adapter for jolt: a minimal HTTP/1.1 server over BSD sockets, bound
  directly through jolt.ffi (no jolt built-in, no JVM). Synchronous Ring 1.x
  handlers. Serves loopback (127.0.0.1).

      (require '[ring-chez.adapter :as adapter])
      (def server (adapter/run-server my-handler {:port 3000}))
      ;; ... later ...
      (adapter/stop-server server)

  VENDORED from jolt-lang/ring-chez-adapter @07f14d9 with one change, marked
  `VERIFRAME` below: the accept loop hands each connection to a future instead
  of serving it inline. Upstream serves one connection at a time on the accept
  thread, so a POST /v1/chat/completions running a multi-minute beam blocks
  /health and every other request until it finishes. Worth offering upstream;
  vendored until then. See PLAN.md, \"Observability and intervention\"."
  (:require [clojure.string :as str]
            [jolt.ffi :as ffi]))

;; The libc/socket symbols are declared in deps.edn (:jolt/native :process) and
;; loaded by jolt before this namespace is required, so the bindings resolve.

;; accept/recv/send may block — :blocking emits them collect-safe so a parked
;; accept thread never pins the garbage collector.
(ffi/defcfn c-socket     "socket"     [:int :int :int] :int)
(ffi/defcfn c-bind       "bind"       [:int :pointer :int] :int)
(ffi/defcfn c-listen     "listen"     [:int :int] :int)
(ffi/defcfn c-setsockopt "setsockopt" [:int :int :int :pointer :int] :int)
(ffi/defcfn c-close      "close"      [:int] :int)
(ffi/defcfn c-shutdown   "shutdown"   [:int :int] :int)
(ffi/defcfn c-accept     "accept"     [:int :pointer :pointer] :int :blocking)
;; fcntl is variadic (int fd, int cmd, ...). The :varargs marker sits at the
;; fixed/variadic boundary; a fixed-arity binding silently corrupts the
;; stack-passed argument on Apple arm64, which is the same trap jolt.http.net
;; documents for its F_SETFL binding. F_GETFD passes no variadic argument, so
;; the reader is a safe fixed-arity binding.
(ffi/defcfn c-fcntl-set  "fcntl"      [:int :int :varargs :int] :int)
(ffi/defcfn c-fcntl-get  "fcntl"      [:int :int] :int)
(ffi/defcfn c-recv       "recv"       [:int :pointer :size_t :int] :ssize_t :blocking)
(ffi/defcfn c-send       "send"       [:int :pointer :size_t :int] :ssize_t :blocking)

(def ^:private AF-INET 2)
(def ^:private SOCK-STREAM 1)
(def ^:private macos?
  (str/includes? (str/lower-case (or (System/getProperty "os.name") "")) "mac"))
;; SOL_SOCKET / SO_REUSEADDR differ by platform: macOS 0xffff / 4, Linux 1 / 2.
(def ^:private sol-socket  (if macos? 0xffff 1))
(def ^:private so-reuse    (if macos? 4 2))
;; SO_RCVTIMEO likewise: macOS 0x1006, Linux 20.
(def ^:private so-rcvtimeo (if macos? 0x1006 20))
;; F_GETFD / F_SETFD / FD_CLOEXEC are 1 / 2 / 1 on both macOS and Linux.
(def ^:private f-getfd     1)
(def ^:private f-setfd     2)
(def ^:private fd-cloexec  1)
;; Linux can ask socket(2) for the flag directly, which avoids fcntl on the
;; listener entirely. macOS has no such bit and must use fcntl.
(def ^:private sock-cloexec (if macos? 0 0x80000))

(defn- close-on-exec!
  "Mark `fd` so it is NOT inherited across exec, and say whether it took.

  Every process the harness spawns — the Lean repl via `lake env`, prolog,
  octave — forks from this one, and without this each child holds a duplicate
  of whatever sockets were open. lsof showed jolt, lake and repl sharing fd 4
  on the listening socket. The port then stays bound while ANY of them lives,
  so killing the server with a Lean session still up leaves the next start
  failing with address-in-use against a server that no longer exists.

  Returns true when the flag is readable back. An earlier version swallowed
  the fcntl result and returned the fd regardless, which passed on macOS and
  silently did nothing on CI — the whole failure mode this guards against,
  reproduced in the guard itself. Callers that care must check."
  [fd]
  (try
    (c-fcntl-set fd f-setfd fd-cloexec)
    (pos? (bit-and (c-fcntl-get fd f-getfd) fd-cloexec))
    (catch Throwable _ false)))

(defn cloexec?
  "Whether `fd` is marked close-on-exec. Exposed for the test that proves it."
  [fd]
  (try (pos? (bit-and (c-fcntl-get fd f-getfd) fd-cloexec))
       (catch Throwable _ false)))

;; sockaddr_in for 127.0.0.1:port. macOS: byte0 = sin_len (16), byte1 = family;
;; Linux: bytes0-1 = family (little-endian, so byte0 = AF_INET).
(defn- make-sockaddr [port]
  (let [sa (ffi/alloc 16)]
    (dotimes [i 16] (ffi/write sa :uint8 0 i))
    (if macos?
      (do (ffi/write sa :uint8 16 0) (ffi/write sa :uint8 AF-INET 1))
      (ffi/write sa :uint8 AF-INET 0))
    (ffi/write sa :uint8 (bit-and (bit-shift-right port 8) 0xff) 2)   ; port hi (network order)
    (ffi/write sa :uint8 (bit-and port 0xff) 3)                       ; port lo
    (ffi/write sa :uint8 127 4) (ffi/write sa :uint8 0 5)             ; 127.0.0.1
    (ffi/write sa :uint8 0 6)   (ffi/write sa :uint8 1 7)
    sa))

(defn- listen-socket [port]
  ;; SOCK_CLOEXEC where the platform has it, so the fd is never briefly
  ;; inheritable between socket() and fcntl(); close-on-exec! below still runs
  ;; and is what macOS relies on.
  (let [fd (c-socket AF-INET (bit-or SOCK-STREAM sock-cloexec) 0)]
    (when (neg? fd) (throw (ex-info "socket() failed" {})))
    (let [opt (ffi/alloc 4)]
      (ffi/write opt :int 1 0)
      (c-setsockopt fd sol-socket so-reuse opt 4)
      (ffi/free opt))
    (let [sa (make-sockaddr port)]
      (when (neg? (c-bind fd sa 16))
        (c-close fd) (ffi/free sa) (throw (ex-info (str "bind() failed on port " port) {})))
      (ffi/free sa))
    (when (neg? (c-listen fd 64)) (c-close fd) (throw (ex-info "listen() failed" {})))
    ;; Return the fd, not the flag: close-on-exec! answers whether it took, and
    ;; ending the let on it handed run-server :socket true, which every later
    ;; accept and close then used as the descriptor.
    (close-on-exec! fd)
    fd))

(defn- set-recv-timeout!
  "review3 #4: without a receive timeout, a client that sent its headers and
  stalled held this connection thread in recv for the life of the server.
  A kernel timeout turns the stall into a dead recv, which read-request reads
  as a dropped connection. struct timeval is two time_t-width fields; the usec
  half is written full-width so both layouts find the value in place (macOS
  stores int32 there, and a value under 2^31 leaves the padding bytes zero)."
  [fd ms]
  (try
    (let [tv (ffi/alloc 16)]
      (ffi/write tv :int64 (quot (long ms) 1000) 0)
      (ffi/write tv :int64 (* 1000 (rem (long ms) 1000)) 8)  ; tv_usec is µs
      (try (c-setsockopt fd sol-socket so-rcvtimeo tv 16)
           (finally (ffi/free tv))))
    (catch Throwable _ nil)))

;; --- request reading --------------------------------------------------------
(def ^:private bufsize 65536)

;; review3 #4: every body this API takes is JSON; 2 MiB is far past any real
;; request and stops a Content-Length claim from being buffered in full.
(def ^:private max-request 2097152)

(defn- content-length [text hdr-end]
  (let [hdrs (str/lower-case (subs text 0 hdr-end))
        i (str/index-of hdrs "content-length:")]
    (if-not i
      0
      (let [s (+ i (count "content-length:"))
            e (loop [j s] (if (or (>= j (count hdrs))
                                  (= \return (nth hdrs j)) (= \newline (nth hdrs j))) j (recur (inc j))))]
        (or (parse-long (str/trim (subs hdrs s e))) 0)))))

;; VERIFRAME: Content-Length is octets, but the accumulator is a decoded
;; string, so `count` on it is characters. Judging completeness by characters
;; left any body with multibyte UTF-8 waiting forever for bytes that had
;; already arrived. Headers are ASCII, so the header/body split index is safe.
(defn- request-complete? [acc]
  (when-let [hdr-end (str/index-of acc "\r\n\r\n")]
    (>= (alength (.getBytes (subs acc (+ hdr-end 4)) "UTF-8"))
        (content-length acc hdr-end))))

;; review3 #5: read-bytes decodes exactly the octets it is handed, so decoding
;; each recv separately and str-ing the pieces together turned a multibyte
;; UTF-8 char split across packets into U+FFFD replacement chars. Octets
;; accumulate raw in one buffer — read-into! appends each recv's slice — and
;; the string is decoded once, from all of them at a time.
(defn- decode-acc [acc len]
  (let [p (ffi/alloc (max 1 len))]
    (try
      (ffi/write-array p acc 0 len)
      (ffi/read-bytes p len)
      (finally (ffi/free p)))))

;; review3 #3: with no Content-Length header, content-length answered 0, so a
;; chunked body looked complete the moment its headers arrived and the handler
;; ran on whatever fragment landed in the first recv. This adapter speaks
;; Content-Length only; a request carrying any Transfer-Encoding is refused
;; with 411 rather than served as a silent truncation.
(defn- chunked? [text]
  (when-let [hdr-end (str/index-of text "\r\n\r\n")]
    (boolean (str/index-of (str/lower-case (subs text 0 hdr-end))
                           "transfer-encoding:"))))

;; read a full request (headers + Content-Length body), or a keyword saying
;; why it could not be read: ::chunked (411) / ::too-large (413). nil means
;; the connection ended before the request was complete — an incomplete
;; request is dropped, never handed on as a truncated one.
(defn- read-request [conn max-bytes]
  (let [buf (ffi/alloc bufsize)]
    (try
      (loop [acc (byte-array bufsize), len 0]
        (let [n (c-recv conn buf bufsize 0)]
          (if (<= n 0)
            nil
            (let [need (+ len n)]
              (if (> need max-bytes)
                ::too-large
                (let [acc (if (> need (alength acc))
                            (java.util.Arrays/copyOfRange acc 0 (* 2 need))
                            acc)]
                  (ffi/read-into! buf acc len n)
                  (let [text (decode-acc acc need)]
                    (cond
                      (chunked? text)          ::chunked
                      (request-complete? text) text
                      :else                    (recur acc need)))))))))
      (finally (ffi/free buf)))))

;; --- request -> Ring map ----------------------------------------------------
(defn- request->ring [text port]
  (let [blank (str/index-of text "\r\n\r\n")
        head (if blank (subs text 0 blank) text)
        body (if blank (subs text (+ blank 4)) "")
        lines (str/split head #"\r\n")
        parts (str/split (or (first lines) "GET / HTTP/1.1") #" ")
        method (or (first parts) "GET")
        target (or (second parts) "/")
        qi (str/index-of target "?")
        [uri qs] (if qi [(subs target 0 qi) (subs target (inc qi))] [target nil])
        headers (reduce (fn [m line]
                          (let [i (str/index-of line ":")]
                            (if (and i (pos? i))
                              (assoc m (str/lower-case (str/trim (subs line 0 i))) (str/trim (subs line (inc i))))
                              m)))
                        {} (rest lines))]
    {:server-port    port
     :server-name    "127.0.0.1"
     :remote-addr    "127.0.0.1"
     :uri            uri
     :query-string   qs
     :scheme         :http
     :request-method (keyword (str/lower-case method))
     :protocol       "HTTP/1.1"
     :headers        headers
     :body           (when (pos? (count body)) (java.io.StringReader. body))}))

;; --- Ring response -> the response string -----------------------------------
(def ^:private status-text
  {200 "OK" 201 "Created" 204 "No Content" 301 "Moved Permanently" 302 "Found"
   303 "See Other" 304 "Not Modified" 400 "Bad Request" 401 "Unauthorized"
   403 "Forbidden" 404 "Not Found" 405 "Method Not Allowed"
   409 "Conflict" 411 "Length Required" 413 "Payload Too Large"
   500 "Internal Server Error" 503 "Service Unavailable"})

(defn- body->string [b]
  (cond (nil? b) ""
        (string? b) b
        (or (seq? b) (vector? b)) (apply str b)
        ;; a File / InputStream / Reader body (ring's resource + file responses):
        ;; read its contents rather than printing the object.
        :else (try (slurp b) (catch Throwable _ (str b)))))

(defn- response->string [resp]
  (let [status (or (:status resp) 200)
        body (body->string (:body resp))
        ;; Content-Length is the body's octet count. ring-defaults'
        ;; wrap-content-length already sets it (as UTF-8 bytes); honor that
        ;; and only compute when absent, so we never stamp a second, conflicting
        ;; Content-Length. Connection: close also delimits the response.
        len (or (->> (:headers resp)
                     (some (fn [[k v]]
                             (let [kn (str/lower-case (if (keyword? k) (name k) (str k)))]
                               (when (= kn "content-length") v)))))
                (alength (.getBytes body "UTF-8")))
        sb (StringBuilder.)]
    (.append sb (str "HTTP/1.1 " status " " (get status-text status "OK") "\r\n"))
    (doseq [[k v] (:headers resp)]
      (let [kn (str/lower-case (if (keyword? k) (name k) (str k)))]
        (when (not= kn "content-length")
          (.append sb (str (if (keyword? k) (name k) (str k)) ": " v "\r\n")))))
    (.append sb (str "Content-Length: " len "\r\n"))
    (.append sb "Connection: close\r\n\r\n")
    (.append sb body)
    (.toString sb)))

(defn- send-all [conn s]
  (let [buf (ffi/alloc (max 1 (* 4 (count s))))     ; UTF-8 worst case 4 bytes/char
        n (ffi/write-bytes buf s)]
    (try
      (loop [off 0]
        (when (< off n)
          (let [sent (c-send conn (+ buf off) (- n off) 0)]
            (if (pos? sent)
              (recur (+ off sent))
              ;; review3 #12: returning here with bytes unwritten would leave
              ;; the client holding a Content-Length-complete truncated body
              ;; — a lie that parses. Throw; serve-conn's error path closes.
              (throw (ex-info "send() stopped mid-body" {:wrote off :of n}))))))
      (finally (ffi/free buf)))))

(defn- drain!
  "Read and discard what the peer still has in flight, bounded by the recv
  timeout and 4 MiB. An error response usually leaves unread request bytes in
  the socket (the chunked body we refused, the tail of an oversized one);
  closing with unread data sends RST, and the RST can destroy the response we
  just wrote before the client has read it."
  [conn]
  (let [buf (ffi/alloc bufsize)]
    (try
      (loop [drained 0]
        (let [n (c-recv conn buf bufsize 0)]
          (when (and (pos? n) (< drained 4194304))
            (recur (+ drained n)))))
      (finally (ffi/free buf)))))

(defn- send-error!
  "Send an error response, then drain so the close that follows cannot RST the
  response away."
  [conn status body]
  (send-all conn (response->string {:status status
                                    :headers {"Content-Type" "text/plain"}
                                    :body body}))
  (drain! conn))

;; --- the accept loop --------------------------------------------------------
;; Clean shutdown: stop-server closes the listen fd (which unblocks accept) and
;; clears `running?`; the loop then exits instead of spinning on the dead fd.
;; VERIFRAME: one connection's full lifecycle, extracted so the accept loop can
;; hand it to a worker instead of running it inline.
(defn- serve-conn [conn handler port opts]
  (try
    (try
      (set-recv-timeout! conn (get opts :read-timeout-ms 30000))
      (let [r (read-request conn (get opts :max-request-bytes max-request))]
        (cond
          (nil? r) nil
          (= ::chunked r)
          (send-error! conn 411
                       "Length Required: this server reads Content-Length bodies only")
          (= ::too-large r)
          (send-error! conn 413 "Payload Too Large")
          :else
          (send-all conn (response->string (handler (request->ring r port))))))
      (catch Throwable _e
        (try (send-error! conn 500 "Internal Server Error")
             (catch Throwable _ nil))))
    (finally (c-close conn))))

(defn- serve-loop [listen-fd handler port running? opts]
  (loop []
    (let [conn (c-accept listen-fd ffi/null ffi/null)]
      (cond
        ;; review3 #12: a connection accepted inside the stop-server window
        ;; was dropped with its fd still open. Close what accept handed back.
        (not @running?) (do (when (>= conn 0) (c-close conn)) nil)
        (neg? conn) (when @running? (recur))
        :else
        (do
          ;; VERIFRAME: thread per connection. The accept loop returns to
          ;; accept immediately, so a slow handler can't stall the server.
          ;; accept(2) does not inherit the listener's close-on-exec, so each
          ;; connection is marked too — otherwise a subprocess spawned while a
          ;; request is in flight holds that client's socket open.
          (close-on-exec! conn)
          (future (serve-conn conn handler port opts))
          (recur))))))

(defn run-server
  "Start the server; return a handle {:socket :port :running}. The accept loop
  runs on a background thread; the handler is a synchronous Ring handler. opts:
  :port (default 3000), :read-timeout-ms (default 30000 — how long a connection
  may stall mid-request before the server drops it), :max-request-bytes
  (default 2 MiB — larger requests are refused with 413)."
  [handler opts]
  (let [port (get opts :port 3000)
        fd (listen-socket port)
        running? (atom true)
        opts (merge {:read-timeout-ms 30000 :max-request-bytes max-request} opts)]
    (future (serve-loop fd handler port running? opts))
    {:socket fd :port port :running running?}))

(defn stop-server
  "Stop the server: unblock + exit the accept loop and close the listen socket."
  [server]
  (reset! (:running server) false)
  ;; shutdown BEFORE close, because close alone does not reliably wake a thread
  ;; already blocked in accept(). On macOS it does; on Linux the blocked accept
  ;; keeps the socket alive and the port stays bound after stop-server returns,
  ;; so a restart fails with address-in-use. shutdown is what wakes it on both.
  ;; ENOTCONN from a listening socket (macOS) is expected and ignored.
  (try (c-shutdown (:socket server) 2) (catch Throwable _ nil))
  (c-close (:socket server))
  nil)
