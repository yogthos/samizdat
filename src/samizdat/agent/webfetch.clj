;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.agent.webfetch
  "Reading a named URL. MECHANISM: fetch, confine, convert, cap.

  WHY IT EXISTS (karamazov-fn68.2). `websearch` finds pages; nothing could
  READ one. A model that already knows the URL of an API doc, an upstream
  issue or a spec had no way to open it, so the knowledge was reachable only
  by searching for what it already had.

  PORTED FROM opencode's webfetch tool, whose decisions are worth keeping: a
  format argument defaulting to text rather than raw HTML, a response size cap,
  a bounded timeout, and http upgraded to https. What is NOT ported is its
  security posture, because opencode's is a browser's and samizdat's is not.

  EGRESS IS THE PART THAT IS OURS TO GET RIGHT. RFC-003 says what contains the
  model and what deliberately does not, and samizdat.security scrubs the
  environment a subprocess sees precisely so a tool cannot read a secret the
  parent holds. A fetch tool is an egress surface: without a check, `webfetch`
  is a way to reach the loopback interface, the cloud metadata endpoint, or any
  service on the private network the harness happens to sit in — and to do it
  with a URL the model composed. So a destination is REFUSED unless it is a
  public host, and every redirect hop is checked before it is requested,
  because a public URL that 302s to 169.254.169.254 defeats a check made only
  at the start.

  TWO WAYS THAT GUARD HAD BEEN WRONG (karamazov-luqc.1, found comparing it
  against ZCode's egress guard). The HTTP client followed redirects on its
  own, so the response the 're-check' looked at was the final page, whose
  Location header is nil — the private body had already been fetched and the
  check passed the original URL. And the host check was a regex over the
  hostname, which `2130706433`, `0177.0.0.1`, `0x7f000001` and
  `[::ffff:169.254.169.254]` all walk past on their way to loopback and the
  metadata endpoint. Now the client is told not to follow, the loop below
  follows one checked hop at a time, and a host that parses as an IP literal
  is judged by its ADDRESS, not its spelling.

  The body is data, never instructions: it is returned as a tool result like
  any other, and nothing here executes or renders it."
  (:require [clojure.string :as str]
            [jolt.http-client :as http]
            [samizdat.lexicon :as lexicon]
            [samizdat.prompt :as prompt]))

(defn- msg [ctx] (prompt/render "webfetch-tool" ctx))

(defn policy
  "The fetch budget, from gates.edn :webfetch."
  []
  (lexicon/policy :webfetch))

;;; ------------------------------------------------------------ confinement

(def ^:private private-name-patterns
  "NAME shapes that must never be fetched, as regex strings: loopback by
  name, and the names a private network hands out. An address — anything
  that parses as an IPv4 or IPv6 literal, in any spelling — is not judged
  here but by `private-address?` below, on its value.

  IN src/ AND NOT IN resources, like the other guards in this project: a check
  the agent can edit at runtime is not a check (samizdat.store.db's
  required-columns makes the same argument). The BUDGETS below are policy and
  live in gates.edn; the confinement is mechanism and lives here."
  ["(?i)^localhost$" "(?i)\\.local$" "(?i)\\.internal$" "(?i)^metadata"])

;; --- reading an address literal --------------------------------------------
;;
;; A tiny inet_aton / inet_pton, pure, so the guard can judge an address by
;; its value without a resolver in the loop. Every accepted spelling is one
;; the platform's resolver also accepts — that is the whole point: a guard
;; that refuses `127.0.0.1` and passes `2130706433` refuses a string, not a
;; destination.

(defn- parse-ipv4-part
  "One inet_aton part: decimal, `0x` hex, or leading-zero octal. nil when it
  is not a number in that grammar."
  [s]
  (try
    (cond
      (re-matches #"0[xX][0-9a-fA-F]+" s) (Long/parseLong (subs s 2) 16)
      (re-matches #"0[0-7]+" s) (Long/parseLong (subs s 1) 8)
      (re-matches #"[0-9]+" s) (Long/parseLong s 10)
      :else nil)
    (catch Throwable _ nil)))

(defn- parse-ipv4
  "The 32-bit value of an IPv4 literal in inet_aton's grammar — one to four
  parts, the last part filling the remaining bytes (`127.1` is 127.0.0.1,
  `2130706433` is too) — or nil when `s` is not one."
  [s]
  (let [parts (str/split (str s) #"\." -1)
        vals (mapv parse-ipv4-part parts)
        n (count parts)]
    (when (and (<= 1 n 4) (every? some? vals))
      (let [heads (subvec vals 0 (dec n))
            tail (peek vals)
            tail-bytes (- 5 n)]
        (when (and (every? #(<= 0 % 255) heads)
                   (<= 0 tail)
                   (< tail (bit-shift-left 1 (* 8 tail-bytes))))
          ;; the leading parts are one byte each; the last fills the rest
          (+ (bit-shift-left (reduce (fn [acc b] (+ (* acc 256) b)) 0 heads)
                             (* 8 tail-bytes))
             tail))))))

(defn- ipv4-bytes->words
  "A 32-bit IPv4 value as the two 16-bit words it occupies inside an IPv6
  address."
  [v]
  [(bit-shift-right v 16) (bit-and v 0xffff)])

(defn- parse-ipv6
  "The eight 16-bit words of an IPv6 literal, or nil. Handles `::`
  compression and a trailing embedded dotted quad (`::ffff:127.0.0.1`).
  Brackets are stripped by the caller."
  [s]
  (let [s (str s)]
    (when (and (str/includes? s ":") (<= (count (re-seq #"::" s)) 1))
      (let [[head tail] (str/split s #"::" -1)
            split (fn [part] (if (str/blank? part) [] (str/split part #":" -1)))
            words-of
            (fn [parts]
              ;; the last part may be a dotted quad, which is two words
              (reduce (fn [acc p]
                        (cond
                          (nil? acc) nil
                          (re-matches #"[0-9a-fA-F]{1,4}" p)
                          (conj acc (Long/parseLong p 16))
                          (and (str/includes? p ".") (= p (peek parts)))
                          (when-let [v (parse-ipv4 p)]
                            (into acc (ipv4-bytes->words v)))
                          :else nil))
                      []
                      parts))
            h (words-of (split head))
            t (when (some? tail) (words-of (split tail)))
            compressed? (str/includes? s "::")]
        (cond
          (or (nil? h) (and compressed? (nil? t))) nil
          compressed? (when (< (+ (count h) (count t)) 8)
                        (vec (concat h (repeat (- 8 (count h) (count t)) 0) t)))
          :else (when (= 8 (count h)) h))))))

(defn- ipv6-carried-ipv4
  "The IPv4 address an IPv6 literal carries, when it is one of the transition
  forms that stands in for one: IPv4-mapped `::ffff:a.b.c.d`, the deprecated
  IPv4-compatible `::a.b.c.d`, NAT64 `64:ff9b::a.b.c.d`, 6to4 `2002:AABB:CCDD::`
  and Teredo `2001:0:…` (client address in the last two words, inverted).
  A guard that judged only the IPv6 prefix would let each of these reach the
  IPv4 host it names."
  [[w0 w1 w2 w3 w4 w5 w6 w7 :as words]]
  (let [v4 (fn [a b] (+ (* a 65536) b))]
    (cond
      (= [0 0 0 0 0 0xffff] (subvec words 0 6)) (v4 w6 w7)
      (and (= [0 0 0 0 0 0] (subvec words 0 6)) (not= [0 0] [w6 w7])) (v4 w6 w7)
      (= [0x64 0xff9b 0 0 0 0] (subvec words 0 6)) (v4 w6 w7)
      (= 0x2002 w0) (v4 w1 w2)
      (and (= 0x2001 w0) (= 0 w1)) (bit-and 0xffffffff (bit-xor 0xffffffff (v4 w6 w7)))
      :else nil)))

(defn- in-v4-block?
  "Whether the 32-bit `v` lies in the CIDR block `base/bits`."
  [v base bits]
  (= (bit-shift-right v (- 32 bits)) (bit-shift-right base (- 32 bits))))

(defn- private-ipv4?
  "The IPv4 blocks that name this machine, its network, or nothing routable:
  this-host 0/8, RFC1918 10/8 172.16/12 192.168/16, CGNAT 100.64/10,
  loopback 127/8, link-local 169.254/16 (cloud metadata lives there),
  benchmarking 198.18/15, multicast 224/4 and reserved 240/4."
  [v]
  (boolean
   (some (fn [[base bits]] (in-v4-block? v base bits))
         [[0x00000000 8] [0x0a000000 8] [0xac100000 12] [0xc0a80000 16]
          [0x64400000 10] [0x7f000000 8] [0xa9fe0000 16] [0xc6120000 15]
          [0xe0000000 4] [0xf0000000 4]])))

(defn- private-ipv6?
  "The IPv6 addresses that name this machine or its network: unspecified and
  loopback, link-local fe80::/10, unique-local fc00::/7, multicast ff00::/8 —
  and any transition address whose carried IPv4 is private."
  [[w0 :as words]]
  (boolean
   (or (= [0 0 0 0 0 0 0 0] words)
       (= [0 0 0 0 0 0 0 1] words)
       (= 0xfe80 (bit-and w0 0xffc0))
       (= 0xfc00 (bit-and w0 0xfe00))
       (= 0xff00 (bit-and w0 0xff00))
       (some-> (ipv6-carried-ipv4 words) private-ipv4?))))

(defn private-address?
  "Whether `host`, read as an IP literal, names something private — or nil
  when it is not a literal at all and has to be judged as a name."
  [host]
  (let [h (str/replace (str host) #"^\[|\]$" "")]
    (if-let [v4 (parse-ipv4 h)]
      (private-ipv4? v4)
      (when-let [v6 (parse-ipv6 h)]
        (private-ipv6? v6)))))

(defn host-of
  "The host of `url`, lowercased, or nil when it does not parse."
  [url]
  (try (some-> (java.net.URI. (str url)) .getHost str/lower-case not-empty)
       (catch Throwable _ nil)))

(defn private-host?
  "Whether `host` names something on the machine or its private network: an
  address literal by its value, a name by `private-name-patterns`.

  Pure and string-shaped on purpose: this is a guard, and a guard that needs a
  DNS round trip to answer has already become a place where a slow resolver
  makes the harness hang. It cannot catch a public name that RESOLVES to a
  private address; that is a known limit, stated rather than papered over, and
  the reason every redirect hop is checked below."
  [host]
  (boolean
   (when host
     (let [by-address (private-address? host)]
       (if (some? by-address)
         by-address
         (some #(re-find (re-pattern %) host) private-name-patterns))))))

(defn allowed?
  "Whether `url` may be fetched at all: https or http, a host that parses, and
  not a private one."
  [url]
  (let [u (str url)
        scheme (try (some-> (java.net.URI. u) .getScheme str/lower-case)
                    (catch Throwable _ nil))]
    (boolean (and (contains? #{"http" "https"} scheme)
                  (not (private-host? (host-of u)))))))

(defn https-upgrade
  "http -> https, as opencode does. A plaintext fetch of a public page is a
  downgrade nobody asked for, and every host worth reading serves TLS."
  [url]
  (if (str/starts-with? (str url) "http://")
    (str "https://" (subs (str url) 7))
    (str url)))

;;; --------------------------------------------------------------- content

(defn strip-html
  "HTML to readable text, by the ordered [pattern replacement] pairs in
  wordlists.edn :html-strip.

  NOT A PARSER, and it does not pretend to be. The reader is a model, which
  wants the prose; a dependency that renders the DOM faithfully would be a
  large addition for a marginal gain, and `format :html` is there for the
  caller that genuinely wants the source.

  The pairs are DATA because they are a vocabulary — a project fetching a site
  that marks up its docs differently retunes them without a rebuild, which is
  base-test's rule and it is right. The confinement above stays in src/."
  [html]
  (-> (reduce (fn [t [pattern replacement]]
                (str/replace t (re-pattern (str pattern)) (str replacement)))
              (str html)
              (lexicon/wordlist :html-strip))
      str/trim))

(defn- redirect-target
  "Where a 3xx response points, resolved against the URL that answered it —
  a Location is allowed to be relative — or nil when the response is not a
  redirect or names nowhere."
  [url {:keys [status headers]}]
  (when (contains? #{301 302 303 307 308} status)
    (when-let [loc (not-empty (str (get headers "location")))]
      (try (str (.resolve (java.net.URI. (str url)) loc))
           (catch Throwable _ nil)))))

(defn- follow
  "Request `url` with `get`, following redirects BY HAND, one checked hop at
  a time, up to `max-redirects` of them. Returns `{:url :resp}` for the
  response that ended the chain, or `{:error msg}` when a hop pointed
  somewhere private or the chain did not end.

  The client is told not to follow (`:follow-redirects false`) because a
  client that follows on its own presents the LAST response, whose Location
  is nil, and a check on that passes whatever the first URL was — which is
  how the earlier version of this guard fetched the private body it was
  refusing (karamazov-luqc.1)."
  [get url opts max-redirects]
  (loop [url url hops 0]
    (let [resp (get url (assoc opts :follow-redirects false))
          to (redirect-target url resp)]
      (cond
        (nil? to) {:url url :resp resp}

        (not (allowed? to))
        {:error (msg {:refused-redirect true :url url :to to})}

        (>= hops max-redirects)
        {:error (msg {:redirect-loop true :url url :hops hops :to to})}

        :else (recur to (inc hops))))))

(defn fetch
  "Fetch `url`. Returns {:ok text :status :content-type} or {:error msg}.

  Every failure is a MESSAGE rather than a throw, because this sits behind a
  tool and a model reading `connection refused` learns more than a branch
  dying does.

  `opts`:
    :format        \"html\" for the raw source; anything else is text
    :timeout-ms    the request's socket timeout, capped by the policy
    :get           THE EFFECT, injectable: (fn [url opts] -> response),
                   `jolt.http-client/get` by default. A test hands in a
                   scripted one and the whole redirect discipline runs
                   offline.
    :max-redirects hops to follow before giving up; gates.edn :webfetch"
  [url {:keys [format timeout-ms get max-redirects]}]
  (let [p (policy)
        get (or get http/get)
        max-redirects (long (or max-redirects (:max-redirects p)))
        url (https-upgrade url)]
    (cond
      (not (allowed? url))
      {:error (msg {:refused true :url url :host (host-of url)})}

      :else
      (try
        (let [{:keys [error resp] final :url}
              (follow get url
                      {:headers {"Accept" "text/html,text/plain,application/json;q=0.9,*/*;q=0.8"
                                 "User-Agent" (:user-agent p)}
                       :socket-timeout (min (long (or timeout-ms (:timeout-ms p)))
                                            (long (:max-timeout-ms p)))
                       :throw-exceptions false}
                      max-redirects)
              status (:status resp)
              body (str (:body resp))
              ctype (str (get-in resp [:headers "content-type"]))]
          (cond
            error {:error error}

            (not= 200 status)
            {:error (msg {:bad-status true :status status :url final})}

            :else
            (let [text (case (str format)
                         "html" body
                         (if (str/includes? (str/lower-case ctype) "html")
                           (strip-html body)
                           body))
                  cap (long (:max-chars p))]
              {:ok (if (> (count text) cap)
                     (str (subs text 0 cap)
                          (msg {:truncated true :chars cap :total (count text)}))
                     text)
               :status status
               :content-type ctype})))
        (catch Throwable e
          {:error (msg {:failed true :url url :detail (ex-message e)})})))))
