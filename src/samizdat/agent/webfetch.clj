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
  public host, and a redirect is re-checked rather than trusted, because a
  public URL that 302s to 169.254.169.254 defeats a check made only at the
  start.

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

(def ^:private private-patterns
  "Host shapes that must never be fetched, as regex strings.

  IN src/ AND NOT IN resources, like the other guards in this project: a check
  the agent can edit at runtime is not a check (samizdat.store.db's
  required-columns makes the same argument). The BUDGETS below are policy and
  live in gates.edn; the confinement is mechanism and lives here."
  [;; loopback, in every spelling that resolves to it
   "(?i)^localhost$" "(?i)^127\\." "(?i)^0\\.0\\.0\\.0$" "(?i)^\\[?::1\\]?$"
   ;; link-local, which is where cloud metadata lives (169.254.169.254)
   "(?i)^169\\.254\\." "(?i)^fe80:"
   ;; RFC1918 private ranges
   "(?i)^10\\." "(?i)^192\\.168\\."
   "(?i)^172\\.(1[6-9]|2[0-9]|3[01])\\."
   ;; and the names a private network hands out
   "(?i)\\.local$" "(?i)\\.internal$" "(?i)^metadata"])

(defn host-of
  "The host of `url`, lowercased, or nil when it does not parse."
  [url]
  (try (some-> (java.net.URI. (str url)) .getHost str/lower-case not-empty)
       (catch Throwable _ nil)))

(defn private-host?
  "Whether `host` names something on the machine or its private network.

  Pure and string-shaped on purpose: this is a guard, and a guard that needs a
  DNS round trip to answer has already become a place where a slow resolver
  makes the harness hang. It cannot catch a public name that RESOLVES to a
  private address; that is a known limit, stated rather than papered over, and
  the reason the redirect check below exists at all."
  [host]
  (boolean (and host (some #(re-find (re-pattern %) host) private-patterns))))

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

(defn fetch
  "Fetch `url`. Returns {:ok text :status :content-type} or {:error msg}.

  Every failure is a MESSAGE rather than a throw, because this sits behind a
  tool and a model reading `connection refused` learns more than a branch
  dying does."
  [url {:keys [format timeout-ms]}]
  (let [p (policy)
        url (https-upgrade url)]
    (cond
      (not (allowed? url))
      {:error (msg {:refused true :url url :host (host-of url)})}

      :else
      (try
        (let [resp (http/get url
                             {:headers {"Accept" "text/html,text/plain,application/json;q=0.9,*/*;q=0.8"
                                        "User-Agent" (:user-agent p)}
                              :socket-timeout (min (long (or timeout-ms (:timeout-ms p)))
                                                   (long (:max-timeout-ms p)))
                              :throw-exceptions false})
              status (:status resp)
              ;; THE REDIRECT, RE-CHECKED. A public URL that 302s to
              ;; 169.254.169.254 defeats a check made only before the request.
              final (or (get-in resp [:headers "location"]) url)
              body (str (:body resp))
              ctype (str (get-in resp [:headers "content-type"]))]
          (cond
            (not (allowed? final))
            {:error (msg {:refused-redirect true :url url :to final})}

            (not= 200 status)
            {:error (msg {:bad-status true :status status :url url})}

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
