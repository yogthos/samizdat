;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Ported from dirge, src/agent/tools/websearch.rs. Dirge rotates between two
;; hosted MCP endpoints and falls back to scraping DuckDuckGo HTML; this takes
;; the Exa endpoint alone. The rotation buys rate-limit headroom and the HTML
;; scrape buys a last resort, and both are worth having ONLY once search is
;; carrying real traffic — a fragile HTML parser maintained against a page
;; nobody controls is a liability until then. The seam is here when it is.

(ns samizdat.agent.websearch
  "Search the web, for the times the answer is not in the repo.

  Run c377260b is the case: the branch spent ~250 turns reading a raylib FFI
  binding layer through `shell`, working out an API it had no documentation
  for. Note honestly that a WEB search would not have solved that one — the
  answer was in a local examples repo `read_file` cannot reach
  (karamazov-1an) — so this is the general capability, not that specific fix.

  The endpoint is Exa's hosted MCP, which answers a plain JSON-RPC POST and
  needs no key; `EXA_API_KEY` only raises the rate limit. Everything except
  the POST itself is a pure function over the response body, because a tool
  whose only test needs the internet is a tool nobody runs the tests for."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [jolt.http-client :as http]
            [samizdat.lexicon :as lexicon]
            [samizdat.prompt :as prompt]))

(defn- msg
  "One of search's failure sentences, from prompts/websearch-tool.md. The
  words the model reads are resources like every other seam."
  [ctx]
  (prompt/render "websearch-tool" ctx))

(def ^:private endpoint "https://mcp.exa.ai/mcp")

(defn envelope
  "The JSON-RPC `tools/call` body. Tool name and arguments match dirge's,
  which match opencode's, so the same backend behaves the same way for all
  three."
  [query n]
  (let [{:keys [websearch-results websearch-max-results]}
        (lexicon/policy :context-budget)]
  {"jsonrpc" "2.0"
   "id" 1
   "method" "tools/call"
   "params" {"name" "web_search_exa"
             "arguments" {"query" (str query)
                          "type" "auto"
                          ;; Capped both ends: a search that returns ten pages
                          ;; costs more context than the reading it replaces,
                          ;; which is the failure this tool must not become.
                          "numResults" (max 1 (min websearch-max-results
                                                    (or n websearch-results)))
                          "livecrawl" "fallback"}}}))

(defn- text-of
  "The first `text` in an MCP result's content array, or nil."
  [parsed]
  (when (map? parsed)
    (some->> (get-in parsed ["result" "content"])
             (filter map?)
             (keep #(get % "text"))
             (remove str/blank?)
             first)))

(defn extract-text
  "The result text out of an MCP response body, or nil.

  Two shapes, because the endpoint answers either and a tool that handles one
  fails intermittently for no visible reason: plain JSON, or text/event-stream
  where the payload rides on a `data: ` line. Total over any input — a body
  that is not JSON at all is nil, never a throw."
  [body]
  (let [body (str body)
        try-parse (fn [s] (try (json/read-str s) (catch Throwable _ nil)))
        trimmed (str/trim body)]
    (or (when (str/starts-with? trimmed "{") (text-of (try-parse trimmed)))
        (some (fn [line]
                (when (str/starts-with? line "data: ")
                  (text-of (try-parse (str/trim (subs line 6))))))
              (str/split-lines body)))))

(defn format-results
  "The result text, clipped to the budget with a marker. The clip is the whole
  cost model: an unbounded search result is a context leak dressed as an
  answer."
  [text {:keys [chars]}]
  (let [t (str text)]
    (if (> (count t) chars)
      (str (subs t 0 chars) "\n…(clipped)")
      t)))

(defn search
  "POST the query and return `{:ok text}` or `{:error message}`. The only
  impure function here."
  [query n {:keys [timeout-ms api-key]}]
  (try
    (let [url (if (str/blank? (str api-key))
                endpoint
                (str endpoint "?exaApiKey=" api-key))
          resp (http/post url
                          {:headers {"Content-Type" "application/json"
                                     "Accept" "application/json, text/event-stream"}
                           :body (json/write-str (envelope query n))
                           :socket-timeout (or timeout-ms
                                              (:websearch-timeout-ms
                                               (lexicon/policy :context-budget)))
                           :throw-exceptions false})
          status (:status resp)
          body (str (:body resp))
          ec (:websearch-error-chars (lexicon/policy :context-budget))
          excerpt (subs body 0 (min ec (count body)))]
      (cond
        (not= 200 status)
        {:error (msg {:bad-status true :status status :body excerpt})}

        :else
        (if-let [t (extract-text body)]
          {:ok t}
          ;; Says WHAT was wrong with what came back, not merely that
          ;; something was: an error naming only the absence gives the caller
          ;; nothing to act on.
          {:error (msg {:unreadable true :body excerpt})})))
    (catch Throwable e
      {:error (msg {:request-failed true
                    :detail (or (ex-message e) (str e))})})))
