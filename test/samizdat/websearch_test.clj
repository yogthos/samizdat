;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.websearch-test
  "The web search tool, offline.

  Every part that can be tested without the network is a pure function over a
  response body, because a tool whose only test needs the internet is a tool
  nobody runs the tests for."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [samizdat.agent.websearch :as ws]))

(deftest the-request-envelope-is-the-mcp-tools-call
  (let [e (ws/envelope "raylib BeginMode3D" 5)]
    (is (= "2.0" (get e "jsonrpc")))
    (is (= "tools/call" (get e "method")))
    (is (= "web_search_exa" (get-in e ["params" "name"])))
    (is (= "raylib BeginMode3D" (get-in e ["params" "arguments" "query"])))
    (is (= 5 (get-in e ["params" "arguments" "numResults"]))))
  (testing "results are capped — a search that returns ten pages costs more
            context than the reading it replaces"
    (is (= 20 (get-in (ws/envelope "q" 500) ["params" "arguments" "numResults"])))
    (is (= 1 (get-in (ws/envelope "q" 0) ["params" "arguments" "numResults"])))))

(deftest a-plain-json-response-is-read
  (is (= "hello world"
         (ws/extract-text
          "{\"result\":{\"content\":[{\"type\":\"text\",\"text\":\"hello world\"}]}}"))))

(deftest an-sse-response-is-read
  ;; The endpoint answers text/event-stream as readily as JSON, and a tool that
  ;; handles only one of them fails intermittently for no visible reason.
  (is (= "from sse"
         (ws/extract-text
          (str "event: message\n"
               "data: {\"result\":{\"content\":[{\"type\":\"text\",\"text\":\"from sse\"}]}}\n\n")))))

(deftest a-response-with-nothing-usable-is-nil-not-a-crash
  (doseq [body ["" "   " "not json at all" "{}" "{\"result\":{}}"
                "{\"result\":{\"content\":[]}}"
                "{\"error\":{\"message\":\"rate limited\"}}"]]
    (is (nil? (ws/extract-text body)) (str "should be nil: " (pr-str body)))))

(deftest results-are-truncated-per-entry-and-in-total
  (let [long-text (apply str (repeat 5000 "x"))]
    (testing "one huge result is clipped"
      (let [out (ws/format-results long-text {:chars 200})]
        (is (<= (count out) 260) "clipped, with room for the marker")
        (is (str/includes? out "…"))))
    (testing "text within the budget is untouched"
      (is (= "short" (ws/format-results "short" {:chars 200}))))))
