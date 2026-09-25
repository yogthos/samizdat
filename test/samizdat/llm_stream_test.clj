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

(ns samizdat.llm-stream-test
  "A provider reply streamed: the chunks folded back into the completion the
  adapters already parse, and each delta handed on as it arrives."
  (:require [jolt.time]
            [clojure.core.async :as async]
            [clojure.data.json :as json]
            [clojure.test :refer [deftest testing is]]
            [ring-chez.adapter :as adapter]
            [ring-chez.sse :as rsse]
            [samizdat.llm.stream :as stream]))

(defn- chunk [delta & [extra]]
  (merge {:id "c1" :model "m" :choices [{:index 0 :delta delta :finish_reason nil}]} extra))

(def ^:private chunks
  [(chunk {:role "assistant" :reasoning_content "think"})
   (chunk {:reasoning_content "ing"})
   (chunk {:content "Hel"})
   (chunk {:content "lo"})
   (chunk {:tool_calls [{:index 0 :id "call_1" :type "function"
                         :function {:name "read_file" :arguments "{\"pa"}}]})
   (chunk {:tool_calls [{:index 0 :function {:arguments "th\":\"a\"}"}}]})
   (merge (chunk {}) {:choices [{:index 0 :delta {} :finish_reason "tool_calls"}]})
   {:id "c1" :model "m" :choices [] :usage {:prompt_tokens 10 :completion_tokens 3 :total_tokens 13}}])

(deftest chunks-fold-into-the-completion
  (let [c (stream/completion (reduce stream/accumulate nil chunks))
        m (get-in c [:choices 0 :message])]
    (is (= "Hello" (:content m)))
    (is (= "thinking" (:reasoning_content m)))
    (is (= "assistant" (:role m)))
    (is (= [{:id "call_1" :type "function"
             :function {:name "read_file" :arguments "{\"path\":\"a\"}"}}]
           (:tool_calls m)))
    (is (= "tool_calls" (get-in c [:choices 0 :finish_reason])))
    (is (= 13 (get-in c [:usage :total_tokens])))
    (is (= "m" (:model c)))))

(deftest a-delta-is-the-text-and-the-reasoning-it-adds
  (is (= {:text "Hel"} (stream/delta (chunk {:content "Hel"}))))
  (is (= {:reasoning "think"} (stream/delta (chunk {:reasoning_content "think"}))))
  (is (= {:reasoning "r"} (stream/delta (chunk {:reasoning "r"}))))
  (is (nil? (stream/delta (chunk {:tool_calls [{:index 0}]})))))

(defn- free-port [] (with-open [s (java.net.ServerSocket. 0)] (.getLocalPort s)))

(defn- with-server [handler f]
  (let [port (free-port)
        server (adapter/run-server handler {:port port})]
    (try (f port) (finally (adapter/stop-server server)))))

(deftest a-streamed-post-hands-on-each-delta-and-returns-the-whole
  (let [seen (atom [])
        handler (fn [_]
                  (let [ch (async/chan 64)]
                    (future
                      (doseq [c chunks]
                        (rsse/send! ch {:data (json/write-str c)})
                        (Thread/sleep 5))
                      (rsse/send! ch {:data "[DONE]"})
                      (async/close! ch))
                    {:status 200 :headers {"Content-Type" "text/event-stream"} :body ch}))]
    (with-server handler
      (fn [port]
        (let [r (stream/post (str "http://127.0.0.1:" port "/v1/chat/completions")
                             {:headers {"Content-Type" "application/json"} :body "{}"
                              :socket-timeout 5000 :conn-timeout 2000}
                             #(swap! seen conj %))]
          (is (= 200 (:status r)))
          (is (= "Hello" (get-in (json/read-str (:body r) :key-fn keyword)
                                 [:choices 0 :message :content])))
          (is (= [{:reasoning "think"} {:reasoning "ing"} {:text "Hel"} {:text "lo"}] @seen)
              "each delta, in order, as it came"))))))

(deftest an-error-status-comes-back-with-its-body
  (with-server (fn [_] {:status 400 :headers {"Content-Type" "application/json"}
                        :body "{\"error\":{\"message\":\"bad request\"}}"})
    (fn [port]
      (let [r (stream/post (str "http://127.0.0.1:" port "/x")
                           {:headers {} :body "{}" :socket-timeout 5000 :conn-timeout 2000}
                           (fn [_] (throw (ex-info "no deltas on an error" {}))))]
        (is (= 400 (:status r)))
        (is (= "{\"error\":{\"message\":\"bad request\"}}" (:body r)))))))

(deftest an-error-frame-mid-stream-is-the-reply
  ;; A stream can end in `data: {"error": {...}}` — GLM sends its business
  ;; codes that way. The frame has no choices, so it folded into nothing and
  ;; the call read as an empty reply, misreported and never classified
  ;; (karamazov-jvdu). The error is what comes back.
  (let [acc (reduce stream/accumulate nil
                    [(chunk {:content "Hel"})
                     {:error {:code "1302" :message "请求过快"}}])]
    (is (= {:error {:code "1302" :message "请求过快"}}
           (select-keys (stream/completion acc) [:error])))))
