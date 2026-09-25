;; samizdat - a claim-first verification harness
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

(ns samizdat.llm.stream
  "A chat completion STREAMED from an OpenAI-compatible endpoint: each delta
  handed on as it arrives, and the chunks folded back into the one completion
  body the adapters already parse — so everything after the transport
  (status handling, parsing, retries) is the same code whether a call
  streamed or not.

  jolt.http-client reads a response whole, so this speaks HTTP/1.1 over the
  socket itself, as samizdat.api.sse does for the harness's own event
  stream, and reads the body with that namespace's parser: plain TCP through
  jolt.http.net, TLS through jolt.http.tls."
  (:require ;; the java.time.* host shim, before data.json — see samizdat.store.journal
            [jolt.time]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [jolt.http.net :as net]
            [jolt.http.tls :as tls]
            [samizdat.api.sse :as sse]))

;; --- the chunks --------------------------------------------------------------

(defn- merge-call
  "One streamed tool-call fragment into the call at its index: the id, type
  and name as they first come, the arguments concatenated."
  [call frag]
  (let [f (:function frag)]
    (cond-> (or call {})
      (:id frag) (assoc :id (:id frag))
      (:type frag) (assoc :type (:type frag))
      (:name f) (update-in [:function :name] str (:name f))
      (:arguments f) (update-in [:function :arguments] str (:arguments f)))))

(defn accumulate
  "Fold one chunk into `acc`. Every string a delta carries is appended to the
  same field of the message — content, and whichever field the endpoint puts
  its reasoning in — so no provider's field needs naming here."
  [acc chunk]
  (let [{:keys [delta finish_reason]} (first (:choices chunk))]
    (cond-> (merge acc (select-keys chunk [:id :model :created]))
      (:usage chunk) (assoc :usage (:usage chunk))
      ;; An error frame mid-stream — GLM sends its business codes this way.
      ;; It has no choices, so it folded into nothing and the call read as an
      ;; empty reply (karamazov-jvdu); kept, it comes back as the reply.
      (:error chunk) (assoc :error (:error chunk))
      finish_reason (assoc :finish-reason finish_reason)
      delta
      (update :message
              (fn [m]
                (reduce-kv (fn [m k v]
                             (cond
                               (= :tool_calls k)
                               (reduce #(update-in %1 [:calls (:index %2 0)] merge-call %2) m v)
                               (string? v) (update m k str v)
                               (some? v) (assoc m k v)
                               :else m))
                           (or m {}) delta))))))

(defn completion
  "`acc` as the completion a non-streamed call returns."
  [{:keys [message usage finish-reason] :as acc}]
  (let [calls (:calls message)]
    (cond-> {:id (:id acc) :object "chat.completion" :model (:model acc)
             :choices [{:index 0
                        :message (cond-> (-> (dissoc message :calls)
                                             (update :role #(or % "assistant")))
                                   (seq calls) (assoc :tool_calls (mapv val (sort-by key calls))))
                        :finish_reason finish-reason}]}
      usage (assoc :usage usage)
      (:error acc) (assoc :error (:error acc)))))

(defn delta
  "What a chunk adds that a reader sees: {:text} and/or {:reasoning}, or nil."
  [chunk]
  (let [d (:delta (first (:choices chunk)))
        text (:content d)
        reasoning (or (:reasoning_content d) (:reasoning d))]
    (not-empty (cond-> {}
                 (not-empty (str text)) (assoc :text text)
                 (not-empty (str reasoning)) (assoc :reasoning reasoning)))))

;; --- the socket --------------------------------------------------------------

(defn- open
  "A connection as {:send! :recv! :close!}. `recv!` answers a chunk of bytes,
  nil at end of stream, or throws a read timeout."
  [{:keys [tls? host port]} read-ms conn-ms]
  (if tls?
    (let [st (tls/tls-connect host port false read-ms conn-ms)]
      {:send! #((jolt.host/ref-get st :write) st %)
       :recv! #((jolt.host/ref-get st :read) st nil)
       :close! #((jolt.host/ref-get st :close))})
    (let [fd (net/connect host port conn-ms)]
      (net/set-read-timeout! fd read-ms)
      {:send! #(net/send-bytes fd %)
       :recv! #(net/recv-bytes fd)
       :close! #(net/close fd)})))

(defn- request-bytes [{:keys [host port path]} headers ^String body]
  (let [b (.getBytes body "UTF-8")]
    (byte-array
     (concat
      (.getBytes
       (str "POST " path " HTTP/1.1\r\n"
            "Host: " host ":" port "\r\n"
            (apply str (for [[k v] headers] (str k ": " v "\r\n")))
            "Accept: text/event-stream\r\n"
            "Content-Length: " (alength b) "\r\n"
            "Connection: close\r\n\r\n")
        "UTF-8")
      b))))

(defn post
  "POST `body` to `url` asking for a stream, calling `on-delta` with each
  delta (see `delta`) as it arrives. Returns {:status :headers :body} like
  jolt.http-client's post: the body is the folded completion as JSON, or
  whatever the endpoint sent instead — an error, or a whole reply to a
  request it did not stream.

  `:socket-timeout` bounds each read; `:max-response-ms` the whole response,
  as jolt.http.platform's cap does for an unstreamed call. `on-delta`
  throwing does not fail the call: a watcher's trouble is not the model's."
  [url {:keys [headers body socket-timeout conn-timeout max-response-ms]} on-delta]
  (let [t (sse/parse-url url)
        started (System/currentTimeMillis)
        deadline (when (and max-response-ms (pos? max-response-ms)) (+ started max-response-ms))
        conn (open t socket-timeout conn-timeout)
        hand-on (fn [d] (try (on-delta d) (catch Throwable _ nil)))]
    (try
      ((:send! conn) (request-bytes t headers (str body)))
      (loop [st (sse/reader {:keep-body? true}) acc nil]
        (when (and deadline (> (System/currentTimeMillis) deadline))
          (throw (jolt.host/throwable "java.net.SocketTimeoutException"
                                      (str "Response exceeded the total time limit of "
                                           max-response-ms "ms"))))
        (let [got ((:recv! conn))
              {:keys [state events]} (if got (sse/feed st got) {:state st :events []})
              [acc done?] (reduce (fn [[acc done?] {:keys [data]}]
                                    (if (= "[DONE]" (str/trim (str data)))
                                      [acc true]
                                      (let [c (try (json/read-str (str data) :key-fn keyword)
                                                   (catch Throwable _ nil))]
                                        (if (map? c)
                                          (do (some-> (delta c) hand-on)
                                              [(accumulate acc c) done?])
                                          [acc done?]))))
                                  [acc false] events)]
          (cond
            (and (nil? got) (nil? (:status state)))
            (throw (jolt.host/throwable "java.net.SocketException"
                                        "Connection closed before a response"))

            (or (nil? got) done? (:done? state))
            {:status (:status state)
             :headers (:headers state)
             :body (if (:raw? state)
                     (sse/body-text state)
                     (json/write-str (completion acc)))}

            :else (recur state acc))))
      (finally ((:close! conn))))))
