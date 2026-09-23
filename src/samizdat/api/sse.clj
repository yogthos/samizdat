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

(ns samizdat.api.sse
  "Following a server-sent event stream: the client end of GET
  /v1/runs/:id/events, for a front end that wants a run pushed to it.

  Toolkit-free, like samizdat.api.client, so the suite covers it with no
  terminal. The http client reads a response whole, and a stream never ends,
  so this speaks HTTP/1.1 over its socket directly: the response head, the
  chunked body, and the event lines inside it.

  BYTES, NOT CHARACTERS, until a line is whole. A chunk's size is octets and a
  multibyte character can be cut across two chunks; a line ends at a newline
  byte, which never occurs inside a UTF-8 sequence, so a line is decoded once
  and whole."
  (:require ;; the java.time.* host shim, before data.json — see samizdat.store.journal
            [jolt.time]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [jolt.http.net :as net]))

;; --- the parser --------------------------------------------------------------

(defn reader
  "A fresh parser: feed it the bytes of one connection with `feed`."
  []
  {:phase :head :buf [] :status nil :chunked? false
   :need nil :skip 0 :line [] :event {} :done? false})

(defn- utf8 [bs] (String. (byte-array bs) "UTF-8"))

(defn- head-end
  "Index just past the blank line ending the head in `buf`, or nil."
  [buf]
  (let [n (count buf)]
    (loop [i 3]
      (cond (>= i n) nil
            (and (= 13 (nth buf (- i 3))) (= 10 (nth buf (- i 2)))
                 (= 13 (nth buf (- i 1))) (= 10 (nth buf i))) (inc i)
            :else (recur (inc i))))))

(defn- dispatch
  "The event a blank line completes, or nil when it carried no data."
  [{:keys [data] :as ev}]
  (when (seq data)
    (cond-> {:data (str/join "\n" data)}
      (:id ev) (assoc :id (:id ev))
      (:event ev) (assoc :event (:event ev)))))

(defn- on-line
  "One whole event-stream line into state `st`, collecting into `out`."
  [st out line]
  (let [line (if (str/ends-with? line "\r") (subs line 0 (dec (count line))) line)]
    (cond
      (= "" line)
      [(assoc st :event {}) (if-let [e (dispatch (:event st))] (conj out e) out)]

      (str/starts-with? line ":") [st out]

      :else
      (let [i (str/index-of line ":")
            field (if i (subs line 0 i) line)
            v (if i (subs line (inc i)) "")
            v (if (str/starts-with? v " ") (subs v 1) v)]
        [(case field
           "data" (update-in st [:event :data] (fnil conj []) v)
           "id" (assoc-in st [:event :id] v)
           "event" (assoc-in st [:event :event] v)
           st)
         out]))))

(defn- payload
  "Body bytes (already de-chunked) into event lines."
  [st out bs]
  (reduce (fn [[st out] b]
            (if (= 10 b)
              (on-line (assoc st :line []) out (utf8 (:line st)))
              [(update st :line conj b) out]))
          [st out] bs))

(defn- chunked
  "De-chunk `bs` into the event parser."
  [st out bs]
  (loop [st st, out out, bs (seq bs)]
    (cond
      (or (nil? bs) (:done? st)) [st out]

      (pos? (:skip st)) (recur (update st :skip dec) out (next bs))

      (nil? (:need st))
      (let [b (first bs)]
        (if (= 10 b)
          (let [size-line (str/trim (first (str/split (utf8 (:buf st)) #";")))
                n (Long/parseLong (if (str/blank? size-line) "0" size-line) 16)]
            (if (zero? n)
              [(assoc st :done? true :buf []) out]
              (recur (assoc st :need n :buf []) out (next bs))))
          (recur (update st :buf conj b) out (next bs))))

      :else
      (let [k (min (:need st) (count bs))
            [st out] (payload st out (take k bs))
            left (- (:need st) k)]
        (recur (if (zero? left) (assoc st :need nil :skip 2) (assoc st :need left))
               out (seq (drop k bs)))))))

(defn- body [st out bs]
  (if (:chunked? st) (chunked st out bs) (payload st out bs)))

(defn feed
  "Feed `bytes` to parser state `st`. Returns {:state :events}: the events
  those bytes completed, each {:data} with :id and :event when the stream
  named them. `:status` on the state once the head is in; `:done?` when the
  stream ended or was refused (a status other than 200)."
  [st bytes]
  (let [bs (vec bytes)]
    (if (= :head (:phase st))
      (let [buf (into (:buf st) bs)]
        (if-let [end (head-end buf)]
          (let [head (utf8 (subvec buf 0 end))
                status (some-> (re-find #"^HTTP/\d\.\d (\d{3})" head) second parse-long)
                chunked? (boolean (re-find #"(?i)\r\ntransfer-encoding:\s*chunked" head))
                st (assoc st :phase :body :buf [] :status status :chunked? chunked?)]
            (if (= 200 status)
              (let [[st out] (body st [] (subvec buf end))]
                {:state st :events out})
              {:state (assoc st :done? true) :events []}))
          {:state (assoc st :buf buf) :events []}))
      (let [[st out] (body st [] bs)]
        {:state st :events out}))))

(defn parse-data
  "An event's data as {:data value}, JSON read with keyword keys; the raw
  text when it is not JSON."
  [text]
  {:data (try (json/read-str (str text) :key-fn keyword)
              (catch Throwable _ text))})

;; --- following a stream --------------------------------------------------------

(defn- parse-url [url]
  (let [[_ host port path] (re-find #"^https?://([^:/]+)(?::(\d+))?(/.*)?$" (str url))]
    {:host host :port (or (some-> port parse-long) 80) :path (or path "/")}))

(defn- request-text [{:keys [host port path]} last-id]
  (str "GET " path " HTTP/1.1\r\n"
       "Host: " host ":" port "\r\n"
       "Accept: text/event-stream\r\n"
       "Cache-Control: no-cache\r\n"
       (when last-id (str "Last-Event-ID: " last-id "\r\n"))
       "\r\n"))

(defn- timeout? [e] (str/includes? (str (ex-message e)) "timed out"))

(defn- one-connection
  "Read one connection until it ends, `stop?` says so, or nothing (not even a
  heartbeat) arrives for `dead-ms`. Returns the last event id seen."
  [target last-id {:keys [on-event on-status stop? slice-ms dead-ms]}]
  (let [fd (net/connect (:host target) (:port target) 3000)]
    (try
      (net/set-read-timeout! fd slice-ms)
      (net/send-bytes fd (.getBytes (request-text target last-id) "UTF-8"))
      (loop [st (reader), last-id last-id, heard (System/currentTimeMillis)]
        (if (stop?)
          last-id
          (let [got (try (net/recv-bytes fd)
                         (catch Throwable e (if (timeout? e) ::quiet (throw e))))]
            (cond
              (nil? got) last-id
              (= ::quiet got) (if (> (- (System/currentTimeMillis) heard) dead-ms)
                                last-id
                                (recur st last-id heard))
              :else
              (let [{:keys [state events]} (feed st got)
                    last-id (or (some :id (reverse events)) last-id)]
                (when (and (:status state) (not (:status st)) on-status)
                  (on-status (:status state)))
                (run! on-event events)
                (if (:done? state)
                  last-id
                  (recur state last-id (System/currentTimeMillis))))))))
      (finally (net/close fd)))))

(defn follow!
  "Follow the event stream at `url` until `stop?` answers true, calling
  `on-event` with each event ({:id :event :data}). Blocks: run it on a thread
  of its own.

  Reconnects when the connection ends or goes silent past `dead-ms`,
  sending the last id seen as Last-Event-ID so the server resumes after it;
  waits `retry-ms` between attempts. `on-status` hears each response's status
  (200 when streaming) and `on-error` each failed attempt."
  [url {:keys [last-event-id on-event on-status on-error stop? retry-ms slice-ms dead-ms]
        :or {retry-ms 1000 slice-ms 250 dead-ms 45000}}]
  (let [target (parse-url url)
        opts {:on-event on-event :on-status on-status :stop? stop?
              :slice-ms slice-ms :dead-ms dead-ms}]
    (loop [last-id last-event-id]
      (when-not (stop?)
        (let [next-id (try (one-connection target last-id opts)
                           (catch Throwable e
                             (when on-error (on-error e))
                             last-id))]
          (when-not (stop?)
            (Thread/sleep retry-ms)
            (recur next-id)))))))
