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

(ns samizdat.api.stream
  "A run's events PUSHED to a front end as server-sent events: GET
  /v1/runs/:id/events, and GET /v1/events for every run.

  THE ROWS COME FROM THE TABLE, THE BUS ONLY SAYS WHEN. Every journal append
  publishes on samizdat.events, but a subscription is a sliding window that
  drops the oldest when a reader falls behind. So a journalled event on the
  bus is taken as a wake-up and the rows are read back from `events` after
  the last id sent — in order, none missed, and a client that reconnects
  with Last-Event-ID resumes exactly where it was. Only what is never
  journalled (a manifest step, a question waiting on a person) is forwarded
  off the bus as it is, with no id to resume from.

  Every event is sent with its kind as the SSE event name and the row as
  JSON data, in the shape GET /v1/runs/:id/journal serves."
  (:require ;; the java.time.* host shim, before data.json — see samizdat.store.journal
            [jolt.time]
            [clojure.core.async :as async]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [clojure.tools.logging :as log]
            [ring-chez.sse :as sse]
            [samizdat.agent.gates :as gates]
            [samizdat.events :as events]
            [samizdat.store.journal :as journal]))

(defn- parse-json [s]
  (if (string? s)
    (try (json/read-str s :key-fn keyword) (catch Throwable _ s))
    s))

(defn- row-event
  "A journal row as an SSE event."
  [row]
  {:id (str (:id row))
   :event (name (keyword (str (:kind row))))
   :data (json/write-str (update row :data parse-json))})

(defn- kw-names
  "Every keyword in `v` as the name a reader expects — `loop/assemble`, not
  `assemble` (what JSON writes) nor `:loop/assemble`."
  [v]
  (walk/postwalk #(if (keyword? %) (subs (str %) 1) %) v))

(defn- bus-event
  "An event that is only ever on the bus, as an SSE event."
  [e]
  {:event (name (:kind e))
   :data (json/write-str (-> (update-vals (dissoc e :kind :run-id :branch-id) kw-names)
                             (assoc :kind (name (:kind e))
                                    :run_id (:run-id e)
                                    :branch_id (:branch-id e))))})

(defn- rows-after [conn run-id cursor page]
  (if run-id
    (journal/events-since conn run-id cursor page)
    (journal/all-events-since conn cursor page)))

(defn pump!
  "Send `run-id`'s events (every run's when nil) after id `since` through
  `emit!` until `stop?` answers true or `emit!` answers false — the client
  has gone. Blocks.

  `emit!` takes an SSE event map, or {:comment text} for a heartbeat."
  [{:keys [conn run-id since emit! stop? poll-ms heartbeat-ms page]}]
  (let [sub (events/subscribe)
        mine? #(or (nil? run-id) (= run-id (:run-id %)))]
    (try
      (loop [cursor (or since 0), quiet-since (System/currentTimeMillis), catch-up? true]
        (when-not (stop?)
          (let [bus (filter mine? (events/collect sub))
                ;; Read the table when this run journalled something, when the
                ;; window may have overflowed, and on first entry.
                read? (or catch-up? (some :id bus)
                          (>= (count bus) events/buffer-size))
                rows (when read? (rows-after conn run-id cursor page))
                live (remove :id bus)
                sent (concat (map row-event rows) (map bus-event live))
                ok? (reduce (fn [_ e] (if (emit! e) true (reduced false))) true sent)
                now (System/currentTimeMillis)
                beat? (and ok? (empty? sent) (> (- now quiet-since) heartbeat-ms))
                ok? (and ok? (or (not beat?) (emit! {:comment "ping"})))]
            (when ok?
              (let [cursor (or (:id (last rows)) cursor)]
                ;; A full page means there is more: go again at once.
                (when-not (= page (count rows)) (Thread/sleep poll-ms))
                (recur cursor
                       (if (or (seq sent) beat?) now quiet-since)
                       (= page (count rows))))))))
      (finally (events/unsubscribe! sub)))))

(defn cursor
  "Where a stream request resumes: its Last-Event-ID header, else `since`
  in the query — an id, or `now` for only what happens from here on — else
  from the start."
  [req]
  (let [since (some->> (:query-string req) (re-find #"(?:^|&)since=([^&]+)") second)]
    (or (some-> (get-in req [:headers "last-event-id"]) str str/trim parse-long)
        (when (= "now" since) :now)
        (some-> since parse-long)
        0)))

(defn response
  "The Ring response that streams `run-id`'s events after `since`. The pump
  runs on a thread of its own and ends with the connection."
  ([conn run-id since] (response conn run-id since (gates/threshold :event-stream)))
  ([conn run-id since {:keys [poll-ms heartbeat-ms page]}]
   (let [ch (async/chan 64)
         since (if (= :now since) (journal/last-event-id conn run-id) since)]
     (future
       (try
         (pump! {:conn conn :run-id run-id :since since
                 :emit! (fn [e]
                          (if-let [c (:comment e)]
                            (async/>!! ch (str ": " c "\r\n\r\n"))
                            (sse/send! ch e)))
                 :stop? (constantly false)
                 :poll-ms poll-ms :heartbeat-ms heartbeat-ms :page page})
         (catch Throwable e
           (log/warn "event stream for" (or run-id "every run") "ended:" (ex-message e)))
         (finally (async/close! ch))))
     (sse/event-response ch))))
