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
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.store.messages
  "Agent mailbox: durable messages between branches working one feature.

  A branch leaves another a note ('I'm taking the parser, you take the
  tests', 'the DB schema changed') and it surfaces in that branch's context
  until the inbox tool reads it. Same shape as knowledge: short ids, plain
  rows, the store owns nothing but SQL. to_branch NULL means broadcast to
  the whole run. read_at NULL marks a message unread — surfacing it in the
  context block does not consume it; the inbox tool stamps read_at."
  (:require [clojure.string :as str]
            [samizdat.store.db :as db]))

(defn- new-id
  "Six hex chars, same scheme as tasks and knowledge."
  []
  (str "msg-" (subs (str/replace (str (random-uuid)) "-" "") 0 6)))

(defn send!
  "Insert a message and return its id. :to nil or absent broadcasts to the
  whole run; every branch but the sender sees it in their inbox."
  [conn {:keys [run-id from to body flow]}]
  (when (str/blank? (str body))
    (throw (ex-info "a message needs a body" {})))
  (when (str/blank? (str run-id))
    (throw (ex-info "a message needs a run-id" {})))
  (when (str/blank? (str from))
    (throw (ex-info "a message needs a from branch" {})))
  (let [now (db/now)]
    (loop [attempt 1]
      (let [id (new-id)
            n (try
                (db/with-writer
                  (db/execute! conn
                               ["INSERT INTO messages
                                 (id, run_id, from_branch, to_branch, body, created_at, flow)
                                 VALUES (?, ?, ?, ?, ?, ?, ?)"
                                ;; `flow`: what the sender had read (v38).
                                id (str run-id) (str from) to (str body) now flow]))
                1
                (catch Exception e
                  ;; Only a UNIQUE collision is an id problem; anything else
                  ;; (disk, lock) must surface as itself (provenance R2-15).
                  (if (db/id-collision? e) 0 (throw e))))]
        (if (pos? n)
          id
          (if (< attempt 5)
            (recur (inc attempt))
            (throw (ex-info "could not allocate a message id" {}))))))))

(defn inbox
  "Unread messages visible to this branch: addressed to it or broadcast,
  excluding its own. Newest first. Reading the context block does not
  consume them — mark-read! does."
  [conn run-id branch]
  (db/fetch conn
            ["SELECT * FROM messages
              WHERE run_id = ?
                AND read_at IS NULL
                AND from_branch != ?
                AND (to_branch = ? OR to_branch IS NULL)
              ORDER BY created_at DESC, rowid DESC"
             (str run-id) (str branch) (str branch)]))

(defn mark-read!
  "Stamp read_at on the given message ids, only where still unread.
  Returns the row count updated — how the inbox tool reports consumption."
  [conn ids]
  (let [ids (vec (remove str/blank? (map str ids)))]
    (if (empty? ids)
      0
      (db/with-writer
        (db/execute! conn
                     (into [(str "UPDATE messages SET read_at = ? WHERE read_at IS NULL AND id IN ("
                                 (str/join "," (repeat (count ids) "?")) ")")
                            (db/now)]
                           ids))))))

(defn thread
  "Recent messages for the run regardless of read state — the whole
  exchange, for orienting a fresh branch or reviewing after the fact."
  ([conn run-id] (thread conn run-id 20))
  ([conn run-id n]
   (db/fetch conn
             ["SELECT * FROM messages WHERE run_id = ?
               ORDER BY created_at DESC, rowid DESC LIMIT ?"
              (str run-id) (long n)])))

(def ^:private inbox-preamble
  "Unread messages for this branch (read them with the message tool's inbox
action to consume them):")

(defn- inbox-line
  "from and a ~60-char preview of the body, one line — enough to decide
  whether to open the inbox, nothing more. Broadcast messages show ALL
  rather than a branch id. A message carrying a flow label shows `withheld`
  in place of its body: the preview is put in front of a branch unasked, and
  its text is only for a branch that opens the inbox and takes the label."
  [withheld {:keys [from_branch body to_branch flow]}]
  (str from_branch
       (when (nil? to_branch) " (broadcast)")
       ": "
       (if (and flow withheld)
         withheld
         (let [flat (str/trim (str/replace (str body) #"\s+" " "))]
           (if (> (count flat) 60)
             (str (subs flat 0 60) "...")
             flat)))))

(defn render-inbox
  "The bounded inbox preview for the context block: a preamble plus one
  line per unread message, capped at 3 lines, or nil when this branch has
  no unread mail — the context block's keep identity drops it."
  ;; The cap is gates.edn :context-budget :inbox-lines — the caller supplies it
  ;; rather than this namespace reading policy, because the store owns nothing
  ;; but SQL. The literal remains as the floor for a caller with no policy in
  ;; reach (a test, a REPL).
  ([conn run-id branch] (render-inbox conn run-id branch 3))
  ([conn run-id branch cap] (render-inbox conn run-id branch cap "…"))
  ([conn run-id branch cap withheld]
   (let [rows (inbox conn run-id branch)]
     (when (seq rows)
        (str/join "\n"
                  (cons inbox-preamble
                        (map (partial inbox-line withheld) (take cap rows))))))))