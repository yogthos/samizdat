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

(ns samizdat.security.flow
  "WHAT A BRANCH HAS READ, and what that lets it do next (karamazov-3vu1.7).

  Each branch carries a label with two parts, after OpenAPPA's:

  - `:trust` — `:trusted` until the branch reads content anyone on the web
    could have written (webfetch, websearch); then `:untrusted`. A page can
    carry instructions, and a branch that has read one runs nothing but a
    read-only command without a person saying so.
  - `:audience` — `:project` until the branch reads something from outside
    the project a person or yolo let it read; then `:operator`, data only the
    operator should see. A branch holding it sends nothing out without a
    person saying so.

  Labels only narrow: nothing a branch reads later raises one back. A sibling
  branch keeps its own.

  THE TABLE IS IN src/ AND NOT IN gates.edn, for the reason
  `policy/protected-paths` gives: it confines the agent, and a table the
  agent can edit is not one.

  The label lives in memory per run and branch, and every change is a `:flow`
  journal note carrying the branch and what lowered it, so a resumed process
  folds it back from the journal. `forget-run!` drops a run's labels when it
  ends, with its other per-run state.

  A sink that finds a gap does not decide what happens: the shell turns its
  allow into an ask, which the approval policy settles like any other. The
  gaps and the ways forward (`remedies`, karamazov-3vu1.8) ride out as data
  for the refusal text and the person's dialog."
  (:refer-clojure :exclude [reset!])
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [jolt.fs :as fs]
            [samizdat.events :as events]
            [samizdat.store.db :as db]))

(def trust-chain
  "Least trusted first."
  [:untrusted :trusted])

(def audience-chain
  "Narrowest first: who may see what the branch holds."
  [:operator :project])

(def top
  "What a branch starts with: read nothing it should not act on or send."
  {:trust :trusted :audience :project})

(defn- rank [chain v]
  (let [i (.indexOf ^java.util.List chain v)]
    ;; A value the chain does not know is the bottom: fail closed.
    (if (neg? i) -1 i)))

(defn- lower [chain a b]
  (if (<= (rank chain a) (rank chain b)) a b))

(defn meet
  "The label of a branch holding both `a` and `b`: the lower trust and the
  narrower audience. `:because` keeps, per part, what lowered it first."
  [a b]
  (let [t (lower trust-chain (:trust a) (:trust b))
        au (lower audience-chain (:audience a) (:audience b))
        why (fn [k v] (or (when (= v (get a k)) (get-in a [:because k]))
                          (get-in b [:because k])))]
    (cond-> {:trust t :audience au}
      (not= t (:trust top)) (assoc-in [:because :trust] (why :trust t))
      (not= au (:audience top)) (assoc-in [:because :audience] (why :audience au)))))

(def table
  "Which tools lower a label (`:sources`) and what a sink needs
  (`:sinks`). `:read-only-heads` are the shell commands that need no trust:
  they print what is there and run nothing else. A `git` head is read-only
  only with one of `:read-only-git`."
  {:sources {"webfetch" {:trust :untrusted}
             "websearch" {:trust :untrusted}}
   :outside-read {:audience :operator}
   ;; The shell: a read-only command needs nothing; one the operator's
   ;; config names needs no trust (`gaps`).
   :sinks {"shell" {:trust :trusted :audience :project :read-only-exempt true}
           "webfetch" {:audience :project}
           "websearch" {:audience :project}
           ;; Eval, by where it runs (tools/run-tool names which): the
           ;; harness's own process is everything the harness can do; a
           ;; project image with network can send what the branch holds. One
           ;; under seatbelt can do neither and is not a sink.
           "eval/harness" {:trust :trusted :audience :project}
           "eval/networked" {:audience :project}
           ;; The workflow is code every later turn and run executes, so
           ;; changing it needs trust (karamazov-3vu1.14). Reading it does not.
           "cell" {:trust :trusted :read-actions #{"list" "show" "versions"}}
           "manifest" {:trust :trusted :read-actions #{"list" "show" "versions" "diff" "refs"}}
           "prompt" {:trust :trusted :read-actions #{"list" "show" "versions"}}
           "policy" {:trust :trusted :read-actions #{"list" "show" "versions"}}
           "adopt" {:trust :trusted :read-actions #{"list" "show"}}}
   ;; Not sort (-o), uniq (an output file), tree (-o) or rg (--pre runs a
   ;; command): each can write or run something.
   :read-only-heads #{"ls" "cat" "head" "tail" "wc" "grep" "find" "pwd" "file"
                      "stat" "diff" "nl" "cut" "du" "which" "basename" "dirname"
                      "realpath" "git"}
   ;; `find` runs commands with these, and deletes with the last.
   :read-only-refused-args #{"-exec" "-execdir" "-ok" "-okdir" "-delete" "-fprint"
                             "-fprintf" "-fls"}
   ;; Not branch, tag or remote: each also creates and deletes.
   :read-only-git #{"status" "log" "diff" "show" "blame" "ls-files" "rev-parse"}})

(defn delta
  "The label a call to `tool` brings in, or nil when it brings in nothing."
  [table tool _args]
  (get-in table [:sources (str tool)]))

(defn gaps
  "What `label` lacks for a call to `tool`, as [{:gap :required :actual
  :because} …] — empty when the call may run. `call` is the call's
  arguments; for the shell, `:read-only?` says the command only reads, and
  `:configured?` that it is one the operator's config names (the verify
  command, an acceptance check), which clears a trust gap but not an
  audience one: the operator wrote it, no page did, but it can still send
  what the branch holds anywhere."
  [table tool call label]
  (let [need (get-in table [:sinks (str tool)])
        need (cond-> need (:configured? call) (dissoc :trust))]
    (if (or (nil? need) (and (:read-only-exempt need) (:read-only? call))
            (contains? (:read-actions need)
                       (some-> (:action call) str str/trim str/lower-case)))
      []
      (vec (concat
            (when (and (:trust need)
                       (< (rank trust-chain (:trust label)) (rank trust-chain (:trust need))))
              [{:gap :trust :required (:trust need) :actual (:trust label)
                :because (get-in label [:because :trust])}])
            (when (and (:audience need)
                       (< (rank audience-chain (:audience label))
                          (rank audience-chain (:audience need))))
              [{:gap :audience :required (:audience need) :actual (:audience label)
                :because (get-in label [:because :audience])}]))))))

(defn remedies
  "The ways forward from `gaps` on a call to `tool`, as plans:
  `{:plan :authorize :authority :person}` — a person allows this one call;
  `{:plan :narrow :to :read-only}` — a read-only command runs anyway. Empty
  when there is no gap. Nothing clears an audience gap but a person."
  [tool gaps]
  (when (seq gaps)
    (cond-> [{:plan :authorize :authority :person :covers (mapv :gap gaps)}]
      (and (= "shell" (str tool)) (every? #(= :trust (:gap %)) gaps))
      (conj {:plan :narrow :to :read-only} {:plan :narrow :to :configured}))))

(defn lower-by
  "`label` lowered by the delta `d`, brought in by `why` ({:turn :tool})."
  [label d why]
  (meet label (merge top d {:because (into {} (map (fn [k] [k why])) (keys d))})))

;; --- the branches' labels ----------------------------------------------------

;; The :flow notes go through the events table directly rather than
;; samizdat.store.journal, which requires samizdat.security.policy (for the
;; paths a shell command read) — and policy is this namespace's caller. The
;; rows are the same ones `journal/notes` reads.

(defn- note! [conn run-id bid turn data]
  (let [now (db/now)]
    (db/with-writer
      (db/execute! conn ["INSERT INTO events (run_id, branch_id, turn, kind, data, created_at)
                          VALUES (?, ?, ?, 'flow', ?, ?)"
                         run-id bid turn (json/write-str data) now])
      (events/publish! {:id (db/last-insert-id conn) :run-id run-id :branch-id bid
                        :turn turn :kind :flow :data data :created-at now}))))

(defn- notes [conn run-id]
  (into []
        (keep (fn [row]
                (try (json/read-str (str (:data row)) :key-fn keyword)
                     (catch Throwable _ nil))))
        (db/fetch conn ["SELECT data FROM events WHERE run_id = ? AND kind = 'flow' ORDER BY id"
                        run-id])))

(defonce ^:private labels
  ;; [run-id branch-id] -> label
  (atom {}))

(defn reset! [] (clojure.core/reset! labels {}))

(defn cached [] @labels)

(defn- branch-id [ctx]
  (or (get-in ctx [:branch :id]) (:branch-id ctx)))

(defn- note-label
  "A note's label, read back: it carries only the parts it lowered."
  [label]
  (merge top (cond-> label
               (:trust label) (update :trust keyword)
               (:audience label) (update :audience keyword))))

(defn- from-journal
  "The label folded from a run's `:flow` notes for one branch."
  [conn run-id bid]
  (reduce (fn [l {:keys [branch label]}]
            (if (= (str bid) (str branch)) (meet l (note-label label)) l))
          top
          (when conn (notes conn run-id))))

(defn label-of
  "The flow label of the branch `ctx` names — from memory, or folded back
  from the journal the first time this process asks."
  [{:keys [conn run-id] :as ctx}]
  (let [k [run-id (branch-id ctx)]]
    (or (get @labels k)
        (let [l (try (from-journal conn run-id (branch-id ctx))
                     ;; A journal that cannot be read is not a clean label.
                     (catch Throwable _ (meet top {:trust :untrusted :audience :operator})))]
          (swap! labels assoc k l)
          l))))

(declare observe!*)

(defn observe!
  "Lower the branch `ctx` names by `d` (a `delta`), brought in by `tool`.
  Journals the change; a delta that changes nothing writes nothing."
  [{:keys [turn] :as ctx} d tool]
  (observe!* ctx d {:turn turn :tool (str tool)}))

(defn observe!*
  "`observe!` with what lowered it given whole, as {:turn :tool :via}."
  [{:keys [conn run-id] :as ctx} d why]
  (when (and d run-id (branch-id ctx))
    (let [was (label-of ctx)
          why (into {} (remove (comp nil? val)) why)
          now (lower-by was d why)]
      (when (not= (dissoc was :because) (dissoc now :because))
        (swap! labels assoc [run-id (branch-id ctx)] now)
        (when conn
          (note! conn run-id (branch-id ctx) (:turn ctx)
                 {:branch (branch-id ctx)
                  :label (assoc d :because (into {} (map (fn [k] [k why])) (keys d)))})))
      now)))

;; --- what a branch writes for others (karamazov-3vu1.13) ----------------------
;;
;; A message, a task, a memory carries its writer's label in its row's `flow`
;; column: JSON, nil for a writer that had read nothing. A branch that reads
;; one on purpose — the inbox, a task it claims or shows, a recalled memory,
;; another branch's turn — takes that label; a fork takes its parent's.

(defn- parts
  "The parts of `label` below the top, as a delta, or nil."
  [label]
  (not-empty (into {} (filter (fn [[k v]] (not= v (get top k))))
                   (select-keys label [:trust :audience]))))

(defn encode
  "`label` as the text a row carries, or nil when it lowers nothing."
  [label]
  (when (parts label)
    (json/write-str (assoc (parts label) :because (:because label)))))

(defn decode
  "The label a row's `flow` text carries, or nil."
  [s]
  (when-not (str/blank? (str s))
    (try (let [m (json/read-str (str s) :key-fn keyword)]
           (cond-> (merge top m)
             (:trust m) (update :trust keyword)
             (:audience m) (update :audience keyword)))
         ;; A label that cannot be read is not a clean one.
         (catch Throwable _ {:trust :untrusted :audience :operator}))))

(defn carried
  "What a row written by the branch `ctx` names carries."
  [ctx]
  (when (and (:run-id ctx) (branch-id ctx))
    (encode (label-of ctx))))

(defn run-label
  "The meet of every branch's label in `run-id`: what a row the run writes
  with no branch to name carries."
  [conn run-id]
  (when (and conn run-id)
    (let [noted (reduce (fn [l {:keys [label]}] (meet l (note-label label)))
                        top
                        (try (notes conn run-id) (catch Throwable _ [])))]
      (reduce (fn [l [[r _] v]] (if (= r run-id) (meet l v) l)) noted @labels))))

(defn carried-by-run [conn run-id] (some-> (run-label conn run-id) encode))

(defn carried-by-branch
  "What a row the branch `branch-id` of `run-id` writes carries."
  [conn run-id branch-id]
  (when (and run-id branch-id)
    (encode (label-of {:conn conn :run-id run-id :branch {:id branch-id}}))))

(defn unlabelled
  "`rows` without the ones carrying a label: what is put in front of a branch
  unasked."
  [rows]
  (remove #(not-empty (str (:flow %))) rows))

(declare combine receive!)

;; --- files ---------------------------------------------------------------------
;;
;; The branches share one tree, and it outlives the run. A file written by a
;; labelled branch is labelled, by canonical path, in file_flow; a branch that
;; reads it takes the label. A clean branch's WHOLE rewrite clears it — what
;; it wrote, it had read nothing to lower; a clean partial edit leaves it.

(defn- canonical [root path]
  (try (str (fs/canonicalize (if (.isAbsolute (java.io.File. (str path)))
                               (str path)
                               (str (or root ".") "/" path))))
       (catch Throwable _ nil)))

(defn wrote!
  "The branch `ctx` names wrote `path`; `whole?` when it replaced the file."
  [{:keys [conn root run-id] :as ctx} path whole?]
  (when-let [p (and conn (canonical root path))]
    (let [mine (carried ctx)
          had (:flow (first (db/fetch conn ["SELECT flow FROM file_flow WHERE path = ?" p])))
          now (if whole? mine (combine had mine))]
      (db/with-writer
        (if now
          (db/execute! conn ["INSERT INTO file_flow (path, flow, run_id, updated_at) VALUES (?, ?, ?, ?)
                              ON CONFLICT(path) DO UPDATE SET flow = excluded.flow,
                                run_id = excluded.run_id, updated_at = excluded.updated_at"
                             p now run-id (db/now)])
          (db/execute! conn ["DELETE FROM file_flow WHERE path = ?" p]))))))

(defn read!
  "The branch `ctx` names read `paths`: it takes their labels, through `via`."
  [{:keys [conn root] :as ctx} paths via]
  (when conn
    (let [ps (keep #(canonical root %) paths)]
      (when (seq ps)
        (receive! ctx
                  (map :flow (db/fetch conn (into [(str "SELECT flow FROM file_flow WHERE path IN ("
                                                        (str/join "," (repeat (count ps) "?")) ")")]
                                                  ps)))
                  via)))))

(defn labelled-paths
  "Every labelled file's canonical path."
  [conn]
  (when conn (mapv :path (db/fetch conn ["SELECT path FROM file_flow"]))))

(defn combine
  "The text a row carries after a writer carrying `b` edits one carrying `a`."
  [a b]
  (encode (meet (or (decode a) top) (or (decode b) top))))

(defn receive!
  "The branch `ctx` names reads rows carrying `flows` (their `flow` texts)
  through `via`: it takes each one's label."
  [ctx flows via]
  (doseq [l (keep decode flows)
          :let [d (parts l)]
          :when d]
    ;; Each part keeps what lowered it first, and says how it arrived.
    (doseq [[k v] d]
      (let [orig (get-in l [:because k])]
        (observe!* ctx {k v} {:turn (:turn ctx) :tool (or (:tool orig) via) :via via})))))

(defn inherit!
  "A fork `child` of `parent` in the run `ctx` names starts with its
  parent's label: it starts from its parent's conversation, or its thesis."
  [ctx parent child]
  (let [l (label-of (assoc ctx :branch {:id parent}))]
    (receive! (assoc ctx :branch {:id child}) [(encode l)] "fork")))

(defn forget-run!
  "Drop a run's labels: the run is over."
  [run-id]
  (swap! labels (fn [m] (into {} (remove (fn [[[r _] _]] (= r run-id))) m))))
