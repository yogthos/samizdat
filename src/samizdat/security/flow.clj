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
   :sinks {"shell" {:trust :trusted :audience :project :read-only-exempt true}
           "webfetch" {:audience :project}
           "websearch" {:audience :project}}
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
  arguments; for the shell, `:read-only?` says the command only reads."
  [table tool call label]
  (let [need (get-in table [:sinks (str tool)])]
    (if (or (nil? need) (and (:read-only-exempt need) (:read-only? call)))
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
      (conj {:plan :narrow :to :read-only}))))

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

(defn- from-journal
  "The label folded from a run's `:flow` notes for one branch."
  [conn run-id bid]
  (reduce (fn [l {:keys [branch label]}]
            (if (= (str bid) (str branch))
              ;; A note carries only the parts it lowered.
              (meet l (merge top (cond-> label
                                   (:trust label) (update :trust keyword)
                                   (:audience label) (update :audience keyword))))
              l))
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

(defn observe!
  "Lower the branch `ctx` names by `d` (a `delta`), brought in by `tool`.
  Journals the change; a delta that changes nothing writes nothing."
  [{:keys [conn run-id turn] :as ctx} d tool]
  (when (and d run-id (branch-id ctx))
    (let [was (label-of ctx)
          why {:turn turn :tool (str tool)}
          now (lower-by was d why)]
      (when (not= (dissoc was :because) (dissoc now :because))
        (swap! labels assoc [run-id (branch-id ctx)] now)
        (when conn
          (note! conn run-id (branch-id ctx) turn
                 {:branch (branch-id ctx)
                  :label (assoc d :because (into {} (map (fn [k] [k why])) (keys d)))})))
      now)))

(defn forget-run!
  "Drop a run's labels: the run is over."
  [run-id]
  (swap! labels (fn [m] (into {} (remove (fn [[[r _] _]] (= r run-id))) m))))
