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

(ns samizdat.agent.tools.files
  "The file tools: read_file, write_file, edit_file, grep.

  read/write/edit are thin dispatchers over samizdat.agent.files; grep is
  the project search. See samizdat.agent.tools.base for the shared result
  helpers and the run-tool multimethod."
  (:require [clojure.string :as str]
            [samizdat.agent.files :as files]
            [samizdat.agent.tools.base :as base]
            [samizdat.sdiff.address :as address]
            [samizdat.sdiff.core :as sdiff]
            [samizdat.prompt :as prompt]
            [samizdat.security.exposure :as exposure]
            [samizdat.userspace :as userspace]))

(defn- with-userspace-check
  "A successful write to a file that serves a role of the project's workflow
  — .samizdat/manifests/…, cells/…, prompts/…, a policy table, the role map —
  is checked on the spot, and an edit that does not pass is REJECTED back to
  the branch that wrote it, in the same result: which file, which role, which
  check, where, and what it said (karamazov-1a51.8). The previous version
  keeps running either way; this is what makes sure the writer knows."
  [result {:keys [branch root args]}]
  (if-not (= :success (:category result))
    result
    (let [path (str (:path args))
          abs (files/resolve-under-root (or root ".") path)
          r (when abs (userspace/check-written! abs {:branch-id (:id branch)}))]
      (if-not r
        result
        (let [{:keys [stage message line column]} (:problem r)
              ctx {:path path :kind (name (:kind r)) :role (:name r)
                   :stage (name (or stage :check)) :message message
                   :line line :column column}
              note (try (prompt/render "userspace-rejected" ctx)
                        ;; The project's map may lack the role that words
                        ;; this; the writer still gets the facts.
                        (catch Throwable _ (pr-str ctx)))]
          (base/rejected branch (str (:result result) "\n\n" note)))))))

(defn- form-lines
  "The lines of the form `form` (and the place `at` in it) in the file
  `ctx` reads, as {:offset :limit}, or {:miss text} (karamazov-0e2c.14)."
  [{:keys [args] :as ctx}]
  (let [refs (files/ctx-reference-roots ctx)
        abs (files/resolve-read! ctx refs (str (:path args)))
        src (when abs (try (slurp abs) (catch Throwable _ nil)))]
    (when src
      (let [{:keys [rows error candidates forms places]} (address/locate src (:form args) (:at args))
            place? (and (not (str/blank? (str (:at args)))) (or places (and candidates (not forms))))]
        (if error
          {:miss (prompt/render "form-miss"
                                {:none (= :none error) :ambiguous (= :ambiguous error)
                                 :place place? :form (str (:form args)) :at (str (:at args))
                                 :name (if place? (str (:at args)) (str (:form args)))
                                 :path (str (:path args))
                                 :choices (str/join ", " (map #(str "`" % "`") (or candidates forms places)))})}
          {:offset (dec (first rows)) :limit (inc (- (second rows) (first rows)))})))))

(defmethod base/run-tool "read_file" [{:keys [branch args] :as ctx}]
  ;; {form, at?}: the lines of one form, or one place in it, by its name
  ;; (samizdat.sdiff.address) rather than by line numbers.
  (if (str/blank? (str (:form args)))
    (let [r (files/read-file ctx)]
      ;; A WHOLE read of a Clojure file says which forms changed since this
      ;; branch last read it (karamazov-0e2c.15, sdiff's fingerprint): after
      ;; compaction a re-read is how a branch catches up, and most of the
      ;; file is usually what it already knew.
      (or (when (and (not= :mechanics (:category r)) (:run-id ctx) (:id branch)
                     (nil? (:offset args)) (nil? (:limit args))
                     (sdiff/clj? (str (:path args))))
            (try
              (let [abs (files/resolve-read! ctx (files/ctx-reference-roots ctx) (str (:path args)))
                    d (address/since-read! (:run-id ctx) (:id branch) (str abs) (slurp abs))]
                (when (and d (or (seq (:changed d)) (seq (:added d)) (seq (:removed d))))
                  (update r :result str "\n\n"
                          (prompt/render "forms-since-read"
                                         {:changed (str/join ", " (:changed d))
                                          :added (str/join ", " (:added d))
                                          :removed (str/join ", " (:removed d))
                                          :same (when (pos? (:same d)) (str (:same d) " forms"))}))))
              (catch Throwable _ nil)))
          r))
    (let [{:keys [miss offset limit] :as found} (form-lines ctx)]
      (cond
        miss (base/malformed branch miss)
        found (files/read-file (assoc ctx :args (-> args (dissoc :form :at)
                                                    (assoc :offset offset :limit limit))))
        :else (files/read-file ctx)))))

(defmethod base/run-tool "write_file" [ctx]
  ;; The sibling notice rides the RESULT rather than gating the call: workers
  ;; sharing a tree are collaborating, and which version should win is not the
  ;; harness's judgement to make. See samizdat.agent.files/stale-note.
  (-> (files/write-file ctx) (files/with-stale ctx) (with-userspace-check ctx)))

(defmethod base/run-tool "edit_file" [ctx]
  (-> (files/edit-file ctx) (files/with-stale ctx) (with-userspace-check ctx)))

(defmethod base/run-tool "patch" [ctx]
  ;; Anchored editing (karamazov-0kk). Beside edit_file rather than replacing
  ;; it: clojure-mcp made the same bet on addressed editing and later demoted
  ;; its own to a fallback, so which one a model actually reaches for is a
  ;; question to MEASURE, not to assume.
  (-> (files/patch-file ctx) (files/with-stale ctx) (with-userspace-check ctx)))

(defmethod base/run-tool "glob" [{:keys [branch root] :as ctx}]
  ;; Find files by NAME (karamazov-fn68). :neutral — locating establishes
  ;; nothing, exactly like grep and read_file.
  ;;
  ;; It pages for the same reason grep does: a tool that silently truncates
  ;; teaches the model that its answer was complete (karamazov-2py). The limit
  ;; and the continuation are grep's, reused rather than reinvented, so the two
  ;; search tools answer in one shape.
  (if-let [m (base/missing ctx :pattern)]
    (base/malformed branch m)
    (let [pattern (str (base/arg ctx :pattern))
          offset (or (some-> (base/arg ctx :offset) str parse-long) 0)
          hits (try (files/glob-project (or root ".") pattern
                                        {:paths (base/arg ctx :paths)})
                    (catch Throwable e [::error (ex-message e)]))]
      (cond
        (and (seq hits) (= ::error (first hits)))
        (base/malformed branch (files/glob-msg {:bad-pattern true :pattern pattern
                                                :detail (second hits)}))

        ;; A scope that names nothing is said to, not searched as empty.
        (and (empty? hits) (seq (files/unresolved-scopes root (base/arg ctx :paths))))
        (base/malformed branch (files/glob-msg {:no-scope true
                                                :missing (str/join ", " (files/unresolved-scopes root (base/arg ctx :paths)))}))

        (empty? hits)
        (base/ok branch (files/glob-msg {:no-matches true :pattern pattern}))

        :else
        (let [{:keys [hits from total next]} (files/grep-page hits offset (files/grep-limit))]
          (base/ok branch
                   (str (files/glob-msg {:found true :total total :pattern pattern
                                         :from (inc from) :to (+ from (count hits))})
                        "\n"
                        (str/join "\n" hits)
                        (when next
                          (files/glob-msg {:more true :remaining (- total (+ from (count hits)))
                                           :next next :pattern (pr-str pattern)}))
                        (when-let [m (seq (files/unresolved-scopes root (base/arg ctx :paths)))]
                          (str "\n" (files/glob-msg {:missing-scopes true :missing (str/join ", " m)}))))))))))

(defmethod base/run-tool "grep" [{:keys [branch root] :as ctx}]
  ;; Search the project's Clojure sources for a regex. :neutral — searching
  ;; establishes nothing, like read_file. The search logic (files/grep-project)
  ;; was written by the agent itself in a supervised self-building run.
  ;;
  ;; It PAGES, for the reason read_file pages: it used to take the first 200
  ;; hits and say nothing about the rest, and carried no offset to continue
  ;; from, so a wide search was a dead end the model could only walk into
  ;; again (karamazov-2py).
  (if-let [m (base/missing ctx :pattern)]
    (base/malformed branch m)
    (let [pattern (str (base/arg ctx :pattern))
          offset (or (some-> (base/arg ctx :offset) str parse-long) 0)
          hits (try (files/grep-project (or root ".") pattern
                                        {:paths (base/arg ctx :paths)
                                         ;; So "grep the examples" is one call
                                         ;; rather than a shell loop.
                                         :refs (files/ctx-reference-roots ctx)})
                    (catch Throwable e [::error (ex-message e)]))
          ;; Hits in files the project keeps from this provider are dropped
          ;; (samizdat.security.exposure).
          hits (if (and (vector? hits) (= ::error (first hits)))
                 hits
                 (vec (exposure/visible ctx hits)))]
      (cond
        (and (vector? hits) (= ::error (first hits)))
        (base/malformed branch (files/grep-msg {:bad-pattern true :detail (second hits)}))

        ;; A scope that names nothing is said to, not searched as empty.
        (and (empty? hits) (seq (files/unresolved-scopes root (base/arg ctx :paths))))
        (base/malformed branch (files/grep-msg {:no-scope true
                                                :missing (str/join ", " (files/unresolved-scopes root (base/arg ctx :paths)))}))

        (empty? hits)
        (base/ok branch (files/grep-msg {:no-matches true :pattern (pr-str pattern)}))

        :else
        (let [{:keys [hits from total next]} (files/grep-page hits offset (files/grep-limit))]
          (base/ok branch
                   (str (files/grep-msg {:found true :total total :pattern (pr-str pattern)
                                         :from (inc from) :to (+ from (count hits))})
                        "\n"
                        (str/join "\n" (for [{:keys [path line text]} hits]
                                         (str path ":" line ": " (str/trim text))))
                        (when next
                          (str "\n" (files/grep-msg {:more true :pattern pattern
                                                     :next next :total total})))
                        (when-let [m (seq (files/unresolved-scopes root (base/arg ctx :paths)))]
                          (str "\n" (files/grep-msg {:missing-scopes true :missing (str/join ", " m)}))))))))))
