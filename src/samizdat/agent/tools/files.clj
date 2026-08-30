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
            [samizdat.agent.tools.base :as base]))

(defmethod base/run-tool "read_file" [ctx]
  (files/read-file ctx))

(defmethod base/run-tool "write_file" [ctx]
  ;; The sibling notice rides the RESULT rather than gating the call: workers
  ;; sharing a tree are collaborating, and which version should win is not the
  ;; harness's judgement to make. See samizdat.agent.files/stale-note.
  (files/with-stale (files/write-file ctx) ctx))

(defmethod base/run-tool "edit_file" [ctx]
  (files/with-stale (files/edit-file ctx) ctx))

(defmethod base/run-tool "patch" [ctx]
  ;; Anchored editing (karamazov-0kk). Beside edit_file rather than replacing
  ;; it: clojure-mcp made the same bet on addressed editing and later demoted
  ;; its own to a fallback, so which one a model actually reaches for is a
  ;; question to MEASURE, not to assume.
  (files/with-stale (files/patch-file ctx) ctx))

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
                    (catch Throwable e [::error (ex-message e)]))]
      (cond
        (and (vector? hits) (= ::error (first hits)))
        (base/malformed branch (files/grep-msg {:bad-pattern true :detail (second hits)}))

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
                                                     :next next :total total}))))))))))
