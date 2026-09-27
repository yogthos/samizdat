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

(ns samizdat.util
  "Small pure helpers shared across the codebase.

  The first code samizdat wrote about itself: truncate-middle was authored by
  the harness in a supervised self-modification run, then wired into the shell
  tool's output truncation."
  (:require [clojure.main :as main]
            [clojure.string :as str]))

(defn sh-quote
  "`s` as a single-quoted shell word, safe to interpolate into `sh -c`.

  The POSIX escape for a quote inside a single-quoted string is `'\\''`:
  close, escaped quote, reopen. verify/run-verify spelled it `'\\\\''`, one
  backslash too many — that closes the quote, emits a literal backslash,
  opens and closes an empty string, and leaves the REST of the command
  inside an unterminated quote. A project root holding a `'` turned every
  verify run into a shell syntax error, and the branch was told its tests
  were red forever.

  Here rather than in either caller because both the ship gate's verify and
  the shell tool have the same root to quote, and a second hand-rolled
  version is how the first one drifted."
  [s]
  (str "'" (str/replace (str s) "'" "'\\''") "'"))

(defn generation-cache
  "Wrap `f` as a no-arg fn whose result is recomputed only when `gen-fn`
  reports a new generation. The seam between a reloadable resource and the
  things COMPILED from it.

  The pattern this replaces: a top-level `(def gates (mapv compile-gate …))`
  reading a cache atom at namespace load. system/start! called reload! on
  every one of those caches, and the comment said long-lived sessions would
  pick up edits without a restart — but only the scalar readers did. The
  compiled gate table, the ship rungs, the forceable-tool schemas, the winner
  rubric, the stopword sets and the judge preamble were all frozen at first
  load, so exactly the parts the docstrings advertised as retunable-without-a-
  rebuild were the parts a reload could not touch.

  A counter rather than a content hash: bumping is what `reload!` already
  means, and it costs nothing on the read path. Racing readers may compute
  `f` twice and the last writer wins — both results are derived from the same
  generation, so they agree."
  [gen-fn f]
  (let [cache (atom nil)]
    (fn []
      (let [g (gen-fn)
            c @cache]
        (if (and c (= g (:gen c)))
          (:val c)
          (:val (reset! cache {:gen g :val (f)})))))))

(defn truncate-middle
  "If `s` is longer than `max-len`, return it shortened to exactly `max-len`
  characters by keeping the head and tail with the literal string \" … \"
  in the middle; otherwise return `s` unchanged."
  [s max-len]
  (if (<= (count s) max-len)
    s
    (let [marker " … "
          marker-len (count marker)
          head-len (quot (- max-len marker-len) 2)
          tail-len (- max-len marker-len head-len)]
      (str (subs s 0 head-len) marker (subs s (- (count s) tail-len))))))

(defn stack-lines
  "`e`'s stack as readable lines, innermost first: `ns/fn (file:line)`, the
  way a person would name the function — `samizdat.agent.beam/spawn-children!`
  rather than `samizdat.agent.beam$spawn_children_BANG_`. At most `n` lines;
  nil `n` keeps them all. A throwable with no stack yields []."
  [e n]
  (let [frames (keep (fn [[cls _method file line]]
                       (when cls
                         (str (main/demunge (str cls))
                              (when file (str " (" file (when line (str ":" line)) ")")))))
                     (:trace (Throwable->map e)))]
    (vec (if n (take n frames) frames))))
