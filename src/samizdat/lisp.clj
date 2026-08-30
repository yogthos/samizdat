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

(ns samizdat.lisp
  "Delimiter repair for the Clojure the agent writes.

  Models drop trailing parens — the single most common way a written-out
  Clojure form fails to load. Ported in spirit from dirge's repair_delimiters
  (semantic/syntax_validator.rs): close a TRAILING truncation by appending the
  missing closers innermost-first, and trim a TRAILING over-close by removing
  stray closers; do neither for a MID-FILE imbalance, because closing it would
  re-parent unrelated code and any balanced arrangement parses, so the mistake
  would go silent. The scan is lexer-aware — a delimiter inside a string, a
  char literal, or a line comment does not count — and every repair is
  re-validated by actually reading it.

  For a lisp this is the whole story, as dirge's comment puts it: any balanced
  paren arrangement reads, so getting the balance right is getting it right."
  (:require [clojure.string :as str]))

(def ^:private opener->closer {\( \), \[ \], \{ \}})
(def ^:private closer->opener {\) \(, \] \[, \} \{})

(defn line-col
  "1-based {:line :col} of character index `idx` in `s`.

  Every position this namespace reports goes through here. A character offset
  is not something a model can navigate to — the same objection
  samizdat.agent.files/page already makes for reads (\"a line number is
  something the model can hold and act on; a character offset into a file it
  has only seen part of is not\"), which had simply never been applied to the
  diagnostics half. Ported in spirit from vis's parse-diagnose, which reports
  every imbalance as line/col and names BOTH ends of a mismatch."
  [s idx]
  (let [n (min (count s) (max 0 idx))]
    (loop [i 0, line 1, col 1]
      (if (>= i n)
        {:line line :col col}
        (if (= \newline (nth s i))
          (recur (inc i) (inc line) 1)
          (recur (inc i) line (inc col)))))))

(defn count-unescaped-quotes
  "How many `\"` characters in `s` are real string delimiters — a backslash
  consumes the character after it, so `\\\"` (a char literal, or an escape
  inside a string) counts for nothing.

  Ported from vis internal/parse_diagnose.clj `count-unescaped-quotes`
  (Copyright 2025-2026 Blockether, Apache-2.0); `.charAt` indexing replaced
  with `nth`."
  [s]
  (let [n (count s)]
    (loop [i 0, cnt 0]
      (cond
        (>= i n) cnt
        (= \\ (nth s i)) (recur (+ i 2) cnt)
        (= \" (nth s i)) (recur (inc i) (inc cnt))
        :else (recur (inc i) cnt)))))

(defn first-odd-quote-line
  "The 1-based line where the running unescaped-quote count first becomes ODD
  at end-of-line, or nil when every line closes what it opened.

  This is the line that most likely opened a string nobody closed — and it is
  NOT where the reader complains. The reader runs on until something else goes
  wrong and blames a row far below, which is why an unterminated string is the
  one imbalance a paren repairer can never help with. Ported from vis
  internal/parse_diagnose.clj `first-odd-quote-line` (Copyright 2025-2026
  Blockether, Apache-2.0)."
  [code]
  (let [lines (str/split (str code) #"\n" -1)]
    (loop [i 0, running 0]
      (when (< i (count lines))
        (let [total (+ running (count-unescaped-quotes (nth lines i)))]
          (if (odd? total) (inc i) (recur (inc i) total)))))))

(defn- string-end
  "The index just past the closing quote of a string that opens at `i` (where
  `(nth s i)` is the opening `\"`), honoring `\\\"` escapes. Returns the length
  of `s` when the string is unterminated."
  [s n i]
  (loop [j (inc i)]
    (cond
      (>= j n) n
      (= (nth s j) \\) (recur (+ j 2))
      (= (nth s j) \") (inc j)
      :else (recur (inc j)))))

(defn- escaped?
  "Whether the character at `i` is preceded by an odd run of backslashes —
  escaped, so not a real delimiter. The run must be counted, not just the one
  character before: an escaped backslash before a quote leaves the quote real,
  while an escaped quote leaves no string terminator at all (provenance A-5,
  docs/provenance.md)."
  [s i]
  (loop [j (dec i), run 0]
    (if (or (neg? j) (not= \\ (nth s j)))
      (odd? run)
      (recur (dec j) (inc run)))))

(defn scan
  "Lexer-aware delimiter scan of Clojure source. Returns one of:
    {:balance :balanced}
    {:balance :unclosed :stack [[opener idx] ...]}   ; openers with no closer
    {:balance :stray :at idx :char c}                ; a closer with no opener
    {:balance :mismatch :at idx :open-at idx
              :expected c :got c}                    ; wrong closer
    {:balance :unterminated-string :at idx}          ; a string with no close
  Delimiters inside strings, char literals (\\x), and line comments (;…) are
  skipped, so they never count toward the balance."
  [s]
  (let [n (count s)]
    (loop [i 0, stack []]              ; stack of [opener-char index]
      (if (>= i n)
        (if (empty? stack) {:balance :balanced} {:balance :unclosed :stack stack})
        (let [c (nth s i)]
          (cond
            ;; char literal: backslash consumes the next char (which may be a
            ;; delimiter, e.g. \( ) — skip both.
            (= c \\) (recur (+ i 2) stack)

            ;; string: jump past it. An unterminated string is its own status,
            ;; not a delimiter imbalance.
            (= c \") (let [e (string-end s n i)]
                       (if (>= e n)
                         ;; string-end returns n both when the final quote
                         ;; closes the string at EOF and when it is ESCAPED —
                         ;; only an unescaped one actually closed it (provenance A-5).
                         (if (and (> e 0) (= (nth s (dec n)) \") (not (escaped? s (dec n))))
                           (recur n stack)               ; closed exactly at EOF
                           {:balance :unterminated-string :at i})
                         (recur e stack)))

            ;; line comment: skip to end of line.
            (= c \;) (let [nl (str/index-of s "\n" i)]
                       (if nl (recur (inc nl) stack) (recur n stack)))

            (opener->closer c) (recur (inc i) (conj stack [c i]))

            (closer->opener c)
            ;; The opener's INDEX travels with the mismatch. It was already on
            ;; the stack and was being thrown away, so a mismatch could only
            ;; say what went wrong and never where the thing it failed to
            ;; close was opened — which is the half a model needs to fix it.
            (if-let [[open open-at] (peek stack)]
              (if (= open (closer->opener c))
                (recur (inc i) (pop stack))
                {:balance :mismatch :at i :open-at open-at
                 :expected (opener->closer open) :got c})
              {:balance :stray :at i :char c})

            :else (recur (inc i) stack)))))))

(defn read-error
  "The reader's own complaint about `content`, or nil when it reads. Wrapped in
  (do …) so multiple top-level forms are allowed; reading does not evaluate, so
  this only checks that it is well-formed.

  The MESSAGE, not a boolean, because it is the only part of this namespace
  that can say what is wrong with source whose delimiters are all in the right
  places — `(def x 1.2.3)` balances perfectly and is not a number."
  [content]
  (try (read-string (str "(do\n" content "\n)")) nil
       (catch Throwable e (or (ex-message e) (str e)))))

(defn- reads? [content] (nil? (read-error content)))

(defn diagnose
  "Why `s` would not load AS WRITTEN — nil when it loads.

  No repair, and that is the point: asking \"could this be fixed by appending
  closers\" answers a different question from \"is this loadable now\", and a
  caller that REFUSES rather than repairs needs the second one. Conflating them
  is how a refused edit came back announcing `auto-closed 1 unclosed
  delimiter(s)… appended `)`` about a file that still had no closer.

  Returns `{:reason … :line L :col C …}`, positions 1-based:
    :unclosed             N openers never closed, at the outermost one
    :stray                a closer with nothing open, at it
    :mismatch             the wrong closer, naming BOTH ends
    :unterminated-string  at the line the quote count first goes odd
    :does-not-read        every delimiter balances and it still is not
                          Clojure — carries the reader's own words"
  [s]
  (let [{:keys [balance stack at open-at expected got]} (scan s)
        pos (fn [i] (line-col s i))]
    (case balance
      :balanced (when-let [err (read-error s)]
                  {:reason :does-not-read :error err})

      :unclosed (merge {:reason :unclosed :count (count stack)}
                       (pos (second (first stack))))

      :stray (merge {:reason :stray} (pos at))

      :mismatch (merge {:reason :mismatch :expected expected :got got}
                       (pos at)
                       (let [{:keys [line col]} (pos open-at)]
                         {:open-line line :open-col col}))

      ;; The line the running quote count first goes ODD, not the reader's
      ;; row: the reader runs on past the real opening line and blames
      ;; something far below (vis parse-diagnose/first-odd-quote-line).
      :unterminated-string (merge {:reason :unterminated-string}
                                  (pos at)
                                  (when-let [l (first-odd-quote-line s)]
                                    {:line l})))))

(defn balance
  "Diagnose, and where it is safe to, repair Clojure source.

  Returns DATA — a `:status`, a `:reason`, and the facts behind it. No
  sentences: the words the model reads live in resources/prompts/file-tool.md,
  so a project working in a language where the paren-balance advice means
  nothing can reword or drop them without a rebuild. Same seam
  samizdat.agent.telemetry/failure-exemplars uses.

    {:status :balanced   :content s}
      it balances AND it reads.

    {:status :repaired   :content s' :reason :auto-closed|:auto-trimmed
                         :count n :closers \"…\"}
      a trailing truncation or over-close was mechanically fixed and the
      result reads.

    {:status :unbalanced :reason :mismatch|:mid-file-stray
                                |:unterminated-string|:unclosed-unresolvable
                                |:not-closeable
                         :line L :col C + reason-specific facts}
      the imbalance is mid-file or a mismatch — left for the model, never
      silently rewritten, because closing it would re-parent code.

    {:status :unreadable :reason :does-not-read :error \"<the reader's words>\"}
      EVERY DELIMITER IS IN THE RIGHT PLACE AND IT STILL IS NOT CLOJURE.
      This case used to return `:balanced`, and so passed silently through
      both write_file and edit_file (karamazov-ozv). vis states the rule we
      were missing (ext/language_clojure/paren_repair.clj): \"Two readers, two
      questions, and neither answers the other's… edamame says whether text
      READS as Clojure, which balanced delimiters do not promise: source cut
      mid-token comes back closed as `(:)` — balanced, and not a keyword.\"
      `(:)` is exactly what a truncated model write produces."
  [s]
  (let [{:keys [balance stack]} (scan s)
        refuse (fn [] (assoc (diagnose s) :status :unbalanced))]
    (case balance
      :balanced
      ;; The delimiters are right. That is a DIFFERENT question from whether
      ;; this is Clojure, and asking only the first one is how balanced-but-
      ;; unreadable source reached disk with no warning at all.
      (if-let [d (diagnose s)]
        (assoc d :status :unreadable)
        {:status :balanced :content s})

      :unclosed
      ;; A trailing truncation: append the missing closers, innermost first.
      ;; Guard: it must actually read afterwards (a mid-file unclosed that
      ;; happens to balance by appending would fail this or re-parent, and an
      ;; unterminated string is not repairable by appending closers).
      (let [closers (apply str (map (comp opener->closer first) (reverse stack)))
            repaired (str s closers)]
        (if (reads? repaired)
          {:status :repaired :content repaired :reason :auto-closed
           :count (count stack) :closers closers}
          (refuse)))

      :stray
      ;; Only a TRAILING run of stray closers is removable — a mid-file stray
      ;; (real code after it) is never touched. Trim closer-by-closer from the
      ;; end, re-scanning, and require the result to read.
      (let [trimmed (loop [cur s, removed 0]
                      (when (< removed 256)
                        (let [end (count (str/trimr cur))]
                          (cond
                            (zero? end) nil
                            (not (closer->opener (nth cur (dec end)))) nil ; non-closer at end → mid-file stray
                            :else
                            (let [cut (str (subs cur 0 (dec end)) (subs cur end))]
                              (case (:balance (scan cut))
                                :balanced (when (reads? cut) [cut (inc removed)])
                                :stray (recur cut (inc removed))
                                nil))))))]
        (if trimmed
          {:status :repaired :content (first trimmed) :reason :auto-trimmed
           :count (second trimmed)}
          (refuse)))

      (refuse))))
