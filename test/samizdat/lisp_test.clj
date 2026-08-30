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

(ns samizdat.lisp-test
  "Delimiter repair for the Clojure the agent writes. Models drop trailing
  parens; this closes a trailing truncation and trims a trailing over-close,
  lexer-aware (strings, char literals, comments don't count), and never
  touches a mid-file imbalance — closing that would silently re-parent code."
  (:require [clojure.test :refer [deftest testing is]]
            [samizdat.lisp :as lisp]))

(deftest balanced-passes-through
  (doseq [s ["(defn f [] 1)"
             "(ns x)\n(defn f [x] (+ x 1))"
             "(str \"a)b\" \\( \\))"       ; parens in string + char literals
             "(f) ; a trailing ) in a comment )"
             ""]]
    (is (= :balanced (:status (lisp/balance s))) (str "should be balanced: " (pr-str s)))
    (is (= s (:content (lisp/balance s))))))

(deftest closes-a-trailing-truncation
  (testing "one unclosed form gets its closer"
    (let [r (lisp/balance "(defn f [] (+ 1 2)")]
      (is (= :repaired (:status r)))
      (is (= "(defn f [] (+ 1 2))" (:content r)))
      (is (= :auto-closed (:reason r)))
      (is (= 1 (:count r)))
      (is (= ")" (:closers r)))))
  (testing "several nested unclosed delimiters close innermost-first"
    ;; open: (defn … (let [ … (* x 2) ] closes the vec, then (let and (defn
    ;; remain open — a real trailing truncation of two forms.
    (let [r (lisp/balance "(defn f [x]\n  (let [y (* x 2)]")]
      (is (= :repaired (:status r)))
      (is (= "(defn f [x]\n  (let [y (* x 2)]))" (:content r)))
      (is (= :balanced (:status (lisp/balance (:content r)))))))
  (testing "a repaired string actually reads"
    (let [r (lisp/balance "(map inc [1 2 3")]
      (is (= :repaired (:status r)))
      (is (some? (read-string (str "(do " (:content r) ")")))))))

(deftest trims-a-trailing-overclose
  (testing "one extra trailing closer is removed"
    (let [r (lisp/balance "(defn f [] 1))")]
      (is (= :repaired (:status r)))
      (is (= "(defn f [] 1)" (:content r)))
      (is (= :auto-trimmed (:reason r)))
      (is (= 1 (:count r)))))
  (testing "several extra trailing closers"
    (let [r (lisp/balance "(inc 1)))")]
      (is (= :repaired (:status r)))
      (is (= "(inc 1)" (:content r))))))

(deftest never-touches-a-mid-file-imbalance
  (testing "a stray closer with real code after it is left for the model"
    (let [r (lisp/balance "(defn f [] 1)) (defn g [] 2)")]
      (is (= :unbalanced (:status r)))
      (is (nil? (:content r)) "no silent rewrite of mid-file code")))
  (testing "a mismatched pair is not auto-repaired"
    (let [r (lisp/balance "(defn f [] (vec [1 2)])")]
      (is (= :unbalanced (:status r))))))

(deftest strings-and-comments-do-not-count
  (testing "an unclosed delimiter INSIDE a string is not a code imbalance"
    (is (= :balanced (:status (lisp/balance "(def s \"an open ( paren\")")))))
  (testing "a delimiter in a line comment is ignored"
    (is (= :balanced (:status (lisp/balance "(def x 1) ; ) ] } trailing junk"))))))

(deftest an-unterminated-string-is-its-own-status
  ;; a#5 (docs/provenance.md): a string whose last quote was ESCAPED used to
  ;; scan as "closed exactly at EOF", so the file read as balanced and
  ;; write_file wrote the broken text with no warning.
  (let [escaped-final-quote (str "\"x " "\\" "\"")   ; "x \"
        escaped-backslash   (str "(def s \"x " "\\" "\\" "\")")] ; (def s "x \")
    (testing "a string ending at an escaped quote is unterminated"
      (is (= :unterminated-string (:balance (lisp/scan escaped-final-quote)))))
    (testing "inside a form it does not silently repair"
      (let [r (lisp/balance (str "(def s " escaped-final-quote))]
        (is (= :unbalanced (:status r)))
        (is (nil? (:content r)) "no rewrite of text with a broken string")))
    (testing "an escaped backslash before a real closing quote still closes"
      (is (= :balanced (:balance (lisp/scan escaped-backslash)))))))

;; --- karamazov-ozv: balanced is not readable ---------------------------------
;; vis (ext/language_clojure/paren_repair.clj) states the rule we were missing:
;; "Two readers, two questions, and neither answers the other's… edamame says
;; whether text READS as Clojure, which balanced delimiters do not promise:
;; source cut mid-token comes back closed as `(:)` — balanced, and not a
;; keyword." `(:)` is literally their example, and it is the shape a truncated
;; model write produces.

(deftest balanced-delimiters-do-not-promise-readable
  (testing "source whose delimiters balance but which does not read is :unreadable"
    (doseq [s ["(def x :)"
               "(ns a)\n(defn f [] 1)\n:"
               "(def x 1.2.3)"
               "(a #)"
               "(def x @)"]]
      (let [r (lisp/balance s)]
        (is (= :balanced (:balance (lisp/scan s)))
            (str "precondition — delimiters balance: " (pr-str s)))
        (is (= :unreadable (:status r))
            (str "should not pass as balanced: " (pr-str s)))
        (is (nil? (:content r)) "unreadable source is never handed back as usable")
        (is (string? (:error r)) "carries the reader's own complaint"))))
  (testing "the reader's message names the actual problem"
    (is (re-find #"(?i)invalid token" (:error (lisp/balance "(def x :)")))))
  (testing "a repair that would balance but not read is refused, not returned"
    ;; closing this gives "(def x :)" — balanced, unreadable.
    (let [r (lisp/balance "(def x :")]
      (is (not= :repaired (:status r)))
      (is (nil? (:content r))))))

;; --- karamazov-mea: diagnostics a model can navigate to -----------------------

(deftest imbalance-is-reported-as-line-and-column
  (testing "a mismatched delimiter names BOTH ends, with line and column"
    ;; the `)` on line 3 tries to close the `[` opened on line 2
    (let [r (lisp/balance "(defn f []\n  (let [x 1\n        )]\n    x))")]
      (is (= :unbalanced (:status r)))
      (is (= :mismatch (:reason r)))
      (is (= 3 (:line r)) "the offending closer's line")
      (is (= 9 (:col r)) "the offending closer's column")
      (is (= 2 (:open-line r)) "the opener it failed to close")
      (is (= 8 (:open-col r)))
      (is (= \] (:expected r)))
      (is (= \) (:got r)))))
  (testing "a mid-file stray closer carries line and column, not a char offset"
    (let [r (lisp/balance "(defn f [] 1))\n(defn g [] 2)")]
      (is (= :unbalanced (:status r)))
      (is (= :stray (:reason r)))
      (is (= 1 (:line r)))
      (is (= 14 (:col r)))))
  (testing "an unterminated string reports the line the quote count first goes odd"
    ;; ported from vis parse_diagnose/first-odd-quote-line: the reader blames a
    ;; row far below the line that actually opened the string.
    (let [r (lisp/balance "(ns demo)\n(def a \"unclosed\n(def b 2)\n(def c 3)\n")]
      (is (= :unbalanced (:status r)))
      (is (= :unterminated-string (:reason r)))
      (is (= 2 (:line r)) "the line where the running quote count first goes odd"))))

(deftest line-col-maps-an-index-to-a-position
  (let [s "abc\ndefg\nhi"]
    (is (= {:line 1 :col 1} (lisp/line-col s 0)))
    (is (= {:line 1 :col 3} (lisp/line-col s 2)))
    (is (= {:line 2 :col 1} (lisp/line-col s 4)))
    (is (= {:line 3 :col 2} (lisp/line-col s 10)))))

(deftest first-odd-quote-line-finds-the-opening-line
  (is (= 2 (lisp/first-odd-quote-line "(def a 1)\n(def b \"oops\n(def c 3)")))
  (is (nil? (lisp/first-odd-quote-line "(def a \"ok\")\n(def b 2)"))
      "balanced quotes report nothing")
  (is (nil? (lisp/first-odd-quote-line "(def a \\\" 1)"))
      "an escaped quote is not a string delimiter"))
