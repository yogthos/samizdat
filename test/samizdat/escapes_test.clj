;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.escapes-test
  "The \\uXXXX drift decoder: undo the escape a model writes when it meant the
  character, and NOTHING else. Every test here is about the second half — the
  narrowness is the whole reason the decode is safe, so most of these assert
  that something is left alone."
  (:require [clojure.test :refer [deftest testing is]]
            [samizdat.escapes :as escapes]))

(def ^:private decode escapes/decode-unicode-escapes)

(deftest a-drifted-escape-becomes-the-character-it-names
  (is (= "an em — dash" (decode "an em \\u2014 dash")))
  (is (= "λ" (decode "\\u03bb")) "lower-case hex")
  (is (= "λ" (decode "\\u03BB")) "upper-case hex")
  (testing "several in one string, and the text between them is untouched"
    (is (= "a—b→c" (decode "a\\u2014b\\u2192c")))))

(deftest a-doubled-backslash-is-text-about-an-escape-and-is-left-alone
  ;; The backslash must itself be UNESCAPED. A Clojure string that legitimately
  ;; contains the six characters is not a drift artifact.
  (is (= "\\\\u2014" (decode "\\\\u2014")))
  (is (= "\\\\\\\\u2014" (decode "\\\\\\\\u2014")))
  (testing "an odd run still decodes, and keeps the escaped backslashes"
    (is (= "\\\\—" (decode "\\\\\\u2014")))))

(deftest ascii-and-control-escapes-are-load-bearing-and-never-decoded
  ;; Below U+00A0 the escape is doing work: \n in a string, \u001b opening an
  ;; ANSI sequence, \u0022 inside JSON. Decoding those changes what the text IS.
  (doseq [s ["\\u0022" "\\u000a" "\\u001b" "\\u0041" "\\u007f" "\\u009f"]]
    (is (= s (decode s)) (str s " is ASCII or a control"))))

(deftest nothing-decodes-into-invisible-or-unreal-ink
  ;; Six visible characters a human can see and fix beat one character that
  ;; cannot be seen at all.
  (doseq [[s what] [["\\u00ad" "soft hyphen"]
                    ["\\u200b" "zero width space"]
                    ["\\u200d" "zero width joiner"]
                    ["\\u202e" "right-to-left override"]
                    ["\\u2028" "line separator"]
                    ["\\u2029" "paragraph separator"]
                    ["\\u00a0" "no-break space"]
                    ["\\u3000" "ideographic space"]
                    ["\\ue000" "private use"]
                    ["\\uffff" "unassigned"]
                    ["\\ud83d" "lone high surrogate"]
                    ["\\ude00" "lone low surrogate"]]]
    (is (= s (decode s)) (str what " must survive verbatim"))))

(deftest a-surrogate-pair-is-judged-by-the-character-it-builds
  ;; Neither half is decodable alone — both are category Cs — so the pair has
  ;; to be recognised as a pair and judged on the code point it names.
  (testing "an emoji survives the trip"
    (is (= "😀" (decode "\\ud83d\\ude00")))
    (is (= "hi 😀 there" (decode "hi \\ud83d\\ude00 there"))))
  (testing "and an INVISIBLE character cannot sneak in as one"
    ;; U+E0020 TAG SPACE, written as the pair D840+DC20, is category Cf.
    (is (= "\\udb40\\udc20" (decode "\\udb40\\udc20")))))

(deftest a-malformed-escape-is-not-an-escape
  (doseq [s ["\\u201" "\\u20 14" "\\uzzzz" "\\u" "\\" "\\u201"]]
    (is (= s (decode s)) (str (pr-str s) " is not a complete escape")))
  (testing "non-ASCII digits are not hex digits — a quad of those is nobody's drift"
    (is (= "\\u٠٦٤٤" (decode "\\u٠٦٤٤")))))

(deftest pure-and-total
  (is (= "" (decode "")))
  (is (= "no escapes here" (decode "no escapes here")))
  (is (nil? (decode nil)))
  (is (= 42 (decode 42)))
  (testing "text with backslashes but no escape comes back unchanged"
    (is (= "C:\\path\\to" (decode "C:\\path\\to")))))
