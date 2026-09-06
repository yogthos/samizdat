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
;; Ported from vis, internal/foundation/editing/escapes.clj —
;; Copyright 2025-2026 Blockether, Apache License 2.0. The JUDGEMENT is taken
;; unchanged, category bitmask and all, because its narrowness is the only
;; reason decoding is safe at all. Changed here: the primitive type hints and
;; the `^long` returns are dropped (jolt does not use them), and the whole
;; UTF-16 layer is gone. A jolt char is a Unicode SCALAR, not a code unit —
;; `(char 0x1F600)` is one character — so there is no `Character/toCodePoint`
;; (jolt has none) and no pair of `.append`s: a drifted surrogate PAIR is
;; recombined arithmetically in `surrogate-pair` and emitted as ONE char.
;; `.length`/`.charAt`/`.indexOf` and `StringBuilder` are used as vis has
;; them, since jolt provides all four.

(ns samizdat.escapes
  "Unicode-escape hygiene for model-authored text.

  A model sometimes emits the six characters `\\u2014` where it means an em
  dash. JSON decoding is not the culprit: the escape arrives already escaped,
  so those six characters ARE the text the tool was asked to write, and they
  land on disk exactly like that.

  Decoding is therefore narrow enough that it can only ever undo drift: the
  backslash must be unescaped, and the escape must name a VISIBLE ASSIGNED
  character, judged on the code point. Real source legitimately contains an
  ASCII or control escape, a doubled escape inside prose about escapes, and
  private-use code points in icon fonts; nothing decodes into invisible ink
  (bidi overrides, zero width, a space that is not a space) or into an
  unassigned point. Every one of those is written through verbatim."
  (:require [clojure.string :as str]))

(defn- hex-digit-value
  "Value of the ASCII hex digit `c`, or -1.

  Deliberately ASCII-only, and deliberately not a character-class predicate:
  those also accept a non-ASCII digit (U+0664 ARABIC-INDIC FOUR, U+FF14
  FULLWIDTH FOUR), and a quad of those is not an escape any model drifted
  into."
  [c]
  (cond (and (<= 48 c) (<= c 57))  (- c 48)
        (and (<= 97 c) (<= c 102)) (- c 87)
        (and (<= 65 c) (<= c 70))  (- c 55)
        :else -1))

(defn- escape-unit
  "The 16-bit value of the `\\uXXXX` escape whose backslash sits at `i` in `s`,
  or -1 when no complete escape starts there."
  [s i]
  ;; An escape is exactly six characters wide: \ u and four hex digits.
  (if (or (> (+ i 6) (.length s))
          (not= \\ (.charAt s i))
          (not= \u (.charAt s (inc i))))
    -1
    (loop [k (+ i 2) acc 0]
      (if (= k (+ i 6))
        acc
        (let [d (hex-digit-value (int (.charAt s k)))]
          (if (neg? d) -1 (recur (inc k) (+ (* acc 16) d))))))))

(def ^:private undecodable-categories
  "Unicode general categories an escape may never be decoded INTO. Cn/Co/Cs are
  unassigned points, private use and half characters; Cc/Cf, Zl/Zp and Zs are
  controls, invisible formatting (bidi overrides, zero width, soft hyphen),
  line separators and spaces that do not look like spaces. Writing one of those
  into a file as a real character is strictly worse than leaving six visible
  characters a human can see and fix."
  [Character/UNASSIGNED Character/PRIVATE_USE Character/SURROGATE
   Character/CONTROL Character/FORMAT Character/LINE_SEPARATOR
   Character/PARAGRAPH_SEPARATOR Character/SPACE_SEPARATOR])

(def ^:private undecodable-category-bits
  "`undecodable-categories` folded into one bit per category value, so the
  judgement is a shift and a mask rather than a set lookup per escape."
  (reduce (fn [acc t] (bit-or acc (bit-shift-left 1 (int t))))
          0
          undecodable-categories))

(defn decodable-code-point?
  "True when `cp` names a VISIBLE ASSIGNED non-ASCII character — the only kind
  of escape that is decoded.

  Below U+00A0 sit ASCII and the C0/C1 controls, where the escape is
  load-bearing: `\\u000a` in a string, `\\u001b` opening an ANSI sequence,
  `\\u0022` inside JSON. Above it, one category lookup answers both 'does this
  character exist' and 'may it be written', since an unassigned point IS
  category Cn."
  [cp]
  (and (>= cp 0xA0)
       (zero? (bit-and undecodable-category-bits
                       (bit-shift-left 1 (Character/getType (int cp)))))))

(defn- surrogate-pair
  "The code point a high/low surrogate pair builds, or -1.

  jolt chars are scalars, so a surrogate never appears as a character here —
  but a model that drifted an emoji writes the PAIR of escapes it saw in
  UTF-16, and neither half is decodable alone (both are category Cs). The
  arithmetic is UTF-16's own, done explicitly because jolt has no
  `Character/toCodePoint`."
  [hi lo]
  (if (and (<= 0xD800 hi) (<= hi 0xDBFF) (<= 0xDC00 lo) (<= lo 0xDFFF))
    (+ 0x10000 (bit-shift-left (- hi 0xD800) 10) (- lo 0xDC00))
    -1))

(defn decode-unicode-escapes
  "Decode the `\\uXXXX` escapes in model-authored text that can only be drift.

  A literal `\\u2014` handed to a file tool is written to disk as those six
  characters, so edited files grow `\\u2014` where an em dash belongs. Here it
  becomes the em dash, while every escape a real file may legitimately carry is
  returned untouched: a doubled `\\\\uXXXX`, an ASCII or control escape, a lone
  surrogate, and anything that would decode into invisible or unreal ink —
  private use, an unassigned point, a bidi override, a zero-width joiner, a
  line separator, a space that does not look like one. A surrogate PAIR is
  judged by the code point it builds, so an emoji survives the trip and an
  invisible U+E0020 tag character cannot sneak in as one.

  Pure and total: a non-string, or a string with no escape in it, comes back
  as-is. Text between backslashes is never walked character by character —
  `indexOf` finds the next candidate and the span is bulk-copied — so the
  common case, which carries no escape at all or one, costs a scan."
  [s]
  (if-not (and (string? s) (str/includes? s "\\u"))
    s
    (let [n (.length s)
          sb (StringBuilder.)]
      (loop [i 0]
        (let [b (.indexOf s \\ i)]
          (if (neg? b)
            ;; No backslash left in the tail: copy it whole and stop.
            (do (.append sb (subs s i n)) (str sb))
            (let [_ (.append sb (subs s i b))
                  run-end (loop [j b]
                            (if (and (< j n) (= \\ (.charAt s j)))
                              (recur (inc j))
                              j))]
              (if (even? (- run-end b))
                ;; Every backslash is itself escaped: this is text ABOUT an
                ;; escape, not an escape.
                (do (.append sb (subs s b run-end)) (recur run-end))
                (let [start (dec run-end)
                      unit (escape-unit s start)
                      lo (if (and (<= 0xD800 unit) (<= unit 0xDBFF))
                           (escape-unit s (+ start 6))
                           -1)
                      pair (if (neg? lo) -1 (surrogate-pair unit lo))]
                  ;; The escaped backslashes before the live one go through
                  ;; whatever happens to the escape itself.
                  (.append sb (subs s b start))
                  (cond
                    (and (pos? pair) (decodable-code-point? pair))
                    (do (.append sb (char pair)) (recur (+ start 12)))

                    (and (not (neg? unit)) (decodable-code-point? unit))
                    (do (.append sb (char unit)) (recur (+ start 6)))

                    :else
                    (do (.append sb \\) (recur run-end))))))))))))
