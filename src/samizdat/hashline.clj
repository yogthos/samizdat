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
;; Ported from vis, internal/foundation/editing/hashline.clj —
;; Copyright 2025-2026 Blockether, Apache License 2.0. Vis credits the anchor
;; shape to Can Bölük's hashline. Changed here: the JVM interop is gone
;; (`String/hashCode` is folded explicitly, see `line-hash`; `.split`,
;; `.charAt`, `.indexOf`, `Integer/toHexString`, `Long/parseLong` and
;; `Math/abs` become their clojure.core equivalents), the clojure.spec
;; declarations are dropped (nothing in samizdat's src/ uses spec), the drift
;; tolerance is an explicit argument rather than a constant so it can live in
;; gates.edn, and `apply-edits` is added — vis plans an edit span at a time and
;; splices in its tool layer.

(ns samizdat.hashline
  "Anchored addressing: `<line>:<hash>` coordinates a read MINTS and an edit
  SPENDS, so the model never restates the text it is replacing.

  An anchor is a 1-based line number, a colon, and three hex characters of the
  line's content hash. TWO COORDINATES, and each answers a different question:
  the LINE says where to look, the HASH says whether what is there is still
  what the caller saw. One alone is not enough — a bare line number lands an
  edit wherever the file has drifted to, and a bare hash cannot tell two
  identical lines apart.

  Why this exists: samizdat's edit_file asks the model to reproduce a
  byte-exact span it read earlier. That is the hardest thing we ask of a weak
  model, it is why files.clj carries a line-trimmed whitespace fallback for
  what dirge measured as ~95% of failed edits, and it is a large share of the
  mechanics rate. An anchor is PRINTED beside the line the model already read,
  so spending one costs it nothing to remember.

  Pure over its arguments — no IO, no tool wiring. `samizdat.agent.files` owns
  the filesystem."
  (:require [clojure.string :as str]
            [samizdat.lexicon :as lexicon]))

(defn drift-tolerance
  "How far (in lines) a content hash may sit from its stated line number before
  the anchor is called MISPLACED and refused — gates.edn `:anchor`.

  The common path never needs it: an anchor spent right after the read that
  minted it resolves exactly. This window only forgives small drift when
  anchors are reused across edits without re-reading; anything larger is the
  genuinely-wrong-anchor case this whole scheme exists to stop, and is
  refused so the caller re-reads."
  []
  (:drift-tolerance (lexicon/policy :anchor)))

(def gutter
  "Between an anchor and its line text in every rendered block. U+2502 plus a
  space: it does not occur in source, so splitting a rendered line on it is
  exact and can never be confused with the `:` inside the anchor itself."
  "│ ")

(def ^:private anchor-sep ":")

(def ^:private hash-width
  "Hex chars in a line's content hash. Three keeps an anchor to 4-7 characters
  while the line coordinate disambiguates collisions."
  3)

(def ^:private hash-mask
  "Low `hash-width` hex digits as a bit mask: (16^hash-width) - 1."
  (dec (bit-shift-left 1 (* 4 hash-width))))

(def ^:private hash-pad (apply str (repeat hash-width \0)))

(defn line-hash
  "Stable three-hex-char content hash of `line`, TRIMMED — so a line that only
  moved horizontally keeps its anchor.

  Folds the JVM `String/hashCode` recurrence `h = 31h + c`, but reduced mod
  2^12 at every step rather than mod 2^32 then masked. Those are the same
  number: 2^12 divides 2^32, so the low twelve bits of the full hash depend
  only on the low twelve bits carried along the way. That makes this an EXACT
  reproduction of vis's anchors on a runtime with no `String/hashCode` at all —
  verified against a JVM, including vis's own documented `4395:573│ (defn-
  patch-edit-rows`. A blank line hashes to 000."
  [line]
  (let [h (reduce (fn [h c] (bit-and (+ (* 31 h) (int c)) hash-mask))
                  0
                  (str/trim (str line)))
        hex (format "%x" h)]
    (if (< (count hex) hash-width)
      (str (subs hash-pad (count hex)) hex)
      hex)))

(defn content-lines
  "Split a file blob into a vector of lines. The trailing empty element a
  final newline produces is dropped, so the vector's count IS the file's line
  count and index 0 is line 1."
  [s]
  (let [v (str/split (str s) #"\n" -1)]
    (if (and (seq v) (= "" (peek v))) (pop v) v)))

(defn char-offset-at-line
  "Char offset in `content` where 0-based `line-idx` starts; the length of
  `content` when `line-idx` runs past the last line."
  [content line-idx]
  (loop [pos 0, i 0]
    (if (= i line-idx)
      pos
      (if-let [nl (str/index-of content "\n" pos)]
        (recur (inc nl) (inc i))
        (count content)))))

(defn line-anchor
  "The editable anchor for a line: `<line-number>:<content-hash>`."
  [ln text]
  (str ln anchor-sep (line-hash text)))

(def ^:private bare-anchor-re #"\d+:[0-9a-fA-F]+")

(defn anchor-token
  "The bare `<line>:<hash>` token inside whatever the caller actually sent.

  Every anchor a model sees is RENDERED — `<line>:<hash>│ <text>` — and the
  contract these tools advertise is that one of their own lines goes straight
  back in. So everything from the gutter on is DECORATION and is cut here.
  Without the cut the line text became part of the anchor, no line could carry
  it, and the refusal handed back the very anchor it had just refused.

  What is left is unwrapped from stray whitespace and quote characters — the
  other common shape, where `\"12:abc\"` arrives WITH its literal quotes."
  [x]
  (let [s (str x)
        cut (or (str/index-of s gutter) (count s))]
    (-> (subs s 0 cut)
        str/trim
        (str/replace #"^['\"`]+" "")
        (str/replace #"['\"`]+$" "")
        str/trim)))

(defn anchor-string?
  "Whether `x` ADDRESSES a line: a bare token, or a whole rendered line, which
  is that same token with the gutter still attached."
  [x]
  (boolean (and (string? x) (re-matches bare-anchor-re (anchor-token x)))))

(defn render-lines
  "Render `[[line-number text] …]` as the model's addressable gutter —
  `<line>:<hash>│ <text>`, one line per tuple. THE single renderer, so a read
  and a search speak one format and any of their lines feeds straight back in.

  A CRLF file's lines still carry their `\\r` (offsets must count every real
  character), but rendering drops it: it is invisible on screen, the hash never
  saw it, and a model copying a rendered line back as a replacement would
  otherwise write a second carriage return in."
  [tuples]
  (->> tuples
       (map (fn [[ln s]]
              (let [s (str s)
                    s (if (str/ends-with? s "\r") (subs s 0 (dec (count s))) s)]
                (str (line-anchor ln s) gutter s))))
       (str/join "\n")))

(defn render
  "The lines of `content` from 1-based `from` to `to` inclusive, rendered as
  addressable text."
  [content from to]
  (let [lines (content-lines content)
        n (count lines)
        from (max 1 from)
        to (min n to)]
    (render-lines (for [i (range from (inc to))] [i (nth lines (dec i))]))))

(defn parse-anchor
  "Parse `<line>:<hash>` into `{:line L :hash H}`. The line number is REQUIRED:
  without it the anchor cannot locate anything, so it parses to
  `{:malformed true :raw S}` and every resolver refuses it."
  [anchor]
  (let [s (anchor-token anchor)
        i (str/index-of s anchor-sep)
        line (when i (parse-long (subs s 0 i)))]
    (if (and i line)
      {:line line :hash (str/lower-case (subs s (inc i)))}
      {:malformed true :raw s})))

(defn- indices-matching-hash
  "0-based indices of `lines` whose content hash is `h`."
  [lines h]
  (into [] (keep-indexed (fn [i l] (when (= (str h) (line-hash l)) i))) lines))

(defn resolve-one
  "Resolve a PARSED anchor to a 0-based index in `lines`, or
  `{:error {:reason KW …}}`. `tolerance` is how far the content may sit from
  the stated line before the anchor is called misplaced.

  THE LADDER, and it is the whole point of the scheme — five outcomes, not
  two:
    1. exact     — the stated line still hashes to `hash`            → use it
    2. drifted   — the content sits at EXACTLY one line within
                   `tolerance`                                       → follow it
    3. line wins — `hash` is AMBIGUOUS but the caller named an EXPLICIT
                   line, so use the line. Duplicate lines do NOT make a
                   `line:hash` anchor ambiguous
    4. misplaced — `hash` matches only lines FAR from the stated one: a
                   strong line-vs-content contradiction → REFUSE. This is
                   the wrong-line guard the scheme exists for
    5. not-found — `hash` matches nothing (the content is gone) → REFUSE,
                   handing back the anchor that IS at the stated line so the
                   caller recovers in ONE call instead of a second read

  A false refuse costs one re-read. A false accept corrupts the file."
  [lines which {:keys [line hash malformed raw]} tolerance]
  (if malformed
    {:error {:reason :anchor-malformed :which which :anchor raw}}
    (let [idx0 (dec line)
          n (count lines)]
      (cond
        (or (neg? idx0) (>= idx0 n))
        {:error {:reason :anchor-line-out-of-range :which which :line line :lines n}}

        ;; 1. exact
        (= hash (line-hash (nth lines idx0)))
        {:index idx0}

        :else
        (let [matches (indices-matching-hash lines hash)]
          (if (empty? matches)
            ;; 5. gone — refuse, but carry the CURRENT anchor at the stated
            ;; line so the common stale-after-edit case recovers in one step.
            {:error {:reason :anchor-not-found
                     :which which
                     :hash hash
                     :stated-line line
                     :current-anchor (line-anchor line (nth lines idx0))
                     :current-text (nth lines idx0)}}
            (let [near (filterv #(<= (abs (- (inc %) line)) tolerance) matches)]
              (cond
                ;; 2. drifted
                (= 1 (count near)) {:index (first near)}

                ;; 4. misplaced — three hex chars make "unique in the file"
                ;; weak evidence, so refuse; but when the content now sits at
                ;; exactly one line, the correct anchor is that line plus this
                ;; same hash. Hand it back.
                (empty? near)
                {:error (cond-> {:reason :anchor-misplaced
                                 :which which
                                 :hash hash
                                 :stated-line line
                                 :found-lines (mapv inc matches)}
                          (= 1 (count matches))
                          (assoc :current-anchor
                                 (line-anchor (inc (first matches))
                                              (nth lines (first matches)))))}

                ;; 3. the explicit line wins
                :else {:index idx0}))))))))

(defn resolve-range
  "Resolve `from` (and `to`, defaulting to `from` for a single line) against
  live `content`. Returns `{:from-line N :to-line N}`, 1-based INCLUSIVE, or
  `{:error {:reason KW …}}`. The WRITE side of the contract — only this side
  refuses."
  ([content from to] (resolve-range content from to (drift-tolerance)))
  ([content from to tolerance]
  (let [lines (content-lines content)
        from-a (parse-anchor from)
        to-a (if (or (nil? to) (= (str to) (str from))) from-a (parse-anchor to))
        fr (resolve-one lines :from from-a tolerance)]
    (if (:error fr)
      fr
      (let [tr (if (identical? from-a to-a) fr (resolve-one lines :to to-a tolerance))]
        (if (:error tr)
          tr
          (let [fi (:index fr), ti (:index tr)]
            (if (< ti fi)
              {:error {:reason :anchor-range-inverted :from-line (inc fi) :to-line (inc ti)}}
              {:from-line (inc fi) :to-line (inc ti)}))))))))

(defn resolve-range-read
  "The READ-tolerant twin of `resolve-range`. A read is NON-DESTRUCTIVE, so a
  stale hash must not block the look the way it correctly blocks a write: each
  anchor still resolves by content first, but when its hash matches no live
  line the anchor's LINE NUMBER is the fallback and the result is marked
  `:stale?`. Only a genuinely unlocatable anchor — malformed, or a line outside
  the file — is an error."
  ([content from to] (resolve-range-read content from to (drift-tolerance)))
  ([content from to tolerance]
  (let [lines (content-lines content)
        n (count lines)
        one (fn [which anchor]
              (let [a (parse-anchor anchor)
                    r (resolve-one lines which a tolerance)]
                (cond
                  (:index r) (assoc r :stale? false)
                  (and (:line a)
                       (contains? #{:anchor-not-found :anchor-misplaced}
                                  (get-in r [:error :reason]))
                       (<= 1 (:line a) n))
                  {:index (dec (:line a)) :stale? true}
                  :else r)))
        fr (one :from from)]
    (if (:error fr)
      fr
      (let [tr (if (or (nil? to) (= (str to) (str from))) fr (one :to to))]
        (if (:error tr)
          tr
          (let [fi (:index fr), ti (:index tr)]
            {:from-line (inc (min fi ti))
             :to-line (inc (max fi ti))
             :stale? (boolean (or (:stale? fr) (:stale? tr)))})))))))

(defn- edit-span
  "An anchored line range as a [start end] CHAR span in `content`, keeping the
  region's trailing terminator OUTSIDE the span so a replace never doubles a
  newline."
  [content line-start line-end]
  (let [start (char-offset-at-line content line-start)
        raw-end (char-offset-at-line content line-end)
        end (if (and (< raw-end (count content))
                     (pos? raw-end)
                     (= \newline (nth content (dec raw-end))))
              (dec raw-end)
              raw-end)
        ;; CRLF: the terminator is TWO characters. Dropping only the `\n`
        ;; leaves the `\r` inside the replaced region, which silently rewrites
        ;; that one line's ending and mixes endings in a CRLF file.
        end (if (and (> end start) (= \return (nth content (dec end)))) (dec end) end)]
    [start end]))

(defn apply-edits
  "Apply `edits` to `content` and return the new content, or
  `{:error {:reason KW …}}`.

  Each edit is `{:from anchor :to anchor? :replace text}`; `:to` defaults to
  `:from`, and an empty `:replace` deletes the lines outright rather than
  blanking them.

  ONE ATOMIC BATCH, and every edit resolves against the SAME read, so they may
  be listed in any order and cannot shift each other. Two edits touching one
  line are refused rather than merged, and ANY bad anchor refuses the WHOLE
  batch — a half-applied edit is the corruption this scheme exists to stop.

  A replacement need not end in a newline (the matched region's terminator is
  preserved), and one that does is not doubled — exactly one trailing
  terminator is dropped, so `\"…\\n\\n\"` still means \"and then a blank line\"."
  ([content edits] (apply-edits content edits (drift-tolerance)))
  ([content edits tolerance]
  (let [resolved (reduce (fn [acc {:keys [from to replace] :as e}]
                           (let [r (resolve-range content from to tolerance)]
                             (if (:error r)
                               (reduced r)
                               (conj acc (assoc r :replace (str replace) :edit e)))))
                         []
                         edits)]
    (cond
      (:error resolved) resolved

      ;; Overlap is checked ACROSS the batch, not per edit: two edits over one
      ;; line have no defined order and picking one silently would be a guess.
      ;; Pairs are compared BY POSITION, not by value — comparing spans and
      ;; skipping equal ones let two edits on the SAME line through, which is
      ;; the case this guard most exists for.
      (let [spans (mapv (juxt :from-line :to-line) resolved)]
        (some (fn [[i j]]
                (let [[a1 a2] (nth spans i)
                      [b1 b2] (nth spans j)]
                  (and (<= a1 b2) (<= b1 a2))))
              (for [i (range (count spans))
                    j (range (inc i) (count spans))]
                [i j])))
      {:error {:reason :anchor-spans-overlap
               :spans (mapv (juxt :from-line :to-line) resolved)}}

      :else
      (reduce (fn [s {:keys [from-line to-line replace]}]
                (let [line-start (dec from-line)]
                  (if (= "" replace)
                    ;; DELETION takes the whole physical line(s) INCLUDING the
                    ;; trailing newline, so the lines actually vanish. The
                    ;; keep-the-terminator-outside rule below is right for a
                    ;; replace and would leave blanks behind here.
                    (str (subs s 0 (char-offset-at-line s line-start))
                         (subs s (char-offset-at-line s to-line)))
                    (let [[start end] (edit-span s line-start to-line)
                          ends-nl? (and (= end (count s))
                                        (< start end)
                                        (= \newline (nth s (dec end))))
                          ends-crlf? (and ends-nl?
                                          (< start (dec end))
                                          (= \return (nth s (- end 2))))
                          body (cond
                                 (str/ends-with? replace "\r\n")
                                 (subs replace 0 (- (count replace) 2))
                                 (str/ends-with? replace "\n")
                                 (subs replace 0 (dec (count replace)))
                                 :else replace)]
                      (str (subs s 0 start)
                           (if ends-nl? (str body (if ends-crlf? "\r\n" "\n")) body)
                           (subs s end))))))
              content
              ;; Splice from the BOTTOM up so earlier offsets stay valid.
              (sort-by :from-line > resolved))))))
