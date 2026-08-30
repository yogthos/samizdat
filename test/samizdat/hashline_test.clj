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

(ns samizdat.hashline-test
  "Anchored addressing: `<line>:<hash>` coordinates that a read MINTS and an
  edit SPENDS, so the model never has to reproduce the text it is replacing.
  Ported from vis (karamazov-0kk); the refusal ladder is the part that matters."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [samizdat.hashline :as hl]))

(def ^:private sample
  (str/join "\n" ["(ns demo)"
                  ""
                  "(defn f [x]"
                  "  (+ x 1))"
                  ""
                  "(defn g [x]"
                  "  (* x 2))"]))

(deftest the-hash-matches-vis-bit-for-bit
  ;; vis masks the TRIMMED String.hashCode to 12 bits. The recurrence
  ;; h = 31h + c is closed under mod 2^12, so folding mod 4096 reproduces it
  ;; exactly without any 32-bit wraparound — and without a JVM. The witness is
  ;; vis's own documented anchor, `4395:573│ (defn- patch-edit-rows`.
  (is (= "573" (hl/line-hash "(defn- patch-edit-rows")))
  (is (= "0b3" (hl/line-hash "(ns demo)")))
  (is (= "078" (hl/line-hash "x")))
  (testing "a blank line hashes to 000"
    (is (= "000" (hl/line-hash "")))
    (is (= "000" (hl/line-hash "     "))))
  (testing "the hash is of the TRIMMED line, so indentation drift does not move it"
    (is (= (hl/line-hash "(f x)") (hl/line-hash "      (f x)"))))
  (testing "always three hex chars"
    (doseq [s ["a" "" "zzz" "(defn f [] 1)" "  \t "]]
      (is (re-matches #"[0-9a-f]{3}" (hl/line-hash s))))))

(deftest rendering-produces-lines-that-go-straight-back-in
  (let [block (hl/render sample 1 3)]
    (is (= ["1:0b3│ (ns demo)"
            "2:000│ "
            "3:217│ (defn f [x]"]
           (str/split-lines block))
        "one addressable line per source line")
    (testing "every rendered line parses back to the anchor that minted it"
      (doseq [line (str/split-lines block)]
        (is (hl/anchor-string? line))
        (is (= (first (str/split line #"│")) (hl/anchor-token line)))))))

(deftest an-anchor-is-cut-out-of-whatever-the-model-actually-sent
  (testing "a bare token"
    (is (= "12:abc" (hl/anchor-token "12:abc"))))
  (testing "a whole rendered line — the gutter and text are DECORATION"
    (is (= "12:abc" (hl/anchor-token "12:abc│ (defn f [] 1)"))))
  (testing "quotes that arrived with the JSON string"
    (is (= "12:abc" (hl/anchor-token "\"12:abc\"")))
    (is (= "12:abc" (hl/anchor-token "  '12:abc'  "))))
  (testing "text that is not an anchor at all"
    (is (not (hl/anchor-string? "(defn f [] 1)")))
    (is (not (hl/anchor-string? "12")))
    (is (not (hl/anchor-string? "abc")))))

(deftest the-refusal-ladder
  (testing "1. exact — the stated line still hashes to the anchor"
    (is (= {:from-line 3 :to-line 3}
           (hl/resolve-range sample "3:217" nil))))

  (testing "2. drifted — the line moved a little, follow the CONTENT"
    (let [shifted (str "(def added 1)\n(def added 2)\n" sample)]
      (is (= {:from-line 5 :to-line 5}
             (hl/resolve-range shifted "3:217" nil))
          "the anchor said 3, the content now sits at 5, within tolerance")))

  (testing "3. line wins — a duplicate hash does NOT make an exact anchor ambiguous"
    (let [dup (str/join "\n" ["(f x)" "(g x)" "(f x)"])
          h (hl/line-hash "(f x)")]
      (is (= {:from-line 1 :to-line 1} (hl/resolve-range dup (str "1:" h) nil)))
      (is (= {:from-line 3 :to-line 3} (hl/resolve-range dup (str "3:" h) nil)))))

  (testing "4. misplaced — the content sits FAR from the stated line, so REFUSE"
    (let [long-file (str/join "\n" (concat (repeat 200 "(def filler 1)")
                                           ["(def needle 42)"]))
          h (hl/line-hash "(def needle 42)")
          r (hl/resolve-range long-file (str "1:" h) nil)]
      (is (= :anchor-misplaced (get-in r [:error :reason]))
          "a false refuse costs one re-read; a false accept corrupts the file")
      (is (= [201] (get-in r [:error :found-lines])))
      (is (= (str "201:" h) (get-in r [:error :current-anchor]))
          "and hands back the right anchor so recovery is ONE call, not a re-read")))

  (testing "5. not-found — the content is gone; refuse, but say what is there now"
    (let [r (hl/resolve-range sample "3:fff" nil)]
      (is (= :anchor-not-found (get-in r [:error :reason])))
      (is (= "3:217" (get-in r [:error :current-anchor])))
      (is (= "(defn f [x]" (get-in r [:error :current-text])))))

  (testing "a malformed anchor is refused — both coordinates are required"
    (is (= :anchor-malformed (get-in (hl/resolve-range sample "9a5" nil) [:error :reason])))
    (is (= :anchor-malformed (get-in (hl/resolve-range sample "abc:def" nil) [:error :reason]))))

  (testing "a line outside the file is refused"
    (is (= :anchor-line-out-of-range
           (get-in (hl/resolve-range sample "99:000" nil) [:error :reason]))))

  (testing "an inverted range is refused"
    (is (= :anchor-range-inverted
           (get-in (hl/resolve-range sample "6:998" "3:217") [:error :reason])))))

(deftest a-read-is-tolerant-where-a-write-refuses
  ;; A read is non-destructive, so a stale hash must not block the LOOK the way
  ;; it (correctly) blocks the write.
  (testing "a stale hash still shows the line, and says it is stale"
    (let [r (hl/resolve-range-read sample "3:fff" nil)]
      (is (= 3 (:from-line r)))
      (is (:stale? r))))
  (testing "a good anchor is not stale"
    (is (not (:stale? (hl/resolve-range-read sample "3:217" nil)))))
  (testing "but a genuinely unlocatable anchor is still an error"
    (is (:error (hl/resolve-range-read sample "9a5" nil)))
    (is (:error (hl/resolve-range-read sample "99:000" nil)))))

(deftest an-edit-span-splices-without-disturbing-its-neighbours
  (testing "a single line is replaced, its terminator preserved"
    (is (= (str/replace sample "(ns demo)" "(ns other)")
           (hl/apply-edits sample [{:from "1:0b3" :replace "(ns other)"}]))))
  (testing "a replacement carrying its own newline does not grow a blank line"
    (is (= (str/replace sample "(ns demo)" "(ns other)")
           (hl/apply-edits sample [{:from "1:0b3" :replace "(ns other)\n"}]))))
  (testing "a multi-line span collapses to one line"
    (is (= "(ns demo)\n\n(defn f [x] (inc x))\n\n(defn g [x]\n  (* x 2))"
           (hl/apply-edits sample [{:from "3:217" :to "4:80c"
                                    :replace "(defn f [x] (inc x))"}]))))
  (testing "an empty replacement removes the lines rather than blanking them"
    (is (= "(ns demo)\n\n\n(defn g [x]\n  (* x 2))"
           (hl/apply-edits sample [{:from "3:217" :to "4:80c" :replace ""}]))))
  (testing "several edits in ONE call all resolve against the SAME read"
    ;; listed out of order on purpose — they must not shift each other
    (is (= "(ns other)\n\n(defn f [x]\n  (+ x 1))\n\n(defn g [x]\n  (* x 9))"
           (hl/apply-edits sample [{:from "7:08c" :replace "  (* x 9))"}
                                   {:from "1:0b3" :replace "(ns other)"}]))))
  (testing "two edits over the same line are refused, not silently merged"
    (is (:error (hl/apply-edits sample [{:from "1:0b3" :replace "a"}
                                        {:from "1:0b3" :replace "b"}]))))
  (testing "one bad anchor refuses the WHOLE batch — nothing is half-written"
    (let [r (hl/apply-edits sample [{:from "1:0b3" :replace "(ns other)"}
                                    {:from "99:000" :replace "nope"}])]
      (is (:error r))
      (is (nil? (:content r))))))
