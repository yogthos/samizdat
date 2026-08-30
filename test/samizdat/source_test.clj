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

(ns samizdat.source-test
  "The one gate every piece of model-authored Clojure passes through.

  There were three behaviours and no rule: write_file repaired, edit_file
  refused, and eval did NOTHING — a live run lost 2 of its first 17 turns to
  `Eval error: Unmatched delimiter: )` with no line, no column, and no repair,
  while the identical text written to a file would have been closed for it.

  The rule is one question: IS THERE PRE-EXISTING CODE THAT AUTO-CLOSING COULD
  RE-PARENT? Text wholly authored in this call (a whole-file write, an eval
  form) has none, so repairing it can only complete what the model was
  writing. Text landing inside a file the model did not write this turn does,
  so it is refused instead. Diagnosis is unconditional either way: it is pure,
  it changes nothing, and no path has an excuse to hand back a bare reader
  message."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [samizdat.agent.source :as source]
            [samizdat.agent.tools.base :as base]
            [samizdat.agent.tools.repl]
            [samizdat.lisp]))

(deftest wholly-authored-text-is-repaired
  (testing "a trailing truncation is closed and the repair is reported"
    (let [r (source/vet "(defn f [] (+ 1 2)" {:whole? true})]
      (is (= "(defn f [] (+ 1 2))" (:code r)))
      (is (nil? (:problem r)))
      (is (= :auto-closed (get-in r [:repaired :reason])))))
  (testing "a trailing over-close is trimmed"
    (is (= "(inc 1)" (:code (source/vet "(inc 1)))" {:whole? true})))))
  (testing "text that already loads passes through untouched"
    (let [r (source/vet "(defn f [] 1)" {:whole? true})]
      (is (= "(defn f [] 1)" (:code r)))
      (is (nil? (:repaired r)))
      (is (nil? (:problem r))))))

(deftest text-landing-in-existing-code-is-never-repaired
  (testing "the same truncation that gets closed above is refused here"
    (let [r (source/vet "(defn f [] (+ 1 2)" {:whole? false})]
      (is (nil? (:code r)) "no repaired text is offered")
      (is (= :unclosed (get-in r [:problem :reason])))))
  (testing "because closing it would re-parent the forms around it"
    (is (some? (:problem (source/vet "(f)) (g)" {:whole? false}))))))

(deftest a-problem-always-carries-a-position-and-a-sentence
  (doseq [[label text] [["mismatch" "(defn f [] (vec [1 2)])"]
                        ["stray" "(f)) (g)"]
                        ["unclosed" "(defn f [] (+ 1 2)"]]]
    (testing label
      (let [r (source/vet text {:whole? false})]
        (is (integer? (get-in r [:problem :line])) "a line, not a char offset")
        (is (string? (:note r)) "and a sentence rendered from resources")
        (is (re-find #"line \d+" (:note r))))))
  (testing "balanced-but-unreadable says what the reader said"
    (let [r (source/vet "(def x :)" {:whole? true})]
      (is (= :does-not-read (get-in r [:problem :reason])))
      (is (re-find #"(?i)invalid token" (:note r)))))
  (testing "an unterminated string points at the line that opened it"
    (let [r (source/vet "(ns d)\n(def a \"oops\n(def b 2)\n" {:whole? true})]
      (is (= :unterminated-string (get-in r [:problem :reason])))
      (is (str/includes? (:note r) "line 2")))))

(deftest a-repair-that-would-not-read-is-refused-not-returned
  ;; closing "(def x :" gives "(def x :)" — balanced, and not Clojure.
  (let [r (source/vet "(def x :" {:whole? true})]
    (is (nil? (:code r)))
    (is (some? (:problem r)))))

;; --- the caveats, so karamazov-2d3 cannot come back ---------------------------
;; That bug was three failures stacked: it wrote the file it had broken, scored
;; it as progress, and announced a repair it never applied. The third came from
;; asking balance's "could this be repaired" question at a site that REFUSES.
;; Unifying the paths is exactly the change that could reintroduce it, so the
;; shape has to make each one impossible rather than merely absent.

(deftest code-and-problem-are-mutually-exclusive
  ;; THE structural guard. A caller cannot write or eval broken text even by
  ;; mistake, because when there is a problem there is no :code to reach for.
  (doseq [whole? [true false]
          text ["(defn f [] 1)"            ; loads
                "(defn f [] (+ 1 2)"       ; repairable when whole
                "(f)) (g)"                 ; never repairable
                "(def x :)"                ; balanced, unreadable
                "(defn f [] (vec [1 2)])"  ; mismatched
                "(def s \"unclosed"        ; unterminated string
                ""]]
    (let [r (source/vet text {:whole? whole?})]
      (is (not (and (:code r) (:problem r)))
          (str "both :code and :problem for " (pr-str text) " whole?=" whole?))
      (is (or (:code r) (:problem r))
          (str "neither :code nor :problem for " (pr-str text) " whole?=" whole?)))))

(deftest a-refusal-never-claims-a-repair
  ;; The exact sentence the model was handed for a file nothing had been
  ;; appended to: "auto-closed 1 unclosed delimiter(s) … appended `)`".
  (doseq [text ["(defn f [] (+ 1 2)" "(inc 1)))" "(f)) (g)" "(def x :)"]]
    (let [r (source/vet text {:whole? false})]
      (is (some? (:problem r)) (pr-str text))
      (is (not (re-find #"(?i)auto-clos|auto-remov|appended" (str (:note r))))
          (str "refusal note claims a repair: " (pr-str (:note r))))
      (is (nil? (:repaired r))
          "a refusing verdict must never carry a repair verdict at all"))))

(deftest the-refusing-path-never-asks-the-repair-question
  ;; Not just "does not report a repair" — must not COMPUTE one. `(inc 1)))`
  ;; is trivially repairable by trimming, and the refusing path must still
  ;; answer with the problem rather than the trimmed text.
  (let [r (source/vet "(inc 1)))" {:whole? false})]
    (is (nil? (:code r)))
    (is (= :stray (get-in r [:problem :reason])))))

(deftest a-repair-is-reported-only-when-the-text-actually-changed
  (testing "unchanged text carries no repair note"
    (let [r (source/vet "(defn f [] 1)" {:whole? true})]
      (is (nil? (:repaired r)))
      (is (nil? (:note r)))))
  (testing "changed text carries one, and the code returned IS the repaired code"
    (let [r (source/vet "(defn f [] (+ 1 2)" {:whole? true})]
      (is (some? (:repaired r)))
      (is (not= "(defn f [] (+ 1 2)" (:code r)))
      (is (str/includes? (:note r) "auto-closed"))))
  (testing "whatever :code says, it loads"
    (doseq [text ["(defn f [] (+ 1 2)" "(inc 1)))" "(defn f [] 1)"]]
      (let [r (source/vet text {:whole? true})]
        (when-let [c (:code r)]
          (is (nil? (samizdat.lisp/diagnose c))
              (str "vet returned code that does not load: " (pr-str c))))))))

(deftest non-clojure-text-is-none-of-this-namespaces-business
  (testing "vet is only asked about Clojure; callers decide what is Clojure"
    (is (= "not { clojure ((" (:code (source/vet "not { clojure ((" {:whole? true
                                                                     :clojure? false}))))
    (is (nil? (:problem (source/vet "((((" {:whole? true :clojure? false}))))))

;; --- the eval path, which is why this namespace exists ------------------------

(deftest eval-repairs-and-diagnoses-like-every-other-path
  ;; :role :supervisor keeps this in the HARNESS image, which is what these
  ;; assertions are about — the syntax gate runs before routing either way, but
  ;; a project-image eval would spawn a subprocess for what is a unit test of
  ;; string repair. Since karamazov-zrq the eval tool's image depends on the
  ;; ctx's role, and a ctx carrying neither role nor root is now refused
  ;; outright rather than confined to nothing.
  (let [run (fn [code]
              (base/run-tool {:branch {:id "B1"} :tool-name "eval"
                              :role :supervisor
                              :args {:code code}}))]
    (testing "a dropped trailing closer is repaired and the form runs"
      (let [r (run "(+ 1 2")]
        (is (= :neutral (:category r)) (:result r))
        (is (str/includes? (:result r) "=> 3"))
        (is (str/includes? (:result r) "auto-closed")
            "and the model is TOLD, so it learns rather than dropping it again")))
    (testing "an imbalance the harness must not guess at is refused with a position"
      (let [r (run "(f)) (g)")]
        (is (= :mechanics (:category r)) "a malformed call, not a failed evaluation")
        (is (re-find #"line \d+, col \d+" (:result r)))
        (is (not (str/includes? (:result r) "Unmatched delimiter"))
            "the bare reader message is exactly what this replaced")))
    (testing "balanced-but-unreadable never reaches the reader"
      (is (= :mechanics (:category (run "(def x :)")))))
    (testing "ordinary runtime errors are still failures, not mechanics"
      (let [r (run "(ffirst-no-such-fn)")]
        (is (= :failure (:category r)))))
    (testing "good code is untouched and carries no harness note"
      (let [r (run "(* 6 7)")]
        (is (str/includes? (:result r) "=> 42"))
        (is (not (str/includes? (:result r) "[harness]")))))))

(deftest the-advice-matches-the-actual-problem
  ;; Run ace34d83: the model wrote "\*\*(.+?)\*\*" — an invalid Clojure string
  ;; escape — and every refusal appended "the harness closes a dropped trailing
  ;; delimiter for you", which is advice about parens. It retried the same form
  ;; six times and the branch ended there. A refusal that misdirects is worse
  ;; than one that only states the error.
  (let [run (fn [code] (base/run-tool {:branch {:id "B1"} :tool-name "eval"
                                       :args {:code code}}))]
    (testing "a bad string escape is not given delimiter advice"
      (let [r (run "(re-find #\"x\" \"\\*\")")]
        (is (= :mechanics (:category r)))
        (is (not (str/includes? (:result r) "trailing delimiter"))
            "delimiter advice on a token error is what caused the loop")
        (is (str/includes? (:result r) "TOKEN")
            "it names the real class of problem")))
    (testing "a genuine mid-file delimiter problem still gets delimiter advice"
      (let [r (run "(f)) (g)")]
        (is (= :mechanics (:category r)))
        (is (str/includes? (:result r) "trailing delimiter"))))))

(deftest an-eval-error-always-says-something
  ;; Run bd56a286 turns 57 and 60: the model was handed `Eval error: ` — the
  ;; label and nothing after it. `(or (ex-message e) (str e))` falls through on
  ;; nil but NOT on "", so an exception with a blank message produced a
  ;; completely empty diagnosis. An unactionable message is the failure mode
  ;; this whole area has been about.
  ;; :role :supervisor so this actually EVALUATES. Without it the ctx carries
  ;; no root, the router refuses, and the assertion below passes on the
  ;; refusal's text instead of on the blank-message diagnosis it is about.
  (let [r (base/run-tool {:branch {:id "B1"} :tool-name "eval"
                          :role :supervisor
                          :args {:code "(throw (ex-info \"\" {}))"}})]
    (is (= :failure (:category r)))
    (is (re-find #"\S" (str/replace (:result r) #"(?i)eval error:" ""))
        (str "nothing after the label: " (pr-str (:result r))))))
