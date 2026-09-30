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

(ns samizdat.agent.exam
  "Whether a write to a test file took assertions OUT of the exam.

  A run that cannot pass its tests can always pass them by changing them, and
  the change looks like ordinary work: run 170f4ec9 replaced four contract
  assertions with a bare call under a docstring explaining that the contract
  would come later, and shipped green. Nothing in the harness noticed.

  MEASURED BEFORE BUILT (karamazov-fgsb), over every run database on the
  machine: 22 runs, 190 writes to test files. Counting assertions only, the
  detector flags 6 events, 5 landed, 3 of them real — the four-assertion gutting
  above, an entire `blank-titles` deftest deleted as collateral in an edit that
  was repairing a different test, and one assertion reduced to its bare call.
  The two false alarms were a deduplicated line and two assertions replaced by
  one correct one; both were drops of exactly ONE, and both severe cases were
  drops of two or more. Hence `:min-drop`, and hence the default of 2: on that
  corpus it caught both severe cases with no false alarm, and a warning that
  cries wolf teaches a supervisor to skip the real one.

  DETECTION, NOT PREVENTION, by the same reasoning that settled M10 — and here
  with a number behind it: 100% precision over three true positives is not a
  mandate to start refusing writes, and the rule already misses a single-
  assertion deletion. `:refuse` exists in the policy so the decision can be
  revisited when more runs have accumulated; it is not the default.

  Pure. The caller supplies the texts; this namespace never reads a file
  and never writes a journal. The ratchet at `done` is further down."
  ;; samizdat.prompt first: it loads jolt.time, which data.json needs.
  (:require [samizdat.prompt :as prompt]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [samizdat.lexicon :as lexicon]))

(defn policy
  "The exam policy, gates.edn :exam-ratchet."
  []
  (lexicon/policy :exam-ratchet))

(defn test-path?
  "Whether `path` names part of the exam, by the vocabulary in
  wordlists.edn :exam :test-paths — regex fragments, because what marks a test
  file is a project's convention (`test/`, `_test.clj`, `test_x.py`, `.spec.ts`)
  and not something to compile in."
  ([path] (test-path? path (:test-paths (lexicon/wordlist :exam))))
  ([path patterns]
   (boolean (when-let [p (some-> path str not-empty)]
              (some #(re-find (re-pattern %) p) patterns)))))

(defn assertions
  "How many assertions `text` contains, by the vocabulary in
  wordlists.edn :exam :assertion-forms.

  Assertions ONLY, and deliberately not `deftest`: a test moved from one file
  to another loses no assertion, and counting the form that names a test made
  a helper extraction look like a gutting — which is the false positive that
  wasted the first pass of this measurement."
  ([text] (assertions text (:assertion-forms (lexicon/wordlist :exam))))
  ([text patterns]
   (let [s (str text)]
     (reduce + 0 (map #(count (re-seq (re-pattern %) s)) patterns)))))

(defn weakening
  "What this write does to the exam, or nil when it does nothing to it.

  `{:before n :after n :drop n}` when the file held more assertions before the
  write than after, and the drop reaches `:min-drop`. nil for a write that adds
  assertions, leaves them alone, or drops fewer than the policy cares about."
  ([path before after] (weakening path before after (policy)))
  ([path before after p]
   (when (and (test-path? path) (not= :off (:mode p)))
     (let [b (assertions before)
           a (assertions after)
           drop (- b a)]
       (when (>= drop (max 1 (:min-drop p)))
         {:before b :after a :drop drop})))))

(defn refuse?
  "Whether the policy says a weakening write does not land. `:detect` records
  and allows; `:refuse` records and declines."
  ([] (refuse? (policy)))
  ([p] (= :refuse (:mode p))))

;; --- the ratchet at done ------------------------------------------------------
;;
;; karamazov-fgsb, the owner's decision (2026-09-29): a run may change or
;; delete a test that existed when it started, and must SAY WHY. At `done`
;; the harness compares each changed test file with the run's baseline and
;; names every pre-existing test the tree no longer holds unchanged; the
;; answer explains each or `done` is refused. A false alarm costs one line
;; of explanation, not the work, which is what lets the detector be broad.
;;
;; WHOLE TEST FORMS, not assertion lines: on the 50 real edit_file hunks of
;; 2026-09-29 a per-line diff missed a data-table row deleted, a changed
;; `let` input and an assertion's continuation line. A Clojure file is read
;; as forms; a file that does not read (another language, a reader feature
;; this image lacks) falls back to its assertion lines.

(defn- clojure-source? [path]
  (some #(str/ends-with? (str path) %) (:clojure-exts (policy))))

(defn- normal
  "A form's printed text with reader gensyms folded: a fn literal reads with
  fresh `p__N#` names every time, and syntax quote with `x__N__auto`."
  [form]
  (str/replace (pr-str form) #"__\d+" "__"))

(defn test-forms
  "`{name normalized-text}` for the top-level forms of `text` whose head is
  one of policy's :test-def-heads (deftest, and def for the tables tests
  read), or nil when `text` does not read. Blank text is no forms."
  [text]
  (if (str/blank? (str text))
    {}
    (try
      (let [heads (set (:test-def-heads (policy)))
            forms (binding [*ns* (the-ns 'user) *read-eval* false]
                    (read-string (str "[" text "\n]")))]
        (into {} (keep (fn [f]
                         (when (and (seq? f) (symbol? (first f)) (contains? heads (name (first f)))
                                    (symbol? (second f)))
                           [(name (second f)) (normal f)])))
              forms))
      (catch Throwable _ nil))))

(defn- assertion-lines
  "The lines of `text` that hold an assertion, whitespace folded."
  [text]
  (let [pats (map re-pattern (:assertion-forms (lexicon/wordlist :exam)))]
    (into #{} (comp (filter (fn [l] (some #(re-find % l) pats)))
                    (map #(str/trim (str/replace % #"\s+" " "))))
          (str/split-lines (str text)))))

(defn touched
  "Every test that was in a file's `:before` and is not in the tree unchanged,
  over `files` — `[{:path :before :after}]`, the changed test files, before as
  the run found it (nil when new) and after as it stands (nil when deleted).

  `[{:path :test :kind :deleted|:changed :after-hash}]`. A test deleted from
  one file whose identical form is in another file's after MOVED and is not
  touched. A file that does not read is one entry, `:test nil`, when an
  assertion line it had is gone. `:after-hash` identifies what the test is
  now, so an explanation given for this state is recognised later in the run."
  [files]
  (let [read (mapv (fn [{:keys [path before after]}]
                     (let [b (when (clojure-source? path) (test-forms before))
                           a (when (clojure-source? path) (test-forms after))]
                       {:path path :before before :after after
                        :b b :a a :forms? (and (some? b) (some? a))}))
                   files)
        everywhere (into #{} (mapcat #(vals (:a %))) read)]
    (vec
     (mapcat
      (fn [{:keys [path before after b a forms?]}]
        (if forms?
          (keep (fn [[nm form]]
                  (let [now (get a nm)]
                    (when-not (or (= form now) (contains? everywhere form))
                      {:path path :test nm :kind (if now :changed :deleted)
                       :after-hash (hash (str now))})))
                (sort-by key b))
          (let [was (assertion-lines before)
                now (assertion-lines after)]
            (when (seq (remove now was))
              [{:path path :test nil :kind :changed
                :after-hash (hash (str/join "\n" (sort now)))}]))))
      read))))

(defn- field [m & ks]
  (some #(let [v (or (get m %) (get m (name %)))] (when (some? v) v)) ks))

(defn explanations
  "done's `changed_tests` argument as `{key reason}`, key a test's name or,
  for a file that does not read, its path. A vector of `{test|path, reason}`
  maps, a map of key to reason, or either as a JSON string; anything else is
  none."
  [raw]
  (let [raw (if (string? raw) (try (json/read-str raw) (catch Throwable _ nil)) raw)
        one (fn [k v] [(str/trim (str k))
                       (str/trim (str (if (map? v) (field v :reason :why :evidence) v)))])]
    (cond
      (map? raw) (into {} (map (fn [[k v]] (one (name k) v))) raw)
      (sequential? raw) (into {} (keep #(when (map? %)
                                          (when-let [k (field % :test :path :name)]
                                            (one k %))))
                              raw)
      :else {})))

(defn- key-of [{:keys [path test]}] (or test path))

(defn- bare
  "An explanation's key as a test name: a trailing `(file)` and a leading
  `ns/` dropped, lower-cased. Run 582980ef's model named the same test as
  `flight.game-test/...` and as `... (game_test.clj)`."
  [k]
  (-> (str k) (str/replace #"\s*\(.*\)\s*$" "") (str/replace #"^.*/(?=[^/]+$)" "")
      str/trim str/lower-case))

(defn- reason-for
  "The reason `explained` gives for touched test `t`: its exact key, else an
  entry whose bare name is the test's."
  [explained t]
  (or (not-empty (get explained (key-of t)))
      (when-let [nm (:test t)]
        (some (fn [[k r]] (when (and (= (str/lower-case nm) (bare k)) (not (str/blank? r))) r))
              explained))))

(defn unexplained
  "The `touched` tests with no non-blank reason in `explained` and not in
  `prior` — the `[path test after-hash]` triples explained earlier in the run
  for the same state of the test."
  ([touched explained] (unexplained touched explained #{}))
  ([touched explained prior]
   (filterv (fn [t]
              (and (str/blank? (reason-for explained t))
                   (not (contains? prior [(:path t) (:test t) (:after-hash t)]))))
            touched)))

(defn explained-record
  "What a ship journals so a later branch is not asked again: each touched
  test with its reason."
  [touched explained prior]
  (vec (keep (fn [t]
               (when-let [r (not-empty (reason-for explained t))]
                 (assoc (select-keys t [:path :test :kind :after-hash]) :reason r)))
             touched)))

(defn refusal [missing]
  (prompt/render "tests-unexplained"
                 {:missing (mapv #(assoc % :key (key-of %) :kind (name (:kind %))) missing)}))

(defn render [touched explained prior]
  (prompt/render "tests-explained"
                 {:rows (vec (keep (fn [t] (when-let [r (not-empty (reason-for explained t))]
                                             (assoc t :key (key-of t) :kind (name (:kind t)) :reason r)))
                                   touched))}))

(defn describe
  "One line for the record: what the file lost."
  [path {:keys [before after drop]}]
  (str (str/trim (str path)) ": " before " assertion(s) before, " after " after"
       " (" drop " removed)"))
