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

(ns samizdat.metrics
  "Code-quality heuristics, as pure arithmetic over source text.

  The measures come from SlopCodeBench (by way of the user's brief): they
  separate established codebases from LLM-slop well, and the model that writes
  the code is the one that should be held to them.

    - cyclomatic complexity CC(f): decision points + 1, per function.
    - erosion: how much of a codebase's mass sits in a few large, complex
      functions. mass(f) = CC(f) * sqrt(SLOC(f)); erosion is the fraction of
      total mass held by functions whose CC exceeds a limit.
    - verbosity: duplicated / unnecessarily-verbose lines over LOC. The paper
      unions AST-grep-flagged lines with clone lines; ast-grep is not reachable
      in this process (no in-process linter), so this is CLONE-ONLY — the
      duplication half, honestly labelled. Do not read it as ast-grep parity.

  MECHANISM ONLY (AGENTS.md): every function here is pure and takes source text
  and its thresholds as arguments. Nothing decides WHEN to measure or what a
  breach means — that is a cell's business, and the numbers live in
  gates.edn :code-quality. Pure computation does not park, so map/mapv are fine.

  Reuses the reader path `samizdat.agent.stubs` already relies on and the
  lexer-aware skipping `samizdat.lisp/scan` documents; the clone term folds the
  same content hash `samizdat.hashline/line-hash` uses for its anchors."
  (:require [clojure.string :as str]
            [samizdat.hashline :as hashline]))

;;; ------------------------------------------------------------ line accounting

(defn- blank? [line] (str/blank? (str line)))

(defn- comment-only?
  "A line whose first non-whitespace character is `;` — a comment, no code."
  [line]
  (str/starts-with? (str/triml (str line)) ";"))

(defn- code-line?
  "A line that carries code: not blank, not comment-only."
  [line]
  (and (not (blank? line)) (not (comment-only? line))))

(defn loc
  "Lines of code in `source`: non-blank, non-comment lines. The denominator of
  verbosity and the size a diff is measured in."
  [source]
  (count (filter code-line? (hashline/content-lines source))))

;;; ---------------------------------------------------- top-level form spans

(def ^:private opener->closer {\( \), \[ \], \{ \}})

(defn top-level-spans
  "Every top-level LIST in `source`, as `{:text :start :end}` character spans.

  A depth-0 lexer scan: it skips strings, char literals (`\\x`) and line
  comments exactly as `samizdat.lisp/scan` does, so a paren inside a string or
  a `;` comment never opens a form. Only lists are returned — every `def*` is a
  list, and a bare top-level symbol or number defines nothing to measure.

  Char offsets, not forms, because the reader under jolt attaches no line
  metadata (nothing in the tree uses a LineNumberingPushbackReader); the caller
  turns a span into a line count textually, which is what SLOC actually is."
  [source]
  (let [s (str source)
        n (count s)]
    (loop [i 0, depth 0, start nil, out []]
      (if (>= i n)
        out
        (let [c (nth s i)]
          (cond
            ;; char literal: backslash consumes the next char (may be a paren).
            (= c \\) (recur (+ i 2) depth start out)

            ;; string: jump past it, honouring \" escapes.
            (= c \")
            (recur (loop [j (inc i)]
                     (cond
                       (>= j n) n
                       (= (nth s j) \\) (recur (+ j 2))
                       (= (nth s j) \") (inc j)
                       :else (recur (inc j))))
                   depth start out)

            ;; line comment: skip to the newline.
            (= c \;)
            (recur (let [nl (str/index-of s "\n" i)] (if nl nl n)) depth start out)

            (opener->closer c)
            (recur (inc i) (inc depth) (if (zero? depth) i start) out)

            (#{\) \] \}} c)
            (let [depth' (max 0 (dec depth))]
              (if (and (= depth' 0) start)
                (recur (inc i) 0 nil (conj out {:text (subs s start (inc i))
                                                :start start :end (inc i)}))
                (recur (inc i) depth' start out)))

            :else (recur (inc i) depth start out)))))))

;;; ----------------------------------------------------------------- SLOC / CC

(def ^:private def-heads
  "Head symbols that introduce a top-level definition — the forms we measure."
  '#{def defn defn- defmacro defmulti defmethod defprotocol definterface
     defrecord deftype defonce})

(def ^:private nested-def-heads
  "Definitions whose body is NOT the enclosing function's own logic, so their
  decision points do not count toward it. An inline `fn`/`fn*` is deliberately
  ABSENT: a lambda's branches are part of the function that carries it."
  '#{defn defn- defmacro defmethod defmulti definterface defprotocol
     defrecord deftype})

(def default-decision-heads
  "The forms that introduce a branch, for CC = 1 + branch count. A default; the
  cell passes gates.edn :code-quality :decision-heads so the set is tunable."
  '#{if if-not if-let if-some when when-not when-let when-some when-first
     cond condp case and or while for doseq})

(defn- head-of [form] (when (and (seq? form) (symbol? (first form))) (first form)))

(defn cyclomatic
  "Cyclomatic complexity of `form`: 1 + the number of decision points in it.

  Walks the form with `tree-seq`, descending through everything except the
  bodies of NESTED named definitions (a `defn` inside a `defn` is its own
  function); inline lambdas are walked, since their branches belong to the
  function. `decision-heads` names what counts as a branch."
  ([form] (cyclomatic form default-decision-heads))
  ([form decision-heads]
   (let [heads (set decision-heads)
         ;; The root is always descended even though it is itself a def — it is
         ;; the function we are measuring; only defs NESTED below it are opaque.
         descend? (fn [x] (or (identical? x form)
                              (and (coll? x)
                                   (not (and (seq? x)
                                             (symbol? (first x))
                                             (nested-def-heads (symbol (name (first x)))))))))
         nodes (rest (tree-seq descend? seq form))]   ; rest: not the form itself
     (->> nodes
          (filter (fn [x] (when-let [h (head-of x)] (heads (symbol (name h))))))
          count
          inc))))

(defn- text-sloc
  "Source lines of code in a form's `text`: non-blank, non-comment lines."
  [text]
  (count (filter code-line? (str/split-lines (str text)))))

(defn fn-metrics
  "Per-definition metrics for `source`: a vector of `{:name :cc :sloc :mass}`,
  one per top-level `def*` list that reads. `mass = cc * sqrt(sloc)` — the
  SlopCodeBench weight that lets one huge, tangled function outweigh many small
  clean ones. A span that will not read is skipped, not fatal (a parent writing
  a half-formed file must get a refusal elsewhere, not an exception here)."
  ([source] (fn-metrics source default-decision-heads))
  ([source decision-heads]
   (into []
         (keep (fn [{:keys [text]}]
                 (let [form (try (read-string {:read-cond :allow} text)
                                 (catch Throwable _ nil))
                       h (head-of form)]
                   (when (and h (def-heads (symbol (name h))))
                     (let [cc (cyclomatic form decision-heads)
                           sloc (text-sloc text)]
                       {:name (str (when (symbol? (second form)) (second form)))
                        :cc cc
                        :sloc sloc
                        :mass (* (double cc) (Math/sqrt (double (max 1 sloc))))})))))
         (top-level-spans source))))

(defn erosion
  "The fraction of a codebase's mass held by functions whose CC exceeds
  `cc-limit`. 0 when there is no mass to divide (an empty or trivial file is
  not eroded). Given per-function metrics rather than source, so a report can
  pool functions across several files before dividing."
  [fn-metrics cc-limit]
  (let [total (reduce + 0.0 (map :mass fn-metrics))]
    (if (zero? total)
      0.0
      (/ (reduce + 0.0 (map :mass (filter #(> (:cc %) cc-limit) fn-metrics)))
         total))))

;;; ------------------------------------------------------------------- clones

(defn- code-line-index
  "The code lines of `source` as `[[line-number hash] …]`, dropping blank and
  comment-only lines. 1-based line numbers, so a clone can be located."
  [source]
  (into []
        (comp (map-indexed (fn [i line] [(inc i) line]))
              (filter (fn [[_ line]] (code-line? line))))
        (hashline/content-lines source)))

(defn clone-windows
  "Sliding windows of `min-block` consecutive CODE lines in `source`, each as
  `[window-hash [line-numbers…]]`. A window is the unit of clone detection: a
  single repeated line (a closing paren, a common require) is not a clone, a
  repeated BLOCK is."
  [source min-block]
  (let [idx (code-line-index source)]
    (when (>= (count idx) min-block)
      (mapv (fn [win]
              [(str/join (map (comp hashline/line-hash second) win))
               (mapv first win)])
            (partition min-block 1 idx)))))

(defn clone-lines
  "The count of distinct code lines that sit inside a duplicated block, across
  the whole corpus `path->source`. A window (a run of `min-block` code lines)
  is a clone when its content hash appears more than once anywhere in the
  corpus — within a file or copy-pasted between files. Windows never cross a
  file boundary, so a coincidental seam is not a clone."
  [path->source min-block]
  (let [per-file (into {} (map (fn [[p s]] [p (or (clone-windows s min-block) [])]))
                       path->source)
        freq (frequencies (mapcat (fn [ws] (map first ws)) (vals per-file)))]
    (reduce
     (fn [acc [p windows]]
       (+ acc (count (into #{}
                           (comp (filter (fn [[h _]] (> (long (get freq h 0)) 1)))
                                 (mapcat second)
                                 (map (fn [ln] [p ln])))
                           windows))))
     0
     per-file)))

;;; ------------------------------------------------------------------- report

(defn report
  "The three heuristics over a corpus `path->source` (path → file text), given
  the `:code-quality` config map. Pure. `:hotspots` is the worst functions by
  mass, so a finding can name them rather than a bare ratio."
  [path->source cfg]
  ;; No fallback numbers: the thresholds live in gates.edn :code-quality and
  ;; nowhere else (the rule files.clj/max-read-chars states). A cfg missing a
  ;; key is a broken table, and letting it throw says so.
  (let [heads (or (some-> (:decision-heads cfg) (->> (map symbol) set))
                  default-decision-heads)
        cc-limit (long (:cc-fn-limit cfg))
        min-block (long (:clone-min-forms cfg))
        per-fn (into [] (mapcat (fn [[_ s]] (fn-metrics s heads))) path->source)
        total-loc (reduce + 0 (map (fn [[_ s]] (loc s)) path->source))
        clones (clone-lines path->source min-block)]
    {:loc total-loc
     :verbosity (if (zero? total-loc) 0.0 (/ (double clones) total-loc))
     :erosion (erosion per-fn cc-limit)
     :max-cc (reduce max 0 (map :cc per-fn))
     :clone-lines clones
     :hotspots (->> per-fn
                    (filter #(> (:cc %) cc-limit))
                    (sort-by (comp - :mass))
                    (take 5)
                    vec)}))

(defn- fmt [x] (format "%.2f" (double x)))

(defn findings
  "STRUCTURED code-quality findings for a `report`, as a vector of maps, or `[]`
  when the code is within its limits. DATA, not prose: the sentences the model
  reads live in prompts/metrics-findings.md, which a cell renders — src holds
  no model-facing text (AGENTS.md). Each finding carries a `:severity` tag, one
  kind flag (`:erosion`/`:verbosity`/`:complexity`) the template switches on,
  and the numbers to interpolate.

  ADVISORY vs BLOCKING is the distance over the line: past the `-max` ceiling is
  `medium` (advisory); past the `-block` ceiling (the agent-level number the
  paper measured) is `high`, which blocks — the article's caveat is that
  hard-optimizing the metric destroys it, so only the egregious case refuses.
  Excess cyclomatic complexity is `low`: erosion already prices it, and the
  hotspot list is a pointer, not a gate. Every threshold comes from `cfg`
  (gates.edn :code-quality); one that is absent simply does not fire."
  [report cfg]
  (let [{:keys [verbosity erosion hotspots]} report
        {:keys [verbosity-max verbosity-block erosion-max erosion-block cc-fn-limit]} cfg
        over? (fn [x limit] (and (some? limit) (> (double x) (double limit))))]
    (cond-> []
      (over? erosion erosion-block)
      (conj {:severity "high" :erosion true :value (fmt erosion) :limit (fmt erosion-block)})
      (and (over? erosion erosion-max) (not (over? erosion erosion-block)))
      (conj {:severity "medium" :erosion true :value (fmt erosion) :limit (fmt erosion-max)})

      (over? verbosity verbosity-block)
      (conj {:severity "high" :verbosity true :value (fmt verbosity) :limit (fmt verbosity-block)})
      (and (over? verbosity verbosity-max) (not (over? verbosity verbosity-block)))
      (conj {:severity "medium" :verbosity true :value (fmt verbosity) :limit (fmt verbosity-max)})

      (seq hotspots)
      (conj {:severity "low" :complexity true :limit cc-fn-limit :count (count hotspots)
             :fns (str/join ", " (map (fn [h] (str (:name h) " (cc " (:cc h) ")")) hotspots))}))))

(defn review
  "The STRUCTURED code-quality findings for a corpus `path->source`, or `[]`
  when it is within its limits — `report` then `findings` in one call, so a
  critic cell computes the whole heuristic once and then renders the result
  through prompts/metrics-findings.md. Pure; the cell supplies the changed
  sources and the `:code-quality` config."
  [path->source cfg]
  (if (seq path->source)
    (findings (report path->source cfg) cfg)
    []))
