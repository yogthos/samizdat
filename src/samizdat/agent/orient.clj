;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.agent.orient
  "What the problem statement already names, found in the tree before the
  first turn (karamazov-fp21.3).

  WHY. Across the 24 campaign dbs, 108 branches spent 1610 turns in
  orientation — reading before their first write, patch or plan — and 35% of
  those turns read or grepped a name the problem statement had given
  verbatim: `render-frame`, src/fps/core.clj, fps.input/sample. ATLAS
  (proxy/symbol_index.go) puts the definitions of such names into the
  opening context and credits it with the turns a model otherwise spends
  finding app.py. This is that, bounded, and measured as an arena arm:
  gates.edn :orient-inject :enabled? is the switch.

  WHAT IT IS NOT. Not a parser and not an index: a bounded walk of the tree
  and a regex per definition shape, both policy. A snippet says where to
  look, not what is there now — the block says so in its own words
  (prompts/orient-inject.md) — and it is computed ONCE per run and kept on
  the run row, so a resume reopens on the opening the branches saw rather
  than on whatever the tree says by then.

  Mechanism only: every number, pattern and word is gates.edn :orient-inject
  or the prompt — there are no defaults here, so a policy missing a key
  fails at the walk rather than quietly running under a number nobody
  wrote down."
  (:require [clojure.string :as str]
            [jolt.fs :as fs]
            [samizdat.prompt :as prompt]))

;; --- the names ---------------------------------------------------------------

(defn problem-names
  "The names a problem statement points at, distinct, in the order the
  policy's `:name-patterns` find them — the most explicit shape first. A
  pattern's first group is the name (the whole match when it has none);
  names under `:min-chars` are dropped; at most `:max-names`."
  [problem {:keys [name-patterns max-names min-chars]}]
  (let [hits (for [pat name-patterns
                   m (re-seq (re-pattern pat) (str problem))]
               (str/trim (str (if (vector? m) (or (second m) (first m)) m))))]
    (->> hits
         (remove str/blank?)
         (filter #(>= (count %) min-chars))
         distinct
         (take max-names)
         vec)))

;; --- the tree ----------------------------------------------------------------

(defn- under-root
  "`p` canonicalised, when it is under `root`; nil when it escapes."
  [root p]
  (let [r (str (fs/canonicalize root))
        c (str (fs/canonicalize p))]
    (when (or (= c r) (str/starts-with? c (str r "/")))
      c)))

(defn- skipped?
  [rel skip-dirs]
  (let [top (first (str/split rel #"/"))]
    (boolean (some #{top} skip-dirs))))

(defn- read-tree
  "The readable regular files under `root` as `[rel lines]` pairs, sorted by
  path, none over `:max-file-chars`, at most `:max-files` of them and
  `:max-total-chars` in all — the walk stops there. Hidden files and
  directories are skipped by the glob; `:skip-dirs` names the visible ones."
  [root {:keys [max-files max-file-chars max-total-chars skip-dirs]}]
  (let [root* (str (fs/canonicalize root))
        rels (->> (concat (fs/glob root* "*") (fs/glob root* "**/*"))
                  (map str)
                  (filter fs/regular-file?)
                  (map #(str (fs/relativize root* (fs/canonicalize %))))
                  (remove #(skipped? % (or skip-dirs [])))
                  distinct
                  sort
                  (take max-files))]
    (loop [rels rels total 0 acc []]
      (if-let [rel (first rels)]
        (let [abs (str (fs/path root* rel))
              size (try (fs/size abs) (catch Throwable _ nil))]
          (cond
            (or (nil? size) (> size max-file-chars))
            (recur (rest rels) total acc)

            (> (+ total size) max-total-chars)
            acc

            :else
            (let [text (try (fs/slurp abs) (catch Throwable _ nil))]
              (recur (rest rels) (+ total size)
                     (if text (conj acc [rel (str/split-lines text)]) acc)))))
        acc))))

;; --- the definitions ---------------------------------------------------------

(defn- quote-re
  "`s` as a regex that matches it literally."
  [s]
  (str/replace s #"[\\^$.|?*+()\[\]{}]" #(str "\\" %)))

(defn- snippet [lines from n]
  (str/join "\n" (take n (drop from lines))))

(defn- named-file
  "The tree entry `name` names as a path: exactly, or by basename."
  [tree name]
  (or (some (fn [[rel _ :as e]] (when (= rel name) e)) tree)
      (some (fn [[rel _ :as e]] (when (str/ends-with? rel (str "/" name)) e)) tree)))

(defn- defined-in
  "The first `[rel index]` in the tree whose line matches a definition
  pattern for `name`, or nil."
  [tree name {:keys [definition-patterns]}]
  (let [pats (map #(re-pattern (str/replace % "{name}" (quote-re name)))
                  definition-patterns)]
    (some (fn [[rel lines]]
            (some (fn [[i line]]
                    (when (some #(re-find % line) pats)
                      [rel i]))
                  (map-indexed vector lines)))
          tree)))

(defn definitions
  "Where each of `names` is defined under `root`: `{:name :path :line
  :snippet}` per name that locates something, in the order given, at most
  `:max-snippets`. A name that is a file's path (or basename) is shown from
  its top; anything else by its first definition line."
  [root names {:keys [max-snippets snippet-lines] :as policy}]
  (let [tree (read-tree root policy)
        n snippet-lines
        find (fn [name]
               (if-let [[rel lines] (named-file tree name)]
                 {:name name :path rel :line 1 :snippet (snippet lines 0 n)}
                 (when-let [[rel i] (defined-in tree name policy)]
                   (let [lines (some (fn [[r ls]] (when (= r rel) ls)) tree)]
                     {:name name :path rel :line (inc i) :snippet (snippet lines i n)}))))]
    (->> names
         (keep find)
         (take max-snippets)
         vec)))

;; --- the block ---------------------------------------------------------------

(defn block
  "The opening block for `problem` under `root`, as `{:block :names :found}`
  — the rendered prose (nil when nothing was found: no heading over an empty
  list), the names taken from the statement, and those that located
  something. nil when the policy is off or there is no root."
  [root problem policy]
  (when (and (:enabled? policy) root (not (str/blank? (str problem))))
    (let [names (problem-names problem policy)
          defs (if (seq names) (definitions root names policy) [])]
      {:names names
       :found (mapv :name defs)
       :block (when (seq defs)
                (prompt/render "orient-inject" {:definitions defs}))})))
