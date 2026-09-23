;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.agent.instructions
  "The project's own instruction files — AGENTS.md, CLAUDE.md, whatever
  gates.edn :instructions names — found and read. Mechanism only
  (karamazov-d5wo.4): which names count, how much is read and whether any of
  it happens are the policy's; WHEN a directory's file enters the tape is the
  :tool/dispatch cell's decision.

  The root file opens the run beside the orient block, so it sits in the
  protected head and on the run row a resume reads back. A subdirectory's
  file is not worth paying for until the run works there, so it is read the
  first time a tool touches a path under it and pinned into the tape, where
  compaction leaves it alone."
  (:require [clojure.string :as str]
            [jolt.fs :as fs]
            [samizdat.agent.files :as files]
            [samizdat.prompt :as prompt]))

(defn- read-capped [root rel max-chars]
  (let [text (str (fs/slurp (fs/path root rel)))
        cap (or max-chars (count text))]
    {:path rel
     :text (if (> (count text) cap) (subs text 0 cap) text)
     :truncated? (> (count text) cap)}))

(defn- render-ctx
  "A file map as a template sees it; Selmer names cannot end in `?`."
  [f]
  (assoc f :truncated (:truncated? f)))

(defn- file-in
  "The first of `names` that is a regular file in `dir` (relative to `root`,
  \"\" for the root itself), as a root-relative path, or nil."
  [root dir names]
  (some (fn [n]
          (let [rel (if (str/blank? dir) n (str dir "/" n))]
            (when (fs/regular-file? (fs/path root rel)) rel)))
        names))

(defn root-file
  "The root's instruction file as {:path :text :truncated?}, or nil when
  there is none or the policy is off."
  [root {:keys [enabled? file-names max-chars]}]
  (when enabled?
    (when-let [rel (file-in root "" file-names)]
      (read-capped root rel max-chars))))

(defn opening-context
  "The run's opening block: the root instruction file, then `orient-block`.
  Either may be absent; nil when both are."
  [root orient-block policy]
  (let [f (root-file root policy)
        rendered (when f (prompt/render "instructions-root" (render-ctx f)))]
    (not-empty (str/join "\n\n" (remove str/blank? [rendered orient-block])))))

(defn touched-paths
  "The path-valued arguments of a parsed call, by the argument names the
  policy lists. Strings only; a call with none touches nothing."
  [parsed {:keys [path-args]}]
  (let [args (:args parsed)]
    (vec (for [k path-args
               :let [v (or (get args (keyword k)) (get args k))]
               p (if (sequential? v) v [v])
               :when (and (string? p) (not (str/blank? p)))]
           p))))

(defn- pop-or-empty [v] (if (seq v) (pop v) v))

(defn- dirs-toward
  "The root-relative directories strictly below `root` that contain `path`,
  outermost first. Empty when `path` escapes the root."
  [root path]
  (if-let [abs (files/resolve-under-root root path)]
    (let [root* (str (fs/canonicalize root))
          rel (str/replace (subs abs (count root*)) #"^/" "")
          parts (vec (remove str/blank? (str/split rel #"/")))
          ;; A path naming a directory keeps its last part; a file drops it.
          parts (if (fs/directory? abs) parts (pop-or-empty parts))]
      (vec (for [i (range 1 (inc (count parts)))]
             (str/join "/" (subvec parts 0 i)))))
    []))

(defn pending
  "What touching `paths` should load: {:found [{:dir :path :text
  :truncated?}] :seen #{dirs}}. `seen` is every directory checked here plus
  the ones already in `seen-before`, whether or not it held a file, so a
  directory is stat'd once per branch. Never the root: the opening carries it."
  [root paths seen-before {:keys [enabled? file-names max-chars]}]
  (if-not enabled?
    {:found [] :seen (set seen-before)}
    (reduce (fn [{:keys [seen] :as acc} dir]
              (if (contains? seen dir)
                acc
                (let [rel (file-in root dir file-names)]
                  (cond-> (update acc :seen conj dir)
                    rel (update :found conj
                                (assoc (read-capped root rel max-chars) :dir dir))))))
            {:found [] :seen (set seen-before)}
            (distinct (mapcat #(dirs-toward root %) paths)))))

(defn pin-message
  "The pinned tape message carrying one directory's instruction file."
  [{:keys [dir] :as f}]
  {:role "user"
   :content (str "[harness] " (prompt/render "instructions-dir" (render-ctx f)))
   :pinned? true
   :instructions dir})
