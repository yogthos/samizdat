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

(ns samizdat.agent.files
  "read_file / write_file: the agent's view of, and edits to, the project tree.

  Both are confined to a root (the project directory). A path is resolved
  against the root and canonicalized, and anything landing outside is refused —
  a self-modifying agent operates on its own repo, not the filesystem. Reads
  are bounded; writes create parent directories and overwrite in place.

  Ordinary edit tools, deliberately not shell redirection: an agent that has to
  write files through `cat > f <<EOF` fights the tool surface, and the point of
  the dogfood is to see it change code, not wrestle heredocs.

  READS AND WRITES NO LONGER SHARE ONE BOUNDARY. Writes stay confined to the
  root exactly as they were; reads may also resolve under the READ-ONLY
  reference roots a project declares. See `reference-roots`."
  (:require [samizdat.agent.gates :as gates]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [jolt.fs :as fs]
            [samizdat.agent.source :as source]
            [samizdat.hashline :as hashline]
            [samizdat.lisp :as lisp]
            [samizdat.prompt :as prompt]
            [samizdat.store.journal :as journal]))

(def ^:private clojure-exts #{"clj" "cljc" "cljs" "cljd" "edn" "bb"})

(defn- msg
  "One of the file tools' branch-facing messages, from prompts/file-tool.md.

  Every one of these was a `str` here. They are the whole of what the model
  learns when an edit does not apply — which text did not match, that
  whitespace was tolerated, that the result no longer balances — and a
  project working in a language the paren-balance note means nothing for
  should be able to reword or drop them without a rebuild. One template keyed
  by reason rather than a file per sentence, because they are one surface."
  [ctx]
  (prompt/render "file-tool" ctx))

(defn- clojure-file? [path]
  (contains? clojure-exts (last (str/split (str path) #"\."))))

(defn- vet-source
  "Vet model-authored text bound for `path` through the ONE gate every piece of
  Clojure passes through (samizdat.agent.source/vet). `whole?` says whether the
  text is wholly authored in this call, which is what decides repair vs
  refusal; whether it is Clojure at all is this caller's judgement, from the
  extension."
  [path text whole?]
  (source/vet text {:whole? whole? :clojure? (clojure-file? path)}))

(defn- max-read-chars
  "How much of a file one `read` returns: the SMALLER of :file-read-chars and
  :tool-result-chars, both gates.edn :context-budget.

  The minimum, not :file-read-chars alone, because every tool result passes
  through the loop's own clip on its way to the model. :file-read-chars was
  60000 and :tool-result-chars 4000, so this tool's budget never bound
  anything — the outer clip did, and it lands AFTER the page marker is
  written. Paging against a budget that is not the effective one produces a
  `continue from line N` the model never sees, which is the dead end it was
  meant to fix, one layer further in."
  []
  (let [{:keys [file-read-chars tool-result-chars]} (gates/threshold :context-budget)]
    ;; No fallback numbers: repeating gates.edn's values here is how the table
    ;; stops being the one place they live. A budget missing from the table is
    ;; a broken table, and throwing says so.
    (apply min (keep identity [file-read-chars tool-result-chars]))))

(defn- grep-ranges
  "How many distinct line ranges a search reports before it says '… and N
  more', from gates.edn :context-budget."
  []
  (:grep-ranges (gates/threshold :context-budget)))

(defn resolve-under-root
  "Canonicalize `path` under `root`; return the resolved absolute path string,
  or nil when it escapes the root. The canonical root is the boundary — a
  `..` or an absolute path that lands outside it fails closed.

  Public because it is THE confinement primitive: every tool that turns a
  model-supplied path into a filesystem read must come through here, and the
  lsp tool rolling its own bare io/file was the escape (karamazov-blt.28)."
  [root path]
  (let [root* (str (fs/canonicalize root))
        target (str (fs/canonicalize (fs/path root path)))]
    (when (or (= target root*) (str/starts-with? target (str root* "/")))
      target)))

(defn- absolute? [p] (.isAbsolute (io/file (str p))))

(defn- under?
  "Whether the absolute path `p` is `dir` or sits inside it. String prefix at a
  segment boundary, so `/a/bc` is not inside `/a/b`."
  [dir p]
  (or (= p dir) (str/starts-with? p (str dir "/"))))

(defn reference-roots
  "The READ-ONLY roots this project declared beside its own, canonicalized.

  Root confinement fails closed and shares one primitive with writes, which is
  right for writes and was an INVERSION for reads: a brief naming a language
  reference and ninety worked examples next to the project got them refused by
  `read_file` — the narrow, paged, bounded tool — and the model read every one
  of them through `shell` instead, which spawns a process. Forcing the safe
  tool to fail so the powerful one gets used protects nothing. It also
  silently handicaps a weaker model, which takes the refusal at face value and
  builds the whole thing having never read an example.

  So reads get a wider set of roots and writes keep the narrow one. Each entry
  is still canonicalized and reads are still refused anywhere outside a
  declared root; what changed is how many roots there are, not whether there
  is a boundary.

  An ABSOLUTE declared path belongs to itself. `java.io.File(parent, child)`
  concatenates an absolute child onto its parent, which is how the REPL's
  classpath spent eight runs pointing at `/…/project//Users/…/lib/src`
  (karamazov-9uc); the same mistake here would refuse exactly the paths the
  project declared."
  [declared root]
  (into []
        (comp (map str)
              (remove str/blank?)
              (map (fn [p]
                     (str (fs/canonicalize
                           (if (absolute? p) p (fs/path (or root ".") p))))))
              ;; A path that is not there is dropped rather than carried. It
              ;; would resolve nothing anyway, and the system prompt names
              ;; these to the model — advertising a directory that does not
              ;; exist buys a wasted turn and a refusal the model cannot make
              ;; sense of.
              (filter #(.exists (io/file %)))
              (distinct))
        (cond (nil? declared) []
              (coll? declared) declared
              :else [declared])))

(defn ctx-reference-roots
  "The reference roots for a tool call, from the run config's
  `:run :reference-paths`.

  The project's own `.samizdat/config.edn` rather than gates.edn: that file is
  the operator's and the agent may not rewrite it (`run-config?`), which is the
  right owner for a decision about what the run may read outside its tree."
  [{:keys [config root]}]
  (reference-roots (get-in config [:run :reference-paths]) root))

(defn resolve-for-read
  "Resolve `path` for READING — under the project root, or under any of `refs`.
  Returns the resolved absolute path, or nil when it is under none of them.

  The read counterpart of `resolve-under-root`, which stays the WRITE
  primitive and is unchanged."
  [root refs path]
  (or (resolve-under-root root path)
      (some #(resolve-under-root % path) refs)))

(def run-config-path
  "The project-local run config, relative to the root — the file that defines
  :run :verify-cmd and :require-test?, i.e. the ship gates this run is judged
  against."
  ".samizdat/config.edn")

(defn run-config?
  "Whether the resolved absolute path `abs` IS the root's run config. The
  write tools refuse it: run 671e8a99 rewrote its own :verify-cmd mid-run to
  a command that ran 0 tests and exited 0 — a Gate 2 that always passes
  (karamazov-kvw). The gate definition belongs to the operator; the party a
  gate judges does not get to edit it. Reads stay open, and the REST of
  .samizdat/ (cells, skills) stays writable — those are workflow, which the
  agent owns. A mechanism-level invariant, not policy data, because a
  protected list in agent-editable gates.edn could be unprotected by the
  party it protects against."
  [root abs]
  (= abs (resolve-under-root (or root ".") run-config-path)))

(defn- miss [branch msg]
  {:result msg :category :mechanics :progress? false :branch branch})

(defn- page
  "The lines of `content` from `offset` (0-based) that fit in the char budget,
  as {:text :from :next :total}. `next` is the line to ask for to continue, or
  nil at the end.

  Lines rather than characters because a line number is something the model
  can hold and act on; a character offset into a file it has only seen part of
  is not. A single line longer than the whole budget is emitted anyway rather
  than dropped — a page that can return nothing would never advance."
  [content offset budget limit]
  (let [lines (str/split-lines content)
        total (count lines)
        from (max 0 (min offset total))]
    (loop [i from, taken [], used 0]
      (if (or (>= i total)
              (and limit (>= (count taken) limit))
              (and (seq taken) (> (+ used (count (nth lines i)) 1) budget)))
        {:text (str/join "\n" taken) :from from :total total
         :next (when (< i total) i)}
        (recur (inc i) (conj taken (nth lines i))
               (+ used (count (nth lines i)) 1))))))

(defn read-file
  "Return the contents of a file under the root, a page at a time. :neutral —
  reading establishes nothing. A missing file or an escaping path is
  :mechanics (a call made wrong), never :failure.

  `offset` is a 0-based LINE to start from and `limit` a maximum number of
  lines; both are optional and the char budget still bounds the result.

  IT PAGES BECAUSE WITHOUT PAGING IT LOOPED. The result was clipped at the
  budget and marked `… [truncated]`, which names a problem and no way out of
  it. Live, against a 7KB brief: the model read the same file six times
  through `read_file`, `cat`, `wc && sed` and `sed`, got the identical first
  4014 characters every time, and then spent four more turns writing a chunked
  reader in `eval` — ten turns of a forty-turn budget to read one file it had
  been told to start from. A truncation marker has to end with the call that
  continues it, or it is a dead end the model can only walk into again."
  [{:keys [branch root args] :as ctx}]
  (let [refs (ctx-reference-roots ctx)
        path (str (:path args))
        offset (or (some-> (:offset args) str parse-long) 0)
        limit (some-> (:limit args) str parse-long)
        anchors? (boolean (or (:anchors args) (get args "anchors")))]
    (cond
      (str/blank? path)
      (miss branch (msg {:needs-path true :tool "read_file"}))

      :else
      (if-let [abs (resolve-for-read (or root ".") refs path)]
        (if (fs/exists? abs)
          (let [content (slurp abs)
                {:keys [text from next total]}
                (page content offset (max-read-chars) limit)
                ;; ANCHORS ARE OPT-IN (karamazov-0kk). Rendering
                ;; `<line>:<hash>│ ` on every read would change what every
                ;; existing flow sees for the sake of one tool, and a model
                ;; copying a region back out would carry the gutter with it.
                ;; A branch that means to `patch` asks for them; every other
                ;; read is untouched, which is also what makes the two edit
                ;; paths comparable.
                text (if anchors?
                       (hashline/render-lines
                        (map-indexed (fn [i l] [(+ from i 1) l])
                                     (str/split text #"\n" -1)))
                       text)]
            {:result (str path
                          (when (pos? from) (str " (from line " from ")"))
                          ":\n" text
                          (when next
                            (str "\n" (msg {:more true :path path :next next
                                            :shown next :total total}))))
             :category :neutral :progress? false :branch branch})
          (miss branch (msg {:no-file true :path path})))
        ;; The refusal NAMES the roots that would have worked. A boundary the
        ;; model cannot see the shape of is one it can only probe by failing,
        ;; and probing costs a turn each time.
        (miss branch (msg {:outside-root true :path path :verb "read"
                           :refs (when (seq refs) (str/join ", " refs))}))))))

(defn patch-file
  "Apply anchored `edits` to a file under the root, as ONE atomic batch.

  Each edit is `{from, to?, replace}` where `from`/`to` are `<line>:<hash>`
  anchors minted by `read_file({anchors: true})`. The model spends a
  coordinate it was HANDED rather than reproducing the text it is replacing,
  which is the failure edit_file's whitespace fallback exists to tolerate.

  Refuses whole and writes nothing when any anchor does not resolve, when two
  edits touch one line, or when the result would not load — the same rule
  edit_file now follows (karamazov-2d3). :mechanics for a call made wrong,
  :success when the batch lands."
  [{:keys [branch root args]}]
  (let [path (str (:path args))
        edits (:edits args)]
    (cond
      (str/blank? path) (miss branch (msg {:needs-path true :tool "patch"}))
      (not (sequential? edits)) (miss branch (msg {:needs-edits true}))
      (empty? edits) (miss branch (msg {:needs-edits true}))
      :else
      (if-let [abs (resolve-under-root (or root ".") path)]
        (cond
          (run-config? root abs) (miss branch (msg {:protected true :path path}))
          (not (fs/exists? abs)) (miss branch (msg {:no-file true :path path}))
          :else
          (let [content (str/replace (slurp abs) "\r\n" "\n")
                edits (mapv (fn [e]
                              {:from (str (or (:from e) (get e "from")))
                               :to (some-> (or (:to e) (get e "to")) str)
                               :replace (str (or (:replace e) (get e "replace") ""))})
                            edits)
                result (hashline/apply-edits content edits)]
            (if-let [err (:error result)]
              (miss branch (msg (assoc err :anchor-error true
                                       :path path
                                       (name (:reason err)) true)))
              (let [{:keys [problem note]} (vet-source path result false)]
                (if problem
                  (miss branch (msg {:refused true :path path :syntax note}))
                  (do
                    (spit abs result)
                    {:result (msg {:patched true :path path :edits (count edits)
                                   :plural (when (> (count edits) 1) "es")})
                     :category :success :progress? true :branch branch}))))))
        (miss branch (msg {:outside-root true :path path :verb "patched"}))))))

(defn grep-limit
  "How many matching lines one search reports before it hands back a
  continuation, from gates.edn :context-budget."
  []
  (:grep-hits (gates/threshold :context-budget)))

(defn grep-msg
  "One of grep's branch-facing sentences, from prompts/grep-tool.md. Its own
  template rather than file-tool.md's: read/write/edit speak about one path,
  grep speaks about a result set, and cramming both into one file made the
  conditionals unreadable."
  [ctx]
  (prompt/render "grep-tool" ctx))

(defn- in-scope?
  "Whether the root-relative path `rel` falls under any of `scopes` — a path
  prefix each, matched at a segment boundary so `sub` selects `sub/b.clj` and
  `submarine.clj` is not swept in with it. No scopes means the whole project."
  [scopes rel]
  (or (empty? scopes)
      (boolean (some (fn [s]
                       (let [s (str/replace (str s) #"^\./|/$" "")]
                         (or (= rel s) (str/starts-with? rel (str s "/")))))
                     scopes))))

(defn grep-project
  "Search the project's source files for `pattern` (a regex string); return a
  seq of {:path :line :text} for each matching line, with paths relative to
  `root`. Globs the Clojure source extensions rather than walking the tree:
  glob skips hidden directories, so cache and VCS noise never matches, and the
  brace pattern covers files sitting directly in the root, which a plain
  `**/*.clj` misses. Reading establishes nothing: :neutral.

  `:paths` scopes the sweep to one or more path prefixes. Without it a wide
  pattern answers the whole project, which is a lot of noise to push through a
  result cap — the search that finds too much should be narrowable rather than
  silently cut.

  `:refs` are the declared reference roots (`reference-roots`). They are swept
  ONLY when an absolute `:paths` entry names one — a reference tree is usually
  far larger than the project, and sweeping ninety examples by default would
  drown every ordinary search. Their hits come back as ABSOLUTE paths, which
  is what `read_file` then takes to open one."
  ([root pattern] (grep-project root pattern nil))
  ([root pattern {:keys [paths refs]}]
   (let [root* (str (fs/canonicalize (or root ".")))
         re (re-pattern pattern)
         scopes (remove str/blank? (map str (cond (nil? paths) []
                                                  (coll? paths) paths
                                                  :else [paths])))
         ;; Only an absolute scope can select a reference root; a relative one
         ;; is a project path and belongs to the sweep below it. Canonicalized,
         ;; because the roots are: on a Mac /tmp is a symlink to /private/tmp,
         ;; so comparing a raw scope against a canonical root matches nothing
         ;; and the sweep silently returns empty.
         abs-scopes (map #(str (fs/canonicalize %)) (filter absolute? scopes))
         ;; [file-to-read reported-path] pairs. The project's files are
         ;; reported relative to the root, as they always were; a reference
         ;; file has no meaningful relative name, so it reports itself.
         project (for [ext clojure-exts
                       p (fs/glob root* (str "{*." ext ",**/*." ext "}"))
                       :let [rel (str (fs/relativize root* (fs/canonicalize (str p))))]
                       :when (in-scope? scopes rel)]
                   [(str p) rel])
         referenced (for [r refs
                          :let [selected (filter #(under? r %) abs-scopes)]
                          :when (seq selected)
                          ext clojure-exts
                          p (fs/glob r (str "{*." ext ",**/*." ext "}"))
                          :let [abs (str (fs/canonicalize (str p)))]
                          :when (some #(under? % abs) selected)]
                      [abs abs])]
     (mapcat (fn [[file reported]]
               (keep-indexed (fn [i line]
                               (when (re-find re line)
                                 {:path reported :line (inc i) :text line}))
                             (str/split (slurp file) #"\n" -1)))
             (concat project referenced)))))

(defn grep-page
  "The window of `hits` from `offset`, at most `limit` of them, as
  {:hits :from :total :next}. `next` is the offset to ask for to continue, or
  nil at the end.

  The same shape read_file pages with, and for the same hard-won reason: this
  tool used to `(take 200 hits)` and print nothing about the rest, with no
  offset argument to continue from. A model could not tell a truncated answer
  from a complete one, and could not have continued it if it had. Pure."
  [hits offset limit]
  (let [all (vec hits)
        total (count all)
        from (max 0 (min (or offset 0) total))
        to (min total (+ from (max 1 limit)))]
    {:hits (subvec all from to)
     :from from
     :total total
     :next (when (< to total) to)}))

;; --- surgical edit ----------------------------------------------------------
;; Ported from dirge's edit tool (src/agent/tools/edit.rs): exact match first,
;; a line-trimmed fallback for the whitespace drift that is ~95% of failed
;; edits, ambiguous matches reported rather than guessed, and replace_all
;; splicing every occurrence in reverse so offsets stay valid.

(defn- exact-ranges
  "Byte [start end] ranges of every exact occurrence of `needle` in `s`."
  [s needle]
  (when (seq needle)
    (loop [from 0, out []]
      (let [i (str/index-of s needle from)]
        (if i
          (recur (+ i (count needle)) (conj out [i (+ i (count needle))]))
          out)))))

(defn- line-starts [s]
  (into [0] (keep-indexed (fn [i c] (when (= c \newline) (inc i))) s)))

(defn- line-trimmed-ranges
  "Ranges where a window of lines matches `needle`'s lines after trimming each
  — the fallback for leading/trailing whitespace drift. dirge's
  find_line_trimmed_matches."
  [s needle]
  (let [clines (str/split s #"\n" -1)
        flines (str/split needle #"\n" -1)
        fn* (count flines)
        starts (reductions + 0 (map #(inc (count %)) clines))]
    (when (and (pos? fn*) (<= fn* (count clines)))
      ;; NON-OVERLAPPING, greedily from the top: a multi-line needle like
      ;; "a\na" matches at consecutive lines of "a\na\na", and the reverse
      ;; splice then wrote new-text into a region the previous replacement had
      ;; already rewritten, corrupting the file under replace_all (blt.38).
      (let [all (for [i (range (inc (- (count clines) fn*)))
                      :let [block (subvec (vec clines) i (+ i fn*))]
                      :when (every? true? (map #(= (str/trim %1) (str/trim %2))
                                               block flines))]
                  (let [start (nth starts i)
                        end (reduce + start (concat (map count block)
                                                    (repeat (dec fn*) 1)))]
                    [start end]))]
        (reduce (fn [acc [start end :as r]]
                  (if (and (seq acc) (< start (second (peek acc))))
                    acc
                    (conj acc r)))
                []
                all)))))

(defn- line-of [starts pos]
  (inc (count (take-while #(<= % pos) (rest starts)))))

(defn edit-file
  "Replace `old_text` with `new_text` in a file under the root. Exact match
  first; on whitespace drift, a line-trimmed fallback (noted). An old_text
  that matches more than once without :replace_all is reported with line
  numbers rather than guessed. For a Clojure file, an edit that breaks the
  delimiter balance is flagged. :mechanics for a call made wrong (not found,
  ambiguous, escaping path); :success when the edit lands."
  [{:keys [branch root args]}]
  (let [path (str (:path args))
        old-text (str (:old_text args))
        new-text (str (:new_text args))
        replace-all? (boolean (:replace_all args))]
    (cond
      (str/blank? path) (miss branch (msg {:needs-path true :tool "edit_file"}))
      (str/blank? old-text) (miss branch (msg {:needs-old-text true}))
      :else
      (if-let [abs (resolve-under-root (or root ".") path)]
        (cond
          (run-config? root abs)
          (miss branch (msg {:protected true :path path}))

          (not (fs/exists? abs))
          (miss branch (msg {:no-file true :path path}))

          :else
          (let [content (str/replace (slurp abs) "\r\n" "\n")
                old-text (str/replace old-text "\r\n" "\n")
                new-text (str/replace new-text "\r\n" "\n")
                [ranges fallback] (if-let [ex (seq (exact-ranges content old-text))]
                                    [ex nil]
                                    (when-let [lt (seq (line-trimmed-ranges content old-text))]
                                      [lt "line-trimmed"]))
                starts (line-starts content)]
            (cond
              (empty? ranges)
              (miss branch (msg {:not-found true :path path}))

              (and (> (count ranges) 1) (not replace-all?))
              (miss branch
                    (msg {:ambiguous true :path path :count (count ranges)
                          :lines (str/join "\n"
                                           (for [[s _] (take (grep-ranges) ranges)]
                                             (str "  Line " (line-of starts s))))
                          :more (when (> (count ranges) (grep-ranges))
                                  (- (count ranges) (grep-ranges)))}))

              :else
              ;; Splice in reverse so earlier offsets stay valid.
              (let [edited (reduce (fn [s [start end]]
                                     (str (subs s 0 start) new-text (subs s end)))
                                   content
                                   (sort-by first > (if replace-all? ranges [(first ranges)])))
                    {:keys [problem note]} (vet-source path edited false)]
                (if problem
                  ;; REFUSED, and the file is left exactly as it was
                  ;; (karamazov-2d3). It used to write the broken text, report
                  ;; :success with :progress? true — so a branch earned credit
                  ;; for breaking the tree and the thrash counters never saw it
                  ;; — and hand back write_file's repair note, which is written
                  ;; in the past tense about a repair this path deliberately
                  ;; does not apply. A surgical edit must not auto-close (that
                  ;; re-parents code); the answer is to refuse, not to narrate.
                  ;; vis: "a syntax-breaking batch is refused whole and the
                  ;; file is left untouched."
                  (miss branch (msg {:refused true :path path :syntax note}))
                  (do
                    (spit abs edited)
                    {:result (let [n (if replace-all? (count ranges) 1)]
                               (msg {:edited true :path path :replacements n
                                     :plural (when (> n 1) "s")
                                     :fallback fallback}))
                     :category :success :progress? true :branch branch
                     :fallback fallback}))))))
        (miss branch (msg {:outside-root true :path path :verb "edited"}))))))

(defn stale-note
  "A line telling this branch that a sibling changed `path` after it last read
  it, or nil.

  A NOTICE, NOT A REFUSAL. Team workers share one working tree on purpose —
  the parts of a feature belong in the same files — so two branches in one
  file is the design working, not a fault to block. What the harness can say
  is what it knows: somebody else has been in here since you looked. Deciding
  which version wins is exactly the judgement it does not have.

  It matters most on `write_file`, which replaces a whole file: a worker
  writing from its own picture of what belongs there silently drops whatever a
  sibling added. Live, three branches wrote src/kit/core.clj fifteen times
  between them, twice on the same turn, and nothing said a word.

  Best effort: a failure to look must never fail the write that succeeded."
  [{:keys [conn run-id branch args]}]
  (try
    (when (and conn run-id (:id branch))
      (when-let [{:keys [branch tool]}
                 (journal/changed-since-read conn run-id (:id branch) (str (:path args)))]
        ;; No turn number in the notice. Branches on one run advance their own
        ;; turn counters independently, so a worker on turn 19 was told a peer
        ;; had acted "on turn 25" — accurate, and it reads like the future.
        ;; "since you last read it" is the ordering that matters and the only
        ;; one both branches share.
        (prompt/render "stale-write" {:branch branch :path (str (:path args))
                                      :tool tool})))
    (catch Throwable _ nil)))

(defn with-stale
  "Append the sibling notice to a successful file result."
  [result ctx]
  (if-let [n (and (= :success (:category result)) (stale-note ctx))]
    (update result :result str "\n\n" n)
    result))

(defn write-file
  "Write `content` to a file under the root, creating parent directories.
  :success with :progress? — changing the tree is real work. An escaping path
  is refused and writes nothing."
  [{:keys [branch root args]}]
  (let [path (str (:path args))
        content (:content args)]
    (cond
      (str/blank? path)
      (miss branch (msg {:needs-path true :tool "write_file"}))

      (nil? content)
      (miss branch (msg {:needs-content true}))

      :else
      (if-let [abs (resolve-under-root (or root ".") path)]
        (if (run-config? root abs)
          (miss branch (msg {:protected true :path path}))
        ;; Paren repair for Clojure sources: models drop trailing closers, and
        ;; a file that does not read is a file that does not load. A trailing
        ;; truncation or over-close is fixed mechanically and noted; anything
        ;; else is written as-is with the problem reported, because closing a
        ;; mid-file imbalance would silently re-parent code (see samizdat.lisp).
        ;;
        ;; A WHOLESALE write reports and writes where edit_file refuses. The
        ;; two are different acts: an edit lands in code the model did not
        ;; write and can leave a working tree broken behind its back, while a
        ;; write_file IS the file — refusing it leaves the model no way to
        ;; replace a file it has decided is wrong. vis draws the line in the
        ;; same place: its anchored `patch` refuses, its wholesale
        ;; `Path.write_text` does not.
        (let [content (str content)
              ;; whole? TRUE: the text IS the file, every character of it
              ;; authored in this call, so there is nothing pre-existing that
              ;; closing a truncation could re-parent.
              {:keys [code problem repaired note]} (vet-source path content true)
              ;; No :code means it does not load and could not be repaired.
              ;; Written as given anyway, and SAID so — a wholesale write is
              ;; the model replacing a file it has decided is wrong, and
              ;; refusing it would leave no way to do that.
              content* (or code content)]
          (when-let [parent (fs/parent abs)]
            (fs/create-dirs parent))
          (spit abs content*)
          {:result (msg {:wrote true :path path :chars (count content*)
                         :repaired (boolean repaired)
                         ;; :unbalanced was the old key and it was a lie by
                         ;; omission — balanced-but-unreadable source has
                         ;; nothing unbalanced about it and used to report
                         ;; nothing at all (karamazov-ozv).
                         :broken (boolean problem)
                         :syntax note})
           :category :success :progress? true :branch branch
           :repaired? (boolean repaired)}))
        (miss branch (msg {:outside-root true :path path :verb "written"}))))))
