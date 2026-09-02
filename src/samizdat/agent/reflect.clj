;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.agent.reflect
  "What a finished task leaves behind about the PROJECT.

  The harness distils plenty about itself — patterns in how the loop ran,
  verdicts on the supervisor's changes, commands that worked or were refused.
  All of that is the harness watching itself. None of it is what a person
  joining the codebase tomorrow would want to have been told, and the
  implementor never wrote any of it down: measured across the live runs, 46
  turns, zero `remember` calls, zero memories.

  So it is a STEP, not an instruction. Asking the model to remember things has
  been in the prompt and produced nothing; a node in the manifest runs whether
  or not the model felt like it.

  THE SCHEMA IS THE MECHANISM, after dirge's compaction summariser: not `write
  a summary` but named sections, each saying what belongs in it and demanding
  specifics — exact paths, exact commands, exact error text. dirge's own
  measurement work (compaction_recall.rs, after the snapcompact write-up) makes
  the case that prose summaries quietly drop exactly the load-bearing facts,
  and that the fix is a schema that forces concreteness.

  What that measurement work also teaches is which half is testable. Nobody can
  unit-test what a model chooses to write. What CAN be tested — and what dirge
  guards in CI — is that every fact the harness meant to hand the summariser
  actually reaches the prompt, so a truncation or a window bug cannot silently
  starve it. `build-prompt` is separate from the call for that reason."
  (:require [clojure.string :as str]
            [samizdat.agent.state :as state]
            [samizdat.lexicon :as lexicon]
            [samizdat.llm.client :as llm]
            [samizdat.prompt :as prompt]
            [samizdat.store.journal :as journal]
            [samizdat.store.knowledge :as knowledge]))

(def ^:private section->kind
  "Which memory kind each section becomes.

  GOTCHAS are procedural rather than episodic on purpose: `find | head is
  refused here` is a rule that holds next time, not a thing that happened
  once."
  {"OVERVIEW" "overview"
   "FACTS"    "semantic"
   "RULES"    "procedural"
   "GOTCHAS"  "procedural"})

(defn- clip [s n] (subs s 0 (min n (count s))))

(defn turn-line
  "One turn as the reflector sees it: what was called, with what, and how it
  came out. The RESULT is included and truncated late, because the gotchas
  worth recording live in error text — a transcript of calls without their
  outcomes cannot say what wasted time. Both widths are gates.edn
  :reflection, where the trade they make is written down."
  [{:keys [turn tool_name args result category]}]
  (let [{:keys [args-chars result-chars]} (lexicon/policy :reflection)]
    (str "t" turn " " tool_name
         (when-let [a (not-empty (str args))] (str " " (clip a args-chars)))
         " -> [" category "] "
         (clip (str/replace (str result) #"\s+" " ") result-chars))))

(defn build-prompt
  "The user message handed to the reflector.

  Separate from the call so it can be asserted on. Everything the harness
  intends the model to see must be IN here; what the model does with it is not
  something a test can pin, but a fact that never arrived is a harness bug and
  is exactly what this makes visible."
  [{:keys [problem turns memories]}]
  (prompt/render "task-reflection-input"
                 {:problem problem
                  :turns (mapv turn-line turns)
                  :memories (mapv #(select-keys % [:id :kind :content]) memories)}))

(defn parse-sections
  "The reflector's reply as `{section [line …]}`.

  Blank sections are dropped rather than kept as empty vectors, and a line with
  no alphanumeric content is not a line — a model asked to leave a section
  empty will often write `None.` or `-` instead."
  [text]
  (let [placeholder (re-pattern (lexicon/wordlist :empty-section))
        chunks (rest (str/split (str text) #"(?m)^##\s+"))]
    (into {}
          (keep (fn [chunk]
                  (let [[header & body] (str/split-lines chunk)
                        section (str/upper-case (str/trim (str header)))
                        lines (->> body
                                   (map #(str/replace % #"^[-*]\s*" ""))
                                   (map str/trim)
                                   (remove str/blank?)
                                   (remove #(re-matches placeholder %))
                                   (remove #(not (re-find #"[A-Za-z0-9]" %))))]
                    (when (seq lines) [section (vec lines)]))))
          chunks)))

(defn corrections
  "The `k-` ids the reflector says are wrong, with its reason."
  [sections]
  (keep (fn [line]
          (when-let [[_ id] (re-find #"(k-[0-9a-f]+)" line)]
            {:id id :why line}))
        (get sections "WRONG")))

(defn record!
  "Write the parsed sections as memories, and report the wrong ones.

  OVERVIEW is capped at one per project by construction: it carries a fixed
  pattern key, so a new one replaces the old rather than accumulating. That is
  what makes it the orientation note rather than a pile of them.

  And REPLACES is literal for the overview, where it used to be a corroborate
  like any other. The overview describes a moving target — what the codebase
  IS — so a run that adds a namespace makes the previous wording wrong without
  making the memory wrong to hold. Live: the first run wrote `nothing is
  implemented yet`, the second run created src/todomvc/db.clj and then
  corroborated that description rather than correcting it, and the same run
  flagged the memory it had just agreed with as WRONG. A standing fact
  corroborates; a description restates."
  [conn {:keys [run-id]} sections]
  (let [written
        (vec (for [[section lines] sections
                   :let [kind (section->kind section)]
                   :when kind
                   line lines
                   :let [key (if (= "OVERVIEW" section)
                               "project:overview"
                               (str (str/lower-case section) ":"
                                    (str/lower-case (str/replace line #"[^A-Za-z0-9]+" "-"))))
                         existing (knowledge/by-pattern conn key)]]
               (if existing
                 ;; Corroborate BEFORE restating, and report whichever row is
                 ;; live afterwards. Restating retires the old version and
                 ;; opens a new one (karamazov-oov), so a count recorded after
                 ;; it lands on the retired row, and handing the old id back
                 ;; would aim every later outcome at a memory nothing can
                 ;; recall. `restate!` carries the record across the lineage.
                 (let [_ (knowledge/corroborate! conn (:id existing) run-id)
                       restated (when (= "OVERVIEW" section)
                                  (knowledge/restate! conn (:id existing) line))]
                   {:id (or restated (:id existing)) :kind kind :repeat? true})
                 {:id (knowledge/remember! conn {:content line :kind kind
                                                 :run-id run-id :pattern-key key
                                                 :confidence 0.7})
                  :kind kind :repeat? false})))]
    (doseq [{:keys [id]} (corrections sections)]
      ;; The implementor is the only role positioned to notice that a memory it
      ;; was handed is stale, so its word counts against that memory's record.
      (when (knowledge/get-by-id conn id)
        (knowledge/record-outcome! conn id false)))
    written))

(defn distil-task!
  "Run the reflection and write what it found. Returns what was written.

  Never throws and never blocks the branch from finishing: this runs at the end
  of a task, and a failure to record what was learned must not turn a completed
  task into a failed one."
  [{:keys [conn run-id llm-adapter llm-config] :as _ctx} branch]
  (try
    (let [turns (journal/branch-turns conn run-id (:id branch))
          memories (knowledge/standing conn)
          user (build-prompt {:problem (:problem branch)
                              :turns turns
                              :memories memories})
          reply (:content (llm/chat llm-adapter llm-config
                                    [{:role "system" :content (prompt/prompt "task-reflection")}
                                     {:role "user" :content user}]
                                    {:temperature 0.0}))
          sections (parse-sections reply)
          written (record! conn {:run-id run-id} sections)]
      (journal/note! conn run-id :task-reflection
                     {:branch-id (:id branch)
                      :data {:sections (keys sections) :written (count written)}})
      written)
    (catch Throwable _ nil)))
