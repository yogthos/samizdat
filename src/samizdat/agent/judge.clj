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

(ns samizdat.agent.judge
  "The pure core of the finalization critic, ported from dirge's unified judge.

  When a branch tries to finish, an LLM judge asks whether the task is actually
  complete — not whether the model believes it is. The judge is fed a
  deterministic EVIDENCE block (what files changed, what commands ran and
  whether they passed, which tools were used) so it can catch an answer that
  claims something the run never did. It is judged against the agent's OWN
  rules so it never demands a forbidden action, and it is calibrated to PASS
  when unsure — a false block wastes a whole turn.

  Everything here is pure and unit-testable; the cell that calls the model and
  routes on the verdict lives in resources/cells/critic.clj.

  WHAT IS MECHANISM HERE, now that the prose and the severity vocabulary have
  moved out: the PARSE PROTOCOL. The regexes that find the VERDICT line and the
  findings block are the other half of the prompt template — they read the shape
  the template asks for. So they travel WITH it: change the requested format in
  prompts/ and change the reader here, in the same edit. That pairing is why
  they are not policy in their own right, and why a project retuning what the
  judge is told to say has to touch both."
  (:require [clojure.string :as str]
            [samizdat.agent.gates :as gates]
            [samizdat.lexicon :as lexicon]
            [samizdat.llm.message :as message]
            [samizdat.prompt :as prompt]
            [samizdat.util :as util]))

;; The judge's preamble is read from resources/prompts/judge.md on each use
;; rather than snapshotted into a def at namespace load. The docstring called
;; it runtime-editable and the def made it exactly not that; a slurp per
;; finalization is nothing next to the model call it is part of.

;; The judge's prompt files read through the shared samizdat.prompt seam
;; (tier 2b) — every gate message reads through the same one.

(def verdicts #{:complete :incomplete :abstain})

(defn- rules
  "The judge's deterministic rules — what the reply should look like and what
  counts as a test run, a code edit or an outside claim. gates.edn
  `:judge-rules`, so a project retunes them without a rebuild."
  []
  (gates/threshold :judge-rules))

(defn usable
  "A judge reply with its reasoning removed, so the parsers read what the
  judge SAID rather than what it considered saying.

  MEASURED on run dbe64eea-successor. GLM-5.3 cannot be told not to think, so
  the critic's reply arrived with its scratchpad inline, and `:findings-regex`
  is greedy from the FIRST `FINDINGS:` — which was inside the <think> block.
  The stored findings began mid-thought, carried `</think>`, and were appended
  to the retry's problem: the next owner was handed the critic's musings as
  part of its task, including a bullet it had talked itself out of (\"Not a
  finding\"). The verdict was at the same risk, and that half is worse — the
  verdict-line scan takes the first matching line, so a rehearsed
  `VERDICT: COMPLETE` inside the reasoning outranks the real one after it.

  `message/strip-think-blocks` already existed and `select.clj` already used
  it for precisely this; the judge path never called it. It happens HERE and
  not at the call sites because there are three critics — :board/review,
  :feature/critique and cells/critic.clj — and a fourth would forget.

  AN UNTERMINATED BLOCK IS ALL REASONING. strip-think-blocks removes matched
  pairs only, so a truncated judge call leaves an open <think> and everything
  after it is rehearsal. Cut there: what remains is empty, and both parsers
  fail open on empty, which is what a judge that could not answer has to do."
  [reply]
  (let [s (message/strip-think-blocks (str reply))]
    (if-let [i (str/index-of s "<think>")]
      (str/trim (subs s 0 i))
      s)))

(defn parse-verdict
  "The verdict from a judge reply's verdict line.

  Whole-word and negation-aware, because INCOMPLETE contains COMPLETE and
  \"NOT COMPLETE\" negates it. Both the vocabulary and the precedence that
  handles those two facts are `gates.edn :judge-rules :verdict-rules`, an
  ordered list of [verdict words] applied first-match-wins — the words the
  judge is ASKED for live in prompts/judge.md, which is userspace, so the
  words it is READ for have to be editable in the same breath or a reworded
  prompt silently stops parsing.

  FAIL-OPEN via `:verdict-default`: an empty or tokenless reply is :complete,
  because a judge that cannot answer must never be able to wedge the loop."
  [reply]
  (let [{:keys [verdict-line-regex verdict-rules verdict-default]} (rules)
        reply (usable reply)
        head (or (some #(when (re-find (re-pattern verdict-line-regex) %) %)
                       (str/split-lines (str reply)))
                 (first (str/split-lines (str reply)))
                 "")
        up (str/upper-case head)
        w? (fn [word] (boolean (re-find (re-pattern (str "\\b" word "\\b")) up)))]
    (or (some (fn [[verdict words]] (when (every? w? words) verdict))
              verdict-rules)
        verdict-default)))

(defn parse-yesno
  "A narrow yes/no verdict from a judge reply: true, false, or nil when the
  reply commits to neither (karamazov-a6mj.2).

  The first word of the first non-blank line decides; failing that, the last
  line's first word, because a judge that reasons first and answers last is
  the other natural shape. Anything else is nil — undecided — and the caller
  treats undecided as fail-open, like every judge here: a judge that cannot
  answer must not be able to refuse a ship on its own. Reasoning blocks are
  stripped first (`usable`), for the reason its docstring gives."
  [reply]
  (let [lines (->> (str/split-lines (usable reply)) (remove str/blank?) vec)
        word (fn [line] (some-> (re-find #"^\s*\W*([A-Za-z]+)" (str line)) second str/lower-case))
        read (fn [w] (case w "yes" true "no" false nil))]
    (when (seq lines)
      (let [head (read (word (first lines)))]
        (if (some? head) head (read (word (peek lines))))))))

(defn- nonce []
  (let [b (byte-array 6)]
    (.nextBytes (java.security.SecureRandom.) b)
    (apply str (map #(format "%02x" (bit-and % 0xff)) b))))

(defn- seal
  "`text` as run material inside a `nonce` tag (karamazov-0e2c.4, after
  iFixAi's analytic_judge): a closing tag written into the text is escaped
  and a line dressed as a chat role is quoted, so what the run produced
  cannot end the material early or pose as the conversation. nil stays nil."
  [nonce text]
  (when (seq (str text))
    (str "<material-" nonce ">\n"
         (-> (str text)
             (str/replace #"(?i)</material" (fn [m] (str "<\\" (subs m 1))))
             (str/replace #"(?im)^(\s*)(system|user|assistant|developer|tool)(\s*):" "$1> $2$3:"))
         "\n</material-" nonce ">")))

(defn- sealing
  "A fresh nonce and the prompt line that names it, for one judge prompt."
  []
  (let [n (nonce)]
    {:seal (partial seal n) :sealed (prompt/render "judge-sealed" {:nonce n})}))

(defn yesno-prompt
  "The user message for one acceptance question — prompts/acceptance-judge.md
  over the question, the answer the branch wants to ship, the run's evidence
  block and its diff. Asks for YES or NO first, which is the shape
  `parse-yesno` reads; change both together."
  [{:keys [question answer evidence diff sources]}]
  (let [{:keys [seal sealed]} (sealing)]
    (prompt/render "acceptance-judge"
                   {:sealed sealed
                    :question (str question)
                    :answer (or (seal answer) "")
                    :evidence (seal evidence)
                    :diff (seal diff)
                    ;; The tree as it stands, for a question the diff cannot
                    ;; answer (karamazov-0way). Optional: the ship gate and
                    ;; the plan critic have none to give.
                    :sources (seal sources)})))

(defn findings
  "The FINDINGS section of a judge reply, verbatim, trimmed — or nil when it
  named none. What the critique passes back to the branch below the verdict."
  [reply]
  (some-> (re-find (re-pattern (:findings-regex (rules))) (usable reply))
          second str/trim not-empty))

(defn for-the-record
  "`s` clipped to the gates.edn `:verdict-record` budget `field`
  (:situation-chars for what a judge was shown, :reply-chars for what it
  said), keeping head and tail — a critic's SCORE lines are at the end. nil
  stays nil.

  Every judgement the harness journals passes through here (karamazov-3htz):
  the verdict alone says nothing about whether the judge was right, so the
  situation and the reasoning go in beside it, bounded so a rambling judge
  cannot grow the events table without limit."
  [field s]
  (when (some? s)
    (util/truncate-middle (str s) (get (gates/threshold :verdict-record) field))))

(defn- one-line [s n]
  (let [flat (-> (str s) (str/replace #"\s+" " ") str/trim)]
    (if (> (count flat) n) (str (subs flat 0 n) "…") flat)))

(defn evidence
  "A deterministic facts block over the run's turn rows: how many tool calls,
  which tools, the files the run wrote, and the shell commands it ran with
  whether each passed. Lets the judge check a claim against what the run
  actually did. `rows` are turn maps with :tool_name :args :category."
  [rows]
  (let [rows (vec rows)
        by-tool (frequencies (keep :tool_name rows))
        arg (fn [r k] (get-in r [:args k] (get (:args r) (name k))))
        files (->> rows
                   (filter #(contains? (gates/tool-vocab :file-write) (:tool_name %)))
                   (keep #(arg % :path))
                   distinct)
        cmds (->> rows
                  (filter #(= "shell" (:tool_name %)))
                  (map (fn [r] (str (if (= "failure" (some-> (:category r) name))
                                      "FAILED" "ok")
                                    ": " (one-line (arg r :command) 70))))
                  distinct)
        ;; The last test-runner summary a SHELL run printed. "commands run"
        ;; says a suite was invoked and whether the command exited 0; it
        ;; cannot say how many tests ran or passed, and the first live rubric
        ;; rated "total test count >= 83 and green" NO for exactly that
        ;; reason (karamazov-0way). Shell rows only: a summary line read out
        ;; of a file is not a run. The pattern is gates.edn :judge-rules.
        summary (let [re (re-pattern (str (:test-summary-regex (rules))))]
                  (->> rows
                       (filter #(= "shell" (:tool_name %)))
                       (keep #(last (re-seq re (str (:result %)))))
                       last))]
    (str "tool calls: " (count (keep :tool_name rows))
         "\ntools used: " (str/join ", " (sort (keys by-tool)))
         (when (seq files)
           (str "\nfiles written: " (str/join ", " files)))
         (when (seq cmds)
           (str "\ncommands run:\n  " (str/join "\n  " cmds)))
         (when summary
           (str "\nlast test summary: " summary)))))

(defn- diff-chunks
  "A unified diff split into its per-file chunks at each `diff --git` header.
  A diff with no header is one chunk; text before the first header is one."
  [diff]
  (let [s (str diff)
        marker "diff --git "
        starts (loop [from 0 acc []]
                 (if-let [i (str/index-of s marker from)]
                   (recur (inc i)
                          (if (or (zero? i) (= \newline (nth s (dec i))))
                            (conj acc i)
                            acc))
                   acc))]
    (if (empty? starts)
      [s]
      (let [starts (if (zero? (first starts)) starts (into [0] starts))
            ends (concat (rest starts) [(count s)])]
        (mapv (fn [a b] (subs s a b)) starts ends)))))

(defn- relevance
  "How much `text` (with `header` naming its file) is about `criterion`: ten
  per path the criterion names that appears in the header, one per
  backticked symbol it names that the text mentions. Shared by focus-diff
  and focus-sources so a question ranks a file the same way in both."
  [criterion header text]
  (let [c (str criterion)
        paths (re-seq #"[\w./-]+\.[A-Za-z]{1,5}" c)
        symbols (map second (re-seq #"`([^`]+)`" c))]
    (+ (* 10 (count (filter #(str/includes? (str header) %) paths)))
       (count (filter #(str/includes? (str text) %) symbols)))))

(defn focus-sources
  "The current SOURCES of the files a run changed, `{path content}`, as one
  text block ordered by relevance to `criterion` and cut at `cap` chars (nil
  for no cut), or nil when there are none.

  A question about the TREE cannot be answered from a diff: run 5f8de58c's
  verify-stage judge answered 'the wind is one field felt and shown
  consistently' NO because 'the diff contains no change unifying wind
  sampling' — the sampling predated the run. The file the question names
  comes first for the same reason focus-diff puts it first (karamazov-0way)."
  [sources criterion cap]
  (when (seq sources)
    (let [blocks (->> sources
                      (sort-by key)
                      (map (fn [[path content]]
                             [(- (relevance criterion path content)) path
                              (str "--- " path " ---\n" content)]))
                      (sort-by (fn [[s p _]] [s p]))
                      (map peek))
          text (str/join "\n" blocks)]
      (if (and cap (> (count text) (long cap)))
        (str (subs text 0 (long cap)) "\n… (sources truncated at " cap " chars)")
        text))))

(defn focus-diff
  "`diff` with its per-file chunks reordered by relevance to `criterion`:
  files whose path the criterion names first, then files whose hunks
  mention a symbol it names in backticks, then the rest in git's order.
  Nothing is dropped — this decides what a later cut falls on.

  The first rubric ever scored on a real epic (run 5f8de58c) rated 8 of 15
  criteria NO with 'the diff is truncated before windview_test.clj' as the
  reason: the epic's diff was 20417 chars, the branch budget cut it at
  12000, and the cut landed on the header of the one file five of the
  questions were about. A rubric question is narrow, so the evidence it
  needs is narrow too, and it should be at the front (karamazov-0way)."
  [diff criterion]
  (let [chunks (diff-chunks diff)]
    (if (< (count chunks) 2)
      (str diff)
      (let [header (fn [chunk] (first (str/split-lines chunk)))
            score (fn [chunk] (relevance criterion (header chunk) chunk))]
        (->> chunks
             (map-indexed (fn [i chunk] [(- (score chunk)) i chunk]))
             (sort-by (fn [[s i _]] [s i]))
             (map peek)
             (apply str))))))

(defn focused-diff
  "focus-diff then cut at `cap` (nil for no cut) — the one call a cell makes
  to show a question its diff under a budget."
  [diff criterion cap]
  (let [s (focus-diff diff criterion)]
    (if (and cap (> (count s) (long cap)))
      (str (subs s 0 (long cap)) "\n… (diff truncated at " cap " chars)")
      s)))

;; --- deterministic finalization gates (run before the LLM judge) -----------


(defn- tool-used? [rows pred]
  (boolean (some (fn [r] (and (:tool_name r) (pred r))) rows)))

(defn- ran-tests? [rows]
  (tool-used? rows
              (fn [r] (and (= "shell" (:tool_name r))
                           (re-find (re-pattern (:test-run-regex (rules)))
                                    (str (get-in r [:args :command])))))))

(defn- edited-code? [rows]
  (tool-used? rows
              (fn [r] (and (contains? (gates/tool-vocab :file-write) (:tool_name r))
                           (re-find (re-pattern (:code-ext-regex (rules)))
                                    (str (get-in r [:args :path])))))))

(defn- outside-tool? [name]
  (re-find (re-pattern (:outside-reach-regex (rules))) (str name)))

(defn verifier-block
  "A concrete, LLM-free finalization gate (dirge's verifier rung): the run
  edited code but never ran a test. Returns a critique message, or nil when
  there is nothing to say. Deterministic, so it fires before the paid judge.
  The vocabulary is gates.edn :judge-rules (drg-4026 #55) — what counts as
  a test run is project data, not kernel code."
  [rows]
  (when (and (edited-code? rows) (not (ran-tests? rows)))
    (:verifier-message (rules))))

(defn claim-block
  "A finalization gate for an unsupported claim: the answer says it tested,
  ran, or verified something, but the evidence shows no test ran. Returns a
  message or nil. Same gates.edn rules."
  [answer rows]
  (when (and (re-find (re-pattern (:claim-regex (rules))) (str answer))
             (not (ran-tests? rows)))
    (:claim-message (rules))))

(defn source-block
  "A finalization gate for an uncheckable source: the answer attributes a
  claim to something outside the repo — a URL, or phrasing like 'according
  to', 'the docs say', 'the spec says', 'the API returns' — but the run used
  no tool that reaches outside it. Returns a message or nil.

  drg-4026 #56: the premise is CHECKED, not assumed. The caller passes the
  registered tool surface; the outside-reach pattern (gates.edn) is matched
  against it and the run's own rows, and the block fires only when nothing
  that could have checked the claim exists — a project that registers a web
  tool disables this block by existing. fetch_artifact / fetch_turn contain
  no web-family words, so the local journal readers never count as reaching
  out. The surface is an argument, not a require, so this ns stays pure and
  free of the aggregator."
  [answer rows tool-surface]
  (when (and (re-find (re-pattern (:outside-claim-regex (rules))) (str answer))
             (not (some outside-tool? tool-surface))
             (not (some (comp outside-tool? :tool_name) rows)))
    (:outside-message (rules))))

(defn- veto-regex
  "A veto phrase as a pattern: its tokens, any whitespace or none between
  them, so a tab, a non-breaking space or a dropped space does not get a
  disabled test past it (iFixAi pipeline.py)."
  [phrase]
  (re-pattern (str "(?i)" (str/join "[\\s\\u00a0]*"
                                    (map #(java.util.regex.Pattern/quote %)
                                         (remove str/blank? (str/split (str phrase) #"\s+")))))))

(defn diff-veto
  "The first gates.edn :judge-vetoes entry a line the run ADDED matches, as
  {:id :line :why}, or nil (karamazov-0e2c.5). Removed and unchanged lines
  are not the run's doing. Cheap and certain, so it runs before any judge."
  [diff]
  (let [vetoes (try (gates/threshold :judge-vetoes) (catch Throwable _ nil))
        added (->> (str/split-lines (str diff))
                   (filter #(and (str/starts-with? % "+") (not (str/starts-with? % "+++"))))
                   (map #(subs % 1)))]
    (first (for [{:keys [id phrase why]} vetoes
                 :let [re (veto-regex phrase)]
                 line added
                 :when (re-find re line)]
             {:id id :line (str/trim line) :why why}))))

(defn deterministic-block
  "The first deterministic finalization gate that fires, or nil. These are
  cheap and specific, so they run before the LLM judge and outrank it.
  `tool-surface` is the registered tool names — source-block checks the
  run's reach against it (drg-4026 #56). `diff`, when given, is checked
  against gates.edn :judge-vetoes (`diff-veto`)."
  ([answer rows tool-surface] (deterministic-block answer rows tool-surface nil))
  ([answer rows tool-surface diff]
   (or (verifier-block rows)
       (claim-block answer rows)
       (source-block answer rows tool-surface)
       (when-let [v (and diff (diff-veto diff))]
         (prompt/render "judge-veto" v)))))

(defn preamble
  "The judge's standing instructions, from resources/prompts/judge.md (tier
  2b — runtime-editable). Calibrated, not trigger-happy. A unified judge:
  it decides completeness AND reviews the run's own diff for defects."
  []
  (prompt/prompt "judge"))

(defn critic-prompt
  "The critic's user message: THE REQUIREMENT, the diff of what the run
  changed, the deterministic evidence, and the answer under review.

  It used to be handed the implementer's system prompt as `rules` and the
  implementer's TRANSCRIPT, and never the requirement at all — it was asked
  \"is this task complete and correct?\" with the task itself nowhere in the
  message. It inferred what had been asked from the implementer's own account
  of it, which is the one source that cannot contradict the work under review.

  So the transcript is gone and the requirement is in. A critic that reads the
  implementer's reasoning adopts its framing; a critic that reads the
  requirement and the diff forms its own. The transcript is still reachable —
  the journal has every turn — but it is no longer poured in by default."
  [{:keys [requirement transcript evidence answer diff]}]
  (let [budget (gates/threshold :context-budget)
        {:keys [seal sealed]} (sealing)]
    (prompt/render
     "judge-user"
     {:sealed sealed
      :requirement (one-line requirement (:judge-rules-chars budget))
      :evidence (seal evidence)
      :diff (seal diff)
      ;; Absent by default. Kept as a seam because a caller that has a
      ;; specific reason to show it can, and removing the parameter would hide
      ;; that this was ever a choice.
      :transcript (when (seq (str transcript))
                    (seal (one-line transcript (:judge-transcript-chars budget))))
      :answer (or (seal answer) "")
      :preamble (preamble)})))

(defn plan-prompt
  "The critic's user message when it is judging a PLAN rather than a diff
  (karamazov-vale). Same shape as critic-prompt — the requirement, then the
  artifact under review — but the artifact is the plan the owner intends to
  carry out, and the question is whether executing it WOULD satisfy the
  requirement rather than whether a finished diff did.

  This is the whole point of the plan phase: the diff critic asks the question
  after the budget is spent, and this asks it before. The reader is judging
  intent against intent, so it has no evidence block — there is nothing done
  yet to be deterministic about, and handing it an empty one would invite it
  to invent findings from nothing."
  [{:keys [requirement plan]}]
  (let [budget (gates/threshold :context-budget)
        {:keys [seal sealed]} (sealing)]
    (prompt/render
     "judge-plan"
     {:sealed sealed
      :requirement (one-line requirement (:judge-rules-chars budget))
      :plan (or (seal plan) "")})))

(declare blocking-findings)

(defn consensus
  "Ask the critic `n` times (`ask` returns a reply or nil) and take the
  majority of the samples that ANSWERED, after iFixAi's judge_consensus
  (karamazov-0e2c.3): one sample is one point of noise, and the record of the
  split says how sure the gate was.

  A sample passes when its verdict is :complete and it names no blocking
  finding. One that errored or came back empty is unscored — not a vote — and
  the published reasoning is a reply that parsed and agrees with the outcome,
  so one bad parse cannot hide a demonstrated fail. A tie does not pass. Every
  sample failing is the single critic's fail-open: a judge that cannot answer
  must not wedge the loop.

  Returns {:pass? :verdict :blocking :reply :split {:passes :total :errors}}.
  Asks one after another: a local server serves one generation at a time."
  [ask n]
  (let [samples (loop [i 0 acc []]
                  (if (< i (max 1 (long n)))
                    (let [r (try (ask) (catch Throwable _ nil))
                          ok? (not (str/blank? (usable r)))
                          v (when ok? (parse-verdict r))
                          b (when ok? (blocking-findings r))]
                      (recur (inc i) (conj acc {:reply r :scored? ok? :verdict v :blocking b
                                                :pass? (and ok? (= :complete v) (nil? b))})))
                    acc))
        scored (filter :scored? samples)
        passes (count (filter :pass? scored))
        total (count scored)
        pass? (or (zero? total) (> (* 2 passes) total))
        chosen (first (filter #(= pass? (:pass? %)) scored))]
    {:pass? pass?
     :verdict (or (:verdict chosen) :complete)
     :blocking (:blocking chosen)
     :reply (:reply chosen)
     :split {:passes passes :total total :errors (- (count samples) total)}}))

(defn blocking-findings
  "The findings a review blocks on. Returns the whole findings text when any
  finding carries a blocking severity, else nil.

  WHICH SEVERITIES BLOCK is gates.edn :review-blocking-severities, not a regex
  in this file. It was `[critical]` or `[high]`, which encoded two project
  judgements as code: that those are the words a reviewer uses, and that
  medium is advisory. A project whose reviews say [P0]/[P1], or one that wants
  medium to block a ship, could not say so without a rebuild."
  [reply]
  (when-let [f (findings reply)]
    (let [severities (gates/threshold :review-blocking-severities)]
      (when (and (seq severities)
                 (re-find (re-pattern (str "(?i)\\[(" (str/join "|" severities) ")\\]"))
                          f))
        f))))

(defn- severity-line?
  "Whether `line` opens a finding: it STARTS with one of gates.edn
  :review-severities as a bracketed tag, after an optional bullet, number or
  emphasis. Anywhere in the line was too loose: a reasoning paragraph quoting
  a candidate's `[low]` became a finding and carried the prose after it
  (karamazov-kdoj)."
  [line]
  (let [sevs (gates/threshold :review-severities)]
    (boolean (and (seq sevs)
                  (re-find (re-pattern (str "(?i)^\\s*(?:[-*+]|\\d+[.)])?\\s*\\**\\[("
                                            (str/join "|" sevs) ")\\]"))
                           (str line))))))

(defn- surviving-section
  "The part of a verify reply under its surviving-findings heading (wordlists
  :surviving-findings), or the reply whole when it has none."
  [reply]
  (let [heads (lexicon/wordlist :surviving-findings)
        lines (str/split-lines (str reply))
        head? (fn [l] (let [t (str/lower-case (str/trim (str/replace (str l) #"[#*:]" "")))]
                        (contains? (set heads) t)))
        after (rest (drop-while (complement head?) lines))]
    (if (and (seq heads) (some head? lines))
      (str/trim (str/join "\n" after))
      reply)))

(defn finding-segments
  "A findings text split into one segment per finding.

  Ported from dirge's `split_on_severity_lines`. A new segment starts at each
  severity-labelled line, and samizdat's format already gives that — one
  finding per line, each prefixed with its tag — so the verify pass gets
  per-finding granularity with no format migration.

  WHY PER FINDING AND NOT PER BLOCK. The verify pass marks each candidate
  VERIFIED or FALSE_POSITIVE; attributing those per block lets one drop clear
  findings the judge explicitly kept, which is the bug dirge-uz95 records.
  Continuation lines stay with the finding they belong to, and any preamble
  before the first tag is its own segment so a summary is never glued onto a
  finding."
  [text]
  (when (seq (str text))
    (->> (str/split-lines (str text))
         (reduce (fn [acc line]
                   (if (and (severity-line? line) (seq acc)
                            (seq (str/trim (peek acc))))
                     (conj acc (str line "\n"))
                     (if (seq acc)
                       (conj (pop acc) (str (peek acc) line "\n"))
                       (conj acc (str line "\n")))))
                 [])
         (filterv #(seq (str/trim %))))))

(defn- false-positive?
  "Whether a verify segment carries the judge's drop verdict.

  The STRUCTURED token only, so prose like \"guards against false positives\"
  in a real finding does not clear it. A segment carrying a VERIFIED or
  CONFIRMED marker as well is contradictory and is KEPT — fail closed, since
  clearing a possibly-real blocking finding is the worse error. `UNVERIFIED`
  is stripped before that check so it cannot read as `VERIFIED`, which is a
  correction dirge made after it misfired."
  [segment]
  (let [up (str/upper-case (str segment))]
    (and (str/includes? up "FALSE_POSITIVE")
         (not (or (str/includes? (str/replace up "UNVERIFIED" "") "VERIFIED")
                  (str/includes? up "CONFIRMED"))))))

(defn- clean-pass?
  "Whether a verify reply says it cleared every candidate."
  [reply]
  (boolean (re-find (re-pattern (:verify-clean-regex (rules))) (str reply))))

(defn dedupe-findings
  "Drop duplicate findings a consolidation pass missed.

  Order-preserving and keyed on the head of each segment normalised to
  alphanumerics — dirge's `dedupe_findings`, a cheap mechanical backstop to
  the model's own merging rather than a replacement for it."
  [text]
  ;; THE WHOLE SEGMENT, normalised — not a prefix of it. A truncation length
  ;; would be a number in src/ deciding something a project might want
  ;; different, which base-test's ratchet is right to refuse, and nobody
  ;; actually wants to tune how many characters of a finding count as its
  ;; identity. Normalising away punctuation and case already collapses the
  ;; near-duplicates a prefix was reaching for: "contradicts the diff." and
  ;; "contradicts the diff!" have the same key. Removing the decision beats
  ;; moving it to gates.edn or granting it an exemption.
  (let [key-of (fn [seg] (-> (str/lower-case (str seg))
                             (str/replace #"[^a-z0-9]" "")))]
    (->> (finding-segments text)
         (reduce (fn [{:keys [seen out]} seg]
                   (let [k (key-of seg)]
                     (if (contains? seen k)
                       {:seen seen :out out}
                       {:seen (conj seen k) :out (conj out seg)})))
                 {:seen #{} :out []})
         :out
         (str/join)
         str/trim
         not-empty)))

(defn verify-prompt
  "The second pass's user message: the verify instructions, the candidate
  findings, and the diff to check them against."
  [{:keys [candidates diff]}]
  (str (prompt/prompt "judge-verify")
       "\n\n--- candidate findings ---\n" (str/trim (str candidates))
       "\n--- end candidates ---\n\n"
       "--- diff ---\n" (str diff) "\n--- end diff ---"))

(defn verified-findings
  "The candidate findings that survived the verify pass, as text — or nil when
  the pass cleared them all.

  PASS 2 of dirge's two-pass reviewer, and the reason the whole port is worth
  it: pass 1 emits candidates, pass 2 re-reads each one against the diff and
  drops what the diff does not support. A single-pass judge ships whatever it
  first thought, and the one that ran on run dbe64eea-successor emitted five
  findings including two it visibly agonised over.

  FAIL-SAFE, which is the opposite of pass 1's fail-open and deliberately so.
  Pass 1 failing means no findings, and a review that found nothing is a
  review that blocks nothing. Pass 2 failing means the candidates are
  unverified — but they are still the only work anybody did, so an errored or
  unparseable second pass keeps them rather than silently clearing a real
  blocking finding.

  So findings are cleared only two ways: the judge said it found none, or its
  explicit FALSE_POSITIVE verdicts account for every candidate. Anything else
  — nothing parseable, a partial answer, silence — keeps pass 1."
  [{:keys [reply candidates]}]
  ;; Through `usable`, like pass 1: a verify reply with its reasoning inline
  ;; kept a thought that mentioned "[low]" as the findings (run 390dcd17,
  ;; karamazov-na2k.21). An unterminated think block reads as blank, which
  ;; keeps the candidates — the fail-safe case.
  (let [reply (usable reply)
        cands (finding-segments candidates)]
    (cond
      (empty? cands) nil
      (clean-pass? reply) nil
      (str/blank? (str reply)) (dedupe-findings candidates)
      :else
      (let [;; Only what the judge listed as surviving, when it listed it:
            ;; the deliberation above that heading is not findings.
            segs (finding-segments (surviving-section reply))
            dropped (count (filterv false-positive? (finding-segments reply)))
            ;; A SURVIVOR HAS TO BE A FINDING. Filtering only on
            ;; false-positive? let any prose the judge emitted through as the
            ;; findings text — "Hmm, hard to say." would have REPLACED two real
            ;; candidates with itself. dirge's parse_findings has the same
            ;; rule for the same reason: a block with no severity label is
            ;; narration, and narration never fabricates a finding.
            kept (->> segs
                      (remove false-positive?)
                      ;; Its first NON-BLANK line: a segment that opens on a
                      ;; blank line is still the finding under it.
                      (filter #(severity-line? (first (remove str/blank? (str/split-lines %))))))
            survivors (not-empty (str/trim (str/join kept)))]
        (cond
          survivors (dedupe-findings survivors)
          ;; Nothing survived and the explicit drops do not account for every
          ;; candidate: the pass is ambiguous, not clean. Keep the candidates.
          (< dropped (count cands)) (dedupe-findings candidates)
          :else nil)))))

(defn review
  "One review, both passes, against an injected `chat`.

  `chat` is `(fn [pass content] -> reply-text-or-nil)` — the effect seam, so
  this namespace stays pure and the cell that owns the provider supplies it
  (AGENTS.md's rule for a capability: mechanism here, the decision in a cell).

  IT TAKES THE PASS, and that argument is not decoration. On sweep5 run 2 the
  critique note carried six real findings and `side_calls` held one row, for a
  reflection — both judge calls were invisible, so the record could not say
  whether the verify pass had run, nor what a second pass costs. Naming the
  pass lets the cell record each call under its own kind, which is the sum
  karamazov-2rqb exists to restore: a call the run paid for that nothing
  counts is a bill moved out of view.

  Returns {:verdict :candidates :findings}. `:candidates` is what pass 1 said
  and `:findings` is what survived pass 2; the record keeps both, so a reader
  can see what was considered as well as what was concluded.

  TWO PASSES, ported from dirge (which took the craft from roborev):

    1. review — the judge reads the requirement, the diff and the evidence
       and emits candidate findings.
    2. verify — the SAME judge re-reads each candidate against the diff and
       drops what the diff does not support.

  One pass ships whatever the judge first thought. The review on run
  dbe64eea-successor emitted five findings, of which two were style opinions
  and one was a bullet it argued itself out of and left in anyway.

  THE FAILURE MODES ARE OPPOSITE, deliberately. Pass 1 fails OPEN — no reply
  means no findings, and a review that found nothing blocks nothing, which is
  what a judge that cannot answer must do rather than wedge the loop. Pass 2
  fails SAFE — the candidates are then unverified, but they are still the only
  work anybody did, so they stand rather than being silently cleared.

  It is EXTRACTED rather than written at each call site because there are
  three critics — :board/review, :feature/critique and cells/critic.clj — and
  the argument is the one `usable` makes: a fourth would forget the second
  pass, and a critic quietly running one is indistinguishable from one running
  two until you read its findings."
  [{:keys [chat requirement evidence diff answer transcript prompt-fn pass1]
    :or {prompt-fn critic-prompt}}]
  (let [reply (chat (or pass1 :review)
                    (prompt-fn {:requirement requirement :evidence evidence
                                :diff diff :answer answer
                                :transcript transcript}))
        verdict (if reply (parse-verdict reply) :complete)
        ;; Nothing on a verdict line: the verdict above is the default, not
        ;; the judge's (karamazov-na2k.6). Still fail-open; now visible.
        unread (boolean (and reply
                             (not-any? #(re-find (re-pattern (:verdict-line-regex (rules))) %)
                                       (str/split-lines (usable reply)))))
        candidates (when reply (findings reply))
        verified (if (and candidates (gates/threshold :judge-verify?))
                   ;; Skipped when pass 1 found nothing, so a clean review
                   ;; still costs exactly one call.
                   (verified-findings
                    {:reply (chat :verify
                                  (verify-prompt {:candidates candidates :diff diff}))
                     :candidates candidates})
                   candidates)]
    {:verdict verdict :candidates candidates :findings verified :unread unread}))

(defn review-plan
  "Judge a PLAN against a requirement, both passes, against an injected `chat`.

  karamazov-vale, the pre-construction critic. It is `review` with a different
  pass-1 prompt: the same two-pass verify/dedupe machinery, so a plan finding
  the plan does not support is dropped exactly as a diff finding is. The
  candidate/finding split, the fail-open pass 1 and fail-safe pass 2, and the
  side-call accounting all come for free.

  It passes the SAME bare pass names as `review` — :review then :verify — and
  leaves the plan/diff distinction to the caller, exactly as the diff critic
  does. board/design-review prefixes `plan-` and board/review prefixes
  `critic-`, so a run's side_calls tell a plan critic apart from a diff critic
  and price each — the plan phase has to be able to show it is cheap, and a
  cost nothing counts is the bill moved out of view (karamazov-2rqb). Overriding
  pass1 here as well double-prefixed it to `plan-plan-review` in the record.

  `plan` is the artifact — the files the owner will change, the tests it will
  add, and the approach in prose. No evidence block: nothing has been built to
  be deterministic about."
  [{:keys [chat requirement plan]}]
  (review {:chat chat
           :requirement requirement
           :diff plan
           :prompt-fn (fn [_] (plan-prompt {:requirement requirement :plan plan}))}))

;; --- the rubric judge (karamazov-a6mj.4) -------------------------------------
;;
;; thinkingbox's RubricJudge: N narrow YES/NO criteria with weights, deduction
;; and multiplicative penalties, a threshold. One verdict over N criteria is
;; the multi-part question a judge is bad at; one question per criterion is
;; what it is good at, and a failure that names the criterion is the steering
;; principle's "what failed" for free. The RFC brief already asks the owner
;; for an "## Acceptance criteria" list — this is what makes that list the
;; contract epic-review judges the whole change by, criterion by criterion.

(defn section-bullets
  "The bullet lines under the first markdown heading matching `heading-re`,
  markers stripped, up to the next heading. [] when the section is absent.
  The one reader behind an RFC's work items and its acceptance criteria."
  [text heading-re]
  (let [section (->> (str/split-lines (str text))
                     (drop-while #(not (re-find heading-re %)))
                     rest
                     (take-while #(not (re-find #"^#+\s" %))))]
    (into []
          (comp (map str/trim)
                (filter #(re-find #"^[-*+]\s" %))
                (map #(str/replace % #"^[-*+]\s+" ""))
                (remove str/blank?))
          section)))

(defn parse-criteria
  "An RFC's acceptance criteria as rubric entries, from the bullets of its
  \"## Acceptance criteria\" section: `{:criterion :weight :kind}`.

  The marker syntax, taught in prompts/rfc-brief.md and read tolerantly:
    - text          positive, weight 1
    - [3] text      positive, weight 3
    - [-2] text     a DEDUCTION penalty: YES (the violation is observed) costs 2
    - [x0.5] text   a MULTIPLICATIVE penalty: YES scales the reward by 0.5
  Anything else in the brackets is left on the text. [] when there is no such
  section — the rubric then does not run, and the two-pass review alone
  decides, as before. Which heading names the section is gates.edn :rubric
  :heading-regex, beside the brief that asks for it."
  [rfc]
  (into []
        (map (fn [line]
               (if-let [[_ mark rest] (re-matches #"(?s)\[\s*(x?-?[0-9]+(?:\.[0-9]+)?)\s*\]\s*(.+)" line)]
                 (let [mult? (str/starts-with? mark "x")
                       n (Double/parseDouble (if mult? (subs mark 1) mark))]
                   (cond
                     mult? {:criterion (str/trim rest) :weight (Math/abs n) :kind :multiplicative}
                     (neg? n) {:criterion (str/trim rest) :weight (- n) :kind :deduction}
                     :else {:criterion (str/trim rest) :weight n :kind :positive}))
                 {:criterion line :weight 1.0 :kind :positive})))
        (section-bullets rfc (re-pattern (:heading-regex (gates/threshold :rubric))))))

(defn rubric-score
  "The reward for a set of rated criteria — `{:criterion :weight :kind
  :rating}` with `:rating` true / false / nil — as thinkingbox computes it:

    base   = clamp((Σ earned − Σ deductions) / Σ positive weight, 0, 1)
    reward = clamp(base × Π (1 − m) over fired multiplicative penalties, 0, 1)

  An UNDECIDED rating (nil — the judge gave no verdict) is left out of both
  sides rather than read as NO: it neither earns nor costs, and a positive
  criterion nobody decided is not in the denominator. Nothing decided is
  `:reward nil`, so the caller fails open the way every judge here does.
  Returns the parts beside the total so a note can show its arithmetic."
  [ratings]
  (let [decided (filter #(boolean? (:rating %)) ratings)
        pos (filter #(= :positive (:kind %)) decided)
        positive-weight (reduce + 0.0 (map :weight pos))
        earned (reduce + 0.0 (map :weight (filter :rating pos)))
        deductions (reduce + 0.0 (map :weight (filter #(and (= :deduction (:kind %)) (:rating %)) decided)))
        mults (map :weight (filter #(and (= :multiplicative (:kind %)) (:rating %)) decided))
        clamp (fn [x] (-> x (max 0.0) (min 1.0)))
        base (when (pos? positive-weight) (clamp (/ (- earned deductions) positive-weight)))
        reward (when base (clamp (reduce * base (map #(- 1.0 %) mults))))]
    {:earned earned :deductions deductions :positive-weight positive-weight
     :multipliers (vec mults) :base base :reward reward
     :undecided (count (remove #(boolean? (:rating %)) ratings))}))

(defn review-rubric
  "Judge `answer` + `diff` + `evidence` against `criteria` (from
  `parse-criteria`), ONE narrow yes/no call per criterion through `chat`
  (fn [content] -> reply), and score the ratings. Returns
  `{:ratings :reward :pass? :findings}` — `:findings` the failed positive
  criteria and the fired penalties, one line each carrying the judge's own
  sentence, or nil when there is nothing to say.

  `:pass?` is reward ≥ `threshold`, and TRUE when the reward is nil (nothing
  decided): a judge that cannot answer must not be able to refuse the ship on
  its own. A reduce over the criteria, not a mapv — `chat` parks.

  `:diff-chars`, when given, is the rubric's OWN diff budget: each question
  is shown `diff` reordered by relevance to it (focus-diff) and cut there.
  Pass the diff uncut (or under a generous fetch cap) for that to mean
  anything — a diff already cut at the branch budget has lost what the
  reorder would have put first (karamazov-0way). Without it the diff is
  shown as it came. `:sources` ({path content}, the files the run changed as
  they stand) are shown the same way under `:sources-chars`, for a question
  about the tree rather than the change."
  [{:keys [chat criteria answer diff evidence threshold diff-chars sources sources-chars]}]
  (let [ratings (reduce (fn [acc {:keys [criterion] :as c}]
                          (let [shown (if diff-chars
                                        (focused-diff diff criterion diff-chars)
                                        diff)
                                reply (try (chat (yesno-prompt
                                                  {:question criterion :answer answer
                                                   :diff shown :evidence evidence
                                                   :sources (focus-sources sources criterion
                                                                           sources-chars)}))
                                           (catch Throwable _ nil))]
                            (conj acc (assoc c :rating (parse-yesno reply)
                                             :reply (for-the-record :reply-chars (usable reply))))))
                        [] criteria)
        score (rubric-score ratings)
        reward (:reward score)
        failed (filter (fn [{:keys [kind rating]}]
                         (or (and (= :positive kind) (false? rating))
                             (and (not= :positive kind) (true? rating))))
                       ratings)
        line (fn [{:keys [criterion kind reply]}]
               (str "- [rubric] "
                    (if (= :positive kind) "not met: " "penalty: ")
                    criterion
                    (when-let [r (some-> reply str/trim not-empty)]
                      (str " — " (first (str/split-lines r))))))]
    {:ratings ratings
     :score score
     :reward reward
     :pass? (or (nil? reward) (>= reward (double threshold)))
     :findings (when (seq failed) (str/join "\n" (map line failed)))}))

(defn critique-message
  "The single consolidated note injected back into the branch when the judge
  does not pass, so its next turn sees exactly what to fix."
  [verdict findings-text]
  (str "[critic] "
       (case verdict
         :incomplete "This may not be done yet."
         :abstain (str "Correctness could not be confirmed — add a focused test "
                       "or state the missing detail, then finish again.")
         "Reconsider before finishing.")
       (when findings-text (str "\n\n" findings-text))
       "\n\nAddress this, then finish again when it holds."))
