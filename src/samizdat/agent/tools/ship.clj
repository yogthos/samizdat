;; samizdat - a self-hosting agentic harness
;; License: GPL-3.0-or-later

(ns samizdat.agent.tools.ship
  "Shipping tools: thesis, done, give_up, branch_theses — and the
  claim-evidence gates those methods share (answer-tokens,
  uncovered-tokens, engages-problem? and friends)."
  (:require [clojure.string :as str]
            [samizdat.agent.acceptance :as acceptance]
            [samizdat.agent.files :as files]
            [samizdat.agent.gitdiff :as gitdiff]
            [samizdat.agent.gates :as gates]
            [samizdat.agent.tools.base :as base]
            [samizdat.agent.state :as state]
            [samizdat.agent.stubs :as stubs]
            [samizdat.agent.roles :as roles]
            [samizdat.agent.verify :as verify]
            [samizdat.lexicon :as lexicon]
            [samizdat.store.journal :as journal]
            [samizdat.store.tasks :as tasks]
            [samizdat.util :as util]
            [samizdat.session :as session]))



;; --- registering intent -----------------------------------------------------

(defmethod base/run-tool "thesis" [{:keys [branch] :as ctx}]
  (if-let [m (base/missing ctx :goal :technique)]
    (base/malformed branch m)
    (let [thesis {:goal (base/arg ctx :goal)
                  :subClaims (vec (or (base/arg ctx :subClaims) []))
                  :technique (base/arg ctx :technique)
                  :set-at-turn (:turn ctx)}]
      (base/ok (assoc branch :thesis thesis)
          (str "Thesis registered: " (:goal thesis)
               "\nTechnique: " (:technique thesis)
               (when (seq (:subClaims thesis))
                 (str "\nSub-claims:\n"
                      (str/join "\n" (map-indexed #(str "  " (inc %1) ". " %2)
                                                  (:subClaims thesis))))))
          :progress? true
          :thesis thesis))))

;; --- the claim-evidence gates -----------------------------------------------

;; Tier 1c: both the framing stopwords and the tool-version pattern are
;; wordlists.edn data — retunable at runtime without a rebuild. The section
;; comments recording why words are on the list moved with the words.
;; Both memoized against the wordlists' generation rather than realized at
;; namespace load: system/start! calls lexicon/reload! so a list edit takes
;; effect, and a top-level def turned that call into a no-op here.
(def ^:private stopwords
  (util/generation-cache lexicon/gen #(lexicon/wordlist :answer-framing)))

(defn- min-token
  "The shortest token the answer-evidence gate will hold against an answer.
  wordlists.edn `:claim-matching :answer-token-min-length`."
  []
  (lexicon/tuning :claim-matching :answer-token-min-length))

(def ^:private tool-version-re
  (util/generation-cache lexicon/gen
                         #(re-pattern (lexicon/wordlist :tool-version))))

;; The completeness rung's three lists (karamazov-g86), wordlists.edn data
;; like every vocabulary above.
(def ^:private completeness-forward
  (util/generation-cache lexicon/gen #(lexicon/wordlist :completeness-forward)))
(def ^:private completeness-work-verbs
  (util/generation-cache lexicon/gen #(lexicon/wordlist :completeness-work-verbs)))
(def ^:private completeness-second-person
  (util/generation-cache lexicon/gen
                         #(lexicon/wordlist :completeness-second-person)))
(def ^:private completeness-blocked
  (util/generation-cache lexicon/gen
                         #(lexicon/wordlist :completeness-blocked)))
(def ^:private asks-the-reader-phrases
  (util/generation-cache lexicon/gen #(lexicon/wordlist :asks-the-reader)))

(defn- word-starting-with?
  "Whether `needle` occurs in `haystack` at the START of a word.

  THE LEADING BOUNDARY ONLY, and both halves of that are load-bearing —
  dirge's completeness_gate.rs learned each one from a false firing.

  Requiring a boundary at all is what stops `test` reading out of `latest`,
  `fix` out of `prefix`, and `you ` out of `bayou `. Measured here before the
  fix: \"The bayou samples are done; I will fix the parser\" did not fire,
  because the unanchored second-person check found `you ` inside `bayou ` and
  read a genuine abandoned intention as advice to the reader.

  Requiring only the LEADING one is what keeps inflections. The verbs are
  listed in the infinitive and a sentence inflects them — \"I will be
  implementing the retry path\" is the most natural way a model announces
  work it has not done, and an exact-token match misses every one of them.
  Measured here before the fix: that sentence did not fire either.

  So the two rules pull in opposite directions and both are needed. A gate
  that fires on honest work gets switched off; a gate that misses the ordinary
  phrasing never fires at all."
  [haystack needle]
  (let [h (str haystack) n (str needle)]
    (and (seq n)
         (loop [from 0]
           (if-let [i (str/index-of h n from)]
             (if (or (zero? i)
                     (not (re-matches #"[a-z0-9]" (subs h (dec i) i))))
               true
               (recur (inc i)))
             false)))))

(defn unfinished-claim?
  "Whether the answer says, in the model's own first-person voice, that work
  remains (dirge completeness_gate.rs, karamazov-g86). Fires only when ONE
  SENTENCE holds a first-person forward marker and a concrete work verb
  (exact token, so 'latest' cannot read as 'test') and NEITHER exemption:
  second-person address — advice to the reader is a legitimate ending — nor
  a stated limit. The conjunction IS the control: a run that edits real
  files, verifies, claims nothing false, and stops halfway is the most
  ordinary bad ending an autonomous run has, and this is its one lexical
  tell. The lists are wordlists.edn data; dirge's warning against widening
  them travels with the lists.

  WHY A LIMIT IS NOT A PLAN, which is the second exemption and the one with
  a measurement behind it. \"I still need to implement the CLI\" is work the
  run chose to leave. \"I have not been able to test the visual output: the
  host has no window server\" is what the run FOUND — the same voice, the
  same verb, the opposite meaning, and the second is a result.

  The exemption is anchored on FIRST-PERSON inability, and that anchor is
  load-bearing: a third-person obstacle reads identically in a limit and in a
  plan (\"I will fix the parser that fails to handle escapes\" names an
  obstacle and IS an abandoned intention), so keying on one would switch the
  rung off for the shape it exists to catch.

  Measured on run dbe64eea-successor of the arena sweep, against its real
  problem text: the owner was asked to exercise its change on screen, the
  host had no window server, and the two most natural ways to say so were
  both refused here while an answer that never mentioned the requirement
  shipped. The critic then failed the round for precisely that — \"the
  requirement is silently dropped rather than met or escalated as a
  blocker\". A gate that refuses the honest answer and passes the silent one
  does not merely fail to catch omission; it selects for it.

  The exemption is per SENTENCE, not per answer, so an honest limit in one
  sentence cannot license an abandoned plan in the next."
  [answer]
  (let [fwd (completeness-forward)
        verbs (completeness-work-verbs)
        second-p (completeness-second-person)
        blocked (completeness-blocked)]
    (boolean
     (some (fn [sentence]
             (let [s (str " " (str/lower-case (str/trim sentence)) " ")]
               (and (some #(word-starting-with? s %) fwd)
                    (some #(word-starting-with? s %) verbs)
                    (not-any? #(word-starting-with? s %) second-p)
                    (not-any? #(word-starting-with? s %) blocked))))
           (str/split (str answer) #"[.!?\n]+")))))

(defn asks-the-reader?
  "Whether the answer ENDS by asking its reader something — a question to the
  user, an offer of more work, a request for confirmation — which is the one
  ending `done` cannot be (karamazov-a6mj.3; thinkingbox's <DONE> rule: a
  final message may not both close and ask). A run with a question left is a
  run that should ask it — `ask_human`, answered by a person or by the
  simulated user — and then finish, not ship the question as the result.

  THE LAST NON-BLANK LINE ONLY, and a phrase from wordlists :asks-the-reader
  on it: the line is a question mark away from a request, but a bare `?` is
  not enough. `Why did it fail? The port was taken.` is prose that answers
  itself; `Is the clamp right? I checked: yes` the same; the `?` operator
  appears in explanations of code. What makes a request is the second person
  or the offer, and those are the list. Checked at word starts for the
  reason unfinished-claim? gives (`bayou` is not `you`). A line with a
  request phrase but no question mark still fires when the phrase is itself
  the request (`let me know`, `please confirm`)."
  [answer]
  (let [line (some->> (str/split-lines (str answer))
                      (map str/trim)
                      (remove str/blank?)
                      last)
        s (when line (str " " (str/lower-case line) " "))
        phrases (asks-the-reader-phrases)
        hit (when s (some #(when (word-starting-with? s %) %) phrases))]
    (boolean
     (and hit
          (or (str/ends-with? (str line) "?")
              ;; the imperative requests carry no question mark
              (str/starts-with? hit "please")
              (= "let me know" hit))))))

(defn answer-tokens
  "Substantive tokens from a proposed answer: numbers and words that are not
  stopwords. Numbers matter most — an answer naming a size, a bound, or a
  witness has to have that number in the evidence."
  [text]
  (->> (str/split (str/lower-case (str/replace (or text "") (tool-version-re) " "))
                  #"[^a-z0-9_.-]+")
       ;; `.` and `-` stay INSIDE the split class so 3.5 and cross-check survive
       ;; as one token, which means a sentence-final period rides along with the
       ;; last word. Trim the edges, keep the interior.
       (map #(str/replace % #"^[.-]+|[.-]+$" ""))
       (remove str/blank?)
       (remove (stopwords))
       ;; A hyphenated compound is one token, so `engine-confirmed` survived a
       ;; list holding every one of its parts. A compound with a substantive
       ;; half — `optimal-flow` — is not exempt.
       (remove #(and (str/includes? % "-")
                     (let [parts (remove str/blank? (str/split % #"-"))]
                       (and (seq parts)
                            (every? (fn [p] (or ((stopwords) p)
                                                (< (count p) (min-token))))
                                    parts)))))
       (filter #(or (re-matches #"[0-9]+(\.[0-9]+)?" %) (>= (count %) (min-token))))
       distinct))

(def ^:private word-suffixes
  "Stripped longest-first, one only. Enough to see that `enumeration` and
  `enumerating` are the same word, which raw substring matching cannot."
  ["ations" "ation" "ising" "izing" "ings" "ing" "ions" "ion" "ies" "ied"
   "es" "ed" "s"])

(defn- stem
  "The token with one morphological suffix removed, or nil.

  Never below the lexicon's `:answer-suffix-min-stem`, so nothing is
  shortened into a prefix that matches everything."
  [w]
  (some (fn [suf]
          (when (and (str/ends-with? w suf)
                     (>= (- (count w) (count suf))
                         (lexicon/tuning :claim-matching :answer-suffix-min-stem)))
            (subs w 0 (- (count w) (count suf)))))
        word-suffixes))

(defn number-token?
  "Whether an answer token is a figure rather than a word. The two halves of
  the coverage check treat them completely differently: a figure blocks a
  ship, a word advises."
  [token]
  (boolean (re-matches #"[0-9]+(\.[0-9]+)?" token)))

(defn- covered?
  "Whether `token` appears in the evidence.

  Numbers are matched exactly, against the artifacts alone. That is the strict
  half and it stays strict: an answer naming a size, a bound or a witness that
  nothing produced is the fabricated report this whole rung exists to catch.

  Words get three chances — the token itself, the token with hyphens
  normalised, and its stem — because a refusal over `residues` when the
  evidence says `residue` teaches the model to strip its prose rather than to
  verify anything."
  [token artifact-text word-text]
  (if (number-token? token)
    (str/includes? artifact-text token)
    (or (str/includes? word-text token)
        (and (str/includes? token "-")
             (or (str/includes? word-text (str/replace token "-" " "))
                 (str/includes? word-text (str/replace token "-" ""))))
        (when-let [s (stem token)] (str/includes? word-text s))
        ;; One derivational step further, for long words only: `computability`
        ;; against `computable` is the same complaint as `residues` against
        ;; `residue`, and no suffix list reaches it.
        (let [long-enough (lexicon/tuning :claim-matching :answer-prefix-token-length)
              prefix (lexicon/tuning :claim-matching :answer-prefix-match-length)]
          (and (>= (count token) long-enough)
               (str/includes? word-text (subs token 0 prefix)))))))

(defn observed-output
  "What this branch MEASURED, as one artifact-shaped entry for the figure rung,
  or nil when it measured nothing.

  On a coding run the only artifact anything produces is an accepted done's
  own answer (see the :artifact below), so the rung used to check a branch's
  figures against its siblings' answers and its own earlier ones — and never
  against the test run or `eval` it had just watched. Run 5f8de58c's exercise
  branch re-derived every figure in a single eval, exactly as the refusal
  told it to, was refused for all of them, and exhausted (karamazov-3s54).

  The corpus is the results of the branch's :verification-vocabulary calls
  (gates.edn — eval and shell): an output the harness ran and handed back is
  a measurement. A read_file or grep result is not — a test file's expected
  value is an input, which is the fabrication this rung exists to catch — and
  a refused `done`'s result echoes the answer's own figures, so it must never
  count either. `turn-rows` are journal/branch-turns rows."
  ([turn-rows] (observed-output turn-rows nil))
  ([turn-rows extra-vocab]
  (let [measuring (into (or (gates/tool-vocab :verification) #{}) extra-vocab)
        text (->> turn-rows
                  (filter #(contains? measuring (str (:tool_name %))))
                  (map #(str (:result %)))
                  (remove str/blank?)
                  (str/join "\n"))]
    (when (seq text)
      {:kind :observed :witness text}))))

(defn uncovered-tokens
  "Answer tokens no confirmed artifact mentions.

  The claim-evidence gate, deterministic and with no model in the path. An
  answer asserting a number that appears nowhere in the evidence is a
  fabricated report."
  ([answer artifacts] (uncovered-tokens answer artifacts nil))
  ([answer artifacts word-context]
   (let [artifact-text (str/lower-case
                        (str/join " " (for [a artifacts]
                                        (str (:claim a) " " (:code a) " "
                                             (pr-str (:witness a))))))
         ;; Words may also come from `word-context`: the problem statement the
         ;; harness handed the branch. Numbers get none of this — a figure has
         ;; to come from an artifact.
         word-text (str artifact-text " "
                        (str/lower-case
                         (if (coll? word-context)
                           (str/join " " (remove nil? word-context))
                           (str word-context))))]
     (remove #(covered? % artifact-text word-text) (answer-tokens answer)))))

(defn engages-problem?
  "Whether the answer shares any substantive vocabulary with the problem.

  The free rung, and deliberately the weakest one: lexical overlap cannot tell
  an answer to the question from an answer about the question's machinery.
  Zero overlap is the only thing it decides, and it decides it with no model
  in the path.

  A problem with no substantive vocabulary of its own — a stub, a test
  fixture — means there is nothing to be irrelevant to, and this passes."
  [problem answer]
  ;; Without the words that frame the REQUEST — "explain", "project", "chat"
  ;; (wordlists.edn :request-framing). They say what to do, not what about:
  ;; "explain the project in the chat" refused an accurate description of the
  ;; game for not saying "project", and the branch got past it by prefixing
  ;; "I am explaining the project in the chat" (karamazov-1wv9).
  (let [terms (set (remove (or (lexicon/wordlist :request-framing) #{})
                           (answer-tokens problem)))]
    (or (empty? terms)
        (boolean (some terms (answer-tokens answer))))))

;; --- the ship rungs as data (drg-4026 #44) -----------------------------------
;;
;; The model-free lexical rungs live in gates.edn :ship-gates and are
;; compiled HERE at load into predicate/message closures — the same pattern
;; as the steer gates (tier 3): the forms see the evidence keys as plain
;; locals, fns resolve in this namespace at compile, and the VALUES arrive
;; at fire time. Adding a rung is a data edit.

(defn- compile-rung-form
  "`*ns*` bound for the same reason as gates/compile-form: the rungs are now
  compiled on first use rather than at namespace load, and `str/blank?` and
  `engages-problem?` resolve in this namespace and nowhere the caller might
  happen to be."
  [form]
  (binding [*ns* (the-ns 'samizdat.agent.tools.ship)]
    (eval `(fn [~'ctx]
             (let [~'answer            (get ~'ctx :answer)
                   ~'problem           (get ~'ctx :problem)
                   ~'evidence          (get ~'ctx :evidence)
                   ~'uncovered-numbers (get ~'ctx :uncovered-numbers)
                   ;; Whether the branch's role may write: what covers a
                   ;; figure, and so what a refusal should ask for, differs.
                   ~'can-write?        (get ~'ctx :can-write? true)]
               ~form)))))

(def ship-gates
  "The lexical ship rungs, compiled from gates.edn :ship-gates.

  A function, memoized against the config generation, so adding or retuning a
  rung is the data edit the docstring in gates.edn promises. As a top-level
  def the rungs were compiled once at namespace load and `reload-config!`
  could not touch them."
  (util/generation-cache
   gates/gen
   #(mapv (fn [rung]
            (assoc rung
                   :when (compile-rung-form (:when rung))
                   :message (if (:message-form rung)
                              (compile-rung-form (:message-form rung))
                              (:message rung))))
          (gates/threshold :ship-gates))))

(defn ship-gate-block
  "The first lexical ship rung that fires on this evidence, or nil. Pure —
  `done` computes the evidence and calls this."
  [evidence]
  (some (fn [rung]
          (when ((:when rung) evidence)
            (let [m (:message rung)]
              (if (fn? m) (m evidence) m))))
        (ship-gates)))

;; --- the give_up rungs ------------------------------------------------------

(defn- compile-give-up-form
  "Like compile-rung-form, with the symbols a give_up rung sees. Its own
  bindings rather than a shared set, because a rung that could reference
  `answer` here would compile and always read nil."
  [form]
  (binding [*ns* (the-ns 'samizdat.agent.tools.ship)]
    (eval `(fn [~'ctx]
             (let [~'reason  (get ~'ctx :reason)
                   ~'problem (get ~'ctx :problem)
                   ~'floor   (get ~'ctx :floor)]
               ~form)))))

(def give-up-gates
  "The lexical give_up rungs, compiled from gates.edn :give-up-gates.
  Memoized against the config generation like `ship-gates`, so retuning them
  is the data edit gates.edn promises."
  (util/generation-cache
   gates/gen
   #(mapv (fn [rung]
            (assoc rung
                   :when (compile-give-up-form (:when rung))
                   :message (if (:message-form rung)
                              (compile-give-up-form (:message-form rung))
                              (:message rung))))
          (gates/threshold :give-up-gates))))

(defn give-up-block
  "The first give_up rung that fires, or nil.

  ABANDONING IS THE ONE ENDING THAT HAS TO EXPLAIN ITSELF, and until
  karamazov-ylte.1 it was the only one with no gate at all: `done` faces four
  lexical rungs plus verification, and `give_up` took an optional string and
  defaulted it to \"no reason given\". Of the four ways a branch can end that
  made this the cheapest, and a loop teaches by what it makes cheap.

  Lexical and model-free, like the ship rungs beside it. The bar is not
  'convince a judge' — it is that the branch spent a sentence naming what it
  tried and what stopped it, which a branch that is genuinely blocked can
  always do and a branch that has merely stopped cannot."
  [reason problem]
  (let [ctx {:reason reason :problem problem
             :floor (gates/threshold :give-up-reason-floor)}]
    (some (fn [rung]
            (when ((:when rung) ctx)
              (let [m (:message rung)]
                (if (fn? m) (m ctx) m))))
          (give-up-gates))))

;; --- shipping ---------------------------------------------------------------

(defmethod base/run-tool "done" [{:keys [branch] :as ctx}]
  ;; Slimmed from the proof harness's seven-rung ship gate: the audit, review
  ;; and LLM-relevance rungs left with the judge machinery. What remains is
  ;; every rung that runs with no model in the path — an answer must exist,
  ;; its figures must come from the evidence, and it must engage the problem
  ;; — now data-defined (gates.edn :ship-gates, drg-4026 #44). The coding
  ;; loop's ship gate (tests pass, review passed) rebuilds on this seam.
  (let [answer (base/arg ctx :answer)
        ;; An ADVISORY branch (a reviewer or supervisor role loop) delivers a
        ;; VERDICT through done, not shippable work: it quotes the run's own
        ;; figures ("19 tests, 7 failed") and, on a red tree, describes the
        ;; redness it is reporting. The figure rung demanded artifacts for
        ;; those numbers and the verify rung demanded the very green the
        ;; verdict may be saying is absent, so every advisory role ground out
        ;; its whole budget unable to say what it had concluded, and the
        ;; caller read an exhaustion fallback instead of the verdict
        ;; (karamazov-t86 — observed on every supervisor branch of three
        ;; consecutive live runs). run-role marks the branch.
        advisory? (boolean (:advisory? branch))
        confirmed (state/confirmed-artifacts branch)
        ;; Plus what the branch itself measured — its eval and shell output
        ;; off the journal. Computed here and banked nowhere: it is evidence
        ;; for THIS check, not an artifact to share (karamazov-3s54).
        observed (when (and (:conn ctx) (:run-id ctx))
                   (observed-output
                    (journal/branch-turns (:conn ctx) (:run-id ctx) (:id branch))
                    ;; A role that cannot write describes what it read, and
                    ;; may quote it (gates.edn :tool-vocab :describing).
                    (when-not (roles/may-use? (:role branch) "write_file")
                      (gates/tool-vocab :describing))))
        own (concat confirmed (state/empirical-artifacts branch)
                    (when observed [observed]))
        ;; And what the rest of the run established: a branch is shown the
        ;; shared-artifact block, so refusing the answer that cites it would
        ;; punish the branch for reading what the harness handed it (vf-b9c).
        elsewhere (when (and (:conn ctx) (:run-id ctx))
                    (journal/corroborating-artifacts
                     (:conn ctx) (:run-id ctx) (:id branch)))
        evidence (concat own elsewhere)
        problem (:problem branch)
        uncovered (uncovered-tokens answer evidence [problem])
        uncovered-numbers (filter number-token? uncovered)
        borrowed (when (seq elsewhere)
                   (seq (remove (set uncovered)
                                (uncovered-tokens answer own [problem]))))
         block (when-not advisory?
                 (ship-gate-block
                  {:answer answer :problem problem
                   :evidence evidence
                   :uncovered-numbers uncovered-numbers
                   :can-write? (roles/may-use? (:role branch) "write_file")}))
        ;; The test rung — what makes the loop test-driven rather than one-shot.
        ;; `done` is not terminal until the unit's tests actually pass: run the
        ;; unit's tests, and a red / hollow / untested result is fed back so the
        ;; branch keeps iterating. Verification is FOCUSED — it runs only the
        ;; test namespaces the branch touched, so the loop iterates in seconds
        ;; against its own new test. On when a :verify-cmd is set or
        ;; :verify-focused? is true; loops with neither behave exactly as before.
        verify-cmd (get-in ctx [:config :run :verify-cmd])
        verify-focused? (get-in ctx [:config :run :verify-focused?] false)
        require-test? (get-in ctx [:config :run :require-test?] true)
        ;; THE TEST FILE THIS PIECE WAS DELEGATED, from the task it holds.
        ;; A split leaves the tree RED on purpose — the stubs are the failing
        ;; tests — so a child that ran the whole suite would drown in its
        ;; siblings' unimplemented work and conclude it had broken something.
        ;; Its contract names the tests that define ITS delivery, and those are
        ;; the ones it is judged on (karamazov-ioo.15).
        held (when (and (:conn ctx) (:run-id ctx) (:id branch))
               (tasks/held-by (:conn ctx) (:run-id ctx) (:id branch)))
        ;; THE HELD TASK AND ANY PIECES UNDER IT. A branch assembling a split
        ;; is judged on its own contract AND on every child's, which is what
        ;; makes the parent's freedom to adjust the pieces safe: it may reshape
        ;; a signature that turned out awkward, and it may not quietly break a
        ;; piece that met its contract. A branch with no children reduces to
        ;; the plain delegated case, and a branch holding nothing to the old
        ;; behaviour exactly.
        specs (when held
                (into [held] (when (:conn ctx) (tasks/children-of (:conn ctx) (:id held)))))
        contracted-tests (->> specs
                              (map #(str (:tests %)))
                              (filter verify/test-file?)
                              distinct
                              vec
                              not-empty)
        ;; THE OTHER HALF OF THE CONTRACT, and the half green tests cannot see:
        ;; are the functions it was handed implemented. The composition calls
        ;; them by name, so a test passing around a hollow stub, or a stub
        ;; deleted rather than filled, leaves that caller broken. Read off the
        ;; tree at ship time rather than trusted.
        source-of (fn [path]
                    (when-let [abs (files/resolve-under-root (:root ctx) path)]
                      (let [f (java.io.File. ^String abs)]
                        (when (.isFile f) (slurp f)))))
        unfilled (vec (distinct
                       (mapcat (fn [t]
                                 (let [file (not-empty (str (:stub_file t)))
                                       owed (when (not-empty (str (:stubs t)))
                                              (str/split (str (:stubs t)) #","))]
                                   (when (and file (seq owed))
                                     ;; A file gone or escaping the root leaves
                                     ;; everything it should define unfilled:
                                     ;; the honest answer, and the safe one.
                                     (stubs/unfilled (source-of file) owed))))
                               specs)))
        stub-file (not-empty (str (:stub_file held)))
        ;; A contract that names tests turns the rung ON by itself. The whole
        ;; point of the delegation is that those tests define delivery, so a
        ;; piece must not ship without them having been run — whether or not
        ;; the run happened to configure verification.
        verify-on? (and (not advisory?)
                        (nil? block)
                        ;; A role that cannot write has no change to verify:
                        ;; the answerer's reply is its whole deliverable, and
                        ;; this rung refused it for changing no files — run
                        ;; 3020cbca's branches wrote four documents to get
                        ;; past it (karamazov-1wv9). A role the table does not
                        ;; name may write, so every other run is as before.
                        (roles/may-use? (:role branch) "write_file")
                        (or verify-focused? contracted-tests
                            (not (str/blank? (str verify-cmd)))))
        changed (when verify-on? (gitdiff/changed-files (:root ctx) (:git-baseline ctx)))
        ;; Prefer the focused command; fall back to the configured one. Run only
        ;; when the cheap pre-checks (nothing changed / no test yet) haven't
        ;; already doomed the ship — a wasted suite run is a wasted minute.
        ;;
        ;; The contracted tests are focused on ALONGSIDE whatever the branch
        ;; touched, not instead of it: a child that wrote extra tests of its own
        ;; should have them run too, and a child that edited a sibling's test
        ;; file should have to face it.
        focus (distinct (concat contracted-tests
                                (when (or verify-focused? contracted-tests) changed)))
        cmd (when verify-on? (or (verify/focused-cmd focus) verify-cmd))
        pre-doomed? (or (and (some? changed) (empty? changed))
                        (and require-test? (some? changed) (seq changed)
                             (not (some verify/test-file? changed))
                             (nil? contracted-tests)))
        vresult (when (and verify-on? cmd (not pre-doomed?))
                  (verify/run-verify (:root ctx) cmd
                                     (get-in ctx [:config :run :verify-timeout-ms])))
        verify-block (verify/verify-block
                      {:verify-on? verify-on? :result vresult
                       :changed changed :require-test? require-test?
                       :contracted-tests contracted-tests
                       :unfilled unfilled :stub-file stub-file})
        block (or block verify-block)
        ;; THE ACCEPTANCE RUNG (karamazov-a6mj.2): criteria the OPERATOR
        ;; wrote before the run, in .samizdat/config.edn — which the run
        ;; cannot write — checked over the tree as it stands. Only the :check
        ;; criteria run here: this gate is model-free like every other rung
        ;; in it, and the :judge criteria wait for the critic role at
        ;; :feature/verify. Not paid for when an earlier rung already
        ;; refused, on ship-verify's economy — the answer is going back
        ;; anyway. system/start! validated the spec, so normalize cannot
        ;; throw here on a spec that let the system come up.
        criteria (when-not advisory?
                   (acceptance/normalize (get-in ctx [:config :run :acceptance])))
        acceptance (when (and (seq criteria) (nil? block))
                     (acceptance/check
                      criteria
                      {:kinds #{:check}
                       :run-check #(verify/run-verify
                                    (:root ctx) %
                                    (get-in ctx [:config :run :verify-timeout-ms]))}))
        block (or block (some-> acceptance acceptance/refusal))]
    ;; Journalled whether the tests RAN or not. A rung that was configured on
    ;; and then did nothing used to leave no trace at all — the note fired only
    ;; when there was a result — so a run that shipped unverified looked
    ;; identical in the record to one that shipped green. `:ran false` with a
    ;; reason is the difference between a gate that passed and a gate that was
    ;; never asked.
    ;; The live tally, so a supervisor can see the gate being skipped WHILE it
    ;; is happening rather than by reading the journal afterwards.
    (when verify-on?
      (let [on-branch (when (and (:run-id ctx) (:id branch))
                        [(:run-id ctx) (:id branch)])]
        (session/observe! (if vresult
                            [:verify (if (:green? vresult) :green :red)]
                            [:verify :skipped])
                          on-branch)
        (when vresult (session/observe! [:verify :ran] on-branch))))
    (when (and verify-on? (:conn ctx) (:run-id ctx))
      (journal/note! (:conn ctx) (:run-id ctx) :ship-verify
                     {:branch-id (:id branch) :turn (:turn ctx)
                      :data (if vresult
                              {:ran true
                               :green (:green? vresult) :timeout (:timeout? vresult)
                               :blocked (some? verify-block)}
                              {:ran false
                               :blocked (some? verify-block)
                               ;; A keyword, not a sentence: this is a journal
                               ;; row somebody queries, and a stable token is
                               ;; worth more to whoever is counting than prose
                               ;; that reads nicely once.
                               :why (cond
                                      (nil? changed) :no-git-baseline
                                      (empty? changed) :nothing-changed
                                      (nil? cmd) :no-test-among-changed
                                      :else :pre-checks-decided)})}))
    ;; Per criterion, whichever way it went: the table is what a reader of
    ;; the run — and the arena's measure — sees of the operator's definition
    ;; of done, and a criterion not run here says so ("not run") rather than
    ;; reading as met.
    (when (and acceptance (:conn ctx) (:run-id ctx))
      (journal/note! (:conn ctx) (:run-id ctx) :acceptance
                     {:branch-id (:id branch) :turn (:turn ctx)
                      :data {:at "done"
                             :passed? (acceptance/all-passed? acceptance)
                             ;; Bounded like every judgement the harness
                             ;; journals (karamazov-3htz): a check's output
                             ;; is a test log, and a test log can be long.
                             :results (mapv #(update % :output
                                                     (fn [o] (util/truncate-middle
                                                              (str o)
                                                              (:reply-chars (gates/threshold :verdict-record)))))
                                            acceptance)}}))
    ;; Journalled whether or not anything blocked, so the run record still
    ;; shows what the lexical check saw even though words no longer decide.
    (when-let [words (and (:conn ctx) (:run-id ctx)
                          (seq (remove number-token? uncovered)))]
      (journal/note! (:conn ctx) (:run-id ctx) :uncovered-words
                     {:branch-id (:id branch) :turn (:turn ctx)
                      :data {:words (vec (take 20 words)) :blocked? (some? block)}}))
    (when (and borrowed (:conn ctx) (:run-id ctx))
      (journal/note! (:conn ctx) (:run-id ctx) :cross-branch-citation
                     {:branch-id (:id branch) :turn (:turn ctx)
                      :data {:tokens (vec (take 20 borrowed))
                             :sources (vec (distinct (keep :branch_id elsewhere)))}}))
    (if block
      (base/fail branch (str "`done` refused.\n\n" block) :done-block block)
      (cond->
       {:branch (assoc branch :final-answer answer :status :done)
        :category :success
        :progress? true
        :done? true
        :answer answer
        ;; The green point the safe-state rung rewinds to (loop.clj tool-step
        ;; consumes this): done with the suite actually passing is the branch's
        ;; last known-good state.
        :verified-green? (boolean (:green? vresult))
        :result (str "Answer accepted.\n\n" answer)}

        ;; A green ship-verify IS this loop's confirmed artifact.
        ;;
        ;; The proof engines produced :confirmed artifacts and left with the
        ;; proof harness, and nothing replaced them — so `confirmed-artifacts`
        ;; was empty on every run, by construction, and everything keyed on it
        ;; was dead code that still read like live policy: the milestone,
        ;; branch-out and emergency-review gates could not fire, `shareable?`
        ;; admitted nothing so the shared pool stayed empty (and with it
        ;; corroborating-artifacts and seed-from-run!), the figure-coverage
        ;; ship rung had no evidence to check figures against, two of the
        ;; winner rubric's five components were always 0, and arbiter's
        ;; `progressed?` — which is how prologue-cap and progress-stalled
        ;; settle — could never be true.
        ;;
        ;; A test run the harness itself spawned and observed exit 0 is the
        ;; coding loop's exact analogue of an engine confirmation: machine-
        ;; checked, not self-reported, and produced by the same
        ;; verify-before-you-ship discipline. So emit one.
        ;;
        ;; :tier :slow because a real test run is not a one-shot syntax check
        ;; — it is the cross-check the rubric's slow-seen component means.
        (:green? vresult)
        (assoc :artifact {:kind :test
                          :claim answer
                          :code cmd
                          :verdict :pass
                          :claim-status :confirmed
                          :tier :slow})))))

(defmethod base/run-tool "give_up" [{:keys [branch] :as ctx}]
  ;; The rungs are in gates.edn; see `give-up-block` for why this ending has
  ;; any at all. An ADVISORY branch is exempt for the same reason it is exempt
  ;; from the ship rungs (karamazov-t86): a reviewer or supervisor loop
  ;; reporting that it cannot reach a verdict is delivering that verdict, and
  ;; holding it to a contract about project work would strand it.
  (let [reason (str (base/arg ctx :reason))
        blocks (or (:give-up-blocks branch) 0)
        ;; BOUNDED, and the bound is not optional. :max-done-blocks is "how
        ;; often `done` may be refused before the branch is told to give up",
        ;; so giving up IS the escape hatch from a refused done — gating it
        ;; with no ceiling leaves a stuck branch refused at both exits and
        ;; spending its budget being told no, which is the quiet spin these
        ;; rungs exist to stop, rebuilt one door along. Fail-open on
        ;; cells/critic.clj's reasoning: a backstop that can wedge the loop is
        ;; worse than no backstop.
        relieved? (>= blocks (gates/threshold :max-give-up-blocks))
        block (when-not (or (:advisory? branch) relieved?)
                (give-up-block reason (:problem branch)))]
    (if block
      ;; :done-block, NOT a key of its own, and the existing wiring says this
      ;; is right: gates.edn's :settle-called already lists `give_up` in the
      ;; :done-blocked gate's vocabulary, so that gate was always meant to
      ;; cover both terminal calls. A key nothing reads would mean a refused
      ;; give_up drew no steer and spent no :max-done-blocks budget.
      (base/fail (assoc branch :give-up-blocks (inc blocks))
                 (str "`give_up` refused.\n\n" block)
                 :done-block block)
      {:branch (assoc branch :status :abandoned :inactive-reason reason)
       :category :neutral :progress? false :gave-up? true
       ;; What the branch could not do is end CHEAPLY. When the ceiling
       ;; relieves it the abandonment still lands, and the record carries the
       ;; fact that the account never came — a finding rather than a silence.
       :gave-up-unaccounted? (boolean (and relieved? (seq (str reason))))
       :result (str "Gave up: " reason)})))

;; --- forking ----------------------------------------------------------------

;; Tier 1b: the cap is gates.edn :max-branch-theses — data, so a project
;; retunes its fork budget at runtime without a rebuild.

(defmethod base/run-tool "branch_theses" [{:keys [branch] :as ctx}]
  (let [proposals (base/arg ctx :theses)
        max-branch-theses (gates/threshold :max-branch-theses)]
    (cond
      (or (not (sequential? proposals)) (empty? proposals))
      (base/malformed branch (str "`theses` must be a non-empty array of"
                        " {goal, subClaims, technique} objects."))

      (> (count proposals) max-branch-theses)
      (base/malformed branch (str "At most " max-branch-theses " theses per call; you proposed "
                             (count proposals) "."))

      (not (every? #(and (map? %) (string? (:goal %))) proposals))
      (base/malformed branch "Every thesis must be an object with a `goal` string.")

      :else
      ;; The first commits THIS branch; the rest become siblings. The scheduler
      ;; reads :pending-branch-theses after the turn and clears it, so a tool
      ;; never creates a branch itself — one place owns the branch table.
      (let [[mine & others] proposals
            thesis (assoc mine :set-at-turn (:turn ctx))]
        (base/ok (assoc branch :thesis thesis
                   :pending-branch-theses (vec others))
            (str "Committed to: " (:goal thesis)
                 (when (seq others)
                   (str "\nRequested " (count others) " sibling branch(es) for: "
                        (str/join "; " (map :goal others))
                        "\nThey explore independently and share this branch's"
                        " failure log, so none of you will repeat another's"
                        " dead end.")))
            :progress? true
            :thesis thesis)))))
