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

(ns samizdat.base-test
  "The mechanical check for the base/userspace rule (RFC-001).

  The rule the whole project rests on is that `src/` holds MECHANISM and
  `resources/` holds BEHAVIOUR, because only the second half is editable by
  the agent at runtime. Until this namespace existed the rule had nothing
  enforcing it: a behaviour added to `src/` failed no test, and RFC-001 said
  so under Known gaps.

  Three rules, because one is not enough. An audit by numeric literal alone
  found the context-budget family and passed straight over four real
  violations that were prose or vocabulary rather than numbers — an entire
  prompt built with `str` in planner.clj, half of critic's prompt inline
  while the other half came from resources, verify.clj's branch-facing
  messages, and judge.clj's severity vocabulary as a regex literal. A prompt
  in compiled code is exactly as unreachable to the supervisor as a threshold
  in compiled code, so all three shapes are scanned:

    :prose       a string the model reads
    :vocabulary  a regex that decides an outcome from words
    :threshold   a number a decision is compared against

  Every hit is either fixed or listed in `allowed` WITH ITS REASON. The
  allow-list is the point of the check rather than a hole in it: a literal
  may stay in the base, but somebody has to write down why, and a new one
  cannot arrive silently."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [clojure.walk :as walk]
            [jolt.fs :as fs]))

;; --- reading src ------------------------------------------------------------

(def ^:private definition-forms
  '#{def defn defn- defmacro defmulti deftype defrecord ns})

(defn- strip-docstrings
  "The forms with every docstring removed.

  A docstring is prose, and prose written for a human reading the source is
  exactly what this check must NOT flag — otherwise the rule that keeps the
  model's words out of the base would punish explaining the base. The
  distinction is positional, so it is made structurally rather than by
  guessing from content: the third element of a definition form, the `:doc`
  value of an attribute map, and the trailing string of a `defprotocol`
  method signature."
  [form]
  (walk/postwalk
   (fn [x]
     (cond
       (and (seq? x) (symbol? (first x))
            (= 'defprotocol (symbol (name (first x)))))
       (map (fn [m] (if (and (seq? m) (string? (last m))) (butlast m) m)) x)

       (and (seq? x) (symbol? (first x))
            (definition-forms (symbol (name (first x))))
            (>= (count x) 3) (string? (nth x 2)))
       (concat (take 2 x) (drop 3 x))

       (and (map? x) (string? (:doc x))) (dissoc x :doc)

       :else x))
   form))

(defn- src-files []
  ;; src/samizdat, not src. The vendored trees (mycelium, maestro, ring_chez)
  ;; moved under src/ and are not ours to hold to this rule — it is about
  ;; samizdat's own discipline of keeping model-facing prose out of compiled
  ;; code, and a vendored library's docstrings are neither prose we wrote nor
  ;; prose the model reads.
  (sort (map str (fs/glob "src/samizdat" "**.clj"))))

(defn- forms-of [file]
  ;; Read rather than grep. The reader knows which characters are inside a
  ;; string, which are a comment, and which are a regex literal; a text scan
  ;; knows none of that and would flag the word "IMPORTANT" in a comment
  ;; while missing a prompt assembled across two lines.
  (map strip-docstrings
       (read-string {:read-cond :allow} (str "[" (slurp file) "]"))))

(defn- collect [pred form]
  (let [acc (atom [])]
    (walk/postwalk (fn [x] (when (pred x) (swap! acc conj x)) x) form)
    @acc))

;; --- the three rules --------------------------------------------------------

(def ^:private prose-markers
  ["You are" "Your task" "Return ONLY" "Return only" "## " "You must"
   "IMPORTANT" "Do not " "Respond with" "Answer with"])

(def ^:private prose-length
  ;; Long enough that no format string, error message or SQL fragment reaches
  ;; it by accident, short enough that a paragraph addressed to the model
  ;; does. A marker catches the short prose; this catches the prose that
  ;; happens to use none of the markers.
  240)

(def ^:private sql-shaped
  ;; SQL is the store's mechanism and its statements are long by nature. It
  ;; is excluded by SHAPE rather than by listing the store's files, so a
  ;; prompt that appeared in a store namespace would still be caught.
  #"(?is)^\s*(SELECT|INSERT|UPDATE|DELETE|CREATE|DROP|ALTER|PRAGMA|WITH|BEGIN|COMMIT|VACUUM)\b")

(defn- prose-hits [forms]
  (->> (mapcat (partial collect string?) forms)
       (remove #(re-find sql-shaped %))
       (filter (fn [s] (or (> (count s) prose-length)
                           (some #(str/includes? s %) prose-markers))))
       distinct))

(def ^:private word-bearing
  ;; What separates a regex that PARSES from a regex that DECIDES. A parser's
  ;; pattern is escapes and character classes — `\s*(\d+)`, `(?s)\{.*\}` — and
  ;; carries no vocabulary. A pattern holding a run of four or more letters is
  ;; matching on words somebody chose, and whoever may change those words must
  ;; be able to reach the pattern.
  #"[A-Za-z]{4,}")

(defn- vocabulary-hits [forms]
  (->> (mapcat (partial collect #(instance? java.util.regex.Pattern %)) forms)
       (map str)
       (filter #(re-find word-bearing %))
       distinct))

(def ^:private comparison-forms '#{< > <= >= min max})

(def ^:private structural-numbers
  ;; Not thresholds in any interesting sense: an index, an off-by-one, a
  ;; pair. Flagging them would bury the numbers that decide something.
  #{0 1 2 -1 0.0 1.0})

(defn- interesting-numbers [xs]
  (filter #(and (number? %) (not (structural-numbers %))) xs))

(defn- threshold-hits
  "Numbers a decision is made against.

  Three shapes, because the first two alone missed a whole family. A number
  compared with, and the fallback of an `or`, are the obvious ones. The third
  is a `def` whose value is a bare number — `max-ledger-claim-chars` was 180
  under a paragraph of justification, which reads as diligence and is exactly
  as unreachable to a supervisor as an anonymous literal would be. Naming a
  constant documents it; it does not move it.

  A number merely computed with is not a threshold: the question this rule
  asks is whether the base contains a value some project might want
  different, not whether arithmetic happened."
  [forms]
  (->> forms
       (mapcat
        (fn [form]
          (collect
           (fn [x]
             (and (seq? x) (symbol? (first x))
                  (let [h (symbol (name (first x)))]
                    (cond
                      (or (comparison-forms h) (= 'or h))
                      (seq (interesting-numbers (rest x)))
                      ;; (def name 180) — the docstring is already stripped,
                      ;; so a two- or three-element def with a numeric tail.
                      (= 'def h)
                      (and (<= (count x) 3) (seq (interesting-numbers (drop 2 x))))
                      :else false))))
           form)))
       (mapcat (fn [x] (interesting-numbers (rest x))))
       distinct))

;; --- what may stay, and why -------------------------------------------------

(def ^:private allowed
  "Literals that stay in the base, each with the reason it is mechanism.

  `:all` allows a whole rule for a file. It is a deliberate coarsening, used
  only where every hit in the file is one class with one reason, and it is
  the weaker form: a new literal of that class in that file will not fail.
  Everywhere else the entry names the literal, so a new one does."
  {"src/samizdat/config.clj"
   {:threshold {:all "config.clj IS the defaults layer: every number here is
                      the fallback for a key an operator sets in samizdat.edn,
                      and the file's whole subject is what to do when that key
                      is absent. Moving them to resources would make the
                      config file's defaults depend on a resource read that
                      the config file has to happen before."}}

   "src/samizdat/server.clj"
   {:threshold {:all "HTTP status codes and the response-body cap of a
                      transport. Protocol constants, not policy."}}

   "src/samizdat/symbolic.clj"
   {:threshold {100 "The rewrite step bound: a runaway backstop, not a tunable.
                     A ruleset that terminates converges in a handful of steps,
                     so raising this cannot express a different policy — only
                     postpone the error a cycling ruleset was always going to
                     raise. A caller that genuinely wants a different bound
                     passes one to the 3-arity, the same seam api/runs.clj uses
                     for page sizes."}}

   "src/samizdat/agent/tournament.clj"
   {:threshold {1103515245 "The LCG multiplier — a PRNG's algorithm constants,
                            like a hash function's primes. Retuning them at
                            runtime cannot express a different policy, only a
                            broken shuffle; what IS policy (the seed, the
                            pivot count) the caller injects."
                12345 "The LCG increment, same reasoning."
                2147483648 "The LCG modulus (2^31), same reasoning."}}

   "src/samizdat/agent/trajectory.clj"
   {:threshold {19 "The span of the A-T letter scale: T minus A. Arithmetic of
                    the alphabet, not a tunable — the scale's GRANULARITY as a
                    choice lives in the criteria and prompt wording
                    (gates.edn :trajectory-score), and changing this number
                    without changing the prompt's 'A to T' would misgrade
                    every reply."}}

   "src/samizdat/llm/client.clj"
   {:threshold {:all "HTTP status classes (200/299/500) and the retry ladder's
                      wall-clock bounds, which RFC-005 fixes deliberately: the
                      ladder is the one thing a provider adapter may not
                      diverge from, so it is not per-project tunable."}}

   "src/samizdat/api/runs.clj"
   {:threshold {:all "Page sizes for a read API. A client that wants fewer
                      passes a limit."}}

   "src/samizdat/engine/proc.clj"
   {:threshold {30000 "Subprocess wall-clock bound. A capability's own safety
                       bound, not a decision about the work."
                2000 "SIGTERM grace before SIGKILL. How long a dying process
                      gets to die, which is a property of processes."}}

   "src/samizdat/events.clj"
   {:threshold {256 "The event tap's sliding-buffer size. RFC-009 states it as
                     a contract: a slow subscriber loses events rather than
                     applying backpressure, because the durable journal is the
                     source of truth and a client that fell behind re-reads by
                     cursor. Tuning it cannot change what a run DOES — only
                     how much a watcher may miss — and a subscriber that
                     stalls the loop is the outcome it exists to prevent."}}

   "src/samizdat/mutation.clj"
   {:threshold {10000 "The soak timeout, and one of the two bounds the
                       mutation protocol is safe BECAUSE OF (RFC-002): a cell
                       that loops cannot hang the protocol.

                       Deliberately not userspace, for the reason secrets.clj
                       is not. Userspace is what the agent edits, and the soak
                       is what stands between a bad edit and a wedged harness
                       — an agent able to raise its own soak timeout can hang
                       the process with one cell and one edit."}}

   "src/samizdat/repl.clj"
   {:threshold {10000 "The default eval timeout, as mutation.clj's soak:
                       in-process eval is the substrate the mutation protocol
                       is built on, and its bound must not be reachable from
                       inside it. Callers pass their own where a longer one is
                       warranted."}}

   "src/samizdat/tape.clj"
   {:threshold {120 "`default-floor`: the shortest a compaction replacement may
                     be before it is refused as having lost the message. An
                     arity default for a PURE function — every production
                     caller passes the value from `:context-budget` — and the
                     namespace takes no configuration by design (RFC-004: the
                     tape decides nothing)."}}

   "src/samizdat/security/policy.clj"
   {:threshold {120000 "Shell command timeout. A security bound, and see the
                        note on secrets.clj below: a control the agent could
                        retune is not a control."}
    :vocabulary {"(?s)^(>&\\d+|>>?[ \\t]*/dev/null)"
                 "What a benign redirection IS — `2>&1`, `2>/dev/null` — is
                  POSIX shell structure the lexer must recognize, like the
                  statement separators beside it. Not a vocabulary a project
                  tunes: widening it weakens the redirection downgrade, which
                  is a security control (see the secrets.clj note)."}}

   "src/samizdat/repl/guard.clj"
   ;; The kernel-path pattern used to need an allowance: it read
   ;; `(^|/)(src|vendor)/`, and "vendor" is a run of four letters, so the
   ;; vocabulary rule counted it as a pattern deciding on words somebody
   ;; chose. The vendored trees now live under src/, the pattern is `src/`
   ;; alone, and three letters is not a vocabulary — it is structure, which
   ;; is what the rule means to let through. No entry, by the rule's own test.
   {:vocabulary {"samizdat"
                 "How the guard recognises a HARNESS namespace in a
                  `(require … :reload)` — the hot-load half of the same
                  escape. Same reasoning; a run that could edit this could
                  rename its way past it."}}

   "src/samizdat/repl/route.clj"
   {:vocabulary {"posix_spawn|Operation not permitted|Permission denied|EPERM"
                 "What the KERNEL says when the sandbox refuses. Protocol
                  strings from the OS, like the LSP header below — not a
                  vocabulary anyone tunes, and widening it would only make
                  ordinary failures read as policy refusals."
                 "posix_spawn|process|sh\\b|exec"
                 "Which half of the refusal message to show — the exec advice
                  or the path advice. Same OS strings, same reasoning."}}

   "src/samizdat/lsp/client.clj"
   {:threshold {5000 "LSP request timeout."
                20000 "LSP read timeout."}
    :vocabulary {"(?i)Content-Length:\\s*(\\d+)"
                 "The LSP base protocol header. Fixed by the specification;
                  nobody may retune it."}}

   "src/samizdat/lisp.clj"
   {:threshold {256 "Reader recursion bound."}}

   "src/samizdat/agent/state.clj"
   {:threshold
    {34 "Characters of task slug in a branch id. An ID FORMAT, not a policy:
         the slug exists so a human and the journal can tell two of an owner's
         branches apart at a glance, and the bound keeps it a label rather
         than a description. Changing it at runtime would make ids for the
         same task differ between rounds, which is the confusion the slug was
         added to remove."
     8 "Minimum slug length before the word-boundary cut applies. Below it the
        cut is taken mid-word rather than producing a uselessly short slug.
        Part of the same format rule."}}

   "src/samizdat/hashline.clj"
   {:threshold
    {3 "Hex chars in an anchor's content hash — the WIRE FORMAT, not a
        tuning knob. Every anchor already printed into a branch's context
        carries this width, so changing it at runtime would invalidate every
        outstanding anchor mid-run, and it is also what makes our anchors
        identical to the ones vis mints. The drift TOLERANCE, which is a real
        judgement call, does live in gates.edn (:anchor)."}}

   "src/samizdat/store/db.clj"
   {:threshold {120 "Busy-timeout clamp for SQLite."}}

   "src/samizdat/store/knowledge.clj"
   {:threshold {5 "Id-collision retry count. A UNIQUE collision is
                   astronomically unlikely twice; five is a loop bound, not a
                   policy."}}
   "src/samizdat/store/tasks.clj"
   {:threshold {5 "Id-collision retry count, as knowledge.clj."}}
   "src/samizdat/store/messages.clj"
   {:threshold {5 "Id-collision retry count, as knowledge.clj."
                60 "Preview truncation for a message listing — display, not a
                    decision."}}

   "src/samizdat/smoke.clj"
   {:threshold {2000 "Platform-probe timeout. smoke is a diagnostic, not the
                      loop."}}

   "src/samizdat/control.clj"
   {:threshold {160 "Single-line truncation for a directive echo."}}

   "src/samizdat/workflow.clj"
   {:threshold {40 "Fallback max-turns when a caller passes none. The real
                    value is config; this is the arity-1 default."}}
   "src/samizdat/agent/beam.clj"
   {:threshold {8 "Concurrency floor for the advance pool."
                40 "Fallback max-turns, as workflow.clj."
                5 "Fallback beam width when config carries none."}}

   "src/samizdat/llm/message.clj"
   {:threshold {90 "Error-text truncation for a log line."}
    :vocabulary {"(?s)<think>.*?</think>"
                 "DeepSeek and Zhipu both document that reasoning_content must
                  not be fed back. The tag is the providers' wire format, not
                  samizdat's vocabulary."}}

   "src/samizdat/llm/fence.clj"
   {:vocabulary {:all "The tool-call fence is the ABI between the harness and
                       the model — the one format the base must be able to
                       read before any resource has been loaded. It is
                       documented to the model in prompts/system.md, and the
                       two are pinned together by prompt-test rather than by
                       moving the parser out of the base: a project that
                       changes the fence is changing the base's wire format
                       and is rebuilding anyway."}}

   "src/samizdat/llm/adapter/openai.clj"
   {:vocabulary {"/beta/?$"
                 "DeepSeek's /beta path, which serves completions but not
                  model listing. A URL shape."}}

   "src/samizdat/security/secrets.clj"
   {:vocabulary {:all "The credential shapes redaction recognises.

                       This one is load-bearing in the opposite direction from
                       the rest of the rule. Userspace is agent-editable by
                       design, so a security control placed there is a control
                       the agent can switch off — and RFC-003's boundary is
                       worth exactly as much as the agent's inability to move
                       it. The vendor-prefix table therefore stays compiled,
                       and that is the reason, not an oversight."}}})

(defn- allowance [file rule literal]
  (let [entry (get-in allowed [file rule])]
    (or (get entry :all) (get entry literal))))

;; --- the check --------------------------------------------------------------

(defn violations
  "Every literal in `src/` that the base/userspace rule would question and
  that nothing in `allowed` accounts for. Public so a REPL can run the audit
  without the assertion."
  []
  (for [file (src-files)
        :let [forms (forms-of file)]
        [rule hits] [[:prose (prose-hits forms)]
                     [:vocabulary (vocabulary-hits forms)]
                     [:threshold (threshold-hits forms)]]
        hit hits
        :when (not (allowance file rule hit))]
    {:file file :rule rule :literal hit}))

(defn- render [{:keys [file rule literal]}]
  (let [s (str literal)]
    (format "  %-44s %-11s %s" file (name rule)
            (pr-str (subs s 0 (min 70 (count s)))))))


;; --- the sentence rule, and its backlog -------------------------------------

(def ^:private function-words
  "Words that make a string a SENTENCE rather than a list of names. A format
  fragment, a SQL clause and a vector of keywords have none of them."
  #{"the" "a" "an" "is" "are" "was" "to" "of" "and" "or" "not" "no" "it" "this"
    "that" "for" "in" "on" "with" "you" "your" "its" "has" "have" "but" "so"
    "if" "than" "then" "does" "do" "be"})

(def ^:private human-facing-forms
  "Forms whose string arguments are for a PERSON reading a log or a stack
  trace, not for the model. Matched on the bare name, so `log/warn` and
  `warn` are the same form — the first version of this compared the qualified
  symbol against a set of qualified symbols and matched nothing, which is why
  it looked like the codebase logged less than it does."
  '#{warn info error debug trace println print pr prn ex-info throw
     assert is testing deftest format printf})

(defn- strip-human-facing
  "The form with every string beneath a log or exception call removed.

  Recursively, not just the direct children: `(ex-info (str \"cells require \"
  ks \" that no driver provides\") {})` puts its message inside a `str`, and a
  shallow strip leaves those fragments looking like model-facing prose. They
  are what a developer reads off a stack trace."
  [form]
  (walk/postwalk
   (fn [x]
     (if (and (seq? x) (symbol? (first x))
              (human-facing-forms (symbol (name (first x)))))
       (walk/postwalk (fn [y] (if (string? y) ::human y)) x)
       x))
   form))

(defn- sentence-hits
  "Strings that read as a sentence addressed to somebody, once the log and
  exception messages are removed.

  This is the rule the marker-and-length one could not be. `:prose` catches a
  markdown heading or a paragraph; it does not catch \"Change the call, or
  change technique\" — a short branch-facing sentence with no heading, no
  marker and forty characters. Those are most of what the model actually
  reads from the harness."
  [forms]
  (->> forms
       (map strip-human-facing)
       (mapcat (partial collect string?))
       (remove #(re-find sql-shaped %))
       (filter (fn [s]
                 (let [ws (re-seq #"[A-Za-z][A-Za-z'-]{1,}" s)]
                   (and (>= (count ws) 5)
                        (some function-words (map str/lower-case ws))))))
       distinct))

(def ^:private prose-backlog
  "Model-facing sentences still assembled in `src/`, by file.

  THIS IS A BACKLOG, NOT AN ALLOWANCE, and the distinction is the whole point
  of keeping it separate from `allowed`. Every entry in `allowed` is a
  decision with a reason. Every entry here is a violation of the base/userspace
  rule that predates the check finding it — the harness's entire branch-facing
  surface (tool descriptions, refusal messages, the fence parser's complaints,
  the mutation protocol's verdicts) is built with `str` in compiled code,
  where a supervisor cannot reach a word of it.

  It is here rather than unmentioned because a lint that cannot be turned on
  is worth nothing: this way a NEW sentence in `src/` fails immediately, and
  the existing ones are counted, visible, and shrinkable. An entry that stops
  appearing must be deleted — the ratchet only turns one way."
  {
   "src/samizdat/agent/beam.clj"
   #{
    "the turn returned no :branch"
    }
   "src/samizdat/agent/critic.clj"
   #{
    "\n\nSibling theses (the diversity this branch is judged against):\n"
    "\n\nWhat the harness last told the branch:\n"
    "  (none — this is the only branch)"
    }
   "src/samizdat/agent/decompose.clj"
   #{
    "Add a focused test that proves the behaviour this unit requires:\n"
    "Make the smallest change that satisfies this unit, then stop:\n"
    "depth exhausted; fresh approach did not land"
    "generic split (architect gave no usable decomposition)"
    "no recovery at depth floor"
    }
   "src/samizdat/agent/judge.clj"
   #{
    "\n\nAddress this, then finish again when it holds."
    "Correctness could not be confirmed — add a focused test "
    "This may not be done yet."
    "or state the missing detail, then finish again."
    }
   "src/samizdat/agent/loop.clj"
   #{
    "\n\n[harness] This exact call has now"
    " Every turn must end with exactly one."
    " Repeating it will fail again. Change"
    " applies to the beam scheduler,"
    " different encoding of the same one."
    " different tool, a smaller claim, or a"
    " emitted a tool call. Think less and call a tool."
    " failed this exact way more than once."
    " the call, or change technique — a"
    "The explore prologue is over: "
    "Your re-planning budget is spent: "
    "[harness] No ```tool-call block in your response."
    "[harness] The provider call failed: "
    "[harness] Your response hit the token limit before you"
    "[harness] Your tool-call block did not parse: "
    }
   "src/samizdat/agent/skills.clj"
   #{
    "Skills are guidance you load only when a task matches. Load one"
    }
   "src/samizdat/agent/state.clj"
   #{
    " established unless an engine confirmed it."
    "PROGRESS REPORT — not a solution. Nothing below is"
    "no green verify to fall back to"
    "the turn log no longer reaches the green point — the journal was pruned or rewritten, and replaying past it would produce a session that never existed"
    }
   "src/samizdat/agent/supervisor.clj"
   #{
    " and test it — a rough version you refine beats more reading. If you were"
    " re-checking something already done, it is done: move to the next step"
    ") without changing anything. Commit your best current version to a file"
    }
   "src/samizdat/agent/tools.clj"
   #{
    ". This is a harness fault, not yours — the call was fine."
    "returned a result map with no :branch, so the turn had no branch to carry forward"
    "returned a result map with no :result for the model to read"
    }
   "src/samizdat/agent/tools/base.clj"
   #{
    " phase. The phase-valve message says when that changes."
    "` is not available in the "
    }
   "src/samizdat/agent/tools/experiments.clj"
   #{
    "`action` is `reverted` or `kept`.\n\n"
    }
   "src/samizdat/agent/tools/introspect.clj"
   #{
    " `doc` for any var whether it is curated or not."
    " names something that does not resolve: "
    "(no run database in this context — wiring only)"
    ". Call `manual` with no arguments for the whole surface, or"
    "=== THE HARNESS'S OWN COMMAND SURFACE ===\n"
    "Call these from `eval`. `manual` with a `name` gives one"
    "` is not in the manual. Groups: "
    "the manual could not be compiled — resources/manual.edn"
    }
   "src/samizdat/agent/tools/journal.clj"
   #{
    " Ids come from the settled-state block: `a#12` for"
    " it inherited. A run cannot reach another run's"
    " something this run established, `s#7` for something"
    "CONFIRMED (inherited from the seed run)"
    }
   "src/samizdat/agent/tools/lsp.clj"
   #{
    "clojure-lsp is not installed; install it to use the lsp tool"
    "could not start clojure-lsp for root"
    "diagnostics needs file. line/col are 0-based ints."
    "line and col must be integers. "
    }
   "src/samizdat/agent/tools/manifest.clj"
   #{
    " up on the next run."
    " will use it; tuning the active manifest is picked"
    }
   "src/samizdat/agent/tools/messages.clj"
   #{
    "Actions: send {to?, body} (to omitted = broadcast), inbox. "
    "`to` is a branch id like b2; broadcast reaches every branch but you."
    }
   "src/samizdat/agent/tools/mutate.clj"
   #{
    "\n\nFix it and save again."
    " Call reload_cells to make it live."
    " It is still the shipped template."
    " `cell list` shows this project's;"
    " `cells` shows what is loaded."
    " in this project — it compiled, it dry-ran, and it"
    " is live on your next turn. The shipped template is"
    " running the shipped templates. Any save starts its"
    " the version you left behind is still readable."
    " unchanged and nothing entered this project's"
    " unchanged; other projects still start from it."
    "' was NOT saved; the loop is"
    ". Reverting is itself an edit, so"
    "This project has stored no cell versions yet — it is"
    }
   "src/samizdat/agent/tools/ship.clj"
   #{
    "\nThey explore independently and share this branch's"
    " failure log, so none of you will repeat another's"
    " theses per call; you proposed "
    "Every thesis must be an object with a `goal` string."
    "`theses` must be a non-empty array of"
    }
   "src/samizdat/agent/tools/skills.clj"
   #{
    "Skills — load one with `skill load {name}` when it is"
    }
   "src/samizdat/agent/tools/tasks.clj"
   #{
    ": no such task, or another run holds it."
    }
   "src/samizdat/agent/verify.clj"
   #{
    "(java.lang.System/exit (if (clojure.core/pos? (+ (:fail s) (:error s))) 1 0)))"
    "The suite is green but you changed no files, so nothing was actually "
    "You added no test, so the new behaviour is not pinned. Write a focused "
    "done. Make the change on disk (edit_file/write_file), prove it with a "
    "test that FAILS without your change and passes with it, get it green, "
    "verify command failed to run: "
    }
   "src/samizdat/api/control.clj"
   #{
    "Applied now. Commands matching the pattern are allowed for the rest of this run; a hard deny still wins."
    "Queued. It applies at the branch's next turn boundary, not now."
    "a grant intervention needs payload.pattern — the shell glob to allow"
    "the run did not start within 30s"
    }
   "src/samizdat/api/openai.clj"
   #{
    "\n\nMeasured along the way — computations at the parameters"
    "The harness did not reach a verified answer ("
    "no user message in `messages`"
    }
   "src/samizdat/config.clj"
   #{
    "HARNESS_STOP_ON_FIRST_DONE"
    }
   "src/samizdat/control.clj"
   #{
    "Review what you have and ship it."
    }
   ;; samizdat/lisp.clj is off the backlog entirely: `balance` answers in DATA
   ;; (a reason plus line/col plus the reader's own words) and every sentence
   ;; around it moved to resources/prompts/file-tool.md, keyed by reason.
   "src/samizdat/llm/client.clj"
   #{
    " :max-tokens or shorten the context."
    " reply had no completion in it: "
    " returned neither content nor reasoning. This usually means"
    " the model spent its entire output budget thinking; raise"
    }
   "src/samizdat/llm/fence.clj"
   #{
    "tool-call `name` must not be empty"
    "tool-call body must be a JSON object with a non-empty `name` string"
    "tool-call body must be a JSON object, not an array or scalar"
    "tool-call body must have a `name` string"
    }
   "src/samizdat/mutation.clj"
   #{
    "reload: the edited cell file did not load — "
    "soak did not terminate within the time budget — the edited cell may loop"
    "soak run produced an error: "
    "the candidate did not load — "
    "validate: the loop no longer compiles — "
    "validate: these cells do not declare :pure or :effects, so the "
    }
   "src/samizdat/repl.clj"
   #{
    "(an infinite loop or a heavy computation?). If it genuinely "
    "(no docstring — jolt strips core-var metadata)"
    "ms — the code ran too long "
    }
   "src/samizdat/smoke.clj"
   #{
    "Count from 1 to 300, one number per line, nothing else."
    "TLS is broken once jolt.nrepl loads. The warm-up in"
    "https survives the nREPL load in the real startup order"
    "libsqlite3 loaded by the FFI binding has no FTS5"
    "ms behind /slow — server is serialized"
    "ms while /slow was running"
    "no API key in the environment"
    "the subprocess did not finish"
    }
   "src/samizdat/store/artifacts.clj"
   #{
    " branches have this; it does not need another)"
    }
   "src/samizdat/store/interventions.clj"
   #{
    "Open a sibling branch on a stated thesis."
    "Raise the run's turn cap."
    "Stop a branch. Refused if it is the last one running."
    "Tell a branch to cross-check and ship what it has."
    "Un-confirm an artifact that was not what it claimed. Payload: {\"artifact_id\": N, \"reason\": \"...\"}."
    }
   "src/samizdat/store/runs.clj"
   #{
    "no process was running it when the server started"
    }
   "src/samizdat/store/tasks.clj"
   #{
    " ORDER BY CASE status WHEN 'in_progress' THEN 0 WHEN 'blocked' THEN 1 ELSE 2 END,\n             CASE priority WHEN 'high' THEN 0 WHEN 'normal' THEN 1 ELSE 2 END,\n             updated_at DESC, id DESC"
    }
   })

(deftest no-new-model-facing-prose-in-src
  ;; The ratchet. A sentence the model reads belongs in resources/prompts,
  ;; where the supervisor can change it without a rebuild.
  (let [new-hits (for [file (src-files)
                       :let [known (get prose-backlog file #{})]
                       hit (sentence-hits (forms-of file))
                       :when (not (contains? known hit))]
                   (str "  " file " " (pr-str hit)))]
    (is (empty? new-hits)
        (str "new model-facing prose in src/. Put the sentence in a template "
             "under resources/prompts/ and render it — every word the model "
             "reads has to be editable without a rebuild:\n"
             (str/join "\n" new-hits)))))

(deftest the-prose-backlog-only-shrinks
  ;; A backlog entry that no longer matches is a violation somebody fixed, and
  ;; leaving it listed would quietly re-permit that exact sentence.
  (let [stale (for [[file known] prose-backlog
                    :let [hits (set (sentence-hits (forms-of file)))]
                    entry known
                    :when (not (contains? hits entry))]
                (str "  " file " " (pr-str entry)))]
    (is (empty? stale)
        (str "these backlog entries are no longer in src/ — delete them, the "
             "ratchet only turns one way:\n" (str/join "\n" stale)))))

(deftest the-backlog-is-counted-so-it-cannot-be-ignored
  ;; Not an assertion about the number, which would fail on every fix. A
  ;; printed count, so the size of the debt is visible in the run rather than
  ;; only in the file.
  (let [total (reduce + 0 (map (comp count val) prose-backlog))]
    (println (str "  [base-test] model-facing sentences still in src/: " total
                  " across " (count prose-backlog) " files"))
    (is (pos? total) "if this is zero, delete prose-backlog and this test")))

(deftest nothing-in-src-decides-what-the-harness-does
  ;; The RFC-001 invariant table used to read "Nothing mechanical. Reviewed by
  ;; hand." — this is what replaced it.
  ;;
  ;; A failure here is not automatically a bug. It is a question: could a
  ;; project want this different, without a rebuild? If yes the literal moves
  ;; to resources/. If no it goes in `allowed` with the reason, which is the
  ;; part that makes the next reader's job possible.
  (let [found (violations)]
    (is (empty? found)
        (str "these literals in src/ decide something a project might want to "
             "change at runtime. Move each to resources/ (gates.edn for a "
             "number, wordlists.edn for a vocabulary, prompts/ for prose) or "
             "add it to base-test/allowed with the reason it is mechanism:\n"
             (str/join "\n" (map render found))))))

(deftest the-check-can-actually-see-a-violation
  ;; A check whose scanner silently matched nothing would pass forever and be
  ;; indistinguishable from a clean tree — the exact failure mode RFC-003
  ;; describes, where a property cannot fail against a graph that omits its
  ;; subject. Drive each rule with a literal that must trip it.
  (let [forms (read-string {:read-cond :allow}
                           (str "[(defn f \"a docstring, which is not prose\" [x]"
                                "   (when (> x 7) \"You are a helpful model\")"
                                "   (re-find #\"(?i)\\bVERDICT\\b\" x))]"))
        forms (map strip-docstrings forms)]
    (testing "prose in an expression position is seen"
      (is (= ["You are a helpful model"] (prose-hits forms))))
    (testing "a docstring is not"
      (is (not-any? #(str/includes? % "docstring") (prose-hits forms))))
    (testing "a word-bearing regex is seen"
      (is (= ["(?i)\\bVERDICT\\b"] (vocabulary-hits forms))))
    (testing "a compared-against number is seen"
      (is (= [7] (threshold-hits forms))))))

(deftest the-sentence-rule-can-actually-see-a-violation
  ;; Same discipline as above: drive it with what must trip it and what must
  ;; not, or a scanner that quietly matched nothing would read as a clean
  ;; tree forever.
  (let [read* (fn [src] (map strip-docstrings
                             (read-string {:read-cond :allow} (str "[" src "]"))))]
    (testing "a short branch-facing sentence is seen — the case the marker rule misses"
      (is (= ["Change the call, or change technique."]
             (sentence-hits (read* "(defn f [] (base/fail b \"Change the call, or change technique.\"))")))))

    (testing "a log line is not: it is for a person reading a stack trace"
      (is (empty? (sentence-hits
                   (read* "(defn f [] (log/warn \"the branch exceeded its deadline on turn\" t))")))))

    (testing "and neither is an exception message"
      (is (empty? (sentence-hits
                   (read* "(defn f [] (throw (ex-info \"the loop no longer compiles for this run\" {})))")))))

    (testing "nor a docstring, a SQL statement, or a list of names"
      (is (empty? (sentence-hits (read* "(defn f \"a docstring that is a whole sentence about it\" [] 1)"))))
      (is (empty? (sentence-hits (read* "(def q \"SELECT id FROM runs WHERE status = ? AND id IS NOT NULL\")"))))
      (is (empty? (sentence-hits (read* "(def ks \"branch turn tool category artifact failure\")")))))))

(deftest every-allowance-still-applies
  ;; An allow-list outlives what it allowed. An entry naming a literal that no
  ;; longer appears is a reason nobody can check, and it quietly widens the
  ;; hole for whatever lands in that file next.
  (let [stale (for [[file rules] allowed
                    :when (some #(= file %) (src-files))
                    [rule entries] rules
                    [literal _] entries
                    :when (not= :all literal)
                    :let [hits (set (case rule
                                      :prose (prose-hits (forms-of file))
                                      :vocabulary (vocabulary-hits (forms-of file))
                                      :threshold (threshold-hits (forms-of file))))]
                    :when (not (contains? hits literal))]
                [file rule literal])
        missing (remove (set (src-files)) (keys allowed))]
    (is (empty? stale)
        (str "base-test/allowed excuses literals that are no longer in src/; "
             "delete these entries: " (pr-str (vec stale))))
    (is (empty? missing)
        (str "base-test/allowed names files that do not exist: "
             (pr-str (vec missing))))))
