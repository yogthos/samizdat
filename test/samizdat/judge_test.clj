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

(ns samizdat.judge-test
  "The finalization critic: the pure judge core, and the block-then-ship loop
  behavior on the `critic` manifest."
  (:require ;; the java.time.* host shim, before data.json — see samizdat.store.journal
            [jolt.time]
            [clojure.data.json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [samizdat.agent.gates :as gates]
            [samizdat.agent.judge :as judge]
            [samizdat.agent.tools :as tools]
            [samizdat.llm.client :as llm]
            [samizdat.store.db :as db]
            [samizdat.workflow :as workflow]))

(deftest verdict-parsing-is-negation-aware-and-fails-open
  (is (= :complete   (judge/parse-verdict "VERDICT: COMPLETE\nlooks right")))
  (is (= :incomplete (judge/parse-verdict "VERDICT: INCOMPLETE\nmissing a test")))
  (is (= :incomplete (judge/parse-verdict "VERDICT: NOT COMPLETE"))
      "negation flips a bare COMPLETE")
  (is (= :abstain    (judge/parse-verdict "VERDICT: ABSTAIN")))
  (testing "a judge that cannot answer must never be able to block"
    (is (= :complete (judge/parse-verdict "")))
    (is (= :complete (judge/parse-verdict "the model rambled with no verdict line")))
    (is (= :complete (judge/parse-verdict nil))))
  (testing "the VERDICT line is found even when it is not first"
    (is (= :incomplete (judge/parse-verdict "Some preamble.\nVERDICT: INCOMPLETE")))))

(deftest findings-and-critique
  (is (= "- no test for X\n- Y unhandled"
         (judge/findings "VERDICT: INCOMPLETE\nFINDINGS:\n- no test for X\n- Y unhandled")))
  (is (nil? (judge/findings "VERDICT: COMPLETE")))
  (is (str/includes? (judge/critique-message :incomplete "- add a test") "add a test"))
  (is (str/includes? (judge/critique-message :abstain nil) "could not be confirmed")))

(deftest a-judge-reply-is-read-past-its-reasoning
  ;; MEASURED on run dbe64eea-successor of the arena sweep. GLM-5.3 cannot be
  ;; told not to think, so the critic's reply arrived with its scratchpad
  ;; inline. :findings-regex is "(?is)FINDINGS:\\s*(.+)$" — greedy from the
  ;; FIRST `FINDINGS:`, which was inside the <think> block. So the stored
  ;; findings began mid-thought, carried `</think>`, and were appended to the
  ;; retry's problem text: the next owner was handed the critic's musings as
  ;; part of its task, including a bullet reasoning its way to "Not a finding."
  ;;
  ;; message/strip-think-blocks already exists and select.clj already uses it
  ;; for exactly this ("<think>…</think>\ncritic" on the first live selection
  ;; it ever made). The judge path simply never called it. Stripping happens
  ;; HERE rather than at the call sites because there are three critics —
  ;; :board/review, :feature/critique and cells/critic.clj — and a fourth
  ;; would forget.
  (let [reply (str "<think>\n"
                   "Let me list what I might flag.\n"
                   "FINDINGS:\n"
                   "- Maybe note: can't verify the counts — not a finding, or "
                   "[low]? I'll skip it. Actually, plausible. Not a finding.\n"
                   "First line: `VERDICT: INCOMPLETE`.</think>\n"
                   "VERDICT: INCOMPLETE\n\n"
                   "FINDINGS:\n\n"
                   "- [high] The required visual exercise was never done.")]
    (testing "the findings are the judge's, not its scratchpad"
      (let [f (judge/findings reply)]
        (is (some? f))
        (is (not (str/includes? f "</think>"))
            "the reasoning terminator leaked into the retry's task text")
        (is (not (str/includes? f "Not a finding"))
            "a bullet the judge talked itself OUT of is not a finding")
        (is (str/includes? f "[high] The required visual exercise"))))
    (testing "and the verdict comes from the verdict line, not the scratchpad"
      (is (= :incomplete (judge/parse-verdict reply)))))
  (testing "a scratchpad that contradicts the verdict cannot decide it"
    ;; parse-verdict scans for the first line matching the verdict regex, so
    ;; before stripping, a rehearsal line inside <think> could win.
    (let [reply (str "<think>I'll say VERDICT: COMPLETE... no, on reflection "
                     "the tests never ran.</think>\nVERDICT: INCOMPLETE\n\n"
                     "FINDINGS:\n- [high] no test was run")]
      (is (= :incomplete (judge/parse-verdict reply)))))
  (testing "a reply that is ALL reasoning fails open rather than being parsed"
    ;; An unterminated block is a truncated judge call: everything is
    ;; rehearsal and none of it is a verdict. :complete is the fail-open
    ;; default a judge that cannot answer must land on — it must never be
    ;; able to wedge the loop.
    (let [reply "<think>VERDICT: INCOMPLETE and here is why the tests are bad"]
      (is (= :complete (judge/parse-verdict reply)))
      (is (nil? (judge/findings reply)))))
  (testing "an ordinary reply with no reasoning is untouched"
    (let [reply "VERDICT: COMPLETE\n\nFINDINGS:\n- [low] a nit"]
      (is (= :complete (judge/parse-verdict reply)))
      (is (str/includes? (judge/findings reply) "[low] a nit")))))

(deftest findings-split-into-one-segment-per-finding
  ;; Ported from dirge's split_on_severity_lines. A new segment starts at each
  ;; severity-labelled line, which samizdat's format already gives us — one
  ;; finding per line, each prefixed with its severity tag — so the verify
  ;; pass gets per-finding granularity without a format migration.
  (let [text (str "- [high] The visual exercise was never done.\n"
                  "  It said so in the task and the answer omits it.\n"
                  "- [low] Colour duplication between palette and draw.\n"
                  "- [medium] The answer's fade band contradicts the diff.")]
    (is (= 3 (count (judge/finding-segments text))))
    (is (str/includes? (first (judge/finding-segments text))
                       "It said so in the task")
        "a finding's continuation lines stay with it"))
  (testing "preamble before the first severity line is its own segment, so a
            summary line is never glued onto a finding"
    (let [segs (judge/finding-segments "A one-line summary.\n- [high] a real one")]
      (is (= 2 (count segs)))
      (is (str/includes? (second segs) "[high]"))))
  (is (empty? (judge/finding-segments "")))
  (is (empty? (judge/finding-segments nil))))

(deftest the-verify-pass-reads-what-the-judge-said-not-what-it-thought
  ;; karamazov-na2k.21, run 390dcd17: GLM-5.3's verify reply carried its
  ;; reasoning inline, the reasoning mentioned "[low]" while weighing a
  ;; candidate, and that paragraph survived as the findings the next round
  ;; was handed. Pass 1 already reads through `usable`; pass 2 did not.
  (let [candidates "- [low] The fade never quite completes at the cull radius.\n"
        reply (str "<think>The candidates are mostly positive observations plus one [low]\n"
                   "finding. Let me verify each against the diff.</think>\n"
                   "- [low] The fade never quite completes at the cull radius. VERIFIED.\n")
        out (judge/verified-findings {:reply reply :candidates candidates})]
    (is (str/includes? (str out) "cull radius"))
    (is (not (str/includes? (str out) "think")) out)
    (is (not (str/includes? (str out) "Let me verify")) out)))

(deftest the-verify-pass-drops-only-what-it-explicitly-calls-a-false-positive
  ;; Pass 2, ported from dirge (roborev's VerifyDedupePreamble). The judge
  ;; re-reads each candidate against the diff and marks it VERIFIED or
  ;; FALSE_POSITIVE. Every rule below is one dirge learned from a live
  ;; misfire, so they are pinned rather than assumed.
  (let [candidates (str "- [high] A real defect.\n"
                        "- [low] A speculative one.\n")]
    (testing "a FALSE_POSITIVE segment is dropped and the rest survive"
      (let [reply (str "- [high] A real defect. VERIFIED.\n"
                       "- [low] A speculative one. FALSE_POSITIVE — not in the diff.\n")
            out (judge/verified-findings {:reply reply :candidates candidates})]
        (is (str/includes? out "A real defect"))
        (is (not (str/includes? out "A speculative one")))))
    (testing "a segment carrying BOTH markers is kept — fail closed, because
              clearing a possibly-real blocking finding is the worse error"
      (let [reply "- [high] A real defect. VERIFIED, though arguably FALSE_POSITIVE.\n"
            out (judge/verified-findings {:reply reply :candidates candidates})]
        (is (str/includes? out "A real defect"))))
    (testing "UNVERIFIED must not read as a VERIFIED marker (dirge's own note).
              ONE candidate here on purpose: with two, the ambiguity rule below
              would keep them both and the assertion would pass without the
              UNVERIFIED guard doing anything — which is how it read on the
              first run of this test."
      (let [one "- [high] A real defect.\n"
            reply "- [high] A real defect. FALSE_POSITIVE — UNVERIFIED speculation.\n"
            out (judge/verified-findings {:reply reply :candidates one})]
        (is (nil? out) "the UNVERIFIED substring was hiding the drop")))
    (testing "prose about false positives is not the structured token"
      (let [reply (str "- [high] A real defect. VERIFIED: this guards against "
                       "false positives downstream.\n")
            out (judge/verified-findings {:reply reply :candidates candidates})]
        (is (str/includes? out "A real defect"))))
    (testing "a clean pass clears everything"
      (is (nil? (judge/verified-findings
                 {:reply "No issues found." :candidates candidates}))))
    (testing "FAIL-SAFE: an unparseable verify keeps the pass-1 candidates
              rather than silently dropping real work"
      (doseq [reply [nil "" "Hmm, hard to say."]]
        (is (str/includes? (str (judge/verified-findings
                                 {:reply reply :candidates candidates}))
                           "A real defect")
            (str "kept on: " (pr-str reply)))))
    (testing "and a verify that drops SOME but re-emits nothing parseable is
              ambiguous, so the candidates stand"
      (let [reply "- [low] A speculative one. FALSE_POSITIVE.\n"
            out (judge/verified-findings {:reply reply :candidates candidates})]
        (is (str/includes? (str out) "A real defect")
            "one explicit drop does not account for two candidates")))))

(deftest duplicate-findings-are-merged-mechanically
  ;; dirge's dedupe_findings: a cheap order-preserving backstop to the LLM's
  ;; consolidation, keyed on severity plus the head of the finding normalised
  ;; to alphanumerics.
  (let [text (str "- [high] The fade band contradicts the diff.\n"
                  "- [high] The fade band contradicts the diff!\n"
                  "- [low] Colour duplication.\n")
        out (judge/finding-segments (judge/dedupe-findings text))]
    (is (= 2 (count out)))
    (is (str/includes? (first out) "fade band"))
    (is (str/includes? (second out) "Colour duplication"))))

(deftest the-verify-prompt-carries-the-candidates-and-the-diff
  (let [p (judge/verify-prompt {:candidates "- [high] A real defect."
                                :diff "diff --git a/src/x.clj"})]
    (is (str/includes? p "A real defect"))
    (is (str/includes? p "diff --git a/src/x.clj"))
    (is (or (str/includes? p "FALSE_POSITIVE") (str/includes? p "VERIFIED"))
        "the verdict contract has to be in the instructions the judge reads")))

(deftest a-review-names-which-pass-each-call-is
  ;; MEASURED as a gap on sweep5 run 2: the critique note carried six real
  ;; findings and side_calls held one row, a reflection. Both judge calls were
  ;; invisible — no count, no tokens — so the record could not say whether the
  ;; verify pass had run at all, nor what the second pass costs. That is the
  ;; hole karamazov-2rqb names: a call the run paid for that nothing sums.
  ;;
  ;; So `chat` takes the pass as its first argument. The cell that owns the
  ;; provider records the side call under that kind, and a reader can then see
  ;; two rows per review that found something and one per clean review.
  (let [seen (atom [])
        chat (fn [pass _content]
               (swap! seen conj pass)
               (if (= :review pass)
                 "VERDICT: INCOMPLETE\n\nFINDINGS:\n- [high] A real defect."
                 "- [high] A real defect. VERIFIED."))
        out (judge/review {:chat chat :requirement "r" :diff "d"
                           :evidence "e" :answer "a"})]
    (is (= [:review :verify] @seen)
        "both passes, each naming itself")
    (is (= :incomplete (:verdict out)))
    (is (str/includes? (:findings out) "A real defect")))
  (testing "a clean review never makes the second call, so it costs one row"
    (let [seen (atom [])
          chat (fn [pass _] (swap! seen conj pass) "VERDICT: COMPLETE")
          out (judge/review {:chat chat :requirement "r" :diff "d"
                             :evidence "e" :answer "a"})]
      (is (= [:review] @seen))
      (is (nil? (:findings out))))))

(deftest evidence-block-is-deterministic-facts
  (let [e (judge/evidence [{:tool_name "edit_file" :args {:path "a.clj"} :category "success"}
                           {:tool_name "shell" :args {:command "jolt -M:test"} :category "failure"}
                           {:tool_name "eval" :args {} :category "neutral"}])]
    (is (str/includes? e "tool calls: 3"))
    (is (str/includes? e "a.clj") "files written are named")
    (is (str/includes? e "FAILED: jolt -M:test") "a failed command is marked failed")))

(defn- scripted-chat
  "A model that always tries to `done`, and answers the critic's judge call
  with the verdicts in `verdicts`, in order (last one repeats)."
  [verdicts]
  (let [calls (atom 0)]
    (fn [_ _ messages & _]
      (let [content (str/join " " (map :content messages))]
        (if (str/includes? content "reviewer deciding")
          (let [n (swap! calls inc)
                v (nth verdicts (min (dec n) (dec (count verdicts))))]
            {:content (str "VERDICT: " v) :finish-reason "stop"})
          {:content "```tool-call\n{\"name\": \"done\", \"args\": {\"answer\": \"x\"}}\n```"
           :finish-reason "stop"})))))

(deftest critic-blocks-a-premature-done-then-ships
  (with-redefs [llm/chat (scripted-chat ["INCOMPLETE" "COMPLETE"])]
    (let [conn (db/open! ":memory:")
          r (workflow/run! {:conn conn :config {:run {:loop "critic"}}
                            :llm-adapter :a :llm-config {:max-tokens 16384}
                            :problem "p" :max-turns 8})]
      (is (= :completed (:status r)))
      (is (= "x" (:answer r)))
      (is (<= 2 (count (db/fetch conn ["SELECT rowid FROM events WHERE kind='critic'"])))
          "the critic ran at least twice — a block then a ship"))))

(deftest an-always-unhappy-critic-still-ships-eventually
  ;; A judge that never passes must not be able to wedge the loop.
  (with-redefs [llm/chat (scripted-chat ["INCOMPLETE"])]
    (let [conn (db/open! ":memory:")
          r (workflow/run! {:conn conn :config {:run {:loop "critic"}}
                            :llm-adapter :a :llm-config {:max-tokens 16384}
                            :problem "p" :max-turns 20})]
      (is (= :completed (:status r)) "bounded: it ships after the attempt cap"))))

(deftest deterministic-finalization-gates
  (testing "verifier: edited code but ran no test blocks; running a test clears it"
    (is (judge/verifier-block [{:tool_name "edit_file" :args {:path "a.clj"}}]))
    (is (nil? (judge/verifier-block [{:tool_name "edit_file" :args {:path "a.clj"}}
                                     {:tool_name "shell" :args {:command "jolt -M:test"}}])))
    (is (nil? (judge/verifier-block [{:tool_name "eval" :args {}}]))
        "no code edit, nothing to verify")
    (is (nil? (judge/verifier-block [{:tool_name "write_file" :args {:path "notes.txt"}}]))
        "editing a non-code file is not a code change"))
  (testing "claim: an answer that says it tested when nothing ran blocks"
    (is (judge/claim-block "I ran the tests and they pass" [{:tool_name "eval" :args {}}]))
    (is (nil? (judge/claim-block "I ran the tests and they pass"
                                 [{:tool_name "shell" :args {:command "jolt -A:test -e ..."}}])))
    (is (nil? (judge/claim-block "here is the plan" [{:tool_name "eval" :args {}}]))
        "no test claim, nothing to check"))
  (testing "source: an answer citing an external source the run could not check blocks"
    (is (judge/source-block "According to the docs, X is true" [{:tool_name "eval"}] ["eval"]))
    (is (judge/source-block "see https://example.com/spec" [{:tool_name "grep"}] ["eval"]))
    (is (nil? (judge/source-block "I built and tested X" [{:tool_name "eval"}] ["eval"]))
        "no external citation, nothing to check")
    (is (nil? (judge/source-block "the docs say X" [{:tool_name "web_fetch"}] ["eval"]))
        "a web-ish tool was used, so the citation could have been checked"))
  (testing "deterministic-block returns the first gate that fires"
    (is (judge/deterministic-block "done" [{:tool_name "edit_file" :args {:path "a.clj"}}] ["eval"]))
    (is (nil? (judge/deterministic-block "done" [{:tool_name "eval" :args {}}] ["eval"])))))

(deftest judge-preamble-is-a-prompt-file
  ;; Tier 2b: the judge's standing instructions moved from a src def to
  ;; resources/prompts/judge.md — runtime-editable, same seam as every gate
  ;; message. The verdict/findings parsers are coupled to the VERDICT:/
  ;; FINDINGS:/severity-tag formats, so the move pins them: the def IS the
  ;; file's contents.
  (let [file (slurp (io/resource "prompts/judge.md"))]
    (is (= file (judge/preamble)))
    (is (str/includes? file "VERDICT: COMPLETE"))
    (is (str/includes? file "FINDINGS:"))
    (is (str/includes? file "[critical]")))
  ;; THE CRITIC IS SHOWN WHAT WAS ASKED. It used to be handed the implementer's
  ;; system prompt as "rules" and the implementer's transcript, and never the
  ;; requirement — it was asked whether the task was complete with the task
  ;; absent from the message, and inferred it from the implementer's own
  ;; account of it, which is the one source that cannot contradict the work
  ;; under review.
  (let [p (judge/critic-prompt {:requirement "BUILD A WIDGET"
                                :evidence "E" :answer "A" :diff "D"})]
    (is (str/includes? p "## What was asked"))
    (is (str/includes? p "BUILD A WIDGET") "the requirement itself is in the message")
    (is (str/includes? p "Judge the DIFF against WHAT WAS ASKED"))
    (is (not (str/includes? p "## Transcript"))
        "the implementer's transcript is not poured in by default"))
  (testing "a caller with a reason may still show the transcript"
    (let [p (judge/critic-prompt {:requirement "R" :transcript "T"
                                  :evidence "E" :answer "A" :diff nil})]
      (is (str/includes? p "## Transcript")))))

(deftest diff-review-blocks-on-severity
  (testing "a critical or high finding blocks; a low one does not"
    (is (judge/blocking-findings "VERDICT: COMPLETE\nFINDINGS:\n[critical] leaks a key"))
    (is (judge/blocking-findings "VERDICT: COMPLETE\nFINDINGS:\n[high] off-by-one"))
    (is (nil? (judge/blocking-findings "VERDICT: COMPLETE\nFINDINGS:\n[low] rename")))
    (is (nil? (judge/blocking-findings "VERDICT: COMPLETE")))))

(defn- verdict-with-findings-chat
  "A model that dones, and whose judge returns the given raw replies in order."
  [replies]
  (let [calls (atom 0)]
    (fn [_ _ messages & _]
      (if (str/includes? (str/join " " (map :content messages)) "reviewer deciding")
        (let [n (swap! calls inc)]
          {:content (nth replies (min (dec n) (dec (count replies))) "VERDICT: COMPLETE")
           :finish-reason "stop"})
        {:content "```tool-call\n{\"name\": \"done\", \"args\": {\"answer\": \"x\"}}\n```"
         :finish-reason "stop"}))))

(deftest the-orchestrator-runs-worker-and-critic-as-nested-sub-loops
  ;; The hierarchical manifest: the worker loop is a nested workflow-cell, the
  ;; critic gates finalization at the top level. Same behavior as the flat
  ;; critic manifest, composed instead of hand-wired.
  (with-redefs [llm/chat (scripted-chat ["INCOMPLETE" "COMPLETE"])]
    (let [conn (db/open! ":memory:")
          r (workflow/run! {:conn conn :config {:run {:loop "orchestrator"}}
                            :llm-adapter :a :llm-config {:max-tokens 16384}
                            :problem "p" :max-turns 8})]
      (is (= :completed (:status r)))
      (is (= "x" (:answer r)) "worker did the work, critic shipped it"))))

(deftest a-complete-verdict-with-a-critical-finding-still-blocks
  ;; The diff-review half of the unified judge: COMPLETE but the diff has a
  ;; critical defect -> undo the done and send it back; a clean pass ships.
  (with-redefs [llm/chat (verdict-with-findings-chat
                          ["VERDICT: COMPLETE\nFINDINGS:\n[critical] deletes user data"
                           "VERDICT: COMPLETE"])]
    (let [conn (db/open! ":memory:")
          r (workflow/run! {:conn conn :config {:run {:loop "critic"}}
                            :llm-adapter :a :llm-config {:max-tokens 16384}
                            :problem "p" :max-turns 8})]
      (is (= :completed (:status r)))
      (is (some #(= true (get-in % [:data :blocked]))
                (map #(update % :data (fn [d] (clojure.data.json/read-str (str d) :key-fn keyword)))
                     (db/fetch conn ["SELECT data FROM events WHERE kind='critic'"])))
          "at least one critic firing recorded a block on the diff"))))

(deftest verifier-rules-are-gates-edn-data
  ;; drg-4026 #55: what counts as a test run, a code edit, and the suite
  ;; invocation named in the refusal moved from judge.clj regexes to
  ;; gates.edn :judge-rules — a fixed test-runner ecosystem is project data,
  ;; not kernel code.
  (let [rules (gates/threshold :judge-rules)]
    (is (re-find (re-pattern (:test-run-regex rules)) "jolt -M:test"))
    (is (re-find (re-pattern (:test-run-regex rules)) "cargo test"))
    (is (not (re-find (re-pattern (:test-run-regex rules)) "ls -la")))
    (is (re-find (re-pattern (:code-ext-regex rules)) "src/foo.clj"))
    (is (not (re-find (re-pattern (:code-ext-regex rules)) "README.md")))
    (is (str/includes? (:verifier-message rules) "jolt -M:test")))
  ;; the detection engine stays in src; the rules come from data
  (is (some? (judge/verifier-block [{:tool_name "edit_file"
                                      :args {:path "a.clj"}}])))
  (is (nil? (judge/verifier-block [{:tool_name "edit_file" :args {:path "a.clj"}}
                                   {:tool_name "shell"
                                    :args {:command "jolt -M:test"}}]))))

(deftest outside-reach-comes-from-the-registry
  ;; drg-4026 #56: source-block asserted "no web or fetch tool" as a
  ;; hard-coded premise. The premise is now an argument the caller supplies
  ;; from the REGISTERED tool surface (the cells pass tools/tool-names): a
  ;; project that adds a web tool must not inherit a false refusal, because
  ;; its run COULD have checked the claim.
  (let [rows [{:tool_name "read_file" :args {:path "x.clj"}}]
        answer "According to the docs at https://example.com it works."]
    (is (some? (judge/source-block answer rows ["read_file" "grep"]))
        "a surface with no outside reach cannot have checked the claim, so it blocks")
    (is (nil? (judge/source-block answer rows ["read_file" "web_fetch"]))
        "a registered outside-reach tool means the claim could have been checked")
    ;; The premise is an ARGUMENT for exactly this reason: registering
    ;; `websearch` disabled this block by existing, which is the behaviour the
    ;; docstring promises. Asserting against the LIVE surface made the test a
    ;; statement about today's tool list rather than about the logic, so it
    ;; broke the moment the harness gained the capability it was written to
    ;; accommodate.
    (is (nil? (judge/source-block answer rows (tools/tool-names)))
        "and this harness now HAS one, so the block no longer fires")))

(defn- judge-with-findings
  "A model that ships on its own turn and answers the critic's judge call
  first with an INCOMPLETE verdict carrying a FINDINGS section, then COMPLETE."
  []
  (let [calls (atom 0)]
    (fn [_ _ messages & _]
      (let [content (str/join " " (map :content messages))]
        (if (str/includes? content "reviewer deciding")
          (if (= 1 (swap! calls inc))
            {:content "VERDICT: INCOMPLETE\nFINDINGS:\n- [high] the new path has no test"
             :finish-reason "stop"}
            {:content "VERDICT: COMPLETE" :finish-reason "stop"})
          {:content "```tool-call\n{\"name\": \"done\", \"args\": {\"answer\": \"x\"}}\n```"
           :finish-reason "stop"})))))

(deftest the-critic-note-keeps-the-findings-it-blocked-on
  ;; karamazov-3htz: the note said {:verdict :blocked} and nothing of WHY —
  ;; the findings went to the branch and nowhere durable. A verdict corpus
  ;; needs the reasoning beside the verdict, and the branch on the row.
  (with-redefs [llm/chat (judge-with-findings)]
    (let [conn (db/open! ":memory:")
          r (workflow/run! {:conn conn :config {:run {:loop "critic"}}
                            :llm-adapter :a :llm-config {:max-tokens 16384}
                            :problem "p" :max-turns 8})
          rows (db/fetch conn ["SELECT branch_id, data FROM events WHERE kind='critic' ORDER BY id"])
          notes (map #(clojure.data.json/read-str (str (:data %)) :key-fn keyword) rows)
          blocked (first (filter :blocked notes))]
      (is (= :completed (:status r)))
      (is (some? blocked) "the first verdict blocked")
      (is (str/includes? (str (:findings blocked)) "no test")
          "and the note carries the findings it blocked on")
      (is (every? #(seq (str (:branch_id %))) rows)
          "the branch is on the row, where every other per-branch note keeps it"))))

;; --- the rubric judge (karamazov-a6mj.4) ------------------------------------------

(def ^:private rfc-with-criteria
  "# RFC: haze
## Purpose
fade the horizon.
## Work items
- pure flight.haze with tests
## Acceptance criteria
- the far ground blends toward the sky colour
- [3] no tree is drawn past the ground it stands on
- [-2] the near field changed appearance
- [x0.5] a test was deleted or weakened
Some trailing prose that is not a bullet.
## Notes
- not a criterion, this is another section")

(deftest acceptance-criteria-parse-from-the-rfc-with-weights-and-penalties
  (let [c (judge/parse-criteria rfc-with-criteria)]
    (is (= 4 (count c)) "the bullets of that section and no other")
    (is (= {:criterion "the far ground blends toward the sky colour" :weight 1.0 :kind :positive}
           (first c))
        "an unmarked bullet is a positive criterion of weight 1")
    (is (= {:criterion "no tree is drawn past the ground it stands on" :weight 3.0 :kind :positive}
           (second c)))
    (is (= {:criterion "the near field changed appearance" :weight 2.0 :kind :deduction}
           (nth c 2))
        "[-N] is a deduction: YES costs N")
    (is (= {:criterion "a test was deleted or weakened" :weight 0.5 :kind :multiplicative}
           (nth c 3))
        "[xM] scales the reward by (1 - M) on YES"))
  (is (= [] (judge/parse-criteria "# RFC\n## Purpose\nnone"))
      "an RFC with no such section has no criteria — the rubric does not run")
  (is (= [] (judge/parse-criteria nil))))

(deftest the-rubric-arithmetic-is-the-documented-one
  ;; thinkingbox docs/rubrics_judge.md's worked example: 50+30+0 earned of
  ;; 100, a 15-point deduction, a x0.5 multiplicative penalty -> 0.325.
  (let [ratings [{:criterion "a" :weight 50.0 :kind :positive :rating true}
                 {:criterion "b" :weight 30.0 :kind :positive :rating true}
                 {:criterion "c" :weight 20.0 :kind :positive :rating false}
                 {:criterion "d" :weight 15.0 :kind :deduction :rating true}
                 {:criterion "e" :weight 0.5 :kind :multiplicative :rating true}]
        s (judge/rubric-score ratings)]
    (is (= 80.0 (:earned s)))
    (is (= 15.0 (:deductions s)))
    (is (= 100.0 (:positive-weight s)))
    (is (< (Math/abs (- 0.65 (:base s))) 1e-9))
    (is (< (Math/abs (- 0.325 (:reward s))) 1e-9)))
  (testing "clamped to [0, 1]: deductions cannot go negative"
    (is (= 0.0 (:reward (judge/rubric-score [{:weight 1.0 :kind :positive :rating false}
                                              {:weight 5.0 :kind :deduction :rating true}])))))
  (testing "an undecided rating is left out of both sides, not read as NO"
    (let [s (judge/rubric-score [{:weight 1.0 :kind :positive :rating true}
                                 {:weight 1.0 :kind :positive :rating nil}
                                 {:weight 3.0 :kind :deduction :rating nil}])]
      (is (= 1.0 (:reward s)))
      (is (= 2 (:undecided s)) "the undecided positive and the undecided penalty")))
  (testing "nothing decided is no reward, not zero — fail-open for the caller"
    (is (nil? (:reward (judge/rubric-score [{:weight 1.0 :kind :positive :rating nil}]))))
    (is (nil? (:reward (judge/rubric-score []))))))

(deftest a-rubric-review-asks-one-narrow-question-per-criterion
  (let [asked (atom [])
        chat (fn [content]
               (swap! asked conj content)
               (cond (str/includes? content "blends toward the sky") "YES — draw.clj lerps toward sky-color."
                     (str/includes? content "past the ground") "NO. tree-radius is still 190."
                     (str/includes? content "near field") "NO"
                     (str/includes? content "deleted or weakened") "well, hard to say"
                     :else "NO"))
        r (judge/review-rubric {:chat chat
                                :criteria (judge/parse-criteria rfc-with-criteria)
                                :answer "faded it" :diff "+ lerp" :evidence "files written: draw.clj"
                                :threshold 0.7})]
    (is (= 4 (count @asked)) "one call per criterion, none combined")
    (is (every? #(str/includes? % "faded it") @asked) "each sees the answer")
    (is (every? #(re-find #"(?i)YES or NO" %) @asked) "each is the narrow yes/no shape")
    (is (= [true false false nil] (mapv :rating (:ratings r))))
    ;; earned 1 of 4 positive weight, no penalty triggered, one undecided
    (is (< (Math/abs (- 0.25 (:reward r))) 1e-9))
    (is (false? (:pass? r)))
    (testing "the findings name the failed criteria with the judge's own words"
      (is (str/includes? (:findings r) "no tree is drawn past the ground"))
      (is (str/includes? (:findings r) "tree-radius is still 190"))
      (is (not (str/includes? (:findings r) "blends toward the sky")) "a met criterion is not a finding")
      (is (not (str/includes? (:findings r) "deleted or weakened")) "an undecided one is not a finding either"))
    (testing "a penalty that fired is a finding too"
      (let [r (judge/review-rubric {:chat (fn [c] (if (str/includes? c "near field") "YES, the fog covers everything" "YES"))
                                    :criteria (judge/parse-criteria rfc-with-criteria)
                                    :answer "a" :threshold 0.7})]
        (is (str/includes? (:findings r) "near field"))
        (is (< (:reward r) 1.0))))
    (testing "over the threshold passes and has nothing to say"
      (let [r (judge/review-rubric {:chat (fn [c] (if (re-find #"near field|deleted" c) "NO" "YES"))
                                    :criteria (judge/parse-criteria rfc-with-criteria)
                                    :answer "a" :threshold 0.7})]
        (is (= 1.0 (:reward r)))
        (is (true? (:pass? r)))
        (is (nil? (:findings r)))))
    (testing "a judge that decides nothing passes — fail-open, and says so"
      (let [r (judge/review-rubric {:chat (fn [_] "hmm") :criteria (judge/parse-criteria rfc-with-criteria)
                                    :answer "a" :threshold 0.7})]
        (is (nil? (:reward r)))
        (is (true? (:pass? r)))))))

(deftest section-bullets-reads-one-markdown-section
  (is (= ["a" "b"] (judge/section-bullets "# t\n## Work items\n- a\n* b\nprose\n## Next\n- c" #"(?i)^#+\s*work items\b")))
  (is (= [] (judge/section-bullets "# t\n## Other\n- c" #"(?i)^#+\s*work items\b"))))

;; --- karamazov-0way: the rubric judge must be shown the file the question is about

(def ^:private epic-diff
  (str "diff --git a/PLAN.md b/PLAN.md\n--- a/PLAN.md\n+++ b/PLAN.md\n+wind arrow over the bird\n"
       "diff --git a/src/flight/game.clj b/src/flight/game.clj\n--- a/src/flight/game.clj\n+++ b/src/flight/game.clj\n"
       "-(def ring-spacing 70.0)\n+(def ring-spacing 50.0)\n"
       "diff --git a/test/flight/windview_test.clj b/test/flight/windview_test.clj\n--- a/test/flight/windview_test.clj\n+++ b/test/flight/windview_test.clj\n"
       "+(deftest arrow-tail-is-the-float-point\n+  (is (= (indicator-arrow pos wind) ...)))\n"))

(deftest focus-diff-puts-the-criterion-s-files-first
  ;; The first live rubric (run 5f8de58c) scored 0.4: the epic's 20417-char
  ;; diff was cut at 12000, exactly at windview_test.clj's header, and five
  ;; criteria about that file were rated NO for evidence never shown. The
  ;; per-file chunks are ordered by what the question names before any cut.
  (testing "a path named in the criterion goes first"
    (let [d (judge/focus-diff epic-diff "The test in test/flight/windview_test.clj pins the arrow")]
      (is (str/starts-with? d "diff --git a/test/flight/windview_test.clj"))
      (is (= (count epic-diff) (count d)) "reordered, nothing dropped")))
  (testing "a backticked symbol the criterion names ranks the file that mentions it"
    (let [d (judge/focus-diff epic-diff "A test pins that `indicator-arrow`'s tail equals the float point")]
      (is (str/starts-with? d "diff --git a/test/flight/windview_test.clj")))
    (let [d (judge/focus-diff epic-diff "`ring-spacing` is 50 so rings stay reachable")]
      (is (str/starts-with? d "diff --git a/src/flight/game.clj"))))
  (testing "a criterion naming nothing leaves the diff in git's order"
    (is (= epic-diff (judge/focus-diff epic-diff "the code is clean"))))
  (testing "a diff with no file headers is returned as it is"
    (is (= "+ lerp" (judge/focus-diff "+ lerp" "`lerp` is used")))
    (is (= "" (judge/focus-diff "" "anything")))))

(deftest the-rubric-judge-sees-the-relevant-hunks-under-its-own-budget
  (let [seen (atom [])
        chat (fn [content] (swap! seen conj content) "YES")
        crit (judge/parse-criteria (str "## Acceptance criteria\n\n"
                                        "- A test pins `indicator-arrow`'s tail\n"
                                        "- `ring-spacing` is 50\n"))]
    (judge/review-rubric {:chat chat :criteria crit :answer "done" :evidence "e"
                          :diff epic-diff :diff-chars 260 :threshold 0.7})
    (is (= 2 (count @seen)))
    (testing "each question is shown its own file first, and the cut falls elsewhere"
      (is (str/includes? (first @seen) "arrow-tail-is-the-float-point"))
      (is (str/includes? (second @seen) "ring-spacing 50.0"))
      (is (every? #(str/includes? % "diff truncated at 260 chars") @seen)
          "the budget is the rubric's, not the branch's :diff-chars"))
    (testing "without a budget the diff is passed through whole"
      (reset! seen [])
      (judge/review-rubric {:chat chat :criteria crit :answer "done" :diff epic-diff :threshold 0.7})
      (is (every? #(str/includes? % "PLAN.md") @seen)))))

(deftest evidence-carries-the-last-test-summary-it-saw
  ;; 'Total test count is >= 83 and the suite is green' was rated NO because
  ;; the evidence block listed `ok: jolt -M:test` with no output. The last
  ;; clojure.test summary line a shell run printed is a fact the judge can read.
  (let [e (judge/evidence [{:tool_name "shell" :args {:command "jolt -M:test"} :category "success"
                            :result "...\nRan 86 tests, 1436 assertions, 0 failures, 0 errors.\n"}
                           {:tool_name "shell" :args {:command "jolt -M:test"} :category "success"
                            :result "Ran 92 tests, 1493 assertions, 0 failures, 0 errors."}
                           {:tool_name "read_file" :args {:path "x"} :category "neutral"
                            :result "Ran 999 tests, 0 assertions, 0 failures, 0 errors."}])]
    (is (str/includes? e "last test summary: Ran 92 tests, 1493 assertions, 0 failures, 0 errors")
        "the LAST one a shell run printed")
    (is (not (str/includes? e "999")) "a summary read out of a file is not a run"))
  (is (not (str/includes? (judge/evidence [{:tool_name "eval" :args {} :category "neutral"}])
                          "last test summary"))
      "nothing to say when no run printed one"))

(deftest focus-sources-ranks-the-files-a-criterion-is-about-and-cuts-there
  ;; The verify-stage judge answered "the wind is one field felt and shown
  ;; consistently" NO because "the diff contains no change unifying wind
  ;; sampling" — the sampling predates the run, and a question about the
  ;; TREE cannot be answered from a diff. The current sources of the files
  ;; the run changed are the evidence, the one the question names first.
  (let [srcs {"src/flight/draw.clj" "(ns flight.draw) (defn hud [] (wind/wind-at pos))"
              "src/flight/game.clj" "(ns flight.game) (def ring-spacing 50.0)"
              "src/flight/wind.clj" "(ns flight.wind) (defn wind-at [pos] ...)"}
        out (judge/focus-sources srcs "The wind is one field: every reader calls `wind-at`" nil)]
    (testing "files mentioning the named symbol come first, in path order among equals"
      (is (str/starts-with? out "--- src/flight/draw.clj ---"))
      (is (< (str/index-of out "src/flight/wind.clj") (str/index-of out "src/flight/game.clj"))))
    (testing "a named path outranks a symbol mention"
      (is (str/starts-with? (judge/focus-sources srcs "`ring-spacing` in src/flight/game.clj is 50" nil)
                            "--- src/flight/game.clj ---")))
    (testing "every source is present when there is no budget"
      (doseq [p (keys srcs)] (is (str/includes? out p))))
    (testing "a budget cuts the tail and says so"
      (let [cut (judge/focus-sources srcs "`wind-at`" 80)]
        (is (<= (count cut) (+ 80 60)))
        (is (str/includes? cut "sources truncated at 80 chars"))))
    (is (nil? (judge/focus-sources {} "anything" 100)) "no sources, no section")))
