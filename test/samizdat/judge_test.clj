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
  (:require [clojure.data.json]
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
