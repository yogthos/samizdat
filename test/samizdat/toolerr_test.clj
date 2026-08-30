;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.toolerr-test
  "Why a tool call failed, whether running it again is allowed, and what a
  cut-short call left behind. Ported from dirge's tool_error_class.rs,
  tool_retry.rs and side_effect.rs."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [samizdat.agent.gates :as gates]
            [samizdat.agent.thinking :as thinking]
            [samizdat.agent.toolerr :as te]
            [samizdat.agent.tools :as tools]
            [samizdat.agent.tools.base :as base]
            [samizdat.lexicon :as lexicon]))

(defn- classes [] (lexicon/wordlist :tool-error-classes))
(defn- read-only [] (lexicon/wordlist :retry-safe-tools))

;; --- the taxonomy ------------------------------------------------------------

(deftest failures-are-classified-by-what-the-branch-should-do-about-them
  (testing "a timeout is transient BY FLAG, before any text matching — it is
            the one failure the tool layer records structurally"
    (is (= :transient (te/classify {:result "anything at all" :timeout? true} (classes)))))
  (testing "the world not being as the branch thinks: LOOK, do not retry"
    (is (= :missing-info (te/classify {:result "No file src/x.clj under the project root"} (classes))))
    (is (= :missing-info (te/classify {:result "old_text not found in a.clj"} (classes)))))
  (testing "a wall"
    (is (= :fatal (te/classify {:result "Permission denied"} (classes)))))
  (testing "and an unmatched failure is :misuse, whose advice — re-read the
            contract — is the harmless one to give wrongly"
    (is (= :misuse (te/classify {:result "something nobody has seen before"} (classes))))))

;; --- the retry gate ----------------------------------------------------------

(deftest retrying-needs-BOTH-a-transient-error-and-a-safe-tool
  (let [policy {:max-attempts 3 :read-only (read-only)}]
    (testing "a pure read that timed out — the case this exists for, a language
              server still indexing, which the branch can do nothing about"
      (is (te/should-retry? {:tool "lsp" :class :transient :attempt 1} policy))
      (is (te/should-retry? {:tool "read_file" :class :transient :attempt 1} policy)))
    (testing "A TIMEOUT DOES NOT MEAN THE WORK DID NOT HAPPEN. A shell command
              killed at its budget may have run to completion, and re-issuing
              it is how one push becomes two"
      (is (not (te/should-retry? {:tool "shell" :class :transient :attempt 1} policy)))
      (is (not (te/should-retry? {:tool "eval" :class :transient :attempt 1} policy)))
      (is (not (te/should-retry? {:tool "write_file" :class :transient :attempt 1} policy))))
    (testing "a safe tool failing for a reason retrying cannot fix"
      (is (not (te/should-retry? {:tool "read_file" :class :missing-info :attempt 1} policy)))
      (is (not (te/should-retry? {:tool "read_file" :class :fatal :attempt 1} policy))))
    (testing "and the attempts are bounded — a read still failing on its third
              try is reporting a real condition, not a blip"
      (is (not (te/should-retry? {:tool "lsp" :class :transient :attempt 3} policy))))))

(deftest the-allowlist-is-positive-so-a-new-tool-is-unsafe-until-argued-for
  (is (not (te/retry-safe? "some_new_tool" (read-only))))
  (is (te/retry-safe? "grep" (read-only))))

(deftest the-backoff-is-a-beat-not-a-rate-limit-wait
  ;; It sits INSIDE the branch's turn; a minute-long pause is indistinguishable
  ;; from a hang.
  (is (= 250 (te/backoff-ms 1 250)))
  (is (= 500 (te/backoff-ms 2 250)))
  (is (< (te/backoff-ms 3 250) 2000)))

(deftest a-retry-is-invisible-when-it-works-and-counted-when-it-does-not
  (let [n (atom 0)]
    (defmethod base/run-tool "lsp" [{:keys [branch]}]
      (swap! n inc)
      (if (< @n 3) (base/fail branch "lsp request timed out")
          (base/ok branch "definition at src/a.clj:12")))
    (let [r (tools/run-tool {:branch {:id "B1"} :tool-name "lsp" :args {}})]
      (is (= 3 @n) "it tried again rather than spending the branch's turn on a blip")
      (is (= :neutral (:category r)))
      (is (str/includes? (:result r) "src/a.clj:12")))))

(deftest a-mutating-tool-that-timed-out-is-never-run-again
  (let [n (atom 0)]
    (defmethod base/run-tool "shell" [{:keys [branch]}]
      (swap! n inc)
      (base/fail branch "command timed out after 120000ms" :timeout? true))
    (tools/run-tool {:branch {:id "B1"} :tool-name "shell" :args {}})
    (is (= 1 @n))))

;; --- what it landed ----------------------------------------------------------

(deftest a-cut-short-mutating-call-says-that-it-does-not-know-what-landed
  ;; A result is success-or-error text, which answers whether the tool
  ;; reported a problem — not the question the next turn needs answered. Left
  ;; unsaid, the branch reads a timeout as a failure and does the reasonable
  ;; thing, which here is the unsafe one.
  (is (te/uncertain-effect? {:tool "shell" :timeout? true} (read-only)))
  (is (te/uncertain-effect? {:tool "write_file" :class :transient} (read-only)))
  (testing "a read is not uncertain — it was retried, and reading twice lands
            nothing either time"
    (is (not (te/uncertain-effect? {:tool "read_file" :timeout? true} (read-only)))))
  (testing "and the branch is told, in the result it actually reads"
    (defmethod base/run-tool "shell" [{:keys [branch]}]
      (base/fail branch "command timed out" :timeout? true))
    (let [r (tools/run-tool {:branch {:id "B1"} :tool-name "shell" :args {}})]
      (is (str/includes? (:result r) "not known whether this call landed"))
      (is (str/includes? (:result r) "issue it again")
          "and names the unsafe move by name, rather than hinting at it"))))

;; --- the runaway-reasoning breaker -------------------------------------------

(deftest the-thinking-cap-is-derived-from-what-the-turn-was-granted
  ;; A flat constant is what dirge shipped first and it was wrong by
  ;; inspection: a high-effort turn was granted a large budget by the request
  ;; and cut off below it by the breaker — the harness truncating reasoning it
  ;; had just asked for.
  (let [p (gates/threshold :thinking-budget)]
    (is (= (* (:factor p) 16384) (thinking/derived-cap 16384 p)))
    (is (= (:fallback p) (thinking/derived-cap nil p))
        "an unknown grant gets the permissive value: a missed runaway costs a
         turn, a truncated good turn costs the work in it")
    (is (>= (:fallback p) (thinking/derived-cap 16384 p))
        "the fallback must stay at or above every derived cap")))

(deftest the-breaker-needs-both-signals
  (let [cap 1000
        long-trace (apply str (repeat 40000 "x"))]
    (is (thinking/runaway? {:truncated? true :reasoning long-trace} cap 4))
    (testing "a truncation on a SHORT trace is a budget that was too small,
              and the fix there is more tokens — the opposite advice"
      (is (not (thinking/runaway? {:truncated? true :reasoning "brief"} cap 4))))
    (testing "and a long think that ended in a tool call is the feature working"
      (is (not (thinking/runaway? {:truncated? true :reasoning long-trace
                                   :parsed {:name "eval"}} cap 4))))))

(deftest the-recovery-holds-for-the-task-and-reaches-the-request
  (let [b (thinking/recovery {})
        off (:off-value (gates/threshold :thinking-budget))]
    (is (:thinking-off? b))
    (is (= 1 (:thinking-breaks b)) "a second firing is visible")
    (is (= "high" (thinking/effort-for {} "high" off)))
    (is (= off (thinking/effort-for b "high" off))
        "read at request time, because the config is the run's and this
         decision is one branch's")))
