;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.verify-test
  "The ship gate's test rung: the pure decision that makes `done` a hard gate on
  a green test run, and the focused-command derivation that keeps the loop fast
  (karamazov-dvz follow-up: the worker loop must be test-driven, not one-shot)."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [samizdat.agent.gates :as gates]
            [samizdat.agent.verify :as verify]
            [samizdat.engine.proc :as proc]
            [samizdat.security.secrets :as secrets]))

(deftest test-file?-recognises-test-and-spec-paths
  (is (verify/test-file? "test/samizdat/foo_test.clj"))
  (is (verify/test-file? "src/samizdat/foo_spec.clj"))
  (is (verify/test-file? "test/samizdat/agent/decompose_test.clj"))
  (is (not (verify/test-file? "src/samizdat/agent/tools/knowledge.clj")))
  (is (not (verify/test-file? "resources/cells/feature.clj"))))

(deftest ns-from-test-path-maps-a-path-to-its-namespace
  (is (= "samizdat.agent.decompose-test"
         (verify/ns-from-test-path "test/samizdat/agent/decompose_test.clj")))
  (is (= "samizdat.knowledge-test"
         (verify/ns-from-test-path "test/samizdat/knowledge_test.clj")))
  (is (nil? (verify/ns-from-test-path "resources/skills/repl-workflow.md"))
      "a non-clj path has no namespace"))

(deftest ns-from-test-path-rejects-anything-not-a-plain-namespace
  (testing "a file name crafted to break out of the sh -c single-quoting yields no ns"
    (is (nil? (verify/ns-from-test-path "test/foo'; echo pwned; echo '.clj")))
    (is (nil? (verify/ns-from-test-path "test/foo$(touch /tmp/pwned)_test.clj")))
    (is (nil? (verify/ns-from-test-path "test/foo`x`_test.clj")))
    (is (nil? (verify/ns-from-test-path "test/foo bar_test.clj"))))
  (testing "plain namespaces still map"
    (is (= "samizdat.knowledge-test"
           (verify/ns-from-test-path "test/samizdat/knowledge_test.clj")))))

(deftest focused-cmd-embeds-no-shell-breakout
  (let [cmd (verify/focused-cmd ["test/foo'; echo MARKER-INJECTED; echo '.clj"
                                 "test/samizdat/knowledge_test.clj"])]
    (is (some? cmd))
    (is (str/includes? cmd "samizdat.knowledge-test") "the focusable ns is still targeted")
    (is (not (str/includes? cmd "MARKER-INJECTED")) "the crafted file name contributes nothing")
    (is (not (str/includes? cmd "echo")) "no injected shell segment"))
  (testing "nothing focusable survives the crafted names => nil (verify-cmd fallback)"
    (is (nil? (verify/focused-cmd ["test/foo'; echo pwned; echo '.clj"])))))

(deftest focused-cmd-runs-only-the-changed-test-namespaces
  (let [cmd (verify/focused-cmd ["src/samizdat/agent/tools/knowledge.clj"
                                 "test/samizdat/knowledge_test.clj"])]
    (is (some? cmd))
    (is (str/includes? cmd "samizdat.knowledge-test") "targets the touched test ns")
    (is (not (str/includes? cmd "-M:test")) "does NOT run the whole suite")
    (is (str/includes? cmd "System/exit") "sets an exit code so red is detectable"))
  (testing "nothing focusable => nil (caller falls back to :verify-cmd)"
    (is (nil? (verify/focused-cmd ["src/samizdat/only_src.clj"])))
    (is (nil? (verify/focused-cmd [])))))

(deftest verify-block-is-inert-when-verify-is-off
  (is (nil? (verify/verify-block {:verify-on? false :result nil :changed nil :require-test? true}))))

(deftest verify-block-blocks-a-red-run-with-the-output
  (let [b (verify/verify-block {:verify-on? true
                                :result {:green? false :output "FAIL: 2 assertions failed\nat foo_test"}
                                :changed ["src/x.clj" "test/x_test.clj"]
                                :require-test? true})]
    (is (some? b))
    (is (str/includes? b "not green"))
    (is (str/includes? b "assertions failed") "feeds the failure output back")))

(deftest verify-block-blocks-a-timeout
  (let [b (verify/verify-block {:verify-on? true
                                :result {:green? false :timeout? true :output ""}
                                :changed ["test/x_test.clj"] :require-test? true})]
    (is (some? b))
    (is (str/includes? (str/lower-case b) "timed out"))))

(deftest verify-block-blocks-a-hollow-done-when-nothing-changed
  (let [b (verify/verify-block {:verify-on? true :result {:green? true :output ""}
                                :changed [] :require-test? false})]
    (is (some? b))
    (is (str/includes? (str/lower-case b) "changed no files"))))

(deftest verify-block-requires-a-test-when-tdd-strict
  (let [b (verify/verify-block {:verify-on? true :result nil ; not even run — the pre-check fires first
                                :changed ["src/samizdat/agent/tools/knowledge.clj"]
                                :require-test? true})]
    (is (some? b))
    (is (str/includes? (str/lower-case b) "test")))
  (testing "with require-test? off, a green src-only change ships"
    (is (nil? (verify/verify-block {:verify-on? true :result {:green? true :output ""}
                                    :changed ["src/x.clj"] :require-test? false})))))

(deftest a-delegated-piece-is-pinned-by-the-tests-its-contract-names
  ;; karamazov-ioo.15. A child gets its tests from the parent that split the
  ;; work — they are already in the tree, and the child implements against
  ;; them. So the TDD rung, which asks whether the branch CHANGED a test file,
  ;; would refuse every delegated piece for not writing a test that was
  ;; written for it. The contract's tests are the pinning test.
  (is (nil? (verify/verify-block {:verify-on? true :result {:green? true :output ""}
                                  :changed ["src/example/core.clj"]
                                  :require-test? true
                                  :contracted-tests "test/example/core_test.clj"}))
      "green against the contract's tests, having changed only source: ships")
  (testing "and it does not excuse changing nothing at all"
    (is (some? (verify/verify-block {:verify-on? true :result {:green? true :output ""}
                                     :changed []
                                     :require-test? true
                                     :contracted-tests "test/example/core_test.clj"}))))
  (testing "nor does it excuse a red run"
    (is (some? (verify/verify-block {:verify-on? true
                                     :result {:green? false :output "FAIL"}
                                     :changed ["src/example/core.clj"]
                                     :require-test? true
                                     :contracted-tests "test/example/core_test.clj"}))))
  (testing "an undelegated branch is unaffected"
    (is (some? (verify/verify-block {:verify-on? true :result nil
                                     :changed ["src/example/core.clj"]
                                     :require-test? true
                                     :contracted-tests nil}))))
  (testing "an ASSEMBLY carries its own tests and every piece's"
    (is (nil? (verify/verify-block
               {:verify-on? true :result {:green? true :output ""}
                :changed ["src/example/core.clj"]
                :require-test? true
                :contracted-tests ["test/example/core_test.clj"
                                   "test/example/parse_test.clj"]})))))

(deftest a-piece-that-left-its-stubs-hollow-has-not-delivered
  ;; Green tests are not enough on their own. The parent's composition CALLS
  ;; these names, so a child whose tests happen to pass around an unimplemented
  ;; stub — or that deleted the stub instead of filling it — has not delivered
  ;; the piece it was given. karamazov-ioo.15.
  (let [b (verify/verify-block {:verify-on? true :result {:green? true :output ""}
                                :changed ["src/example/core.clj"]
                                :require-test? true
                                :contracted-tests "test/example/core_test.clj"
                                :unfilled ["parse-line"]})]
    (is (some? b) "a green run does not ship a hollow stub")
    (is (str/includes? b "parse-line") "and it names the one still owed"))
  (testing "with every stub filled, the same green run ships"
    (is (nil? (verify/verify-block {:verify-on? true :result {:green? true :output ""}
                                    :changed ["src/example/core.clj"]
                                    :require-test? true
                                    :contracted-tests "test/example/core_test.clj"
                                    :unfilled []})))))

(deftest verify-block-passes-a-green-tdd-change
  (is (nil? (verify/verify-block {:verify-on? true :result {:green? true :output ""}
                                  :changed ["src/samizdat/agent/tools/knowledge.clj"
                                            "test/samizdat/knowledge_test.clj"]
                                  :require-test? true}))))

(deftest verify-block-trusts-a-green-run-when-git-cannot-tell
  (is (nil? (verify/verify-block {:verify-on? true :result {:green? true :output ""}
                                  :changed nil :require-test? true}))))

;; --- run-verify: the trust boundary (provenance R3-1) ------------------------------
;;
;; The verify child must not inherit the parent's secrets, and its output is
;; model-bound: what run-verify returns has to pass the same redaction boundary
;; the shell tool enforces (docs/RFCS/RFC-003-security-model.md properties 1 and 2).

(deftest run-verify-spawns-with-a-scrubbed-environment
  (let [captured (atom nil)]
    (with-redefs [proc/run (fn [opts & args]
                             (reset! captured (assoc opts :args args))
                             {:exit 0 :out "" :err ""})]
      (verify/run-verify "/tmp/some-root" "jolt test" 1000))
    (let [env (:env @captured)]
      (is (map? env) "proc/run receives an explicit :env")
      (is (= (secrets/scrubbed-process-env) env)
          "the child environment is the scrubbed process environment")
      (is (str/starts-with? (str (last (:args @captured))) "cd '/tmp/some-root'")
          "the root is single-quoted, not interpolated bare"))))

(deftest run-verify-redacts-known-secrets-in-the-output
  (let [token "sk-Abcdefghijklmnopqrstuvwxyz123456"]
    (with-redefs [proc/run (fn [_ & _] {:exit 0
                                         :out (str "dump: " token "\n")
                                         :err ""})]
      (let [r (verify/run-verify "/tmp/r" "printenv" 1000)]
        (is (:green? r))
        (is (str/includes? (:output r) "[REDACTED]") "the token is redacted")
        (is (not (str/includes? (:output r) token)) "the token itself is gone")))))

(deftest focused-verify-conventions-are-gates-edn-data
  ;; drg-4026 #47/48: what counts as a test file, how a test path becomes a
  ;; namespace, and the command shape that runs a focused suite are PROJECT
  ;; conventions, not kernel code. They live in gates.edn :focused-verify;
  ;; the derivations stay in verify.clj and read the data at fire time, so
  ;; a pytest/maven project retunes without a rebuild.
  (let [cfg (gates/threshold :focused-verify)]
    (is (re-find (re-pattern (:test-file-regex cfg)) "test/samizdat/foo_test.clj"))
    (is (not (re-find (re-pattern (:test-file-regex cfg)) "src/samizdat/tools/knowledge.clj")))
    (is (re-find (re-pattern (:ns-whitelist-regex cfg)) "samizdat.agent.decompose-test"))
    (is (str/starts-with? (verify/focused-cmd ["test/samizdat/agent_test.clj"])
                          (:cmd-prefix cfg)))
    ;; fire-time read: the command prefix is data, swapped without touching src
    (with-redefs [gates/threshold (fn [_] (assoc cfg :cmd-prefix "pytest {{expr}}"))]
      (is (str/starts-with? (verify/focused-cmd ["test/x_test.clj"]) "pytest ")))))
