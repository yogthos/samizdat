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

(ns samizdat.repl-confinement-test
  "karamazov-zrq: what a role's `eval` can and cannot reach.

  THESE TEST THE ROUTER, NOT `repl/eval-code`. Round 0 wrote them against
  `repl/eval-code` and that was the wrong seam: that function IS the
  unconfined in-process image, deliberately, because it is what the supervisor
  and the mutation protocol are built on. Confining it would break the thing
  it exists for. What must be true is narrower and stronger — that an
  ORDINARY ROLE's eval never reaches it. So the assertions moved to
  `route/eval-for`, which is the path a real tool call takes, and
  `the-harness-image-is-the-supervisors-alone` pins the only door to the old
  behaviour.

  Each of these reproduces something dogfood run a3ba69bb's supervisor branch
  S2 actually did at turns 26-38, after `read_file` refused
  `src/samizdat/agent/tools/introspect.clj` and the model routed around the
  refusal with `eval`."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest testing is use-fixtures]]
            [jolt.fs :as fs]
            [samizdat.repl.route :as route]
            [samizdat.security.sandbox :as sandbox]))

(use-fixtures :once (fn [f] (try (f) (finally (route/release-all!)))))

(defn- project!
  "A run root that is NOT the harness checkout, with a marker of its own."
  []
  (let [root (str (fs/create-temp-dir))]
    (spit (str root "/deps.edn") (pr-str {:paths ["src"]}))
    (spit (str root "/README.md") "PROJECT-README-MARKER")
    root))

(defn- as-implementor
  "The ctx an ordinary role's eval runs under."
  [root]
  {:root root :role :implementor})

(defn- ev [root code]
  (route/eval-for (as-implementor root) code nil 15000))

(defn- sandboxed?
  "Whether an OS sandbox is actually in force here.

  THE RESOLVED BACKEND, not the config setting. `:sandbox :auto` is what the
  operator asked for; `backend-for` is what the platform can deliver, and off
  macOS it is `:none` (karamazov-zrq.8). Guarding on the setting was the bug
  that made these pass locally and fail on CI's Linux: :auto is not :none, but
  it resolves to it.

  The filesystem assertions below are TWO-SIDED rather than skipped. A test
  that quietly passes on the platform where the protection does not exist
  would hide exactly the gap zrq.8 is open about."
  []
  (not= :none (sandbox/backend-for :auto (System/getProperty "os.name"))))

;; --- the door to the unconfined image ---------------------------------------

(deftest the-harness-image-is-the-supervisors-alone
  ;; The whole confinement rests on this. Everything below is a consequence.
  (let [root (project!)]
    (is (= :project (route/image-of (as-implementor root))))
    (is (= :project (route/image-of {:root root :role :reviewer})))
    (is (= :project (route/image-of {:root root}))
        "a ctx with no role reached the harness image")
    (is (= :harness (route/image-of {:root root :role :supervisor})))))

;; --- what an ordinary role's eval cannot do ---------------------------------

(deftest eval-resolves-relative-paths-against-the-run-root
  ;; Observed live at turn 4: the agent read samizdat's README believing it was
  ;; the project's, listed samizdat's directory at turn 5, and slurped
  ;; samizdat's deps.edn at turn 6. A wrong answer that looks right is the
  ;; worst failure this seam has.
  (let [root (project!)
        r (ev root "(slurp \"README.md\")")]
    (is (:ok r) (str "eval failed outright: " (:error r)))
    (is (str/includes? (str (:value r)) "PROJECT-README-MARKER")
        "eval read the HARNESS README, not the project's")))

(deftest eval-cannot-read-harness-source-from-a-project-run
  ;; The S2 escape's first move, verbatim. `read_file` refuses this path with
  ;; "No file … under the project root"; eval used to answer it.
  (let [root (project!)
        r (ev root "(slurp \"src/samizdat/agent/tools/introspect.clj\")")]
    (is (not (:ok r))
        "eval slurped harness source, routing around the file-tool confinement")))

(deftest eval-cannot-read-harness-source-by-absolute-path-either
  ;; THE RELATIVE TEST ABOVE PROVES ONLY THAT cwd MOVED. Reads are open by
  ;; default in the profile — a strict read allowlist aborts the runtime — so
  ;; for a while this was readable: the review measured the project image
  ;; slurping route.clj's own source by absolute path. The harness checkout is
  ;; in :deny-read now, and the project root is re-allowed after the denies so
  ;; a self-hosting run can still read itself.
  (let [root (project!)
        here (str (fs/cwd))
        r (ev root (str "(slurp \"" here "/src/samizdat/repl/route.clj\")"))]
    (if (sandboxed?)
      (is (not (:ok r))
          "the project image read the harness's own source by absolute path")
      (is (:ok r)
          (str "WITHOUT a sandbox this is expected to succeed. If it now fails, "
               "the platform grew a backend and this test should assert "
               "confinement here too (karamazov-zrq.8).")))))

(deftest the-project-can-still-read-itself
  ;; The other half of the same rule. Denying a tree that CONTAINS the project
  ;; would confine it out of its own files.
  (let [root (project!)
        r (ev root "(slurp \"deps.edn\")")]
    (is (:ok r) (str "the image cannot read its own project: " (:error r)))))

(deftest a-ctx-with-no-root-is-refused-rather-than-half-confined
  ;; Confinement is defined relative to the project, so there is nothing to
  ;; confine to. It used to answer `(+ 1 2)` with 3: the profile came out with
  ;; no writable project root and the subprocess inherited the HARNESS's
  ;; working directory — a broken image that looked like a working one.
  (doseq [ctx [{:role :implementor} {:role :implementor :root "  "}]]
    (let [r (route/eval-for ctx "(+ 1 2)" nil 8000)]
      (is (not (:ok r)) (str "an image was started for " (pr-str ctx)))
      (is (= "image-down" (:error-type r))))))

(deftest eval-cannot-write-outside-the-run-root
  ;; write_file is root-confined. eval was not, which is what made the stale
  ;; policy.clj justification for `sed` false (karamazov-zrq.7).
  (let [root (project!)
        outside (str (fs/create-temp-dir) "/escape.txt")]
    (ev root (pr-str (list 'spit outside "escaped")))
    (if (sandboxed?)
      (is (not (.exists (io/file outside)))
          "eval wrote to a path outside the run root")
      (is (.exists (io/file outside))
          (str "WITHOUT a sandbox this write is expected to land. The subprocess "
               "split still holds — the harness cannot be hot-patched — but "
               "filesystem confinement is macOS-only until karamazov-zrq.8.")))))

(deftest eval-cannot-mutate-the-live-harness-image
  ;; The payload of the escape: model-authored code taking effect in the
  ;; running harness, invisible to the mutation protocol — no checkpoint, no
  ;; validate, no soak, no userspace version, nothing for rollback to see.
  (let [root (project!)]
    (try
      (ev root "(intern 'samizdat.repl 'zrq-characterization-probe :reached)")
      (is (nil? (ns-resolve 'samizdat.repl 'zrq-characterization-probe))
          "eval interned a var into the LIVE harness namespace")
      (finally
        (when-let [n (find-ns 'samizdat.repl)]
          (ns-unmap n 'zrq-characterization-probe))))))

(deftest the-project-image-never-receives-the-harness-secrets
  ;; boundary-test's security map states the rule this obeys: a tool whose
  ;; reach is :spawns-process "must not RECEIVE secrets in the first place —
  ;; output redaction is not enough on its own". eval was :in-process for
  ;; every role and covered by the redaction wrapper alone; it spawns now, and
  ;; the first version inherited the harness environment wholesale — every
  ;; provider key, inside a process that can also write files into the project
  ;; where no output redaction would ever see them.
  (let [root (project!)]
    (doseq [k ["DEEPSEEK_API_KEY" "ANTHROPIC_API_KEY" "OPENAI_API_KEY"]]
      (let [r (ev root (str "(some? (System/getenv \"" k "\"))"))]
        (is (= "false" (:value r)) (str k " reached the project image"))))
    (testing "but the environment it needs to RUN is still there"
      (is (= "true" (:value (ev root "(some? (System/getenv \"PATH\"))")))))))

(deftest a-sandbox-refusal-does-not-read-as-a-defect
  ;; karamazov-60c: a model that reads a refusal as a bug in its own form goes
  ;; hunting in the file. Ten turns, once.
  (let [root (project!)
        r (ev root "(do (require 'jolt.process) (jolt.process/sh \"echo\" \"X\"))")]
    (if (sandboxed?)
      (do (is (not (:ok r)))
          (is (str/includes? (str (:error r)) "not the problem")
              "the refusal read as a defect in the model's own code")
          (is (str/includes? (str (:error r)) "shell")
              "the refusal did not name the tool to reach for instead"))
      (is (:ok r)
          "WITHOUT a sandbox the shell is reachable from the REPL — zrq.8"))))
