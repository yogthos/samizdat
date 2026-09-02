;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.repl-guard-test
  "Eval may not kill the server.

  Run a3ba69bb: the serve process exited 0 mid-run and nothing in abort, beam
  teardown or process disposal calls exit. Reproduced directly — an eval of
  `(System/exit 0)` ends the process, and the line after it never runs
  (karamazov-1xx)."
  (:require [clojure.test :refer [deftest testing is]]
            [samizdat.repl :as repl]
            [samizdat.repl.guard :as guard]))

(defn- forms [s] (read-string (str "[" s "\n]")))

(deftest the-routes-out-of-the-process-are-refused
  (testing "the observed one"
    (is (guard/terminating-form? (forms "(System/exit 0)"))))
  (testing "and both interop routes to the same thing, which read as ordinary
            method calls — only the receiver gives them away"
    (is (guard/terminating-form? (forms "(.exit (Runtime/getRuntime) 1)")))
    (is (guard/terminating-form? (forms "(.halt (Runtime/getRuntime) 0)"))))
  (testing "nesting does not hide it: the walk is over the read form as data"
    (is (guard/terminating-form? (forms "(do (println :a) (when true (System/exit 2)))")))
    (is (guard/terminating-form? (forms "(let [n 0] (System/exit n))"))))
  (testing "a later form is refused even though an earlier one is fine — the
            forms share a process, so form 1 having run does not make form 2's
            exit survivable"
    (is (guard/terminating-form? (forms "(+ 1 2)\n(System/exit 0)")))))

(deftest defining-a-thing-that-exits-is-not-exiting
  ;; NOT ACADEMIC. Every Clojure test runner ends (System/exit 0); this
  ;; project's deps.edn requires the agent to write one; and the eval tool's
  ;; whole pitch is to iterate on a form before writing it to a file. A guard
  ;; that refuses the work it exists to protect gets worked around, and then
  ;; protects nothing.
  (is (not (guard/terminating-form? (forms "(defn -main [& args] (System/exit 0))"))))
  (is (not (guard/terminating-form? (forms "(defn -main [] (when bad (System/exit 1)) (System/exit 0))"))))
  (is (not (guard/terminating-form? (forms "(fn [] (System/exit 0))"))))
  (testing "but the same call outside a definition body still runs, and is
            still refused"
    (is (guard/terminating-form? (forms "(do (println :a) (System/exit 2))")))))

(deftest calling-a-main-is-refused-and-writing-one-is-not
  ;; THE ROUTE THE SYMBOL CHECK CANNOT SEE. The exit is inside the callee, one
  ;; file away, in code the agent wrote itself minutes earlier — and the
  ;; harness's own eval guidance invites requiring and exercising the project's
  ;; namespaces, which walks it straight in.
  (is (guard/entry-point-call? (forms "(flight.test-runner/-main)")))
  (is (guard/entry-point-call? (forms "(do (require 'flight.main) (flight.main/-main))")))
  (is (= ["flight.main/-main"] (guard/main-calls (forms "(flight.main/-main \"a\")")))
      "named as written, so the refusal points at the one it reached for")
  (testing "HEAD POSITION ONLY — that is the difference between calling one and
            writing one, and writing one is required of this project"
    (is (not (guard/entry-point-call? (forms "(defn -main [] 1)"))))
    (is (not (guard/entry-point-call? (forms "(defn -main [& args] (System/exit 0))")))))
  (testing "and the alternative it names actually works"
    (is (not (guard/entry-point-call?
              (forms "(clojure.test/run-tests 'flight.mechanics-test)"))))))

(deftest the-main-refusal-names-both-things-that-do-work
  (let [r (repl/eval-code "(flight.test-runner/-main)")]
    (is (false? (:ok r)))
    (is (re-find #"flight\.test-runner/-main" (str (:error r))))
    (is (re-find #"jolt -M:test" (str (:error r)))
        "the child-process route, where an exit code is the point")
    (is (re-find #"run-tests" (str (:error r)))
        "and the in-eval route, skipping the runner that wraps them")))

(deftest ordinary-evals-are-untouched
  ;; The guard costs the agent nothing on the work it actually does. System is
  ;; a common namespace and only the exit member of it is a terminator.
  (is (not (guard/terminating-form? (forms "(+ 1 2)"))))
  (is (not (guard/terminating-form? (forms "(println (System/currentTimeMillis))"))))
  (is (not (guard/terminating-form? (forms "(System/getenv \"HOME\")"))))
  (is (not (guard/terminating-form? (forms "(require '[clojure.string :as str])")))))

(deftest the-refusal-names-the-call-and-the-alternative
  (let [r (repl/eval-code "(System/exit 0)")]
    (is (false? (:ok r)) "refused, not run")
    (is (re-find #"System/exit" (str (:error r)))
        "names the call, so the model knows which of its forms was the problem")
    (is (re-find #"(?i)abort" (str (:error r)))
        "and names what to do instead — a refusal with no alternative is one the
         model works around")))

(deftest the-process-survives-the-eval-that-used-to-end-it
  ;; The whole point. If this regresses, the test run itself dies here rather
  ;; than reporting a failure — which is exactly what the bug looks like.
  (repl/eval-code "(System/exit 0)")
  (is true "still running after evaluating an exit"))

(deftest an-exit-with-work-in-flight-reads-differently-from-a-clean-one
  ;; The forensic layer. It cannot prevent anything and does not try; what it
  ;; does is make the 0 stop looking like somebody stopping the server.
  (is (:bug? (guard/exit-note ["run-abc" "run-def"]))
      "work in flight at exit is the bug, and the note says so as data")
  (is (= ["run-abc"] (:active (guard/exit-note ["run-abc"])))
      "names the runs, because that is what makes the record actionable")
  (is (= 2 (:count (guard/exit-note ["run-abc" "run-def"]))))
  (is (not (:bug? (guard/exit-note [])))
      "an idle exit is somebody stopping the server, not this bug")
  (testing "the WORDS live at the log call, not here — a note assembled one
            function away from its logging is prose the ratchet cannot tell
            from a sentence aimed at the model"
    (is (not (string? (guard/exit-note ["run-abc"]))))))

(deftest every-refusal-is-a-named-rule
  ;; The guard is rules over facts read off the form (karamazov-41a.4), so a
  ;; refusal can say WHICH rule fired and on what — the thing a model needs
  ;; in order to comply rather than guess — and the reach is enumerable.
  (is (= [:process-exit] (map :rule (guard/findings (forms "(System/exit 0)")))))
  (is (= [:entry-point-call]
         (map :rule (guard/findings (forms "(flight.runner/-main)")))))
  (is (= [] (guard/findings (forms "(+ 1 2)"))))
  (testing "one form, two rules, both named"
    (is (= #{:process-exit :entry-point-call}
           (set (map :rule (guard/findings (forms "(do (System/exit 0) (-main))"))))))))

(deftest the-rules-are-enumerable-with-what-each-catches
  (let [rules (guard/rules)]
    (is (= #{:process-exit :kernel-source-write :harness-reload :entry-point-call}
           (set (map :name rules))))
    (is (every? (comp string? :doc) rules) "each says what it catches")))

(deftest the-exit-refusal-names-its-rule
  (let [r (repl/eval-code "(System/exit 0)")]
    (is (re-find #"process-exit" (str (:error r))))))
