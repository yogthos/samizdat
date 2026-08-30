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

(ns samizdat.boundary-test
  "The tool dispatch seam: the redaction boundary and the result envelope.

  Two RFC gaps closed in one place, because they were one place all along.

  RFC-003 said redaction had ONE structural chokepoint (`run-shell`) and one
  wrapper (`tools/repl`), so `any future tool returning host-derived content
  is outside the boundary by default and nothing will say so`. That was
  already true of four shipped tools, not just future ones: `read_file` on a
  `.env`, `grep` for a pattern that happens to match a key, `lsp` relaying a
  server's diagnostics, `skill` reading a file off disk. Each returns host
  bytes and none passed redact.

  RFC-008 said the result envelope has no schema, and that a tool returning a
  bare string instead of a map NPE'd the loop once (`provenance CR1-1`).

  Both are properties of what comes BACK from a tool, so both belong at the
  one seam every tool's return crosses — `samizdat.agent.tools/run-tool`. The
  point of testing the seam rather than the tools is that a seam covers the
  tool nobody has written yet, which is exactly what RFC-003 said nothing
  did."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is use-fixtures]]
            [samizdat.agent.tools :as tools]
            [samizdat.agent.tools.base :as base]))

(def ^:private canary "sk-CANARYcanarycanary00000")

(def ^:private planted-env
  {"SOME_API_KEY" canary "PATH" "/usr/bin"})

(defn- call
  "Drive one tool through the production seam."
  [tool-name & {:as extra}]
  (tools/run-tool (merge {:tool-name tool-name
                          :branch {:id "b1"}
                          :env planted-env}
                         extra)))

;; Tools registered ONLY while these tests run, so the seam is exercised
;; against something it knows nothing about — which is the whole claim.
;;
;; Registered from the fixture, not at the top level. `defmethod` is a
;; side effect on a global registry that happens at REQUIRE time, so
;; top-level forms here put four fake tools into every other namespace's
;; view of the tool surface — `prompt-test/every-tool-is-documented`
;; caught exactly that and was right to.

(defn- register-test-tools! []
  (defmethod base/run-tool "boundary-test-leaky" [{:keys [branch]}]
    (base/ok branch (str "the config says " canary " which it should not")))
  (defmethod base/run-tool "boundary-test-bare-string" [_]
    "I am a string, not an envelope")
  (defmethod base/run-tool "boundary-test-no-branch" [_]
    {:result "I forgot the branch" :category :neutral})
  (defmethod base/run-tool "boundary-test-throws" [_]
    (throw (ex-info "the tool exploded" {:deliberate true}))))

(def ^:private test-tool-names
  ["boundary-test-leaky" "boundary-test-bare-string"
   "boundary-test-no-branch" "boundary-test-throws"
   "boundary-test-ordinary" "boundary-test-good" "boundary-test-redefined"])

(use-fixtures :once
  (fn [f]
    (register-test-tools!)
    (try (f)
         (finally
           (doseq [n test-tool-names] (remove-method base/run-tool n))))))

;; --- the redaction boundary -------------------------------------------------

(deftest the-dispatch-seam-redacts-every-tool-result
  ;; The structural claim, tested against a tool written after the seam and
  ;; unknown to it. Before this, a tool was inside the boundary only if its
  ;; own author remembered to put it there.
  (let [r (call "boundary-test-leaky")]
    (is (not (str/includes? (:result r) canary))
        "a credential in a tool result does not reach model space")
    (is (str/includes? (:result r) "[REDACTED]"))
    (is (str/includes? (:result r) "the config says")
        "everything that was not a secret is still there")))

(deftest redaction-does-not-eat-ordinary-output
  ;; The cost RFC-003 weighed when it declined to make this structural:
  ;; redacting content that was never sensitive. `sensitive-value?` is
  ;; high-confidence by design — a long opaque base64 string alone does not
  ;; trip it — so a build hash, a UUID and a git sha come back untouched, and
  ;; the cost it was weighing is smaller than it looked.
  (doseq [ordinary ["abc123def456abc123def456abc123def456"
                    "d3adb33fd3adb33fd3adb33fd3adb33fd3adb33f"
                    "8f14e45f-ceea-467a-9575-2c14e2b1f0a2"
                    "Compiled 41 namespaces in 3.2s"]]
    (defmethod base/run-tool "boundary-test-ordinary" [{:keys [branch]}]
      (base/ok branch ordinary))
    (is (= ordinary (:result (call "boundary-test-ordinary")))
        (str "redaction altered ordinary output: " ordinary)))
  (remove-method base/run-tool "boundary-test-ordinary"))

(def ^:private host-reach
  "Every registered tool, and how far it reaches toward the host.

  RFC-003's own lesson, made mechanical: *a property is only as strong as the
  graph it is checked against*, and the `eval` node was missing from that
  graph for four review passes. A check cannot fail against a graph that
  omits its subject — so the graph is enumerated here, and a tool that is not
  in it fails the test rather than being quietly assumed harmless.

  `:reach` is what the tool can touch, not what it is allowed to:

    :in-process     runs code in the harness image; can read the environment,
                    the resolved config and the secret store directly
    :spawns-process starts a subprocess, which must not RECEIVE secrets in
                    the first place — output redaction is not enough on its
                    own
    :host-bytes     returns bytes that came off the machine (a file, a
                    matched line, a language server's reply)
    :harness-only   returns the harness's own state: the db, the branch, the
                    userspace tables, the prompt catalogue

  Every one of these is covered by the dispatch seam's redaction. The two
  above `:host-bytes` need MORE than that, and name what supplies it."
  {;; BOTH, and which one depends on the role (karamazov-zrq). The supervisor
   ;; still evaluates in the harness image; every other role gets a sandboxed
   ;; `jolt nrepl-server` subprocess rooted at the project. The subprocess half
   ;; is why :env matters here — image/start! scrubs before spawning, because
   ;; the rule above says output redaction alone is not enough for a tool that
   ;; spawns.
   "eval"        {:reach :in-process     :also "tools/repl scrubbed wrapper; repl.image/start! scrub-env before spawn (project image)"}
   "shell"       {:reach :spawns-process :also "policy/run-shell: scrub-env before spawn"}

   ;; The two READ tools. Wider than the write tools by one deliberate step:
   ;; resolve-for-read admits the project root plus the reference roots the
   ;; project declared in .samizdat/config.edn, which the agent may not write
   ;; (karamazov-1an). Still canonicalized, still refused outside all of them.
   "read_file"   {:reach :host-bytes}
   "grep"        {:reach :host-bytes}
   "lsp"         {:reach :host-bytes}
   "skill"       {:reach :host-bytes}
   "write_file"  {:reach :host-bytes}
   "edit_file"   {:reach :host-bytes}
   ;; Anchored editing (karamazov-0kk). Same reach as edit_file and confined
   ;; the same way — resolve-under-root, and the run config refused — because
   ;; it is the same act by a different address.
   "patch"       {:reach :host-bytes}
   ;; The repl session's declaration. Records file PATHS on the branch and
   ;; reads nothing off the machine — the paths are the model's own words,
   ;; not a directory listing, and nothing resolves or opens them here.
   "plan"        {:reach :harness-only}
   ;; The only tool that reaches OFF the machine. Nothing local is read and no
   ;; secret is sent — the query is the model's own words and EXA_API_KEY is a
   ;; rate-limit token, not a credential granting access to anything of ours —
   ;; but the result is third-party text entering model space, so it crosses
   ;; the redaction boundary like every other tool result.
   "websearch"   {:reach :spawns-process
                  :also "outbound HTTP only; query is model text, no local read"}
   "doc"         {:reach :in-process     :also "reads the live image, same as eval"}
   "complete"    {:reach :in-process     :also "reads the live image, same as eval"}

   "task"        {:reach :harness-only}
   "message"     {:reach :harness-only}
   "remember"    {:reach :harness-only}
   "forget"      {:reach :harness-only}
   ;; Withdrawing a belief rather than deleting a note. Same reach as forget —
   ;; it moves rows in the harness's own db and touches nothing on the host —
   ;; but it KEEPS the row, which is the point: what was believed and why it
   ;; fell stay readable (karamazov-oov).
   "retire"      {:reach :harness-only}
   "recall"      {:reach :harness-only}
   "outcome"     {:reach :harness-only}
   ;; Writes a row to the run's own interventions table. It reaches no host
   ;; resource — but it is the only tool that changes what ANOTHER branch will
   ;; do, which is why the implementor role is denied it (roles.edn :denied).
   "intervene"   {:reach :harness-only}
   "experiment"  {:reach :harness-only}
   "verdict"     {:reach :harness-only}
   "fetch_turn"  {:reach :harness-only}
   "fetch_artifact" {:reach :harness-only}
   "cells"       {:reach :harness-only}
   "cell"        {:reach :harness-only}
   "reload_cells" {:reach :harness-only}
   "manifest"    {:reach :harness-only}
   "policy"      {:reach :harness-only}
   "prompt"      {:reach :harness-only}
   "manual"      {:reach :harness-only}
   "introspect"  {:reach :harness-only}
   "thesis"      {:reach :harness-only}
   "done"        {:reach :harness-only}
   "give_up"     {:reach :harness-only}
   "branch_theses" {:reach :harness-only}})

(deftest every-registered-tool-is-placed-on-the-security-graph
  (let [registered (set (remove #(str/starts-with? % "boundary-test-")
                                (tools/tool-names)))
        classified (set (keys host-reach))]
    (is (empty? (remove classified registered))
        (str "these tools are dispatched but do not appear in the security "
             "model's graph, so no property about the boundary is being "
             "checked against them: "
             (str/join ", " (sort (remove classified registered)))
             ". Add each to boundary-test/host-reach with what it can touch, "
             "and to RFC-003's flow diagram."))
    (is (empty? (remove registered classified))
        (str "boundary-test/host-reach names tools that are not registered: "
             (str/join ", " (sort (remove registered classified)))))))

(deftest a-tool-that-reaches-past-its-own-output-names-what-covers-it
  ;; Redacting the result is enough for a tool that only READS. It is not
  ;; enough for one that hands the environment to a subprocess, or that runs
  ;; inside this process — those need something before the output exists, and
  ;; this asserts each says what.
  (doseq [[nm {:keys [reach also]}] host-reach
          :when (#{:in-process :spawns-process} reach)]
    (is (seq also)
        (str nm " reaches " reach " but names nothing that covers it"))))

;; --- the result envelope ----------------------------------------------------

(deftest a-bare-string-does-not-reach-the-loop
  ;; provenance CR1-1: a tool returned a string instead of a map and the loop
  ;; NPE'd on `(:branch result)`. The helpers exist to prevent it and nothing
  ;; enforced their use, so the invariant table said "Convention. Not
  ;; mechanically checked."
  (let [r (call "boundary-test-bare-string")]
    (is (map? r) "the seam returns an envelope whatever the tool returned")
    (is (= :mechanics (:category r))
        "a malformed return is the harness's fault to report, not the
         branch's evidence — :mechanics, never :failure")
    (is (some? (:branch r)) "the loop still gets a branch to carry forward")
    (is (str/includes? (:result r) "boundary-test-bare-string")
        "the complaint names the tool, so it is fixable")))

(deftest a-result-missing-its-branch-does-not-reach-the-loop
  (let [r (call "boundary-test-no-branch")]
    (is (some? (:branch r)))
    (is (= :mechanics (:category r)))
    (is (str/includes? (:result r) "branch"))))

(deftest a-throwing-tool-does-not-take-the-turn-with-it
  ;; A tool that throws is the same shape of problem as one that returns the
  ;; wrong thing: the loop is handed something it cannot use. It costs the
  ;; turn, not the branch.
  (let [r (call "boundary-test-throws")]
    (is (map? r))
    (is (= :mechanics (:category r)))
    (is (str/includes? (:result r) "the tool exploded"))))

(deftest a-well-formed-result-passes-through-unchanged
  ;; The seam must be invisible to every tool that was already correct.
  (defmethod base/run-tool "boundary-test-good" [{:keys [branch]}]
    (assoc (base/ok branch "all fine") :artifact {:claim "c"} :done? true))
  (let [r (call "boundary-test-good")]
    (is (= "all fine" (:result r)))
    (is (= :neutral (:category r)))
    (is (= {:claim "c"} (:artifact r)))
    (is (true? (:done? r)))
    (is (= {:id "b1"} (:branch r))))
  (remove-method base/run-tool "boundary-test-good"))

(deftest the-seam-still-dispatches-a-redefined-tool
  ;; RFC-008: a multimethod rather than a case, "because that is what lets a
  ;; tool be redefined against a running process and picked up on the next
  ;; branch turn". Wrapping the multimethod in a function must not cost that
  ;; — dispatch has to stay at call time.
  (defmethod base/run-tool "boundary-test-redefined" [{:keys [branch]}]
    (base/ok branch "first"))
  (is (= "first" (:result (call "boundary-test-redefined"))))
  (defmethod base/run-tool "boundary-test-redefined" [{:keys [branch]}]
    (base/ok branch "second"))
  (is (= "second" (:result (call "boundary-test-redefined")))
      "the wrapper resolved the method once and froze it")
  (remove-method base/run-tool "boundary-test-redefined"))

(deftest an-unknown-tool-does-not-crash-a-branch-without-a-mechanics-tally
  ;; The :default method counted the call with `inc` on a counter that starts
  ;; absent. state/new-branch seeds the tally, so a production branch survived
  ;; it — but a resumed branch, a hand-built one, or any branch whose mechanics
  ;; map has not been touched did not.
  ;;
  ;; An unknown tool name is exactly what a model produces when it hallucinates
  ;; a capability, so the one path that exists to handle a bad call was itself
  ;; the crash. The seam above would have turned it into a :mechanics result
  ;; and hidden it, which is why this asserts the method rather than the seam.
  (let [r (base/run-tool {:branch {:id "b1"} :tool-name "no_such_tool"})]
    (is (map? r))
    (is (= 1 (get-in r [:branch :mechanics :unknown-tools])))
    (is (str/includes? (:result r) "no_such_tool")))
  (testing "and it still counts up from an existing tally"
    (let [r (base/run-tool {:branch {:id "b1" :mechanics {:unknown-tools 2}}
                            :tool-name "no_such_tool"})]
      (is (= 3 (get-in r [:branch :mechanics :unknown-tools]))))))
