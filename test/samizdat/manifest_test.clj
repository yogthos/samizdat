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

(ns samizdat.manifest-test
  "Multiple named loop manifests: config selects which drives a run, and the
  manifest tool lists/shows/saves them behind a real compile."
  (:require [samizdat.llm.client :as llm]
            [samizdat.agent.gitdiff :as gitdiff]
            [samizdat.agent.judge :as judge]
            [samizdat.agent.beam :as beam]
            [samizdat.manifests :as manifests]
            [samizdat.cells :as cells]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [mycelium.cell :as cell]
            [samizdat.agent.tools.base :as base]
            [samizdat.agent.tools.manifest]
            [samizdat.store.db :as db]
            [samizdat.agent.state :as state]
            [samizdat.store.userspace :as us]
            [samizdat.userspace :as userspace]
            [samizdat.workflow :as wf]))

(defn- with-db [f]
  (let [conn (db/open! ":memory:")]
    (f conn)))

(deftest factory-manifest-names-match-what-ships
  ;; wf/catalog used to glob a cwd-relative "resources/manifests", which found
  ;; nothing from a built binary or a process started elsewhere and silently
  ;; served a catalogue with the factory half missing (the provenance R3-11 bug in
  ;; a second place). It now resolves an enumerated list through io/resource,
  ;; which cannot drift on its own — so pin the list against the directory.
  (let [on-disk (->> (file-seq (io/file "resources/manifests"))
                     (filter #(str/ends-with? (.getName %) ".edn"))
                     (map #(str/replace (.getName %) #"\.edn$" ""))
                     set)
        catalogued (set (map :name (wf/catalog nil)))]
    (is (seq on-disk) "the manifests dir is readable from the test's cwd")
    (is (= on-disk catalogued)
        (str "wf/catalog and resources/manifests disagree; missing from the"
             " catalogue: " (sort (remove catalogued on-disk))
             ", catalogued but absent from disk: "
             (sort (remove on-disk catalogued))))))

(deftest turn-manifest-is-one-turn-of-the-loop
  ;; The beam drives the per-TURN slice of a manifest, derived rather than
  ;; maintained as a second file: edges back to :start and edges into
  ;; :loop/finish become :end, and the finish node goes away. This is what
  ;; lets one driver serve both the scheduler and the single-branch path, and
  ;; what finally makes :run :loop reach a production run.
  (let [def' (wf/read-definition (slurp (io/resource "manifests/loop.edn")))
        turn (wf/turn-manifest def')]
    (testing "the back edge terminates the turn"
      (is (= :end (:continue (:route (:edges turn))))))
    (testing "the slice is ONE turn — no path through it returns to :start"
      ;; Stated as acyclicity rather than as a literal route map. The map
      ;; version pinned {:done :end :abandoned :end :exhausted :end} and so
      ;; failed the moment an ending was routed through a legitimate extra
      ;; node (:distil) on its way out — a test that breaks on a correct
      ;; change is a test that gets edited to match, which is no test. What
      ;; the rewrite actually has to guarantee is that the slice terminates.
      (let [targets (fn [to] (if (map? to) (vals to) [to]))
            walk (fn walk [node seen]
                   (cond
                     (= :end node) true
                     (contains? seen node) false
                     :else (every? #(walk % (conj seen node))
                                   (targets (get (:edges turn) node)))))]
        (is (walk :start #{})
            "every path from :start reaches :end without revisiting a node")))
    (testing "the finish node is dropped, not orphaned"
      (is (contains? (:cells def') :finish))
      (is (not (contains? (:cells turn) :finish)))
      (is (not (contains? (:edges turn) :finish))))
    (testing "the per-turn chain is untouched"
      (is (= (:infer (:edges def')) (:infer (:edges turn))))
      (is (= (:parse (:edges def')) (:parse (:edges turn))))
      (is (= (:dispatches def') (:dispatches turn)))
      (is (= (:constraints def') (:constraints turn))))))

(deftest every-shipped-manifest-has-a-compilable-turn-slice
  ;; The rewrite must leave a graph mycelium still accepts — reachable nodes,
  ;; covered dispatches, satisfied constraints — for every manifest, not just
  ;; the factory loop. A slice that fails to compile is a run that cannot
  ;; start, and the beam compiles this before POST /v1/runs answers.
  (doseq [nm ["loop" "critic" "review" "worker" "reviewer" "supervisor"
              "orchestrator" "team" "feature" "decompose"]]
    (testing nm
      (let [d (wf/read-definition (slurp (io/resource (wf/manifest-resource nm))))]
        (is (some? (wf/compile-loop (wf/turn-manifest d)))
            (str nm "'s turn slice does not compile"))))))

(deftest a-whole-run-manifest-never-routes-back-to-its-entry
  ;; Run 3b8d2af5: the feature loop's revise edge went to :start, and under the
  ;; BEAM driver every revision round ran as a separate turn with a fresh data
  ;; map — the revision counter reset, branch ids collided, the guidance was
  ;; lost. The single-branch driver the tests use carries data across that
  ;; edge, so no behavioral test catches it; the SHAPE is the testable thing.
  ;; turn-manifest redirects edges returning to :start into :end, which for an
  ;; iterating loop is the definition of a turn — and for a whole-run workflow
  ;; is silent data loss. A whole-run manifest that wants to re-enter its
  ;; dispatch adds a node of its own (feature's :redispatch, orchestrator's
  ;; :retry).
  (doseq [nm ["team" "feature" "decompose" "orchestrator" "board"]]
    (testing nm
      (let [d (wf/read-definition (slurp (io/resource (wf/manifest-resource nm))))
            targets (mapcat (fn [[_ e]] (if (map? e) (vals e) [e])) (:edges d))]
        (is (not-any? #{:start} targets)
            (str nm " routes an edge back to :start — under the beam driver "
                 "that runs each cycle on a fresh data map"))))))

(deftest iterating-classification-decides-width-and-deadline
  ;; A pass through the slice is one model call only when the slice contains
  ;; :llm/infer AND loops back to start. Both halves matter: orchestrator
  ;; loops back to a start node that is an entire nested worker run, and
  ;; scheduling that as a "turn" would run five whole runs at once under a
  ;; 900s deadline meant for one provider call.
  (let [iterating? (fn [nm]
                     (wf/iterating?
                      (wf/read-definition
                       (slurp (io/resource (wf/manifest-resource nm))))))]
    (doseq [nm ["loop" "critic" "review" "worker" "reviewer" "supervisor"]]
      (is (true? (iterating? nm)) (str nm " is a per-turn loop")))
    (doseq [nm ["team" "feature" "decompose" "orchestrator"]]
      (is (false? (iterating? nm)) (str nm " is a whole-run workflow")))))

(deftest active-loop-name-comes-from-config
  (is (= "loop" (wf/active-loop-name {})))
  (is (= "loop" (wf/active-loop-name {:run {}})))
  (is (= "critic" (wf/active-loop-name {:run {:loop "critic"}}))))

(deftest the-default-loop-seeds-and-compiles
  (with-db
    (fn [conn]
      (let [{:keys [name version compiled]} (wf/load-loop! conn)]
        (is (= "loop" name))
        (is (= 1 version))
        (is (some? compiled) "the factory loop compiles")))))

(deftest a-named-manifest-with-no-resource-and-no-row-is-an-error
  (with-db
    (fn [conn]
      (is (thrown? Exception (wf/load-loop! conn "does-not-exist"))))))

(deftest saving-a-manifest-validates-then-stores
  (with-db
    (fn [conn]
      (let [good (slurp (io/resource "manifests/loop.edn"))]
        (testing "a manifest that compiles is stored and then loads by name"
          (let [r (base/run-tool {:branch {:id "B1"} :conn conn :tool-name "manifest"
                                  :args {:action "save" :name "loop2" :edn good
                                         :rationale "a second loop"}})]
            (is (= :neutral (:category r)))
            (is (= 1 (:version (us/load-latest conn :manifest "loop2"))))
            (is (= "loop2" (:name (wf/load-loop! conn "loop2"))))))
        (testing "a manifest that cannot compile is refused, not stored"
          (let [r (base/run-tool {:branch {:id "B1"} :conn conn :tool-name "manifest"
                                  :args {:action "save" :name "bad"
                                         :edn "{:cells {:x :no-such-cell}}"
                                         :rationale "a bad save on purpose"}})]
            ;; :mechanics since karamazov-gn64 — a refused edit is correctable,
            ;; not evidence about the branch's line of inquiry. What this test
            ;; is actually about is the second assertion.
            (is (= :mechanics (:category r)))
            (is (nil? (us/load-latest conn :manifest "bad")) "nothing broken was stored")))))))

(deftest a-composed-manifest-registers-and-compiles-its-sub-loops
  (with-db
    (fn [conn]
      (let [loaded (wf/load-loop! conn "orchestrator")]
        (is (= "orchestrator" (:name loaded)))
        (is (some? (:compiled loaded)) "the top level compiles once its sub-loops are registered")
        (is (= {:loop/worker "worker"} (:subworkflows (:definition loaded)))
            "it declares the worker sub-loop")))))

(deftest a-manifest-can-inject-its-own-prompt
  (with-db
    (fn [conn]
      (let [loaded (wf/load-loop! conn "review")]
        (is (some? (:compiled loaded)) "the review workflow compiles with a :prompt")
        (is (= "review" (:prompt (:definition loaded))))
        (is (str/includes? (wf/workflow-prompt (:definition loaded)) "CODE REVIEW")
            "the manifest's prompt resource is resolved")
        (is (nil? (wf/workflow-prompt {:cells {}}))
            "a manifest with no :prompt injects nothing")))))

(deftest list-and-show-round-trip
  (with-db
    (fn [conn]
      (wf/load-loop! conn)                                  ; seed "loop"
      (let [lst (base/run-tool {:branch {:id "B1"} :conn conn :tool-name "manifest"
                                :args {:action "list"}})]
        (is (re-find #"loop.*factory" (:result lst))))
      (let [shown (base/run-tool {:branch {:id "B1"} :conn conn :tool-name "manifest"
                                  :args {:action "show" :name "loop"}})]
        (is (re-find #":cells" (:result shown)) "shows the manifest as data")))))

(deftest show-and-save-missing-their-name-are-mechanics-complaints
  ;; provenance CR1-1, same shape as the skill tool: base/missing was
  ;; handed `branch` instead of ctx and its string returned raw, dropping
  ;; :category/:branch from the result map.
  (with-db
    (fn [conn]
      (let [show (base/run-tool {:branch {:id "B1"} :conn conn :tool-name "manifest"
                                 :args {:action "show"}})
            save (base/run-tool {:branch {:id "B1"} :conn conn :tool-name "manifest"
                                 :args {:action "save" :edn "{:cells {}"}})]
        (is (= :mechanics (:category show)))
        (is (map? (:branch show)))
        (is (= :mechanics (:category save)))
        (is (str/includes? (:result save) "Missing required argument(s): name"))
        (is (str/includes? (:result save) "\"manifest\"") "the skeleton names the tool")))))

(deftest an-unseeded-factory-manifest-is-readable-before-any-run
  ;; list/show read only the store, and seeding happens per-manifest on the
  ;; run that drives it — so `manifest show worker` before any worker run
  ;; answered "No manifest worker": the agent could not read the thing it is
  ;; invited to tune (karamazov-blt.4).
  (with-db
    (fn [conn]
      (let [shown (base/run-tool {:branch {:id "B1"} :conn conn :tool-name "manifest"
                                  :args {:action "show" :name "worker"}})]
        (is (= :neutral (:category shown)))
        (is (str/includes? (str (:result shown)) ":cells")
            "the factory template is served, not a refusal"))
      (let [lst (base/run-tool {:branch {:id "B1"} :conn conn :tool-name "manifest"
                                :args {:action "list"}})]
        (is (str/includes? (str (:result lst)) "worker")
            "the listing names shipped manifests before they are seeded")))))

(deftest a-manifest-that-cannot-run-cannot-be-saved
  ;; The tool validated with a bare pre-compile, skipping the ctx-key
  ;; requires check and the derived constraints that load-loop! runs — a
  ;; manifest that could not run saved fine and threw at the next run start
  ;; (karamazov-blt.6). Validation now goes through the loader's own
  ;; pipeline.
  (with-db
    (fn [conn]
      (cell/register-spec! :test/bad-requires
                           {:id :test/bad-requires :doc "x" :pure true
                            :requires [:no-such-ctx-key]
                            :handler (fn [_ d] d)})
      (try
        (let [bad (pr-str '{:cells {:start :test/bad-requires}
                            :edges {:start :end}})
              r (base/run-tool {:branch {:id "B1"} :conn conn :tool-name "manifest"
                                :args {:action "save" :name "badreq" :edn bad
                                       :rationale "a bad require on purpose"}})]
          ;; :mechanics since karamazov-gn64, like every other refused edit.
          (is (= :mechanics (:category r)))
          (is (str/includes? (str (:result r)) "ctx key")
              "the refusal names the loader check that would have thrown")
          (is (nil? (us/load-latest conn :manifest "badreq")) "nothing was stored"))
        (finally (cell/remove-cell! :test/bad-requires))))))

(deftest a-parent-can-compose-a-stored-only-child
  ;; register-subworkflows! read children from io/resource, so a parent whose
  ;; :subworkflows named a manifest the agent authored (store-only) threw
  ;; "has no resource" — composing new manifests, which the tool advertises,
  ;; was impossible for the nested case (karamazov-blt.6). Children now
  ;; resolve through the userspace seam.
  (with-db
    (fn [conn]
      (userspace/bind! conn)
      (try
        (userspace/save! :manifest "authored-child"
                         (pr-str '{:cells {:start :journal/record}
                                   :edges {:start :end}}))
        (let [parent (pr-str '{:cells {:start :child-cell}
                               :edges {:start :end}
                               :subworkflows {:child-cell "authored-child"}})
              r (base/run-tool {:branch {:id "B1"} :conn conn :tool-name "manifest"
                                :args {:action "save" :name "composed" :edn parent
                                       :rationale "compose a stored child"}})]
          (is (= :neutral (:category r)) (str "save refused: " (:result r)))
          (is (some? (us/load-latest conn :manifest "composed"))))
        (finally (userspace/unbind!))))))

(deftest a-non-iterating-manifest-may-not-route-back-to-its-entry
  ;; The other half of a-whole-run-manifest-never-routes-back-to-its-entry
  ;; above. That test pins the SHIPPED manifests; this refuses the shape at
  ;; COMPILE time, so an agent-authored manifest saved at runtime cannot
  ;; reintroduce it — the mutation protocol compiles before it stores
  ;; (karamazov-emw).
  (cells/load-cells!)
  (testing "a whole-run manifest routing an edge to :start is refused, and the
            refusal names the fix rather than only the fault"
    (let [e (try (manifests/turn-manifest
                  {:cells {:start :loop/assemble :work :feature/route}
                   :edges {:start :work :work {:again :start :done :end}}
                   :dispatches {:work [[:again (fn [d] true)] [:done (fn [d] true)]]}})
                 nil
                 (catch Throwable t t))]
      (is (some? e) "accepted a shape that silently resets the data map")
      (is (str/includes? (str (ex-message e)) "fresh data map"))
      (is (str/includes? (str (ex-message e)) "re-entry node"))))
  (testing "an ITERATING loop routing back to :start is fine — that IS the
            definition of a turn, and the slice cuts it deliberately"
    (doseq [nm ["loop" "worker" "supervisor" "reviewer"]]
      (is (some? (manifests/compiled-manifest nm)) nm)))
  (testing "and every shipped whole-run manifest still compiles"
    (doseq [nm ["feature" "team" "board" "decompose" "orchestrator"]]
      (is (some? (manifests/compiled-manifest nm)) nm)))
  (testing "the scheduler's OWN manifest routes :tick back to :start and is
            non-iterating by the same test — it schedules the branches that
            make model calls rather than making one — and is never sliced, so
            checking this at compile time was wrong and caught it"
    (is (some? (manifests/compiled-manifest "beam")))))

(deftest the-beam-driver-runs-a-whole-run-manifest-end-to-end
  ;; THE STRUCTURAL BLIND SPOT karamazov-emw names: every other test of these
  ;; flows drives workflow/run!, which carries data across a back edge, while
  ;; POST /v1/runs drives beam/run!, which turn-slices. That is how the :start
  ;; back edge shipped — no behavioural test could see it.
  (with-redefs [judge/deterministic-block (constantly nil)
                judge/parse-verdict (constantly :complete)
                judge/blocking-findings (constantly nil)
                gitdiff/changed-files (constantly ["src/example.clj" "test/example_test.clj"])
                llm/chat (fn [_ _ msgs & _]
                           (let [c (str/join " " (map :content msgs))]
                             {:content
                              (str "```tool-call\n{\"name\":\"done\",\"args\":{\"answer\":\""
                                   (cond
                                     (str/includes? c "Your role: reviewer")
                                     "PASS: the implementors' work satisfies the feature and the tests pass."
                                     (str/includes? c "Your role: supervisor")
                                     "CONTINUE: the implementors shipped and the reviewer passed."
                                     :else "built the part as asked; the suite is green")
                                   "\"}}\n```")
                              :finish-reason "stop"}))]
    (let [conn (db/open! ":memory:")
          r (beam/run! {:conn conn
                        :llm-adapter :a :llm-config {:max-tokens 16384}
                        :problem "the feature" :max-turns 6 :beam-width 1
                        :config {:run {:loop "feature" :subtasks ["alpha"]
                                       :max-revisions 2 :max-revisions-hard 1}}})]
      (is (contains? #{:completed :abandoned :done} (:status r))
          (str "a beam-driven feature run reached a real ending, not a crash: "
               (pr-str (:status r))))
      (is (some? (:run-id r))))))

(deftest a-refused-edit-is-not-charged-to-the-branch
  ;; karamazov-gn64. A manifest that does not compile is a CORRECTABLE edit:
  ;; the branch produced no claim and tested nothing about its line of
  ;; inquiry, it wrote something that did not hold together and was told
  ;; exactly why, before anything was stored. Charging that to
  ;; :consecutive-failures is the vf-jki mistake — base/refusal's docstring
  ;; counts five earlier places, and this is the seventh.
  ;;
  ;; What made it sting here: the edit-fix cycle is the one the whole mutation
  ;; protocol exists to invite. A supervisor that writes a manifest, is told
  ;; it does not compile, fixes it and saves again had done the right thing
  ;; twice and been billed two failures for it — enough to trip the stuck gate
  ;; on its third round of honest work.
  (with-db
    (fn [conn]
      (let [refused (base/run-tool {:branch {:id "B1"} :conn conn
                                    :tool-name "manifest"
                                    :args {:action "save" :name "bad"
                                           :edn "{:cells {:x :no-such-cell}}"
                                           :rationale "a bad save on purpose"}})]
        (is (= :mechanics (:category refused))
            "a manifest that does not compile is mechanics, not failure")
        (is (str/includes? (str (:result refused)) "no-such-cell")
            "and it names the actual fault, so the next attempt can fix it
             rather than guess — a refusal the model cannot act on is a
             failure whatever it is scored as")
        (is (nil? (us/load-latest conn :manifest "bad"))))

      (testing "the counters agree: mechanics is bounded, failures untouched"
        ;; The count is still KEPT — a branch looping on edits that never
        ;; compile is still spending turns, and :consecutive-mechanics-failures
        ;; bounds exactly that. What changed is which counter, and therefore
        ;; whether the stuck gate reads it as evidence about the branch's work.
        (let [b (state/new-branch {:id "B1" :problem "p"})
              after (state/record-outcome b {:category :mechanics :tool "manifest"})]
          (is (= 1 (:consecutive-mechanics-failures after)))
          (is (zero? (or (:consecutive-failures after) 0))
              "a refused edit did not move the counter that decides whether the
               branch lives")))

      (testing "a manifest that DOES compile is still progress"
        (let [ok (base/run-tool {:branch {:id "B1"} :conn conn
                                 :tool-name "manifest"
                                 :args {:action "save" :name "fine"
                                        :edn (slurp (io/resource "manifests/loop.edn"))
                                        :rationale "a good save"}})]
          (is (= :neutral (:category ok)))
          (is (:progress? ok)))))))

(deftest a-shadowed-dispatch-branch-is-refused-with-the-pattern-rules
  ;; Dispatch entries are patterns now (karamazov-41a.3), and a pattern table
  ;; can be analysed where a table of closures could not: a branch an earlier
  ;; pattern makes unreachable is refused at save. The refusal names both
  ;; branches and is rendered from a template that carries the pattern rules,
  ;; because the author here is the model, and a refusal that only states the
  ;; error sends it guessing at a language it has never been shown.
  (with-db
    (fn [conn]
      (let [def (manifests/read-definition (slurp (io/resource "manifests/loop.edn")))
            bad (pr-str (assoc-in def [:dispatches :parse]
                                  '[[:tool _]
                                    [:provider-error {:call {:ok false}}]
                                    [:no-call {:parsed nil}]]))
            r (base/run-tool {:branch {:id "B1"} :conn conn :tool-name "manifest"
                              :args {:action "save" :name "shadow" :edn bad
                                     :rationale "a shadowed branch on purpose"}})
            out (str (:result r))]
        (is (= :mechanics (:category r)))
        (is (str/includes? out "can never fire"))
        (is (str/includes? out ":tool") "names the branch in front")
        (is (str/includes? out "first match wins") "and the pattern rules, from the template")
        (is (nil? (us/load-latest conn :manifest "shadow")) "nothing was stored")))))

(deftest saving-and-showing-report-where-branch-order-decides
  ;; Two branches that overlap with neither more specific are legal — the
  ;; loop's own :parse table has them — but the order is then the only thing
  ;; deciding, and the moment to say so is when the author is looking at the
  ;; table: on save, and on show.
  (with-db
    (fn [conn]
      (let [good (slurp (io/resource "manifests/loop.edn"))
            saved (base/run-tool {:branch {:id "B1"} :conn conn :tool-name "manifest"
                                  :args {:action "save" :name "loop4" :edn good
                                         :rationale "the factory loop under another name"}})
            shown (base/run-tool {:branch {:id "B1"} :conn conn :tool-name "manifest"
                                  :args {:action "show" :name "loop4"}})]
        (is (= :neutral (:category saved)))
        (is (str/includes? (str (:result saved)) "Order-dependent"))
        (is (str/includes? (str (:result saved)) ":provider-error"))
        (is (str/includes? (str (:result shown)) "Order-dependent"))))))
