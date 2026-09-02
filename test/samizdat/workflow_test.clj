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

(ns samizdat.workflow-test
  "The loop as data: the workflow definition lives in the db, compiles through
  mycelium's checks, and the manifest-driven driver produces the same runs the
  hand-written loop did. Editing the stored definition changes the next run —
  that is the whole point."
  (:require [clojure.data.json :as json]
            [samizdat.agent.beam :as beam]
            [samizdat.agent.tools.base :as base]
            [samizdat.agent.tools.introspect]
            [samizdat.cells :as cells]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest testing is use-fixtures]]
            [samizdat.agent.state :as state]
            [samizdat.llm.client :as llm]
            [samizdat.manifests :as manifests]
            [samizdat.store.db :as db]
            [samizdat.store.journal :as journal]
            [samizdat.store.runs :as runs]
            [samizdat.store.userspace :as us]
            [samizdat.userspace :as userspace]
            [samizdat.workflow :as workflow]
            [mycelium.workflow :as wf]))

;; The loop cells now live in resources and load at runtime — nothing
;; registers them as a namespace side effect anymore. Load them before the
;; tests that inspect the definition directly (compile-loop loads them itself,
;; but workflow-effects-are-fully-declared reads them without compiling).
(use-fixtures :once (fn [f] (cells/load-cells!) (f)))

(defmacro with-db [[binding] & body]
  `(let [~binding (db/open! ":memory:")]
     (try ~@body (finally (db/close ~binding)))))

(defn- fence [m]
  {:content (str "```tool-call\n" (json/write-str m) "\n```")
   :finish-reason "stop"})

(defn- scripted
  "An llm/chat replacement that returns each response in turn, repeating the
  last one when the script runs out."
  [& responses]
  (let [remaining (atom responses)]
    (fn [& _]
      (let [[r & more] @remaining]
        (when (seq more) (reset! remaining more))
        r))))

;; --- the store --------------------------------------------------------------

(defn- seed-loop!
  "Seed the shipped loop manifest into `c` as version 1. Was
  `workflows/seed! c \"loop\" \"manifests/loop.edn\"` — the shim's one
  convenience was slurping the resource for you, so that moves here rather
  than into every call site."
  [c]
  (us/seed! c :manifest "loop"
            (slurp (clojure.java.io/resource "manifests/loop.edn"))))

(deftest workflow-store-roundtrip-and-versioning
  (with-db [c]
    (is (nil? (us/load-latest c :manifest "loop")))
    (is (= 1 (us/save! c :manifest "loop" "{:cells {}}")))
    (is (= 2 (us/save! c :manifest "loop" "{:cells {:a :b}}")))
    (let [w (us/load-latest c :manifest "loop")]
      (is (= 2 (:version w)))
      (is (= "{:cells {:a :b}}" (:body w))))
    (is (= "{:cells {}}" (:body (us/load-version c :manifest "loop" 1))))))

(deftest seeding-is-idempotent
  (with-db [c]
    (is (= 1 (:version (seed-loop! c))))
    (is (= 1 (:version (seed-loop! c)))
        "a second seed does not stack versions")
    (is (some? (:body (us/load-latest c :manifest "loop"))))))

;; --- the definition ---------------------------------------------------------

(deftest the-shipped-loop-definition-compiles-clean
  (let [def (workflow/read-definition (slurp (clojure.java.io/resource "manifests/loop.edn")))
        compiled (workflow/compile-loop def)]
    (is (some? compiled))
    (is (nil? (:mycelium/compile-warnings (:compiled-fsm compiled)))
        "every loop cell declares its effects")))

(deftest removing-the-journal-hop-fails-compile
  ;; The constraint is the mutation protocol's teeth: an agent edit that
  ;; routes a tool result around the journal must die at compile, not ship.
  (let [def (workflow/read-definition (slurp (clojure.java.io/resource "manifests/loop.edn")))
        ;; Route the tool path around the journal while keeping :journal
        ;; reachable from the no-call path, so the unreachable check cannot
        ;; catch it first — only the constraint can. Around the journal ONLY:
        ;; skipping :settle as well would be caught earlier by the schema
        ;; chain, since the arbiter requires what settle writes.
        broken (-> def
                   (assoc-in [:edges :dispatch] :settle)
                   (assoc-in [:edges :no-call] :journal))]
    (is (thrown-with-msg? Exception #"must-follow"
                          (workflow/compile-loop broken)))))

;; --- the driver -------------------------------------------------------------

(deftest a-scripted-run-ships-through-the-manifest
  (with-db [c]
    (with-redefs [llm/chat (scripted
                            (fence {:name "thesis"
                                    :args {:goal "solve the problem"
                                           :technique "direct"}})
                            (fence {:name "done"
                                    :args {:answer "the problem is solved directly"}}))]
      (let [r (workflow/run! {:conn c :config {:run {}}
                              :llm-adapter :a :llm-config {:max-tokens 16384}
                              :problem "solve the problem" :max-turns 10})]
        (is (= :completed (:status r)))
        (is (= "the problem is solved directly" (:answer r)))
        (let [turns (journal/branch-turns c (:run-id r) "B1")]
          (is (= ["thesis" "done"] (mapv :tool_name turns))))
        (is (= "completed" (:status (runs/get-run c (:run-id r)))))))))

(deftest the-beam-drives-the-manifest-too
  ;; The fix for the review's biggest finding: the beam used to call
  ;; samizdat.agent.loop's steps directly and never touch a manifest, so
  ;; `:run :loop` was documented, parsed from HARNESS_LOOP, and read by
  ;; nothing on the production path — every POST /v1/runs got the factory
  ;; composition no matter what was configured, and critic/team/feature/
  ;; decompose ran only under this suite. The beam now compiles the per-turn
  ;; SLICE of the selected manifest and runs each branch through it.
  (with-db [c]
    (with-redefs [llm/chat (scripted
                            (fence {:name "thesis"
                                    :args {:goal "solve the problem"
                                           :technique "direct"}})
                            (fence {:name "done"
                                    :args {:answer "the problem is solved directly"}}))]
      (let [r (beam/run! {:conn c :config {:run {:beam-width 1}}
                          :llm-adapter :a :llm-config {:max-tokens 16384}
                          :problem "solve the problem" :max-turns 10 :beam-width 1})]
        (is (= :completed (:status r)))
        (is (= "the problem is solved directly" (:answer r)))
        (testing "the branch ran the manifest's per-turn chain"
          (is (= ["thesis" "done"]
                 (mapv :tool_name (journal/branch-turns c (:run-id r) "B1")))))
        (testing "the run records which loop drove it, like the other driver"
          (let [note (->> (journal/events-since c (:run-id r) 0)
                          (filter #(= "loop-workflow" (:kind %)))
                          first)]
            (is (some? note)
                "a beam run journals its :loop-workflow provenance")))))))

(deftest a-non-iterating-manifest-forces-beam-width-1
  ;; team/feature/decompose are whole-run workflows: one pass is the branch's
  ;; entire job, not one model call. Running five concurrently would multiply
  ;; the job rather than explore five lines of one, so the beam overrides the
  ;; requested width and says so in the run row.
  (with-db [c]
    (with-redefs [llm/chat (scripted (fence {:name "give_up"
                                             :args {:reason "stub"}}))]
      (let [r (beam/run! {:conn c :config {:run {:loop "team" :subtasks ["a"]}}
                          :llm-adapter :a :llm-config {:max-tokens 16384}
                          :problem "anything" :max-turns 4 :beam-width 5})]
        (is (= 1 (:beam_width (runs/get-run c (:run-id r))))
            "a whole-run manifest runs one branch regardless of the width asked for")))))

(deftest the-turn-cap-exhausts-through-the-manifest
  (with-db [c]
    (with-redefs [llm/chat (scripted
                            (fence {:name "thesis"
                                    :args {:goal "keep going" :technique "loop"}}))]
      (let [r (workflow/run! {:conn c :config {:run {}}
                              :llm-adapter :a :llm-config {:max-tokens 16384}
                              :problem "never finishes" :max-turns 2})]
        (is (= :exhausted (:status r)))
        (is (some? (:residual r)))
        (is (= 2 (count (journal/branch-turns c (:run-id r) "B1"))))
        (is (= "failed" (:status (runs/get-run c (:run-id r)))))))))

(deftest editing-the-stored-definition-changes-the-next-run
  ;; The acceptance in one test: save a v2 of the loop from the REPL and the
  ;; next run behaves differently, no restart, no code change.
  (with-db [c]
    (with-redefs [llm/chat (scripted
                            (fence {:name "thesis"
                                    :args {:goal "g" :technique "t"}}))]
      ;; Seed v1, then write a v2 that routes every response down the no-call
      ;; path — a visible behavior change made purely by editing stored EDN.
      (seed-loop! c)
      (let [v1 (edn/read-string (:body (us/load-latest c :manifest "loop")))
            v2 (assoc-in v1 [:dispatches :parse]
                         '[[:provider-error (fn [d] (not (:ok (:call d))))]
                           [:no-call (fn [d] true)]
                           [:tool (fn [d] false)]])]
        (us/save! c :manifest "loop" (pr-str v2))
        (let [r (workflow/run! {:conn c :config {:run {}}
                                :llm-adapter :a :llm-config {:max-tokens 16384}
                                :problem "p" :max-turns 1})]
          (is (= :exhausted (:status r)))
          (is (= ["mechanics"]
                 (mapv :category (journal/branch-turns c (:run-id r) "B1")))
              (str "v2 sends every response down the no-call path, which"
                   " journals it as mechanics — v1 dispatches the same"
                   " response as a neutral thesis turn"))
          (is (some #(and (= "loop-workflow" (:kind %))
                          (str/includes? (str (:data %)) "2"))
                    (journal/events-since c (:run-id r) 0 100))
              "the run records which workflow version drove it"))))))

(deftest provider-failure-routes-through-the-manifest
  (with-db [c]
    (let [calls (atom 0)]
      (with-redefs [llm/chat (fn [& _]
                               (if (= 1 (swap! calls inc))
                                 (throw (ex-info "socket reset" {}))
                                 (fence {:name "done"
                                         :args {:answer "recovered and finished"}})))]
        (let [r (workflow/run! {:conn c :config {:run {}}
                                :llm-adapter :a :llm-config {:max-tokens 16384}
                                :problem "recovered and finished" :max-turns 5})]
          (is (= :completed (:status r)))
          (let [turns (journal/branch-turns c (:run-id r) "B1")]
            (is (= "__provider_error__" (:tool_name (first turns)))
                "the failed call is journalled like any turn")))))))

(deftest workflow-effects-are-fully-declared
  (let [def (workflow/read-definition (slurp (clojure.java.io/resource "manifests/loop.edn")))
        fx (wf/workflow-effects def)]
    (is (not-any? :undeclared (vals fx))
        (str "cells with undeclared effects: "
             (keep (fn [[k v]] (when (:undeclared v) k)) fx)))
    (is (:pure (get fx :parse)) "fence parsing is pure")
    (is (contains? (:effects (get fx :infer)) :net))))

(deftest role-ctx-assigns-a-per-role-model
  (let [base {:config {:run {:role-models {:supervisor {:provider "glm" :model "glm-5.3"}
                                           :implementor {:provider :deepseek}}}}
              :llm-adapter :base-adapter
              :llm-config {:provider :openai :model "gpt-4o"}}]
    (testing "a configured role gets its own provider + model + adapter"
      (let [c (workflow/role-ctx base :supervisor)]
        (is (= :glm (get-in c [:llm-config :provider])))
        (is (= "glm-5.3" (get-in c [:llm-config :model])))
        (is (not= :base-adapter (:llm-adapter c)) "the adapter is swapped too")))
    (testing "a role configured with only a provider takes that provider's default model"
      (is (= "deepseek-v4-flash" (get-in (workflow/role-ctx base :implementor)
                                         [:llm-config :model]))))
    (testing "an unconfigured role keeps the run's default model and adapter"
      (let [c (workflow/role-ctx base :reviewer)]
        (is (= :openai (get-in c [:llm-config :provider])))
        (is (= :base-adapter (:llm-adapter c)))))))

(deftest catalog-lists-every-workflow-with-a-description
  ;; self-healing: the supervisor can only switch to / tune a workflow it knows
  ;; exists. The catalog is that discoverable menu.
  (let [conn (db/open! ":memory:")
        by-name (into {} (map (juxt :name identity)) (workflow/catalog conn))]
    (is (contains? by-name "feature"))
    (is (contains? by-name "team"))
    (is (contains? by-name "decompose"))
    (is (contains? by-name "loop"))
    (is (str/includes? (:description (by-name "decompose")) "Decompose")
        "each carries its :description")
    (is (str/includes? (workflow/render-catalog conn) "decompose")
        "and renders as a text menu for the supervisor")))

(deftest a-definition-with-no-cells-is-refused-at-compile
  ;; Found by a real failure, and the failure is the argument for the guard.
  ;; store/workflows.clj renamed the body column to :edn on the way out; when
  ;; it was retired, load-loop! kept reading :edn from a raw userspace row and
  ;; got nil. read-definition returned nil, mycelium compiled it without
  ;; complaint, and the run died four frames into the driver with
  ;; "class nil cannot be cast to class clojure.lang.IFn" — nothing in the
  ;; message pointing at the manifest, the column, or the loader.
  ;;
  ;; A compile that accepts a non-workflow is worse than one that rejects a
  ;; workflow: the mutation protocol's whole first line of defence is that a
  ;; definition which cannot run cannot be stored.
  (doseq [d [nil {} {:edges {:start :end}} {:cells {}}]]
    (is (thrown-with-msg? Exception #"no :cells" (workflow/compile-loop d))
        (str "compiled a definition that is not a workflow: " (pr-str d)))))

(deftest load-loop-reads-the-body-column
  ;; The specific regression, pinned: a stored manifest must round-trip
  ;; through load-loop! as a real definition, not a nil one.
  (with-db [c]
    (seed-loop! c)
    (let [{:keys [definition compiled version]} (workflow/load-loop! c "loop")]
      (is (= 1 version))
      (is (seq (:cells definition)) "the definition came back with its cells")
      (is (some? compiled))
      (is (= :loop/assemble (get-in definition [:cells :start]))))))

(deftest a-stored-role-manifest-is-what-compiles-and-what-the-catalog-serves
  ;; compiled-manifest (the seam every role sub-loop runs through) read the
  ;; FACTORY resource, so `manifest save "worker"` landed in userspace and was
  ;; never read back — the self-tuning story held only for the run's top-level
  ;; manifest (karamazov-blt.3). The catalog likewise served the factory
  ;; :description for an evolved manifest (karamazov-blt.4).
  (with-db [c]
    (userspace/bind! c)
    (try
      (let [d (assoc (edn/read-string (userspace/body :manifest "worker"))
                     :description "tuned-by-test")]
        (userspace/save! :manifest "worker" (pr-str d)))
      (is (str/includes? (str (manifests/manifest-body "worker")) "tuned-by-test")
          "the role seam serves the stored version")
      (is (some? (workflow/compiled-manifest "worker"))
          "and the stored version is what compiles as the role sub-loop")
      (is (= "tuned-by-test"
             (:description (some #(when (= "worker" (:name %)) %)
                                 (workflow/catalog c))))
          "the catalog describes the version that will actually run")
      (finally (userspace/unbind!)))))

(deftest introspect-renders-the-driving-manifest-not-the-factory-loop
  ;; The self-observation tool dumped manifests/loop.edn whatever was running,
  ;; ignoring both the stored version and the run's actual manifest — the
  ;; supervisor diagnosed from a wiring dump that was wrong twice over
  ;; (karamazov-blt.4). The beam's ctx :turn-workflow is the version-true
  ;; wiring, and introspect now renders it.
  (let [r (base/run-tool {:tool-name "introspect" :branch {:id "B1"}
                          :turn-workflow {:name "custom" :version 7
                                          :definition {:cells {:start :x/one}
                                                       :edges {:start :end}}}})]
    (is (str/includes? (str (:result r)) "custom v7")
        "the header names the manifest and version that drive the run")
    (is (str/includes? (str (:result r)) "x/one")
        "and the wiring rendered is that manifest's, not loop.edn's")))

(deftest introspect-falls-back-when-the-turn-workflow-carries-no-definition
  ;; karamazov-2ld, found (and hot-patched live) by the harness's own
  ;; supervisor during the todomvc dogfood run: the beam destructures
  ;; compile-turn-loop's COMPILED slice into ctx :turn-workflow, which has no
  ;; :definition — so introspect showed the supervisor an EMPTY loop wiring at
  ;; exactly the moment it was diagnosing the loop. A :turn-workflow without a
  ;; :definition falls back to the stored manifest body rather than rendering
  ;; nothing.
  (let [r (base/run-tool {:tool-name "introspect" :branch {:id "B1"}
                          :config {:run {:loop "loop"}}
                          :turn-workflow {:compiled-fsm {} :input-schema-raw {}}})]
    (is (str/includes? (str (:result r)) "loop/assemble")
        "the wiring shown is the stored loop manifest's, not an empty dump")))
