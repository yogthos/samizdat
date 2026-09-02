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

(ns samizdat.cell-schema-test
  "The cells declare the shape of the data map they read and write, so
  mycelium's schema-chain check has something to check.

  WHY THIS EXISTS. mycelium walks every path from :start accumulating the keys
  each cell's :output declares, and refuses a manifest where a cell's required
  :input is not among them (mycelium.workflow/validate-schema-chain!). With no
  schemas anywhere the walk accumulates nothing, compares nothing, and passes
  everything — 59 cells declared none, so the check had been running and
  finding nothing since it was written. Measured on the same two-cell graph:
  with schemas mycelium refuses `:demo/b requires keys #{:y} but only #{:x}
  available`; without them it compiles.

  That is the one thing propose-cell! could not catch. A cell edit that renames
  a key the next node reads compiles, survives the soak — which dry-runs ONE
  path with the effectful cells stubbed to identity — and reaches a run."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [mycelium.cell :as cell]
            [mycelium.core :as myc]
            [mycelium.dev :as dev]
            [malli.core :as m]
            [samizdat.agent.tools.base :as base]
            [samizdat.agent.tools.manifest]
            [samizdat.cells :as cells]
            [samizdat.lexicon :as lexicon]
            [samizdat.manifests :as manifests]
            [samizdat.store.db :as db]
            [samizdat.store.userspace :as us]))

(defn- loop-def []
  (edn/read-string (slurp (io/resource "manifests/loop.edn"))))

(deftest every-cell-a-shipped-manifest-reaches-declares-its-shape
  ;; The general form. This began as a list of one manifest and grew a
  ;; manifest at a time through karamazov-6y7.3; it is now every shipped
  ;; manifest, which is the whole registry, because beam-test separately pins
  ;; that no cell is registered which no manifest reaches.
  ;;
  ;; A cell added without a shape is caught HERE rather than by the manifest
  ;; that first suffers for it — and an agent-authored cell goes through the
  ;; same defcell, so the rule holds for cells this suite never sees.
  (cells/load-cells!)
  (doseq [nm manifests/shipped-manifests]
    (let [d (edn/read-string (slurp (io/resource (manifests/manifest-resource nm))))
          ;; What a compile does. A composed sub-workflow cell (orchestrator's
          ;; :loop/worker) exists only once its child has been registered, so
          ;; without this it is absent from the registry rather than
          ;; shapeless — a different complaint with the same symptom.
          _ (manifests/register-subworkflows! d)]
      (doseq [[node cell-id] (sort-by key (:cells d))]
        (testing (str nm " " node " (" cell-id ")")
          (let [spec (cell/get-cell cell-id)]
            (is (some? spec) "the cell is registered")
            (is (some? (get-in spec [:schema :input]))
                (str cell-id " declares no :input, so nothing upstream is"
                     " required to produce anything for it"))
            (is (some? (get-in spec [:schema :output]))
                (str cell-id " declares no :output, so it contributes nothing"
                     " to what downstream cells may rely on"))))))))

(deftest the-turn-accumulates-keys-rather-than-nothing
  ;; The direct measurement of the bug: dev/infer-workflow-schema walks the
  ;; graph reporting what each node adds. Every node reported #{} — which reads
  ;; as "this loop moves no data", and was true only of the declarations.
  (cells/load-cells!)
  (let [inferred (dev/infer-workflow-schema (loop-def))]
    (doseq [node [:infer :parse :dispatch :arbiter :route]]
      (testing node
        (is (seq (:adds (get inferred node)))
            (str node " adds no keys any downstream cell can rely on"))))
    (testing "and the keys reach the end of the turn"
      (is (contains? (:available-after (get inferred :route)) :verdict)
          ":verdict is what :finish routes on"))))

(deftest a-cell-reading-a-key-nothing-produces-is-refused
  ;; The check earning its keep. :gate/arbiter reads :before, which only
  ;; :loop/assemble writes; a manifest that routes around assemble is a
  ;; manifest whose arbiter steers on nil. Compiled through the loader's own
  ;; pipeline, so this is the error an agent-authored manifest would get from
  ;; `manifest save`.
  (cells/load-cells!)
  (let [broken '{:cells {:start :llm/infer
                         :arbiter :gate/arbiter}
                 :edges {:start :arbiter
                         :arbiter :end}}
        e (try (manifests/compile-definition broken) nil
               (catch Throwable t t))]
    (is (some? e) "compiled a manifest whose arbiter reads a key nothing wrote")
    (is (re-find #"(?i)schema chain" (str (ex-message e)))
        (str "the refusal names the schema chain rather than failing later: "
             (ex-message e)))))

(deftest every-shipped-manifest-still-compiles-with-partial-schemas
  ;; THE ROLLOUT HAZARD, pinned. Accumulation is monotonic — a cell with no
  ;; schema adds nothing to what is available but takes nothing away — so a
  ;; schema'd cell sitting downstream of unschema'd ones is refused for keys
  ;; that ARE produced, just not declared.
  ;;
  ;; Which is why :loop/finish declares :verdict optional: feature, team,
  ;; decompose and orchestrator reach it without :loop/route, and their
  ;; routers are not schema'd yet. Tighten it in karamazov-6y7.3, when they
  ;; are — and until then this test is what says the rollout is still safe
  ;; mid-flight rather than only at the end.
  (cells/load-cells!)
  (doseq [nm manifests/shipped-manifests]
    (testing nm
      (is (some? (manifests/compiled-manifest nm))
          (str nm " no longer compiles — a cell schema requires a key that"
               " something upstream produces but does not yet declare")))))

;; --- what a mismatch costs at run time (karamazov-6y7.2) ---------------------

(defn- with-drifting-cell
  "Registers a cell that PROMISES :y and produces :z — the rename this epic
  exists to catch, in the form it takes at run time rather than at compile
  time. Its manifest is one node, so nothing else can explain a failure."
  [f]
  (cell/defcell :schema-test/drift
    {:doc "Declares :y and writes :z. Deliberately wrong."
     :pure true
     :input  [:map [:x :int]]
     :output [:map [:y :int]]}
    (fn [_ d] (assoc d :z 1)))
  (try (f '{:cells {:start :schema-test/drift}
            :edges {:start :end}})
       (finally (cell/remove-cell! :schema-test/drift))))

(deftest warn-mode-records-the-drift-and-runs-on
  (with-drifting-cell
    (fn [wf]
      (let [data (myc/run-compiled (myc/pre-compile wf {:validate :warn}) {} {:x 1})]
        (is (not (myc/error? data)) "the run completed")
        (is (seq (:mycelium/warnings data)) "and said so")
        (testing "the warning names the rename rather than only the mismatch"
          (let [w (first (:mycelium/warnings data))]
            (is (contains? (set (:missing (:key-diff w))) :y)
                (str "no :missing #{:y} in " (pr-str w)))))))))

(deftest bare-mycelium-throws-a-strict-violation
  ;; What mycelium does on its own, kept because it is the reason
  ;; manifests/on-error exists: an ::fsm/error state is installed only when a
  ;; compile is handed an :on-error, so without one maestro's throwing default
  ;; runs. Compiled here directly rather than through the loader, which is the
  ;; difference the next test is about.
  (with-drifting-cell
    (fn [wf]
      (let [e (try (myc/run-compiled (myc/pre-compile wf {:validate :strict})
                                     {} {:x 1})
                   nil
                   (catch Throwable t t))]
        (is (some? e))
        (is (some? (get-in (ex-data e) [:data :mycelium/schema-error]))
            "the schema error is in the ex-data, not only in the message")))))

(deftest the-loader-returns-a-strict-violation-as-data
  ;; karamazov-6y7.5. Every driver reads (myc/error? data) after run-compiled
  ;; — run-turn, run! and beam/advance alike — and for a schema violation that
  ;; branch used to be unreachable, because the throw went past it. The beam
  ;; could not abandon one branch with a reason, and what surfaced was a page
  ;; of FSM internals instead of the message naming the cell and the keys.
  (cells/load-cells!)
  (with-drifting-cell
    (fn [wf]
      (with-redefs [lexicon/policy (fn [_] {:mode :strict})]
        (let [data (myc/run-compiled (manifests/compile-definition wf) {} {:x 1})
              err (myc/workflow-error data)]
          (is (myc/error? data) "the violation came back as data, not a throw")
          (is (= :schema/output (:error-type err))
              (str "and kept its own error type rather than becoming a generic"
                   " handler failure: " (pr-str err)))
          (is (= :schema-test/drift (:cell-id err)) "naming the cell")
          (is (contains? (set (:missing (:key-diff err))) :y)
              "and the key, which is what makes it actionable"))))))

(deftest a-handler-that-throws-still-throws
  ;; The other half of on-error, and the reason it is surgical. feature's
  ;; `safely` catches a throwing stage to record it and fall through to a safe
  ;; default; the beam's unwrap-round-error digs the real cause out of the
  ;; nested ex-data. Turning every crash into data would leave both looking at
  ;; a nil verdict nothing notices — a loud failure traded for a silent one.
  (cells/load-cells!)
  (cell/defcell :schema-test/boom
    {:doc "Throws. Deliberately."
     :pure true
     :input [:map] :output [:map]}
    (fn [_ _] (throw (ex-info "boom" {}))))
  (try
    (is (thrown? Exception
                 (myc/run-compiled
                  (manifests/compile-definition '{:cells {:start :schema-test/boom}
                                                  :edges {:start :end}})
                  {} {})))
    (finally (cell/remove-cell! :schema-test/boom))))

(deftest the-validate-mode-is-policy-rather-than-a-constant
  (testing "the shipped default"
    (is (= :warn (manifests/validate-mode))))
  (testing "an edit to gates.edn moves it"
    (with-redefs [lexicon/policy (fn [k] (when (= k :schema-validation) {:mode :strict}))]
      (is (= :strict (manifests/validate-mode)))))
  (testing "and a MISSING policy does not silently switch checking off"
    ;; :off would be the dangerous default — nobody finds out. :strict would
    ;; be the other kind of wrong, halting runs over declarations the rollout
    ;; has not finished tightening.
    (with-redefs [lexicon/policy (constantly nil)]
      (is (= :warn (manifests/validate-mode))))))

(deftest the-loader-compiles-under-the-policy-mode
  ;; The accessor being right is not the same as it reaching mycelium, which
  ;; reads :validate at COMPILE time into the interceptors it builds. This is
  ;; the half that would silently do nothing if compile-definition dropped the
  ;; opts map.
  (cells/load-cells!)
  (with-drifting-cell
    (fn [wf]
      (testing "strict"
        (with-redefs [lexicon/policy (fn [_] {:mode :strict})]
          (is (myc/error? (myc/run-compiled (manifests/compile-definition wf)
                                            {} {:x 1})))))
      (testing "warn"
        (with-redefs [lexicon/policy (fn [_] {:mode :warn})]
          (let [data (myc/run-compiled (manifests/compile-definition wf) {} {:x 1})]
            (is (not (myc/error? data)))
            (is (seq (:mycelium/warnings data)))))))))

(deftest the-chain-check-is-not-switchable-by-the-mode
  ;; THE HALF THAT MUST NOT BE TUNABLE. :validate feeds the runtime
  ;; interceptors; the schema CHAIN is checked inside validate-workflow
  ;; unconditionally. A supervisor that sets :off to quiet a warning must not
  ;; thereby be able to save a manifest whose wiring does not hold together.
  (cells/load-cells!)
  (let [broken '{:cells {:start :llm/infer :arbiter :gate/arbiter}
                 :edges {:start :arbiter :arbiter :end}}]
    (doseq [mode [:off :warn :strict]]
      (testing mode
        (with-redefs [lexicon/policy (fn [_] {:mode mode})]
          (is (thrown? Exception (manifests/compile-definition broken))
              (str "under " mode " a manifest with a broken chain compiled")))))))

;; --- the map a run STARTS from (karamazov-6y7.4) ------------------------------

(def ^:private entry-data
  "What each driver actually hands its manifest, by manifest. The point of
  :input-schema is that this table and the manifests agree; the test below is
  what makes disagreeing fail here rather than six cells into a run.

  Four shapes, because there are four kinds of entry point: the loop family
  (every driver that advances a branch), the beam's own scheduler round, the
  oversight stream, and the repair ladder."
  (let [branch {:id "B1" :problem "p" :messages []}]
    {:beam      {:branches [] :turn 1}
     :oversight {:oversight/carry nil}
     :repair    {:body "{}"}
     :default   {:branch branch :turn 1}}))

(defn- entry-for [nm]
  (get entry-data (keyword nm) (:default entry-data)))

(deftest every-shipped-manifest-declares-what-it-starts-from
  (doseq [nm manifests/shipped-manifests]
    (testing nm
      (let [d (edn/read-string (slurp (io/resource (manifests/manifest-resource nm))))]
        (is (some? (:input-schema d))
            (str nm " declares no :input-schema, so the map a run starts from"
                 " is never checked — myc/pre-compile compiles nil and"
                 " check-input-schema returns nil for anything"))))))

(deftest the-drivers-satisfy-the-schemas-they-run-under
  ;; THE HALF THAT MATTERS. A schema no caller satisfies is not a contract,
  ;; it is an outage waiting for the first real run — and every one of these
  ;; entry points is reached only by a driver, never by a test that would
  ;; notice. Same reasoning manifests/ctx-keys is checked from both ends.
  (doseq [nm manifests/shipped-manifests]
    (testing nm
      (let [d (edn/read-string (slurp (io/resource (manifests/manifest-resource nm))))]
        (is (m/validate (:input-schema d) (entry-for nm))
            (str nm " refuses the map its own driver hands it: "
                 (pr-str (m/explain (:input-schema d) (entry-for nm)))))))))

(deftest a-malformed-start-is-refused-before-any-cell-runs
  (cells/load-cells!)
  (let [compiled (manifests/compiled-manifest "loop")
        out (myc/run-compiled compiled {} {:branch {:id "B1"} :turn "one"})]
    (is (some? (:mycelium/input-error out))
        "a :turn that is not an int reached the cells")
    (is (= [:turn] (:in (first (:errors (:mycelium/input-error out)))))
        (str "the error names the bad key rather than failing later: "
             (pr-str (:mycelium/input-error out))))))

(deftest a-composed-cell-declares-what-its-child-guarantees
  ;; karamazov-6y7.6. mycelium infers a composed cell's output from the
  ;; child's END-REACHING cells, while the handler returns the child's whole
  ;; final data map — so a key written mid-graph was delivered and not
  ;; declared. orchestrator dispatches on :verdict, which the worker's
  ;; :loop/route writes, and :loop/route does not edge to :end.
  ;;
  ;; samizdat derives the output instead: the union, over cells lying on EVERY
  ;; path from :start to :end, of the keys each guarantees on every
  ;; transition. Both halves are what make it safe to require :verdict at
  ;; :loop/finish rather than merely plausible.
  (cells/load-cells!)
  (let [d (edn/read-string (slurp (io/resource "manifests/orchestrator.edn")))]
    (manifests/register-subworkflows! d)
    (let [out (get-in (cell/get-cell :loop/worker) [:schema :output])
          ;; mycelium wraps a composed cell's output as
          ;; [:per-transition {:success [:map ...] :failure [:map ...]}],
          ;; so what samizdat derived is the :success arm.
          ks (set (map first (rest (:success (second out)))))]
      (is (contains? ks :verdict)
          (str ":loop/worker does not promise :verdict, so :loop/finish cannot"
               " require it: " (pr-str out)))
      (testing "and does not promise what only some paths write"
        ;; :parsed is written on two of :llm/parse's three transitions and on
        ;; none of the provider-error path. A union over all the child's cells
        ;; would declare it, and a parent reading it would get nil.
        (is (not (contains? ks :parsed))
            (str "a key only some paths write leaked into the promise: "
                 (pr-str ks)))))))

;; --- what the review of this epic found (198a677e..HEAD) ---------------------

(deftest a-validation-throwable-with-no-message-still-refuses-the-save
  ;; `(ex-message (RuntimeException.))` is nil, and the save path read the
  ;; complaint straight out of it — so a nil message took the "it compiled"
  ;; branch and STORED the manifest. The invariant the tool exists for,
  ;; inverted by the one throwable that says nothing about itself. A bare
  ;; NPE or RuntimeException out of mycelium, malli or the userspace read is
  ;; enough to get there.
  (let [conn (db/open! ":memory:")]
    (with-redefs [manifests/compile-loop (fn [_] (throw (RuntimeException.)))]
      (let [r (base/run-tool {:branch {:id "B1"} :conn conn :tool-name "manifest"
                              :args {:action "save" :name "nilmsg"
                                     :edn "{:cells {:start :loop/assemble}
                                            :edges {:start :end}}"
                                     :rationale "a message-less throwable"}})]
        (is (nil? (us/load-latest conn :manifest "nilmsg"))
            "a manifest whose validation threw was stored anyway")
        (is (str/includes? (str (:result r)) "refused"))))))

(deftest a-composed-child-written-with-map-cell-refs-still-declares-itself
  ;; mycelium accepts both `:ns/id` and `{:id :ns/id}` in :cells, and
  ;; cell/get-cell returns nil for the map form. Every shipped manifest here
  ;; uses the bare form, so the derivations read fine — and an agent-authored
  ;; child using the map form would have silently declared nothing, which is
  ;; the bug child-input-schema was added to fix, one level further down.
  (cells/load-cells!)
  (with-redefs [manifests/manifest-body
                (fn [nm]
                  (when (= nm "mapref-child")
                    (pr-str '{:cells {:start {:id :loop/assemble}
                                      :route {:id :loop/route}}
                              :edges {:start :route
                                      :route {:continue :end :done :end
                                              :abandoned :end :exhausted :end}}
                              :dispatches
                              {:route [[:continue (fn [d] true)]
                                       [:done (fn [d] false)]
                                       [:abandoned (fn [d] false)]
                                       [:exhausted (fn [d] false)]]}})))]
    (manifests/register-subworkflows! '{:subworkflows {:test/mapref "mapref-child"}})
    (try
      (let [spec (cell/get-cell :test/mapref)
            in (get-in spec [:schema :input])
            out (get-in spec [:schema :output])]
        (is (some? in)
            "the composed cell declares no :input, so a parent entering through
             it starts its chain walk with nothing available")
        (is (contains? (set (map first (rest in))) :branch))
        (is (contains? (set (map first (rest (:success (second out))))) :verdict)
            "and the derived output missed what the child guarantees"))
      (finally (cell/remove-cell! :test/mapref)))))

;; --- Tier 1 satisfiability: one precondition pass (karamazov-41a.6) ---------

(defn- shipped-definition [n]
  (edn/read-string (slurp (io/resource (manifests/manifest-resource n)))))

(deftest preconditions-are-one-report-over-ctx-and-data
  ;; check-requires! answers for ctx keys and the schema chain for data keys,
  ;; each by throwing. This is the same two facts as ONE report, per node:
  ;; what it requires from the driver and from upstream, and what holds on
  ;; every path that reaches it — so an author can ask before an edit rather
  ;; than learn from the refusal after.
  (cells/load-cells!)
  (let [r (manifests/preconditions (loop-def))]
    (is (= [] (:unsatisfied r)))
    (is (contains? (get-in r [:nodes :arbiter :established]) :before)
        "the arbiter can rely on assemble's :before on every path in")
    (is (= #{:max-turns} (get-in r [:nodes :route :ctx-requires])))
    (is (every? (fn [[_ n]] (set? (:established n))) (:nodes r))))
  (doseq [n manifests/shipped-manifests
          :when (io/resource (manifests/manifest-resource n))]
    (testing n
      (is (= [] (:unsatisfied (manifests/preconditions (shipped-definition n))))))))

(deftest an-unsatisfied-precondition-names-the-node-the-key-and-the-path
  (cells/load-cells!)
  (let [broken '{:cells {:start :llm/infer :arbiter :gate/arbiter}
                 :edges {:start :arbiter :arbiter :end}}
        r (manifests/preconditions broken)
        u (first (:unsatisfied r))]
    (is (= :data (:kind u)))
    (is (= :arbiter (:node u)))
    (is (contains? (:missing u) :before))
    (is (= [:start :arbiter] (:path u)))
    (testing "and the compile refusal quotes the same path"
      (let [e (try (manifests/compile-definition broken) nil
                   (catch Throwable t t))]
        (is (re-find #"\[:start :arbiter\]" (str (ex-message e))))))))

(deftest a-ctx-key-no-driver-provides-is-an-unsatisfied-precondition-too
  (cell/register-spec! :test/wants-ctx
                       {:id :test/wants-ctx :doc "x" :pure true
                        :requires [:no-such-ctx-key]
                        :handler (fn [_ d] d)
                        :schema {:input [:map] :output [:map]}})
  (try
    (let [r (manifests/preconditions '{:cells {:start :test/wants-ctx}
                                       :edges {:start :end}})
          u (first (:unsatisfied r))]
      (is (= :ctx (:kind u)))
      (is (= :start (:node u)))
      (is (= #{:no-such-ctx-key} (:missing u))))
    (finally (cell/remove-cell! :test/wants-ctx))))
