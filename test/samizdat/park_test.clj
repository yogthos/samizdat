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

(ns samizdat.park-test
  "What a parked workflow tells the supervisor.

  The test this namespace exists for is the size one. Every :mycelium/trace
  entry carries a snapshot of the whole data map, so a branch with a real
  message history repeated twenty times is what a naive brief would hand the
  model. The projection has to keep every step and drop every snapshot."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [mycelium.cell :as cell]
            [mycelium.core :as myc]
            [samizdat.cells :as cells]
            [samizdat.lexicon :as lexicon]
            [samizdat.manifests :as manifests]
            [samizdat.park :as park]))

(defn- big-branch
  "A branch the size a real one reaches: a message history that dwarfs
  everything else on the data map."
  []
  {:id "B1"
   :messages (vec (for [i (range 60)]
                    {:role "assistant"
                     :content (apply str (repeat 400 (str "turn " i " ")))}))})

(defn- parked-data
  "What run-compiled returns for a halted workflow — the shape manifests/
  on-error produces, with a trace whose entries each carry the whole map."
  []
  (let [b (big-branch)
        at (fn [m] (merge {:branch b :turn 3} m))]
    ;; The real shape of this failure: :llm/parse returns `data` UNTOUCHED on
    ;; the provider-error route, so :parsed is absent rather than nil — and
    ;; :tool/dispatch requires it. A key present with a nil value would pass
    ;; malli's [:parsed :any] and never get here, which is why "added nothing"
    ;; and "added the key as nil" have to stay different answers.
    (merge
     (at {:call {:ok true}})
     {:mycelium/halt true
      :mycelium/resume :mycelium.workflow/dispatch
      :mycelium/schema-error {:cell-id :tool/dispatch
                              :phase :input
                              :message "Schema input validation failed at :tool/dispatch"
                              :key-diff {:missing #{:parsed} :extra #{}}}
      :mycelium/trace
      [{:cell :start :cell-id :loop/assemble :transition nil
        :data (at {})}
       {:cell :infer :cell-id :llm/infer :transition nil
        :data (at {:call {:ok true}})}
       {:cell :parse :cell-id :llm/parse :transition :tool
        :data (at {:call {:ok true}})}
       {:cell :dispatch :cell-id :tool/dispatch :transition nil
        :data (at {:call {:ok true}})
        :error {:cell-id :tool/dispatch}}]})))

(deftest a-parked-workflow-knows-where-to-re-enter
  (let [d (parked-data)]
    (is (park/parked? d))
    (is (= :mycelium.workflow/dispatch (park/resume-state d))
        "re-entry is the cell that FAILED, so a fix is retried not skipped")
    (is (not (park/parked? {:branch {}})) "an ordinary data map is not parked")))

(deftest the-failure-names-the-fix-rather-than-the-symptom
  (let [f (park/failure (parked-data))]
    (is (= :schema/input (:type f)))
    (is (= :tool/dispatch (:cell f)))
    (testing "the key-diff, which is the part that says what to change"
      (is (= [:parsed] (:missing f)))
      (is (= [] (:extra f))))))

(deftest the-path-is-the-route-taken-with-what-each-step-added
  (let [p (park/path (parked-data))]
    (is (= 4 (count p)) "every cell that ran is a step")
    (is (= [:start :infer :parse :dispatch] (mapv :node p)))
    (testing "each step reports what it ADDED, not everything available"
      (is (= [:branch :turn] (:adds (first p)))
          "the first step establishes the map")
      (is (= [:call] (:adds (second p)))
          ":llm/infer adds exactly :call — not :branch and :turn again"))
    (testing "a step that produced nothing says so, which is where answers are"
      (is (= [] (:adds (nth p 2)))
          ":llm/parse added nothing, so the tool route carried no :parsed"))
    (testing "and the failing step is marked"
      (is (:failed (last p)))
      (is (not-any? :failed (butlast p))))))

(deftest the-brief-carries-shapes-and-never-values
  ;; THE POINT OF THE PROJECTION. A brief that inlined the trace would hand
  ;; the model four copies of a 60-message branch to explain one missing key.
  (let [d (parked-data)
        b (park/brief {:workflow "loop" :version 3 :run-id "R1"
                       :branch-id "B1" :data d})
        rendered (pr-str b)]
    (is (= "loop" (:workflow b)))
    (is (= :mycelium.workflow/dispatch (:resume-state b)))
    (is (seq (:path b)))
    (testing "no message history reaches the supervisor"
      (is (not (str/includes? rendered "turn 0 turn 0"))
          "a branch's message content leaked into the brief"))
    (testing "and the brief stays small enough to read"
      ;; The trace alone is ~4 x 60 x 400 chars. Anything near that is the
      ;; failure this projection exists to prevent.
      (is (< (count rendered) 4000)
          (str "the brief is " (count rendered) " chars — the projection is "
               "not projecting")))
    (testing "what it does carry is the keys, which name their producer"
      (is (contains? (set (:available b)) :branch))
      (is (contains? (set (:available b)) :call)))))

(deftest what-is-stored-drops-the-trace
  ;; Each entry is a copy of the data map, so storing the trace makes the
  ;; parked row quadratic in the turn count for no reader — path has already
  ;; reduced it to the deltas worth keeping.
  (let [s (park/strip (parked-data))]
    (is (nil? (:mycelium/trace s)))
    (is (= :mycelium.workflow/dispatch (:mycelium/resume s))
        "but it stays resumable")
    (is (some? (:branch s)) "and keeps the state the resume re-enters with")
    (testing "the diagnosis goes too, or every successful resume reports failed"
      ;; resume-compiled clears :halt and :resume and nothing else, so an error
      ;; left here survives the whole resumed run and workflow-error finds it
      ;; at the end.
      (is (nil? (:mycelium/schema-error s)))
      (is (nil? (:mycelium/error s))))
    (is (< (count (pr-str s)) (/ (count (pr-str (parked-data))) 3))
        "stripping the trace is most of the size")))

(deftest the-rendered-path-reads-as-a-route
  (let [lines (str/split-lines (park/render-path (park/path (parked-data))))]
    (is (= 4 (count lines)))
    (is (str/includes? (first lines) "start"))
    (is (str/includes? (first lines) "loop/assemble"))
    (is (str/includes? (nth lines 2) "-tool->")
        "the transition taken is on the line, so the route is readable")
    (is (str/includes? (nth lines 2) "added nothing"))
    (is (str/starts-with? (last lines) "  ✗")
        "the failing step is marked in the rendering too")))

;; --- the round trip: park, fix, resume ---------------------------------------

(deftest a-failed-workflow-parks-and-resumes-after-the-fix
  ;; THE WHOLE POINT, end to end. Before this a schema violation killed the
  ;; branch and the supervisor could only edit a cell for the NEXT run; for a
  ;; mismatch in a shared cell every branch died within seconds and the run was
  ;; over before the supervisor's 120s poll. Parking removes the clock: the
  ;; stopped state is a value, so nothing races it.
  (cells/load-cells!)
  (let [;; A cell that fails its own output schema, then stops failing — the
        ;; supervisor's fix, in the smallest form that can be tested.
        fixed? (atom false)]
    (cell/defcell :park-test/flaky
      {:doc "Declares :y. Writes it only once someone has fixed it."
       :pure true
       :input  [:map [:x :int]]
       :output [:map [:y :int]]}
      (fn [_ d] (if @fixed? (assoc d :y 1) (assoc d :z 1))))
    (cell/defcell :park-test/after
      {:doc "Downstream of the flaky cell; needs what it promised."
       :pure true
       :input  [:map [:y :int]]
       :output [:map [:done :boolean]]}
      (fn [_ d] (assoc d :done true)))
    (try
      (with-redefs [lexicon/policy (fn [_] {:mode :strict})]
        (let [wf (manifests/compile-definition
                  '{:cells {:start :park-test/flaky :after :park-test/after}
                    :edges {:start :after :after :end}})
              parked (myc/run-compiled wf {} {:x 1})]

          (testing "the violation parks rather than ending the run"
            (is (park/parked? parked))
            (is (myc/error? parked) "and still reports as an error")
            (is (= :park-test/flaky (:cell (park/failure parked)))
                "naming the cell to fix"))

          (testing "the brief says what to change"
            (let [b (park/brief {:workflow "t" :run-id "R1" :branch-id "B1"
                                 :data parked})]
              (is (= [:y] (:missing (:failure b))))
              (is (seq (:path b)))))

          (testing "and after the fix it resumes from where it stopped"
            (reset! fixed? true)
            (let [out (myc/resume-compiled wf {} (park/strip parked))]
              (is (not (myc/error? out)) (str "resume failed: " (pr-str out)))
              (is (:done out)
                  "the run finished, having re-entered at the failed cell")
              (is (= 1 (:y out)) "with the fixed cell's output")
              (is (= 1 (:x out))
                  "and the state it had when it stopped, not a fresh map")))))
      (finally
        (cell/remove-cell! :park-test/flaky)
        (cell/remove-cell! :park-test/after)))))
