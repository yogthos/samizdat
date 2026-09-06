;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.decompose-test
  "The pure decompose-on-stuck core: the architect prompt and decision parsing."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [samizdat.agent.decompose :as dec]))

(deftest architect-prompt-is-a-prompt-file
  ;; Tier 2c: the architect prompt moved from src prose to
  ;; resources/prompts/architect.md — runtime-editable, same seam as every
  ;; gate message. parse-decision is coupled to the JSON decision format,
  ;; so that format survives the move pinned.
  (let [file (slurp (io/resource "prompts/architect.md"))]
    (is (str/includes? file "{{problem}}"))
    (is (str/includes? file "DECOMPOSE"))
    (is (str/includes? file "FRESH_APPROACH"))
    (is (str/includes? file "\"decision\": \"decompose\" | \"fresh_approach\""))
    (is (str/includes? (dec/architect-prompt {:problem "the pin problem"} {})
                       "the pin problem"))))

(deftest architect-prompt-carries-the-evidence
  (let [p (dec/architect-prompt {:problem "gate the remember tool"
                                 :tests "(deftest ...)"}
                                {:attempts 3 :last-failure "AssertionError: expected refusal"
                                 :depth 0})]
    (is (str/includes? p "gate the remember tool"))
    (is (str/includes? p "AssertionError"))
    (is (str/includes? p "DECOMPOSE"))
    (is (str/includes? p "FRESH_APPROACH"))))

(deftest architect-prompt-forces-fresh-approach-at-the-depth-edge
  (let [p (dec/architect-prompt {:problem "x"} {:depth (dec (dec/max-depth))})]
    (is (str/includes? p "MUST choose FRESH_APPROACH"))))

(deftest parse-decision-reads-a-decompose
  (let [d (dec/parse-decision
           "{\"decision\":\"decompose\",\"reason\":\"two jobs\",\"subtasks\":[
              {\"name\":\"detect-completion\",\"description\":\"match the content against completion words\"},
              {\"name\":\"gate-on-diff\",\"description\":\"refuse when the tree changed nothing\"}]}"
           0)]
    (is (= :decompose (:kind d)))
    (is (= ["detect-completion" "gate-on-diff"] (mapv :name (:subtasks d))))
    (is (every? :description (:subtasks d)))))

(deftest parse-decision-reads-a-fresh-approach
  (let [d (dec/parse-decision
           "{\"decision\":\"fresh_approach\",\"reason\":\"wrong strategy\",\"hint\":\"regex, not substring\"}")]
    (is (= :fresh-approach (:kind d)))
    (is (= "regex, not substring" (:hint d)))))

(deftest parse-decision-honours-the-depth-budget
  (testing "a decompose too deep degrades to a fresh approach, not a split"
    (let [d (dec/parse-decision
             "{\"decision\":\"decompose\",\"subtasks\":[{\"name\":\"a\",\"description\":\"x\"}]}"
             (dec (dec/max-depth)))]
      (is (= :fresh-approach (:kind d)) "no split at the depth edge"))))

(deftest parse-decision-degrades-a-subtaskless-decompose
  (let [d (dec/parse-decision "{\"decision\":\"decompose\",\"subtasks\":[]}" 0)]
    (is (= :fresh-approach (:kind d)))))

(deftest parse-decision-is-nil-on-junk
  (is (nil? (dec/parse-decision "no json here" 0)))
  (is (nil? (dec/parse-decision "" 0))))

(deftest parse-decision-ignores-prose-around-the-json
  (let [d (dec/parse-decision
           "Here is my call:\n{\"decision\":\"fresh_approach\",\"hint\":\"try recursion\"}\nDone.")]
    (is (= :fresh-approach (:kind d)))
    (is (= "try recursion" (:hint d)))))

;; --- the recursion: solve (decompose-on-stuck) over injected ops ---

(def ^:private seq-fan (fn [thunks] (mapv #(%) thunks)))

(deftest solve-lands-a-unit-that-passes-directly
  (let [r (dec/solve {:id "x" :problem "p"} 0
                     {:attempt (constantly {:passed? true :answer "done"})
                      :recover (fn [& _] (throw (ex-info "should not recover a passing unit" {})))
                      :fan seq-fan})]
    (is (= :landed (:status r)))
    (is (= "done" (:answer r)))))

(deftest solve-retries-once-with-a-fresh-approach-hint
  (let [r (dec/solve {:id "x" :problem "p"} 0
                     {:attempt (fn [node] (if (:hint node)
                                            {:passed? true :answer "hinted"}
                                            {:passed? false :failure "wrong strategy"}))
                      :recover (fn [& _] {:kind :fresh-approach :hint "try recursion"})
                      :fan seq-fan})]
    (is (= :landed (:status r)))
    (is (= "hinted" (:answer r)))))

(deftest solve-decomposes-a-stuck-unit-and-assembles-the-children
  (let [attempts (atom [])
        r (dec/solve {:id "root" :problem "big task"} 0
                     {:attempt (fn [node]
                                 (swap! attempts conj (:id node))
                                 (cond
                                   (:assembly node) {:passed? true :answer (str "assembled " (:id node))}
                                   (:parent node)   {:passed? true :answer (str "built " (:id node))}
                                   :else            {:passed? false :failure "too big"}))
                      :recover (fn [& _] {:kind :decompose
                                          :subtasks [{:name "a" :description "do a"}
                                                     {:name "b" :description "do b"}]})
                      :fan seq-fan})]
    (is (= :landed (:status r)) "the parent lands once its children + assembly pass")
    (is (= 2 (count (:children r))))
    (is (every? #(= :landed (:status %)) (:children r)))
    (is (contains? (set @attempts) "root/a") "each sub-unit was attempted")
    (is (contains? (set @attempts) "root/b"))))

(deftest solve-follows-a-split-the-agent-made-itself
  ;; UNIFORM RECURSION (karamazov-ioo.15). The agent working the task is the
  ;; agent that decides to break it up: it writes the stubs, calls `split`, and
  ;; the harness verifies and opens the child tasks. There is no separate
  ;; architect deciding from outside, and no stuck-detection involved — this
  ;; unit was never stuck, it was DELEGATED on the first attempt.
  (let [seen (atom [])
        r (dec/solve {:id "root" :problem "big task"} 0
                     {:attempt (fn [node]
                                 (swap! seen conj (:id node))
                                 (cond
                                   (:assembly node) {:passed? true :answer "assembled"}
                                   (= "root" (:id node))
                                   {:split [{:id "root/a" :problem "do a" :task-id "T1"}
                                            {:id "root/b" :problem "do b" :task-id "T2"}]}
                                   :else {:passed? true :answer (str "built " (:id node))}))
                      :recover (fn [& _]
                                 (throw (ex-info "a unit that split was never stuck" {})))
                      :fan seq-fan})]
    (is (= :landed (:status r)))
    (is (= ["root/a" "root/b"] (mapv #(get-in % [:node :id]) (:children r))))
    (is (= #{"root" "root/a" "root/b"} (set @seen))
        "the children were attempted, and the root twice — once to split, once to assemble")))

(deftest a-split-that-lost-a-child-fails-rather-than-assembling-over-it
  (let [r (dec/solve {:id "root" :problem "big"} 0
                     {:attempt (fn [node]
                                 (cond
                                   (:assembly node)
                                   (throw (ex-info "must not assemble over a failed piece" {}))
                                   (= "root" (:id node))
                                   {:split [{:id "root/a" :problem "do a"}]}
                                   :else {:passed? false :failure "could not build it"}))
                      :recover (constantly nil)
                      :fan seq-fan})]
    (is (= :failed (:status r)))))

(deftest solve-fails-hard-at-the-depth-budget
  (let [r (dec/solve {:id "x" :problem "p"} (dec/max-depth)
                     {:attempt (constantly {:passed? false :failure "nope"})
                      :recover (fn [& _] {:kind :decompose :subtasks [{:name "a" :description "x"}]})
                      :fan seq-fan})]
    (is (= :failed (:status r)))
    (is (str/includes? (:reason r) "depth"))))

;; --- the fix (karamazov-dvz): a stuck unit is never abandoned while it can
;; still be split. Fresh-approach is a first, cheap try; when it fails, the unit
;; is DECOMPOSED (forced), recursively, until pieces land or the floor is hit. ---

(deftest solve-splits-a-leaf-when-fresh-approach-fails
  ;; the exact bug the user caught: architect calls the leaf "one thing" ->
  ;; fresh-approach -> the retry fails. The old code abandoned here. It must now
  ;; escalate to a split of that same leaf.
  (let [ids (atom [])
        recover (fn [_node ev]
                  (if (:fresh-failed ev)
                    ;; told the fresh angle failed -> split it now
                    {:kind :decompose :subtasks [{:name "p1" :description "part 1"}
                                                 {:name "p2" :description "part 2"}]}
                    {:kind :fresh-approach :hint "try X"}))
        attempt (fn [node]
                  (swap! ids conj (:id node))
                  (cond
                    (re-find #"/p[12]$" (:id node)) {:passed? true :answer (str "built " (:id node))}
                    (:assembly node) {:passed? true :answer "assembled"}
                    :else {:passed? false :failure "one thing but keeps missing"}))
        r (dec/solve {:id "leaf" :problem "p"} 0
                     {:attempt attempt :recover recover :fan seq-fan})]
    (is (= :landed (:status r)) "the leaf lands by splitting after fresh-approach failed")
    (is (some #{"leaf/p1"} @ids) "the failing leaf was decomposed further")
    (is (some #{"leaf/p2"} @ids))))

(deftest solve-generic-splits-when-architect-refuses-to-help
  ;; the architect keeps saying fresh-approach even when told to split (or returns
  ;; nothing usable). The system must STILL go smaller — a generic split — rather
  ;; than abandon.
  (let [ids (atom [])
        attempt (fn [node]
                  (swap! ids conj (:id node))
                  (cond
                    (:assembly node) {:passed? true :answer "asm"}
                    (str/includes? (:id node) "/") {:passed? true :answer "sub"} ; any child lands
                    :else {:passed? false :failure "stuck"}))
        r (dec/solve {:id "u" :problem "p"} 0
                     {:attempt attempt :recover (constantly nil) :fan seq-fan})]
    (is (= :landed (:status r)) "generic split lands the unit even with no architect help")
    (is (> (count @ids) 1) "it fell back to a generic split instead of abandoning")))

(deftest solve-recurses-splitting-until-pieces-land
  ;; multi-level: root splits into a,b; b is itself still stuck and splits again
  ;; into b1,b2 which land. Proves the recursion goes as deep as it needs to.
  (let [attempt (fn [node]
                  (let [id (:id node)]
                    (cond
                      (:assembly node) {:passed? true :answer (str "asm " id)}
                      (#{"root/a" "root/b/b1" "root/b/b2"} id) {:passed? true :answer (str "built " id)}
                      :else {:passed? false :failure "still too big"})))
        recover (fn [node _]
                  {:kind :decompose
                   :subtasks (if (= "root/b" (:id node))
                               [{:name "b1" :description "x"} {:name "b2" :description "y"}]
                               [{:name "a" :description "x"} {:name "b" :description "y"}])})
        r (dec/solve {:id "root" :problem "p"} 0
                     {:attempt attempt :recover recover :fan seq-fan})]
    (is (= :landed (:status r)))
    (let [b (first (filter #(= "root/b" (get-in % [:node :id])) (:children r)))]
      (is (= :landed (:status b)) "the stuck child landed")
      (is (= 2 (count (:children b))) "the stuck child was itself decomposed further"))))

(deftest solve-eventually-fails-honestly-when-nothing-lands
  ;; when even splitting to the floor doesn't land a single piece, the run fails
  ;; honestly — but only after it exhausted splitting, not on the first miss.
  ;; :max-depth 1 bounds the recursion for the test.
  (let [ids (atom [])
        r (dec/solve {:id "root" :problem "p"} 0
                     {:attempt (fn [n] (swap! ids conj (:id n)) {:passed? false :failure "stuck"})
                      :recover (fn [_ _] {:kind :decompose :subtasks [{:name "a" :description "x"}]})
                      :fan seq-fan
                      :max-depth 1})]
    (is (= :failed (:status r)))
    (is (> (count @ids) 1) "it split before giving up")))

(deftest the-architect-is-told-how-many-times-this-was-really-attempted
  ;; v21: the count comes off the task row, so a resumed run diagnoses a unit
  ;; on its fourth try as one rather than starting the tally over.
  (let [seen (atom nil)]
    (dec/solve {:id "x" :problem "p"} 0
               {:attempt (constantly {:passed? false :failure "no" :attempts 4})
                :recover (fn [_ ev] (reset! seen (:attempts ev)) nil)
                :fan seq-fan})
    (is (= 4 @seen) "the architect sees the durable count, not a fresh one")))

(deftest a-child-row-hangs-off-its-parents-row-on-the-fallback-path-too
  ;; Run 3b3ce405: the split path nested its task rows properly and the
  ;; architect path left every unit an orphan, so one run recorded its work two
  ;; different ways depending on which path produced a unit. The attempt is
  ;; what learns a unit's task id, so it has to travel back to the node before
  ;; the children are built from it.
  (let [seen (atom [])]
    (dec/solve {:id "root" :problem "big"} 0
               {:attempt (fn [node]
                           (swap! seen conj [(:id node) (:parent-task node)])
                           (if (= "root" (:id node))
                             {:passed? false :failure "too big" :task-id "sz-root"}
                             {:passed? true :answer "built" :task-id "sz-kid"}))
                :recover (fn [& _] {:kind :decompose
                                    :subtasks [{:name "a" :description "do a"}]})
                :fan seq-fan})
    (is (= ["root" "root/a" "root"] (mapv first @seen))
        "root, then its piece, then root again as the assembly")
    (is (= [nil "sz-root" nil] (mapv second @seen))
        "the child is told which row to hang off; the root has none to hang off")))
