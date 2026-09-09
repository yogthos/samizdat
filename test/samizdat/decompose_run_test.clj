;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.decompose-run-test
  "The decompose-on-stuck loop end to end: a stuck root is split, the sub-units
  land, and the parent assembles — driven by a role-dispatching mock."
  (:require [babashka.fs :as fs]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [ebb.core :as ebb]
            [mycelium.core :as myc]
            [samizdat.agent.decompose :as dec]
            [samizdat.agent.gitdiff :as gitdiff]
            [samizdat.agent.tools.base :as base]
            [samizdat.agent.tools.split]
            [samizdat.agent.state :as state]
            [samizdat.cells :as cells]
            [samizdat.llm.client :as llm]
            [samizdat.store.db :as db]
            [samizdat.store.journal :as journal]
            [samizdat.store.runs :as runs]
            [samizdat.store.tasks :as tasks]
            [samizdat.workflow :as workflow]))

(defn- cell-fn
  "A var out of the loaded cell namespace. find-ns first: ns-resolve THROWS on
  an absent namespace rather than returning nil."
  [sym]
  (when-not (find-ns 'cells.decompose) (cells/load-cells!))
  (or (ns-resolve 'cells.decompose sym)
      (throw (ex-info (str "cells.decompose/" sym " did not load") {}))))

(defn- done-call [answer]
  {:content (str "```tool-call\n{\"name\":\"done\",\"args\":{\"answer\":\"" answer "\"}}\n```")
   :finish-reason "stop"})

(defn- roles
  "Architect decomposes once; the root's first attempt gives up (stuck); the
  sub-units and the assembly ship a done."
  [_ _ messages & _]
  (let [c (str/join " " (map :content messages))]
    (cond
      (str/includes? c "architect diagnosing")
      {:content (str "{\"decision\":\"decompose\",\"reason\":\"two jobs\","
                     "\"subtasks\":[{\"name\":\"part-a\",\"description\":\"do part a\"},"
                     "{\"name\":\"part-b\",\"description\":\"do part b\"}]}")
       :finish-reason "stop"}

      ;; done answers must engage their unit's problem or the done-gate refuses
      ;; them (which would make the unit loop and look stuck).
      (str/includes? c "ASSEMBLY step") (done-call "assembled the big feature from its parts")
      (str/includes? c "do part a")     (done-call "handled part a")
      (str/includes? c "do part b")     (done-call "handled part b")

      :else                       ; root's first direct attempt — get stuck
      {:content "```tool-call\n{\"name\":\"give_up\",\"args\":{\"reason\":\"too big to do at once\"}}\n```"
       :finish-reason "stop"})))

(deftest decompose-splits-a-stuck-task-lands-the-pieces-and-assembles
  (with-redefs [llm/chat roles
                gitdiff/baseline (constantly "HEAD")
                ;; every attempt "changed files" so passed? tracks the worker's
                ;; verdict; the root give_up still fails (not done).
                gitdiff/changed-files (constantly ["src/piece.clj"])]
    (let [conn (db/open! ":memory:")
          r (workflow/run! {:conn conn :config {:run {:loop "decompose"}}
                            :llm-adapter :a :llm-config {:max-tokens 16384}
                            :problem "the big feature" :max-turns 6})]
      (is (= :completed (:status r)) "the root lands once its pieces + assembly pass")
      (testing "the answer records the decompose tree with landed pieces"
        (is (str/includes? (:answer r) "landed"))
        (is (str/includes? (:answer r) "T/part-a"))
        (is (str/includes? (:answer r) "T/part-b")))
      (testing "each unit ran on its own branch: root, both sub-units, the assembly"
        (let [b (set (map :branch_id (db/fetch conn ["SELECT DISTINCT branch_id FROM turns"])))]
          (is (contains? b "DT") "root direct attempt")
          (is (contains? b "DT_part_a"))
          (is (contains? b "DT_part_b"))
          (is (contains? b "DT-a") "the assembly attempt")))
      (testing "each unit's row records the attempt framing it opened on (v24)"
        (let [suffix #(str (:prompt_suffix (db/fetch-one conn ["SELECT prompt_suffix FROM branches WHERE id = ?" %])))]
          (is (str/includes? (suffix "DT-a") (workflow/prompt-text "assembly"))
              "the assembly attempt opened on the assembly prompt")
          (is (not (str/includes? (suffix "DT_part_a") (workflow/prompt-text "assembly")))
              "a sub-unit did not")
          (is (str/includes? (suffix "DT_part_a") (workflow/prompt-text "roles/implementor"))
              "it opened as an implementor"))))))

(defn- escalating-roles
  "Architect first calls the stuck unit 'one thing' (fresh-approach); the hinted
  retry still fails; on the SECOND diagnosis — which carries the 'already tried
  and also failed' evidence — it splits. Proves the cell wires force-split
  evidence through so a fresh-approach dead-end escalates to a real split
  (karamazov-dvz) rather than abandoning."
  [_ _ messages & _]
  (let [c (str/join " " (map :content messages))]
    (cond
      (and (str/includes? c "architect diagnosing")
           (str/includes? c "already tried and also failed"))
      {:content (str "{\"decision\":\"decompose\",\"reason\":\"split it\","
                     "\"subtasks\":[{\"name\":\"part-a\",\"description\":\"do part a\"},"
                     "{\"name\":\"part-b\",\"description\":\"do part b\"}]}")
       :finish-reason "stop"}

      (str/includes? c "architect diagnosing")
      {:content "{\"decision\":\"fresh_approach\",\"reason\":\"one thing\",\"hint\":\"try a different tactic\"}"
       :finish-reason "stop"}

      (str/includes? c "ASSEMBLY step") (done-call "assembled the big feature from its parts")
      (str/includes? c "do part a")     (done-call "handled part a")
      (str/includes? c "do part b")     (done-call "handled part b")

      :else                       ; the root's direct attempt AND its hinted retry both get stuck
      {:content "```tool-call\n{\"name\":\"give_up\",\"args\":{\"reason\":\"still stuck\"}}\n```"
       :finish-reason "stop"})))

(deftest fresh-approach-dead-end-escalates-to-a-real-split
  (with-redefs [llm/chat escalating-roles
                gitdiff/baseline (constantly "HEAD")
                gitdiff/changed-files (constantly ["src/piece.clj"])]
    (let [conn (db/open! ":memory:")
          r (workflow/run! {:conn conn :config {:run {:loop "decompose"}}
                            :llm-adapter :a :llm-config {:max-tokens 16384}
                            :problem "the big feature" :max-turns 6})]
      (is (= :completed (:status r)) "the unit lands by splitting after the fresh-approach retry failed")
      (let [b (set (map :branch_id (db/fetch conn ["SELECT DISTINCT branch_id FROM turns"])))]
        (is (contains? b "DT") "root direct attempt")
        (is (contains? b "DT-h") "the fresh-approach hinted retry ran")
        (is (contains? b "DT_part_a") "then it was split — sub-unit a")
        (is (contains? b "DT_part_b") "sub-unit b")
        (is (contains? b "DT-a") "and assembled")))))

(deftest decompose-reports-its-round-in-the-shared-outcome-vocabulary
  ;; karamazov-u5uy. The three implement strategies must report a round's
  ;; outcome in ONE vocabulary or the supervisor cannot read the round it is
  ;; supervising. decompose used to report only :verdict and :branch, so on
  ;; that strategy the digest counted nothing; now every unit of the tree is
  ;; an entry in the :implement-round note, keyed the way the fan-out keys
  ;; its workers.
  (with-redefs [llm/chat roles
                gitdiff/baseline (constantly "HEAD")
                gitdiff/changed-files (constantly ["src/piece.clj"])]
    (let [conn (db/open! ":memory:")
          r (workflow/run! {:conn conn :config {:run {:loop "decompose"}}
                            :llm-adapter :a :llm-config {:max-tokens 16384}
                            :problem "the big feature" :max-turns 6})
          note (journal/last-note conn (:run-id r) :implement-round)
          results (:results note)]
      (is (= "decompose" (:strategy note)))
      (testing "every unit of the tree is an owner, not just the root"
        (is (<= 3 (count results)) "the root and its two sub-units at least")
        (is (contains? (set (map :subtask results)) "T/part-a"))
        (is (contains? (set (map :subtask results)) "T/part-b")))
      (testing "statuses are the fan-out's, so the digest counts them unchanged"
        (is (every? #{"done" "abandoned"} (map :status results)))
        (is (some #(= "done" (:status %)) results))))))

;; --- the fan runs on a fiber, and so must this test -------------------------
;;
;; Every test above drives the loop from the test thread, where a park is a
;; plain block and jolt asserts nothing. The live loop is an sp process on a
;; fiber, and there a park under a counted lock throws "a fiber cannot leave
;; the CPU while its carrier holds a counted lock". jolt's `mapv` is
;; (vec (apply map f colls)), so a park inside its function IS under one
;; (karamazov-p3jo) — and the fan's function is a whole sub-unit solve, which
;; parks at every provider call. base-test's no-park-inside-a-lazy-body
;; ratchet could not see it: the lazy body was `#(%)`, which names no parking
;; call lexically. Same shape as beam/advance-all, same fix shape as its test.
;; A HANG here is the bug, not a slow test.

(deftest the-fan-runs-parking-sub-units-on-a-fiber
  (let [fan-out (cell-fn 'fan-out)]
    (is (= [:a :b :c]
           (ebb/? (ebb/sp (fan-out [#(do (ebb/? (ebb/sleep 10)) :a)
                                    #(do (ebb/? (ebb/sleep 10)) :b)
                                    #(do (ebb/? (ebb/sleep 10)) :c)]))))
        "each thunk ran and the answers came back in order")))

(deftest the-fan-of-nothing-is-nothing
  (let [fan-out (cell-fn 'fan-out)]
    (is (= [] (ebb/? (ebb/sp (fan-out [])))))))

(deftest every-attempt-of-a-unit-holds-that-units-task
  ;; karamazov-ioo.15.1. Each attempt of a unit runs on its OWN branch — DT for
  ;; the direct build, DT-h for the hinted retry, DT-a for the assembly — and
  ;; every one of them claims the SAME row. tasks/claim! is first-writer-wins
  ;; by branch, so the second and third attempts got nil back and opened
  ;; holding nothing: the per-turn block told the assembly "No task claimed"
  ;; (the nag that ate run 938b4eb8's turns), and ship.clj reads the contract
  ;; off tasks/held-by, so the one attempt that composes every piece was the
  ;; one judged with no contracted tests and no unfilled-stub check.
  (with-redefs [llm/chat roles
                gitdiff/baseline (constantly "HEAD")
                gitdiff/changed-files (constantly ["src/piece.clj"])]
    (let [conn (db/open! ":memory:")
          _ (workflow/run! {:conn conn :config {:run {:loop "decompose"}}
                            :llm-adapter :a :llm-config {:max-tokens 16384}
                            :problem "the big feature" :max-turns 6})
          rows (db/fetch conn ["SELECT title, parent_id, branch_id, status
                                  FROM tasks ORDER BY created_at"])
          root (first rows)]
      (is (= 3 (count rows)) "the root's row and one per piece")
      (testing "the assembly holds the root's task"
        (is (= "DT-a" (:branch_id root))
            "the claim followed the unit to the attempt that is working it")
        (is (= "the big feature" (:title root))))
      (testing "a landed unit's row is closed, so the board ends clean"
        (is (every? #(= "done" (:status %)) rows)
            (str "still open: " (pr-str (remove #(= "done" (:status %)) rows))))))))

(deftest a-hinted-retry-holds-the-task-too
  (with-redefs [llm/chat escalating-roles
                gitdiff/baseline (constantly "HEAD")
                gitdiff/changed-files (constantly ["src/piece.clj"])]
    (let [conn (db/open! ":memory:")
          _ (workflow/run! {:conn conn :config {:run {:loop "decompose"}}
                            :llm-adapter :a :llm-config {:max-tokens 16384}
                            :problem "the big feature" :max-turns 6})
          root (db/fetch-one conn ["SELECT branch_id, status, attempts FROM tasks
                                     WHERE parent_id IS NULL"])]
      ;; DT tried, DT-h retried on the hint, DT-a assembled: three attempts on
      ;; one row, and the count is the row's rather than a counter in process.
      (is (= 3 (:attempts root)))
      (is (= "DT-a" (:branch_id root)))
      (is (= "done" (:status root))))))

(deftest an-assembly-does-not-mistake-its-own-pieces-for-a-fresh-split
  ;; THE ORDINARY PATH COULD NOT LAND. attempt-node asks "did this unit
  ;; delegate?" by walking the task's stub-bearing children — and it asked it
  ;; on EVERY attempt, including the assembly. So on the agent's own split
  ;; (the path the whole design is about, as against the architect fallback,
  ;; whose children carry no stubs and are filtered out) the assembly attempt
  ;; re-read the very pieces it was composing, reported a split instead of a
  ;; result, and `assemble` read the missing :passed? as a failure. Every
  ;; agent-made split therefore ended "assembly did not land", whatever the
  ;; pieces did. The pure recursion's tests could not see it: they mock
  ;; `attempt` and never reach this walk.
  (let [attempt-node (cell-fn 'attempt-node)]
    (with-redefs [myc/run-compiled (fn [_ _ _]
                                     {:verdict :done
                                      :branch {:final-answer "composed the report"}})
                  gitdiff/baseline (constantly "HEAD")
                  gitdiff/changed-files (constantly ["src/example/core.clj"])]
      (let [conn (db/open! ":memory:")
            run-id (runs/start-run! conn {:problem "build the report"
                                          :max-turns 10 :beam-width 1})
            parent (tasks/create! conn {:title "build the report" :body "build the report"
                                        :run-id run-id})
            piece (tasks/create! conn {:title "parse-line" :body "Parse one line into a map."
                                       :parent-id parent :run-id run-id
                                       :contract "(defn parse-line [line] ...)"
                                       :tests "test/example/core_test.clj"
                                       :stub-file "src/example/core.clj"
                                       :stubs ["parse-line"]})
            ctx {:conn conn :run-id run-id :root "."}
            node {:id "T" :problem "build the report" :task-id parent}
            direct (attempt-node ctx nil node)
            asm (attempt-node ctx nil (assoc node :assembly true
                                             :child-answers ["parsed one line"]
                                             :assembled #{piece}))]
        (is (= [piece] (mapv :task-id (:split direct)))
            "a plain attempt on a unit that split finds its pieces")
        (is (empty? (:split asm))
            "the assembly is composing those pieces, not discovering them")
        (is (true? (:passed? asm)) "so it can actually land")))))

(deftest the-assembly-wakes-the-parked-branch-instead-of-replacing-it
  ;; karamazov-ioo.15.4. A unit that delegates PARKS: `split` leaves the branch
  ;; :parked and blocks the row it holds. The wait used to be incidental — the
  ;; branch kept its turns and spent them, then a stranger with a fresh tape
  ;; and a fresh budget was opened under the name DT-a to assemble pieces it
  ;; had never designed. Now the parked branch itself comes back: same id, same
  ;; tape, same turn counter, with the news appended as a user turn.
  (let [attempt-node (cell-fn 'attempt-node)
        seen (atom nil)]
    (with-redefs [myc/run-compiled (fn [_ _ data]
                                     (reset! seen data)
                                     {:verdict :done
                                      :turn (:turn data)
                                      :branch (assoc (:branch data)
                                                     :status :done
                                                     :final-answer "composed the report")})
                  gitdiff/baseline (constantly "HEAD")
                  gitdiff/changed-files (constantly ["src/example/core.clj"])]
      (let [conn (db/open! ":memory:")
            run-id (runs/start-run! conn {:problem "build the report"
                                          :max-turns 25 :beam-width 1})
            parent (tasks/create! conn {:title "build the report" :body "build the report"
                                        :run-id run-id})
            piece (tasks/create! conn {:title "parse-line" :body "Parse one line into a map."
                                       :parent-id parent :run-id run-id
                                       :stub-file "src/example/core.clj"
                                       :stubs ["parse-line"]})
            _ (tasks/claim! conn parent run-id "DT")
            ;; where split left it: parked, holding a blocked row
            _ (tasks/update! conn parent {:status "blocked"})
            parked (-> (state/new-branch {:id "DT" :problem "build the report"
                                          :messages [{:role "user"
                                                      :content "the stubs I wrote"}]})
                       (assoc :status :parked :delegated [piece]
                              :inactive-reason "delegated 1 pieces"))
            asm (attempt-node {:conn conn :run-id run-id :root "."} nil
                              {:id "T" :problem "build the report" :task-id parent
                               :assembly true :assembled #{piece}
                               :child-answers ["parsed one line into a map"]
                               :resume {:branch parked :turn 7}})
            woken (:branch @seen)
            said (map #(str (:content %)) (:messages woken))]
        (testing "the same agent, woken"
          (is (= "DT" (:id woken)) "not a DT-a stranger")
          (is (= 7 (:turn @seen)) "picking its turn counter up where it parked")
          (is (= :active (:status woken)) "and taking turns again")
          (is (nil? (:delegated woken)) "with the delegation it was waiting on cleared"))
        (testing "on its own tape, told the news"
          (is (some #(str/includes? % "the stubs I wrote") said)
              "what it did before it parked is still there")
          (is (some #(str/includes? % "ASSEMBLY step") said))
          (is (some #(str/includes? % "parsed one line into a map") said)
              "with what each piece delivered"))
        (testing "the row it was parked on is worked again, then closed"
          (is (true? (:passed? asm)))
          (is (= "done" (:status (tasks/get-task conn parent)))))))))

;; --- the agent's own split, end to end --------------------------------------
;;
;; The path the whole design is about, and the one nothing drove: every test
;; above splits through the ARCHITECT fallback, whose children are described
;; rather than stubbed. That is how the assembly re-reading its own pieces
;; (karamazov-ioo.15.3) survived a green suite — the fallback's children carry
;; no stubs, so the walk that broke the agent path never fired on it.
;;
;; Real tree, real `split` tool, real attempt-node and fan. Only the turn loop
;; is stood in for: the stub plays one turn per branch, which is enough to
;; exercise the delegation and is not what has ever broken here.

(def ^:private stubbed
  "(ns example.core)

(defn parse-line
  \"Parse one line into {:key :value}.\"
  [line]
  (throw (ex-info \"not implemented\" {})))

(defn render-report
  \"Render parsed lines as a report string.\"
  [rows]
  (throw (ex-info \"not implemented\" {})))

(defn run
  \"The composition: parse every line, then render.\"
  [lines]
  (render-report (map parse-line lines)))
")

(def ^:private sketched-tests
  "(ns example.core-test
  (:require [clojure.test :refer [deftest is]]))

(deftest parse-line-splits-on-the-first-colon
  (is (= {:key \"a\"} (example.core/parse-line \"a:b\"))))
")

(def ^:private good-parts
  [{"name" "parse-line" "description" "Parse one line into a map."
    "file" "src/example/core.clj" "stubs" ["parse-line"]
    "tests" "test/example/core_test.clj"}
   {"name" "render-report" "description" "Render the rows as a report."
    "file" "src/example/core.clj" "stubs" ["render-report"]
    "tests" "test/example/core_test.clj"}])

(deftest an-agent-split-parks-builds-its-pieces-and-wakes-to-assemble
  (let [attempt-node (cell-fn 'attempt-node)
        fan-out (cell-fn 'fan-out)
        root-dir (str (fs/create-temp-dir {:prefix "decompose-split"}))
        opened (atom [])]
    (try
      (fs/create-dirs (fs/path root-dir "src" "example"))
      (fs/create-dirs (fs/path root-dir "test" "example"))
      (spit (io/file root-dir "src/example/core.clj") stubbed)
      (spit (io/file root-dir "test/example/core_test.clj") sketched-tests)
      (with-redefs [gitdiff/baseline (constantly "HEAD")
                    gitdiff/changed-files (constantly ["src/example/core.clj"])
                    myc/run-compiled
                    (fn [_ ictx {:keys [branch turn]}]
                      (swap! opened conj {:id (:id branch) :turn turn})
                      (let [said (str/join " " (map :content (:messages branch)))]
                        (cond
                          ;; the woken parent: compose what came back and ship
                          (str/includes? said "ASSEMBLY step")
                          {:verdict :done :turn turn
                           :branch (assoc branch :status :done
                                          :final-answer "composed the report")}

                          ;; the root's first turn: hand the two pieces down
                          (= "DT" (:id branch))
                          (let [r (base/run-tool
                                   (assoc ictx :branch branch :tool-name "split"
                                          :args {:reason "two responsibilities"
                                                 :parts good-parts}))]
                            {:verdict :abandoned :turn (inc turn) :branch (:branch r)})

                          ;; a piece: build it
                          :else
                          {:verdict :done :turn turn
                           :branch (assoc branch :status :done
                                          :final-answer (str "built " (:id branch)))})))]
        (let [conn (db/open! ":memory:")
              run-id (runs/start-run! conn {:problem "build the report"
                                            :max-turns 25 :beam-width 1})
              ctx {:conn conn :run-id run-id :root root-dir}
              result (dec/solve {:id "T" :problem "build the report"} 0
                                {:attempt #(attempt-node ctx nil %)
                                 :recover (fn [_ _] nil)
                                 :fan fan-out})
              rows (db/fetch conn ["SELECT title, parent_id, branch_id, status
                                     FROM tasks ORDER BY created_at"])
              parent (first rows)]
          (is (= :landed (:status result))
              (str "the tree landed: " (pr-str (dissoc result :children))))
          (testing "the parent parked and was woken, rather than replaced"
            ;; The parent's two openings are ordered; the two PIECES between
            ;; them are not — nothing sequences siblings of one split against
            ;; each other, and asserting a sequence the scheduler does not
            ;; promise fails whenever it picks the second piece first
            ;; (karamazov-x7ji).
            (let [ids (map :id @opened)]
              (is (= ["DT" "DT"] [(first ids) (last ids)])
                  "the same branch id opens twice, at both ends")
              (is (= #{"DT_parse_line" "DT_render_report"} (set (butlast (rest ids))))
                  "and the pieces between are the two pieces, no DT-a stranger")
              (is (= 4 (count ids)) "four openings, not five"))
            (is (= [1 1 1 2] (map :turn @opened))
                "and it picks its turn counter up where it parked"))
          (testing "the board tells the same story"
            (is (= 3 (count rows)) "the parent and one row per piece")
            (is (every? #(= "done" (:status %)) rows)
                (str "still open: " (pr-str (remove #(= "done" (:status %)) rows))))
            (is (= "DT" (:branch_id parent))
                "the row stayed with the branch that was parked on it"))))
      (finally (fs/delete-tree root-dir)))))
