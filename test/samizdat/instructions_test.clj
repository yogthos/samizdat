;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.instructions-test
  "The project's own instruction files (karamazov-d5wo.4): the root one opens
  the run beside the orient block, and a subdirectory's is pinned into the
  tape the first time a tool touches a file under it."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [jolt.fs :as fs]
            [mycelium.cell :as cell]
            [samizdat.agent.gates :as gates]
            [samizdat.agent.instructions :as instr]
            [samizdat.agent.loop]
            [samizdat.cells :as cells]))

(defn- policy [] (gates/threshold :instructions))

(defn- tree []
  (let [dir (str (fs/create-temp-dir))]
    (fs/create-dirs (fs/path dir "a/b"))
    (fs/create-dirs (fs/path dir "c"))
    (fs/spit (fs/path dir "AGENTS.md") "ROOT RULES")
    (fs/spit (fs/path dir "CLAUDE.md") "@AGENTS.md")
    (fs/spit (fs/path dir "a/AGENTS.md") "A RULES")
    (fs/spit (fs/path dir "a/b/CLAUDE.md") "B RULES")
    (fs/spit (fs/path dir "a/b/x.clj") "(ns x)")
    (fs/spit (fs/path dir "c/y.clj") "(ns y)")
    dir))

;; --- the root ----------------------------------------------------------------

(deftest the-root-file-is-the-first-listed-name-that-exists
  (let [dir (tree)]
    (is (= {:path "AGENTS.md" :text "ROOT RULES" :truncated? false}
           (instr/root-file dir (policy)))
        "AGENTS.md before CLAUDE.md, so a CLAUDE.md that only imports it is not read twice")
    (testing "a CLAUDE.md alone is read"
      (fs/delete (fs/path dir "AGENTS.md"))
      (is (= "@AGENTS.md" (:text (instr/root-file dir (policy))))))
    (testing "none at all, or switched off, is nil"
      (is (nil? (instr/root-file (str (fs/create-temp-dir)) (policy))))
      (is (nil? (instr/root-file dir (assoc (policy) :enabled? false)))))))

(deftest a-long-file-is-capped-and-says-so
  (let [dir (str (fs/create-temp-dir))]
    (fs/spit (fs/path dir "AGENTS.md") (apply str (repeat 500 "x")))
    (let [f (instr/root-file dir (assoc (policy) :max-chars 100))]
      (is (= 100 (count (:text f))))
      (is (:truncated? f)))))

(deftest the-opening-context-carries-the-root-file-before-orient
  (let [dir (tree)
        opening (instr/opening-context dir "ORIENT BLOCK" (policy))]
    (is (str/includes? opening "ROOT RULES"))
    (is (str/includes? opening "AGENTS.md") "it names the file it came from")
    (is (< (str/index-of opening "ROOT RULES") (str/index-of opening "ORIENT BLOCK"))))
  (testing "no instruction file leaves orient exactly as it was"
    (is (= "ORIENT BLOCK" (instr/opening-context (str (fs/create-temp-dir)) "ORIENT BLOCK" (policy))))
    (is (nil? (instr/opening-context (str (fs/create-temp-dir)) nil (policy))))))

;; --- the directories ---------------------------------------------------------

(deftest touched-paths-come-from-the-call-arguments
  (is (= ["a/b/x.clj"] (instr/touched-paths {:name "read_file" :args {:path "a/b/x.clj"}} (policy))))
  (is (= [] (instr/touched-paths {:name "shell" :args {:command "ls a"}} (policy))))
  (is (= [] (instr/touched-paths nil (policy)))))

(deftest a-touched-file-loads-every-unseen-directory-file-above-it
  (let [dir (tree)
        {:keys [found seen]} (instr/pending dir ["a/b/x.clj"] #{} (policy))]
    (is (= [["a" "a/AGENTS.md" "A RULES"] ["a/b" "a/b/CLAUDE.md" "B RULES"]]
           (mapv (juxt :dir :path :text) found))
        "outermost first, and never the root, which the opening already carries")
    (is (= #{"a" "a/b"} seen))
    (testing "a directory already seen is not loaded again"
      (is (= ["a/b"] (mapv :dir (:found (instr/pending dir ["a/b/x.clj"] #{"a"} (policy)))))))
    (testing "a directory with no file is remembered as seen, so it is not re-checked"
      (let [r (instr/pending dir ["c/y.clj"] #{} (policy))]
        (is (= [] (:found r)))
        (is (= #{"c"} (:seen r)))))
    (testing "a path outside the root loads nothing"
      (is (= [] (:found (instr/pending dir ["../elsewhere/z.clj"] #{} (policy))))))
    (testing "switched off loads nothing"
      (is (= [] (:found (instr/pending dir ["a/b/x.clj"] #{} (assoc (policy) :enabled? false))))))))

(deftest dispatch-pins-a-directory-file-the-first-time-it-is-touched
  (cells/load-cells!)
  (let [dir (tree)
        branch {:id "B1" :messages [{:role "user" :content "go"}]}
        step (fn [b]
               (with-redefs [samizdat.agent.loop/tool-step
                             (fn [_ctx b _turn _parsed] {:branch b :result {:ok "read"} :tool "read_file"})]
                 (:branch ((:handler (cell/get-cell! :tool/dispatch))
                           {:root dir}
                           {:branch b :turn 3
                            :parsed {:name "read_file" :args {:path "a/b/x.clj"}}}))))
        once (step branch)
        pinned (filter :pinned? (:messages once))]
    (is (= 2 (count pinned)) "a/ and a/b/, each once")
    (is (every? #(str/includes? (:content %) "RULES") pinned))
    (is (= ["a" "a/b"] (mapv :instructions pinned)))
    (testing "touching the same directory again adds nothing"
      (is (= (count (:messages once)) (count (:messages (step once))))))))
