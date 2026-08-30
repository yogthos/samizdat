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

(ns samizdat.files-test
  "read_file / write_file — the tools that let the agent read and change the
  project tree. Writes are confined to the project root; a path that escapes it
  is refused, so a self-modifying agent cannot reach outside its own repo."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [jolt.fs :as fs]
            [samizdat.agent.files :as files]))

(defn- ctx [root tool args]
  {:tool-name tool :args args :branch {:id "B1"} :root root})

(deftest read-file-bounds-and-missing
  (let [root (str "/tmp/samizdat-files-" (random-uuid))]
    (fs/create-dirs root)
    (spit (str root "/hello.txt") "line one\nline two\n")
    (try
      (testing "reads a file under the root"
        (let [r (files/read-file (ctx root "read_file" {:path "hello.txt"}))]
          (is (= :neutral (:category r)))
          (is (str/includes? (:result r) "line one"))))
      (testing "a missing file is a mechanics miss, not a failure"
        (is (= :mechanics (:category (files/read-file (ctx root "read_file" {:path "nope.txt"}))))))
      (testing "a path escaping the root is refused"
        (is (= :mechanics (:category (files/read-file (ctx root "read_file" {:path "../etc/passwd"})))))
        (is (= :mechanics (:category (files/read-file (ctx root "read_file" {:path "/etc/passwd"}))))))
      (finally (fs/delete-tree root)))))

(deftest write-file-confined-to-root
  (let [root (str "/tmp/samizdat-files-" (random-uuid))]
    (fs/create-dirs root)
    (try
      (testing "writes a new file under the root, creating parent dirs"
        (let [r (files/write-file (ctx root "write_file"
                                       {:path "src/new/thing.clj" :content "(ns thing)"}))]
          (is (= :success (:category r)))
          (is (:progress? r))
          (is (= "(ns thing)" (slurp (str root "/src/new/thing.clj"))))))
      (testing "overwrites an existing file"
        (files/write-file (ctx root "write_file" {:path "a.txt" :content "v1"}))
        (files/write-file (ctx root "write_file" {:path "a.txt" :content "v2"}))
        (is (= "v2" (slurp (str root "/a.txt")))))
      (testing "a path escaping the root is refused and writes nothing"
        (let [r (files/write-file (ctx root "write_file"
                                       {:path "../escape.txt" :content "x"}))]
          (is (= :mechanics (:category r)))
          (is (not (fs/exists? (str root "/../escape.txt")))))
        (is (= :mechanics (:category (files/write-file
                                      (ctx root "write_file"
                                           {:path "/tmp/abs-escape.txt" :content "x"}))))))
      (finally (fs/delete-tree root)))))

(deftest write-file-repairs-unbalanced-clojure
  (let [root (str "/tmp/samizdat-files-" (random-uuid))]
    (fs/create-dirs root)
    (try
      (testing "a trailing-truncated Clojure file is closed and noted"
        (let [r (files/write-file (ctx root "write_file"
                                       {:path "trunc.clj" :content "(defn f [] (+ 1 2)"}))]
          (is (= :success (:category r)))
          (is (:repaired? r))
          (is (str/includes? (:result r) "auto-closed"))
          (is (= "(defn f [] (+ 1 2))" (slurp (str root "/trunc.clj"))))))
      (testing "a non-clojure file is written verbatim, no repair"
        (files/write-file (ctx root "write_file" {:path "notes.txt" :content "(unbalanced"}))
        (is (= "(unbalanced" (slurp (str root "/notes.txt")))))
      (testing "a mid-file imbalance is written as-is with a clear warning"
        (let [r (files/write-file (ctx root "write_file"
                                       {:path "mid.clj" :content "(f)) (g)"}))]
          (is (str/includes? (:result r) "will not load"))
          ;; karamazov-mea: the position is a line and column, not a char
          ;; offset into a file the model has only seen part of.
          (is (str/includes? (:result r) "line 1, col 4"))
          (is (= "(f)) (g)" (slurp (str root "/mid.clj"))))))
      (finally (fs/delete-tree root)))))

;; --- karamazov-2d3 / karamazov-ozv -------------------------------------------
;; The three defects vis exposed, each verified live before the fix:
;;   (a) edit_file wrote the file it had just broken
;;   (b) and scored it :success / :progress? true
;;   (c) and announced write_file's repair note — "auto-closed 1 unclosed
;;       delimiter(s) … appended `)`" — for a repair the edit path deliberately
;;       does NOT apply, so the model was told a closer had been added to a
;;       file that still did not have one.
;; vis's rule (token-optimization.md): "It re-parses the file after the write,
;; so a syntax-breaking batch is refused whole and the file is left untouched."

(deftest an-edit-that-breaks-the-file-is-refused-not-written
  (let [root (str "/tmp/samizdat-files-" (random-uuid))
        original "(ns demo)\n\n(defn f [] 1)\n"]
    (fs/create-dirs root)
    (try
      (doseq [[label broken] [["unbalanced" "(defn f [] 1"]
                              ["balanced but unreadable" "(defn f [] :)"]
                              ["mismatched" "(defn f [] (vec [1 2)])"]]]
        (spit (str root "/core.clj") original)
        (testing label
          (let [r (files/edit-file (ctx root "edit_file"
                                        {:path "core.clj"
                                         :old_text "(defn f [] 1)"
                                         :new_text broken}))]
            (is (= :mechanics (:category r)) "a call that would break the file is a miss")
            (is (not (:progress? r)) "breaking the tree is never progress")
            (is (= original (slurp (str root "/core.clj")))
                "the file on disk is untouched")
            (is (not (re-find #"(?i)auto-clos|auto-remov" (:result r)))
                "never announce a repair the edit path does not apply"))))
      (testing "the refusal says where the problem is, in line and column"
        (spit (str root "/core.clj") original)
        (let [r (files/edit-file (ctx root "edit_file"
                                      {:path "core.clj"
                                       :old_text "(defn f [] 1)"
                                       :new_text "(defn f [] (vec [1 2)])"}))]
          (is (re-find #"line \d+" (:result r)))
          (is (re-find #"col \d+" (:result r)))))
      (testing "a good edit still lands and still counts as progress"
        (spit (str root "/core.clj") original)
        (let [r (files/edit-file (ctx root "edit_file"
                                      {:path "core.clj"
                                       :old_text "(defn f [] 1)"
                                       :new_text "(defn f [] 2)"}))]
          (is (= :success (:category r)))
          (is (:progress? r))
          (is (= "(ns demo)\n\n(defn f [] 2)\n" (slurp (str root "/core.clj"))))))
      (testing "a non-Clojure file is not syntax-gated"
        (spit (str root "/notes.txt") "hello\n")
        (let [r (files/edit-file (ctx root "edit_file"
                                      {:path "notes.txt"
                                       :old_text "hello" :new_text "(oops"}))]
          (is (= :success (:category r)))
          (is (= "(oops\n" (slurp (str root "/notes.txt"))))))
      (finally (fs/delete-tree root)))))

(deftest write-file-reports-balanced-but-unreadable-clojure
  ;; karamazov-ozv: this used to be silent — "Wrote 17 chars to w.clj." and
  ;; nothing else, because balance/scan answered the delimiter question and
  ;; nobody asked the reader.
  (let [root (str "/tmp/samizdat-files-" (random-uuid))]
    (fs/create-dirs root)
    (try
      (let [r (files/write-file (ctx root "write_file"
                                     {:path "w.clj" :content "(ns w)\n(def x :)\n"}))]
        (is (re-find #"(?i)not clojure|does not read|invalid token" (:result r))
            "the model is told, and told what the reader said")
        (is (not (:repaired? r))))
      (finally (fs/delete-tree root)))))

(deftest anchored-read-and-patch-round-trip
  ;; karamazov-0kk: the model spends a coordinate it was HANDED instead of
  ;; reproducing the text it is replacing.
  (let [root (str "/tmp/samizdat-files-" (random-uuid))]
    (fs/create-dirs root)
    (try
      (spit (str root "/core.clj") "(ns demo)\n\n(defn f [] 1)\n")
      (testing "a plain read is unchanged — anchors are opt-in"
        (let [r (files/read-file (ctx root "read_file" {:path "core.clj"}))]
          (is (str/includes? (:result r) "(ns demo)"))
          (is (not (str/includes? (:result r) "│")))))
      (testing "an anchored read prints an address beside every line"
        (let [r (files/read-file (ctx root "read_file"
                                      {:path "core.clj" :anchors true}))]
          (is (str/includes? (:result r) "1:0b3│ (ns demo)"))
          (is (str/includes? (:result r) "│ (defn f [] 1)"))))
      (testing "an anchor read off that result patches the line"
        (let [r (files/patch-file (ctx root "patch"
                                      {:path "core.clj"
                                       :edits [{:from "3:36d" :replace "(defn f [] 2)"}]}))]
          (is (= :success (:category r)) (:result r))
          (is (:progress? r))
          (is (= "(ns demo)\n\n(defn f [] 2)\n" (slurp (str root "/core.clj"))))))
      (testing "a stale anchor is refused, and the refusal hands back the live one"
        (let [r (files/patch-file (ctx root "patch"
                                      {:path "core.clj"
                                       :edits [{:from "3:36d" :replace "(defn f [] 3)"}]}))]
          (is (= :mechanics (:category r)))
          (is (str/includes? (:result r) "Nothing was written"))
          (is (re-find #"3:[0-9a-f]{3}" (:result r)) "the current anchor, so recovery is one call")
          (is (= "(ns demo)\n\n(defn f [] 2)\n" (slurp (str root "/core.clj"))))))
      (testing "a patch that would break the file is refused like an edit"
        (let [r (files/patch-file (ctx root "patch"
                                      {:path "core.clj"
                                       :edits [{:from "3:38c" :replace "(defn f [] :)"}]}))]
          (is (= :mechanics (:category r)))
          (is (= "(ns demo)\n\n(defn f [] 2)\n" (slurp (str root "/core.clj"))))))
      (testing "the run config is protected here too"
        (fs/create-dirs (str root "/.samizdat"))
        (spit (str root "/.samizdat/config.edn") "{}")
        (is (= :mechanics (:category (files/patch-file
                                      (ctx root "patch"
                                           {:path ".samizdat/config.edn"
                                            :edits [{:from "1:000" :replace "{}"}]}))))))
      (finally (fs/delete-tree root)))))

(deftest write-then-read-roundtrips
  (let [root (str "/tmp/samizdat-files-" (random-uuid))]
    (fs/create-dirs root)
    (try
      (files/write-file (ctx root "write_file" {:path "round.clj" :content "(+ 1 2)"}))
      (is (str/includes? (:result (files/read-file (ctx root "read_file" {:path "round.clj"})))
                         "(+ 1 2)"))
      (finally (fs/delete-tree root)))))

(deftest a-long-file-pages-instead-of-dead-ending
  ;; The truncation marker used to be `… [truncated]` and nothing else. Live,
  ;; against a 7KB brief, that had the model read the same file six times
  ;; through four different tools, get the identical first 4014 characters
  ;; every time, and then spend four more turns writing a chunked reader in
  ;; `eval` — ten turns of forty to read the file it was told to start from.
  (let [dir (str (fs/create-temp-dir))
        big (str/join "\n" (map #(str "line " % " " (apply str (repeat 60 \x)))
                                (range 400)))
        _ (spit (str dir "/big.txt") big)
        read #(files/read-file {:branch {:id "B1"} :root dir :args %})
        p1 (:result (read {:path "big.txt"}))]
    (testing "the first page names the call that continues it"
      (is (str/includes? p1 "read_file"))
      (is (str/includes? p1 "offset")))
    (testing "that call returns DIFFERENT content, not the same first page"
      (let [next-line (Integer/parseInt (second (re-find #"\"offset\": (\d+)" p1)))
            p2 (:result (read {:path "big.txt" :offset next-line}))]
        (is (pos? next-line))
        (is (not= p1 p2))
        (is (str/includes? p2 (str "line " next-line " ")))
        (is (not (str/includes? p2 "line 0 ")))))
    (testing "paging reaches the end, where nothing more is offered"
      (let [last-page (:result (read {:path "big.txt" :offset 395}))]
        (is (str/includes? last-page "line 399"))
        (is (not (str/includes? last-page "Continue with")))))
    (testing "limit bounds the page in lines"
      (let [r (:result (read {:path "big.txt" :offset 10 :limit 3}))]
        (is (str/includes? r "line 10 "))
        (is (str/includes? r "line 12 "))
        (is (not (str/includes? r "line 13 ")))))
    (testing "a short file still comes back whole and offers no continuation"
      (spit (str dir "/small.txt") "a\nb\nc")
      (let [r (:result (read {:path "small.txt"}))]
        (is (str/includes? r "c"))
        (is (not (str/includes? r "Continue with")))))))

(deftest the-run-config-is-not-writable-by-the-run-it-gates
  ;; karamazov-kvw: run 671e8a99 rewrote .samizdat/config.edn mid-run,
  ;; replacing :verify-cmd "jolt -M:test" (3 real tests) with an alias whose
  ;; runner executes 0 tests and exits 0 — a Gate 2 that always passes. The
  ;; gate definition belongs to the operator; the party a gate judges does
  ;; not get to edit it. Reads stay open — a run may always inspect its gates.
  (let [root (str "/tmp/samizdat-files-" (random-uuid))
        config "{:run {:verify-cmd \"jolt -M:test\"}}"]
    (fs/create-dirs (str root "/.samizdat"))
    (spit (str root "/.samizdat/config.edn") config)
    (try
      (testing "write_file refuses and writes nothing"
        (let [r (files/write-file (ctx root "write_file"
                                       {:path ".samizdat/config.edn"
                                        :content "{:run {:verify-cmd \"true\"}}"}))]
          (is (= :mechanics (:category r)))
          (is (re-find #"operator" (str (:result r)))
              "the refusal says whose file it is")
          (is (= config (slurp (str root "/.samizdat/config.edn"))))))
      (testing "edit_file refuses too"
        (let [r (files/edit-file (ctx root "edit_file"
                                      {:path ".samizdat/config.edn"
                                       :old_text "jolt -M:test" :new_text "true"}))]
          (is (= :mechanics (:category r)))
          (is (= config (slurp (str root "/.samizdat/config.edn"))))))
      (testing "a dressed-up path does not slip past the resolve"
        (let [r (files/write-file (ctx root "write_file"
                                       {:path "src/../.samizdat/config.edn"
                                        :content "x"}))]
          (is (= :mechanics (:category r)))
          (is (= config (slurp (str root "/.samizdat/config.edn"))))))
      (testing "reading it stays allowed"
        (is (= :neutral (:category (files/read-file
                                    (ctx root "read_file"
                                         {:path ".samizdat/config.edn"}))))))
      (testing "the rest of .samizdat/ is untouched by the rule"
        (is (= :success (:category (files/write-file
                                    (ctx root "write_file"
                                         {:path ".samizdat/skills/mine.md"
                                          :content "# a project skill"}))))))
      (finally (fs/delete-tree root)))))

;; --- declared reference paths (karamazov-1an) --------------------------------
;;
;; THE INVERSION this fixes, from run bd56a286: the brief named a language
;; reference and ~95 worked examples, both siblings of the project root.
;; read_file refused every one of them, and the model read them all with
;; `shell` instead — so the confinement protected nothing and moved the read
;; from the narrow tool to the one that spawns a process.

(defn- ref-ctx [root refs tool args]
  (assoc (ctx root tool args) :config {:run {:reference-paths refs}}))

(deftest an-absolute-reference-path-belongs-to-itself
  ;; The same mistake that made `eval` unable to load fps-game's own FFI
  ;; dependency for eight runs (karamazov-9uc): File(parent, child) glues an
  ;; absolute child onto its parent instead of replacing it.
  (let [lib (str "/tmp/samizdat-ref-lib-" (random-uuid))]
    (fs/create-dirs lib)
    (try
      (let [roots (files/reference-roots [lib] "/tmp")]
        (is (some #{(str (fs/canonicalize lib))} roots)
            "declared absolute, used absolute")
        (is (not-any? #(str/includes? % "/tmp/tmp/") roots)))
      (finally (fs/delete-tree lib)))))

(deftest reads-reach-a-declared-reference-path-and-writes-do-not
  (let [root (str "/tmp/samizdat-files-" (random-uuid))
        examples (str "/tmp/samizdat-examples-" (random-uuid))]
    (fs/create-dirs root)
    (fs/create-dirs examples)
    (spit (str examples "/camera.clj") "(ns camera) ;; how the pros do it\n")
    (try
      (testing "with nothing declared, the read is refused as it always was"
        (let [r (files/read-file (ctx root "read_file"
                                      {:path (str examples "/camera.clj")}))]
          (is (= :mechanics (:category r)))
          (is (str/includes? (:result r) "outside the project root"))))
      (testing "declared, the same read goes through the narrow tool"
        (let [r (files/read-file (ref-ctx root [examples] "read_file"
                                          {:path (str examples "/camera.clj")}))]
          (is (= :neutral (:category r)))
          (is (str/includes? (:result r) "how the pros do it"))))
      (testing "a path outside every declared root is still refused, and the
                refusal names the roots that would have worked — a boundary the
                model cannot see the shape of can only be probed by failing"
        (let [r (files/read-file (ref-ctx root [examples] "read_file"
                                          {:path "/etc/hosts"}))]
          (is (= :mechanics (:category r)))
          (is (str/includes? (:result r) examples))))
      (testing "WRITES are unchanged: reference material is read-only, and the
                write path keeps the one confinement primitive it always had"
        (let [r (files/write-file (ref-ctx root [examples] "write_file"
                                           {:path (str examples "/mine.clj")
                                            :content "(ns mine)"}))]
          (is (= :mechanics (:category r)))
          (is (not (fs/exists? (str examples "/mine.clj")))))
        (let [r (files/edit-file (ref-ctx root [examples] "edit_file"
                                          {:path (str examples "/camera.clj")
                                           :old_text "pros" :new_text "amateurs"}))]
          (is (= :mechanics (:category r)))
          (is (str/includes? (slurp (str examples "/camera.clj")) "pros"))))
      (finally (fs/delete-tree root) (fs/delete-tree examples)))))

(deftest grep-sweeps-a-reference-tree-only-when-asked-for-it
  ;; Ninety examples swept by default would drown every ordinary search, so a
  ;; reference root is searched only when an ABSOLUTE :paths entry names it.
  (let [root (str "/tmp/samizdat-files-" (random-uuid))
        examples (str "/tmp/samizdat-examples-" (random-uuid))]
    (fs/create-dirs (str root "/src"))
    (fs/create-dirs examples)
    (spit (str root "/src/main.clj") "(defn draw-cube [])\n")
    (spit (str examples "/camera.clj") "(defn draw-cube [])\n")
    (try
      (let [refs (files/reference-roots [examples] root)]
        (testing "an unscoped search answers the project alone"
          (let [hits (files/grep-project root "draw-cube" {:refs refs})]
            (is (= ["src/main.clj"] (map :path hits)))))
        (testing "an absolute scope naming the reference root searches it, and
                  the hit comes back as an absolute path read_file takes"
          (let [hits (files/grep-project root "draw-cube"
                                         {:paths [examples] :refs refs})]
            (is (= 1 (count hits)))
            (is (str/includes? (:path (first hits)) "camera.clj"))
            (is (= :neutral (:category (files/read-file
                                        (ref-ctx root [examples] "read_file"
                                                 {:path (:path (first hits))})))))))
        (testing "an absolute scope under no declared root sweeps nothing"
          (is (empty? (files/grep-project root "draw-cube"
                                          {:paths ["/etc"] :refs refs})))))
      (finally (fs/delete-tree root) (fs/delete-tree examples)))))

(deftest a-declared-path-that-is-not-there-is-dropped
  ;; The system prompt names these to the model. Advertising a directory that
  ;; does not exist buys a wasted turn and a refusal that reads as a harness
  ;; fault, which is how every silent-failure bug in this project has looked.
  (let [real (str "/tmp/samizdat-ref-real-" (random-uuid))]
    (fs/create-dirs real)
    (try
      (let [roots (files/reference-roots [real "/no/such/reference/tree"] "/tmp")]
        (is (= 1 (count roots)))
        (is (str/includes? (first roots) "samizdat-ref-real")))
      (finally (fs/delete-tree real)))))
