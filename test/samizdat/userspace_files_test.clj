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

(ns samizdat.userspace-files-test
  "A project's workflow as FILES under .samizdat/ (karamazov-1a51.3).

  The shipped templates are a generic starting point. The first time samizdat
  runs in a project it copies them into .samizdat/, mirroring resources/, and
  from then on the project's files are what runs — a person edits them, the
  agent edits them, and the store keeps the history of every version."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [samizdat.agent.gates :as gates]
            [samizdat.store.db :as db]
            [samizdat.store.userspace :as store]
            [samizdat.system :as system]
            [samizdat.userspace :as us]))

(defn- temp-dir []
  (str (java.nio.file.Files/createTempDirectory
        "samizdat-us-files" (make-array java.nio.file.attribute.FileAttribute 0))))

(defn- delete-recursively [^java.io.File f]
  (when (.isDirectory f)
    (doseq [c (.listFiles f)] (delete-recursively c)))
  (.delete f))

(defmacro with-project
  "A bound project: a fresh root and an in-memory store."
  [[root conn] & body]
  `(let [~root (temp-dir)
         ~conn (db/open! ":memory:")
         prev-root# (us/bind-root! ~root)]
     (us/bind! ~conn)
     (try ~@body
          (finally (us/unbind!)
                   (us/bind-root! prev-root#)
                   (db/close ~conn)
                   (delete-recursively (io/file ~root))))))

(def ^:private shipped
  "A slice of the shipped role map, one or two roles per kind — enough to see
  every path shape: a policy at the top level, a prompt under a subdirectory,
  a cell in the ordered list."
  {:policies {:gates "gates.edn"}
   :manifests {:loop "manifests/loop.edn"}
   :cells ["cells/critic.clj"]
   :prompts {:critic "prompts/critic.md"
             :roles/reviewer "prompts/roles/reviewer.md"}})

(defn- file [root rel] (io/file root ".samizdat" rel))

(defmacro unchecked
  "Accept every manifest for the body. These tests are about where the text
  comes from, and the slice above ships one cell — a real loop would not
  compile against it. Validation has its own tests
  (userspace-validation-test)."
  [& body]
  `(let [prev# (us/validator :manifest)]
     (us/register-validator! :manifest (fn [_# _#] nil))
     (try ~@body (finally (us/register-validator! :manifest prev#)))))

(defn- versions-of [kind name] (mapv :source (us/versions kind name)))

;; --- the first run -----------------------------------------------------------

(deftest the-first-run-copies-the-templates-into-the-project-mirroring-resources
  (with-project [root _c]
    (let [r (us/seed-project! shipped)]
      (doseq [rel ["gates.edn" "manifests/loop.edn" "cells/critic.clj"
                   "prompts/critic.md" "prompts/roles/reviewer.md"]]
        (is (.isFile (file root rel)) (str rel " was written")))
      (is (= (us/template :manifest "loop") (slurp (file root "manifests/loop.edn")))
          "a copy, byte for byte")
      (is (not (.exists (file root "tui.edn")))
          "tui.edn is a LAYERED settings file: a full copy in the project would
           sit above a person's ~/.config/samizdat/tui.edn and hide it")
      (is (= 5 (count (:written r))))
      (is (= shipped (edn/read-string (slurp (file root "userspace.edn"))))
          "and the map itself, which from now on is the project's")
      (testing "and the store starts each history at the factory copy"
        (is (= ["factory"] (versions-of :manifest "loop")))))))

(deftest after-the-first-run-nothing-is-copied-again
  ;; A project whose workflow adapted to it is not re-templated. A shipped
  ;; name the project removed stays removed; one a later release adds is
  ;; offered to the supervisor, never written behind its back.
  (with-project [root _c]
    (us/seed-project! shipped)
    (.delete (file root "manifests/loop.edn"))
    (let [r (us/seed-project! (assoc-in shipped [:manifests :beam] "manifests/beam.edn"))]
      (is (empty? (:written r)))
      (is (not (.exists (file root "manifests/loop.edn"))))
      (is (not (.exists (file root "manifests/beam.edn")))))))

(deftest the-shipped-map-names-every-shipped-file-and-nothing-else
  ;; The map replaced the name lists src/ used to carry, and it is what a
  ;; project's first run copies — so it must agree with resources/ exactly,
  ;; or a shipped file is never seeded, or a role points at nothing.
  (let [m (us/template-map)
        named (set (concat (vals (:manifests m)) (vals (:policies m))
                           (:cells m) (vals (:prompts m))))
        on-disk (set (concat
                      (for [d ["manifests" "cells" "prompts"]
                            ^java.io.File f (file-seq (io/file "resources" d))
                            :when (.isFile f)
                            :when (re-find #"\.(edn|clj|md)$" (.getName f))]
                        (subs (.getPath f) (count "resources/")))
                      (for [p (:policies m)] (val p))))]
    (is (= on-disk named)
        (str "missing from the map: " (sort (remove named on-disk))
             "; named but not shipped: " (sort (remove on-disk named))))
    (doseq [p named]
      (is (some? (io/resource p)) (str p " does not resolve on the classpath")))
    (is (not (contains? (set (keys (:policies m))) :tui))
        "tui.edn is layered, not part of a project's workflow")))

;; --- reading and writing -----------------------------------------------------

(deftest the-project-file-is-what-runs
  (with-project [root _c]
    (us/seed-project! shipped)
    (unchecked
     (let [edited (str/replace (slurp (file root "manifests/loop.edn"))
                               "The factory loop:" "Edited by hand:")]
       (spit (file root "manifests/loop.edn") edited)
       (is (= edited (us/body :manifest "loop")))
       (testing "an edit made in the file is recorded as a version on the next read"
         (is (= ["factory" "file"] (versions-of :manifest "loop")))
         (us/body :manifest "loop")
         (is (= ["factory" "file"] (versions-of :manifest "loop"))
             "once — reading the same text again records nothing"))))))

(deftest a-role-the-map-does-not-have-is-refused-and-says-where-to-add-it
  ;; No fallback to the shipped template: the project's map is the whole
  ;; story, and a role src/ needs that it lacks is something the agent is
  ;; told to add rather than something papered over.
  (with-project [root _c]
    (us/seed-project! shipped)
    (is (nil? (us/body :prompt "judge")))
    (let [e (try (us/body! :prompt "judge") nil (catch Throwable e e))]
      (is (some? e))
      (is (= {:kind :prompt :role :judge :missing :role}
             (select-keys (ex-data e) [:kind :role :missing])))
      (is (str/includes? (ex-message e) ":judge"))
      (is (str/includes? (ex-message e) (str root "/.samizdat/userspace.edn"))))
    (testing "a role whose file is gone is refused the same way, naming the file"
      (.delete (file root "manifests/loop.edn"))
      (let [e (try (us/body! :manifest "loop") nil (catch Throwable e e))]
        (is (= :file (:missing (ex-data e))))
        (is (str/includes? (ex-message e) "manifests/loop.edn"))))))

(deftest the-agent-can-point-a-role-at-another-file
  (with-project [root _c]
    (us/seed-project! shipped)
    (unchecked
     (let [other (str/replace (slurp (file root "manifests/loop.edn"))
                              "The factory loop:" "Another file:")]
       (spit (file root "manifests/other.edn") other)
       (spit (file root "userspace.edn")
             (pr-str (assoc-in shipped [:manifests :loop] "manifests/other.edn")))
       (is (= other (us/body :manifest "loop")))
       (is (= ["loop"] (us/roles :manifest)))))))

(deftest a-save-writes-the-file-and-the-history
  (with-project [root _c]
    (us/seed-project! shipped)
    (let [v (us/save! :policy "gates" "{:a 1}" "tighter")]
      (is (= 2 v))
      (is (= "{:a 1}" (slurp (file root "gates.edn"))))
      (is (= "{:a 1}" (us/body :policy "gates")))
      (is (= "tighter" (:rationale (last (us/versions :policy "gates")))))
      (is (= ["factory" "project"] (versions-of :policy "gates"))
          "the save is not ALSO recorded as a file edit when it is read back"))))

(deftest a-revert-rewrites-the-file
  (with-project [root _c]
    (us/seed-project! shipped)
    (us/save! :policy "gates" "{:a 1}" "one")
    (us/save! :policy "gates" "{:a 2}" "two")
    (us/revert! :policy "gates" 2 "two was wrong")
    (is (= "{:a 1}" (slurp (file root "gates.edn"))))
    (is (= "{:a 1}" (us/body :policy "gates")))))

(deftest a-save-of-a-new-role-creates-its-file-and-maps-it
  (with-project [root _c]
    (us/seed-project! shipped)
    (us/save! :manifest "mine" "{:mine true}" "a loop of our own")
    (is (= "{:mine true}" (slurp (file root "manifests/mine.edn"))))
    (is (= "{:mine true}" (us/body :manifest "mine")))
    (is (= "manifests/mine.edn"
           (get-in (edn/read-string (slurp (file root "userspace.edn"))) [:manifests :mine])))
    (testing "a new cell is appended to the ordered list"
      (us/save! :cell "extra" "(ns x)" "a cell of our own")
      (is (= ["cells/critic.clj" "cells/extra.clj"]
             (:cells (edn/read-string (slurp (file root "userspace.edn")))))))))

(deftest the-tui-file-is-written-where-the-tui-reads-it
  ;; Not seeded, but the agent may still save one: that is the project layer
  ;; of the TUI's layered file.
  (with-project [root _c]
    (us/seed-project! shipped)
    (us/save! :policy "tui" "{:prose-turns 4}" "fewer turns")
    (is (= "{:prose-turns 4}" (slurp (file root "tui.edn"))))))

(deftest a-model-specific-prompt-still-wins-over-the-plain-file
  (with-project [root _c]
    (us/seed-project! shipped)
    (let [prev (us/bind-model! {:provider :local :model "qwen3-27b"})]
      (try
        (let [f (file root "prompts/local/qwen3/critic.md")]
          (.mkdirs (.getParentFile f))
          (spit f "for qwen")
          (is (= "for qwen" (us/body :prompt "critic")))
          (is (= ["factory"] (versions-of :prompt "critic"))
              "a variant is not the prompt's own history"))
        (finally (us/bind-model! prev))))))

;; --- a project that predates the files ---------------------------------------

(deftest an-older-projects-stored-versions-are-queued-for-the-supervisor
  ;; A project that ran before its workflow lived in files has its evolution
  ;; in the store. The first run writes the templates like any other project
  ;; and QUEUES the stored versions that differ, so the supervisor can decide
  ;; what to carry over — nothing is applied behind its back.
  (with-project [root c]
    (store/seed! c :policy "gates" (us/template :policy "gates"))
    (store/save! c :policy "gates" "{:tuned true}" "project" "learned")
    (store/save! c :manifest "invented" "{:ours true}" "project" "authored")
    (let [r (us/seed-project! shipped)
          m (edn/read-string (slurp (file root "adoption.edn")))]
      (is (= (us/template :policy "gates") (slurp (file root "gates.edn"))))
      (is (= #{{:kind :policy :name "gates" :version 2}
               {:kind :manifest :name "invented" :version 1}}
             (set (:pending r))
             (set (:pending m))))
      (is (not (.exists (file root "manifests/invented.edn")))))))

;; --- no project directory ----------------------------------------------------

(deftest with-no-root-the-store-is-what-answers-as-before
  (let [c (db/open! ":memory:")
        prev (us/bind-root! nil)]
    (try
      (us/bind! c)
      (us/save! :policy "gates" "{:a 1}" "r")
      (is (= "{:a 1}" (us/body :policy "gates")))
      (finally (us/unbind!) (us/bind-root! prev) (db/close c)))))

;; --- startup -----------------------------------------------------------------

(deftest binding-a-project-seeds-it-before-the-policy-caches-fill
  ;; The caches bind-project! reloads — gates, wordlists, phases — must read
  ;; the project's files, so the files have to exist first.
  (let [root (temp-dir)
        c (db/open! ":memory:")
        prev-root (us/bind-root! root)]
    (try
      (system/bind-project! c)
      (is (= (slurp (io/resource "userspace.edn")) (slurp (file root "userspace.edn")))
          "the full shipped map is copied verbatim, comments and all")
      (doseq [rel ["gates.edn" "phases.edn" "manifests/loop.edn" "manifests/beam.edn"
                   "cells/loop.clj" "prompts/system.md" "prompts/roles/supervisor.md"]]
        (is (.isFile (file root rel)) (str rel " was seeded")))
      (is (not (.exists (file root "tui.edn"))))
      (testing "the gate cache holds the project's own table"
        (let [g (edn/read-string (slurp (file root "gates.edn")))]
          (spit (file root "gates.edn")
                (pr-str (assoc-in g [:cull-threshold :value] 77)))
          (gates/reload-config!)
          (is (= 77 (gates/threshold :cull-threshold)))))
      (finally
        (us/unbind!) (us/bind-root! prev-root) (gates/reload-config!)
        (db/close c)
        (delete-recursively (io/file root))))))

(deftest a-run-drives-the-loop-the-projects-file-says
  ;; load-loop! read the STORED row, so in a project with files a run started
  ;; on whatever the store last held — never on the file a person or the
  ;; agent had just edited.
  (with-project [root _c]
    (us/seed-project!)
    (let [f (file root "manifests/loop.edn")
          d (edn/read-string (slurp f))]
      (spit f (pr-str (assoc d :description "edited in the file")))
      (is (= "edited in the file"
             (:description (:definition ((requiring-resolve 'samizdat.workflow/load-loop!)
                                         _c "loop"))))))))
