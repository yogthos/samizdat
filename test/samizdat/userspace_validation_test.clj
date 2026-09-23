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

(ns samizdat.userspace-validation-test
  "A broken edit to a project's workflow is REJECTED (karamazov-1a51.8).

  The project's files are edited while the harness runs — by a person, by the
  agent's file tools, by a shell command. An edit that does not parse, does
  not compile, or names a cell that is not there is never what runs: the
  last version that passed keeps running, the file is left as written so it
  can be fixed in place, and whoever made the edit is told exactly what broke."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [samizdat.agent.tools.base :as base]
            [samizdat.agent.tools]
            [samizdat.store.db :as db]
            [samizdat.userspace :as us]
            ;; Each registers its kind's validator as it loads.
            [samizdat.manifests]
            [samizdat.prompt]
            [samizdat.cells]
            [samizdat.agent.tools.policy]))

(defn- temp-dir []
  (str (java.nio.file.Files/createTempDirectory
        "samizdat-us-valid" (make-array java.nio.file.attribute.FileAttribute 0))))

(defn- delete-recursively [^java.io.File f]
  (when (.isDirectory f)
    (doseq [c (.listFiles f)] (delete-recursively c)))
  (.delete f))

(defmacro with-project
  "A bound, seeded project: the full shipped map, so the real validators have
  the real cells and manifests to check against."
  [[root] & body]
  `(let [~root (temp-dir)
         c# (db/open! ":memory:")
         prev-root# (us/bind-root! ~root)]
     (us/bind! c#)
     (try (us/seed-project!)
          ~@body
          (finally (us/unbind!)
                   (us/bind-root! prev-root#)
                   (db/close c#)
                   (delete-recursively (io/file ~root))))))

(defn- file [root rel] (io/file root ".samizdat" rel))

(defmacro with-validator
  "Replace `kind`'s validator for the body, and put the real one back."
  [kind f & body]
  `(let [prev# (us/validator ~kind)]
     (us/register-validator! ~kind ~f)
     (try ~@body (finally (us/register-validator! ~kind prev#)))))

(defn- broken-if-marked [_name text]
  (when (str/includes? text "BROKEN")
    {:stage :compile :message "it says BROKEN" :line 2 :column 3}))

;; --- the mechanism -----------------------------------------------------------

(deftest a-broken-edit-is-not-what-runs
  (with-project [root]
    (with-validator :manifest broken-if-marked
      (let [good (us/body :manifest "loop")
            f (file root "manifests/loop.edn")]
        (spit f "{:cells {}}\n BROKEN")
        (is (= good (us/body :manifest "loop"))
            "the last version that passed keeps running")
        (is (= "{:cells {}}\n BROKEN" (slurp f))
            "and the file is left as written, so the fix can be made in place")
        (is (= ["factory"] (mapv :source (us/versions :manifest "loop")))
            "a rejected text is never recorded as a version")
        (let [r (us/rejection :manifest "loop")]
          (is (= {:stage :compile :message "it says BROKEN" :line 2 :column 3}
                 (:problem r)))
          (is (= (.getPath f) (:path r))))
        (testing "fixing the file clears the rejection and the fix runs"
          (spit f "{:fixed true}")
          (is (= "{:fixed true}" (us/body :manifest "loop")))
          (is (nil? (us/rejection :manifest "loop")))
          (is (= ["factory" "file"] (mapv :source (us/versions :manifest "loop")))))))))

(deftest a-rejection-is-reported-once-per-broken-text
  (with-project [root]
    (with-validator :manifest broken-if-marked
      (let [seen (atom [])
            prev (us/on-reject! #(swap! seen conj %))]
        (try
          (spit (file root "manifests/loop.edn") "BROKEN 1")
          (dotimes [_ 3] (us/body :manifest "loop"))
          (is (= 1 (count @seen)) "reading the same broken text again reports nothing new")
          (spit (file root "manifests/loop.edn") "BROKEN 22")
          (us/body :manifest "loop")
          (is (= 2 (count @seen)) "a different broken text is a new report")
          (is (= #{:manifest} (set (map :kind @seen))))
          (finally (us/on-reject! prev)))))))

(deftest a-broken-role-with-no-good-version-refuses-and-says-why
  (with-project [root]
    (with-validator :manifest broken-if-marked
      (spit (file root "manifests/fresh.edn") "BROKEN")
      (spit (file root "userspace.edn")
            (str/replace (slurp (file root "userspace.edn"))
                         "{:loop" "{:fresh \"manifests/fresh.edn\"\n   :loop"))
      (let [e (try (us/body! :manifest "fresh") nil (catch Throwable e e))]
        (is (= :rejected (:missing (ex-data e))))
        (is (str/includes? (ex-message e) "it says BROKEN"))))))

(deftest check-written-names-the-role-a-path-serves
  (with-project [root]
    (with-validator :manifest broken-if-marked
      (let [p (.getPath (file root "manifests/loop.edn"))]
        (spit p "BROKEN")
        (is (= {:kind :manifest :name "loop" :path p}
               (select-keys (us/check-written! p) [:kind :name :path])))
        (spit p "{:ok true}")
        (is (nil? (us/check-written! p)) "an accepted edit is nothing to report")
        (is (nil? (us/check-written! (str root "/src/app.clj")))
            "a path that serves no role is not the harness's business")))))

;; --- the real validators -----------------------------------------------------

(defn- problem-for [root kind name rel text]
  (spit (file root rel) text)
  (us/body kind name)
  (:problem (us/rejection kind name)))

(deftest a-manifest-that-does-not-read-says-where
  (with-project [root]
    (let [p (problem-for root :manifest "loop" "manifests/loop.edn" "{:cells {:a :b}\n :edges [}")]
      (is (= :read (:stage p)))
      (is (= 2 (:line p)))
      (is (number? (:column p))))))

(deftest a-manifest-naming-a-cell-that-does-not-exist-does-not-compile
  (with-project [root]
    (let [good (us/edn-body :manifest "loop")
          bad (assoc-in good [:cells :start] :no-such/cell)
          p (problem-for root :manifest "loop" "manifests/loop.edn" (pr-str bad))]
      (is (= :compile (:stage p)))
      (is (str/includes? (:message p) "no-such/cell")))))

(deftest a-prompt-that-does-not-render-is-rejected
  (with-project [root]
    (let [p (problem-for root :prompt "critic" "prompts/critic.md" "{% if x %} never closed")]
      (is (= :render (:stage p))))))

(deftest a-gate-naming-a-function-that-does-not-exist-is-rejected
  ;; Explicitly, not by the compiler: in a jolt dev-mode image an unresolved
  ;; symbol compiles and only fails when the gate fires (karamazov-i6f1).
  (with-project [root]
    (let [g (us/edn-body :policy "gates")
          bad (update g :gates conj {:gate :broken :priority 1
                                     :when '(no-such-fn-xyz branch)
                                     :message-suffix "x"
                                     :prediction {:kind :tool-called :window 1}})
          p (problem-for root :policy "gates" "gates.edn" (pr-str bad))]
      (is (some? p))
      (is (str/includes? (:message p) "no-such-fn-xyz")))))

(deftest a-cell-that-does-not-load-is-rejected
  (with-project [root]
    (let [p (problem-for root :cell "critic" "cells/critic.clj" "(ns cells.critic)\n(defn f [x\n")]
      (is (= :read (:stage p)))
      (is (number? (:line p))))))

(deftest a-map-naming-a-file-that-is-not-there-keeps-the-last-good-map
  (with-project [root]
    (let [before (us/role-path :manifest "loop")]
      (spit (file root "userspace.edn")
            (pr-str (assoc-in (us/template-map) [:manifests :loop] "manifests/gone.edn")))
      (is (= before (us/role-path :manifest "loop")) "the last good map is in force")
      (let [p (:problem (us/rejection :map "userspace"))]
        (is (= :shape (:stage p)))
        (is (str/includes? (:message p) "manifests/gone.edn"))))))

;; --- the writer is told, in its own turn -------------------------------------

(deftest a-file-tool-write-that-breaks-a-role-tells-the-writer-what-broke
  (with-project [root]
    (let [r (base/run-tool {:tool-name "write_file" :branch {:id "B1"} :root root
                            :args {:path ".samizdat/manifests/loop.edn"
                                   :content "{:cells {:start :no-such/cell} :edges {:start {:done :end}}}"}})]
      (is (= :mechanics (:category r)) "a rejected edit, not a failure of the branch's work")
      (is (str/includes? (:result r) "REJECTED"))
      (is (str/includes? (:result r) ".samizdat/manifests/loop.edn"))
      (is (str/includes? (:result r) "no-such/cell"))
      (is (str/includes? (:result r) "the previous version is still what runs")))))
