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

(ns samizdat.userspace-adoption-test
  "What a project has not taken yet is OFFERED, never applied
  (karamazov-1a51.6).

  A project's workflow is its own from the first run, so a template a later
  release adds or changes is not written into it, and the versions a project
  evolved before its workflow lived in files are not put back behind its
  back. The supervisor is shown each one and adopts or declines it; either
  answer is remembered, so the same offer is not made twice."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [samizdat.agent.tools]
            [samizdat.agent.tools.base :as base]
            [samizdat.store.db :as db]
            [samizdat.store.userspace :as store]
            [samizdat.userspace :as us]
            [samizdat.manifests]
            [samizdat.prompt]
            [samizdat.cells]
            [samizdat.agent.tools.policy]))

(defn- temp-dir []
  (str (java.nio.file.Files/createTempDirectory
        "samizdat-us-adopt" (make-array java.nio.file.attribute.FileAttribute 0))))

(defn- delete-recursively [^java.io.File f]
  (when (.isDirectory f)
    (doseq [c (.listFiles f)] (delete-recursively c)))
  (.delete f))

(defmacro with-project
  "A bound project, `before` run against its store before the first seed,
  then seeded with `m` (the shipped map when nil)."
  [[root conn m before] & body]
  `(let [~root (temp-dir)
         ~conn (db/open! ":memory:")
         prev-root# (us/bind-root! ~root)]
     (us/bind! ~conn)
     (try ~before
          (us/seed-project! ~m)
          ~@body
          (finally (us/unbind!)
                   (us/bind-root! prev-root#)
                   (db/close ~conn)
                   (delete-recursively (io/file ~root))))))

(defn- file [root rel] (io/file root ".samizdat" rel))

(defn- offer [kind name]
  (some #(when (and (= kind (:kind %)) (= name (:name %))) %) (us/offers)))

(defn- adoption [root]
  (edn/read-string (slurp (file root "adoption.edn"))))

(defn- without-critic-prompt []
  (update (us/template-map) :prompts dissoc :critic))

;; --- nothing to offer ---------------------------------------------------------

(deftest a-freshly-seeded-project-has-nothing-to-adopt
  (with-project [_root _conn nil nil]
    (is (= [] (us/offers)))))

;; --- a role the project does not have ---------------------------------------

(deftest a-shipped-role-the-project-lacks-is-offered-and-adopted
  (with-project [root _ (without-critic-prompt) nil]
    (let [o (offer :prompt "critic")]
      (is (= :new (:offer o)))
      (is (= "prompts/critic.md" (:path o))))
    (is (nil? (us/project-path :prompt "critic")) "offered, not applied")
    (let [r (us/adopt! :prompt "critic" "the critic needs its prompt")]
      (is (= :new (get-in r [:adopted :offer])))
      (is (number? (:version r))))
    (is (= (us/template :prompt "critic") (slurp (file root "prompts/critic.md"))))
    (is (= "prompts/critic.md" (us/role-path :prompt "critic")) "the map names it now")
    (is (= (us/template :prompt "critic") (us/body :prompt "critic")))
    (is (nil? (offer :prompt "critic")) "an adopted offer is not made again")
    (is (= "the critic needs its prompt"
           (:rationale (last (us/versions :prompt "critic")))))))

(deftest a-declined-offer-is-not-made-again-and-says-why
  (with-project [root _ (without-critic-prompt) nil]
    (is (some? (us/decline! :prompt "critic" "this project has no critic")))
    (is (nil? (offer :prompt "critic")))
    (is (nil? (us/project-path :prompt "critic")) "declining writes nothing")
    (is (some #(= {:kind :prompt :name "critic" :offer :new
                   :reason "this project has no critic"}
                  (select-keys % [:kind :name :offer :reason]))
              (:declined (adoption root))))))

;; --- a template that moved on -------------------------------------------------

(defn- as-if-seen!
  "Pretend the project was seeded from `text` for `kind`/`name`: its file is
  that text and the template it saw hashed to it."
  [root kind name text]
  (spit (file root (us/role-path kind name)) text)
  (let [a (adoption root)]
    (spit (file root "adoption.edn")
          (pr-str (assoc-in a [:seen (str (clojure.core/name kind) "/" name)] (hash text))))))

(deftest a-template-a-later-release-changed-is-offered-over-an-untouched-file
  (with-project [root _ nil nil]
    (as-if-seen! root :prompt "critic" "The critic, as an older release had it.")
    (let [o (offer :prompt "critic")]
      (is (= :updated (:offer o)))
      (is (false? (:edited? o)) "the project never touched its copy"))
    (us/adopt! :prompt "critic" "take the new wording")
    (is (= (us/template :prompt "critic") (slurp (file root "prompts/critic.md"))))
    (is (nil? (offer :prompt "critic")))))

(deftest an-update-over-a-file-the-project-edited-says-so-and-can-be-declined
  (with-project [root _ nil nil]
    (as-if-seen! root :prompt "critic" "The critic, as an older release had it.")
    (spit (file root "prompts/critic.md") "The critic, as this project rewrote it.")
    (is (true? (:edited? (offer :prompt "critic"))))
    (us/decline! :prompt "critic" "ours is better for this codebase")
    (is (nil? (offer :prompt "critic")))
    (is (= "The critic, as this project rewrote it."
           (slurp (file root "prompts/critic.md"))) "declining leaves the file alone")))

(deftest a-file-that-already-says-what-the-template-says-is-not-an-offer
  (with-project [root _ nil nil]
    (as-if-seen! root :prompt "critic" "The critic, as an older release had it.")
    (spit (file root "prompts/critic.md") (us/template :prompt "critic"))
    (is (nil? (offer :prompt "critic")))))

;; --- versions the store held before the files ---------------------------------

(defn- stored-before [conn kind name text]
  (store/save! conn kind name text "project" "evolved before files"))

(deftest a-version-the-store-held-before-the-files-is-offered-and-adopted
  (with-project [root conn nil (stored-before conn :prompt "critic" "Our own critic {{answer}}.")]
    (let [o (offer :prompt "critic")]
      (is (= :pending (:offer o)))
      (is (= 1 (:version o))))
    (is (not= "Our own critic {{answer}}." (slurp (file root "prompts/critic.md")))
        "the file is the template until the supervisor says otherwise")
    (is (= "Our own critic {{answer}}." (us/offer-text (offer :prompt "critic"))))
    (us/adopt! :prompt "critic" "keep what this project learned")
    (is (= "Our own critic {{answer}}." (slurp (file root "prompts/critic.md"))))
    (is (nil? (offer :prompt "critic")))
    (is (empty? (:pending (adoption root))))))

(deftest a-broken-offer-is-refused-and-stays-offered
  (with-project [root conn nil (stored-before conn :prompt "critic" "{% if x %} never closed")]
    (let [before (slurp (file root "prompts/critic.md"))
          r (us/adopt! :prompt "critic" "try it")]
      (is (= :render (get-in r [:problem :stage])))
      (is (= before (slurp (file root "prompts/critic.md"))) "nothing was written")
      (is (some? (offer :prompt "critic")) "still there to decline or fix"))))

(deftest adopting-what-is-not-offered-says-so
  (with-project [_root _conn nil nil]
    (is (= {:no-offer true} (us/adopt! :prompt "critic" "why not")))
    (is (nil? (us/decline! :prompt "critic" "why not")))))

;; --- the tool -----------------------------------------------------------------

(defn- run [root args]
  (base/run-tool {:tool-name "adopt" :branch {:id "SUP"} :root root :args args}))

(deftest the-adopt-tool-lists-shows-takes-and-declines
  (with-project [root _ (without-critic-prompt) nil]
    (let [l (:result (run root {:action "list"}))]
      (is (str/includes? l "prompt critic"))
      (is (str/includes? l "new")))
    (is (str/includes? (:result (run root {:action "show" :kind "prompt" :name "critic"}))
                       (us/template :prompt "critic")))
    (testing "a take needs a reason, like every other edit"
      (let [r (run root {:action "take" :kind "prompt" :name "critic"})]
        (is (= :mechanics (:category r)))
        (is (str/includes? (:result r) "rationale"))
        (is (some? (offer :prompt "critic")) "and nothing was taken")))
    (let [r (run root {:action "take" :kind "prompt" :name "critic" :rationale "wanted"})]
      (is (str/includes? (:result r) "Adopted")))
    (is (str/includes? (:result (run root {:action "list"})) "Nothing on offer"))))
