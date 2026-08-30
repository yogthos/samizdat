;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.replroots-test
  "Which directories `eval` can require from.

  The system prompt's whole first section is REPL-first, so a project whose
  own declared paths are not reachable from `eval` cannot follow the
  instruction it is given. That failure does not look like a harness bug from
  inside the run — it looks like a model that keeps reading source files with
  `shell` instead of loading them, which is exactly what runs fps5 through
  fps9 did."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest testing is]]
            [samizdat.repl :as repl]))

(defn- tmp-project!
  "A project directory whose deps.edn declares `paths`."
  [paths]
  (let [d (io/file (System/getProperty "java.io.tmpdir")
                   (str "samizdat-roots-" (System/nanoTime)))]
    (.mkdirs d)
    (spit (io/file d "deps.edn") (pr-str {:paths paths}))
    d))

(deftest an-absolute-declared-path-is-used-as-declared
  ;; THE BUG, from run 628ffd2e. fps-game's deps.edn puts the raylib FFI
  ;; binding on the classpath by absolute path. ensure-project-roots! resolved
  ;; every declared path against the run root, and java.io.File joins an
  ;; absolute child onto its parent rather than replacing it — so the root
  ;; became
  ;;
  ;;   /…/fps-game//Users/yogthos/src/raylib-examples/raylib-jolt-examples/src
  ;;
  ;; which does not exist. `(require 'net.b12n.raylib-jlt.raylib)` therefore
  ;; failed inside eval while succeeding from a shell in the same directory,
  ;; and the model spent eighty turns reading the library's source with `shell`
  ;; because that was the only way it could see it.
  (let [abs (str (io/file (System/getProperty "java.io.tmpdir") "some-other-lib" "src"))
        d (tmp-project! ["src" abs])
        roots (repl/declared-roots (str d))]
    (is (some #{abs} roots)
        "an absolute path belongs to itself, not under the run root")
    (is (not-any? #(and (not= % abs) (.endsWith (str %) abs)) roots)
        "and must not appear with the root glued in front of it")))

(deftest a-relative-declared-path-still-resolves-against-the-root
  (let [d (tmp-project! ["src" "test"])
        roots (set (repl/declared-roots (str d)))]
    (is (contains? roots (str (io/file d "src"))))
    (is (contains? roots (str (io/file d "test"))))))

(deftest a-project-with-no-deps-edn-still-gets-the-usual-two
  (let [d (io/file (System/getProperty "java.io.tmpdir")
                   (str "samizdat-roots-bare-" (System/nanoTime)))]
    (.mkdirs d)
    (let [roots (set (repl/declared-roots (str d)))]
      (is (contains? roots (str (io/file d "src")))
          "guessing src/test costs a failed require; not guessing costs the
           whole REPL-first workflow")
      (is (contains? roots (str (io/file d "test")))))))
