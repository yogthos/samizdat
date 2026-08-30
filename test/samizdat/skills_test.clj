;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.skills-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [jolt.fs :as fs]
            [samizdat.agent.skills :as skills]
            [samizdat.agent.tools.base :as base]
            [samizdat.agent.tools.skills]))

(deftest the-bundled-mycelium-skill-is-discovered
  (let [cat (skills/catalog)]
    (is (some #(= "mycelium" (:name %)) cat) "mycelium is in the catalogue")
    (is (every? (comp seq :description) cat) "every skill has a description")))

(deftest load-returns-content-and-nil-for-a-miss
  (is (str/includes? (skills/load-skill "mycelium") "defcell"))
  (is (nil? (skills/load-skill "no-such-skill"))))

(deftest frontmatter-parses-and-drives-the-catalogue
  (let [fm (skills/parse-frontmatter "---\nname: x\ndescription: Use when Y.\n---\n# T\nbody")]
    (is (= "Use when Y." (get-in fm [:meta :description])))
    (is (= "# T\nbody" (:body fm))))
  (let [fm (skills/parse-frontmatter "no frontmatter here\nmore")]
    (is (= {} (:meta fm)))
    (is (str/starts-with? (:body fm) "no frontmatter"))))

(deftest the-catalogue-is-always-renderable-with-triggers
  (let [r (skills/render-catalog)]
    (is (str/includes? r "mycelium"))
    (is (str/includes? r "Use when") "the description is a when-to-use trigger")
    (is (not (str/includes? r "defcell")) "the body is NOT in the always-on catalogue")))

(deftest a-skill-with-no-description-drops-from-the-catalogue-but-stays-loadable
  (is (nil? (#'skills/describe {:meta {} :body "# only a heading"} "h"))
      "no frontmatter description and only headings -> no trigger")
  (is (some? (#'skills/describe {:meta {} :body "a real line"} "h"))))

(deftest the-skill-tool-lists-and-loads
  (let [lst (base/run-tool {:branch {:id "B1"} :tool-name "skill" :args {:action "list"}})]
    (is (str/includes? (:result lst) "mycelium")))
  (let [ld (base/run-tool {:branch {:id "B1"} :tool-name "skill"
                           :args {:action "load" :name "mycelium"}})]
    (is (= :neutral (:category ld)) "loading a skill is neutral bookkeeping")
    (is (str/includes? (:result ld) "manifest")))
  (let [miss (base/run-tool {:branch {:id "B1"} :tool-name "skill"
                             :args {:action "load" :name "nope"}})]
    (is (= :mechanics (:category miss)) "an unknown skill is a malformed call")))

(deftest a-load-without-a-name-is-a-mechanics-complaint-with-a-skeleton
  ;; provenance CR1-1: base/missing was called with `branch` and its
  ;; complaint string returned RAW as the tool result — no :category, no
  ;; :branch — so tool-step threaded a nil branch into state/record-outcome,
  ;; which NPE'd on (update nil :turns-since-progress inc).
  (let [r (base/run-tool {:branch {:id "B1"} :tool-name "skill"
                          :args {:action "load"}})]
    (is (= :mechanics (:category r)) "a missing arg is malformed, not a failure")
    (is (map? (:branch r)) "the branch rides along")
    (is (str/includes? (:result r) "Missing required argument(s): name"))
    (is (str/includes? (:result r) "\"skill\"") "the skeleton names the tool")))

(deftest shipped-skills-resolve-off-the-classpath-from-any-cwd
  ;; karamazov-blt.33: the loader scanned cwd-relative "resources/skills", so
  ;; from any other project (and from a built binary, where the dir does not
  ;; exist on disk) every implementor silently lost the REPL/TDD guidance and
  ;; the catalogue rendered empty — the classpath-has-no-listing trap
  ;; cells/shipped-cells enumerates around, in a fourth place.
  (let [cat (skills/catalog ["/no/such/dir"])]
    (is (some #(= "repl-workflow" (:name %)) cat)
        "the bundled skills survive a foreign cwd"))
  (is (str/includes? (str (skills/load-skill ["/no/such/dir"] "repl-workflow"))
                     "eval")
      "and load by name with no overlay directory present")
  (let [on-disk (->> (file-seq (java.io.File. "resources/skills"))
                     (filter #(str/ends-with? (.getName ^java.io.File %) ".md"))
                     (map #(str/replace (.getName ^java.io.File %) #"\.md$" ""))
                     set)]
    (is (= on-disk (set skills/shipped-skills))
        "the enumerated list is pinned against the directory")))

;; --- karamazov-15h -----------------------------------------------------------
;; Discovery scanned `<dir>/*.md` only, so it could not read a skill DIRECTORY
;; holding SKILL.md — the shape Claude Code, pi, opencode, vis and the agents
;; standard all use, and the shape of every skill checked into this very repo.
;; AGENTS.md points the agent at .agents/skills/beads/SKILL.md; its own harness
;; had no way to load it.

(deftest a-skill-directory-holding-SKILL-md-is-discovered
  (let [root (str "/tmp/samizdat-skills-" (random-uuid))
        dir (str root "/.samizdat/skills")]
    (fs/create-dirs (str dir "/nested-one"))
    (fs/create-dirs (str dir "/nested-two"))
    (try
      (spit (str dir "/flat.md")
            "---\nname: flat\ndescription: Use when flat.\n---\nflat body")
      (spit (str dir "/nested-one/SKILL.md")
            "---\nname: nested-one\ndescription: Use when nested.\n---\nnested body")
      ;; no `name:` in the frontmatter — the DIRECTORY name is the fallback
      (spit (str dir "/nested-two/SKILL.md")
            "---\ndescription: Use when unnamed.\n---\nunnamed body")
      (let [found (skills/discover (skills/project-dirs root))]
        (is (contains? found "flat") "the flat form still works")
        (is (contains? found "nested-one") "a SKILL.md directory is found")
        (is (contains? found "nested-two") "and names itself after its directory")
        (is (str/includes? (get-in found ["nested-one" :body]) "nested body"))
        (is (= "Use when unnamed." (get-in found ["nested-two" :description]))))
      (finally (fs/delete-tree root)))))

(deftest the-skill-roots-are-policy-not-a-literal-in-src
  (let [roots (skills/skill-roots)]
    (is (seq roots) "there is a root list, and it comes from resources")
    (is (some #(str/includes? % ".samizdat/skills") roots)
        "samizdat's own root still wins")
    (is (some #(str/includes? % ".agents/skills") roots)
        "the agents-standard root AGENTS.md points at is scanned")))

(deftest this-repos-own-beads-skill-is-reachable
  ;; The regression that motivated the bead: the canonical instruction file
  ;; directs the agent to a skill the harness could not read.
  (let [found (skills/discover (skills/project-dirs "."))]
    (is (contains? found "beads")
        "AGENTS.md names .agents/skills/beads/SKILL.md; it must be loadable")))
