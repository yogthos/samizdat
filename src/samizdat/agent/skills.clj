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

(ns samizdat.agent.skills
  "Skills: instruction bundles the agent loads on demand, so a guide costs
  context only when it is needed. A skill is a markdown file whose first
  meaningful line is its one-line description (the catalogue entry) and whose
  body is the guidance loaded on request. Bundled skills live in
  resources/skills/; a project can add or override them in .samizdat/skills/,
  first match by directory order — the same layering as cells and config."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [samizdat.lexicon :as lexicon]))

(def shipped-skills
  "Basenames of the skills that ship, ENUMERATED not globbed — a classpath has
  no directory listing and an embedded resource has no filesystem path, so a
  dir scan found nothing inside a built binary or from any cwd but the
  samizdat repo itself: every implementor silently lost the REPL/TDD guidance
  and the catalogue rendered empty (karamazov-blt.33). Same reasoning as
  cells/shipped-cells; pinned against resources/skills by skills-test."
  ["clojure-style" "mycelium" "repl-workflow"])

(defn skill-roots
  "The project-relative directories a skill may live in, in precedence order —
  wordlists.edn `:skill-roots`, earliest wins.

  A LIST, and policy rather than a literal here, because samizdat is not the
  only harness that leaves skills on disk. Claude Code, pi, opencode and the
  agents standard all write `<root>/<name>/SKILL.md`, and which of those roots
  a given project honours is exactly the sort of thing the standing rule says
  the agent should be able to change without a rebuild. Adapted from vis's
  ordered cross-harness registry (resources/vis-docs/skills.md), whose framing
  is the right one: reading another harness's skills is a compatibility layer,
  not a format."
  []
  (lexicon/wordlist :skill-roots))

(def default-dirs
  "The project-overlay directories scanned in addition to the shipped
  resources. Relative to the process cwd; a caller working on another project
  passes `(project-dirs root)` instead."
  (skill-roots))

(defn project-dirs
  "Where a project's own skills live, under the RUN's root — not the
  harness's cwd (karamazov-blt.33)."
  [root]
  (mapv #(str (io/file (str (or root ".")) (str %))) (skill-roots)))

(def ^:private skill-doc
  "The filename a skill DIRECTORY carries its instructions in. Every other
  harness spells it this way, so reading theirs costs nothing."
  "SKILL.md")

(defn- skill-files
  "The skill sources in `dir`, as [name file] pairs, in stable name order.

  TWO SHAPES, because a skill on disk has two conventions and only one of them
  was ever read here (karamazov-15h):
    <dir>/<name>.md          — samizdat's own flat form
    <dir>/<name>/SKILL.md    — a directory per skill, plus its bundled
                               resources; what every other harness writes,
                               and what this repo's own .agents/skills holds
  A directory's name is the fallback skill name, so a SKILL.md with no
  frontmatter `name` still lands under something meaningful."
  [dir]
  (let [d (io/file dir)]
    (when (.isDirectory d)
      (->> (.listFiles d)
           (keep (fn [^java.io.File f]
                   (cond
                     (and (.isFile f) (str/ends-with? (.getName f) ".md"))
                     [(str/replace (.getName f) #"\.md$" "") f]

                     (.isDirectory f)
                     (let [doc (io/file f skill-doc)]
                       (when (.isFile doc) [(.getName f) doc])))))
           (sort-by first)))))

(defn parse-frontmatter
  "Split a skill file into {:meta {..} :body ..}. A leading `---` fenced block
  is parsed as simple `key: value` lines (name, description); the rest is the
  body. No frontmatter -> empty meta and the whole file as body."
  [content]
  (let [content (str content)]
    (if-let [m (re-find #"(?s)\A---\s*\n(.*?)\n---\s*\n?(.*)\z" content)]
      (let [meta (into {}
                       (keep (fn [line]
                               (when-let [[_ k v] (re-find #"^\s*([A-Za-z_-]+)\s*:\s*(.+?)\s*$" line)]
                                 [(keyword (str/lower-case k)) v]))
                             (str/split-lines (nth m 1))))]
        {:meta meta :body (str/trim (nth m 2))})
      {:meta {} :body (str/trim content)})))

(defn- describe
  "A skill's one-line catalogue description — its trigger. The frontmatter
  `description` (write it as \"Use when …\"), else the first non-heading line,
  else nil so the skill drops from the catalogue but stays loadable."
  [fm name]
  (or (not-empty (str/trim (str (get-in fm [:meta :description]))))
      (some #(when-not (or (str/blank? %) (str/starts-with? (str/trim %) "#"))
               (str/trim %))
            (str/split-lines (:body fm)))))

(defn- shipped-entries
  "The bundled skills, read off the classpath so they resolve from a built
  binary and from any cwd."
  []
  (into {}
        (keep (fn [nm]
                (try
                  (when-let [r (io/resource (str "skills/" nm ".md"))]
                    (let [fm (parse-frontmatter (slurp r))
                          nm' (or (not-empty (str (get-in fm [:meta :name]))) nm)]
                      [nm' {:path (str "classpath:skills/" nm ".md")
                            :description (describe fm nm')
                            :body (:body fm)}]))
                  (catch Throwable _ nil))))
        shipped-skills))

(defn discover
  "skill-name -> {:path :description :body}. The shipped skills come off the
  classpath; `dirs` overlay them in order, so a project's .samizdat/skills
  wins over a bundled skill of the same name. Malformed files are skipped,
  never fatal."
  ([] (discover default-dirs))
  ([dirs]
   (reduce
    (fn [acc dir]
      (reduce (fn [m [nm ^java.io.File f]]
                (try
                  (let [fm (parse-frontmatter (slurp f))
                        nm (or (not-empty (str (get-in fm [:meta :name]))) nm)]
                    (assoc m nm {:path (.getPath f)
                                 :description (describe fm nm)
                                 :body (:body fm)}))
                  (catch Throwable _ m)))
              acc (skill-files dir)))
    (shipped-entries)
    dirs)))

(defn catalog
  "The always-on catalogue the agent sees: [{:name :description} ...], sorted.
  A skill with no description is omitted (it can still be loaded by name), so
  the visible list stays high-signal."
  ([] (catalog default-dirs))
  ([dirs]
   (->> (discover dirs)
        (keep (fn [[nm {:keys [description]}]]
                (when description {:name nm :description description})))
        (sort-by :name)
        vec)))

(defn render-catalog
  "The cheap, always-injected system-prompt block: names + trigger descriptions
  only, never bodies. Empty string when there are no skills."
  ([] (render-catalog default-dirs))
  ([dirs]
   (let [cat (catalog dirs)]
     (if (empty? cat)
       ""
       (str "Skills are guidance you load only when a task matches. Load one"
            " with `skill load {name}` when its description fits what you are"
            " about to do:\n"
            (str/join "\n" (for [{:keys [name description]} cat]
                             (str "- **" name "** — " description))))))))

(defn load-skill
  "The full body of a named skill (frontmatter stripped), or nil for a miss."
  ([name] (load-skill default-dirs name))
  ([dirs name]
   (get-in (discover dirs) [name :body])))
