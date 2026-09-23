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

(ns samizdat.tui.commands
  "Slash commands, read from tui.edn `:commands`.

  Each entry names what the command DOES — an action the core implements
  (:model, :mode, :intervene …) — and how it reads: its arguments and its
  help line. `{:alias \"/model\"}` makes a second name for one. So renaming
  a command, adding an alias, or turning a steer kind into a command of its
  own is an edit to tui.edn, not a rebuild; what an action does is the
  core's (samizdat.tui.core).

  Pure: parsing, completion and help, over the command map."
  (:require [clojure.string :as str]))

(defn- resolve-name [n commands]
  (loop [n n, seen #{}]
    (let [c (get commands n)]
      (if (and (:alias c) (not (contains? seen n)))
        (recur (:alias c) (conj seen n))
        [n c]))))

(defn parse
  "A line typed into the compose box, as a command: {:name :do :arg …} plus
  the entry's own keys, {:error …} for a slash nothing names, or nil when
  the line is not a command at all."
  [text commands]
  (let [t (str/trim (str text))]
    (when (str/starts-with? t "/")
      (let [[n rest-of] (str/split t #"\s+" 2)
            [n c] (resolve-name n commands)]
        (if (:do c)
          (assoc c :name n :arg (some-> rest-of str/trim not-empty))
          {:error (str "unknown command " n " — /help lists them")})))))

(defn candidates
  "The commands whose names start with what has been typed, while only the
  name is being typed. Aliases are left out: they are the command they name."
  [text commands]
  (let [t (str text)]
    (when (and (str/starts-with? t "/") (not (re-find #"\s" t)))
      (->> commands
           (remove (comp :alias val))
           (filter #(str/starts-with? (key %) t))
           (sort-by key)
           (mapv (fn [[n c]] (assoc c :name n)))))))

(defn- common-prefix [ss]
  (reduce (fn [a b]
            (subs a 0 (count (take-while true? (map = a b)))))
          ss))

(defn complete
  "What Tab makes of `text`: one match completes to it and a space; several
  complete to what they share; anything else is left as it is."
  [text commands]
  (let [cs (mapv :name (candidates text commands))]
    (case (count cs)
      0 text
      1 (str (first cs) " ")
      (let [p (common-prefix cs)] (if (> (count p) (count text)) p text)))))

(defn usage [{:keys [name args doc]}]
  (str name (when args (str " " args)) (when doc (str " — " doc))))

(defn help-lines
  "Every command, one line each: its name, arguments and what it does."
  [commands]
  (->> commands
       (remove (comp :alias val))
       (sort-by key)
       (mapv (fn [[n c]] (str "  " (usage (assoc c :name n)))))))
