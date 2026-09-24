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

(ns samizdat.tui.markdown
  "A reply's markdown as rows, the way dirge draws it: headings bold, bullets
  as •, code indented and set apart, quotes behind a bar. Block structure
  only — inline emphasis markers are dropped, since a styled span inside a
  wrapped line would stop it wrapping. Pure: hiccup rows for the widgets."
  (:require [clojure.string :as str]))

(defn- row
  ([text] (row nil text))
  ([class text] [:wrapped (cond-> {} class (assoc :class class)) text]))

(defn- inline [s]
  (-> s
      (str/replace #"\*\*(.+?)\*\*" "$1")
      (str/replace #"__(.+?)__" "$1")))

(defn- block-row [line]
  (condp re-find line
    #"^#\s+(.*)" :>> #(row :md-h1 (inline (second %)))
    #"^#{2,6}\s+(.*)" :>> #(row :md-h2 (inline (second %)))
    #"^\s*[-*+]\s+(.*)" :>> #(row (str "  • " (inline (second %))))
    #"^\s*(\d+)[.)]\s+(.*)" :>> #(row (str " " (nth % 1) ". " (inline (nth % 2))))
    #"^>\s?(.*)" :>> #(row :md-quote (str "│ " (inline (second %))))
    (row (inline line))))

(defn lines
  "`text` as a vector of rows."
  [text]
  (loop [[l & more :as ls] (str/split-lines (str text)) code? false out []]
    (cond
      (empty? ls) out
      (str/starts-with? (str/triml l) "```") (recur more (not code?) out)
      code? (recur more code? (conj out (row :md-code (str "  " l))))
      :else (recur more code? (conj out (block-row l))))))
