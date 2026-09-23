;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.agent.outline
  "A file's definitions with the line ranges they span (karamazov-d5wo.10).

  Reading was ~70% of tool-result characters across the campaign dbs, and
  most of it whole files. An outline is what a model needs to ask for the
  range it wants instead: `read_file({path, outline: true})` returns it, and
  an offset and limit read the one definition. Pure; what counts as a
  definition is gates.edn :outline :patterns, one regex per language shape
  whose first group is the name."
  (:require [clojure.string :as str]))

(defn outline
  "Every line of `text` a pattern in `patterns` matches, as
  [{:line :end :name}] (1-based, inclusive). A definition runs to the line
  before the next one; the last runs to the end of the file."
  [text patterns]
  (let [lines (str/split-lines (str text))
        res (mapv re-pattern patterns)
        hits (vec (keep-indexed
                   (fn [i l]
                     (some (fn [re] (when-let [m (re-find re l)]
                                      {:line (inc i) :name (if (vector? m) (second m) m)}))
                           res))
                   lines))]
    (vec (map-indexed (fn [k h]
                        (assoc h :end (if-let [nxt (get hits (inc k))]
                                        (dec (:line nxt))
                                        (count lines))))
                      hits))))
