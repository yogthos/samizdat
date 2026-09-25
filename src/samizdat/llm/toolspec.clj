;; samizdat - a claim-first verification harness
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

(ns samizdat.llm.toolspec
  "Native tool specs — the `tools` array of an OpenAI-compatible request —
  read off the tool documentation a branch was actually given.

  WHY NATIVE AT ALL. The harness's protocol is a fenced ```tool-call block in
  the text, and the tools used to travel natively only when a gate forced
  one. DeepSeek's and Zhipu's own harnesses send `tools` on every request and
  parse nothing out of text (2026-09-24 review); with no `tools` in a
  request, deepseek-v4-flash wrote its trained call format, DSML, into the
  content instead. Measured the same day: with `tools` present, both
  deepseek-v4-flash and glm-5.3 answered with native tool_calls — even with
  fenced calls in the history and a system prompt asking for fences.

  WHY FROM THE PROMPT. Every tool is documented to the model as a signature
  line — `edit_file({path, old_text, new_text, replace_all?})` — followed by
  an indented description, in resources/prompts. That prose is the single
  source: userspace, editable at runtime, and exactly the set this branch's
  role was shown. A second hand-written schema per tool would drift from it.
  So a derived spec names the arguments and REQUIRES NONE: the prose says
  which are required, the tool checks, and a schema that wrongly required one
  would have the model inventing a value to satisfy it. Where gates.edn
  :forceable-tools holds an exact schema, that one is sent instead.

  Pure: text in, specs out."
  (:require [clojure.string :as str]))

(def ^:private signature-re
  ;; A signature is a whole line: a tool name, then `({args})` or `()`.
  ;; Anchored so a mention inside a sentence — `call done({answer}) when…` —
  ;; is not a second definition.
  #"(?m)^([a-z_][a-z0-9_]*)\((?:\{([^}]*)\})?\)\s*$")

(defn- clip
  "`s` cut at the last sentence end within `limit` characters, or at the
  limit; whole when `limit` is nil."
  [s limit]
  (if (or (nil? limit) (<= (count s) limit))
    s
    (let [head (subs s 0 limit)
          end (str/last-index-of head ". ")]
      (if (and end (pos? end)) (subs head 0 (inc end)) (str (str/trimr head) "…")))))

(defn- description
  "The indented lines right after a signature, joined — up to the first line
  that is not indented — and clipped to `limit`."
  [lines limit]
  (clip (->> lines
             (take-while #(re-find #"^\s+\S" %))
             (map str/trim)
             (str/join " "))
        limit))

(defn- arg-names [args]
  (->> (str/split (str args) #",")
       (map str/trim)
       (remove #(or (str/blank? %) (= "..." %)))
       (map #(str/replace % #"\?$" ""))
       (filter #(re-matches #"[A-Za-z_][A-Za-z0-9_]*" %))))

(defn signatures
  "Every tool signature documented in `text`, first definition of each name
  kept, as {:name :description :parameters}. `limit` caps a description's
  characters (nil: whole) — it rides in every request's tools array, and the
  full prose is in the system prompt already."
  ([text] (signatures text nil))
  ([text limit]
   (let [lines (str/split-lines (str text))]
     (->> (map-indexed vector lines)
          (keep (fn [[i line]]
                  (when-let [[_ nm args] (re-matches signature-re line)]
                    {:name nm
                     :description (description (drop (inc i) lines) limit)
                     :parameters {:type "object"
                                  :properties (into {}
                                                    (map (fn [a] [(keyword a) {}]))
                                                    (arg-names args))}})))
          (reduce (fn [acc s] (if (some #(= (:name s) (:name %)) acc) acc (conj acc s))) [])))))

(defn for-messages
  "The tool specs for a request whose history is `messages`: the signatures
  in its SYSTEM messages — the documentation the branch was given — with
  `exact` (name -> spec, gates.edn :forceable-tools) taking precedence.
  Sorted by name, so the array, which lands in the cached prefix, is the same
  bytes every turn. `limit` as for `signatures`."
  ([messages exact] (for-messages messages exact nil))
  ([messages exact limit]
   (let [doc (->> messages
                  (filter #(= "system" (str (:role %))))
                  (map #(str (:content %)))
                  (str/join "\n"))
         derived (signatures doc limit)]
     (->> derived
          (map (fn [s] (or (get exact (:name s)) s)))
          (sort-by :name)
          vec))))
