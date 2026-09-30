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

(ns samizdat.agent.checklist
  "The ship checklist (karamazov-dsfx): what an answer must account for,
  item by item.

  WHY. A requirement could ship by being left out of the answer. The lexical
  rungs catch an answer that CONFESSES unfinished work; an answer that is
  simply silent on a requirement passed every one, and the critic caught it a
  whole round (~90 turns) later. Deciding from prose that a requirement went
  unaddressed is a judgement — measured 2026-09-29 with lev on 50 real
  cases: the BERT encoders never once answered 'absent', and Qwen3.5-4B
  called four of five silent answers 'met' at 0.95-1.0 confidence.

  So the judgement is removed rather than made. The checklist is data that
  exists before the work — the operator's acceptance criteria, the held
  task's tests, what the branch declared with `plan`, the problem's own list
  items — and `done` must carry one entry per item, by id: met, not met, or
  not applicable, each with a reason. Silence becomes a missing key. Whether
  a `met` is true stays with the evidence rungs and the critic, who read the
  entries on the shipped answer.

  MECHANISM ONLY. Which sources apply to which branch is the done handler's
  and gates.edn :ship-checklist's; the wording is prompts/checklist-*.md."
  ;; samizdat.prompt first: it loads jolt.time, which data.json needs.
  (:require [samizdat.prompt :as prompt]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [samizdat.agent.gates :as gates]
            [samizdat.agent.verify :as verify]))

(defn policy [] (gates/threshold :ship-checklist))

;; --- the items ----------------------------------------------------------------

(defn problem-items
  "The list items of a problem statement, in order: each line policy's
  :list-item-regex matches, its first group trimmed. Lines inside a fenced
  code block are code, not requirements."
  [text]
  (let [re (re-pattern (:list-item-regex (policy)))]
    (loop [[line & more :as lines] (str/split-lines (str text)) fenced? false out []]
      (cond
        (empty? lines) out
        (str/starts-with? (str/triml line) "```") (recur more (not fenced?) out)
        fenced? (recur more fenced? out)
        :else (recur more fenced?
                     (if-let [[_ item] (re-find re line)]
                       (conj out (str/trim item))
                       out))))))

(defn- first-paragraph [s] (str/trim (first (str/split (str s) #"\n\s*\n"))))

(defn- problem-list
  "`problem`'s list items; with `whole?` and none, its first paragraph as the
  one item. A board piece's contract is usually a single line — the RFC's
  work item — and then the contract itself is what the piece owes (run
  582980ef: three pieces shipped owing nothing)."
  [problem whole?]
  (let [its (when problem (problem-items problem))]
    (cond (seq its) its
          (and whole? (not (str/blank? (str problem)))) [(first-paragraph problem)]
          :else [])))

(defn items
  "Every item owed, as `[{:id :text :source}]`, ids by source — a1.. the
  acceptance criteria, t1 the held task's tests, c1.. what `plan` declared,
  p1.. the list items of what the branch was asked (the run's problem, or a
  board piece's task contract). Every argument is optional; the caller
  passes only the sources that apply to the branch.

  `whole-problem?` makes a problem with no list items one item, its first
  paragraph: set for a branch that works a task, whose problem is a contract.

  A task's :tests that is a test PATH is not an item: it is judged by running
  it (the verify rung), not by the answer saying so."
  [{:keys [acceptance task-tests declared problem whole-problem?]}]
  (let [tag (fn [prefix source texts]
              (map-indexed (fn [i t] {:id (str prefix (inc i)) :text t :source source}) texts))
        ;; Its first paragraph: the board writes a standing paragraph of
        ;; testing guidance here, and the item is its first sentence group,
        ;; not the essay after it.
        task (when-let [t (some-> task-tests str str/trim not-empty)]
               (when-not (verify/test-file? t)
                 [(first-paragraph t)]))]
    (vec (concat (tag "a" :acceptance (map #(if (= :judge (:kind %)) (:text %) (:name %)) acceptance))
                 (tag "t" :task task)
                 (tag "c" :declared (remove str/blank? (map str declared)))
                 (tag "p" :problem (problem-list problem whole-problem?))))))

;; --- the answer's entries -----------------------------------------------------

(defn- status-of
  "`s` as :met / :not-met / :n-a by policy's :statuses, or nil."
  [s]
  (let [k (-> (str s) str/trim str/lower-case (str/replace #"[\s_]+" " "))]
    (some (fn [[status words]] (when (contains? (set words) k) status))
          (:statuses (policy)))))

(defn- field [m & ks]
  (some #(let [v (or (get m %) (get m (name %)))] (when (some? v) v)) ks))

(defn entries
  "The `checklist` argument of `done` as `{id {:status :evidence}}`, id
  lower-cased. Read leniently: a vector of `{item|id, status, evidence|note|
  reason}` maps, a map keyed by id, or either as a JSON string. Anything else
  is no entries — never an error, since the missing items are what the
  refusal will name. A status off the menu is kept as nil so it counts as
  unaccounted rather than vanishing."
  [raw]
  (let [raw (if (string? raw) (try (json/read-str raw) (catch Throwable _ nil)) raw)
        one (fn [id m]
              (let [m (if (map? m) m {:status m})]
                [(str/lower-case (str/trim (str id)))
                 {:status (status-of (field m :status))
                  :evidence (str/trim (str (or (field m :evidence :note :reason) "")))}]))]
    (cond
      (map? raw) (into {} (map (fn [[k v]] (one (name k) v))) raw)
      (sequential? raw) (into {} (keep #(when (map? %)
                                          (when-let [id (field % :item :id)] (one id %))))
                              raw)
      :else {})))

(defn- plain [s] (-> (str s) str/trim str/lower-case (str/replace #"[.\s]+$" "")))

(defn- entry-for
  "`item`'s entry: by its id, else by its exact text — case and a closing
  full stop aside. Run 6e3eda8a's model named items by sentence; a
  paraphrase is still not the item."
  [entries {:keys [id text]}]
  (or (get entries (str/lower-case id))
      (some (fn [[k e]] (when (= (plain k) (plain text)) e)) entries)))

(defn unaccounted
  "The items with no entry that has a known status and a non-blank reason."
  [items entries]
  (filterv (fn [item]
             (let [{:keys [status evidence]} (entry-for entries item)]
               (or (nil? status) (str/blank? evidence))))
           items))

;; --- rendering ----------------------------------------------------------------

(defn- rows [items entries]
  (mapv (fn [{:keys [id text] :as item}]
          (let [{:keys [status evidence]} (entry-for entries item)]
            {:id id :text text :status (some-> status name) :evidence evidence
             :label (get (:labels (policy)) status "?")}))
        items))

(defn render
  "The accounted checklist as text to append to the shipped answer."
  [items entries]
  (prompt/render "checklist-answer" {:rows (rows items entries)}))

(defn refusal
  "The ship refusal naming each unaccounted item."
  [missing]
  (prompt/render "checklist-missing" {:missing missing}))

(defn record
  "The checklist as a journal value: items, and entries as a vector (a
  journal row is JSON, and ids as keys would come back as keywords)."
  [items entries]
  {:items items
   :entries (mapv (fn [[id e]] (assoc e :id id :status (some-> (:status e) name))) entries)})
