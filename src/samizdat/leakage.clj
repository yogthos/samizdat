;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.leakage
  "WHETHER A PROJECT'S EDIT IS FIT TO PROMOTE into the shipped templates
  (karamazov-na2k.13).

  Inside a project an edit that names the project is the point: the workflow
  is the project's. Promoting it into resources/ changes every project, and
  there RRSI's critic applies (2609.24972, rrsi/critic.py): an edit that
  encodes the task it was made on is a fit to that task, not a mechanism,
  and an edit that removes a safety mechanism without a replacement is a
  regression that happened to score. This is the deterministic half of that
  screen — pure over the edit's text and its template, flags only. Deciding
  is the person promoting it."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]))

(defn- read-edn [s] (try (edn/read-string (str s)) (catch Throwable _ nil)))

(defn- guards
  "The guards a body carries, by kind: a policy table's gates, a manifest's
  declared invariants, a phases table's refusals."
  [kind text]
  (let [v (read-edn text)]
    (when (map? v)
      (case (str kind)
        "policy" (set (concat (keep #(some-> (:gate %) str) (:gates v))
                              (keep #(some-> (:refusal %) str) (:refusals v))))
        "manifest" (set (map pr-str (:invariants v)))
        nil))))

(defn screen
  "Flags for promoting `edited` over `template`: {:names-subject [terms the
  edit introduces] :drops-guard [guards the template had and the edit lost]}.
  `subject-terms` are the project's own words — namespace prefixes, task ids,
  file names — which the caller knows and this namespace does not."
  [{:keys [kind template edited subject-terms]}]
  (let [t (str template) e (str edited)
        introduced (vec (filter #(and (str/includes? e %) (not (str/includes? t %)))
                                (map str subject-terms)))
        before (guards kind t)
        after (guards kind e)]
    {:names-subject introduced
     :drops-guard (if (and before after) (vec (sort (remove after before))) [])}))

(defn clean? [flags]
  (and (empty? (:names-subject flags)) (empty? (:drops-guard flags))))
