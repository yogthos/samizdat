;; Built on sdiff, https://github.com/semantic-namespace/diff (commit
;; 70e436e), MIT License, Copyright (c) 2026 @tangrammer: find-form and
;; find-change from its review.clj, turned from naming a CHANGE in a report
;; to naming a PLACE in source (karamazov-0e2c.14). Paths are the ones its
;; diff prints, built from the same children-ids.
;;
;; SPDX-License-Identifier: MIT

(ns samizdat.sdiff.address
  "Naming a top-level form, and a place inside it, by the shortest thing that
  is unambiguous: its id (`defn route`) or, when unique, its bare name
  (`route`); then a path inside it as sdiff prints one (`body › binding cap`,
  `arity 2 › let 2 › binding cap`) or any end of that path no other place in
  the form shares (`binding cap`). A miss says what there is; an ambiguous
  end names every place it fits — nothing is guessed."
  (:require [clojure.string :as str]
            [rewrite-clj.node :as n]
            [samizdat.sdiff.core :as core]))

(defn- id-str [id] (str/join " " (remove nil? (map str id))))

(defn- step-str [step] (str/join " " (map str step)))

(defn- labelled-children
  "A node's children as [[step child] …], labelled the way sdiff's diff
  labels them: by role where children-ids has one, else by position."
  [node]
  (let [ids (core/children-ids node)]
    (if (#{::core/lcs ::core/seq} ids)
      (let [hd (core/head node)
            xs (if hd (vec (rest (core/kids node))) (core/kids node))]
        (map-indexed (fn [i k]
                       [[(cond (core/head k) (str (core/head k) (when (> (count xs) 1) (str " " (inc i))))
                               (seq (core/kids k)) (str (name (n/tag k)) " " (inc i))
                               :else (core/short (n/string k)))]
                        k])
                     xs))
      ids)))

(defn- places
  "Every [path node] under `node`, path a vector of printed steps."
  [node]
  (letfn [(walk [path nd]
            (cons [path nd]
                  (when (seq (core/kids nd))
                    (mapcat (fn [[step child]] (walk (conj path (step-str step)) child))
                            (labelled-children nd)))))]
    (rest (walk [] node))))

(defn- rows [node] (let [{:keys [row end-row]} (meta node)] (when row [row end-row])))

(defn find-form
  "The top-level form `spec` names in `src`: its id as printed, or a bare
  name only one form carries. {:node :id} or {:error :none|:ambiguous …}."
  [src spec]
  (let [spec (str/trim (str spec))
        forms (map (fn [f] {:node f :id (id-str (core/top-id f))}) (core/forms src))
        exact (filter #(= spec (:id %)) forms)
        named (filter #(some #{spec} (map str (core/top-id (:node %)))) forms)]
    (cond
      (= 1 (count exact)) (first exact)
      (= 1 (count named)) (first named)
      (seq named) {:error :ambiguous :candidates (mapv :id named)}
      :else {:error :none :forms (mapv :id forms)})))

(defn find-place
  "The place in `form` that `at` names: a whole printed path, or an end of
  one no other place shares. {:node :path} or {:error …}."
  [form at]
  (let [at (str/replace (str/trim (str at)) #"^(‥|…|\.\.)\s*›\s*" "")
        all (places (core/unwrap form))
        fmt #(str/join " › " %)
        exact (filter #(= at (fmt (first %))) all)
        ends (filter (fn [[p _]] (some #(= at (fmt (drop % p))) (range 1 (count p)))) all)]
    (cond
      (= 1 (count exact)) {:node (second (first exact)) :path (fmt (ffirst exact))}
      (= 1 (count ends)) {:node (second (first ends)) :path (fmt (ffirst ends))}
      (seq ends) {:error :ambiguous :candidates (mapv (comp fmt first) ends)}
      :else {:error :none :places (mapv (comp fmt first) (take 40 all))})))

(defn locate
  "Where `form-spec` (and `at` inside it, when given) is in `src`, as
  {:rows [first-line last-line] :id :path} or the {:error …} that says why
  not, with what there is to choose from."
  ([src form-spec] (locate src form-spec nil))
  ([src form-spec at]
   (let [{:keys [node id error] :as f} (find-form src form-spec)]
     (cond
       error f
       (str/blank? (str at)) {:rows (rows node) :id id}
       :else (let [{pnode :node :keys [path] :as p} (find-place node at)]
               (if (:error p) (assoc p :id id) {:rows (rows pnode) :id id :path path}))))))
