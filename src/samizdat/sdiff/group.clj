;; Ported from sdiff, https://github.com/semantic-namespace/diff (commit
;; 70e436e), MIT License, Copyright (c) 2026 @tangrammer: components, hubs
;; and split-hubs copied as written. Adapted (karamazov-0e2c.17): clj-kondo
;; does not run under jolt, so the call edges are read from the source — a
;; changed form calls another when it names that form's var, bare in the
;; same namespace or through the namespace's alias or full name elsewhere —
;; and the decorate grouper registry is left out (a global plugin atom).
;; The section titles are prose and live in prompts/change-groups.md.
;;
;; SPDX-License-Identifier: MIT

(ns samizdat.sdiff.group
  "Groupings of a report's changed forms by the calls between them, hubs
  (forms used from three or more other files) set apart so they do not join
  every caller into one group. Derived from the code, never from a reading
  of it."
  (:require [clojure.string :as str]
            [rewrite-clj.node :as n]
            [samizdat.prompt :as prompt]
            [samizdat.sdiff.core :as core]))

(defn- form-id [form] (str/join " " (remove nil? (map str (:id form)))))

(defn nodes
  "`[path form-id]` of every changed form in a file that changes behaviour."
  [report]
  (vec (for [f (:clj report) :when (= :semantic (:verdict f)) form (:forms f)]
         [(:path f) (form-id form)])))

(defn- ns-info
  "The ns name and {alias full} of a file's source."
  [src]
  (let [ns-name (some->> (re-find #"\(ns\s+(?:\^\S+\s+)*([\w.\-]+)" (str src)) second)
        aliases (into {} (for [[_ lib a] (re-seq #"\[([\w.\-]+)\s+(?:[^\[\]]*?\s)?:as\s+([\w.\-]+)" (str src))]
                           [a lib]))]
    {:ns ns-name :aliases aliases}))

(defn- symbols [node]
  (if (seq (core/kids node))
    (mapcat symbols (core/kids node))
    (when (= :token (n/tag node)) [(n/string node)])))

(defn- side-call-edges
  "Edges between changed forms on one side, read from the source."
  [report side]
  (let [files (for [f (:clj report) :when (and (= :semantic (:verdict f)) (seq (side f)))] f)
        info (into {} (for [f files] [(:path f) (ns-info (side f))]))
        index (into {} (for [f files] [(:path f) (core/index (side f))]))
        ;; [ns var-name] -> node key, for the changed forms that define one
        defs (into {} (for [f files form (:forms f)
                            :let [node (get-in index [(:path f) (if (= side :old) (or (:was form) (:id form)) (:id form))])
                                  nm (some-> (second (core/kids (core/unwrap node))) n/string)]
                            :when (and node nm (re-matches #"[^\s()\[\]{}\"]+" nm))]
                        [[(:ns (info (:path f))) nm] [(:path f) (form-id form)]]))]
    (set (for [f files form (:forms f)
               :let [from [(:path f) (form-id form)]
                     node (get-in index [(:path f) (if (= side :old) (or (:was form) (:id form)) (:id form))])
                     {:keys [ns aliases]} (info (:path f))]
               :when node
               sym (distinct (symbols (core/unwrap node)))
               :let [[q nm] (if-let [[_ q nm] (re-matches #"([\w.\-]+)/(.+)" sym)] [q nm] [nil sym])
                     target-ns (if q (get aliases q q) ns)
                     to (defs [target-ns nm])]
               :when (and to (not= to from))]
           #{from to}))))

(defn call-edges
  "Pairs of changed forms where one calls a var the other defines, before or
  after the change, so a removed function stays with its former callers."
  [report]
  (into (side-call-edges report :new) (side-call-edges report :old)))

(defn components
  "Connected components of `nodes` under `edges` (sets of two nodes), largest first."
  [nodes edges]
  (let [adj (reduce (fn [m e] (let [[a b] (seq e)] (-> m (update a (fnil conj #{}) b) (update b (fnil conj #{}) a)))) {} edges)]
    (loop [left (vec nodes) seen #{} out []]
      (if-let [n (first left)]
        (if (seen n)
          (recur (rest left) seen out)
          (let [c (loop [q [n] c #{n}]
                    (if-let [x (first q)]
                      (let [nx (remove c (adj x))] (recur (into (vec (rest q)) nx) (into c nx)))
                      c))]
            (recur (rest left) (into seen c) (conj out (filterv c nodes)))))
        (sort-by (comp - count) out)))))

(defn test-path? [path] (boolean (re-find #"(^|/)test/|_test\.clj" path)))

(defn hubs
  "Nodes whose neighbours lie in at least `files` other source files: shared
  code such as a constant or a dispatcher, which would otherwise join every
  caller into one group. Tests do not count, since calling from many test
  files is what tested code is for."
  [edges files]
  (let [adj (reduce (fn [m e] (let [[a b] (seq e)] (-> m (update a (fnil conj #{}) b) (update b (fnil conj #{}) a)))) {} edges)]
    (set (for [[n ns] adj :when (>= (count (disj (set (remove test-path? (map first ns))) (first n))) files)] n))))

(defn split-hubs
  "`[groups shared]`: components of `nodes` once hubs no longer link them, and the hubs."
  [nodes edges]
  (let [hs (hubs edges 3)]
    [(components (remove hs nodes) (remove #(some hs %) edges)) (filterv hs nodes)]))

(defn- hub [members edges]
  (first (sort-by (fn [m] [(- (count (filter #(contains? % m) edges))) (str m)]) members)))

(defn by-calls
  "{:groups [[node …] …] :shared [node …] :singles [node …] :edges #{…}}."
  [report]
  (let [ns (nodes report) es (call-edges report)
        [comps shared] (split-hubs ns es)
        {groups true singles false} (group-by #(> (count %) 1) comps)]
    {:groups (vec groups) :shared shared :singles (vec (apply concat singles)) :edges es}))

(defn outline-text
  "The grouping as the text prompts/change-groups.md makes of it."
  [{:keys [groups shared singles edges]}]
  (let [m (fn [[p id]] (str p " · " id))]
    (prompt/render "change-groups"
                   {:groups (for [g groups :let [[_ id] (hub g edges)]]
                              {:hub id :count (count g) :files (count (distinct (map first g)))
                               :members (map m g)})
                    :shared (seq (map m shared))
                    :singles (seq (map m singles))})))
