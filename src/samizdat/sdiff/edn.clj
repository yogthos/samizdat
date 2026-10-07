;; Ported from sdiff, https://github.com/semantic-namespace/diff (commit
;; 70e436e), MIT License, Copyright (c) 2026 @tangrammer. Copied as written
;; and adapted only where samizdat needs it (karamazov-0e2c); the original
;; notice is in docs/third-party/sdiff-LICENSE.
;;
;; SPDX-License-Identifier: MIT

(ns samizdat.sdiff.edn
  "The report as plain data. rewrite-clj nodes become `{:src :row :col :end-row :end-col}`
  so a consumer can print the source, anchor a review comment to a head-side line,
  or re-read the form. File-level sources are dropped; each form carries its own."
  (:require [rewrite-clj.node :as n]
            [samizdat.sdiff.core :as core]))

(defn node->edn [node]
  (when node
    (let [{:keys [row col end-row end-col]} (meta node)]
      (cond-> {:src (n/string node)}
        row (assoc :row row :col col :end-row end-row :end-col end-col)))))

(defn- pair->edn [kept] (mapv (fn [[o nw]] [(node->edn o) (node->edn nw)]) kept))

(defn change->edn [c]
  (into {} (for [[k v] c :when (some? v)]
             [k (case k
                  (:old :new) (node->edn v)
                  :kept (pair->edn v)
                  v)])))

(defn extraction->edn [{:keys [drift kept] :as e}]
  (cond-> (dissoc e :kept)
    true (update :drift #(mapv change->edn %))
    kept (assoc :kept (pair->edn kept))))

(defn form->edn [{:keys [old new]} {:keys [id was changes extraction] :as f}]
  (let [op0 (:op (first changes))]
    (cond-> (assoc f :changes (mapv change->edn changes))
      (not= op0 :added-form)   (assoc :old (node->edn (get (core/index old) (or was id))))
      (not= op0 :removed-form) (assoc :new (node->edn (get (core/index new) id)))
      extraction (assoc :extraction (extraction->edn extraction)))))

(defn file->edn [f]
  (-> f
      (dissoc :old :new)
      (assoc :forms (mapv (partial form->edn f) (:forms f)))))

(defn report->edn [r] (update r :clj #(mapv file->edn %)))
