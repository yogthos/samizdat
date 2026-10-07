;; Ported from sdiff, https://github.com/semantic-namespace/diff (commit
;; 70e436e), MIT License, Copyright (c) 2026 @tangrammer. Copied as written
;; and adapted only where samizdat needs it (karamazov-0e2c.16): clj-kondo
;; does not run under jolt, so `table` reads the ns forms, sdiff's own
;; fallback; the hiccup halves are left out.
;;
;; SPDX-License-Identifier: MIT

(ns samizdat.sdiff.names
  "Short names for one pull request. A path or namespace is shown by the
  shortest suffix no other path or namespace in the PR ends with; a namespace
  the PR's own files alias is shown by that alias. Two-segment namespaces, which
  is where registry dev-ids live, keep their full name. The table is
  deterministic, so a short name means the same thing everywhere on the page,
  and a legend maps each one back."
  (:require [clojure.string :as str]))

(defn- unique-suffixes
  "{full shortest-suffix} over `names` split by `sep-re` and joined by `sep`."
  [names sep-re sep]
  (let [parts (into {} (for [n names] [n (str/split n sep-re)]))
        suffix (fn [n k] (str/join sep (take-last k (parts n))))]
    (into {}
          (for [n names]
            [n (or (some (fn [k] (let [s (suffix n k)]
                                   (when (not-any? #(and (not= % n) (str/ends-with? (str sep %) (str sep s))) names) s)))
                         (range 1 (count (parts n))))
                   n)]))))

(def ^:private ns-form-re #"\(ns\s+(?:\^\S+\s+)*([\w.\-]+)")
(def ^:private alias-re #"\[([\w.\-]+)\s+(?:[^\[\]]*?\s)?:as\s+([\w.\-]+)")
(def ^:private qualified-re #"(?<![\w.\-:/])::?([a-zA-Z][\w\-]*(?:\.[\w\-]+){2,})/")


(defn- segments [ns] (count (str/split ns #"\.")))

(defn- choose
  "Short names: the alias the PR's source files give a namespace most often,
  else its shortest unique suffix. Test files' aliases (`SUT`, `sut`) are not
  names."
  [nss alias-of]
  (let [suffix (unique-suffixes (vec nss) #"\." ".")
        taken (atom #{})]
    (into {} (for [n (sort nss)
                   :let [a (alias-of n)
                         s (if (and a (not (@taken a)) (not (nss a))) a (suffix n))]]
               (do (swap! taken conj s) [n s])))))

(defn table
  "`{:paths {full short} :nss {full short}}` for a report. Namespaces and their
  aliases come from reading the files' `ns` forms (sdiff's fallback when
  clj-kondo is unavailable, which under jolt it always is)."
  [{:keys [clj other] :as report}]
  (let [test? (fn [f] (re-find #"(^|/)test/|_test\.clj" (:path f)))
        srcs-of (fn [fs] (for [f fs s [(:old f) (:new f)] :when (seq s)] s))
        srcs (srcs-of clj)
        own (set (keep #(second (re-find ns-form-re %)) srcs))
        alias-counts (fn [ss] (frequencies (for [s ss [_ lib a] (re-seq alias-re s)] [lib a])))
        best (fn [counts] (into {} (for [[lib pairs] (group-by ffirst counts)]
                                     [lib (ffirst (sort-by (fn [[a n]] [(- n) (- (count a)) a]) (map (fn [[[_ a] n]] [a n]) pairs)))])))
        alias-of (best (alias-counts (srcs-of (remove test? clj))))
        mentioned (set (for [s srcs [_ ns] (re-seq qualified-re s)] ns))
        nss (set (filter #(>= (segments %) 3) (concat own mentioned (keys alias-of))))
        ns-short (choose nss alias-of)
        paths (vec (distinct (concat (map :path clj) (map :path other))))]
    {:paths (into {} (remove (fn [[k v]] (= k v)) (unique-suffixes paths #"/" "/")))
     :nss (into {} (remove (fn [[k v]] (= k v)) ns-short))}))

(defn- replacer [{:keys [paths nss]}]
  (let [ps (sort-by (comp - count key) paths)
        ns (sort-by (comp - count key) nss)
        p-re (when (seq ps) (re-pattern (str "(?<![\\w/.\\-])(" (str/join "|" (map (comp java.util.regex.Pattern/quote key) ps)) ")(?![\\w/\\-])")))
        n-re (when (seq ns) (re-pattern (str "(?<![\\w.\\-])(" (str/join "|" (map (comp java.util.regex.Pattern/quote key) ns)) ")(?=/)")))]
    (fn [s]
      (cond-> s
        p-re (str/replace p-re #(get paths (second %) (first %)))
        n-re (str/replace n-re #(get nss (second %) (first %)))))))

(defn shorten-text [t s] (if (and t (string? s)) ((replacer t) s) s))

(defn used
  "The part of the table worth using in `text`: names that occur in it and save
  more characters there than their legend entry costs."
  [t text]
  (when t
    (let [pays? (fn [[full short]]
                  (let [n (count (re-seq (re-pattern (java.util.regex.Pattern/quote full)) text))]
                    (> (* n (- (count full) (count short))) (+ (count full) (count short) 5))))]
      {:paths (into {} (filter pays? (:paths t))) :nss (into {} (filter pays? (:nss t)))})))

(defn legend-text [{:keys [paths nss]}]
  (let [rows (concat (sort-by val nss) (sort-by val paths))]
    (when (seq rows)
      (str "names: " (str/join ", " (for [[full short] rows] (str short " = " full))) "\n"))))
