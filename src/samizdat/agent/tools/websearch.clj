;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.agent.tools.websearch
  "The `websearch` tool. Mechanism is samizdat.agent.websearch; this is the
  dispatch, the budget read, and the branch-facing words."
  (:require [samizdat.agent.tools.base :as base]
            [samizdat.agent.websearch :as ws]
            [samizdat.lexicon :as lexicon]
            [samizdat.prompt :as prompt]))

(defn- msg [ctx] (prompt/render "websearch-tool" ctx))

(defmethod base/run-tool "websearch" [{:keys [branch] :as ctx}]
  (if-let [m (base/missing ctx :query)]
    (base/malformed branch m)
    (let [query (str (base/arg ctx :query))
          n (some-> (base/arg ctx :num_results) str parse-long)
          chars (lexicon/budget :websearch-chars)
          {:keys [ok error]} (ws/search query n
                                        {:api-key (System/getenv "EXA_API_KEY")})]
      (cond
        error (base/fail branch (msg {:failed true :detail error}))
        (clojure.string/blank? (str ok)) (base/ok branch (msg {:empty true :query query}))
        :else (base/ok branch (ws/format-results ok {:chars chars}))))))
