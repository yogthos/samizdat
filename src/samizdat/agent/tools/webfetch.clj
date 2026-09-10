;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.agent.tools.webfetch
  "The `webfetch` tool. Mechanism is samizdat.agent.webfetch; this is the
  dispatch, the argument read, and the branch-facing words — the same split
  `websearch` beside it follows."
  (:require [clojure.string :as str]
            [samizdat.agent.tools.base :as base]
            [samizdat.agent.webfetch :as wf]))

(defmethod base/run-tool "webfetch" [{:keys [branch] :as ctx}]
  ;; :neutral. Reading a page establishes nothing about the branch's line of
  ;; inquiry, exactly as read_file and grep do not — what it found may, and
  ;; that is for the branch to claim rather than for the fetch to assert.
  (if-let [m (base/missing ctx :url)]
    (base/malformed branch m)
    (let [url (str (base/arg ctx :url))
          fmt (some-> (base/arg ctx :format) str not-empty)
          secs (some-> (base/arg ctx :timeout) str parse-long)
          {:keys [ok error content-type]}
          (wf/fetch url {:format fmt
                         :timeout-ms (when secs (* 1000 secs))})]
      (cond
        error (base/fail branch error)
        (str/blank? (str ok)) (base/ok branch (str url " returned nothing."))
        :else (base/ok branch (str url
                                   (when (seq (str content-type))
                                     (str " (" content-type ")"))
                                   "\n\n" ok))))))
