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

(ns samizdat.security.replay
  "POLICY TRACE FILES (karamazov-3vu1.8, after OpenAPPA's .appa replays): a
  sequence of tool calls, each with the decision it should get, replayed
  against the real decisions with nothing run.

      {:about \"what this trace pins\"
       :grants [\"make *\"]                ; a person's grants, if any
       :configured [\"jolt test\"]         ; the run's configured commands
       :steps [{:tool \"webfetch\" :args {:url \"…\"} :expect :allow}
               {:tool \"shell\" :args {:command \"make\"} :expect :ask :gaps [:trust]}
               {:tool \"read_file\" :args {:path \"/elsewhere\"} :outside true
                :expect :allow}]}

  `:expect` is `:allow`, `:ask` (a person would be asked) or `:deny`;
  `:gaps`, when given, is checked too. A step that would run brings in what
  its tool brings in (`flow/delta`); `:outside true` marks a read from
  outside the project, which the replay cannot see from a path alone. A step
  that would wait or be refused brings in nothing.

  The shell's decision is `policy/decide` then `policy/with-flow`, the same
  two run-shell makes; every other tool's is `flow/gaps`, as tools/run-tool
  makes it. What a trace cannot pin is the filesystem: outside reads by a
  shell command, protected paths that exist only on disk.

      jolt policy-test [file-or-dir …]   ; test/policy by default

  exits 0 when every trace passes, 1 on a mismatch, 2 on a trace that cannot
  be read."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [samizdat.security.flow :as flow]
            [samizdat.security.policy :as policy]))

(def ^:private effects #{:allow :ask :deny})

(defn- step-error
  "Why step `s` cannot be replayed, as a keyword, or nil."
  [s]
  (cond (not (map? s)) :step-not-a-map
        (str/blank? (str (:tool s))) :no-tool
        (not (contains? effects (:expect s))) :expect-not-allow-ask-or-deny
        (and (= "shell" (str (:tool s))) (str/blank? (str (get-in s [:args :command]))))
        :shell-step-without-command))

(defn- decision
  "{:effect :gaps} for step `s` on a branch labelled `label`."
  [{:keys [tool args]} label grants configured]
  (if (= "shell" (str tool))
    (let [d (policy/with-flow (policy/decide {:grants grants} (:command args))
                              label (:command args) configured)]
      {:effect (:effect d) :gaps (mapv :gap (:gaps d))})
    (let [gaps (flow/gaps flow/table tool args label)]
      {:effect (if (seq gaps) :ask :allow) :gaps (mapv :gap gaps)})))

(defn run-trace
  "Replay `trace`. {:passed n :failures [{:step :tool :expected :actual :gaps}]},
  or {:error {:step :why}} for a trace that cannot be replayed."
  [{:keys [steps grants configured] :as trace}]
  (let [bad (cond (not (map? trace)) {:trace :not-a-map}
                  (not (sequential? steps)) {:trace :no-steps}
                  (empty? steps) {:trace :no-steps}
                  :else (some (fn [[i s]] (some->> (step-error s) (hash-map :step (inc i) :why)))
                              (map-indexed vector steps)))]
    (if bad
      {:error bad}
      (loop [[s & more] steps, i 1, label flow/top, passed 0, failures []]
        (if-not s
          {:passed passed :failures failures}
          (let [{:keys [effect gaps]} (decision s label (vec grants) (set configured))
                ok? (and (= effect (:expect s))
                         (or (not (contains? s :gaps)) (= (vec (:gaps s)) gaps)))
                why {:turn i :tool (str (:tool s))}
                label (if (= :allow effect)
                        (cond-> label
                          (flow/delta flow/table (:tool s) (:args s))
                          (flow/lower-by (flow/delta flow/table (:tool s) (:args s)) why)
                          (:outside s) (flow/lower-by (:outside-read flow/table) why))
                        label)]
            (recur more (inc i) label
                   (if ok? (inc passed) passed)
                   (if ok? failures
                       (conj failures {:step i :tool (str (:tool s)) :expected (:expect s)
                                       :actual effect :gaps gaps})))))))))

(defn run-file
  "Replay the trace in file `f`."
  [f]
  (try (assoc (run-trace (edn/read-string (slurp f))) :file (str f))
       (catch Throwable e {:file (str f) :error (ex-message e)})))

(defn trace-files
  "The .edn traces at `path` — the file itself, or those in the directory."
  [path]
  (let [f (io/file path)]
    (if (.isDirectory f)
      (vec (sort (filter #(str/ends-with? % ".edn") (map str (.listFiles f)))))
      [(str f)])))

(defn exit-code
  "0 when every result passed, 2 when one could not be read, else 1."
  [results]
  (cond (some :error results) 2
        (some (comp seq :failures) results) 1
        :else 0))

(defn -main [& paths]
  (let [results (mapv run-file (mapcat trace-files (or (seq paths) ["test/policy"])))]
    (doseq [{:keys [file passed failures error]} results]
      (println (cond error (str "ERROR " file ": " (pr-str error))
                     (seq failures) (str "FAIL  " file)
                     :else (str "ok    " file " (" passed " steps)")))
      (doseq [{:keys [step tool expected actual gaps]} failures]
        (println (str "      step " step " " tool ": expected " (name expected)
                      ", got " (name actual) (when (seq gaps) (str " " gaps))))))
    (System/exit (exit-code results))))
