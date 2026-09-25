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

(ns samizdat.toolspec-test
  "Native tool specs, read off the tool documentation the branch was given."
  (:require [clojure.test :refer [deftest testing is]]
            [samizdat.llm.toolspec :as toolspec]))

(def ^:private doc
  (str "## Tools\n\n```\n"
       "done({answer})\n"
       "    Ship. `answer` is REQUIRED and is the run's actual output.\n"
       "    A `done` with no answer is refused.\n"
       "read_file({path, offset, limit, outline})\n"
       "    Read a file, a window of it at a time.\n"
       "edit_file({path, old_text, new_text, replace_all?})\n"
       "    Replace one exact piece of text.\n"
       "task({action, ...})\n"
       "    The task board.\n"
       "eval({code, timeout_ms?})\n"
       "    Evaluate Clojure in the {% if x %}live{% endif %} image.\n"
       "```\n\nSome prose that mentions done({answer}) in passing.\n"))

(deftest a-signature-is-a-tool-its-arguments-and-its-description
  (let [specs (into {} (map (juxt :name identity)) (toolspec/signatures doc))]
    (is (= #{"done" "read_file" "edit_file" "task" "eval"} (set (keys specs))))
    (is (= "Ship. `answer` is REQUIRED and is the run's actual output. A `done` with no answer is refused."
           (:description (specs "done"))))
    (is (= #{:path :old_text :new_text :replace_all}
           (set (keys (get-in specs ["edit_file" :parameters :properties])))))
    (testing "nothing is required: the prose says what is, and the tool checks"
      (is (not (contains? (get-in specs ["read_file" :parameters]) :required))))
    (testing "`...` is the open end of an action tool: its properties are the named ones"
      (is (= #{:action} (set (keys (get-in specs ["task" :parameters :properties]))))))
    (is (= "object" (get-in specs ["task" :parameters :type])))))

(deftest a-mention-in-prose-is-not-a-second-definition
  (is (= 1 (count (filter #(= "done" (:name %)) (toolspec/signatures doc))))))

(deftest the-branchs-own-system-messages-are-the-source-and-exact-schemas-win
  (let [messages [{:role "system" :content doc}
                  {:role "user" :content "read_file({path}) — a user message is not documentation"}]
        exact {"done" {:name "done" :description "Finish."
                       :parameters {:type "object" :properties {:answer {:type "string"}}
                                    :required ["answer"]}}}
        specs (toolspec/for-messages messages exact)]
    (is (= ["done" "edit_file" "eval" "read_file" "task"] (mapv :name specs))
        "sorted by name, so the array — and the prefix it lands in — is stable")
    (is (= ["answer"] (get-in (first specs) [:parameters :required]))
        "a tool gates.edn gives an exact schema to is sent with it")
    (is (empty? (toolspec/for-messages [{:role "user" :content doc}] exact))
        "no system message, no documentation, no tools")))

(deftest a-long-description-is-cut-at-a-sentence-end
  (let [text (str "```\neval({code})\n"
                  "    Evaluate Clojure. This is how to work: try a form and iterate.\n"
                  "    More prose follows here.\n```")
        [spec] (toolspec/signatures text 40)]
    (is (= "Evaluate Clojure." (:description spec)))
    (is (= 1 (count (toolspec/signatures text nil))) "no limit, the whole paragraph")
    (is (clojure.string/includes? (:description (first (toolspec/signatures text nil))) "More prose"))))
