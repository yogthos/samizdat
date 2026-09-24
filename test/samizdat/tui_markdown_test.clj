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

(ns samizdat.tui-markdown-test
  "Replies are markdown, drawn the way dirge draws them: headings bold,
  bullets as •, code indented and set apart, quotes behind a bar."
  (:require [clojure.test :refer [deftest testing is]]
            [samizdat.tui.markdown :as md]))

(defn- shape [lines] (mapv (fn [[_ {:keys [class]} text]] [class text]) lines))

(deftest blocks
  (is (= [[:md-h1 "Title"] [:md-h2 "Part"] [:md-h2 "Sub"]]
         (shape (md/lines "# Title\n## Part\n### Sub"))))
  (is (= [[nil "  • one"] [nil "  • two"] [nil " 3. three"]]
         (shape (md/lines "- one\n* two\n3. three"))))
  (is (= [[:md-quote "│ said"]] (shape (md/lines "> said"))))
  (testing "code keeps its text verbatim, fence lines dropped"
    (is (= [[nil "run it:"] [:md-code "  (+ 1 2)"] [:md-code "  **not bold**"]]
           (shape (md/lines "run it:\n```clojure\n(+ 1 2)\n**not bold**\n```")))))
  (testing "inline emphasis markers go, the words stay"
    (is (= [[nil "a bold word and `code`"]] (shape (md/lines "a **bold** word and `code`")))))
  (testing "a blank line is a blank row"
    (is (= [[nil "a"] [nil ""] [nil "b"]] (shape (md/lines "a\n\nb"))))))
