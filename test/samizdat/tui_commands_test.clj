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

(ns samizdat.tui-commands-test
  "Slash commands as data (karamazov-tq7m.5): tui.edn names each command,
  what it does, its arguments and its help line, so a person adds an alias
  or renames one without a rebuild."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [samizdat.tui.commands :as cmd]
            [samizdat.tui.layout :as layout]
            [samizdat.tui.state :as st]))

(def ^:private commands
  {"/model" {:do :model :args "[id]" :doc "switch the model"}
   "/mode" {:do :mode :args "[refuse|block]" :doc "the approval mode"}
   "/cull" {:do :intervene :kind "cull" :target :arg :doc "stop a branch"}
   "/steer" {:do :intervene :kind "message" :target :branch :doc "say something"}
   "/help" {:do :help :doc "the commands"}
   "/m" {:alias "/model"}})

(deftest a-slash-line-is-a-command-with-its-argument
  (is (= {:name "/model" :do :model :arg "glm-5.3"}
         (select-keys (cmd/parse "/model  glm-5.3 " commands) [:name :do :arg])))
  (is (= {:name "/help" :do :help :arg nil}
         (select-keys (cmd/parse "/help" commands) [:name :do :arg])))
  (testing "an alias is the command it names"
    (is (= :model (:do (cmd/parse "/m x" commands)))))
  (testing "an unknown command says so and points at /help"
    (is (re-find #"unknown command /nope.*/help" (:error (cmd/parse "/nope" commands)))))
  (testing "a line that is not a command is not parsed as one"
    (is (nil? (cmd/parse "fix the parser" commands)))
    (is (nil? (cmd/parse "  " commands)))))

(deftest completion-offers-what-matches-and-completes-the-common-part
  (is (= ["/mode" "/model"] (mapv :name (cmd/candidates "/mo" commands))))
  (is (= "/mode" (cmd/complete "/mo" commands)) "the common prefix of both")
  (is (= "/help " (cmd/complete "/he" commands)) "one match completes, with the space")
  (is (= "/model x" (cmd/complete "/model x" commands)) "past the name there is nothing to complete")
  (is (empty? (cmd/candidates "/zz" commands)))
  (is (empty? (cmd/candidates "hello" commands)))
  (is (not-any? #(= "/m" (:name %)) (cmd/candidates "/m" commands))
      "an alias is not offered beside the command it names"))

(deftest help-names-every-command
  (let [lines (cmd/help-lines commands)]
    (is (some #(str/includes? % "/model [id]") lines))
    (is (some #(str/includes? % "switch the model") lines))
    (is (not-any? #(str/starts-with? (str/trim %) "/m ") lines))))

(deftest the-shipped-commands-are-well-formed
  (let [cs (:commands (layout/template))]
    (is (seq cs))
    (doseq [[n c] cs]
      (is (str/starts-with? n "/") n)
      (is (or (:do c) (contains? cs (:alias c))) (str n " does something or names a command")))
    (testing "the ones the plan promised"
      (is (every? #(contains? cs %) ["/model" "/effort" "/mode" "/help" "/quit" "/run"
                                     "/abort" "/resume" "/steer"])))))

(deftest what-the-tui-printed-is-a-note-in-the-conversation
  (let [s (st/note-local (st/initial "b") ["one" "two"])]
    (is (= ["one" "two"] (mapv :text (:local-notes s))))
    (is (every? :at (:local-notes s)))
    (is (= [] (:local-notes (st/clear-local s))))))

(deftest history-walks-back-and-forth
  (let [s (-> (st/initial "b") (st/remember-input "one") (st/remember-input "two"))]
    (is (= "two" (:input (st/history-back s))))
    (is (= "one" (:input (st/history-back (st/history-back s)))))
    (is (= "one" (:input (-> s st/history-back st/history-back st/history-back))) "stops at the oldest")
    (is (= "two" (:input (-> s st/history-back st/history-back st/history-forward))))
    (is (= "" (:input (-> s st/history-back st/history-forward))) "past the newest, an empty line")
    (is (= ["one" "two"] (:history (st/remember-input s "two"))) "a repeat is not stored twice")))
