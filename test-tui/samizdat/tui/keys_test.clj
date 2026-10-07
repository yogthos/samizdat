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

(ns samizdat.tui.keys-test
  "The loop's own key handler (samizdat.tui.core/on-event) over the state it
  really holds, with the HTTP client recorded rather than called: the claim
  that a questionnaire can be answered without touching the mouse, and that
  Ctrl+C does not take a run down with one press."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [samizdat.api.client :as client]
            [samizdat.tui.core :as core]
            [samizdat.tui.state :as st]))

(def ^:private on-event @#'core/on-event)

(defn- key! [k] (on-event {:type :key :key k}))
(defn- char! [c] (on-event {:type :character :char c}))

(defn- settle
  "Answers go out on a future; give them a moment to land."
  [pred]
  (loop [n 0] (when (and (not (pred)) (< n 200)) (Thread/sleep 10) (recur (inc n)))))

(def ^:private sent (atom []))

(use-fixtures :each
  (fn [f]
    (reset! sent [])
    (reset! core/state (assoc (st/initial "http://x") :run-id "r1"))
    (with-redefs [client/decide! (fn [_ id d] (swap! sent conj [id d]) {:ok true})
                  client/approvals (fn [& _] {:ok true :body {:approvals []}})
                  client/run-detail (fn [& _] {:ok false :error "stub"})
                  client/layout (fn [& _] {:ok false})
                  client/project (fn [& _] {:ok false})
                  client/list-runs (fn [& _] {:ok false :error "stub"})
                  client/steps-since (fn [& _] {:ok false :error "stub"})]
      (f))))

(def ^:private q
  {:id "q1" :questions [{:question "which store?" :options ["sqlite" "postgres"]}
                        {:question "which?" :multi true :options ["a" "b" "c"]}
                        {:question "name it" :options []}]})

(deftest a-questionnaire-is-answered-from-the-keyboard
  (swap! core/state st/apply-approvals {:ok true :body {:approvals [q]}})
  (key! :arrow-down)
  (is (= 1 (:question-option @core/state)))
  (key! :return)
  (is (= ["postgres"] (:question-answers @core/state)) "Enter picked the row under the cursor")
  (char! "1") (char! "3")
  (is (= #{0 2} (:question-selected @core/state)) "digits tick on a multi-select")
  (key! :return)
  (is (= 2 (:question-cursor @core/state)) "confirmed, on to the open question")
  (testing "the open question's answer is typed in the compose box"
    (char! "y")
    (is (empty? @sent) "a letter is the answer being typed, not a verdict")
    (swap! core/state st/set-input "mycelium")
    (@#'core/send! (fn [_] (throw (ex-info "steered instead of answering" {}))) "mycelium")
    (settle #(seq @sent))
    (is (= [["q1" {:decision :answer :answers ["postgres" ["a" "c"] "mycelium"]}]] @sent))))

(deftest esc-half-way-sends-what-was-answered
  (swap! core/state st/apply-approvals {:ok true :body {:approvals [q]}})
  (key! :return)
  (key! :escape)
  (settle #(seq @sent))
  (is (= [["q1" {:decision :answer :answers ["sqlite"]}]] @sent)))

(deftest ctrl-c-with-a-run-going-asks-twice
  (let [exited (atom 0)]
    (with-redefs [ftxui.core/exit! #(swap! exited inc)]
      (swap! core/state assoc :detail {:run {:status "running"}} :input "draft")
      (key! :ctrl-c)
      (is (= "" (:input @core/state)) "the first press cleared the draft")
      (key! :ctrl-c)
      (is (zero? @exited) "the next only warned")
      (is (re-find #"again to quit" (str (:notice @core/state))))
      (key! :ctrl-c)
      (is (= 1 @exited) "and the one after quit"))))

(deftest what-is-typed-is-drawn-in-the-whole-frame
  (swap! core/state assoc :input "abc-typed-text")
  (let [frame (ftxui.core/with-screen [s core/root] (ftxui.core/render-text s 200 50))]
    (is (clojure.string/includes? frame "abc-typed-text"))))

(deftest enter-sends-what-the-box-holds-now-not-what-the-last-frame-saw
  ;; Every run started from the TUI had its problem cut to 128 characters.
  ;; FTXUI reads the terminal 128 bytes at a time; the widget's Enter
  ;; handler closed over the state of the frame drawn between two reads, so
  ;; it sent the first 128 while the state already held the whole line.
  (let [steered (atom nil)
        handlers @#'core/handlers]
    (with-redefs [client/intervene! (fn [_ _ m] (reset! steered (:payload m)) {:ok true})]
      (swap! core/state assoc :input "the whole line, typed after the last frame"
             :detail {:run {:status "running"}})
      ((:submit handlers) "the whole li")
      (is (= "the whole line, typed after the last frame" @steered)))))

(deftest a-paste-keeps-its-newlines-and-a-big-one-collapses
  ;; FTXUI has no bracketed paste of its own: a paste arrived as keys, and its
  ;; first newline was Enter — a pasted stack trace went out at line one.
  (let [sent (atom nil)
        paste! (fn [text]
                 (on-event {:type :unknown :input "\u001b[200~"})
                 (doseq [c text]
                   (if (= \newline c) (key! :return) (char! (str c))))
                 (on-event {:type :unknown :input "\u001b[201~"})
                 ;; the frame that joins it
                 (core/root))]
    (with-redefs [client/intervene! (fn [_ _ m] (reset! sent (:payload m)) {:ok true})]
      (swap! core/state assoc :detail {:run {:status "running"}} :input "see: ")
      (paste! "one\ntwo")
      (is (= "see: one\ntwo" (:input @core/state)) "a line break, and nothing sent")
      (is (nil? @sent))
      (let [trace (clojure.string/join "\n" (map #(str "at frame " %) (range 8)))]
        (paste! (str "\n" trace))
        (is (re-find #"\[9 lines pasted #1\]$" (:input @core/state)) "collapsed in the box")
        ((:submit @#'core/handlers) (:input @core/state))
        (is (= (str "see: one\ntwo\n" trace) @sent) "and sent whole")))))

(deftest ctrl-f-finds-a-line-that-was-sent
  (swap! core/state #(-> % (st/remember-input "run the tests") (st/remember-input "fix it")
                         (st/set-input "draft")))
  (key! :ctrl-f)
  (char! "t") (char! "e")
  (is (= "run the tests" (:input @core/state)))
  (key! :escape)
  (is (= "draft" (:input @core/state)) "Esc puts the draft back")
  (key! :ctrl-f) (char! "f") (key! :return)
  (is (= "fix it" (:input @core/state)) "Enter takes the match")
  (is (nil? (:search @core/state))))

(deftest an-at-mention-is-picked-from-the-keyboard
  (with-redefs [client/project-files (fn [_ q _] {:ok true :body {:files (if (= q "co")
                                                                          ["src/core.clj" "test/core_test.clj"]
                                                                          [])}})]
    ((:input @#'core/handlers) "fix @co")
    (settle #(st/mention-active? @core/state))
    (key! :tab)
    (key! :return)
    (is (= "fix test/core_test.clj" (:input @core/state)) "Tab moved, Enter put the path in")
    (is (nil? (:mention @core/state)))
    ((:input @#'core/handlers) "fix @co")
    (settle #(st/mention-active? @core/state))
    (key! :escape)
    (is (= "fix " (:input @core/state)) "Esc drops the @word")))
