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

(ns samizdat.tui-notify-test
  "Desktop notifications, from dirge: when a run needs you, and when it ends
  — the two moments a person who looked away wants to know about."
  (:require [clojure.test :refer [deftest testing is]]
            [samizdat.tui.layout :as layout]
            [samizdat.tui.notify :as notify]))

(def ^:private settings {:on #{"question" "run-finished"}})

(def ^:private state {:run-id "0622a35f-ebb0" :project {:project "samizdat"}
                      :detail {:run {:problem "fix the parser"}}})

(deftest a-question-for-you-is-a-notification
  (let [n (notify/for-event state {:event "approval" :data {:status "pending" :kind "shell"}} settings)]
    (is (re-find #"samizdat" (:title n)))
    (is (re-find #"needs you" (:body n)))
    (is (re-find #"fix the parser" (:body n)) "and says which run"))
  (is (nil? (notify/for-event state {:event "approval" :data {:status "decided"}} settings))
      "an answered one is not news"))

(deftest a-run-ending-is-a-notification
  (let [n (notify/for-event state {:event "run-finished" :data {:data {:status "done"}}} settings)]
    (is (re-find #"done" (:body n)))))

(deftest what-is-not-asked-for-is-quiet
  (is (nil? (notify/for-event state {:event "turn" :data {}} settings)))
  (is (nil? (notify/for-event state {:event "run-finished" :data {:data {:status "done"}}}
                              {:on #{"question"}})))
  (is (nil? (notify/for-event state {:event "approval" :data {:status "pending"}} {:on #{}}))))

(deftest the-command-is-the-platforms-or-the-one-configured
  (let [n {:title "samizdat" :body "a \"quoted\" body"}]
    (is (= "osascript" (first (notify/command "Mac OS X" nil n))))
    (is (re-find #"a \\\"quoted\\\" body" (last (notify/command "Mac OS X" nil n)))
        "AppleScript's quotes are escaped, so a body cannot end the string")
    (is (= ["notify-send" "samizdat" "a \"quoted\" body"] (notify/command "Linux" nil n)))
    (is (= ["terminal-notifier" "-title" "samizdat" "-message" "a \"quoted\" body"]
           (notify/command "Linux" ["terminal-notifier" "-title" "{title}" "-message" "{body}"] n))
        "a configured command, with {title} and {body} filled in")
    (is (nil? (notify/command "Windows 11" nil n)) "no known way is no notification, not an error")))

(deftest the-shipped-settings-turn-them-on
  (is (= #{"question" "run-finished"} (set (:on (:notifications (layout/template)))))))
