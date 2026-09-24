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

(ns samizdat.tui.notify
  "Desktop notifications, after dirge's: when the run on screen needs a
  person, and when it ends — the two moments somebody who looked away wants
  to know about.

  Which moments, and how to raise one, are tui.edn `:notifications`: `:on`
  names the moments (\"question\", \"run-finished\"), and `:command`, when
  set, is the program to run with {title} and {body} filled in. Unset, the
  platform's own: osascript on macOS, notify-send elsewhere.

  Pure, bar `notify!`, which runs the command and never lets a failure
  reach the UI."
  (:require [clojure.string :as str]
            [jolt.process :as proc]))

(defn- run-label [state]
  (let [p (str (get-in state [:detail :run :problem]))
        p (str/replace p #"\s+" " ")]
    (if (> (count p) 60) (str (subs p 0 59) "…") p)))

(defn for-event
  "The notification a pushed event deserves, {:title :body}, or nil."
  [state {:keys [event data]} {:keys [on]}]
  (let [on (set on)
        title (str "samizdat" (some->> (get-in state [:project :project]) (str " · ")))]
    (cond
      (and (= "approval" event) (= "pending" (:status data)) (contains? on "question"))
      {:title title
       :body (str "a run needs you"
                  (when (= "shell" (:kind data)) " to allow a command")
                  (when-let [l (not-empty (run-label state))] (str ": " l)))}

      (and (= "run-finished" event) (contains? on "run-finished"))
      {:title title
       :body (str "run " (or (get-in data [:data :status]) "finished")
                  (when-let [l (not-empty (run-label state))] (str ": " l)))}

      :else nil)))

(defn- applescript-string [s]
  (str "\"" (str/replace (str s) #"[\\\\\"]" #(str "\\" %)) "\""))

(defn command
  "The argv that raises `n` on `os` (os.name), or with `configured` — a
  vector whose {title} and {body} are filled in. nil when there is no known
  way, which is a quiet nothing rather than an error."
  [os configured {:keys [title body]}]
  (cond
    (seq configured)
    (mapv #(-> (str %) (str/replace "{title}" (str title)) (str/replace "{body}" (str body)))
          configured)

    (str/includes? (str/lower-case (str os)) "mac")
    ["osascript" "-e" (str "display notification " (applescript-string body)
                           " with title " (applescript-string title))]

    (str/includes? (str/lower-case (str os)) "linux")
    ["notify-send" (str title) (str body)]

    :else nil))

(defn notify!
  "Raise `n`, off the caller's thread. Best effort: a notifier that is not
  installed or fails costs the notification, nothing else."
  [settings n]
  (when-let [argv (command (System/getProperty "os.name") (:command settings) n)]
    (future
      (try (deref (proc/process argv {})) (catch Throwable _ nil)))
    nil))
