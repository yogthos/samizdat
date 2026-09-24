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
  TERMINAL raises it when it knows how (`escape`) — then the notification is
  the terminal's, and clicking it goes to the tab. osascript's belonged to
  Script Editor: it showed as a script and a click opened Script Editor.
  Past that, the platform's own: osascript on macOS, notify-send elsewhere.

  Pure, bar `notify!`, which does it and never lets a failure reach the UI."
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

(defn- clean
  "`s` fit to ride inside an escape sequence: a line break is a space and any
  other control character — BEL or ESC would end the sequence — is gone."
  [s]
  (-> (str s)
      (str/replace #"[\n\r\t]" " ")
      (str/replace #"[\x00-\x1f\x7f]" "")))

(defn escape
  "The sequence that has the terminal named by `env` (a map of environment
  variables) raise `n` itself, or nil when it would not be understood.
  Ghostty and WezTerm take OSC 777 with a title, iTerm2 OSC 9, kitty OSC 99.
  Under tmux nothing: it keeps OSC sequences to itself unless configured to
  pass them through, and a notification that silently never arrives is worse
  than the platform's."
  [env {:keys [title body]}]
  (let [program (str (get env "TERM_PROGRAM"))
        term (str (get env "TERM"))
        title (clean title)
        body (clean body)
        joined (str title ": " body)]
    (cond
      (not (str/blank? (get env "TMUX"))) nil
      (#{"ghostty" "WezTerm"} program)
      (str "\u001b]777;notify;" (str/replace title ";" ",") ";" body "\u0007")
      (= "iTerm.app" program) (str "\u001b]9;" joined "\u0007")
      (str/includes? term "kitty") (str "\u001b]99;;" joined "\u001b\\")
      :else nil)))

(defn raise!
  "Raise `n` by the first way that applies: the configured command, the
  terminal's own sequence (`write` sends it), the platform's command (`run`
  runs an argv). The effects are passed in so the choice is testable."
  [settings n {:keys [env os write run]}]
  (let [configured (:command settings)
        esc (when (empty? configured) (escape env n))]
    (if esc
      (write esc)
      (when-let [argv (command os configured n)] (run argv)))
    nil))

(defn notify!
  "Raise `n`, off the caller's thread. Best effort: a notifier that is not
  installed or fails costs the notification, nothing else. `write` sends an
  escape sequence to the terminal (the toolkit's, which puts it between
  frames)."
  [settings n write]
  (future
    (try
      (raise! settings n {:env (into {} (System/getenv))
                          :os (System/getProperty "os.name")
                          :write write
                          :run #(deref (proc/process % {}))})
      (catch Throwable _ nil)))
  nil)
