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

(ns samizdat.launch-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [clojure.tools.logging :as log]
            [jolt.ffi :as ffi]
            [samizdat.launch :as launch]))

(ffi/defcfn ^:private c-write "write" [:int :string :int] :int)

(deftest parse-args
  (testing "no arguments starts the server and the TUI in one process"
    (is (= {:mode :tui} (launch/parse-args []))))
  (testing "--headless serves HTTP and nothing else"
    (is (= {:mode :headless} (launch/parse-args ["--headless"]))))
  (testing "--connect attaches the TUI to a server that is already running"
    (is (= {:mode :connect :url "http://10.0.0.2:3985"}
           (launch/parse-args ["--connect" "http://10.0.0.2:3985"])))
    (is (= {:mode :connect :url nil} (launch/parse-args ["--connect"]))
        "with no URL the TUI's own default applies (SAMIZDAT_URL, HARNESS_PORT)"))
  (testing "help"
    (is (= :help (:mode (launch/parse-args ["--help"]))))
    (is (= :help (:mode (launch/parse-args ["-h"])))))
  (testing "a flag it does not know is refused, not ignored"
    (let [{:keys [mode message]} (launch/parse-args ["--headles"])]
      (is (= :error mode))
      (is (str/includes? message "--headles"))))
  (testing "--headless and --connect contradict each other"
    (is (= :error (:mode (launch/parse-args ["--headless" "--connect"])))))
  (testing "a stray positional argument is refused"
    (is (= :error (:mode (launch/parse-args ["http://x"]))))))

(deftest log-path
  (is (= "/p/.samizdat/samizdat.log" (launch/log-path "/p"))))

(deftest redirect-stderr
  (testing "what the process writes to fd 2 lands in the file while redirected"
    (let [f (java.io.File/createTempFile "samizdat-launch" ".log")
          path (.getPath f)
          saved (launch/redirect-stderr! path)]
      (try
        (c-write 2 "into the log\n" 13)
        (finally (launch/restore-stderr! saved)))
      (is (str/includes? (slurp path) "into the log"))
      (.delete f))))

(deftest log-to-file
  (testing "tools.logging writes to the file, not the terminal, until restored"
    (let [f (java.io.File/createTempFile "samizdat-launch" ".log")
          path (.getPath f)
          prev (launch/log-to-file! path)]
      (try
        (log/warn "a logged line")
        (finally (launch/restore-logging! prev)))
      (let [body (slurp path)]
        (is (str/includes? body "WARN"))
        (is (str/includes? body "a logged line")))
      (is (= prev log/*logger-factory*))
      (.delete f))))
