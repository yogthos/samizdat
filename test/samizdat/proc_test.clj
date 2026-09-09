;; samizdat - a claim-first verification harness
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

(ns samizdat.proc-test
  "The subprocess reaper, against a process that refuses to cooperate."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [samizdat.engine.proc :as proc]))

(def ^:private needle "sleep 987")

(defn- survivors []
  ;; pgrep exits 1 when nothing matches — an empty list, not an error.
  (let [r (proc/run {:timeout-ms 5000} "pgrep" "-f" needle)]
    (if (zero? (:exit r)) (str/split-lines (:out r)) [])))

(defn- sweep! []
  (try (proc/run {:timeout-ms 5000} "pkill" "-9" "-f" needle)
       (catch Throwable _ nil)))

(deftest a-term-trapping-tree-dies-to-the-last-process
  ;; provenance R3-8. reap!'s escalation was destroyForcibly on the ROOT only, and
  ;; worse: jolt's ProcessHandle.destroy — what destroy-tree signals through —
  ;; silently fails to signal at all, so the tree-wide TERM never landed. A
  ;; child that ignores TERM outlived the reap as an orphan, which is the
  ;; 28-z3-processes failure mode this namespace exists to prevent. The kill
  ;; must reach every descendant with SIGKILL, enumerated before the root dies
  ;; (afterwards they reparent to init and are invisible to descendants()).
  (sweep!)
  (try
    (let [r (proc/run {:timeout-ms 1500} "sh" "-c"
                      "trap '' TERM; sleep 987 & wait")]
      (is (:timeout r) "the run times out")
      (Thread/sleep 300)
      (is (empty? (survivors))
          "a TERM-trapping child tree must not outlive the reap"))
    (finally (sweep!))))

(defn- open-fds
  "How many descriptors this process holds, via /dev/fd. nil where that is
  not a directory, so the test can skip rather than fail on a platform that
  does not publish it."
  []
  (let [d (java.io.File. "/dev/fd")]
    (when (.isDirectory d) (count (.list d)))))

(deftest run-does-not-leak-the-pipes-it-reads
  ;; `:out :string` reads the child's output into a string and left the pipe
  ;; descriptors open: measured 2 per call, never released — 30 spawns cost
  ;; 60 fds, 60 cost 120. That is the leak behind provenance A-3, where the
  ;; suite "holds ~260 fds at peak" and the `test` task raises ulimit -n to
  ;; 1024 to paper over it; adding three git-spawning tests was enough to
  ;; cross even the raised limit, and it surfaced as "sqlite step failed:
  ;; unable to open database file" in namespaces that had nothing to do with
  ;; subprocesses.
  ;;
  ;; Not a test-only concern: the `shell` tool runs through here, so a run
  ;; that shells out a few hundred times exhausted its own descriptors and
  ;; then could not open its database.
  (when-let [before (open-fds)]
    (dotimes [_ 40]
      (proc/run {:timeout-ms 5000} "sh" "-c" "echo hello"))
    (let [after (open-fds)]
      (is (<= after (+ before 4))
          (str "40 spawns should cost no descriptors; held " before
               " before and " after " after. Two per call is the pipe leak.")))))

(deftest a-timed-out-run-closes-its-pipes-too
  ;; The timeout path returns before the deref that collects output, so it
  ;; needs its own close — which is why the close sits in a finally rather
  ;; than beside the successful return.
  (when-let [before (open-fds)]
    (dotimes [_ 5]
      (is (:timeout (proc/run {:timeout-ms 50} "sh" "-c" "sleep 5"))))
    (let [after (open-fds)]
      (is (<= after (+ before 4))
          (str "held " before " before and " after " after five timeouts")))))
