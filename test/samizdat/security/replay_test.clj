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

(ns samizdat.security.replay-test
  "Policy trace files (karamazov-3vu1.8): every test/policy/*.edn replays
  against the real decisions, and a trace that is wrong says where."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest testing is]]
            [samizdat.security.replay :as replay]))

(deftest every-shipped-trace-passes
  (let [files (replay/trace-files "test/policy")]
    (is (<= 3 (count files)))
    (doseq [f files]
      (let [{:keys [failures]} (replay/run-file f)]
        (is (empty? failures) (str f ": " (pr-str failures)))))))

(deftest a-wrong-expectation-names-the-step
  (let [{:keys [failures passed]}
        (replay/run-trace {:steps [{:tool "webfetch" :args {:url "u"} :expect :allow}
                                   {:tool "shell" :args {:command "make"} :expect :allow}]})]
    (is (= 1 passed))
    (is (= [{:step 2 :tool "shell" :expected :allow :actual :ask :gaps [:trust]}]
           failures))))

(deftest a-call-that-would-wait-brings-nothing-in
  ;; An ask is a call that did not run, so what it would have read is not
  ;; the branch's.
  (let [{:keys [failures]}
        (replay/run-trace {:steps [{:tool "read_file" :args {:path "/x"} :outside true :expect :allow}
                                   {:tool "webfetch" :args {:url "u"} :expect :ask :gaps [:audience]}
                                   {:tool "shell" :args {:command "make"} :expect :ask :gaps [:audience]}]})]
    (is (empty? failures) (pr-str failures))))

(deftest a-malformed-trace-is-an-error-not-a-pass
  (is (some? (:error (replay/run-trace {:steps [{:tool "shell" :expect :maybe}]}))))
  (is (some? (:error (replay/run-trace {})))))

(deftest the-runner-exits-by-outcome
  (is (= 0 (replay/exit-code [{:passed 3 :failures []}])))
  (is (= 1 (replay/exit-code [{:passed 2 :failures [{:step 1}]}])))
  (is (= 2 (replay/exit-code [{:error "unreadable"}]))))
