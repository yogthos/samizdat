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

(ns samizdat.grant-proposal-test
  "The supervisor drafts a narrow grant from what the run was refused; only
  a person's yes records it (karamazov-0e2c.20, after xi's recommend-a-rule).
  The grants table stays a person's: the model still has no edge into it."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is use-fixtures]]
            [samizdat.agent.tools :as tools]
            [samizdat.approval :as approval]
            [samizdat.store.db :as db]
            [samizdat.store.grants :as grants]
            [samizdat.store.journal :as journal]
            [samizdat.store.runs :as runs]))

(use-fixtures :each (fn [t] (approval/set-mode! nil) (try (t) (finally (approval/set-mode! nil)))))

(defn- propose [c rid args]
  (tools/run-tool {:tool-name "policy" :conn c :run-id rid :turn 9
                   :branch {:id "SUP"} :role :supervisor
                   :args (merge {:action "propose-grant" :reason "the build needs it"} args)}))

(defmacro with-refusal [[c rid] & body]
  `(let [~c (db/open! ":memory:")
         ~rid (runs/start-run! ~c {:problem "p"})]
     (try
       (journal/record-turn! ~c ~rid {:branch-id "B1" :turn 4 :tool-name "shell"
                                      :args {:command "python3 build.py --release"}
                                      :result "Command needs approval" :category :neutral
                                      :policy-refusal? true})
       ~@body
       (finally (db/close ~c)))))

(deftest a-person-turns-a-proposal-into-a-grant
  (with-refusal [c rid]
    (let [asked (atom nil)]
      (with-redefs [approval/policy (constantly {:mode :block :wait-ms 1 :on-timeout :deny})
                    approval/request! (fn [q] (reset! asked q) "id")
                    approval/await! (fn [_ _ _] {:decision :allow})]
        (propose c rid {:pattern "python3 build.py *"})
        (is (= :grant (:kind @asked)))
        (is (= "python3 build.py *" (:input @asked)))
        (is (str/includes? (str (:details @asked)) "python3 build.py --release")
            "the person sees what it would have let through")
        (is (= ["python3 build.py *"] (:grants (grants/for-run c rid))))))))

(deftest a-denied-or-unanswered-proposal-grants-nothing
  (with-refusal [c rid]
    (with-redefs [approval/policy (constantly {:mode :block :wait-ms 1 :on-timeout :deny})
                  approval/request! (fn [_] "id")
                  approval/await! (fn [_ _ d] (assoc d :timed-out true))]
      (propose c rid {:pattern "python3 build.py *"})
      (is (empty? (:grants (grants/for-run c rid)))))
    (testing "and with nobody asked, it is journaled for the operator"
      (propose c rid {:pattern "python3 build.py *"})
      (is (empty? (:grants (grants/for-run c rid))))
      (is (= 1 (count (journal/notes c rid :grant-proposed)))))))

(deftest a-proposal-must-be-narrow-and-about-what-was-refused
  (with-refusal [c rid]
    (with-redefs [approval/policy (constantly {:mode :block :wait-ms 1 :on-timeout :deny})
                  approval/request! (fn [_] (throw (ex-info "should not ask" {})))]
      (is (str/includes? (:result (propose c rid {:pattern "make *"})) "refused")
          "it lets through nothing this run was refused")
      (doseq [p ["*" "**" "python3 *"]]
        (is (not= :success (:category (propose c rid {:pattern p}))) p))
      (is (empty? (:grants (grants/for-run c rid)))))))
