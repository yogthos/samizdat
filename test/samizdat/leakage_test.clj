;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.leakage-test
  "karamazov-na2k.13: what makes a project's edit unfit to promote into the
  shipped templates, from RRSI's critic: it names the project, or it removes
  a safety mechanism without a replacement."
  (:require [clojure.test :refer [deftest is testing]]
            [samizdat.leakage :as leakage]))

(deftest an-edit-that-names-its-project-is-flagged
  (let [f (leakage/screen {:kind "prompt"
                           :template "Run the tests before you ship."
                           :edited "Run the tests before you ship. flight.draw is the only namespace that calls rl/."
                           :subject-terms ["flight." "horizon-fade"]})]
    (is (= ["flight."] (:names-subject f))))
  (testing "a term the template already had is not the edit's"
    (is (empty? (:names-subject (leakage/screen {:kind "prompt"
                                                 :template "flight. is mentioned already"
                                                 :edited "flight. is mentioned already, still"
                                                 :subject-terms ["flight."]}))))))

(deftest an-edit-that-drops-a-guard-is-flagged
  (testing "a gate removed from gates.edn"
    (let [f (leakage/screen {:kind "policy" :name "gates"
                             :template (pr-str {:gates [{:gate :stuck} {:gate :storm}]})
                             :edited (pr-str {:gates [{:gate :stuck}]})})]
      (is (= [":storm"] (:drops-guard f)))))
  (testing "an invariant removed from a manifest"
    (let [f (leakage/screen {:kind "manifest"
                             :template (pr-str {:invariants [{:type :must-follow :if :dispatch :then :journal}]})
                             :edited (pr-str {:invariants []})})]
      (is (seq (:drops-guard f)))))
  (testing "an edit that keeps every guard is clean"
    (is (leakage/clean? (leakage/screen {:kind "policy" :name "gates"
                                         :template (pr-str {:gates [{:gate :stuck}]})
                                         :edited (pr-str {:gates [{:gate :stuck} {:gate :new}]})})))))
