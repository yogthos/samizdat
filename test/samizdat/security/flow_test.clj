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

(ns samizdat.security.flow-test
  "A branch's flow label (karamazov-3vu1.7): what it has read lowers it, and
  a sink checks it before the call runs. A block says what is missing and
  the ways forward (karamazov-3vu1.8)."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is use-fixtures]]
            [jolt.fs :as fs]
            [samizdat.agent.tools :as tools]
            [samizdat.agent.tools.base :as base]
            [samizdat.approval :as approval]
            [samizdat.security.flow :as flow]
            [samizdat.security.policy :as policy]
            [samizdat.store.db :as db]
            [samizdat.store.journal :as journal]
            [samizdat.store.runs :as runs]))

(use-fixtures :each (fn [t] (flow/reset!) (approval/set-mode! nil) (try (t) (finally (flow/reset!)))))

(def ^:private untrusted (assoc flow/top :trust :untrusted))
(def ^:private private (assoc flow/top :audience :operator))

;; --- the lattice ------------------------------------------------------------

(deftest labels-only-narrow
  (is (= flow/top (flow/meet flow/top flow/top)))
  (is (= :untrusted (:trust (flow/meet flow/top untrusted))))
  (is (= :untrusted (:trust (flow/meet untrusted flow/top))) "a later trusted read does not restore it")
  (is (= {:trust :untrusted :audience :operator}
         (select-keys (flow/meet untrusted private) [:trust :audience]))))

;; --- what lowers it ----------------------------------------------------------

(deftest web-content-is-untrusted
  (is (= :untrusted (:trust (flow/delta flow/table "webfetch" {:url "https://x"}))))
  (is (= :untrusted (:trust (flow/delta flow/table "websearch" {:query "q"}))))
  (is (nil? (flow/delta flow/table "read_file" {:path "src/a.clj"})) "the project's own files lower nothing"))

;; --- what a sink needs -------------------------------------------------------

(deftest a-shell-command-after-web-content-has-a-trust-gap
  (let [[g] (policy/shell-gaps untrusted "make install")]
    (is (= :trust (:gap g)))
    (is (= :trusted (:required g)))
    (is (= :untrusted (:actual g)))))

(deftest a-read-only-command-has-no-gap
  (doseq [c ["ls -la" "cat src/a.clj" "grep -rn foo src" "git status" "git log -5"
             "git diff HEAD~1" "ls src && wc -l src/a.clj" "find src -name '*.clj'"]]
    (is (empty? (policy/shell-gaps untrusted c)) c))
  (doseq [c ["git push" "curl https://x" "ls; curl https://x" "cat $(curl x)" "sed -i s/a/b/ f"
             "find . -exec rm {} ;" "find . -delete" "ls > out" "FOO=1 ls" "git -c x=y status"]]
    (is (seq (policy/shell-gaps untrusted c)) c)))

(deftest sending-out-after-a-private-read-has-an-audience-gap
  (let [[g] (flow/gaps flow/table "webfetch" {:url "https://x"} private)]
    (is (= :audience (:gap g)))
    (is (= :project (:required g)))
    (is (= :operator (:actual g))))
  (is (seq (policy/shell-gaps private "curl https://x")))
  (is (empty? (flow/gaps flow/table "webfetch" {:url "https://x"} untrusted))
      "reading another page after one is not a leak of anything private"))

(deftest a-clean-label-has-no-gaps
  (is (empty? (policy/shell-gaps flow/top "make install")))
  (is (empty? (flow/gaps flow/table "webfetch" {:url "https://x"} flow/top))))

(deftest a-gap-names-what-lowered-the-label
  (let [l (flow/meet flow/top (assoc untrusted :because {:trust {:turn 7 :tool "webfetch"}}))
        [g] (policy/shell-gaps l "make")]
    (is (= {:turn 7 :tool "webfetch"} (:because g)))))

;; --- the ways forward (karamazov-3vu1.8) ------------------------------------

(deftest a-block-lists-its-ways-forward
  (let [gs (policy/shell-gaps untrusted "make install")
        plans (flow/remedies "shell" gs)]
    (is (some #(= :authorize (:plan %)) plans) "a person can allow it")
    (is (some #(= :narrow (:plan %)) plans) "a read-only command still runs"))
  (let [plans (flow/remedies "webfetch" (flow/gaps flow/table "webfetch" {:url "u"} private))]
    (is (= [:authorize] (mapv :plan plans)) "nothing but a person clears a leak")))

;; --- the branch's label, kept and rebuilt -------------------------------------

(deftest observing-a-source-lowers-the-branch-and-journals-it
  (let [c (db/open! ":memory:")
        rid (runs/start-run! c {:problem "p"})
        ctx {:conn c :run-id rid :branch {:id "B1"} :turn 3}]
    (try
      (is (= flow/top (flow/label-of ctx)))
      (flow/observe! ctx (flow/delta flow/table "webfetch" {:url "u"}) "webfetch")
      (is (= :untrusted (:trust (flow/label-of ctx))))
      (is (= :trusted (:trust (flow/label-of (assoc ctx :branch {:id "B2"}))))
          "a sibling read nothing")
      (testing "the change is in the journal"
        (is (= 1 (count (journal/notes c rid :flow)))))
      (testing "and a resumed process rebuilds it from there"
        (flow/reset!)
        (let [l (flow/label-of ctx)]
          (is (= :untrusted (:trust l)))
          (is (= {:turn 3 :tool "webfetch"} (get-in l [:because :trust])))))
      (testing "observing what changes nothing writes nothing"
        (flow/observe! ctx (flow/delta flow/table "webfetch" {:url "v"}) "webfetch")
        (is (= 1 (count (journal/notes c rid :flow)))))
      (testing "a run that ends is forgotten"
        (flow/forget-run! rid)
        (is (empty? (flow/cached))))
      (finally (db/close c)))))

;; --- the shell, live ---------------------------------------------------------

(defn- shell [ctx cmd]
  (policy/run-shell (merge {:env {"PATH" (System/getenv "PATH")}
                            :root (str (fs/create-temp-dir))
                            :args {:command cmd}}
                           ctx)))

(deftest after-web-content-the-shell-asks-before-running-what-it-would-have-run
  (let [c (db/open! ":memory:")
        ctx {:conn c :run-id (runs/start-run! c {:problem "p"}) :branch {:id "B1"} :turn 4}]
    (try
      (flow/observe! ctx (flow/delta flow/table "webfetch" {:url "u"}) "webfetch")
      (with-redefs [samizdat.security.confine/backend (constantly :none)]
        (let [r (shell ctx "touch made")]
          (is (= :ask (get-in r [:policy :effect])) (:result r))
          (is (= [:trust] (mapv :gap (get-in r [:policy :gaps]))))
          (is (some #(= :authorize (:plan %)) (get-in r [:policy :remedies])))
          (testing "the refusal says why and how to go on"
            (is (str/includes? (:result r) "webfetch"))
            (is (str/includes? (:result r) "turn 4"))))
        (testing "a read-only command still runs"
          (is (= :allow (get-in (shell ctx "ls") [:policy :effect]))))
        (testing "yolo is still yolo"
          (with-redefs [approval/policy (constantly {:mode :yolo :wait-ms 1 :on-timeout :deny})]
            (is (= :allow (get-in (shell ctx "touch made") [:policy :effect])))))
        (testing "a branch that read nothing untrusted is not affected"
          (is (= :allow (get-in (shell (assoc ctx :branch {:id "B2"}) "touch made")
                                [:policy :effect])))))
      (finally (db/close c)))))

(deftest a-person-is-shown-the-call-and-its-gaps
  (let [c (db/open! ":memory:")
        ctx {:conn c :run-id (runs/start-run! c {:problem "p"}) :branch {:id "B1"} :branch-id "B1" :turn 2}
        asked (atom nil)]
    (try
      (flow/observe! ctx (flow/delta flow/table "webfetch" {:url "u"}) "webfetch")
      (with-redefs [approval/policy (constantly {:mode :block :wait-ms 1 :on-timeout :deny})
                    approval/request! (fn [q] (reset! asked q) "id")
                    approval/await! (fn [_ _ d] (assoc d :timed-out true))]
        (shell ctx "touch made"))
      (is (= "touch made" (:input @asked)))
      (is (= [:trust] (mapv :gap (:gaps @asked))))
      (finally (db/close c)))))

;; --- every tool, through the one chokepoint -----------------------------------

(deftest a-web-read-through-the-tools-lowers-the-branch
  (let [c (db/open! ":memory:")
        ctx {:conn c :run-id (runs/start-run! c {:problem "p"}) :branch {:id "B1"} :turn 5
             :tool-name "webfetch" :args {:url "https://example.com"}}]
    (try
      (with-redefs [base/run-tool (fn [{:keys [branch]}] {:branch branch :result "page" :category :success})]
        (tools/run-tool ctx))
      (is (= :untrusted (:trust (flow/label-of ctx))))
      (is (= {:turn 5 :tool "webfetch"} (get-in (flow/label-of ctx) [:because :trust])))
      (finally (db/close c)))))

(deftest a-call-refused-before-it-ran-lowers-nothing
  (let [c (db/open! ":memory:")
        ctx {:conn c :run-id (runs/start-run! c {:problem "p"}) :branch {:id "B1"} :turn 5
             :tool-name "webfetch" :args {:url "https://example.com"}}]
    (try
      (with-redefs [base/run-tool (fn [{:keys [branch]}] {:branch branch :result "no" :category :mechanics})]
        (tools/run-tool ctx))
      (is (= :trusted (:trust (flow/label-of ctx))))
      (finally (db/close c)))))

(deftest a-branch-holding-private-data-does-not-fetch-without-a-person
  (let [c (db/open! ":memory:")
        ctx {:conn c :run-id (runs/start-run! c {:problem "p"}) :branch {:id "B1"} :turn 6
             :tool-name "webfetch" :args {:url "https://example.com/?q=secret"}}
        ran (atom false)]
    (try
      (flow/observe! (assoc ctx :turn 2) (:outside-read flow/table) "read_file")
      (with-redefs [base/run-tool (fn [{:keys [branch]}] (reset! ran true)
                                    {:branch branch :result "page" :category :success})]
        (let [r (tools/run-tool ctx)]
          (is (not @ran) "the request never went out")
          (is (= [:audience] (mapv :gap (get-in r [:policy :gaps]))))
          (is (= [:authorize] (mapv :plan (get-in r [:policy :remedies]))))
          (is (str/includes? (:result r) "read_file"))
          (is (str/includes? (:result r) "turn 2")))
        (testing "yolo lets it through, as it lets every ask through"
          (with-redefs [approval/policy (constantly {:mode :yolo :wait-ms 1 :on-timeout :deny})]
            (tools/run-tool ctx)
            (is @ran))))
      (finally (db/close c)))))

(deftest reading-outside-the-project-narrows-the-branch
  (let [c (db/open! ":memory:")
        root (str (fs/create-temp-dir))
        other (str (fs/create-temp-dir))
        ctx {:conn c :run-id (runs/start-run! c {:problem "p"}) :branch {:id "B1"} :turn 3
             :root root :tool-name "read_file"}]
    (spit (str other "/notes.txt") "private")
    (try
      (with-redefs [approval/policy (constantly {:mode :yolo :wait-ms 1 :on-timeout :deny})]
        (is (some? (samizdat.agent.files/resolve-read! ctx [] (str other "/notes.txt")))))
      (is (= :operator (:audience (flow/label-of ctx))))
      (testing "a read inside it does not"
        (let [ctx2 (assoc ctx :branch {:id "B2"})]
          (spit (str root "/a.txt") "x")
          (samizdat.agent.files/resolve-read! ctx2 [] "a.txt")
          (is (= :project (:audience (flow/label-of ctx2))))))
      (finally (db/close c)))))
