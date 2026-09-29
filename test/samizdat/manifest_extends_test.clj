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

(ns samizdat.manifest-extends-test
  "One turn chain, extended by every turn-at-a-time role (karamazov-xtd3).
  loop, worker, reviewer and supervisor carried four copies of the same
  graph; now turn.edn holds it and each says :extends \"turn\"."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [samizdat.agent.tools.base :as base]
            [samizdat.agent.tools.manifest]
            [samizdat.manifests :as manifests]
            [samizdat.store.db :as db]
            [samizdat.store.userspace :as us]
            [samizdat.userspace :as userspace]
            [samizdat.workflow :as wf]))

(def ^:private roles ["loop" "worker" "reviewer" "supervisor"])

(defn- resolved [nm] (manifests/read-definition (manifests/manifest-body! nm)))

(defn- raw [nm] (edn/read-string (slurp (io/resource (str "manifests/" nm ".edn")))))

(deftest the-four-turn-roles-share-one-chain
  (let [turn (resolved "turn")]
    (doseq [nm roles
            :let [d (resolved nm)]]
      (testing nm
        (is (= "turn" (:extends (raw nm))) "the file extends turn rather than copying it")
        (is (= (:cells turn) (select-keys (:cells d) (keys (:cells turn))))
            "every turn node, with the same cell")
        (is (= (dissoc (:edges turn) :route) (select-keys (:edges d) (keys (dissoc (:edges turn) :route))))
            "every turn edge but the tail's")
        (is (= (:dispatches turn) (:dispatches d)))
        (is (every? (set (:invariants d)) (:invariants turn)) "the turn's invariants hold in every role")
        (is (= (:input-schema turn) (:input-schema d)))
        (is (not (:fragment? d)) "being a fragment is not inherited")
        (is (not (str/blank? (:description d))))
        (is (some? (manifests/compile-loop d)) "it compiles")))))

(deftest turn-is-a-fragment-and-not-on-the-menu
  (let [with-conn (db/open! ":memory:")
        entry (some #(when (= "turn" (:name %)) %) (manifests/catalog with-conn))]
    (is (:fragment? entry) "the catalogue says what it is")
    (is (not (str/includes? (wf/render-catalog with-conn) "- turn"))
        "and the switch menu does not offer it")))

(deftest how-an-extension-resolves
  (binding [userspace/*candidate*
            {[:manifest "b"] (pr-str {:description "base" :fragment? true
                                      :cells {:start :x/a :mid :x/b}
                                      :edges {:start :mid :mid :end}
                                      :invariants [{:type :must-follow :if :start :then :mid}]})}]
    (let [d (manifests/read-definition
             (pr-str {:extends "b"
                      :cells {:mid nil :other :x/c}
                      :edges {:mid nil :start :other :other :end}
                      :invariants [{:type :must-follow :if :start :then :other}]}))]
      (is (= {:start :x/a :other :x/c} (:cells d)) "a child entry wins, a nil removes one")
      (is (= {:start :other :other :end} (:edges d)))
      (is (= 2 (count (:invariants d))) "invariants are the base's plus the child's")
      (is (= "base" (:description d)) "a key the child lacks is the base's")
      (is (not (contains? d :fragment?)))
      (is (not (contains? d :extends)) "the resolved definition carries no link"))
    (testing ":replaces makes a key the child's alone"
      (let [d (manifests/read-definition
               (pr-str {:extends "b" :replaces [:invariants]
                        :invariants [{:type :must-follow :if :start :then :other}]}))]
        (is (= [{:type :must-follow :if :start :then :other}] (:invariants d)))))))

(deftest a-missing-base-or-a-cycle-is-refused-by-name
  (let [e (try (manifests/read-definition (pr-str {:extends "no-such-base" :cells {}}))
               nil (catch Throwable e e))]
    (is (some? e))
    (is (str/includes? (ex-message e) "no-such-base")))
  (binding [userspace/*candidate* {[:manifest "c1"] (pr-str {:extends "c2"})
                                   [:manifest "c2"] (pr-str {:extends "c1"})}]
    (let [e (try (manifests/read-definition (pr-str {:extends "c1"}))
                 nil (catch Throwable e e))]
      (is (some? e))
      (is (str/includes? (ex-message e) "c1")))))

(defn- run-manifest [conn args]
  (base/run-tool {:branch {:id "B1"} :conn conn :tool-name "manifest" :args args}))

(defn- rename-node [d from to]
  (let [kw #(if (= from %) to %)
        edge (fn [e] (if (map? e) (update-vals e kw) (kw e)))]
    (-> d
        (update :cells update-keys kw)
        (update :edges #(into {} (map (fn [[k v]] [(kw k) (edge v)])) %))
        (update :dispatches update-keys kw))))

(deftest saving-a-base-that-breaks-an-extension-is-refused
  (let [conn (db/open! ":memory:")
        ;; Compiles on its own; loop's tail still names :route, so loop does not.
        broken (pr-str (rename-node (raw "turn") :route :router))]
    (is (some? (manifests/compile-loop (manifests/read-definition broken))) "the base alone is fine")
    (let [r (run-manifest conn {:action "save" :name "turn" :edn broken
                                :rationale "rename the router"})]
      (is (= :mechanics (:category r)) (:result r))
      (is (re-find #"loop|worker|reviewer" (:result r)) "the refusal names what it broke")
      (is (not (str/includes? (str (:body (us/load-latest conn :manifest "turn"))) ":router"))
          "nothing was stored"))))

(deftest a-patch-to-an-extension-writes-only-its-own-part
  (let [conn (db/open! ":memory:")]
    (run-manifest conn {:action "save" :name "w2" :rationale "a worker to tune"
                        :edn (slurp (io/resource "manifests/worker.edn"))})
    (let [r (run-manifest conn {:action "patch" :name "w2" :rationale "end without distilling"
                                :ops [{:op "set-edge" :from "route" :label "abandoned" :to "end"}]})
          body (:body (us/load-latest conn :manifest "w2"))
          own (edn/read-string body)]
      (is (= :neutral (:category r)) (:result r))
      (is (= "turn" (:extends own)) "the file still extends the chain")
      (is (not (contains? (:cells own) :infer)) "and does not inline it")
      (is (= :end (get-in own [:edges :route :abandoned])))
      (is (= :end (get-in (manifests/read-definition body) [:edges :route :abandoned]))))
    (testing "a rename reaches the base's invariants through :replaces"
      (let [r (run-manifest conn {:action "patch" :name "w2" :rationale "name the node for what it does"
                                  :ops [{:op "rename-cell" :from "journal" :to "record"}]})
            body (:body (us/load-latest conn :manifest "w2"))
            d (manifests/read-definition body)]
        (is (= :neutral (:category r)) (:result r))
        (is (= "turn" (:extends (edn/read-string body))))
        (is (contains? (:cells d) :record))
        (is (not (contains? (:cells d) :journal)))
        (is (some? (manifests/compile-loop d)))))))
