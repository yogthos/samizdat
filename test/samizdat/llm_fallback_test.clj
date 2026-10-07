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

(ns samizdat.llm-fallback-test
  "A role with fallback models (karamazov-0e2c.9, after iFixAi's judge
  fallback chain): a model that fails is retired for the rest of the run,
  and the role goes on with the next one."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [samizdat.config :as config]
            [samizdat.llm.client :as client]))

(use-fixtures :each (fn [t] (client/forget-retired! "R") (try (t) (finally (client/forget-retired! "R")))))

(defn- cfg [model] {:provider :deepseek :model model})

(deftest a-failing-model-is-retired-and-the-next-one-answers
  (let [calls (atom [])
        primary (assoc (cfg "a") :fallback-scope "R"
                       :fallbacks [{:adapter :b-adapter :config (cfg "b")}])]
    (with-redefs [client/chat-one (fn [_ c _ _]
                                    (swap! calls conj (:model c))
                                    (if (= "a" (:model c))
                                      (throw (ex-info "a is down" {:reason :call-failed}))
                                      {:content (str "from " (:model c))}))]
      (is (= "from b" (:content (client/chat :a-adapter primary [{:role "user" :content "q"}]))))
      (is (= ["a" "b"] @calls))
      (testing "the next call skips the retired model"
        (reset! calls [])
        (client/chat :a-adapter primary [{:role "user" :content "q"}])
        (is (= ["b"] @calls)))
      (testing "for this run only"
        (reset! calls [])
        (client/chat :a-adapter (assoc primary :fallback-scope "R2") [{:role "user" :content "q"}])
        (is (= "a" (first @calls)))
        (client/forget-retired! "R2")))))

(deftest every-candidate-failing-is-the-error
  (with-redefs [client/chat-one (fn [_ c _ _] (throw (ex-info (str (:model c) " down") {})))]
    (is (thrown? Exception
                 (client/chat :a (assoc (cfg "a") :fallback-scope "R"
                                        :fallbacks [{:adapter :b :config (cfg "b")}])
                              [])))
    (testing "and with all of them retired the role is tried again, not dead for the run"
      (let [calls (atom 0)]
        (with-redefs [client/chat-one (fn [_ _ _ _] (swap! calls inc) {:content "back"})]
          (is (= "back" (:content (client/chat :a (assoc (cfg "a") :fallback-scope "R"
                                                         :fallbacks [{:adapter :b :config (cfg "b")}])
                                               [])))))))))

(deftest a-cancel-is-not-a-failure
  (let [calls (atom [])]
    (with-redefs [client/chat-one (fn [_ c _ _] (swap! calls conj (:model c))
                                    (throw (ex-info "cancelled" {:samizdat.cancel/signal true})))
                  samizdat.cancel/control-signal? (fn [e] (contains? (ex-data e) :samizdat.cancel/signal))]
      (is (thrown? Exception (client/chat :a (assoc (cfg "a") :fallback-scope "R"
                                                   :fallbacks [{:adapter :b :config (cfg "b")}])
                                          [])))
      (is (= ["a"] @calls) "the run is stopping; the next model is not asked"))))

(deftest a-role-may-name-its-fallbacks
  (let [c {:providers {:fast {:type :deepseek :model "deepseek-flash"}
                       :strong {:type :glm :model "glm-5.3"}}
           :roles {:critic [:fast :strong]}}
        llm (config/role-llm c {:provider :local} :critic)]
    (is (= "deepseek-flash" (:model llm)))
    (is (= ["glm-5.3"] (mapv :model (:fallback-llms llm))))))
