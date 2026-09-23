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

(ns samizdat.live-model-test
  "Switching a RUNNING run's model, or how hard it thinks (karamazov-tq7m.7):
  the `model` and `effort` interventions apply on arrival, and the next
  request every branch makes goes out under them."
  (:require [clojure.test :refer [deftest testing is]]
            [samizdat.agent.infer :as infer]
            [samizdat.agent.live :as live]
            [samizdat.agent.loop :as aloop]
            [samizdat.api.control :as control]
            [samizdat.store.db :as db]
            [samizdat.store.journal :as journal]
            [samizdat.store.runs :as runs]))

(defn- sent-config
  "The llm config call-model hands the inference seam for `run-id`."
  [run-id]
  (let [seen (atom nil)]
    (with-redefs [infer/complete-fn (fn [ctx] (reset! seen (:llm-config ctx))
                                      (fn [_] {:ok true :content "x"}))]
      (aloop/call-model {:run-id run-id :llm-config {:model "base" :reasoning-effort "high"}}
                        {:id "B1" :messages []}))
    @seen))

(deftest a-model-switch-lands-on-the-next-request
  (let [conn (db/open! ":memory:")
        rid (runs/start-run! conn {:problem "p"})]
    (try
      (is (= "base" (:model (sent-config rid))))
      (let [r (control/intervene! conn rid {:kind "model" :payload "bigger-model"})]
        (is (nil? (:status r)) "applied, not refused")
        (is (= "switched" (get-in r [:body :status]))))
      (is (= "bigger-model" (:model (sent-config rid))) "the next request goes to the new model")
      (is (= "high" (:reasoning-effort (sent-config rid))) "and nothing else changed")
      (control/intervene! conn rid {:kind "effort" :payload "low"})
      (is (= {:model "bigger-model" :reasoning-effort "low"}
             (select-keys (sent-config rid) [:model :reasoning-effort])))
      (testing "the switch is on the record, where the conversation can show it"
        (is (= {:model "bigger-model"} (first (journal/notes conn rid :llm-switch))))
        (is (= 2 (count (journal/notes conn rid :llm-switch)))))
      (testing "a blank model is not a model"
        (is (= 400 (:status (control/intervene! conn rid {:kind "model" :payload "  "})))))
      (testing "another run is not switched"
        (is (= "base" (:model (sent-config "other-run")))))
      (finally (live/forget-run! rid) (db/close conn)))))

(deftest an-ended-run-cannot-be-switched
  (let [conn (db/open! ":memory:")
        rid (runs/start-run! conn {:problem "p"})]
    (runs/finish-run! conn rid :failed nil)
    (is (= 409 (:status (control/intervene! conn rid {:kind "model" :payload "m"}))))
    (is (nil? (live/get rid)))
    (db/close conn)))
