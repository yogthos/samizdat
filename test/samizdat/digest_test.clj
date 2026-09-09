;; samizdat - a claim-first verification harness
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

(ns samizdat.digest-test
  "read_digest, the large-read refusal that points at it, and the roles that
  make both cheap (karamazov-b76m).

  The pattern is Spotify's shunt: most of what a coding agent does is moving
  text around, not reasoning, and a read that returns a digest instead of a
  page keeps the file out of the expensive model's context. The enforcement
  lives where every other withhold lives — phases.edn, consulted before
  dispatch — because a rule the model may ignore is a suggestion."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is use-fixtures]]
            [jolt.fs :as fs]
            [mycelium.cell :as cell]
            [samizdat.agent.gates :as gates]
            [samizdat.agent.tools :as tools]
            [samizdat.agent.tools.base :as tools-base]
            [samizdat.cells :as cells]
            [samizdat.config :as config]
            [samizdat.llm.client :as llm]
            [samizdat.store.db :as db]
            [samizdat.store.journal :as journal]
            [samizdat.store.runs :as runs]))

(def ^:private root (atom nil))

(use-fixtures :each
  (fn [f]
    (reset! root (str "/tmp/samizdat-digest-" (random-uuid)))
    (fs/create-dirs (str @root "/src"))
    (try (f) (finally (fs/delete-tree @root)))))

(defn- file! [rel lines]
  (let [p (str @root "/" rel)]
    (fs/create-dirs (subs p 0 (str/last-index-of p "/")))
    (spit p (str/join "\n" lines))
    p))

(defn- capturing-chat
  "A model that answers `reply`; every call's adapter, config and messages
  land in `seen`."
  [seen reply]
  (fn [adapter cfg messages & _]
    (swap! seen conj {:adapter adapter :config cfg :messages messages})
    {:content reply :finish-reason "stop"
     ;; The shape the CLIENT returns, not the wire's: adapters normalise to
     ;; kebab keys (llm/adapter/openai.clj), and a fake in the wire's shape
     ;; passes tests that production would fail.
     :usage {:prompt-tokens 10 :completion-tokens 3 :total-tokens 13}}))

(defn- user-text [call] (:content (last (:messages call))))

(defn- ctx [& {:as more}]
  (merge {:branch {:id "B1" :phase :build :task {:id "t1"}}
          :root @root
          :llm-adapter :run-adapter
          :llm-config {:provider :openai :model "gpt-4o" :max-tokens 16384}
          :config {:run {}}
          :tool-name "read_digest"}
         more))

;; --- the tool ----------------------------------------------------------------

(deftest read-digest-sends-the-files-to-the-reader-and-returns-only-its-answer
  (file! "src/big.clj" (map #(str "(defn f" % " [] " % ")") (range 400)))
  (let [seen (atom [])]
    (with-redefs [llm/chat (capturing-chat seen "- f0: returns 0\n- f399: returns 399")]
      (let [r (tools-base/run-tool
               (ctx :args {:paths ["src/big.clj"] :question "what do f0 and f399 return?"}))]
        (is (= :neutral (:category r)) "a digest establishes nothing, like a read")
        (is (= "- f0: returns 0\n- f399: returns 399" (:result r))
            "the answer is the reader's bullets and nothing else")
        (is (= 1 (count @seen)) "one call to the reader")
        (is (str/includes? (user-text (first @seen)) "(defn f399 [] 399)")
            "the whole file went to the reader")
        (is (str/includes? (user-text (first @seen)) "what do f0 and f399 return?")
            "with the question")))))

(deftest the-reader-role-answers-when-one-is-configured
  (file! "src/a.clj" ["(ns a)" "(def x 1)"])
  (let [seen (atom [])]
    (with-redefs [llm/chat (capturing-chat seen "- x: 1")]
      (testing "no assignment: the branch's own model, still a digest"
        (tools-base/run-tool (ctx :args {:paths "src/a.clj" :question "x?"}))
        (is (= "gpt-4o" (get-in (first @seen) [:config :model]))))
      (reset! seen [])
      (testing "an assignment: the reader's model and adapter"
        (tools-base/run-tool
         (ctx :args {:paths "src/a.clj" :question "x?"}
              :config {:run {:role-models {:reader {:provider "deepseek" :model "cheap"}}}}))
        (is (= "cheap" (get-in (first @seen) [:config :model])))
        (is (= :deepseek (get-in (first @seen) [:config :provider])))
        (is (not= :run-adapter (:adapter (first @seen))) "a real adapter for the provider")))))

(deftest anchors-give-the-reader-addresses-to-cite
  ;; A digest that names `line:hash` anchors hands the branch valid `patch`
  ;; coordinates without a second read — the one thing Spotify's reader could
  ;; not do, because its summaries carried no reliable line numbers.
  (file! "src/a.clj" ["(ns a)" "(def x 1)"])
  (let [seen (atom [])]
    (with-redefs [llm/chat (capturing-chat seen "- 2:abc x is 1")]
      (tools-base/run-tool (ctx :args {:paths ["src/a.clj"] :question "x?" :anchors true}))
      (is (re-find #"(?m)^2:[0-9a-f]{3}│ \(def x 1\)" (user-text (first @seen)))
          "every line carries its line:hash anchor"))))

(deftest a-digest-call-is-checked-like-a-read
  (let [seen (atom [])]
    (with-redefs [llm/chat (capturing-chat seen "never")]
      (testing "no paths, or no question"
        (is (= :mechanics (:category (tools-base/run-tool (ctx :args {:question "x?"})))))
        (is (= :mechanics (:category (tools-base/run-tool (ctx :args {:paths ["src/a.clj"]}))))))
      (testing "a path outside the root is refused, not read"
        (let [r (tools-base/run-tool (ctx :args {:paths ["../../etc/passwd"] :question "x?"}))]
          (is (= :mechanics (:category r)))
          (is (str/includes? (:result r) "outside"))))
      (testing "a missing file is named"
        (let [r (tools-base/run-tool (ctx :args {:paths ["src/nope.clj"] :question "x?"}))]
          (is (= :mechanics (:category r)))
          (is (str/includes? (:result r) "src/nope.clj"))))
      (is (empty? @seen) "and the reader was never called for any of them"))))

(deftest the-reader-input-is-capped-and-says-so
  (let [cap (:input-chars (gates/digest-policy))]
    (file! "src/huge.clj" (repeat (inc (quot cap 10)) "0123456789"))
    (let [seen (atom [])]
      (with-redefs [llm/chat (capturing-chat seen "- big")]
        (tools-base/run-tool (ctx :args {:paths ["src/huge.clj"] :question "?"}))
        (is (<= (count (user-text (first @seen))) (+ cap 800))
            "the material stops at the cap")
        (is (str/includes? (user-text (first @seen)) "not sent")
            "and says what was left out, so the reader does not guess at it")))))

(deftest a-digest-is-journaled-with-its-usage
  ;; The saving is the point, and a saving nobody can read back is a claim.
  (file! "src/a.clj" ["(ns a)"])
  (let [conn (db/open! ":memory:")
        run-id (runs/start-run! conn {:problem "p"})]
    (with-redefs [llm/chat (capturing-chat (atom []) "- a")]
      (tools-base/run-tool (ctx :args {:paths ["src/a.clj"] :question "?"}
                                :conn conn :run-id run-id)))
    (let [[n] (journal/notes conn run-id :digest)]
      (is (some? n) "one :digest note")
      (is (= ["src/a.clj"] (:paths n)))
      (is (pos? (:chars-in n)) "and how much the branch did not have to read"))
    ;; karamazov-2rqb.1: the tokens live on the side-call row, not in the note,
    ;; because that is the row the run's budget sums. One home per fact.
    (let [u (journal/run-usage conn run-id)]
      (is (= 1 (:side-calls u)))
      (is (= 13 (:total-tokens u))
          "the reader's bill lands where spent-tokens can see it"))))

(deftest the-readers-thinking-is-not-the-answer
  ;; The client merges a reasoning model's thinking into :content as a
  ;; <think> block for the loop's fence parser to strip. The validation run's
  ;; first real digest (deepseek-v4-flash) came back as 1,790 tokens of
  ;; reasoning wrapped around eight bullets — prose, and far over budget, the
  ;; exact payload the routing exists to keep out of the branch.
  (file! "src/a.clj" ["(ns a)" "(def x 1)"])
  (let [budget (:budget-chars (gates/digest-policy))]
    (testing "a think block is dropped"
      (with-redefs [llm/chat (capturing-chat (atom []) "<think>let me look\nat x</think>\n- x: 1")]
        (is (= "- x: 1" (:result (tools-base/run-tool (ctx :args {:paths ["src/a.clj"] :question "x?"})))))))
    (testing "an answer far over budget is clipped, and says so"
      (with-redefs [llm/chat (capturing-chat (atom []) (str/join "\n" (repeat (* 3 budget) "- a")))]
        (let [r (:result (tools-base/run-tool (ctx :args {:paths ["src/a.clj"] :question "x?"})))]
          (is (<= (count r) (+ (* 2 budget) 200)) "twice the budget is the ceiling")
          (is (str/includes? r "clipped") "and the clip is named"))))))

;; --- the refusal ---------------------------------------------------------------

(deftest an-untargeted-read-of-a-large-file-is-refused-toward-the-digest
  (let [min-lines (:min-lines (gates/digest-policy))
        call (fn [args] (tools-base/phase-refusal (ctx :tool-name "read_file" :args args)))]
    (file! "src/big.clj" (repeat (+ min-lines 5) "(comment x)"))
    (file! "src/small.clj" (repeat 3 "(comment x)"))
    (let [r (call {:path "src/big.clj"})]
      (is (some? r) "a whole-file read of a large file is refused")
      (is (:policy-refusal? r) "as policy, not as a failure")
      (is (str/includes? (:result r) "read_digest") "and told where to go")
      (is (str/includes? (:result r) "src/big.clj") "for which file"))
    (is (nil? (call {:path "src/big.clj" :offset 10})) "a targeted read passes")
    (is (some? (call {:path "src/big.clj" :offset 0})) "offset 0 is the whole file under another name")
    (is (nil? (call {:path "src/big.clj" :limit 40})) "so does a limited one")
    (is (nil? (call {:path "src/small.clj"})) "a small file is read whole")
    (is (nil? (call {:path "src/nope.clj"})) "a missing file is read_file's to report")
    (is (nil? (tools-base/phase-refusal (ctx :tool-name "grep" :args {:pattern "x"})))
        "only reads are steered")))

;; --- the roles ---------------------------------------------------------------

(deftest role-llm-resolves-an-assignment-and-nothing-else
  (let [run {:provider :openai :model "gpt-4o"}]
    (is (nil? (config/role-llm {:run {}} run :reader)) "no assignment, no override")
    (let [l (config/role-llm {:run {:role-models {:reader {:provider "deepseek" :model "cheap"}}}}
                             run :reader)]
      (is (= :deepseek (:provider l)))
      (is (= "cheap" (:model l))))
    (testing "no provider in the spec: the run's provider, another model"
      (let [l (config/role-llm {:run {:role-models {:reader {:model "mini"}}}} run :reader)]
        (is (= :openai (:provider l)))
        (is (= "mini" (:model l)))))))

(deftest a-fold-summarises-on-the-summarizer-role-when-one-is-assigned
  ;; The fold's summary is the most delegable call in the loop: old history
  ;; in, a short structured summary out. It used to run on the branch's own
  ;; model. The window math stays on the branch's config — only the summary
  ;; call moves.
  (cells/load-cells!)
  (let [handler (:handler (cell/get-cell :compaction/fold))
        msgs (vec (for [i (range 40)]
                    {:role (if (even? i) "user" "assistant")
                     :content (str "turn " i " " (apply str (repeat 200 "x")))}))
        data {:branch {:id "B1" :messages msgs}
              :compaction/tier :fold
              :compaction/before 1000000000}
        summary "## Active Task\nFix step-move.\n## Goal\nShip the game.\n## Goal\nx"
        run-ctx (fn [role-models]
                  (let [conn (db/open! ":memory:")]
                    {:conn conn :run-id "run-fold"
                     :llm-adapter :run-adapter
                     :llm-config {:provider :openai :model "gpt-4o" :max-tokens 16384
                                  :context-window 32768}
                     :config {:run {:role-models role-models}}}))]
    (testing "no assignment: the branch's own model"
      (let [seen (atom [])]
        (with-redefs [llm/chat (capturing-chat seen summary)]
          (handler (run-ctx nil) data))
        (is (= 1 (count @seen)) "one summary call")
        (is (= "gpt-4o" (get-in (first @seen) [:config :model])))))
    (testing "an assignment: the summarizer's model"
      (let [seen (atom [])]
        (with-redefs [llm/chat (capturing-chat seen summary)]
          (handler (run-ctx {:summarizer {:provider "deepseek" :model "cheap-sum"}}) data))
        (is (= "cheap-sum" (get-in (first @seen) [:config :model])))
        (is (= :deepseek (get-in (first @seen) [:config :provider])))))))
