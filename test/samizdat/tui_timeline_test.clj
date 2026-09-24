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

(ns samizdat.tui-timeline-test
  "A branch's conversation as one timeline of who said what
  (karamazov-tq7m.4): the person, the agent, the critic, the supervisor, the
  harness — in the order it happened."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [samizdat.tui.state :as st]
            [samizdat.tui.timeline :as tl]))

(def ^:private settings
  {:issued-by {"human" :user "supervisor" :supervisor "watch" :supervisor}
   :notes {"critic-score" {:role :critic :say [:reply]}
           "oversight" {:role :supervisor :say [:notes]}}})

(def ^:private state
  {:run-id "R" :branch-id "B1"
   :detail {:run {:problem "fix the parser" :started_at "2026-09-23T10:00:00.000Z"}
            :interventions [{:id 1 :branch_id "B1" :kind "message" :issued_by "human"
                             :payload "{\"text\":\"look at the lexer\"}"
                             :status "applied" :created_at "2026-09-23T10:00:03.000Z"}
                            {:id 2 :branch_id "B2" :kind "message" :issued_by "human"
                             :payload "not this branch" :created_at "2026-09-23T10:00:03.500Z"}
                            {:id 3 :branch_id nil :kind "cull" :issued_by "supervisor"
                             :payload "B3 is stuck" :status "pending"
                             :created_at "2026-09-23T10:00:06.000Z"}]}
   :branch {:turns [{:turn 1 :tool_name "read_file" :args "{\"path\":\"src/lex.clj\"}"
                     :result "(ns lex)" :category "neutral" :created_at "2026-09-23T10:00:02.000Z"}
                    {:turn 2 :tool_name "edit_file" :args "{\"path\":\"src/lex.clj\"}"
                     :result "boom" :category "failure" :created_at "2026-09-23T10:00:05.000Z"}]
            :notes [{:id 9 :kind "critic-score" :data {:reply "progress is slow"}
                     :created_at "2026-09-23T10:00:04.000Z"}
                    {:id 10 :kind "oversight" :data {:notes nil :verdict "done"}
                     :created_at "2026-09-23T10:00:07.000Z"}]}
   :turn-text {1 {:assistant_text "Reading the lexer." :reasoning_text "hmm"}
               2 {:assistant_text "Fixing it."}}})

(deftest everyone-speaks-in-the-order-it-happened
  (let [es (tl/entries state settings)]
    (is (= [[:user :say] [:agent :thinking] [:agent :say] [:agent :tool]
            [:user :say] [:critic :say] [:agent :say] [:agent :tool]
            [:supervisor :say] [:supervisor :say]]
           (mapv (juxt :role :kind) es)))
    (testing "the problem opens it, as the person's"
      (is (= "fix the parser" (:text (first es)))))
    (testing "a steer says what it said, not the JSON it went over the wire as"
      (is (= "look at the lexer" (:text (nth es 4)))))
    (testing "another branch's steer is not in this branch's story"
      (is (not-any? #(= "not this branch" (:text %)) es)))
    (testing "a steer that is not a message says what kind it is, and one not applied yet says so"
      (let [cull (nth es 8)]
        (is (= "[cull] B3 is stuck" (:text cull)))
        (is (:pending? cull))))
    (testing "a note with nothing to say at its path says what it has"
      (is (re-find #"done" (:text (last es)))))))

(deftest a-tool-entry-carries-what-its-chamber-draws
  (let [[_ _ _ read-tool] (tl/entries state settings)]
    (is (= {:tool "read_file" :arg "src/lex.clj" :result "(ns lex)" :failed? false :turn 1}
           (select-keys read-tool [:tool :arg :result :failed? :turn]))))
  (let [edit (nth (tl/entries state settings) 7)]
    (is (:failed? edit))))

(deftest keys-are-stable-as-the-story-grows
  (let [before (mapv :key (tl/entries state settings))
        after (mapv :key (tl/entries (update-in state [:branch :turns] conj
                                                {:turn 3 :tool_name "done" :result "ok"
                                                 :created_at "2026-09-23T10:00:09.000Z"})
                                     settings))]
    (is (= before (take (count before) after)))))

;; --- following the bottom -------------------------------------------------------
;;
;; A pane scrolls by ROWS (ftxui's :scroll): :top is the first row shown, nil
;; follows the bottom. The pane reports {:top :max :rows}; keys page from it.

(deftest a-pane-follows-the-bottom-until-paged-up
  (let [s (st/scrolled (st/initial "b") :conversation {:top nil :max 100 :rows 22})]
    (is (nil? (st/scroll-top s :conversation)) "following")
    (let [up (st/page s :conversation -1)]
      (is (= 80 (st/scroll-top up :conversation)) "a page is the rows shown, less two")
      (is (= 60 (st/scroll-top (st/page up :conversation -1) :conversation)))
      (is (= 0 (st/scroll-top (reduce #(st/page %1 :conversation %2) up (repeat 9 -1))
                              :conversation))
          "not past the top")
      (testing "paging back down to the end follows again"
        (is (nil? (st/scroll-top (st/page up :conversation 1) :conversation))))
      (is (nil? (st/scroll-top (st/follow up :conversation) :conversation))))
    (testing "what the pane reports is what is kept"
      (is (= 12 (st/scroll-top (st/scrolled s :activity {:top 12 :max 40 :rows 10}) :activity))))))

(deftest only-folds-that-are-drawn-are-folds
  (let [es (tl/entries (assoc-in state [:branch :turns 1 :result] "1\n2\n3\n4\n5") settings)]
    (is (= ["t1/thinking" "t2/tool/more"] (vec (tl/fold-ids es {:result-lines 4})))
        "a four-line result shows whole; a five-line one folds its fifth")))

(deftest ctrl-o-opens-the-newest-fold-and-shuts-it-again
  (let [ids ["t1/thinking" "t1/tool/more" "t2/tool/more"]
        s (st/initial "b")
        opened (st/toggle-latest-fold s ids)]
    (is (= #{"t2/tool/more"} (:expanded opened)) "the newest collapsed thing opens")
    (is (= #{} (:expanded (st/toggle-latest-fold opened ids))) "pressed again, it shuts")
    (is (= s (st/toggle-latest-fold s [])) "nothing to open, nothing changes")))

(deftest the-agents-words-are-drawn-without-markup
  ;; A reply's call syntax is the chamber's to show and its <think> the
  ;; thinking fold's; drawn raw they were a column of `</invoke>` lines.
  (let [s {:run-id "R" :branch-id "B1"
           :branch {:turns [{:turn 1 :tool_name "shell" :args "{\"command\":\"ls\"}"
                             :result "a" :category "success" :created_at "t1"}]}
           :turn-text {1 {:assistant_text "<think>plan it</think>Listing.\n<invoke name=\"shell\"></invoke>\n</invoke>"
                          :reasoning_text "first"}}}
        es (tl/entries s {})
        say (first (filter #(= :say (:kind %)) es))
        thinking (first (filter #(= :thinking (:kind %)) es))]
    (is (= "Listing." (:text say)))
    (is (= "first\nplan it" (:text thinking)) "inline reasoning joins the thinking fold"))
  (testing "a turn that said nothing but a call has no say entry"
    (let [s {:branch {:turns [{:turn 1 :tool_name "shell" :created_at "t1"}]}
             :turn-text {1 {:assistant_text "```tool-call\n{\"name\": \"shell\"}\n```"}}}]
      (is (not-any? #(= :say (:kind %)) (tl/entries s {}))))))

(deftest a-notes-reasoning-folds-too
  (let [s {:branch {:notes [{:id 7 :kind "critic-score" :created_at "t1"
                             :data "{\"reply\":\"<think>hmm</think>Progress 2.\"}"}]}}
        es (tl/entries s settings)]
    (is (= ["Progress 2."] (map :text (filter #(= :say (:kind %)) es))))
    (is (= ["hmm"] (map :text (filter #(= :thinking (:kind %)) es))))))

(deftest the-reply-being-written-is-the-last-entry
  (let [s {:branch-id "B1"
           :branch {:turns [{:turn 1 :tool_name "shell" :created_at "t1"}]}
           :live {"B1" {:text "<think>so</think>Writing it\n<invoke name=\"x\">" :reasoning "first"}
                  "B2" {:text "not this branch"}}}
        es (tl/entries s {})]
    (is (= [[:agent :thinking "first\nso"] [:agent :say "Writing it"]]
           (mapv (juxt :role :kind :text) (take-last 2 es))))
    (is (every? :live? (take-last 2 es)))))

;; --- a frame that changed nothing the conversation shows (karamazov-iimi) -----

(deftest a-keystroke-does-not-rebuild-the-conversation
  ;; Every keystroke is a frame. Rebuilding the timeline from every turn row
  ;; was 108ms of a 170ms frame on a 3067-turn branch, for a vector that
  ;; had not changed: typing only moves :input.
  (let [es (tl/entries state settings)]
    (is (identical? es (tl/entries (assoc state :input "a") settings)))
    (is (identical? es (tl/entries (assoc state :input "ab" :scroll {:x 1}) settings)))
    (testing "what the conversation shows still reaches it"
      (let [more (update-in state [:branch :turns] conj
                            {:turn 3 :tool_name "done" :result "ok"
                             :created_at "2026-09-23T10:00:09.000Z"})]
        (is (= "t3/tool" (:key (last (tl/entries more settings)))))
        (is (= "later" (:text (last (tl/entries (assoc state :local-notes
                                                       [{:key "l1" :at "2026-09-23T11:00:00.000Z"
                                                         :text "later"}])
                                                settings)))))))
    (testing "the reply being streamed is folded in on top of the cached history"
      (let [live (assoc state :live {"B1" {:text "Writing"}})]
        (is (= "Writing" (:text (last (tl/entries live settings)))))
        (is (= (count es) (dec (count (tl/entries live settings)))))))))

;; --- a branch that numbers its turns more than once (karamazov-qqqr) ----------

(def ^:private repeating
  "The supervisor's SUP branch as runs before karamazov-pefk wrote it: every
  oversight pass numbered its turns from 1 again."
  {:branch-id "SUP"
   :branch {:turns (vec (map-indexed (fn [i n] {:id (inc i) :turn n :tool_name "shell"
                                                :result (str "r" i)
                                                :created_at (format "2026-09-23T10:00:%02d.000Z" i)})
                                     [1 2 3 1 2 1 2 3]))}
   :turn-text {1 {:assistant_text "the first pass's first turn"}}})

(deftest a-repeated-turn-number-is-still-its-own-entry
  (let [es (tl/entries repeating {})
        ks (map :key es)]
    (is (= (count ks) (count (distinct ks))) "keys are what ftxui and the folds are keyed by")
    (is (= ["t1/tool" "t2/tool" "t3/tool" "t1.2/tool" "t2.2/tool" "t1.3/tool" "t2.3/tool" "t3.2/tool"]
           (vec (filter #(str/ends-with? % "/tool") ks)))
        "the first of a number keeps the plain key, so a branch that never repeats is unchanged")
    (is (= (count (distinct (map :turn-key es))) 8) "each row is its own turn for the window")
    (testing "a turn number's prose is the first row's, which is the row the server returns"
      (is (= ["the first pass's first turn"] (keep #(when (= :say (:kind %)) (:text %)) es))))))

(deftest reasoning-the-reply-repeats-is-shown-once
  ;; A provider that returns reasoning_text AND leaves the same text in a
  ;; <think> block drew the thinking fold with every paragraph twice.
  (let [s {:branch {:turns [{:turn 1 :tool_name "shell" :created_at "t1"}]}
           :turn-text {1 {:assistant_text "<think>check the tree</think>Reading."
                          :reasoning_text "check the tree"}}}]
    (is (= ["check the tree"] (keep #(when (= :thinking (:kind %)) (:text %)) (tl/entries s {})))))
  (testing "different reasoning from each source is still joined"
    (let [s {:branch {:turns [{:turn 1 :tool_name "shell" :created_at "t1"}]}
             :turn-text {1 {:assistant_text "<think>second</think>ok" :reasoning_text "first"}}}]
      (is (= ["first\nsecond"] (keep #(when (= :thinking (:kind %)) (:text %)) (tl/entries s {})))))))

(deftest the-runs-answer-ends-the-conversation
  ;; The answer a finished run gave was only a truncated claim in a side
  ;; panel (karamazov-ttrn). It is what the person asked for; it is the last
  ;; thing the conversation says.
  (let [s (assoc-in state [:detail :run :final_answer] "The project is a parser.")
        es (tl/entries s settings)]
    (is (= {:role :agent :kind :say :text "The project is a parser." :final? true}
           (select-keys (last es) [:role :kind :text :final?])))
    (is (not-any? :final? (tl/entries state settings)) "no answer, no entry")))
