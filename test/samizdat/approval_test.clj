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

(ns samizdat.approval-test
  "Asking a person, and not waiting forever.

  samizdat runs unattended by design: the shell policy's `:ask` refuses the
  call and teaches the model to retry, and a human adds a grant afterwards.
  A blocking gate is the opposite bet, and the whole risk of it is a campaign
  run that hangs at 3am because nobody was watching.

  So the contract these tests hold is not `a human can approve` — that is the
  easy half. It is that EVERY WAIT ENDS: bounded by a deadline from
  gates.edn, resolved to a stated default when it expires, and abandoned
  wholesale when the run does. A gate that can hang is a worse harness than
  no gate."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is use-fixtures]]
            [samizdat.approval :as approval]
            [samizdat.agent.tools :as tools]
            [samizdat.security.policy :as policy]))

(use-fixtures :each (fn [f] (approval/reset!) (f) (approval/reset!)))

(defn- req [& {:as over}]
  (merge {:run-id "r1" :branch-id "B1" :kind :shell
          :input "rm -rf build" :reason "compound command"}
         over))

(deftest a-request-is-pending-until-it-is-decided
  (let [id (approval/request! (req))]
    (is (string? id))
    (let [[p] (approval/pending "r1")]
      (is (= id (:id p)))
      (is (= "rm -rf build" (:input p)))
      (is (= "pending" (:status p))))
    (approval/decide! id {:decision :allow})
    (is (empty? (approval/pending "r1")) "and gone from the queue once answered")))

(deftest awaiting-returns-the-decision-a-person-made
  (let [id (approval/request! (req))]
    (future (Thread/sleep 20) (approval/decide! id {:decision :allow :note "fine"}))
    (let [d (approval/await! id 3000 {:decision :deny})]
      (is (= :allow (:decision d)))
      (is (= "fine" (:note d))))))

(deftest a-wait-that-nobody-answers-ends-at-the-deadline
  ;; The property the whole design turns on. An unattended campaign run must
  ;; come out of this, not sit in it.
  (let [id (approval/request! (req))
        started (System/currentTimeMillis)
        d (approval/await! id 120 {:decision :deny :note "nobody answered"})]
    (is (= :deny (:decision d)))
    (is (= "nobody answered" (:note d)))
    (is (< (- (System/currentTimeMillis) started) 3000)
        "it returned at the deadline rather than blocking")
    (testing "and the request is retired, not left pending forever"
      (is (empty? (approval/pending "r1"))))))

(deftest an-answer-that-lands-before-the-wait-is-still-the-answer
  ;; The race that made the first cut of this wrong. `request!` publishes the
  ;; question and `await!` parks on it, and a person watching a TUI can
  ;; answer in between. Removing the entry on decide! delivered the promise
  ;; to something nobody could find any more, and the branch was DENIED a
  ;; command a human had just allowed — the worst direction for that bug.
  (let [id (approval/request! (req))]
    (approval/decide! id {:decision :allow :note "go ahead"})
    (let [d (approval/await! id 500 {:decision :deny})]
      (is (= :allow (:decision d)))
      (is (not (:timed-out d))))))

(deftest deciding-twice-does-not-change-the-answer
  ;; Two operators with two TUIs open. The first answer is the answer; the
  ;; second must not race the branch that already resumed on the first.
  (let [id (approval/request! (req))]
    (is (true? (approval/decide! id {:decision :allow})))
    (is (false? (approval/decide! id {:decision :deny}))
        "the second is refused rather than silently overwriting")
    (is (= :allow (:decision (approval/await! id 500 {:decision :deny}))))))

(deftest deciding-something-nobody-asked-is-refused
  (is (false? (approval/decide! "no-such-id" {:decision :allow}))))

(deftest requests-are-scoped-to-their-run
  (approval/request! (req))
  (approval/request! (req :run-id "r2"))
  (is (= 1 (count (approval/pending "r1"))))
  (is (= 1 (count (approval/pending "r2"))))
  (is (= 2 (count (approval/pending nil))) "nil asks for all of them"))

(deftest abandoning-a-run-releases-every-waiter-it-had
  ;; A run that is aborted or crashes must not leave a thread parked on a
  ;; question nobody will ever answer.
  (let [id (approval/request! (req))
        answer (future (approval/await! id 60000 {:decision :deny}))]
    (Thread/sleep 20)
    (approval/abandon! "r1")
    (is (= :deny (:decision (deref answer 3000 {:decision :hung})))
        "the waiter came back with the default rather than hanging")
    (is (empty? (approval/pending "r1")))))

(deftest a-questionnaire-carries-its-questions-and-comes-back-with-answers
  ;; The other shape of the same machinery: `ask_human` needs structured
  ;; questions out and structured answers back, not a yes/no.
  (let [id (approval/request! (req :kind :question
                                   :questions [{:question "which backend?"
                                                :options ["sqlite" "postgres"]}]))]
    (is (= [{:question "which backend?" :options ["sqlite" "postgres"]}]
           (:questions (first (approval/pending "r1")))))
    (approval/decide! id {:decision :answer :answers ["sqlite"]})
    (is (= ["sqlite"] (:answers (approval/await! id 500 {:decision :deny}))))))

;; --- the policy seam ---------------------------------------------------------

(deftest an-ask-is-a-refusal-unless-the-project-asked-for-a-person
  ;; The default stays what samizdat has always done: refuse and teach. A
  ;; harness that started blocking because a feature landed would hang every
  ;; unattended run on upgrade.
  (let [ask {:effect :ask :input "x"}]
    (with-redefs [approval/policy (constantly {:mode :refuse :wait-ms 1000
                                               :on-timeout :deny})]
      (is (= ask (approval/resolve-ask {:run-id "r1"} ask))
          "the decision comes back untouched, and nothing was queued")
      (is (empty? (approval/pending "r1")))))
  (testing "and anything that is not an ask passes straight through"
    (let [allow {:effect :allow :input "x"}]
      (with-redefs [approval/policy (constantly {:mode :block :wait-ms 1000
                                                 :on-timeout :deny})]
        (is (= allow (approval/resolve-ask {:run-id "r1"} allow)))
        (is (empty? (approval/pending "r1")))))))

(deftest in-blocking-mode-an-allow-from-a-person-becomes-an-allow
  (with-redefs [approval/policy (constantly {:mode :block :wait-ms 3000
                                             :on-timeout :deny})]
    (let [asked (promise)
          answer (future (approval/resolve-ask {:run-id "r1"} {:effect :ask :input "ls"}))]
      ;; Wait for the request to appear, then answer it the way a TUI would.
      (future (loop [] (if-let [p (first (approval/pending "r1"))]
                         (deliver asked (approval/decide! (:id p) {:decision :allow}))
                         (do (Thread/sleep 5) (recur)))))
      (is (= :allow (:effect (deref answer 5000 {:effect :hung})))))))

(deftest in-blocking-mode-nobody-answering-falls-back-to-the-stated-default
  (with-redefs [approval/policy (constantly {:mode :block :wait-ms 80
                                             :on-timeout :deny})]
    (let [r (approval/resolve-ask {:run-id "r1"} {:effect :ask :input "ls"})]
      (is (= :ask (:effect r))
          "an expired wait leaves the ask as the refusal it always was")
      (is (:timed-out r) "and says that is why, so the refusal can mention it")))
  (testing "a project that would rather proceed unattended can say so"
    (with-redefs [approval/policy (constantly {:mode :block :wait-ms 80
                                               :on-timeout :allow})]
      (is (= :allow (:effect (approval/resolve-ask {:run-id "r1"}
                                                   {:effect :ask :input "ls"})))))))

;; --- ask_human ---------------------------------------------------------------

(defn- ask [args]
  (tools/run-tool {:tool-name "ask_human" :args args
                   :branch {:id "B1"} :run-id "r1" :branch-id "B1"}))

(deftest ask-human-is-refused-when-nobody-is-there-to-ask
  ;; The default. A tool that parked an unattended run on a question would be
  ;; a way for the model to stop a campaign dead, so it has to be opt-in the
  ;; same way the permission gate is.
  (with-redefs [approval/policy (constantly {:mode :refuse :wait-ms 1000
                                             :on-timeout :deny})]
    (let [r (ask {:questions [{:question "which?" :options ["a" "b"]}]})]
      (is (= :mechanics (:category r)))
      (is (str/includes? (str/lower-case (:result r)) "no human"))
      (is (empty? (approval/pending "r1"))))))

(deftest ask-human-carries-the-questions-and-returns-the-answers
  (with-redefs [approval/policy (constantly {:mode :block :wait-ms 5000
                                             :on-timeout :deny})]
    (let [result (future (ask {:questions [{:question "which backend?"
                                            :options ["sqlite" "postgres"]}]}))]
      (future (loop [] (if-let [p (first (approval/pending "r1"))]
                         (approval/decide! (:id p) {:decision :answer
                                                    :answers ["postgres"]})
                         (do (Thread/sleep 5) (recur)))))
      (let [r (deref result 8000 ::hung)]
        (is (not= ::hung r))
        (is (str/includes? (:result r) "postgres"))
        (is (= :neutral (:category r))
            "asking establishes nothing — it must not read as progress")))))

(deftest ask-human-that-nobody-answers-comes-back-and-says-so
  (with-redefs [approval/policy (constantly {:mode :block :wait-ms 60
                                             :on-timeout :deny})]
    (let [r (deref (future (ask {:questions [{:question "which?"}]})) 5000 ::hung)]
      (is (not= ::hung r) "the tool returned rather than parking the run")
      (is (str/includes? (str/lower-case (:result r)) "nobody")))))

(deftest ask-human-needs-questions
  (with-redefs [approval/policy (constantly {:mode :block :wait-ms 1000
                                             :on-timeout :deny})]
    (is (= :mechanics (:category (ask {}))))))

;; --- the shell path, end to end ---------------------------------------------

(defn- shell [cmd]
  (policy/run-shell {:args {:command cmd} :run-id "r1" :branch-id "B1" :env {}}))

(deftest by-default-an-unapproved-command-is-refused-exactly-as-before
  ;; The regression that would matter most: a release must not turn an
  ;; unattended harness into one that parks on the first unusual command.
  (let [r (shell "frobnicate --widgets")]
    (is (= :ask (get-in r [:policy :effect])))
    (is (:needs-approval r))
    (is (empty? (approval/pending "r1")) "nothing was queued and nothing waited")))

(deftest with-blocking-on-a-person-can-let-a-command-through
  (with-redefs [approval/policy (constantly {:mode :block :wait-ms 5000
                                             :on-timeout :deny})]
    (let [result (future (shell "frobnicate --widgets"))]
      (future (loop [] (if-let [p (first (approval/pending "r1"))]
                         (approval/decide! (:id p) {:decision :allow})
                         (do (Thread/sleep 5) (recur)))))
      (let [r (deref result 8000 ::hung)]
        (is (not= ::hung r) "the branch resumed once a person answered")
        (is (not= :ask (get-in r [:policy :effect]))
            "and the command was no longer treated as unapproved")))))

(deftest with-blocking-on-and-nobody-there-the-refusal-says-so
  ;; The unattended case. It must still END, and the model must be able to
  ;; tell "nobody was watching" from "this is against the rules" — otherwise
  ;; it learns a policy from an empty room.
  (with-redefs [approval/policy (constantly {:mode :block :wait-ms 60
                                             :on-timeout :deny})]
    (let [r (deref (future (shell "frobnicate --widgets")) 5000 ::hung)]
      (is (not= ::hung r) "the wait ended at the deadline")
      (is (= :ask (get-in r [:policy :effect])))
      (is (str/includes? (:result r) "ABSENCE")
          "the refusal tells the model it was never judged, only unseen"))))

(deftest a-person-can-deny-and-say-what-to-do-instead
  ;; dirge's `d` key: the refusal is more useful when it carries a redirect.
  (with-redefs [approval/policy (constantly {:mode :block :wait-ms 3000
                                             :on-timeout :deny})]
    (let [answer (future (approval/resolve-ask {:run-id "r1"} {:effect :ask :input "curl x"}))]
      (future (loop [] (if-let [p (first (approval/pending "r1"))]
                         (approval/decide! (:id p) {:decision :deny
                                                    :note "use the vendored copy"})
                         (do (Thread/sleep 5) (recur)))))
      (let [r (deref answer 5000 {:effect :hung})]
        (is (= :ask (:effect r)))
        (is (= "use the vendored copy" (:note r)))))))
