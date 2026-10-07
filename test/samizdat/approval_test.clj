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
            [samizdat.events]
            [samizdat.agent.tools :as tools]
            [samizdat.security.policy :as policy]))

(use-fixtures :each (fn [f] (approval/reset!) (approval/set-mode! nil)
                      (f) (approval/reset!) (approval/set-mode! nil)))

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

(deftest allow-always-holds-for-the-session-and-is-never-stored
  ;; dirge's `a`: a person who has allowed `cargo test` twice should not be
  ;; asked a third time. Session-only, by choice: it is this person's call
  ;; about this sitting, not a fact about the project, and a standing allow
  ;; left behind is the dangerous default — so it lives in memory, per run,
  ;; and is gone with the process. The question offers the pattern it would
  ;; allow so the person sees what they are agreeing to.
  (with-redefs [approval/policy (constantly {:mode :block :wait-ms 5000
                                             :on-timeout :deny})]
    (let [result (future (shell "frobnicate --widgets"))]
      (future (loop [] (if-let [p (first (approval/pending "r1"))]
                         (do (is (= "frobnicate *" (:always p))
                                 "the question names what always would allow")
                             (approval/decide! (:id p) {:decision :allow :always true}))
                         (do (Thread/sleep 5) (recur)))))
      (is (not= :ask (get-in (deref result 8000 ::hung) [:policy :effect]))))
    (is (= ["frobnicate *"] (approval/session-grants "r1")))
    (let [again (deref (future (shell "frobnicate --gadgets")) 3000 ::hung)]
      (is (not= ::hung again) "not asked again")
      (is (not= :ask (get-in again [:policy :effect]))))
    (is (empty? (approval/session-grants "r2")) "another run is asked as usual")
    (testing "a run that ends forgets it"
      (approval/abandon! "r1")
      (is (empty? (approval/session-grants "r1"))))))

(deftest a-question-the-person-rejects-says-so
  ;; Esc on a questionnaire: the question was seen and not answered, which
  ;; is not the same thing as nobody being there.
  (with-redefs [approval/policy (constantly {:mode :block :wait-ms 3000 :on-timeout :deny})]
    (let [r (future (ask {:questions [{:question "which store?" :options ["a" "b"]}]}))]
      (future (loop [] (if-let [p (first (approval/pending "r1"))]
                         (approval/decide! (:id p) {:decision :deny :note "not now"})
                         (do (Thread/sleep 5) (recur)))))
      (let [out (deref r 5000 ::hung)]
        (is (re-find #"declined to answer it: not now" (:result out)))))))

;; --- who sets the policy (karamazov-3vu1.2, karamazov-3vu1.10) ----------------
;;
;; gates.edn is userspace: the agent edits it. `:on-timeout :allow` there was an
;; approval the agent could grant itself by editing a file, and a yolo mode
;; reachable from it would be the same hole wider. The loosening settings come
;; from the operator — config.edn, which no tool the agent holds can write —
;; or from a person at a front end (the session mode). The agent's file may
;; still choose between the two SAFE modes and how long to wait.

(defn- policy-with [gates operator]
  (with-redefs [samizdat.agent.gates/threshold (fn [k] (when (= k :approval) gates))]
    (approval/configure! operator)
    (try (approval/policy) (finally (approval/configure! nil)))))

(deftest the-agents-file-cannot-loosen-the-approval-policy
  (is (= :deny (:on-timeout (policy-with {:mode :block :wait-ms 5 :on-timeout :allow} nil)))
      "an unanswered question would have become the agent's own yes")
  (is (= :refuse (:mode (policy-with {:mode :yolo} nil))))
  (is (= :block (:mode (policy-with {:mode :block} nil)))
      "asking a person is the safe direction, and stays the project's choice")
  (is (= 5 (:wait-ms (policy-with {:mode :block :wait-ms 5} nil)))))

(deftest the-operator-can
  (is (= :allow (:on-timeout (policy-with {:mode :block :wait-ms 5} {:on-timeout :allow}))))
  (is (= :yolo (:mode (policy-with {:mode :refuse} {:mode :yolo}))))
  (testing "and the session mode, set by a person at a front end, is over both"
    (approval/set-mode! :yolo)
    (try (is (= :yolo (:mode (policy-with {:mode :block} {:mode :refuse}))))
         (finally (approval/set-mode! nil)))))

(deftest yolo-allows-every-ask-without-asking
  (with-redefs [approval/policy (constantly {:mode :yolo :wait-ms 1000 :on-timeout :deny})]
    (let [r (approval/resolve-ask {:run-id "r1"} {:effect :ask :input "curl example.com"})]
      (is (= :allow (:effect r)))
      (is (:yolo r) "journalled as yolo, not as a person's approval")
      (is (empty? (approval/pending "r1"))))
    (testing "a deny is not an ask, and stays a deny"
      (is (= :deny (:effect (approval/resolve-ask {:run-id "r1"} {:effect :deny :input "rm -rf /"})))))))

(deftest allow-always-names-the-subcommand-not-the-whole-tool
  ;; karamazov-3vu1.6. `head *` meant a person who allowed `git push origin
  ;; main` once had allowed `git push --force`, `git reset --hard` and every
  ;; other git for the rest of the session. The pattern stops at the
  ;; subcommand when there is one.
  (is (= "git push *" (approval/grant-pattern "git" "git push origin main")))
  (is (= "cargo run *" (approval/grant-pattern "cargo" "cargo run --release")))
  (is (= "frobnicate *" (approval/grant-pattern "frobnicate" "frobnicate --widgets")))
  (is (= "curl *" (approval/grant-pattern "curl" "curl https://example.com"))
      "a URL is not a subcommand")
  (is (= "cat *" (approval/grant-pattern "cat" "cat /etc/hosts")) "nor is a path")
  (is (= "python3 *" (approval/grant-pattern "python3" "python3 script.py")) "nor a file name")
  (is (nil? (approval/grant-pattern nil "x"))))

;; --- attended: ask when somebody is watching ---------------------------------
;;
;; The TUI is a person at a terminal. With the mode :refuse a run started from
;; it told the model "nobody is attached" while somebody sat watching the
;; question box, and :block hangs the 3am campaign this namespace exists to
;; protect. :attended is both: a person is asked when a front end is
;; following the run's event stream, and nobody is waited on when not.

(deftest attended-asks-only-while-a-front-end-follows-the-run
  (with-redefs [samizdat.agent.gates/threshold (fn [k] (when (= k :approval) {:mode :attended}))]
    (is (= :attended (:mode (approval/policy))) "the mode as configured")
    (is (= :refuse (:mode (approval/policy "r1"))) "nobody is following r1")
    (let [w (samizdat.events/watch! "r1")]
      (try
        (is (= :block (:mode (approval/policy "r1"))) "a front end follows r1: ask")
        (is (= :refuse (:mode (approval/policy "r2"))) "but not r2")
        (finally (samizdat.events/unwatch! w))))
    (is (= :refuse (:mode (approval/policy "r1"))) "and once it stops following, nobody is")
    (testing "a front end following every run attends every run"
      (let [w (samizdat.events/watch! nil)]
        (try (is (= :block (:mode (approval/policy "r9"))))
             (finally (samizdat.events/unwatch! w)))))))

(deftest attended-is-a-mode-the-project-and-a-person-may-pick
  (is (= :attended (:mode (policy-with {:mode :attended} nil))))
  (try (is (= :attended (approval/set-mode! "attended")))
       (finally (approval/set-mode! nil))))

(deftest ask-human-reaches-a-person-who-is-watching
  (with-redefs [samizdat.agent.gates/threshold
                (fn [k] (when (= k :approval) {:mode :attended :wait-ms 5000}))]
    (let [w (samizdat.events/watch! "r1")]
      (try
        (let [result (future (ask {:questions [{:question "which backend?"
                                                :options ["sqlite" "postgres"]}]}))]
          (future (loop [n 0]
                    (if-let [p (first (approval/pending "r1"))]
                      (approval/decide! (:id p) {:decision :answer :answers ["postgres"]})
                      (when (< n 1000) (Thread/sleep 5) (recur (inc n))))))
          (is (str/includes? (:result (deref result 8000 {:result "hung"})) "postgres")))
        (finally (samizdat.events/unwatch! w))))))

(deftest ask-human-takes-options-with-descriptions-and-a-header
  ;; dirge's `question`: an option is a label and what it means, and a header
  ;; groups a question. The labels are what is answered; the descriptions are
  ;; shown beside them.
  (with-redefs [approval/policy (constantly {:mode :block :wait-ms 5000 :on-timeout :deny})]
    (let [result (future (ask {:questions [{:header "Storage"
                                            :question "which backend?"
                                            :options [{:label "sqlite (Recommended)"
                                                       :description "one file, no server"}
                                                      {:label "postgres" :description "a server"}
                                                      "memory"]}]}))
          p (loop [n 0] (or (first (approval/pending "r1"))
                            (when (< n 1000) (Thread/sleep 5) (recur (inc n)))))
          q (first (:questions p))]
      (is (= ["sqlite (Recommended)" "postgres" "memory"] (:options q)))
      (is (= ["one file, no server" "a server" ""] (:descriptions q)))
      (is (= "Storage" (:header q)))
      (approval/decide! (:id p) {:decision :answer :answers ["postgres"]})
      (let [r (:result (deref result 8000 {:result "hung"}))]
        (is (str/includes? r "Storage"))
        (is (str/includes? r "which backend? → postgres"))))))

(deftest a-half-answered-questionnaire-says-which-went-unanswered
  (with-redefs [approval/policy (constantly {:mode :block :wait-ms 5000 :on-timeout :deny})]
    (let [result (future (ask {:questions [{:question "one?"} {:question "two?"}]}))
          p (loop [n 0] (or (first (approval/pending "r1"))
                            (when (< n 1000) (Thread/sleep 5) (recur (inc n)))))]
      (approval/decide! (:id p) {:decision :answer :answers ["yes"]})
      (let [r (:result (deref result 8000 {:result "hung"}))]
        (is (str/includes? r "one? → yes"))
        (is (str/includes? r "two? → (no answer)"))))))

(deftest allow-always-covers-the-exact-command-when-no-pattern-can
  ;; A flow gap or a compound command offered no "allow always": a person
  ;; allowing `jolt -M:test 2>&1 | head -40` was asked again on every run of
  ;; the suite. With no pattern to widen to, always means this exact text,
  ;; for this session.
  (with-redefs [approval/policy (constantly {:mode :block :wait-ms 5000 :on-timeout :deny})]
    (let [ask {:effect :ask :head "jolt" :flow? true :input "jolt -M:test 2>&1 | head -40"}
          first-ask (future (approval/resolve-ask {:run-id "r1"} ask))]
      (loop [n 0]
        (if-let [p (first (approval/pending "r1"))]
          (do (is (= "jolt -M:test 2>&1 | head -40" (:always p)))
              (is (true? (:always-exact p)) "and says it is the exact text, not a pattern")
              (approval/decide! (:id p) {:decision :allow :always true}))
          (when (< n 1000) (Thread/sleep 5) (recur (inc n)))))
      (is (= :allow (:effect (deref first-ask 5000 {:effect :hung}))))
      (is (= :allow (:effect (deref (future (approval/resolve-ask {:run-id "r1"} ask)) 2000 {:effect :hung})))
          "the same text again is not asked")
      (is (empty? (approval/pending "r1")))
      (let [other (future (approval/resolve-ask {:run-id "r1"} (assoc ask :input "jolt -M:test")))]
        (Thread/sleep 100)
        (is (seq (approval/pending "r1")) "a different command still asks")
        (approval/reset!)
        (deref other 2000 nil))
      (is (= :ask (:effect (approval/resolve-ask {:run-id "r2"} (assoc ask :input "x"))))
          "nor does it reach another run")
      (approval/abandon! "r1")
      (let [again (future (approval/resolve-ask {:run-id "r1"} ask))]
        (Thread/sleep 100)
        (is (seq (approval/pending "r1")) "a run that ends forgets it")
        (approval/reset!)
        (deref again 2000 nil)))))
