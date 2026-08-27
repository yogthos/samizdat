;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.infer-test
  "The inference step and its drivers, driven from literal tapes.

  That every test here runs with no db, no provider, no branch map and no
  config IS the property under test: the step is a pure function of a tape
  plus an injected `complete`. Before the refactor there was no way to ask
  what a turn would do without running one."
  (:require [clojure.test :refer [deftest is testing]]
            [samizdat.agent.infer :as infer]
            [samizdat.agent.loop :as aloop]
            [samizdat.agent.state :as state]
            [samizdat.store.journal :as journal]
            [samizdat.tape :as tape]))

(defn- fenced
  "An assistant reply carrying one well-formed tool call."
  [tool args]
  (str "Here is what I will do.\n\n```tool-call\n"
       "{\"name\": \"" tool "\", \"args\": " args "}\n```"))

(defn- replying
  "A `complete` stub: answers with `content`, recording every tape it saw."
  ([content] (replying content (atom [])))
  ([content seen]
   (fn [t]
     (swap! seen conj t)
     {:ok true :response {:content content :finish-reason "stop"}})))

(defn- failing [msg]
  (fn [_] {:ok false :error msg}))

(def ^:private base-tape
  {:id "B1"
   :messages [{:role "system" :content "sys"}
              {:role "user" :content "## Problem\n\ndo the thing"}]
   :turns []})

;; --- the projection ---------------------------------------------------------

(deftest of-branch-carries-exactly-what-a-call-depends-on
  (let [b (assoc (state/new-branch {:id "B1" :problem "p"
                                    :messages [{:role "user" :content "u"}]})
                 :prefill "```tool-call\n"
                 :force-tool {:name "done"})
        t (infer/of-branch b)]
    (is (= #{:id :messages :turns :prefill :force-tool :squeeze} (set (keys t)))
        "nothing else about a branch can change what the model is sent —
         :squeeze is on the list because the overflow squeeze legitimately
         changes how much history the wire sees (karamazov-d41)")
    (is (= "B1" (:id t)))
    (is (= "```tool-call\n" (:prefill t)))))

(deftest into-branch-writes-messages-and-clears-the-per-turn-knobs
  (let [b (assoc (state/new-branch {:id "B1" :problem "p"})
                 :prefill "x" :force-tool {:name "done"} :thesis {:goal "keep me"})
        b' (infer/into-branch b (assoc base-tape :messages [{:role "user" :content "new"}]))]
    (is (= [{:role "user" :content "new"}] (:messages b')))
    (is (nil? (:prefill b')) "one steer forecloses prose on ONE turn")
    (is (nil? (:force-tool b')))
    (is (= {:goal "keep me"} (:thesis b')) "everything else about the branch survives")))

;; --- absorb is pure ---------------------------------------------------------

(deftest absorb-appends-the-reply-and-parses-the-call
  (let [{:keys [tape parsed said]}
        (infer/absorb base-tape {:content (fenced "eval" "{\"code\": \"(+ 1 2)\"}")
                                 :finish-reason "stop"})]
    (is (= "eval" (:name parsed)))
    (is (= 3 (tape/depth (:messages tape))) "the reply is appended, once")
    (is (= "assistant" (:role (last (:messages tape)))))
    (is (= said (:content (last (:messages tape))))
        "what is stored is what was said, opener included")))

(deftest absorb-does-not-mutate-its-input
  (let [t base-tape]
    (infer/absorb t {:content "prose" :finish-reason "stop"})
    (is (= 2 (tape/depth (:messages t)))
        "the accumulator is a value — a fork of it cannot see this call")))

(deftest absorb-reattaches-a-prefilled-opener
  (let [t (assoc base-tape :prefill "```tool-call\n")
        {:keys [tape parsed]}
        (infer/absorb t {:content "{\"name\": \"done\", \"args\": {}}\n```"
                         :finish-reason "stop"})]
    (is (= "done" (:name parsed))
        "without the prefill this parses as a no-call — the failure prefill exists to prevent")
    (is (nil? (:prefill tape)) "the knob is cleared by absorb, not by its caller")))

;; --- the step ---------------------------------------------------------------

(deftest step-advances-the-tape-on-a-reply
  (let [seen (atom [])
        {:keys [tape call parsed]}
        (infer/step (replying (fenced "read" "{\"path\": \"x\"}") seen) base-tape)]
    (is (:ok call))
    (is (= "read" (:name parsed)))
    (is (= 3 (tape/depth (:messages tape))))
    (is (= 1 (count @seen)) "one call, not a retry ladder on a clean reply")))

(deftest step-leaves-the-tape-alone-when-the-provider-fails
  (let [{:keys [tape call parsed]} (infer/step (failing "socket reset") base-tape)]
    (is (false? (:ok call)))
    (is (= "socket reset" (:error call)))
    (is (nil? parsed))
    (is (= base-tape tape)
        "a failed call costs the turn, never the history — retry continues from here")))

;; --- bounce: the non-committing probe ---------------------------------------

(deftest bounce-reads-the-outcome-and-leaves-the-tape-fixed
  (let [out (infer/bounce (replying (fenced "grep" "{\"pattern\": \"x\"}")) base-tape)]
    (is (= "grep" (:name (:parsed out))) "it tells you what the model WOULD call")
    (is (= 2 (:depth out)) "the fixed depth — the tape did not move")
    (is (= 2 (tape/depth (:messages base-tape))))))

(deftest bounce-has-no-tool-seam-at-all
  (let [out (infer/bounce (replying (fenced "shell" "{\"cmd\": \"rm -rf /\"}")) base-tape)]
    (is (= "shell" (:name (:parsed out))))
    (is (nil? (:result out))
        "a probe reports the call it saw and cannot run it — there is no dispatch here")
    (is (nil? (:branch out))
        "and no branch bookkeeping: a bad probe is not a branch that called badly")))

(deftest bounce-returns-a-provider-failure-as-data
  (let [out (infer/bounce (failing "429") base-tape)]
    (is (= "429" (:error out)))
    (is (= 2 (:depth out)))))

(deftest bounce-catches-a-throwing-complete
  (let [out (infer/bounce (fn [_] (throw (ex-info "boom" {}))) base-tape)]
    (is (re-find #"probe failed: boom" (:error out))
        "a probe never throws into the loop that scheduled it")))

;; --- trampoline: N probes off one fixed point -------------------------------

(deftest trampoline-fans-inputs-without-accumulating-them
  (let [seen (atom [])
        out (infer/trampoline (replying "ok" seen) base-tape ["try A" "try B" "try C"])]
    (is (= 3 (count (:bounces out))))
    (is (= ["try A" "try B" "try C"] (mapv :input (:bounces out))))
    (is (= 2 (:depth out)) "the fixed point never moves")
    (testing "each bounce forked the SAME prefix — inputs never see each other"
      (is (= [3 3 3] (mapv (comp count :messages) @seen)))
      (is (= ["try A" "try B" "try C"]
             (mapv (comp :content last :messages) @seen))))))

(deftest trampoline-isolates-a-failing-bounce
  (let [flaky (fn [t]
                (if (= "bad" (:content (last (:messages t))))
                  {:ok false :error "nope"}
                  {:ok true :response {:content "fine" :finish-reason "stop"}}))
        out (infer/trampoline flaky base-tape ["good" "bad" "good"])]
    (is (= [nil "nope" nil] (mapv :error (:bounces out)))
        "one failed probe does not sink the scan — unlike a fold, bounces are independent")))

(deftest trampoline-over-nothing-is-not-an-error
  (is (= [] (:bounces (infer/trampoline (replying "ok") base-tape [])))))

;; --- ab: one probe, N configs -----------------------------------------------

(deftest ab-varies-the-config-and-holds-the-tape
  (let [complete-for (fn [vk] (replying (str "answer from " (name vk))))
        out (infer/ab complete-for base-tape [:cheap :strong] "which approach?")]
    (is (= #{:cheap :strong} (set (keys (:variants out)))))
    (is (re-find #"answer from cheap" (:said (get-in out [:variants :cheap]))))
    (is (re-find #"answer from strong" (:said (get-in out [:variants :strong]))))
    (is (= 2 (:depth out)) "the parent never moves")))

(deftest ab-reports-a-failing-arm-as-data
  (let [complete-for (fn [vk] (if (= :broken vk) (failing "no such model") (replying "ok")))
        out (infer/ab complete-for base-tape [:fine :broken])]
    (is (nil? (:error (get-in out [:variants :fine]))))
    (is (= "no such model" (:error (get-in out [:variants :broken])))
        "one failed arm does not sink the fan")))

(deftest ab-without-an-input-probes-the-tape-as-it-stands
  (let [seen (atom [])
        _ (infer/ab (fn [_] (replying "ok" seen)) base-tape [:a])]
    (is (= 2 (count (:messages (first @seen))))
        "no probe turn is invented when the caller did not ask for one")))

;; --- the context squeeze (karamazov-d41) ------------------------------------

(deftest the-overflow-squeeze-scales-the-compaction-budget
  (let [budget {:keep-pairs 10 :compaction-chars 50000}
        policy {:factor 0.5 :min-keep-pairs 1 :min-compaction-chars 5000}]
    (testing "no squeeze, no change"
      (is (= {:keep-pairs 10 :threshold-chars 50000}
             (infer/squeezed-budget budget nil policy)))
      (is (= {:keep-pairs 10 :threshold-chars 50000}
             (infer/squeezed-budget budget 0 policy))))
    (testing "each level halves"
      (is (= {:keep-pairs 5 :threshold-chars 25000}
             (infer/squeezed-budget budget 1 policy)))
      (is (= {:keep-pairs 2 :threshold-chars 12500}
             (infer/squeezed-budget budget 2 policy))))
    (testing "the floors hold no matter how deep the squeeze"
      (is (= {:keep-pairs 1 :threshold-chars 5000}
             (infer/squeezed-budget budget 9 policy))
          "a squeezed branch still sees its current exchange"))))

(deftest the-squeeze-rides-the-tape
  (let [b (-> (state/new-branch {:id "B1" :problem "p"})
              state/squeeze-context
              state/squeeze-context)]
    (is (= 2 (:context-squeeze b)))
    (is (= 2 (:squeeze (infer/of-branch b))))))

(deftest a-context-overflow-squeezes-instead-of-asking-for-a-retry
  (with-redefs [journal/record-turn! (fn [& _] nil)]
    (let [b (state/new-branch {:id "B1" :problem "p"})
          n (count (:messages b))]
      (testing "overflow: squeeze, and DO NOT append — a message grows the
                very thing that overflowed, and the model never saw the
                failure anyway"
        (let [b' (aloop/provider-error-step {} b 3 "context exceeded"
                                            :context-overflow)]
          (is (= 1 (:context-squeeze b')))
          (is (= n (count (:messages b'))))))
      (testing "any other provider failure keeps the try-again message"
        (let [b' (aloop/provider-error-step {} b 3 "boom" :call-failed)]
          (is (nil? (:context-squeeze b')))
          (is (= (inc n) (count (:messages b')))))))))
