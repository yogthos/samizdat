;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.grammar-test
  "The fence grammar for llama.cpp (karamazov-fp21.1): the automaton that keeps
  the opener out of the prefix, the grammar text itself, and the two seams it
  crosses — the adapter that sends it only to an endpoint that samples under
  one, and the inference step that builds it only when policy says to."
  (:require ;; the java.time.* host shim, before data.json — see samizdat.store.journal
            [jolt.time]
            [clojure.data.json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [jolt.http-client]
            [samizdat.agent.gates :as gates]
            [samizdat.agent.infer :as infer]
            [samizdat.agent.state :as state]
            [samizdat.llm.adapter :as adapter]
            [samizdat.llm.client]
            [samizdat.llm.grammar :as grammar]
            [samizdat.llm.registry :as registry]))

;; --- the automaton ----------------------------------------------------------

(deftest the-opener-automaton-rejects-the-opener-however-it-is-approached
  (let [p grammar/opener
        n (count p)
        walk (fn [s]
               (reduce (fn [k c]
                         (let [t (grammar/next-state p k c)]
                           (if (= t n) (reduced n) t)))
                       0 s))]
    (is (< (walk "text ```tool-cal") n) "one short of the opener is still prefix")
    (is (= n (walk "text ```tool-call\n")))
    (is (= n (walk "````tool-call\n"))
        "an extra backtick in front does not hide the opener — the overlap KMP exists for")
    (is (< (walk "``tool-call\n```tool-cal") n))
    (is (= n (walk "``` ```tool-call\n")))
    (is (< (walk "```tool-call") n) "without the newline it is not yet the opener")))

(deftest the-no-substring-rules-have-one-state-per-character-and-no-accepting-one
  (let [rules (grammar/no-substring-rules "p" grammar/opener)]
    (is (= (count grammar/opener) (count rules)))
    (is (every? #(str/ends-with? % ")?") rules) "every state may end the text")
    (is (not-any? #(str/includes? % (str "p" (count grammar/opener))) rules)
        "no rule reaches the completed pattern")
    (is (= "p0 ::= ( [^\\n`aclot-] p0 | \"\\n\" p0 | \"-\" p0 | \"`\" p1 | \"a\" p0 | \"c\" p0 | \"l\" p0 | \"o\" p0 | \"t\" p0 )?"
           (first rules))
        "every character of the alphabet has its own transition; everything else stays in p0")
    (testing "a class never escapes - or ^: llama.cpp's parser has no such escapes"
      ;; PrismML's build (mainline 10687) refused the whole grammar over
      ;; `[^\\-a]`; the newer stock build had let it through.
      (doseq [r rules]
        (is (not (re-find #"\\\\[-^]" r)) r))
      (let [[r0] (grammar/no-substring-rules "q" "^-x")]
        (is (str/starts-with? r0 "q0 ::= ( [^x^-] q0"))))))

;; --- the grammar ------------------------------------------------------------

(deftest the-fence-grammar-names-the-tools-and-the-force-is-the-plus
  (let [g (grammar/fence-grammar {:tools ["done" "give_up"] :require? true})]
    (is (str/includes? g "root ::= ( prefix call )+ prefix"))
    (is (str/includes? g "name ::= \"\\\"done\\\"\" | \"\\\"give_up\\\"\""))
    (is (str/includes? g "call ::= \"```tool-call\\n\" ws \"{\" ws \"\\\"name\\\"\" ws \":\" ws name ws \",\" ws \"\\\"args\\\"\" ws \":\" ws object ws \"}\" ws \"```\""))
    (is (not (str/includes? g "</think>"))
        "a call that does not think has no reasoning to leave free")
    (is (str/includes? g "string ::= \"\\\"\" ( [^\"\\\\\\x7F\\x00-\\x1F] | \"\\\\\" ( [\"\\\\bfnrt/] | \"u\" [0-9a-fA-F]{4} ) )* \"\\\"\"")
        "the JSON string rule is the one the 3B probe ran under"))
  (testing "a call that thinks: free text up to the closer, then the constrained part"
    ;; Under a reasoning split the sampled text starts INSIDE the think
    ;; block — Bonsai 2's template puts the opener in the prompt — so the
    ;; grammar matches the closer, never an opener (2026-09-18).
    (let [g (grammar/fence-grammar {:tools ["done"] :require? true :think-close "</think>"})]
      (is (str/includes? g "root ::= t0 \"</think>\" ( prefix call )+ prefix"))
      (is (str/includes? g "t0 ::= ( [^/<>hiknt] t0 | \"/\" t0 | \"<\" t1 |")
          "the reasoning excludes only its own closer — a draft fence inside it is free")
      (is (not (str/includes? g "<think>")))))
  (testing "restrict: a fence is optional, and only the listed names may appear"
    (let [g (grammar/fence-grammar {:tools ["read_file"] :require? false})]
      (is (str/includes? g "root ::= ( prefix call )* prefix"))
      (is (str/includes? g "name ::= \"\\\"read_file\\\"\""))))
  (testing "no tools, no grammar — an empty list is a grammar no reply satisfies"
    (is (nil? (grammar/fence-grammar {:tools [] :require? true})))))

;; --- the adapter seam -------------------------------------------------------

(deftest a-grammar-reaches-a-llama-cpp-endpoint-and-nobody-else
  (let [done-spec {:name "done" :description "Finish."
                   :parameters {:type "object" :properties {} :required []}}
        req {:messages [{:role "user" :content "x"}]
             :force-tool done-spec
             :grammar "root ::= \"x\""}]
    (testing "on :local the grammar replaces the tools+tool_choice fallback"
      (let [body (adapter/chat-body (registry/adapter-for :local)
                                    {:base-url "http://127.0.0.1:8080/v1" :model "m"} req)]
        (is (= "root ::= \"x\"" (:grammar body)))
        (is (nil? (:tools body)) "no tools array, so the template rewrites no prefix")
        (is (nil? (:tool_choice body)))))
    (testing "a probed llama.cpp server under another id gets it too"
      (let [body (adapter/chat-body (registry/adapter-for :openai)
                                    {:base-url "http://h/v1" :model "m" :api-key "k" :llama-cpp? true}
                                    req)]
        (is (= "root ::= \"x\"" (:grammar body)))
        (is (nil? (:tool_choice body)))))
    (testing "a hosted provider never sees the field, and forces as before"
      (let [body (adapter/chat-body (registry/adapter-for :glm)
                                    {:base-url "https://open.bigmodel.cn/api/coding/paas/v4"
                                     :model "glm-5.3" :api-key "k"}
                                    req)]
        (is (nil? (:grammar body)) "a strict server 422s the whole request over an unknown key")
        (is (= {:type "function" :function {:name "done"}} (:tool_choice body)))))))

;; --- the inference step -----------------------------------------------------

(def ^:private done-spec
  {:name "done" :description "Finish."
   :parameters {:type "object" :properties {} :required []}})

(defn- capturing
  "A stub `llm/chat` that records the opts it was called with."
  [seen]
  (fn [_ _ _ opts]
    (swap! seen conj opts)
    {:content "```tool-call\n{\"name\": \"done\", \"args\": {}}\n```" :finish-reason "stop"}))

(defn- policy-with [m]
  (let [orig gates/threshold]
    (fn [k] (if (= k :local-grammar) m (orig k)))))

(deftest a-forced-turn-on-llama-cpp-is-forced-by-grammar-not-by-the-prompt
  (let [seen (atom [])
        ctx {:llm-adapter ::adapter
             :llm-config {:provider :local :base-url "http://127.0.0.1:8080/v1" :model "m"}}
        tape {:id "B1"
              :messages [{:role "system" :content "sys"} {:role "user" :content "go"}]
              :turns []
              :force-tool done-spec}]
    (with-redefs [samizdat.llm.client/chat (capturing seen)]
      ((infer/complete-fn ctx {:journal? false}) tape))
    (let [opts (first @seen)]
      (is (string? (:grammar opts)))
      (is (str/includes? (:grammar opts) "name ::= \"\\\"done\\\"\""))
      (is (str/includes? (:grammar opts) "( prefix call )+") "a force demands the call")
      (is (= done-spec (:force-tool opts))
          "the force is still named, so the adapter and the journal see the same thing"))
    (testing "a call that thinks gets the closer; the breaker's off-value takes it away"
      (let [thinking (assoc ctx :llm-config {:provider :local :model "m" :thinking? true})]
        (reset! seen [])
        (with-redefs [samizdat.llm.client/chat (capturing seen)]
          ((infer/complete-fn thinking {:journal? false}) tape))
        (is (str/includes? (:grammar (first @seen)) "root ::= t0 \"</think>\""))
        (reset! seen [])
        (with-redefs [samizdat.llm.client/chat (capturing seen)]
          ((infer/complete-fn (assoc-in thinking [:llm-config :reasoning-effort]
                                        (:off-value (gates/threshold :thinking-budget)))
                              {:journal? false})
           tape))
        (is (str/includes? (:grammar (first @seen)) "root ::= ( prefix call )+")
            "thinking off for this branch: the whole reply is constrained")))
    (testing "on a hosted provider nothing changes"
      (reset! seen [])
      (with-redefs [samizdat.llm.client/chat (capturing seen)]
        ((infer/complete-fn (assoc ctx :llm-config {:provider :glm :model "glm-5.3"})
                            {:journal? false})
         tape))
      (is (nil? (:grammar (first @seen)))))
    (testing "the policy can hand the force back to native tool_choice"
      (reset! seen [])
      (with-redefs [gates/threshold (policy-with {:force :native :restrict-after-refusal? false})
                    samizdat.llm.client/chat (capturing seen)]
        ((infer/complete-fn ctx {:journal? false}) tape))
      (is (nil? (:grammar (first @seen)))))))

(deftest a-refusal-restricts-the-next-decision-only-when-policy-says-so
  (let [b (state/record-outcome (state/new-branch {:id "B1" :problem "p"})
                                {:category :mechanics :policy-refusal? true :tool "eval"})]
    (is (= "eval" (:refused-tool b)))
    (is (= "eval" (:refused-tool (infer/of-branch b))) "the tape carries it")
    (is (nil? (:refused-tool (infer/into-branch b {:messages []})))
        "and one model call spends it")
    (is (nil? (:refused-tool (state/record-outcome b {:category :success :tool "read_file"})))
        "a call that ran clears it")
    (let [seen (atom [])
          ctx {:llm-adapter ::a :llm-config {:provider :local :model "m"}}
          tape (assoc (infer/of-branch b)
                      :messages [{:role "system" :content "s"} {:role "user" :content "go"}])]
      (testing "off by default: a merely refused branch is not constrained"
        (with-redefs [samizdat.llm.client/chat (capturing seen)]
          ((infer/complete-fn ctx {:journal? false}) tape))
        (is (nil? (:grammar (first @seen)))))
      (testing "on: every other tool may be called, this one cannot"
        (reset! seen [])
        ;; The registry is pinned: which tool groups a test process has
        ;; loaded is an accident of require order, and an empty registry
        ;; minus one name is no grammar at all.
        (with-redefs [gates/threshold (policy-with {:force :grammar :restrict-after-refusal? true})
                      samizdat.agent.tools.base/tool-names (fn [] ["eval" "read_file" "done"])
                      samizdat.llm.client/chat (capturing seen)]
          ((infer/complete-fn ctx {:journal? false}) tape))
        (let [g (:grammar (first @seen))]
          (is (string? g))
          (is (str/includes? g "( prefix call )* prefix") "a fence is optional — nothing is forced")
          (is (not (str/includes? g "\"\\\"eval\\\"\"")) "the refused tool is off the list")
          (is (str/includes? g "\"\\\"read_file\\\"\"") "the rest of the registry stays"))))))

;; --- the client seam ---------------------------------------------------------

(deftest the-client-hands-the-grammar-to-the-adapter
  ;; client/chat builds the adapter's request map BY NAME, so a knob it does
  ;; not list never reaches the wire. The adapter and infer tests above stub
  ;; the client and could not see that; the first live forced turn on Bonsai
  ;; did — it went out over the tools array and the model called edit_file
  ;; under a "done" force (2026-09-18).
  (let [sent (atom nil)
        local (registry/adapter-for :local)
        cfg {:base-url "http://127.0.0.1:8080/v1" :model "m"}]
    (let [reply (with-redefs [jolt.http-client/post
                              (fn [_ {:keys [body]}]
                                (reset! sent (clojure.data.json/read-str body :key-fn keyword))
                                {:status 200
                                 :body "{\"choices\":[{\"message\":{\"content\":\"```tool-call\\n{\\\"name\\\": \\\"done\\\", \\\"args\\\": {}}\\n```\"},\"finish_reason\":\"stop\"}]}"})]
                  (samizdat.llm.client/chat local cfg [{:role "user" :content "x"}]
                                            {:max-tokens 10
                                             :force-tool {:name "done" :parameters {:type "object"}}
                                             :grammar "root ::= \"x\""}))]
      (is (= "root ::= \"x\"" (:grammar @sent)) "the grammar is on the wire")
      (is (nil? (:tools @sent)) "and the tools array is not")
      (is (nil? (:tool_choice @sent)))
      (testing "and the reply says the turn was forced, and how, for the journal (v32)"
        (is (= "done" (:forced reply)))
        (is (= :grammar (:forced-via reply)))))))
