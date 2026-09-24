;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.features-test
  "Provider features are DECLARED in config and DISCOVERED at startup, and
  everything that used to guess a capability from a URL or a provider id
  reads them instead: the adapter emits only what the features allow, and
  the force mechanism for a steered turn is chosen in one place from a
  policy order intersected with them.

  The failure that motivated it (2026-09-18): the no-call clamp is a
  prefill, the local adapter said it could not prefill, so on Bonsai eight
  no-call turns in a row got a clamp that did nothing — while the same
  server continues a trailing assistant message perfectly well."
  (:require ;; the java.time.* host shim, before data.json — see samizdat.store.journal
            [jolt.time]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [jolt.http-client]
            [samizdat.agent.gates :as gates]
            [samizdat.agent.infer :as infer]
            [samizdat.config :as config]
            [samizdat.llm.adapter :as adapter]
            [samizdat.llm.client]
            [samizdat.llm.registry :as registry]))

;; --- declared -----------------------------------------------------------------

(deftest every-provider-declares-its-features
  (doseq [[p preset] config/providers-for-test]
    (is (set? (:features preset)) (str (name p) " declares a feature set")))
  (testing "what each was measured to do"
    (is (contains? (config/features-of :deepseek) :prefill) "DeepSeek /beta continues a prefix")
    (is (contains? (config/features-of :deepseek) :thinking-toggle) "and turns thinking off on request")
    (is (not (contains? (config/features-of :glm) :prefill)) "GLM ignores a trailing assistant message")
    (is (not (contains? (config/features-of :glm) :thinking-toggle)) "and cannot be told not to think")
    (is (contains? (config/features-of :glm) :native-tool-choice))
    (is (not (contains? (config/features-of :local) :grammar))
        "a bare :local is an OpenAI-compatible endpoint until the probe says which")))

(deftest the-loaded-config-carries-the-features-and-a-file-can-name-its-own
  (let [c (config/load-config {:roles {:default :glm}})]
    (is (= (config/features-of :glm) (get-in c [:llm :features]))))
  (testing "DeepSeek off its beta URL loses the prefill it would be refused for"
    ;; 'prefix is only available when using beta api' is a 400 on /v1, so a
    ;; misconfigured endpoint would fail every steered turn.
    (let [c (config/load-config {:providers {:deepseek {:base-url "https://api.deepseek.com/v1"}}
                                 :roles {:default :deepseek}})]
      (is (not (contains? (get-in c [:llm :features]) :prefill)))
      (is (contains? (get-in c [:llm :features]) :native-tool-choice) "the rest stays")))
  (testing "a declaration that names features names them all"
    (let [c (config/load-config {:providers {:local {:features #{:grammar}}}
                                 :roles {:default :local}})]
      (is (= #{:grammar} (get-in c [:llm :features])))))
  (testing "a role model built for another provider carries that provider's features"
    (is (= (config/features-of :deepseek) (:features (config/provider-llm nil :deepseek {}))))))

;; --- discovered ---------------------------------------------------------------

(deftest a-probed-llama-cpp-server-adds-what-it-was-measured-to-do
  (let [llm {:provider :local :features (config/features-of :local)}
        found (config/apply-discovery llm {:llama-cpp? true :total-slots 4})]
    (is (every? (:features found) [:grammar :cache-prompt :reasoning-budget :thinking-toggle :prefill]))
    (is (true? (:llama-cpp? found)) "the probe's own facts ride along as before")
    (is (= (config/features-of :local) (:features (config/apply-discovery llm nil)))
        "no probe, no change")))

(deftest supports?-is-the-one-question-a-cell-asks
  (is (config/supports? {:features #{:prefill}} :prefill))
  (is (not (config/supports? {:features #{:prefill}} :grammar)))
  (is (not (config/supports? {} :prefill)) "no features declared is no features"))

;; --- the adapter emits only what the features allow --------------------------

(deftest the-adapter-honours-features-not-urls-or-ids
  (let [local (registry/adapter-for :local)
        req {:messages [{:role "user" :content "x"}] :max-tokens 10 :cache-key "B1"
             :prefill "```tool-call\n" :grammar "root ::= \"x\"" :reasoning-budget 256}]
    (testing "an endpoint with no features gets a plain OpenAI body"
      (let [b (adapter/chat-body local {:base-url "http://127.0.0.1:8080/v1" :model "m"} req)]
        (is (nil? (:cache_prompt b)))
        (is (nil? (:chat_template_kwargs b)))
        (is (nil? (:grammar b)))
        (is (nil? (:reasoning_budget_tokens b)))
        (is (not-any? #(= "assistant" (:role %)) (:messages b)) "no prefill either")))
    (testing "each feature unlocks exactly its own knob"
      (let [with (fn [fs] (adapter/chat-body local {:base-url "u" :model "m" :features fs} req))]
        (is (true? (:cache_prompt (with #{:cache-prompt}))))
        (is (= {:enable_thinking false} (:chat_template_kwargs (with #{:thinking-toggle}))))
        (is (= "root ::= \"x\"" (:grammar (with #{:grammar}))))
        (is (= 256 (:reasoning_budget_tokens (with #{:reasoning-budget}))))
        (is (some #(and (= "assistant" (:role %)) (:prefix %)) (:messages (with #{:prefill}))))))
    (testing "prefill-support? is the features, on any adapter of the family"
      (is (adapter/prefill-support? (registry/adapter-for :deepseek) {:features #{:prefill}}))
      (is (not (adapter/prefill-support? (registry/adapter-for :deepseek) {:base-url "https://api.deepseek.com/beta"})))
      (is (adapter/prefill-support? (registry/adapter-for :openai) {:features #{:prefill}})))))

;; --- the force mechanism is chosen once, from policy and features ------------

(def ^:private done-spec
  {:name "done" :description "Finish." :parameters {:type "object" :properties {} :required []}})

(defn- sent-by
  "What infer hands the client for `tape` on an endpoint with `features`."
  ([features tape] (sent-by features tape nil))
  ([features tape policy]
   (let [seen (atom [])]
     (with-redefs [gates/threshold (let [orig gates/threshold]
                                     (fn [k] (if (and policy (= k :force-mechanism)) policy (orig k))))
                   samizdat.agent.tools.base/tool-names (fn [] ["done" "read_file" "eval"])
                   samizdat.llm.client/chat (fn [_ _ _ opts]
                                              (swap! seen conj opts)
                                              {:content "```tool-call\n{\"name\": \"done\", \"args\": {}}\n```"
                                               :finish-reason "stop"})]
       ((infer/complete-fn {:llm-adapter ::a :llm-config {:provider :x :model "m" :features features}}
                           {:journal? false})
        tape))
     (select-keys (first @seen) [:prefill :force-tool :grammar]))))

(def ^:private base {:id "B1" :messages [{:role "system" :content "s"} {:role "user" :content "go"}] :turns []})
(def ^:private named (assoc base :prefill "```tool-call\n{\"name\": \"done\"" :force-tool done-spec))
(def ^:private fence (assoc base :prefill "```tool-call\n"))

(deftest a-named-force-takes-the-first-mechanism-the-provider-has-in-policy-order
  (testing "the shipped order is grammar, prefill, native"
    (is (= [:grammar :prefill :native] (:named (gates/threshold :force-mechanism)))))
  (testing "llama.cpp, every feature: the grammar, and nothing else on the wire"
    (let [s (sent-by #{:grammar :prefill :native-tool-choice} named)]
      (is (string? (:grammar s)))
      (is (str/includes? (:grammar s) "\"\\\"done\\\"\""))
      (is (nil? (:prefill s)) "a prefill beside a grammar would skip the reasoning the grammar leaves free")
      (is (= done-spec (:force-tool s)) "the name still travels, for the journal")))
  (testing "DeepSeek /beta: the prefill, with the name"
    (let [s (sent-by #{:prefill :native-tool-choice} named)]
      (is (= "```tool-call\n{\"name\": \"done\"" (:prefill s)))
      (is (nil? (:grammar s)))))
  (testing "GLM: native tool_choice is all it has"
    (let [s (sent-by #{:native-tool-choice} named)]
      (is (nil? (:prefill s)))
      (is (nil? (:grammar s)))
      (is (= done-spec (:force-tool s)))))
  (testing "a provider with none of them is steered by words alone"
    (is (= {} (sent-by #{} named))))
  (testing "policy reorders: native first where the provider has it"
    (let [s (sent-by #{:grammar :prefill :native-tool-choice} named
                     {:named [:native :grammar :prefill] :fence [:prefill :grammar]})]
      (is (nil? (:grammar s)))
      (is (nil? (:prefill s)))
      (is (= done-spec (:force-tool s))))))

(deftest a-bare-fence-force-is-the-no-call-clamp-and-it-now-works-on-llama-cpp
  (testing "the shipped order prefers the prefill: the model starts inside the fence and cannot ramble"
    (is (= [:prefill :grammar] (:fence (gates/threshold :force-mechanism)))))
  (testing "llama.cpp: the prefill — the clamp that was a no-op for eight turns on Bonsai"
    (let [s (sent-by #{:grammar :prefill} fence)]
      (is (= "```tool-call\n" (:prefill s)))
      (is (nil? (:grammar s)))))
  (testing "a grammar-only endpoint: a fence is required, any tool allowed"
    (let [s (sent-by #{:grammar} fence)]
      (is (nil? (:prefill s)))
      (is (str/includes? (:grammar s) "( prefix call )+"))
      (is (str/includes? (:grammar s) "\"\\\"read_file\\\"\""))
      (is (str/includes? (:grammar s) "\"\\\"eval\\\"\""))))
  (testing "GLM: nothing forces a bare fence, as before"
    (is (= {} (sent-by #{:native-tool-choice} fence)))))

(deftest the-journal-learns-a-prefill-force-too
  (let [sent (atom nil)
        reply (with-redefs [jolt.http-client/post
                            (fn [_ {:keys [body]}]
                              (reset! sent (json/read-str body :key-fn keyword))
                              {:status 200
                               :body "{\"choices\":[{\"message\":{\"content\":\"{\\\"args\\\": {}}\\n```\"},\"finish_reason\":\"stop\"}]}"})]
                (samizdat.llm.client/chat (registry/adapter-for :local)
                                          {:base-url "u" :model "m" :features #{:prefill}}
                                          [{:role "user" :content "x"}]
                                          {:max-tokens 10 :prefill "```tool-call\n{\"name\": \"done\""
                                           :force-tool done-spec}))]
    (is (some #(= "assistant" (:role %)) (:messages @sent)) "the prefill went out")
    (is (nil? (:tool_choice @sent)))
    (is (= "done" (:forced reply)))
    (is (= :prefill (:forced-via reply)))))
