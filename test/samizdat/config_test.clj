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

(ns samizdat.config-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [samizdat.config :as config]
            [samizdat.layers :as layers]
            [samizdat.store.db]))

(deftest provider-llm-builds-a-named-provider-config
  ;; per-role model assignment: build the :llm map for a specific provider,
  ;; independent of the run's detected default, with overrides winning.
  (let [c (config/provider-llm nil :glm {:model "glm-x"})]
    (is (= :glm (:provider c)))
    (is (= "glm-x" (:model c)) "the override wins over the table default")
    (is (str/includes? (:base-url c) "bigmodel") "and it carries GLM's endpoint"))
  (testing "an unknown provider is an error, not a silent default"
    (is (thrown? Exception (config/provider-llm nil :nope {})))))

(deftest glm-uses-the-coding-endpoint
  ;; Aligned with the config dirge drives GLM through: the coding endpoint,
  ;; glm-5.3, low temperature. The coding /models listing advertises the base
  ;; models (glm-4.5/4.6), not the coding alias, but chat accepts glm-5.3.
  (let [cfg (config/load-config {:roles {:default :glm}})
        {:keys [base-url model temperature]} (:llm cfg)]
    (testing "the provider defaults resolve to dirge's working GLM config"
      ;; Asserted on the LOADED config, which is the claim worth making. It
      ;; used to bind these three and then assert the static table instead,
      ;; with a comment about key-env detection — because naming a provider
      ;; did not in fact expand its preset, so the loaded values were
      ;; whatever the environment had detected. Now it does, and the binding
      ;; is the thing under test rather than dead.
      (is (= "https://open.bigmodel.cn/api/coding/paas/v4" base-url))
      (is (= "glm-5.3" model))
      (is (= 0.2 temperature)))))

(deftest provider-temperature-wins-over-family-default
  ;; A provider that pins a temperature (GLM's 0.2) beats the 0.7 family
  ;; default; a provider that doesn't falls back to 0.7.
  (is (= 0.2 (config/provider-temperature :glm)))
  (is (= 0.7 (config/provider-temperature :deepseek))))

(defn- temp-project-root
  "A temp dir with .samizdat/ created; caller passes config.edn content or nil."
  [edn-content]
  (let [root (str (java.nio.file.Files/createTempDirectory
                   "samizdat-proj-cfg"
                   (make-array java.nio.file.attribute.FileAttribute 0)))
        dir (java.io.File. root ".samizdat")]
    (.mkdirs dir)
    (when edn-content
      (spit (java.io.File. dir "config.edn") edn-content))
    root))

(defn- delete-recursively [^java.io.File f]
  (when (.isDirectory f) (doseq [c (.listFiles f)] (delete-recursively c)))
  (.delete f))

(deftest deep-merge-merges-nested-maps-and-later-wins
  (is (= {:a {:b 2 :c 3} :d 4}
         (config/deep-merge {:a {:b 1 :c 3}} {:a {:b 2} :d 4})))
  ;; non-map collisions: the later value simply replaces
  (is (= {:a 2} (config/deep-merge {:a 1} {:a 2})))
  (is (= {:a [2]} (config/deep-merge {:a {:b 1}} {:a [2]})))
  ;; no later layer leaves the base untouched
  (is (= {:a {:b 1}} (config/deep-merge {:a {:b 1}} {}))))

(deftest project-config-layers-between-defaults-and-overrides
  (let [root (temp-project-root "{ :http { :port 4242 } :run { :max-turns 7 } }")]
    (try
      (testing "project-config reads the file"
        (is (= {:http {:port 4242} :run {:max-turns 7}}
               (config/project-config root))))
      (testing "load-config picks up the project value"
        (let [cfg (config/load-config {:run {:root root}})]
          (is (= 4242 (get-in cfg [:http :port])))
          (is (= 7 (get-in cfg [:run :max-turns])))))
      (testing "an explicit override still beats the project value"
        (let [cfg (config/load-config {:run {:root root} :http {:port 5555}})]
          (is (= 5555 (get-in cfg [:http :port])))
          ;; untouched keys keep the project value
          (is (= 7 (get-in cfg [:run :max-turns])))))
      (finally (delete-recursively (java.io.File. root))))))

(deftest missing-or-broken-project-file-is-ignored
  (testing "absent file"
    (let [root (temp-project-root nil)]
      (try
        (is (= {} (config/project-config root)))
        (is (= 3985 (get-in (config/load-config {:run {:root root}}) [:http :port])))
        (finally (delete-recursively (java.io.File. root))))))
  (testing "garbage EDN"
    (let [root (temp-project-root "{:http {:port ")]
      (try
        (is (= {} (config/project-config root)))
        (finally (delete-recursively (java.io.File. root))))))
  (testing "valid EDN that is not a map"
    (let [root (temp-project-root "[:not :a :map]")]
      (try
        (is (= {} (config/project-config root)))
        (finally (delete-recursively (java.io.File. root)))))))

(deftest redacted-masks-an-api-key-wherever-it-sits
  ;; /health serves (config/redacted …), which masked [:llm :api-key] only,
  ;; and a nested key was served cleartext (karamazov-blt.29).
  (let [cfg {:llm {:api-key "sk-top" :model "m"}
             :providers {:critic {:type :glm :model "g" :api-key "sk-role"}}
             :db {:path "x"}}
        r (config/redacted cfg)]
    (is (= "***" (get-in r [:llm :api-key])))
    (is (= "***" (get-in r [:providers :critic :api-key]))
        "a declared provider's key is masked too")
    (is (= "g" (get-in r [:providers :critic :model]))
        "only the key is touched")
    (is (= {:db {:path "x"}} (config/redacted {:db {:path "x"}}))
        "a config with no key gains none")))

;; --- the global layer (karamazov-1g6b.1) -------------------------------------

(defn- temp-config-home
  "A temp dir standing in for ~/.config, with samizdat/config.edn written when
  `edn-content` is given."
  [edn-content]
  (let [home (str (java.nio.file.Files/createTempDirectory
                   "samizdat-cfg-home"
                   (make-array java.nio.file.attribute.FileAttribute 0)))
        dir (java.io.File. home "samizdat")]
    (.mkdirs dir)
    (when edn-content
      (spit (java.io.File. dir "config.edn") edn-content))
    home))

(defmacro ^:private with-files
  "Run `body` with the global and project config.edn set to `global` and
  `project` (nil for none), binding `root`."
  [[root global project] & body]
  `(let [home# (temp-config-home ~global)
         ~root (temp-project-root ~project)]
     (try
       (with-redefs [layers/config-home (fn [] home#)]
         ~@body)
       (finally (delete-recursively (java.io.File. home#))
                (delete-recursively (java.io.File. ~root))))))

(deftest a-config-file-that-selects-a-provider-gets-that-provider
  ;; Selecting a provider selects its whole preset — endpoint, key, model,
  ;; context window, temperature — not just the adapter. The provider used
  ;; to be picked from the environment first and a file's choice merged over
  ;; the expanded preset, which dispatched GLM's adapter at DeepSeek's
  ;; endpoint with DeepSeek's key and said nothing.
  (with-files [root "{:roles {:default :glm}}" nil]
    (let [{:keys [provider base-url model temperature]}
          (:llm (config/load-config {:run {:root root}}))]
      (is (= :glm provider))
      (is (= "https://open.bigmodel.cn/api/coding/paas/v4" base-url)
          "GLM's endpoint, not whichever provider the environment detected")
      (is (= "glm-5.3" model))
      (is (= 0.2 temperature) "and GLM's coding temperature")))
  (testing "a project file's choice beats the global one's"
    (with-files [root "{:roles {:default :glm}}" "{:roles {:default :deepseek}}"]
      (let [{:keys [provider base-url model]} (:llm (config/load-config {:run {:root root}}))]
        (is (= :deepseek provider))
        (is (str/includes? base-url "deepseek"))
        (is (= "deepseek-v4-flash" model)))))
  (testing "a declaration may pin one key and keep the rest of the preset"
    (with-files [root "{:providers {:glm {:model \"glm-4.6\"}} :roles {:default :glm}}" nil]
      (let [{:keys [base-url model]} (:llm (config/load-config {:run {:root root}}))]
        (is (= "glm-4.6" model))
        (is (str/includes? base-url "bigmodel")))))
  (testing "a project may select an alias only the global file declares"
    (with-files [root "{:providers {:flash {:type :deepseek :model \"f\"}}}"
                 "{:roles {:default :flash}}"]
      (is (= "f" (:model (:llm (config/load-config {:run {:root root}})))))))
  (testing "a provider spelled as a string is the provider it names"
    ;; `"glm"` is what someone who has read the JSON in /health writes.
    (with-files [root "{:roles {:default \"glm\"}}" nil]
      (is (= :glm (get-in (config/load-config {:run {:root root}}) [:llm :provider])))))
  (testing "a provider no table knows is an error, not a silent fallback"
    (with-files [root "{:roles {:default :nope}}" nil]
      (is (thrown? Exception (config/load-config {:run {:root root}}))))))

(deftest an-unrecognized-key-is-refused-naming-its-file
  (doseq [[body k] [["{:llm {:provider :glm}}" ":llm"]
                    ["{:porvider :glm}" ":porvider"]]]
    (with-files [root nil body]
      (let [e (try (config/load-config {:run {:root root}}) nil (catch Exception e e))]
        (is e body)
        (is (str/includes? (str (ex-message e)) "/.samizdat/config.edn"))
        (is (str/includes? (str (ex-message e)) (str "unrecognized key " k)))))))

(deftest a-global-config-sits-beneath-the-project-config
  (let [home (temp-config-home "{:http {:port 4100} :run {:max-turns 9 :beam-width 2}}")
        root (temp-project-root "{:run {:max-turns 7}}")]
    (try
      (with-redefs [layers/config-home (fn [] home)]
        (testing "global-config reads <config-home>/samizdat/config.edn"
          (is (= {:http {:port 4100} :run {:max-turns 9 :beam-width 2}}
                 (config/global-config))))
        (let [cfg (config/load-config {:run {:root root}})]
          (testing "a value set only globally reaches the run"
            (is (= 4100 (get-in cfg [:http :port]))))
          (testing "the same key set in the project wins over global"
            (is (= 7 (get-in cfg [:run :max-turns]))))
          (testing "nested maps MERGE: the project's :run :max-turns did not wipe the global :run :beam-width"
            (is (= 2 (get-in cfg [:run :beam-width])))))
        (testing "an explicit override still beats both files"
          (is (= 5555 (get-in (config/load-config {:run {:root root} :http {:port 5555}})
                              [:http :port]))))
        (testing "the layered file config is one map, project on top"
          (is (= {:http {:port 4100}
                  :run {:max-turns 7 :beam-width 2}}
                 (config/file-config root)))))
      (finally
        (delete-recursively (java.io.File. root))
        (delete-recursively (java.io.File. home))))))

(deftest the-eval-settings-read-the-global-layer-too
  ;; eval-mode used to read project-config directly, which would have left a
  ;; global :eval setting silently ignored — a security posture the operator
  ;; set once for the machine has to hold on every project.
  (let [home (temp-config-home "{:eval {:mode :off}}")
        root (temp-project-root nil)]
    (try
      (with-redefs [layers/config-home (fn [] home)]
        (is (= :off (config/eval-mode root)))
        (testing "and the project can still override it"
          (spit (java.io.File. (str root "/.samizdat") "config.edn") "{:eval {:mode :harness}}")
          (is (= :harness (config/eval-mode root)))))
      (finally
        (delete-recursively (java.io.File. root))
        (delete-recursively (java.io.File. home))))))

(deftest no-config-files-means-exactly-todays-config
  (let [home (temp-config-home nil)
        root (temp-project-root nil)]
    (try
      (with-redefs [layers/config-home (fn [] home)]
        (let [with-home (config/load-config {:run {:root root}})
              without (with-redefs [layers/config-home (fn [] nil)]
                        (config/load-config {:run {:root root}}))]
          (is (= with-home without)
              "an absent global file contributes nothing, byte for byte")))
      (finally
        (delete-recursively (java.io.File. root))
        (delete-recursively (java.io.File. home))))))

(deftest a-broken-global-file-is-ignored-like-a-broken-project-file
  (let [home (temp-config-home "{:http {:port ")]
    (try
      (with-redefs [layers/config-home (fn [] home)]
        (is (= {} (config/global-config))))
      (finally (delete-recursively (java.io.File. home))))))

(deftest config-sources-name-every-layer-and-whether-it-was-read
  ;; The boot log and /health show these, so a surprising value can be traced
  ;; to the file that set it.
  (let [home (temp-config-home "{}")
        root (temp-project-root nil)]
    (try
      (with-redefs [layers/config-home (fn [] home)]
        (let [srcs (config/config-sources root)]
          (is (= [:global :project] (mapv :layer srcs)))
          (is (= (str home "/samizdat/config.edn") (:path (first srcs))))
          (is (true? (:present? (first srcs))))
          (is (= (str root "/.samizdat/config.edn") (:path (second srcs))))
          (is (false? (:present? (second srcs))))))
      (testing "no config home at all is reported, not thrown"
        (with-redefs [layers/config-home (fn [] nil)]
          (let [[g] (config/config-sources root)]
            (is (= :global (:layer g)))
            (is (nil? (:path g)))
            (is (false? (:present? g))))))
      (finally
        (delete-recursively (java.io.File. root))
        (delete-recursively (java.io.File. home))))))

;; --- the db under .samizdat/ (karamazov-1g6b.2) -----------------------------

(deftest the-db-lives-under-samizdat-dir-by-default
  (let [root (temp-project-root nil)]
    (try
      (testing "a fresh project gets .samizdat/samizdat.sqlite3, root-relative"
        (is (= {:path (str root "/.samizdat/samizdat.sqlite3") :from :default}
               (config/db-location root nil))))
      (testing "an env path wins whatever is on disk"
        (is (= {:path "ci.sqlite3" :from :env}
               (config/db-location root "ci.sqlite3"))))
      (testing "a checkout with only the old root db still opens it, and says so"
        (spit (java.io.File. root "samizdat.sqlite3") "")
        (is (= {:path (str root "/samizdat.sqlite3") :from :legacy}
               (config/db-location root nil))))
      (testing "once the .samizdat one exists it is preferred over the legacy file"
        (spit (java.io.File. (str root "/.samizdat") "samizdat.sqlite3") "")
        (is (= {:path (str root "/.samizdat/samizdat.sqlite3") :from :project}
               (config/db-location root nil))))
      (testing "load-config carries the location under :db"
        (when (nil? (System/getenv "HARNESS_DB"))
          (let [cfg (config/load-config {:run {:root root}})]
            (is (= (str root "/.samizdat/samizdat.sqlite3") (get-in cfg [:db :path])))
            (is (= :project (get-in cfg [:db :from]))))))
      (testing "a :db :path in the config file still wins over the default"
        (spit (java.io.File. (str root "/.samizdat") "config.edn") "{:db {:path \"pinned.sqlite3\"}}")
        (when (nil? (System/getenv "HARNESS_DB"))
          (is (= "pinned.sqlite3" (get-in (config/load-config {:run {:root root}}) [:db :path])))))
      (finally (delete-recursively (java.io.File. root))))))

(deftest opening-the-db-creates-its-parent-directory
  ;; sqlite does not create directories, and the new default sits one level
  ;; down — a fresh checkout has no .samizdat/ until something makes it.
  (let [root (str (java.nio.file.Files/createTempDirectory
                   "samizdat-db-dir"
                   (make-array java.nio.file.attribute.FileAttribute 0)))
        path (str root "/.samizdat/samizdat.sqlite3")
        c (samizdat.store.db/open! path)]
    (try
      (is (.exists (java.io.File. path)))
      (finally
        (samizdat.store.db/close c)
        (delete-recursively (java.io.File. root))))))

;; --- one resolver for the file layers (karamazov-1a51.2) ---------------------

(deftest an-env-named-config-file-sits-above-the-project-file
  (let [home (temp-config-home "{:http {:port 4100}}")
        root (temp-project-root "{:http {:port 4242} :run {:max-turns 7}}")
        envf (str root "/elsewhere.edn")]
    (spit envf "{:http {:port 4343}}")
    (try
      (with-redefs [layers/config-home (fn [] home)
                    layers/getenv (fn [k] (when (= k "SAMIZDAT_CONFIG_FILE") envf))]
        (is (= {:http {:port 4343} :run {:max-turns 7}}
               (config/file-config root)))
        (testing "and the boot log names it, highest last like the rest"
          (is (= [:global :project :env] (mapv :layer (config/config-sources root))))
          (is (= envf (:path (last (config/config-sources root)))))))
      (finally
        (delete-recursively (java.io.File. root))
        (delete-recursively (java.io.File. home))))))

(deftest a-broken-project-file-no-longer-hides-the-global-one
  ;; A project file that did not parse used to be read as {} — fine on its
  ;; own — but it was merged as one map with the global file, and the whole
  ;; project layer vanishing is the same as it never having been there. The
  ;; difference now is the boot log: the broken file is NAMED.
  (let [home (temp-config-home "{:http {:port 4100}}")
        root (temp-project-root "{:http {:port ")]
    (try
      (with-redefs [layers/config-home (fn [] home)]
        (is (= {:http {:port 4100}} (config/file-config root)))
        (let [p (some #(when (= :project (:layer %)) %) (config/config-sources root))]
          (is (true? (:present? p)))
          (is (re-find #"did not parse" (str (:error p))))))
      (finally
        (delete-recursively (java.io.File. root))
        (delete-recursively (java.io.File. home))))))

;; --- declared providers and role assignment ----------------------------------
;;
;; The dirge shape: :providers declares endpoints by alias, :roles says which
;; alias serves each role, :default being the run's own model.

(def ^:private home (System/getenv "HOME"))

(def ^:private declared
  {:providers {:fast {:type :deepseek :model "deepseek-v4-flash"}
               :vllm {:type :openai
                      :base-url "http://gpu:8000/v1"
                      :model "qwen"
                      :api-key "${HOME}"
                      :headers {"X-Home" "${HOME}" "X-Lit" "v"}
                      :context-window 64000
                      :thinking? true}}
   :roles {:default :vllm :reader :fast}})

(deftest roles-default-picks-the-runs-provider
  (let [llm (:llm (config/load-config declared))]
    (is (= :openai (:provider llm)) "the adapter is the entry's :type")
    (is (= :vllm (:provider-name llm)) "the alias rides along")
    (is (= "http://gpu:8000/v1" (:base-url llm)))
    (is (= "qwen" (:model llm)))
    (is (= 64000 (:context-window llm)))
    (is (true? (:thinking? llm)) "any :llm knob can sit on the entry")
    (testing "${VAR} expands from the environment, in the key and in headers"
      (is (= home (:api-key llm)))
      (is (= {"X-Home" home "X-Lit" "v"} (:headers llm))))
    (is (not-any? #(contains? llm %) [:type :api-key-env])
        "declaration-only keys do not leak into the resolved config")))

(deftest roles-assign-a-declared-provider-to-a-role
  (let [cfg (config/load-config declared)
        llm (:llm cfg)]
    (let [r (config/role-llm cfg llm :reader)]
      (is (= :deepseek (:provider r)))
      (is (= :fast (:provider-name r)))
      (is (= "deepseek-v4-flash" (:model r)))
      (is (str/includes? (:base-url r) "deepseek") "the preset fills what the entry leaves out"))
    (is (nil? (config/role-llm cfg llm :supervisor))
        "an unassigned role runs on the default")
    (is (nil? (config/role-llm (assoc-in cfg [:roles :reader] :vllm) llm :reader))
        "a role assigned the default's own alias keeps the default (and what the probe learned)")))

(deftest an-alias-named-after-a-built-in-needs-no-type
  (let [llm (:llm (config/load-config {:providers {:glm {:model "glm-9"}}
                                       :roles {:default :glm}}))]
    (is (= :glm (:provider llm)))
    (is (= "glm-9" (:model llm)))
    (is (str/includes? (:base-url llm) "bigmodel"))
    (is (= 0.2 (:temperature llm)) "the preset's own defaults still apply")))

(deftest api-key-env-names-the-variable-holding-the-key
  (is (= home (:api-key (:llm (config/load-config
                               {:providers {:box {:type :local :api-key-env "HOME"}}
                                :roles {:default :box}}))))))

(deftest a-bad-declaration-is-refused-by-name
  (testing "an alias that is not a built-in needs a :type"
    (let [e (try (config/load-config {:providers {:mystery {:model "m"}}
                                      :roles {:default :mystery}})
                 nil (catch Exception e e))]
      (is e)
      (is (str/includes? (ex-message e) "mystery"))))
  (testing "a :type no adapter serves"
    (is (thrown? Exception (config/load-config {:providers {:x {:type :nope}}
                                                :roles {:default :x}}))))
  (testing "a role naming a provider nobody declared"
    (let [cfg (config/load-config declared)]
      (is (thrown? Exception (config/role-llm (assoc-in cfg [:roles :reader] :ghost)
                                              (:llm cfg) :reader)))))
  (testing "a typo in :roles fails at load, not when that role first runs"
    (let [e (try (config/load-config (assoc-in declared [:roles :judge] :ghost))
                 nil (catch Exception e e))]
      (is e)
      (is (str/includes? (str (ex-message e)) "ghost"))))
  (testing "${VAR} that is unset"
    (let [e (try (config/load-config {:providers {:x {:type :local
                                                      :api-key "${SAMIZDAT_SURELY_UNSET_VAR}"}}
                                      :roles {:default :x}})
                 nil (catch Exception e e))]
      (is e)
      (is (str/includes? (ex-message e) "SAMIZDAT_SURELY_UNSET_VAR")))))

(deftest an-undeclared-unused-provider-costs-nothing
  ;; A machine-wide file may declare endpoints whose keys this machine lacks;
  ;; only the providers actually selected are resolved.
  (is (= :local (:provider (:llm (config/load-config
                                  {:providers {:far {:type :openai
                                                     :api-key "${SAMIZDAT_SURELY_UNSET_VAR}"}}
                                   :roles {:default :local}}))))))

(deftest provider-names-lists-built-ins-and-declared-aliases
  (let [names (set (config/provider-names (config/load-config declared)))]
    (is (every? names [:vllm :fast :glm :deepseek :local]))))

(deftest redacted-masks-declared-keys-and-headers
  (let [r (config/redacted (config/load-config declared))]
    (is (= "***" (get-in r [:llm :api-key])))
    (is (= "***" (get-in r [:providers :vllm :api-key])))
    (is (every? #{"***"} (vals (get-in r [:llm :headers]))))
    (is (every? #{"***"} (vals (get-in r [:providers :vllm :headers]))))))
