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
            [samizdat.store.db]))

(deftest provider-llm-builds-a-named-provider-config
  ;; per-role model assignment: build the :llm map for a specific provider,
  ;; independent of the run's detected default, with overrides winning.
  (let [c (config/provider-llm :glm {:model "glm-x"})]
    (is (= :glm (:provider c)))
    (is (= "glm-x" (:model c)) "the override wins over the table default")
    (is (str/includes? (:base-url c) "bigmodel") "and it carries GLM's endpoint"))
  (testing "an unknown provider is an error, not a silent default"
    (is (thrown? Exception (config/provider-llm :nope {})))))

(deftest glm-uses-the-coding-endpoint
  ;; Aligned with the config dirge drives GLM through: the coding endpoint,
  ;; glm-5.3, low temperature. The coding /models listing advertises the base
  ;; models (glm-4.5/4.6), not the coding alias, but chat accepts glm-5.3.
  (let [cfg (config/load-config {:llm {:provider :glm}})
        {:keys [base-url model temperature]} (:llm cfg)]
    (testing "the provider defaults resolve to dirge's working GLM config"
      ;; provider defaults are read by key-env detection; assert the static
      ;; provider table rather than a live-env-dependent selection.
      (is (= "https://open.bigmodel.cn/api/coding/paas/v4"
             (get-in config/providers-for-test [:glm :base-url])))
      (is (= "glm-5.3" (get-in config/providers-for-test [:glm :model])))
      (is (= 0.2 (get-in config/providers-for-test [:glm :temperature]))))))

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
  (let [root (temp-project-root "{ :http { :port 4242 } :llm { :model \"project-model\" } }")]
    (try
      (testing "project-config reads the file"
        (is (= {:http {:port 4242} :llm {:model "project-model"}}
               (config/project-config root))))
      (testing "load-config picks up the project value"
        (let [cfg (config/load-config {:run {:root root}})]
          (is (= 4242 (get-in cfg [:http :port])))
          (is (= "project-model" (get-in cfg [:llm :model])))))
      (testing "an explicit override still beats the project value"
        (let [cfg (config/load-config {:run {:root root} :http {:port 5555}})]
          (is (= 5555 (get-in cfg [:http :port])))
          ;; untouched keys keep the project value
          (is (= "project-model" (get-in cfg [:llm :model])))))
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
  ;; /health serves (config/redacted …), which masked [:llm :api-key] only. A
  ;; role spec under :run :role-models may carry its own :api-key override —
  ;; role-ctx merges it into the provider config — and it was served cleartext
  ;; (karamazov-blt.29).
  (let [cfg {:llm {:api-key "sk-top" :model "m"}
             :run {:role-models {:critic {:provider :glm :model "g"
                                          :api-key "sk-role"}}}
             :db {:path "x"}}
        r (config/redacted cfg)]
    (is (= "***" (get-in r [:llm :api-key])))
    (is (= "***" (get-in r [:run :role-models :critic :api-key]))
        "a nested per-role key is masked too")
    (is (= "g" (get-in r [:run :role-models :critic :model]))
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

(deftest a-global-config-sits-beneath-the-project-config
  (let [home (temp-config-home "{:http {:port 4100} :llm {:model \"global-model\" :base-url \"http://global\"}}")
        root (temp-project-root "{:llm {:model \"project-model\"}}")]
    (try
      (with-redefs [config/config-home (fn [] home)]
        (testing "global-config reads <config-home>/samizdat/config.edn"
          (is (= {:http {:port 4100} :llm {:model "global-model" :base-url "http://global"}}
                 (config/global-config))))
        (let [cfg (config/load-config {:run {:root root}})]
          (testing "a value set only globally reaches the run"
            (is (= 4100 (get-in cfg [:http :port]))))
          (testing "the same key set in the project wins over global"
            (is (= "project-model" (get-in cfg [:llm :model]))))
          (testing "nested maps MERGE: the project's :llm :model did not wipe the global :llm :base-url"
            (is (= "http://global" (get-in cfg [:llm :base-url])))))
        (testing "an explicit override still beats both files"
          (is (= 5555 (get-in (config/load-config {:run {:root root} :http {:port 5555}})
                              [:http :port]))))
        (testing "the layered file config is one map, project on top"
          (is (= {:http {:port 4100}
                  :llm {:model "project-model" :base-url "http://global"}}
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
      (with-redefs [config/config-home (fn [] home)]
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
      (with-redefs [config/config-home (fn [] home)]
        (let [with-home (config/load-config {:run {:root root}})
              without (with-redefs [config/config-home (fn [] nil)]
                        (config/load-config {:run {:root root}}))]
          (is (= with-home without)
              "an absent global file contributes nothing, byte for byte")))
      (finally
        (delete-recursively (java.io.File. root))
        (delete-recursively (java.io.File. home))))))

(deftest a-broken-global-file-is-ignored-like-a-broken-project-file
  (let [home (temp-config-home "{:http {:port ")]
    (try
      (with-redefs [config/config-home (fn [] home)]
        (is (= {} (config/global-config))))
      (finally (delete-recursively (java.io.File. home))))))

(deftest config-sources-name-every-layer-and-whether-it-was-read
  ;; The boot log and /health show these, so a surprising value can be traced
  ;; to the file that set it.
  (let [home (temp-config-home "{}")
        root (temp-project-root nil)]
    (try
      (with-redefs [config/config-home (fn [] home)]
        (let [srcs (config/config-sources root)]
          (is (= [:global :project] (mapv :layer srcs)))
          (is (= (str home "/samizdat/config.edn") (:path (first srcs))))
          (is (true? (:present? (first srcs))))
          (is (= (str root "/.samizdat/config.edn") (:path (second srcs))))
          (is (false? (:present? (second srcs))))))
      (testing "no config home at all is reported, not thrown"
        (with-redefs [config/config-home (fn [] nil)]
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
