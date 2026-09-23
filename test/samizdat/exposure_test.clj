;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.exposure-test
  "Which providers may see which files (karamazov-d5wo.8): the project's
  config names path patterns per provider, and a read tool bound for that
  provider refuses them."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [jolt.fs :as fs]
            [samizdat.agent.tools :as tools]
            [samizdat.security.exposure :as exposure]))

(def ^:private config
  {:run {:provider-deny {:deepseek ["secrets/**" "**/*.pem"]}}})

(deftest a-glob-matches-the-paths-it-names
  (is (exposure/glob-matches? "secrets/**" "secrets/a.txt"))
  (is (exposure/glob-matches? "secrets/**" "secrets/deep/b.txt"))
  (is (not (exposure/glob-matches? "secrets/**" "src/secrets.clj")))
  (is (exposure/glob-matches? "**/*.pem" "k.pem") "** matches no directory too")
  (is (exposure/glob-matches? "**/*.pem" "a/b/k.pem"))
  (is (not (exposure/glob-matches? "*.pem" "a/k.pem")) "a single * stays inside one directory")
  (is (exposure/glob-matches? ".env" ".env"))
  (is (not (exposure/glob-matches? ".env" "xenv")) "a dot is a dot"))

(deftest the-denied-pattern-is-per-provider
  (is (= "secrets/**" (exposure/denied config :deepseek "secrets/k.txt")))
  (is (= "secrets/**" (exposure/denied config "deepseek" "secrets/k.txt")) "a string provider too")
  (is (nil? (exposure/denied config :glm "secrets/k.txt")) "another provider may read it")
  (is (nil? (exposure/denied config :deepseek "src/a.clj")))
  (is (nil? (exposure/denied {} :deepseek "secrets/k.txt")) "no policy, nothing denied"))

(defn- tree []
  (let [dir (str (fs/create-temp-dir))]
    (fs/create-dirs (fs/path dir "secrets"))
    (fs/spit (fs/path dir "secrets/key.clj") "TOPSECRET\n")
    (fs/spit (fs/path dir "a.clj") "(ns a) ; TOPSECRET is not here\n")
    dir))

(deftest a-read-bound-for-a-denied-provider-is-refused
  (let [dir (tree)
        ctx {:config config :root dir :branch {:id "B1"} :tool-name "read_file"
             :args {:path "secrets/key.clj"} :llm-config {:provider :deepseek}}]
    (is (str/includes? (str (exposure/refusal ctx)) "secrets/key.clj"))
    (testing "through the dispatcher, the content never comes back"
      (let [r (tools/run-tool ctx)]
        (is (= :mechanics (:category r)))
        (is (not (str/includes? (str (:result r)) "TOPSECRET")))))
    (testing "the same read on another provider goes through"
      (is (nil? (exposure/refusal (assoc ctx :llm-config {:provider :glm})))))
    (testing "read_digest checks the reader's provider as well"
      (is (some? (exposure/refusal
                  (assoc ctx :tool-name "read_digest" :args {:paths ["secrets/key.clj"]}
                         :llm-config {:provider :glm}
                         :config (assoc-in config [:run :role-models :reader] {:provider "deepseek"}))))))
    (testing "a path outside the patterns is not refused"
      (is (nil? (exposure/refusal (assoc ctx :args {:path "a.clj"})))))))

(deftest grep-hits-in-denied-files-are-dropped
  (let [dir (tree)
        ctx {:config config :root dir :branch {:id "B1"} :tool-name "grep"
             :args {:pattern "TOPSECRET"} :llm-config {:provider :deepseek}}
        r (tools/run-tool ctx)]
    (is (str/includes? (str (:result r)) "a.clj"))
    (is (not (str/includes? (str (:result r)) "secrets/key.clj")))))
