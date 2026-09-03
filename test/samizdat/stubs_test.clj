;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.stubs-test
  "The delegation boundary as CODE: a parent writes the stubs a piece must
  fill, a child implements them, and both halves are checkable rather than
  judged. See samizdat.agent.stubs."
  (:require [clojure.test :refer [deftest testing is]]
            [samizdat.agent.stubs :as stubs]))

(def ^:private src
  "(ns example.core)

(defn ready
  \"Already implemented.\"
  [x]
  (* 2 x))

(defn unimplemented-throw
  \"The shape the split prompt asks for.\"
  [a b]
  (throw (ex-info \"not implemented\" {:fn 'unimplemented-throw})))

(defn unimplemented-nil
  [a]
  nil)

(defn- private-stub
  []
  (throw (ex-info \"not implemented\" {})))

(def a-value 42)
")

(deftest definitions-are-found-by-name
  (let [ds (stubs/definitions src)]
    (is (= #{"ready" "unimplemented-throw" "unimplemented-nil" "private-stub" "a-value"}
           (set (keys ds)))
        "every top-level def form, private ones included")
    (is (some? (stubs/definition src "ready")))
    (is (nil? (stubs/definition src "absent"))
        "a name nothing defines is nil, not a throw")))

(deftest a-file-that-does-not-read-yields-no-definitions
  ;; A parent that writes a broken stub file must get a legible refusal, not a
  ;; reader exception out of the middle of the split.
  (is (= {} (stubs/definitions "(defn broken [")))
  (is (nil? (stubs/definition "(defn broken [" "broken"))))

(deftest stub-recognises-the-shapes-a-stub-is-written-in
  (testing "a body that only announces it is unimplemented"
    (is (stubs/stub? (stubs/definition src "unimplemented-throw")))
    (is (stubs/stub? (stubs/definition src "unimplemented-nil")))
    (is (stubs/stub? (stubs/definition src "private-stub"))))
  (testing "and a real body is not one"
    (is (not (stubs/stub? (stubs/definition src "ready"))))
    (is (not (stubs/stub? (stubs/definition src "a-value")))))
  (testing "nil is not a stub — absence and emptiness are different answers"
    (is (not (stubs/stub? nil)))))

(deftest filled-in-is-the-childs-half-of-the-contract
  ;; What a child has to make true: the stub its parent wrote still exists
  ;; under the same name, and it is no longer a stub.
  (let [after "(ns example.core)
(defn unimplemented-throw [a b] (+ a b))"]
    (is (not (stubs/filled? src "unimplemented-throw")) "before: present but hollow")
    (is (stubs/filled? after "unimplemented-throw") "after: implemented"))
  (is (not (stubs/filled? "(ns example.core)" "unimplemented-throw"))
      "a stub the child DELETED is not filled in — it is gone"))
