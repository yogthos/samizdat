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

(ns samizdat.layers-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest testing is]]
            [samizdat.layers :as layers]))

;; --- fixtures ----------------------------------------------------------------

(defn- temp-dir [prefix]
  (str (java.nio.file.Files/createTempDirectory
        prefix (make-array java.nio.file.attribute.FileAttribute 0))))

(defn- put!
  "Write `body` at `rel` under `dir`, making the parents."
  [dir rel body]
  (let [f (io/file dir rel)]
    (.mkdirs (.getParentFile f))
    (spit f body)
    (.getPath f)))

(defn- delete-recursively [^java.io.File f]
  (when (.isDirectory f)
    (doseq [c (.listFiles f)] (delete-recursively c)))
  (.delete f))

(defmacro with-dirs
  "A project root and a global dir, both empty, removed afterwards."
  [[root global] & body]
  `(let [~root (temp-dir "samizdat-layers-root")
         ~global (temp-dir "samizdat-layers-global")]
     (try ~@body
          (finally (delete-recursively (io/file ~root))
                   (delete-recursively (io/file ~global))))))

(defn- opts
  "Resolver options over the two dirs, with no environment unless given."
  ([root global] (opts root global {}))
  ([root global env]
   {:root root :global-dir global :getenv #(get env %)}))

(defn- layer-names [r] (mapv :layer (:sources r)))

;; --- the merge primitive -----------------------------------------------------

(deftest deep-merge-recurses-into-maps-and-replaces-everything-else
  (is (= {:a {:b 2 :c 3} :d 4}
         (layers/deep-merge {:a {:b 1 :c 3}} {:a {:b 2} :d 4})))
  (testing "a vector is a value, not a thing to merge into — a hiccup layout
            replaced position by position would be a tree nobody wrote"
    (is (= {:layout [:hbox [:b]]}
           (layers/deep-merge {:layout [:vbox [:a] [:c]]} {:layout [:hbox [:b]]}))))
  (testing "nil is a value a higher layer may set"
    (is (= {:a nil} (layers/deep-merge {:a {:b 1}} {:a nil}))))
  (is (= {:a 1} (layers/deep-merge {:a 1} nil))))

;; --- the layers --------------------------------------------------------------

(deftest with-only-the-shipped-file-its-text-comes-back-verbatim
  (with-dirs [root global]
    (let [t ";; the arrangement\n{:a 1}\n"
          r (layers/resolve "tui" (assoc (opts root global) :shipped t))]
      (is (= t (:body r)) "comments and layout survive: a person reads this")
      (is (= {:a 1} (:value r)))
      (is (= [:shipped] (layer-names r))))))

(deftest layers-merge-env-over-project-over-served-over-global-over-shipped
  (with-dirs [root global]
    (put! global "tui.edn" "{:a :global :b :global :c :global :d :global}")
    (put! root ".samizdat/tui.edn" "{:a :project :b :project}")
    (let [envf (put! root "elsewhere/t.edn" "{:a :env}")
          r (layers/resolve "tui"
                            (assoc (opts root global {"SAMIZDAT_TUI_FILE" envf})
                                   :served "{:a :served :b :served :c :served}"
                                   :shipped "{:a :s :b :s :c :s :d :s :e :s}"))]
      (is (= {:a :env :b :project :c :served :d :global :e :s} (:value r)))
      (is (= [:env :project :served :global :shipped] (layer-names r))
          "every layer that contributed, highest first")
      (is (= (:value r) (edn/read-string (:body r)))
          "a merged body is the merged value, readable by anything that reads the text"))))

(deftest a-layout-vector-is-replaced-whole-while-its-theme-merges
  (with-dirs [root global]
    (put! global "tui.edn" "{:theme {:agent {:color \"#111\"} :user {:color \"#222\"}}
                             :layout [:vbox [:widget/status]]}")
    (put! root ".samizdat/tui.edn" "{:theme {:agent {:color \"#999\"}}}")
    (let [r (layers/resolve "tui"
                            (assoc (opts root global)
                                   :shipped "{:layout [:hbox [:widget/input]] :prose-turns 12}"))]
      (is (= {:theme {:agent {:color "#999"} :user {:color "#222"}}
              :layout [:vbox [:widget/status]]
              :prose-turns 12}
             (:value r))))))

(deftest the-tui-keeps-its-old-environment-variable
  (with-dirs [root global]
    (let [f (put! root "mine.edn" "{:prose-turns 3}")
          r (layers/resolve "tui"
                            (assoc (opts root global {"SAMIZDAT_TUI_LAYOUT" f})
                                   :shipped "{:prose-turns 12}"))]
      (is (= 3 (get-in r [:value :prose-turns])))
      (is (= f (:path (first (:sources r))))))))

(deftest a-layer-that-does-not-read-costs-itself-and-says-so
  (with-dirs [root global]
    (put! global "config.edn" "{:a :global}")
    (let [bad (put! root ".samizdat/config.edn" "{:a ")
          r (layers/resolve "config" (assoc (opts root global) :shipped "{:b 1}"))]
      (is (= {:a :global :b 1} (:value r)) "the rest still merge")
      (is (= [bad] (mapv :path (:errors r))))
      (is (re-find #"did not parse" (:error (first (:errors r)))))))
  (with-dirs [root global]
    (let [bad (put! global "config.edn" "[:not :a :map]")
          r (layers/resolve "config" (assoc (opts root global) :shipped "{:b 1}"))]
      (is (= {:b 1} (:value r)))
      (is (= [bad] (mapv :path (:errors r))))))
  (testing "an env variable naming a file that is not there is an error, not a
            silent fall-through: somebody set it meaning something"
    (with-dirs [root global]
      (let [r (layers/resolve "tui" (opts root global {"SAMIZDAT_TUI_FILE" "/no/such.edn"}))]
        (is (= ["/no/such.edn"] (mapv :path (:errors r))))))))

(deftest an-edit-to-a-layer-file-is-seen-on-the-next-resolve
  (with-dirs [root global]
    (let [p (put! root ".samizdat/config.edn" "{:a 1}")
          o (opts root global)]
      (is (= {:a 1} (:value (layers/resolve "config" o))))
      ;; Different length, so the stamp moves even inside one mtime tick.
      (spit p "{:a 22}")
      (is (= {:a 22} (:value (layers/resolve "config" o)))))))

(deftest with-no-root-and-no-global-dir-only-what-was-handed-in-answers
  (let [r (layers/resolve "tui" {:getenv (constantly nil)
                                 :served "{:a 1}" :shipped "{:a 0 :b 0}"})]
    (is (= {:a 1 :b 0} (:value r)))
    (is (= [:served :shipped] (layer-names r)))))

(deftest nothing-anywhere-is-nil-not-an-error
  (with-dirs [root global]
    (let [r (layers/resolve "webui" (opts root global))]
      (is (nil? (:body r)))
      (is (nil? (:value r)))
      (is (empty? (:sources r)))
      (is (empty? (:errors r))))))

(deftest any-name-resolves-so-a-new-front-end-needs-no-loader
  (with-dirs [root global]
    (put! global "gui.edn" "{:theme {:bg \"#000\"}}")
    (is (= {:theme {:bg "#000"}} (:value (layers/resolve "gui" (opts root global)))))))

;; --- where things are --------------------------------------------------------

(deftest the-global-dir-is-under-the-config-home
  (with-redefs [layers/config-home (fn [] "/tmp/cfg")]
    (is (= "/tmp/cfg/samizdat" (layers/global-dir))))
  (with-redefs [layers/config-home (fn [] nil)]
    (is (nil? (layers/global-dir)))))

(deftest the-environment-variable-is-named-after-the-file
  (is (= ["SAMIZDAT_CONFIG_FILE"] (layers/env-vars "config")))
  (is (= ["SAMIZDAT_WEB_UI_FILE"] (layers/env-vars "web-ui")))
  (is (= ["SAMIZDAT_TUI_FILE" "SAMIZDAT_TUI_LAYOUT"] (layers/env-vars "tui"))))

(deftest the-paths-read-are-listed-highest-first
  (with-dirs [root global]
    (let [envf (put! root "x.edn" "{}")]
      (is (= [{:layer :env :path envf :var "SAMIZDAT_TUI_FILE"}
              {:layer :project :path (str root "/.samizdat/tui.edn")}
              {:layer :global :path (str global "/tui.edn")}]
             (layers/candidates "tui" (opts root global {"SAMIZDAT_TUI_FILE" envf})))))))
