;; Ported from sdiff, https://github.com/semantic-namespace/diff (commit
;; 70e436e), MIT License, Copyright (c) 2026 @tangrammer.
;;
;; SPDX-License-Identifier: MIT

(ns samizdat.sdiff.names-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [samizdat.sdiff.names :as names]))

(def a-src "(ns co.acme.billing.invoice (:require [co.acme.billing.spec.invoice :as spec.invoice] [co.acme.shared.money :as money]))\n(defn total [x] (::spec.invoice/lines x) (money/sum x))")
(def b-src "(ns co.acme.billing.payment (:require [co.acme.billing.spec.invoice :as spec.invoice] [co.acme.shared.money :as m]))\n(defn pay [x] (m/sum x))")
(def t-src "(ns co.acme.billing.invoice-test (:require [co.acme.billing.invoice :as SUT]))")

(def report {:clj [{:path "src/co/acme/billing/invoice.clj" :new a-src :old a-src}
                   {:path "src/co/acme/billing/payment.clj" :new b-src :old b-src}
                   {:path "test/co/acme/billing/invoice_test.clj" :new t-src :old t-src}]
             :other [{:path "resources/billing/invoice.edn"}]})

(deftest names-come-from-the-authors-aliases-then-unique-suffixes
  (let [{:keys [paths nss]} (names/table report)]
    (is (= "spec.invoice" (nss "co.acme.billing.spec.invoice")) "the alias the source files agree on")
    (is (= "money" (nss "co.acme.shared.money")) "ties go to one alias, consistently")
    (is (not= "SUT" (nss "co.acme.billing.invoice")) "a test file's alias does not name a source namespace")
    (is (= "invoice.clj" (paths "src/co/acme/billing/invoice.clj")))
    (is (= "invoice.edn" (paths "resources/billing/invoice.edn")))))

(deftest the-legend-pays-for-itself
  (let [t (names/table report)]
    (is (= {"co.acme.billing.spec.invoice" "spec.invoice"}
           (:nss (names/used t (apply str (repeat 3 ":co.acme.billing.spec.invoice/lines "))))) "the legend lists only names the text uses")
    (is (empty? (:nss (names/used t "once :co.acme.billing.spec.invoice/lines"))) "a name used once costs more in the legend than it saves")))

(deftest only-qualified-names-are-shortened
  (let [t {:paths {} :nss {"org.acme.billing.core" "billing"}}]
    (is (= "billing/charge" (names/shorten-text t "org.acme.billing.core/charge")))
    (is (= "[org.acme.billing.core :as billing]" (names/shorten-text t "[org.acme.billing.core :as billing]")))))
