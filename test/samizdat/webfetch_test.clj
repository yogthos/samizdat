;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.webfetch-test
  "The webfetch tool's confinement, offline.

  Every claim RFC-003 and the tool's own docstring make about egress is
  checked here against a fake HTTP effect, because the two ways this guard
  had been wrong (karamazov-luqc.1) were both invisible to a test that only
  reads the code: the client followed redirects on its own, so the
  're-checked' redirect was the final page's missing Location header, and a
  private address written as a decimal, an octal quad or an IPv4-mapped IPv6
  literal matched none of the hostname patterns."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [samizdat.agent.webfetch :as wf]))

;;; ------------------------------------------------------------ the guard

(deftest every-spelling-of-a-private-address-is-refused
  (doseq [host [;; the canonical spellings the patterns always caught
                "localhost" "LOCALHOST" "127.0.0.1" "127.0.0.2" "0.0.0.0"
                "::1" "[::1]" "169.254.169.254" "10.0.0.5" "192.168.1.1"
                "172.16.0.1" "172.31.255.255"
                ;; non-canonical IPv4 spellings that resolve to the same places
                "2130706433" "0177.0.0.1" "0x7f000001" "0x7f.0.0.1" "127.1"
                "2852039166" "0xa9fea9fe" "012.0.0.1"
                ;; IPv6 literals carrying an IPv4 address inside them
                "[::ffff:169.254.169.254]" "::ffff:7f00:1" "[::ffff:127.0.0.1]"
                "[::127.0.0.1]" "[64:ff9b::a9fe:a9fe]" "[2002:7f00:1::1]"
                "[2001:0:1234:5678::80ff:fffe]"
                ;; the other private v6 ranges
                "[fc00::1]" "[fd12:3456::1]" "[fe80::1]" "[::]"
                ;; CGNAT, and the names a private network hands out
                "100.64.0.1" "100.127.255.255"
                "metadata.google.internal" "printer.local" "metadata"]]
    (is (wf/private-host? host) host)))

(deftest a-public-address-is-not-refused
  (doseq [host ["example.com" "docs.rs" "93.184.216.34" "8.8.8.8"
                "[2606:4700::1111]" "[2001:db8::1]"
                ;; the edges of the private ranges, one outside each
                "100.63.255.255" "100.128.0.1" "172.32.0.1" "172.15.0.1"
                "11.0.0.1" "9.255.255.255" "169.253.0.1" "192.169.0.1"
                ;; letters that happen to be hex digits are still a name
                "cafe.beef.example"]]
    (is (not (wf/private-host? host)) host)))

(deftest allowed-needs-http-or-https-and-a-public-host
  (is (wf/allowed? "https://example.com/x"))
  (is (wf/allowed? "http://example.com/x"))
  (is (not (wf/allowed? "ftp://example.com/x")))
  (is (not (wf/allowed? "file:///etc/passwd")))
  (is (not (wf/allowed? "https://2130706433/")))
  (is (not (wf/allowed? "https://[::ffff:169.254.169.254]/latest/meta-data")))
  (is (not (wf/allowed? "not a url"))))

;;; -------------------------------------------------------- the redirect

(defn- scripted-get
  "A fake `http/get` answering each URL from `script`, recording what it was
  asked in `asked`, and refusing to follow anything itself — a client that
  follows redirects on its own is the bug this file exists to pin."
  [script asked]
  (fn [url opts]
    (swap! asked conj url)
    (is (false? (:follow-redirects opts)) "the client must not follow on its own")
    (or (get script url) {:status 404 :headers {} :body ""})))

(defn- redirect [to] {:status 302 :headers {"location" to} :body ""})
(defn- page [text] {:status 200 :headers {"content-type" "text/plain"} :body text})

(deftest a-redirect-into-the-private-network-is-refused-before-it-is-followed
  (let [asked (atom [])
        get (scripted-get {"https://example.com/r"
                           (redirect "http://169.254.169.254/latest/meta-data")
                           "http://169.254.169.254/latest/meta-data"
                           (page "SECRET")}
                          asked)
        r (wf/fetch "https://example.com/r" {:get get})]
    (is (:error r))
    (is (not (str/includes? (str (:error r)) "SECRET")))
    (is (str/includes? (str (:error r)) "169.254.169.254") "the refusal names the hop")
    (is (= ["https://example.com/r"] @asked) "the private hop was never requested")))

(deftest a-redirect-into-a-disguised-private-address-is-refused-too
  (let [asked (atom [])
        get (scripted-get {"https://example.com/r" (redirect "https://2130706433/s")
                           "https://2130706433/s" (page "SECRET")}
                          asked)
        r (wf/fetch "https://example.com/r" {:get get})]
    (is (:error r))
    (is (= ["https://example.com/r"] @asked))))

(deftest a-redirect-to-a-public-page-is-followed-one-checked-hop-at-a-time
  (let [asked (atom [])
        get (scripted-get {"https://example.com/r" (redirect "https://other.example/p")
                           "https://other.example/p" (redirect "https://other.example/q")
                           "https://other.example/q" (page "hello")}
                          asked)
        r (wf/fetch "https://example.com/r" {:get get})]
    (is (= "hello" (:ok r)))
    (is (= ["https://example.com/r" "https://other.example/p" "https://other.example/q"]
           @asked))))

(deftest a-relative-location-resolves-against-the-page-it-came-from
  (let [asked (atom [])
        get (scripted-get {"https://example.com/a/r" (redirect "/b")
                           "https://example.com/b" (page "moved")}
                          asked)
        r (wf/fetch "https://example.com/a/r" {:get get})]
    (is (= "moved" (:ok r)))
    (is (= ["https://example.com/a/r" "https://example.com/b"] @asked))))

(deftest a-redirect-chain-is-bounded
  (let [asked (atom [])
        get (scripted-get {"https://example.com/1" (redirect "https://example.com/2")
                           "https://example.com/2" (redirect "https://example.com/1")}
                          asked)
        r (wf/fetch "https://example.com/1" {:get get :max-redirects 3})]
    (is (:error r))
    (is (str/includes? (str/lower-case (str (:error r))) "redirect"))
    (is (= 4 (count @asked)) "the first request plus the allowed hops, then stop")))

(deftest a-direct-page-still-comes-back
  (testing "text is returned as it is"
    (let [get (scripted-get {"https://example.com/p" (page "plain text")} (atom []))]
      (is (= "plain text" (:ok (wf/fetch "https://example.com/p" {:get get}))))))
  (testing "a non-2xx answer is an error naming the status"
    (let [get (scripted-get {"https://example.com/p" {:status 404 :headers {} :body "gone"}}
                            (atom []))
          r (wf/fetch "https://example.com/p" {:get get})]
      (is (:error r))
      (is (str/includes? (str (:error r)) "404"))))
  (testing "the initial URL is upgraded to https before anything is asked"
    (let [asked (atom [])
          get (scripted-get {"https://example.com/p" (page "ok")} asked)]
      (is (= "ok" (:ok (wf/fetch "http://example.com/p" {:get get}))))
      (is (= ["https://example.com/p"] @asked)))))
