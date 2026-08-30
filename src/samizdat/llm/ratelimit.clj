;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Ported from dirge's src/agent/agent_loop/rate_limit_gate.rs.

(ns samizdat.llm.ratelimit
  "A PROCESS-GLOBAL LATCH, keyed by host: one account is one window.

  The beam runs several branches at once against one endpoint on one API key,
  and they do not know about each other. When the provider says it is
  exhausted, every branch learns that independently, one 429 at a time, and
  each one then sleeps and tries again — so the exhaustion costs as many
  requests as there are branches, repeatedly. dirge measured the shape it
  produces: 118 of 154 requests refused in three minutes.

  A latch turns that into one discovery. The first definitive signal marks the
  host as exhausted until a stated time, and every other request to that host
  is refused immediately with a shaped 429 rather than sent.

  TWO RULES THAT MAKE IT SAFE:

  - FAIL FAST, NEVER SLEEP HERE. The retry ladder above owns waiting, and it
    checks the abort flag between attempts; sleeping down here would hold a
    thread past an abort and race the run's teardown.
  - LATCH ONLY ON A DEFINITIVE SIGNAL. A bare 429 is not one — it may be a
    burst limit that clears in a second, and latching the host on it would
    stop every branch over one unlucky request. What counts is the provider
    stating WHEN: a Retry-After, or a remaining-count of zero with a reset.

  Cleared by any success, because the provider saying yes is better evidence
  than our own arithmetic about when it would."
  (:require [clojure.string :as str]))

(defonce ^:private latches (atom {}))

(defn host-of
  "The host a URL addresses, or the URL itself when it does not parse — the
  key only has to be stable and per-endpoint, not correct as a URL."
  [url]
  (or (second (re-find #"^[a-zA-Z]+://([^/]+)" (str url))) (str url)))

(defn definitive-signal
  "How long this response says the endpoint is exhausted for, in ms, or nil
  when it does not say.

  `headers` are lower-cased. A Retry-After is the provider stating a time
  outright. A remaining-count of ZERO plus a reset is the same statement in
  the rate-limit vocabulary — and the zero matters: a remaining count above
  zero with a reset is the ordinary shape of a healthy response."
  [status headers]
  (when (= 429 status)
    (let [h (fn [k] (some-> (get headers k) str str/trim not-empty))
          secs (some-> (h "retry-after") parse-long)
          remaining (some-> (or (h "x-ratelimit-remaining-requests")
                                (h "x-ratelimit-remaining-tokens"))
                            parse-long)
          reset (some-> (or (h "x-ratelimit-reset-requests")
                            (h "x-ratelimit-reset-tokens"))
                        (str/replace #"[sm]$" "")
                        parse-long)]
      (cond
        secs (* 1000 (max 0 secs))
        (and remaining (zero? remaining) reset) (* 1000 (max 0 reset))
        :else nil))))

(defn latched-for
  "Milliseconds this host is still latched for, or nil. Expired latches are
  dropped as they are read, so nothing has to sweep."
  [url now]
  (let [host (host-of url)]
    (when-let [until (get @latches host)]
      (if (< now until)
        (- until now)
        (do (swap! latches dissoc host) nil)))))

(defn latch!
  "Mark `url`'s host exhausted for `ms`. Returns the absolute deadline.

  The LONGER of an existing latch and this one: two branches learning about
  the same window must not shorten it between them."
  [url ms now]
  (let [host (host-of url)
        until (+ now (max 0 ms))]
    (get (swap! latches update host (fnil max 0) until) host)))

(defn clear!
  "Release `url`'s host. Called on any success — the provider saying yes is
  better evidence than our arithmetic about when it would."
  [url]
  (swap! latches dissoc (host-of url))
  nil)

(defn reset-all!
  "Drop every latch. For tests and for a process that has been idle long
  enough that its recorded windows mean nothing."
  []
  (reset! latches {})
  nil)
