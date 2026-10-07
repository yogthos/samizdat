;; samizdat - a claim-first verification harness
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

(ns samizdat.core
  "Entry point: bring the system up, validate the provider, start nREPL, park.

  The workflow this is built for is to leave the process running and develop
  against it from an editor. The slow parts of this harness are exactly the
  parts you never want to restart: a swipl session, a Lean REPL that spends
  thirty seconds importing Mathlib, and a provider call that takes minutes.

      jolt dev                 # or: jolt serve, without the dev/test trees
      # connect CIDER / Calva / Cursive to the port in .nrepl-port, then:
      (require '[samizdat.system :as system])
      (system/config)
      (system/restart! {:http {:port 3999}})

  Redefining a namespace other than samizdat.system takes effect immediately;
  the server holds the handler var, not the function it currently contains.

  ## nREPL load order, previously load bearing, now not

  Loading jolt.nrepl before any TLS handshake used to leave jolt.http-client
  unable to complete one for the rest of the process, so this namespace warmed
  TLS against the provider first and only then loaded nREPL through a runtime
  `require`. That is fixed upstream: jolt.nrepl calls (ffi/load-library) at ns
  load to bind sockets, which loads the running process's own symbols, and on
  macOS the process image transitively links LibreSSL — so the SSL_* symbols
  came from a mix of LibreSSL and OpenSSL and the first call through it faulted.
  jolt.http.tls now loads libcrypto and libssl itself immediately before its
  bindings resolve, so whoever loads first no longer decides.

  nREPL is therefore a plain static require again, which also puts it in a
  `jolt build` binary — the runtime require was the reason it never started
  there. `samizdat.smoke/nrepl-load-order-check` stays as the regression guard.

  `warm-tls!` survives on its own merit: it validates at boot that the provider
  is reachable and the key works, which the TypeScript harness also does. It is
  no longer ordering-critical."
  (:require [clojure.tools.logging :as log]
            [jolt.fs]
            [samizdat.config :as config]
            [samizdat.security.listen :as listen]
            [samizdat.security.sandbox :as sandbox]
            ;; Statically required, both so the ordering bug stays fixed in the
            ;; open rather than by accident and so `jolt build` reaches it.
            [jolt.nrepl]
            ;; Every namespace the manual names, so the binary carries them
            ;; (samizdat.capabilities says why).
            [samizdat.capabilities]
            [nrepl.middleware]
            [samizdat.control :as control]
            [samizdat.llm.client :as llm]
            [samizdat.repl.guard :as guard]
            ;; Statically required so the build reaches the whole server and
            ;; engine subtree; core is the only place that can, since server
            ;; requires system.
            [samizdat.server :as server]
            [samizdat.system :as system]))

(defn warm-tls!
  "Make one real HTTPS request to the configured provider.

  Two jobs in one call. It validates that the provider is reachable and the
  key works, which the TypeScript harness also does at boot. And it completes
  a TLS handshake early, which used to be what kept https working for the rest
  of the process and is now merely a warm cache.

  Never throws. A harness whose provider is down should still come up: the
  engines need no network, and so does most development."
  [config]
  (let [{:keys [provider model]} (:llm config)]
    (try
      (let [models (llm/list-models (system/adapter) (:llm config))]
        (cond
          (empty? models)
          (do (log/warn "provider" provider "listed no models; TLS is warm but the"
                        "endpoint may be misconfigured")
              :unverified)

          (some #{model} models)
          (do (log/info "provider" provider "reachable," model "available") :ok)

          :else
          (do (log/warn "provider" provider "is reachable but does not list" model
                        "— available:" (pr-str (take 10 models)))
              :model-missing)))
      (catch Throwable e
        ;; This used to say https would stay broken once nREPL loaded — true
        ;; of an ordering bug fixed upstream long ago (see the ns docstring),
        ;; and wrong about the usual cause, a model server not started yet
        ;; (karamazov-mxf9).
        (log/warn "provider" provider "is not reachable:" (ex-message e)
                  "— the harness will start; with gates.edn :endpoint-preflight on,"
                  "a run is refused until the endpoint answers")
        :failed))))

(defn start-nrepl!
  "Start nREPL with the session/completion/lookup middleware on a background
  thread. SIGINT is blocked on this thread first so ^C lands here rather than
  on an accept loop parked in a foreign recv.

  jolt.nrepl is required statically in the ns form; see the namespace docstring
  for why it no longer has to wait for a TLS handshake."
  [port]
  (jolt.host/block-sigint)
  (try
    (let [stop (jolt.nrepl/start port ['nrepl.middleware/default-middleware])]
      ;; The nREPL is the whole harness process; nothing the agent runs may
      ;; reach it (security.listen).
      (listen/register! :nrepl port)
      (jolt.host/add-shutdown-hook stop)
      stop)
    (catch Throwable e
      ;; Not fatal — the harness serves without an editor attached.
      (log/warn "nREPL not started:" (ex-message e))
      nil)))

(defn nrepl-allowed?
  "Whether the harness nREPL may listen: only where the sandbox keeps the
  agent's shell and eval image off loopback (`:backend :seatbelt` for both),
  or where the operator's config.edn says `:nrepl {:unconfined :allow}`.

  The nREPL asks for no credentials and is the whole harness process, so on
  a host whose sandbox shares the network — Linux under bwrap, or no sandbox
  at all — any command the agent runs could connect to it and leave every
  confinement behind (karamazov-3vu1.11)."
  [{:keys [backend nrepl]}]
  (or (= :seatbelt backend)
      (= :allow (:unconfined nrepl))))

(defn- loopback-backend
  "The sandbox the agent's processes run under on this host, as far as
  loopback is concerned: :seatbelt only when the shell's and the eval image's
  both are."
  [cfg]
  (let [os (System/getProperty "os.name")
        bwrap? (some? (jolt.fs/which "bwrap"))
        root (get-in cfg [:run :root])
        bs [(sandbox/backend-for (:sandbox (config/shell-sandbox root)) os bwrap?)
            (sandbox/backend-for (config/eval-sandbox root) os bwrap?)]]
    (if (every? #{:seatbelt} bs) :seatbelt (first (remove #{:seatbelt} bs)))))

(defn- record-exit!
  "Name what was still running when the process ended.

  Best-effort and silent on failure by design: this runs during shutdown, when
  the store may already be closing, and a hook that throws would replace a
  useful warning with a stack trace about the warning."
  []
  (try
    (guard/record-exit!
     (->> (control/runs (system/conn))
          (filter #(= "running" (:status %)))
          (map :id)))
    (catch Throwable _ nil)))

(defn start!
  "Bring the harness up: the system, the shutdown hooks, nREPL, the provider
  check. Returns `{:config cfg :warm status}` and leaves the server running
  on its worker threads; the caller decides what the main thread does next —
  park (`-main`, headless) or run a front end (`samizdat.main`)."
  []
  (system/start! #'server/handler)
  (let [cfg (system/config)
        warm (warm-tls! cfg)]
    ;; RECORD THE EXIT BEFORE TEARING ANYTHING DOWN. A parked server does not
    ;; exit on its own, so an exit with work still in flight is a bug — and the
    ;; reason run a3ba69bb cost a whole investigation is that it went with a 0
    ;; and no message, which is indistinguishable from somebody stopping it
    ;; (karamazov-1xx). Registered FIRST so it runs while the store is still
    ;; open and can still say what was running.
    (jolt.host/add-shutdown-hook record-exit!)
    (jolt.host/add-shutdown-hook system/stop!)
    (if (nrepl-allowed? {:backend (loopback-backend cfg) :nrepl (:nrepl cfg)})
      (start-nrepl! (get-in cfg [:nrepl :port]))
      (log/warn "nREPL not started: on this host the agent's shell can reach"
                "loopback, and the nREPL asks for no credentials. config.edn"
                ":nrepl {:unconfined :allow} starts it anyway."))
    {:config cfg :warm warm}))

(defn local-url
  "The loopback URL a front end in this process reaches the server on."
  [cfg]
  (str "http://127.0.0.1:" (get-in cfg [:http :port])))

(defn print-banner! [{cfg :config warm :warm}]
  (println)
  (println "samizdat")
  (println (str "  http   " (local-url cfg) "/health"))
  (println (str "  nrepl  127.0.0.1:" (get-in cfg [:nrepl :port])))
  (println (str "  model  " (name (get-in cfg [:llm :provider]))
                " / " (get-in cfg [:llm :model])
                " (" (name warm) ")"))
  (println))

(defn -main
  "The server alone: start, print where it is, park. `samizdat --headless`
  and `jolt serve` both end up here."
  [& _args]
  (print-banner! (start!))
  ;; Park. The server and nREPL run on worker threads; the shutdown hooks
  ;; close them. Returning here lets the launcher tear the process down.
  (jolt.host/park-until-interrupt)
  (system/stop!))
