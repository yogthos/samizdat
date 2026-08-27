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

(ns samizdat.system
  "The one place long-lived resources live, so everything else can be a
  function that an editor redefines against a running process.

  The rule this namespace exists to enforce: resources that are expensive to
  recreate go in `system` behind start!/stop!; logic does not. A swipl session,
  a Lean REPL that spends thirty seconds importing Mathlib, the database
  connection, and the HTTP server are resources. Gate definitions, prompts,
  tool methods, and parsers are logic, and reloading their namespace mid-run is
  supposed to work.

  `defonce` so reloading this namespace from a connected editor does not drop
  the handles to a server that is still listening."
  (:require [clojure.tools.logging :as log]
            ;; installs the java.time.* host shim tools.logging's timestamp
            ;; formatter resolves against; must load before the first log call
            [jolt.time]
            [jolt.http.platform :as platform]
            [ring-chez.adapter :as adapter]
            [samizdat.api.control :as api-control]
            [samizdat.agent.gates :as gates]
            [samizdat.agent.phases :as phases]
            [samizdat.lexicon :as lexicon]
            [samizdat.config :as config]
            [samizdat.llm.client :as llm-client]
            [samizdat.llm.fence :as fence]
            [samizdat.llm.registry :as registry]
            [samizdat.manifests :as manifests]
            [mycelium.core :as myc]
            [samizdat.session :as session]
            [samizdat.lsp.client :as lsp-client]
            [samizdat.store.db :as db]
            [samizdat.store.runs :as runs]
            [samizdat.userspace :as userspace]))

(defonce system (atom nil))

(defn started? [] (some? @system))

(defn config [] (:config @system))

(defn conn
  "The single writer connection. See store.db for why there is only one."
  []
  (:conn @system))

(defn adapter
  "The provider adapter for the configured provider."
  []
  (registry/adapter-for (get-in @system [:config :llm :provider])))

(defn bind-project!
  "Point userspace at this project's store, THEN reload every policy table.

  One seam, because the order is the whole point (karamazov-blt.1): the
  reloads used to run at the top of start!, ~35 lines before `bind!`, so all
  three caches were filled from the SHIPPED templates and nothing re-read
  them after the project bound — a project whose gates, wordlists or phases
  had diverged silently ran factory policy for the whole process lifetime.
  Reloading after the bind is what makes the caches hold the project's own
  policy; the reload-on-every-start half (rather than trusting an atom that
  survives stop!/start!) is what lets a long-lived interpreted session pick
  up edits without a process restart."
  [conn]
  (userspace/bind! conn)
  (gates/reload-config!)
  (lexicon/reload!)
  (phases/reload!)
  conn)

(defn start!
  "Bring the system up. `overrides` is merged into the config, so a REPL
  session can do (start! {:db {:path \":memory:\"} :http {:port 3999}}).

  The handler is passed IN as a var rather than resolved here, because
  samizdat.server requires this namespace and resolving it dynamically would
  invert the dependency. It also has to be static: `jolt build` embeds what it
  can reach statically, and a `requiring-resolve` here left the entire server
  and engine subtree out of the binary, which then failed at startup trying to
  compile namespaces off source roots that do not exist in an image.

  Vars are callable and deref on each call, so redefining
  samizdat.server/handler in a connected editor still takes effect on the
  next request."
  ([handler] (start! handler nil))
  ([handler overrides]
   (when (started?)
     (throw (ex-info "system already started; call stop! first" {})))
   (let [cfg (config/load-config overrides)
         ;; A fresh session tally per process start. Short-term memory is
         ;; scoped to the process on purpose: a pattern that shows up across
         ;; three runs is exactly the pattern a single-run digest cannot see,
         ;; and a tally that survived a restart would be measuring a harness
         ;; that no longer exists.
         _ (session/reset!)
         ;; Process-wide, and set here rather than in core so that every entry
         ;; point gets it: the tests, the benchmark runner and a REPL session
         ;; all bring the system up through start! without going through -main.
         _ (platform/set-max-response-ms! (get-in cfg [:llm :max-response-ms]))
         ;; Ask the endpoint what it is, once, at startup. A llama.cpp server
         ;; answers /props with a total_slots; anything else answers something
         ;; else, and the probe returns nil. RFC-005 recorded that :local was
         ;; decided by which config key an endpoint sat under, so a
         ;; llama-server configured as :openai silently lost prefix pinning —
         ;; asking is what fixes that, and asking is specifically NOT the same
         ;; as sending the knob hopefully, which a strict OpenAI-compatible
         ;; server answers with a 422 on the whole request.
         ;;
         ;; Merged into the LLM config so it reaches chat-body the way every
         ;; other endpoint fact does, and so a test can set it directly.
         probed (llm-client/probe-llama-cpp (:llm cfg))
         cfg (cond-> cfg probed (update :llm merge probed))
         _ (when probed
             (log/info "endpoint identified as llama.cpp:"
                       (:total-slots probed) "KV slots — prefix caching on"))
         c (db/open! (get-in cfg [:db :path]))
         ;; Point the userspace reads at THIS project's store, and reload the
         ;; policy caches AFTER the bind so they hold the project's own
         ;; gates/wordlists/phases (bind-project! carries the ordering
         ;; argument). From here on a cell, manifest, policy table or prompt
         ;; resolves to the project's own version — seeded from the shipped
         ;; template on first read — so two projects running this binary can
         ;; evolve different loops and neither can edit the other's. Unbound
         ;; (a bare REPL, a unit test) the same reads fall back to the
         ;; templates, which is what the harness did before the store existed.
         _ (bind-project! c)
         ;; The repair ladder is a COMPOSITION, so the workflow layer owns it:
         ;; the `repair` manifest wires the fence's rung fns as cells, and
         ;; this install is how the fence — which sits below the workflow
         ;; layer and cannot require it — runs the project's version. Resolved
         ;; per call through compiled-manifest, so a manifest or cell edit
         ;; takes effect on the very next malformed call; fence's repair-json
         ;; fails open to its built-in chain if the manifest is broken.
         _ (fence/install-repair!
            (fn [body]
              (:body (myc/run-compiled (manifests/compiled-manifest "repair")
                                       {} {:body body}))))
         server (adapter/run-server handler {:port (get-in cfg [:http :port])})]
     (reset! system {:config cfg :conn c :server server})
     (log/info "samizdat up on port" (get-in cfg [:http :port])
               "provider" (get-in cfg [:llm :provider])
               "model" (get-in cfg [:llm :model])
               "db" (get-in cfg [:db :path]))
     ;; Nothing can be running yet, so any row that says it is, is a leftover
     ;; from a process that died. This is the only moment that inference is
     ;; sound. See store.runs/reconcile-orphans!.
     (let [n (runs/reconcile-orphans! c)]
       (when (pos? n)
         (log/info "marked" n "run(s) interrupted: still flagged running with no process")))
     :started)))

(defn stop!
  "Tear the system down. Best effort per resource: one failing close must not
  strand the others, which is the whole reason the RAX manager could always
  stop the Lisp task regardless of what the agent believed."
  []
  (when-let [s @system]
    (doseq [[label f] [;; Active runs FIRST, before anything they depend on
                       ;; closes under them: set every abort flag and give the
                       ;; run threads a bounded window to reach a boundary and
                       ;; journal their ending. Tearing the db down while run
                       ;; futures kept executing meant their writes — including
                       ;; the crash record — landed on a closed handle, and a
                       ;; restart!'s reconcile-orphans! marked still-executing
                       ;; runs interrupted while their threads kept going
                       ;; (karamazov-blt.14).
                       ["active runs"
                        #(let [runs @api-control/active]
                           (doseq [[_ {:keys [abort]}] runs]
                             (when abort (reset! abort true)))
                           (doseq [[rid {:keys [future]}] runs]
                             (when future
                               (when (= ::hung (deref future 15000 ::hung))
                                 (log/warn "run" rid "did not stop within 15s;"
                                           "closing the system under it")))))]
                       ["http server" #(adapter/stop-server (:server s))]
                       ;; Uninstall so a bare REPL after stop! parses with the
                       ;; built-in chain instead of resolving manifests against
                       ;; an unbound store.
                       ["repair seam" #(fence/install-repair! nil)]
                       ["lsp clients" #(lsp-client/shutdown-all!)]
                       ;; Unbind BEFORE the connection closes: a userspace read
                       ;; against a closed handle would throw where the same
                       ;; read against no handle simply serves the template.
                       ["userspace" #(userspace/unbind!)]
                       ["database" #(db/close (:conn s))]]]
      (try (f) (catch Throwable e (log/warn "stopping" label "failed:" (ex-message e)))))
    (reset! system nil)
    :stopped))

(defn restart!
  ([handler] (restart! handler nil))
  ([handler overrides] (stop!) (start! handler overrides)))
