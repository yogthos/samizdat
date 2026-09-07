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

(ns samizdat.repl.image
  "The PROJECT image: a `jolt nrepl-server` subprocess rooted at the project,
  under the sandbox, that the harness evaluates into over loopback.

  WHY A SUBPROCESS AT ALL, separately from the sandbox. Three bugs share one
  cause — `eval` running in the harness process:

  - Confinement. A run could slurp harness source by relative path, patch it,
    and `(require … :reload)` it into the LIVE image, past the file tools'
    root confinement, the shell policy, and the mutation protocol
    (karamazov-zrq, run a3ba69bb S2).
  - Classpath. Workers on another project complained `eval` could not see
    that project's namespaces, because the harness image's source roots are
    samizdat's.
  - Working directory. `(slurp \"README.md\")` inside `eval` read SAMIZDAT's
    README and answered plausibly, because relative paths resolved against
    the harness's cwd. `samizdat.repl/warn-if-not-cwd!` exists only to shout
    about that, and this is the fix it could not be.

  Moving evaluation into a process whose cwd IS the project root fixes all
  three, and it fixes the last two even with `:sandbox :none` — which is why
  the container case loses nothing that matters.

  THE PROFILE LIVES WHERE THE IMAGE CANNOT WRITE. The image gets its own
  scratch directory rather than all of /tmp, and the profile is written
  somewhere outside every writable path. A confined process that can rewrite
  its own confinement has none the moment anything restarts it."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.logging :as log]
            [ebb.core :as ebb]
            [jolt.fs :as fs]
            [jolt.process :as process]
            [nrepl.transport :as transport]
            [samizdat.agent.gates :as gates]
            [samizdat.cancel :as cancel]
            [samizdat.prompt :as prompt]
            [samizdat.security.sandbox :as sandbox]
            [samizdat.security.secrets :as secrets]))

(defn connect-timeout-ms
  "How long to wait for the image's nREPL port to answer. gates.edn
  :image-connect-ms.

  Userspace-tunable, and safe to be: shrinking it cannot buy an escape,
  because an image that fails to start fails the eval and never falls back to
  the harness image."
  []
  (gates/threshold :image-connect-ms))

(defn free-port
  "A port nothing is listening on. Racy by nature — the gap between closing
  this socket and the child binding it is unavoidable without passing an
  inherited fd — but it is the standard answer and a lost race surfaces as a
  clean bind failure rather than a wrong answer."
  []
  (with-open [s (java.net.ServerSocket. 0)]
    (.getLocalPort s)))

(def nrepl-sha
  "The jolt-lang/nrepl the image runs: the SAME one deps.edn pins for the
  harness's own nREPL, so the image carries nrepl.middleware.interruptible-eval
  and a runaway eval can be interrupted rather than the image killed
  (RFC-013). A bare `jolt nrepl-server` answers the interrupt op with
  unknown-op. base-test holds this equal to deps.edn's pin, so the two cannot
  drift."
  "9cc45acc7a241ff9704e8f43e2c4fb90f7bd819c")

(def nrepl-sdeps
  "The -Sdeps map the image is started with: the nrepl dependency and its
  default middleware stack, merged over the project's own deps.edn."
  (pr-str {:deps {'jolt-lang/nrepl {:git/url "https://github.com/jolt-lang/nrepl"
                                    :git/sha nrepl-sha}}
           :nrepl/middleware ['nrepl.middleware/default-middleware]}))

(defn spawn-argv
  "The argv that starts the image: `jolt -Sdeps <nrepl-sdeps> nrepl-server
  <port>`, wrapped for the backend given `{:profile path :spec spec}`. Pure."
  [backend confinement port]
  (sandbox/wrap backend confinement ["jolt" "-Sdeps" nrepl-sdeps "nrepl-server" (str port)]))

(defn- await-port!
  "Block until `port` accepts a connection, or `deadline-ms` passes. True when
  it came up."
  [port deadline-ms]
  (let [end (+ (System/currentTimeMillis) deadline-ms)]
    (loop []
      (if (try (with-open [_ (java.net.Socket. "127.0.0.1" (int port))] true)
               (catch Exception _ false))
        true
        (when (< (System/currentTimeMillis) end)
          (Thread/sleep 100)
          (recur))))))

(defn start!
  "Start a project image at `root` and return it, or nil when it could not be
  started.

  `:backend` decides confinement; `:sandbox-spec` is the paths it confines,
  minus the scratch and profile locations, which are made here precisely so
  the image cannot write the file that confines it."
  [{:keys [root backend sandbox-spec]}]
  (let [scratch (str (fs/create-temp-dir))
        ;; A SEPARATE directory from the scratch, and not under any writable
        ;; path: this is the file that says what the image may do.
        profile-dir (str (fs/create-temp-dir))
        profile (str (io/file profile-dir "image.profile"))
        port (free-port)
        ;; The deny list classified for bwrap, which hides a directory and a
        ;; file differently and cannot mount over a path that is not there;
        ;; seatbelt reads the plain list and ignores the classification.
        spec (merge sandbox-spec
                    {:project-root root :scratch-paths [scratch]}
                    (sandbox/deny-read-kinds (:deny-read sandbox-spec)))]
    (sandbox/write-profile! backend profile spec)
    (let [argv (spawn-argv backend {:profile profile :spec spec} port)
          ;; THE CHILD SEES ONLY A SCRUBBED ENVIRONMENT, the same one the shell
          ;; tool's subprocess gets. boundary-test's security map records the
          ;; rule this is obeying: a tool whose reach is :spawns-process "must
          ;; not RECEIVE secrets in the first place — output redaction is not
          ;; enough on its own". eval used to be :in-process for every role and
          ;; so was covered by the redaction wrapper alone; for every role but
          ;; the supervisor it now spawns, and inheriting the harness's
          ;; environment would have handed the model every provider key inside
          ;; a process that can also write files into the project.
          proc (process/process argv {:dir (str root)
                                      :env (secrets/scrub-env
                                            (into {} (System/getenv)))})]
      (if (await-port! port (connect-timeout-ms))
        (do (log/info "project image up on" port "rooted at" root
                      (if (= :none backend) "(unsandboxed)" (str "under " (name backend))))
            {:proc proc :port port :root (str root) :backend backend
             :profile profile :scratch scratch
             ;; Namespaces already created in this image. See eval-in.
             ;; No shared transport: eval-in connects per call, because a
             ;; shared one crossed concurrent branches' replies and let a
             ;; timed-out eval poison every later one.
             :sessions (atom #{})})
        (do (log/error "project image did not come up on" port
                       "— profile at" profile)
            (try (process/destroy-tree proc) (catch Exception _ nil))
            nil)))))

(defn- reap-timeout-ms
  "How long to wait for a killed image to actually be gone. gates.edn
  :image-reap-ms.

  DESTROYING IS ASYNCHRONOUS. `destroy-tree` signals and returns, and the
  first version of `stop!` returned with it — so the image was still alive the
  instant afterwards, and a long-lived `serve` process would have leaked one
  sandboxed nREPL server per run, each holding a port. Teardown is not done
  until the process is gone. The kill itself is unconditional, so this only
  decides how long to watch before warning."
  []
  (gates/threshold :image-reap-ms))

(defn- reaped?
  "Poll until `proc` is gone or the deadline passes."
  [proc]
  (let [end (+ (System/currentTimeMillis) (reap-timeout-ms))]
    (loop []
      (cond
        (not (try (process/alive? proc) (catch Exception _ false))) true
        (< end (System/currentTimeMillis)) false
        :else (do (Thread/sleep 50) (recur))))))

(defn stop!
  "Tear an image down: close the connection, kill the process TREE, wait for it
  to actually die, drop the profile. Idempotent and never throws — teardown
  runs on paths that are already failing, and a teardown that throws loses the
  original error."
  [{:keys [proc profile scratch] :as image}]
  (when image
    ;; The process first. Connections are per-eval and owned by their caller;
    ;; killing the image EOFs any that are still open, including one an
    ;; abandoned timeout future is blocked on.
    (try (some-> proc process/destroy-tree) (catch Exception _ nil))
    (when (and proc (not (reaped? proc)))
      ;; Say so rather than leaking quietly. A survivor holds a port and a
      ;; sandbox, and the next run's free-port will simply route around it.
      (log/warn "project image did not die within" (reap-timeout-ms)
                "ms — it may be holding port" (:port image)))
    ;; The profile is the last thing to go: it is evidence while anything is
    ;; still running, and litter afterwards.
    (doseq [d [profile scratch]]
      (try (when d (fs/delete-tree (io/file d))) (catch Exception _ nil))))
  nil)

(defn- collect
  "Drain one nREPL eval exchange into `{:value :out :err}`.

  The value and the `done` status arrive in the SAME message, so a loop that
  returns on `done` before reading the message it saw it in loses the answer
  every time — which is what the first version of this did."
  [t]
  (loop [acc {:value nil :out [] :err []}]
    (let [m (transport/recv t)
          acc (cond-> acc
                (get m "value") (assoc :value (get m "value"))
                (get m "out") (update :out conj (get m "out"))
                (get m "err") (update :err conj (get m "err")))]
      (if (some #{"done"} (get m "status"))
        (-> acc
            (update :out #(str/join "" %))
            (update :err #(str/join "" %)))
        (recur acc)))))

(defn- ensure-session!
  "Make `ns-sym` exist in the image, once, with clojure.core referred into it.

  BRANCH ISOLATION IS OURS TO KEEP, not nREPL's. jolt's nREPL hands out
  distinct session ids from `clone`, but they share one evaluation namespace —
  measured: a def made under one session was readable from the other. Branches
  are competing approaches and a def one makes after a fork must not appear in
  the other (`repl/fork-session` exists for exactly that), so isolation here is
  a namespace per session, which the eval op's `ns` key does honour."
  [{:keys [transport sessions]} ns-sym]
  (when (and ns-sym sessions (not (contains? @sessions ns-sym)))
    (transport/send transport
                    {"op" "eval"
                     "code" (str "(do (create-ns '" ns-sym ")"
                                 " (binding [*ns* (the-ns '" ns-sym ")] (refer-clojure))"
                                 " :ok)")})
    (loop [] (when-not (some #{"done"} (get (transport/recv transport) "status")) (recur)))
    (swap! sessions conj ns-sym)))

(defn- clone!
  "A session of our own in the image, so a runaway eval can be interrupted by
  id from another connection. nil if the image does not answer within a few
  seconds, in which case the eval runs without one and a timeout falls back
  to the restart. On its OWN connection with a short receive timeout: the
  eval connection's timeout is the whole eval budget, and a server that never
  answers `clone` would otherwise spend the budget here and look like a
  stuck eval. Sessions are server-global, so an id cloned here is valid on
  the eval connection."
  [{:keys [port]}]
  (let [t (try (transport/connect "127.0.0.1" port {:recv-timeout-secs 3})
               (catch Exception _ nil))]
    (when t
      (try
        (transport/send t {"op" "clone"})
        (loop []
          (let [m (transport/recv t)]
            (cond (nil? m) nil
                  (get m "new-session") (get m "new-session")
                  (some #{"done"} (get m "status")) nil
                  :else (recur))))
        (catch Exception _ nil)
        (finally (try (transport/close t) (catch Exception _ nil)))))))

(defn- interrupt!
  "Ask the image to abort the eval running in session `sid`. jolt's nREPL
  aborts even a tight loop at its next engine tick; an eval blocked in a
  foreign call aborts when that call returns. Best effort, on its own
  connection, because the eval's connection is parked in recv."
  [{:keys [port]} sid]
  (when sid
    (let [t (try (transport/connect "127.0.0.1" port {:recv-timeout-secs 5})
                 (catch Exception _ nil))]
      (when t
        (try
          (transport/send t {"op" "interrupt" "session" sid})
          (loop []
            (let [m (transport/recv t)]
              (when (and m (not (some #{"done"} (get m "status")))) (recur))))
          (catch Exception _ nil)
          (finally (try (transport/close t) (catch Exception _ nil))))))))

(defn- finish [{:keys [value out err]}]
  (if (str/blank? err)
    {:ok true :value value :out out}
    {:ok false :error err :out out}))

(defn eval-in
  "Evaluate `code` in the image, in `session`'s namespace when one is given.
  Same result shape as `samizdat.repl/eval-code` so the caller cannot tell
  which image answered except by asking.

  ONE CONNECTION PER EVAL, and both reasons it has to be are bugs that a
  shared one actually produced:

  - CONCURRENT BRANCHES CROSSED WIRES. The beam runs branches in parallel and
    they share an image, so two `send`/`recv` pairs interleaved on one socket
    and each could drain the other's reply. Competing branches reading each
    other's results is the precise thing branch isolation exists to prevent.
  - A TIMED-OUT EVAL POISONED THE NEXT ONE. `route/eval-for` bounds an eval by
    abandoning the future that is blocked in `recv`; closing the shared socket
    under it freed the fd for reuse, and the abandoned reader then ate the
    replies meant for the new image. Measured: after one timeout, every later
    eval timed out too, and the image never recovered.

  Namespace state lives in the IMAGE, not the connection — isolation is the
  `ns` key, not an nREPL session — so a fresh socket per call costs a loopback
  connect and no semantics.

  WITH `:timeout-ms` (RFC-013) the eval is bounded in two stages. At the
  budget the image is asked to INTERRUPT the eval — its nREPL aborts even a
  tight loop at the next engine tick — and the aborted eval's reply is waited
  for up to `:grace-ms` (gates.edn :image-interrupt-grace-ms): `{:timeout? true :interrupted? true}`,
  image alive, defs intact. Only an eval that does not answer the interrupt
  (blocked in a foreign call) comes back `{:timeout? true :stuck? true}`, and
  that is the one case the caller restarts the image for. The eval connection
  carries a receive timeout so an abandoned reader dies on its own."
  ([im code] (eval-in im code nil))
  ([im code session] (eval-in im code session nil))
  ([{:keys [port] :as im} code session {:keys [timeout-ms grace-ms]}]
   (let [grace-ms (or grace-ms (gates/threshold :image-interrupt-grace-ms))
         t (try (transport/connect "127.0.0.1" port
                                   (when timeout-ms
                                     {:recv-timeout-secs
                                      (int (Math/ceil (/ (+ timeout-ms grace-ms 1000) 1000.0)))}))
                (catch Exception _ nil))]
     (if-not t
       {:ok false :error-type "image-down"
        :error (prompt/render "image-down" {:detail "the connection was refused"})}
       (try
         (ensure-session! (assoc im :transport t) session)
         (let [sid (when timeout-ms (clone! im))]
           (transport/send t (cond-> {"op" "eval" "code" (str code)}
                               session (assoc "ns" (str session))
                               sid (assoc "session" sid)))
           (if-not timeout-ms
             (finish (collect t))
             (let [started (cancel/start! (ebb/via ebb/blk (collect t)))
                   [tag r] (cancel/await-or-cancel started timeout-ms)]
               (case tag
                 :ok (finish r)
                 :err (throw r)
                 :timeout
                 (do (interrupt! im sid)
                     ;; await-or-cancel already cancelled the reader; the
                     ;; recv itself cannot be interrupted (the spike), so it
                     ;; is still parked and still delivers if the image
                     ;; answers the interrupt. Give it the grace.
                     (let [[tag2 r2] (cancel/await-or-cancel started grace-ms)]
                       (if (= :ok tag2)
                         {:ok false :error-type "timeout" :timeout? true :interrupted? true
                          :error (:err r2) :out (:out r2)}
                         {:ok false :error-type "timeout" :timeout? true :stuck? true})))))))
         (catch Exception e
           {:ok false
            :error (prompt/render "image-down" {:detail (ex-message e)})
            :error-type "image-down"})
         (finally
           (try (transport/close t) (catch Exception _ nil))))))))

(defn alive?
  [{:keys [proc]}]
  (boolean (try (some-> proc process/alive?) (catch Exception _ false))))
