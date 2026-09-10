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

(ns samizdat.engine.proc
  "Subprocess helpers shared by the engines.

  Every engine call is bounded. An unbounded one is not a slow call, it is a
  branch that never returns its turn, and with five branches in the beam that
  is the whole run. `run` kills the process tree on timeout rather than
  leaving an orphan holding a pipe."
  (:require [jolt.ffi :as ffi]
            [jolt.process :as p]))

(ffi/defcfn ^:private c-kill "kill" [:int :int] :int)

(def ^:private sigterm-grace-ms
  "How long a process gets to die of SIGTERM before it is sent SIGKILL."
  2000)

(defn- descendant-pids
  "Every live process below `p`, root excluded. jolt's ProcessHandle can
  ENUMERATE the tree, but its destroy never signals (probed: a plain sleep
  survives a direct ProcessHandle.destroy), so the killing here is done by
  pid through kill(2). Must run before the root dies — afterwards the
  children reparent to init and disappear from descendants()."
  [^java.lang.Process p]
  (try
    (mapv (fn [h] (.pid ^java.lang.ProcessHandle h))
          (iterator-seq (.iterator (.descendants (.toHandle p)))))
    (catch Throwable _ [])))

(defn- kill!
  "kill(2) on a pid; signal 0 probes existence. Never throws."
  [pid sig]
  (try (c-kill pid sig) (catch Throwable _ -1)))

(defn- reap!
  "Kill `proc`'s whole tree and do not return until the root is gone.

  SIGTERM the tree first, then SIGKILL whatever is still alive — including
  processes that trapped TERM, which outlived the old root-only escalation as
  orphans (provenance R3-8). p/destroy-tree is deliberately NOT used: on jolt it
  signals through ProcessHandle.destroy, which silently does nothing, so its
  tree-wide TERM never landed at all.

  Same principle the rest of the loop follows: a stop path that depends on
  the component agreeing to stop is not a stop path."
  [proc]
  (let [^java.lang.Process p (:proc proc)
        root (try (.pid p) (catch Throwable _ -1))]
    (doseq [pid (cons root (descendant-pids p))] (kill! pid 15))
    (when-not (try (.waitFor p sigterm-grace-ms java.util.concurrent.TimeUnit/MILLISECONDS)
                   (catch Throwable _ false))
      ;; Still alive. SIGKILL the children BEFORE the root — enumerated now,
      ;; because once the root dies they reparent to init and vanish from
      ;; descendants(). Re-enumerated here, not reused: the tree may have
      ;; spawned between the TERM and the KILL.
      (doseq [pid (descendant-pids p)] (kill! pid 9))
      (kill! root 9)
      (try (.destroyForcibly p) (catch Throwable _ nil))
      (try (.waitFor p sigterm-grace-ms java.util.concurrent.TimeUnit/MILLISECONDS)
           (catch Throwable _ nil)))))

(defn run
  "Run `args` with `input` on stdin, capturing stdout and stderr.

  Returns {:exit :out :err} or {:timeout true :ms n} if it did not finish
  inside `timeout-ms`. Never throws on a non-zero exit: z3 exits non-zero
  after `(get-model)` on an unsat formula even though the verdict itself was
  emitted cleanly, so the caller reads the output and decides.

  The wait is `.waitFor` with an explicit timeout rather than a timed `deref`,
  which does not work. jolt's `clojure.core/deref` forwards no opts to a record
  implementing IBlockingDeref, so (deref proc ms ::timeout) silently calls the
  blocking one-arity and waits for however long the process takes. It fails
  quietly, in the direction of doing nothing: the timeout branch below was
  simply unreachable, every engine call was unbounded, and the processes this
  believed it was killing accumulated. Twenty-eight orphaned z3 processes were
  found on one dev machine, the oldest at seventeen hours, slowing everything
  else enough to make an unrelated Mathlib import look sixteen times more
  expensive than it is. Fixed upstream too, but this does not depend on that."
  [{:keys [input timeout-ms env]} & args]
  ;; babashka.process/process takes the command vector FIRST and the options
  ;; map second. Passing them the other way round (which is what `sh` accepts)
  ;; stringifies the vector into an argv[0] of "[z3".
  ;;
  ;; :env, when given, is the COMPLETE environment the child sees — jolt's
  ;; process shim runs it as `env -i K=V …`, so nothing from the parent leaks
  ;; in. The shell tool relies on this: it hands a scrubbed environment here so
  ;; a subprocess cannot read a secret the parent holds (samizdat.security).
  (let [proc (p/process (vec args)
                        (cond-> {:in (or input "") :out :string :err :string}
                          env (assoc :env env)))
        ^java.lang.Process p (:proc proc)
        ms (or timeout-ms 30000)
        finished? (try
                    (.waitFor p ms java.util.concurrent.TimeUnit/MILLISECONDS)
                    (catch Throwable _ false))]
    (try
      (if-not finished?
        (do (reap! proc) {:timeout true :ms ms})
        ;; Exited, so this deref returns immediately and only collects the
        ;; already-complete stdout/stderr.
        (let [done @proc]
          {:exit (:exit done)
           :out (or (:out done) "")
           :err (or (:err done) "")}))
      ;; CLOSE THE PIPES. `:out :string` reads the child's output into a
      ;; string and leaves the pipe fds open: measured 2 fds per call, never
      ;; released, so 30 spawns cost 60 descriptors and 60 cost 120. That is
      ;; the leak behind provenance A-3 — the suite "holds ~260 fds at peak"
      ;; and needed `ulimit -n 1024` — and it is not only a test problem: the
      ;; `shell` tool comes through here, so a long run that shells out a few
      ;; hundred times exhausts its own descriptors and starts failing to open
      ;; the database ("sqlite step failed: unable to open database file",
      ;; which is exactly how it surfaced).
      ;;
      ;; In a `finally` so the timeout path closes too, and each close is
      ;; independent: a stream already closed by the shim must not stop the
      ;; other two from being closed.
      (finally
        (doseq [stream [(.getInputStream p) (.getErrorStream p) (.getOutputStream p)]]
          (try (some-> stream .close) (catch Throwable _ nil)))))))

(defn available?
  "Whether `bin` can be executed at all. Used by the smoke probes and to give
  a clear error instead of a stack trace when a toolchain is missing."
  [bin]
  (try
    (let [{:keys [exit timeout]} (run {:timeout-ms 5000} bin "--version")]
      (and (not timeout) (some? exit)))
    (catch Throwable _ false)))
