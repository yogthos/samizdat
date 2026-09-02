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

(ns samizdat.repl.route
  "WHICH IMAGE AN EVAL LANDS IN, and how a refusal reads when it does not.

  `config/eval-image` decides — `:harness` for the supervisor, whose job IS the
  harness, `:project` for everyone else, `:off` for nobody. This namespace owns
  the consequences: starting the project image on first use, keeping one per
  project root for the life of the process, bounding an eval that never
  returns, and turning a kernel refusal into a sentence the model can act on.

  THERE IS NO FALLBACK TO THE HARNESS IMAGE. A project image that will not
  start fails the eval and says so. That is not defensiveness, it is the
  security property: `:image-connect-ms` lives in gates.edn, which is
  agent-editable userspace, so a fallback would let a run shrink the timeout
  until the project image 'failed' and it was handed the unconfined one
  instead. Failing is the only safe direction, and the model is told the
  harness is at fault so it does not go hunting in its own code.

  A REFUSAL IS NOT A DEFECT, and the model cannot tell them apart on its own.
  `posix_spawn failed (errno 1)` reads exactly like a bug in the form that
  produced it, and a model that believes that goes looking in the file — the
  karamazov-60c failure, ten turns spent reading code that was never wrong.
  Every refusal this seam returns says what was refused, why, and which tool
  to reach for instead."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [jolt.fs :as fs]
            [clojure.tools.logging :as log]
            [samizdat.config :as config]
            [samizdat.prompt :as prompt]
            [samizdat.repl :as repl]
            [samizdat.repl.guard :as guard]
            [samizdat.repl.image :as image]
            [samizdat.security.sandbox :as sandbox]))

(defonce ^:private images
  ;; root -> image. Per ROOT rather than per run: two runs against the same
  ;; checkout share a process the way two branches share one, and the cost of
  ;; a jolt start is paid once.
  (atom {}))

(defn runtime-exec-roots
  "The directory holding the `jolt` binary, resolved through its symlink.

  Falls back to the Homebrew prefix only when `jolt` is not on PATH — which
  cannot happen in a process that is itself running under jolt, but an empty
  list here would stop every image starting, and that is a worse failure than
  a slightly wide directory."
  []
  (or (some-> (fs/which "jolt") str sandbox/resolved
              (->> (io/file)) .getParentFile .getPath vector)
      ["/opt/homebrew"]))

(defn sandbox-spec
  "The paths the image is confined to, for a project at `root`.

  IN src/ AND NOT IN gates.edn, deliberately — the same rule
  `policy/protected-paths` states. This is the list of places the agent's own
  image may not read; a list the agent can edit is not a list."
  [home harness-root]
  {:deny-read (remove nil?
                      [(when home (str home "/.ssh"))
                       (when home (str home "/.aws"))
                       (when home (str home "/.gnupg"))
                       (when home (str home "/.config/gh"))
                       (when home (str home "/.claude"))
                       (when home (str home "/.samizdat-secrets"))
                       "/etc"
                       ;; THE HARNESS'S OWN CHECKOUT. Writes and hot-patching
                       ;; are already closed, but reads are open by default
                       ;; here (a strict read allowlist aborts the runtime), so
                       ;; without this the project image can slurp harness
                       ;; source by ABSOLUTE path — measured, and the reason
                       ;; the confinement test that "passed" only proved cwd
                       ;; had moved. Harmless when the harness IS the project:
                       ;; seatbelt-profile re-allows the project root after the
                       ;; denies, so self-hosting reads itself fine.
                       harness-root])
   ;; THE RUNTIME BINARY'S OWN DIRECTORY, and nothing else.
   ;;
   ;; This has to be narrow and it is easy to get wrong in the safe-looking
   ;; direction. `exec-roots` cannot be empty — sandbox-exec's own exec of jolt
   ;; is subject to the profile, so an empty list means nothing starts — and
   ;; the obvious fix is to add the directories a runtime "might" need. Adding
   ;; /bin and /usr/bin does start it, and also hands the REPL every standard
   ;; unix utility: measured, `(jolt.process/sh "echo" "X")` came back
   ;; `:exit 0` through a profile that looked confining. /bin/echo was allowed
   ;; because /bin was.
   ;;
   ;; So it is resolved from the jolt binary itself rather than guessed, and
   ;; through its symlink — a Homebrew jolt resolves into Cellar, which is a
   ;; directory holding jolt and not much else.
   :exec-roots (runtime-exec-roots)})

(defn image-for!
  "The project image for `root`, started if this is the first eval against it.
  nil when it could not be started."
  [root backend]
  (or (get @images root)
      (locking images
        (or (get @images root)
            (when-let [im (image/start!
                           {:root root :backend backend
                            :sandbox-spec (sandbox-spec (System/getenv "HOME")
                                                        (str (fs/cwd)))})]
              (swap! images assoc root im)
              im)))))

(defn release!
  "Stop and forget the image for `root`. Called at run teardown, and whenever
  an eval times out — a runaway computation is only really stopped by killing
  the process it is running in."
  [root]
  (when-let [im (get @images root)]
    (swap! images dissoc root)
    (image/stop! im))
  nil)

(defn release-all!
  "Stop every image. For shutdown, and for tests that must not leak a process."
  []
  (doseq [root (keys @images)] (release! root))
  nil)

;; --- making a refusal legible ------------------------------------------------

(def ^:private exec-refusal
  "What the kernel says when the sandbox refused to start a process."
  #"posix_spawn|Operation not permitted|Permission denied|EPERM")

(defn- exec-attempt? [s]
  (boolean (re-find #"posix_spawn|process|sh\b|exec" (str s))))

(defn denial
  "A kernel refusal rendered as policy, or nil when `error` is an ordinary
  failure. Pure over the strings so the wording is testable without a sandbox.

  `sandboxed?` GATES IT, because the strings are not exclusive to the sandbox:
  an ordinary `Permission denied` on a chmod-000 file inside the project would
  otherwise be dressed up as policy and the model told its code is fine, which
  is the same wrong-answer-that-looks-right this whole seam exists to remove.
  Under `:sandbox :none` nothing here can be a kernel refusal at all. The
  residual case — a genuine in-project permission error while the sandbox IS
  on — still mislabels, and narrowing that needs the path out of the message."
  [error root sandboxed?]
  (when (and error sandboxed? (re-find exec-refusal (str error)))
    (prompt/render "image-denied"
                   {:root root
                    :detail (str/trim (str error))
                    :exec (exec-attempt? error)})))

(defn- legible
  "`r` with a sandbox refusal reworded, and `:where` naming the image either
  way so a failure can never be mistaken for the wrong process."
  [r root sandboxed?]
  (if-let [d (and (not (:ok r)) (denial (:error r) root sandboxed?))]
    (assoc r :error d :policy-refusal? true)
    r))

;; --- routing -----------------------------------------------------------------

(defn image-of
  "Which image `ctx` evaluates in. An unidentified role gets `:project` — the
  safe direction, and the reason `config/eval-image` treats an unknown role
  that way rather than falling through to the harness."
  [ctx]
  (config/eval-image (config/eval-mode (:root ctx))
                     (or (:role ctx) (get-in ctx [:branch :role]))))

(defn eval-for
  "Evaluate `code` for `ctx`, in whichever image its role and the operator's
  config put it. Same result shape as `repl/eval-code`, always."
  [{:keys [root] :as ctx} code session timeout-ms]
  (let [timeout (or timeout-ms repl/default-eval-timeout-ms)]
    (case (image-of ctx)
      :off {:ok false :error (prompt/render "image-off" {}) :error-type "eval-off"}
      ;; THE SUPERVISOR'S IMAGE IS STILL THE LIVE ONE, so the last door to the
      ;; S2 escape is here. Static, and honest about being static — see
      ;; guard/kernel-write?. The refusal names the rule and what it fired on.
      :harness (if-let [hit (first (guard/kernel-writes
                                    (try (read-string (str "(do " code "\n)"))
                                         (catch Exception _ nil))))]
                 {:ok false :error-type "kernel-write"
                  :policy-refusal? true
                  :error (prompt/render "kernel-write-refused"
                                        {:rule (name (:rule hit))
                                         :on (str (:on hit))})}
                 (repl/eval-code code session timeout))
      ;; NO ROOT, NO IMAGE. Confinement is defined relative to the project, so
      ;; a ctx that never said which project cannot be confined to it: the
      ;; profile came out with no writable project root and the subprocess
      ;; inherited the HARNESS's working directory, which is a broken image
      ;; wearing a working one's clothes — it answered `(+ 1 2)` with 3 while
      ;; being unable to read the tree it was supposed to be developing. Refuse
      ;; instead, and say so to the operator rather than only to the model.
      (if (str/blank? (str root))
        (do (log/error "eval routed to a project image with no :root —"
                       "the ctx did not carry one; refusing rather than"
                       "confining to nothing")
            {:ok false :error-type "image-down"
             :error (prompt/render "image-down" {})})
        (let [backend (sandbox/backend-for (config/eval-sandbox root)
                                           (System/getProperty "os.name"))]
          (if-let [im (image-for! root backend)]
          ;; The transport is an FFI socket with no read timeout, so the bound
          ;; is a deadline on a future. On expiry the IMAGE goes: a runaway
          ;; form keeps burning a core until the process holding it dies, and
          ;; leaving it running would hand the next eval a busy image.
          (let [f (future (image/eval-in im code session))
                r (deref f timeout ::timeout)]
            (if (= ::timeout r)
              (do (release! root)
                  (log/warn "project image eval timed out after" timeout "ms — image restarted")
                  {:ok false :error-type "timeout" :timeout? true
                   :error (prompt/render "image-timeout" {:ms timeout})})
              (legible r root (not= :none backend))))
            {:ok false :error-type "image-down"
             :error (prompt/render "image-down" {})}))))))

;; --- doc and complete answer out of the SAME image ---------------------------
;;
;; They used to read the harness image unconditionally, which under `:project`
;; makes them the same wrong-answer-that-looks-right the cwd bug was: a model
;; asking for the docstring of a var in the project it is building would have
;; been handed samizdat's, plausibly and silently. Whatever image an eval lands
;; in is the one these have to describe.

(defn- read-value
  "Evaluate `form` in `ctx`'s image and read the printed value back, or nil."
  [ctx form]
  (let [r (eval-for ctx form nil nil)]
    (when (:ok r)
      (try (read-string (str (:value r))) (catch Exception _ nil)))))

(defn doc-for
  [ctx sym]
  (if (= :harness (image-of ctx))
    (repl/doc-sym sym)
    ;; The tool pr-strs :arglists itself, so this hands back the DATA rather
    ;; than a string of it — otherwise the model reads a quoted quote.
    (or (read-value ctx (str "(if-let [v (resolve (symbol " (pr-str (str sym)) "))]"
                             " (let [m (meta v)]"
                             "   {:name (str (:name m)) :arglists (:arglists m)"
                             "    :doc (str (:doc m))})"
                             " {:not-found true})"))
        {:not-found true})))

(defn complete-for
  [ctx prefix]
  (if (= :harness (image-of ctx))
    (repl/complete prefix)
    (or (read-value ctx (str "(->> (all-ns)"
                             " (mapcat (fn [n] (map #(str (ns-name n) \"/\" %)"
                             "                      (keys (ns-publics n)))))"
                             " (filter #(clojure.string/includes? % " (pr-str (str prefix)) "))"
                             " sort (take 50) vec"))
        [])))
