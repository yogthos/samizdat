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

(ns samizdat.security.confine
  "WHICH paths and ports the agent's processes are confined to — the decisions
  samizdat.security.sandbox deliberately does not make.

  IN src/ AND NOT IN gates.edn, for the reason `policy/protected-paths` and
  `route/sandbox-spec` give: this is the list of places the agent may not
  reach, and a list the agent can edit is not a list. An operator widens it in
  config.edn (`:shell :writable`), which no tool the agent holds can write.

  The shell tool's confinement lives here (karamazov-3vu1.5). The shell ran
  unconfined while its allow table held heads that run code the agent wrote
  — `make`, `just`, `jolt -e`, `cargo run` — so every confinement the eval
  image has was one Makefile away."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.logging :as log]
            [jolt.fs :as fs]
            [samizdat.security.sandbox :as sandbox]
            [samizdat.util :as util]))

(defn secret-regions
  "Where the operator's credentials live, for a user whose home is `home`,
  plus the harness's own checkout at `harness-root`. Unreadable from the eval
  image and from the shell.

  Not /etc: the image denies it, and the shell cannot — name resolution and
  TLS verification read it. `route/sandbox-spec` adds it for the image."
  [home harness-root]
  (vec (remove nil?
               (concat
                (when home
                  (map #(str home "/" %)
                       [".ssh" ".aws" ".gnupg" ".config/gh" ".claude"
                        ".samizdat-secrets"
                        ;; The machine-wide config: provider keys, the roles
                        ;; map. Readable from both until karamazov-3vu1.1.
                        ".config/samizdat"]))
                ;; THE HARNESS'S OWN CHECKOUT. Harmless when the harness IS the
                ;; project: both backends re-allow the project root after the
                ;; denies, so self-hosting reads itself fine.
                [harness-root]))))

(defn build-caches
  "The trees a build writes under `home` — dependency caches and toolchain
  state. Writable from the shell, because `jolt -M:test`, `cargo build` and
  `go test` fail without them; not $HOME as a whole."
  [home]
  (when home
    (mapv #(str home "/" %)
          [".jolt" ".cache" ".m2" ".gitlibs" ".cpcache" ".clojure" ".lein"
           ".cargo" ".rustup" "go" ".npm" ".yarn" ".pnpm-store" ".gradle"
           ".local/share" "Library/Caches"])))

(defn protected
  "The trees under the project root nothing the agent spawns may write: its
  .samizdat, which holds the cells the harness runs, the role surfaces and
  refusal rules that confine the agent, and the database with the grants
  table (karamazov-3vu1.2). The supervisor changes the workflow through its
  own tools, which run in the harness."
  [root]
  [(str root "/.samizdat")])

(defn shell-spec
  "The confinement for one shell command, as samizdat.security.sandbox reads
  it. Pure.

  `:scratch` is the shell's own temp dir (its TMPDIR); `:ports` the loopback
  ports the harness listens on (samizdat.security.listen); `:settings` the
  operator's `config/shell-settings`."
  [{:keys [home root harness-root scratch ports settings]}]
  {:project-root root
   ;; /tmp as well as the shell's own scratch: tools write there by name.
   ;; The profile is NOT under either — see `shell-command`.
   :scratch-paths (vec (remove nil? [scratch "/tmp"]))
   :writable (vec (concat (build-caches home) (:writable settings)))
   :deny-read (secret-regions home harness-root)
   :deny-ports (vec ports)
   :protect (protected root)})

(defn unconfined-reason
  "Why the shell setting `setting` leaves the shell unconfined on `os-name` —
  `:configured-off`, `:no-bwrap` (Linux without bubblewrap) or `:no-backend` —
  or nil when it does not."
  [setting os-name bwrap?]
  (when (= :none (sandbox/backend-for setting os-name bwrap?))
    (cond (= :none setting) :configured-off
          (str/starts-with? (str os-name) "Linux") :no-bwrap
          :else :no-backend)))

(defonce ^:private warned (atom #{}))

(defn backend
  "The backend for the shell setting `setting` on this host. Says once per
  reason when that is no confinement at all — it used to be silent
  (karamazov-3vu1.6)."
  [setting]
  (let [os (System/getProperty "os.name")
        bwrap? (some? (fs/which "bwrap"))]
    (when-let [why (unconfined-reason setting os bwrap?)]
      (when-not (contains? @warned why)
        (swap! warned conj why)
        (log/warn "the shell tool runs UNSANDBOXED:"
                  (case why
                    :configured-off "config.edn :shell :sandbox is :none"
                    :no-bwrap "no bubblewrap on this Linux host; install it"
                    (str "no sandbox backend on " os)))))
    (sandbox/backend-for setting os bwrap?)))

(defonce ^:private dirs
  ;; root -> {:scratch :profile-dir}. The scratch is the shell's; the profile
  ;; dir is a SIBLING temp dir, not inside it and not inside anything writable.
  (atom {}))

(defn- dirs-for! [root]
  (or (get @dirs root)
      (get (swap! dirs (fn [m] (if (get m root)
                                 m
                                 (assoc m root {:scratch (str (fs/create-temp-dir))
                                                :profile-dir (str (fs/create-temp-dir))}))))
           root)))

(defn shell-command
  "The argv and environment to run `command` in `root` under `backend`, as
  `{:argv :env :spec :profile}`.

  `:env` is the child's complete environment (already scrubbed by the
  caller); under a sandbox it gains TMPDIR pointing at the shell's own scratch.
  THE SYSTEM TEMP DIR IS NOT WRITABLE, and that is the point: the profile is
  written into a temp dir of its own, and a shell that could write where the
  profile lives could rewrite the confinement of its next command.

  `:ports`, `:settings`, `:home` and `:harness-root` default to the process's
  own; tests pass them."
  [{:keys [backend root command env ports settings home harness-root]}]
  (let [cd-cmd (str "cd " (util/sh-quote (or root ".")) " && " command)
        bash ["bash" "-c" cd-cmd]]
    (if (or (nil? backend) (= :none backend))
      {:argv bash :env env}
      (let [{:keys [scratch profile-dir]} (dirs-for! (str root))
            spec (shell-spec {:home (or home (System/getenv "HOME"))
                              :root (str root)
                              :harness-root (or harness-root (str (fs/cwd)))
                              :scratch scratch
                              :ports ports
                              :settings settings})
            spec (merge spec (sandbox/deny-read-kinds (:deny-read spec)))
            profile (str (io/file profile-dir "shell.sb"))]
        (when (= :seatbelt backend)
          (let [text (sandbox/seatbelt-shell-profile spec)]
            (when-not (= text (try (slurp profile) (catch Throwable _ nil)))
              (spit profile text))))
        {:argv (sandbox/wrap-shell backend {:profile profile :spec spec} bash)
         :env (when env (assoc env "TMPDIR" scratch))
         :spec spec
         :profile profile}))))
