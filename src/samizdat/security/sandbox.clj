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

(ns samizdat.security.sandbox
  "The OS confinement the project image runs under. MECHANISM ONLY — it builds
  a profile and an argv from paths it is handed, and decides nothing about
  which paths those are (samizdat.config) or when an image is spawned
  (samizdat.repl).

  WHY THIS IS AN OS PROBLEM AND NOT A LANGUAGE ONE. jolt compiles to Chez, and
  Chez's R6RS `environment` is a real lexical boundary — `#%foo` reads as
  `($primitive foo)` and `$primitive` is itself unbound in a restricted
  environment, so the usual primitive escape is closed, which is more than
  Racket's sandbox claims for itself. It still cannot give us what this bead
  needs, for two reasons. The boundary only holds for what you DO NOT inject,
  and \"let the model do IO through the application\" means injecting the
  application: an injected procedure that shells out escapes wholesale, and one
  that takes a path is a confused deputy (measured — it read /etc/passwd).
  Second, jolt does not expose Chez environments at all; jolt's eval resolves
  jolt vars, and `jolt.process/sh` and `jolt.ffi` sit right there.

  So confinement is the OS's job, on a separate process. Racket reaches the
  same conclusion by admitting its sandbox is escapable; Guile's (ice-9
  sandbox) avoids it by forbidding OS interaction entirely, which is the
  opposite of the requirement. Anthropic's sandbox-runtime is the reference
  implementation of the shape used here.

  TWO RULES THAT LOOK ARBITRARY AND ARE NOT, both measured on macOS 26.3:

  1. Reads are deny-then-allow; writes are allow-only. A strict read allowlist
     — /usr, /System, /Library, /opt/homebrew, ~/.jolt, cwd — SIGABRTs jolt
     before it prints anything. Enumerating every read a language runtime
     needs is unbounded, so the profile constrains what the image can CHANGE
     and REACH rather than what it can look at, and carves specific secret
     regions out of the reads.
  2. Seatbelt matches RESOLVED paths. `(literal \"/etc/passwd\")` denies
     nothing because /etc is a symlink to private/etc, and the failure is
     silent — the profile loads and the rule simply never fires. The first
     profile written for this leaked /etc/passwd exactly that way. Everything
     here goes through `resolved`."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn resolved
  "`path` with symlinks resolved, so a rule about it actually matches.

  The canonical form is what the kernel compares against: /etc is
  private/etc, /tmp is private/tmp, /var is private/var. A path that does not
  exist yet still canonicalises lexically, and anything that cannot be
  resolved at all falls back to the absolute form rather than being dropped —
  a rule on the wrong path is a bug, a rule that vanished is a hole."
  [path]
  (let [f (io/file (str path))]
    (try (.getCanonicalPath f)
         (catch Exception _ (.getAbsolutePath f)))))

(defn- sbpl-string
  "`s` as an SBPL string literal, quotes and backslashes escaped.

  A project directory is attacker-influenced in the case that matters — the
  agent creates directories — and an unescaped quote would close the literal
  and let the rest of the path be read as SBPL. That is a profile written by
  the thing being confined."
  [s]
  (str \" (-> (str s)
              (str/replace "\\" "\\\\")
              (str/replace "\"" "\\\""))
       \"))

(defn- clean
  "`paths` with nils and blanks dropped. A nil would render as the string
  \"nil\" and match a directory literally called nil; a blank resolves to the
  working directory, which is never what anyone meant."
  [paths]
  (->> paths (remove nil?) (map str) (remove str/blank?) distinct))

(defn- subpaths [paths]
  (str/join " " (map #(str "(subpath " (sbpl-string (resolved %)) ")") paths)))

(defn- rule
  "`(op filters…)`, or NOTHING when there are no filters.

  AN EMPTY FILTER LIST MATCHES EVERYTHING. `(deny file-read* )` denies every
  read and `(allow process-exec* )` allows every exec, and both parse without
  complaint — measured: a profile built from empty lists could not start jolt
  at all, because reading the binary was denied. The same shape pointing the
  other way silently grants what the line above it just took away, which is
  how a profile ends up reading as confinement while allowing the escape this
  whole bead is about. Emitting nothing is the only safe answer: an absent
  deny denies nothing, and an absent allow leaves the preceding deny standing."
  [op paths]
  (when-let [ps (seq (clean paths))]
    (str "(" op " " (subpaths ps) ")")))

(defn seatbelt-profile
  "A macOS seatbelt (SBPL) profile confining the project image.

  SBPL is LAST-MATCH-WINS, so every deny is written before the allows that
  carve exceptions out of it. Getting that order backwards produces a profile
  that loads, runs, and confines nothing.

  `:project-root`   the one tree the image may write
  `:scratch-paths`  additional writable trees (temp dirs the runtime needs)
  `:deny-read`      secret-bearing regions carved out of the default read
  `:exec-roots`     where the runtime binary lives; nothing else may exec

  `:exec-roots` IS EFFECTIVELY REQUIRED. sandbox-exec's own exec of the
  runtime is subject to the profile, so omitting it does not merely fail to
  allow the shell — it stops the image starting, with `execvp() … Operation
  not permitted`. Failing closed is the right direction here and the message
  says what happened, but a spawner has to pass them."
  [{:keys [project-root scratch-paths deny-read exec-roots]}]
  (->>
   [";; samizdat.security.sandbox — generated"
    "(version 1)"
    ""
    ";; reads: open by default (see sandbox.clj)"
    "(allow default)"
    ""
    ";; writes"
    "(deny file-write*)"
    ;; The device literals are unconditional: a writable project with no way to
    ;; print is not a working image.
    (str "(allow file-write* "
         (subpaths (clean (cons project-root scratch-paths)))
         " (literal \"/dev/null\") (literal \"/dev/stdout\")"
         " (literal \"/dev/stderr\") (literal \"/dev/tty\"))")
    ""
    ";; read denies"
    (rule "deny file-read*" deny-read)
    ;; RE-ALLOWED AFTER THE DENIES, because SBPL is last-match-wins and a
    ;; denied region may CONTAIN the project — a run whose root sits inside a
    ;; denied tree would otherwise be unable to read its own files. This is the
    ;; same allow-beats-deny shape sandbox-runtime documents for reads, and it
    ;; is what makes it safe to deny a whole directory without first proving
    ;; the project is not under it.
    (rule "allow file-read*" (cons project-root scratch-paths))
    ""
    ";; exec"
    "(deny process-exec*)"
    (rule "allow process-exec*" exec-roots)
    ""
    ";; network"
    "(deny network*)"
    "(allow network-bind network-inbound (local ip \"localhost:*\"))"
    "(allow network-outbound (remote ip \"localhost:*\"))"
    ""]
   (remove nil?)
   (str/join "\n")))

(defn backend-for
  "The sandbox backend for `setting` on `os-name` (java.lang.System's os.name).

  `:auto` resolves to seatbelt on macOS and to NOTHING anywhere else. That is
  deliberate and not a stub: shipping an unverified bubblewrap invocation
  would be a sandbox that reads as protection without having been shown to be
  one, which is worse than an honest `:none` — under `:none` the project image
  is still a separate sandboxless process, which already ends in-process
  access to the harness and fixes the classpath and cwd bugs. The Linux
  backend is karamazov-zrq.8, to be verified on a real host before `:auto`
  picks it."
  [setting os-name]
  (if (and (= :auto setting) (str/starts-with? (str os-name) "Mac OS X"))
    :seatbelt
    :none))

(defn wrap
  "`cmd` (an argv vector) wrapped so it runs under `backend`, given a profile
  already written to `profile-path`. Pure: the caller owns the file.

  Child processes INHERIT a seatbelt sandbox, so wrapping the nREPL server
  covers anything it manages to spawn — which, with `process-exec*` denied, is
  nothing."
  [backend profile-path cmd]
  (case backend
    :seatbelt (into ["sandbox-exec" "-f" (str profile-path)] cmd)
    (vec cmd)))
