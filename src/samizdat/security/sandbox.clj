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

;; --- Linux: bubblewrap + seccomp ----------------------------------------------
;;
;; The same shape as the seatbelt profile, from the same spec, with the three
;; things Linux has no direct rule for handled honestly (karamazov-zrq.8):
;;
;; - "deny read" is a MOUNT, not a rule. bwrap cannot refuse a read; a secret
;;   directory is hidden under an empty tmpfs and a secret file under
;;   /dev/null. Both read as empty rather than failing, which is what the deny
;;   is for. A path that does not exist cannot be mounted over, and a mount
;;   that fails stops the image rather than confining it, so the spawner
;;   classifies the list first (`deny-read-kinds`).
;; - "deny exec" cannot be a seccomp rule on execve: bwrap installs the filter
;;   before exec'ing the image itself, so that rule would refuse the image.
;;   What the macOS rule is FOR is that the image cannot spawn a shell, and
;;   that is a rule on making CHILD PROCESSES: fork, vfork and clone3 are
;;   refused and clone is allowed only with CLONE_THREAD, so threads work and
;;   subprocesses do not.
;; - the network is NOT confined. Loopback is how the harness reaches the
;;   image, a network namespace of its own would put the image's loopback out
;;   of reach, and seccomp cannot tell a loopback connect from any other. A
;;   Linux image may reach the network where the macOS one may not; the
;;   docstrings say so rather than pretending otherwise.
;;
;; Verified on a real kernel — Ubuntu 24.04 under Docker, privileged — by
;; dev/linux-sandbox/verify.sh, which runs sandbox-test and image-test there.

(def ^:private clone-thread 0x10000)

(defn- bpf
  "One sock_filter instruction as its eight little-endian bytes."
  [code jt jf k]
  (map unchecked-byte
       [(bit-and code 0xff) (bit-and (bit-shift-right code 8) 0xff)
        (bit-and jt 0xff) (bit-and jf 0xff)
        (bit-and k 0xff) (bit-and (bit-shift-right k 8) 0xff)
        (bit-and (bit-shift-right k 16) 0xff) (bit-and (bit-shift-right k 24) 0xff)]))

(def ^:private seccomp-program
  "The filter as [code jt jf k] rows. Jumps are forward offsets in
  instructions; the index comments say where each lands."
  (let [LD 0x20 JEQ 0x15 AND 0x54 RET 0x06
        allow 0x7fff0000 kill 0x80000000
        errno (fn [n] (bit-or 0x00050000 n))
        eperm (errno 1) enosys (errno 38)
        x86-64 0xC000003E aarch64 0xC00000B7]
    [[LD 0 0 4]                  ; 0  arch
     [JEQ 0 5 x86-64]            ; 1  x86_64 -> 2, else -> 7
     [LD 0 0 0]                  ; 2  nr
     [JEQ 7 0 56]                ; 3  clone   -> 11
     [JEQ 9 0 57]                ; 4  fork    -> 14 EPERM
     [JEQ 8 0 58]                ; 5  vfork   -> 14 EPERM
     [JEQ 8 9 435]               ; 6  clone3  -> 15 ENOSYS, else -> 16 allow
     [JEQ 0 9 aarch64]           ; 7  aarch64 -> 8, else -> 17 kill
     [LD 0 0 0]                  ; 8  nr
     [JEQ 1 0 220]               ; 9  clone   -> 11
     [JEQ 4 5 435]               ; 10 clone3  -> 15 ENOSYS, else -> 16 allow
     [LD 0 0 16]                 ; 11 args[0], low word: the clone flags
     [AND 0 0 clone-thread]      ; 12
     [JEQ 2 0 clone-thread]      ; 13 a thread -> 16 allow, else -> 14
     [RET 0 0 eperm]             ; 14
     [RET 0 0 enosys]            ; 15
     [RET 0 0 allow]             ; 16
     [RET 0 0 kill]]))           ; 17

(defn seccomp-no-subprocess
  "A seccomp-bpf filter — the bytes bwrap's --seccomp reads — refusing the
  image any CHILD PROCESS while leaving threads alone: fork, vfork and clone3
  fail, clone succeeds only with CLONE_THREAD. Both x86_64 and aarch64 in one
  program, keyed on the arch the KERNEL reports: an x86_64 image under Rosetta
  or qemu makes aarch64 syscalls, so a filter built for the binary's arch
  would let everything through. Any other arch is killed."
  []
  (byte-array (mapcat #(apply bpf %) seccomp-program)))

(defn- under?
  "Is `path` equal to or inside `root`?"
  [root path]
  (let [r (str (resolved root) "/") p (str (resolved path) "/")]
    (str/starts-with? p r)))

(defn deny-read-kinds
  "`paths` split into the directories and the files that exist, which bwrap
  hides differently, dropping what does not exist. The one impure step; the
  argv builder takes its answer."
  [paths]
  (let [fs (map io/file (clean paths))]
    {:deny-dirs (mapv str (filter #(.isDirectory %) fs))
     :deny-files (mapv str (filter #(.isFile %) fs))}))

(defn bwrap-argv
  "The bwrap argv confining the project image, ending in `--` so the image's
  own argv follows: the whole filesystem read-only, the project and scratch
  trees writable, secret directories hidden under an empty tmpfs and secret
  files under /dev/null, a private pid namespace, the image dying with the
  harness, a fresh session so it cannot push keystrokes at a terminal, and the
  seccomp filter on `seccomp-fd` (opened by the spawner — see `wrap`).

  A deny that CONTAINS a writable tree is not mounted: it would hide the
  project. The seatbelt profile re-allows the project after its denies for
  the same reason, and it is what lets a self-hosting run read itself. Pure."
  [{:keys [project-root scratch-paths deny-dirs deny-files seccomp-fd]}]
  (let [writable (clean (cons project-root scratch-paths))
        hides-writable? (fn [d] (some #(under? d %) writable))]
    (-> ["bwrap" "--ro-bind" "/" "/" "--dev" "/dev" "--proc" "/proc"
         "--unshare-pid" "--die-with-parent" "--new-session"]
        (into (mapcat (fn [p] ["--bind" p p]) writable))
        (into (mapcat (fn [p] ["--tmpfs" p])
                      (remove hides-writable? (clean deny-dirs))))
        (into (mapcat (fn [p] ["--ro-bind" "/dev/null" p])
                      (remove hides-writable? (clean deny-files))))
        (into (when seccomp-fd ["--seccomp" (str seccomp-fd)]))
        (into ["--chdir" (resolved project-root) "--"]))))

(defn backend-for
  "The sandbox backend for `setting` on `os-name` (java.lang.System's os.name),
  given whether `bwrap` is installed.

  `:auto` resolves to seatbelt on macOS, to bwrap on Linux when bubblewrap is
  installed, and to NOTHING otherwise — `:none`, under which the project image
  is still a separate sandboxless process that already ends in-process access
  to the harness and fixes the classpath and cwd bugs; image/start! logs it as
  unsandboxed so nobody mistakes it for confinement. `:bwrap` asks for the
  Linux backend by name and fails closed without bubblewrap: the image does
  not start and the eval refuses. Verified on a real x86_64 kernel before
  `:auto` picked it (karamazov-zrq.8, .github/workflows/linux-sandbox.yml;
  dev/linux-sandbox/verify.sh measures what an emulating host can). Ubuntu
  24.04 and later restrict unprivileged user namespaces by default
  (kernel.apparmor_restrict_unprivileged_userns); there bwrap fails to start
  and the image fails closed the same way."
  ([setting os-name] (backend-for setting os-name false))
  ([setting os-name bwrap?]
   (cond
     (= :bwrap setting) :bwrap
     (not= :auto setting) :none
     (str/starts-with? (str os-name) "Mac OS X") :seatbelt
     (and (str/starts-with? (str os-name) "Linux") bwrap?) :bwrap
     :else :none)))

(defn write-profile!
  "Write what `backend` reads at spawn to `path`: the SBPL profile for
  seatbelt, the seccomp filter for bwrap, nothing for none. Returns `path`."
  [backend path spec]
  (case backend
    :seatbelt (spit path (seatbelt-profile spec))
    :bwrap (with-open [o (io/output-stream path)]
             (.write o (seccomp-no-subprocess)))
    nil)
  path)

(defn wrap
  "`cmd` (an argv vector) wrapped so it runs under `backend`, given
  `{:profile path :spec spec}` — the file `write-profile!` wrote and the paths
  it confines to. Pure: the caller owns the file.

  Child processes INHERIT a seatbelt sandbox, so wrapping the nREPL server
  covers anything it manages to spawn — which, with `process-exec*` denied, is
  nothing. Under bwrap the filter reaches it as a file descriptor: the `sh`
  here is the HARNESS's own shell, exec'ing bwrap with fd 3 opened on the
  filter — $0 is the filter path, \"$@\" everything after it — and nothing
  of it survives into the image."
  [backend {:keys [profile spec]} cmd]
  (case backend
    :seatbelt (into ["sandbox-exec" "-f" (str profile)] cmd)
    :bwrap (-> ["sh" "-c" "exec bwrap \"$@\" 3<\"$0\"" (str profile)]
               (into (rest (bwrap-argv (assoc spec :seccomp-fd 3))))
               (into cmd))
    (vec cmd)))
