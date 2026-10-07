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

(ns samizdat.sandbox-test
  "The OS sandbox profile the project image runs under.

  EVERY ASSERTION HERE IS SOMETHING MEASURED, not something reasoned about.
  The shape of the profile was arrived at empirically against Chez 10.4.1 /
  jolt 0.7.28 on macOS 26.3, and the two rules that look arbitrary are the two
  that cost the most to find:

  - Reads are DENY-THEN-ALLOW, writes are ALLOW-ONLY. A strict read allowlist
    (/usr, /System, /Library, /opt/homebrew, ~/.jolt, cwd) SIGABRTs the
    runtime before it prints anything — enumerating every read a language
    runtime needs is unbounded. This is the same asymmetry Anthropic's
    sandbox-runtime documents, and it is not a preference.
  - Seatbelt matches RESOLVED paths. `(literal \"/etc/passwd\")` denies
    nothing, because /etc is a symlink to private/etc — the first profile
    leaked /etc/passwd exactly this way and read it back through the sandbox.
    Same for /tmp and /var.

  Verified working end to end before any of this was written: project reads
  and writes succeed, $HOME writes and ~/.ssh reads are denied,
  (jolt.process/sh \"echo\") and (jolt.process/sh \"/bin/sh\" \"-c\" \"id\")
  both fail with posix_spawn errno 1, and `jolt nrepl-server` still binds
  loopback so the harness can reach it."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [jolt.fs :as fs]
            [jolt.process]
            [samizdat.security.sandbox :as sandbox]))

(def ^:private spec
  {:project-root "/Users/dev/proj"
   :scratch-paths ["/tmp"]
   :deny-read ["/Users/dev/.ssh" "/etc"]
   :exec-roots ["/opt/homebrew"]})

(defn- profile [] (sandbox/seatbelt-profile spec))

;; --- the shape that a runtime survives --------------------------------------

(deftest reads-are-not-allowlisted
  ;; The lesson that cost a SIGABRT. If this ever becomes (deny default) with
  ;; an enumerated read allowlist, jolt stops starting.
  (let [p (profile)]
    (is (str/includes? p "(allow default)")
        "a strict read allowlist aborts the runtime — see the ns docstring")
    (is (not (str/includes? p "(deny default)")))))

(deftest writes-are-denied-before-they-are-allowed
  (let [p (profile)
        deny (str/index-of p "(deny file-write*)")
        allow (str/index-of p "(allow file-write*")]
    (is (some? deny) "writes were never denied by default")
    (is (some? allow))
    (is (< deny allow)
        "the allow precedes the blanket deny, so the deny overrides it — SBPL is last-match-wins")))

;; --- the footgun that leaked /etc/passwd ------------------------------------

(deftest deny-read-paths-are-resolved-through-symlinks
  ;; Writing an UNRESOLVED path denies nothing at all, and the failure is
  ;; SILENT: the profile loads and the rule never matches. On macOS /etc is a
  ;; symlink to private/etc and that is the bug that leaked /etc/passwd; the
  ;; PROPERTY is "whatever the kernel would canonicalise this to is what gets
  ;; written", which is what to assert on any platform. Hardcoding
  ;; /private/etc made this a macOS-only test and it failed on CI's Linux,
  ;; where /etc is not a symlink and resolves to itself.
  (let [paths ["/etc" "/tmp/x" "/var/y"]
        p (sandbox/seatbelt-profile (assoc spec :deny-read paths))]
    (doseq [path paths]
      (is (str/includes? p (sandbox/resolved path))
          (str path " was written unresolved")))))

(deftest writable-paths-are-resolved-too
  ;; A writable path that does not resolve is a project the image cannot
  ;; write to — the same silent mismatch pointing the other way.
  (let [p (sandbox/seatbelt-profile (assoc spec :scratch-paths ["/tmp"]))]
    (is (str/includes? p (sandbox/resolved "/tmp")))))

;; --- what the image may and may not do --------------------------------------

(deftest the-project-root-is-writable
  (is (str/includes? (profile) "/Users/dev/proj")))

(deftest arbitrary-exec-is-denied-and-the-runtime-is-not
  (let [p (profile)]
    (is (str/includes? p "(deny process-exec*)")
        "the REPL could still shell out")
    (is (str/includes? p "/opt/homebrew")
        "the jolt runtime itself could not exec, so nothing would start")
    (is (< (str/index-of p "(deny process-exec*)")
           (str/index-of p "(allow process-exec*"))
        "last match wins — the allow has to come after the deny")))

(deftest the-network-is-inbound-loopback-only
  ;; nREPL is how the harness talks to this image at all, so loopback bind has
  ;; to survive the network deny. Verified: the sandboxed server binds 7899
  ;; and is reachable, with outbound still refused.
  ;;
  ;; OUTBOUND LOOPBACK IS NOT ALLOWED EITHER (karamazov-3vu1.1). It was, and
  ;; loopback is where the harness listens: its HTTP API (approvals, grants,
  ;; the approval mode) and its own nREPL, which is the whole harness process.
  ;; A per-port carve-out does not work — measured on macOS 26.3, a
  ;; `(deny network-outbound (remote ip "localhost:P"))` after an allow of
  ;; localhost:* is ignored, and so is a require-not inside the allow — so the
  ;; image, which only ever ACCEPTS the harness's connection, gets no outbound
  ;; at all.
  (let [p (profile)]
    (is (str/includes? p "(deny network*)"))
    (is (str/includes? p "network-inbound")
        "denying all network leaves the harness unable to reach the image")
    (is (not (str/includes? p "network-outbound"))
        "the image could connect to the harness's own ports")
    (is (< (str/index-of p "(deny network*)")
           (str/index-of p "network-bind")))))

;; --- the shell's profile (karamazov-3vu1.5) ----------------------------------

(def ^:private shell-spec
  {:project-root "/Users/dev/proj"
   :scratch-paths ["/tmp"]
   :writable ["/Users/dev/.jolt"]
   :deny-read ["/Users/dev/.ssh"]
   :deny-ports [3985 7888]})

(deftest the-shell-may-run-anything-but-write-only-where-it-is-told
  (let [p (sandbox/seatbelt-shell-profile shell-spec)]
    (is (str/includes? p "(allow default)"))
    (is (not (str/includes? p "process-exec"))
        "a shell that cannot exec is not a shell; the confinement is on what it touches")
    (is (< (str/index-of p "(deny file-write*)") (str/index-of p "(allow file-write*")))
    (doseq [w ["/Users/dev/proj" "/Users/dev/.jolt"]]
      (is (str/includes? p (sandbox/resolved w)) w))
    (is (< (str/index-of p "(deny file-read*") (str/last-index-of p "(allow file-read*"))
        "the project is re-allowed after the secret denies")))

(deftest the-shell-cannot-reach-the-harness-ports
  (let [p (sandbox/seatbelt-shell-profile shell-spec)]
    (is (not (str/includes? p "(deny network*)"))
        "git fetch and a dependency download are ordinary shell work")
    (doseq [port [3985 7888]]
      (is (str/includes? p (str "(deny network-outbound (remote ip \"localhost:" port "\"))"))
          (str port)))))

(deftest a-port-that-is-not-a-port-is-not-emitted
  (let [p (sandbox/seatbelt-shell-profile (assoc shell-spec :deny-ports [nil 0 70000 "3985\")(allow default"]))]
    (is (not (str/includes? p "remote ip")))))

;; --- a path is not a place to put SBPL --------------------------------------

(deftest a-path-cannot-break-out-of-the-profile
  ;; A project directory is attacker-influenced in the case that matters: the
  ;; agent creates directories. An unescaped quote would end the string and
  ;; the rest of the path would be read as SBPL — which is a profile the
  ;; attacker writes.
  (let [p (sandbox/seatbelt-profile
           (assoc spec :project-root "/tmp/ev\"il\") (allow default) (subpath \"/"))]
    (is (not (str/includes? p "\"il\") (allow default)"))
        "a quote in a path terminated the SBPL string")
    (is (str/includes? p "\\\"")
        "the quote was dropped rather than escaped — that silently changes the path")))

(deftest a-path-with-a-backslash-is-escaped
  (let [p (sandbox/seatbelt-profile (assoc spec :project-root "/tmp/a\\b"))]
    (is (str/includes? p "\\\\"))))

;; --- an empty filter list matches EVERYTHING --------------------------------

(deftest an-empty-path-list-omits-its-rule-rather-than-emitting-a-bare-one
  ;; In SBPL an operation with no filter matches everything, so `(deny
  ;; file-read* )` denies every read and `(allow process-exec* )` allows every
  ;; exec. Both parse without complaint. Measured: a profile built with empty
  ;; lists could not start jolt at all — "execvp() of 'jolt' failed: No such
  ;; file or directory", because reading the binary was denied. The same shape
  ;; pointing the other way silently reopens the shell escape this bead is
  ;; about, which is the dangerous half.
  (let [p (sandbox/seatbelt-profile
           {:project-root "/Users/dev/proj" :scratch-paths [] :deny-read [] :exec-roots []})]
    (testing "no read denies at all, rather than denying every read"
      (is (not (str/includes? p "(deny file-read*"))))
    (testing "exec stays denied, rather than being allowed everywhere"
      ;; This FAILS CLOSED and it fails hard: with no exec-roots the image
      ;; cannot start at all — "execvp() of 'jolt' failed: Operation not
      ;; permitted", since sandbox-exec's own exec of the runtime is subject
      ;; to the profile. That is the correct direction and a loud one, but it
      ;; means a spawner must always supply exec-roots (karamazov-zrq.4).
      (is (str/includes? p "(deny process-exec*)"))
      (is (not (str/includes? p "(allow process-exec*"))))
    (testing "the project is still writable"
      (is (str/includes? p "/Users/dev/proj")))))

(deftest nil-and-blank-paths-are-dropped-not-emitted
  ;; A nil in a path list would render as the string "nil" and match a
  ;; directory literally called nil; a blank would resolve to the cwd.
  (let [p (sandbox/seatbelt-profile
           (assoc spec :deny-read ["/etc" nil ""] :exec-roots ["/opt/homebrew" nil]))]
    (is (not (str/includes? p "\"nil\"")))
    (is (str/includes? p (sandbox/resolved "/etc")))))

;; --- backend selection ------------------------------------------------------

(deftest auto-resolves-to-the-platform-backend
  (testing "macOS gets seatbelt"
    (is (= :seatbelt (sandbox/backend-for :auto "Mac OS X"))))
  (testing "an unverified platform gets nothing rather than a guess"
    ;; Shipping an untested bubblewrap profile would be a sandbox that reads
    ;; as protection and is not one. karamazov-zrq.8 carries the Linux
    ;; backend, to be verified on a real host before :auto selects it.
    (is (= :none (sandbox/backend-for :auto "Linux")))
    (is (= :none (sandbox/backend-for :auto "Windows 11"))))
  (testing ":none is honoured everywhere — the container case"
    (is (= :none (sandbox/backend-for :none "Mac OS X")))))

(deftest wrapping-a-command-is-a-no-op-without-a-backend
  ;; :sandbox :none still gets the subprocess split, which is most of the fix.
  (is (= ["jolt" "nrepl-server"]
         (sandbox/wrap :none {:profile "/x/p.sb"} ["jolt" "nrepl-server"]))))

(deftest wrapping-under-seatbelt-puts-the-profile-on-the-command
  ;; Pure: the caller writes the profile and hands over the path. Round 3 owns
  ;; the file, this owns the argv.
  (is (= ["sandbox-exec" "-f" "/x/p.sb" "jolt" "nrepl-server"]
         (sandbox/wrap :seatbelt {:profile "/x/p.sb"} ["jolt" "nrepl-server"]))))

;; --- the Linux backend (karamazov-zrq.8) ------------------------------------

(deftest auto-picks-bwrap-on-linux-only-where-it-is-installed
  ;; Never a stub: without bubblewrap :auto is an honest :none, under which
  ;; the image is still a separate process and logged as unsandboxed. :bwrap
  ;; asks for it by name and fails closed at spawn when it is missing.
  (is (= :bwrap (sandbox/backend-for :auto "Linux" true)))
  (is (= :none (sandbox/backend-for :auto "Linux" false)))
  (is (= :none (sandbox/backend-for :auto "Linux")))
  (is (= :bwrap (sandbox/backend-for :bwrap "Linux" false)))
  (is (= :seatbelt (sandbox/backend-for :auto "Mac OS X" true)))
  (is (= :none (sandbox/backend-for :none "Linux" true)))
  (is (= :none (sandbox/backend-for :auto "Windows 11" true))))

(defn- run-of
  "Does `argv` contain `run` as consecutive elements?"
  [argv run]
  (boolean (some #(= run (vec %)) (partition (count run) 1 argv))))

(deftest bwrap-mounts-the-shape-the-seatbelt-profile-rules
  (let [argv (sandbox/bwrap-argv {:project-root "/work/proj"
                                  :scratch-paths ["/tmp/scratch-1"]
                                  :deny-dirs ["/home/dev/.ssh"]
                                  :deny-files ["/home/dev/.netrc"]
                                  :seccomp-fd 3})]
    (testing "everything read-only FIRST, then the writable trees over it"
      (is (= ["bwrap" "--ro-bind" "/" "/"] (subvec argv 0 4)))
      (is (run-of argv ["--bind" "/work/proj" "/work/proj"]))
      (is (run-of argv ["--bind" "/tmp/scratch-1" "/tmp/scratch-1"])))
    (testing "a secret directory is hidden under a tmpfs, a secret file under /dev/null"
      (is (run-of argv ["--tmpfs" "/home/dev/.ssh"]))
      (is (run-of argv ["--ro-bind" "/dev/null" "/home/dev/.netrc"])))
    (testing "its own pid namespace, dies with the harness, no terminal to push keys at"
      (is (every? (set argv) ["--unshare-pid" "--die-with-parent" "--new-session"])))
    (testing "the filter arrives on the fd the spawner opened"
      (is (run-of argv ["--seccomp" "3"])))
    (testing "and it starts in the project, with the image's argv after --"
      (is (= ["--chdir" "/work/proj" "--"] (subvec argv (- (count argv) 3)))))))

(deftest a-deny-that-would-hide-the-project-is-not-mounted
  ;; The self-hosting case: the harness root is denied and IS the project.
  ;; seatbelt re-allows the project after its denies; bwrap has to leave
  ;; the mount out, or the run could not read its own tree.
  (let [argv (sandbox/bwrap-argv {:project-root "/work/samizdat"
                                  :deny-dirs ["/work/samizdat" "/work" "/home/dev/.ssh"]
                                  :deny-files ["/work/samizdat/secret" "/home/dev/.netrc"]})]
    (is (not (run-of argv ["--tmpfs" "/work/samizdat"])))
    (is (not (run-of argv ["--tmpfs" "/work"])))
    (is (run-of argv ["--tmpfs" "/home/dev/.ssh"]))
    (testing "a deny INSIDE the project still holds"
      (is (run-of argv ["--ro-bind" "/dev/null" "/work/samizdat/secret"])))
    (testing "no filter fd, no --seccomp"
      (is (not (some #{"--seccomp"} argv))))))

(deftest the-seccomp-filter-refuses-child-processes-and-nothing-else
  ;; Read back as instructions rather than trusted as bytes: the filter is
  ;; the only thing standing between the image and a shell, and a wrong
  ;; jump offset is a filter that allows everything while looking strict.
  (let [rows (partition 8 (map #(bit-and 0xff %) (sandbox/seccomp-no-subprocess)))
        code (fn [[a b]] (+ a (* 256 b)))
        k (fn [[_ _ _ _ a b c d]] (+ a (* 256 b) (* 65536 c) (* 16777216 d)))]
    (is (= 18 (count rows)) "eighteen instructions of eight bytes")
    (testing "it loads the arch the KERNEL reports before it looks at a number"
      (is (= [0x20 4] [(code (first rows)) (k (first rows))])))
    (testing "both arches are named, because an x86_64 image under Rosetta makes aarch64 syscalls"
      (is (= #{0xC000003E 0xC00000B7}
             (set (map k (filter #(and (= 0x15 (code %)) (> (k %) 0xC0000000)) rows))))))
    (testing "the syscalls it names are clone, fork, vfork and clone3 on x86_64, clone and clone3 on aarch64"
      (is (= #{56 57 58 435 220}
             (set (map k (filter #(and (= 0x15 (code %)) (< (k %) 1000)) rows))))))
    (testing "CLONE_THREAD is the one flag that lets a clone through"
      (is (some #(and (= 0x54 (code %)) (= 0x10000 (k %))) rows)))
    (testing "every verdict is allow, EPERM, ENOSYS or kill — never a silent fallthrough"
      (is (= #{0x7fff0000 0x00050001 0x00050026 0x80000000}
             (set (map k (filter #(= 6 (code %)) rows)))))
      (is (= 6 (code (last rows))) "the program ends in a return"))
    (testing "every jump lands inside the program"
      (doseq [[i [_ _ jt jf :as row]] (map-indexed vector rows)
              :when (= 0x15 (code row))]
        (is (< (+ i 1 jt) 18) (str "instruction " i " jumps past the end"))
        (is (< (+ i 1 jf) 18) (str "instruction " i " jumps past the end"))))))

(deftest deny-read-kinds-splits-what-exists-and-drops-what-does-not
  (let [dir (str (fs/create-temp-dir))
        file (str dir "/secret")]
    (spit file "s")
    (is (= {:deny-dirs [dir] :deny-files [file]}
           (sandbox/deny-read-kinds [dir file (str dir "/absent") nil ""])))))

(deftest bwrap-confines-the-shell-without-the-image-filter
  ;; The shell has to make child processes — that is what a shell is — so it
  ;; gets the image's mounts without the seccomp filter that refuses them.
  (let [argv (sandbox/bwrap-shell-argv {:project-root "/work/proj"
                                        :scratch-paths ["/tmp/s"]
                                        :writable ["/home/dev/.jolt"]
                                        :deny-dirs ["/home/dev/.ssh"]
                                        :deny-files ["/home/dev/.netrc"]})]
    (is (= ["bwrap" "--ro-bind" "/" "/"] (subvec argv 0 4)))
    (is (run-of argv ["--bind" "/home/dev/.jolt" "/home/dev/.jolt"]))
    (is (run-of argv ["--tmpfs" "/home/dev/.ssh"]))
    (is (not (some #{"--seccomp"} argv)))
    (is (= "--" (peek argv)))))

(deftest wrapping-a-shell-command
  (is (= ["bash" "-c" "ls"] (sandbox/wrap-shell :none {} ["bash" "-c" "ls"])))
  (is (= ["sandbox-exec" "-f" "/p" "bash" "-c" "ls"]
         (sandbox/wrap-shell :seatbelt {:profile "/p"} ["bash" "-c" "ls"])))
  (let [argv (sandbox/wrap-shell :bwrap {:spec {:project-root "/w"}} ["bash" "-c" "ls"])]
    (is (= "bwrap" (first argv)))
    (is (= ["--" "bash" "-c" "ls"] (subvec argv (- (count argv) 4))))))

;; --- the shell's profile, measured ------------------------------------------

(defn- mac? [] (str/starts-with? (str (System/getProperty "os.name")) "Mac OS X"))

(deftest a-sandboxed-shell-works-in-the-project-and-cannot-leave-it
  ;; Measured under sandbox-exec, not reasoned about: the per-port deny is the
  ;; one rule shape seatbelt honours for the network here (see
  ;; the-network-is-inbound-loopback-only for the two it ignores).
  (when (mac?)
    (let [root (str (fs/create-temp-dir))
          secret (str (fs/create-temp-dir))
          outside (str (fs/create-temp-dir))
          _ (spit (str secret "/key") "SECRET")
          harness (java.net.ServerSocket. 0)
          other (java.net.ServerSocket. 0)
          profile (str (fs/create-temp-dir) "/shell.sb")
          _ (spit profile (sandbox/seatbelt-shell-profile
                           {:project-root root :deny-read [secret]
                            :deny-ports [(.getLocalPort harness)]}))
          sh (fn [cmd] (let [r (jolt.process/sh "sandbox-exec" "-f" profile "bash" "-c" cmd)]
                         (str (:out r) (:err r) " exit=" (:exit r))))]
      (try
        (testing "the project is writable and readable"
          (is (str/includes? (sh (str "cd " root " && echo ok > f && cat f")) "ok")))
        (testing "outside it is not writable"
          (sh (str "echo x > " outside "/escape"))
          (is (not (.exists (java.io.File. (str outside "/escape"))))))
        (testing "a secret directory is not readable"
          (is (not (str/includes? (sh (str "cat " secret "/key")) "SECRET"))))
        (testing "the harness port is unreachable and another loopback port is not"
          (is (str/includes? (sh (str "echo > /dev/tcp/127.0.0.1/" (.getLocalPort harness) " && echo CONNECTED"))
                             "exit=1"))
          (is (str/includes? (sh (str "echo > /dev/tcp/127.0.0.1/" (.getLocalPort other) " && echo CONNECTED"))
                             "CONNECTED")))
        (finally (.close harness) (.close other))))))

;; --- read-only inside a writable tree (karamazov-3vu1.2) ---------------------
;;
;; <root>/.samizdat holds the project's workflow: the cells that run in the
;; harness, the role surfaces, the refusal rules, the database with the
;; grants table. The project root is writable to the image and the shell, so
;; all of that was too — `cp evil.clj .samizdat/cells/loop.clj` was an allowed
;; command. The supervisor changes the workflow through its own tools, in the
;; harness; nothing the agent spawns writes it.

(deftest a-protected-tree-is-not-writable-inside-a-writable-one
  (doseq [p [(sandbox/seatbelt-profile (assoc spec :protect ["/Users/dev/proj/.samizdat"]))
             (sandbox/seatbelt-shell-profile (assoc shell-spec :protect ["/Users/dev/proj/.samizdat"]))]]
    (let [deny (str "(deny file-write* (subpath \"" (sandbox/resolved "/Users/dev/proj/.samizdat") "\"))")]
      (is (str/includes? p deny))
      (is (< (str/index-of p "(allow file-write*") (str/index-of p deny))
          "last match wins: the deny has to come after the allow it carves from"))))

(deftest bwrap-mounts-a-protected-tree-read-only-over-the-writable-one
  (let [dir (str (fs/create-temp-dir))
        prot (str dir "/.samizdat")]
    (fs/create-dirs prot)
    (doseq [argv [(sandbox/bwrap-argv {:project-root dir :protect [prot]})
                  (sandbox/bwrap-shell-argv {:project-root dir :protect [prot]})]]
      (let [bind (.indexOf argv "--bind")
            ro (some (fn [i] (when (= ["--ro-bind" prot prot] (subvec argv i (+ i 3))) i))
                     (range (- (count argv) 2)))]
        (is ro)
        (is (< bind ro) "mounted after the writable bind, so it lands on top")))
    (testing "a protected path that does not exist is not mounted — bwrap would fail"
      (is (not (some #{"/nope/.samizdat"}
                     (sandbox/bwrap-argv {:project-root dir :protect ["/nope/.samizdat"]})))))))

(deftest a-sandboxed-shell-cannot-write-the-workflow
  (when (mac?)
    (let [root (str (fs/create-temp-dir))
          prot (str root "/.samizdat")
          _ (fs/create-dirs prot)
          _ (spit (str prot "/gates.edn") "{}")
          profile (str (fs/create-temp-dir) "/shell.sb")
          _ (spit profile (sandbox/seatbelt-shell-profile {:project-root root :protect [prot]}))
          sh (fn [cmd] (jolt.process/sh "sandbox-exec" "-f" profile "bash" "-c" (str "cd " root " && " cmd)))]
      (sh "echo pwned > .samizdat/gates.edn; echo x > .samizdat/new.clj")
      (is (= "{}" (slurp (str prot "/gates.edn"))))
      (is (not (.exists (java.io.File. (str prot "/new.clj")))))
      (is (str/includes? (:out (sh "cat .samizdat/gates.edn && echo ok > f && cat f")) "ok")
          "readable, and the rest of the project still writable"))))
