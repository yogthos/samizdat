#!/bin/sh
# The mount half of the Linux confinement, measured with jolt under bwrap and
# WITHOUT the seccomp filter — what a host that cannot install seccomp (x86_64
# under Rosetta or qemu) can still verify. Run inside the samizdat-linux-sandbox
# container; exits non-zero on any escape.
set -eu
mkdir -p /root/.ssh /tmp/proj
echo KEY > /root/.ssh/id
echo asset > /tmp/proj/asset.txt
exec bwrap --ro-bind / / --dev /dev --proc /proc --unshare-pid --die-with-parent --new-session \
  --bind /tmp/proj /tmp/proj --tmpfs /root/.ssh --tmpfs /work --tmpfs /etc --chdir /tmp/proj -- \
  jolt -e '
(assert (= "asset" (clojure.string/trim (slurp "asset.txt"))) "project unreadable")
(spit "out.txt" "x")
(assert (= :refused (try (spit "/root/escape.txt" "x") :written (catch Throwable _ :refused))) "home writable")
(assert (= :hidden (try (slurp "/root/.ssh/id") :read (catch Throwable _ :hidden))) "secret readable")
(assert (= :hidden (try (slurp "/work/deps.edn") :read (catch Throwable _ :hidden))) "harness readable")
(println "mount half: project rw, home ro, secrets and harness hidden")'
