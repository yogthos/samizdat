#!/bin/sh
# Run the sandbox escape battery on a real Linux kernel. Needs Docker; the
# container is privileged because bubblewrap needs user namespaces and the
# seccomp filter needs to be installable. Exits non-zero if any check fails.
#
# ON APPLE SILICON THIS IS ONLY HALF A VERIFICATION. jolt ships no aarch64
# Linux build, so the image runs x86_64 under Rosetta (or qemu), and neither
# can install a seccomp filter: prctl(PR_SET_SECCOMP) answers EINVAL even for
# an allow-everything program. The script detects that and runs what the host
# CAN measure — the pure parts and the mount half — then exits 2 saying so.
# The full battery, jolt under the filter, needs an x86_64 Linux kernel.
set -eu
cd "$(dirname "$0")/../.."
docker build --platform linux/amd64 -f dev/linux-sandbox/Dockerfile -t samizdat-linux-sandbox .

run() { docker run --rm --privileged --platform linux/amd64 samizdat-linux-sandbox "$@"; }

if run sh -c 'printf "\006\000\000\000\000\000\377\177" > /tmp/allow.bpf; sh -c "exec bwrap \"\$@\" 3<\"\$0\"" /tmp/allow.bpf --ro-bind / / --dev /dev --proc /proc --seccomp 3 -- /bin/true' >/dev/null 2>&1
then
  run jolt -A:test -e '(require (quote clojure.test) (quote samizdat.sandbox-test) (quote samizdat.image-test))
                       (let [r (clojure.test/run-tests (quote samizdat.sandbox-test) (quote samizdat.image-test))]
                         (println r)
                         (System/exit (if (zero? (+ (:fail r) (:error r))) 0 1)))'
else
  echo "seccomp cannot be installed in this container (x86_64 under emulation); running the pure parts and the mount half only" >&2
  run jolt -A:test -e '(require (quote clojure.test) (quote samizdat.sandbox-test))
                       (let [r (clojure.test/run-tests (quote samizdat.sandbox-test))]
                         (println r)
                         (System/exit (if (zero? (+ (:fail r) (:error r))) 0 1)))'
  run sh /work/dev/linux-sandbox/mount-half.sh
  echo "the seccomp half — jolt serving nREPL under the no-subprocess filter — still needs an x86_64 Linux kernel" >&2
  exit 2
fi
