#!/bin/sh
# Run the sandbox escape battery on a real Linux kernel. Needs Docker; the
# container is privileged because bubblewrap needs user namespaces and the
# seccomp filter needs to be installable. Exits non-zero if any check fails.
set -eu
cd "$(dirname "$0")/../.."
docker build --platform linux/amd64 -f dev/linux-sandbox/Dockerfile -t samizdat-linux-sandbox .
docker run --rm --privileged --platform linux/amd64 samizdat-linux-sandbox \
  jolt -A:test -e '(require (quote clojure.test) (quote samizdat.sandbox-test) (quote samizdat.image-test))
                   (let [r (clojure.test/run-tests (quote samizdat.sandbox-test) (quote samizdat.image-test))]
                     (println r)
                     (System/exit (if (zero? (+ (:fail r) (:error r))) 0 1)))'
