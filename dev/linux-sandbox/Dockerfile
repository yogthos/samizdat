# The Linux sandbox backend (samizdat.security.sandbox, karamazov-zrq.8),
# verified on a real kernel: bubblewrap + seccomp on Ubuntu 24.04, run by
# dev/linux-sandbox/verify.sh under a privileged container. jolt ships only an
# x86_64 Linux build, so on Apple Silicon this image runs under Rosetta or
# qemu — which is the case the dual-arch seccomp filter exists for. The
# platform is pinned by verify.sh's build command, not here.
FROM ubuntu:24.04
ARG JOLT_VERSION=0.8.0
RUN apt-get update \
 && apt-get install -y --no-install-recommends \
      bubblewrap ca-certificates curl git libsqlite3-0 unzip \
 && rm -rf /var/lib/apt/lists/*
RUN curl -fsSL "https://github.com/jolt-lang/jolt/releases/download/v${JOLT_VERSION}/jolt-v${JOLT_VERSION}-x86_64-linux.tar.gz" \
      | tar -xz -C /opt \
 && ln -s "$(find /opt -maxdepth 3 -type f -name jolt | head -1)" /usr/local/bin/jolt \
 && jolt --version
WORKDIR /work
# Dependencies first, so a source edit does not refetch them.
COPY deps.edn /work/
RUN jolt -A:test -e nil || true
COPY . /work
