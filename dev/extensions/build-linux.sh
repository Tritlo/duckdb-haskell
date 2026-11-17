#!/usr/bin/env bash
# Build the demo bundle on its Ubuntu 24.04 baseline.
set -euo pipefail
image=ubuntu@sha256:77eb101c607d6e3d4f5ad0c8a02c2136c689b98aba1f359e6502ba517e5c57ec
if [[ ${1-} == --container ]]; then
    export DEBIAN_FRONTEND=noninteractive GHC_ENVIRONMENT=-
    [[ $(uname -m) == x86_64 ]]
    [[ $(cat /sys/fs/cgroup/memory.max) == 12884901888 ]]
    [[ $(cat /sys/fs/cgroup/memory.swap.max) == 0 ]]
    [[ $(cat /sys/fs/cgroup/cpu.max) == '200000 100000' ]]
    mkdir -p /out
    cp /sys/fs/cgroup/{memory.max,memory.swap.max,cpu.max} /out/
    apt-get update
    apt-get install -y --no-install-recommends build-essential curl ca-certificates xz-utils \
        libgmp-dev libffi-dev libncurses-dev libnuma-dev python3 binutils patchelf pkg-config zlib1g-dev
    mkdir -p /opt/downloads /opt/ghc /opt/toolchain-licenses /usr/local/bin
    cd /opt/downloads
    curl -fL --retry 2 -O https://downloads.haskell.org/ghc/9.14.1/ghc-9.14.1-x86_64-ubuntu24_04-linux.tar.xz
    echo '530f0861d1d7b45476c158a9e68aa89987687aaf9939df7a15995c45726e2344  ghc-9.14.1-x86_64-ubuntu24_04-linux.tar.xz' | sha256sum -c
    tar -xf ghc-9.14.1-x86_64-ubuntu24_04-linux.tar.xz
    cd ghc-9.14.1-x86_64-unknown-linux
    # Read exact boot-package notices from the matching verified source release.
    curl -fL --retry 2 -o /opt/downloads/ghc-9.14.1-src.tar.xz https://downloads.haskell.org/ghc/9.14.1/ghc-9.14.1-src.tar.xz
    echo '2a83779c9af86554a3289f2787a38d6aa83d00d136aa9f920361dd693c101e77  /opt/downloads/ghc-9.14.1-src.tar.xz' | sha256sum -c
    python3 - <<'PYCODE'
from pathlib import Path
import shutil
import tarfile
with tarfile.open('/opt/downloads/ghc-9.14.1-src.tar.xz') as archive:
    for member in archive.getmembers():
        name = Path(member.name)
        if (member.isfile() or member.islnk() or member.issym()) and (name.name.upper().startswith(('LICENSE', 'COPYING')) or name.name.endswith(('.cabal', '.cabal.in'))):
            relative = name.relative_to('ghc-9.14.1')
            assert '..' not in relative.parts
            dest = Path('/opt/ghc-source-notices') / relative
            dest.parent.mkdir(parents=True, exist_ok=True)
            source = archive.extractfile(member)
            assert source is not None
            with source, dest.open('wb') as target:
                shutil.copyfileobj(source, target)
PYCODE
    ./configure --prefix=/opt/ghc
    make install
    export PATH=/opt/ghc/bin:$PATH
    cd /opt/downloads
    curl -fL --retry 2 -O https://downloads.haskell.org/cabal/cabal-install-3.18.1.0/cabal-install-3.18.1.0-x86_64-linux-unknown.tar.xz
    echo 'c6385c155ff61f792dfed0e8c101004db63f8d8b556dd6c9838f81a012bf28a6  cabal-install-3.18.1.0-x86_64-linux-unknown.tar.xz' | sha256sum -c
    tar -xf cabal-install-3.18.1.0-x86_64-linux-unknown.tar.xz cabal
    install cabal /usr/local/bin/cabal
    cd /work/source
    (cd duckdb-ffi && sha256sum -c generated.sha256)
    bash dev/fetch-duckdb.sh /work/native
    cabal update
    cd dev/extensions
    cabal build --builddir=/work/build --with-compiler=ghc --disable-documentation -j1 \
        duckdb-haskell-extension-example:flib:haskell_demo \
        duckdb-haskell-extension-example:test:haskell-demo-test
    library=$(cabal list-bin --builddir=/work/build --with-compiler=ghc duckdb-haskell-extension-example:flib:haskell_demo)
    test_binary=$(cabal list-bin --builddir=/work/build --with-compiler=ghc duckdb-haskell-extension-example:test:haskell-demo-test)
    cabal sdist all --output-directory=/work/local-sources
    export DUCKDB_CLI=/work/native/native/duckdb
    python3 bundle-linux.py "$library" /out/bundle
    mkdir -p /out/verification
    cp "$test_binary" /out/verification/haskell-demo-test
    cp "$DUCKDB_CLI" /out/verification/duckdb
    cp verify-linux.sh /out/verification/
    cp /work/build/cache/plan.json /out/build-plan.json
    exit
fi
if [[ $# != 1 || $1 != /* || -e $1 ]]; then
    echo 'Usage: build-linux.sh /absolute/new-output-directory' >&2
    exit 2
fi
repo=$(cd "$(dirname "$0")/../.." && pwd)
output=$1
docker_client=${DOCKER:-docker}
container=duckdb-haskell-bundle-build-$$
verify_container=duckdb-haskell-bundle-verify-$$
mkdir -p "$output"
cleanup() {
    status=$?
    if [[ $status == 0 ]]; then
        "$docker_client" rm -f "$container" "$verify_container" >/dev/null 2>&1 || true
    else
        printf 'Task containers retained for inspection. Cleanup: %q rm -f %q %q\n' "$docker_client" "$container" "$verify_container" >&2
    fi
}
trap cleanup EXIT
# Stream only source files. Generated source inputs must already match their hashes.
(cd "$repo/duckdb-ffi" && sha256sum -c generated.sha256)
"$docker_client" pull --platform linux/amd64 "$image"
tar -C "$repo" --exclude=.git --exclude=dist-newstyle --exclude='dist-newstyle-*' \
    --exclude=dev/extensions/dist -cf - . |
    "$docker_client" run --name "$container" --platform linux/amd64 -i \
        --memory=12g --memory-swap=12g --cpus=2 --pids-limit=512 \
        -e BUNDLE_BUILD_IMAGE="$image" "$image" bash -c \
        'mkdir -p /work/source; tar -xf - -C /work/source; exec bash /work/source/dev/extensions/build-linux.sh --container'
"$docker_client" cp "$container:/out/." - | tar -xf - -C "$output"
# The verification process receives no compiler, source, Cabal store, or build tree.
tar -C "$output" -cf - bundle verification |
    "$docker_client" run --name "$verify_container" --platform linux/amd64 -i \
        --memory=3g --memory-swap=3g --cpus=1 --pids-limit=256 \
        "$image" bash -c \
        'mkdir /verification; tar -xf - -C /verification; exec bash /verification/verification/verify-linux.sh'
"$docker_client" inspect "$container" "$verify_container" > "$output/container-config.json"
