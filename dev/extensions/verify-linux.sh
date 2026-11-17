#!/usr/bin/env bash
# Verify the bundle without the original build tree or toolchain.
set -euo pipefail
export DEBIAN_FRONTEND=noninteractive
[[ $(uname -m) == x86_64 ]]
[[ $(cat /sys/fs/cgroup/memory.max) == 3221225472 ]]
[[ $(cat /sys/fs/cgroup/memory.swap.max) == 0 ]]
[[ $(cat /sys/fs/cgroup/cpu.max) == '100000 100000' ]]
[[ ! -e /work && ! -e /opt/ghc && ! -e /root/.cabal && ! -e /nix ]]
# These packages run the verification tools and the native CLI. No compiler is installed.
apt-get update
apt-get install -y --no-install-recommends python3 binutils libstdc++6 libgmp10 libffi8 libnuma1 libdw1t64
mv /verification/bundle '/tmp/relocated extension bundle'
export DUCKDB_CLI=/verification/verification/duckdb
export DUCKDB_HASKELL_DEMO_EXTENSION='/tmp/relocated extension bundle/haskell_demo.duckdb_extension'
unset LD_LIBRARY_PATH LD_PRELOAD GHCRTS
python3 - <<'PY'
import hashlib
import json
import os
from pathlib import Path
import re
import subprocess

root = Path(os.environ['DUCKDB_HASKELL_DEMO_EXTENSION']).parent
manifest = json.loads((root / 'manifest.json').read_text())
assert manifest['ghc'] == '9.14.1'
assert manifest['cabal'] == '3.18.1.0'
for item in manifest['objects']:
    path = root / item['path']
    assert hashlib.sha256(path.read_bytes()).hexdigest() == item['sha256'], path
    dynamic = subprocess.check_output(['readelf', '-d', str(path)], text=True)
    paths = re.findall(r'\((?:RPATH|RUNPATH)\).*\[(.*?)\]', dynamic)
    assert paths == ['$ORIGIN/lib' if path.name.endswith('.duckdb_extension') else '$ORIGIN'], (path, paths)
    listing = subprocess.check_output(['ldd', str(path)], text=True)
    assert 'not found' not in listing, listing
    assert not any(x in listing for x in ['/work/', '/opt/ghc/', '/root/.cabal/', '/nix/store/']), listing
    for resolved in re.findall(r'=> (/.+?) \(0x', listing):
        p = Path(resolved)
        assert str(p).startswith(str(root)) or p.name in manifest['host_glibc'], (path, p)
for name, expected in manifest['license_sha256'].items():
    assert hashlib.sha256((root / name).read_bytes()).hexdigest() == expected, name
# Change only metadata fields in separate negative fixtures. Keep the entrypoint name.
data = (root / 'haskell_demo.duckdb_extension').read_bytes()
for kind, offset, value, diagnostic in [('version', -352, b'v0.0.0', "The file was built specifically for DuckDB version 'v0.0.0'"), ('platform', -320, b'osx_arm64', "The file was built for the platform 'osx_arm64', but we can only load extensions built for platform")]:
    dest = Path('/tmp') / ('bad-' + kind)
    dest.mkdir()
    (dest / 'lib').symlink_to(root / 'lib', target_is_directory=True)
    altered = bytearray(data)
    altered[len(data) + offset:len(data) + offset + 32] = value.ljust(32, b'\0')
    target = dest / 'haskell_demo.duckdb_extension'
    target.write_bytes(altered)
    result = subprocess.run([os.environ['DUCKDB_CLI'], '-unsigned', '-c', f"LOAD '{target}';"], text=True, capture_output=True)
    assert result.returncode != 0 and diagnostic in (result.stdout + result.stderr), result
print('Bundle hashes, dependency paths, relocation, and footer rejection checks passed.')
PY
/verification/verification/haskell-demo-test --num-threads=1
