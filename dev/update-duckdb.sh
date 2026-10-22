#!/usr/bin/env bash
# Pin a DuckDB release tag. Copy its client headers and license into
# duckdb-ffi/cbits/. Record the checksums of these files, the source archive,
# and the release library archives. Review the diff after a run.
set -euo pipefail

if [ "$#" -ne 1 ]; then
  echo "Usage: $0 TAG (for example v2.0.0)" >&2
  exit 2
fi

tag=$1
upstream=https://github.com/duckdb/duckdb
package_dir=$(cd "$(dirname "$0")/../duckdb-ffi" && pwd)
platforms=(linux-amd64 linux-arm64 osx-universal)

# Use the commit of an annotated tag, not the tag object.
commit=$(git ls-remote "$upstream" "refs/tags/$tag^{}" "refs/tags/$tag" | sort -k2 -r | head -n 1 | cut -f1)
if ! [[ $commit =~ ^[0-9a-f]{40}$ ]]; then
  echo "Unknown release tag: $tag" >&2
  exit 1
fi

sha256() {
  if command -v sha256sum >/dev/null; then sha256sum "$@"; else shasum -a 256 "$@"; fi
}
fetch() {
  curl --fail --location --proto '=https' --proto-redir '=https' --retry 2 \
    --silent --show-error --output "$2" "$1"
}

scratch=$(mktemp -d)
trap 'rm -rf -- "$scratch"' EXIT

# Download every file before the pin changes.
raw=https://raw.githubusercontent.com/duckdb/duckdb/$commit
fetch "$raw/src/include/duckdb.h" "$scratch/duckdb.h"
fetch "$raw/src/include/duckdb_v2.h" "$scratch/duckdb_v2.h"
fetch "$raw/LICENSE" "$scratch/duckdb-LICENSE"
fetch "https://codeload.github.com/duckdb/duckdb/tar.gz/$commit" "$scratch/source.tar.gz"
for platform in "${platforms[@]}"; do
  fetch "$upstream/releases/download/$tag/duckdb-shared-libs-$platform.tar.gz" \
    "$scratch/duckdb-shared-libs-$platform.tar.gz"
done

cp "$scratch/duckdb.h" "$scratch/duckdb_v2.h" "$scratch/duckdb-LICENSE" "$package_dir/cbits/"
(cd "$package_dir" && sha256 cbits/duckdb.h cbits/duckdb_v2.h cbits/duckdb-LICENSE > vendor/upstream.sha256)
for platform in "${platforms[@]}"; do
  digest=$(sha256 < "$scratch/duckdb-shared-libs-$platform.tar.gz")
  printf '%s  %s\n' "${digest%% *}" "$tag/duckdb-shared-libs-$platform.tar.gz"
done > "$package_dir/native.sha256"
source_digest=$(sha256 < "$scratch/source.tar.gz")
cat > "$package_dir/vendor/duckdb-api.json" <<JSON
{
  "commit": "$commit",
  "ref": "$tag",
  "repository": "$upstream",
  "source_archive": {
    "sha256": "${source_digest%% *}",
    "url": "https://codeload.github.com/duckdb/duckdb/tar.gz/$commit"
  }
}
JSON
echo "Pinned DuckDB $tag at $commit."
