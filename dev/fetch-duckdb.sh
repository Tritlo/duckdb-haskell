#!/usr/bin/env bash
# Download the pinned DuckDB release library and check it against
# duckdb-ffi/native.sha256 and the vendored client headers.
set -euo pipefail

if [ "$#" -ne 1 ]; then
  echo "Usage: $0 /absolute/output-directory" >&2
  exit 2
fi
case "$1" in
  /*) ;;
  *) echo "Use an absolute output directory." >&2; exit 2 ;;
esac

repo_dir=$(cd "$(dirname "$0")/.." && pwd)
output_dir=$1
case "$(uname -s):$(uname -m)" in
  Linux:x86_64) platform=linux-amd64 ;;
  Linux:aarch64|Linux:arm64) platform=linux-arm64 ;;
  Darwin:x86_64|Darwin:arm64) platform=osx-universal ;;
  *) echo "Supported platforms: glibc Linux amd64/arm64 and macOS." >&2; exit 2 ;;
esac
read -r expected release < <(grep "/duckdb-shared-libs-$platform.tar.gz\$" "$repo_dir/duckdb-ffi/native.sha256")

archive="$output_dir/$(basename "$release")"
mkdir -p "$output_dir/native"
if [ ! -f "$archive" ]; then
  curl --fail --location --proto '=https' --proto-redir '=https' --retry 2 \
    --output "$archive.tmp" "https://github.com/duckdb/duckdb/releases/download/$release"
  mv "$archive.tmp" "$archive"
fi
if command -v sha256sum >/dev/null; then
  digest=$(sha256sum < "$archive")
else
  digest=$(shasum -a 256 < "$archive")
fi
if [ "${digest%% *}" != "$expected" ]; then
  echo "DuckDB archive checksum mismatch: $archive" >&2
  exit 1
fi
tar -xzf "$archive" -C "$output_dir/native"
for header in duckdb.h duckdb_v2.h; do
  cmp "$repo_dir/duckdb-ffi/cbits/$header" "$output_dir/native/$header"
done
cp "$repo_dir/duckdb-ffi/cbits/duckdb_arrow.h" "$repo_dir/duckdb-ffi/cbits/duckdb-LICENSE" "$output_dir/native/"
