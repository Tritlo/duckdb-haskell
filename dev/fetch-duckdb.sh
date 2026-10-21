#!/usr/bin/env bash
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
version=2.0.0
case "$(uname -s):$(uname -m)" in
  Linux:x86_64)
    platform=linux-amd64
    library_sha=993ad89e7dfca66cd6da994c2cad090c2fc160ddf9f550498a098ab4bb13cfb1
    cli_sha=c6507206baf7793c6013dbd82368469a45f20dc0e17a6f92f204602f652ed2bc
    ;;
  Linux:aarch64|Linux:arm64)
    platform=linux-arm64
    library_sha=c60eecf91143ca6bbac55c4f4d40dc22520488b0ac257c86e0fe9a2b451cf2b7
    cli_sha=859e1875d2a9515fbb4bf8871061437a14af82320da68c753f094c4b776348fe
    ;;
  Darwin:x86_64|Darwin:arm64)
    platform=osx-universal
    library_sha=14cd63180614cfd961f7959146ecd9087527ff20b67311efdc3fa1c29c18204d
    cli_sha=8a8f67e7c7f7d1392fa63ba3971948391392fe2a25fca63e7008e33c50beb4c2
    ;;
  *) echo "Supported platforms: glibc Linux amd64/arm64 and macOS." >&2; exit 2 ;;
esac

mkdir -p "$output_dir/native"
fetch() {
  local name=$1 expected=$2 archive="$output_dir/$1" digest
  if [ ! -f "$archive" ]; then
    curl --fail --location --proto '=https' --proto-redir '=https' --retry 2 \
      --output "$archive.tmp" "https://github.com/duckdb/duckdb/releases/download/v$version/$name"
    mv "$archive.tmp" "$archive"
  fi
  if command -v sha256sum >/dev/null; then
    digest=$(sha256sum "$archive")
  else
    digest=$(shasum -a 256 "$archive")
  fi
  if [ "${digest%% *}" != "$expected" ]; then
    echo "DuckDB archive checksum mismatch: $archive" >&2
    exit 1
  fi
  tar -xzf "$archive" -C "$output_dir/native"
}

fetch "duckdb-shared-libs-$platform.tar.gz" "$library_sha"
fetch "duckdb-cli-$platform.tar.gz" "$cli_sha"
python3 "$repo_dir/dev/bindings/duckdb-api.py" --compare-headers "$output_dir/native"
cp "$repo_dir/duckdb-ffi/cbits/duckdb_arrow.h" "$repo_dir/duckdb-ffi/cbits/duckdb-LICENSE" "$output_dir/native/"
test "$("$output_dir/native/duckdb" -csv -noheader -c 'SELECT version();')" = "v$version"
