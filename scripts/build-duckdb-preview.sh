#!/usr/bin/env bash
set -euo pipefail

if [ "$#" -ne 1 ]; then
  echo "Usage: $0 /absolute/build-directory" >&2
  exit 2
fi
case "$1" in
  /*) ;;
  *) echo "Use an absolute build directory." >&2; exit 2 ;;
esac

repo_dir=$(cd "$(dirname "$0")/.." && pwd)
build_dir=$1
mkdir -p "$build_dir"
source_info=$(python3 - "$repo_dir/duckdb-ffi/vendor/duckdb-api.json" <<'PY'
import json
import sys
with open(sys.argv[1]) as handle:
    manifest = json.load(handle)
print(manifest["commit"])
print(manifest["source_archive"]["url"])
print(manifest["source_archive"]["sha256"])
PY
)
{
  IFS= read -r commit
  IFS= read -r source_url
  IFS= read -r source_digest
} <<< "$source_info"
archive="$build_dir/source.tar.gz"
if [ ! -f "$archive" ]; then
  curl --fail --location --proto '=https' --proto-redir '=https' --retry 2 \
    --output "$archive.tmp" "$source_url"
  mv "$archive.tmp" "$archive"
fi
if command -v sha256sum >/dev/null; then
  digest=$(sha256sum "$archive")
else
  digest=$(shasum -a 256 "$archive")
fi
if [ "${digest%% *}" != "$source_digest" ]; then
  echo "DuckDB source checksum mismatch: $archive" >&2
  exit 1
fi
source_dir="$build_dir/duckdb-$commit"
if [ ! -d "$source_dir" ]; then
  tar -xzf "$archive" -C "$build_dir"
fi
python3 "$repo_dir/scripts/duckdb-api.py" --compare-headers "$source_dir/src/include"
cmake -S "$source_dir" -B "$build_dir/build" \
  -DCMAKE_BUILD_TYPE=Release -DBUILD_UNITTESTS=OFF -DBUILD_SHELL=OFF \
  '-DBUILD_EXTENSIONS=icu;json;parquet' \
  -DDUCKDB_EXPLICIT_VERSION=v2.0.0-dev0 -DGIT_COMMIT_HASH="$commit"
cmake --build "$build_dir/build" --target duckdb --parallel "${DUCKDB_BUILD_JOBS:-2}"
mkdir -p "$build_dir/native"
cp "$source_dir"/src/include/duckdb*.h "$build_dir/native/"
case "$(uname -s)" in
  Darwin) cp "$build_dir/build/src/libduckdb.dylib" "$build_dir/native/" ;;
  *) cp "$build_dir/build/src/libduckdb.so" "$build_dir/native/" ;;
esac
echo "Native library: $build_dir/native"
