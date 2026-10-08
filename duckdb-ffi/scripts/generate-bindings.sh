#!/usr/bin/env bash
# Generate release sources in dev/nix/generate.nix. Consumers use the sources.
set -euo pipefail

case "${1:-}" in
  "") check=false ;;
  --check) check=true ;;
  *) echo "Usage: $0 [--check]" >&2; exit 2 ;;
esac

script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
package_dir=$(cd -- "$script_dir/.." && pwd)
scratch_dir=$(mktemp -d)
trap 'rm -rf -- "$scratch_dir"' EXIT
cd -- "$package_dir"

if [[ $(hs-bindgen-cli --version) != *"1.0.0.0"* ]]; then
  echo "Use hs-bindgen 1.0.0.0 from dev/nix/generate.nix." >&2
  exit 1
fi

generate() {
  local output=$1
  shift
  hs-bindgen-cli -v 0 preprocess \
    -I cbits --module=Database.DuckDB.FFI --unique-id=duckdb-ffi \
    --omit-field-prefixes --single-file --safe='' \
    '--select-except-by-decl-name=^macro DUCKDB_API_VERSION_(AT_LEAST|BELOW)$' \
    --hs-output-dir="$scratch_dir/$output" --create-output-dirs \
    --gen-binding-spec="$scratch_dir/$output.json" \
    "$@" duckdb.h duckdb_arrow.h
}

generate initial --clang-option=--target=x86_64-unknown-linux-gnu --clang-option=-ffreestanding
python3 "$script_dir/binding-metadata.py" opaque cbits/duckdb.h \
  "$scratch_dir/initial.json" "$scratch_dir/opaque.json"

for target in x86_64-unknown-linux-gnu aarch64-unknown-linux-gnu \
              x86_64-apple-macos10.13 arm64-apple-macos11; do
  generate "$target" --prescriptive-binding-spec="$scratch_dir/opaque.json" \
    --clang-option="--target=$target" --clang-option=-ffreestanding
done
generate native --prescriptive-binding-spec="$scratch_dir/opaque.json"

generated="$scratch_dir/x86_64-unknown-linux-gnu/Database/DuckDB/FFI.hs"
for output in "$scratch_dir"/*/Database/DuckDB/FFI.hs; do
  [[ $output == "$scratch_dir/initial/Database/DuckDB/FFI.hs" ]] && continue
  cmp -- "$generated" "$output"
done
python3 "$script_dir/binding-metadata.py" abi "$generated" \
  "$scratch_dir/x86_64-unknown-linux-gnu.json" "$scratch_dir/abi-checks.c" cbits
python3 "$script_dir/binding-metadata.py" compat "$generated" \
  "$scratch_dir/x86_64-unknown-linux-gnu.json" "$scratch_dir/Compat.hs" \
  "$script_dir/legacy-names.json"

if "$check"; then
  cmp -- "$generated" src/Database/DuckDB/FFI.hs
  cmp -- "$scratch_dir/abi-checks.c" cbits/abi-checks.c
  cmp -- "$scratch_dir/Compat.hs" src/Database/DuckDB/FFI/Compat.hs
else
  cp -- "$generated" src/Database/DuckDB/FFI.hs
  cp -- "$scratch_dir/abi-checks.c" cbits/abi-checks.c
  mkdir -p src/Database/DuckDB/FFI
  cp -- "$scratch_dir/Compat.hs" src/Database/DuckDB/FFI/Compat.hs
fi
