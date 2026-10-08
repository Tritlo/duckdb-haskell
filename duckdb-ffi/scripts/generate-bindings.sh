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

if [[ $(ghc-pkg field hs-bindgen version --simple-output) != "1.0.0.0" ]]; then
  echo "Use hs-bindgen 1.0.0.0 from dev/nix/generate.nix." >&2
  exit 1
fi

bindgen_unit_id=$(ghc-pkg field z-hs-bindgen-z-internal id --simple-output)
bindgen_public_unit_id=$(ghc-pkg field hs-bindgen id --simple-output)
# Expose both components explicitly. Otherwise GHC hides one of them.
ghc -hide-all-packages -package base -package data-default \
  -package-id "$bindgen_public_unit_id" -package-id "$bindgen_unit_id" -Wall -Werror \
  -outputdir "$scratch_dir/build" -o "$scratch_dir/generate" \
  "$script_dir/GenerateBindings.hs"

generate() {
  local output=$1
  local target=$2
  local specification=${3:--}
  "$scratch_dir/generate" "$target" "$scratch_dir/$output" \
    "$scratch_dir/$output.json" "$specification"
}

generate initial x86_64-unknown-linux-gnu
python3 "$script_dir/binding-metadata.py" spec cbits/duckdb.h \
  "$scratch_dir/initial.json" "$scratch_dir/types.json"

for target in x86_64-unknown-linux-gnu aarch64-unknown-linux-gnu \
              x86_64-apple-macos10.13 arm64-apple-macos11; do
  generate "$target" "$target" "$scratch_dir/types.json"
done
generate native native "$scratch_dir/types.json"

generated="$scratch_dir/x86_64-unknown-linux-gnu/Database/DuckDB/FFI.hs"
for output in "$scratch_dir"/*/Database/DuckDB/FFI.hs; do
  [[ $output == "$scratch_dir/initial/Database/DuckDB/FFI.hs" ]] && continue
  cmp -- "$generated" "$output"
done
python3 "$script_dir/binding-metadata.py" abi "$generated" \
  "$scratch_dir/x86_64-unknown-linux-gnu.json" "$scratch_dir/abi-checks.c" cbits
if "$check"; then
  cmp -- "$generated" src/Database/DuckDB/FFI.hs
  cmp -- "$scratch_dir/abi-checks.c" cbits/abi-checks.c
else
  cp -- "$generated" src/Database/DuckDB/FFI.hs
  cp -- "$scratch_dir/abi-checks.c" cbits/abi-checks.c
fi
