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

digest=$(sha256sum vendor/duckdb-api.tar.gz)
if [[ ${digest%% *} != "$(cat vendor/duckdb-api.sha256)" ]]; then
  echo "Bundled DuckDB API archive checksum mismatch." >&2
  exit 1
fi
tar -xzf vendor/duckdb-api.tar.gz -C "$scratch_dir" duckdb-2.0/duckdb.h duckdb-2.0/duckdb_v2.h

if [[ $(ghc-pkg field hs-bindgen version --simple-output) != "1.0.0.0" ]]; then
  echo "Use hs-bindgen 1.0.0.0 from dev/nix/generate.nix." >&2
  exit 1
fi

bindgen_unit_id=$(ghc-pkg field z-hs-bindgen-z-internal id --simple-output)
bindgen_public_unit_id=$(ghc-pkg field hs-bindgen id --simple-output)
# Expose both components explicitly. Otherwise GHC hides one of them.
ghc -hide-all-packages -package base -package aeson -package bytestring \
  -package containers -package data-default -package directory -package filepath \
  -package text \
  -package-id "$bindgen_public_unit_id" -package-id "$bindgen_unit_id" -Wall -Werror \
  -outputdir "$scratch_dir/build" -o "$scratch_dir/generate" \
  "$script_dir/GenerateBindings.hs"

"$scratch_dir/generate" "$scratch_dir"
outputs=(generated/Database/DuckDB/FFI.hs generated/Database/DuckDB/FFI/V2.hs generated/abi-checks.c generated/abi-checks-v2.c)
if "$check"; then
  (cd -- "$scratch_dir" && sha256sum --check "$package_dir/generated.sha256")
fi
mkdir -p generated/Database/DuckDB/FFI
for output in "${outputs[@]}"; do
  cp -- "$scratch_dir/$output" "$output"
done
if ! "$check"; then
  sha256sum "${outputs[@]}" > generated.sha256
fi
