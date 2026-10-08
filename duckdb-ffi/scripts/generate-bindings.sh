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
ghc -hide-all-packages -package base -package aeson -package bytestring \
  -package containers -package data-default -package directory -package filepath \
  -package text \
  -package-id "$bindgen_public_unit_id" -package-id "$bindgen_unit_id" -Wall -Werror \
  -outputdir "$scratch_dir/build" -o "$scratch_dir/generate" \
  "$script_dir/GenerateBindings.hs"

"$scratch_dir/generate" "$scratch_dir"
if "$check"; then
  sha256sum --check generated.sha256
fi
