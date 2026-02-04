#!/usr/bin/env bash

set -euo pipefail

repo_root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
log_dir="$repo_root/ci"
mkdir -p "$log_dir"

if [[ $# -gt 0 ]]; then
  ghc_versions=("$@")
else
  ghc_versions=(
    "9.6.7"
    "9.8.4"
    "9.10.3"
    "9.12.2"
    "9.14.1"
  )
fi

status=0

for ghc_version in "${ghc_versions[@]}"; do
  log_file="$log_dir/${ghc_version}.log"
  printf '=== Building and testing with GHC %s ===\n' "$ghc_version" | tee "$log_file"

  if docker build \
      --progress=plain \
      --build-arg GHC_VERSION="$ghc_version" \
      --build-arg UID="$(id -u)" \
      --build-arg GID="$(id -g)" \
      --tag "duckdb-ghc-$ghc_version" \
      "$repo_root" 2>&1 | tee -a "$log_file"; then
    printf '=== Success for GHC %s ===\n' "$ghc_version" | tee -a "$log_file"
  else
    printf '=== Failure for GHC %s ===\n' "$ghc_version" | tee -a "$log_file"
    status=1
  fi
done

exit $status
