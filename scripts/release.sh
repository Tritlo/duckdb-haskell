#!/bin/bash
set -euo pipefail

usage() {
  echo "Usage: $0 [--publish] <package>"
  echo ""
  echo "Packages: duckdb-ffi, duckdb-simple, all"
  echo ""
  echo "Without --publish, uploads as a package candidate (dry run)."
  echo "With --publish, uploads as a published release."
  exit 1
}

publish_flag=""
package=""
for arg in "$@"; do
  case "$arg" in
    --publish) publish_flag="--publish" ;;
    -h|--help) usage ;;
    *) package="$arg" ;;
  esac
done

if [ -z "$package" ]; then
  usage
fi

case "$package" in
  duckdb-ffi|duckdb-simple|all) ;;
  *) echo "Error: unknown package '$package'. Must be duckdb-ffi, duckdb-simple, or all." ; exit 1 ;;
esac

read -p "Username: " username
read -sp "Password: " password
echo ""

upload_package() {
  local name="$1"
  echo ""
  echo "=== Releasing $name ==="

  # Use [0-9] so package prefixes do not cross-match.
  rm -rf dist-newstyle/"$name"-[0-9]*-docs.tar.gz
  rm -rf dist-newstyle/sdist/"$name"-[0-9]*.tar.gz
  cabal haddock --haddock-for-hackage "$name"
  cabal sdist "$name"

  cabal upload $publish_flag -u "$username" -p "$password" dist-newstyle/sdist/"$name"-[0-9]*.tar.gz
  cabal upload $publish_flag -d -u "$username" -p "$password" dist-newstyle/"$name"-[0-9]*-docs.tar.gz

  echo "=== Done: $name ==="
}

case "$package" in
  duckdb-ffi)
    upload_package duckdb-ffi
    ;;
  duckdb-simple)
    upload_package duckdb-simple
    ;;
  all)
    upload_package duckdb-ffi
    upload_package duckdb-simple
    ;;
esac
