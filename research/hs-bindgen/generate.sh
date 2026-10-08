#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")"
hs-bindgen-cli preprocess \
  --unique-id duckdb.haskell.spike \
  --module DuckDB \
  --hs-output-dir generated \
  --create-output-dirs \
  --overwrite-files \
  --single-file --safe '' \
  --omit-field-prefixes \
  -I ../../duckdb-ffi/cbits \
  duckdb.h duckdb_arrow.h
