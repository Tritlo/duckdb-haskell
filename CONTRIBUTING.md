# Contributing

## Project structure

`duckdb-ffi/src` contains the C API bindings. The C wrappers are in
`duckdb-ffi/cbits`, and the tests are in `duckdb-ffi/test`.
`duckdb-simple/src` contains the higher-level API. Its integration and property
tests are in `duckdb-simple/test`, and its leak test is in `duckdb-simple/leaktest`.

## Testing guidelines

Test new behavior through the database where possible. Add regressions for
bug fixes. Run the full test suite before submitting a change.

Build all components before running the tests. With Cabal 3.16 and 3.18,
`cabal test all` on a clean tree can skip building the tests for a package
with a custom setup.

```sh
cabal build all
cabal test all --test-show-details=direct
```

Use `--with-compiler=ghc-VERSION` to select a supported compiler.
Set `DUCKDB_TEST_VERSION` to the expected native version, such as `1.5.3`,
to check which library the test executable loads.

Nix CI builds and tests both packages with GHC 9.14.1 and Nix's DuckDB library.
Run the same build locally with `nix-build dev/nix/ci.nix --no-out-link`.

## Code style

Format Haskell code with `fourmolu` before committing.
Format Cabal files with `cabal-gild`. Keep names consistent with the surrounding
module.

## Native library configuration

The bindings support DuckDB >= 1.5.3 and < 1.6. Cabal downloads DuckDB 1.5.3
to the user cache on glibc Linux and macOS. Set
`--configure-option=--duckdb-install-dir=/absolute/path` to select another
installation directory. Enable the `systemlib` Cabal flag to use the system
library. Set `extra-lib-dirs` to an absolute path to select
a library in another directory. See the `cabal.project` example in the README.
Nix builds use the library supplied by Nix and do not download it.

## Documentation

Document exported functions and types with Haddock comments.
Build the documentation with `cabal haddock all`.
