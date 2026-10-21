# Contributing

## Project structure

`duckdb-ffi/generated` contains the generated C API bindings and ABI checks.
The client headers and upstream license are in `duckdb-ffi/cbits`. Their source
commit and SHA256 digests are in `duckdb-ffi/vendor/duckdb-api.json`.
The generation driver and header audit are in `dev/bindings`. The tests are in
`duckdb-ffi/test`.
`duckdb-simple/src` contains the higher-level API. Its integration and property
tests are in `duckdb-simple/test`, and its leak test is in `duckdb-simple/leaktest`.

The pure geometry types and codecs are maintained in
[geometry-simple](https://github.com/Tritlo/geometry-simple).

## Testing guidelines

Test new behavior through the database where possible. Add regressions for
bug fixes. Run the full test suite before submitting a change.

Generate the local binding artefacts before building a checkout:

```sh
nix-shell dev/nix/generate.nix --run 'dev/bindings/generate-bindings.sh --check'
```

The ignored `duckdb-ffi/generated/` directory contains the Haskell modules and
C ABI assertions. Keep it outside Git. The checksum file and generator are
tracked. Run the generator without `--check` after an intentional input change.
Review the new checksums and run the coverage audit.

`dev/release.sh` generates and verifies both APIs before it builds the
Hackage source archives and documentation. It then uploads the selected
packages. The source archives contain the generated Haskell modules and C ABI
checks. Package users do not need Nix, LLVM, or hs-bindgen. The release script
uploads candidates by default. Use `--publish` for an authorized release.

Build all components before running the tests. With Cabal 3.16 and 3.18,
`cabal test all` on a clean tree can skip building the tests for a package
with a custom setup.

```sh
cabal build all
cabal test all --test-show-details=direct
cabal run duckdb-simple-leak-test -- all 100
```

The last command checks 10,000 query cycles, 10,000 callback cycles, 10,000
VARIANT/GEOMETRY cycles, and 7,000 cancellation attempts.
Each phase keeps one connection open.
The normal leak test uses one tenth of those counts. Native RSS and thread
checks require Linux `/proc`.
Callback collection and functional checks also run on other systems.

The Linux CI job with GHC 9.12.4 also runs the open/close, callback,
cancellation, VARIANT/GEOMETRY, and DataFrame checks under Valgrind. Definite
and indirect native leaks fail the job. That job sets `DUCKDB_LEAK_RSS_CHECK=0`
because Valgrind changes process RSS. The ordinary leak test keeps the RSS
assertion enabled.

Use `--with-compiler=ghc-VERSION` to select a supported compiler.
For benchmarks, build both versions with the same compiler and DuckDB library.
For example:

```sh
cabal bench duckdb-simple-bench --benchmark-options='fold 100000'
```

Nix CI builds and tests both packages with GHC 9.14.1 and Nix's DuckDB library.
Run the same build locally with `nix-build dev/nix/ci.nix --no-out-link`.

The separate DataFrame integration suite checks Arrow export with
`dataframe-arrow-bridge`. It runs in the optimized Cabal CI jobs on Linux and
macOS. Enable it locally with:

```sh
cabal build duckdb-simple-dataframe-test -fdataframe-tests
cabal test duckdb-simple-dataframe-test -fdataframe-tests --test-show-details=direct
```

The flag defaults to off because the bridge also depends on DataFrame's
file-format packages. These packages are absent from the pinned Nix package
set. The ordinary suite checks Arrow ownership without those dependencies.

## Code style

Format Haskell code with `fourmolu` before committing.
Format Cabal files with `cabal-gild`. Keep names consistent with the surrounding
module.

Build parameter values with the V2 handle of the statement's connection.
Use the managed type cache for SQL-derived VARIANT and configured GEOMETRY
types. Run metadata queries on its helper connection so binding and decoding
do not interrupt an active stream. Use public V2 operations and SQL metadata.
Do not read private payload layouts or convert handles between the two APIs.

## Native library configuration

See [docs/duckdb-2.0.md](docs/duckdb-2.0.md) for DuckDB 2.0 development.
Supply the matching native library. The package versions target DuckDB 2.0.
Check the header ABI before you run the suites. The source-build workflow
builds the native library on Linux and macOS from the pinned header commit.
The optional `quack-tests` suite requires matching local `httpfs` and Quack
extensions. See the same document for its commands and native limitations.

The bindings target DuckDB 2.0.0. The automatic installer verifies the final
native archive checksums. Set
`--configure-option=--duckdb-install-dir=/absolute/path` to select another
installation directory. Enable the `systemlib` Cabal flag to use the system
library. Set `extra-lib-dirs` to an absolute path to select
a library in another directory. See the `cabal.project` example in the README.
Nix builds use the library supplied by Nix and do not download it.

CI tests the pinned 2.0.0 release with all supported compilers on Linux and
GHC 9.14.1 on macOS. V2 is unstable and requires the native source pin in
`duckdb-ffi/vendor/duckdb-api.json`. Set
`DUCKDB_TEST_VERSION` to the exact expected version, such as `2.0.0`, to detect
loader configuration errors. The version string alone does not identify a
development snapshot. Check its source ID too. Run the full suite after each
native pin update.

CI also builds and tests with `--enable-optimization=0` on GHC 9.14.1 and
DuckDB 2.0. Floating-point conversion must preserve special values without
compiler rewrite rules.

## Documentation

Document exported functions and types with Haddock comments.
Build the documentation with `cabal haddock all`.
