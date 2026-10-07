# Contributing

## Project structure

`duckdb-ffi/src` contains the C API bindings. The C wrappers are in
`duckdb-ffi/cbits`, and the tests are in `duckdb-ffi/test`.
`duckdb-simple/src` contains the higher-level API. Its integration and property
tests are in `duckdb-simple/test`, and its leak test is in `duckdb-simple/leaktest`.

The pure geometry types and codecs are maintained in
[geometry-simple](https://github.com/Tritlo/geometry-simple).

## Testing guidelines

Test new behavior through the database where possible. Add regressions for
bug fixes. Run the full test suite before submitting a change.

Build all components before running the tests. With Cabal 3.16 and 3.18,
`cabal test all` on a clean tree can skip building the tests for a package
with a custom setup.

```sh
cabal build all
cabal test all --test-show-details=direct
cabal run duckdb-simple-leak-test -- all 100
```

The last command checks 10,000 query cycles, 10,000 callback cycles, 10,000
VARIANT/GEOMETRY cycles, and 7,000 cancellations. Each phase keeps one connection open.
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

Build parameter types with the type cache of the statement's connection. The
cache reads its types on a separate connection the first time a parameter
needs one. Do not run queries on the statement's connection during binding.

## Native library configuration

See [docs/duckdb-2.0.md](docs/duckdb-2.0.md) for DuckDB 2.0 development.
Enable `duckdb-v2` in both packages and supply the matching native library.
Check the header ABI before you run the preview suites. The preview workflow
builds the native library on Linux and macOS from the pinned header commit.

The bindings support DuckDB >= 1.5.3 and < 1.6. Cabal downloads DuckDB 1.5.6
to the user cache on glibc Linux and macOS. Set
`--configure-option=--duckdb-install-dir=/absolute/path` to select another
installation directory. Enable the `systemlib` Cabal flag to use the system
library. Set `extra-lib-dirs` to an absolute path to select
a library in another directory. See the `cabal.project` example in the README.
Nix builds use the library supplied by Nix and do not download it.

CI tests 1.5.3, 1.5.4, and 1.5.5 with GHC 9.14.1 on Linux. It tests 1.5.6
with all supported compilers on Linux and GHC 9.14.1 on macOS. The FFI suite
rejects runtimes outside the supported range. Set
`DUCKDB_TEST_VERSION` to the exact expected version, such as `1.5.3`, to detect
loader configuration errors. Run the full suite on each supported native
version. Do not skip feature tests that pass on the minimum version.

CI also builds and tests with `--enable-optimization=0` on GHC 9.14.1 and
DuckDB 1.5.3. Floating-point conversion must preserve special values without
compiler rewrite rules.

## Documentation

Document exported functions and types with Haddock comments.
Build the documentation with `cabal haddock all`.
