# Contributing

## Project structure

`duckdb-ffi/generated` contains the generated C API bindings and ABI checks.
The client headers and upstream license are in `duckdb-ffi/cbits`.
`duckdb-ffi/vendor/upstream.sha256` records their digests, and
`duckdb-ffi/vendor/duckdb-api.json` records their source commit.
The generation driver is in `dev/bindings`. The tests are in
`duckdb-ffi/test`.
`duckdb-simple/src` contains the higher-level API. Its integration and property
tests are in `duckdb-simple/test`, and its leak test is in `duckdb-simple/leaktest`.

The pure geometry types and codecs are maintained in
[geometry-simple](https://github.com/Tritlo/geometry-simple).

## Design principle

The motto of `duckdb-simple` is "least surprise". The library does not add
hidden behavior. Functions that accept the same input must give the same result
as DuckDB. When two functions treat an input differently, make them consistent
unless a documented reason requires the difference.

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
Review the new checksums.

`dev/release.sh` generates and verifies both APIs. It then builds the Hackage
source archives and documentation of all selected packages, and asks for
confirmation before the first upload. Answer `n` for a rehearsal without an
upload. Cabal manages authentication through its configuration and prompts.
The source archives contain the generated Haskell modules and C ABI checks.
Package users do not need Nix, LLVM, or hs-bindgen. The release script uploads
candidates by default. Use `--publish` for an authorized release.

Build all components before running the tests. With Cabal 3.16 and 3.18,
`cabal test all` on a clean tree can skip building the tests for a package
with a custom setup.

```sh
cabal build all
cabal test all --test-show-details=direct
cabal run duckdb-simple-leak-test -- all 100
```

The last command runs the `all` mode with 100 batches: 32
open/close cycles, the dropped-handles check, 10,000 query cycles, 10,000
callback cycles, 7,000 cancellation attempts, 10,000 cycles of VARIANT,
GEOMETRY, and other composite values, and 2,000 typed registration cycles.
The sustained phases keep one connection open.
The normal leak test uses 10 batches, so each batched phase runs one tenth
of those counts. Native RSS and thread checks require Linux `/proc`.
Callback collection and functional checks also run on other systems.

The Linux CI job with GHC 9.12.4 also runs these under Valgrind: the V2 test of
`duckdb-ffi`, the leak test modes `open-close`, `dropped-handles`, `long-lived`,
`callbacks`, `cancel`, `new-types`, and `typed` with one batch each, and the
DataFrame test. Definite and indirect native leaks fail the job. That job sets
`DUCKDB_LEAK_RSS_CHECK=0` because Valgrind changes process RSS. The ordinary
leak test keeps the RSS assertion enabled.

Use `--with-compiler=ghc-VERSION` to select a supported compiler.
For benchmarks, build both versions with the same compiler and DuckDB library.
For example:

```sh
cabal bench duckdb-simple-bench --benchmark-options='fold 100000 +RTS -N1 -T -RTS'
```

Nix CI builds DuckDB from the pinned source. It builds and tests both packages
with GHC 9.14.1.
Run the same build locally with `nix-build dev/nix/ci.nix --no-out-link`.

The separate DataFrame integration suite checks Arrow export with
`dataframe-arrow-bridge`. It runs in each Cabal CI job on Linux and macOS.
Enable it locally with:

```sh
cabal build duckdb-simple-dataframe-test -fdataframe-tests
cabal test duckdb-simple-dataframe-test -fdataframe-tests --test-show-details=direct
```

The flag defaults to off because the bridge also depends on DataFrame's
file-format packages. These packages are absent from the pinned Nix package
set. The ordinary suite checks Arrow ownership without those dependencies.

## Code style

Format Haskell code with `fourmolu` before committing. The file `fourmolu.yaml`
sets the style and the column limit.
Format Cabal files with `cabal-gild`. Keep names consistent with the surrounding
module.

Build parameter values with a factory borrowed from the statement's connection.
Use the managed type cache for configured GEOMETRY types. Run VARIANT metadata
queries on its helper connection so decoding does not interrupt an active
stream. Use public V2 operations and SQL metadata.
Do not read private payload layouts or convert handles between the two APIs.

## Native library configuration

See [docs/duckdb-2.0.md](docs/duckdb-2.0.md) for DuckDB 2.0 development.
Supply the matching native library. The package versions target DuckDB 2.0.
Check the header ABI before you run the suites. CI uses the verified release
archives on Linux and macOS. Nix builds the pinned release source.
The optional `quack-tests` suite requires matching local `httpfs` and Quack
extensions. See the same document for its commands and native limitations.

The bindings target DuckDB 2.0.0. The automatic installer verifies the archive
checksums in `duckdb-ffi/native.sha256`. Set
`--configure-option=--duckdb-install-dir=/absolute/path` to select another
installation directory. Enable the `systemlib` Cabal flag to use the system
library. Set `extra-lib-dirs` to an absolute path to select
a library in another directory. See the `cabal.project` example in the README.
Nix builds use the library supplied by Nix and do not download it.

CI tests the pinned 2.0.0 release with all supported compilers on Linux x86_64,
and with GHC 9.14.1 on Linux aarch64 and on macOS. The V2 API requires exactly
DuckDB 2.0.0. Set `DUCKDB_TEST_VERSION` to the exact expected version, such as
`2.0.0`, to detect loader configuration errors. Run the full suite after each
native pin update.

CI also builds and tests with `--enable-optimization=0` on GHC 9.14.1 and
DuckDB 2.0. Floating-point conversion must preserve special values without
compiler rewrite rules.

## Documentation

Document exported functions and types with Haddock comments.
Build the documentation with `cabal haddock all`.

Explain a concept that more than one definition depends on in a Note, in the
style of GHC. Put the Note next to the definition that owns the concept:

```haskell
{- Note [Callback marks]
~~~~~~~~~~~~~~~~~~~~~~~~
The text of the Note.
-}
```

At each other place that depends on the concept, refer to the Note: `See
Note [Callback marks]`. Add the module name when the Note is in another module.
Keep the Haddock comment of a definition short. Put the reasons that connect
several definitions in the Note, and do not repeat them at the other places.
Start each item of a list in a Note with `-`. `fourmolu` changes a `*` at the
start of a line to `\*`.
