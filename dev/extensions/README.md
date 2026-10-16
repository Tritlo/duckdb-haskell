# Loadable Haskell extensions

For the pinned DuckDB 2.0 preview only. This directory prepares a loadable
Haskell example. It is not part of either Hackage package. The reusable
`Database.DuckDB.Simple.Extension` helper and extension-mode FFI headers are
part of the packages.

No compilation, linking, native loading, or integration checks have run for
this change. The PR remains a draft. Run the commands below when the machine
is free. This is the validation handoff, not a record of successful builds.

## Design

The separate Cabal project enables the `extension` flags on `duckdb-ffi` and
`duckdb-simple`. The FFI mode exposes only `Database.DuckDB.FFI.V2`. It skips
native-library installation and does not link `libduckdb`. A forced C include
uses DuckDB's official function-table macros in the existing hs-bindgen C
wrappers. The generated bindings remain unchanged and outside Git.

`duckdb_haskell_extension_init_api` negotiates the table before Haskell starts.
It copies the borrowed table into the FFI library and publishes it once under
a mutex. This mode supports one DuckDB implementation per process. Generated
C wrappers, the table, and managed callback helpers have hidden visibility.
The initializer and the demo's native entrypoint remain exported.

The demo initializes the threaded RTS once. It uses private arguments, ignores
`GHCRTS`, and disables the RTS signal handlers. It keeps the RTS alive for the
process lifetime. Do not call `hs_exit` after registration or database close.
DuckDB can still own Haskell callbacks, state, and destructor addresses.

`Extension.runExtension` converts initialization exceptions to the borrowed
native load error slot. Registration targets and contexts must not escape the
entrypoint. Managed registration retains callback resources after it returns.

The example registers `haskell_answer()`, `haskell_add_one(BIGINT)`, and
`haskell_fail()`. They exercise native results, batch inputs, NULLs, overflow,
and callback error handling. The entrypoint is
`haskell_demo_init_c_api_v2`. Keep the packaged filename
`haskell_demo.duckdb_extension`.

## Deferred build and load

Use the pinned GHC 9.14.1 environment. These commands can start substantial
builds. Do not run them until the machine is available.

Generate ignored API artifacts if they are absent:

```sh
nix-shell dev/nix/generate.nix --run 'dev/bindings/generate-bindings.sh --check'
```

Build only the example in its separate project and build directory:

```sh
nix-shell dev/nix/shell.nix --run 'GHC_ENVIRONMENT=- cabal build \
  --project-file=dev/extensions/cabal.project \
  --builddir=dist-newstyle-extensions --with-compiler=ghc \
  duckdb-haskell-extension-example:flib:haskell_demo'
```

Locate the library after the build. `list-bin` does not check that it exists:

```sh
nix-shell dev/nix/shell.nix --run 'GHC_ENVIRONMENT=- cabal list-bin \
  --project-file=dev/extensions/cabal.project \
  --builddir=dist-newstyle-extensions --with-compiler=ghc \
  duckdb-haskell-extension-example:flib:haskell_demo'
```

Build a plain native CLI from the same pin. The preview builder normally
builds only the shared library. This opt-in also builds the CLI:

```sh
nix-shell dev/nix/shell.nix --run \
  'DUCKDB_BUILD_SHELL=ON dev/preview/build-duckdb.sh /tmp/duckdb-extension-native'
```

Read the target platform and version before packaging:

```sh
/tmp/duckdb-extension-native/native/duckdb -csv -noheader \
  -c 'PRAGMA platform; PRAGMA version;'
```

The pinned development loader uses source ID `13d1d8f56a` as its version field.
It does not use `v2.0.0-dev0`. See [PACKAGING.md](PACKAGING.md). Use the exact
platform printed by the CLI. Replace the library path and platform below:

```sh
mkdir -p dev/extensions/dist
uv run dev/extensions/pack.py /absolute/path/to/libhaskell_demo.so \
  --output dev/extensions/dist/haskell_demo.duckdb_extension \
  --platform linux_amd64 --duckdb-version 13d1d8f56a \
  --extension-version 0.1.0

uv run dev/extensions/smoke.py \
  --duckdb /tmp/duckdb-extension-native/native/duckdb \
  dev/extensions/dist/haskell_demo.duckdb_extension
```

The smoke script starts two plain native CLI processes. It checks repeated
loading, scalar results, 10,000 input rows, NULLs, callback errors, overflow,
and connection reuse after errors. It does not use a Haskell host executable.

## Deployment and remaining validation

The prepared recipe targets Linux and macOS. It uses shared GHC and package
libraries. A renamed foreign library is not a self-contained distribution.
Keep its dependency closure available, including the threaded RTS. Cabal and
Nix build-store paths require those stores to remain available. Relocatable
bundling, Windows, and loading into another Haskell executable remain outside
this prepared recipe.

Before declaring support, complete these checks:

- Confirm that the extension and its Haskell dependency closure have no
  `libduckdb` dependency or unresolved direct `duckdb_v2_*` symbols. Use
  `readelf` on Linux or `otool` and `nm` on macOS.
- Compare the packaged footer with the pinned official CMake writer.
- Run the CLI smoke script on Linux and macOS. Inspect startup, symbol lookup,
  NULL behavior, error recovery, and process shutdown.
- Check initialization failure, multiple databases in one native host,
  concurrent loads, callback-state destruction, and cancellation under Valgrind.
- Run the ordinary package suites with both extension flags disabled.
- Check source archives and installed extension-mode consumers without LLVM.
- Run strict Pyright checks for the two Python tools.

The PR head uses `[skip ci]` because builds were explicitly deferred. Existing
GitHub workflows use `push` or `pull_request`, which honor this marker. Push a
new commit without the marker when validation is authorized. See
[GitHub's skip instructions](https://docs.github.com/en/actions/how-tos/manage-workflow-runs/skip-workflow-runs).

References: [Cabal foreign libraries](https://cabal.readthedocs.io/en/3.18/cabal-package-description-file.html#foreign-libraries),
[GHC runtime initialization](https://downloads.haskell.org/ghc/9.14.1/docs/users_guide/exts/ffi.html#using-your-own-main),
and [shared-library deployment](https://downloads.haskell.org/ghc/9.14.1/docs/users_guide/shared_libs.html#finding-shared-libraries-at-runtime).
