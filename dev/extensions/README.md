# Loadable Haskell extensions

For the pinned DuckDB 2.0 preview only. This directory prepares a loadable
Haskell example. It is not part of either Hackage package. The reusable
`Database.DuckDB.Simple.Extension` helper and extension-mode FFI headers are
part of the packages.

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
Extension mode omits interface unfoldings. This keeps calls to private C
wrappers inside the FFI shared library when a client uses optimization.

The demo initializes the threaded RTS once. It uses private arguments, ignores
`GHCRTS`, and disables the RTS signal handlers. It keeps the RTS alive for the
process lifetime. Do not call `hs_exit` after registration or database close.
DuckDB can still own Haskell callbacks, state, and destructor addresses.

`Extension.runExtension` passes an opaque `Extension` to the registration action.
`Extension.createFunction` and `Extension.createFunctionWithState` use the same
typed argument and result conversions as the connection API. The scoped
`Extension.withExtensionContext` helper gives advanced registration code access
to its borrowed target and context. These handles must not escape the entrypoint.
Initialization and callback exceptions become native errors. Managed
registration retains callback resources after the entrypoint returns.

The example registers `haskell_answer()`, `haskell_add_one(BIGINT)`, and
`haskell_fail()`. They exercise native results, batch inputs, NULLs, overflow,
and callback error handling. The entrypoint is
`haskell_demo_init_c_api_v2`. Keep the packaged filename
`haskell_demo.duckdb_extension`.

Registration code uses ordinary Haskell values and functions:

```haskell
register extension = do
    Extension.createFunction extension "answer" (42 :: Int64)
    Extension.createFunction extension "add" ((+) :: Int64 -> Int64 -> Int64)
```

See [Demo.hs](example/src/Demo.hs) for the complete entrypoint. GHC requires
the two native handle constructors in scope at its foreign export. Application
functions do not need native handles, vectors, or allocation calls.

## Build and load

Use the pinned GHC 9.14.1 environment.

Generate ignored API artifacts if they are absent:

```sh
nix-shell dev/nix/generate.nix --run 'dev/bindings/generate-bindings.sh --check'
```

Build only the example in its separate project and build directory:

```sh
nix-shell dev/nix/shell.nix --run '(cd dev/extensions && \
  GHC_ENVIRONMENT=- cabal build --project-file=cabal.project \
  --builddir=../../dist-newstyle-extensions --with-compiler=ghc \
  duckdb-haskell-extension-example:flib:haskell_demo)'
```

Locate the library after the build. `list-bin` does not check that it exists:

```sh
nix-shell dev/nix/shell.nix --run '(cd dev/extensions && \
  GHC_ENVIRONMENT=- cabal list-bin --project-file=cabal.project \
  --builddir=../../dist-newstyle-extensions --with-compiler=ghc \
  duckdb-haskell-extension-example:flib:haskell_demo)'
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
nix-shell dev/nix/shell.nix --run 'GHC_ENVIRONMENT=- runghc \
  dev/extensions/Pack.hs /absolute/path/to/libhaskell_demo.so \
  --output dev/extensions/dist/haskell_demo.duckdb_extension \
  --platform linux_amd64 --duckdb-version 13d1d8f56a \
  --extension-version 0.1.0'

nix-shell dev/nix/shell.nix --run 'GHC_ENVIRONMENT=- runghc \
  dev/extensions/Smoke.hs \
  --duckdb /tmp/duckdb-extension-native/native/duckdb \
  dev/extensions/dist/haskell_demo.duckdb_extension'
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

Linux x86_64 validation uses GHC 9.14.1 and the pinned native preview:

- The typed demo builds and passes the native CLI smoke checks.
- A plain C host passes concurrent first loads, repeated loads, independent
  databases, callback cleanup, cancellation, and connection recovery.
- Runtime startup preserves ten installed host signal handlers. Refused API
  negotiation leaves the borrowed native error slot unchanged.
- `ldd`, `readelf`, and `nm` confirm no `libduckdb` dependency, direct native
  imports, exported generated C wrappers, or exported callback cleanup helpers.
- Valgrind reports no errors and no definite or indirect lost bytes. The live
  RTS retains 1,536 possible lost bytes in four thread-local-storage blocks
  and about 4.8 MB of reachable process-lifetime state.
- All 602 ordinary functional tests pass with four test workers. The default
  leak suite and Haddock build also pass. Generated Haddock link warnings remain.
- The exact sandboxed Nix CI build passes for both packages.
- The Haskell packer matches the official CMake writer byte for byte. Both
  tools compile with `-Wall -Werror`.
- Fresh source archives build and load without LLVM or the hs-bindgen
  generator. Temporary consumers verify worker state and initialization-error
  containment. These checks use extracted source packages and the existing
  dependency cache. They do not use `cabal install --lib`.

macOS loading and deployment remain unverified. Run the same native checks on
macOS before declaring support there. Inspect dependencies with `otool` and
`nm` on that platform.

References: [Cabal foreign libraries](https://cabal.readthedocs.io/en/3.18/cabal-package-description-file.html#foreign-libraries),
[GHC runtime initialization](https://downloads.haskell.org/ghc/9.14.1/docs/users_guide/exts/ffi.html#using-your-own-main),
and [shared-library deployment](https://downloads.haskell.org/ghc/9.14.1/docs/users_guide/shared_libs.html#finding-shared-libraries-at-runtime).
