# Loadable Haskell extensions

This directory contains a loadable Haskell example for the pinned DuckDB 2.0.0
release. The example is not part of either Hackage package. The reusable
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

`Extension.registerMacro`, `registerPragma`, `registerCustomOption`, and
`registerLogSink` use the same opaque extension value. Macro signatures and
custom-option defaults use `FactoryDefaultValue` during initialization.
All V2 declarations are currently unstable. Rebuild the extension when the
native source pin or release version changes. A matching major or minor
version does not establish ABI compatibility.

The example registers typed scalar functions, scalar and table macros, a
PRAGMA, a typed custom option, and a log sink. They exercise native results,
batch inputs, NULLs, Unicode text, overflow, and callback errors. The entrypoint is
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

Build only the example in its separate project and build directory. Use a fresh
build directory after a native header change. When the source pin changes,
clean an existing directory before the build. Cabal can retain C objects from
the previous function table:

```sh
(cd dev/extensions && cabal clean --builddir=../../dist-newstyle-extensions)
```

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

Fetch the verified native CLI and shared library for the pinned release:

```sh
dev/fetch-duckdb.sh /tmp/duckdb-extension-native
```

Read the target platform and version before packaging:

```sh
/tmp/duckdb-extension-native/native/duckdb -csv -noheader \
  -c 'PRAGMA platform; PRAGMA version;'
```

The pinned release loader uses `v2.0.0` as its version field, including the
leading `v`. See [PACKAGING.md](PACKAGING.md). Use the exact
platform printed by the CLI.

On Linux, Nix's GHC can require a newer glibc than the host CLI uses.
`nix-shell` does not replace the downloaded CLI's ELF interpreter. In that
case, create this wrapper and use its path for `--duckdb` in the smoke command.
Run it inside the same Nix shell as the extension build:

```sh
cat > /tmp/duckdb-extension-native/native/duckdb-nix <<'SH'
#!/usr/bin/env bash
set -eu
loader=$(cat "$NIX_CC/nix-support/dynamic-linker")
libc_dir=$(dirname "$loader")
cxx_dir=$(dirname "$(cc -print-file-name=libstdc++.so.6)")
exec "$loader" --library-path "$libc_dir:$cxx_dir" "$(dirname "$0")/duckdb" "$@"
SH
chmod +x /tmp/duckdb-extension-native/native/duckdb-nix
```

Replace the library path and platform in the packaging command:

```sh
mkdir -p dev/extensions/dist
nix-shell dev/nix/shell.nix --run 'GHC_ENVIRONMENT=- runghc \
  dev/extensions/Pack.hs /absolute/path/to/libhaskell_demo.so \
  --output dev/extensions/dist/haskell_demo.duckdb_extension \
  --platform linux_amd64 --duckdb-version v2.0.0 \
  --extension-version 0.1.0'

nix-shell dev/nix/shell.nix --run 'GHC_ENVIRONMENT=- runghc \
  dev/extensions/Smoke.hs \
  --duckdb /tmp/duckdb-extension-native/native/duckdb \
  dev/extensions/dist/haskell_demo.duckdb_extension'
```

The smoke script starts two plain native CLI processes. It checks repeated
loading, scalar results, 10,000 input rows, NULLs, callback errors, overflow,
and connection reuse after errors. It also checks both macro kinds, PRAGMA
results before and after an option update, and native log-sink selection.
It does not use a Haskell host executable. The timeout includes process exit
and output collection. A timeout kills and reaps the CLI child.

The CI matrix builds and loads the example on Linux and macOS with
GHC 9.14.1 at optimization level 1. Each job uses a fresh extension build
directory and the matching native CLI.

## Deployment and remaining validation

The example targets Linux and macOS and uses shared GHC and package libraries.
Keep those dependencies available, including the threaded RTS, when you rename
or deploy the foreign library. Cabal and Nix build-store paths require those
stores to remain available. This example does not cover relocatable bundling,
Windows, or loading into another Haskell executable.

Final-release Linux x86_64 checks on 2026-10-21 use the verified official
`v2.0.0` CLI and shared library:

- A fresh optimized Haskell extension build passes both native CLI smoke
  processes. The CLI uses the matching Nix loader for the Nix GHC libraries.
- The loader accepts `v2.0.0` metadata and rejects the development source ID.
  The 534-byte footer matches the final official CMake writer byte for byte.
- All 624 ordinary functional tests and the default leak suite pass.
- The final SDK headers match the pinned inputs. All four generated-file
  checksums remain unchanged. The V2 table still has 620 unstable entries.
- `ldd` confirms the threaded RTS dependency and no `libduckdb` dependency.

The post-release review repeats both native CLI smoke processes after the V2
value and lifetime fixes. All 633 client functional tests, the default leak
suite, and Haddock pass. See [the V2 review](../v2-migration.md) for the fixes
and focused Valgrind results. These checks use Linux x86_64 and Nix GHC 9.14.1.

Linux x86_64 checks on 2026-10-20 use GHC 9.14.1 and native source
`d54cbbe3df17e8e5ce68312f483b4155836112aa`:

- A clean extension build passes both native CLI smoke processes, including
  both macro kinds, the PRAGMA, custom-option updates, and log-sink selection.
- The updated native function table has 620 entries. An old C object can retain
  the previous table size. Use a fresh build directory after a header change.
- The ordinary client suites pass all 617 functional tests and the default
  leak suite against the same native library.
- Fresh source archives build and install without LLVM or hs-bindgen. Static
  and dynamic consumers pass with the loader environment unset.
- `ldd` confirms the threaded RTS dependency and no `libduckdb` dependency.
- Haddock builds for both extension-mode libraries. Existing generated-link
  warnings remain.

The committed-code audit on 2026-10-21 repeats the native CLI checks with a
fresh optimized extension build. A C host also passes concurrent first loads,
repeated loads, callback cleanup, cancellation, connection recovery, and host
signal preservation. The packer output matches the pinned official CMake
writer byte for byte. Valgrind reports no errors and no definite or indirect
lost bytes. The live RTS retains 1,536 possible lost bytes in four glibc TLS
blocks and about 4.8 MB of reachable process-lifetime storage.

The following checks used the October 16 pin. They do not establish current
platform compatibility.

Linux x86_64 validation used GHC 9.14.1 and the earlier native preview:

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

macOS ARM64 validation used Nix GHC 9.14.1, Clang 21.1.8, and the earlier native
preview:

- The Mach-O demo passes both native CLI smoke processes.
- The C hosts pass concurrent first loads, repeated loads, independent
  databases, cancellation, connection recovery, host signal preservation,
  and refused API negotiation.
- `otool` and `nm` confirm the threaded RTS dependency and no `libduckdb`
  dependency, direct native imports, or exported private C helpers.
- All 602 ordinary functional tests pass with four test workers. Callback
  collection and the functional leak checks also pass. Native RSS and thread
  counters require Linux `/proc`.

These checks use the local Cabal and Nix dependency stores. They do not verify
relocatable deployment.

References: [Cabal foreign libraries](https://cabal.readthedocs.io/en/3.18/cabal-package-description-file.html#foreign-libraries),
[GHC runtime initialization](https://downloads.haskell.org/ghc/9.14.1/docs/users_guide/exts/ffi.html#using-your-own-main),
and [shared-library deployment](https://downloads.haskell.org/ghc/9.14.1/docs/users_guide/shared_libs.html#finding-shared-libraries-at-runtime).
