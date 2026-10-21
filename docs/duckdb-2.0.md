# DuckDB 2.0

`duckdb-ffi` 2.0.0.0 and `duckdb-simple` 0.4.0.0 target DuckDB
[2.0.0](https://github.com/duckdb/duckdb/releases/tag/v2.0.0). The package
version selects the supported native API. Earlier package releases remain
available for DuckDB 1.5.

## Pinned API

The client headers in `duckdb-ffi/cbits/duckdb.h` and `duckdb_v2.h` come from
the upstream `v2.0.0` tag, commit
[d54cbbe3df17e8e5ce68312f483b4155836112aa](https://github.com/duckdb/duckdb/commit/d54cbbe3df17e8e5ce68312f483b4155836112aa).
`duckdb-ffi/cbits/duckdb-LICENSE` contains the upstream license.
The headers and license are unmodified upstream files.
`duckdb-ffi/vendor/upstream.sha256` records their SHA256 digests.
`duckdb-ffi/vendor/duckdb-api.json` records the source commit and the native
source archive that Nix builds.

Builds use the checked-in headers directly. Cabal installs both client headers,
`duckdb_arrow.h`, and `duckdb-LICENSE` with the library. Package users do not
need LLVM or hs-bindgen. The pin update and download scripts are maintainer
tools outside the Hackage packages.

The retained header declares 548 functions. Compared with the 1.5.6 header, it adds
`duckdb_create_timestamp_tz_ns`, `duckdb_get_timestamp_tz_ns`, type tag
`DUCKDB_TYPE_TIMESTAMP_TZ_NS = 42`, and error tags
`DUCKDB_ERROR_DATA_CORRUPTION = 43` and `DUCKDB_ERROR_RESOURCE_IN_USE = 44`.
It removes no functions and changes no existing function signatures or
structure definitions. The V2 header declares 620 functions.

## Generated modules

Both client APIs are generated with `hs-bindgen` 1.0. `Database.DuckDB.FFI`
exposes the retained API and replaces the old aggregate and Deprecated modules.
`Database.DuckDB.FFI.V2` exposes the V2 API. The V2 Functions and Types
submodules are no longer exposed. The raw API exposes native primitives.
`duckdb-simple` retains its existing connection and result API.

Each generated module has its own native types. Equal C layouts do not make
the corresponding Haskell types interchangeable. Keep handles and enums from
the two APIs separate. Their result, connection, error, and ownership
contracts differ. File flag 4 means exclusive creation in the retained API and
truncation in V2.

The generator supplies layouts, typed callbacks, const pointers, and adapters
for structures passed by value. All 620 V2 functions are unstable and require
the matching native version. The generated C source enables
`DUCKDB_V2_API_ALLOW_UNSTABLE` explicitly. Ownership, callback lifetimes,
exception handling, Arrow release operations, and value conversion remain in
the calling code.

Generate the artefacts before building a checkout:

```sh
nix-shell dev/nix/generate.nix --run 'dev/bindings/generate-bindings.sh --check'
```

Generation verifies identical Haskell output for x86_64 and aarch64 Linux,
x86_64 and arm64 macOS, and the native target. It fails if a native function
has no binding. The generated C files contain ABI assertions. Compilation
checks their sizes, alignments, and field offsets against the pinned headers.
These checks do not replace runtime ownership tests.

The generated Haskell modules and ABI-check C files are build artefacts in
`duckdb-ffi/generated/`. Git ignores that directory. CI generates and checks
the artefacts, then passes them to the build jobs. `generated.sha256` records
the expected output. Source archives include the generated files. Package
users do not need the generator or Clang. Local Docker and Nix builds require
these artefacts too.

## Ownership

Raw V2 callers must follow the ownership rules in the header documentation.
Do not destroy borrowed handles. Keep callback function pointers alive until
DuckDB releases the registration. Catch Haskell exceptions inside callbacks
and report failures through the native error handle.

After a `duckdb_v2_result_render_box` error, check the result slot and destroy
any owned handle that remains. The pinned implementation checks the render mode
and view arguments before it transfers ownership. These argument errors leave
the result intact. After transfer, it clears the slot on success and failure.
See the [validation before transfer](https://github.com/duckdb/duckdb/blob/d54cbbe3df17e8e5ce68312f483b4155836112aa/src/main/capi/v2/capi_v2_result.cpp#L464).

The [upstream versioning rules](https://github.com/duckdb/duckdb/blob/d54cbbe3df17e8e5ce68312f483b4155836112aa/api_spec/VERSIONING.md)
define stable, deprecated, unstable, and removed lifecycle states. Headers
default to the newest specified version. To use unstable declarations, enable
them explicitly and target the newest API. Extension headers use versioned
function tables. Ordinary clients link exported functions.

## Pin updates

`dev/update-duckdb.sh` pins a release tag. It resolves the tag to a full commit
SHA before it downloads files. It replaces the client headers and the upstream
license. It writes new digests to `vendor/upstream.sha256`, `native.sha256`, and
`vendor/duckdb-api.json`. Regenerate with the pinned Nix toolchain and review
the diff:

```sh
dev/update-duckdb.sh v2.0.0
nix-shell dev/nix/generate.nix --run 'dev/bindings/generate-bindings.sh'
git diff
```

`generate-bindings.sh` checks the vendored files against
`vendor/upstream.sha256` before it runs. CI repeats this check.

## Native runtime

`duckdb-ffi/native.sha256` pins the official shared-library archives.
`Setup.hs` downloads from it by default. `dev/fetch-duckdb.sh` downloads the
same archive for `-fsystemlib` builds. It checks the archive digest and
requires the release headers to be identical to the vendored headers.

```sh
dev/fetch-duckdb.sh /tmp/duckdb-2.0-native
cabal build all -fsystemlib --extra-lib-dirs=/tmp/duckdb-2.0-native/native
DUCKDB_TEST_VERSION=2.0.0 cabal test all -fsystemlib --extra-lib-dirs=/tmp/duckdb-2.0-native/native --test-show-details=direct
```

The `.github/workflows/duckdb-2.0.yml` job and Docker use the release
archives. Nix builds the same source pin with explicit version 2.0.0:

```sh
nix-build dev/nix/ci.nix --no-out-link
docker build --tag duckdb-haskell:2.0 .
```

These commands build local artifacts without uploading or deploying them.

## Feature coverage

The SQL feature list follows the [upstream 2.0 preview](https://duckdb.org/2026/08/17/duckdb-20-highlights).
The C API list follows the pinned headers. SQL support depends on the native
build and its installed extensions.

| Feature | Access from this repository |
| --- | --- |
| Triggers, DML in CTEs, nested schemas, NEAREST joins, variables, JSON mutation, recursive CTE additions | `query` and `execute` |
| PEG parser and lambda syntax changes | `query` and `execute`. Client SQL needs `SELECT`; bare expression statements use the CLI's shell grammar. |
| Quack, CONNECT, remote pushdown | SQL with the required extensions |
| VARIANT and GEOMETRY | `Variant`, geometry types, and codecs |
| Nanosecond TIMESTAMPTZ | Retained value access and typed conversion |
| Asynchronous I/O, storage format 2.0, query optimizations | The native engine through SQL |
| Timezones, collations, extension repositories | SQL |
| Environment, instances, connections, attachment options, schemas, catalog | Raw V2 |
| SQL tokenization, statements, prepared statements, expressions, result cursors, Arrow results and streams | Raw V2 |
| Values, logical and custom types, vectors, data chunks, arenas, column collections | Raw V2 |
| Scalar, aggregate, table, cast, and COPY functions and signatures | Raw V2 and callback constructors |
| Multi-file functions, table partitioning, batch claiming | Raw V2 and callbacks |
| Replacement scans, file systems, logging, extension entry points | Raw V2 |

Appender access uses the retained API.

## Remote tests

The optional `duckdb-simple-quack-test` suite requires compatible local
`httpfs` and Quack artifacts. Their version and platform must match the
selected native library. The suite checks authentication, server-visible
writes, bound queries, streaming, and nanosecond timestamps. It fails if
either path is missing. Run it with:

```sh
cabal build duckdb-simple:duckdb-simple-quack-test -fquack-tests -fsystemlib --extra-lib-dirs=/absolute/path/to/native
DUCKDB_HTTPFS_EXTENSION=/absolute/path/to/httpfs.duckdb_extension \
DUCKDB_QUACK_EXTENSION=/absolute/path/to/quack.duckdb_extension \
cabal test duckdb-simple:duckdb-simple-quack-test -fquack-tests -fsystemlib --extra-lib-dirs=/absolute/path/to/native --test-show-details=direct
```

## Native pin update

1. Run `dev/update-duckdb.sh` with the release tag and regenerate both APIs.
   Review signature, enum, callback, and ownership changes. Update the
   function counts in this document.
2. Keep untested platforms out of `duckdb-ffi/native.sha256`.
3. Update the Nix source pin and its version assertions.
4. Run the full suites, the leak suite, and Haddock on all supported compilers.
5. Build source archives with `cabal sdist all`. Check that they contain the
   generated modules, C checks, client headers, license, and manifest.
6. `dev/release.sh` regenerates both APIs and verifies the header digests and
   generated checksums before it builds and uploads the Hackage archives. It uploads
   package candidates even without `--publish`.
