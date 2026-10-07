# DuckDB 2.0 release preparation

Research date: 2026-10-07. The upstream [release calendar](https://duckdb.org/release_calendar)
lists 2026-10-21 for DuckDB 2.0.0. The date is tentative. DuckDB 2.0 is still a
preview. Confirm the final tag and API, and verify the native archives and
checksums before release.

## Pinned API

The snapshot in `duckdb-ffi/vendor/duckdb-api.tar.gz` comes from
[13d1d8f56a82b3413055819eb90d0b0e68b5c847](https://github.com/duckdb/duckdb/commit/13d1d8f56a82b3413055819eb90d0b0e68b5c847)
on the upstream `v2.0-cyanoptera` release branch, dated 2026-10-07. It contains
both client headers and both extension headers. It also contains the v1 and v2
YAML specifications, versioning rules, and upstream license.
The archive contains `provenance.json`, which records each upstream path and
SHA256 digest. `duckdb-api.sha256` verifies the archive. These are
development headers. A `stable` lifecycle entry for v2.0.0 describes the intended
release contract. The snapshot's ABI can still change before release.
The small `duckdb-api.json` manifest also pins the source archive used by
`scripts/build-duckdb-preview.sh`.

The existing 1.5 header stays in `duckdb-ffi/cbits/duckdb.h`. Preview builds
verify the archive and extract its two client headers into the Cabal build
directory. This step runs offline. It requires `tar` and `sha256sum` or `shasum`.
Python is required only for the audit, refresh, and generation tools.

The v1 header declares 548 functions and 21 callback typedefs. Compared with the
original 1.5.6 header, it adds `duckdb_create_timestamp_tz_ns`,
`duckdb_get_timestamp_tz_ns`, type tag `DUCKDB_TYPE_TIMESTAMP_TZ_NS = 42`, and
error tag `DUCKDB_ERROR_DATA_CORRUPTION = 43`. It removes no v1 functions and
changes no existing v1 function signatures or structure definitions.
The v2 header declares 582 functions and 37 callback typedefs. It also has six
Arrow callback fields, for a total of 43 callbacks.

The default build uses DuckDB 1.5.6. The `duckdb-v2` flag enables the
preview additions. `Database.DuckDB.FFI.V2` exposes the v2 API. Keep v1 and v2
handles separate. Their result, connection, error, and ownership contracts differ.
The raw API exposes native primitives. `duckdb-simple` retains its existing
connection and result API.

The binding uses the C API and does not call the C++ API. Haskell calls the v2
API directly because it passes structures by pointer. The v1 API retains its C
adapters. The source build compiles DuckDB's C++ engine to produce a native
library for the checks. You can also use an official native archive whose
headers match the pinned API. The selected official preview matches this snapshot.

Raw v2 callers must follow the ownership rules in the header documentation.
Do not destroy borrowed handles. Keep callback function pointers alive until
DuckDB releases the registration. Catch Haskell exceptions inside callbacks
and report failures through the native error handle.

If this preview rejects an invalid `duckdb_v2_result_render_box` render mode,
it leaves the result slot non-null. The header says the call consumes the result
on failure. After an error, destroy any owned result that remains in the slot. The
[implementation validates the mode before transfer](https://github.com/duckdb/duckdb/blob/13d1d8f56a82b3413055819eb90d0b0e68b5c847/src/main/capi/v2/capi_v2_result.cpp#L554).
Recheck this ownership behavior against the final runtime.

The [upstream versioning rules](https://github.com/duckdb/duckdb/blob/13d1d8f56a82b3413055819eb90d0b0e68b5c847/api_spec/VERSIONING.md)
define stable, deprecated, unstable, and removed lifecycle states. Headers default
to the newest specified version. To use unstable declarations, enable them
explicitly and target the newest API. Extension headers use versioned function
tables. Ordinary clients link exported functions. Header coverage alone does not
verify that a Haskell shared library can load as a DuckDB extension.

## Audit and refresh

These commands use Python's standard library. The checks run offline:

```sh
uv run scripts/duckdb-api.py --check
uv run scripts/duckdb-api.py --report > /tmp/duckdb-api-report.json
uv run scripts/duckdb-api.py --compare-headers /absolute/path/to/native/include
```

`--check` verifies the archive and all snapshot digests. For every v1 and v2 function, it requires
a direct FFI import or a C adapter that calls that function. It also requires
every callback typedef and Arrow callback field. `--report` lists each covered function,
callback alias, omission, and v1 declaration change. It compares function types
without parameter names. It also compares complete typedefs, including
enumerations and structure definitions. Run compilation, ABI checks, and behavior
tests in addition to the coverage checks.

Refresh only after selecting an upstream commit, release branch, or release tag.
Use `v2.0-cyanoptera` for 2.0 preparation. Regeneration requires `fourmolu`
0.20.1.0 on `PATH`.

```sh
uv run scripts/duckdb-api.py --refresh v2.0-cyanoptera
uv run duckdb-ffi/scripts/gen_ffi_v2.py
uv run scripts/duckdb-api.py --check
uv run scripts/duckdb-api.py --report > /tmp/duckdb-api-final.json
```

`--refresh` resolves the ref to a full SHA before it downloads any files. It
replaces the archive, manifest, and checksum. Review the manifest, API report,
and generated code. The archive preserves the upstream files without edits.
Use the recorded SHA for subsequent downloads and builds.
Select `v2.0.0` instead of the branch when the final release tag is available.

The official Linux preview fetched during preparation reports
`v2.0.0-alpha45189`. Its archive SHA256 is
`5d08f26d7e5f07fcae3d315c039a09da627f52639aeace3af06abc5456e35132`.
`PRAGMA version` reports source ID `13d1d8f56a` and codename `Cyanoptera`.
All four headers in the archive are byte-identical to this snapshot. Verify the
archive digest and headers before use because the
[preview installation links](https://duckdb.org/install/preview) can change.
This preview works directly with both APIs. Set
`DUCKDB_TEST_VERSION=2.0.0-alpha45189` for its test run.

The [alpha announcement](https://duckdb.org/2026/09/02/try-duckdb-20-alpha)
identifies `v2.0-cyanoptera` as the release branch. Select a commit on that
branch for 2.0 work. The separate `main` branch has diverged and does not provide
the same 2.0 C API. This snapshot uses the exact preview source commit so the
native binary and source pin agree.

The source build requires Bash, Python 3, CMake, a C/C++ compiler, a build tool
such as Make, `curl`, `tar`, and `sha256sum` or `shasum`.
Build and test the pinned source with:

```sh
scripts/build-duckdb-preview.sh /tmp/duckdb-2.0-native
uv run scripts/duckdb-api.py --compare-headers /tmp/duckdb-2.0-native/native
cabal build all -fduckdb-v2 -fsystemlib --extra-lib-dirs=/tmp/duckdb-2.0-native/native
DUCKDB_TEST_VERSION=2.0.0-dev0 cabal test all -fduckdb-v2 -fsystemlib --extra-lib-dirs=/tmp/duckdb-2.0-native/native --test-show-details=direct
```

The `.github/workflows/duckdb-2.0.yml` job uses this source pin. Its checks apply
to this development commit. Validate the final release archives separately.

The Nix and Docker preview paths use the same source archive pin:

```sh
nix-build dev/nix/ci.nix --arg preview true --no-out-link
docker build --build-arg DUCKDB_PREVIEW=true --tag duckdb-haskell:2.0-preview .
```

The default Nix and Docker paths use DuckDB 1.5. These commands build local
artifacts without uploading or deploying them.

## Feature coverage

The SQL feature list follows the [upstream 2.0 preview](https://duckdb.org/2026/08/17/duckdb-20-highlights).
The C API list follows the pinned headers and specifications. SQL support depends
on the native build and its installed extensions.

| Feature | Access from this repository | Release validation |
| --- | --- | --- |
| Triggers, DML in CTEs, nested schemas, NEAREST joins, variables, JSON mutation, recursive CTE additions | Existing `query` and `execute` SQL paths | Execute representative SQL against the final runtime. |
| PEG parser, expression statements, lambda syntax changes | Existing SQL paths | Check parameters, multi-statement extraction, quoted names, and parser errors. The [parser extension API](https://duckdb.org/2026/08/20/duckdb-20-peg-parser) remains a preview. |
| Quack, CONNECT, remote pushdown | SQL with the required extensions | Test a local server separately. Check authentication and extension availability. |
| VARIANT and GEOMETRY | Existing `Variant`, geometry types, and codecs | Check nested values, Parquet round trips, CRS, and the VARIANT storage codec. |
| Nanosecond TIMESTAMPTZ | Preview v1 value access and typed conversion | Check sub-microsecond precision and temporal infinity. |
| Asynchronous I/O, storage format 2.0, query optimizations | Native engine through existing SQL paths | Check file reopen, cancellation, and older database reads. State the new write format. |
| Timezones and collations | SQL and existing temporal types | Check timezone conversion and extension loading. |
| Extension repositories | SQL | Check local signed repository loading separately. |
| Environment, instances, connections, attachment options, schemas, catalog | Raw v2 | Check lifetime order, resource-in-use errors, attach/detach, and option discovery. |
| SQL tokenization, statements, prepared statements, expressions, result cursors, Arrow results and streams | Raw v2 | Check token iteration, bind/execute, chunk exhaustion, cancellation, errors, and Arrow release callbacks. |
| Values, logical/custom types, vectors, data chunks, arenas, column collections | Raw v2 | Check widths, buffer layouts, nulls, borrowed data, and destruction. |
| Scalar, aggregate, table, cast, COPY functions and signatures | Raw v2 and callback constructors | Check registration and execution, parameter kinds, named/default arguments, callback errors, state destruction, and COPY statistics. |
| Multi-file functions, table partitioning, batch claiming | Raw v2 and callbacks | Check file metadata, partition values, batch order, and callback lifetimes. |
| Replacement scans, file systems, logging, extension entry points | Raw v2 | Check callback lifetime and custom file operations. Loadable-extension packaging needs separate validation. |

Some features use SQL or the raw API without a dedicated `duckdb-simple`
wrapper. Appender access uses the v1 API in this snapshot. Check extension
availability and final API changes again at release time.

The preview tests exercise triggers, DML CTEs, nested schemas, variables, JSON
mutation, lambdas, NEAREST joins, recursive aggregation, and storage version
2.0. They also check VARIANT storage and Parquet shredding. The raw v2 tests
check instance lifetimes, prepared statements, streaming, cancellation,
callbacks, and Arrow ownership.

The selected preview's Quack extension downloads returned HTTP 403 from
`core_nightly` and HTTP 404 from `core` in this environment. The public Haskell
API reached the installer. The download errors prevented it from loading Quack.
CONNECT, authentication, remote parameter binding, and remote streaming remain
unverified. Run a local server test when a compatible signed extension is
available. See the [extension installation documentation](https://duckdb.org/docs/preview/extensions/installing_extensions).

The first macOS preview CI run passed both FFI suites and the leak test. The
high-level test process stopped during the nested streaming test without an
assertion or exception message. This failure is unresolved. Confirm the cause
and pass the macOS suite before release.

## Final release checklist

1. Confirm the final `v2.0.0` tag and release notes. Refresh the snapshot and
   regenerate bindings. Review signature, enum, callback, and ownership changes.
   Recheck result-rendering cleanup after an error. Update the documented counts.
   Resolve every failed audit.
2. Fetch the final native archives for each supported platform. Record their
   SHA256 digests in `duckdb-ffi/Setup.hs`. Verify headers, exported symbols,
   architecture, loader paths, and `duckdb_library_version` against those archives.
   Check glibc Linux x86_64/arm64 and macOS. Keep untested native platforms outside
   the supported download matrix.
3. Set the final native version, package versions, and dependency bounds. Update
   runtime tests, README support claims, CONTRIBUTING, migration notes, and both
   changelogs. Decide the supported native range from tested final versions.
   Preserve the existing 1.5 path until the 2.0 path passes the release checks.
4. Test all supported GHC versions on Linux and macOS with the final archives.
   Include the preview API flag, optimization-disabled conversions, Arrow and
   DataFrame integration, callbacks, cancellation, leak checks, and Valgrind.
   Run `cabal build all` before `cabal test all`, as CONTRIBUTING requires.
5. Update the pinned Nix DuckDB input and its version assertions. Run the build
   and tests in the Nix environment. Update the Docker native library and run
   its smoke checks. Verify that neither path downloads a different library.
6. Run `fourmolu`, `cabal-gild`, `cabal check`, and Haddock. Build source archives
   for both packages. Confirm the archives contain generated modules, C adapters,
   preview headers, API specifications, and provenance. Extract them into a clean
   directory and build/test there against the final native library.
7. Re-run the API check and the final-native tests after any release adjustment.
   Publish only after these checks pass and the release is explicitly authorized.
   `scripts/release.sh` uploads package candidates even without `--publish`.
   Local archive preparation does not need that script.

These commands build and check the packages locally without uploading:

```sh
cabal build all
cabal test all --test-show-details=direct
cabal run duckdb-simple-leak-test -- all 100
cabal haddock all
cabal sdist all
```

Select the final native installation and enable `duckdb-v2` for the 2.0 run.
Use the existing DataFrame and Nix commands in CONTRIBUTING for those checks.
