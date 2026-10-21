# DuckDB 2.0

DuckDB [2.0.0](https://github.com/duckdb/duckdb/releases/tag/v2.0.0) was released
on 2026-10-21. Its final tag is the same commit as the audited October 20
snapshot, so the generated API and ABI checks match the final release.

## Pinned API

The client headers in `duckdb-ffi/cbits/duckdb.h` and `duckdb_v2.h` come from
[d54cbbe3df17e8e5ce68312f483b4155836112aa](https://github.com/duckdb/duckdb/commit/d54cbbe3df17e8e5ce68312f483b4155836112aa)
at the upstream `v2.0.0` tag. The source commit is dated 2026-10-20.
`duckdb-ffi/cbits/duckdb-LICENSE` contains the upstream license.
The headers and license are unmodified upstream files. Their source paths and
SHA256 digests are in `duckdb-ffi/vendor/duckdb-api.json`, which also pins the
native source archive used by Nix.

Builds use the checked-in headers directly. Cabal installs both client headers,
`duckdb_arrow.h`, and `duckdb-LICENSE` with the library. Package users do not
need header extraction tools, Python, LLVM, or hs-bindgen. The header audit and
runtime download helper are maintainer tools outside the Hackage packages.

The v1 header declares 548 functions, 21 callback typedefs, and nine inline
callback fields. Compared with the
original 1.5.6 header, it adds `duckdb_create_timestamp_tz_ns`,
`duckdb_get_timestamp_tz_ns`, type tag `DUCKDB_TYPE_TIMESTAMP_TZ_NS = 42`, and
error tags `DUCKDB_ERROR_DATA_CORRUPTION = 43` and
`DUCKDB_ERROR_RESOURCE_IN_USE = 44`. It removes no v1 functions and
changes no existing v1 function signatures or structure definitions.
The v2 header declares 620 functions and 45 callback typedefs. It also has six
Arrow callback fields, for a total of 51 callbacks.

`duckdb-ffi` 2.0.0.0 and `duckdb-simple` 0.4.0.0 target DuckDB 2.0.
The package version selects the supported native API. There is no preview
build flag. Earlier package releases remain available for DuckDB 1.5.
`Database.DuckDB.FFI.V2` exposes the v2 API. Keep v1 and v2
handles separate. Their result, connection, error, and ownership contracts differ.
The raw API exposes native primitives. `duckdb-simple` uses V2 handles for its
connection, statement, result, conversion, callback, and Arrow backends. The
query and execute entrypoints keep their public signatures. Shared environments
and instances let connections share catalogs and configuration. Ad-hoc helpers
parse all input before execution; a syntax error prevents earlier statements
in the input from running.

Both client APIs are generated with `hs-bindgen` 1.0. The package exposes
`Database.DuckDB.FFI` and `Database.DuckDB.FFI.V2`. The former replaces the old
aggregate and Deprecated module. The V2 Functions and Types submodules are no
longer exposed. Each generated module has its own native types. Equal C layouts
do not make the corresponding Haskell types interchangeable. In particular,
keep handles and enums from the two APIs separate. File flag 4 means exclusive
creation in v1 and truncation in v2.

The generator supplies layouts, typed callbacks, const pointers, and adapters
for structures passed by value. All 620 V2 functions are unstable and must
match the native snapshot. The
generated C source enables `DUCKDB_V2_API_ALLOW_UNSTABLE` explicitly. Ownership,
callback lifetimes, exception handling, Arrow release operations, and value
conversion remain in the calling code. Cabal builds the generated modules.

Generate the artefacts before building a checkout:

```sh
nix-shell dev/nix/generate.nix --run 'dev/bindings/generate-bindings.sh --check'
```

Generation verifies identical Haskell output for x86_64 and aarch64 Linux,
x86_64 and arm64 macOS, and the native target. The generated C files contain
508 retained-API and 642 V2 ABI assertions. Compilation checks their sizes,
alignments, and field offsets against the pinned headers. These checks do not
replace runtime ownership tests.

The generated Haskell modules and ABI-check C files are build artefacts in
`duckdb-ffi/generated/`. Git ignores that directory. CI generates and checks
the artefacts, then passes them to the build jobs. `generated.sha256` records
the expected output. Source archives include the generated files. Package
users do not need the generator or Clang. Local Docker and Nix builds require
these artefacts too.

Raw v2 callers must follow the ownership rules in the header documentation.
Do not destroy borrowed handles. Keep callback function pointers alive until
DuckDB releases the registration. Catch Haskell exceptions inside callbacks
and report failures through the native error handle.

After a `duckdb_v2_result_render_box` error, check the result slot and destroy
any owned handle that remains. The pinned implementation checks the render mode
and view arguments before it transfers ownership. These argument errors leave
the result intact. After transfer, it clears the slot on success and failure.
See the [validation before transfer](https://github.com/duckdb/duckdb/blob/d54cbbe3df17e8e5ce68312f483b4155836112aa/src/main/capi/v2/capi_v2_result.cpp#L464).
Recheck this ownership behavior against the final runtime.

The [upstream versioning rules](https://github.com/duckdb/duckdb/blob/d54cbbe3df17e8e5ce68312f483b4155836112aa/api_spec/VERSIONING.md)
define stable, deprecated, unstable, and removed lifecycle states. Headers default
to the newest specified version. To use unstable declarations, enable them
explicitly and target the newest API. Extension headers use versioned function
tables. Ordinary clients link exported functions. Header coverage alone does not
verify that a Haskell shared library can load as a DuckDB extension.

## Final release verification

The final tag, published SDK, and supported shared-library archives contain
the same client headers as the audited snapshot. Fresh generation matches
all four stored checksums. The final SDK passes all 1,150 ABI assertions.

On Linux with Nix GHC 9.14.1, the official 2.0.0 binary passes all 609
retained-backend functional checks and the default leak suite. The installer
checks pass for cold download, warm cache, wrong digests, incomplete caches,
and concurrent publication. The three supported archive digests and their
library paths are verified. Final native runtime checks on other systems
remain part of CI validation.

## October 20 API refresh

The source archive SHA256 is
`93e3d1f9c84b6ec3493237d21565c01c2c19b9862e54e5552d3d7999ccc3bbb4`.
This immutable source archive also matches the final 2.0.0 tag. Its digest
is separate from the shared-library archive checksums in the native installer.

Compared with the October 7 pin, the retained API keeps all function signatures.
V2 adds 172 functions, removes 134, and changes five existing signatures. Most
constructor pairs now use one borrowed factory. Get the factory from an instance,
connection, or callback context. Register completed builders with an explicit
connection or extension target. Configuration uses borrowed config handles.

`duckdb_v2_data_chunk_create` and `duckdb_v2_value_create_null` gain a factory
argument. `duckdb_v2_instance_attach` takes the name before the path. Detach and
set-default operations now take identifiers. Scalar return-type resolution and
table scan metadata have dedicated callbacks. Scope names change from AUTOMATIC
to DEFAULT and from LOCAL to SESSION. OPTION_TARGET_SCOPE is removed.

Eight callback typedefs, fifteen opaque handle types, log and macro enums, and
the metadata-deferred error are new. Existing value structures and callback
signatures remain unchanged. No surviving enum constant is renumbered. The
extension function-pointer table changes with the API and requires a rebuild.

Validation on 2026-10-20 used Nix GHC 9.14.1 on Linux x86_64 and a native
source build at this pin. The library and CLI report `v2.0.0-dev0` with source
ID `d54cbbe3df`.

- Generation, checksum verification, and the complete API audit pass. Both
  generated modules and their C ABI checks build with `-Wall -Werror`.
- All 97 retained API tests and all 19 V2 tests pass.
- All 19 V2 tests pass under Valgrind 3.26 with zero errors and zero definite,
  indirect, or possible leaks. The run uses `--fair-sched=yes`,
  `--undef-value-errors=no`, and one Haskell runtime capability.
- A fresh FFI source archive contains both generated modules and both C ABI
  checks. All four generated checksums pass after extraction.
- The installer integration fixture verifies complete tar extraction,
  versioned symlink targets, warm-cache reuse, and rejection of wrong checksums
  or missing targets before publication.

The committed-code audit on 2026-10-21 also checks the retained backend.
Its full functional and default leak suites pass against the matching native
library. The geometry fixtures now require the native rejection of mixed Z/M
collection members. The standalone WKB codec still preserves those layouts.
Client SQL tests use explicit `SELECT`; SELECT-less expressions use shell
parsing only.

The project-owned Arrow header now uses the standard Arrow interface guards.
Installed consumers can include it with `duckdb_v2.h` in either order.
Fresh regeneration, checksum verification, API coverage, installer fixtures,
and static and dynamic archive consumers pass. Package consumers do not need
LLVM or hs-bindgen.

The source-build CI audit at this pin also passes on Linux with GHC 9.6.7,
9.8.4, 9.10.3, 9.12.4, and 9.14.1, and macOS with GHC 9.14.1.
These results precede the final binary archive validation.

## Previous binding validation

The following results apply to the October 7 pin `13d1d8f56a`. They do not
establish runtime compatibility with the October 20 API.

Local validation on 2026-10-16 used Nix GHC 9.14.1 on Linux x86_64 and the
then-pinned native `2.0.0-dev0` library. The build uses `-Wall -Werror`.

- All 97 retained raw, 19 V2, 484 high-level, and six DataFrame tests
  pass. The default leak suite passes.
- All 19 V2 cases pass under Valgrind with zero errors and zero definite,
  indirect, or possible lost bytes. Use `--fair-sched=yes`. The first query
  stalled with default Valgrind scheduling. The same query also passes in a
  separate native C probe.
- All seven managed registration and five COPY fixtures pass under Valgrind
  with zero errors and zero definite, indirect, or possible lost bytes. They
  cover default/named parameters, grouped and window aggregates, table state,
  custom types and casts, replacement scans, statistics, partition paths,
  COPY FROM, and multi-file reads.
- Haddock builds for both packages. Generated links produce type/constructor
  ambiguity and unresolved-link warnings. Review those before release.
- Regeneration matches all four stored checksums. The coverage audit passes.
  Eleven temporary omission probes confirm that the audit rejects missing
  exports, C shims, native calls, and callback conversions.
- Both extracted source packages install without LLVM or hs-bindgen. Separate
  consumers pass with static and dynamic Haskell linking and the loader
  environment unset. They check both raw APIs, shared instances, named/default
  scalar registration, and COPY callbacks. Installed header constants, all
  three headers, the native license, and generated hashes match their inputs.
  Both Haskell linking modes use the supplied shared native DuckDB library.

The platform matrix is configured in CI. Runtime checks on other compilers,
macOS, and ARM64 remain for CI. Matching generation output across targets does
not replace those runtime checks. Docker and the complete Nix derivations were
not rebuilt during this local validation.

## Audit and refresh

These commands use Python's standard library. The checks run offline:

```sh
uv run dev/bindings/duckdb-api.py --check
uv run dev/bindings/duckdb-api.py --report > /tmp/duckdb-api-report.json
uv run dev/bindings/duckdb-api.py --compare-headers /absolute/path/to/native/include
```

`--verify-headers` verifies the pinned source metadata and each client file digest.
It does not need generated bindings. `--check` also verifies generated coverage.
For every v1 and v2 function, it requires an exported generated binding, its FFI
shim, and the native call in that shim. It also requires every callback typedef
and inline callback field, with both callback conversions. `--report` lists each
covered function, callback type, and omission. `--compare-headers` compares both
APIs with another header directory. It compares function types without parameter
names and complete typedefs, including enumerations and structure definitions.
Run compilation, ABI checks, and behavior tests in addition to the coverage checks.

Select an upstream commit or release tag before refreshing. For this release,
use `v2.0.0`. Generate output with the pinned Nix toolchain and review any
checksum changes.

```sh
uv run dev/bindings/duckdb-api.py --refresh v2.0.0
nix-shell dev/nix/generate.nix --run 'dev/bindings/generate-bindings.sh'
uv run dev/bindings/duckdb-api.py --check
uv run dev/bindings/duckdb-api.py --report > /tmp/duckdb-api-final.json
```

`--refresh` resolves the ref to a full SHA before it downloads any files. It
reports function and type changes for both APIs against the current verified pin.
It replaces the client headers, upstream license, and per-file manifest. Review
the plain-file diff, manifest, and generated code. Upstream files remain unmodified.
Use the recorded SHA for subsequent downloads and builds.
Use `v2.0.0` for the released API. Do not select later release-branch commits.

The historical October 7 Linux preview fetched during preparation reports
`v2.0.0-alpha45189`. Its archive SHA256 is
`5d08f26d7e5f07fcae3d315c039a09da627f52639aeace3af06abc5456e35132`.
`PRAGMA version` reports source ID `13d1d8f56a` and codename `Cyanoptera`.
At that pin, both client headers in the native archive matched the checked-in
headers. This records the earlier validation binary. Its former download URL now returns
404. The [preview installation links](https://duckdb.org/install/preview)
can move and can serve an older binary. Use the final release archives below.
If you use another binary, verify its digest, headers,
exported symbols, and reported version before testing.
This preview works directly with both APIs. Set
`DUCKDB_TEST_VERSION=2.0.0-alpha45189` for its test run.

The final tag uses the audited October 20 source commit. The historical
October 7 binary does not match this V2 API. The release branch and `main`
can contain later changes. Use the final tag for this package release.

The preview source builder has been removed. The runtime helper downloads
the official final shared library and CLI. It verifies their pinned SHA256
digests. The header audit compares the SDK with the checked-in headers.
These tools do not run during ordinary package builds.

Download and test the final release with:

```sh
dev/fetch-duckdb.sh /tmp/duckdb-2.0-native
uv run dev/bindings/duckdb-api.py --compare-headers /tmp/duckdb-2.0-native/native
cabal build all -fsystemlib --extra-lib-dirs=/tmp/duckdb-2.0-native/native
DUCKDB_TEST_VERSION=2.0.0 cabal test all -fsystemlib --extra-lib-dirs=/tmp/duckdb-2.0-native/native --test-show-details=direct
```

The `.github/workflows/duckdb-2.0.yml` job and Docker use the final release
archives. Nix builds the same final source pin with explicit version 2.0.0:

```sh
nix-build dev/nix/ci.nix --no-out-link
docker build --tag duckdb-haskell:2.0 .
```

These commands build local artifacts without uploading or deploying them.

## Feature coverage

The current client suites pass 633 functional tests and the default leak
suite against the verified official `v2.0.0` Linux x86_64 library. See
[V2 migration validation](../dev/v2-migration.md) for the counts and native
memory checks. The Quack, CONNECT, and remote-cancellation observations below
use the earlier October 7 runtime. They require separate validation with
compatible extensions.

The SQL feature list follows the [upstream 2.0 preview](https://duckdb.org/2026/08/17/duckdb-20-highlights).
The C API list follows the pinned headers and specifications. SQL support depends
on the native build and its installed extensions.

| Feature | Access from this repository | Release validation |
| --- | --- | --- |
| Triggers, DML in CTEs, nested schemas, NEAREST joins, variables, JSON mutation, recursive CTE additions | Existing `query` and `execute` SQL paths | Execute representative SQL against the final runtime. |
| PEG parser and lambda syntax changes | Existing SQL paths | Check parameters, multi-statement extraction, quoted names, and parser errors. Bare expression statements use the CLI's shell grammar; client SQL needs `SELECT`. The [parser extension API](https://duckdb.org/2026/08/20/duckdb-20-peg-parser) remains a preview. |
| Quack, CONNECT, remote pushdown | SQL with the required extensions | Test a local server separately. Check authentication and extension availability. |
| VARIANT and GEOMETRY | V2 binding and decoding, SQL metadata, native extraction and casts, and geometry codecs | Check nested values, scalar precision, object order, Parquet round trips, and configured custom CRS names. Native geometry collections require one Z/M layout. |
| Nanosecond TIMESTAMPTZ | V2 value access and typed conversion | Check sub-microsecond precision and temporal infinity. |
| Asynchronous I/O, storage format 2.0, query optimizations | Native engine through existing SQL paths | Check file reopen, cancellation, and older database reads. State the new write format. |
| Timezones and collations | SQL and existing temporal types | Check timezone conversion and extension loading. |
| Extension repositories | SQL | Check local signed repository loading separately. |
| Environment, instances, connections, attachment options, schemas, catalog | `duckdb-simple`, `Config`, `Catalog`, and raw V2 | Check lifetime order, resource-in-use errors, attach/detach, and option discovery. |
| SQL tokenization, statements, prepared statements, expressions, result cursors, Arrow results and streams | `Tokenizer`, query and streaming APIs, `Arrow`, and raw V2 expression operations | Check token iteration, bind/execute, chunk exhaustion, cancellation, errors, and Arrow release callbacks. |
| Values, logical/custom types, vectors, data chunks, arenas, column collections | Typed conversions, `LogicalRep`, `Registration`, and raw V2 vector operations | Check widths, buffer layouts, nulls, borrowed data, and destruction. |
| Scalar, aggregate, table, cast, COPY functions and signatures | Managed `Registration` and `Copy` callbacks, typed signatures, and raw V2 | Check registration and execution, parameter kinds, named/default arguments, callback errors, state destruction, and COPY statistics. |
| Multi-file functions, table partitioning, batch claiming | Managed `Copy` multi-file setup and `Registration.TableCallbacks` | Check file metadata, partition values, batch order, and callback lifetimes. |
| Replacement scans, file systems, logging, extension entry points | Managed `Registration`, `FileSystem`, `Logging`, and extension-target builders | Check callback lifetime, log-sink cleanup, and file operations. V2 has no custom file-system registration. Loadable-extension packaging needs separate validation. |
| Custom options, macro and pragma functions, function documentation, query profiles | `Config`, `Macro`, `Pragma`, `FunctionDocs`, and `QueryMetrics` | Check typed defaults and scopes, callback errors, extension targets, and copied metric ownership. |

Some features use SQL or the raw API without a dedicated `duckdb-simple`
wrapper. Appender access uses the v1 API in this snapshot. Check extension
availability and final API changes again at release time.

The preview tests exercise triggers, DML CTEs, nested schemas, variables, JSON
mutation, lambdas, NEAREST joins, recursive aggregation, and storage version
2.0. They also check VARIANT storage and Parquet shredding. The raw v2 tests
check instance lifetimes, prepared statements, streaming, cancellation,
callbacks, and Arrow ownership. They also execute grouped and window
aggregates, table functions, custom casts, COPY TO callbacks, vector mutations,
custom configuration, filesystem operations, and context logging. An independent
native compiler check verifies all function and callback signatures.
These checks cover representative behavior. They do not execute every raw
function. Managed registration and COPY fixtures also check COPY FROM,
partitioned output, statistics, and multi-file reads. Small fixtures do not
guarantee a progress callback invocation. Extension targets compile, but a
loadable Haskell extension still needs separate packaging validation.

The selected preview's Quack extension downloads returned HTTP 403 from
`core_nightly` and HTTP 404 from `core` in this environment. The second review
built Quack from commit
[f964cece8ecfe9006a607aa0a3e9b9030296c54f](https://github.com/duckdb/duckdb-quack/commit/f964cece8ecfe9006a607aa0a3e9b9030296c54f)
against the pinned DuckDB source. It also built `httpfs` from commit
[5e34903685e4d429cbb19b063406abdd8ce30591](https://github.com/duckdb/duckdb-httpfs/commit/5e34903685e4d429cbb19b063406abdd8ce30591)
with the two patches selected by that DuckDB source. Quack needs a writable
crypto provider from `httpfs`, even when the server uses an explicit token.
The local builds require OpenSSL and libcurl development files.

The authenticated loopback check passes unparameterized CONNECT queries,
remote reads and writes through ATTACH, and a streamed fold of 5,000 remote
rows. Bound ATTACH queries require
`SET disabled_optimizers='remote_pushdown'` in this snapshot. Default pushdown
sends unresolved placeholders to the remote server during preparation.
Parameterized CONNECT execution is also unsupported by the
[pinned native engine](https://github.com/duckdb/duckdb/blob/13d1d8f56a82b3413055819eb90d0b0e68b5c847/src/main/client_context.cpp#L646).
The binding preserves the native error and does not change optimizer settings.
Use ATTACH with the checked setting for parameterized remote queries.
Recheck both paths, signed extension installation, and TLS with the final
release. The local check uses unsigned source-built fixtures on loopback.
Remote cancellation also needs a release check. In a native-only CONNECT
query, interrupting the client did not release a running remote aggregate
within five seconds. The binding waits for the native worker before it frees
resources. It cannot promise prompt cancellation while that worker is blocked.

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

The macOS crash report shows a native stack overflow in `Value::ToSQLString`
while DuckDB generates a default column name for a deeply nested VARIANT
constant. A native-only Linux probe also fails at depth 256 with a 544 KiB
thread stack. Adding `AS payload` avoids that recursive name generation.
The decoder regression keeps the depth-128 and depth-256 values and uses that
alias. This does not fix arbitrary native queries with the same expression.
Confirm the workaround in macOS CI and recheck the engine at release time.

UTC parameters now follow a prepared `TIMESTAMPTZ_NS` type, including leaves
inside collections, STRUCTs, MAPs, and UNIONs. Untyped UTC parameters retain
microsecond TIMESTAMPTZ behavior. Finite values outside the nanosecond range
raise an encoding error. Infinity remains available through `UTCTimestamp`.
The library refreshes cached metadata when it resets or rebinds a statement.
A reused statement follows a column changed to TIMESTAMPTZ_NS. A parameter
used with both microsecond and nanosecond casts has no single prepared type.
Use separate parameters for those expressions to retain the intended precision.

VARIANT objects can bind an empty key alongside named keys in this preview.
The C constructor cannot represent an object with only an empty key; it
produces a tuple that casts to an array. The binding rejects that shape change.
Binding duplicate or NUL-containing object keys also raises errors. The pinned V2 API
has no direct structural VARIANT accessor. The backend uses batched SQL type
metadata, key extraction, and native casts to decode scalar and nested payloads
without losing precision. Decoding supports empty and NUL-containing object
keys through bound extraction parameters. Object keys follow DuckDB's native
lexicographic order. Arrays retain element order. The decoder uses public V2
operations. Native factories construct VARIANT types directly. The helper
connection caches configured GEOMETRY types. Set `connectionGeometryCRS` to
preserve arbitrary custom CRS names in nested parameters. Helper queries do
not interrupt the caller's active stream.

## V2 migration limits

The migration uses the pinned API. It does not add native symbols. The missing
operations are documented in the package README and report controlled errors:

- Prepared expanding statements, including dynamic PIVOT, have deferred schemas
  and cannot have parameters or require a cacheable plan. The backend fetches
  before reading their output schema.
- V2 cannot register custom file systems. Native log sinks support managed write
  and flush callbacks. Native logging contains and discards callback errors.
- The native type builder rejects zero-length fixed ARRAY types. Empty LIST
  values remain supported.
- Native GEOMETRY collections require the same Z/M dimensions on all members,
  including empty members. Mixed-layout WKB still round trips through the
  standalone codec. Native import raises `SQLError` without normalization.
- Direct GEOMETRY type construction requires a recognized CRS identifier or a
  complete definition. The high-level type cache also preserves custom names
  configured through `connectionGeometryCRS`. Unconfigured unknown names raise
  errors. The binding rejects construction that silently drops CRS metadata.
- The metadata API cannot perform scoped collation lookup. Other catalog
  lookups use SQL metadata and accept a top-level schema identifier. Table
  descriptions use native V2 handles and cannot address nested schemas.

## Final release checklist

1. Confirm the final `v2.0.0` tag and release notes. Refresh the snapshot and
   regenerate bindings. Review signature, enum, callback, and ownership changes.
   Recheck result-rendering cleanup after an error. Update the documented counts.
   Resolve every failed audit.
2. Fetch the final `duckdb-shared-libs-PLATFORM.tar.gz` archives for each
   supported platform. Check the complete versioned library and symlink closure.
   Record their SHA256 digests in `duckdb-ffi/Setup.hs`. Verify headers, exported symbols,
   architecture, loader paths, and `duckdb_library_version` against those archives.
   Check glibc Linux x86_64/arm64 and macOS. Keep untested native platforms outside
   the supported download matrix.
3. Set the final native version, package versions, and dependency bounds. Update
   runtime tests, README support claims, CONTRIBUTING, migration notes, and both
   changelogs. Decide the supported native range from tested final versions.
   Keep the 1.5 package release available to existing applications.
4. Test all supported GHC versions on Linux and macOS with the final archives.
   Include both C API generations, optimization-disabled conversions, Arrow and
   DataFrame integration, callbacks, cancellation, leak checks, and Valgrind.
   Run `cabal build all` before `cabal test all`, as CONTRIBUTING requires.
   Run the Quack suite with matching extensions. Recheck prepared parameters
   with default pushdown and CONNECT. Verify remote cancellation and TLS.
5. Update the pinned Nix DuckDB input and its version assertions. Run the build
   and tests in the Nix environment. Update the Docker native library and run
   its smoke checks. Verify that neither path downloads a different library.
6. Run `fourmolu`, `cabal-gild`, `cabal check`, and Haddock. Build source archives
   for both packages. Confirm the archives contain generated modules, C adapters,
   client headers, the upstream license, and per-file provenance. Extract them into a clean
   directory and build/test there against the final native library.
   Install the extracted FFI package. Check both headers and the native license
   with a separate consumer under normal and dynamic Haskell linking.
7. Re-run the API check and the final-native tests after any release adjustment.
   Publish only after these checks pass and the release is explicitly authorized.
   `dev/release.sh` regenerates both APIs and verifies their checksums and
   coverage before it builds and uploads the Hackage archives.
   `dev/release.sh` uploads package candidates even without `--publish`.
   Local archive preparation does not need that script.

These commands build and check the packages locally without uploading:

```sh
nix-shell dev/nix/generate.nix --run 'dev/bindings/generate-bindings.sh --check'
cabal build all
cabal test all --test-show-details=direct
cabal run duckdb-simple-leak-test -- all 100
cabal haddock all
cabal sdist all
```

Select the final DuckDB 2.0 native installation.
Use the existing DataFrame and Nix commands in CONTRIBUTING for those checks.
