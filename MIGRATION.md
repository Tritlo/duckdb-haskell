# Migration Guide: DuckDB 1.5 and hs-bindgen

This guide covers the changes needed when upgrading this repository from the
DuckDB 1.4 line to the DuckDB 1.5 line.

The short version is:

- `duckdb-ffi` uses the DuckDB `1.5.6` C header. The native minimum is 1.5.3.
- `duckdb-simple` now depends on `duckdb-ffi-1.5`.
- The raw FFI is generated with hs-bindgen 1.0. It keeps `DuckDBFoo` type names
  and `c_duckdb_*` function names in `Database.DuckDB.FFI`.
- The runtime `libduckdb` must be >= 1.5.3 and < 1.6. High-level SQL operations
  retain their behavior. Types that expose raw FFI values change.

## Native DuckDB 1.5.6

The default native download is DuckDB 1.5.6. Native DuckDB >= 1.5.3 and < 1.6
remains supported. `duckdb-simple` uses a separate API version.

The FFI adds the GEOMETRY and VARIANT type tags, the geometry CRS accessor,
and the COPY_DATABASE, UPDATE_EXTENSIONS, and MERGE_INTO statement tags.
Free strings returned by the CRS accessor with `c_duckdb_free`.

## Geometry support

`duckdb-simple-0.3.0.0` adds Geometry support.

Use `Data.Geometry.Geometry` from the published `geometry-simple` package for
decoded shapes. Use `RawGeometry` for WKB bytes and CRS metadata. The package's
`Geometry` type has no CRS. It has parameter and result instances.
`RawGeometry` has a result instance. Import its WKB and CRS with explicit SQL.
See [the Geometry notes](duckdb-simple/README.md#geometry) for examples and limits.

`FieldValue` gains `FieldGeometry`, and `LogicalTypeRep` gains
`LogicalTypeGeometry`. Update exhaustive matches.

The C API cannot create a GEOMETRY type with a CRS. Each connection reads
GEOMETRY types for a list of CRS definitions the first time a parameter needs
one, and composite parameters use them. The list defaults to `OGC:CRS84`. Set
it with `ConnectionOptions`, `openWithOptions`, or `withConnectionWithOptions`.
A CRS outside the list binds without a CRS, and `logicalTypeFromRep` creates
`GEOMETRY` with no CRS. To apply a CRS in these cases, insert the value into a
column with that CRS, or cast it in SQL.

## VARIANT support

`duckdb-simple-0.3.0.0` adds VARIANT support. VARIANT results decode to the
`FieldValue` of the stored value, so existing `FromField` instances read them.
Objects decode to `FieldStruct` values with VARIANT fields. `Variant` from
`Database.DuckDB.Simple.Variant` wraps the `FieldValue` of a result. Native
VARIANT loses geometry CRS metadata. Import raw geometry payloads with
`ST_GeomFromWKB(?)::VARIANT`. See [the VARIANT
notes](duckdb-simple/README.md#variant) for private-format and
persistent-storage requirements.

`Variant` binds as a VARIANT parameter. The connection reads the VARIANT type
the first time a parameter needs it, because the C API cannot create a usable
VARIANT type.
`Variant` has no `ToDuckValue` instance, and `logicalTypeFromRep` raises an
error for VARIANT.

Array elements require `ToField` and `DuckDBColumnType`. Arrays use each
element's `ToField` instance. If a custom element type only has `ToDuckValue`,
add `instance ToField YourType`. The default implementation requires `Show`.

## Binding fixes

- Default cursors and folds use DuckDB's supported materialized execution API. They
  decode one Haskell row at a time, but native result memory depends on the
  result size. To request native streaming, import
  `Database.DuckDB.Simple.Deprecated.Streaming` qualified. Its row and Arrow
  folds emit a deprecation warning because they use DuckDB's deprecated
  streaming execution entry point. The raw bindings remain in duckdb-ffi.

- `UTCTime` parameters have SQL type TIMESTAMPTZ. Use `LocalTime` for TIMESTAMP.
- `Float` parameters have SQL type FLOAT. REAL values decode to Float or Double.
- Word and Word64 scalar function results have SQL type UBIGINT.
- Scalar callbacks receive NULL arguments. Use `Maybe` to accept NULL.
  A non-nullable Haskell argument produces a conversion error for NULL.
- Use `Date`, `LocalTimestamp`, or `UTCTimestamp` from
  `Database.DuckDB.Simple.Time` for columns that contain temporal infinity.
  These types use `NegInfinity`, `Finite value`, and `PosInfinity`.
  Ordinary `Day`, `LocalTime`, and `UTCTime` reject infinity with a conversion
  error. Finite inputs outside DuckDB's storage range still fail.
- `FieldDate`, `FieldTimestamp`, and `FieldTimestampTZ` now hold `Unbounded`
  payloads. For example, change `FieldDate day` to `FieldDate (Finite day)`.
  Custom decoders receive infinity directly in these constructors.
- Text and String values preserve embedded NUL. SQL text and native names
  reject NUL because the C API requires terminated strings.
- After EOF, the cursor remains exhausted until bindings are reset. An
  additional `nextRow` does not execute the statement again.
- Generic records and sums match SQL field and member names. An incompatible
  schema fails instead of decoding values by position.
- Update applications that use the FFI directly to the generated types.
  Import `Database.DuckDB.FFI`.

## Who Needs to Change What

If you use `duckdb-ffi` directly:

- Rebuild and relink against DuckDB `1.5.6`.
- Update any packaging, Nix, CI, Docker, or deployment config that still pulls
  a 1.4 `libduckdb`.
- Import generated functions and types from `Database.DuckDB.FFI`. Update enum
  constants, handle representations, const pointers, and struct arguments/results.

If you use `duckdb-simple`:

- Rebuild against `duckdb-simple-0.3.0.0` and DuckDB `1.5.6`.
- Review the binding changes above for SQL parameter types, NULL handling,
  and generic decoding.
- New 1.5 helpers are available from dedicated modules instead of being folded
  into the core query API.

## Runtime Compatibility

The biggest practical change is the runtime baseline.

Before:

- The packages were validated against DuckDB 1.4.x.

Now:

- `duckdb-ffi-1.5.6.0` and `duckdb-simple-0.3.0.0` require a DuckDB 1.5
  shared library >= 1.5.3 and < 1.6 at runtime.

If your executable still finds a 1.4 shared library first, you will see symbol
lookup failures for new 1.5 APIs such as config, catalog, or logging symbols.

Typical places to update:

- `LD_LIBRARY_PATH`
- Docker images
- CI build images
- Nix derivations
- system packages
- Cabal `extra-lib-dirs`

Example test invocation:

```bash
LD_LIBRARY_PATH=/path/to/duckdb-1.5 \
  cabal test --extra-lib-dirs=/path/to/duckdb-1.5
```

## `duckdb-ffi` Migration

hs-bindgen 1.0 generates all 546 native functions from the pinned DuckDB 1.5.6
header. It also generates struct layouts, the VARCHAR union, Arrow records,
enum patterns, callback constructors/invokers, and C ABI wrappers.
`Database.DuckDB.FFI` contains the native API.
The 44 handwritten modules, 22 manual `Storable` instances, C shims, and old
binding generator are removed. The package ships generated source and C ABI
assertions. Maintainers regenerate both files from the pinned headers.
Consumers do not run the generator.

### Names and types

| Previous raw API | Generated API |
| --- | --- |
| `c_duckdb_open` | `c_duckdb_open` |
| `DuckDBConnection` | `DuckDBConnection`, a typed pointer newtype |
| `DuckDBIdx` | `DuckDBIdx`, a `Word64` newtype |
| `DuckDBTypeInteger` | `DUCKDB_TYPE_INTEGER` |
| `CString` for a const string | `ConstPtr CChar` |
| Scalar temporal wrappers | Records such as `DuckDBDate` and `DuckDBTimestamp` |
| Handwritten record selectors | Record fields through `OverloadedRecordDot` |
| Pointer/out-parameter ABI shims | Direct struct arguments and results |

The C typedef `duckdb_type` is a separate generated `DuckDBType` newtype around
`DUCKDB_TYPE`. For example, create an INTEGER logical type with
`c_duckdb_create_logical_type (DuckDBType DUCKDB_TYPE_INTEGER)`.
Unwrap `DuckDBType` when inspecting `c_duckdb_get_type_id`.
Do not assume that a handle is a `Ptr ()` or compare it directly with `nullPtr`.
Use its generated constructor, or its `unwrap` field where necessary.

Functions with struct results return records directly.
For example, `c_duckdb_from_date` returns `DuckDBDateStruct` instead of writing
an output pointer. `c_duckdb_fetch_chunk` takes a `DuckDBResult` value.
Keep the owning result alive while that copied record refers to native data.
Generated struct marshalling does not transfer ownership.

### Naming configuration

The maintainer driver uses existing hs-bindgen 1.0 APIs. Prescriptive binding
specifications retain all 158 earlier type names. The `RenameTerm` category
option adds `c_` directly to all 546 generated function names. There is no alias
module. Record and newtype constructors use the configured type names.
Enum constants retain their header spellings.

```haskell
import Database.DuckDB.FFI

integerType = c_duckdb_create_logical_type (DuckDBType DUCKDB_TYPE_INTEGER)
```

`RenameTerm` runs after frontend identifier validation and collision detection.
The caller must supply valid, unique names. A fixed `c_` prefix meets these
requirements for DuckDB functions: it is valid and maps distinct names to distinct
names. Types and fields are in the separate `CType` category, so they do not
receive the function prefix.

Retained names do not preserve the previous representations. Handles and
indexes use newtypes. Constant pointers use `ConstPtr`. Struct calls use generated
records. Old record selectors, callback wrapper names, per-topic module paths,
and NULL-default helpers are removed. Constructor field types can also change.

### Const pointers and callbacks

Read a constant string with `ConstPtr` unwrapped for `peekCString`.
Use `ConstPtr` when passing a `withCString` buffer to a constant argument.
Keep the buffer alive through the native call.
An owned string still needs `c_duckdb_free`, even when it has a const-qualified
type. Constness does not specify ownership.

Use `toFunPtr` and `fromFunPtr` from
`HsBindgen.Runtime.Support.FunPtr` for generated callbacks.
A callback typedef wraps a `FunPtr` to its generated `_Aux` function type.
Free an allocated callback with `freeHaskellFunPtr` after native code can no
longer call it. Catch Haskell exceptions inside the callback.
Generated callbacks do not implement ownership transfer or exception cleanup.
The static callback destructors in `duckdb-simple` retain those rules.

Arrow release helpers now live in `Database.DuckDB.Simple.Arrow`.
They still mask asynchronous exceptions and check the release pointer.
Deprecated Arrow handles point directly to Arrow records.
The old synthetic internal-pointer helpers and NULL-default C shims are removed.
Raw callers must supply valid input and output storage.

### Build and maintenance

Package builds use checked-in source. They need a C compiler and the small
`hs-bindgen-runtime` package. They do not need hs-bindgen, LLVM, libclang, or
Doxygen. The two unused API-version predicate macros are excluded from generation.
They were absent from the previous raw API. This also removes `c-expr-runtime`
and its arithmetic dependencies from the package.

Maintainers use `nix-shell dev/nix/generate.nix` and run
`duckdb-ffi/scripts/generate-bindings.sh`. Use `--check` to check committed output.
The script compiles and runs `GenerateBindings.hs` in the Nix shell. It creates
naming and opaque binding specifications from header metadata and the spelling
map. hs-bindgen, libclang, and the generator's compiler are maintainer tools.
The preprocessing entry point is in hs-bindgen's internal library. Keep its
version pinned when you update the driver.
Consumer builds still need a C compiler for the generated ABI wrappers.
The script compares output for Linux x86_64/aarch64 and macOS x86_64/ARM64.
The two release artifacts are `FFI.hs` and `abi-checks.c`. The 508 C static
assertions check generated layouts against the build compiler.
The script refuses output that differs between targets. An incompatible target
fails compilation instead of using incompatible layouts.

The generator uses `OmitFieldPrefixes`, `DuplicateRecordFields`, and
`NoFieldSelectors`. Default field prefixes collide with DECIMAL width/scale
functions and omit eight related functions.
The three untranslated macros contain compiler attributes; Clang still applies
the attributes while parsing the declarations.

Keep the native installer and checksum pins in `Setup.hs`.
Keep high-level ownership, cancellation, conversion, and the private VARIANT
format checks in `duckdb-simple`. They cannot be inferred from a C header.
When updating DuckDB, update the header and native pins, then run the existing
native, compiler, platform, and leak checks. Do not edit generated declarations.

### Binding audit

The `_duckdb_*` records in the C header contain placeholder fields. Native
handles point to private objects. Reading those fields is invalid. The generation
script selects an opaque representation for all 49 placeholder records. It keeps
all 546 functions and the real value and Arrow records.

The audit compared all 546 public signatures, 21 callbacks, 94 old C shim
signatures, and the supported LP64 layouts. It found no signature or layout
mismatch on the supported targets. The old 32-bit `ArrowSchema` formula allocates
36 bytes where the C ABI requires 44 bytes. The generated layout avoids manual
pointer-size formulas. This release does not support 32-bit targets.

Generated enum storage can be unsigned where the previous bindings used `CInt`.
Use the generated enum patterns and typedef wrappers. Equal storage size does
not make their Haskell types interchangeable.

Struct results can contain borrowed pointers. A copied struct is not a copied
native allocation. Keep its owner alive and free each owned allocation once.
The generator checks the ABI. It cannot infer these ownership rules.

### binding-combinators assessment

A native prototype used
[binding-combinators](https://github.com/well-typed/binding-combinators/tree/e9b54b56791538a784cde2fdf796f2838248ab80)
for handle outputs, status checks, UTF-8 input, logical types, and error cleanup.
The library can reduce marshalling code. It does not yet replace this package's
ownership and cancellation code. Its current runtime bound excludes
`hs-bindgen-runtime-1.0`, so the prototype needed a scoped bound override.

A registered DuckDB callback must remain allocated after registration returns.
The library's scoped `funPtrIn` frees it when the call returns. Owned strings
need `duckdb_free`. Native results can need destruction even when their status
indicates failure. Resource acquisition also needs masking before the native
call returns an owned value. These paths need custom marshallers. This revision
does not add binding-combinators as a package dependency.

## `duckdb-simple` Migration

### Existing Query Code

Most `duckdb-simple` code should not need source changes.

The following remain source-compatible:

- `open`, `close`, `withConnection`
- prepared statements and named parameters
- `execute`, `query`, `fold`, `nextRow`
- `ToField`/`FromField`-based row and parameter handling
- existing scalar function registration with `createFunction`

### New Modules

DuckDB 1.5 features are exposed in additive modules:

- `Database.DuckDB.Simple.Config`
- `Database.DuckDB.Simple.Catalog`
- `Database.DuckDB.Simple.FileSystem`
- `Database.DuckDB.Simple.Copy`
- `Database.DuckDB.Simple.Logging`

Import them only where needed. The main `Database.DuckDB.Simple` module stays
focused on the core query interface.

### Opening with Config

If you previously created connections only with:

```haskell
open ":memory:"
```

you can keep doing that.

If you want startup config at open time, switch to:

```haskell
openWithConfig ":memory:" [("threads", "1")]
```

or:

```haskell
withConnectionWithConfig ":memory:" [("threads", "1")] $ \conn -> ...
```

### Stateful Scalar Functions

DuckDB 1.5 adds scalar-function init/state hooks. In `duckdb-simple`, that is
surfaced as `createFunctionWithState`.

Use `createFunction` when:

- your scalar function is pure
- or all state can live in global `IORef`/`MVar` values that you manage

Use `createFunctionWithState` when:

- you want per-worker thread-local state
- you want state initialized once per worker thread, not once per row

Example:

```haskell
createFunctionWithState conn "hs_counter" (newIORef (0 :: Int)) $ \ref -> do
  atomicModifyIORef' ref $ \n ->
    let next = n + 1
     in (next, next)
```

This is an additive feature. Existing `createFunction` users do not need to
rewrite working code.

### Copy Functions

DuckDB 1.5 adds C APIs for custom `COPY` integrations. `duckdb-simple` now
exposes `registerCopyToFunction` in `Database.DuckDB.Simple.Copy`.

Use it when you want to receive rows emitted by:

```sql
COPY (SELECT ...) TO 'path' (FORMAT your_format)
```

The wrapper separates the callback phases:

- bind
- global init
- sink
- finalize

This mirrors DuckDB’s ownership model closely on purpose. If you had no custom
COPY integration before, nothing changes for normal SQL `COPY` usage.

### Logging

DuckDB 1.5 adds custom log storage registration. `duckdb-simple` now exposes
`registerLogStorage` in `Database.DuckDB.Simple.Logging`.

This is optional advanced functionality. Existing applications do not need to
change anything unless they want to capture DuckDB log events.

## Behavior Changes Worth Noting

### `TIME_NS` Decoding

The test suite was updated because DuckDB 1.5 now successfully decodes
`TIME_NS`, where older expectations treated that as unsupported.

For most users this is a strict improvement:

- code that already handled `FieldTime` continues to work
- tests that expected a failure for `TIME_NS` should be updated

### `duckdb_string_t` Handling

Read `c_duckdb_string_t_data` with its explicit length. Do not assume NUL
termination.

If you have downstream helper code using:

```haskell
peekCString ...
```

on `c_duckdb_string_t_data`, switch to a length-aware read such as:

```haskell
peekCStringLen ...
```

using `c_duckdb_string_t_length`.

## Suggested Upgrade Steps

1. Upgrade the Haskell packages to:
   - `duckdb-ffi-1.5.6.0`
   - `duckdb-simple-0.3.0.0`
2. Upgrade the native DuckDB shared library to `1.5.6`.
3. Run your test suite with the 1.5 shared library explicitly selected.
4. Update any tests that expected old 1.4 behavior, especially around
   `TIME_NS` or string helper assumptions.
5. Adopt the new modules only where you need 1.5-specific features.

## Repository Notes

Relevant release notes live in:

- [duckdb-ffi/CHANGELOG.md](duckdb-ffi/CHANGELOG.md)
- [duckdb-simple/CHANGELOG.md](duckdb-simple/CHANGELOG.md)

If you are upgrading code in this repository itself, use those together with
this guide.
