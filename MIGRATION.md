# Migration notes

## DuckDB 2.0

`duckdb-ffi` 2.0 and `duckdb-simple` 0.4 require DuckDB 2.0. Earlier package
releases remain available for DuckDB 1.5. The retained API in
`Database.DuckDB.FFI` supports DuckDB 2.0.x releases. The V2 API in
`Database.DuckDB.FFI.V2` requires exactly DuckDB 2.0.0, because upstream marks
it as unstable. `duckdb-simple` 0.4 uses the V2 API, so it also requires
exactly DuckDB 2.0.0.

### duckdb-ffi 1.5 to 2.0

`hs-bindgen` 1.0 generates both C APIs from the pinned 2.0.0 headers. The
function names stay the same. Many of their types change.

- Import `Database.DuckDB.FFI` for the retained API. The
  `Database.DuckDB.FFI.Deprecated` module is removed. Its functions are in
  `Database.DuckDB.FFI`. `Database.DuckDB.FFI.V2` is new.
- Add `hs-bindgen-runtime` to your dependencies to use `toFunPtr`,
  `fromFunPtr`, and the generated record fields.
- Enum types are newtypes over `CUInt` with one field, `unwrap`. The pattern
  names stay the same. Logical type tags keep the type `DuckDBType`.
  Functions take and return its wrapper `DuckDBTypeAlias`, because the C
  header declares `duckdb_type` as an alias of the enum. For example,
  `c_duckdb_create_logical_type DuckDBTypeInteger` becomes
  `c_duckdb_create_logical_type (DuckDBTypeAlias DuckDBTypeInteger)`.
- Handles are newtypes over `Ptr`. `DuckDBIdx` and `DuckDBSel` are newtypes
  over `Word64` and `Word32`. Write a null handle as, for example,
  `DuckDBDataChunk nullPtr`.
- `const char *` is `ConstPtr CChar`. `void *` is `Ptr Void`.
- Record fields have the C names, and the module uses `NoFieldSelectors`. Read
  them with `getField` or `OverloadedRecordDot`. For example,
  `duckDBDateStructYear` becomes `year`, `arrowSchemaFormat` becomes `format`,
  `arrowArrayNullCount` becomes `null_count`, and `duckDBBlobData` becomes
  `data'`. `DuckDBDate`, `DuckDBTime`, and `DuckDBTimestamp` are records with a
  `days` or `micros` field.
- Callback types are newtypes over `FunPtr`. Use `toFunPtr` and `fromFunPtr`.
- Structures pass by value, as in C. `c_duckdb_fetch_chunk resultPtr` becomes
  `peek resultPtr >>= c_duckdb_fetch_chunk`. Functions that wrote a structure
  through a pointer now return it. For example, `c_duckdb_from_date` and
  `c_duckdb_query_progress` return `DuckDBDateStruct` and
  `DuckDBQueryProgress`. The value getters for HUGEINT, UHUGEINT, DECIMAL,
  INTERVAL, BLOB, BIT, UUID, and BIGNUM also return structures.
- These names are removed: `mkArrow*`, `wrapArrow*`, `releaseArrowSchema`,
  `releaseArrowArray`, `releaseArrowStream`, `duckdbArrow*Internal`,
  `duckdbArrow*Clear`, `ArrowSchemaPtr`, `ArrowArrayPtr`, `ArrowStreamPtr`, and
  `DuckDBErrorInvalidConfiguration`. `Database.DuckDB.Simple.Arrow` provides
  the release helpers. Use `DuckDBInvalidConfiguration` for the error tag.
- The generated functions have no `DEPRECATED` pragmas. The header
  documentation still identifies the deprecated functions.
- The generated ABI checks accept only Linux and macOS on x86_64 and aarch64.
- The two APIs have separate Haskell types, including their Arrow structures.
  Use qualified imports when you combine them.

The generator does not manage ownership. Callers must still release native
resources, keep callbacks alive for their required lifetime, and catch
exceptions inside callbacks.

### duckdb-simple 0.3 to 0.4

`duckdb-simple` 0.4 uses the V2 C API for connections, results, conversions,
and callbacks. Upstream marks that API as unstable, so 0.4 requires exactly
DuckDB 2.0.0. Creating an environment raises `SQLError` for another version.
The SQL workflow stays the same. These changes can affect your code:

- The package requires `base` 4.18 or later, that is GHC 9.6 or later.
  Version 0.3 accepted `base` 4.14.
- Raw types come from `Database.DuckDB.FFI.V2`. They are distinct from the
  retained API types in `Database.DuckDB.FFI`.
- `Database.DuckDB.Simple.Internal` uses the V2 handles, and some of its
  functions take other arguments. `withResult` and `executePreparedResult` take
  a `Statement`. `fetchResultChunk` takes no `ResultMode`.
  `runInterruptibleQuery` runs an action of each result type.
  `withDatabaseHandle` gives the native instance handle. The changelog lists
  the removed names and the new record fields.
- `SQLError.sqlErrorType` is `Maybe ErrorType`. `Database.DuckDB.Simple`
  exports `ErrorType`. It has one constructor for each error code of the V2 C
  API. The V2 codes are a different set from the codes of version 0.3. Thus
  some codes of version 0.3 have no one-to-one replacement, for example
  `DuckDBErrorIO` and `DuckDBErrorInvalid`. Some constructors are new, for
  example `ErrorResourceInUse` and `ErrorApi`. Other codes have a direct
  replacement. For example, `DuckDBErrorConstraint` becomes
  `ErrorDatabaseConstraint`, `DuckDBErrorCatalog` becomes
  `ErrorDatabaseCatalog`, `DuckDBErrorBinder` becomes `ErrorQueryBinder`,
  `DuckDBErrorConversion` becomes `ErrorTypeConversion`,
  `DuckDBErrorInvalidInput` becomes `ErrorInputInvalid`, and
  `DuckDBErrorInterrupt` becomes `ErrorRuntimeInterrupt`.
- `ToDuckValue.toDuckValue` takes a `ValueFactory` and returns a
  `NativeValue`. `Database.DuckDB.Simple.ToField` exports the two names. They
  are synonyms of the V2 handles `DuckDBV2FactoryHandle` and
  `DuckDBV2ValueHandle`. `logicalTypeFromRep` also takes the factory. To get
  the factory of a `Connection`, call `withConnectionHandle` and then
  `withConnectionFactory` from `Database.DuckDB.Simple.Internal`. Create and
  destroy the value inside `withConnectionHandle`. `withConnectionFactory`
  does not keep the connection alive. Registration code gets the factory of
  its target from `withRegistrationFactory` in
  `Database.DuckDB.Simple.V2.Registration`.
- `logicalTypeFromRepWith` takes the factory, and its resolver returns
  `Maybe` a native type. `Nothing` selects the default construction.
- `logicalTypeToRep`, `logicalTypeFromRep`, `logicalTypeFromRepWith`, and
  `destroyLogicalType` moved from `Database.DuckDB.Simple.LogicalRep` to
  `Database.DuckDB.Simple.V2.LogicalRep`. Import them from the new module.
- `LogicalTypeScalar`, `Function.argumentTypes`, and
  `CopyBindInfo.copyBindColumnTypes` use `LogicalTypeId` instead of
  `DuckDBType`. `Database.DuckDB.Simple`, `Database.DuckDB.Simple.LogicalRep`,
  `FromField`, `Function`, and `Copy` export `LogicalTypeId`. Replace the prefix
  `DuckDBType` of each pattern with `LogicalTypeId`. For example,
  `DuckDBTypeBigInt` becomes `LogicalTypeIdBigInt`. `DuckDBTypeTimestampS`
  becomes `LogicalTypeIdTimestampSec`. `DuckDBTypeIntegerLiteral` and
  `DuckDBTypeStringLiteral` have no replacement.
- SQL TUPLE values decode as `FieldTuple` with a `TupleValue`. `LogicalTypeRep`
  adds `LogicalTypeTuple` and `LogicalTypeCollatedVarchar`. Update exhaustive
  matches.
- `LogEntry` has six new fields. Use record syntax to construct or match it.
  `logEntrySeverity` has the type `LogLevel`, and `logEntryScope` has the type
  `LogScope`. `Database.DuckDB.Simple.Logging` exports both types.
- `ConfigValue.configValueScope` is `SettingScope` instead of
  `Maybe DuckDBConfigOptionScope`. `Database.DuckDB.Simple.Config` exports
  `SettingScope`.
- `listConfigFlags` lists the options of a temporary in-memory database.
  With DuckDB 2.0.0 the list has 201 names, where version 0.3 gave 377. The
  names that are not in the list are options of extensions that the new
  database does not load.
- `CatalogEntry.catalogEntryType` and `lookupCatalogEntry` use the new
  `CatalogEntryType`. For example, `DuckDBCatalogEntryTypeTable` becomes
  `CatalogTable`. `DuckDBCatalogEntryTypeCollation` has no replacement, because
  V2 metadata cannot look up collations.
- `withFileHandle` takes `[FileFlag]` instead of `[DuckDBFileFlag]`. It gives
  its action an opaque `FileHandle` instead of `DuckDBFileHandle`, and the
  other handle functions take `FileHandle`.
  `Database.DuckDB.Simple.FileSystem` exports both types. For example,
  `DuckDBFileFlagRead` becomes `FileFlagRead`. `DuckDBFileFlagInvalid` has no
  replacement. You cannot get the native handle from a `FileHandle`. To use a
  native file function, open the file with `Database.DuckDB.FFI.V2`.
- The Arrow release helpers moved from `duckdb-ffi` to
  `Database.DuckDB.Simple.Arrow`. These helpers and the `foldArrow` callbacks
  use the V2 `ArrowSchema` and `ArrowArray` types.
- `Database.DuckDB.Simple.Deprecated.Streaming` is now
  `Database.DuckDB.Simple.Streaming`, with the same exports.
- While an explicit stream is incomplete, DuckDB rejects other statements on
  its connection with a `RESOURCE_IN_USE` error. Closing an incomplete stream
  aborts its statement. In autocommit mode, DuckDB rolls back its writes.
- Each connection has a lock for statement preparation, for its open native
  result, and for each fetch of an explicit stream. Threads that share a
  connection wait for each other in the default query functions. Serialize a
  transaction, a shared `Statement`, and an explicit stream in the
  application. A callback that queries its active connection waits for this
  lock. After the time limit of `connectionCallbackLockWait` in
  `ConnectionOptions` (10 seconds by default), the query raises an `SQLError`
  of the type `ErrorResourceInUse`. For most callbacks, the outer statement
  then fails. `Nothing` sets no time limit.
  The time limit applies also to a callback that waits for a second
  connection.
  With more than one DuckDB thread, DuckDB can run a callback of an explicit
  stream between two fetches. Then the query fails at once with the
  `RESOURCE_IN_USE` error of DuckDB. The README gives the cases that the time
  limit does not cover.
- `ConnectionOptions` has new fields, for example `connectionCallbackLockWait`.
  Code that builds the record with only the fields of version 0.3 gets a
  warning from GHC. The program then fails with a missing-field error when the
  library reads such a field. Build options as a record update of
  `defaultConnectionOptions`, for example
  `defaultConnectionOptions {connectionGeometryCRS = ["OGC:CRS84"]}`.
- `query_` and `execute_` parse all statements before they run any. A syntax
  error stops earlier statements in the same input from running.
- A conversion error names the types `TIMESTAMPTZ` and `TIMETZ`, in
  `errSQLType` and in the message of the `SQLError`. Version 0.3 gave
  `TIMESTAMP_TZ` and `TIME_TZ`. Change code that matches the old texts.
- `TimeWithZone` holds the offset as seconds east of UTC. The field
  `timeWithZoneZone :: TimeZone` is now `timeWithZoneOffsetSeconds :: Int32`.
  Convert a `TimeZone` with `fromIntegral (timeZoneMinutes zone) * 60`, and
  convert an offset with
  ``minutesToTimeZone (fromIntegral (offset `quot` 60))``.
  The second conversion drops the seconds of an offset. A TIMETZ with an offset
  that has seconds, for example `12:00:00+01:00:30`, now reads. Version 0.3
  raised an error for it.
- A `TimeOfDay`, a `LocalTime`, or a `UTCTime` with digits below a microsecond
  keeps them for a parameter of the type `TIME_NS`, `TIMESTAMP_NS`, or
  `TIMESTAMPTZ_NS`. Version 0.3 bound the microsecond value for each
  parameter. Thus an explicit cast to a nanosecond type for a microsecond
  column now rounds, as the SQL literal does: `?::TIMESTAMP_NS` for a
  `TIMESTAMP` column, `?::TIME_NS` for a `TIME` column, and `?::TIMESTAMPTZ_NS`
  for a `TIMESTAMPTZ` column store .123457 for a value with the fraction
  .123456999, where version 0.3 stored .123456. A plain `?` for such a column
  still cuts the digits. A value with no such digits binds as in version 0.3.
  A `LocalTime` or a `UTCTime` outside the range of the nanosecond type (about
  the years 1677 to 2262) raises an error for a nanosecond parameter, as in
  version 0.3. The type of that error can differ from version 0.3 for a
  `UTCTime` with digits below a microsecond, and for a value in a list, an
  `Array`, a `Map`, a STRUCT, a TUPLE, or a UNION. For a value just above the
  range, from 2262-04-11 23:47:16.854775807 to 23:47:16.854775999, version 0.3
  stored the value without its digits below a microsecond. Such a value now
  raises an error.
- A value with the declared type SQLNULL must be `FieldNull`. Each other value
  raises a conversion error. Version 0.3 bound it as NULL with no error.
- `genericToFieldValue` gives the STRUCT without fields (`{}` in DuckDB) for a
  product type with no fields. A constructor with no fields in a sum type has
  this STRUCT as its payload. This includes each constructor of an enum-like
  sum type, such as `data Colour = Red | Blue`. Version 0.3 gave `FieldNull`
  in both places. Change code that matches `FieldNull` for such a value. The
  decoders accept `FieldNull` and the STRUCT without fields, so data that
  version 0.3 wrote still reads. A UNION payload or a STRUCT field that
  version 0.3 stored keeps NULL in place of `{}`, so `WHERE v = ?` with a new
  parameter of the same Haskell value does not find that row.
- An `Integer` or a `Natural` parameter binds as HUGEINT when the value is in
  the HUGEINT range. Version 0.3 bound BIGNUM in each case. `typeof(?)` gives
  HUGEINT, and a parameter that is stored in a VARIANT column holds a HUGEINT.
  A result of such a parameter, for example of `SELECT ?`, reads as `Integer`
  and no longer as `BigNum`. Arithmetic on the parameter in SQL has the range
  of HUGEINT. A negative value for an unsigned column now raises an error;
  version 0.3 stored a wrapped value. Bind a `BigNum` for the BIGNUM value of
  version 0.3. `toDuckValue` uses the same rule: an `Integer` or a `Natural`
  in the HUGEINT range gives a HUGEINT value, where version 0.3 gave a BIGNUM
  value.
- Each function that reads rows gives one row with one BIGINT column for an
  INSERT, UPDATE, or DELETE without RETURNING: the number of changed rows.
  This applies to `query`, `query_`, `queryNamed`, `fold`, `fold_`,
  `foldNamed`, `nextRow`, `nextRowWith`, and the same functions of
  `Database.DuckDB.Simple.Streaming`. Version 0.3 gave this row only from
  `query`, `query_`, and `queryNamed`. The folds and `nextRow` gave no row.
- A session variable can supply a named parameter that you omit. DuckDB
  raises an error when no variable has that name. Too few named parameters no
  longer raise `FormatError`. Too many named parameters still raise it.
- `deleteFunction` raises `SQLError` for a function made with `createFunction`.
  DuckDB does not permit deletion of functions registered through the C API,
  and version 0.3 ignored that error. Remove the call, or catch the error. The
  function stays registered until its database closes. To replace the
  function, register the name again. Each use of `deleteFunction` causes a
  compile-time warning. A build with `-Werror` must allow that warning, for
  example with `-Wno-error=deprecations`.
- The SQL error of a failed Haskell callback has the plain message of the
  exception. This applies, for example, to a function from `createFunction`
  and to the callbacks of `registerCopyToFunction`. For a `userError`, version
  0.3 gave `user error (...)`. For an `SQLError`, it gave the `Show` text of
  the record. Update code that compares the text of these errors.
- The error of an argument that does not convert names the argument `arg0`,
  `arg1`, and so on, with numbers from 0. For example, the text contains
  `argument arg1 is NULL, but Int64 cannot hold NULL`. Version 0.3 gave
  `unable to convert argument #2: ...`, with numbers from 1. Update code that
  compares these texts.
- `Database.DuckDB.Simple` exports new names, for example `connect`,
  `Environment`, `Instance`, `withInstance`, `attachDatabase`, `createCast`,
  `createTableFunction`, `createAggregate`, `Aggregate` with its fields, and
  `TypedRow`. It also exports `ErrorType` and `LogicalTypeId` with their
  constructors. These names can clash with your own names when you import the
  module without a list. Hide these names in the import, or import the module
  qualified.
- An empty `Array` parameter raises `SQLError`. Version 0.3 bound an empty
  `Array` of scalar elements. Bind an empty list for an empty LIST.
- A list binds as a LIST parameter. Version 0.3 had no `ToField` instance for
  lists, so code bound an `Array` and let DuckDB cast it. That code still
  works for an `Array` with elements. A `String` stays one VARCHAR parameter,
  and a list as a row is still a row of parameters, so put a list parameter in
  `Only`.
- A composite parameter with a GEOMETRY CRS that DuckDB does not recognize
  raises `SQLError`. Version 0.3 bound it without the CRS. Configure custom CRS
  names through `connectionGeometryCRS`.
- A composite parameter that the native value constructor rejects raises an
  `SQLError`. For example, a STRUCT parameter with a MAP field that has a NULL
  key raises an `SQLError` with the type `ErrorInputInvalid`. Version 0.3
  raised an `IOException`. Catch `SQLError` for these errors.
- A name in `connectionGeometryCRS` that DuckDB drops, for example a blank
  name, fails when the connection opens. Version 0.3 read the configured types
  the first time a parameter needed one, and did not check that a type kept its
  CRS.
- DuckDB 2.0 rejects WKB with mixed Z/M member layouts.
- `RawGeometry` has a `ToField` instance. It binds as a GEOMETRY with its own
  CRS. Version 0.3 had only a `FromField` instance, so code imported the WKB with
  `ST_GeomFromWKB(?)` and the CRS with `ST_SetCRS`. That SQL still works.
- Decoded VARIANT objects list their keys in DuckDB's lexicographic order.
- A `Variant` parameter with a UNION payload, at any depth, raises `SQLError`
  with the type `ErrorInputInvalid`, because a VARIANT keeps only the member
  value of a UNION. Version 0.3 bound such a value with no error. Bind the
  member value, or bind the UNION value as a UNION parameter.

The typed registrations are new in 0.4. Version 0.3 had no `Registration`
or `Pragma` module and no custom options. Its `Copy` module had
only `registerCopyToFunction`. The arguments of that function do not change,
except for the type of `copyBindColumnTypes` above. See
[typed registration](duckdb-simple/README.md#typed-registration).

A repository checkout must generate the ignored build artefacts before it
builds. CI does this too. Source archives include those artefacts. See the
[generation command](README.md#generate-the-bindings) and
[2.0 API and runtime notes](docs/duckdb-2.0.md).

## DuckDB 1.4 to 1.5

This guide covers the changes needed when upgrading this repository from the
DuckDB 1.4 line to the DuckDB 1.5 line.
This part describes `duckdb-ffi` 1.5.6.0 and `duckdb-simple` 0.3.0.0. The part
[DuckDB 2.0](#duckdb-20) gives the changes of the later releases.

The short version is:

- `duckdb-ffi` uses the DuckDB `1.5.6` C header. The native minimum is 1.5.3.
- `duckdb-simple` now depends on `duckdb-ffi-1.5`.
- Existing 1.4 bindings continue to work, but the runtime `libduckdb` you load
  must be >= 1.5.3 and < 1.6.
- New 1.5 functionality is exposed through additive modules and helpers; no
  wholesale rewrite is required.

### Native DuckDB 1.5.6

The default native download was DuckDB 1.5.6. Native DuckDB >= 1.5.3 and < 1.6
stayed supported. `duckdb-simple` uses a separate API version.

The FFI adds the GEOMETRY and VARIANT type tags, the geometry CRS accessor,
and the COPY_DATABASE, UPDATE_EXTENSIONS, and MERGE_INTO statement tags.
Free strings returned by the CRS accessor with `duckdb_free`.

### Geometry support

`duckdb-simple-0.3.0.0` adds Geometry support.

Use `Data.Geometry.Geometry` from the published `geometry-simple` package for
decoded shapes. Use `RawGeometry` for WKB bytes and CRS metadata. The package's
`Geometry` type has no CRS. It has parameter and result instances.
In version 0.3, `RawGeometry` had only a `FromField` instance, so code imported
its WKB and CRS with explicit SQL. Version 0.4 adds a `ToField` instance. See
[the Geometry notes](duckdb-simple/README.md#geometry) for examples and limits.

`FieldValue` gains `FieldGeometry`, and `LogicalTypeRep` gains
`LogicalTypeGeometry`. Update exhaustive matches.

The C API of DuckDB 1.5 cannot create a GEOMETRY type with a CRS. In version
0.3, each connection read GEOMETRY types for a list of CRS definitions the first
time a parameter needed one, and composite parameters used them. The list
defaults to `OGC:CRS84`. Set it with `ConnectionOptions`, `openWithOptions`, or
`withConnectionWithOptions`. In version 0.3, a CRS outside the list bound
without a CRS, and `logicalTypeFromRep` created `GEOMETRY` with no CRS. To
apply a CRS in these cases, version 0.3 needed an insert into a column with
that CRS, or a cast in SQL.

### VARIANT support

`duckdb-simple-0.3.0.0` adds VARIANT support. VARIANT results decode to the
`FieldValue` of the stored value, so existing `FromField` instances read them.
Objects decode to `FieldStruct` values with VARIANT fields. `Variant` from
`Database.DuckDB.Simple.Variant` wraps the `FieldValue` of a result. Native
VARIANT loses geometry CRS metadata. Import raw geometry payloads with
`ST_GeomFromWKB(?)::VARIANT`. See [the VARIANT
notes](duckdb-simple/README.md#variant) for current decoding and
persistent-storage requirements.

`Variant` binds as a VARIANT parameter. In version 0.3, the connection read the
VARIANT type the first time a parameter needed it, because the C API of DuckDB
1.5 cannot create a usable VARIANT type. In version 0.3, `Variant` had no
`ToDuckValue` instance, and `logicalTypeFromRep` raised an error for VARIANT.

Array elements require `ToField` and `DuckDBColumnType`. Arrays use each
element's `ToField` instance. If a custom element type only has `ToDuckValue`,
add `instance ToField YourType`. The default implementation requires `Show`.

### Binding fixes

- Default cursors and folds use DuckDB's supported materialized execution API. They
  decode one Haskell row at a time, but native result memory depends on the
  result size. To request native streaming, import
  `Database.DuckDB.Simple.Streaming` qualified with `duckdb-simple` 0.4.
  Earlier releases use `Database.DuckDB.Simple.Deprecated.Streaming` and
  DuckDB's deprecated streaming entry point. The raw bindings remain in duckdb-ffi.

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
- Rebuild applications that use the FFI directly. The corrected C adapters
  preserve Haskell types.

### Who Needs to Change What

If you use `duckdb-ffi` directly:

- Rebuild and relink against DuckDB `1.5.6`.
- Update any packaging, Nix, CI, Docker, or deployment config that still pulls
  a 1.4 `libduckdb`.
- If you want the new 1.5 APIs, import the new raw modules and bind against the
  new opaque handle types and callbacks.

If you use `duckdb-simple`:

- Rebuild against `duckdb-simple-0.3.0.0` and DuckDB `1.5.6`.
- Review the binding changes above for SQL parameter types, NULL handling,
  and generic decoding.
- New 1.5 helpers are available from dedicated modules instead of being folded
  into the core query API.

### Runtime Compatibility

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

### `duckdb-ffi` Migration

#### Header and Symbol Surface

The vendored `duckdb.h` of `duckdb-ffi` 1.5.6.0 matched DuckDB 1.5.6.

The 1.5 change is additive for the C API surface used here:

- existing 1.4 function imports remain valid
- new 1.5 functions, enums, handles, and callbacks are now available

This means most direct FFI users do not need to rewrite existing code. Instead:

- keep existing bindings for legacy 1.4 APIs
- opt into the new 1.5 APIs where needed

#### New Raw Modules

The new 1.5 areas are exposed through these modules:

- `Database.DuckDB.FFI.Catalog`
- `Database.DuckDB.FFI.CopyFunctions`
- `Database.DuckDB.FFI.FileSystem`
- `Database.DuckDB.FFI.Logging`

Use these when adopting new 1.5 functionality rather than trying to infer the
symbols manually from the header.

#### New Types and Callbacks

DuckDB 1.5 introduces new opaque handles and enums that may affect your own FFI
layer if you were maintaining local bindings. In this repository they are now
provided centrally, including:

- file flags
- config option scopes
- catalog entry types
- copy-function handles and callback types
- file-system and file-handle handles
- catalog and catalog-entry handles
- log-storage handles
- scalar function init callbacks

If you had local downstream bindings for any of these, delete them and import
the shared versions from `Database.DuckDB.FFI.Types`.

#### Error-Handling Guidance

DuckDB 1.5 adds more `duckdb_error_data`-style paths.

You do not need to rewrite all existing code to use them immediately.

Recommended approach:

- leave old bindings that already use legacy error accessors in place
- prefer the structured 1.5 error-data interface when binding new APIs

That is the policy used in this repository.

### `duckdb-simple` Migration

#### Existing Query Code

Most `duckdb-simple` code should not need source changes.

The following remain source-compatible:

- `open`, `close`, `withConnection`
- prepared statements and named parameters
- `execute`, `query`, `fold`, `nextRow`
- `ToField`/`FromField`-based row and parameter handling
- existing scalar function registration with `createFunction`

#### New Modules

DuckDB 1.5 features are exposed in additive modules:

- `Database.DuckDB.Simple.Config`
- `Database.DuckDB.Simple.Catalog`
- `Database.DuckDB.Simple.FileSystem`
- `Database.DuckDB.Simple.Copy`
- `Database.DuckDB.Simple.Logging`

Import them only where needed. The main `Database.DuckDB.Simple` module stays
focused on the core query interface.

#### Opening with Config

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

#### Stateful Scalar Functions

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

#### Copy Functions

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

#### Logging

DuckDB 1.5 adds custom log storage registration. `duckdb-simple` now exposes
`registerLogStorage` in `Database.DuckDB.Simple.Logging`.

This is optional advanced functionality. Existing applications do not need to
change anything unless they want to capture DuckDB log events.

### Behavior Changes Worth Noting

#### `TIME_NS` Decoding

The test suite was updated because DuckDB 1.5 now successfully decodes
`TIME_NS`, where older expectations treated that as unsupported.

For most users this is a strict improvement:

- code that already handled `FieldTime` continues to work
- tests that expected a failure for `TIME_NS` should be updated

#### `duckdb_string_t` Handling

DuckDB 1.5 made it more obvious that `duckdb_string_t_data` must be consumed
with its explicit length, not by assuming NUL termination.

If you have downstream helper code using:

```haskell
peekCString ...
```

on `duckdb_string_t_data`, switch to a length-aware read such as:

```haskell
peekCStringLen ...
```

using `duckdb_string_t_length`.

### Suggested Upgrade Steps

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
