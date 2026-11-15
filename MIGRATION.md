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

DuckDB 2.0.0 plans a cast between some pairs of types as a cast that cannot
fail, also a registered cast, for example a cast from INTEGER to VARCHAR. A V1
cast that reports an error for such a pair gives an INTERNAL error, because the
V1 API cannot choose the type of the error. A V2 cast gives an INTERNAL error
for such a pair when it reports an Invalid Input, Out of Range, or Conversion
code, or a code that DuckDB does not map, for example `DUCKDB_V2_ERROR_API`.
After an INTERNAL error during the execution of a query, DuckDB invalidates the
database. A V2 cast can report the code `DUCKDB_V2_ERROR_TYPE_MISMATCH`. The
section Errors of the duckdb-simple README gives the details.

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
  `runInterruptibleQuery` takes an `IO a` action, not an `IO DuckDBState`
  action. `withDatabaseHandle` gives the native instance handle. The changelog lists
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
- `SQLError` has a fourth field, `sqlErrorBody :: Maybe Text`. Code that builds
  an `SQLError` with positional arguments, or matches it by position, gets a
  compile error. Use record syntax, for example
  `SQLError {sqlErrorMessage = text, sqlErrorType = Nothing, sqlErrorQuery = Nothing, sqlErrorBody = Nothing}`,
  and match only the fields that you need, for example
  `SQLError {sqlErrorType}`.
- A statement whose callback raises an `SQLError` with a type fails with that
  type, except a failed cast, which gives `ErrorTypeMismatch`, and the 16 types
  that the section "Errors" of `duckdb-simple/README.md` lists. Code that
  expects `ErrorInputInvalid` for such a statement gets the type of the inner
  error, for example `ErrorTypeConversion` for a failed query in a scalar
  function.
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
- `LogicalTypeScalar` and
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
- `ConfigValue.configValueText` is `Maybe Text` instead of `Text`. It is
  `Nothing` for an option with a NULL value, for example `enable_profiling`
  before a write. For such an option, version 0.3 with DuckDB 1.5.6 stopped
  the process: DuckDB raised the C++ exception "Calling StringValue::Get on a
  NULL value" in `duckdb_get_varchar`.
- A date or a timestamp outside the range of DuckDB raises an `SQLError` of the
  type `ErrorTypeConversion` when it binds, also as an element of a list, an
  `Array`, or a `Map`, inside a STRUCT, a TUPLE, or a UNION, and in a VARIANT.
  For a `Day`, that range is the range of DATE, from -5877641-06-25 to
  5881580-07-10. For a `LocalTime` or a `UTCTime` (in UTC), it is the range of
  TIMESTAMP, TIMESTAMPTZ, TIMESTAMP_S, and TIMESTAMP_MS, from -290308-12-22
  00:00:00 to 294247-01-10 04:00:54.775806. The dates are in the notation of
  `Data.Time`. A target of the type TIMESTAMP_NS or TIMESTAMPTZ_NS is different.
  These types have a smaller range, about the years 1677 to 2262. A value
  outside that range also gets the type `ErrorTypeConversion`, in each position.
  The error comes from the library or from DuckDB by the position of the value:

  - A parameter: the error of the library for a value outside the range of
    DuckDB, and the Conversion Error of DuckDB for each other such value, for
    example in the year 2300.
  - An element of a list, an `Array`, or a `Map`: the error of the library. For
    a value in the range of DuckDB, its text is "TIMESTAMP value out of finite
    range".
  - A field of a STRUCT, an element of a TUPLE, or a member of a UNION: the
    error of the library, "TIMESTAMP value out of finite range", also for a
    value outside the range of DuckDB.
  - In each position, a `LocalTime` or a `UTCTime` (in UTC) from 2262-04-11
    23:47:16.854775807 to 23:47:16.854775999 gets that error of the library.
- Version 0.3 raised the `IOException` "DATE value out of finite range" or
  "TIMESTAMP value out of finite range" for a date or a timestamp at an
  infinity value or outside the storage. It bound the other values outside the
  range of DuckDB, for example the `Day` at day -2147483648 from the Unix epoch
  (-5877641-06-23 in the notation of `Data.Time`), and a `LocalTime` or a
  `UTCTime` from -290308-12-21 19:59:05.224192 to 23:59:59.999999, except the
  -infinity value at 19:59:05.224193. A stored TIMESTAMP of that time makes a
  cast to VARCHAR raise an INTERNAL error in DuckDB 2.0.0. Code that catches
  the `IOException` must catch `SQLError`.
- A VARIANT of a `LocalTime` whose microseconds do not fit in INT64 bound as
  TIMESTAMP_S or TIMESTAMP_MS in version 0.3. It now raises the range error, as
  such a `LocalTime` does as a parameter.
- `listConfigFlags` takes a `Connection`. It lists the options of the
  database of that connection, also its custom options. Version 0.3 listed
  the options of the C API function `duckdb_get_config_flag`, which takes no
  database. For the old call, use `withConnection ":memory:" listConfigFlags`.
  It lists the options of a new in-memory database.
- `deleteFunction` reads the name with the rules of DuckDB for a qualified
  name, as `describeTable` does. A part in double quotes can contain a dot, so
  `"\"a.b\""` drops the function `a.b`. A quote inside a part without quotes,
  or more than three parts, now raises `SQLError`. Version 0.3 doubled each
  quote in a part, so `deleteFunction conn "a\"b"` dropped the function
  `a"b`. Now that call raises `SQLError` ("Unexpected quote"). Write the
  name as SQL writes it: `"\"a\"\"b\""`. For a function from `createFunction`,
  `deleteFunction` raises `SQLError`. A later entry gives that change.
- `CatalogEntry.catalogEntryType` and `lookupCatalogEntry` use the new
  `CatalogEntryType`. For example, `DuckDBCatalogEntryTypeTable` becomes
  `CatalogTable`. `DuckDBCatalogEntryTypeCollation` has no replacement. The
  SQL metadata of DuckDB 2.0.0 gives the name of each collation
  (`pragma_collations()`), but not its catalog and its schema, so
  `lookupCatalogEntry` cannot find a collation by catalog, schema, and name.
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
  The time limit applies also to a wait for a second connection that another
  statement uses.
  With more than one DuckDB thread, DuckDB can run a callback of an explicit
  stream between two fetches. Then the query fails at once with the
  `RESOURCE_IN_USE` error of DuckDB. The README gives the cases that the time
  limit does not cover.
- `ConnectionOptions` has new fields: `connectionCallbackLockWait`,
  `connectionAggregateStripes`, and `connectionArrowBatchSize`. Code that
  builds the record with only the fields of version 0.3 gets a warning from
  GHC. The program then fails with a missing-field error when the library
  reads such a field. Build options as a record update of
  `defaultConnectionOptions`, for example
  `defaultConnectionOptions {connectionGeometryCRS = ["OGC:CRS84"]}`.
- A conversion error names the types `TIMESTAMPTZ` and `TIMETZ`, in
  `errSQLType` and in the message of the `SQLError`. Version 0.3 gave
  `TIMESTAMP_TZ` and `TIME_TZ`. Change code that matches the old texts.
- A conversion error names the types `TINYINT`, `SMALLINT`, `INTEGER`,
  `BIGINT`, and `VARCHAR`, in `errSQLType` and in the message of the
  `SQLError`. Version 0.3 gave `INT1`, `INT2`, `INT4`, `INT8`, and `TEXT`.
  Change code that matches the old texts.
- `duckdbColumnType` gives `VARCHAR` for `Text` and `String`. Version 0.3 gave
  `TEXT`. Change code that compares this text.
- A BOOLEAN read as `Text` or `String` gives `true` or `false`. Version 0.3
  gave `1` or `0`. Read `Bool`, or write `c::INTEGER` in SQL for the old text.
- A DECIMAL read as `Double` or `Float`, and a BIGINT read as `Float`, give
  the value of the cast of DuckDB. A value can differ from version 0.3 in the
  last bit.
- A TIMESTAMP, TIMESTAMP_S, TIMESTAMP_MS, or TIMESTAMP_NS read as `UTCTime`
  raises the error of an incompatible column. Write
  `c::TIMESTAMP AT TIME ZONE 'UTC'` for the instant that version 0.3 gave, or
  read `LocalTime`. For a TIMESTAMP_NS, write `c AT TIME ZONE 'UTC'`, because
  the cast to TIMESTAMP rounds the value to the microsecond. Without the cast,
  DuckDB converts a TIMESTAMP_S or TIMESTAMP_MS value to nanoseconds, which
  fails outside the years 1677 to 2262.
- A DATE read as `UTCTime` raises the error of an incompatible column. Write
  `c::TIMESTAMP AT TIME ZONE 'UTC'` for midnight in UTC, as version 0.3 gave,
  or read `Day`. Without the cast, the value fails outside the years 1677 to
  2262.
- A TIMESTAMPTZ or TIMESTAMPTZ_NS read as `LocalTime` raises the error of an
  incompatible column. Write `c AT TIME ZONE 'UTC'` for the time in UTC that
  version 0.3 gave, or read `UTCTime`.
- The `Unbounded` forms of `UTCTime` and `LocalTime` (`UTCTimestamp` and
  `LocalTimestamp`) follow the three rules above.
- A FLOAT or DOUBLE read as `Text` or `String` raises the error of an
  incompatible column. Read `Double` or `Float`, or write `c::VARCHAR` for the
  text of DuckDB, for example `0.01`. Version 0.3 gave the text of `show`, for
  example `1.0e-2`.
- A VARCHAR read as `ByteString` raises the error of an incompatible column.
  Read `Text`, or write `encode(c)` for the UTF-8 bytes that version 0.3 gave.
- An ENUM read as an integer type (`Int`, `Int8` to `Int64`, `Word`, `Word8` to
  `Word64`, or `Natural`) raises the error of an incompatible column. Write
  `enum_code(c)::BIGINT` for the index that version 0.3 gave, or `c::INTEGER`
  for the label as a number. Without the cast, `enum_code` gives an unsigned
  type, which `Int` and `Int8` to `Int64` do not read.
- An ENUM now reads as `Text` or `String` with its label.
- `FieldEnum` has a second field, the label: `FieldEnum Word32 Text`. Change
  each pattern and each construction. A `FieldEnum` inside a declared ENUM type
  binds its label.
- `displayException` of a `ResultError` gives the rendered text of the error,
  and the error of a field of a `ViaDuckDB` record starts with
  `duckdb-simple: field NAME: `.
  The errors of the generic decoders name the SQL type of a value, not the
  value, for example `a value has type MAP but expected LIST or ARRAY` in place
  of `expected LIST, got FieldMap [...]`. The bind errors
  `cannot encode ... as ...` and `unsupported scalar conversion for ... from ...`
  also name the SQL type of the value, for example `from a value of type
  VARCHAR` in place of `from FieldText "x"`. Change code that matches the old
  texts.
- A `Variant` read accepts only a value that a VARIANT can hold. A column that
  holds an ENUM, a MAP, a UNION, an ARRAY, a TUPLE, a STRUCT that is not in the
  form of a VARIANT object (each field of the type VARIANT, the keys in
  ascending order), or a GEOMETRY with a CRS, at any depth, raises the error of
  an incompatible column. Write `c::VARIANT` in SQL, or read `FieldValue` for the
  decoded value. A JSON column still reads as its text; write `c::VARIANT` for
  the parsed value.
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
  HUGEINT, and a parameter that is stored in a VARIANT column holds a HUGEINT. A
  result of such a parameter, for example of `SELECT ?`, reads as `Integer` and
  no longer as `BigNum`. Arithmetic on the parameter in SQL has the range of
  HUGEINT. A negative value for an unsigned column now raises an error; version
  0.3 stored a wrapped value. Bind a `BigNum` for the BIGNUM value of version
  0.3. `toDuckValue` uses the same rule: an `Integer` or a `Natural` in the
  HUGEINT range gives a HUGEINT value, where version 0.3 gave a BIGNUM value. A
  `Natural` up to 2^128 - 1 gives a UHUGEINT value. Each other value raises an
  `SQLError` of the type `ErrorTypeConversion`, where version 0.3 gave a BIGNUM
  value. This applies to `setConfigValue`, `defaultValue`, the typed rows of a
  table function and of COPY FROM, the result of a scalar function, and a
  parameter of a newtype over `Integer` with the default `toField`. Use `BigNum`
  for a larger value, and derive `ToField` with newtype deriving to keep the
  parameter rule of `Integer`. An `Array` of such values with no integer or
  DECIMAL target takes the type that its elements give, for example `HUGEINT[2]`
  for 1 and 2, where version 0.3 gave `BIGNUM[2]`. A list and a `Map` of such
  values follow the same rule. Stored in a VARIANT column, such a value keeps
  the HUGEINT elements, so a later cast checks their range: `[-5]` cast to
  `UBIGINT[]` raises an error, where a BIGNUM element gave 18446744073709551611.
- A typed registration declares HUGEINT for an `Integer` and UHUGEINT for a
  `Natural`: a column of a typed row, an argument, and a result. The column type
  name (`duckdbColumnTypeFor`, also `duckdbColumnType`) still gives BIGNUM for
  them, as in version 0.3. DuckDB computes with the declared types. It casts no
  BIGNUM or UHUGEINT value to a HUGEINT argument. It casts a non-negative
  integer literal to a UHUGEINT argument, but no signed column or typed value
  without an explicit cast. `sum()` over a UHUGEINT column gives a DOUBLE value,
  and so do UHUGEINT + INTEGER and HUGEINT + UHUGEINT for columns and typed
  values. Use `BigNum` for a BIGNUM column or argument.
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
- `FunctionArg` is gone. An argument of `createFunction` can have each type
  with `FromField` and `DuckDBColumnType` instances. Its SQL type is the
  `duckdbTypeDeclaration` of the type. The argument types of version 0.3 keep
  their SQL types. `Function.argumentTypes` gives
  `[Either Text TypeDeclaration]` in place of `[DuckDBType]`. An argument type
  that is or contains VARIANT or GEOMETRY was a compile error. A VARIANT
  argument is now an `SQLError` at registration, and a GEOMETRY argument
  reads. An `Array` argument or result is an `SQLError` at registration,
  because its ARRAY type has no size. Use a list, or a newtype of the `Array`
  with its own column type name that gives the size, for example `BIGINT[3]`.
  DuckDB casts a LIST of the same length to that ARRAY with no error.
- A result of `createFunction` has the SQL type of its Haskell type. In
  version 0.3, an `Int16`, `Int32`, `Word16`, or `Word32` result was BIGINT,
  and a `Float` result was DOUBLE. Now they are SMALLINT, INTEGER, USMALLINT,
  UINTEGER, and FLOAT. DuckDB computes with these types, so an expression can
  now raise an Out of Range Error. For example, `30000::SMALLINT *
  30000::SMALLINT` raises an error, so `f(x) * f(y)` of an `Int16` function
  raises an error for `x = y = 30000`, where version 0.3 gave 900000000. For a
  `Word16` function, `f(1) - f(2)` raises an error, where version 0.3 gave -1.
  Cast the result to a wider type first, for example `f(x)::INTEGER * f(y)`.
- `ScalarType` is gone. `Function.returnType` gives
  `Either Text TypeDeclaration` in place of `ScalarType`. `FunctionResult` has
  an instance for each type with `ToDuckValue`, so a result can also be, for
  example, a record through `ViaDuckDB`, a list, a `Map`, or a `UUID`.
- `Database.DuckDB.Simple` exports new names, for example `connect`,
  `Environment`, `Instance`, `withInstance`, `attachDatabase`,
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
- An `Array` of STRUCT values, UNION values, generic records, or arrays with
  different element types gets the type that DuckDB gives a SQL list literal of
  the elements. Version 0.3 raised an `IOException` for it. The common type can
  add fields with NULL values, and a `BIGINT` field with a `DOUBLE` field gives
  a `DOUBLE` field, which can round a large value. Elements with no common type
  now raise the `SQLError` of DuckDB. Such an `Array` with only NULL elements
  takes the element type of a parameter with a known type, and otherwise the
  type of `array_value(NULL, ...)`, for example `"NULL"[2]`. Version 0.3 raised
  an error for it. For a parameter with no known type, such an `Array` of
  generic records through `ViaDuckDB` takes the STRUCT or UNION type of the
  record.
- A composite parameter with a GEOMETRY CRS that DuckDB does not recognize
  raises `SQLError`. Version 0.3 bound it without the CRS. Configure custom CRS
  names through `connectionGeometryCRS`.
- A composite parameter that the native value constructor rejects raises an
  `SQLError`. For example, a STRUCT parameter with a MAP field that has a NULL
  key raises an `SQLError` with the type `ErrorInputInvalid`. Version 0.3
  raised an `IOException`. Catch `SQLError` for these errors.
- A `TimeOfDay`, a `LocalTime`, or a `UTCTime` with digits below a microsecond
  keeps them for a `VARIANT` parameter: it binds as `TIME_NS`, `TIMESTAMP_NS`,
  or `TIMESTAMPTZ_NS` when it fits the range of that type. Version 0.3 bound
  the microsecond value. A list, an `Array`, or a `Map` of such values for a
  `VARIANT` parameter still binds microsecond elements. A VARIANT of a
  nanosecond value is not equal to a VARIANT of the microsecond value, so a row
  that version 0.3 stored in a `VARIANT` column does not match `WHERE x = ?`
  for the same Haskell value with such digits.
- Each input check of the library raises an `SQLError` with the type
  `ErrorInputInvalid`, except a value outside the range of its SQL type, which
  raises an `SQLError` with the type `ErrorTypeConversion` when it binds, as the
  Conversion Error of DuckDB for the same value in SQL (an integer for its
  parameter type or its declared type, a date or a timestamp for the range of DuckDB, a nanosecond
  timestamp for its storage, a HUGEINT or UHUGEINT field, a TIMETZ offset, a
  time of day that is not a TIME value, a `DecimalValue` for its width, or an
  ENUM label for its dictionary), and a parameter count or a parameter name that
  does not match the query, which raises a `FormatError`. A value of the wrong
  shape or type keeps `ErrorInputInvalid`, also where DuckDB gives another type
  for the same input, for example a UNION tag that is not a member, a leaf built
  from another type, a STRUCT field that does not match its type, or an unknown
  CRS. A check in a callback fails the outer statement with the type of the
  check: such a value as the result of a typed function or a row of a typed
  table function gives `ErrorTypeConversion` and the text
  `Conversion Error: duckdb-simple: ...` (see the entry on a callback that
  raises an `SQLError`).
- Version 0.3 raised an `IOException` (a `userError`) for a BIT, DECIMAL, ENUM,
  HUGEINT, UHUGEINT, TIMETZ, time of day, or timestamp value that does not fit
  its type, a STRUCT or UNION value whose fields do not match its types, a
  VARIANT payload that a VARIANT does not keep, a NUL in a logical type name,
  and a bad name in `connectionGeometryCRS`. Code that catches `IOException`
  for these errors must catch `SQLError`.
- The error for a scalar value that does not match its declared type names the
  SQL type, for example `NULL` in place of `LogicalTypeIdSQLNull`.
- A check of the state of the library, or of a vector, a type, or a value that
  DuckDB gave, raises an `SQLError` with the type `ErrorRuntimeInternal`. Inside
  a callback, the statement that runs the callback fails instead, with an
  `SQLError` of the type `ErrorInputInvalid` and the text "Invalid Input Error:
  duckdb-simple: ...", because a callback does not pass the type
  `ErrorRuntimeInternal` (a cast gives `ErrorTypeMismatch`). The use of a closed
  environment, instance, connection, statement, or type cache, a DuckDB library
  of another version, and a callback state read with the wrong type are not
  internal checks; they raise an `SQLError` with no type. Version 0.3 raised an
  `IOException` (a `userError`) or an `ErrorCall` for the internal checks. Code
  that catches either for them must catch `SQLError`.
- A name in `connectionGeometryCRS` that DuckDB drops, for example a blank
  name, fails when the connection opens. Version 0.3 read the configured types
  the first time a parameter needed one, and did not check that a type kept its
  CRS.
- DuckDB 2.0 rejects a GEOMETRY collection whose members have different Z/M
  layouts. A `Geometry` or a `RawGeometry` parameter with such a collection
  raises the Invalid Input Error of DuckDB "Geometry has inconsistent Z/M
  dimensions". Give each member the same Z and M dimensions, or store the WKB
  bytes in a BLOB column.
- `RawGeometry` has a `ToField` instance. It binds as a GEOMETRY with its own
  CRS. Version 0.3 had only a `FromField` instance, so code imported the WKB with
  `ST_GeomFromWKB(?)` and the CRS with `ST_SetCRS`. That SQL still works.
- Decoded VARIANT objects list their keys in ascending order, as DuckDB sorts
  them.
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
