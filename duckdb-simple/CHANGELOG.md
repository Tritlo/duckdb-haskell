# Changelog

## 0.4.0.0

- Target DuckDB 2.0 with `duckdb-ffi` 2.0.0.x. The backend uses the
  generated V2 C API. Upstream marks the V2 API as unstable ("The C API v2 is
  not yet stable. Its interface may change in upcoming releases."), so this
  release requires exactly DuckDB 2.0.0, and the `duckdb-ffi` release for a
  later DuckDB version is outside the dependency bound. Creating an
  environment raises `SQLError` for another library version. Use the earlier
  release with DuckDB 1.5.
- Require `base` 4.18 or later, that is GHC 9.6 or later. Version 0.3 accepted
  `base` 4.14.
- Raw handles, callback handles, and the error codes of raw calls use V2
  types. `sqlErrorType` is `Maybe ErrorType`. See MIGRATION.md.
- `SQLError` has a fourth field, `sqlErrorBody`: the text of a native error
  without the prefix of its type, and `Nothing` for an error of the library.
  Code that builds or matches an `SQLError` by position does not compile. See
  MIGRATION.md.
- Add shared environments and instances, attachment management, option
  discovery, table descriptions, SQL tokenization, and positioned file I/O.
- `listConfigFlags` takes a `Connection`, and it lists the options of the
  database of that connection, also its custom options. See MIGRATION.md.
- `deleteFunction` reads the name with the rules of DuckDB for a qualified
  name, so a part in double quotes can contain a dot. See MIGRATION.md.
- `ConfigValue.configValueText` is `Maybe Text`, and `Nothing` for an option
  with a NULL value. Version 0.3 stopped the process for such an option. See
  MIGRATION.md.
- A `Day` outside the range of DuckDB, from -5877641-06-25 to 5881580-07-10,
  and a `LocalTime` or a `UTCTime` outside the range from -290308-12-22
  00:00:00 to 294247-01-10 04:00:54.775806, in the notation of `Data.Time`,
  raise an `SQLError` of the type `ErrorInputOutOfRange` when they bind. For a
  target of the type TIMESTAMP_NS or TIMESTAMPTZ_NS, a value outside the
  smaller range of that type can get another error, by its position. Version
  0.3 bound some of these values and raised an `IOException` for the others.
  See MIGRATION.md.
- Add typed configuration values and defaults, managed log sinks, macro
  registration, function documentation, query metrics, and file path
  operations. Expose table bind metadata for multi-file readers.
- Add typed registrations. Their signatures have no native handle.
  `Database.DuckDB.Simple.Function` has `createCast`, `createAggregate`,
  `createTableFunction`, and the row class `TypedRow`. `Database.DuckDB.Simple`
  exports the three functions, `Aggregate`, and `TypedRow`.
- `createCast` registers a pure Haskell function as an explicit cast. A
  registered cast does not change a value that the library reads or binds on a
  connection. The exception is a GEOMETRY value that a callback of a function
  of an extension reads (see the README). A failed cast gives an `SQLError` of
  the type `ErrorTypeMismatch`, and so does an exception in the callback of
  `registerCast`. `TRY_CAST` gives NULL for a failed row, and `TRY(CAST(...))`
  raises the error. The section Errors of the README tells why.
- `createAggregate` registers an aggregate function as a fold over Haskell
  values. The record `Aggregate` has the initial state and the step, combine,
  and result functions. DuckDB gives the finalize callback no factory, so the
  result type must be BOOLEAN, TINYINT to BIGINT, UTINYINT to UBIGINT, FLOAT,
  DOUBLE, or VARCHAR.
- `createTableFunction` registers a Haskell function that gives the rows of a
  table function as a list. DuckDB reads the list one chunk at a time. It reads
  only the columns that the query uses, and the first column for a query such
  as `SELECT count(*)`. The library does not evaluate the other columns of a
  row, but it evaluates each strict field of each row that it writes.
- `DuckDBColumnType` has the method `duckdbTypeDeclaration`. It gives the SQL
  type that a typed table function and a typed aggregate function declare. A
  record or a sum type gets `ToDuckValue` and a STRUCT or UNION declaration
  through `ViaDuckDB`. `StructValue`, `TupleValue`, and `UnionValue` have no
  declaration, and the registration raises an error for them. An `Array` has
  no declaration, because its ARRAY type has no size, so a registration
  refuses an `Array` argument, result, parameter, or column, also inside a
  `Maybe`, a list, or a `Map`. A newtype of the `Array` with its own column
  type name, for example `BIGINT[3]`, declares that ARRAY. DuckDB casts a LIST
  of the same length to it with no error.
  `Database.DuckDB.Simple.LogicalRep` exports `TypeDeclaration`.
- An argument of `createFunction` can have each type with `FromField` and
  `DuckDBColumnType`, for example a record through `ViaDuckDB`, a list, a
  `Map`, or a newtype with its own type text. `FunctionArg` is gone. An
  argument type that is or contains VARIANT raises an error at registration,
  also for `createAggregate`. A GEOMETRY argument reads, also in
  `createAggregate` and as the source of `createCast`. An aggregate function of
  an extension refuses a GEOMETRY argument at registration, because its update
  callback has no factory to read the value.
- A result of `createFunction` can have each type with `ToDuckValue`. Its SQL
  type is the `duckdbTypeDeclaration` of the type: an `Int16` result is
  SMALLINT, a `Word16` result is USMALLINT, and a `Float` result is FLOAT, in
  place of BIGINT and DOUBLE. `ScalarType` is gone, and `FunctionResult` has an
  instance for each type with `ToDuckValue`.
- `Database.DuckDB.Simple.Registration` has `TypeDeclaration`, `parameter`,
  `defaultValue`, `registerCustomType`, and typed replacement scans.
  `registerReplacementScan` and `registerInstanceReplacementScan` give the
  parts of an unknown table name to a Haskell function. The function can return
  a SELECT statement that DuckDB reads instead.
- Add `registerCopyFromFunction`, `registerCopyFunction` with the record
  `CopyTo`, `CopyFromInfo`, and `registerMultiFileFunction` to
  `Database.DuckDB.Simple.Copy`. `registerCopyFromFunction` registers a COPY
  FROM side that gives a list of typed rows. `registerCopyFunction` registers a
  COPY TO side and a COPY FROM side under one name. DuckDB keeps the first
  registration of a format name and ignores a later one. Thus a format with two
  directions needs `registerCopyFunction`.
- Add `Database.DuckDB.Simple.Pragma`. `registerPragmaFunction` takes a typed
  callback `PragmaCallback args`. The callback gets the arguments of the call
  as a `FromRow` row.
- Add custom options to `Database.DuckDB.Simple.Config`. `customOptionUpdate`
  is a function that gets the new value as a `FieldValue`. An exception in the
  function rejects the value.
- A typed registration costs more for each row than raw vector access. Use the
  V2 modules for speed. Use them also for projection and filter pushdown,
  parallel table scans, and scalar functions with a bind callback or variadic
  parameters.
- Add `SettingScope`, `LogLevel`, `LogScope`, `TokenType`, `MacroFunctionType`,
  `ErrorType`, and `LogicalTypeId`. The configuration, logging, tokenizer,
  macro, error, and logical type APIs use these types instead of the generated
  V2 enum types, so a later C API version does not change these APIs. The file
  system API uses `FileFlag` and the opaque `FileHandle` instead of the
  generated V2 types. `ToDuckValue.toDuckValue` uses the synonyms
  `ValueFactory` and `NativeValue` for the two native handles in its signature.
- The functions with native handles are in modules with `V2` in the name.
  `Database.DuckDB.Simple.V2.Registration` has the scalar, aggregate, table,
  and cast registrations and the replacement scans with native callbacks. It
  also has `registerMacroFunctionWith`, which gives the native macro builder to
  an action. `Database.DuckDB.Simple.V2.Copy` has the native COPY callback
  interface: `CopyToCallbacks`, `CopyFromCallbacks`, the raw
  `registerCopyFunction`, `registerCopyFunctionWith`,
  `registerCopyToCallbacks`, `registerCopyFromCallbacks`, and
  `registerMultiFileFunctionWith`. `Database.DuckDB.Simple.V2.Pragma` has the
  pragma callbacks with native handles. `Database.DuckDB.Simple.V2.Config` has
  `registerCustomOptionWith`. `Database.DuckDB.Simple.V2.FunctionDocs` has
  `setFunctionDocs`. `Database.DuckDB.Simple.V2.Logging` has `logMessage`,
  which writes a message through the context of a native callback.
  `Database.DuckDB.Simple.V2.LogicalRep` has `logicalTypeToRep`,
  `logicalTypeFromRep`, `logicalTypeFromRepWith`, and `destroyLogicalType`.
- `Database.DuckDB.Simple.Internal` changes. It re-exports the native helper
  layer. It no longer exports `toSQLError`, `fetchPrepareError`,
  `fetchResultError`, `throwResultError`, `mkExecuteError`,
  `throwRegistrationError`, `withClientContext`, and `destroyClientContext`.
  `Connection` adds `connectionInstance`, `connectionLock`,
  `connectionWindow`, `connectionLockBound`, `connectionStripes`, and
  `connectionArrowBatch`. `ConnectionOpen` has no
  `connectionDatabase` and adds `connectionStatements` and
  `connectionReleaseParent`. `Statement` adds `statementArrowResult`,
  `statementBindings`, `statementParameterNames`, and
  `statementParameterTypes`.
  `StatementOpen` adds `statementParsed` and `statementSchemaCache`.
  `StatementStream` has no `statementStreamMode` and adds
  `statementStreamPendingChunks`. `StatementStreamColumn` has no
  `statementStreamColumnType` and adds `statementStreamColumnTree`.
- The SQL error of a failed Haskell callback has the plain message of the
  exception. Version 0.3 gave `user error (...)` for a `userError` and the
  Haskell record for an `SQLError`.
  The error of an argument that does not convert names the argument (`arg0`,
  `arg1`, and so on). Its text has one form for scalar functions, casts, table
  functions, and aggregate functions, for example
  `cannot convert argument arg0 from VARCHAR to Int64`. A table function and an
  aggregate function put their name before it:
  `duckdb-simple: NAME: cannot convert argument ...`.
- Add scoped named collection inputs and prepared dynamic PIVOT with a deferred
  output schema. Prepared dynamic PIVOT cannot have parameters.
- Rename `Database.DuckDB.Simple.Deprecated.Streaming` to
  `Database.DuckDB.Simple.Streaming`.
- Export Arrow release helpers from `Database.DuckDB.Simple.Arrow`.
- A DuckDB callback that waits for the lock of a connection waits at most the
  time limit of that connection. Add `connectionCallbackLockWait` to
  `ConnectionOptions`, in microseconds, with the default
  `defaultCallbackLockWait` (10 seconds). `Nothing` sets no time limit, and the
  open rejects a negative time limit. A statement of a callback on the
  connection that runs the callback thus raises an `SQLError` of the type
  `ErrorResourceInUse` after the time limit. For most callbacks, the outer
  statement then fails. The README gives the cases that the time limit does
  not cover.
- The README and the module header of `Database.DuckDB.Simple.Function` say that
  a program that registers a callback must be linked with `-threaded`. DuckDB
  can call a callback from several threads at the same time, and without
  `-threaded` such a program can crash. This is not a change of behaviour:
  version 0.3 needed `-threaded` in the same way, and its README named it only
  for prompt cancellation.
- Add `connectionAggregateStripes` to `ConnectionOptions`. It sets the number
  of stripes in the state table of each typed aggregate that `createAggregate`
  registers through the connection. The
  default is `defaultAggregateStripes` (64). The open rejects a number that is
  not positive. A registration on an extension uses the default.
- Add `connectionArrowBatchSize` to `ConnectionOptions`. It sets the largest
  number of rows in one Arrow array of an Arrow fold on the connection. The
  default is `defaultArrowBatchSize` (2048 rows). As in DuckDB, 0 means 131072.
  The open rejects a negative value.
- The library decodes the message of a native error with replacement
  characters. Thus `show` of such an `SQLError` does not raise for a text that
  is not valid UTF-8. Version 0.3 decoded the text with `decodeUtf8`, which
  raises for such a text.
- While an explicit stream is incomplete, DuckDB rejects other statements on
  its connection. Closing an incomplete stream aborts its statement.
- Each connection serializes statement preparation, its open native result,
  and each fetch of an explicit stream. Threads that share a connection wait
  for each other in the default query functions. Serialize transactions and
  explicit streams in the application.
- `query_` and `execute_` parse all statements before they run any.
- An INSERT, UPDATE, or DELETE without RETURNING gives one row with the number
  of changed rows in each function that reads rows, also in `fold`, `nextRow`,
  and the same functions of `Database.DuckDB.Simple.Streaming`.
- Decode and bind TUPLE values with `TupleValue`, `FieldTuple`, and
  `LogicalTypeTuple`.
- Preserve composite VARCHAR collations with `LogicalTypeCollatedVarchar`.
- Decode nanosecond timezone timestamps in results and VARIANT values.
- Preserve the digits below a microsecond of a `TimeOfDay`, a `LocalTime`, and
  a `UTCTime`, and of their `Unbounded` forms, for a nanosecond parameter
  (`TIME_NS`, `TIMESTAMP_NS`, or `TIMESTAMPTZ_NS`), also in collections,
  STRUCTs, MAPs, and UNIONs. For a `VARIANT` parameter, one such value keeps
  them when it fits the range of the nanosecond type, but the elements of a
  list, an `Array`, or a `Map` of such values get the microsecond value (see
  the entry on `VARIANT` parameters below). Only a value with such digits
  reads the parameter type. Each other parameter gets the microsecond value,
  as for the SQL literal. An explicit cast such as `?::TIMESTAMP_NS` makes a
  nanosecond parameter, so for a `TIMESTAMP` column DuckDB rounds the value to
  the microsecond, where a plain `?` cuts the digits. Version 0.3 bound the
  microsecond value for each parameter.
- Decode VARIANT values through public V2 operations. Object keys use DuckDB's
  lexicographic order.
- Allow a VARIANT object to contain an empty key with other named keys.
  Version 0.3 rejected such an object.
- A `Variant` parameter with a UNION, a MAP, an ENUM, or a GEOMETRY with a CRS
  in its payload, at any depth, raises `SQLError` with the type
  `ErrorInputInvalid`. The error gives the reason: a VARIANT keeps only the
  member value of a UNION, stores a MAP as a list of key and value objects,
  stores an ENUM as its text, and does not keep the CRS of a GEOMETRY. Version
  0.3 bound a UNION payload with no error.
- Read GEOMETRY results through a native cast to BLOB. DuckDB 2.0 rejects WKB
  with mixed Z/M member layouts.
- `ToDuckValue.toDuckValue`, `logicalTypeFromRep`, and `logicalTypeFromRepWith`
  take a V2 factory. Composite parameters keep GEOMETRY CRS metadata. A CRS
  that DuckDB does not recognize raises `SQLError` instead of being dropped.
- `logicalTypeFromRep` creates a VARIANT type, and a GEOMETRY type with a CRS
  that DuckDB recognizes. Version 0.3 raised an error for VARIANT and created
  GEOMETRY without the CRS.
- Add a `ToDuckValue` instance for `Variant`. Version 0.3 had none.
- A composite parameter that the native value constructor rejects raises an
  `SQLError`. For example, a STRUCT parameter with a MAP field that has a NULL
  key raises an `SQLError` with the type `ErrorInputInvalid`. Version 0.3
  raised an `IOException`.
- A connection constructs each name of `connectionGeometryCRS` directly when it
  opens. It queries a SQL-derived type only for a name that DuckDB does not keep,
  on the new connection, so the default list runs no query. The connection keeps
  each type, and a bind copies it. A name that DuckDB drops, for example a blank
  name, fails the open.
- A UNION parameter is built in a one-row UNION vector, because the V2 API has
  no constructor for a UNION value; this needs flat child vectors, which DuckDB
  2.0.0 gives a new vector, but its header does not state it.
- A STRUCT, TUPLE, or UNION parameter binds a `FieldGeometry` value as a
  GEOMETRY with the same WKB bytes and CRS, through DuckDB's cast from BLOB.
  A value whose CRS is not the CRS of its declared type raises `SQLError` with
  the type `ErrorInputInvalid`. A `Variant` parameter binds a `FieldGeometry`
  value without a CRS. One with a CRS raises `SQLError`, because a VARIANT does
  not keep the CRS. Version 0.3 raised an error for each such value, because
  DuckDB 1.5 has no cast from BLOB to GEOMETRY.
- Add a `ToField` instance for `RawGeometry`. It binds as a GEOMETRY with the
  same WKB bytes and its own CRS. DuckDB casts it to the type of the parameter,
  as in SQL text. A value without a CRS takes the CRS of the target. A target
  without a CRS removes the CRS. A target with another CRS raises `SQLError`.
  Version 0.3 had only a `FromField` instance for `RawGeometry`. An `Array`, a
  list, or a typed row cannot hold a `RawGeometry`.
- An empty `Array` parameter raises `SQLError`. Version 0.3 bound an empty
  `Array` of scalar elements. Bind an empty list for an empty LIST.
- `TimeWithZone` holds the offset in seconds east of UTC, as a TIMETZ stores
  it. The field `timeWithZoneZone :: TimeZone` of version 0.3 is now
  `timeWithZoneOffsetSeconds :: Int32`. A TIMETZ with an offset that has
  seconds, for example `12:00:00+01:00:30`, now reads. Version 0.3 raised an
  error for it. Update code that uses the old field. See MIGRATION.md.
- Add `ToField` instances for `IntervalValue`, `TimeWithZone`, and
  `DecimalValue`, and `ToDuckValue` instances for `IntervalValue` and
  `TimeWithZone`. An `IntervalValue` binds as an INTERVAL and a `TimeWithZone`
  as a TIMETZ, also in an `Array`, a list, or a typed row. A `TimeWithZone`
  reads back equal to the bound value, except that a time loses its digits
  below one microsecond. An offset outside -15:59:59 to +15:59:59 raises an
  error. A `DecimalValue` binds as a DECIMAL with its own width and scale. An
  `Array`, a list, or a typed row cannot hold a `DecimalValue`. Version 0.3
  had only `FromField` instances for these types.
- Add `ToField` and `ToDuckValue` instances for `Map`. A `Map` binds as a MAP
  value with its pairs in ascending key order. The keys and the values follow
  the element rules of a list, and an empty `Map` binds as an empty MAP of its
  key and value types. Version 0.3 had only a `FromField` instance for `Map`.
- Add `ToField` and `ToDuckValue` instances for lists. A list binds as a LIST
  value with the element rules of an `Array`, and an empty list binds as an
  empty LIST of its element type. A `String` stays one VARCHAR parameter, and a
  list as a row is still a row of parameters, so a list parameter is
  `Only [..]`. Version 0.3 had no `ToField` instance for lists.
- Each input check of the library raises `SQLError` with the type
  `ErrorInputInvalid`: a parameter value that does not fit its type (for example
  a `DecimalValue` outside its width, a `BitString` with bad padding, a
  `UnionValue` with a tag out of range, a nanosecond timestamp outside the
  storage of its type, or an empty `Array`), a logical type with a NUL in a name
  or a bad DECIMAL width, a CRS name that DuckDB does not keep, a NUL in a
  database path, an alias, an attach option, a configuration, a query, or a
  parameter name, a NUL in a file path, a negative file offset or read size, a
  file range outside `Int64`, a bad `ConnectionOptions` field, SQL text with no
  statement or with more than one statement, a parameter index out of bounds, a
  column index of `columnName` out of bounds, a bad name or argument of a
  registration, a catalog lookup, a configuration option, or a log sink, the
  column count of a COPY FROM table, and a VARIANT argument of a typed function,
  a table function, a pragma, a COPY function, or a custom option. An error
  about a statement has the query of the statement, and each other error has no
  query. A date or a timestamp outside the range of DuckDB, and an integer
  outside the range of its parameter type, raise `SQLError` with the type
  `ErrorInputOutOfRange`. A parameter count or a parameter name that does not
  match the query keeps its `FormatError`. A check in the callback of a scalar
  function fails the query, as each other exception of a callback does. Version
  0.3 raised an `IOException` for the checks of parameter values, logical types,
  and CRS names. The error for a scalar value that does not match its declared
  type names the SQL type, for example `NULL` in place of
  `LogicalTypeIdSQLNull`.
- A `TimeOfDay`, a `LocalTime`, or a `UTCTime` with digits below a microsecond,
  and a `FieldTime`, a `FieldTimestamp`, or a `FieldTimestampTZ` with such
  digits, binds with the nanosecond type for a nanosecond parameter, and for a
  `VARIANT` parameter when it fits the range of that type, so the VARIANT keeps
  the digits. A value with no such digits binds as before, with no read of the
  parameter type. These values get the microsecond value: a value outside the
  range of the nanosecond type, an element of a list, an `Array`, or a `Map`
  for a `VARIANT` parameter, and an element of a `VARIANT[]` or a
  `MAP(..., VARIANT)` parameter. In a `Variant` payload, each time leaf, also
  an element of a list payload and a field of an object payload, binds with the
  nanosecond type when it has such digits and fits the range.
- An empty `Array` and an empty `FieldArray` raise the same `SQLError`, of the
  type `ErrorInputInvalid`.
- Add a parameter instance for `FieldValue`, so a row read as `[FieldValue]`
  binds back. A scalar constructor binds as the DuckDB type of the
  constructor, and a `FieldList`, a `FieldArray`, or a `FieldMap` takes the
  element type that its elements give, with the rule of a SQL list literal.
  A `FieldEnum` binds its label as a VARCHAR. An empty `FieldArray` raises
  `SQLError` with the type `ErrorInputInvalid`. A `FieldTimestamp`, a
  `FieldTime`, and a `FieldTimestampTZ` bind with the microsecond type. A value
  with digits below a microsecond binds with the nanosecond type for a
  nanosecond parameter, and for a `VARIANT` parameter when it fits the range of
  that type.
- An `Array`, a list, or a `Map` of STRUCT values, UNION values, generic
  records, lists, or arrays with different element types gets the type that
  DuckDB gives a SQL list literal of the elements, for example
  `STRUCT(a INTEGER, b INTEGER)` for `{'a': 1}` and `{'b': 2}`. Elements with
  no common type raise the `SQLError` of DuckDB. Version 0.3 raised an error
  for each `Array` with different element types.
- A list or a `Map` of such elements that has no element, and an `Array`, a
  list, or a `Map` with only NULL elements, take the element type of the
  parameter when the parameter has a known type, for example for an `INSERT`
  into a typed column. An empty `Array` raises an error, as an entry above
  says. For a parameter with no known type, such a list of generic records
  through `ViaDuckDB` takes the STRUCT or UNION type of the record. Each other
  such list or `Map` with only NULL elements takes the type of the SQL list
  literal, such as `"NULL"[]`, and each other empty list or `Map` takes the
  type NULL, as `[]` and `MAP {}` have. Version 0.3 raised an error for an
  `Array` of NULL elements.
- Some parameters bind for the type that DuckDB gives the parameter: a time
  value with digits below a microsecond, an `Array`, a list, a `Map`, a manual
  STRUCT, TUPLE, or UNION value, and an `Integer` or a `Natural` outside the
  HUGEINT range. `executeMany` reads these types one time for each call. A
  column type that changes during the call does not change how the later rows
  of that call bind.
- An `Integer` or a `Natural` parameter binds as a HUGEINT value when the value
  is in the HUGEINT range. Before, it bound as a BIGNUM value in each case. In
  DuckDB 2.0.0 the casts from BIGNUM to the integer types have three defects. A
  cast to HUGEINT, UHUGEINT, or DECIMAL fails. A cast of a negative value to an
  unsigned type gives a wrapped value. A cast of a value of 2^128 or more gives
  the value modulo 2^128. DuckDB checks the range of a HUGEINT value for its
  target. A value outside the HUGEINT range binds as BIGNUM, or as UHUGEINT for
  a UHUGEINT parameter that can hold it. For each other integer or DECIMAL
  parameter, such a value raises an `SQLError` of the type
  `ErrorInputOutOfRange`. `typeof(?)` is thus HUGEINT for an `Integer` in the
  HUGEINT range. Bind a `BigNum` to bind a BIGNUM value in each case. An `Array`
  of `Integer` or `Natural` values takes the element type of its target when
  that type is an integer type or DECIMAL, so DuckDB checks the range of each
  element. Before, a negative element for an unsigned array column was stored as
  a wrapped value.
- `toDuckValue` for an `Integer` or a `Natural` gives a HUGEINT value when the
  value is in the HUGEINT range, and a BIGNUM value for each other value.
  Before, it gave a BIGNUM value in each case. DuckDB checks the range when it
  casts a HUGEINT value. Thus `setConfigValue` with -5 for an unsigned option
  raises an error, and a default value from `defaultValue` that its parameter
  cannot hold raises an error at a call that omits the argument. A value
  outside the HUGEINT range keeps the BIGNUM casts of DuckDB: `setConfigValue`
  with 2^128 + 5 for an unsigned option sets 5. The column type of an
  `Integer` or a `Natural` in a typed row stays BIGNUM.
- A generic record with an `Array` field that has elements binds. The value
  gives the ARRAY size. When no value gives the size, DuckDB rejects the record
  with "ARRAY type size must be at least 1". Examples are an `Array` field with
  the value `Nothing`, an `Array` inside a LIST, ARRAY, or MAP field, and a sum
  type with an `Array` constructor.
- A product type with no fields has one encoding: the STRUCT without fields,
  which DuckDB shows as `{}`. `genericToFieldValue` gives this STRUCT, as the
  `ViaDuckDB` binding and `genericToStructValue` did before. A constructor with
  no fields in a sum type has this STRUCT as its payload. This includes each
  constructor of an enum-like sum type, such as `data Colour = Red | Blue`.
  Version 0.3 gave `FieldNull` in these two places, so `Just x` and `Nothing`
  of such a type were the same value in a field. The decoders accept
  `FieldNull` and the STRUCT without fields.
- A value that is not NULL with the declared type SQLNULL raises a conversion
  error. Before, it was bound as NULL with no error. This applies to a manual
  `StructValue`, `TupleValue`, or `UnionValue`, to a LIST of that type, and to
  a `Variant` that holds such a value.
- `errSQLType` of a `ResultError`, and the `SQLError` message of a failed
  conversion, give the DuckDB type names `TIMESTAMPTZ` and `TIMETZ`. Earlier
  versions gave `TIMESTAMP_TZ` and `TIME_TZ`, which DuckDB does not accept as
  type names.
- `errSQLType` of a `ResultError`, and the `SQLError` message of a failed
  conversion, give the type names of DuckDB's `typeof`: `TINYINT`, `SMALLINT`,
  `INTEGER`, `BIGINT`, and `VARCHAR`. Earlier versions gave `INT1`, `INT2`,
  `INT4`, `INT8`, and `TEXT`. The errors of DuckDB use `INT8` for the 8-bit
  type, not for `BIGINT`.
- `duckdbColumnType` gives `VARCHAR` for `Text` and `String`. Earlier versions
  gave `TEXT`.
- A DECIMAL read as `Double` or `Float`, and a BIGINT read as `Float`, give
  the value of the cast of DuckDB to DOUBLE or FLOAT. Earlier versions rounded
  two times, so the value could differ from the cast in the last bit. For
  example, `2706284552926637.70::DECIMAL(18,2)` read as `2706284552926638.0`,
  and the cast gives `2706284552926637.5`. For scale 0 and a width of 19 to 38,
  the cast of DuckDB does not always give the nearest value, and the library
  gives the value of the cast.
- A BOOLEAN read as `Text` or `String` gives `true` or `false`, as the cast to
  VARCHAR does. Earlier versions gave `1` or `0`.
- A `FromField` instance reads another SQL type only when it gives the value
  of the cast of DuckDB, or an error. The Haddock of each instance gives the
  SQL types that it reads. MIGRATION.md gives the SQL to write in place of each
  read that the next three entries remove.
- A TIMESTAMP, TIMESTAMP_S, TIMESTAMP_MS, TIMESTAMP_NS, or DATE read as
  `UTCTime`, and a TIMESTAMPTZ or TIMESTAMPTZ_NS read as `LocalTime`, raise the
  error of an incompatible column, also in the `Unbounded` forms. The casts of
  DuckDB use the TimeZone setting of the session. Earlier versions used UTC.
- A FLOAT or DOUBLE read as `Text` or `String` raises the error of an
  incompatible column. Earlier versions gave the text of `show`, for example
  `1.0e-2` for 0.01, where the cast gives `0.01`.
- A VARCHAR read as `ByteString` raises the error of an incompatible column.
  Earlier versions gave the UTF-8 bytes, where the cast to BLOB reads escape
  sequences.
- `FieldEnum` holds the index and the label of an ENUM value:
  `FieldEnum Word32 Text`. An ENUM value reads as `Text` or `String` with its
  label, as the cast to VARCHAR gives it. Earlier versions raised an error. An
  ENUM value read as an integer type raises the error of an incompatible
  column. Earlier versions gave the index, where the cast of DuckDB parses the
  label. The reader reads a label through the dictionary of the type and does
  not cast. A result column reads each label that it needs one time for each
  execution. The readers of a MAP key or value, of an argument, and of a COPY
  TO row read the labels again for each vector. A child of a STRUCT, a TUPLE,
  or a UNION reads the whole dictionary one time for each execution.
- A `FieldEnum` inside a value of a declared ENUM type binds its label, not its
  index. A label that is not in the dictionary raises `SQLError`.
- `displayException` of a `ResultError` gives the text of the error, with the
  column and the two types, as the `SQLError` of a column. Earlier versions
  gave the derived `show`. The error of a field of a `ViaDuckDB` record names
  the field, for example
  `duckdb-simple: field stampedAt: a value has type TIMESTAMP but expected UTCTime`.
- A list field of a generic record reads an ARRAY, as a top-level list does.
  Earlier versions read only a LIST in a field.
- The errors of the generic decoders name the SQL type of a value, for example
  `duckdb-simple: field xs: a value has type MAP but expected LIST or ARRAY`.
  Earlier versions printed the derived `show` of the whole value.
- Two bind errors name the SQL type of the value in the same way:
  `cannot encode a value of type INTEGER as ...` and
  `unsupported scalar conversion for BIGINT from a value of type VARCHAR`.
  Earlier versions printed the derived `show` of the whole `FieldValue`.
- A `Variant` read accepts only a value that a VARIANT can hold. A column that
  holds an ENUM, a MAP, a UNION, an ARRAY, a TUPLE, a STRUCT that is not in the
  form of a VARIANT object (each field of the type VARIANT, the keys in
  ascending order), or a GEOMETRY with a CRS, at any depth, raises the error of
  an incompatible column. Earlier versions wrapped the value, which DuckDB's cast
  to VARIANT changes. A JSON column reads as its text, where the cast gives the
  parsed value.
- Allow session variables to supply omitted named parameters. Too few named
  parameters no longer raise `FormatError`. Too many named parameters still
  raise it.
- Reject duplicate named parameters with DuckDB's ASCII case rules. Keep
  distinct non-ASCII names separate. Preserve sparse numeric parameter names
  such as `$3`.
- Resolve configuration names and aliases case-insensitively. Use DuckDB's
  ASCII identifier rules for catalog names and top-level schemas.
- `deleteFunction` reports the error from DuckDB. DuckDB does not permit
  deletion of functions registered through the C API, so the call raises
  `SQLError` for a function made with `createFunction`. Version 0.3 ignored
  this error. A `WARNING` pragma on `deleteFunction` causes a compile-time
  warning at each use. A build with `-Werror` must allow that warning, for
  example with `-Wno-error=deprecations`.
- Add the optional `quack-tests` suite for authenticated remote queries,
  parameter binding, writes, streaming, and nanosecond timestamps.

## 0.3.0.0

- Use native DuckDB 1.5.6 by default. Keep native support for DuckDB >= 1.5.3
  and < 1.6. Test native versions 1.5.3 through 1.5.6.
- Require `duckdb-ffi >= 1.5.6.0` for the updated native bindings.
- Decode VARIANT results to the `FieldValue` of the stored value, so existing
  `FromField` instances read them. Objects decode to `FieldStruct` values with
  VARIANT fields, in entry order. The decoder checks DuckDB's private 1.5
  format and payload bounds, and rejects cyclic child references. It has no
  depth limit for acyclic values.
- Add `Variant`, a `FieldValue` that binds as a VARIANT, and `variantObject`,
  which builds an object payload. Scalars keep their native type. Object
  parameters reject duplicate and NUL-containing keys, and objects with only
  an empty key, because the native constructors cannot represent these names.
  `Variant` has no `ToDuckValue` instance, and `logicalTypeFromRep` raises an
  error for VARIANT.
- A VARIANT timestamp payload outside the microsecond range binds as a
  millisecond or second timestamp, so it keeps its value.
- Read the VARIANT type and GEOMETRY types for a list of CRS definitions,
  because the C API cannot create them. A connection reads them with one
  query on a separate connection, the first time a parameter needs one.
  Parameters use these types. Add `ConnectionOptions`, `openWithOptions`, and
  `withConnectionWithOptions` to set the CRS list, which defaults to
  `OGC:CRS84`.
- Array parameters use the element's `ToField` instance. Elements require
  `ToField` and `DuckDBColumnType`. Add a `ToDuckValue` instance for arrays
  whose elements have `ToDuckValue`. It does not need a connection.
- Array parameters of STRUCT values, UNION values, generic records, and
  arrays bind. They take the element type of the first element that is not
  NULL. The other non-NULL elements must have the same type, including field
  names, decimal precision and scale, and nested types. Different types raise
  an error, so DuckDB cannot cast away fields or round values. An empty or
  all-NULL array of these elements raises an error. Scalar elements keep the
  type of their column type name.
- A GEOMETRY payload decodes to `FieldGeometry` with raw WKB. Import those
  bytes with `ST_GeomFromWKB(?)::VARIANT`.
- Add parameter and result instances for `Data.Geometry.Geometry` from
  `geometry-simple`. It provides decoded shapes, unboxed coordinate vectors,
  and runtime coordinate layouts. This type does not store CRS metadata.
  `RawGeometry` retains WKB and CRS without decoding coordinates. Existing
  `ByteString` results still return WKB. Empty points remain distinct from SQL NULL.
- Use `geometry-simple >= 0.1.1.0` for pure geometry types and codecs.
  Structured parameters use its WKT writer and DuckDB's native cast.
  `RawGeometry` has no parameter instance. Import its WKB with
  `ST_GeomFromWKB` and apply its CRS with `ST_SetCRS`. This preserves mixed
  layouts, dimensional empties, and native NaN payloads without WKT conversion.
- Add `FieldGeometry` and `LogicalTypeGeometry` to the public value and type
  representations. Update exhaustive matches when upgrading.
  Nested result decoding retains raw WKB and CRS. Generic parameters with
  non-NULL `FieldGeometry` values raise an error instead of converting their
  bytes through WKT. Construct those nested values with explicit SQL import.
- Keep CRS metadata in this package. The standalone shape has no CRS.
  Both forms own their memory and remain usable after the connection closes.
  CRS text can hold an identifier, a custom name, or a full WKT2/PROJJSON
  definition.
- Composite parameters keep GEOMETRY CRS metadata for the CRS definitions
  that the connection read. Other CRS definitions bind without a CRS, and
  `logicalTypeFromRep` creates `GEOMETRY` with no CRS. Insert the value into a
  column with a CRS, or cast it in SQL, to apply the CRS.
- Read STRUCT and UNION type metadata once for each result vector instead of
  once for each row. Prepare their direct child readers at the same time.
  A local benchmark with a CRS-tagged GEOMETRY field ran about ten times faster.
  LIST, ARRAY, and MAP decoders can still read child metadata for each parent row.
- `Database.DuckDB.Simple.Internal` changes. `StatementStreamChunk` keeps one
  reader for each column in `statementStreamChunkReaders`, and
  `StatementStreamChunkVector` is removed. The module also exports
  `withTypeCache`, `fetchPrepareError`, `duckDBTypeFromName`, and
  `duckDBTypeToName`.

## 0.2.0.0

### Query execution and resource lifetime

- Add `Database.DuckDB.Simple.Deprecated.Streaming` for callers that need native
  streaming. It provides row folds, cursors, and Arrow folds with a deprecation
  warning. Both execution modes share decoding and resource cleanup. Native
  chunk fetching is interruptible, including cleanup of a chunk fetched just
  before cancellation. Default APIs continue to use materialized execution.
- Previously, a long native query could defer Ctrl-C until execution finished.
  Query preparation and execution now run in a worker so the caller can receive
  asynchronous exceptions. Cancellation interrupts DuckDB and waits for the
  worker before releasing its resources. Prompt cancellation requires the
  threaded RTS. Native code and Haskell callbacks must return before cleanup
  can finish.
- Add scoped Arrow batch export through `foldArrow` and `foldArrow_`. The
  callback receives a separate schema and array for each batch. Consumers may
  release or move them under the Arrow C Data Interface. The fold releases
  remaining contents on every exit path. This uses the supported schema/chunk
  conversion API and retains a materialized native result while the fold runs.
- The initial Arrow fold shared one borrowed schema across callbacks. Consumers
  such as `dataframe-arrow-bridge` release that schema when they import a batch,
  which made subsequent batches unusable. Each batch now has its own schema.
  Moved root objects remain usable after query and connection close; their
  consumer must release them.

- Previously, cursors decoded rows with prepare-time column types. Parameter
  binding or schema rebinding could change those types and cause truncated
  values or invalid memory access. Cursors now read types from the executed
  result. This addresses the streaming failures in #18.
- Cursors use the supported materialized execution API. DuckDB 1.5 provides
  native streaming only through deprecated entry points, which the default
  interface does not use. Folds decode one row at a time, but native result
  memory still depends on the result size. Fetch failures now produce SQL
  errors; previously, they were indistinguishable from end of input.
- Previously, reading past EOF could execute the statement again, including
  INSERT statements. Cursors now remain exhausted until an explicit reset.
  Clearing bindings also destroys any active result.
- Previously, exceptions during result decoding could skip native destruction.
  Results, chunks, and nested logical types now have exception-safe cleanup.
  Cursor state records chunk ownership before decoding and clears it before
  destruction, so cancellation cannot cause a leak or a second destruction.
- Previously, GC could finalize a connection or statement during its last
  native call. The binding now keeps the Haskell owner alive until the call
  returns. Reads through a closed parent connection fail before native access.
- Previously, streaming rejected STRUCT and UNION columns even though the
  shared decoder supported them. Eager queries and cursors now use the same
  row decoder, including nested collections and NULLs.
- Previously, a rollback failure could replace the exception from the user's
  transaction. The original exception is now preserved. A failed commit also
  attempts rollback.

### Callbacks and native helpers

- Previously, failed scalar, COPY, or logging registration could free callback
  resources twice. Registration now transfers ownership once and releases
  resources acquired before a failure. Static destructors avoid allocating
  a separate destructor callback for each registration.
- Previously, callback closures or query state could remain live on a long-lived
  connection after replacement or execution. Cleanup now releases scalar
  worker state, COPY state, and replaced closures before connection close.
- Previously, exceptions during callback initialization or error formatting
  could escape into native code. Scalar and COPY callbacks now report SQL
  errors. Logging callbacks contain exceptions because their API has no error
  channel. This includes asynchronous exceptions raised inside a callback.
- Previously, DuckDB could skip a scalar callback when an argument was NULL.
  Callbacks now receive those arguments. Use `Maybe` to accept NULL; a
  non-nullable Haskell argument produces a conversion error.
- Previously, Word and Word64 scalar results used signed BIGINT storage and
  could overflow. They now use UBIGINT. Float callbacks preserve NaN, infinity,
  and negative zero without depending on compiler optimization rules.
- Catalog, configuration, and filesystem helpers now bracket native allocations
  on failure paths. File reads reject sizes that cannot fit a Haskell buffer.
  Unsupported catalog entry kinds fail before the native lookup.

### Value conversion

- Previously, temporal infinity could abort the process or decode as an
  unrelated finite value. `Database.DuckDB.Simple.Time` now provides `Unbounded`,
  `Date`, `LocalTimestamp`, and `UTCTimestamp` to read and bind both infinities.
  Ordinary `Day`, `LocalTime`, and `UTCTime` report a conversion error for
  infinity. Floating-point NaN and infinities remain supported.
- `FieldDate`, `FieldTimestamp`, and `FieldTimestampTZ` now hold `Unbounded`
  payloads. Wrap existing finite payloads in `Finite`. Custom `FromField`
  instances can inspect infinity before conversion. Generic composites and
  nested collections preserve infinity and NULL separately.
- Previously, binding dates outside native storage limits could abort in C++.
  Finite date/time conversions now use checked epoch arithmetic and reject
  out-of-range inputs with Haskell exceptions.
- Previously, TIMESTAMP_S and TIMESTAMP_MS decoding multiplied Int64 values
  into microseconds and could overflow. Each timestamp family now retains its
  own units. Composite TIMESTAMP_S/MS/NS and TIME_NS values can be rebound
  without losing units, nanoseconds, or typed NULLs.
- Previously, UTCTime parameters had SQL type TIMESTAMP and could change their
  meaning under a non-UTC session timezone. They now have type TIMESTAMPTZ.
- Previously, Float parameters used DOUBLE, which hid a missing REAL decoder.
  Float parameters now use FLOAT, and REAL results decode to Float or Double.
  NaN, infinity, and negative zero remain supported. Narrowing a finite Double
  that exceeds Float's range now fails instead of producing infinity.
- Previously, Int8 and unsigned narrowing conversions could wrap out-of-range
  values. They now report conversion errors. Intermediate signed and unsigned
  values remain 64-bit until the target bounds have been checked.
- Previously, text parameters were terminated at an embedded NUL. Text and
  String parameters now pass their UTF-8 byte length and preserve NULs. SQL,
  native names, paths, and configuration strings reject NUL to prevent silent
  truncation. Native names and error messages are decoded as UTF-8.
- DECIMAL values retain their exact integer representation. Invalid width,
  scale, or magnitude now fails before native construction. Invalid ENUM
  indexes, UNION tags, and composite constructors also produce controlled errors.
- Previously, BIT padding could give incorrect SQL bit counts. Padding now
  follows DuckDB's representation; unsupported empty or malformed inputs fail
  before native use.
- Previously, generic records and sums decoded by position. They now match
  field and member names and reject incompatible schemas. NULL non-nullable
  products report a conversion failure instead of reaching a partial `error`.
- Previously, a NULL UNION payload could lose its declared member type. Typed
  NULL payloads now retain that type through native construction.
- TIMETZ offsets containing seconds now fail when decoding to Haskell's
  minute-based TimeZone. Invalid clock components and offsets fail before
  binding instead of being rounded or narrowed silently.

### Testing and compatibility

- Add an optional DataFrame integration suite with `-fdataframe-tests`. It
  imports several Arrow batches through `dataframe-arrow-bridge` and checks
  values, NULLs, column order, consumer failures, and use after connection close.
  CI runs it on Linux and macOS. The ordinary suite also checks consumption,
  ownership transfer, and cleanup after a consumer has released its objects.
- Add crash reproductions for #18, real native ownership tests, and sustained
  checks for long-lived connections, callback release, and cancellation.
  Property tests now include embedded NUL rather than filtering it out.
- Add repeatable benchmarks with checked results. Linux CI checks callback
  and cancellation workloads under Valgrind.
- Raise the minimum native DuckDB version to 1.5.3.
- Use GHC 9.14.1 by default. Test the latest stable patch release in each GHC
  series from 9.6 to 9.14.

## 0.1.5.2
- Fix a connection leak: `close` and the connection finalizer built the close action but then discarded it, so the DuckDB connection and database handles stayed open. Every leaked database instance also kept its own DuckDB thread pool alive. (Reported by @winitzki, see #15.)
- Fix the same defect in `closeStatement`, which discarded the action that destroys the prepared statement. (Fixed by @bgamari in #15.)
- Add a `duckdb-simple-leak-test` test suite that runs many open/close cycles and fails if the thread count or the resident set size of the process grows.

## 0.1.5.1
- Re-export the `RowParser` data constructor from `Database.DuckDB.Simple.FromRow`, restoring the API that `0.1.5.0` unintentionally broke (see #6). Downstream packages such as `beam-duckdb` rely on this constructor. (Sorry!)

## 0.1.5.0
- Raise the supported DuckDB runtime baseline to `1.5.0+` via the `duckdb-ffi-1.5` dependency line.
- Add DuckDB 1.5 high-level wrappers for startup config inspection and connection setup, catalog lookup, file-system handles, copy-function registration, and custom log storage registration.
- Extend scalar-function support with `createFunctionWithState`, allowing thread-local per-worker execution state backed by the new DuckDB 1.5 scalar init API.
- Deduplicate shared helpers (`withClientContext`, `destroyValue`, `destroyLogicalType`, etc.) into `Internal.hs`.
- Update the test suite for DuckDB 1.5 behavior, including `TIME_NS` decoding and new wrapper coverage for copy/logging/config/catalog/file-system helpers.

## 0.1.2.3
- Move DuckDBColumnType requirement for ToField to DuckValue

## 0.1.2.2
- Add support for reading and writing arrays.
- Added `Database.DuckDB.Simple.Generic` with GHC generics helpers (`GToField`/`GFromField`) for encoding records as DuckDB STRUCTs and sum types as UNIONs, plus a `ViaDuckDB` newtype for convenient @DerivingVia@ support.
- Added `DuckValue` instances for `[]`, `Array Int a`, and `Map k v` to enable lists, arrays, and maps as fields within generically-encoded structs and unions.
- Documented struct/union generic support; added richer tests covering database round-trips and deriving-via examples.
- Support decoding and binding STRUCT and UNION values via new `StructValue`/`UnionValue` helpers and corresponding `FromField`/`ToField` instances.

## 0.1.2.0
- Added LIST/MAP coverage note: LIST columns decode into Haskell lists and MAP columns into strict `Map k v`, with matching parameter bindings via `ToField`.
- Taught `FromField` to interpret `FieldBigNum` as `Integer`/`Natural` and added matching `ToField` instances for `BigNum`, `Integer`, and `Natural` so BIGNUM parameters round-trip without truncation.
- Added `duckdbColumnType` helper and `DuckDBColumnType` class, exposing the DuckDB column type associated with each `ToField` instance.
- Fixed UUID decoding by undoing DuckDB’s upper-word bias and added a UUID round-trip regression test.
- Fixed BIT encoding and decoding

## 0.1.1.2
- Broadened `FieldValue` and `FromField` coverage to handle all DuckDB scalar types, including intervals, HUGEINT/UHUGEINT, decimals (with metadata), time/timestamp with additional precisions, timezone-aware values, bit strings, bignums, and enums.
- Fixed enum decoding for both query results and scalar-function inputs by honouring the logical type’s underlying storage width (uint8/uint16/uint32).
- Ensured decimal vectors read accurate width/scale metadata when materializing results or invoking UDFs.
- Added unsigned `ToField` bindings that route through DuckDB’s native uint creators and exposed `FromField Word`.
- Expanded the test suite with regressions covering unsigned round-trips, huge integers, intervals, decimals, and timezone-aware values.

## 0.1.0.0
- Initial release
