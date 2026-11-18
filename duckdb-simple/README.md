# duckdb-simple

`duckdb-simple` is a Haskell interface to DuckDB. Its API is like the APIs of
[`sqlite-simple`](https://hackage.haskell.org/package/sqlite-simple) and
[`postgresql-simple`](https://hackage.haskell.org/package/postgresql-simple).
It uses the [`duckdb-ffi`](https://hackage.haskell.org/package/duckdb-ffi) bindings.
It opens connections, runs queries, binds parameters, and decodes results into
Haskell types. It supports DuckDB scalar types, for example signed and unsigned integers,
decimals, HUGEINT and UHUGEINT, intervals, dates, times, and timestamps of each
precision and with a time zone, blobs, enums, bit strings, and bignums.

The backend uses the generated DuckDB V2 API. Upstream marks the complete V2
API as unstable: "The C API v2 is not yet stable. Its interface may change in
upcoming releases." This release therefore requires exactly DuckDB 2.0.0.
Creating an environment raises `SQLError` for another library version.

The examples use the `OverloadedStrings` extension for `Query` and `Text`
literals.

## Shared environments and instances

Use an explicit instance when several connections must share catalogs and
configuration. Each connection has at most one open native result. Use a
separate connection for each concurrent worker.
Wait for child creation and all worker operations to finish before calling
`closeInstance` or `closeEnvironment`. Closing a parent also closes its children.

```haskell
withEnvironment $ \environment ->
  withInstance environment [("threads", "2")] $ \instance_ -> do
    attachDatabase instance_ ":memory:" "shared" []
    setDefaultDatabase instance_ "shared"
    withConnectionOn instance_ $ \writer -> do
      execute_ writer "CREATE TABLE items(id BIGINT)"
      execute writer "INSERT INTO items VALUES (?)" (Only (42 :: Int))
    withConnectionOn instance_ $ \reader ->
      query_ reader "SELECT id FROM items" :: IO [Only Int]
```

Closing an environment closes its instances, connections, and statements.
DuckDB can keep an instance alive that `createInstance` did not create. In
that case, `closeEnvironment` raises an `SQLError` and keeps the environment open.
Closing an instance closes its connections and statements. Each close operation
is idempotent. `open` and `withConnection` continue to create private parents.

## V2 facilities

- `Config` lists options with effective values, typed defaults, aliases, and
  supported scopes. It reads and writes typed values and registers custom
  options. The update function of a custom option gets the new value as a
  `FieldValue`. `V2.Config` registers custom options with native builders.
- `Catalog.describeTable` copies resolved names, column types, default flags,
  generated-column flags, and catalog read-only status. A column type keeps the
  alias of DuckDB at each depth as a `LogicalTypeAlias`, for example JSON and
  the name of a type from `registerCustomType`. A column of a type from
  `CREATE TYPE name AS base` has the base type, and an ENUM from `CREATE TYPE`
  has no name.
- `V2.LogicalRep.logicalTypeToRep factory logical` copies a native logical type
  into a pure `LogicalTypeRep`. Keep the factory valid throughout the
  conversion. The function does not destroy the logical type handle.
- `Tokenizer.tokenize` returns token kinds, UTF-8 byte offsets, token bytes, and
  the native indication of an unterminated final token.
- `Function` registers scalar, aggregate, and table functions with
  Haskell values. `Registration` registers custom types and replacement scans,
  and has the type and parameter declarations. See
  [Typed registration](#typed-registration).
- `V2.Registration` supports scalar, aggregate, table, cast, and custom-type
  registration, replacement scans, function properties, and parameter kinds,
  names, and defaults. Haskell callbacks use typed V2 info handles. The library
  retains their function pointers and contains callback exceptions. The advanced
  extension builders accept a borrowed native extension handle and context.
  Scalar type resolution runs before binding. Table callbacks can report
  multi-file column identifiers and metadata after binding.
- `Copy` registers COPY formats and multi-file table functions with Haskell
  values. `V2.Copy` has the native COPY callbacks, with batch transfer, flush,
  statistics, progress, and multi-file setup.
- `FileSystem` supports sequential and positioned reads and writes, file size,
  position, seek, sync, path tests, expansion, globbing, movement, and removal.
- `Logging` registers native log sinks with managed write and flush callbacks.
  It copies timestamps, severity, scope, and available context identifiers.
  Callbacks can run concurrently. They must not log or change logging settings.
  `V2.Logging` writes a message through the context of a native callback.
- `Pragma` registers query and execution callbacks. A callback gets the
  arguments of the call as a `FromRow` row. `V2.Pragma` has the callbacks that
  get native handles.
- `Extension` has the typed registrations for a loadable extension. It also
  registers scalar and table macros during extension initialization. The
  native macro builder has no connection target; use SQL `CREATE MACRO` on a
  connection. `V2.Registration.registerMacroFunctionWith` gives the native
  macro builder to a configuration action.
- `V2.FunctionDocs` sets descriptions, examples, and categories on function
  builders.
- `QueryMetrics` enables profiling and copies query and operator metric trees.
- `Internal.withCollectionStatement` binds native column collections as named
  table inputs for one scoped prepared-statement action. The caller retains the
  collections.

The pinned V2 API does not expose custom file-system registration or collation
lookup. DuckDB 2.0 rejects an ARRAY type with zero elements. An empty LIST is
valid. The direct `query_` and `execute_` paths support statements that expand
during parsing, including dynamic PIVOT. Prepared dynamic PIVOT supports
repeated execution with a changing schema. It does not support parameters or a
cacheable plan. Read its schema after execution starts. VARIANT decoding uses
SQL metadata and native casts on a helper connection. An unknown custom
GEOMETRY CRS label uses a helper SQL query when a parameter needs its type.
GEOMETRY results use a native cast to BLOB. Native GEOMETRY collections require
the same Z/M dimensions on every member, including empty members. The standalone WKB codec
retains mixed layouts.

## Getting started

```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

import Database.DuckDB.Simple

main :: IO ()
main =
  withConnection ":memory:" \conn -> do
    _ <- execute_ conn "CREATE TABLE items (id INTEGER, name TEXT)"
    _ <- execute conn "INSERT INTO items VALUES (?, ?)" (1 :: Int, "banana" :: String)
    rows <- query_ conn "SELECT id, name FROM items ORDER BY id"
    mapM_ print (rows :: [(Int, String)])
```

### Key modules

- `Database.DuckDB.Simple`: connections, prepared statements, execution,
  queries, metadata, and error handling.
- `Database.DuckDB.Simple.ToField` / `ToRow`: typeclasses and helpers for
  preparing positional or named parameters.
- `Database.DuckDB.Simple.FromField` / `FromRow`: typeclasses for decoding
  query results, with generic deriving support for product types.
- `Database.DuckDB.Simple.Conversion`: explicit IO and conversion errors for
  custom decoders. `runConversion` returns `IO (Ok a)`.
- `Database.DuckDB.Simple.Generic`: automatic encoding/decoding of Haskell
  ADTs as DuckDB STRUCTs and UNIONs via GHC generics and the `ViaDuckDB`
  deriving-via helper.
- `Database.DuckDB.Simple.LogicalRep`: structured value types (`StructValue`,
  `TupleValue`, `UnionValue`) for working with DuckDB's composite types.
- `Database.DuckDB.Simple.Types`: shared types (`Query`, `Null`, `Only`,
  `(:.)`, `SQLError`).
- `Database.DuckDB.Simple.Function`: register scalar Haskell functions that
  SQL can call, and casts, aggregate functions, and table functions.
- `Database.DuckDB.Simple.Registration`, `Copy`, `Pragma`, and `Config`:
  register custom types, replacement scans, COPY formats, pragmas, and custom
  options with Haskell values.

[Types](#types) lists the SQL type of each Haskell type.
[Errors](#errors) describes the exceptions that a program can catch.

## Querying data

```haskell
import Database.DuckDB.Simple

fetchNames :: Connection -> IO [Maybe String]
fetchNames conn = do
  _ <- execute_ conn "CREATE TABLE names (value TEXT)"
  _ <- executeMany conn "INSERT INTO names VALUES (?)"
    [Only (Just "Alice"), Only (Nothing :: Maybe String)]
  fmap fromOnly <$> query_ conn "SELECT value FROM names ORDER BY value IS NULL, value"
```

The execution helpers return the number of affected rows (`Int`). Callers can
use this count to check data changes.

An `Integer` or a `Natural` parameter binds as a HUGEINT, a UHUGEINT, or a
BIGNUM value, or raises an error, depending on its size and target type:
note 1 of [Types](#types) gives the rule.

In DuckDB 2.0.0 the casts from BIGNUM to the integer types have defects: some
fail, and some store a wrong value with no error ([issue
41](https://github.com/Tritlo/duckdb-haskell/issues/41)). A `SizedArray` or a
list of `Integer` or `Natural` values takes the element type of its target when
that type is an integer type or DECIMAL, so DuckDB checks each element. A
`BigNum` value, and a `SizedArray` or a list of `BigNum` values, still have
these defects for an integer target. For each other target, and for a parameter
with no known type, a `SizedArray`, a list, or a `Map` of `Integer` or `Natural`
values takes the type that its elements give, as a scalar `Integer` and a SQL
list literal of the HUGEINT and BIGNUM values do: `[1, 2^64]` binds as
`HUGEINT[]`, and `[1, 2^200]` as `BIGNUM[]`. An empty list and a list of only
NULL elements get the type `"NULL"[]`, as `[]` and `[NULL]` in SQL, because an
`Integer` has no fixed SQL type. The rule follows the column type name BIGNUM,
so an all-NULL list of `BigNum` values with no target also gets `"NULL"[]`.

An `IntervalValue` binds as an INTERVAL, and DuckDB keeps its months, days, and
microseconds. DuckDB compares intervals with 30 days for each month, so
`INTERVAL 30 DAY = INTERVAL 1 MONTH`. A `TimeWithZone` binds as a TIMETZ with
its time and its offset in seconds east of UTC (`timeWithZoneOffsetSeconds`).
`timeWithZone` makes a value from a `Data.Time` `TimeZone`, and
`timeWithZoneZone` gives the zone of a value, with the seconds of its offset
dropped. DuckDB stores an offset from -15:59:59 to +15:59:59, that is from
-57599 to 57599 seconds, and a value reads back equal to the bound value. A time
with digits below one microsecond loses these digits, as in SQL text. A
`DecimalValue` binds as a DECIMAL with its own width and scale. DuckDB casts it
to the type of its target: a smaller scale rounds half away from zero, and a
value that the target cannot hold raises an error. A `SizedArray`, a list, or a
typed row (a row type with `TypedRow`) of `DecimalValue` does not compile: note
14 of [Types](#types).

`query_` and `execute_` parse all input statements before execution. A syntax
error prevents every statement in that input from running. An execution error
can leave effects from earlier statements. Use `withTransaction` when those
effects must roll back together.

## Named parameters

duckdb-simple supports both positional (`?`) and named parameters. Bind a
named parameter with `(:=)` from `Database.DuckDB.Simple`.

```haskell
import Database.DuckDB.Simple

insertNamed :: Connection -> IO Int
insertNamed conn =
  executeNamed conn
    "INSERT INTO events VALUES ($kind, $payload)"
    ["$kind" := ("metric" :: String), "$payload" := ("ok" :: String)]
```

DuckDB does not allow positional and named placeholders in the same SQL
statement. The library preserves DuckDB's error message in that case.
DuckDB does not support savepoints. This library does not provide `withSavepoint`.

Parameter names use DuckDB's ASCII case rules. For example, `$amount` and
`$AMOUNT` refer to the same parameter. Supplying both raises a `FormatError`.
Non-ASCII characters retain their case. Session variables
can supply omitted named parameters. DuckDB rejects an omitted parameter
that has no variable value.

`duckdb-simple` raises a `FormatError` before it executes the query in these
cases:

- The number of positional parameters is not the number of placeholders.
- You bind positional parameters to a statement with named placeholders.
- You supply more named parameters than the statement declares.
- You supply a name that the statement does not declare.
- You supply one parameter two times, for example as `$amount` and `$AMOUNT`.
- You bind named parameters to a statement with only positional placeholders.

`bind` and `bindNamed` reset the statement cursor before validation. If
validation or value conversion fails, the previous set of bindings remains.
A successful `bindNamed` replaces the explicit bindings. Current session
variables can supply omitted parameters when the statement executes.

### Decoding rows

The `fromRow` method of a `FromRow` instance is a `RowParser`. Write it with
`Applicative` or `Monad` functions, or derive the instance with `Generic` for a
product type:

```haskell
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import Database.DuckDB.Simple
import GHC.Generics (Generic)

data Person = Person
  { personId :: Int
  , personName :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow)

fetchPeople :: Connection -> IO [Person]
fetchPeople conn = query_ conn "SELECT id, name FROM person ORDER BY id"
```

Use `field`, `fieldWith`, and `numFieldsRemaining` for more control in a custom
instance.

A `FromField` instance reads its Haskell type's SQL type, for example BIGINT
for `Int64`. It also reads other SQL types when it can return the value of
DuckDB's cast. The "Reads" column and notes 3 to 5 of [Types](#types) list
these types.

Decoders return `Conversion a`. This follows the `postgresql-simple` style:
a conversion can run IO and report conversion errors. This library's
`runConversion` takes no connection argument. The query and callback APIs run
conversions for you. The decoder types are:

```haskell
type FieldParser a = Field -> Conversion a

fromField :: FromField a => Field -> Conversion a
parseRow :: RowParser a -> [Field] -> Conversion a
runConversion :: Conversion a -> IO (Ok a)
```

For example, `runConversion (fromField field :: Conversion Int64)` returns
`Ok value` or `Errors errors` in IO. Import the runner from
`Database.DuckDB.Simple.Conversion` and the result constructors from
`Database.DuckDB.Simple.Ok`. Use `liftConversion` to run IO inside a custom
decoder. IO exceptions, including cancellation, propagate to the caller.
Sequential and applicative conversions stop at the first conversion failure.
`Alternative` tries the second branch if the first fails. If both fail, it
combines their diagnostics.

A decoder for a registered function's arguments runs inside its callback.
Do not run another query on that callback's connection from the decoder. Use
another connection. See [User-defined functions](#user-defined-functions)
for the connection rules.
`fieldType` reads its scoped logical type without taking the connection lock.

`fieldType :: Field -> Conversion LogicalTypeRep` reads the SQL type of a
column, or of an element, key, or value in a LIST, ARRAY, or MAP. It preserves
aliases such as JSON. A query result or callback owns the native metadata.
The library converts a column's type to `LogicalTypeRep` when a decoder
requests `fieldType` and caches a successful result within that scope. This
metadata action does no tree traversal or ENUM dictionary copy until requested.
Structured value readers can also build type trees to decode their values.
If cancellation interrupts a metadata read while the scope remains open,
another call can retry it. The cache does not store exceptions.

Native metadata is available while a query parser runs. A cursor retains its
metadata until exhaustion, reset, or statement closure. Callback metadata is
available during that callback invocation. An escaped native `fieldType`
action raises `SQLError` with the message
`duckdb-simple: field type scope is closed`, including when its result was
cached before closure. Read a pure
`LogicalTypeRep` snapshot inside the scope if you need the type later.
Snapshots remain usable after the statement, connection, instance, and
environment close.

Metadata scopes release their native types when they close. They use no
metadata finalizers. Existing statement and connection finalizers are
unchanged. `Field` has no `Eq` or `Show` instance. A value in a VARIANT has the
type VARIANT, and so does a `Field` made from a `FieldValue` alone. In those
cases, the stored value determines the type. `logicalTypeSQL` gives the SQL
type text, for example DECIMAL(18,3). A custom decoder can check the type:

```haskell
newtype Cents = Cents DecimalValue

instance FromField Cents where
  fromField field = do
    logical <- fieldType field
    case logical of
      LogicalTypeDecimal _ 2 -> Cents <$> fromField field
      _ -> returnError Incompatible field "expected a DECIMAL with the scale 2"
```

A field of a `ViaDuckDB` record reads with its type when its `DuckValue`
instance sets `duckFromTypedField = typedFromField`. The library's instances
do this. Generic decoders have these types:

```haskell
duckFromField :: DuckValue a => FieldValue -> Conversion a
duckFromTypedField :: DuckValue a => LogicalTypeRep -> FieldValue -> Conversion a
typedFromField :: FromField a => LogicalTypeRep -> FieldValue -> Conversion a
genericFromFieldValue :: (Generic a, GFromField (Rep a)) => FieldValue -> Conversion a
```

Encoding and logical type declarations remain pure.

## Types

Each row gives a Haskell type and these facts:

- Binds as: the SQL type of a parameter of this type.
- Declared: the SQL type that a typed registration declares, if it differs
  from "Binds as". This applies to columns and parameters of
  `createTableFunction`, and arguments and results of `createAggregate` and
  `createFunction`. "none" means that the type has no declaration.
  `registerCopyFromFunction` declares no type. The COPY FROM table supplies the
  column types. The row type must have the same number of columns. DuckDB casts
  each row value to its column's type.
- Reads: the SQL types that the `FromField` instance reads.
- Function: the use of the type in `createFunction`, as an argument, a result,
  or both. See the paragraph after the notes.

| Haskell type | Binds as | Declared | Reads | Function |
| --- | --- | --- | --- | --- |
| `Bool` | BOOLEAN | | BOOLEAN, TINYINT to BIGINT (2) | argument, result |
| `Int8`, `Int16`, `Int32`, `Int64`, `Int` | TINYINT, SMALLINT, INTEGER, BIGINT, BIGINT | | TINYINT to BIGINT, HUGEINT, UHUGEINT (3) | argument, result |
| `Word8`, `Word16`, `Word32`, `Word64`, `Word` | UTINYINT, USMALLINT, UINTEGER, UBIGINT, UBIGINT | | each integer type (3) | argument, result |
| `Integer` | HUGEINT (1) | | each integer type, BIGNUM (3) | argument, result |
| `Natural` | HUGEINT (1) | UHUGEINT (10) | each integer type, BIGNUM (3) | argument, result |
| `BigNum` | BIGNUM | | BIGNUM | argument, result |
| `Double` | DOUBLE | | DOUBLE, FLOAT, TINYINT to BIGINT, DECIMAL (4) | argument, result |
| `Float` | FLOAT | | as `Double` (4) | argument, result |
| `DecimalValue` | DECIMAL of the value | none (14) | DECIMAL | none |
| `Text`, `String` | VARCHAR | | VARCHAR, TINYINT to BIGINT, BOOLEAN, ENUM (5) | argument, result |
| `ByteString` | BLOB | | BLOB, BIT, GEOMETRY (5) | argument, result |
| `BitString` | BIT | | BIT | argument, result |
| `UUID` | UUID | | UUID, VARCHAR (5) | argument, result |
| `Day`, `Unbounded Day` | DATE (7) | | DATE, each timestamp type (6) | argument, result |
| `TimeOfDay` | TIME (7) | | TIME, TIME_NS, each timestamp type (6) | argument, result |
| `LocalTime`, `Unbounded LocalTime` | TIMESTAMP (7) | | each timestamp type (6), DATE (5) | argument, result |
| `UTCTime`, `Unbounded UTCTime` | TIMESTAMPTZ (7) | | TIMESTAMPTZ, TIMESTAMPTZ_NS (5) | argument, result |
| `TimeWithZone` | TIMETZ | | TIMETZ | argument, result |
| `IntervalValue` | INTERVAL | | INTERVAL | argument, result |
| `[a]` | LIST | | LIST, ARRAY | argument, result |
| `SizedArray n a` | ARRAY of size `n` (8) | | ARRAY, LIST (8) | argument, result |
| `Map k v` | MAP | | MAP | argument, result |
| `StructValue FieldValue` | STRUCT (9) | none | STRUCT | none |
| `TupleValue FieldValue` | TUPLE (9) | none | TUPLE | none |
| `UnionValue FieldValue` | UNION (9) | none | UNION | none |
| `ViaDuckDB a` | STRUCT or UNION (10) | | STRUCT or UNION | argument, result |
| `Maybe a` | as `a`, or NULL | as `a` | NULL, or as `a` | as `a` |
| `G.Geometry` | GEOMETRY without a CRS | | GEOMETRY | argument, result |
| `RawGeometry` | GEOMETRY with its CRS | none (14) | GEOMETRY | none |
| `Variant` | VARIANT | | VARIANT, and the values that a VARIANT can hold (13) | argument, result |
| `FieldValue` | the type of its constructor (11) | none | each type | none |
| `Null` | `"NULL"` (12) | | NULL | argument, result (12) |

Notes:

1. A value from -2^127 to 2^127 - 1 binds as a HUGEINT. DuckDB then checks that
   the value fits its target: for example, a negative value for an unsigned
   column raises an error. A value outside that range binds as a UHUGEINT for a
   UHUGEINT parameter that can hold it. It binds as a BIGNUM for a parameter
   that has no integer or DECIMAL type. For each other integer or DECIMAL
   parameter, it raises an `SQLError` of the type `ErrorTypeConversion`. Bind a
   `BigNum` to bind a BIGNUM value in each case. [Querying Data](#querying-data)
   gives the defects of the casts from BIGNUM in DuckDB 2.0.0, and the rule for
   a list, a `SizedArray`, or a `Map` of `Integer` values.
2. The value 0 reads as `False`, and each other value reads as `True`.
3. "Each integer type" is TINYINT to UHUGEINT. A value that the Haskell type
   cannot hold raises a library conversion error: an `SQLError` with no type.
   This includes a negative value for an unsigned type or for `Natural`. An ENUM
   value does not read as an integer type. Write `enum_code(c)::BIGINT` in SQL
   for its index, or `c::INTEGER` for the value of its label.
4. A read returns the value of DuckDB's `c::DOUBLE` or `c::FLOAT` cast. This
   includes a DECIMAL, and a BIGINT read as `Float`. As seen in CI, the DuckDB
   2.0.0 build for macOS on Apple Silicon rounds some BIGINT values above 2^53 two
   times in its cast to FLOAT, and the library gives the nearest value there. A
   finite value that `Float` cannot hold raises a conversion error of the
   library, an `SQLError` with no type.
5. A read of another SQL type returns the value of DuckDB's cast to the Haskell
   type's SQL type, or an error if the value does not fit. For
   example, `Int8` reads a BIGINT that fits in a TINYINT, and `Double` reads a
   DECIMAL with the value of `c::DOUBLE`. Thus `Text` reads a BOOLEAN as `true`
   or `false`, and `Text` and `String` read an ENUM as its label, the text of
   `c::VARCHAR`; a `FieldEnum` holds the index and the label. `ByteString` reads
   a BIT or a GEOMETRY as the bytes of `c::BLOB`. `UUID` reads a VARCHAR in the
   form `a0eebc99-9c0b-4ef8-bb6d-6bb9bd380a11`, and another form that the cast
   reads, for example with braces, raises an error. A read that needs the
   TimeZone setting of the session, or another text than the cast gives, raises
   the error of an incompatible column. For each such read, write the SQL in the
   parentheses in place of the column `c` (note 3 gives the SQL for an ENUM read
   as an integer type):
   - `Text` from FLOAT or DOUBLE (`c::VARCHAR`).
   - `ByteString` from VARCHAR (`encode(c)`).
   - `LocalTime` from TIMESTAMPTZ or TIMESTAMPTZ_NS (`c AT TIME ZONE 'UTC'`).
   - `UTCTime` from DATE, TIMESTAMP_S, TIMESTAMP_MS, or TIMESTAMP
     (`c::TIMESTAMP AT TIME ZONE 'UTC'`), and from TIMESTAMP_NS
     (`c AT TIME ZONE 'UTC'`).

   The `Unbounded` forms of `LocalTime` and `UTCTime` follow the same rule. The
   Haddock of each `FromField` instance gives the SQL types that it reads.
6. "Each timestamp type" is TIMESTAMP_S, TIMESTAMP_MS, TIMESTAMP, and
   TIMESTAMP_NS. TIMESTAMP alone is the type with microseconds. `Day` reads the
   date of a timestamp, and `TimeOfDay` reads its time. A nanosecond type keeps
   its nanoseconds. An infinite value raises a conversion error of the library,
   an `SQLError` with no type, for `Day`, `TimeOfDay`, `LocalTime`, and
   `UTCTime`. The `Unbounded` forms read it. Use `Maybe Date` to distinguish SQL
   NULL from infinity. `Date`, `LocalTimestamp`, and `UTCTimestamp` from
   `Database.DuckDB.Simple.Time` are the `Unbounded` forms.
7. A value with digits below a microsecond keeps them for a nanosecond
   parameter, and as a scalar value that fits the range of the nanosecond type
   for a `VARIANT` parameter: [Infinite dates and
   timestamps](#infinite-dates-and-timestamps) gives the rule. The range of
   `Day` is 5877642-06-25 (BC), the year -5877641 of `Data.Time`, to
   5881580-07-10. The range of `LocalTime` and `UTCTime` is 290309-12-22 (BC)
   00:00:00 to 294247-01-10 04:00:54.775806. A value outside its range raises an
   `SQLError` of the type `ErrorTypeConversion`. A target of the type
   TIMESTAMP_NS or TIMESTAMPTZ_NS has a smaller range, about the years 1677 to
   2262, and a value outside it gets the same type in each position. A
   `TimeOfDay` with 60 seconds binds as the next second, as `make_time` of
   DuckDB gives it: 23:59:60 binds as 24:00:00. A later time, for example
   23:59:60.5, and a field outside its own range, for example 61 minutes, raise
   an `SQLError` of the type `ErrorTypeConversion`. [Infinite dates and
   timestamps](#infinite-dates-and-timestamps) gives the full rule.
8. A DuckDB ARRAY type has a size, for example `BIGINT[3]`. A `SizedArray n a`
   is the Haskell type of that ARRAY: an `Array Int a` of exactly `n` values,
   with the bounds `(0, n - 1)`. `sizedArray` makes one from an `Array` with
   any bounds and gives `Nothing` for an `Array` of another count. The value
   has the elements in their order, and `sizedArrayToArray` gives its `Array`.
   `sizedArrayFromList` makes one from a list, and `toList` gives the values.
   A `SizedArray`
   binds as an ARRAY of size `n`, and a typed registration declares that type
   in each position, also as a field of a record through `ViaDuckDB`. It reads
   an ARRAY and a LIST of `n` elements, as the cast of DuckDB to the ARRAY
   does. A query that reads a value of another length raises an `SQLError`.
   Its message names the column type, the `SizedArray` type, and the length.
   In a typed registration, DuckDB rejects an ARRAY of another size when it
   binds the call. It casts a LIST of the same length to the ARRAY with no
   error, by its own implicit cast, and a LIST of another length raises an
   `SQLError` when the statement runs. Use one type for each need:
   - An ARRAY of a fixed size: a `SizedArray n a`.
   - An ARRAY of a size that varies: a list. A list reads an ARRAY of each
     size, and DuckDB casts a bound list to an ARRAY column of the same
     length.
   - An ARRAY value that the program builds at run time: a `FieldArray`.

   A DuckDB ARRAY has 1 to 100000 elements. A `SizedArray` or a `FieldArray`
   parameter with no element or with more than 100000 elements raises an
   `SQLError` of the type `ErrorInputInvalid`, and a typed registration raises
   the same error for a `SizedArray` of such a size. Bind an empty list for an
   empty LIST.
9. A STRUCT, TUPLE, or UNION parameter binds with the types of its own fields
   or members. These types keep an alias, so a value that a query reads from a
   `STRUCT(k JSON)` column binds as `STRUCT(k JSON)`. These values have no
   static field types, so a typed registration with such a type raises an
   error. Use a record with `ViaDuckDB`.
10. A record binds as a STRUCT, and a sum type binds as a UNION. A typed
    registration declares the STRUCT or UNION type of a record or a sum type
    that derives `DuckDBColumnType` and `ToDuckValue` via `ViaDuckDB`, also in a
    list or a MAP. Each field type comes from `DuckValue`. An `Integer` declares
    HUGEINT and a `Natural` declares UHUGEINT, as a field, a column of a typed
    row, an argument, and a result. A value that this type cannot hold raises an
    `SQLError` of the type `ErrorTypeConversion`: use `BigNum`, which declares
    BIGNUM, for a larger value. DuckDB computes with these types: HUGEINT
    arithmetic stays HUGEINT and raises an error on overflow. DuckDB casts a
    non-negative integer literal to a UHUGEINT argument, but no signed column or
    typed value without an explicit cast. `sum()` over a UHUGEINT column gives a
    DOUBLE value, and so do UHUGEINT + INTEGER and HUGEINT + UHUGEINT for
    columns and typed values. A `SizedArray` field declares the ARRAY of its
    size (see note 8). A `duckLogicalType`
    that gives a `LogicalTypeAlias`, for example
    `LogicalTypeAlias "JSON" (LogicalTypeScalar LogicalTypeIdVarchar)`, declares
    that alias type in a typed registration and in a parameter, and a value of
    the field reads as its base type. See
    [Generic Encoding with ViaDuckDB](#generic-encoding-with-viaduckdb).
11. Each constructor binds as its SQL type, with these exceptions:
    - A `FieldEnum` binds its label as a VARCHAR.
    - A `FieldTimestamp` binds as TIMESTAMP, also when it came from another
      timestamp type. [Infinite dates and
      timestamps](#infinite-dates-and-timestamps) gives the rule for digits
      below a microsecond.
    - An empty `FieldArray` raises an `SQLError` of the type
      `ErrorInputInvalid`, because a DuckDB ARRAY has at least one element.
    - An empty `FieldList` or `FieldMap`, and a `FieldList`, a `FieldArray`, or
      a `FieldMap` with only `FieldNull` elements, bind as the SQL list literal
      does: they take the element type of a parameter with a known type. For a
      parameter with no known type, `[]` and `[NULL, NULL]` have the type
      `"NULL"[]`, and `MAP {}` has the type `MAP("NULL", "NULL")`.

    See [FieldValue parameters](#fieldvalue-parameters).
12. `typeof` gives `"NULL"`, with the quotes, for a `Null` parameter and for a
    `Null` declaration. A `Null` argument takes only a NULL value. A call with
    another value is a Binder Error. A record cannot have a `Null` field.
13. A `Variant` reads a value that a VARIANT can hold: a VARIANT column, or a
    column of another type with the same shape. That is a value of each scalar
    type other than ENUM, for example an INTEGER or a GEOMETRY with no CRS, a
    LIST of such values, and a STRUCT in the form of a VARIANT object: each
    field of the type VARIANT, the keys in ascending order. At each depth, it
    refuses an ENUM, an ARRAY, a MAP, a TUPLE, a UNION, another STRUCT, and a
    GEOMETRY with a CRS, with the error of an incompatible column, because the
    cast of DuckDB to VARIANT changes these values. Write `c::VARIANT` in SQL to
    read such a column. It also refuses a type that is or contains JSON, at
    each depth, because DuckDB casts JSON to VARIANT by parsing the text: the
    error says `DuckDB casts JSON to VARIANT by parsing the text; write
    c::VARIANT`, and `c::VARIANT` gives the parsed value.
14. `DecimalValue` and `RawGeometry` have no `DuckDBColumnType` and no
    `ToDuckValue` instance, so a `SizedArray`, a list, or a typed row of either
    does not compile: GHC reports
    `No instance for 'DuckDBColumnType DecimalValue'` (or `RawGeometry`), or the
    same for `ToDuckValue` in a typed row. One column type cannot give the width
    and scale of each `DecimalValue`. A declared GEOMETRY type has no CRS, so it
    would remove the CRS of each `RawGeometry`. A composite parameter holds a
    `RawGeometry` as a `FieldGeometry`.

A value of a VARIANT column reads as its stored value, so the row of the stored
type applies. A value in a LIST, ARRAY, MAP, STRUCT, or UNION reads with the
same rules, and so does a field of a `ViaDuckDB` record. A SQL type that is not
in "Reads" raises the error of an incompatible column. See [Errors](#errors).

A function registered with `createFunction` or `createAggregate` declares the "Declared" type, or
the "Binds as" type if "Declared" is empty, for each argument and its result.
An argument type needs `FromField` and a declaration. A result
type needs `ToDuckValue` and a declaration. Some result types write each value
directly into the data of the result vector: `Bool` (BOOLEAN), an integer of 8
to 64 bits (TINYINT to BIGINT, UTINYINT to UBIGINT), `Float` (FLOAT), `Double`
(DOUBLE), `Text` and `String` (VARCHAR), `Maybe` of one of them, and a newtype
that derives `FunctionResult` from one of them
(`deriving newtype (FunctionResult)`). Each other result type, for example a
record or a list, requires a native value for each row. DuckDB uses its default
cast to convert that value to the declared type. For example, it rounds a
DECIMAL to the declared scale and raises an error if the value does not fit.

An aggregate result encoder receives an instance factory. This factory resolves
built-in types. DuckDB resolves the result declaration on the registration
host, so a custom encoder can return the underlying value of a catalog type.
See [User-Defined Functions](#user-defined-functions).

## Errors

A program can catch these exceptions:

- `SQLError`: an error of DuckDB, an input check of the library, a failed
  internal check of the library, or a result value that does not convert.
- `FormatError`: parameters that do not agree with the placeholders of the
  statement.
- The exception of a cancellation, for example from `timeout`.

A failed check of the library state, or of a vector, type, or value from
DuckDB, raises an `SQLError` of the type `ErrorRuntimeInternal`. Such an error
indicates a defect in the library or DuckDB, not an input error. Report it.
Inside a callback, the statement that runs the callback fails with
the type `ErrorInputInvalid` and the text
`Invalid Input Error: duckdb-simple: ...`, because a callback does not pass the
type `ErrorRuntimeInternal`; a cast gives `ErrorTypeMismatch` (see the errors of
a callback below). The following errors raise an `SQLError` with no type:
use of a closed environment, instance, connection, or statement;
a DuckDB library of another version; or a callback state read with the wrong
type. These errors are not internal checks.

`SQLError` has four fields:

- `sqlErrorMessage` is the text of the error. For an error of DuckDB, it is
  the text of DuckDB, for example `Binder Error: ...`.
- `sqlErrorType` is the error category, as `Just` an `ErrorType`. For a DuckDB
  error, it is DuckDB's error code. The library assigns a type to an input
  check, a lock wait, or a failed internal check.
  `ErrorType` has one constructor for each error code of the V2 C API, for
  example `ErrorQueryBinder` and `ErrorTypeConversion`.
- `sqlErrorQuery` is the SQL text of the statement for an error during its
  preparation or execution, and for a result value that does not convert. It
  is `Nothing` for an error outside a statement, for example when a connection
  opens. An error of a parameter value can also have `Nothing`.
- `sqlErrorBody` is DuckDB's error text without the type prefix. It is
  `Nothing` for a library error.

An input check of the library raises an `SQLError`. A check for a value outside
the range of its SQL type gives the type `ErrorTypeConversion`, the type of the
Conversion Error that DuckDB gives the same value in SQL: an `Integer` that its
parameter or its declared type cannot hold, a date or a timestamp outside the
range of DATE or TIMESTAMP of DuckDB (note 7), a nanosecond timestamp outside
its storage, a time of day that is not a TIME value, a TIMETZ offset outside
-57599 to 57599 seconds, an `Integer` or `Natural` field of a record outside HUGEINT or
UHUGEINT, a `DecimalValue` that its width cannot hold, and an ENUM label that is
not in the dictionary of a declared ENUM type. Each other input check gives the
type `ErrorInputInvalid`. This includes a value of the wrong shape or type, also
where DuckDB gives another type for the same input, for example a UNION tag that
is not a member, a leaf built from another type, a STRUCT field that does not
match its type, or a blank GEOMETRY CRS label. Examples are a VARIANT object with two equal
keys, a registration with an argument type that the library cannot read, and a
DECIMAL width above 38.

A statement in a callback that waits for a connection lock longer than
`connectionCallbackLockWait` raises an `SQLError` of the type
`ErrorResourceInUse` ([User-Defined Functions](#user-defined-functions) gives
the rule). A statement on a connection that streams a result, for example in the
step of a fold of `Database.DuckDB.Simple.Streaming`, also gives
`ErrorResourceInUse`, and so does a close of an environment while an instance
that `createInstance` did not create is alive.

A result value that its Haskell type cannot read raises an `SQLError` with no
type. The error includes the query. Its message describes the first field that
fails:

- The error of an incompatible column, for a SQL type that the instance does
  not read: `duckdb-simple: column NAME has type TYPE but expected HASKELLTYPE`.
  The message can end with `: ` and a detail, for example `: expected NULL`
  for `Null`. TYPE is the type of the column in the text of DuckDB, as
  `typeof` gives it, for example DECIMAL(18,2), TIMESTAMP_NS, TIMESTAMP WITH
  TIME ZONE, ENUM('a', 'b'), INTEGER[], or JSON. `logicalTypeSQL` gives that
  text. For a VARIANT column, TYPE is the type of the stored value in the same
  text, for example DOUBLE or TIMESTAMP WITH TIME ZONE. A value that does not
  hold its full type, for example a LIST or a STRUCT in a VARIANT, gives the
  name of its kind: LIST or STRUCT. The library reads the diagnostic type
  only when the conversion's failure is selected.
  HASKELLTYPE is the type of the instance that reads, for example `Text` for
  `String`.
- `duckdb-simple: column NAME is NULL but expected HASKELLTYPE` for NULL in a
  type without `Maybe`.
- `duckdb-simple: field FIELD: ` and the error of the value for a field of a
  `ViaDuckDB` record, for example
  `duckdb-simple: field pairValue: a value has type DOUBLE but expected Int64`.
- A conversion error of the library, for a value that the instance cannot
  convert: the message of the instance, for example `integer value out of
  bounds`.

The library creates a `ResultError` for each failure and puts its text in the
`SQLError`. It does not raise the `ResultError`. A custom `FromField` instance
reports a failure with `returnError` and a constructor of `ResultError`.
`returnError` defers its metadata read until the final conversion fails. If an
`Alternative` branch succeeds, the library discards the other branch's error
without reading its diagnostic metadata. A completed `ResultError` contains
pure data. Its rendering and comparison perform no native IO.
`query` and `query_` raise the error and give no rows. A fold raises it at the
row that fails. A row type with more or fewer fields than the result has
columns raises an `SQLError` with the text `column index N out of bounds`.

An exception in a registered Haskell function, for example one registered with
`createFunction`, fails the statement that runs the function. That statement
raises an `SQLError`. An `SQLError` with a type keeps its type, and its text
gets one prefix. For example, a function that runs a failing query on another
connection fails the outer statement with `ErrorTypeConversion` and the text
`Conversion Error: ...` of the inner query, and a check of the library in a
result, for example a time of day after 24:00:00, keeps its type
`ErrorTypeConversion`. A failed cast is the exception: it gives
`ErrorTypeMismatch` (see below). Each other exception, an `SQLError` with no
type, and an `SQLError` of one of the 16 types below give `ErrorInputInvalid`
and the text `Invalid Input Error: ` followed by the message of the exception:
the message of an `SQLError`, the text of a `userError`, or the
`displayException` text of another exception. An argument that does not convert
gives an `SQLError` of the type `ErrorInputInvalid` with a text that names the
argument, for example
`duckdb-simple: argument arg0 is NULL, but Int64 cannot hold NULL` for a scalar
function. For an aggregate or a table function, the text names the function
after the prefix, for example `duckdb-simple: tf_int: argument arg0 ...`. DuckDB
ignores an exception of a log sink.

These 16 types do not pass through a callback:

- `ErrorQueryBinder`, `ErrorDatabaseCatalog`, `ErrorResourceConnection`,
  `ErrorQueryParameterNotAllowed`, `ErrorQueryParser`,
  `ErrorConfigurationPermission`, and `ErrorResourceInUse`. DuckDB expects these
  types before the execution of a statement. Under the DuckDB setting
  `SYNTACTIC_ERRORS_DO_NOT_INVALIDATE`, it does not invalidate an explicit
  transaction after them, and a callback raises during the execution.
- `ErrorRuntimeInternal` and `ErrorRuntimeFatal`. DuckDB can invalidate the
  database after them.
- `ErrorApi`, `ErrorIOFileNotFound`, `ErrorIOReadFailure`, `ErrorIOEof`,
  `ErrorInputParameterInvalid`, `ErrorInputMetadataDeferred`, and
  `ErrorIONetwork`. DuckDB 2.0.0 cannot raise them again with their own type.

When an error of a callback fails a statement in an explicit transaction, DuckDB
aborts that transaction, also under the setting
`SYNTACTIC_ERRORS_DO_NOT_INVALIDATE`: [User-Defined
Functions](#user-defined-functions) gives what this means for a `COMMIT`.

An exception in the callback of `registerCast` in
`Database.DuckDB.Simple.V2.Registration` fails the cast with an `SQLError` of
the type `ErrorTypeMismatch`, also an `SQLError` with another type. Its message
is `Mismatch Type Error: ` and the text of the exception. When DuckDB casts a
constant through the cast while it binds the query, for example the INTEGER
literal of `range(10)` with a cast from INTEGER to BIGINT, the error has the
type `ErrorInputInvalid` and the text `Failed to cast value: ` before the text
of the exception, and the database stays usable. The type `ErrorTypeMismatch`
is the same for each pair of types, because of a property of DuckDB 2.0.0.
DuckDB plans casts between some pairs of types as casts that cannot fail,
including registered casts. Examples are casts to VARCHAR, including
from an ENUM, a LIST, or a STRUCT, and a widening cast such as INTEGER to
BIGINT or FLOAT to DOUBLE.
For such a cast, DuckDB changes an error of the type `ErrorInputInvalid`,
`ErrorInputOutOfRange`, or `ErrorTypeConversion` to an INTERNAL error. It does
the same for an error type that it does not map, for example `ErrorApi`. An
INTERNAL error during the execution of a query invalidates the database. Then
each statement on each connection of the database fails until a new instance
opens the database. An error of the type `ErrorTypeMismatch` keeps the database
usable, and it aborts an explicit transaction. In `TRY_CAST`, DuckDB discards
the error and keeps the rows that the callback wrote, so the callback must write
each row, for example NULL for a failed row. `TRY(CAST(...))` raises the error,
because `TRY` catches only the types `ErrorInputInvalid`,
`ErrorInputOutOfRange`, and `ErrorTypeConversion`. DuckDB still plans such a
cast as one that cannot fail, so it can run the cast for a row that another
condition of the query excludes, for example in
`WHERE c < 2500 AND CAST(c AS VARCHAR) <> 'x'`. In DuckDB 2.0.0, only a guard in
a CASE expression, for example
`CASE WHEN d <> 0 THEN CAST(c AS VARCHAR) <> 'x' ELSE false END`, kept the cast
from the excluded rows. A cast that a program registers with the native API of
`duckdb-ffi` also gets the INTERNAL error for such a pair and such an error
type. A V2 cast can report the code `DUCKDB_V2_ERROR_TYPE_MISMATCH` in their
place.

An asynchronous exception, for example from `timeout` or Ctrl-C, cancels a
running query. The library interrupts DuckDB, waits until the native call
returns, and raises the same exception again. Catch that exception, not
`SQLError`. See [Resource Management](#resource-management) for `-threaded`.

`FormatError` has the fields `formatErrorMessage`, `formatErrorQuery`, and
`formatErrorParams` (the parameter values as text). The library raises it
before it executes the statement, in the cases that
[Named Parameters](#named-parameters) lists. A `FormatError` is not an
`SQLError`, so catch it separately.

This function gives `Nothing` when the table `items` has no column `value`:

```haskell
import Control.Exception (throwIO, try)
import Database.DuckDB.Simple

countValues :: Connection -> IO (Maybe Int)
countValues conn = do
  result <- try (query_ conn "SELECT count(value) FROM items")
  case result of
    Right [Only n] -> pure (Just n)
    Right _ -> pure Nothing
    Left err
      | sqlErrorType err == Just ErrorQueryBinder -> pure Nothing
      | otherwise -> throwIO err
```

## Generic encoding with ViaDuckDB

The `Database.DuckDB.Simple.Generic` module provides automatic encoding and
decoding of Haskell algebraic data types as DuckDB STRUCTs and UNIONs via
GHC generics.

### Product types as STRUCTs

The generic encoder converts product types (records) to DuckDB STRUCT values:

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

import Data.Int (Int64)
import Data.Text (Text)
import Database.DuckDB.Simple
import Database.DuckDB.Simple.Generic (ViaDuckDB (..))
import GHC.Generics (Generic)

data User = User
  { userId :: Int64
  , userName :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving (DuckDBColumnType, ToField, FromField) via (ViaDuckDB User)

-- Round-trip through the database
storeAndFetchUser :: Connection -> User -> IO [User]
storeAndFetchUser conn user = do
  _ <- execute_ conn "CREATE TABLE users (data STRUCT(userId BIGINT, userName TEXT))"
  _ <- execute conn "INSERT INTO users VALUES (?)" (Only user)
  fmap fromOnly <$> query_ conn "SELECT data FROM users"
```

### Sum types as UNIONs

The generic encoder converts sum types to DuckDB UNION values. Each constructor
becomes a union member:

```haskell
data Shape
  = Circle Double
  | Rectangle Double Double
  | Point
  deriving stock (Eq, Show, Generic)
  deriving (DuckDBColumnType, ToField, FromField) via (ViaDuckDB Shape)

-- Store and retrieve shape data
storeShape :: Connection -> Shape -> IO [Shape]
storeShape conn shape = do
  _ <- execute_ conn
    "CREATE TABLE shapes (s UNION(Circle STRUCT(field1 DOUBLE), \
    \Rectangle STRUCT(field1 DOUBLE, field2 DOUBLE), Point STRUCT))"
  _ <- execute conn "INSERT INTO shapes VALUES (?)" (Only shape)
  fmap fromOnly <$> query_ conn "SELECT s FROM shapes"
```

A constructor without fields (like `Point`) has an empty STRUCT as
its payload. DuckDB shows this payload as `{}`, and it is not NULL.
Non-record constructors use positional field names (`field1`, `field2`, etc.).

### Arrays and lists

The library supports fixed-length DuckDB arrays and variable-length lists:

```haskell
{-# LANGUAGE DataKinds #-}

import Control.Monad (forM_)
import Data.Array (listArray)

storeArray :: Connection -> IO [SizedArray 3 Int]
storeArray conn = do
  _ <- execute_ conn "CREATE TABLE arrays (vals INTEGER[3])"
  forM_ (sizedArray (listArray (0, 2) [1, 2, 3]) :: Maybe (SizedArray 3 Int)) $ \arr ->
    execute conn "INSERT INTO arrays VALUES (?)" (Only arr)
  fmap fromOnly <$> query_ conn "SELECT vals FROM arrays"

storeList :: Connection -> IO [[Int]]
storeList conn = do
  _ <- execute_ conn "CREATE TABLE lists (vals INTEGER[])"
  _ <- execute conn "INSERT INTO lists VALUES (?)" (Only ([1, 2, 3] :: [Int]))
  _ <- execute conn "INSERT INTO lists VALUES (?)" (Only ([] :: [Int]))
  fmap fromOnly <$> query_ conn "SELECT vals FROM lists"
```

A Haskell list binds as a LIST parameter and reads a LIST result. Put a list
parameter in `Only`, because a list as a row is a row of parameters. A
`String` stays one VARCHAR parameter. A `SizedArray` binds as an ARRAY of its
size (note 8 of [Types](#types)). DuckDB casts a LIST or an ARRAY to the type
of a LIST column. It casts a LIST to an ARRAY column only when the lengths are
equal.

A list or an array parameter with scalar elements takes its element type from
the column type name (`duckdbColumnTypeFor`) of the element. Some elements take
the element type of the target instead: `Integer`, `Natural`, and `BigNum`
elements for an integer or DECIMAL element type, and otherwise the type of their
values (see above), and `TimeOfDay`, `LocalTime`, and `UTCTime` elements for a
nanosecond element type (see [Infinite dates and
timestamps](#infinite-dates-and-timestamps)). An empty list binds as an empty
LIST of its element type, except an empty list of `Integer` or `Natural` values
with no integer or DECIMAL target (see above). An empty array raises an error:
note 8 of [Types](#types).

A list or an array of STRUCT values, UNION values, generic records, lists, or
arrays gets the type that DuckDB gives a SQL list literal of the same elements.
When the elements that are not NULL have one type, the list has that type.
When their types are different, DuckDB uses their common type: `{'a': 1}` and
`{'b': 2}` give `STRUCT(a INTEGER, b INTEGER)[]`, with NULL for a field that an
element does not have. A `BIGINT` field and a `DOUBLE` field give a `DOUBLE`
field, so `9007199254740993` becomes `9007199254740992.0`, as in SQL. Elements
with no common type, such as `{'a': 1}` and `{'a': 'x'}`, raise the `SQLError`
of DuckDB. The column type of an `INSERT` does not change the type of the
list: DuckDB casts the list to the column type after it gives the list its
type. A list or an array of such elements that are all NULL, and an empty
list, take the element type of the parameter when the parameter has a known
type, as for `INSERT INTO t VALUES ([])` and `([NULL, NULL])` in SQL. For a
parameter with no known type, an empty list or a list of NULL elements gets
the type of the SQL literal, `"NULL"[]`, and an empty `Map` of such values gets
the type NULL for its values, as `MAP {}` does. A generic record through
`ViaDuckDB` has a declared type, so for a parameter with no known type an
empty list or a list of NULL records gets the STRUCT or UNION type of the
record.

A `Map` from `Data.Map` binds as a MAP parameter and reads a MAP result. The
pairs go in ascending key order. The keys and the values follow the element
rules of a list, and an empty `Map` binds as an empty MAP of its key and value
types. DuckDB compares two MAP values pair by pair in their order, so
`WHERE m = ?` finds a stored MAP only when it has the same pairs in ascending
key order. DuckDB rejects a NULL key and two equal keys, for example two NaN
keys of a `Map Double`. When reading a MAP, a decoder rejects keys that become
equal after conversion. For example, the integer keys 1 and 2 both decode as
`True` in a `Map Bool v`.

### Infinite dates and timestamps

Use `Database.DuckDB.Simple.Time` when a column can contain temporal infinity.
Its `Date`, `LocalTimestamp`, and `UTCTimestamp` types wrap `Day`, `LocalTime`,
and `UTCTime` in `Unbounded`: `NegInfinity`, `Finite value`, or `PosInfinity`.
These types support parameters, results, and fields in generic composites.
`UTCTimestamp` binds as TIMESTAMPTZ.

```haskell
import Database.DuckDB.Simple
import Database.DuckDB.Simple.Time

infiniteDates :: Connection -> IO [Only Date]
infiniteDates conn = query conn "SELECT ?::DATE" (Only (PosInfinity :: Date))
```

The ordinary `Day`, `LocalTime`, and `UTCTime` instances reject infinity: note 6
of [Types](#types). Floating-point NaN and infinities remain valid `Float` and
`Double` values.

A `TimeOfDay`, a `LocalTime`, or a `UTCTime` parameter, and an `Unbounded`
form, keeps its digits below a microsecond for a nanosecond parameter:
`TIME_NS`, `TIMESTAMP_NS`, or `TIMESTAMPTZ_NS`. Use an explicit cast, such as
`SELECT ?::TIMESTAMP_NS`, or a column with that type. A `VARIANT` parameter
also gets the nanosecond value, when the value fits the range of the nanosecond
type, because a VARIANT holds `TIME_NS`, `TIMESTAMP_NS`, and `TIMESTAMPTZ_NS`
values. This applies only to a scalar value: a list, a `SizedArray`, or a `Map` of
time values for a `VARIANT` parameter binds microsecond elements, and so does
an element of a `VARIANT[]` list or a value of a MAP with `VARIANT` values,
because one list has one element type for all its elements. Each other
parameter, and a parameter with no known type, gets the microsecond value: the
digits below a microsecond are discarded, as for the SQL literal. Only a value
with such digits reads the parameter type. An explicit cast such as
`?::TIMESTAMP_NS` makes a nanosecond parameter. For a `TIMESTAMP` column, DuckDB
then rounds the value to the microsecond. A plain `?` truncates the digits.
Values inside collections, STRUCTs, MAPs, and UNIONs follow their target types.
The library refreshes prepared metadata when it resets or
rebinds a statement.
One `executeMany` call reads the parameter types once. A column type that
changes during the call does not change how its later rows bind.
Use separate parameters when one expression requires microseconds and another
requires nanoseconds.

A date or a timestamp outside the range of DuckDB raises an `SQLError` of the
type `ErrorTypeConversion` when it binds, also as an element of a list, a
`SizedArray`, or a `Map`, inside a STRUCT, a TUPLE, or a UNION, and in a VARIANT.
For a `Day`, that range is the range of DATE, from -5877641-06-25 to 5881580-07-10.
For a `LocalTime` or a `UTCTime` (in UTC), it is the range of TIMESTAMP,
TIMESTAMPTZ, TIMESTAMP_S, and TIMESTAMP_MS, from -290308-12-22 00:00:00 to
294247-01-10 04:00:54.775806. The dates are in the notation of `Data.Time`. A
TIMESTAMP_NS or TIMESTAMPTZ_NS target has a smaller range, about the years
1677 to 2262. A value outside that range also gets the type
`ErrorTypeConversion` in each position. The position of the value determines
whether the library or DuckDB raises the error:

- A parameter: the error of the library for a value outside the range of DuckDB,
  and the Conversion Error of DuckDB for each other such value, for example in
  the year 2300.
- An element of a list, a `SizedArray`, or a `Map`: the error of the library.
  For a value in the range of DuckDB, its text is "TIMESTAMP value out of finite
  range".
- A field of a STRUCT, an element of a TUPLE, or a member of a UNION: the error
  of the library, "TIMESTAMP value out of finite range", also for a value
  outside the range of DuckDB.
- In each position, a `LocalTime` or a `UTCTime` (in UTC) from 2262-04-11
  23:47:16.854775807 to 23:47:16.854775999 gets that error of the library.

In the notation of `Data.Time`, the year -5877641 is 5877642 BC and the year
-290308 is 290309 BC. The rule holds also for a finite `Unbounded` value. DuckDB
does not accept a value outside its range: its SQL literals reject it, and a
stored value can read as an infinity or make DuckDB raise an INTERNAL error. A
leap second, a `TimeOfDay` with 60 seconds, binds as the next second, as
`make_time` and `make_timestamp` of DuckDB give it. The SQL literal `'23:59:60'`
raises an error. A time after 24:00:00, for example 23:59:60.5, and a field
outside its own range, for example 61 minutes, raise an `SQLError` of the type
`ErrorTypeConversion`, also as the result of a typed function or a row of a
table function.

### Manual STRUCT, TUPLE, and UNION handling

SQL tuples, such as `row(42, 'text')`, decode as `TupleValue`. Read their
positional fields in `tupleValueFields` and types in `tupleValueTypes`.
`FieldTuple` and `LogicalTypeTuple` preserve empty tuples independently of
empty STRUCT values. STRUCT fields retain their names in `StructValue`.
`LogicalTypeCollatedVarchar` preserves a VARCHAR field's collation in composite
parameters. Its comparisons use that collation after a round trip.

Temporal fields retain their SQL units when composite values are rebound.
The `FieldDate`, `FieldTimestamp`, and `FieldTimestampTZ` constructors hold
`Unbounded` values. Wrap finite payloads in `Finite` when constructing them.
TIMESTAMP_S and TIMESTAMP_MS fields use the finite range described in
[Infinite dates and timestamps](#infinite-dates-and-timestamps). An explicit
parameter cast does not extend that range.

For more control, you can work directly with `StructValue` and `UnionValue`
from `Database.DuckDB.Simple.LogicalRep`:

```haskell
import Database.DuckDB.Simple
import Database.DuckDB.Simple.LogicalRep (StructValue (..), UnionValue (..))
import Database.DuckDB.Simple.FromField (FieldValue (..))

manualStruct :: Connection -> IO [(StructValue FieldValue, UnionValue FieldValue)]
manualStruct conn = do
  _ <- execute_ conn
    "CREATE TABLE composite (s STRUCT(a INT, b INT), \
    \u UNION(x INT, y VARCHAR))"
  [(s, u)] <- query_ conn
    "SELECT {'a': 1, 'b': 2}, \
    \CAST(union_value(x := 42) AS UNION(x INT, y VARCHAR))"
    :: IO [(StructValue FieldValue, UnionValue FieldValue)]
  _ <- execute conn "INSERT INTO composite VALUES (?, ?)" (s, u)
  query_ conn "SELECT s, u FROM composite"
```

Low-level `ToDuckValue.toDuckValue` takes a `ValueFactory`, which is the V2
factory handle, and `logicalTypeFromRep` takes the same handle.
`Database.DuckDB.Simple.V2.LogicalRep` has `logicalTypeFromRep`.
Get the factory of a connection with `withConnectionHandle` and
`withConnectionFactory` from `Database.DuckDB.Simple.Internal`. Keep owned
values and logical types within the lifetime of the factory's owner.

### FieldValue parameters

A `FieldValue` binds as a parameter, so a row that a query reads as
`[FieldValue]` binds back as a row. A scalar constructor binds as the DuckDB
type of the constructor, with the parameter instance of its payload.
`FieldStruct`, `FieldTuple`, and `FieldUnion` bind with their own types, and
`FieldGeometry` binds with its CRS. `FieldList`, `FieldArray`, and `FieldMap`
take the element type that their elements give, with the rule of a SQL list
literal (see [Arrays and Lists](#arrays-and-lists)). They read the parameter
type, so that a list read from a `TIME_NS[]` column binds back with its
nanoseconds.

TIME aliases keep their alias and exact microsecond value when they bind
inside a composite. The library uses public vector storage because the
default TIME-to-alias cast fails in DuckDB 2.0.0. Other aliases require native
cast support.

Some values bind with another type than the column that they came from. A
`FieldTimestamp` binds as a `TIMESTAMP`, or as a `TIMESTAMP_NS` when it has
digits below a microsecond and the parameter has the type `TIMESTAMP_NS` or
`VARIANT` (see [Infinite dates and timestamps](#infinite-dates-and-timestamps)).
The same holds for `FieldTime` and `FieldTimestampTZ`. A `FieldText` binds as a
`VARCHAR` with no collation, also when it came from a `JSON` value. A value
from a `VARIANT` column binds as its payload. A `FieldEnum` binds its label as
a `VARCHAR`. `FieldNull` binds as an untyped NULL. DuckDB casts each of these
values to the type of a column with no loss, with one exception: a time value
with digits below a microsecond in a `FieldList` or a `FieldMap` for a
`VARIANT` parameter gets the microsecond value, so a `VARIANT` array of
`TIMESTAMP_NS` values loses these digits when it binds back. A `VARIANT` object
reads as a `FieldStruct` with `VARIANT` fields and keeps them.

An empty or all-NULL `FieldList`, `FieldArray`, or `FieldMap` follows note 11 of
[Types](#types). Inside a `FieldStruct`, a `FieldTuple`, or a `FieldUnion`, a
`FieldEnum` binds its label to the declared ENUM type, and a label that is not
in the dictionary raises an `SQLError` of the type `ErrorTypeConversion`, as the
cast of DuckDB gives for that label. `FieldValue` has no `DuckDBColumnType` and
no `ToDuckValue` instance, because no one DuckDB type holds each of its values.

### Resource management

- `withConnection` and `withStatement` open and close resources, including when
  an exception occurs. Use them whenever possible to avoid leaking C handles.
- The query functions release their results, prepared statements, and values
  after use, and return a Haskell list of all rows. Folds and cursors decode
  rows incrementally from owned native chunks. The materialized functions of
  `Database.DuckDB.Simple` drain and close the native result before row
  callbacks; the streaming functions of `Database.DuckDB.Simple.Streaming` do
  not.
- `execute`/`query` variants reset statement bindings each run so prepared
  statements can be reused safely.

For concurrent workers, use a separate connection per worker. Threads can also
share one connection. DuckDB permits one open result for each connection, so
each connection has a lock. `query`, `execute`, and the materialized folds hold
this lock from the execution of a statement until its native result closes.
Statement preparation holds it only while DuckDB prepares the statement.
Metadata binding holds it only while DuckDB binds the statement. A call from
another thread waits. The lock does not cover these cases. Serialize them in
the application:

- A transaction. Statements of other threads run inside it.
- One `Statement` that two threads use.
- An explicit stream from `Database.DuckDB.Simple.Streaming`. Each fetch of
  the stream holds the lock, but its result stays open between calls, and
  another query on that connection fails until the stream is complete or
  closed.
- `close`. Wait for all operations on the connection before you close it.

A callback must not run a statement on the connection that runs the callback,
or close that connection: [User-Defined Functions](#user-defined-functions)
gives the rule, its time limit, and its limits.

Link your executable with `ghc-options: -threaded`. A program that registers a
callback needs it, because DuckDB can call a callback from several threads at
the same time: [User-Defined Functions](#user-defined-functions) gives the rule.
It also allows prompt cancellation of native queries, including Ctrl-C. On
cancellation, the library interrupts DuckDB and waits for the native call to
return before it releases resources and propagates the exception. Cancellation
is cooperative: native code and Haskell callbacks must return before cleanup can
finish.

Native work that can take a long time runs in a helper thread so that an
asynchronous exception can interrupt it. The materialized functions run the
native calls of one result in one helper thread. A bound thread, such as the
main thread, changes operating system threads to wait for a helper thread.
Thus a small query takes longer from a bound thread than from an unbound
thread. Run code with many small queries in a thread made with `forkIO`, or
wrap the main action in `Control.Concurrent.runInUnboundThread`.

Call `closeStatement` or `clearStatementBindings` on an abandoned statement to
release its result. DuckDB can retain native result buffers until that result
is destroyed.
Use `withStatement` for manual iteration. `fold` releases the result after
success or an exception. The accumulator determines Haskell memory use.

### Metadata helpers

- `columnCount` and `columnName` expose prepared-statement metadata so you can
  inspect result shapes before executing a query.
- `execute` returns the number of affected rows. Use SQL `RETURNING` clauses
  when you need generated identifiers.

### Cursors and folds

`fold`, `fold_`, and `foldNamed` decode one row at a time from DuckDB's result
chunks. This API materializes the native result before the first row is
returned. These functions avoid a complete Haskell row list, but native memory
use still depends on the result size. The materialized functions close the
native result before row delivery. A row callback can run another query on the
same connection.

```haskell
import Database.DuckDB.Simple

sumValues :: Connection -> IO Int
sumValues conn =
  fold_ conn "SELECT n FROM stream_fold ORDER BY n" 0 $ \acc (Only n) ->
    pure (acc + n)
```

For manual cursor-style iteration, use `nextRow`/`nextRowWith` on an open
`Statement` to pull rows one at a time and decide when to stop.

Cursors support the same column types as the query functions, including STRUCT
and UNION values with nested collections and NULLs. GEOMETRY and VARIANT work
with the query functions, cursors, and folds.

#### Optional native streaming

`Database.DuckDB.Simple.Streaming` provides `fold`, `fold_`,
`foldNamed`, `nextRow`, and `nextRowWith` with native streaming enabled.
Import it qualified:

```haskell
import Database.DuckDB.Simple
import qualified Database.DuckDB.Simple.Streaming as Streaming

streamSum :: Connection -> IO Int
streamSum conn =
  Streaming.fold_ conn "SELECT i FROM range(1000000) t(i)" 0 $ \acc (Only n) ->
    pure (acc + n)
```

This module uses V2 execution and incremental chunk fetching. DuckDB can still
materialize some queries. Streaming does not bound the memory used by query
operators.

The first cursor fetch selects the execution mode. The mode stays until `bind`,
`bindNamed`, `clearStatementBindings`, or `executeStatement` releases the
result. Switching between materialized and streaming `nextRow` calls retains
that mode. Keep the connection dedicated to the explicit stream. Until the
stream is complete, DuckDB rejects other statements on that connection with the
`ErrorResourceInUse` error of DuckDB. If you close an incomplete stream, DuckDB
aborts its statement. In autocommit mode, DuckDB rolls back the writes of that
statement. Cancellation interrupts native chunk fetching as well as execution.

This module also provides `foldArrow` and `foldArrow_` for streaming Arrow
batches. They use the same supported Arrow conversion and scoped ownership
as `Database.DuckDB.Simple.Arrow`.

### Arrow batches

`Database.DuckDB.Simple.Arrow` provides `foldArrow` and `foldArrow_` for clients
that consume the Arrow C Data Interface. Each callback receives a separate
schema and array batch. It can read them or pass them to an Arrow consumer
that releases or moves them. The fold releases any remaining contents on
success, failure, or cancellation. The consumer owns any contents it moves.

Each array has at most `connectionArrowBatchSize` rows. Set this option in
`ConnectionOptions`. The default is `defaultArrowBatchSize`, 2048 rows. DuckDB
can give shorter arrays. Opening a connection rejects a value that is not
positive. For DuckDB's batch size, set 131072.

The original pointers are valid only during the callback. To retain contents,
a consumer must move the root structs into its own storage and set the source
release fields to NULL. Moved contents remain valid after the query and
connection close. Empty results do not produce a callback.

For example, the `dataframe-arrow-bridge` package can copy each batch into a
Haskell `DataFrame` and release the Arrow objects:

```haskell
import qualified DataFrame.IO.Arrow as DataFrame
import qualified Database.DuckDB.Simple.Arrow as Arrow
import Foreign.Ptr (castPtr)

frames <- Arrow.foldArrow_ conn "SELECT id::BIGINT, name::VARCHAR FROM people" [] $ \acc schema array -> do
  frame <- DataFrame.arrowToDataframe (castPtr schema) (castPtr array)
  pure (frame : acc)
-- Reverse frames to recover the query's batch order.
```

The bridge currently imports signed 32-bit and 64-bit integers, Float, Double,
and text columns. It is a test dependency of this repository; applications
that use it must declare their own dependency on `dataframe-arrow-bridge`.

Arrow export uses V2 Arrow results. The materialized `foldArrow` retains owned
batches and closes the native result before callbacks start. Its memory use
depends on the result size. The streaming `foldArrow` delivers each batch while
the native result remains open.

### VARIANT

VARIANT results decode to the `FieldValue` of the stored value. Existing
`FromField` instances read scalar payloads. Objects decode to `FieldStruct`
values with VARIANT fields in DuckDB's native key order (lexicographic).
Arrays decode to `FieldList` and preserve element order.
The decoder uses SQL type metadata and native casts. It preserves scalar types,
precision, and nested values without reading DuckDB's private payload format.

```haskell
query_ conn "SELECT 42::BIGINT::VARIANT" :: IO [Only Int64]
```

SQL NULL VARIANT values decode to `FieldNull`. `Variant` from
`Database.DuckDB.Simple.Variant` wraps a `FieldValue`. Its `ToField` instance
binds the payload as VARIANT. `variantObject` builds an object from named fields.
It can read the bound value back directly:

```haskell
query conn "SELECT ?"
  (Only (Variant (FieldList [FieldInt64 1, FieldInt64 2])))
  :: IO [Only Variant]
```

A scalar payload keeps its native type, such as `TINYINT` or `DECIMAL(4,2)`.
Time and timestamp payloads bind as microsecond types, or as nanosecond types
when they have sub-microsecond digits and fit the nanosecond range. So a
`TIMESTAMP_S` result binds back as a `TIMESTAMP` when its value fits. Each
element of a list payload and each field of an object payload follow the same
rule, so one list payload can hold both types. A Haskell list, a `SizedArray`,
or a `Map` of time values for a `VARIANT` parameter is not a payload: it binds
microsecond elements (see [Infinite dates and
timestamps](#infinite-dates-and-timestamps)). A value outside the range of
DuckDB raises an `SQLError` of the type `ErrorTypeConversion`. Lists, arrays,
and STRUCT fields bind as VARIANT values. A VARIANT does not keep a UNION, a
MAP, an ENUM, or the CRS of a GEOMETRY, so these payloads raise an error at any
depth: a VARIANT keeps only the member value of a UNION, stores a MAP as a list
of key and value objects, and stores an ENUM as its text. Object parameters
reject duplicate keys and keys that contain NUL. An object with only an empty
key also raises an error because DuckDB treats its STRUCT type as a tuple.
DuckDB 2.0 permits an empty key with other named keys. You can also bind a plain
value and cast it in SQL, as in `?::VARIANT`.

A `Variant` result reads only a value that a VARIANT can hold: note 13 of
[Types](#types) gives the shapes, and the refusal of a JSON type.

For query results the library uses a separate helper connection for VARIANT decoder metadata.
It caches one prepared VARIANT metadata statement and the types for VARIANT
casts. It batches VARIANT metadata queries. It constructs VARIANT types
directly through the native factory. This preserves an explicit stream on the
caller's connection. The helper connection has the settings of the database,
not the caller's session settings. Metadata preparation needs an expression
depth of 11. A GLOBAL `max_expression_depth` below 11 fails preparation with a
Binder Error, including preparation for a connection's COPY TO. Extension COPY TO
and typed callback arguments use an independent instance with default expression
depth. A prepared plan on a query helper can remain
usable after the limit is lowered. The query for unusual object keys still
binds on each execution and can fail at a lower limit.

New DuckDB 2.0 databases use storage format 2.0, which supports VARIANT columns.
The library does not change an existing database's storage compatibility
setting. VARIANT columns require storage format `v1.5.0` or later.

### GEOMETRY

Use `Geometry` from `Data.Geometry` for decoded shapes. The `geometry-simple`
package supplies the type and unboxed coordinate vectors. `duckdb-simple`
supplies its `ToField` and `FromField` instances:

```haskell
import qualified Data.Geometry as G
import qualified Data.Vector.Unboxed as U
import Database.DuckDB.Simple

let line = G.LineString (G.CoordinatesXY (U.fromList [G.XY 0 0, G.XY 1 2, G.XY 3 4]))
rows <- query conn "SELECT ?" (Only line) :: IO [Only G.Geometry]
```

Points and coordinate sequences store their XY, XYZ, XYM, or XYZM layout.
`G.EmptyPoint G.DimXYZ` is an empty XYZ point. Use `Maybe G.Geometry` for SQL NULL.
Use `decodeWKT` from `Data.Geometry.WKT` to parse text without a database.

`G.Geometry` has no CRS metadata. Reading it returns the coordinates without
their CRS label. Binding it creates a `GEOMETRY` with no CRS. To attach a label,
use `ST_SetCRS(?, ?)` with a shape and CRS text. This does not transform coordinates.

Use `RawGeometry` from `Database.DuckDB.Simple.Geometry` to read WKB and CRS
without decoding coordinates. Its `rawGeometryWKB` and `rawGeometryCRS` fields
own their data. They remain usable after the connection closes. Existing
`ByteString` result decoding also returns WKB.

A `RawGeometry` parameter binds as a `GEOMETRY` value with the same WKB bytes
and its own CRS. DuckDB then casts it to the type of the parameter, as it casts
a `GEOMETRY` value in SQL text. A value without a CRS takes the CRS of the
target. A target without a CRS removes the CRS of the value, with no error. A
target with another CRS raises `SQLError`. Use `Maybe RawGeometry` for SQL NULL.
A `SizedArray`, a list, and a typed row cannot hold a `RawGeometry` (note 14 of
[Types](#types)). A composite parameter holds it as `FieldGeometry` (see below):

```haskell
import Database.DuckDB.Simple.Geometry

[Only raw] <- query_ conn "SELECT 'POINT Z (1 2 3)'::GEOMETRY('OGC:CRS84')" :: IO [Only RawGeometry]
rows <- query conn "SELECT ?" (Only raw) :: IO [Only RawGeometry]
```

For explicit SQL import, use `system.main.ST_GeomFromWKB(?)` with the WKB
bytes. To attach a CRS, use
`system.main.ST_SetCRS(system.main.ST_GeomFromWKB(?), ?)` with the bytes and CRS
text. Omit `ST_SetCRS` for no CRS: a NULL CRS argument returns NULL geometry.
CRS text can be a recognized name, a custom label, or WKT2/PROJJSON text.
DuckDB can normalize WKB byte order and reduce a known CRS definition to its
registered identifier. The qualified names select the built-ins even if a
user macro has the same name.

`fromRawGeometry` decodes the shape and drops CRS metadata. `toRawGeometry`
encodes a shape with no CRS. These helpers follow the geometry-simple contracts.
NaN in both WKB point X and Y denotes an empty point. Empty multi-geometries
and collections have no stored layout tag in the decoded representation.
Keep the raw bytes when these details must survive.

Structured parameters use `encodeWKT` and DuckDB's native cast. The WKT writer
combines layouts in multi-geometries and polygon rings. It fills absent Z or M
with NaN.
DuckDB's WKT parser limits nesting to 16 levels and rejects empty polygon rings
and mixed collection layouts. DuckDB 2.0 also rejects WKB with mixed Z/M member
layouts. Explicit WKB import preserves native NaN payload bits that WKT cannot
retain.

`FieldGeometry` contains a raw result. Result decoding retains WKB and CRS,
including inside LIST, ARRAY, MAP, STRUCT, and UNION values. A STRUCT, TUPLE,
or UNION parameter binds each `FieldGeometry` as a `GEOMETRY` with the same
WKB bytes and CRS, also inside LIST, ARRAY, and MAP values. Thus a value that
a query returns binds again without a change. The CRS of the value must be
equal to the CRS of its declared type. A mismatch raises `SQLError` with the
type `ErrorInputInvalid`, for example for a value with the CRS `OGC:CRS84` in
a field of type `GEOMETRY`. A `Variant` parameter binds a `FieldGeometry`
without a CRS as a `GEOMETRY` payload. A VARIANT does not keep the CRS of a
GEOMETRY, so a `FieldGeometry` with a CRS in a `Variant` raises `SQLError`.

`LogicalTypeGeometry` describes CRS metadata. Composite parameters retain it.
Recognized names, such as `OGC:CRS84`, and full WKT2/PROJJSON definitions use
direct V2 type construction. For an unknown custom label, parameter binding
reads a SQL-derived type with `ST_SetCRS` on a helper connection. This happens
on demand, including for nested types, typed NULLs, empty collections, and
inactive UNION members. Binding the same custom label again repeats the lookup.
A blank label or a label with NUL raises `SQLError` at bind time.

Query results retain a custom CRS label. Typed registrations and direct
low-level `logicalTypeFromRep` calls use native type construction. They require
a recognized name or a full definition. A custom label in a declared argument,
result, cast source or target, pragma parameter, or table-function column fails
at registration. DuckDB casts a plain `GEOMETRY` argument to its declared type,
which removes the CRS. Direct type construction raises an error if DuckDB
drops the CRS under `ignore_unknown_crs`.

The CRS in a cast must be a constant. DuckDB rejects a parameter as a type
modifier, so `?::GEOMETRY(?)` is not valid. To use a CRS that is known only at
run time, write it into the query text as a SQL string literal, and double each
single quote. A cast accepts a CRS that DuckDB recognizes, such as `OGC:CRS84`,
or a full WKT2 or PROJJSON definition. Unless an extension recognizes it,
DuckDB rejects other identifiers, such as `EPSG:4326`, and custom names.
`ST_SetCRS` also accepts custom names, but it returns `GEOMETRY` with no CRS
for a NULL input.

A bound composite without a CRS also changes the type of expressions that
combine it with CRS data. `UNION ALL` and `COALESCE` of `GEOMETRY` and
`GEOMETRY('OGC:CRS84')` give `GEOMETRY` with no CRS for all rows. Cast the
parameter before you combine it with other values.

See [geometry-simple](https://github.com/Tritlo/geometry-simple) for the seven
supported families, construction checks, and codec normalization rules.

### Feature coverage

- Connections, prepared statements, positional/named parameter binding.
- High-level execution (`execute*`) and the query functions (`query*`,
  `queryNamed`).
- Cursor and fold helpers (`fold`, `foldNamed`, `fold_`, `nextRow`) that decode
  native result chunks one row at a time.
- Scalar types: signed/unsigned integers, HUGEINT/UHUGEINT,
  decimals (with width/scale), intervals, precise and timezone-aware temporals,
  enums, bit strings, blobs, bignums, and UUIDs.
- Composite types: STRUCTs, UNIONs, LISTs, fixed-length ARRAYs, and MAPs with
  typed parameters and results.
- Generic encoding/decoding: automatic STRUCT/UNION mapping for Haskell ADTs via
  GHC generics and the `ViaDuckDB` deriving-via helper.
- Row decoding via `FromField`/`FromRow`, with generic deriving for product types.
- User-defined scalar functions backed by Haskell functions (including IO and
  nullable arguments).
- Aggregate functions, table functions, replacement scans, COPY formats, and
  pragmas. See [Typed registration](#typed-registration).
- Transaction helpers (`withTransaction`) and metadata accessors (`columnCount`,
  `columnName`).

## User-defined functions

Register a scalar Haskell function on a connection, and call it in SQL
expressions. The arguments use `FromField`, and the result uses
`ToDuckValue`. A function can accept and return `Maybe` values, and it can
run an `IO` action.

An argument type needs `FromField` and `DuckDBColumnType` instances. Its SQL
type is the `duckdbTypeDeclaration` of the type, as for a column of a typed row.
A newtype with its own column type name (`duckdbColumnTypeFor`), for example
`DECIMAL(18,3)`, gives that text. Arguments use the same conversion as query
results, including nested values. Registered casts do not change how the
library reads GEOMETRY arguments. See [Typed registration](#typed-registration).

The SQL type of the result is the `duckdbTypeDeclaration` of the result type:
an `Int16` result is SMALLINT, and a `Float` result is FLOAT. DuckDB computes
with that type. Thus `hs_small(x) * hs_small(y)` of an `Int16` function raises
an Out of Range Error when the product does not fit in SMALLINT, as
`30000::SMALLINT * 30000::SMALLINT` does. Cast the result to a wider type
first. Some result types write directly into the result vector. Each other
result type requires a native value for each row. [Types](#types) lists them.

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Int (Int64)
import Database.DuckDB.Simple

registerAndUse :: Connection -> IO [Only Int64]
registerAndUse conn = do
  createFunction conn "hs_times_two" (\(x :: Int64) -> x * 2)
  query_ conn "SELECT hs_times_two(21)"
```

An exception in the function fails the statement with an `SQLError`:
[Errors](#errors) gives its text. `deleteFunction` issues a
`DROP FUNCTION IF EXISTS` statement and reports native errors. DuckDB registers
C API scalar functions as internal entries and rejects their deletion. The
function remains registered after this `SQLError`. Deleting an absent name
succeeds. To replace a function, register the name again. `deleteFunction` has a
GHC `WARNING` pragma, so each use causes a compile-time warning. A build with
`-Werror` must allow that warning, for example with `-Wno-error=deprecations`.

A program that registers a callback, for example of a scalar function, a cast,
an aggregate function, a table function, a COPY format, a replacement scan, a
pragma, or a log sink, must be linked with `ghc-options: -threaded`. DuckDB can
call a callback from several of its threads at the same time. Without
`-threaded`, the GHC runtime does not support calls from several operating
system threads at the same time, so the program can crash.

Without `-threaded`, registering a COPY format with callbacks or a log sink
raises `ErrorInputInvalid` regardless of the settings. DuckDB can open a COPY
file on an asynchronous worker, which can also write to the log sink.
For other callbacks, registration raises `ErrorInputInvalid` when the DuckDB
setting `threads` is above 1. This check only reads the setting at registration.
It cannot prevent another connection from raising the thread count later.

With `-threaded`, each call of a callback needs a capability of the GHC runtime.
With one capability, which is the default, a callback from a second DuckDB
thread waits until the capability is free, and the operating system switches
threads for each such call. A query that makes many callbacks on several DuckDB
threads is then much slower. Give the program as many capabilities as DuckDB has
threads, for example with `ghc-options: -threaded -rtsopts "-with-rtsopts=-N"`,
or set the DuckDB setting `threads` to 1.

A callback must not run a statement on the connection that runs the callback,
and it must not close that connection. Use another connection of the same
instance. This rule applies to each registered callback, for example of a scalar
function, a cast, an aggregate function, a table function, a COPY format, a
replacement scan, a pragma, or a log sink. `query`, `execute`, the materialized
folds, statement preparation, and each fetch of an explicit stream hold the lock
of the connection while DuckDB runs these callbacks. The lock is not reentrant,
so a statement in the callback waits for the lock. With more than one DuckDB
thread, DuckDB can also run a callback of an explicit stream between two
fetches, while no fetch holds the lock. Then a statement in the callback on the
same connection fails at once with the `ErrorResourceInUse` error of DuckDB,
because the stream has an open result.

That wait has a time limit. `connectionCallbackLockWait` in `ConnectionOptions`
sets it in microseconds. The default is `defaultCallbackLockWait`, 10 seconds,
and `Nothing` sets no time limit. When the time limit ends, the statement in the
callback raises an `SQLError` of the type `ErrorResourceInUse`, and its text
gives the rule. If the callback does not catch the error, the outer statement
fails with the type `ErrorInputInvalid`, because a callback does not pass the
type `ErrorResourceInUse` ([Errors](#errors) gives the list; a cast gives
`ErrorTypeMismatch`), and its message contains the text of that error. The
connection can run the next statement. DuckDB ignores the error of a log sink.
The time limit applies to each wait of a callback for the lock of a connection.
It applies also to another connection that a different statement uses. If
callbacks share a connection that runs slow statements, set a larger time limit
or `Nothing` for that connection.

In these cases the time limit does not apply, and a statement in the callback
can wait indefinitely:

- DuckDB runs the callback during a call that does not hold the lock of the
  connection, for example a log sink for a message of `logConnectionMessage`.
  Then the statement goes into DuckDB on the same connection.
- The statement runs in a thread that the callback starts. The library marks
  only the thread of the callback.
- The statement runs inside `uninterruptibleMask`.
- The statement runs in the flush function of a log sink when DuckDB destroys
  the sink.

A statement in a callback on another connection of the same instance runs in its
own transaction, also when the outer statement runs in an explicit transaction.
It does not see rows that the outer transaction has not committed. A write to
a row that the outer transaction changed fails immediately with DuckDB's
`TransactionContext Error: Conflict on update!` error. The outer statement
fails with that text. That failure aborts the outer transaction, as each error
during the execution of a statement does in DuckDB:
each later statement in the transaction fails, and a `COMMIT` returns with no
error but stores no change of the transaction.

A callback that refers to a connection keeps that connection and its instance
reachable while DuckDB keeps the callback, possibly until the database closes.
The garbage collector cannot close them while they remain reachable. The
program must close them.

## Typed registration

These functions register SQL objects with Haskell values. Their signatures have
no native handle. `Database.DuckDB.Simple.Extension` has the same functions for
a loadable extension. An exception in a Haskell function gives an SQL error with
the message of the exception (see [Errors](#errors)). The examples use these
imports:

```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Database.DuckDB.Simple
import Database.DuckDB.Simple.Copy
import Database.DuckDB.Simple.FromField (Field (..), FieldValue (..))
import Database.DuckDB.Simple.Pragma (PragmaCallback (..), registerPragmaFunction)
import Database.DuckDB.Simple.Registration
```

The library has no typed cast in this release. A registered cast can give
wrong rows with no error, because the optimizer of DuckDB 2.0.0 decides from
the two types of a cast alone whether it can move the cast away from a column
in a filter or a join. In a run, it also gave min and max from the statistics
of the column without the cast. The cast function then does not run on that
column, or, in a comparison of two columns, it runs on one side only. In runs
with DuckDB 2.0.0, a cast from INTEGER to BIGINT, and a cast between BIGINT and
a custom type with the base type BIGINT, gave wrong rows when its function
changed the value. `registerCast` in
`Database.DuckDB.Simple.V2.Registration` registers a native cast, and its
Haddock gives the warning. For a conversion that changes the value, register a
scalar function with `createFunction` (see
[User-Defined Functions](#user-defined-functions)).

A registered cast does not change a value that the library reads or binds, on
a connection and in the callbacks of an extension. The library reads GEOMETRY
and VARIANT values with the built-in casts, and it builds GEOMETRY, ENUM, and
VARIANT values with the default cast of DuckDB. For example, a cast from
GEOMETRY to BLOB changes the result of `CAST(g AS BLOB)`, but not a GEOMETRY
value that a query gives or that a callback reads. Of the casts that the library
performs itself, only `setConfigValue` and `setConfigOption` use registered
casts. DuckDB casts the value to the type of the option, as SQL `SET` does.

`createAggregate` registers an aggregate function as a fold. Its result needs
`ToDuckValue` and a static `duckdbTypeDeclaration`, as a scalar function result
does. Lists, maps, and records or sum types through `ViaDuckDB` can be results.
A `Maybe a` result declares the type of `a`, and `Nothing` gives NULL. Types
with no static declaration, such as `StructValue`, are errors at registration.
See [Types](#types) for the encoding rules. Aggregates support `GROUP BY`,
`DISTINCT`, `FILTER`, `ORDER BY`, and window functions.

```haskell
aggregateExample :: Connection -> IO [Only Int64]
aggregateExample conn = do
  createAggregate conn "haskell_sum" Aggregate
    { aggregateInitial = 0 :: Int64
    , aggregateStep = \total (Only value) -> pure (total + value)
    , aggregateCombine = \a b -> pure (a + b)
    , aggregateResult = pure
    }
  query_ conn "SELECT haskell_sum(i) FROM range(10) t(i)" -- [Only 45]
```

The library keeps the live aggregate states in a table with stripes. Two
DuckDB threads wait for each other only when they change the same stripe.
`connectionAggregateStripes` in `ConnectionOptions` sets the number of stripes
for each aggregate that `createAggregate` registers through the connection.
The default is `defaultAggregateStripes`, 64. No result depends on the number.

Scalar and aggregate registrations create a private helper instance when an
argument needs VARIANT decoding. In a loadable extension, this helper also
supplies the factory for type metadata, GEOMETRY reads, and aggregate result
encoders. The registration shares the instance
across queries until the last native callback owner releases it. Each VARIANT
vector uses a separate connection and decoder state. The helper uses
`threads=1` and `async_threads=0`, with no background worker pool. A registration
on a connection uses its existing instance for type metadata and GEOMETRY reads.

`createTableFunction` registers a table function that gives its rows as a
list. DuckDB reads only the columns that the query uses, also in `WHERE` and
`ORDER BY`. For a query that uses no column, such as `SELECT count(*)`, DuckDB
reads the first column. The library does not evaluate the other columns of a
row, so an exception in such a column does not fail the query. The library
evaluates each row that it writes to its constructor. This evaluates each
strict field of the row (a field with `!`, or each field with `StrictData`).

```haskell
tableExample :: Connection -> IO [(Int64, Int64)]
tableExample conn = do
  createTableFunction conn "squares" ["n", "square"] \(Only count) ->
    pure [(n, n * n) | n <- [0 .. count - 1 :: Int64]]
  query_ conn "SELECT n, square FROM squares(3)" -- [(0, 0), (1, 1), (2, 4)]
```

The type of a column, a parameter, or an aggregate argument comes from
`duckdbTypeDeclaration`: notes 8, 9, and 10 of [Types](#types) give the rule
for a `SizedArray`, records, sum types, and `StructValue`, `TupleValue`, and
`UnionValue`. Arguments use the same conversion as query results.

The decoder uses SQL to inspect VARIANT values. For callback arguments it
copies the rows into a private DuckDB instance. The helper has no access to
the host's catalog, settings, or registered casts. A concrete SQL argument type
avoids this work when all rows have a known type. A decoding error fails the
calling statement under the usual callback error rules.

`registerReplacementScan` gives the parts of an unknown table name to the
function. The function can give a SELECT statement that DuckDB reads instead.

```haskell
scanExample :: Connection -> IO [Only Int64]
scanExample conn = do
  registerReplacementScan conn \parts ->
    pure (if parts == ["numbers"] then Just (ReplaceWithQuery "SELECT i FROM range(3) t(i)") else Nothing)
  query_ conn "SELECT i FROM numbers ORDER BY i" -- [Only 0, Only 1, Only 2]
```

`registerCopyFunction` registers a COPY format with a COPY TO side and a COPY
FROM side. DuckDB keeps the first registration of a format name. Thus register
a format with two directions in one call.

```haskell
copyExample :: Connection -> Text -> IO [Only Text]
copyExample conn path = do
  registerCopyFunction conn "haskell_lines" linesTo \info ->
    map Only . Text.lines <$> TextIO.readFile (copyFromPath info)
  _ <- execute conn "COPY (SELECT i::VARCHAR AS line FROM range(3) t(i)) TO ? (FORMAT haskell_lines)" (Only path)
  _ <- execute_ conn "CREATE TABLE copied (line VARCHAR)"
  _ <- execute conn "COPY copied FROM ? (FORMAT haskell_lines)" (Only path)
  query_ conn "SELECT line FROM copied ORDER BY line" -- [Only "0", Only "1", Only "2"]

linesTo :: CopyTo () FilePath
linesTo =
  CopyTo
    { copyToBind = \_ -> pure ()
    , copyToInit = \info -> TextIO.writeFile (copyInitFilePath info) "" >> pure (copyInitFilePath info)
    , copyToSink = \info rows -> TextIO.appendFile (copySinkGlobalState info) (Text.unlines (map line rows))
    , copyToFinalize = \_ -> pure ()
    }
  where
    line [Field{fieldValue = FieldText text}] = text
    line _ = error "haskell_lines: each row must have one VARCHAR value"
```

`registerPragmaFunction` gives the arguments of the call to the callback as a
`FromRow` row. A `QueryPragma` callback gives SQL that DuckDB runs in place of
the PRAGMA statement.

```haskell
pragmaExample :: Connection -> IO [Only Int64]
pragmaExample conn = do
  registerPragmaFunction conn "repeat_answer" [parameter "times" (TypeExpression "BIGINT")] $
    QueryPragma \(Only (times :: Int64)) -> pure ("SELECT 42::BIGINT FROM range(" <> Text.pack (show times) <> ")")
  query_ conn "PRAGMA repeat_answer(2)" -- [Only 42, Only 42]
```

A typed registration has more overhead for each row than raw vector access. Use
`Database.DuckDB.Simple.V2.Registration`, `V2.Copy`, `V2.Pragma`, and
`V2.Config` for speed, and for filter pushdown, parallel table scans, and
scalar functions with a bind callback or variadic parameters. A typed table
function has projection pushdown (see `createTableFunction` above), but no
filter pushdown.

## Loadable extensions

`Database.DuckDB.Simple.Extension` registers the typed functions of this package
from a loadable DuckDB extension. Build the extension with the `extension` flag
of `duckdb-ffi` and of `duckdb-simple`. The module's Haddock describes the
entry point, typed registrations, and resource lifetimes. The repository's
[extension recipe](https://github.com/Tritlo/duckdb-haskell/blob/fc494d8bdf8226f19fb8551f5ac8049f2d1d9920/dev/extensions/README.md)
has the build, packaging, loading, and test commands. Use a repository checkout
that matches the package sources. The recipe and example are not included in
the Hackage archives. CI builds and tests the example.

## Tests

The package has five test suites. Each suite tests the library through a
database.

- `duckdb-simple-test` is the main suite, built with
  [tasty](https://hackage.haskell.org/package/tasty). It covers connections,
  statements, parameters, results, streaming, Arrow batches, cancellation,
  COPY functions, VARIANT and GEOMETRY values, typed registrations, and
  callbacks.
- `duckdb-simple-leak-test` counts native threads and resident memory through
  repeated queries, failures, and cancellation.
- `duckdb-simple-nonthreaded-test` is linked without `-threaded`. It checks the
  error for the registration of a callback in such a program.
- `duckdb-simple-dataframe-test` tests the Arrow export with the dataframe
  library. It needs the flag `dataframe-tests`.
- `duckdb-simple-quack-test` tests authenticated local Quack connections. It
  needs the flag `quack-tests`, and the variables `DUCKDB_HTTPFS_EXTENSION` and
  `DUCKDB_QUACK_EXTENSION` must name the extension files.

```
cabal test all --test-show-details=direct
cabal test duckdb-simple-test --test-show-details=direct
cabal test duckdb-simple-dataframe-test -fdataframe-tests
```

`cabal test all` runs the first three suites. The repository's CONTRIBUTING.md
gives the rules for new tests and the variables for a test run.
