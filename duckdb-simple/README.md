# duckdb-simple

`duckdb-simple` is a Haskell interface to DuckDB. Its API is like the APIs of
[`sqlite-simple`](https://hackage.haskell.org/package/sqlite-simple) and
[`postgresql-simple`](https://hackage.haskell.org/package/postgresql-simple).
It uses the bindings of [`duckdb-ffi`](../duckdb-ffi). It opens connections,
runs queries, binds parameters, and decodes results into Haskell types. It
supports the scalar types of DuckDB, for example signed and unsigned integers,
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
configuration. Each connection supports one active native stream. Use a separate
connection for each concurrent worker.
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
DuckDB can keep alive an instance that `createInstance` did not create. Then
`closeEnvironment` raises an `SQLError` and keeps the environment open.
Closing an instance closes its connections and statements. Each close operation
is idempotent. `open` and `withConnection` create private parents and retain their
existing convenience behavior.

## V2 facilities

- `Config` lists options with effective values, typed defaults, aliases, and
  supported scopes. It reads and writes typed values and registers custom
  options. The update function of a custom option gets the new value as a
  `FieldValue`. `V2.Config` registers custom options with native builders.
- `Catalog.describeTable` copies resolved names, column types, default flags,
  generated-column flags, and catalog read-only status.
- `Tokenizer.tokenize` returns token kinds, UTF-8 byte offsets, token bytes, and
  the native indication of an unterminated final token.
- `Function` registers scalar, cast, aggregate, and table functions with
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
- `V2.Registration.registerMacroFunctionWith` gives the native macro builder
  to a configuration action during extension initialization. The native macro
  builder has no connection target; use SQL `CREATE MACRO` on a connection.
- `V2.FunctionDocs` sets descriptions, examples, and categories on function
  builders.
- `QueryMetrics` enables profiling and copies query and operator metric trees.
- `Internal.withCollectionStatement` binds native column collections as named
  table inputs for one scoped prepared-statement action. The caller retains the
  collections.

The pinned V2 API does not expose custom file-system registration or
collation lookup. DuckDB 2.0 rejects an ARRAY type with zero elements. An
empty LIST is valid.
Prepared dynamic PIVOT supports repeated execution with a changing schema. It
does not support parameters or a cacheable plan. VARIANT decoding uses SQL
metadata and native casts on a helper connection. A configured custom GEOMETRY
CRS name uses one SQL query when the connection opens.
GEOMETRY results use a native cast to BLOB.
Native GEOMETRY collections require the same Z/M dimensions on every member,
including empty members. The standalone WKB codec retains mixed layouts.

## Getting Started

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

### Key Modules

- `Database.DuckDB.Simple` – connections, prepared statements, execution,
  queries, metadata, and error handling.
- `Database.DuckDB.Simple.ToField` / `ToRow` – typeclasses and helpers for
  preparing positional or named parameters.
- `Database.DuckDB.Simple.FromField` / `FromRow` – typeclasses for decoding
  query results, with generic deriving support for product types.
- `Database.DuckDB.Simple.Generic` – automatic encoding/decoding of Haskell
  ADTs as DuckDB STRUCTs and UNIONs via GHC generics and the `ViaDuckDB`
  deriving-via helper.
- `Database.DuckDB.Simple.LogicalRep` – structured value types (`StructValue`,
  `TupleValue`, `UnionValue`) for working with DuckDB's composite types.
- `Database.DuckDB.Simple.Types` – shared types (`Query`, `Null`, `Only`,
  `(:.)`, `SQLError`).
- `Database.DuckDB.Simple.Function` – register scalar Haskell functions that
  SQL can call, and casts, aggregate functions, and table functions.
- `Database.DuckDB.Simple.Registration`, `Copy`, `Pragma`, and `Config` –
  register custom types, replacement scans, COPY formats, pragmas, and custom
  options with Haskell values.

## Querying Data

```haskell
import Database.DuckDB.Simple

fetchNames :: Connection -> IO [Maybe String]
fetchNames conn = do
  _ <- execute_ conn "CREATE TABLE names (value TEXT)"
  _ <- executeMany conn "INSERT INTO names VALUES (?)"
    [Only (Just "Alice"), Only (Nothing :: Maybe String)]
  fmap fromOnly <$> query_ conn "SELECT value FROM names ORDER BY value IS NULL, value"
```

The execution helpers return the number of affected rows (`Int`) so callers can
assert on data changes when needed.

An `Integer` or a `Natural` parameter binds as a HUGEINT value when the value is
in the HUGEINT range (-2^127 to 2^127 - 1). DuckDB then checks that the value
fits its target. For example, a negative value for an unsigned column raises an
error. A value outside the HUGEINT range binds as a BIGNUM value. If the
parameter has an integer type or a DECIMAL type, such a value raises an
`SQLError` of the type `ErrorInputOutOfRange`, except for a UHUGEINT parameter
that can hold it. Bind a `BigNum` to bind a BIGNUM value in each case.

In DuckDB 2.0.0 the casts from BIGNUM to the integer types have defects: some
fail, and some store a wrong value with no error
([issue 41](https://github.com/Tritlo/duckdb-haskell/issues/41)). An `Array`
or a list of `Integer` or `Natural` values takes the element type of its target
when that type is an integer type or DECIMAL, so DuckDB checks each element. A
`BigNum` value, and an `Array` or a list of `BigNum` values, still have these
defects for an integer target.

An `IntervalValue` binds as an INTERVAL, and DuckDB keeps its months, days, and
microseconds. DuckDB compares intervals with 30 days for each month, so
`INTERVAL 30 DAY = INTERVAL 1 MONTH`. A `TimeWithZone` binds as a TIMETZ with its
time and its offset in seconds east of UTC (`timeWithZoneOffsetSeconds`).
DuckDB stores an offset from -15:59:59 to +15:59:59, and a value reads back
equal to the bound value. A time with digits below one microsecond loses these
digits, as in SQL text. A `DecimalValue` binds as a DECIMAL with
its own width and scale. DuckDB casts it to the type of its target: a smaller
scale rounds half away from zero, and a value that the target cannot hold
raises an error. An `Array`, a list, or a typed row cannot hold a
`DecimalValue`, because its column type name has no width and scale.

`query_` and `execute_` parse all input statements before execution. A syntax
error prevents every statement in that input from running. An execution error
can leave effects from earlier statements. Use `withTransaction` when those
effects must roll back together.

## Named Parameters

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

DuckDB does not allow mixing positional and named placeholders within the same
SQL statement; the library preserves DuckDB’s error message in that situation.
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

### Decoding rows

The method `fromRow` of a `FromRow` instance is a `RowParser`. Write it with
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

Helper combinators such as `field`, `fieldWith`, and `numFieldsRemaining` are
available when a custom instance needs fine-grained control.

A `FromField` instance reads the SQL type of its Haskell type, for example
BIGINT for `Int64`. It also reads another SQL type when it gives the value of
the cast of DuckDB to the SQL type of the Haskell type, or an error where the
value does not fit. For example, `Int8` reads a BIGINT that fits in a TINYINT,
and `Double` reads a DECIMAL with the value of `c::DOUBLE`. A read that needs
the TimeZone setting of the session, or another text than the cast gives,
raises the error of an incompatible column: a TIMESTAMP or a DATE as `UTCTime`,
a TIMESTAMPTZ as `LocalTime`, a FLOAT or a DOUBLE as `Text`, a VARCHAR as
`ByteString`, and an ENUM as an integer type. Write the cast in SQL for these
values, for example `c::TIMESTAMP AT TIME ZONE 'UTC'`, `c::VARCHAR`,
`encode(c)`, or `enum_code(c)::BIGINT`. An ENUM value reads as `Text` or
`String` with its label, the text of `c::VARCHAR`, and a `FieldEnum` holds the
index and the label. The Haddock of each `FromField` instance gives the SQL
types that it reads.

## Generic Encoding with ViaDuckDB

The `Database.DuckDB.Simple.Generic` module provides automatic encoding and
decoding of Haskell algebraic data types as DuckDB STRUCTs and UNIONs via
GHC generics.

### Product Types as STRUCTs

Product types (records) are automatically encoded as DuckDB STRUCT values:

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

### Sum Types as UNIONs

Sum types are encoded as DuckDB UNION values, with each constructor becoming a
union member:

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

A constructor without fields (like `Point`) has the STRUCT without fields as
its payload. DuckDB shows this payload as `{}`, and it is not NULL.
Non-record constructors use positional field names (`field1`, `field2`, etc.).

### Arrays and Lists

DuckDB arrays (fixed-length) and lists (variable-length) are also supported:

```haskell
import Data.Array (Array, listArray)

storeArray :: Connection -> IO [Array Int Int]
storeArray conn = do
  _ <- execute_ conn "CREATE TABLE arrays (vals INTEGER[3])"
  let arr = listArray (0, 2) [1, 2, 3] :: Array Int Int
  _ <- execute conn "INSERT INTO arrays VALUES (?)" (Only arr)
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
`String` stays one VARCHAR parameter. An `Array` binds as an ARRAY with the
size of the value. DuckDB casts a LIST or an ARRAY to the type of a LIST
column. It casts a LIST to an ARRAY column only when the lengths are equal.

A list or an array parameter with scalar elements takes its element type from
the column type name of the element. Some elements take the element type of
the target instead: `Integer`, `Natural`, and `BigNum` elements for an integer
or DECIMAL element type (see above), and `TimeOfDay`, `LocalTime`, and
`UTCTime` elements for a nanosecond element type (see
[Infinite dates and timestamps](#infinite-dates-and-timestamps)). An empty list
binds as an empty LIST of its element type. An empty array raises an
`SQLError`, because a DuckDB ARRAY has at least one element. Bind an empty list
for an empty LIST.

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
keys of a `Map Double`.

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

The ordinary `Day`, `LocalTime`, and `UTCTime` instances reject infinity with
a conversion error. Use `Maybe Date` to distinguish SQL NULL from infinity.
Floating-point NaN and infinities remain valid `Float` and `Double` values.

A `TimeOfDay`, a `LocalTime`, or a `UTCTime` parameter, and an `Unbounded`
form, keeps its digits below a microsecond for a nanosecond parameter:
`TIME_NS`, `TIMESTAMP_NS`, or `TIMESTAMPTZ_NS`. Use an explicit cast, such as
`SELECT ?::TIMESTAMP_NS`, or a column with that type. A `VARIANT` parameter
also gets the nanosecond value, when the value fits the range of the nanosecond
type, because a VARIANT holds `TIME_NS`, `TIMESTAMP_NS`, and `TIMESTAMPTZ_NS`
values. This holds only for a scalar value: a list, an `Array`, or a `Map` of
time values for a `VARIANT` parameter binds microsecond elements, and so does
an element of a `VARIANT[]` list or a value of a MAP with `VARIANT` values,
because one list has one element type for all its elements. Each other
parameter, and a parameter with no known type, gets the microsecond value: the
digits below a microsecond go away, as for the SQL literal. Only a value with
such digits reads the parameter type. An explicit cast such as `?::TIMESTAMP_NS` makes a
nanosecond parameter, so for a `TIMESTAMP` column DuckDB rounds the value to
the microsecond, where a plain `?` cuts the digits. Values inside collections, STRUCTs, MAPs, and UNIONs follow their
target types. The library refreshes prepared metadata when it resets or
rebinds a statement.
One `executeMany` call reads the parameter types one time. A column type that
changes during the call does not change how its later rows bind.
Use separate parameters when one expression requires microseconds and another
requires nanoseconds.

A `Day` outside the range of DuckDB, from -5877641-06-25 to 5881580-07-10, and
a `LocalTime` or a `UTCTime` outside the range from -290308-12-22 00:00:00 to
294247-01-10 04:00:54.775806, raise an `SQLError` of the type
`ErrorInputOutOfRange` when they bind. These dates are in the notation of
`Data.Time`, where the year -5877641 is 5877642 BC and the year -290308 is
290309 BC. The rule holds also inside a collection, a STRUCT, or a VARIANT,
and for a finite `Unbounded` value. DuckDB does not accept these values: its
SQL literals reject them, and a stored value can read as an infinity or make
DuckDB raise an INTERNAL error. A TIMESTAMP_S or TIMESTAMP_MS value has the
same range at both ends. A leap second, a `TimeOfDay` with 60 seconds, binds
as the next second, as `make_time` and `make_timestamp` of DuckDB give it. The
SQL literal `'23:59:60'` raises an error. A time after 24:00:00, for example
23:59:60.5, raises an `SQLError` of the type `ErrorInputInvalid`.

### Manual STRUCT, TUPLE, and UNION Handling

SQL tuples, such as `row(42, 'text')`, decode as `TupleValue`. Read their
positional fields in `tupleValueFields` and types in `tupleValueTypes`.
`FieldTuple` and `LogicalTypeTuple` preserve empty tuples independently of
empty STRUCT values. STRUCT fields retain their names in `StructValue`.
`LogicalTypeCollatedVarchar` preserves a VARCHAR field's collation in composite
parameters. Its comparisons use that collation after a round trip.

Temporal fields retain their SQL units when composite values are rebound.
The `FieldDate`, `FieldTimestamp`, and `FieldTimestampTZ` constructors hold
`Unbounded` values. Wrap finite payloads in `Finite` when constructing them.
For TIMESTAMP_S or TIMESTAMP_MS values outside the TIMESTAMP range, use an
explicit parameter cast, such as `SELECT ?::STRUCT(value TIMESTAMP_S)`.
DuckDB otherwise attempts to convert these parameters to microseconds.

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

A `FieldList`, a `FieldArray`, or a `FieldMap` that is empty or has only
`FieldNull` elements binds as the SQL list literal does: it takes the element
type of a parameter with a known type, and otherwise the type NULL, as `[]`,
`[NULL, NULL]`, and `MAP {}` have. An empty `FieldArray` raises an `SQLError`
of the type `ErrorInputInvalid`, because a DuckDB ARRAY has at least one
element. Inside a `FieldStruct`, a `FieldTuple`, or a `FieldUnion`, a
`FieldEnum` binds its label to the declared ENUM type, and a label that is not
in the dictionary raises an error. `FieldValue` has no `DuckDBColumnType` and
no `ToDuckValue` instance, because no one DuckDB type holds each of its values.

### Resource Management

- `withConnection` and `withStatement` wrap the open/close lifecycle and guard
  against exceptions; use them whenever possible to avoid leaking C handles.
- The query helpers release their results, prepared statements, and values after
  use. Query helpers return a Haskell list of all rows. Folds and cursors decode
  rows incrementally from owned native chunks. The default mode drains and
  closes the native cursor before row callbacks.
- `execute`/`query` variants reset statement bindings each run so prepared
  statements can be reused safely.

For concurrent workers, use a separate connection per worker. Threads can
also share one connection. DuckDB permits one open result for each connection,
so each connection has a lock. `query`, `execute`, the default folds, and
statement preparation hold this lock until their native result closes. A call
from another thread waits. The lock does not cover these cases. Serialize them
in the application:

- A transaction. Statements of other threads run inside it.
- One `Statement` that two threads use.
- An explicit stream from `Database.DuckDB.Simple.Streaming`. Each fetch of
  the stream holds the lock, but its result stays open between calls, and
  another query on that connection fails until the stream is complete or
  closed.
- `close`. Wait for all operations on the connection before you close it.

A callback must not execute another query on its active connection or close
that connection. With the default API, such a query waits for the lock of the
active query. After the time limit of `connectionCallbackLockWait` (10
seconds by default), it raises an `SQLError`. See
[User-Defined Functions](#user-defined-functions) for the rule and its limits.

Link your executable with `ghc-options: -threaded` to allow prompt cancellation
of native queries, including Ctrl-C. On cancellation, the library interrupts
DuckDB and waits for the native call to return before it releases resources
and propagates the exception. Cancellation is cooperative: native code and
Haskell callbacks must return before cleanup can finish.

Native work that can run for a long time runs in a helper thread, so that an
asynchronous exception can interrupt it. The default query functions run the
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
use still depends on the result size. The default interface closes the native
cursor before row delivery. A row callback can run another query on the same
connection.

```haskell
import Database.DuckDB.Simple

sumValues :: Connection -> IO Int
sumValues conn =
  fold_ conn "SELECT n FROM stream_fold ORDER BY n" 0 $ \acc (Only n) ->
    pure (acc + n)
```

For manual cursor-style iteration, use `nextRow`/`nextRowWith` on an open
`Statement` to pull rows one at a time and decide when to stop.

Cursors support the same column types as eager queries, including STRUCT
and UNION values with nested collections and NULLs. GEOMETRY and VARIANT work
with eager queries, cursors, and folds.

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

The first cursor fetch selects the execution mode. The mode stays until
`bind`, `bindNamed`, `clearStatementBindings`, or `executeStatement` releases
the result. Switching between default and streaming `nextRow` calls retains
that mode.
Keep the connection dedicated to the active stream. Until the stream is
complete, DuckDB rejects other statements on that connection with a
`RESOURCE_IN_USE` error. If you close an incomplete stream, DuckDB aborts its
statement. In autocommit mode, DuckDB rolls back the writes of that statement.
Cancellation interrupts native chunk fetching as well as execution.

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
can give shorter arrays. As in DuckDB, 0 means 131072.

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

Arrow export uses V2 Arrow results. The default interface retains owned batches
and closes the native cursor before callbacks start. Its memory use depends on
the result size. The streaming interface delivers each batch while the native
cursor remains open.

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
`TIMESTAMP_S` result binds back as a `TIMESTAMP` when its value fits. A value
outside the range of DuckDB raises an `SQLError` of the type
`ErrorInputOutOfRange`. Lists, arrays, and STRUCT fields bind as VARIANT
values. A VARIANT does not keep a UNION, a MAP, an ENUM, or the CRS of a
GEOMETRY, so these payloads raise an error at any depth: a VARIANT keeps only
the member value of a UNION, stores a MAP as a list of key and value objects,
and stores an ENUM as its text. Object parameters reject
duplicate keys and keys that contain NUL. An object with only an empty key
also raises an error because DuckDB treats its STRUCT type as a tuple.
DuckDB 2.0 permits an empty key with other named keys. You can also bind a plain
value and cast it in SQL, as in `?::VARIANT`.

A `Variant` result reads a value that a VARIANT can hold: a VARIANT column, or
a column of another type with the same shape, such as an INTEGER, a LIST, or a
GEOMETRY with no CRS. A column that holds a UNION, a MAP, an ENUM, an ARRAY, a
TUPLE, a STRUCT that is not in the form of a VARIANT object (each field of the
type VARIANT, the keys in ascending order), or a GEOMETRY with a CRS, at any
depth, raises the error of an incompatible column, because the cast of DuckDB
to VARIANT changes these values. Write `c::VARIANT` in SQL to read such a
column. One difference stays, because the reader does not know the type of a
column: a JSON column reads as its text, as a VARCHAR column does, where
`c::VARIANT` gives the parsed value.

The library uses a separate helper connection for VARIANT decoder metadata.
It caches the parsed VARIANT metadata statement and the types for VARIANT
casts. It batches VARIANT
metadata queries. It constructs VARIANT types directly through the native
factory. This preserves an active stream on the caller's connection. The
helper connection has the settings of the database, not the session settings of
the caller, so a GLOBAL `max_expression_depth` below 11 makes each VARIANT read,
and each COPY TO of a VARIANT column, fail with a Binder Error.

New DuckDB 2.0 databases use storage format 2.0, which supports VARIANT columns.
The library does not change an existing database's storage compatibility
setting. VARIANT columns require storage format `v1.5.0` or later.

The direct `query_` and `execute_` paths support statements that expand during
parsing, including dynamic PIVOT. Prepared dynamic PIVOT also supports repeated
execution and schema changes. It cannot have parameters or require a cacheable
plan. Read its schema after execution starts.

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
target with another CRS raises `SQLError`. Use `Maybe RawGeometry` for SQL
NULL. An `Array`, a list, and a typed row cannot hold a `RawGeometry`, because
their `GEOMETRY` type would have no CRS. A composite parameter holds it as
`FieldGeometry` (see below):

```haskell
import Database.DuckDB.Simple.Geometry

[Only raw] <- query_ conn "SELECT 'POINT Z (1 2 3)'::GEOMETRY('OGC:CRS84')" :: IO [Only RawGeometry]
rows <- query conn "SELECT ?" (Only raw) :: IO [Only RawGeometry]
```

You can also import the bytes with explicit SQL:

```haskell
rows <- (case rawGeometryCRS raw of
    Nothing -> query conn
        "SELECT system.main.ST_GeomFromWKB(?)"
        (Only (rawGeometryWKB raw))
    Just crs -> query conn
        "SELECT system.main.ST_SetCRS(system.main.ST_GeomFromWKB(?), ?)"
        (rawGeometryWKB raw, crs)
    ) :: IO [Only RawGeometry]
```

Use `Nothing` for no CRS and omit `ST_SetCRS` in that case. Passing SQL NULL
to `ST_SetCRS` returns a NULL geometry. CRS text can contain `OGC:CRS84`,
a custom name, or a full WKT2/PROJJSON definition. DuckDB can reduce a known
CRS definition to its registered identifier. It can also normalize WKB byte
order. The qualified function names select DuckDB's built-ins even if a user
macro has the same name.

`fromRawGeometry` decodes the shape and drops CRS metadata. `toRawGeometry`
encodes a shape with no CRS. These helpers follow the geometry-simple contracts.
NaN in both WKB point X and Y denotes an empty point. Empty multi-geometries
and collections have no stored layout tag in the decoded representation.
Keep the raw bytes when these details must survive.

Structured parameters use `encodeWKT` and DuckDB's native cast. The WKT writer combines layouts
in multi-geometries and polygon rings. It fills absent Z or M with NaN.
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

`LogicalTypeGeometry` describes CRS metadata. Composite parameters retain CRS
metadata for typed NULLs, empty collections, and inactive UNION members.
Set `connectionGeometryCRS` in `ConnectionOptions` to preserve arbitrary custom
CRS names in these parameters. The list defaults to `OGC:CRS84`. When a
connection opens, DuckDB constructs a recognized name, such as `OGC:CRS84`, and
a complete definition directly. For each other name, the library reads the
type through SQL on the new connection. The connection caches each type, and a
bind copies it. A name that DuckDB drops, for example a blank name, fails the
open. For a custom CRS in a typed registration, see the Haddock of
`connectionGeometryCRS`.

CRS definitions outside the configured list use direct V2 type construction.
This requires a recognized identifier or a complete WKT2/PROJJSON definition.
An unconfigured custom name raises an error. The binding rejects construction
that silently drops CRS metadata under `ignore_unknown_crs`. Direct low-level
`logicalTypeFromRep` calls use the same native construction rules.

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

### Feature Coverage

- Connections, prepared statements, positional/named parameter binding.
- High-level execution (`execute*`) and eager queries (`query*`, `queryNamed`).
- Cursor and fold helpers (`fold`, `foldNamed`, `fold_`, `nextRow`) that decode
  native result chunks one row at a time.
- Comprehensive scalar type support: signed/unsigned integers, HUGEINT/UHUGEINT,
  decimals (with width/scale), intervals, precise and timezone-aware temporals,
  enums, bit strings, blobs, bignums, and UUIDs.
- Composite types: STRUCTs, UNIONs, LISTs, fixed-length ARRAYs, and MAPs with
  typed parameters and results.
- Generic encoding/decoding: automatic STRUCT/UNION mapping for Haskell ADTs via
  GHC generics and the `ViaDuckDB` deriving-via helper.
- Row decoding via `FromField`/`FromRow`, with generic deriving for product types.
- User-defined scalar functions backed by Haskell functions (including IO and
  nullable arguments).
- Typed casts, aggregate functions, table functions, replacement scans, COPY
  formats, and pragmas. See [Typed registration](#typed-registration).
- Transaction helpers (`withTransaction`) and metadata accessors (`columnCount`,
  `columnName`).

## User-Defined Functions

Register a scalar Haskell function on a connection, and call it in SQL
expressions. The arguments use `FromField`, and the result uses
`ToDuckValue`. Thus a function can take and give `Maybe` values, and it can
give an `IO` action.

An argument type needs `FromField` and `DuckDBColumnType` instances. Its SQL
type is the `duckdbTypeDeclaration` of the type, as for a typed column. A
newtype with its own `DuckDBColumnType` text, for example `DECIMAL(18,3)`,
gives that text. An argument type that is or contains VARIANT raises an error
at registration, because the argument reader does not read VARIANT. A GEOMETRY
argument reads, also inside a list or a STRUCT, in a scalar function, in an
aggregate, and as the source of a typed cast. On a connection, it reads as a
GEOMETRY result does, so a registered cast does not change it. A function that
an extension registers reads it with the casts of the session, and an aggregate
function of an extension refuses a GEOMETRY argument at registration. See
[Typed registration](#typed-registration).

The SQL type of the result is the `duckdbTypeDeclaration` of the result type:
an `Int16` result is SMALLINT, and a `Float` result is FLOAT. DuckDB computes
with that type. Thus `hs_small(x) * hs_small(y)` of an `Int16` function raises
an Out of Range Error when the product does not fit in SMALLINT, as
`30000::SMALLINT * 30000::SMALLINT` does. Cast the result to a wider type
first. A `Bool`, an integer of 8 to 64 bits, a `Float`, a `Double`, a `Text`,
or a `String` result goes to the memory of the result vector. Each other
result type, for example a record or a list, costs a native value for each
row. DuckDB casts such a value to the declared type, as a SQL `CAST` does: it
rounds a value to the scale of a DECIMAL type text, and it raises an error for
a value that does not fit.

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

Exceptions raised while the function executes become `SQLError` values.
`deleteFunction` issues a `DROP FUNCTION IF EXISTS` statement and reports native
errors. DuckDB registers C API scalar functions as internal entries and rejects
their deletion. The function remains registered after this `SQLError`.
Deleting an absent name succeeds. To replace a function, register the name
again. `deleteFunction` has a GHC `WARNING` pragma, so each use causes a
compile-time warning. A build with `-Werror` must allow that warning, for
example with `-Wno-error=deprecations`.

DuckDB runs callbacks on its own threads. Each call of a callback needs a
capability of the GHC runtime. With one capability, which is the default, a
callback from a second DuckDB thread waits until the capability is free, and
the operating system switches threads for each such call. A query that makes
many callbacks on several DuckDB threads is then much slower. Give the program
as many capabilities as DuckDB has threads, for example with
`ghc-options: -threaded -rtsopts "-with-rtsopts=-N"`, or set the DuckDB setting
`threads` to 1.

A callback must not run a statement on the connection that runs the callback.
Use another connection of the same instance. This rule applies to each
registered callback, for example of a scalar function, a cast, an aggregate
function, a table function, a COPY format, a replacement scan, a pragma, or a
log sink. `query`, `execute`, the default folds, statement preparation, and
each fetch of an explicit stream hold the lock of the connection while DuckDB
runs these callbacks. The lock is not reentrant, so a statement in the callback
waits for the lock. With more than one DuckDB thread, DuckDB can also run a
callback of an explicit stream between two fetches, while no fetch holds the
lock. Then a statement in the callback on the same connection fails at once
with the `ErrorResourceInUse` error of DuckDB, because the stream has an open
result.

That wait has a time limit. `connectionCallbackLockWait` in
`ConnectionOptions` sets it in microseconds. The default is
`defaultCallbackLockWait`, 10 seconds, and `Nothing` sets no time limit. When
the time limit ends, the statement in the callback raises an `SQLError` of the
type `ErrorResourceInUse`, and its text gives the rule. If the callback does
not catch the error, DuckDB fails the outer statement with the text of that
error, and the connection can run the next statement. DuckDB ignores the error
of a log sink. The time limit applies to each wait of a callback for the lock
of a connection. It applies also to another connection that a different
statement uses. If callbacks share a connection that runs slow statements, set
a larger time limit or `Nothing` for that connection.

In these cases the time limit does not apply, and a statement in the callback can
wait and not return:

- DuckDB runs the callback during a call that does not hold the lock of the
  connection, for example a log sink for a message of `logConnectionMessage`.
  Then the statement goes into DuckDB on the same connection.
- The statement runs in a thread that the callback starts. The library marks
  only the thread of the callback.
- The statement runs inside `uninterruptibleMask`.
- The statement runs in the flush function of a log sink when DuckDB destroys
  the sink.

A callback that refers to a connection keeps that connection and its instance
reachable while DuckDB keeps the callback, possibly until the database closes.
Thus the garbage collector does not close them, and the program must close
them.

## Typed registration

These functions register SQL objects with Haskell values. Their signatures have
no native handle. An exception in a Haskell function gives an SQL error with
the message of the exception. `TRY_CAST` gives NULL instead. The examples use
these imports:

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

`createCast` registers an explicit cast. `Left` fails the cast with a message.

```haskell
castExample :: Connection -> IO [Only Int64]
castExample conn = do
  registerCustomType conn "shifted" (TypeExpression "BIGINT")
  createCast conn (TypeExpression "BIGINT") (TypeExpression "shifted") \(value :: Int64) ->
    if value > maxBound - 100 then Left "BIGINT overflow" else Right (value + 100)
  query_ conn "SELECT CAST(1::BIGINT AS shifted)::BIGINT" -- [Only 101]
```

DuckDB uses a registered cast in place of its built-in cast for the same pair
of types, on each connection of the database. On a connection, a registered
cast does not change a value that the library reads or binds. The library reads
GEOMETRY and VARIANT values with the casts of the instance, which are the
built-in casts, and it builds GEOMETRY, ENUM, and VARIANT values with the
default cast of DuckDB. For example, a cast from GEOMETRY to BLOB changes the
result of `CAST(g AS BLOB)`, but not a GEOMETRY value that a query gives. Two
places still use registered casts:

- The callbacks of a function that an extension registers. An extension has no
  instance, so these callbacks read a GEOMETRY value with the casts of the
  session: the value of an option update, the arguments of a pragma, a scalar
  function, a typed cast, or a table function, and the rows of a COPY TO
  function. The update callback of an aggregate function has no context, so an
  aggregate function of an extension refuses a GEOMETRY argument at
  registration.
- `setConfigValue` and `setConfigOption`. DuckDB casts the value to the type of
  the option, as SQL `SET` does.

`createAggregate` registers an aggregate function as a fold. The result type
must be BOOLEAN, TINYINT to BIGINT, UTINYINT to UBIGINT, FLOAT, DOUBLE, or
VARCHAR, or `Maybe` of one of them. Another result type is an error at
registration. The finalize callback of DuckDB has no factory to make a value
of another type, and in an ordered window its result vector has the size 0. A
newtype over one of these types gets the writer of that type with
`deriving newtype (FunctionResult)`.

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

`createTableFunction` registers a table function that gives its rows as a
list. DuckDB reads only the columns that the query uses, also in `WHERE` and
`ORDER BY`. For a query that uses no column, such as `SELECT count(*)`, DuckDB
reads the first column. The library does not evaluate the other columns of a
row, so an exception in such a column does not fail the query. But the library
evaluates each row that it writes to its constructor, and this evaluates each
strict field of the row (a field with `!`, or each field with `StrictData`).

```haskell
tableExample :: Connection -> IO [(Int64, Int64)]
tableExample conn = do
  createTableFunction conn "squares" ["n", "square"] \(Only count) ->
    pure [(n, n * n) | n <- [0 .. count - 1 :: Int64]]
  query_ conn "SELECT n, square FROM squares(3)" -- [(0, 0), (1, 1), (2, 4)]
```

The type of a column, a parameter, or an aggregate argument comes from
`duckdbTypeDeclaration`. A record or a sum type that derives `DuckDBColumnType`
and `ToDuckValue` via `ViaDuckDB` is a STRUCT or a UNION, also in a list or a
MAP. Each field type comes from `DuckValue`: an `Integer` field is HUGEINT and
a `Natural` field is UHUGEINT, but an `Integer` column is BIGNUM. The
registration raises an error for an `Array` field, because its ARRAY type has
no size. Use a list field. `StructValue`,
`TupleValue`, and `UnionValue` have no static field types. The registration
raises an error for them. An aggregate argument has the VARIANT and GEOMETRY
rules of [User-Defined Functions](#user-defined-functions).

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

A typed registration costs more for each row than raw vector access. Use
`Database.DuckDB.Simple.V2.Registration`, `V2.Copy`, `V2.Pragma`, and
`V2.Config` for speed, and for projection and filter pushdown, parallel table
scans, and scalar functions with a bind callback or variadic parameters.

## Tests

The test suite is built with [tasty](https://hackage.haskell.org/package/tasty)
and covers connection management, statement lifecycle, parameter binding, and
query execution.

```
cabal test duckdb-simple-test --test-show-details=direct
```
