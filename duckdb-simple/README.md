# duckdb-simple

`duckdb-simple` provides a high-level Haskell interface to DuckDB inspired by
the APIs of [`sqlite-simple`](https://hackage.haskell.org/package/sqlite-simple) and
[`postgresql-simple`](https://hackage.haskell.org/package/postgresql-simple).
It builds on the low-level bindings exposed by [`duckdb-ffi`](../duckdb-ffi) and
provides a focused API for opening connections, running queries, binding
parameters, and decoding typed results—including the full set of DuckDB scalar
types (signed/unsigned integers, decimals, hugeints, intervals, precise and
timezone-aware temporals, blobs, enums, bit strings, and bignums).

The backend uses the generated DuckDB V2 API. Upstream marks the complete V2
API as unstable: "The C API v2 is not yet stable. Its interface may change in
upcoming releases." This release therefore requires exactly DuckDB 2.0.0.
Creating an environment raises `SQLError` for another library version.

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
collation lookup. Fixed ARRAY types with zero elements are
not supported by this native API. Empty LIST values remain supported.
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
  can be invoked directly from SQL, and casts, aggregate functions, and table
  functions.
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

`query_` and `execute_` parse all input statements before execution. A syntax
error prevents every statement in that input from running. An execution error
can leave effects from earlier statements. Use `withTransaction` when those
effects must roll back together.

## Named Parameters

duckdb-simple supports both positional (`?`) and named parameters. Named
parameters are bound with the `(:=)` helper exported from
`Database.DuckDB.Simple`.

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
cases: the number of positional parameters does not match the placeholders,
you supply more named parameters than the statement declares, or you bind named
arguments to a statement with only positional placeholders.

### Decoding rows

`FromRow` is powered by a `RowParser`, which means instances can be written in a
monadic/Applicative style and even derived generically for product types:

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
    \Rectangle STRUCT(field1 DOUBLE, field2 DOUBLE), Point STRUCT()))"
  _ <- execute conn "INSERT INTO shapes VALUES (?)" (Only shape)
  fmap fromOnly <$> query_ conn "SELECT s FROM shapes"
```

Nullary constructors (like `Point`) are encoded with a null payload.
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
  let arr = listArray (0, 2) [1, 2, 3] :: Array Int Int
  _ <- execute conn "INSERT INTO lists VALUES (?)" (Only arr)
  fmap fromOnly <$> query_ conn "SELECT vals FROM lists"
```

A Haskell list reads a LIST result. Lists have no parameter instance, so bind
an `Array`. DuckDB casts the array to the LIST column type.

An array parameter with scalar elements takes its element type from the
column type name of the element, so an empty array keeps its type. An array
of STRUCT values, UNION values, generic records, or arrays takes the type of
its first element that is not NULL. All other non-NULL elements must have the
same type, including field names, decimal precision and scale, and nested
types. Different types raise an error before DuckDB can cast away fields or
round values. An empty or all-NULL array of such elements raises an error.

### Infinite dates and timestamps

Use `Database.DuckDB.Simple.Time` when a column can contain temporal infinity.
Its `Date`, `LocalTimestamp`, and `UTCTimestamp` types wrap `Day`, `LocalTime`,
and `UTCTime` in `Unbounded`: `NegInfinity`, `Finite value`, or `PosInfinity`.
These types support parameters, results, and fields in generic composites.
`UTCTimestamp` binds as TIMESTAMPTZ.

```haskell
import Database.DuckDB.Simple.Time

infiniteDates :: Connection -> IO [Only Date]
infiniteDates conn = query conn "SELECT ?::DATE" (Only (PosInfinity :: Date))
```

The ordinary `Day`, `LocalTime`, and `UTCTime` instances reject infinity with
a conversion error. Use `Maybe Date` to distinguish SQL NULL from infinity.
Floating-point NaN and infinities remain valid `Float` and `Double` values.

`UTCTime` and `UTCTimestamp` parameters preserve
nanoseconds when the prepared parameter type is `TIMESTAMPTZ_NS`. Use an
explicit cast, such as `SELECT ?::TIMESTAMPTZ_NS`, or a column with that type.
UTC values inside collections, STRUCTs, MAPs, and UNIONs follow their target
types. An untyped UTC parameter still binds as microsecond TIMESTAMPTZ.
The library refreshes prepared metadata when it resets or rebinds a statement.
Use separate parameters when one expression requires microseconds and another
requires nanoseconds.

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

### Resource Management

- `withConnection` and `withStatement` wrap the open/close lifecycle and guard
  against exceptions; use them whenever possible to avoid leaking C handles.
- All intermediate DuckDB objects (results, prepared statements, values) are
  released immediately after use. Query helpers return a Haskell list of all
  rows. Folds and cursors decode rows incrementally from owned native chunks.
  The default mode drains and closes the native cursor before row callbacks.
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
active query. After the bound of `connectionCallbackLockWait` (10 seconds by
default), it raises an `SQLError`. See
[User-Defined Functions](#user-defined-functions) for the rule and its limits.

Link your executable with `ghc-options: -threaded` to allow prompt cancellation
of native queries, including Ctrl-C. On cancellation, the library interrupts
DuckDB and waits for the native call to return before it releases resources
and propagates the exception. Cancellation is cooperative: native code and
Haskell callbacks must return before cleanup can finish.

Native work that can run for a long time runs in a helper thread, so that an
asynchronous exception can interrupt it. The default query functions run the
native calls of one result in one helper thread. A bound thread, such as the
main thread, changes operating system threads to wait for a helper thread. A
small query then takes about 1.2 to 1.3 times as long as from an unbound
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
when they have sub-microsecond digits. So a `TIMESTAMP_S` result binds back as
a `TIMESTAMP` when its value fits. Wider timestamp values use milliseconds or
seconds without losing digits. Lists, arrays, and STRUCT fields bind as
VARIANT values. MAP and ENUM payloads raise an error. Object parameters reject
duplicate keys and keys that contain NUL. An object with only an empty key
also raises an error because DuckDB treats its STRUCT type as a tuple.
DuckDB 2.0 permits an empty key with other named keys. You can also bind a plain
value and cast it in SQL, as in `?::VARIANT`.

The library uses a separate helper connection for VARIANT decoder metadata.
It caches the parsed VARIANT metadata statement and the types for VARIANT
casts. It batches VARIANT
metadata queries. It constructs VARIANT types directly through the native
factory. This preserves an active stream on the caller's connection. Low-level
`ToDuckValue.toDuckValue` takes a `ValueFactory`, which is the V2 factory
handle, and `logicalTypeFromRep` takes the same handle.
`Database.DuckDB.Simple.V2.LogicalRep` has `logicalTypeFromRep`.
Get the factory of a connection with `withConnectionHandle` and
`withConnectionFactory` from `Database.DuckDB.Simple.Internal`. Keep owned
values and logical types within the lifetime of the factory's owner.

Keep the connection dedicated to the active stream; another query on that
connection receives a native resource-in-use error. Cancellation interrupts native chunk fetching
as well as execution.

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
supplies its parameter and result instances:

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

`RawGeometry` has no `ToField` instance. Import its bytes explicitly:

```haskell
import Database.DuckDB.Simple.Geometry

[Only raw] <- query_ conn "SELECT 'POINT Z (1 2 3)'::GEOMETRY('OGC:CRS84')"
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

`FieldGeometry` contains a raw result. Generic STRUCT and UNION parameters
that contain non-NULL raw geometry raise an error. Construct those values in
SQL with explicit WKB import. Their result decoding retains WKB and CRS,
including inside LIST, ARRAY, MAP, STRUCT, and UNION values.

`LogicalTypeGeometry` describes CRS metadata. Composite parameters retain CRS
metadata for typed NULLs, empty collections, and inactive UNION members.
Set `connectionGeometryCRS` in `ConnectionOptions` to preserve arbitrary custom
CRS names in these parameters. The list defaults to `OGC:CRS84`. When a
connection opens, DuckDB constructs a recognized name, such as `OGC:CRS84`, and
a complete definition directly. For each other name, the library reads the
type through SQL on the new connection. The connection caches each type, and a
bind copies it. A name that DuckDB drops, for example a blank name, fails the
open.

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

Scalar Haskell functions can be registered with DuckDB connections and used in
SQL expressions. Argument and result types reuse the existing `FromField` and
`FunctionResult` machinery, so `Maybe` values and `IO` actions work out of the
box.

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

That wait has a bound. `connectionCallbackLockWait` in `ConnectionOptions`
sets it in microseconds. The default is `defaultCallbackLockWait`, 10 seconds,
and `Nothing` sets no bound. When the bound ends, the statement in the callback
raises an `SQLError` of the type `ErrorResourceInUse`, and its text gives the
rule. If the callback does not catch the error, DuckDB fails the outer
statement with the text of that error, and the connection can run the next
statement. DuckDB ignores the error of a log sink. The bound applies to each wait of a
callback for the lock of a connection. It applies also to another connection
that a different statement uses. If callbacks share a connection that runs slow
statements, set a larger bound or `Nothing` for that connection.

In these cases the bound does not apply, and a statement in the callback can
wait and not return:

- DuckDB runs the callback during a call that does not hold the lock of the
  connection, for example a log sink for a message of `logConnectionMessage`.
  Then the statement goes into DuckDB on the same connection.
- The statement runs in a thread that the callback starts. The library marks
  only the thread of the callback.
- The callback changes the label of its own thread with `labelThread`. The mark
  of the library is the label of the thread.
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

`createAggregate` registers an aggregate function as a fold. The result type
is a result type of `createFunction`.

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

`createTableFunction` registers a table function that gives its rows as a
list. DuckDB reads all columns of each row, also the columns that the query
does not use.

```haskell
tableExample :: Connection -> IO [(Int64, Int64)]
tableExample conn = do
  createTableFunction conn "squares" ["n", "square"] \(Only count) ->
    pure [(n, n * n) | n <- [0 .. count - 1 :: Int64]]
  query_ conn "SELECT n, square FROM squares(3)" -- [(0, 0), (1, 1), (2, 4)]
```

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
