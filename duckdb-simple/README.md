# duckdb-simple

`duckdb-simple` provides a high-level Haskell interface to DuckDB inspired by
the APIs of [`sqlite-simple`](https://hackage.haskell.org/package/sqlite-simple) and
[`postgresql-simple`](https://hackage.haskell.org/package/postgresql-simple).
It builds on the low-level bindings exposed by [`duckdb-ffi`](../duckdb-ffi) and
provides a focused API for opening connections, running queries, binding
parameters, and decoding typed results—including the full set of DuckDB scalar
types (signed/unsigned integers, decimals, hugeints, intervals, precise and
timezone-aware temporals, blobs, enums, bit strings, and bignums).

## Getting Started

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Database.DuckDB.Simple
import Database.DuckDB.Simple.Types (Only (..))

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
  `UnionValue`) for working with DuckDB's composite types.
- `Database.DuckDB.Simple.Types` – shared types (`Query`, `Null`, `Only`,
  `(:.)`, `SQLError`).
- `Database.DuckDB.Simple.Function` – register scalar Haskell functions that
  can be invoked directly from SQL.

## Querying Data

```haskell
import Database.DuckDB.Simple
import Database.DuckDB.Simple.Types (Only (..))

fetchNames :: Connection -> IO [Maybe String]
fetchNames conn = do
  _ <- execute_ conn "CREATE TABLE names (value TEXT)"
  _ <- executeMany conn "INSERT INTO names VALUES (?)"
    [Only (Just "Alice"), Only (Nothing :: Maybe String)]
  fmap fromOnly <$> query_ conn "SELECT value FROM names ORDER BY value IS NULL, value"
```

The execution helpers return the number of affected rows (`Int`) so callers can
assert on data changes when needed.

## Named Parameters

duckdb-simple supports both positional (`?`) and named parameters. Named
parameters are bound with the `(:=)` helper exported from
`Database.DuckDB.Simple.ToField`.

```haskell
import Database.DuckDB.Simple
import Database.DuckDB.Simple.ToField (NamedParam ((:=)))

insertNamed :: Connection -> IO Int
insertNamed conn =
  executeNamed conn
    "INSERT INTO events VALUES ($kind, $payload)"
    ["$kind" := ("metric" :: String), "$payload" := ("ok" :: String)]
```

DuckDB does not allow mixing positional and named placeholders within the same
SQL statement; the library preserves DuckDB’s error message in that situation.
DuckDB does not support savepoints. This library does not provide `withSavepoint`.

If the number of supplied parameters does not match the statement’s declared
placeholders—or if you attempt to bind named arguments to a positional-only
statement—`duckdb-simple` raises a `FormatError` before executing the query.

### Decoding rows

`FromRow` is powered by a `RowParser`, which means instances can be written in a
monadic/Applicative style and even derived generically for product types:

```haskell
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

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

### Manual STRUCT and UNION Handling

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
  rows. Folds and cursors decode rows incrementally, while DuckDB retains the
  materialized native result until it is exhausted, reset, or closed.
- `execute`/`query` variants reset statement bindings each run so prepared
  statements can be reused safely.

For concurrent workers, use a separate connection per worker. A connection
can move between threads or be shared when the application serializes access.
Hold that lock for the whole transaction or cursor lifetime, including `close`.
DuckDB serializes native query calls, but this does not protect the Haskell
handle state or prevent another call from interfering with an active cursor.
Statements and connections do not provide their own lock. A callback must not
execute another query on its active connection or close that connection.

Link your executable with `ghc-options: -threaded` to allow prompt cancellation
of native queries, including Ctrl-C. On cancellation, the library interrupts
DuckDB and waits for the native call to return before it releases resources
and propagates the exception. Cancellation is cooperative: native code and
Haskell callbacks must return before cleanup can finish.

Close or reset an abandoned statement to release its result. DuckDB can retain
native result buffers until that result is destroyed.
Use `withStatement` for manual iteration. `fold` releases the result after
success or an exception. The accumulator determines Haskell memory use.

### Metadata helpers

- `columnCount` and `columnName` expose prepared-statement metadata so you can
  inspect result shapes before executing a query.
- `execute` returns the number of affected rows. Use SQL `RETURNING` clauses
  when you need generated identifiers.
### Cursors and folds

`fold`, `fold_`, and `foldNamed` decode one row at a time from DuckDB's result
chunks. DuckDB 1.5 materializes the native result before the first row is
returned. These functions avoid a complete Haskell row list, but native memory
use still depends on the result size. The native API for starting a streaming
result is deprecated; the default interface uses the supported execution API.

```haskell
import Database.DuckDB.Simple.Types (Only (..))

sumValues :: Connection -> IO Int
sumValues conn =
  fold_ conn "SELECT n FROM stream_fold ORDER BY n" 0 $ \acc (Only n) ->
    pure (acc + n)
```

For manual cursor-style iteration, use `nextRow`/`nextRowWith` on an open
`Statement` to pull rows one at a time and decide when to stop.

Cursors support the same column types as eager queries, including STRUCT
and UNION values with nested collections and NULLs. VARIANT and GEOMETRY
work with eager queries, cursors, and folds.

#### Optional native streaming

`Database.DuckDB.Simple.Deprecated.Streaming` provides `fold`, `fold_`,
`foldNamed`, `nextRow`, and `nextRowWith` with native streaming enabled.
Import it qualified:

```haskell
import qualified Database.DuckDB.Simple.Deprecated.Streaming as Streaming

streamSum :: Connection -> IO Int
streamSum conn =
  Streaming.fold_ conn "SELECT i FROM range(1000000) t(i)" 0 $ \acc (Only n) ->
    pure (acc + n)
```

The import emits a deprecation warning because DuckDB has deprecated the
execution entry point. DuckDB can still materialize some queries. Streaming
does not bound the memory used by query operators.

The first cursor fetch selects the execution mode until an explicit reset.
Switching between default and streaming `nextRow` calls retains that mode.
Keep the connection dedicated to the active stream; another query on that
connection can invalidate it. Cancellation interrupts native chunk fetching
as well as execution.

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

Arrow export uses DuckDB's schema and chunk conversion API. DuckDB materializes
the native result before callbacks start, so its memory use depends on the
result size. The older Arrow query and scan bindings remain available through
`Database.DuckDB.FFI.Deprecated` and emit deprecation warnings.

### VARIANT

A VARIANT result decodes to the `FieldValue` of the stored value, with its
native type. The usual `FromField` instances read VARIANT columns, for example
as `Int64`, `Text`, `[a]`, or a generic record. Arrays decode to `FieldList`.
Objects decode to `FieldStruct` values whose fields have the VARIANT type, in
entry order. `variantObject` builds such a payload from a list of entries.
SQL NULL decodes to `FieldNull`.

`Variant` from `Database.DuckDB.Simple.Variant` wraps a `FieldValue`. Its
`FromField` instance reads any column. Its `ToField` instance binds the
payload as a VARIANT, so `SELECT ?` returns a VARIANT:

```haskell
query conn "SELECT ?" (Only (Variant (FieldList [FieldInt8 1, FieldText "two"])))
```

A scalar payload keeps its native type, such as `TINYINT` or `DECIMAL(4,2)`.
Time and timestamp payloads bind as microsecond types, or as nanosecond types
when they have sub-microsecond digits. So a `TIMESTAMP_S` result binds back as
a `TIMESTAMP` when its value fits. Wider timestamp values use milliseconds or
seconds without losing digits. Lists, arrays, and STRUCT fields bind as
VARIANT values. MAP and ENUM payloads raise an error. Object parameters reject
duplicate keys, empty keys, and keys that contain NUL. You can also bind a
plain value and cast it in SQL, as in `?::VARIANT`.

The C API cannot create a usable VARIANT type
([#27](https://github.com/Tritlo/duckdb-haskell/issues/27)). Each connection
reads the VARIANT type the first time a parameter needs it, and its
parameters use that type. The connection reads the type with a query on a
separate connection. So the query does not run in your transaction, and it
does not end a streaming result.
`Variant` has no `ToDuckValue` instance, and `logicalTypeFromRep` raises an
error for VARIANT, because neither has a connection.

A GEOMETRY payload decodes to `FieldGeometry` with raw WKB and no CRS. Import
these bytes with `ST_GeomFromWKB(?)::VARIANT`. A `Variant` parameter that
contains `FieldGeometry` raises an error, also inside arrays and objects. A
TIMETZ payload with an offset in seconds raises an error, as a TIMETZ column
does. Results can contain empty or NUL object keys. Text and blob payloads can
contain NUL.

DuckDB 1.5 has no C API for reading VARIANT values. The decoder checks the
native version and physical schema before it reads the internal representation.
It checks payload bounds and rejects unknown tags. This format dependency is
limited to the supported DuckDB 1.5 line.

Persistent VARIANT columns require storage format `v1.5.0` or later. For a new
database, pass `[("storage_compatibility_version", "v1.5.0")]` to
`openWithConfig` or `withConnectionWithConfig`. The library does not change an
existing database's storage compatibility setting.

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

Structured parameters use `encodeWKT` and DuckDB's native cast. DuckDB 1.5
has no WKB value constructor in its C API. The WKT writer combines layouts
in multi-geometries and polygon rings. It fills absent Z or M with NaN.
DuckDB's WKT parser limits nesting to 16 levels and rejects empty polygon rings
and mixed collection layouts. Explicit WKB import preserves mixed member
layouts and native NaN payload bits that WKT cannot retain.

`FieldGeometry` contains a raw result. Generic STRUCT and UNION parameters
that contain non-NULL raw geometry raise an error. Construct those values in
SQL with explicit WKB import. Their result decoding retains WKB and CRS,
including inside LIST, ARRAY, MAP, STRUCT, and UNION values.

`LogicalTypeGeometry` describes CRS metadata. The C API cannot create a
GEOMETRY type with a CRS. So each connection reads a GEOMETRY type for each
CRS in a list, together with the VARIANT type, the first time a parameter
needs one of them. The default list holds
`OGC:CRS84`. Composite parameters use these types, so typed NULLs, empty
collections, and inactive UNION members keep their CRS. Set the list in
`ConnectionOptions`:

```haskell
let options = defaultConnectionOptions{connectionGeometryCRS = ["OGC:CRS84", "EPSG:3857"]}
withConnectionWithOptions "shapes.duckdb" options \conn -> ...
```

A list entry can be an identifier, a custom name, or a full WKT2 or PROJJSON
definition. A composite parameter with a CRS that is not in the list binds its
geometry members without a CRS. `logicalTypeFromRep` has no connection, so it
creates `GEOMETRY` without a CRS. To apply a CRS in these cases, insert the
value into a column with that CRS, or cast it in SQL:

```haskell
query conn "SELECT ?::UNION(number BIGINT, shape GEOMETRY('OGC:CRS84'))" (Only value)
```

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
- Transaction helpers (`withTransaction`) and metadata accessors (`columnCount`,
  `columnName`).

## User-Defined Functions

Scalar Haskell functions can be registered with DuckDB connections and used in
SQL expressions. Argument and result types reuse the existing `FromField` and
`FunctionResult` machinery, so `Maybe` values and `IO` actions work out of the
box.

```haskell
import Data.Int (Int64)
import Database.DuckDB.Simple
import Database.DuckDB.Simple.Function (createFunction, deleteFunction)
import Database.DuckDB.Simple.Types (Only (..))

registerAndUse :: Connection -> IO [Only Int64]
registerAndUse conn = do
  createFunction conn "hs_times_two" (\(x :: Int64) -> x * 2)
  result <- query_ conn "SELECT hs_times_two(21)" :: IO [Only Int64]
  deleteFunction conn "hs_times_two"
  pure result
```

Exceptions raised while the function executes are propagated back to DuckDB as
`SQLError` values, and `deleteFunction` issues a `DROP FUNCTION IF EXISTS`
statement to remove the registration. DuckDB registers C API scalar functions
as internal entries; attempting to drop them this way will yield an error, which
the library surfaces as an `SQLError`.

## Tests

The test suite is built with [tasty](https://hackage.haskell.org/package/tasty)
and covers connection management, statement lifecycle, parameter binding, and
query execution.

```
cabal test duckdb-simple-test --test-show-details=direct
```
