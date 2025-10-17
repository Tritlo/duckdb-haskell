# duckdb-simple

`duckdb-simple` provides a high-level Haskell interface to DuckDB inspired by
the ergonomics of [`sqlite-simple`](https://hackage.haskell.org/package/sqlite-simple).
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
Savepoints are currently rejected by DuckDB, so `withSavepoint` raises an
`SQLError` describing the limitation.

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
  let arr = listArray (0, 2) [1, 2, 3]
  _ <- execute conn "INSERT INTO arrays VALUES (?)" (Only arr)
  fmap fromOnly <$> query_ conn "SELECT vals FROM arrays"

storeList :: Connection -> IO [[Int]]
storeList conn = do
  _ <- execute_ conn "CREATE TABLE lists (vals INTEGER[])"
  _ <- execute conn "INSERT INTO lists VALUES (?)" (Only [1, 2, 3])
  fmap fromOnly <$> query_ conn "SELECT vals FROM lists"
```

### Manual STRUCT and UNION Handling

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
  _ <- execute conn "INSERT INTO composite VALUES (?, ?)" (s, u)
  query_ conn "SELECT s, u FROM composite"
```

### Resource Management

- `withConnection` and `withStatement` wrap the open/close lifecycle and guard
  against exceptions; use them whenever possible to avoid leaking C handles.
- All intermediate DuckDB objects (results, prepared statements, values) are
  released immediately after use. Long queries still materialise their result
  sets when using the eager helpers; reach for `fold`/`fold_`/`foldNamed` (or
  the lower-level `nextRow`) to stream results in constant space.
- `execute`/`query` variants reset statement bindings each run so prepared
  statements can be reused safely.

### Metadata helpers

- `columnCount` and `columnName` expose prepared-statement metadata so you can
  inspect result shapes before executing a query.
- `rowsChanged` tracks the number of rows affected by the most recent mutation
  on a connection. DuckDB does not offer a `lastInsertRowId`; prefer SQL
  `RETURNING` clauses when you need generated identifiers.
### Streaming Results

`fold`, `fold_`, and `foldNamed` expose DuckDB’s chunked result API, letting you
aggregate or stream rows without materialising the entire result set:

```haskell
import Database.DuckDB.Simple.Types (Only (..))

sumValues :: Connection -> IO Int
sumValues conn =
  fold_ conn "SELECT n FROM stream_fold ORDER BY n" 0 $ \acc (Only n) ->
    pure (acc + n)
```

For manual cursor-style iteration, use `nextRow`/`nextRowWith` on an open
`Statement` to pull rows one at a time and decide when to stop.

### Feature Coverage

- Connections, prepared statements, positional/named parameter binding.
- High-level execution (`execute*`) and eager queries (`query*`, `queryNamed`).
- Streaming helpers (`fold`, `foldNamed`, `fold_`, `nextRow`) for constant-space
  result processing.
- Comprehensive scalar type support: signed/unsigned integers, HUGEINT/UHUGEINT,
  decimals (with width/scale), intervals, precise and timezone-aware temporals,
  enums, bit strings, blobs, bignums, and UUIDs.
- Composite types: STRUCTs, UNIONs, LISTs, fixed-length ARRAYs, and MAPs with
  full encoding/decoding support.
- Generic encoding/decoding: automatic STRUCT/UNION mapping for Haskell ADTs via
  GHC generics and the `ViaDuckDB` deriving-via helper.
- Row decoding via `FromField`/`FromRow`, with generic deriving for product types.
- User-defined scalar functions backed by Haskell functions (including IO and
  nullable arguments).
- Transaction helpers (`withTransaction`) and metadata accessors (`columnCount`,
  `columnName`, `rowsChanged`).

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
