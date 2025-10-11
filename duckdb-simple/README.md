# duckdb-simple

`duckdb-simple` provides a high-level Haskell interface to DuckDB inspired by
the ergonomics of [`sqlite-simple`](https://hackage.haskell.org/package/sqlite-simple).
It builds on the low-level bindings exposed by [`duckdb-ffi`](../duckdb-ffi) and
provides a small, focused API for opening connections, running queries, binding
parameters, and decoding typed results.

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

- `Database.DuckDB.Simple` – primary API: connections, statements, execution,
  queries, and error handling.
- `Database.DuckDB.Simple.ToField` / `ToRow` – typeclasses for preparing
  parameters that can be passed to `execute`/`query`.
- `Database.DuckDB.Simple.FromField` / `FromRow` – typeclasses for decoding
  query results returned by `query`/`query_`.
- `Database.DuckDB.Simple.Types` – common utility types (`Query`, `Null`,
  `Only`, `SQLError`).
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
import Database.DuckDB.Simple (NamedParam ((:=)))

insertNamed :: Connection -> IO Int
insertNamed conn =
  executeNamed conn
    "INSERT INTO events VALUES ($kind, $payload)"
    ["$kind" := ("metric" :: String), "$payload" := ("ok" :: String)]
```

DuckDB currently does not allow mixing positional and named placeholders within
the same SQL statement; the library preserves DuckDB’s error message in that
situation. Savepoints are also unavailable in DuckDB at the moment, so
`withSavepoint` throws an `SQLError` detailing the limitation.

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

### Resource Management

- `withConnection` and `withStatement` wrap the open/close lifecycle and guard
  against exceptions; use them whenever possible to avoid leaking C handles.
- All intermediate DuckDB objects (results, prepared statements, values) are
  released immediately after use. Long queries still materialise their result
  sets when using the eager helpers; reach for `fold`/`fold_`/`foldNamed` (or
  the lower-level `nextRow`) to stream results in constant space.
- `execute`/`query` variants reset statement bindings each run so prepared
  statements can be reused safely.

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

### Feature Coverage & Roadmap

Included today:

- Connections, prepared statements, positional/named parameter binding.
- High-level execution (`execute*`) and eager queries (`query*`, `queryNamed`).
- Streaming/folding helpers (`fold`, `foldNamed`, `fold_`, `nextRow`).
- Row decoding via `FromField`/`FromRow`, including generic deriving support.
- Basic transaction helpers (`withTransaction`, `withSavepoint` fallback).

Planned for a future release:

- Broader type coverage (temporal, structured, and vector types).
- Metadata helpers such as `columnName`/`columnCount`.
- Native savepoints once DuckDB exposes the required support.

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
statement to remove the registration. Current DuckDB releases mark C API
registrations as internal, so the drop operation reports an error instead of
removing the function; duckdb-simple surfaces that limitation as an
`SQLError`.

## Tests

The test suite is built with [tasty](https://hackage.haskell.org/package/tasty)
and covers connection management, statement lifecycle, parameter binding, and
query execution.

```
cabal test duckdb-simple-test --test-show-details=direct
```
