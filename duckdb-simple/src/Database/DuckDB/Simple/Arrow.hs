{- |
Module      : Database.DuckDB.Simple.Arrow
Description : Scoped Arrow export through the DuckDB C Data Interface.

These functions execute a query and visit its Arrow batches. DuckDB materializes
the native result before the first callback. The Haskell code converts and
releases one batch at a time. Native result memory can grow with the query size.

The callback borrows the schema and array. Read them during the callback only.
Do not retain, change, release, or transfer either object. Copy data that must
outlive the callback. All native resources are released if the callback throws.

This module uses the schema and chunk conversion API. The older query and scan
functions in @Database.DuckDB.FFI.Deprecated@ are deprecated by DuckDB.
-}
module Database.DuckDB.Simple.Arrow (
    foldArrow,
    foldArrow_,
) where

import Database.DuckDB.FFI (ArrowArray, ArrowSchema)
import Database.DuckDB.Simple.Arrow.Internal (foldArrowWith)
import Database.DuckDB.Simple.Internal (Connection, Query, ResultMode (MaterializedResult))
import Database.DuckDB.Simple.ToRow (ToRow)
import Foreign.Ptr (Ptr)

{- | Execute a parameterized query and fold over borrowed Arrow batches.

The schema includes the executed result's column names and types. The callback
runs once per nonempty batch and never runs for an empty result. The schema and
array pointers are valid only during the callback. The accumulator is evaluated
to weak head normal form after each callback.

DuckDB materializes the native result before this fold starts. This function
does not provide bounded-memory query execution.
-}
foldArrow :: (ToRow q) => Connection -> Query -> q -> a -> (a -> Ptr ArrowSchema -> Ptr ArrowArray -> IO a) -> IO a
foldArrow = foldArrowWith MaterializedResult

-- | Fold over borrowed Arrow batches from a query without parameters.
foldArrow_ :: Connection -> Query -> a -> (a -> Ptr ArrowSchema -> Ptr ArrowArray -> IO a) -> IO a
foldArrow_ conn queryText = foldArrow conn queryText ()
