{- |
Module      : Database.DuckDB.Simple.Arrow
Description : Scoped Arrow export through the DuckDB C Data Interface.

These functions execute a query and visit its Arrow batches. DuckDB materializes
the native result before the first callback. The Haskell code converts and
releases one batch at a time. Native result memory can grow with the query size.

Each callback receives a separate schema and array. It may read them or pass
them to a consumer that releases or moves them under the Arrow C Data Interface.
The fold releases objects that the consumer has not released or moved, including
when the callback throws. The consumer must release any contents it moves.

The pointers themselves are valid during the callback only. To retain contents,
the consumer must move the root structs into its own storage and set the source
release fields to NULL. Do not retain the original pointers or move children
separately. Moved contents remain valid after the query and connection close.
Haskell consumers must mask asynchronous exceptions while moving a root and
register its cleanup before restoring exceptions.

This module uses the schema and chunk conversion API. The older query and scan
functions in @Database.DuckDB.FFI@ are deprecated by DuckDB.
-}
module Database.DuckDB.Simple.Arrow (
    foldArrow,
    foldArrow_,
    releaseArrowSchema,
    releaseArrowArray,
    releaseArrowStream,
) where

import Database.DuckDB.FFI.Compat (ArrowArray, ArrowSchema)
import Database.DuckDB.Simple.Arrow.Internal (foldArrowWith, releaseArrowArray, releaseArrowSchema, releaseArrowStream)
import Database.DuckDB.Simple.Internal (Connection, Query, ResultMode (MaterializedResult))
import Database.DuckDB.Simple.ToRow (ToRow)
import Foreign.Ptr (Ptr)

{- | Execute a parameterized query and fold over Arrow batches.

The schema includes the executed result's column names and types. The callback
runs once per nonempty batch and never runs for an empty result. The schema and
array pointers are valid only during the callback. A consumer may release or
move their contents as described in the module documentation. The accumulator
is evaluated to weak head normal form after each callback.

DuckDB materializes the native result before this fold starts. This function
does not provide bounded-memory query execution.
-}
foldArrow :: (ToRow q) => Connection -> Query -> q -> a -> (a -> Ptr ArrowSchema -> Ptr ArrowArray -> IO a) -> IO a
foldArrow = foldArrowWith MaterializedResult

-- | Fold over Arrow batches from a query without parameters.
foldArrow_ :: Connection -> Query -> a -> (a -> Ptr ArrowSchema -> Ptr ArrowArray -> IO a) -> IO a
foldArrow_ conn queryText = foldArrow conn queryText ()
