{-# LANGUAGE BlockArguments #-}

{- |
Module      : Database.DuckDB.Simple.Deprecated.Streaming
Description : Optional native streaming through DuckDB's deprecated execution API.

Import this module qualified to request native streaming. DuckDB can still
materialize a result, and operators such as sorting can buffer data. Streaming
reduces result storage when the query supports it; it does not bound all query
memory. The default @Database.DuckDB.Simple@ API materializes native results.

Rows and Arrow batches use the same decoders and cleanup as the default API.
Cancellation also interrupts native chunk fetching. Prompt interruption requires
@-threaded@ and waits for native code and Haskell callbacks to return.

Serialize connection use for the entire cursor or fold. Another query on the
same connection can invalidate an active stream. Close or reset an abandoned
statement to release its result.
-}
module Database.DuckDB.Simple.Deprecated.Streaming
    {-# DEPRECATED "Uses DuckDB's deprecated streaming execution API. Prefer Database.DuckDB.Simple or Database.DuckDB.Simple.Arrow when materialization is acceptable." #-} (
    fold,
    fold_,
    foldNamed,
    nextRow,
    nextRowWith,
    foldArrow,
    foldArrow_,
) where

import Database.DuckDB.FFI (ArrowArray, ArrowSchema)
import Database.DuckDB.Simple (bind, bindNamed, withStatement)
import qualified Database.DuckDB.Simple.Arrow.Internal as Arrow
import Database.DuckDB.Simple.FromRow (FromRow (..), RowParser)
import Database.DuckDB.Simple.Internal (Connection, Query, ResultMode (StreamingResult), Statement)
import qualified Database.DuckDB.Simple.Result as Result
import Database.DuckDB.Simple.ToField (NamedParam)
import Database.DuckDB.Simple.ToRow (ToRow (..))
import Foreign.Ptr (Ptr)

-- | Fold a parameterized query, requesting native streaming.
fold :: (FromRow row, ToRow params) => Connection -> Query -> params -> a -> (a -> row -> IO a) -> IO a
fold conn sql params initial step =
    withStatement conn sql \stmt -> do
        bind stmt (toRow params)
        Result.foldStatementWith StreamingResult fromRow stmt initial step

-- | Fold a query without parameters, requesting native streaming.
fold_ :: (FromRow row) => Connection -> Query -> a -> (a -> row -> IO a) -> IO a
fold_ conn sql initial step =
    withStatement conn sql \stmt ->
        Result.foldStatementWith StreamingResult fromRow stmt initial step

-- | Fold a query with named parameters, requesting native streaming.
foldNamed :: (FromRow row) => Connection -> Query -> [NamedParam] -> a -> (a -> row -> IO a) -> IO a
foldNamed conn sql params initial step =
    withStatement conn sql \stmt -> do
        bindNamed stmt params
        Result.foldStatementWith StreamingResult fromRow stmt initial step

-- | Fetch the next row, requesting native streaming on the first fetch.
nextRow :: (FromRow r) => Statement -> IO (Maybe r)
nextRow = nextRowWith fromRow

{- | Fetch a row with a custom parser, requesting streaming on the first fetch.
  The active result retains its execution mode until reset. Switching between
  this module and the default @Database.DuckDB.Simple.nextRow@ does not restart
  or change an active result. EOF remains exhausted until an explicit reset.
-}
nextRowWith :: RowParser r -> Statement -> IO (Maybe r)
nextRowWith = Result.nextRowWith StreamingResult

{- | Fold Arrow batches, requesting native streaming.
  Each batch has a separate schema and array. The callback may read, release,
  or move them under the Arrow C Data Interface. The pointers themselves are
  valid during the callback only. See @Database.DuckDB.Simple.Arrow@ for the
  ownership rules.
  Arrow conversion uses the supported API; query execution uses the deprecated
  streaming entry point. The fold releases contents that the consumer has not
  released or moved. The consumer owns any moved contents.
-}
foldArrow :: (ToRow params) => Connection -> Query -> params -> a -> (a -> Ptr ArrowSchema -> Ptr ArrowArray -> IO a) -> IO a
foldArrow = Arrow.foldArrowWith StreamingResult

-- | Fold Arrow batches from a query without parameters.
foldArrow_ :: Connection -> Query -> a -> (a -> Ptr ArrowSchema -> Ptr ArrowArray -> IO a) -> IO a
foldArrow_ conn sql = foldArrow conn sql ()
