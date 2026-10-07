{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

{- |
Module      : Database.DuckDB.Simple.Internal
Description : Internal machinery backing the duckdb-simple API surface.

This module provides access to the opaque data constructors and helper
utilities required by the high-level API.  It is not part of the supported
public interface; consumers should depend on @Database.DuckDB.Simple@ instead.
-}
module Database.DuckDB.Simple.Internal (
    -- * Data constructors (internal use only)
    Query (..),
    Connection (..),
    ConnectionState (..),
    Statement (..),
    StatementState (..),
    StatementStreamState (..),
    StatementStream (..),
    ResultMode (..),
    StatementStreamColumn (..),
    StatementStreamChunk (..),
    SQLError (..),
    toSQLError,

    -- * Helpers
    connectionClosedError,
    statementClosedError,
    keepAlive,
    withDatabaseHandle,
    withConnectionHandle,
    withStatementHandle,
    withQueryCString,
    peekUtf8CString,
    fetchPrepareError,
    duckDBTypeFromName,
    duckDBTypeToName,
    withResult,
    runInterruptibleQuery,
    executePreparedResult,
    fetchResultChunk,
    destroyDataChunk,
    fetchResultError,
    throwResultError,
    mkExecuteError,
    withClientContext,
    destroyClientContext,
    destroyValue,
    destroyLogicalType,
    throwRegistrationError,
) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, readMVar, threadDelay, tryReadMVar)
import Control.Exception (Exception, SomeException, bracket, bracket_, mask, mask_, onException, throwIO, try, uninterruptibleMask_)
import Control.Monad (when)
import qualified Data.ByteString as BS
import Data.IORef (IORef, readIORef)
import Data.List (find)
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.Foreign as TextForeign
import Database.DuckDB.FFI (
    DuckDBClientContext,
    DuckDBConnection,
    DuckDBDataChunk,
    DuckDBDatabase,
    DuckDBErrorType,
    DuckDBPreparedStatement,
    DuckDBResult,
    DuckDBState,
    DuckDBType,
    DuckDBValue,
    c_duckdb_connection_get_client_context,
    c_duckdb_destroy_client_context,
    c_duckdb_destroy_data_chunk,
    c_duckdb_destroy_result,
    c_duckdb_destroy_value,
    c_duckdb_execute_prepared,
    c_duckdb_fetch_chunk,
    c_duckdb_interrupt,
    c_duckdb_prepare_error,
    c_duckdb_result_error,
    c_duckdb_result_error_type,
    pattern DuckDBErrorInvalid,
    pattern DuckDBSuccess,
 )
import qualified Database.DuckDB.FFI as FFI
import Database.DuckDB.FFI.Deprecated (c_duckdb_execute_prepared_streaming)
import Database.DuckDB.Simple.FromField (FieldValue)
import Database.DuckDB.Simple.LogicalRep (destroyLogicalType)
import Foreign.C.String (CString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek, poke, sizeOf)
import GHC.Exts (keepAlive#)
import GHC.IO (IO (..))

-- | Represents a textual SQL query with UTF-8 encoding semantics.
newtype Query = Query
    { fromQuery :: Text
    -- ^ Extract the underlying textual representation of the query.
    }
    deriving stock (Eq, Ord, Show)

instance Semigroup Query where
    Query a <> Query b = Query (a <> b)

instance IsString Query where
    fromString = Query . Text.pack

-- | Tracks the lifetime of a DuckDB database and connection pair.
newtype Connection = Connection {connectionState :: IORef ConnectionState}

-- | Internal connection lifecycle state.
data ConnectionState
    = ConnectionClosed
    | ConnectionOpen
        { connectionDatabase :: DuckDBDatabase
        , connectionHandle :: DuckDBConnection
        }

-- | Tracks the lifetime of a prepared statement.
data Statement = Statement
    { statementState :: IORef StatementState
    , statementConnection :: Connection
    , statementQuery :: Query
    , statementStream :: IORef StatementStreamState
    }

-- | Internal statement lifecycle state.
data StatementState
    = StatementClosed
    | StatementOpen
        { statementHandle :: DuckDBPreparedStatement
        }

-- | Streaming execution state for prepared statements.
data StatementStreamState
    = StatementStreamIdle
    | StatementStreamExhausted
    | StatementStreamActive !StatementStream

-- | Streaming cursor backing an active result set.
data StatementStream = StatementStream
    { statementStreamResult :: Ptr DuckDBResult
    , statementStreamColumns :: [StatementStreamColumn]
    , statementStreamChunk :: Maybe StatementStreamChunk
    , statementStreamMode :: ResultMode
    }

-- | Select native execution. DuckDB can materialize a streaming request.
data ResultMode = MaterializedResult | StreamingResult

-- | Metadata describing a result column surfaced through streaming.
data StatementStreamColumn = StatementStreamColumn
    { statementStreamColumnIndex :: Int
    , statementStreamColumnName :: Text
    , statementStreamColumnType :: DuckDBType
    }

-- | Currently loaded data chunk plus iteration cursor.
data StatementStreamChunk = StatementStreamChunk
    { statementStreamChunkPtr :: DuckDBDataChunk
    , statementStreamChunkSize :: Int
    , statementStreamChunkIndex :: Int
    , statementStreamChunkReaders :: [Int -> IO FieldValue]
    -- ^ One reader for each column. A reader must not outlive its chunk.
    }

-- | Represents an error reported by DuckDB or by duckdb-simple itself.
data SQLError = SQLError
    { sqlErrorMessage :: Text
    , sqlErrorType :: Maybe DuckDBErrorType
    , sqlErrorQuery :: Maybe Query
    }
    deriving stock (Eq, Show)

instance Exception SQLError

-- | Convert an arbitrary exception into an untyped @SQLError@.
toSQLError :: (Exception e) => e -> SQLError
toSQLError ex =
    SQLError
        { sqlErrorMessage = Text.pack (show ex)
        , sqlErrorType = Nothing
        , sqlErrorQuery = Nothing
        }

-- | Shared error value used when an operation targets a closed connection.
connectionClosedError :: SQLError
connectionClosedError =
    SQLError
        { sqlErrorMessage = Text.pack "duckdb-simple: connection is closed"
        , sqlErrorType = Nothing
        , sqlErrorQuery = Nothing
        }

-- | Shared error value used when an operation targets a closed statement.
statementClosedError :: Statement -> SQLError
statementClosedError Statement{statementQuery} =
    SQLError
        { sqlErrorMessage = Text.pack "duckdb-simple: statement is closed"
        , sqlErrorType = Nothing
        , sqlErrorQuery = Just statementQuery
        }

-- | Provide a UTF-8 encoded C string view of the query text.
withQueryCString :: Query -> (CString -> IO a) -> IO a
withQueryCString query@(Query txt) action
    | Text.any (== '\0') txt =
        throwIO (SQLError (Text.pack "duckdb-simple: SQL contains NUL") Nothing (Just query))
    | otherwise = TextForeign.withCString txt action

-- | Copy a NUL-terminated UTF-8 string from DuckDB.
peekUtf8CString :: CString -> IO Text
peekUtf8CString ptr = TextEncoding.decodeUtf8 <$> BS.packCString ptr

-- | Execute a query and destroy its result after success or failure.
withResult :: Connection -> Query -> (Ptr DuckDBResult -> IO DuckDBState) -> (Ptr DuckDBResult -> IO a) -> IO a
withResult conn queryText executeResult action =
    alloca $ \resPtr ->
        bracket_
            (fillBytes resPtr 0 (sizeOf (undefined :: DuckDBResult)))
            (c_duckdb_destroy_result resPtr)
            $ do
                rc <- runInterruptibleQuery conn (executeResult resPtr)
                when (rc /= DuckDBSuccess) $ do
                    (message, errorType) <- fetchResultError resPtr
                    throwIO (mkExecuteError queryText message errorType)
                action resPtr

-- | Execute a prepared statement with the requested native result mode.
executePreparedResult :: ResultMode -> DuckDBPreparedStatement -> Ptr DuckDBResult -> IO DuckDBState
executePreparedResult MaterializedResult = c_duckdb_execute_prepared
executePreparedResult StreamingResult = c_duckdb_execute_prepared_streaming

{- | Fetch an owned chunk. The caller must remain masked until it records ownership.
  Streaming fetches can execute SQL. Keep their output in a caller-owned slot
  so cancellation can destroy a chunk that the worker has already fetched.
-}
fetchResultChunk :: ResultMode -> Connection -> Ptr DuckDBResult -> IO DuckDBDataChunk
fetchResultChunk MaterializedResult conn result =
    withConnectionHandle conn $ \_ -> c_duckdb_fetch_chunk result
fetchResultChunk StreamingResult conn result =
    alloca $ \chunkPtr -> mask_ $ do
        poke chunkPtr nullPtr
        let fetch = do
                chunk <- c_duckdb_fetch_chunk result
                poke chunkPtr chunk
                pure DuckDBSuccess
        (runInterruptibleQuery conn fetch >> peek chunkPtr)
            `onException` c_duckdb_destroy_data_chunk chunkPtr

-- | Destroy an owned native data chunk. A null chunk is allowed.
destroyDataChunk :: DuckDBDataChunk -> IO ()
destroyDataChunk chunk = alloca $ \ptr -> poke ptr chunk >> c_duckdb_destroy_data_chunk ptr

{- | Run a native query while the caller can receive asynchronous exceptions.
  Prompt cancellation requires the threaded RTS. Cleanup interrupts DuckDB and
  waits for the worker before the caller can release native storage.
-}
runInterruptibleQuery :: Connection -> IO DuckDBState -> IO DuckDBState
runInterruptibleQuery conn action =
    withConnectionHandle conn $ \handle ->
        mask $ \restore -> do
            done <- newEmptyMVar
            _ <- forkIO $ do
                outcome <- try action :: IO (Either SomeException DuckDBState)
                putMVar done outcome
            let cancelAndWait = do
                    finished <- tryReadMVar done
                    case finished of
                        Just _ -> pure ()
                        Nothing -> do
                            -- DuckDB clears the interrupt flag at query entry.
                            -- Repeat the interrupt to cover cancellation before entry.
                            c_duckdb_interrupt handle
                            threadDelay interruptRetryDelayMicros
                            cancelAndWait
            -- A second exception must not release storage still used by the worker.
            -- Native code and Haskell callbacks must return before cleanup can finish.
            outcome <- restore (readMVar done) `onException` uninterruptibleMask_ cancelAndWait
            either throwIO pure outcome

{- | Retry cancellation at most 100 times per second while native work finishes.
DuckDB can clear an earlier interrupt at query entry. This interval avoids
busy-spinning and can add up to one interval to completion detection.
-}
interruptRetryDelayMicros :: Int
interruptRetryDelayMicros = 10 * 1000

-- | Copy a result error while its native result remains alive.

{- | The column type names that 'Database.DuckDB.Simple.ToField.DuckDBColumnType'
instances use, and the types they denote. Each type has one name.
-}
duckDBTypeNames :: [(Text, DuckDBType)]
duckDBTypeNames =
    map
        (\(name, dtype) -> (Text.pack name, dtype))
        [ ("BOOLEAN", FFI.DuckDBTypeBoolean)
        , ("TINYINT", FFI.DuckDBTypeTinyInt)
        , ("SMALLINT", FFI.DuckDBTypeSmallInt)
        , ("INTEGER", FFI.DuckDBTypeInteger)
        , ("BIGINT", FFI.DuckDBTypeBigInt)
        , ("HUGEINT", FFI.DuckDBTypeHugeInt)
        , ("UTINYINT", FFI.DuckDBTypeUTinyInt)
        , ("USMALLINT", FFI.DuckDBTypeUSmallInt)
        , ("UINTEGER", FFI.DuckDBTypeUInteger)
        , ("UBIGINT", FFI.DuckDBTypeUBigInt)
        , ("UHUGEINT", FFI.DuckDBTypeUHugeInt)
        , ("FLOAT", FFI.DuckDBTypeFloat)
        , ("DOUBLE", FFI.DuckDBTypeDouble)
        , ("DATE", FFI.DuckDBTypeDate)
        , ("TIME", FFI.DuckDBTypeTime)
        , ("TIMETZ", FFI.DuckDBTypeTimeTz)
        , ("TIMESTAMP", FFI.DuckDBTypeTimestamp)
        , ("TIMESTAMPTZ", FFI.DuckDBTypeTimestampTz)
        , ("INTERVAL", FFI.DuckDBTypeInterval)
        , ("TEXT", FFI.DuckDBTypeVarchar)
        , ("BLOB", FFI.DuckDBTypeBlob)
        , ("GEOMETRY", FFI.DuckDBTypeGeometry)
        , ("UUID", FFI.DuckDBTypeUUID)
        , ("BIT", FFI.DuckDBTypeBit)
        , ("BIGNUM", FFI.DuckDBTypeBigNum)
        , -- NULL gives an element type to Maybe values without data.
          ("NULL", FFI.DuckDBTypeSQLNull)
        ]

-- | Find the type that a column type name denotes.
duckDBTypeFromName :: Text -> Maybe DuckDBType
duckDBTypeFromName name = lookup name duckDBTypeNames

-- | Find the column type name of a type. Other types use their 'Show' text.
duckDBTypeToName :: DuckDBType -> Text
duckDBTypeToName dtype =
    maybe (Text.pack (show dtype)) fst (find ((== dtype) . snd) duckDBTypeNames)

{- | Read the error message of a prepared statement as UTF-8. Use the fallback
when DuckDB reports no message.
-}
fetchPrepareError :: Text -> DuckDBPreparedStatement -> IO Text
fetchPrepareError fallback statement = do
    messagePtr <- c_duckdb_prepare_error statement
    if messagePtr == nullPtr then pure fallback else peekUtf8CString messagePtr

fetchResultError :: Ptr DuckDBResult -> IO (Text, Maybe DuckDBErrorType)
fetchResultError resultPtr = do
    msgPtr <- c_duckdb_result_error resultPtr
    message <-
        if msgPtr == nullPtr
            then pure (Text.pack "duckdb-simple: query failed")
            else peekUtf8CString msgPtr
    errorType <- c_duckdb_result_error_type resultPtr
    pure (message, if errorType == DuckDBErrorInvalid then Nothing else Just errorType)

-- | Attach the query and native error category to an execution error.
mkExecuteError :: Query -> Text -> Maybe DuckDBErrorType -> SQLError
mkExecuteError queryText message errorType =
    SQLError
        { sqlErrorMessage = message
        , sqlErrorType = errorType
        , sqlErrorQuery = Just queryText
        }

-- | Report a fetch failure before treating a null chunk as end of input.
throwResultError :: Query -> Ptr DuckDBResult -> IO ()
throwResultError queryText resPtr = do
    errorPtr <- c_duckdb_result_error resPtr
    when (errorPtr /= nullPtr) $ do
        (message, errorType) <- fetchResultError resPtr
        throwIO (mkExecuteError queryText message errorType)

-- | Keep the weak-finalizer owner alive until the native operation returns.
keepAlive :: a -> IO b -> IO b
keepAlive owner (IO action) = IO (\state -> keepAlive# owner state action)

-- | Internal helper for safely accessing the underlying prepared statement.
withStatementHandle :: Statement -> (DuckDBPreparedStatement -> IO a) -> IO a
withStatementHandle stmt@Statement{statementState, statementConnection} action =
    keepAlive stmt $
        withConnectionHandle statementConnection $ \_ -> do
            state <- readIORef statementState
            case state of
                StatementClosed -> throwIO (statementClosedError stmt)
                StatementOpen{statementHandle} -> action statementHandle

-- | Internal helper for safely accessing the underlying connection handle.
withConnectionHandle :: Connection -> (DuckDBConnection -> IO a) -> IO a
withConnectionHandle conn@Connection{connectionState} action =
    keepAlive conn $ do
        state <- readIORef connectionState
        case state of
            ConnectionClosed -> throwIO connectionClosedError
            ConnectionOpen{connectionHandle} -> action connectionHandle

-- | Internal helper for safely accessing the underlying database handle.
withDatabaseHandle :: Connection -> (DuckDBDatabase -> IO a) -> IO a
withDatabaseHandle conn@Connection{connectionState} action =
    keepAlive conn $ do
        state <- readIORef connectionState
        case state of
            ConnectionClosed -> throwIO connectionClosedError
            ConnectionOpen{connectionDatabase} -> action connectionDatabase

-- | Acquire the client context for the connection, destroying it after the action.
withClientContext :: Connection -> (DuckDBClientContext -> IO a) -> IO a
withClientContext conn action =
    withConnectionHandle conn $ \connPtr ->
        bracket
            (alloca $ \ctxPtr -> c_duckdb_connection_get_client_context connPtr ctxPtr >> peek ctxPtr)
            destroyClientContext
            action

-- | Destroy a client context handle.
destroyClientContext :: DuckDBClientContext -> IO ()
destroyClientContext ctx =
    alloca $ \ptr -> poke ptr ctx >> c_duckdb_destroy_client_context ptr

-- | Destroy a value handle.
destroyValue :: DuckDBValue -> IO ()
destroyValue value =
    alloca $ \ptr -> poke ptr value >> c_duckdb_destroy_value ptr

-- | Throw a standardised registration error.
throwRegistrationError :: String -> IO a
throwRegistrationError label =
    throwIO
        SQLError
            { sqlErrorMessage = Text.pack ("duckdb-simple: " <> label <> " failed")
            , sqlErrorType = Nothing
            , sqlErrorQuery = Nothing
            }
