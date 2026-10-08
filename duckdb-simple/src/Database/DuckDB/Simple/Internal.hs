{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
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
    withTypeCache,
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
import Data.Coerce (Coercible, coerce)
import Data.IORef (IORef, readIORef)
import Data.List (find)
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.Foreign as TextForeign
import Database.DuckDB.FFI (DUCKDB_TYPE, Duckdb_client_context (..), Duckdb_connection, Duckdb_data_chunk (..), Duckdb_database, Duckdb_error_type, Duckdb_prepared_statement, Duckdb_result, Duckdb_state, Duckdb_value, duckdb_connection_get_client_context, duckdb_destroy_client_context, duckdb_destroy_data_chunk, duckdb_destroy_result, duckdb_destroy_value, duckdb_execute_prepared, duckdb_execute_prepared_streaming, duckdb_fetch_chunk, duckdb_interrupt, duckdb_prepare_error, duckdb_result_error, duckdb_result_error_type, pattern DUCKDB_ERROR_INVALID, pattern DuckDBSuccess)
import qualified Database.DuckDB.FFI as FFI
import Database.DuckDB.Simple.FromField (FieldValue)
import Database.DuckDB.Simple.LogicalRep (destroyLogicalType)
import Database.DuckDB.Simple.TypeCache (TypeCache)
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.C.String (CString)
import Foreign.C.Types (CChar)
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
        { connectionDatabase :: Duckdb_database
        , connectionHandle :: Duckdb_connection
        , connectionTypeCache :: TypeCache
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
        { statementHandle :: Duckdb_prepared_statement
        }

-- | Streaming execution state for prepared statements.
data StatementStreamState
    = StatementStreamIdle
    | StatementStreamExhausted
    | StatementStreamActive !StatementStream

-- | Streaming cursor backing an active result set.
data StatementStream = StatementStream
    { statementStreamResult :: Ptr Duckdb_result
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
    , statementStreamColumnType :: DUCKDB_TYPE
    }

-- | Currently loaded data chunk plus iteration cursor.
data StatementStreamChunk = StatementStreamChunk
    { statementStreamChunkPtr :: Duckdb_data_chunk
    , statementStreamChunkSize :: Int
    , statementStreamChunkIndex :: Int
    , statementStreamChunkReaders :: [Int -> IO FieldValue]
    -- ^ One reader for each column. A reader must not outlive its chunk.
    }

-- | Represents an error reported by DuckDB or by duckdb-simple itself.
data SQLError = SQLError
    { sqlErrorMessage :: Text
    , sqlErrorType :: Maybe Duckdb_error_type
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
withQueryCString :: Query -> (ConstPtr CChar -> IO a) -> IO a
withQueryCString query@(Query txt) action
    | Text.any (== '\0') txt =
        throwIO (SQLError (Text.pack "duckdb-simple: SQL contains NUL") Nothing (Just query))
    | otherwise = TextForeign.withCString txt (action . ConstPtr)

-- | Copy a NUL-terminated UTF-8 string from DuckDB.
peekUtf8CString :: (Coercible p CString) => p -> IO Text
peekUtf8CString ptr = TextEncoding.decodeUtf8 <$> BS.packCString (coerce ptr)

-- | Execute a query and destroy its result after success or failure.
withResult :: Connection -> Query -> (Ptr Duckdb_result -> IO Duckdb_state) -> (Ptr Duckdb_result -> IO a) -> IO a
withResult conn queryText executeResult action =
    alloca $ \resPtr ->
        bracket_
            (fillBytes resPtr 0 (sizeOf (undefined :: Duckdb_result)))
            (duckdb_destroy_result resPtr)
            $ do
                rc <- runInterruptibleQuery conn (executeResult resPtr)
                when (rc /= DuckDBSuccess) $ do
                    (message, errorType) <- fetchResultError resPtr
                    throwIO (mkExecuteError queryText message errorType)
                action resPtr

-- | Execute a prepared statement with the requested native result mode.
executePreparedResult :: ResultMode -> Duckdb_prepared_statement -> Ptr Duckdb_result -> IO Duckdb_state
executePreparedResult MaterializedResult = duckdb_execute_prepared
executePreparedResult StreamingResult = duckdb_execute_prepared_streaming

{- | Fetch an owned chunk. The caller must remain masked until it records ownership.
  Streaming fetches can execute SQL. Keep their output in a caller-owned slot
  so cancellation can destroy a chunk that the worker has already fetched.
-}
fetchResultChunk :: ResultMode -> Connection -> Ptr Duckdb_result -> IO Duckdb_data_chunk
fetchResultChunk MaterializedResult conn result =
    withConnectionHandle conn $ \_ -> peek result >>= duckdb_fetch_chunk
fetchResultChunk StreamingResult conn result =
    alloca $ \chunkPtr -> mask_ $ do
        poke chunkPtr (Duckdb_data_chunk nullPtr)
        let fetch = do
                chunk <- peek result >>= duckdb_fetch_chunk
                poke chunkPtr chunk
                pure DuckDBSuccess
        (runInterruptibleQuery conn fetch >> peek chunkPtr)
            `onException` duckdb_destroy_data_chunk chunkPtr

-- | Destroy an owned native data chunk. A null chunk is allowed.
destroyDataChunk :: Duckdb_data_chunk -> IO ()
destroyDataChunk chunk = alloca $ \ptr -> poke ptr chunk >> duckdb_destroy_data_chunk ptr

{- | Run a native query while the caller can receive asynchronous exceptions.
  Prompt cancellation requires the threaded RTS. Cleanup interrupts DuckDB and
  waits for the worker before the caller can release native storage.
-}
runInterruptibleQuery :: Connection -> IO Duckdb_state -> IO Duckdb_state
runInterruptibleQuery conn action =
    withConnectionHandle conn $ \handle ->
        mask $ \restore -> do
            done <- newEmptyMVar
            _ <- forkIO $ do
                outcome <- try action :: IO (Either SomeException Duckdb_state)
                putMVar done outcome
            let cancelAndWait = do
                    finished <- tryReadMVar done
                    case finished of
                        Just _ -> pure ()
                        Nothing -> do
                            -- DuckDB clears the interrupt flag at query entry.
                            -- Repeat the interrupt to cover cancellation before entry.
                            duckdb_interrupt handle
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

{- | The column type names that 'Database.DuckDB.Simple.ToField.DuckDBColumnType'
instances use, and the types they denote. Each type has one name.
-}
duckDBTypeNames :: [(Text, DUCKDB_TYPE)]
duckDBTypeNames =
    map
        (\(name, dtype) -> (Text.pack name, dtype))
        [ ("BOOLEAN", FFI.DUCKDB_TYPE_BOOLEAN)
        , ("TINYINT", FFI.DUCKDB_TYPE_TINYINT)
        , ("SMALLINT", FFI.DUCKDB_TYPE_SMALLINT)
        , ("INTEGER", FFI.DUCKDB_TYPE_INTEGER)
        , ("BIGINT", FFI.DUCKDB_TYPE_BIGINT)
        , ("HUGEINT", FFI.DUCKDB_TYPE_HUGEINT)
        , ("UTINYINT", FFI.DUCKDB_TYPE_UTINYINT)
        , ("USMALLINT", FFI.DUCKDB_TYPE_USMALLINT)
        , ("UINTEGER", FFI.DUCKDB_TYPE_UINTEGER)
        , ("UBIGINT", FFI.DUCKDB_TYPE_UBIGINT)
        , ("UHUGEINT", FFI.DUCKDB_TYPE_UHUGEINT)
        , ("FLOAT", FFI.DUCKDB_TYPE_FLOAT)
        , ("DOUBLE", FFI.DUCKDB_TYPE_DOUBLE)
        , ("DATE", FFI.DUCKDB_TYPE_DATE)
        , ("TIME", FFI.DUCKDB_TYPE_TIME)
        , ("TIMETZ", FFI.DUCKDB_TYPE_TIME_TZ)
        , ("TIMESTAMP", FFI.DUCKDB_TYPE_TIMESTAMP)
        , ("TIMESTAMPTZ", FFI.DUCKDB_TYPE_TIMESTAMP_TZ)
        , ("INTERVAL", FFI.DUCKDB_TYPE_INTERVAL)
        , ("TEXT", FFI.DUCKDB_TYPE_VARCHAR)
        , ("BLOB", FFI.DUCKDB_TYPE_BLOB)
        , ("GEOMETRY", FFI.DUCKDB_TYPE_GEOMETRY)
        , ("VARIANT", FFI.DUCKDB_TYPE_VARIANT)
        , ("UUID", FFI.DUCKDB_TYPE_UUID)
        , ("BIT", FFI.DUCKDB_TYPE_BIT)
        , ("BIGNUM", FFI.DUCKDB_TYPE_BIGNUM)
        , -- NULL gives an element type to Maybe values without data.
          ("NULL", FFI.DUCKDB_TYPE_SQLNULL)
        ]

-- | Find the type that a column type name denotes.
duckDBTypeFromName :: Text -> Maybe DUCKDB_TYPE
duckDBTypeFromName name = lookup name duckDBTypeNames

-- | Find the column type name of a type. Other types use their 'Show' text.
duckDBTypeToName :: DUCKDB_TYPE -> Text
duckDBTypeToName dtype =
    maybe (Text.pack (show dtype)) fst (find ((== dtype) . snd) duckDBTypeNames)

{- | Read the error message of a prepared statement as UTF-8. Use the fallback
when DuckDB reports no message.
-}
fetchPrepareError :: Text -> Duckdb_prepared_statement -> IO Text
fetchPrepareError fallback statement = do
    messagePtr <- duckdb_prepare_error statement
    if messagePtr == ConstPtr nullPtr then pure fallback else peekUtf8CString messagePtr

-- | Copy a result error while its native result remains alive.
fetchResultError :: Ptr Duckdb_result -> IO (Text, Maybe Duckdb_error_type)
fetchResultError resultPtr = do
    msgPtr <- duckdb_result_error resultPtr
    message <-
        if msgPtr == ConstPtr nullPtr
            then pure (Text.pack "duckdb-simple: query failed")
            else peekUtf8CString msgPtr
    errorType <- duckdb_result_error_type resultPtr
    pure (message, if errorType == DUCKDB_ERROR_INVALID then Nothing else Just errorType)

-- | Attach the query and native error category to an execution error.
mkExecuteError :: Query -> Text -> Maybe Duckdb_error_type -> SQLError
mkExecuteError queryText message errorType =
    SQLError
        { sqlErrorMessage = message
        , sqlErrorType = errorType
        , sqlErrorQuery = Just queryText
        }

-- | Report a fetch failure before treating a null chunk as end of input.
throwResultError :: Query -> Ptr Duckdb_result -> IO ()
throwResultError queryText resPtr = do
    errorPtr <- duckdb_result_error resPtr
    when (errorPtr /= ConstPtr nullPtr) $ do
        (message, errorType) <- fetchResultError resPtr
        throwIO (mkExecuteError queryText message errorType)

-- | Keep the weak-finalizer owner alive until the native operation returns.
keepAlive :: a -> IO b -> IO b
keepAlive owner (IO action) = IO (\state -> keepAlive# owner state action)

-- | Internal helper for safely accessing the underlying prepared statement.
withStatementHandle :: Statement -> (Duckdb_prepared_statement -> IO a) -> IO a
withStatementHandle stmt@Statement{statementState, statementConnection} action =
    keepAlive stmt $
        withConnectionHandle statementConnection $ \_ -> do
            state <- readIORef statementState
            case state of
                StatementClosed -> throwIO (statementClosedError stmt)
                StatementOpen{statementHandle} -> action statementHandle

-- | Internal helper for safely accessing the underlying connection handle.
withConnectionHandle :: Connection -> (Duckdb_connection -> IO a) -> IO a
withConnectionHandle conn@Connection{connectionState} action =
    keepAlive conn $ do
        state <- readIORef connectionState
        case state of
            ConnectionClosed -> throwIO connectionClosedError
            ConnectionOpen{connectionHandle} -> action connectionHandle

-- | Borrow the type cache of an open connection.
withTypeCache :: Connection -> (TypeCache -> IO a) -> IO a
withTypeCache conn@Connection{connectionState} action =
    keepAlive conn $ do
        state <- readIORef connectionState
        case state of
            ConnectionClosed -> throwIO connectionClosedError
            ConnectionOpen{connectionTypeCache} -> action connectionTypeCache

-- | Internal helper for safely accessing the underlying database handle.
withDatabaseHandle :: Connection -> (Duckdb_database -> IO a) -> IO a
withDatabaseHandle conn@Connection{connectionState} action =
    keepAlive conn $ do
        state <- readIORef connectionState
        case state of
            ConnectionClosed -> throwIO connectionClosedError
            ConnectionOpen{connectionDatabase} -> action connectionDatabase

-- | Acquire the client context for the connection, destroying it after the action.
withClientContext :: Connection -> (Duckdb_client_context -> IO a) -> IO a
withClientContext conn action =
    withConnectionHandle conn $ \connPtr ->
        bracket
            (alloca $ \ctxPtr -> duckdb_connection_get_client_context connPtr ctxPtr >> peek ctxPtr)
            destroyClientContext
            action

-- | Destroy a client context handle.
destroyClientContext :: Duckdb_client_context -> IO ()
destroyClientContext ctx =
    alloca $ \ptr -> poke ptr ctx >> duckdb_destroy_client_context ptr

-- | Destroy a value handle.
destroyValue :: Duckdb_value -> IO ()
destroyValue value =
    alloca $ \ptr -> poke ptr value >> duckdb_destroy_value ptr

-- | Throw a standardised registration error.
throwRegistrationError :: String -> IO a
throwRegistrationError label =
    throwIO
        SQLError
            { sqlErrorMessage = Text.pack ("duckdb-simple: " <> label <> " failed")
            , sqlErrorType = Nothing
            , sqlErrorQuery = Nothing
            }
