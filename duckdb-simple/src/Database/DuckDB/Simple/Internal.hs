{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StrictData #-}

{- |
Module      : Database.DuckDB.Simple.Internal
Description : Internal machinery backing the duckdb-simple API surface.

This module provides access to the opaque data constructors and helper
utilities required by the high-level API.  It is not part of the supported
public interface; consumers should depend on 'Database.DuckDB.Simple' instead.
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
    StatementStreamColumn (..),
    StatementStreamChunk (..),
    StatementStreamChunkVector (..),
    SQLError (..),
    toSQLError,

    -- * Helpers
    connectionClosedError,
    statementClosedError,
    withDatabaseHandle,
    withConnectionHandle,
    withStatementHandle,
    withQueryCString,
    withClientContext,
    destroyClientContext,
    destroyValue,
    destroyLogicalType,
    throwRegistrationError,
    releaseStablePtrData,
    mkDeleteCallback,
) where

import Control.Exception (Exception, bracket, throwIO)
import Control.Monad (when)
import Data.IORef (IORef, readIORef)
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Foreign as TextForeign
import Data.Word (Word64)
import Database.DuckDB.FFI (
    DuckDBClientContext,
    DuckDBConnection,
    DuckDBDataChunk,
    DuckDBDatabase,
    DuckDBDeleteCallback,
    DuckDBErrorType,
    DuckDBLogicalType,
    DuckDBPreparedStatement,
    DuckDBResult,
    DuckDBType,
    DuckDBValue,
    DuckDBVector,
    c_duckdb_connection_get_client_context,
    c_duckdb_destroy_client_context,
    c_duckdb_destroy_logical_type,
    c_duckdb_destroy_value,
 )
import Foreign.C.String (CString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.StablePtr (StablePtr, castPtrToStablePtr, freeStablePtr)
import Foreign.Storable (peek, poke)

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
    | StatementStreamActive !StatementStream

-- | Streaming cursor backing an active result set.
data StatementStream = StatementStream
    { statementStreamResult :: Ptr DuckDBResult
    , statementStreamColumns :: [StatementStreamColumn]
    , statementStreamChunk :: Maybe StatementStreamChunk
    }

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
    , statementStreamChunkVectors :: [StatementStreamChunkVector]
    }

-- | Raw vector pointers backing a chunk column.
data StatementStreamChunkVector = StatementStreamChunkVector
    { statementStreamChunkVectorHandle :: DuckDBVector
    , statementStreamChunkVectorData :: Ptr ()
    , statementStreamChunkVectorValidity :: Ptr Word64
    }

-- | Represents an error reported by DuckDB or by duckdb-simple itself.
data SQLError = SQLError
    { sqlErrorMessage :: Text
    , sqlErrorType :: Maybe DuckDBErrorType
    , sqlErrorQuery :: Maybe Query
    }
    deriving stock (Eq, Show)

instance Exception SQLError

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
withQueryCString (Query txt) = TextForeign.withCString txt

-- | Internal helper for safely accessing the underlying prepared statement.
withStatementHandle :: Statement -> (DuckDBPreparedStatement -> IO a) -> IO a
withStatementHandle stmt@Statement{statementState} action = do
    state <- readIORef statementState
    case state of
        StatementClosed -> throwIO (statementClosedError stmt)
        StatementOpen{statementHandle} -> action statementHandle

-- | Internal helper for safely accessing the underlying connection handle.
withConnectionHandle :: Connection -> (DuckDBConnection -> IO a) -> IO a
withConnectionHandle Connection{connectionState} action = do
    state <- readIORef connectionState
    case state of
        ConnectionClosed -> throwIO connectionClosedError
        ConnectionOpen{connectionHandle} -> action connectionHandle

-- | Internal helper for safely accessing the underlying database handle.
withDatabaseHandle :: Connection -> (DuckDBDatabase -> IO a) -> IO a
withDatabaseHandle Connection{connectionState} action = do
    state <- readIORef connectionState
    case state of
        ConnectionClosed -> throwIO connectionClosedError
        ConnectionOpen{connectionDatabase} -> action connectionDatabase

-- | Acquire the client context for the connection, destroying it after the action.
withClientContext :: Connection -> (DuckDBClientContext -> IO a) -> IO a
withClientContext conn action =
    withConnectionHandle conn $ \connPtr ->
        alloca $ \ctxPtr -> do
            c_duckdb_connection_get_client_context connPtr ctxPtr
            ctx <- peek ctxPtr
            bracket (pure ctx) destroyClientContext action

-- | Destroy a client context handle.
destroyClientContext :: DuckDBClientContext -> IO ()
destroyClientContext ctx =
    alloca $ \ptr -> poke ptr ctx >> c_duckdb_destroy_client_context ptr

-- | Destroy a value handle.
destroyValue :: DuckDBValue -> IO ()
destroyValue value =
    alloca $ \ptr -> poke ptr value >> c_duckdb_destroy_value ptr

-- | Destroy a logical type handle.
destroyLogicalType :: DuckDBLogicalType -> IO ()
destroyLogicalType logicalType =
    alloca $ \ptr -> poke ptr logicalType >> c_duckdb_destroy_logical_type ptr

-- | Throw a standardised registration error.
throwRegistrationError :: String -> IO a
throwRegistrationError label =
    throwIO
        SQLError
            { sqlErrorMessage = Text.pack ("duckdb-simple: " <> label <> " failed")
            , sqlErrorType = Nothing
            , sqlErrorQuery = Nothing
            }

-- | Free a stable pointer stored behind a raw @Ptr ()@.
releaseStablePtrData :: Ptr () -> IO ()
releaseStablePtrData rawPtr =
    when (rawPtr /= nullPtr) $
        freeStablePtr (castPtrToStablePtr rawPtr :: StablePtr ())

-- | Create a DuckDB delete callback from a Haskell function.
foreign import ccall "wrapper"
    mkDeleteCallback :: (Ptr () -> IO ()) -> IO DuckDBDeleteCallback
