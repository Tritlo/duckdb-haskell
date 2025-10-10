{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}

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
    SQLError (..),

    -- * Helpers
    connectionClosedError,
    statementClosedError,
    withConnectionHandle,
    withStatementHandle,
    withQueryCString,
) where

import Control.Exception (Exception, throwIO)
import Data.IORef (IORef, readIORef)
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Foreign as TextForeign
import Database.DuckDB.FFI (
    DuckDBConnection,
    DuckDBDatabase,
    DuckDBErrorType,
    DuckDBPreparedStatement,
 )
import Foreign.C.String (CString)

-- | Represents a textual SQL query with UTF-8 encoding semantics.
newtype Query = Query
    { fromQuery :: Text
    -- ^ Extract the underlying textual representation of the query.
    }
    deriving stock (Eq, Ord, Show)

instance IsString Query where
    fromString = Query . Text.pack

-- | Tracks the lifetime of a DuckDB database and connection pair.
data Connection = Connection
    { connectionState :: !(IORef ConnectionState)
    }

-- | Internal connection lifecycle state.
data ConnectionState
    = ConnectionClosed
    | ConnectionOpen
        { connectionDatabase :: !DuckDBDatabase
        , connectionHandle :: !DuckDBConnection
        }

-- | Tracks the lifetime of a prepared statement.
data Statement = Statement
    { statementState :: !(IORef StatementState)
    , statementConnection :: !Connection
    , statementQuery :: !Query
    }

-- | Internal statement lifecycle state.
data StatementState
    = StatementClosed
    | StatementOpen
        { statementHandle :: !DuckDBPreparedStatement
        }

-- | Represents an error reported by DuckDB or by duckdb-simple itself.
data SQLError = SQLError
    { sqlErrorMessage :: !Text
    , sqlErrorType :: !(Maybe DuckDBErrorType)
    , sqlErrorQuery :: !(Maybe Query)
    }
    deriving stock (Eq, Show)

instance Exception SQLError

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
