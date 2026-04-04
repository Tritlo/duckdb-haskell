{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StrictData #-}

{- |
Module      : Database.DuckDB.Simple.Internal
Description : Internal machinery backing the duckdb-simple API surface.

This module provides access to the opaque data constructors and helper
utilities required by the high-level API.  It is not part of the supported
public interface; consumers should depend on @Database.DuckDB.Simple@ instead.
-}
module Database.DuckDB.Simple.Internal (
    -- * Field value (to be used by FromField)
    Field (..),
    FieldValue (..),
    StructField (..),
    StructValue (..),
    UnionMemberType (..),
    UnionValue (..),
    LogicalTypeRep (..),
    BitString (..),
    BigNum (..),
    DecimalValue (..),
    IntervalValue (..),
    TimeWithZone (..),
    -- * Data constructors (internal use only)
    Query (..),
    Connection (..),
    ConnectionState (..),
    RowParser (..),
    RowParseRO (..),
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

import Control.Applicative (Alternative)
import Control.Exception (Exception, bracket, throwIO)
import Control.Monad (MonadPlus, when)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.State.Strict (StateT)
import Data.Array (Array)
import Data.Bits (Bits (..))
import qualified Data.ByteString as BS
import Data.Int (Int16, Int32, Int64, Int8)
import Data.IORef (IORef, readIORef)
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Foreign as TextForeign
import Data.Time.Calendar (Day)
import Data.Time.Clock (UTCTime (..))
import Data.Time.LocalTime (
    LocalTime (..),
    TimeOfDay (..),
    TimeZone (..),
 )
import qualified Data.UUID as UUID
import Data.Word (Word16, Word32, Word64, Word8)
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
import Database.DuckDB.Simple.LogicalRep (
    LogicalTypeRep (..),
    StructField (..),
    StructValue (..),
    UnionMemberType (..),
    UnionValue (..),
 )
import Database.DuckDB.Simple.Ok (Ok)
import Foreign.C.String (CString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.StablePtr (StablePtr, castPtrToStablePtr, freeStablePtr)
import Foreign.Storable (peek, poke)

-- | Internal representation of a column value.
data FieldValue
    = FieldNull
    | FieldInt8 Int8
    | FieldInt16 Int16
    | FieldInt32 Int32
    | FieldInt64 Int64
    | FieldWord8 Word8
    | FieldWord16 Word16
    | FieldWord32 Word32
    | FieldWord64 Word64
    | FieldUUID UUID.UUID
    | FieldFloat Float
    | FieldDouble Double
    | FieldText Text
    | FieldBool Bool
    | FieldBlob BS.ByteString
    | FieldDate Day
    | FieldTime TimeOfDay
    | FieldTimestamp LocalTime
    | FieldInterval IntervalValue
    | FieldHugeInt Integer
    | FieldUHugeInt Integer
    | FieldDecimal DecimalValue
    | FieldTimestampTZ UTCTime
    | FieldTimeTZ TimeWithZone
    | FieldBit BitString
    | FieldBigNum BigNum
    | FieldEnum Word32
    | FieldArray (Array Int FieldValue)
    | FieldList [FieldValue]
    | FieldMap [(FieldValue, FieldValue)]
    | FieldStruct (StructValue FieldValue)
    | FieldUnion (UnionValue FieldValue)
    deriving (Eq, Show)

-- | Exact-width decimal payload plus its declared width and scale.
data DecimalValue = DecimalValue
    { decimalWidth :: !Word8
    , decimalScale :: !Word8
    , decimalInteger :: !Integer
    }
    deriving (Eq, Show)

-- | DuckDB interval payload split into months, days, and microseconds.
data IntervalValue = IntervalValue
    { intervalMonths :: !Int32
    , intervalDays :: !Int32
    , intervalMicros :: !Int64
    }
    deriving (Eq, Show)

-- | Arbitrary-precision integer wrapper used for DuckDB's BIGNUM type.
newtype BigNum = BigNum Integer
    deriving stock (Eq, Show)
    deriving (Num) via Integer

-- | Time-of-day paired with its associated timezone offset.
data TimeWithZone = TimeWithZone
    { timeWithZoneTime :: !TimeOfDay
    , timeWithZoneZone :: !TimeZone
    }
    deriving (Eq, Show)

-- | DuckDB BIT value represented as raw bytes plus left-padding bit count.
data BitString = BitString
    { padding :: !Word8
    , bits :: !BS.ByteString
    }
    deriving stock (Eq)

instance Show BitString where
    show (BitString padding bits) =
        drop (fromIntegral padding) $ concatMap word8ToString $ BS.unpack bits
      where
        word8ToString :: Word8 -> String
        word8ToString w = map (\n -> if testBit w n then '1' else '0') [7, 6 .. 0]

-- | Metadata for a single column in a row.
data Field = Field
    { fieldName :: Text
    , fieldIndex :: Int
    , fieldValue :: FieldValue
    }
    deriving (Eq, Show)

-- | Parser used by @FromRow@ implementations.
newtype RowParser a = RowParser
    { runRowParser :: ReaderT RowParseRO (StateT (Int, [Field]) Ok) a
    }
    deriving stock (Functor)
    deriving newtype (Applicative, Alternative, Monad, MonadPlus)

-- | Row parsing environment (read-only data available to the parser).
newtype RowParseRO = RowParseRO
    { rowParseColumnCount :: Int
    }

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
