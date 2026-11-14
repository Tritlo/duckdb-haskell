{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Database.DuckDB.Simple.Native
-- Description : Shared V2 error handling and UTF-8 views.
module Database.DuckDB.Simple.Native (
  module Database.DuckDB.FFI.V2,
  Query (..),
  SQLError (..),
  ErrorType (..),
  errorTypeCodes,
  errorTypeFromNative,
  SettingScope (..),
  settingScopeToNative,
  settingScopeFromNative,
  LogLevel (..),
  logLevelToNative,
  logLevelFromNative,
  LogScope (..),
  logScopeFromNative,
  TokenType (..),
  tokenTypeFromNative,
  MacroFunctionType (..),
  macroFunctionTypeToNative,
  LogicalTypeId (..),
  logicalTypeIdToNative,
  logicalTypeIdFromNative,
  FileFlag (..),
  fileFlagToNative,
  checked,
  checkCode,
  output,
  destroy,
  destroyValue,
  destroyLogicalType,
  acquireItems,
  releaseItems,
  withCreatedValues,
  nullHandle,
  indices,
  withText,
  withTextArray,
  readText,
  valueText,
  withBytes,
  readBytes,
  withIdentifier,
  withIdentifierArray,
  readIdentifier,
  foldIdentifier,
  throwInvalidArgument,
  withConnectionFactory,
  factoryOutput,
  withSQLStatement,
  withContextFactory,
  withInstanceFactory,
  ValueFactory,
  NativeValue,
) where

import Control.Applicative ((<|>))
import Control.Exception (Exception, SomeException, bracket, finally, mask_, onException, throwIO, try)
import Control.Monad (foldM, unless, when, (<$!>))
import qualified Data.ByteString as BS
import Data.Char (isAsciiUpper, toLower)
import Data.Coerce (Coercible, coerce)
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Text.Foreign as TextForeign
import Database.DuckDB.FFI.V2
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.C.Types (CUInt)
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Array (withArray)
import Foreign.Marshal.Utils (fillBytes, with, withMany)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (Storable, peek, poke, sizeOf)

-- | SQL text encoded as UTF-8 when it is sent to DuckDB.
newtype Query = Query {fromQuery :: Text}
  deriving stock (Eq, Ord, Show)

instance Semigroup Query where
  Query a <> Query b = Query (a <> b)

instance IsString Query where
  fromString = Query . Text.pack

-- | A native V2 error or an error reported by duckdb-simple.
data SQLError = SQLError
  { sqlErrorMessage :: Text
  , sqlErrorType :: Maybe ErrorType
  , sqlErrorQuery :: Maybe Query
  }
  deriving stock (Eq, Show)

instance Exception SQLError

-- | The category of a native DuckDB error. An error that duckdb-simple reports
-- has no error type.
data ErrorType
  = ErrorApi
  | ErrorIOFileNotFound
  | ErrorIOReadFailure
  | ErrorIOEof
  | ErrorIOGeneral
  | ErrorIONetwork
  | ErrorIOHTTP
  | ErrorInputInvalid
  | ErrorInputParameterInvalid
  | ErrorInputOutOfRange
  | ErrorInputObjectSize
  | ErrorInputMetadataDeferred
  | ErrorResourceInUse
  | ErrorResourceOutOfMemory
  | ErrorResourceConnection
  | ErrorResourceDependency
  | ErrorResourceMissingExtension
  | ErrorResourceAutoload
  | ErrorTypeConversion
  | ErrorTypeUnknown
  | ErrorTypeInvalid
  | ErrorTypeMismatch
  | ErrorTypeDecimal
  | ErrorTypeDivideByZero
  | ErrorQueryParser
  | ErrorQuerySyntax
  | ErrorQueryBinder
  | ErrorQueryPlanner
  | ErrorQueryOptimizer
  | ErrorQueryExpression
  | ErrorQueryExecutor
  | ErrorQueryScheduler
  | ErrorQueryNotImplemented
  | ErrorQueryParameterNotResolved
  | ErrorQueryParameterNotAllowed
  | ErrorDatabaseCatalog
  | ErrorDatabaseTransaction
  | ErrorDatabaseConstraint
  | ErrorDatabaseIndex
  | ErrorDatabaseSequence
  | ErrorDatabaseStatistics
  | ErrorDatabaseSerialization
  | ErrorConfigurationSettings
  | ErrorConfigurationInvalid
  | ErrorConfigurationPermission
  | ErrorRuntimeInternal
  | ErrorRuntimeFatal
  | ErrorRuntimeInterrupt
  | ErrorRuntimeNullPointer
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Each error type and its native error code.
errorTypeCodes :: [(ErrorType, DuckDBV2Error)]
errorTypeCodes =
  [ (ErrorApi, DuckDBV2ErrorApi)
  , (ErrorIOFileNotFound, DuckDBV2ErrorIOFileNotFound)
  , (ErrorIOReadFailure, DuckDBV2ErrorIOReadFailure)
  , (ErrorIOEof, DuckDBV2ErrorIOEof)
  , (ErrorIOGeneral, DuckDBV2ErrorIOGeneral)
  , (ErrorIONetwork, DuckDBV2ErrorIONetwork)
  , (ErrorIOHTTP, DuckDBV2ErrorIOHTTP)
  , (ErrorInputInvalid, DuckDBV2ErrorInputInvalid)
  , (ErrorInputParameterInvalid, DuckDBV2ErrorInputParameterInvalid)
  , (ErrorInputOutOfRange, DuckDBV2ErrorInputOutOfRange)
  , (ErrorInputObjectSize, DuckDBV2ErrorInputObjectSize)
  , (ErrorInputMetadataDeferred, DuckDBV2ErrorInputMetadataDeferred)
  , (ErrorResourceInUse, DuckDBV2ErrorResourceInUse)
  , (ErrorResourceOutOfMemory, DuckDBV2ErrorResourceOutOfMemory)
  , (ErrorResourceConnection, DuckDBV2ErrorResourceConnection)
  , (ErrorResourceDependency, DuckDBV2ErrorResourceDependency)
  , (ErrorResourceMissingExtension, DuckDBV2ErrorResourceMissingExtension)
  , (ErrorResourceAutoload, DuckDBV2ErrorResourceAutoload)
  , (ErrorTypeConversion, DuckDBV2ErrorTypeConversion)
  , (ErrorTypeUnknown, DuckDBV2ErrorTypeUnknown)
  , (ErrorTypeInvalid, DuckDBV2ErrorTypeInvalid)
  , (ErrorTypeMismatch, DuckDBV2ErrorTypeMismatch)
  , (ErrorTypeDecimal, DuckDBV2ErrorTypeDecimal)
  , (ErrorTypeDivideByZero, DuckDBV2ErrorTypeDivideByZero)
  , (ErrorQueryParser, DuckDBV2ErrorQueryParser)
  , (ErrorQuerySyntax, DuckDBV2ErrorQuerySyntax)
  , (ErrorQueryBinder, DuckDBV2ErrorQueryBinder)
  , (ErrorQueryPlanner, DuckDBV2ErrorQueryPlanner)
  , (ErrorQueryOptimizer, DuckDBV2ErrorQueryOptimizer)
  , (ErrorQueryExpression, DuckDBV2ErrorQueryExpression)
  , (ErrorQueryExecutor, DuckDBV2ErrorQueryExecutor)
  , (ErrorQueryScheduler, DuckDBV2ErrorQueryScheduler)
  , (ErrorQueryNotImplemented, DuckDBV2ErrorQueryNotImplemented)
  , (ErrorQueryParameterNotResolved, DuckDBV2ErrorQueryParameterNotResolved)
  , (ErrorQueryParameterNotAllowed, DuckDBV2ErrorQueryParameterNotAllowed)
  , (ErrorDatabaseCatalog, DuckDBV2ErrorDatabaseCatalog)
  , (ErrorDatabaseTransaction, DuckDBV2ErrorDatabaseTransaction)
  , (ErrorDatabaseConstraint, DuckDBV2ErrorDatabaseConstraint)
  , (ErrorDatabaseIndex, DuckDBV2ErrorDatabaseIndex)
  , (ErrorDatabaseSequence, DuckDBV2ErrorDatabaseSequence)
  , (ErrorDatabaseStatistics, DuckDBV2ErrorDatabaseStatistics)
  , (ErrorDatabaseSerialization, DuckDBV2ErrorDatabaseSerialization)
  , (ErrorConfigurationSettings, DuckDBV2ErrorConfigurationSettings)
  , (ErrorConfigurationInvalid, DuckDBV2ErrorConfigurationInvalid)
  , (ErrorConfigurationPermission, DuckDBV2ErrorConfigurationPermission)
  , (ErrorRuntimeInternal, DuckDBV2ErrorRuntimeInternal)
  , (ErrorRuntimeFatal, DuckDBV2ErrorRuntimeFatal)
  , (ErrorRuntimeInterrupt, DuckDBV2ErrorRuntimeInterrupt)
  , (ErrorRuntimeNullPointer, DuckDBV2ErrorRuntimeNullPointer)
  ]

-- | Find the error type of a native error code. The success code and an unknown
-- code give 'Nothing'.
errorTypeFromNative :: DuckDBV2Error -> Maybe ErrorType
errorTypeFromNative code = lookup code [(native, errorType) | (errorType, native) <- errorTypeCodes]

-- | A null pointer represented by an opaque native handle.
nullHandle :: (Coercible (Ptr ()) a) => a
nullHandle = coerce (nullPtr :: Ptr ())

-- | Check a status from an operation without an error-info output.
checkCode :: DuckDBV2Error -> IO ()
checkCode code =
  unless (code == DuckDBV2ErrorNone) $
    throwIO (SQLError (Text.pack (show code)) (errorTypeFromNative code) Nothing)

-- | Copy a native diagnostic and release its error-info handle. Decode the
-- error text with replacement characters, so that a native text that is not
-- valid UTF-8 cannot raise when the error is shown.
checked :: (Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error) -> IO ()
checked action = mask_ $ alloca $ \slot -> do
  poke slot nullHandle
  -- A successful call leaves the slot null. Destroy only a handle in the slot.
  let release = do
        info <- peek slot
        unless (info == nullHandle) (c_duckdb_v2_error_info_destroy slot >>= checkCode)
  flip finally release $ do
    code <- action slot
    unless (code == DuckDBV2ErrorNone) $ do
      info <- peek slot
      message <-
        if info == nullHandle
          then pure (Text.pack (show code))
          else alloca $ \view -> do
            rc <- c_duckdb_v2_error_info_get_text info view
            if rc == DuckDBV2ErrorNone
              then Text.decodeUtf8With lenientDecode <$> (peek view >>= readBytes)
              else pure (Text.pack (show code))
      throwIO (SQLError message (errorTypeFromNative code) Nothing)

-- | Read one checked native output. Initialize its storage before the call.
output :: forall a. (Storable a) => (Ptr a -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error) -> IO a
output action = alloca $ \slot -> do
  fillBytes slot 0 (sizeOf (undefined :: a))
  checked (action slot)
  peek slot

-- | Destroy an owned handle held in a temporary native slot.
destroy :: (Storable a) => (Ptr a -> IO DuckDBV2Error) -> a -> IO ()
destroy release value = with value (\slot -> release slot >>= checkCode)

-- | Release an owned native value.
destroyValue :: DuckDBV2ValueHandle -> IO ()
destroyValue = destroy c_duckdb_v2_value_destroy

-- | Release an owned native logical type.
destroyLogicalType :: DuckDBV2LogicalTypeHandle -> IO ()
destroyLogicalType = destroy c_duckdb_v2_logical_type_destroy

-- | Run the acquire actions in list order under 'mask_', and give the items in
-- list order. If an acquire action raises an error, release the items that the
-- earlier actions gave, from the last to the first, and raise that error. The
-- fold keeps a constant stack.
acquireItems :: (a -> IO ()) -> [IO a] -> IO [a]
acquireItems release = mask_ . fmap reverse . foldM (\owned acquire -> (: owned) <$> acquire `onException` releaseItems release owned) []

-- | Release each item in list order, also when a release raises an error.
-- Raise the first error after the last release. The fold keeps a constant stack.
releaseItems :: (a -> IO ()) -> [a] -> IO ()
releaseItems release items = foldM step (Nothing :: Maybe SomeException) items >>= mapM_ throwIO
  where
    step failure item = either (\err -> failure <|> Just err) (const failure) <$!> try (release item)

-- | Create the values in list order, and give them to the action. Destroy the
-- values in reverse order of creation after the action or after a failure. One
-- 'bracket' covers the whole list.
withCreatedValues :: [IO DuckDBV2ValueHandle] -> ([DuckDBV2ValueHandle] -> IO a) -> IO a
withCreatedValues creates = bracket (acquireItems destroyValue creates) (releaseItems destroyValue . reverse)

-- | List the indices below a native count without unsigned underflow.
indices :: DuckDBIdx -> [DuckDBIdx]
indices count = if count == 0 then [] else [0 .. count - 1]

-- | Lend UTF-8 text to DuckDB for the duration of the action.
withText :: Text -> (ConstPtr DuckDBV2Str -> IO a) -> IO a
withText value = withBytes (Text.encodeUtf8 value)

-- | Lend an array of UTF-8 views to DuckDB for the duration of the action.
withTextArray :: [Text] -> (Ptr DuckDBV2Str -> IO a) -> IO a
withTextArray values action =
  withMany withText values $ \views -> mapM (\(ConstPtr view) -> peek view) views >>= (`withArray` action)

-- | Copy the bytes of a UTF-8 view whose memory DuckDB owns. The result stays
-- valid after DuckDB frees that memory.
readText :: DuckDBV2Str -> IO Text
readText view = Text.decodeUtf8 <$> readBytes view

-- | Copy the native text of a value.
valueText :: DuckDBV2ValueHandle -> IO Text
valueText value = do
  count <- output (c_duckdb_v2_value_to_string value nullPtr 0)
  when (toInteger count >= toInteger (maxBound :: Int)) $ throwInvalidArgument "value text exceeds Int range"
  allocaBytes (fromIntegral count + 1) $ \buffer -> do
    _ <- output (c_duckdb_v2_value_to_string value buffer (count + 1))
    Text.decodeUtf8 <$> BS.packCStringLen (buffer, fromIntegral count)

-- | Lend bytes to DuckDB through a length-delimited view for the duration of the action.
withBytes :: BS.ByteString -> (ConstPtr DuckDBV2Str -> IO a) -> IO a
withBytes value action = BS.useAsCStringLen value $ \(ptr, count) ->
  with (DuckDBV2Str (ConstPtr ptr) (fromIntegral count)) (action . ConstPtr)

-- | Copy the bytes of a view whose memory DuckDB owns, including interior NUL
-- bytes. The result stays valid after DuckDB frees that memory.
readBytes :: DuckDBV2Str -> IO BS.ByteString
readBytes (DuckDBV2Str (ConstPtr ptr) count)
  | count == 0 = pure BS.empty
  | otherwise = BS.packCStringLen (ptr, fromIntegral count)

-- | Lend a UTF-8 identifier to DuckDB for the duration of the action.
withIdentifier :: Text -> (ConstPtr DuckDBV2IdentifierT -> IO a) -> IO a
withIdentifier value action = withText value (action . coerce)

-- | Lend an array of UTF-8 identifiers to DuckDB for the duration of the action.
withIdentifierArray :: [Text] -> (Ptr DuckDBV2IdentifierT -> IO a) -> IO a
withIdentifierArray values action = withTextArray values (action . castPtr)

-- | Copy the bytes of an identifier whose memory DuckDB owns. The result stays
-- valid after DuckDB frees that memory.
readIdentifier :: DuckDBV2IdentifierT -> IO Text
readIdentifier = readText . coerce

-- | Fold ASCII letters to lower case, as DuckDB does for identifiers.
foldIdentifier :: Text -> Text
foldIdentifier = Text.map (\char -> if isAsciiUpper char then toLower char else char)

-- | Raise an error for an invalid library argument.
throwInvalidArgument :: Text -> IO a
throwInvalidArgument message = throwIO (SQLError ("duckdb-simple: " <> message) Nothing Nothing)

-- | Borrow a factory while its connection remains alive. Do not destroy it.
withConnectionFactory :: DuckDBV2ConnectionHandle -> (DuckDBV2FactoryHandle -> IO a) -> IO a
withConnectionFactory connection action = output (c_duckdb_v2_connection_get_factory connection) >>= action

-- | Read one checked output from a call that borrows the connection's factory.
factoryOutput
  :: (Storable a) => DuckDBV2ConnectionHandle -> (DuckDBV2FactoryHandle -> Ptr a -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error) -> IO a
factoryOutput connection create = withConnectionFactory connection (output . create)

-- | Parse trusted SQL and borrow its first statement.
withSQLStatement :: DuckDBV2ConnectionHandle -> Text -> (DuckDBV2SqlStatementHandle -> IO a) -> IO a
withSQLStatement connection sql action =
  TextForeign.withCString sql $ \source ->
    bracket (output (c_duckdb_v2_parse_sql connection (ConstPtr source))) (destroy c_duckdb_v2_statement_iterator_destroy) $ \iterator ->
      bracket (output (c_duckdb_v2_statement_iterator_next iterator)) (destroy c_duckdb_v2_sql_statement_destroy) action

-- | Borrow a factory during its native callback. Do not retain or destroy it.
withContextFactory :: DuckDBV2ContextHandle -> (DuckDBV2FactoryHandle -> IO a) -> IO a
withContextFactory context action = output (c_duckdb_v2_context_get_factory context) >>= action

-- | Borrow an instance factory for built-in types, casts, and allocations.
withInstanceFactory :: DuckDBV2InstanceHandle -> (DuckDBV2FactoryHandle -> IO a) -> IO a
withInstanceFactory instance_ action = output (c_duckdb_v2_instance_get_factory instance_) >>= action

-- | The native factory that makes values and types. A connection, a callback
-- context, and an instance each lend one. The name has no C API version: a later
-- version changes only this synonym.
type ValueFactory = DuckDBV2FactoryHandle

-- | An owned native value. Destroy it after use. The name has no C API
-- version: a later version changes only this synonym.
type NativeValue = DuckDBV2ValueHandle

-- | The scope of a configuration value. The default scope selects the scope that the option declares.
data SettingScope
  = SettingScopeDefault
  | SettingScopeGlobal
  | SettingScopeSession
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Convert a setting scope to its native value.
settingScopeToNative :: SettingScope -> DuckDBV2SettingScope
settingScopeToNative = \case
  SettingScopeDefault -> DuckDBV2SettingScopeDefault
  SettingScopeGlobal -> DuckDBV2SettingScopeGlobal
  SettingScopeSession -> DuckDBV2SettingScopeSession

-- | Convert a native setting scope. An unknown value raises 'SQLError'.
settingScopeFromNative :: DuckDBV2SettingScope -> IO SettingScope
settingScopeFromNative = \case
  DuckDBV2SettingScopeDefault -> pure SettingScopeDefault
  DuckDBV2SettingScopeGlobal -> pure SettingScopeGlobal
  DuckDBV2SettingScopeSession -> pure SettingScopeSession
  DuckDBV2SettingScope value -> throwUnknownNative "setting scope" value

-- | The severity of a log entry.
data LogLevel
  = LogLevelTrace
  | LogLevelDebug
  | LogLevelInfo
  | LogLevelWarning
  | LogLevelError
  | LogLevelFatal
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Convert a log level to its native value.
logLevelToNative :: LogLevel -> DuckDBV2LogLevel
logLevelToNative = \case
  LogLevelTrace -> DuckDBV2LogLevelTrace
  LogLevelDebug -> DuckDBV2LogLevelDebug
  LogLevelInfo -> DuckDBV2LogLevelInfo
  LogLevelWarning -> DuckDBV2LogLevelWarning
  LogLevelError -> DuckDBV2LogLevelError
  LogLevelFatal -> DuckDBV2LogLevelFatal

-- | Convert a native log level. An unknown value raises 'SQLError'.
logLevelFromNative :: DuckDBV2LogLevel -> IO LogLevel
logLevelFromNative = \case
  DuckDBV2LogLevelTrace -> pure LogLevelTrace
  DuckDBV2LogLevelDebug -> pure LogLevelDebug
  DuckDBV2LogLevelInfo -> pure LogLevelInfo
  DuckDBV2LogLevelWarning -> pure LogLevelWarning
  DuckDBV2LogLevelError -> pure LogLevelError
  DuckDBV2LogLevelFatal -> pure LogLevelFatal
  DuckDBV2LogLevel value -> throwUnknownNative "log level" value

-- | The context of a log entry: the database, a connection, or a thread.
data LogScope
  = LogScopeDatabase
  | LogScopeConnection
  | LogScopeThread
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Convert a native log scope. An unknown value raises 'SQLError'.
logScopeFromNative :: DuckDBV2LogScope -> IO LogScope
logScopeFromNative = \case
  DuckDBV2LogScopeDatabase -> pure LogScopeDatabase
  DuckDBV2LogScopeConnection -> pure LogScopeConnection
  DuckDBV2LogScopeThread -> pure LogScopeThread
  DuckDBV2LogScope value -> throwUnknownNative "log scope" value

-- | The kind of a SQL token.
data TokenType
  = TokenTypeInvalid
  | TokenTypeKeyword
  | TokenTypeIdentifier
  | TokenTypeStringLiteral
  | TokenTypeNumberLiteral
  | TokenTypeOperator
  | TokenTypeComment
  | TokenTypeTerminator
  | TokenTypeEndOfInput
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Convert a native token type. An unknown value raises 'SQLError'.
tokenTypeFromNative :: DuckDBV2TokenType -> IO TokenType
tokenTypeFromNative = \case
  DuckDBV2TokenTypeInvalid -> pure TokenTypeInvalid
  DuckDBV2TokenTypeKeyword -> pure TokenTypeKeyword
  DuckDBV2TokenTypeIdentifier -> pure TokenTypeIdentifier
  DuckDBV2TokenTypeStringLiteral -> pure TokenTypeStringLiteral
  DuckDBV2TokenTypeNumberLiteral -> pure TokenTypeNumberLiteral
  DuckDBV2TokenTypeOperator -> pure TokenTypeOperator
  DuckDBV2TokenTypeComment -> pure TokenTypeComment
  DuckDBV2TokenTypeTerminator -> pure TokenTypeTerminator
  DuckDBV2TokenTypeEndOfInput -> pure TokenTypeEndOfInput
  DuckDBV2TokenType value -> throwUnknownNative "token type" value

-- | The kind of a macro: a scalar expression or a table SELECT.
data MacroFunctionType
  = MacroFunctionTypeScalar
  | MacroFunctionTypeTable
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Convert a macro kind to its native value.
macroFunctionTypeToNative :: MacroFunctionType -> DuckDBV2MacroFunctionType
macroFunctionTypeToNative = \case
  MacroFunctionTypeScalar -> DuckDBV2MacroFunctionTypeScalar
  MacroFunctionTypeTable -> DuckDBV2MacroFunctionTypeTable

-- | The id of a logical type. The id gives the kind of the type, for example
-- BIGINT or STRUCT. It does not give the parameters of the type.
data LogicalTypeId
  = LogicalTypeIdInvalid
  | LogicalTypeIdSQLNull
  | LogicalTypeIdUnknown
  | LogicalTypeIdAny
  | LogicalTypeIdType
  | LogicalTypeIdBoolean
  | LogicalTypeIdTinyInt
  | LogicalTypeIdSmallInt
  | LogicalTypeIdInteger
  | LogicalTypeIdBigInt
  | LogicalTypeIdDate
  | LogicalTypeIdTime
  | LogicalTypeIdTimestampSec
  | LogicalTypeIdTimestampMs
  | LogicalTypeIdTimestamp
  | LogicalTypeIdTimestampNs
  | LogicalTypeIdDecimal
  | LogicalTypeIdFloat
  | LogicalTypeIdDouble
  | LogicalTypeIdVarchar
  | LogicalTypeIdBlob
  | LogicalTypeIdInterval
  | LogicalTypeIdUTinyInt
  | LogicalTypeIdUSmallInt
  | LogicalTypeIdUInteger
  | LogicalTypeIdUBigInt
  | LogicalTypeIdTimestampTz
  | LogicalTypeIdTimestampTzNs
  | LogicalTypeIdTimeTz
  | LogicalTypeIdTimeNs
  | LogicalTypeIdBit
  | LogicalTypeIdBigNum
  | LogicalTypeIdUHugeInt
  | LogicalTypeIdHugeInt
  | LogicalTypeIdUUID
  | LogicalTypeIdGeometry
  | LogicalTypeIdStruct
  | LogicalTypeIdList
  | LogicalTypeIdMap
  | LogicalTypeIdEnum
  | LogicalTypeIdUnion
  | LogicalTypeIdArray
  | LogicalTypeIdVariant
  | LogicalTypeIdTuple
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Convert a logical type id to its native value.
logicalTypeIdToNative :: LogicalTypeId -> DuckDBV2LogicalTypeId
logicalTypeIdToNative = \case
  LogicalTypeIdInvalid -> DuckDBV2LogicalTypeIdInvalid
  LogicalTypeIdSQLNull -> DuckDBV2LogicalTypeIdSQLNull
  LogicalTypeIdUnknown -> DuckDBV2LogicalTypeIdUnknown
  LogicalTypeIdAny -> DuckDBV2LogicalTypeIdAny
  LogicalTypeIdType -> DuckDBV2LogicalTypeIdType
  LogicalTypeIdBoolean -> DuckDBV2LogicalTypeIdBoolean
  LogicalTypeIdTinyInt -> DuckDBV2LogicalTypeIdTinyInt
  LogicalTypeIdSmallInt -> DuckDBV2LogicalTypeIdSmallInt
  LogicalTypeIdInteger -> DuckDBV2LogicalTypeIdInteger
  LogicalTypeIdBigInt -> DuckDBV2LogicalTypeIdBigInt
  LogicalTypeIdDate -> DuckDBV2LogicalTypeIdDate
  LogicalTypeIdTime -> DuckDBV2LogicalTypeIdTime
  LogicalTypeIdTimestampSec -> DuckDBV2LogicalTypeIdTimestampSec
  LogicalTypeIdTimestampMs -> DuckDBV2LogicalTypeIdTimestampMs
  LogicalTypeIdTimestamp -> DuckDBV2LogicalTypeIdTimestamp
  LogicalTypeIdTimestampNs -> DuckDBV2LogicalTypeIdTimestampNs
  LogicalTypeIdDecimal -> DuckDBV2LogicalTypeIdDecimal
  LogicalTypeIdFloat -> DuckDBV2LogicalTypeIdFloat
  LogicalTypeIdDouble -> DuckDBV2LogicalTypeIdDouble
  LogicalTypeIdVarchar -> DuckDBV2LogicalTypeIdVarchar
  LogicalTypeIdBlob -> DuckDBV2LogicalTypeIdBlob
  LogicalTypeIdInterval -> DuckDBV2LogicalTypeIdInterval
  LogicalTypeIdUTinyInt -> DuckDBV2LogicalTypeIdUTinyInt
  LogicalTypeIdUSmallInt -> DuckDBV2LogicalTypeIdUSmallInt
  LogicalTypeIdUInteger -> DuckDBV2LogicalTypeIdUInteger
  LogicalTypeIdUBigInt -> DuckDBV2LogicalTypeIdUBigInt
  LogicalTypeIdTimestampTz -> DuckDBV2LogicalTypeIdTimestampTz
  LogicalTypeIdTimestampTzNs -> DuckDBV2LogicalTypeIdTimestampTzNs
  LogicalTypeIdTimeTz -> DuckDBV2LogicalTypeIdTimeTz
  LogicalTypeIdTimeNs -> DuckDBV2LogicalTypeIdTimeNs
  LogicalTypeIdBit -> DuckDBV2LogicalTypeIdBit
  LogicalTypeIdBigNum -> DuckDBV2LogicalTypeIdBigNum
  LogicalTypeIdUHugeInt -> DuckDBV2LogicalTypeIdUHugeInt
  LogicalTypeIdHugeInt -> DuckDBV2LogicalTypeIdHugeInt
  LogicalTypeIdUUID -> DuckDBV2LogicalTypeIdUUID
  LogicalTypeIdGeometry -> DuckDBV2LogicalTypeIdGeometry
  LogicalTypeIdStruct -> DuckDBV2LogicalTypeIdStruct
  LogicalTypeIdList -> DuckDBV2LogicalTypeIdList
  LogicalTypeIdMap -> DuckDBV2LogicalTypeIdMap
  LogicalTypeIdEnum -> DuckDBV2LogicalTypeIdEnum
  LogicalTypeIdUnion -> DuckDBV2LogicalTypeIdUnion
  LogicalTypeIdArray -> DuckDBV2LogicalTypeIdArray
  LogicalTypeIdVariant -> DuckDBV2LogicalTypeIdVariant
  LogicalTypeIdTuple -> DuckDBV2LogicalTypeIdTuple

-- | Convert a native logical type id. An unknown value raises 'SQLError'.
logicalTypeIdFromNative :: DuckDBV2LogicalTypeId -> IO LogicalTypeId
logicalTypeIdFromNative = \case
  DuckDBV2LogicalTypeIdInvalid -> pure LogicalTypeIdInvalid
  DuckDBV2LogicalTypeIdSQLNull -> pure LogicalTypeIdSQLNull
  DuckDBV2LogicalTypeIdUnknown -> pure LogicalTypeIdUnknown
  DuckDBV2LogicalTypeIdAny -> pure LogicalTypeIdAny
  DuckDBV2LogicalTypeIdType -> pure LogicalTypeIdType
  DuckDBV2LogicalTypeIdBoolean -> pure LogicalTypeIdBoolean
  DuckDBV2LogicalTypeIdTinyInt -> pure LogicalTypeIdTinyInt
  DuckDBV2LogicalTypeIdSmallInt -> pure LogicalTypeIdSmallInt
  DuckDBV2LogicalTypeIdInteger -> pure LogicalTypeIdInteger
  DuckDBV2LogicalTypeIdBigInt -> pure LogicalTypeIdBigInt
  DuckDBV2LogicalTypeIdDate -> pure LogicalTypeIdDate
  DuckDBV2LogicalTypeIdTime -> pure LogicalTypeIdTime
  DuckDBV2LogicalTypeIdTimestampSec -> pure LogicalTypeIdTimestampSec
  DuckDBV2LogicalTypeIdTimestampMs -> pure LogicalTypeIdTimestampMs
  DuckDBV2LogicalTypeIdTimestamp -> pure LogicalTypeIdTimestamp
  DuckDBV2LogicalTypeIdTimestampNs -> pure LogicalTypeIdTimestampNs
  DuckDBV2LogicalTypeIdDecimal -> pure LogicalTypeIdDecimal
  DuckDBV2LogicalTypeIdFloat -> pure LogicalTypeIdFloat
  DuckDBV2LogicalTypeIdDouble -> pure LogicalTypeIdDouble
  DuckDBV2LogicalTypeIdVarchar -> pure LogicalTypeIdVarchar
  DuckDBV2LogicalTypeIdBlob -> pure LogicalTypeIdBlob
  DuckDBV2LogicalTypeIdInterval -> pure LogicalTypeIdInterval
  DuckDBV2LogicalTypeIdUTinyInt -> pure LogicalTypeIdUTinyInt
  DuckDBV2LogicalTypeIdUSmallInt -> pure LogicalTypeIdUSmallInt
  DuckDBV2LogicalTypeIdUInteger -> pure LogicalTypeIdUInteger
  DuckDBV2LogicalTypeIdUBigInt -> pure LogicalTypeIdUBigInt
  DuckDBV2LogicalTypeIdTimestampTz -> pure LogicalTypeIdTimestampTz
  DuckDBV2LogicalTypeIdTimestampTzNs -> pure LogicalTypeIdTimestampTzNs
  DuckDBV2LogicalTypeIdTimeTz -> pure LogicalTypeIdTimeTz
  DuckDBV2LogicalTypeIdTimeNs -> pure LogicalTypeIdTimeNs
  DuckDBV2LogicalTypeIdBit -> pure LogicalTypeIdBit
  DuckDBV2LogicalTypeIdBigNum -> pure LogicalTypeIdBigNum
  DuckDBV2LogicalTypeIdUHugeInt -> pure LogicalTypeIdUHugeInt
  DuckDBV2LogicalTypeIdHugeInt -> pure LogicalTypeIdHugeInt
  DuckDBV2LogicalTypeIdUUID -> pure LogicalTypeIdUUID
  DuckDBV2LogicalTypeIdGeometry -> pure LogicalTypeIdGeometry
  DuckDBV2LogicalTypeIdStruct -> pure LogicalTypeIdStruct
  DuckDBV2LogicalTypeIdList -> pure LogicalTypeIdList
  DuckDBV2LogicalTypeIdMap -> pure LogicalTypeIdMap
  DuckDBV2LogicalTypeIdEnum -> pure LogicalTypeIdEnum
  DuckDBV2LogicalTypeIdUnion -> pure LogicalTypeIdUnion
  DuckDBV2LogicalTypeIdArray -> pure LogicalTypeIdArray
  DuckDBV2LogicalTypeIdVariant -> pure LogicalTypeIdVariant
  DuckDBV2LogicalTypeIdTuple -> pure LogicalTypeIdTuple
  DuckDBV2LogicalTypeId value -> throwUnknownNative "logical type id" value

-- | A behaviour of a file open. The flags are not a bitmask. Each flag in the
-- list adds one behaviour.
data FileFlag
  = -- | Open the file for reading.
    FileFlagRead
  | -- | Open the file for writing.
    FileFlagWrite
  | -- | Create the file if it does not exist. Keep the contents of an existing file.
    FileFlagCreate
  | -- | Create the file if it does not exist. Truncate an existing file to empty.
    FileFlagCreateNew
  | -- | Open the file in append mode.
    FileFlagAppend
  | -- | Fail if the file exists. Use it only together with 'FileFlagCreate'.
    FileFlagExclusiveCreate
  | -- | Read and write at explicit offsets from more than one thread at the same time.
    FileFlagParallelAccess
  | -- | Read the file through the external file cache of DuckDB. Use it only for reading.
    FileFlagExternalFileCache
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Convert a file flag to its native value.
fileFlagToNative :: FileFlag -> DuckDBV2FileFlag
fileFlagToNative = \case
  FileFlagRead -> DuckDBV2FileFlagRead
  FileFlagWrite -> DuckDBV2FileFlagWrite
  FileFlagCreate -> DuckDBV2FileFlagCreate
  FileFlagCreateNew -> DuckDBV2FileFlagCreateNew
  FileFlagAppend -> DuckDBV2FileFlagAppend
  FileFlagExclusiveCreate -> DuckDBV2FileFlagExclusiveCreate
  FileFlagParallelAccess -> DuckDBV2FileFlagParallelAccess
  FileFlagExternalFileCache -> DuckDBV2FileFlagExternalFileCache

-- | Raise 'SQLError' for a native enum value that has no Haskell constructor.
throwUnknownNative :: Text -> CUInt -> IO a
throwUnknownNative kind value = throwIO (SQLError ("duckdb-simple: unknown native " <> kind <> " " <> Text.pack (show value)) Nothing Nothing)
