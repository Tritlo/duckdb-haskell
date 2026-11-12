{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Shared V2 error handling and borrowed UTF-8 views.
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
    logScopeToNative,
    logScopeFromNative,
    TokenType (..),
    tokenTypeToNative,
    tokenTypeFromNative,
    MacroFunctionType (..),
    macroFunctionTypeToNative,
    macroFunctionTypeFromNative,
    checked,
    checkCode,
    output,
    destroy,
    destroyValue,
    destroyLogicalType,
    withCreatedValues,
    nullHandle,
    indices,
    mapFlat,
    withText,
    withTextArray,
    readText,
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
) where

import Control.Exception (Exception, bracket, finally, mask_, throwIO)
import Control.Monad (unless)
import qualified Data.ByteString as BS
import Data.Char (isAsciiUpper, toLower)
import Data.Coerce (Coercible, coerce)
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Foreign as TextForeign
import Database.DuckDB.FFI.V2
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.C.Types (CUInt)
import Foreign.Marshal.Alloc (alloca)
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

{- | The category of a native DuckDB error. An error that duckdb-simple reports
has no error type.
-}
data ErrorType
    = ErrorTypeApi
    | ErrorTypeIOFileNotFound
    | ErrorTypeIOReadFailure
    | ErrorTypeIOEof
    | ErrorTypeIOGeneral
    | ErrorTypeIONetwork
    | ErrorTypeIOHTTP
    | ErrorTypeInputInvalid
    | ErrorTypeInputParameterInvalid
    | ErrorTypeInputOutOfRange
    | ErrorTypeInputObjectSize
    | ErrorTypeInputMetadataDeferred
    | ErrorTypeResourceInUse
    | ErrorTypeResourceOutOfMemory
    | ErrorTypeResourceConnection
    | ErrorTypeResourceDependency
    | ErrorTypeResourceMissingExtension
    | ErrorTypeResourceAutoload
    | ErrorTypeTypeConversion
    | ErrorTypeTypeUnknown
    | ErrorTypeTypeInvalid
    | ErrorTypeTypeMismatch
    | ErrorTypeTypeDecimal
    | ErrorTypeTypeDivideByZero
    | ErrorTypeQueryParser
    | ErrorTypeQuerySyntax
    | ErrorTypeQueryBinder
    | ErrorTypeQueryPlanner
    | ErrorTypeQueryOptimizer
    | ErrorTypeQueryExpression
    | ErrorTypeQueryExecutor
    | ErrorTypeQueryScheduler
    | ErrorTypeQueryNotImplemented
    | ErrorTypeQueryParameterNotResolved
    | ErrorTypeQueryParameterNotAllowed
    | ErrorTypeDatabaseCatalog
    | ErrorTypeDatabaseTransaction
    | ErrorTypeDatabaseConstraint
    | ErrorTypeDatabaseIndex
    | ErrorTypeDatabaseSequence
    | ErrorTypeDatabaseStatistics
    | ErrorTypeDatabaseSerialization
    | ErrorTypeConfigurationSettings
    | ErrorTypeConfigurationInvalid
    | ErrorTypeConfigurationPermission
    | ErrorTypeRuntimeInternal
    | ErrorTypeRuntimeFatal
    | ErrorTypeRuntimeInterrupt
    | ErrorTypeRuntimeNullPointer
    deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Each error type and its native error code.
errorTypeCodes :: [(ErrorType, DuckDBV2Error)]
errorTypeCodes =
    [ (ErrorTypeApi, DuckDBV2ErrorApi)
    , (ErrorTypeIOFileNotFound, DuckDBV2ErrorIOFileNotFound)
    , (ErrorTypeIOReadFailure, DuckDBV2ErrorIOReadFailure)
    , (ErrorTypeIOEof, DuckDBV2ErrorIOEof)
    , (ErrorTypeIOGeneral, DuckDBV2ErrorIOGeneral)
    , (ErrorTypeIONetwork, DuckDBV2ErrorIONetwork)
    , (ErrorTypeIOHTTP, DuckDBV2ErrorIOHTTP)
    , (ErrorTypeInputInvalid, DuckDBV2ErrorInputInvalid)
    , (ErrorTypeInputParameterInvalid, DuckDBV2ErrorInputParameterInvalid)
    , (ErrorTypeInputOutOfRange, DuckDBV2ErrorInputOutOfRange)
    , (ErrorTypeInputObjectSize, DuckDBV2ErrorInputObjectSize)
    , (ErrorTypeInputMetadataDeferred, DuckDBV2ErrorInputMetadataDeferred)
    , (ErrorTypeResourceInUse, DuckDBV2ErrorResourceInUse)
    , (ErrorTypeResourceOutOfMemory, DuckDBV2ErrorResourceOutOfMemory)
    , (ErrorTypeResourceConnection, DuckDBV2ErrorResourceConnection)
    , (ErrorTypeResourceDependency, DuckDBV2ErrorResourceDependency)
    , (ErrorTypeResourceMissingExtension, DuckDBV2ErrorResourceMissingExtension)
    , (ErrorTypeResourceAutoload, DuckDBV2ErrorResourceAutoload)
    , (ErrorTypeTypeConversion, DuckDBV2ErrorTypeConversion)
    , (ErrorTypeTypeUnknown, DuckDBV2ErrorTypeUnknown)
    , (ErrorTypeTypeInvalid, DuckDBV2ErrorTypeInvalid)
    , (ErrorTypeTypeMismatch, DuckDBV2ErrorTypeMismatch)
    , (ErrorTypeTypeDecimal, DuckDBV2ErrorTypeDecimal)
    , (ErrorTypeTypeDivideByZero, DuckDBV2ErrorTypeDivideByZero)
    , (ErrorTypeQueryParser, DuckDBV2ErrorQueryParser)
    , (ErrorTypeQuerySyntax, DuckDBV2ErrorQuerySyntax)
    , (ErrorTypeQueryBinder, DuckDBV2ErrorQueryBinder)
    , (ErrorTypeQueryPlanner, DuckDBV2ErrorQueryPlanner)
    , (ErrorTypeQueryOptimizer, DuckDBV2ErrorQueryOptimizer)
    , (ErrorTypeQueryExpression, DuckDBV2ErrorQueryExpression)
    , (ErrorTypeQueryExecutor, DuckDBV2ErrorQueryExecutor)
    , (ErrorTypeQueryScheduler, DuckDBV2ErrorQueryScheduler)
    , (ErrorTypeQueryNotImplemented, DuckDBV2ErrorQueryNotImplemented)
    , (ErrorTypeQueryParameterNotResolved, DuckDBV2ErrorQueryParameterNotResolved)
    , (ErrorTypeQueryParameterNotAllowed, DuckDBV2ErrorQueryParameterNotAllowed)
    , (ErrorTypeDatabaseCatalog, DuckDBV2ErrorDatabaseCatalog)
    , (ErrorTypeDatabaseTransaction, DuckDBV2ErrorDatabaseTransaction)
    , (ErrorTypeDatabaseConstraint, DuckDBV2ErrorDatabaseConstraint)
    , (ErrorTypeDatabaseIndex, DuckDBV2ErrorDatabaseIndex)
    , (ErrorTypeDatabaseSequence, DuckDBV2ErrorDatabaseSequence)
    , (ErrorTypeDatabaseStatistics, DuckDBV2ErrorDatabaseStatistics)
    , (ErrorTypeDatabaseSerialization, DuckDBV2ErrorDatabaseSerialization)
    , (ErrorTypeConfigurationSettings, DuckDBV2ErrorConfigurationSettings)
    , (ErrorTypeConfigurationInvalid, DuckDBV2ErrorConfigurationInvalid)
    , (ErrorTypeConfigurationPermission, DuckDBV2ErrorConfigurationPermission)
    , (ErrorTypeRuntimeInternal, DuckDBV2ErrorRuntimeInternal)
    , (ErrorTypeRuntimeFatal, DuckDBV2ErrorRuntimeFatal)
    , (ErrorTypeRuntimeInterrupt, DuckDBV2ErrorRuntimeInterrupt)
    , (ErrorTypeRuntimeNullPointer, DuckDBV2ErrorRuntimeNullPointer)
    ]

{- | Find the error type of a native error code. The success code and an unknown
code give 'Nothing'.
-}
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

-- | Copy a native diagnostic and release its error-info handle.
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
                            then peek view >>= readText
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

-- | Release temporary values after construction or failure.
withCreatedValues :: [IO DuckDBV2ValueHandle] -> ([DuckDBV2ValueHandle] -> IO a) -> IO a
withCreatedValues = withMany (\create -> bracket create destroyValue)

-- | List the indices below a native count without unsigned underflow.
indices :: DuckDBIdx -> [DuckDBIdx]
indices count = if count == 0 then [] else [0 .. count - 1]

{- | Map a list in IO without a stack frame for each element. The runtime
scans the stack of a thread at each native call, and 'mapM' keeps one frame
for each element. A native call in a 'mapM' over 2000 elements then takes
about 20 times as long as in this loop.
-}
mapFlat :: (a -> IO b) -> [a] -> IO [b]
mapFlat action = go []
  where
    go done [] = pure (reverse done)
    go done (item : rest) = action item >>= \result -> go (result : done) rest

-- | Borrow UTF-8 text for the duration of a native call.
withText :: Text -> (ConstPtr DuckDBV2Str -> IO a) -> IO a
withText value = withBytes (Text.encodeUtf8 value)

-- | Borrow an array of UTF-8 views for the duration of a native call.
withTextArray :: [Text] -> (Ptr DuckDBV2Str -> IO a) -> IO a
withTextArray values action =
    withMany withText values $ \views -> mapM (\(ConstPtr view) -> peek view) views >>= (`withArray` action)

-- | Copy a borrowed UTF-8 view.
readText :: DuckDBV2Str -> IO Text
readText view = Text.decodeUtf8 <$> readBytes view

-- | Borrow bytes through a length-delimited native view.
withBytes :: BS.ByteString -> (ConstPtr DuckDBV2Str -> IO a) -> IO a
withBytes value action = BS.useAsCStringLen value $ \(ptr, count) ->
    with (DuckDBV2Str (ConstPtr ptr) (fromIntegral count)) (action . ConstPtr)

-- | Copy bytes from a borrowed native view, including interior NUL bytes.
readBytes :: DuckDBV2Str -> IO BS.ByteString
readBytes (DuckDBV2Str (ConstPtr ptr) count)
    | count == 0 = pure BS.empty
    | otherwise = BS.packCStringLen (ptr, fromIntegral count)

-- | Borrow a UTF-8 native identifier.
withIdentifier :: Text -> (ConstPtr DuckDBV2IdentifierT -> IO a) -> IO a
withIdentifier value action = withText value (action . coerce)

-- | Borrow an array of UTF-8 native identifiers.
withIdentifierArray :: [Text] -> (Ptr DuckDBV2IdentifierT -> IO a) -> IO a
withIdentifierArray values action = withTextArray values (action . castPtr)

-- | Copy a borrowed native identifier.
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
factoryOutput :: (Storable a) => DuckDBV2ConnectionHandle -> (DuckDBV2FactoryHandle -> Ptr a -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error) -> IO a
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

-- | Convert a log scope to its native value.
logScopeToNative :: LogScope -> DuckDBV2LogScope
logScopeToNative = \case
    LogScopeDatabase -> DuckDBV2LogScopeDatabase
    LogScopeConnection -> DuckDBV2LogScopeConnection
    LogScopeThread -> DuckDBV2LogScopeThread

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

-- | Convert a token type to its native value.
tokenTypeToNative :: TokenType -> DuckDBV2TokenType
tokenTypeToNative = \case
    TokenTypeInvalid -> DuckDBV2TokenTypeInvalid
    TokenTypeKeyword -> DuckDBV2TokenTypeKeyword
    TokenTypeIdentifier -> DuckDBV2TokenTypeIdentifier
    TokenTypeStringLiteral -> DuckDBV2TokenTypeStringLiteral
    TokenTypeNumberLiteral -> DuckDBV2TokenTypeNumberLiteral
    TokenTypeOperator -> DuckDBV2TokenTypeOperator
    TokenTypeComment -> DuckDBV2TokenTypeComment
    TokenTypeTerminator -> DuckDBV2TokenTypeTerminator
    TokenTypeEndOfInput -> DuckDBV2TokenTypeEndOfInput

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

-- | Convert a native macro kind. An unknown value raises 'SQLError'.
macroFunctionTypeFromNative :: DuckDBV2MacroFunctionType -> IO MacroFunctionType
macroFunctionTypeFromNative = \case
    DuckDBV2MacroFunctionTypeScalar -> pure MacroFunctionTypeScalar
    DuckDBV2MacroFunctionTypeTable -> pure MacroFunctionTypeTable
    DuckDBV2MacroFunctionType value -> throwUnknownNative "macro function type" value

-- | Raise 'SQLError' for a native enum value that has no Haskell constructor.
throwUnknownNative :: Text -> CUInt -> IO a
throwUnknownNative kind value = throwIO (SQLError ("duckdb-simple: unknown native " <> kind <> " " <> Text.pack (show value)) Nothing Nothing)
