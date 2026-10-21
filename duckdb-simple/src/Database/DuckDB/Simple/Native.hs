{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Shared V2 error handling and borrowed UTF-8 views.
module Database.DuckDB.Simple.Native (
    module Database.DuckDB.FFI.V2,
    Query (..),
    SQLError (..),
    checked,
    checkCode,
    output,
    destroy,
    destroyValue,
    destroyLogicalType,
    withCreatedValues,
    nullHandle,
    indices,
    withText,
    withTextArray,
    readText,
    withBytes,
    readBytes,
    withIdentifier,
    withIdentifierArray,
    readIdentifier,
    throwInvalidArgument,
    withConnectionFactory,
    factoryOutput,
    withContextFactory,
    withInstanceFactory,
) where

import Control.Exception (Exception, bracket, finally, mask_, throwIO)
import Control.Monad (unless)
import qualified Data.ByteString as BS
import Data.Coerce (Coercible, coerce)
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Database.DuckDB.FFI.V2
import Foreign.C.ConstPtr (ConstPtr (..))
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
    , sqlErrorType :: Maybe DuckDBV2Error
    , sqlErrorQuery :: Maybe Query
    }
    deriving stock (Eq, Show)

instance Exception SQLError

-- | A null pointer represented by an opaque native handle.
nullHandle :: (Coercible (Ptr ()) a) => a
nullHandle = coerce (nullPtr :: Ptr ())

-- | Check a status from an operation without an error-info output.
checkCode :: DuckDBV2Error -> IO ()
checkCode code =
    unless (code == DuckDBV2ErrorNone) $
        throwIO (SQLError (Text.pack (show code)) (Just code) Nothing)

-- | Copy a native diagnostic and release its error-info handle.
checked :: (Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error) -> IO ()
checked action = mask_ $ alloca $ \slot -> do
    poke slot nullHandle
    let release = c_duckdb_v2_error_info_destroy slot >>= checkCode
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
            throwIO (SQLError message (Just code) Nothing)

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

-- | Raise an error for an invalid library argument.
throwInvalidArgument :: Text -> IO a
throwInvalidArgument message = throwIO (SQLError ("duckdb-simple: " <> message) Nothing Nothing)

-- | Borrow a factory while its connection remains alive. Do not destroy it.
withConnectionFactory :: DuckDBV2ConnectionHandle -> (DuckDBV2FactoryHandle -> IO a) -> IO a
withConnectionFactory connection action = output (c_duckdb_v2_connection_get_factory connection) >>= action

-- | Read one checked output from a call that borrows the connection's factory.
factoryOutput :: (Storable a) => DuckDBV2ConnectionHandle -> (DuckDBV2FactoryHandle -> Ptr a -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error) -> IO a
factoryOutput connection create = withConnectionFactory connection (output . create)

-- | Borrow a factory during its native callback. Do not retain or destroy it.
withContextFactory :: DuckDBV2ContextHandle -> (DuckDBV2FactoryHandle -> IO a) -> IO a
withContextFactory context action = output (c_duckdb_v2_context_get_factory context) >>= action

-- | Borrow an instance factory for built-in types, casts, and allocations.
withInstanceFactory :: DuckDBV2InstanceHandle -> (DuckDBV2FactoryHandle -> IO a) -> IO a
withInstanceFactory instance_ action = output (c_duckdb_v2_instance_get_factory instance_) >>= action
