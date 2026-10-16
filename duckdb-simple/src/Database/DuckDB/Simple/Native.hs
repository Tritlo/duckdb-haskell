{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Shared V2 error handling and borrowed UTF-8 views.
module Database.DuckDB.Simple.Native (
    module Database.DuckDB.FFI.V2,
    Query (..),
    SQLError (..),
    toSQLError,
    checked,
    checkCode,
    output,
    destroy,
    nullHandle,
    withText,
    readText,
    withBytes,
    readBytes,
    withIdentifier,
    readIdentifier,
) where

import Control.Exception (Exception, finally, mask_, throwIO)
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
import Foreign.Marshal.Utils (fillBytes, with)
import Foreign.Ptr (Ptr, nullPtr)
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

-- | Convert an exception into an error without a native code or query.
toSQLError :: (Exception e) => e -> SQLError
toSQLError ex = SQLError (Text.pack (show ex)) Nothing Nothing

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

-- | Borrow UTF-8 text for the duration of a native call.
withText :: Text -> (ConstPtr DuckDBV2Str -> IO a) -> IO a
withText value = withBytes (Text.encodeUtf8 value)

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

-- | Copy a borrowed native identifier.
readIdentifier :: DuckDBV2IdentifierT -> IO Text
readIdentifier = readText . coerce
