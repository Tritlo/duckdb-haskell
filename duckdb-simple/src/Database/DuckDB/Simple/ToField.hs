{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

{- |
Module      : Database.DuckDB.Simple.ToField
Description : Convert Haskell parameters into DuckDB bindable values.

The 'ToField' class mirrors the interface provided by @sqlite-simple@ while
delegating to the DuckDB C API under the hood.
-}
module Database.DuckDB.Simple.ToField (
    FieldBinding,
    ToField (..),
    NamedParam (..),
    bindFieldBinding,
) where

import Control.Exception (bracket, throwIO)
import Control.Monad (when)
import qualified Data.ByteString as BS
import Data.Int (Int16, Int32, Int64)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Foreign as TextForeign
import Data.Word (Word16, Word32, Word64, Word8)
import Database.DuckDB.FFI (
    DuckDBIdx,
    DuckDBPreparedStatement,
    DuckDBValue,
    c_duckdb_bind_value,
    c_duckdb_create_blob,
    c_duckdb_create_bool,
    c_duckdb_create_double,
    c_duckdb_create_int64,
    c_duckdb_create_null_value,
    c_duckdb_create_varchar,
    c_duckdb_destroy_value,
    c_duckdb_prepare_error,
    pattern DuckDBSuccess,
 )
import Database.DuckDB.Simple.Internal (
    SQLError (..),
    Statement (..),
    withStatementHandle,
 )
import Database.DuckDB.Simple.Types (Null (..))
import Foreign.C.String (peekCString)
import Foreign.C.Types (CDouble (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (poke)

-- | Represents a named parameter binding using the @:=@ operator.
data NamedParam where
    (:=) :: (ToField a) => Text -> a -> NamedParam

infixr 3 :=

-- | Encapsulates the action required to bind a single positional parameter.
newtype FieldBinding = FieldBinding (Statement -> DuckDBIdx -> IO ())

-- | Apply a 'FieldBinding' to the given statement/index.
bindFieldBinding :: Statement -> DuckDBIdx -> FieldBinding -> IO ()
bindFieldBinding stmt idx (FieldBinding action) = action stmt idx

-- | Types that can be used as positional parameters.
class ToField a where
    toField :: a -> FieldBinding

instance ToField Null where
    toField Null = nullBinding

instance ToField Bool where
    toField value =
        FieldBinding \stmt idx ->
            bindDuckValue stmt idx (c_duckdb_create_bool (if value then 1 else 0))

instance ToField Int where
    toField = toField . (fromIntegral :: Int -> Int64)

instance ToField Int16 where
    toField = toField . (fromIntegral :: Int16 -> Int64)

instance ToField Int32 where
    toField = toField . (fromIntegral :: Int32 -> Int64)

instance ToField Int64 where
    toField value =
        FieldBinding \stmt idx ->
            bindDuckValue stmt idx (c_duckdb_create_int64 value)

instance ToField Word where
    toField = toField . (fromIntegral :: Word -> Int64)

instance ToField Word16 where
    toField = toField . (fromIntegral :: Word16 -> Int64)

instance ToField Word32 where
    toField = toField . (fromIntegral :: Word32 -> Int64)

instance ToField Word64 where
    toField = toField . (fromIntegral :: Word64 -> Int64)

instance ToField Double where
    toField value =
        FieldBinding \stmt idx ->
            bindDuckValue stmt idx (c_duckdb_create_double (CDouble value))

instance ToField Float where
    toField value = toField (realToFrac value :: Double)

instance ToField Text where
    toField txt =
        FieldBinding \stmt idx ->
            TextForeign.withCString txt \cstr ->
                bindDuckValue stmt idx (c_duckdb_create_varchar cstr)

instance ToField String where
    toField = toField . Text.pack

instance ToField BS.ByteString where
    toField bs =
        FieldBinding \stmt idx ->
            BS.useAsCStringLen bs \(ptr, len) ->
                bindDuckValue stmt idx (c_duckdb_create_blob (castPtr ptr :: Ptr Word8) (fromIntegral len))

instance (ToField a) => ToField (Maybe a) where
    toField Nothing = nullBinding
    toField (Just value) = toField value

-- | Helper for binding 'Null' values.
nullBinding :: FieldBinding
nullBinding =
    FieldBinding \stmt idx ->
        bindDuckValue stmt idx c_duckdb_create_null_value

bindDuckValue :: Statement -> DuckDBIdx -> IO DuckDBValue -> IO ()
bindDuckValue stmt idx makeValue =
    withStatementHandle stmt \handle ->
        bracket makeValue destroyValue \value -> do
            rc <- c_duckdb_bind_value handle idx value
            when (rc /= DuckDBSuccess) $ do
                err <- fetchPrepareError handle
                throwBindError stmt err

destroyValue :: DuckDBValue -> IO ()
destroyValue value =
    alloca \ptr -> do
        poke ptr value
        c_duckdb_destroy_value ptr

fetchPrepareError :: DuckDBPreparedStatement -> IO Text
fetchPrepareError handle = do
    msgPtr <- c_duckdb_prepare_error handle
    if msgPtr == nullPtr
        then pure (Text.pack "duckdb-simple: parameter binding failed")
        else Text.pack <$> peekCString msgPtr

throwBindError :: Statement -> Text -> IO a
throwBindError Statement{statementQuery} msg =
    throwIO
        SQLError
            { sqlErrorMessage = msg
            , sqlErrorType = Nothing
            , sqlErrorQuery = Just statementQuery
            }
