{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Native types that the C API cannot create. A connection reads them the
first time a parameter needs one, and destroys them when it closes.
-}
module Database.DuckDB.Simple.TypeCache (
    TypeCache,
    defaultGeometryCRS,
    createTypeCache,
    destroyTypeCache,
    cachedLogicalType,
) where

import Control.Concurrent.MVar (MVar, modifyMVarMasked, modifyMVar_, newMVar)
import Control.Exception (bracket, finally, mask_, onException, throwIO)
import Control.Monad (forM_, when)
import qualified Data.ByteString as BS
import Data.List (nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Database.DuckDB.FFI
import Database.DuckDB.Simple.LogicalRep (LogicalTypeRep (..), destroyLogicalType, logicalTypeToRep)
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek, poke, sizeOf)

{- | The configured CRSs and the native types of a connection. The types are
absent until a parameter needs one. The connection owns the types.
-}
data TypeCache = TypeCache
    { typeCacheDatabase :: !DuckDBDatabase
    , typeCacheCRS :: ![Text]
    , typeCacheTypes :: !(MVar (Maybe NativeTypes))
    }

{- | The VARIANT type and the GEOMETRY types for the configured CRSs. Each
GEOMETRY type has two keys: the configured CRS text and the CRS text that
DuckDB reports for the type.
-}
data NativeTypes = NativeTypes
    { nativeVariant :: !DuckDBLogicalType
    , nativeGeometry :: !(Map Text DuckDBLogicalType)
    , nativeOwned :: ![DuckDBLogicalType]
    }

-- | The CRS that a connection reads when the options do not give a list.
defaultGeometryCRS :: [Text]
defaultGeometryCRS = ["OGC:CRS84"]

{- | Make an empty cache for a database. The caller must destroy the cache. A
CRS must be nonempty and must not contain NUL.
-}
createTypeCache :: DuckDBDatabase -> [Text] -> IO TypeCache
createTypeCache database crsList = do
    let crss = nub crsList
    when (any Text.null crss || any (Text.any (== '\0')) crss) $
        throwIO (userError "duckdb-simple: a GEOMETRY CRS must be nonempty and must not contain NUL")
    TypeCache database crss <$> newMVar Nothing

-- | Destroy the types in the cache, if the connection read them.
destroyTypeCache :: TypeCache -> IO ()
destroyTypeCache TypeCache{typeCacheTypes} =
    modifyMVar_ typeCacheTypes \types -> Nothing <$ mapM_ (mapM_ destroyLogicalType . nativeOwned) types

{- | Copy a cached type for a leaf that the C API cannot create. The caller
must destroy the copy. The first VARIANT leaf or GEOMETRY leaf with a CRS
reads the types. A GEOMETRY CRS that the cache does not hold gives GEOMETRY
without a CRS.
-}
cachedLogicalType :: TypeCache -> LogicalTypeRep -> IO DuckDBLogicalType
cachedLogicalType cache = \case
    LogicalTypeScalar DuckDBTypeVariant -> nativeTypes cache >>= copyLogicalType . nativeVariant
    LogicalTypeGeometry (Just crs) -> do
        NativeTypes{nativeGeometry} <- nativeTypes cache
        maybe (c_duckdb_create_logical_type (DuckDBType DuckDBTypeGeometry)) copyLogicalType (Map.lookup crs nativeGeometry)
    LogicalTypeGeometry Nothing -> c_duckdb_create_logical_type (DuckDBType DuckDBTypeGeometry)
    other -> throwIO (userError ("duckdb-simple: the type cache cannot create " <> show other))

{- | Get the native types. Read them on first use with a separate connection,
so the query does not run in the transaction of the caller. A failed read
leaves the cache empty.
-}
nativeTypes :: TypeCache -> IO NativeTypes
nativeTypes TypeCache{typeCacheDatabase, typeCacheCRS, typeCacheTypes} =
    modifyMVarMasked typeCacheTypes \cached -> do
        types <- maybe (withTypeConnection typeCacheDatabase (`readNativeTypes` typeCacheCRS)) pure cached
        pure (Just types, types)

-- | Read the VARIANT type and a GEOMETRY type for each CRS with one query.
readNativeTypes :: DuckDBConnection -> [Text] -> IO NativeTypes
readNativeTypes connection crss = do
    let sql = Text.concat ("SELECT NULL::VARIANT" : [", system.main.ST_SetCRS('POINT EMPTY'::GEOMETRY, ?)" | _ <- crss])
    withTypeQuery connection sql crss \result -> mask_ do
        owned <- columnTypes result (length crss + 1)
        case owned of
            [] -> throwIO (userError "duckdb-simple: the type query returned no columns")
            variant : geometry -> do
                reported <- mapM logicalTypeToRep geometry `onException` mapM_ destroyLogicalType owned
                pure
                    NativeTypes
                        { nativeVariant = variant
                        , nativeGeometry =
                            Map.fromList
                                ( concat
                                    [ (crs, logical) : [(reportedCRS, logical) | LogicalTypeGeometry (Just reportedCRS) <- [rep]]
                                    | (crs, rep, logical) <- zip3 crss reported geometry
                                    ]
                                )
                        , nativeOwned = owned
                        }

-- | Take the types of the first columns. Destroy the taken types on failure.
columnTypes :: Ptr DuckDBResult -> Int -> IO [DuckDBLogicalType]
columnTypes result count = go [] 0
  where
    go taken column
        | column == count = pure (reverse taken)
        | otherwise = do
            logical <- c_duckdb_column_logical_type result (fromIntegral column) `onException` mapM_ destroyLogicalType taken
            when (logical == DuckDBLogicalType nullPtr) do
                mapM_ destroyLogicalType taken
                throwIO (userError "duckdb-simple: the type query returned no type")
            go (logical : taken) (column + 1)

-- | Copy a type through a LIST type, because the C API has no copy function.
copyLogicalType :: DuckDBLogicalType -> IO DuckDBLogicalType
copyLogicalType logical =
    -- TODO: use a copy function when the C API has one.
    -- See https://github.com/Tritlo/duckdb-haskell/issues/30 and
    -- https://github.com/duckdb/duckdb/issues/26664.
    bracket (c_duckdb_create_list_type logical) destroyLogicalType c_duckdb_list_type_child_type

-- | Run an action with a new connection to the database. Disconnect after it.
withTypeConnection :: DuckDBDatabase -> (DuckDBConnection -> IO a) -> IO a
withTypeConnection database action =
    alloca \connectionPtr -> do
        connected <- c_duckdb_connect database connectionPtr
        when (connected /= DuckDBSuccess) $
            throwIO (userError "duckdb-simple: cannot connect to read the VARIANT and GEOMETRY types")
        (peek connectionPtr >>= action) `finally` c_duckdb_disconnect connectionPtr

-- | Prepare and run a constant query with text parameters, and borrow its result.
withTypeQuery :: DuckDBConnection -> Text -> [Text] -> (Ptr DuckDBResult -> IO a) -> IO a
withTypeQuery connection sql parameters action =
    BS.useAsCString (TextEncoding.encodeUtf8 sql) \sqlPtr ->
        alloca \statementPtr -> do
            poke statementPtr (DuckDBPreparedStatement nullPtr)
            bracket (c_duckdb_prepare connection (ConstPtr sqlPtr) statementPtr) (const (c_duckdb_destroy_prepare statementPtr)) \prepared -> do
                statement <- peek statementPtr
                when (prepared /= DuckDBSuccess) do
                    ConstPtr errorPtr <- c_duckdb_prepare_error statement
                    message <- if errorPtr == nullPtr then pure "prepare failed" else TextEncoding.decodeUtf8 <$> BS.packCString errorPtr
                    queryFailed message
                forM_ (zip [1 ..] parameters) \(index, parameter) ->
                    BS.useAsCStringLen (TextEncoding.encodeUtf8 parameter) \(ptr, len) -> do
                        bound <- c_duckdb_bind_varchar_length statement index (ConstPtr ptr) (fromIntegral len)
                        when (bound /= DuckDBSuccess) (queryFailed ("cannot bind CRS " <> parameter))
                alloca \result -> do
                    fillBytes result 0 (sizeOf (undefined :: DuckDBResult))
                    bracket (c_duckdb_execute_prepared statement result) (const (c_duckdb_destroy_result result)) \executed -> do
                        when (executed /= DuckDBSuccess) do
                            ConstPtr errorPtr <- c_duckdb_result_error result
                            message <- if errorPtr == nullPtr then pure "execution failed" else TextEncoding.decodeUtf8 <$> BS.packCString errorPtr
                            queryFailed message
                        action result
  where
    queryFailed message = throwIO (userError ("duckdb-simple: cannot read the VARIANT and GEOMETRY types: " <> Text.unpack message))
