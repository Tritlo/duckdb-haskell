{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Native types that the C API cannot create. A connection reads them once
when it opens and destroys them when it closes.
-}
module Database.DuckDB.Simple.TypeCache (
    TypeCache,
    defaultGeometryCRS,
    createTypeCache,
    destroyTypeCache,
    cachedLogicalType,
) where

import Control.Exception (bracket, mask_, onException, throwIO)
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
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek, poke, sizeOf)

{- | The VARIANT type and the GEOMETRY types for the configured CRSs. Each
GEOMETRY type has two keys: the configured CRS text and the CRS text that
DuckDB reports for the type. The connection owns the types.
-}
data TypeCache = TypeCache
    { typeCacheVariant :: !DuckDBLogicalType
    , typeCacheGeometry :: !(Map Text DuckDBLogicalType)
    , typeCacheOwned :: ![DuckDBLogicalType]
    }

-- | The CRS that a connection reads when the options do not give a list.
defaultGeometryCRS :: [Text]
defaultGeometryCRS = ["OGC:CRS84"]

{- | Read the VARIANT type and a GEOMETRY type for each CRS with one query.
The caller must destroy the cache. A CRS must be nonempty and must not
contain NUL.
-}
createTypeCache :: DuckDBConnection -> [Text] -> IO TypeCache
createTypeCache connection crsList = do
    let crss = nub crsList
    when (any Text.null crss || any (Text.any (== '\0')) crss) $
        throwIO (userError "duckdb-simple: a GEOMETRY CRS must be nonempty and must not contain NUL")
    let sql = Text.concat ("SELECT NULL::VARIANT" : [", system.main.ST_SetCRS('POINT EMPTY'::GEOMETRY, ?)" | _ <- crss])
    withTypeQuery connection sql crss \result -> mask_ do
        owned <- columnTypes result (length crss + 1)
        case owned of
            [] -> throwIO (userError "duckdb-simple: the type query returned no columns")
            variant : geometry -> do
                reported <- mapM logicalTypeToRep geometry `onException` mapM_ destroyLogicalType owned
                pure
                    TypeCache
                        { typeCacheVariant = variant
                        , typeCacheGeometry =
                            Map.fromList
                                ( concat
                                    [ (crs, logical) : [(reportedCRS, logical) | LogicalTypeGeometry (Just reportedCRS) <- [rep]]
                                    | (crs, rep, logical) <- zip3 crss reported geometry
                                    ]
                                )
                        , typeCacheOwned = owned
                        }

-- | Take the types of the first columns. Destroy the taken types on failure.
columnTypes :: Ptr DuckDBResult -> Int -> IO [DuckDBLogicalType]
columnTypes result count = go [] 0
  where
    go taken column
        | column == count = pure (reverse taken)
        | otherwise = do
            logical <- c_duckdb_column_logical_type result (fromIntegral column) `onException` mapM_ destroyLogicalType taken
            when (logical == nullPtr) do
                mapM_ destroyLogicalType taken
                throwIO (userError "duckdb-simple: the type query returned no type")
            go (logical : taken) (column + 1)

-- | Destroy every type in the cache.
destroyTypeCache :: TypeCache -> IO ()
destroyTypeCache TypeCache{typeCacheOwned} = mapM_ destroyLogicalType typeCacheOwned

{- | Copy a cached type for a leaf that the C API cannot create. The caller
must destroy the copy. A GEOMETRY CRS that the cache does not hold gives
GEOMETRY without a CRS.
-}
cachedLogicalType :: TypeCache -> LogicalTypeRep -> IO DuckDBLogicalType
cachedLogicalType TypeCache{typeCacheVariant, typeCacheGeometry} = \case
    LogicalTypeScalar DuckDBTypeVariant -> copyLogicalType typeCacheVariant
    LogicalTypeGeometry (Just crs)
        | Just logical <- Map.lookup crs typeCacheGeometry -> copyLogicalType logical
    LogicalTypeGeometry _ -> c_duckdb_create_logical_type DuckDBTypeGeometry
    other -> throwIO (userError ("duckdb-simple: the type cache cannot create " <> show other))

-- | Copy a type through a LIST type, because the C API has no copy function.
copyLogicalType :: DuckDBLogicalType -> IO DuckDBLogicalType
copyLogicalType logical =
    bracket (c_duckdb_create_list_type logical) destroyLogicalType c_duckdb_list_type_child_type

-- | Prepare and run a constant query with text parameters, and borrow its result.
withTypeQuery :: DuckDBConnection -> Text -> [Text] -> (Ptr DuckDBResult -> IO a) -> IO a
withTypeQuery connection sql parameters action =
    BS.useAsCString (TextEncoding.encodeUtf8 sql) \sqlPtr ->
        alloca \statementPtr -> do
            poke statementPtr nullPtr
            bracket (c_duckdb_prepare connection sqlPtr statementPtr) (const (c_duckdb_destroy_prepare statementPtr)) \prepared -> do
                statement <- peek statementPtr
                when (prepared /= DuckDBSuccess) do
                    errorPtr <- c_duckdb_prepare_error statement
                    message <- if errorPtr == nullPtr then pure "prepare failed" else TextEncoding.decodeUtf8 <$> BS.packCString errorPtr
                    queryFailed message
                forM_ (zip [1 ..] parameters) \(index, parameter) ->
                    BS.useAsCStringLen (TextEncoding.encodeUtf8 parameter) \(ptr, len) -> do
                        bound <- c_duckdb_bind_varchar_length statement index ptr (fromIntegral len)
                        when (bound /= DuckDBSuccess) (queryFailed ("cannot bind CRS " <> parameter))
                alloca \result -> do
                    fillBytes result 0 (sizeOf (undefined :: DuckDBResult))
                    bracket (c_duckdb_execute_prepared statement result) (const (c_duckdb_destroy_result result)) \executed -> do
                        when (executed /= DuckDBSuccess) do
                            errorPtr <- c_duckdb_result_error result
                            message <- if errorPtr == nullPtr then pure "execution failed" else TextEncoding.decodeUtf8 <$> BS.packCString errorPtr
                            queryFailed message
                        action result
  where
    queryFailed message = throwIO (userError ("duckdb-simple: cannot read the VARIANT and GEOMETRY types: " <> Text.unpack message))
