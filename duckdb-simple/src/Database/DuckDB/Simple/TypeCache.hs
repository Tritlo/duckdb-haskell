{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Connection-scoped construction of native logical types.
module Database.DuckDB.Simple.TypeCache (
    TypeCache,
    typeCacheConnection,
    defaultGeometryCRS,
    createTypeCache,
    destroyTypeCache,
    cachedLogicalType,
    withTypeConnection,
) where

import Control.Concurrent.MVar (MVar, modifyMVarMasked, newMVar)
import Control.Exception (SomeException, bracket, finally, mask, onException, throwIO, try, uninterruptibleMask_)
import Control.Monad (when)
import Data.List (nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Foreign as TextForeign
import Database.DuckDB.Simple.LogicalRep (LogicalTypeRep (..), logicalTypeFromRepWith, logicalTypeToRep)
import Database.DuckDB.Simple.Native
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.Marshal.Array (withArray)
import Foreign.Marshal.Utils (withMany)
import Foreign.Ptr (nullPtr)

-- | Owned helper state. The caller connection owns this cache.
data TypeCache = TypeCache
    { typeCacheConnection :: !DuckDBV2ConnectionHandle
    , typeCacheInstance :: !DuckDBV2InstanceHandle
    , typeCacheCRS :: ![Text]
    , typeCacheState :: !(MVar CacheState)
    }

-- | The helper and its logical types share the lifetime of the instance.
data CacheState = Unopened | Open !DuckDBV2ConnectionHandle !NativeTypes | Closed

-- | Keep each owned type once. CRS aliases can refer to the same type.
data NativeTypes = NativeTypes
    { nativeGeometry :: !(Map Text DuckDBV2LogicalTypeHandle)
    , nativeOwned :: ![DuckDBV2LogicalTypeHandle]
    }

-- | The default CRS used by connection options.
defaultGeometryCRS :: [Text]
defaultGeometryCRS = ["OGC:CRS84"]

-- | Create a lazy cache. The caller keeps the instance and connection alive.
createTypeCache :: DuckDBV2InstanceHandle -> DuckDBV2ConnectionHandle -> [Text] -> IO TypeCache
createTypeCache instance_ connection crss = do
    when (any Text.null crss || any (Text.any (== '\0')) crss) $
        throwIO (userError "duckdb-simple: a GEOMETRY CRS must be nonempty and must not contain NUL")
    TypeCache connection instance_ (nub crss) <$> newMVar Unopened

-- | Close the helper after all users finish. Destroy cached types once.
destroyTypeCache :: TypeCache -> IO ()
destroyTypeCache TypeCache{typeCacheState} = uninterruptibleMask_ do
    previous <- modifyMVarMasked typeCacheState (\state -> pure (Closed, state))
    case previous of
        Open helper NativeTypes{nativeOwned} ->
            mapM_ destroyLogicalType nativeOwned `finally` destroy c_duckdb_v2_connection_destroy helper
        _ -> pure ()

{- | Borrow the persistent helper connection. Calls execute in sequence.
Do not enter this cache again from the supplied action.
-}
withTypeConnection :: TypeCache -> (DuckDBV2ConnectionHandle -> IO a) -> IO a
withTypeConnection cache action = withNativeTypes cache (\helper _ -> action helper)

-- | Construct an owned type. Resolve configured CRS leaves at every depth.
cachedLogicalType :: TypeCache -> LogicalTypeRep -> IO DuckDBV2LogicalTypeHandle
cachedLogicalType cache@TypeCache{typeCacheConnection} = logicalTypeFromRepWith typeCacheConnection resolve
  where
    copy = fmap Just . output . c_duckdb_v2_logical_type_copy
    resolve (LogicalTypeGeometry (Just crs)) =
        withNativeTypes cache \_ NativeTypes{nativeGeometry} -> maybe (pure Nothing) copy (Map.lookup crs nativeGeometry)
    resolve _ = pure Nothing

-- | Initialize native state once. Keep it after an action throws.
withNativeTypes :: TypeCache -> (DuckDBV2ConnectionHandle -> NativeTypes -> IO a) -> IO a
withNativeTypes TypeCache{typeCacheInstance, typeCacheCRS, typeCacheState} action = mask \restore -> do
    outcome <- modifyMVarMasked typeCacheState \state -> do
        (helper, types) <- case state of
            Closed -> throwIO (SQLError "duckdb-simple: type cache is closed" Nothing Nothing)
            Open helper types -> pure (helper, types)
            Unopened -> do
                helper <- output (c_duckdb_v2_connection_create typeCacheInstance)
                types <- readNativeTypes helper typeCacheCRS `onException` destroy c_duckdb_v2_connection_destroy helper
                pure (helper, types)
        result <- try (restore (action helper types))
        pure (Open helper types, result)
    either (throwIO :: SomeException -> IO a) pure outcome

-- | Read SQL-derived GEOMETRY types without using the caller.
readNativeTypes :: DuckDBV2ConnectionHandle -> [Text] -> IO NativeTypes
readNativeTypes _ [] = pure (NativeTypes Map.empty [])
readNativeTypes connection crss = do
    let sql = "SELECT " <> Text.intercalate ", " ["system.main.ST_SetCRS('POINT EMPTY'::GEOMETRY, ?)" | _ <- crss]
        ownValue value = bracket (withText value (\text -> factoryOutput connection (\factory -> c_duckdb_v2_value_create_varchar factory text))) (destroy c_duckdb_v2_value_destroy)
    TextForeign.withCString sql \text ->
        bracket (output (c_duckdb_v2_parse_sql connection (ConstPtr text))) (destroy c_duckdb_v2_statement_iterator_destroy) \iterator ->
            bracket (output (c_duckdb_v2_statement_iterator_next iterator)) (destroy c_duckdb_v2_sql_statement_destroy) \parsed ->
                withMany ownValue crss \values -> withArray values \valuePtr ->
                    bracket (output (c_duckdb_v2_statement_execute connection parsed (ConstPtr nullPtr) (ConstPtr valuePtr) (fromIntegral (length values)))) (destroy c_duckdb_v2_result_destroy) \result ->
                        bracket (output (c_duckdb_v2_result_fetch_chunk result)) (destroy c_duckdb_v2_data_chunk_destroy) \chunk -> do
                            when (chunk == nullHandle) $ throwIO (userError "duckdb-simple: the type query returned no row")
                            owned <- columnTypes chunk (length crss)
                            flip onException (mapM_ destroyLogicalType owned) do
                                reported <- mapM logicalTypeToRep owned
                                when (any (\rep -> case rep of LogicalTypeGeometry (Just _) -> False; _ -> True) reported) $
                                    throwIO (userError "duckdb-simple: the type query did not preserve a configured GEOMETRY CRS")
                                pure
                                    NativeTypes
                                        { nativeGeometry = Map.fromList (concat [(crs, logical) : [(normalized, logical) | LogicalTypeGeometry (Just normalized) <- [rep]] | (crs, rep, logical) <- zip3 crss reported owned])
                                        , nativeOwned = owned
                                        }

-- | Take owned vector types. Destroy earlier types if an accessor fails.
columnTypes :: DuckDBV2DataChunkHandle -> Int -> IO [DuckDBV2LogicalTypeHandle]
columnTypes chunk count = go 0
  where
    go index
        | index == count = pure []
        | otherwise = do
            vector <- output (c_duckdb_v2_data_chunk_get_vector chunk (fromIntegral index))
            logical <- output (c_duckdb_v2_vector_get_logical_type vector)
            (logical :) <$> go (index + 1) `onException` destroyLogicalType logical
