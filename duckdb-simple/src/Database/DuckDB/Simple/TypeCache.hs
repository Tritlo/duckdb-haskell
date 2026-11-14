{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Database.DuckDB.Simple.TypeCache
-- Description : Connection-scoped construction of native logical types.
--
-- DuckDB constructs a GEOMETRY type with a recognized CRS or a complete CRS
-- definition directly. The type binder rejects a custom CRS name, but the SQL
-- function @ST_SetCRS@ keeps it. The cache therefore holds a SQL-derived type
-- only for each configured name that direct construction does not keep.
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
import Control.Monad (filterM, when)
import Data.Either (isRight)
import Data.List (nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Database.DuckDB.Simple.LogicalRep (LogicalTypeRep (..))
import Database.DuckDB.Simple.Native
import Database.DuckDB.Simple.V2.LogicalRep (logicalTypeFromRep, logicalTypeFromRepWith, logicalTypeToRep)
import Database.DuckDB.Simple.VariantCodec (VariantState, destroyVariantState, newVariantState)
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (nullPtr)

-- | Owned helper state. The caller connection owns this cache.
data TypeCache = TypeCache
  { typeCacheConnection :: !DuckDBV2ConnectionHandle
  , typeCacheInstance :: !DuckDBV2InstanceHandle
  , typeCacheGeometry :: !NativeTypes
  -- ^ The SQL-derived types of the configured custom CRS names. The map does
  --     not change after 'createTypeCache'.
  , typeCacheVariant :: !VariantState
  -- ^ The VARIANT decoder state of the helper connection. Use it only inside
  --     'withHelperConnection', which holds the cache 'MVar'.
  , typeCacheState :: !(MVar CacheState)
  }

-- | The helper connection for VARIANT reads. It opens at the first read.
data CacheState = Unopened | Open !DuckDBV2ConnectionHandle | Closed

-- | Keep each owned type once. CRS aliases can refer to the same type.
data NativeTypes = NativeTypes
  { nativeGeometry :: !(Map Text DuckDBV2LogicalTypeHandle)
  , nativeOwned :: ![DuckDBV2LogicalTypeHandle]
  }

-- | The default CRS used by connection options.
defaultGeometryCRS :: [Text]
defaultGeometryCRS = ["OGC:CRS84"]

-- | Create the cache on a new caller connection, before any statement of the
-- caller runs. Construct each configured CRS directly. For the names that
-- direct construction does not keep, query the types on the caller connection.
-- The caller keeps the instance and connection alive. The helper connection
-- opens later, at the first VARIANT read.
createTypeCache :: DuckDBV2InstanceHandle -> DuckDBV2ConnectionHandle -> [Text] -> IO TypeCache
createTypeCache instance_ connection crss = do
  when (any Text.null crss || any (Text.any (== '\0')) crss) $
    throwIO (userError "duckdb-simple: a GEOMETRY CRS must be nonempty and must not contain NUL")
  custom <- filterM (fmap not . keepsCRS connection) (nub crss)
  geometry <- readNativeTypes connection custom
  flip onException (releaseItems destroyLogicalType (nativeOwned geometry)) $
    TypeCache connection instance_ geometry <$> newVariantState <*> newMVar Unopened

-- | Tell if direct type construction keeps a CRS. Destroy the type at once.
-- The construction raises 'SQLError' when DuckDB rejects the name, and also
-- when DuckDB drops the CRS.
keepsCRS :: DuckDBV2ConnectionHandle -> Text -> IO Bool
keepsCRS connection crs = withConnectionFactory connection \factory ->
  isRight
    <$> (try (bracket (logicalTypeFromRep factory (LogicalTypeGeometry (Just crs))) destroyLogicalType (const (pure ()))) :: IO (Either SQLError ()))

-- | Close the helper after all users finish. Destroy the VARIANT state before
-- the helper connection. Destroy each cached type once, also if a destroy
-- raises an error. A second call does nothing.
destroyTypeCache :: TypeCache -> IO ()
destroyTypeCache TypeCache{typeCacheGeometry, typeCacheVariant, typeCacheState} = uninterruptibleMask_ do
  previous <- modifyMVarMasked typeCacheState (\state -> pure (Closed, state))
  let releaseTypes = releaseItems destroyLogicalType (nativeOwned typeCacheGeometry)
  case previous of
    Open helper -> ((destroyVariantState typeCacheVariant `finally` destroy c_duckdb_v2_connection_destroy helper) `finally` releaseTypes)
    Unopened -> releaseTypes
    Closed -> pure ()

-- | Borrow the persistent helper connection and its VARIANT state. Calls
-- execute in sequence. Do not enter this cache again from the supplied action.
withTypeConnection :: TypeCache -> (DuckDBV2ConnectionHandle -> VariantState -> IO a) -> IO a
withTypeConnection cache@TypeCache{typeCacheVariant} action = withHelperConnection cache (`action` typeCacheVariant)

-- | Construct an owned type. Resolve configured CRS leaves at every depth.
-- Read the immutable map of custom CRS types without a lock.
cachedLogicalType :: TypeCache -> LogicalTypeRep -> IO DuckDBV2LogicalTypeHandle
cachedLogicalType TypeCache{typeCacheConnection, typeCacheGeometry = NativeTypes{nativeGeometry}} rep =
  withConnectionFactory typeCacheConnection \factory -> logicalTypeFromRepWith factory resolve rep
  where
    copy = fmap Just . output . c_duckdb_v2_logical_type_copy
    resolve (LogicalTypeGeometry (Just crs)) = maybe (pure Nothing) copy (Map.lookup crs nativeGeometry)
    resolve _ = pure Nothing

-- | Open the helper connection once. Keep it after an action throws.
withHelperConnection :: TypeCache -> (DuckDBV2ConnectionHandle -> IO a) -> IO a
withHelperConnection TypeCache{typeCacheInstance, typeCacheState} action = mask \restore -> do
  outcome <- modifyMVarMasked typeCacheState \state -> do
    helper <- case state of
      Closed -> throwIO (SQLError "duckdb-simple: type cache is closed" Nothing Nothing)
      Open helper -> pure helper
      Unopened -> output (c_duckdb_v2_connection_create typeCacheInstance)
    result <- try (restore (action helper))
    pure (Open helper, result)
  either (throwIO :: SomeException -> IO a) pure outcome

-- | Read the SQL-derived GEOMETRY types of custom CRS names. Run no statement
-- for an empty list.
readNativeTypes :: DuckDBV2ConnectionHandle -> [Text] -> IO NativeTypes
readNativeTypes _ [] = pure (NativeTypes Map.empty [])
readNativeTypes connection crss = do
  let sql = "SELECT " <> Text.intercalate ", " ["system.main.ST_SetCRS('POINT EMPTY'::GEOMETRY, ?)" | _ <- crss]
      textValue value = withText value (\text -> factoryOutput connection (\factory -> c_duckdb_v2_value_create_varchar factory text))
  withSQLStatement connection sql \parsed ->
    withCreatedValues (map textValue crss) \values -> withArray values \valuePtr ->
      bracket
        (output (c_duckdb_v2_statement_execute connection parsed (ConstPtr nullPtr) (ConstPtr valuePtr) (fromIntegral (length values))))
        (destroy c_duckdb_v2_result_destroy)
        \result ->
          bracket (output (c_duckdb_v2_result_fetch_chunk result)) (destroy c_duckdb_v2_data_chunk_destroy) \chunk -> do
            when (chunk == nullHandle) $ throwIO (userError "duckdb-simple: the type query returned no row")
            owned <- columnTypes chunk (length crss)
            flip onException (mapM_ destroyLogicalType owned) do
              reported <- mapM logicalTypeToRep owned
              when (any (\rep -> case rep of LogicalTypeGeometry (Just _) -> False; _ -> True) reported) $
                throwIO (userError "duckdb-simple: the type query did not preserve a configured GEOMETRY CRS")
              pure
                NativeTypes
                  { nativeGeometry =
                      Map.fromList
                        ( concat
                            [ (crs, logical) : [(normalized, logical) | LogicalTypeGeometry (Just normalized) <- [rep]] | (crs, rep, logical) <- zip3 crss reported owned
                            ]
                        )
                  , nativeOwned = owned
                  }

-- | Take the owned type of each of the first vectors. Destroy the earlier
-- types if an accessor fails.
columnTypes :: DuckDBV2DataChunkHandle -> Int -> IO [DuckDBV2LogicalTypeHandle]
columnTypes chunk count =
  acquireItems
    destroyLogicalType
    [ output (c_duckdb_v2_data_chunk_get_vector chunk (fromIntegral index)) >>= output . c_duckdb_v2_vector_get_logical_type
    | index <- [0 .. count - 1]
    ]
