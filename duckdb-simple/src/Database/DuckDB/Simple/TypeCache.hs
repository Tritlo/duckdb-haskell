{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- |
-- Module      : Database.DuckDB.Simple.TypeCache
-- Description : Connection-scoped construction of native logical types.
--
-- The type cache of a connection holds one GEOMETRY type for each configured
-- CRS name. It also holds the helper connection for VARIANT reads. See
-- Note [CRS types of a connection].
module Database.DuckDB.Simple.TypeCache (
  TypeCache,
  typeCacheConnection,
  typeCacheCasts,
  defaultGeometryCRS,
  createTypeCache,
  destroyTypeCache,
  cachedLogicalType,
  withVariantState,
) where

import Control.Concurrent.MVar (MVar, modifyMVarMasked, newMVar)
import Control.Exception (SomeException, bracket, finally, mask, onException, throwIO, try, uninterruptibleMask_)
import Control.Monad (when)
import qualified Data.ByteString as BS
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
  { typeCacheConnection :: DuckDBV2ConnectionHandle
  , typeCacheInstance :: DuckDBV2InstanceHandle
  , typeCacheCasts :: DuckDBV2FactoryHandle
  -- ^ The factory of the instance. The library runs its own casts with it.
  --     It is valid for as long as the instance is. See
  --     Note [User casts and the casts of the library] in
  --     "Database.DuckDB.Simple.Materialize".
  , typeCacheGeometry :: NativeTypes
  -- ^ The types of the configured CRS names. The map does not change after
  --     'createTypeCache'.
  , typeCacheState :: MVar CacheState
  }

-- | The helper connection for VARIANT reads and the VARIANT decoder state that
-- borrows it. They open at the first read. The cache owns the connection. See
-- Note [VARIANT decoding] in "Database.DuckDB.Simple.VariantCodec".
data CacheState = Unopened | Open DuckDBV2ConnectionHandle VariantState | Closed

-- | Keep each owned type once. CRS aliases can refer to the same type. See
-- Note [CRS types of a connection].
data NativeTypes = NativeTypes
  { nativeGeometry :: Map Text DuckDBV2LogicalTypeHandle
  , nativeOwned :: [DuckDBV2LogicalTypeHandle]
  }

-- | The default CRS used by connection options.
defaultGeometryCRS :: [Text]
defaultGeometryCRS = ["OGC:CRS84"]

{- Note [CRS types of a connection]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A GEOMETRY type can have a CRS. The connection option connectionGeometryCRS
gives a list of CRS names, and defaultGeometryCRS is the default list. The type
cache of a connection holds one type for each configured name: the type of
direct construction, or a SQL-derived type for a name that direct construction
does not keep.

- Direct construction (directGeometry). DuckDB constructs a GEOMETRY type with
  a recognized CRS or a complete CRS definition directly. For another name, the
  construction raises SQLError, because DuckDB rejects the name or drops the
  CRS. The type binder rejects a custom CRS name.

- The query (readNativeTypes). The SQL function ST_SetCRS keeps a custom CRS
  name. One statement reads the types of the names that direct construction
  does not keep: it applies ST_SetCRS to an empty point for each name. The
  statement runs on the new caller connection when the connection opens,
  before any statement of the caller runs. A connection whose names DuckDB
  constructs directly runs no statement. The default list OGC:CRS84 is such a
  list. geometryTypes raises an error if a type has no CRS. A name that DuckDB
  drops, for example a blank name, fails the open.

- What the cache keeps (NativeTypes). The map has an entry for each configured
  name, and an entry for the CRS text that DuckDB reports for its type. Thus
  two names can refer to the same type, and the cache keeps each owned type
  once. The map does not change after createTypeCache, so cachedLogicalType
  reads it without a lock. destroyTypeCache destroys each owned type once.

- The use (cachedLogicalType). The resolver of cachedLogicalType copies a
  configured CRS type at each depth of a type tree (logicalTypeFromRepWith).
  The builder of a statement uses this resolver (ValueBuilder and
  bindFieldBinding), and a bind copies the type. A type with a CRS that is not
  in the list uses direct construction.

The helper connection of the cache serves VARIANT reads only. Its opening runs
no CRS query.
-}

-- | Create the cache on a new caller connection, before any statement of the
-- caller runs. Construct each configured CRS directly, and keep each type that
-- has the CRS. For the other names, query the types on the caller connection.
-- If a step fails, destroy the types that the earlier steps took. The caller
-- keeps the instance and connection alive. The helper connection opens later,
-- at the first VARIANT read. See Note [CRS types of a connection].
createTypeCache :: DuckDBV2InstanceHandle -> DuckDBV2ConnectionHandle -> [Text] -> IO TypeCache
createTypeCache instance_ connection crss = do
  when (any Text.null crss || any (Text.any (== '\0')) crss) $
    throwInvalidArgument "a GEOMETRY CRS must be nonempty and must not contain NUL"
  let names = nub crss
  casts <- withInstanceFactory instance_ pure
  direct <- acquireItems (mapM_ destroyLogicalType) [directGeometry connection crs | crs <- names]
  let kept = [(crs, logical) | (crs, Just logical) <- zip names direct]
  flip onException (releaseItems destroyLogicalType (map snd kept)) do
    keptTypes <- geometryTypes (map fst kept) (map snd kept)
    queried <- readNativeTypes connection [crs | (crs, Nothing) <- zip names direct]
    flip onException (releaseItems destroyLogicalType (nativeOwned queried)) do
      let geometry = NativeTypes (nativeGeometry keptTypes <> nativeGeometry queried) (nativeOwned keptTypes <> nativeOwned queried)
      TypeCache connection instance_ casts geometry <$> newMVar Unopened

-- | Construct a GEOMETRY type with a CRS directly. Return 'Nothing' when DuckDB
-- rejects the name or drops the CRS. 'logicalTypeFromRep' raises t'SQLError' in
-- both cases and destroys a type that lost its CRS. See
-- Note [CRS types of a connection].
directGeometry :: DuckDBV2ConnectionHandle -> Text -> IO (Maybe DuckDBV2LogicalTypeHandle)
directGeometry connection crs = withConnectionFactory connection \factory ->
  either (const Nothing) Just
    <$> (try (logicalTypeFromRep factory (LogicalTypeGeometry (Just crs))) :: IO (Either SQLError DuckDBV2LogicalTypeHandle))

-- | Close the helper after all users finish. Destroy the VARIANT state before
-- the helper connection. Destroy each cached type once, even if an earlier
-- destroy raises an error. A second call does nothing.
destroyTypeCache :: TypeCache -> IO ()
destroyTypeCache TypeCache{typeCacheGeometry, typeCacheState} = uninterruptibleMask_ do
  previous <- modifyMVarMasked typeCacheState (\state -> pure (Closed, state))
  let releaseTypes = releaseItems destroyLogicalType (nativeOwned typeCacheGeometry)
  case previous of
    Open helper variant -> ((destroyVariantState variant `finally` destroy c_duckdb_v2_connection_destroy helper) `finally` releaseTypes)
    Unopened -> releaseTypes
    Closed -> pure ()

-- | Construct an owned type. Resolve configured CRS leaves at every depth.
-- Copy a configured CRS type from the immutable map, without a lock. See
-- Note [CRS types of a connection].
cachedLogicalType :: TypeCache -> LogicalTypeRep -> IO DuckDBV2LogicalTypeHandle
cachedLogicalType TypeCache{typeCacheConnection, typeCacheGeometry = NativeTypes{nativeGeometry}} rep =
  withConnectionFactory typeCacheConnection \factory -> logicalTypeFromRepWith factory resolve rep
  where
    copy = fmap Just . output . c_duckdb_v2_logical_type_copy
    resolve (LogicalTypeGeometry (Just crs)) = maybe (pure Nothing) copy (Map.lookup crs nativeGeometry)
    resolve _ = pure Nothing

-- | Borrow the VARIANT decoder state of the persistent helper connection. Open
-- the connection once, and keep it after an action throws. Calls execute in
-- sequence. Do not enter this cache again from the supplied action. See
-- Note [VARIANT decoding] in "Database.DuckDB.Simple.VariantCodec".
withVariantState :: TypeCache -> (VariantState -> IO a) -> IO a
withVariantState TypeCache{typeCacheInstance, typeCacheCasts, typeCacheState} action = mask \restore -> do
  outcome <- modifyMVarMasked typeCacheState \state -> do
    (helper, variant) <- case state of
      Closed -> throwIO (SQLError "duckdb-simple: type cache is closed" Nothing Nothing Nothing)
      Open helper variant -> pure (helper, variant)
      Unopened -> do
        helper <- output (c_duckdb_v2_connection_create typeCacheInstance)
        (,) helper <$> newVariantState helper typeCacheCasts
    result <- try (restore (action variant))
    pure (Open helper variant, result)
  either (throwIO :: SomeException -> IO a) pure outcome

-- | Read the SQL-derived GEOMETRY types of custom CRS names. Run no statement
-- for an empty list. See Note [CRS types of a connection]. The statement binds
-- the empty point as a BLOB value and each name as a VARCHAR value. Each value
-- has the type of its parameter, so the statement has no cast. See
-- Note [User casts and the casts of the library] in
-- "Database.DuckDB.Simple.Materialize".
readNativeTypes :: DuckDBV2ConnectionHandle -> [Text] -> IO NativeTypes
readNativeTypes _ [] = pure (NativeTypes Map.empty [])
readNativeTypes connection crss = do
  -- See Note [SQL text of the library] in "Database.DuckDB.Simple.Native".
  let sql = "SELECT " <> Text.intercalate ", " ["system.main.ST_SetCRS(system.main.ST_GeomFromWKB(?), ?)" | _ <- crss]
      textValue value = withText value (\text -> factoryOutput connection (\factory -> c_duckdb_v2_value_create_varchar factory text))
      emptyPoint = withBytes emptyPointWKB (\bytes -> factoryOutput connection (\factory -> c_duckdb_v2_value_create_blob factory bytes))
  withSQLStatement connection sql \parsed ->
    withCreatedValues (concat [[emptyPoint, textValue crs] | crs <- crss]) \values -> withArray values \valuePtr ->
      bracket
        (output (c_duckdb_v2_statement_execute connection parsed (ConstPtr nullPtr) (ConstPtr valuePtr) (fromIntegral (length values))))
        (destroy c_duckdb_v2_result_destroy)
        \result ->
          bracket (output (c_duckdb_v2_result_fetch_chunk result)) (destroy c_duckdb_v2_data_chunk_destroy) \chunk -> do
            when (chunk == nullHandle) $ throwInternalError "the type query returned no row"
            owned <- columnTypes chunk (length crss)
            geometryTypes crss owned `onException` releaseItems destroyLogicalType owned

-- | The WKB of POINT EMPTY: a point with NaN coordinates.
emptyPointWKB :: BS.ByteString
emptyPointWKB = BS.pack [1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 248, 127, 0, 0, 0, 0, 0, 0, 248, 127]

-- | Map each configured name, and the CRS text that DuckDB reports for its
-- type, to the type. Raise an error if a type has no CRS. The caller keeps the
-- ownership of the types. See Note [CRS types of a connection].
geometryTypes :: [Text] -> [DuckDBV2LogicalTypeHandle] -> IO NativeTypes
geometryTypes crss owned = do
  reported <- mapM logicalTypeToRep owned
  when (any (\rep -> case rep of LogicalTypeGeometry (Just _) -> False; _ -> True) reported) $
    throwInvalidArgument "the type query did not preserve a configured GEOMETRY CRS"
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
