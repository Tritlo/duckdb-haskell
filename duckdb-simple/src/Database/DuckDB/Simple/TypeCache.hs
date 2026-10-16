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
) where

import Control.Exception (throwIO)
import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.DuckDB.Simple.LogicalRep (LogicalTypeRep, logicalTypeFromRep)
import Database.DuckDB.Simple.Native

-- | The connection used to construct parameter types and values.
newtype TypeCache = TypeCache {typeCacheConnection :: DuckDBV2ConnectionHandle}

-- | The default CRS used by connection options.
defaultGeometryCRS :: [Text]
defaultGeometryCRS = ["OGC:CRS84"]

-- | Use the caller's connection. Reject invalid CRS configuration.
createTypeCache :: DuckDBV2ConnectionHandle -> [Text] -> IO TypeCache
createTypeCache connection crss = do
    when (any Text.null crss || any (Text.any (== '\0')) crss) $
        throwIO (userError "duckdb-simple: a GEOMETRY CRS must be nonempty and must not contain NUL")
    pure (TypeCache connection)

-- | Release cache state. The caller owns the connection.
destroyTypeCache :: TypeCache -> IO ()
destroyTypeCache _ = pure ()

-- | Construct an owned type with the connection's native catalog.
cachedLogicalType :: TypeCache -> LogicalTypeRep -> IO DuckDBV2LogicalTypeHandle
cachedLogicalType TypeCache{typeCacheConnection} = logicalTypeFromRep typeCacheConnection
