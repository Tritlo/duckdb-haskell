{-# LANGUAGE BlockArguments #-}

-- | Structured logical types, values, and scoped native type construction.
module Database.DuckDB.Simple.LogicalRep (
    StructField (..),
    StructValue (..),
    UnionMemberType (..),
    UnionValue (..),
    LogicalTypeRep (..),
    structValueTypeRep,
    unionValueTypeRep,
    logicalTypeToRep,
    logicalTypeFromRep,
    withLogicalType,
    destroyLogicalType,
) where

import Control.Exception (bracket)
import Database.DuckDB.FFI (DuckDBLogicalType)
import Database.DuckDB.Simple.Internal (Connection, withConnectionHandle)
import Database.DuckDB.Simple.LogicalRep.Internal
import Database.DuckDB.Simple.TypeContext (logicalTypeForConnection)

{- | Construct a native logical type on an open connection.
The callback borrows the type. This function destroys it when the callback
returns or throws. Use this function for types that require SQL metadata,
such as geometry with a CRS. The connection supplies its active catalog.
-}
withLogicalType :: Connection -> LogicalTypeRep -> (DuckDBLogicalType -> IO a) -> IO a
withLogicalType connection rep action =
    withConnectionHandle connection \native ->
        bracket (logicalTypeForConnection native rep) destroyLogicalType action
