{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

-- | V2 access to SQL NULL and explicit errors for unavailable VARIANT payloads.
module Database.DuckDB.Simple.VariantCodec (
    decodeVariant,
    prepareVariantDecoder,
) where

import Control.Exception (bracket, throwIO)
import Database.DuckDB.Simple.FromField (FieldValue (..))
import Database.DuckDB.Simple.Native

-- | Report the V2 boundary for automatic VARIANT payload decoding.
decodeVariant :: DuckDBV2VectorHandle -> Int -> IO FieldValue
decodeVariant vector row = prepareVariantDecoder vector >>= ($ row)

-- | V2 does not expose the payload type or children of a VARIANT value.
prepareVariantDecoder :: DuckDBV2VectorHandle -> IO (Int -> IO FieldValue)
prepareVariantDecoder vector = pure \row ->
    bracket (output (c_duckdb_v2_vector_get_value vector (fromIntegral row))) (destroy c_duckdb_v2_value_destroy) \value -> do
        isNull <- output (c_duckdb_v2_value_is_null value)
        if isNull /= 0
            then pure FieldNull
            else
                throwIO
                    SQLError
                        { sqlErrorMessage = "duckdb-simple: automatic VARIANT decoding is unavailable in DuckDB's V2 API; cast the value to an explicit SQL type"
                        , sqlErrorType = Nothing
                        , sqlErrorQuery = Nothing
                        }
