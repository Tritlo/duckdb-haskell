{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : TestUtils
-- Description : Assertions that several test modules share.
module TestUtils (assertFailureIO, filteredQueries, withConstCString, writeIntegers) where

import Control.Exception (SomeException, try)
import Control.Monad (forM_)
import Data.Int (Int64)
import Database.DuckDB.FFI.V2 (
  DuckDBV2VectorHandle,
  c_duckdb_v2_vector_flatten,
  c_duckdb_v2_vector_get_data_mutable,
  c_duckdb_v2_vector_set_size,
 )
import Database.DuckDB.Simple (Query)
import Database.DuckDB.Simple.V2.Registration (checked, output)
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.C.String (withCString)
import Foreign.C.Types (CChar)
import Foreign.Ptr (castPtr)
import Foreign.Storable (pokeElemOff)
import Test.Tasty.HUnit (Assertion, assertFailure)

-- | Require an exception without depending on native error text.
assertFailureIO :: IO a -> Assertion
assertFailureIO action = do
  result <- try (action >> pure ()) :: IO (Either SomeException ())
  case result of
    Left _ -> pure ()
    Right () -> assertFailure "expected an exception"

-- | Supply a constant C string for one native call.
withConstCString :: String -> (ConstPtr CChar -> IO a) -> IO a
withConstCString text action = withCString text (action . ConstPtr)

-- | Set the size of a BIGINT vector and write the values to its rows. The
-- function flattens the vector first, so that each row has its own slot: the
-- header gives the data pointer of a FLAT or CONSTANT vector, and a CONSTANT
-- vector has one value for all rows. A flatten does nothing for a FLAT vector.
writeIntegers :: DuckDBV2VectorHandle -> [Int64] -> IO ()
writeIntegers vector values = do
  checked (c_duckdb_v2_vector_set_size vector (fromIntegral (length values)))
  checked (c_duckdb_v2_vector_flatten vector)
  raw <- output (c_duckdb_v2_vector_get_data_mutable vector)
  forM_ (zip [0 ..] values) $ \(index, value) -> pokeElemOff (castPtr raw) index value

-- | Filtered queries over more than one chunk, each with the rows that DuckDB
-- gives. After the filter, most chunks of the second and the third query have
-- no row.
filteredQueries :: [(Query, [Int64])]
filteredQueries =
  [ ("SELECT i FROM range(100000) t(i) WHERE i < 3 OR i >= 99997", [0, 1, 2, 99997, 99998, 99999])
  , ("SELECT i FROM range(1000000) t(i) WHERE i % 100000 = 99999", [99999, 199999 .. 999999])
  , ("SELECT i FROM range(1000000) t(i) WHERE i = 999999", [999999])
  ]
