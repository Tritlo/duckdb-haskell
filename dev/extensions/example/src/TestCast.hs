{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : TestCast
-- Description : A cast from GEOMETRY to BLOB that only the test suite registers.
--
-- The GEOMETRY reader of the library must not run a cast that another party
-- registers. The test suite sets @DUCKDB_HASKELL_DEMO_GEOMETRY_CAST@ before
-- @LOAD@. Only then does the extension register a native cast from GEOMETRY to
-- BLOB that writes NULL and counts its calls, and the function
-- @haskell_geometry_cast_calls()@ that gives the count. A normal load registers
-- neither, because a cast between two built-in types changes the plain SQL of
-- the host.
module TestCast (register) where

import Control.Monad (forM_)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Int (Int64)
import Database.DuckDB.FFI.V2
import Database.DuckDB.Simple.Extension (Extension)
import qualified Database.DuckDB.Simple.Extension as Extension
import Database.DuckDB.Simple.V2.Registration (
  TypeDeclaration (..),
  checked,
  output,
  registerCastWith,
  withRegistrationFactory,
  withTypeDeclaration,
 )
import System.Environment (lookupEnv)

-- | Register the cast and its count function when the test suite asks for
-- them. Do nothing otherwise.
register :: Extension -> IO ()
register extension =
  lookupEnv "DUCKDB_HASKELL_DEMO_GEOMETRY_CAST" >>= mapM_ \_ -> do
    calls <- newIORef (0 :: Int64)
    let target = Extension.extensionTarget extension
    withRegistrationFactory target \factory ->
      registerCastWith
        target
        ( \function -> do
            withTypeDeclaration factory (TypeExpression "GEOMETRY") $ checked . c_duckdb_v2_cast_function_set_source_type function
            withTypeDeclaration factory (TypeExpression "BLOB") $ checked . c_duckdb_v2_cast_function_set_target_type function
        )
        \info _ -> do
          atomicModifyIORef' calls (\count -> (count + 1, ()))
          rows <- output (c_duckdb_v2_cast_function_exec_get_row_count info)
          vector <- output (c_duckdb_v2_cast_function_exec_get_output info)
          checked (c_duckdb_v2_vector_set_size vector rows)
          checked (c_duckdb_v2_vector_flatten vector)
          forM_ [0 .. fromIntegral rows - 1 :: Int] \row -> checked (c_duckdb_v2_vector_set_null vector (fromIntegral row))
    Extension.createFunction extension "haskell_geometry_cast_calls" (readIORef calls)
