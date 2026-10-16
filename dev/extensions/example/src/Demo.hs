{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Register typed Haskell functions with the loading DuckDB host.
module Demo where

import Control.Exception (throwIO)
import Data.Int (Int64)
import Database.DuckDB.FFI.V2 (DuckDBV2ContextHandle (..), DuckDBV2ExtensionHandle (..))
import Database.DuckDB.Simple.Extension (Extension, ExtensionEntry)
import qualified Database.DuckDB.Simple.Extension as Extension

-- | Export the registration action called by the C load entrypoint.
initialize :: ExtensionEntry
initialize = Extension.runExtension register

foreign export ccall "duckdb_haskell_demo_register" initialize :: ExtensionEntry

-- | Register scalar results, nullable arithmetic, and a callback error.
register :: Extension -> IO ()
register extension = do
    Extension.createFunction extension "haskell_answer" (42 :: Int64)
    Extension.createFunction extension "haskell_add_one" addOne
    Extension.createFunction extension "haskell_fail" (throwIO (userError "Haskell callback failure") :: IO Int64)

-- | Increment a BIGINT argument. Keep NULL values and report overflow.
addOne :: Maybe Int64 -> IO (Maybe Int64)
addOne Nothing = pure Nothing
addOne (Just value)
    | value == maxBound = throwIO (userError "Haskell BIGINT overflow")
    | otherwise = pure (Just (value + 1))
