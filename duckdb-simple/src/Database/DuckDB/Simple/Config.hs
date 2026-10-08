{-# LANGUAGE BlockArguments #-}

{- |
Module      : Database.DuckDB.Simple.Config
Description : High-level helpers for DuckDB 1.5 configuration inspection.
-}
module Database.DuckDB.Simple.Config (
    ConfigFlag (..),
    ConfigValue (..),
    listConfigFlags,
    getConfigOption,
) where

import Control.Exception (bracket)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Foreign as TextForeign
import Database.DuckDB.FFI
import Database.DuckDB.Simple.Internal (Connection, destroyValue, peekUtf8CString, throwRegistrationError, withClientContext)
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (castPtr, nullPtr)
import Foreign.Storable (peek, poke)

-- | Static metadata describing a known DuckDB configuration flag.
data ConfigFlag = ConfigFlag
    { configFlagName :: !Text
    , configFlagDescription :: !Text
    }
    deriving (Eq, Show)

-- | The current value of a configuration option plus its scope, when available.
data ConfigValue = ConfigValue
    { configValueText :: !Text
    , configValueScope :: !(Maybe DuckDBConfigOptionScope)
    }
    deriving (Eq, Show)

-- | List all configuration flags known to the linked DuckDB runtime.
listConfigFlags :: IO [ConfigFlag]
listConfigFlags = do
    count <- c_duckdb_config_count
    let indices = [0 .. fromIntegral count - 1] :: [Int]
    mapM fetchFlag indices
  where
    fetchFlag idx =
        alloca \namePtr ->
            alloca \descPtr -> do
                rc <- c_duckdb_get_config_flag (fromIntegral idx) namePtr descPtr
                if rc /= DuckDBSuccess
                    then pure ConfigFlag{configFlagName = Text.pack (show idx), configFlagDescription = Text.pack ""}
                    else do
                        name <- peek namePtr >>= peekUtf8CString
                        description <- peek descPtr >>= peekUtf8CString
                        pure ConfigFlag{configFlagName = name, configFlagDescription = description}

-- | Read a configuration option from a live connection's client context.
getConfigOption :: Connection -> Text -> IO (Maybe ConfigValue)
getConfigOption conn name
    | Text.any (== '\0') name = throwRegistrationError "config option name contains NUL"
    | otherwise =
        withClientContext conn \ctx ->
            TextForeign.withCString name \cName ->
                alloca \scopePtr -> do
                    poke scopePtr DuckDBConfigOptionScopeInvalid
                    bracket
                        (c_duckdb_client_context_get_config_option ctx (ConstPtr cName) scopePtr)
                        destroyValue
                        \duckValue ->
                            if duckValue == DuckDBValue nullPtr
                                then pure Nothing
                                else do
                                    rendered <-
                                        bracket
                                            (c_duckdb_get_varchar duckValue)
                                            (c_duckdb_free . castPtr)
                                            \strPtr ->
                                                if strPtr == nullPtr
                                                    then pure Text.empty
                                                    else peekUtf8CString strPtr
                                    scope <- peek scopePtr
                                    pure $
                                        Just
                                            ConfigValue
                                                { configValueText = rendered
                                                , configValueScope =
                                                    if scope == DuckDBConfigOptionScopeInvalid
                                                        then Nothing
                                                        else Just scope
                                                }
