{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

-- | V2 configuration values, declared scopes, defaults, and aliases.
module Database.DuckDB.Simple.Config (
    ConfigFlag (..),
    ConfigValue (..),
    ConfigOption (..),
    listConfigFlags,
    listConfigOptions,
    listInstanceOptions,
    getConfigOption,
    setConfigOption,
) where

import Control.Exception (bracket)
import Control.Monad (forM, when)
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.DuckDB.Simple (withConnection)
import Database.DuckDB.Simple.Internal (Connection, Instance, throwRegistrationError, withConnectionHandle, withInstanceHandle)
import Database.DuckDB.Simple.Native

-- | The canonical name and description of a configuration option.
data ConfigFlag = ConfigFlag
    { configFlagName :: !Text
    , configFlagDescription :: !Text
    }
    deriving (Eq, Show)

-- | An effective option value and its permitted setting scope.
data ConfigValue = ConfigValue
    { configValueText :: !Text
    , configValueScope :: !(Maybe DuckDBV2OptionTargetScope)
    }
    deriving (Eq, Show)

-- | A copied option descriptor. Native defaults and aliases can be empty.
data ConfigOption = ConfigOption
    { configOptionName :: !Text
    , configOptionDescription :: !Text
    , configOptionSetting :: !Text
    , configOptionDefault :: !Text
    , configOptionScope :: !DuckDBV2OptionTargetScope
    , configOptionAliases :: ![Text]
    }
    deriving (Eq, Show)

-- | List built-in configuration flags using a temporary connection.
listConfigFlags :: IO [ConfigFlag]
listConfigFlags = withConnection ":memory:" \conn ->
    map (\option -> ConfigFlag (configOptionName option) (configOptionDescription option)) <$> listConfigOptions conn

-- | List options with effective values from a live connection.
listConfigOptions :: Connection -> IO [ConfigOption]
listConfigOptions conn = withConnectionHandle conn \handle -> do
    count <- output (c_duckdb_v2_connection_get_option_count handle)
    forM (indices count) \index ->
        bracket (output (c_duckdb_v2_connection_get_option_by_index handle index)) (destroy c_duckdb_v2_option_destroy) copyOption

-- | List options and global values from an instance.
listInstanceOptions :: Instance -> IO [ConfigOption]
listInstanceOptions instance_ = withInstanceHandle instance_ \handle -> do
    count <- output (c_duckdb_v2_instance_get_option_count handle)
    forM (indices count) \index ->
        bracket (output (c_duckdb_v2_instance_get_option_by_index handle index)) (destroy c_duckdb_v2_option_destroy) copyOption

-- | Read an effective option by its canonical name or an alias.
getConfigOption :: Connection -> Text -> IO (Maybe ConfigValue)
getConfigOption conn name = do
    when (Text.any (== '\0') name) $ throwRegistrationError "config option name contains NUL"
    options <- listConfigOptions conn
    pure $ do
        option <- find (\value -> configOptionName value == name || name `elem` configOptionAliases value) options
        let scope = configOptionScope option
        pure (ConfigValue (configOptionSetting option) (if scope == DuckDBV2OptionTargetScopeUnknown then Nothing else Just scope))

-- | Write an option at automatic, global, or local scope.
setConfigOption :: Connection -> DuckDBV2SettingScope -> Text -> Text -> IO ()
setConfigOption conn scope name value = withConnectionHandle conn \handle ->
    withIdentifier name \key -> withText value \setting ->
        checked (c_duckdb_v2_connection_set_option handle key setting scope)

-- | Copy a descriptor while its native strings are alive.
copyOption :: DuckDBV2OptionHandle -> IO ConfigOption
copyOption handle = do
    name <- output (c_duckdb_v2_option_get_name handle) >>= readIdentifier
    description <- output (c_duckdb_v2_option_get_description handle) >>= readText
    setting <- output (c_duckdb_v2_option_get_setting handle) >>= readText
    defaultSetting <- output (c_duckdb_v2_option_get_default_setting handle) >>= readText
    scope <- output (c_duckdb_v2_option_get_target_scope handle)
    count <- output (c_duckdb_v2_option_get_alias_count handle)
    aliases <- forM (indices count) \index -> output (c_duckdb_v2_option_get_alias handle index) >>= readIdentifier
    pure (ConfigOption name description setting defaultSetting scope aliases)

-- | Enumerate a native count without unsigned underflow for empty input.
indices :: DuckDBIdx -> [DuckDBIdx]
indices count = if count == 0 then [] else [0 .. count - 1]
