{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

-- | V2 configuration values, declared scopes, defaults, and aliases.
module Database.DuckDB.Simple.Config (
    ConfigFlag (..),
    ConfigValue (..),
    ConfigOption (..),
    SettingScope (..),
    listConfigFlags,
    listConfigOptions,
    listInstanceOptions,
    getConfigOption,
    setConfigOption,
    getConfigValue,
    getConfigDefaultValue,
    setConfigValue,
    CustomOption (..),
    customOption,
    registerCustomOption,
) where

import Control.Exception (bracket, catch, throwIO)
import Control.Monad (filterM, forM, when)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.DuckDB.Simple (withConnection)
import Database.DuckDB.Simple.Config.Internal (CustomOption (..), registerCustomOptionOn, unexpectedVariant)
import Database.DuckDB.Simple.FromField (FieldValue)
import Database.DuckDB.Simple.Internal (Connection, Instance, withConnectionHandle, withInstanceHandle)
import Database.DuckDB.Simple.Materialize (materializeValue)
import Database.DuckDB.Simple.Native
import Database.DuckDB.Simple.ToField (ToDuckValue (..))
import Database.DuckDB.Simple.V2.Registration (RegistrationTarget (..), TypeDeclaration)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)

-- | The canonical name and description of a configuration option.
data ConfigFlag = ConfigFlag
    { configFlagName :: !Text
    , configFlagDescription :: !Text
    }
    deriving (Eq, Show)

-- | An effective option value and the scope that supplied it.
data ConfigValue = ConfigValue
    { configValueText :: !Text
    , configValueScope :: !SettingScope
    }
    deriving (Eq, Show)

-- | A copied option descriptor. Native defaults and aliases can be empty.
data ConfigOption = ConfigOption
    { configOptionName :: !Text
    , configOptionDescription :: !Text
    , configOptionSetting :: !Text
    , configOptionDefault :: !Text
    , configOptionScope :: !SettingScope
    , configOptionScopes :: ![SettingScope]
    , configOptionAliases :: ![Text]
    }
    deriving (Eq, Show)

-- | Declare a global option without a default value or an update function.
customOption :: Text -> TypeDeclaration -> CustomOption
customOption name dtype = CustomOption name dtype Nothing "" SettingScopeGlobal [SettingScopeGlobal] Nothing

{- | Register a typed custom option on the connection's database.
The update function must not write options.
-}
registerCustomOption :: Connection -> CustomOption -> IO ()
registerCustomOption conn = registerCustomOptionOn (ConnectionTarget conn)

{- | List the configuration flags of a new in-memory database. DuckDB 2.0
gives its options only from an instance or a connection, so the function opens
a temporary connection. The list does not have the options of an extension that
the new database does not load. Version 0.3 listed those options also. Use
'listConfigOptions' for the options of a connection that you have.
-}
listConfigFlags :: IO [ConfigFlag]
listConfigFlags = withConnection ":memory:" \conn ->
    map (\option -> ConfigFlag (configOptionName option) (configOptionDescription option)) <$> listConfigOptions conn

-- | List options with effective values from a live connection.
listConfigOptions :: Connection -> IO [ConfigOption]
listConfigOptions conn = withConnectionHandle conn \handle -> do
    config <- output (c_duckdb_v2_connection_get_config handle)
    copyOptions config

-- | List options and global values from an instance.
listInstanceOptions :: Instance -> IO [ConfigOption]
listInstanceOptions instance_ = withInstanceHandle instance_ \handle -> do
    config <- output (c_duckdb_v2_instance_get_config handle)
    copyOptions config

-- | Read an effective option by its canonical name or an alias.
getConfigOption :: Connection -> Text -> IO (Maybe ConfigValue)
getConfigOption conn name = do
    when (Text.any (== '\0') name) $ throwInvalidArgument "config option name contains NUL"
    (withConfigOptionValue conn name \value scope -> Just . (`ConfigValue` scope) <$> valueText value)
        `catch` \err ->
            if sqlErrorType err == Just ErrorInputInvalid
                then pure Nothing
                else throwIO err

-- | Write an option at its default, global, or session scope.
setConfigOption :: Connection -> SettingScope -> Text -> Text -> IO ()
setConfigOption conn scope name value = withConnectionHandle conn \handle -> do
    config <- output (c_duckdb_v2_connection_get_config handle)
    withIdentifier name \key -> withText value \setting ->
        checked (c_duckdb_v2_config_set_option_text config key setting (settingScopeToNative scope))

-- | Read a typed option value during one action. The action borrows the value.
withConfigOptionValue :: Connection -> Text -> (DuckDBV2ValueHandle -> SettingScope -> IO a) -> IO a
withConfigOptionValue conn name action = withConnectionHandle conn \handle -> do
    config <- output (c_duckdb_v2_connection_get_config handle)
    withOptionValue config name action

{- | Copy the effective typed value of an option and the scope of that value.
An unknown option name raises 'SQLError'. The function releases all native
handles before it returns.
-}
getConfigValue :: Connection -> Text -> IO (FieldValue, SettingScope)
getConfigValue conn name = withConnectionHandle conn \handle -> do
    config <- output (c_duckdb_v2_connection_get_config handle)
    withConnectionFactory handle \factory ->
        withOptionValue config name \value scope -> do
            copied <- materializeValue unexpectedVariant factory value
            pure (copied, scope)

{- | Copy the typed static default of an option. An unknown option name raises
'SQLError'. The function releases all native handles before it returns.
-}
getConfigDefaultValue :: Connection -> Text -> IO FieldValue
getConfigDefaultValue conn name = withConnectionHandle conn \handle -> do
    config <- output (c_duckdb_v2_connection_get_config handle)
    withConnectionFactory handle \factory ->
        withIdentifier name \key ->
            bracket (output (c_duckdb_v2_config_get_option_by_name config key)) (destroy c_duckdb_v2_option_destroy) \option ->
                bracket (output (c_duckdb_v2_option_get_default_value option)) (destroy c_duckdb_v2_value_destroy) (materializeValue unexpectedVariant factory)

-- | Set an option from a typed Haskell value. Native code casts it to the option type.
setConfigValue :: (ToDuckValue a) => Connection -> SettingScope -> Text -> a -> IO ()
setConfigValue conn scope name value = withConnectionHandle conn \handle -> do
    config <- output (c_duckdb_v2_connection_get_config handle)
    withConnectionFactory handle \factory ->
        bracket (toDuckValue factory value) (destroy c_duckdb_v2_value_destroy) \native ->
            withIdentifier name \key -> checked (c_duckdb_v2_config_set_option_value config key native (settingScopeToNative scope))

-- | Copy the options visible through a borrowed config.
copyOptions :: DuckDBV2ConfigHandle -> IO [ConfigOption]
copyOptions config = do
    count <- output (c_duckdb_v2_config_get_option_count config)
    forM (indices count) \index ->
        bracket (output (c_duckdb_v2_config_get_option_by_index config index)) (destroy c_duckdb_v2_option_destroy) (copyOption config)

-- | Copy a descriptor while its native strings are alive.
copyOption :: DuckDBV2ConfigHandle -> DuckDBV2OptionHandle -> IO ConfigOption
copyOption config handle = do
    name <- output (c_duckdb_v2_option_get_name handle) >>= readIdentifier
    description <- output (c_duckdb_v2_option_get_description handle) >>= readText
    setting <- withOptionValue config name \value _ -> valueText value
    defaultSetting <- bracket (output (c_duckdb_v2_option_get_default_value handle)) (destroy c_duckdb_v2_value_destroy) valueText
    scope <- output (c_duckdb_v2_option_get_default_scope handle) >>= settingScopeFromNative
    scopes <- filterM (\candidate -> (/= 0) <$> output (c_duckdb_v2_option_supports_scope handle (settingScopeToNative candidate))) [SettingScopeGlobal, SettingScopeSession]
    count <- output (c_duckdb_v2_option_get_alias_count handle)
    aliases <- forM (indices count) \index -> output (c_duckdb_v2_option_get_alias handle index) >>= readIdentifier
    pure (ConfigOption name description setting defaultSetting scope scopes aliases)

-- | Release each owned option value after its action.
withOptionValue :: DuckDBV2ConfigHandle -> Text -> (DuckDBV2ValueHandle -> SettingScope -> IO a) -> IO a
withOptionValue config name action = withIdentifier name \key -> alloca \scope ->
    bracket
        (output (\value -> c_duckdb_v2_config_get_option_value config key value scope))
        (destroy c_duckdb_v2_value_destroy)
        \value -> peek scope >>= settingScopeFromNative >>= action value
