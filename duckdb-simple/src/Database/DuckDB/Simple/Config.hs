{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- |
-- Module      : Database.DuckDB.Simple.Config
-- Description : V2 configuration values, declared scopes, defaults, and aliases.
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
import Database.DuckDB.Simple.Config.Internal (CustomOption (..), registerCustomOptionOn)
import Database.DuckDB.Simple.FromField (FieldValue)
import Database.DuckDB.Simple.Internal (Connection, Instance, withConnectionHandle, withInstanceHandle, withTypeCache)
import Database.DuckDB.Simple.Materialize (VectorReader, materializeValue, prepareVariantReader)
import Database.DuckDB.Simple.Native
import Database.DuckDB.Simple.ToField (ToDuckValue (..))
import Database.DuckDB.Simple.TypeCache (TypeCache, typeCacheCasts, withVariantState)
import Database.DuckDB.Simple.V2.Registration (RegistrationTarget (..), TypeDeclaration)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)

-- | The canonical name and description of a configuration option.
data ConfigFlag = ConfigFlag
  { configFlagName :: Text
  , configFlagDescription :: Text
  }
  deriving (Eq, Show)

-- | An effective option value and the scope that supplied it.
-- 'configValueText' is 'Nothing' for a NULL value, as 'configOptionSetting'.
data ConfigValue = ConfigValue
  { configValueText :: Maybe Text
  , configValueScope :: SettingScope
  }
  deriving (Eq, Show)

-- | A copied option descriptor. The list of aliases can be empty.
-- 'configOptionSetting' is the effective value. 'Nothing' means a NULL value,
-- which the @value@ column of @duckdb_settings()@ also gives as NULL.
-- 'configOptionDefault' is the static default of the declaration. 'Nothing'
-- means that the declaration has no default. The value after @RESET@ can
-- differ from the static default: @progress_bar_time@ has no static default,
-- and DuckDB 2.0.0 gives 2000 after @RESET@.
data ConfigOption = ConfigOption
  { configOptionName :: Text
  , configOptionDescription :: Text
  , configOptionSetting :: Maybe Text
  , configOptionDefault :: Maybe Text
  , configOptionScope :: SettingScope
  , configOptionScopes :: [SettingScope]
  , configOptionAliases :: [Text]
  }
  deriving (Eq, Show)

-- | Declare a global option without a default value or an update function.
customOption :: Text -> TypeDeclaration -> CustomOption
customOption name dtype = CustomOption name dtype Nothing "" SettingScopeGlobal [SettingScopeGlobal] Nothing

-- | Register a typed custom option on the connection's database.
-- The update function must not write options. For an option with an update
-- function, a type that is or contains VARIANT is an error at registration,
-- because the library cannot read a VARIANT value in a callback (see the
-- section /Typed registration/ of the README).
registerCustomOption :: Connection -> CustomOption -> IO ()
registerCustomOption conn = registerCustomOptionOn (ConnectionTarget conn)

-- | List the name and the description of each option of the database of the
-- connection, for example a custom option of the database.
-- 'listConfigOptions' gives the same options with their values.
listConfigFlags :: Connection -> IO [ConfigFlag]
listConfigFlags conn =
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
  checkOptionName name
  (withConfigOptionValue conn name \value scope -> Just . (`ConfigValue` scope) <$> nullableText value)
    `catch` \err ->
      if sqlErrorType err == Just ErrorInputInvalid
        then pure Nothing
        else throwIO err

-- | Write an option at its default, global, or session scope.
setConfigOption :: Connection -> SettingScope -> Text -> Text -> IO ()
setConfigOption conn scope name value = do
  checkOptionName name
  withConnectionHandle conn \handle -> do
    config <- output (c_duckdb_v2_connection_get_config handle)
    withIdentifier name \key -> withText value \setting ->
      checked (c_duckdb_v2_config_set_option_text config key setting (settingScopeToNative scope))

-- | Read a typed option value during one action. The action borrows the value.
withConfigOptionValue :: Connection -> Text -> (DuckDBV2ValueHandle -> SettingScope -> IO a) -> IO a
withConfigOptionValue conn name action = withConnectionHandle conn \handle -> do
  config <- output (c_duckdb_v2_connection_get_config handle)
  withOptionValue config name action

-- | Copy the effective typed value of an option and the scope of that value.
-- An unknown option name raises t'SQLError'. A VARIANT value gives the value
-- that it holds, as in a result. A GEOMETRY value reads with the factory of the
-- instance. See Note [User casts and the casts of the library] in
-- "Database.DuckDB.Simple.Materialize". The function releases all native
-- handles before it returns.
getConfigValue :: Connection -> Text -> IO (FieldValue, SettingScope)
getConfigValue conn name = do
  checkOptionName name
  withConnectionHandle conn \handle -> do
    config <- output (c_duckdb_v2_connection_get_config handle)
    withTypeCache conn \cache ->
      withOptionValue config name \value scope -> do
        copied <- materializeValue (readVariant cache) (typeCacheCasts cache) value
        pure (copied, scope)

-- | Copy the typed static default of an option. An unknown option name raises
-- t'SQLError'. A VARIANT value gives the value that it holds, as in a result. A
-- GEOMETRY value reads as 'getConfigValue' reads it. The function releases all
-- native handles before it returns.
getConfigDefaultValue :: Connection -> Text -> IO FieldValue
getConfigDefaultValue conn name = do
  checkOptionName name
  withConnectionHandle conn \handle -> do
    config <- output (c_duckdb_v2_connection_get_config handle)
    withTypeCache conn \cache ->
      withIdentifier name \key ->
        bracket (output (c_duckdb_v2_config_get_option_by_name config key)) (destroy c_duckdb_v2_option_destroy) \option ->
          bracket
            (output (c_duckdb_v2_option_get_default_value option))
            (destroy c_duckdb_v2_value_destroy)
            (materializeValue (readVariant cache) (typeCacheCasts cache))

-- | Read a VARIANT value with the helper connection of the type cache, as a
-- result does. See Note [VARIANT decoding] in
-- "Database.DuckDB.Simple.VariantCodec".
readVariant :: TypeCache -> VectorReader
readVariant cache vector = withVariantState cache (`prepareVariantReader` vector)

-- | Set an option from a typed Haskell value. Native code casts it to the option type.
setConfigValue :: (ToDuckValue a) => Connection -> SettingScope -> Text -> a -> IO ()
setConfigValue conn scope name value = do
  checkOptionName name
  withConnectionHandle conn \handle -> do
    config <- output (c_duckdb_v2_connection_get_config handle)
    withConnectionFactory handle \factory ->
      bracket (toDuckValue factory value) (destroy c_duckdb_v2_value_destroy) \native ->
        withIdentifier name \key -> checked (c_duckdb_v2_config_set_option_value config key native (settingScopeToNative scope))

-- | Reject an option name that contains NUL, before a native call. Each
-- function of this module that takes an option name calls it first.
checkOptionName :: Text -> IO ()
checkOptionName name = when (Text.any (== '\0') name) $ throwInvalidArgument "config option name contains NUL"

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
  setting <- withOptionValue config name \value _ -> nullableText value
  defaultSetting <- bracket (output (c_duckdb_v2_option_get_default_value handle)) (destroy c_duckdb_v2_value_destroy) nullableText
  scope <- output (c_duckdb_v2_option_get_default_scope handle) >>= settingScopeFromNative
  scopes <-
    filterM
      (\candidate -> (/= 0) <$> output (c_duckdb_v2_option_supports_scope handle (settingScopeToNative candidate)))
      [SettingScopeGlobal, SettingScopeSession]
  count <- output (c_duckdb_v2_option_get_alias_count handle)
  aliases <- forM (indices count) \index -> output (c_duckdb_v2_option_get_alias handle index) >>= readIdentifier
  pure (ConfigOption name description setting defaultSetting scope scopes aliases)

-- | Copy the text of a value, or 'Nothing' for a NULL value.
nullableText :: DuckDBV2ValueHandle -> IO (Maybe Text)
nullableText value = do
  isNull <- output (c_duckdb_v2_value_is_null value)
  if isNull /= 0 then pure Nothing else Just <$> valueText value

-- | Release each owned option value after its action.
withOptionValue :: DuckDBV2ConfigHandle -> Text -> (DuckDBV2ValueHandle -> SettingScope -> IO a) -> IO a
withOptionValue config name action = withIdentifier name \key -> alloca \scope ->
  bracket
    (output (\value -> c_duckdb_v2_config_get_option_value config key value scope))
    (destroy c_duckdb_v2_value_destroy)
    \value -> peek scope >>= settingScopeFromNative >>= action value
