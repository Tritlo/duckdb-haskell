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
    withConfigOptionValue,
    withConfigOptionDefaultValue,
    setConfigValue,
    CustomOption (..),
    customOption,
    registerCustomOption,
    registerCustomOptionOn,
    registerCustomOptionWith,
) where

import Control.Exception (bracket)
import Control.Monad (filterM, forM, when)
import qualified Data.ByteString as BS
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Database.DuckDB.Simple (withConnection)
import Database.DuckDB.Simple.Callback (attachOpaque, runCallback, withCallbackResources)
import Database.DuckDB.Simple.Internal (Connection, Instance, throwRegistrationError, withConnectionHandle, withInstanceHandle)
import Database.DuckDB.Simple.Native
import Database.DuckDB.Simple.Registration (ContextCallback, DefaultValue (..), RegistrationTarget (..), TypeDeclaration, withRegistrationFactory, withTypeDeclarationWithFactory)
import Database.DuckDB.Simple.ToField (ToDuckValue (..))
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (peek)
import HsBindgen.Runtime.Support.FunPtr (toFunPtr)

-- | The canonical name and description of a configuration option.
data ConfigFlag = ConfigFlag
    { configFlagName :: !Text
    , configFlagDescription :: !Text
    }
    deriving (Eq, Show)

-- | An effective option value and the scope that supplied it.
data ConfigValue = ConfigValue
    { configValueText :: !Text
    , configValueScope :: !DuckDBV2SettingScope
    }
    deriving (Eq, Show)

-- | A copied option descriptor. Native defaults and aliases can be empty.
data ConfigOption = ConfigOption
    { configOptionName :: !Text
    , configOptionDescription :: !Text
    , configOptionSetting :: !Text
    , configOptionDefault :: !Text
    , configOptionScope :: !DuckDBV2SettingScope
    , configOptionScopes :: ![DuckDBV2SettingScope]
    , configOptionAliases :: ![Text]
    }
    deriving (Eq, Show)

-- | A custom setting. The optional update callback may validate or replace values.
data CustomOption = CustomOption
    { customOptionName :: !Text
    , customOptionType :: !TypeDeclaration
    , customOptionDefault :: Maybe DefaultValue
    , customOptionDescription :: !Text
    , customOptionDefaultScope :: !DuckDBV2SettingScope
    , customOptionScopes :: ![DuckDBV2SettingScope]
    , customOptionUpdate :: Maybe (ContextCallback DuckDBV2CustomOptionUpdateInfoHandle)
    }

-- | Declare a global option without a default value or an update callback.
customOption :: Text -> TypeDeclaration -> CustomOption
customOption name dtype = CustomOption name dtype Nothing "" DuckDBV2SettingScopeGlobal [DuckDBV2SettingScopeGlobal] Nothing

{- | Register a typed custom option on the connection's database.
Update callbacks must not write options. They borrow their native handles.
-}
registerCustomOption :: Connection -> CustomOption -> IO ()
registerCustomOption conn = registerCustomOptionOn (ConnectionTarget conn)

{- | Register a declared option on a connection or loading extension.
Extension defaults must use 'FactoryDefaultValue'.
-}
registerCustomOptionOn :: RegistrationTarget -> CustomOption -> IO ()
registerCustomOptionOn target option = do
    when (Text.null (customOptionName option) || Text.any (== '\0') (customOptionName option)) $
        throwRegistrationError "custom option name must be nonempty and must not contain NUL"
    registerCustomOptionWith
        target
        ( \handle -> withRegistrationFactory target \factory -> do
            withIdentifier (customOptionName option) $ checked . c_duckdb_v2_custom_option_set_name handle
            withTypeDeclarationWithFactory factory (customOptionType option) $ checked . c_duckdb_v2_custom_option_set_type handle
            case customOptionDefault option of
                Nothing -> pure ()
                Just value -> bracket (makeDefault factory value) (destroy c_duckdb_v2_value_destroy) \native -> checked (c_duckdb_v2_custom_option_set_default_value handle native)
            withText (customOptionDescription option) $ checked . c_duckdb_v2_custom_option_set_description handle
            mapM_ (\scope -> checked (c_duckdb_v2_custom_option_set_supports_scope handle scope (if scope `elem` customOptionScopes option then 1 else 0))) [DuckDBV2SettingScopeGlobal, DuckDBV2SettingScopeSession]
            checked (c_duckdb_v2_custom_option_set_default_scope handle (customOptionDefaultScope option))
        )
        (customOptionUpdate option)
  where
    makeDefault factory (FactoryDefaultValue make) = make factory
    makeDefault _ (DefaultValue make) = case target of
        ConnectionTarget conn -> withConnectionHandle conn make
        ExtensionTarget{} -> throwRegistrationError "extension defaults require FactoryDefaultValue"

{- | Configure and register a custom option with managed callback cleanup.
The configuration action borrows the builder. Do not set user data or callbacks.
An extension target includes the borrowed initialization context.
-}
registerCustomOptionWith :: RegistrationTarget -> (DuckDBV2CustomOptionHandle -> IO ()) -> Maybe (ContextCallback DuckDBV2CustomOptionUpdateInfoHandle) -> IO ()
registerCustomOptionWith target configure callback = case target of
    ConnectionTarget conn -> withConnectionHandle conn \native -> withConnectionFactory native \factory ->
        build factory (checked . c_duckdb_v2_connection_register_custom_option native)
    ExtensionTarget extension context -> withContextFactory context \factory ->
        build factory (checked . c_duckdb_v2_extension_register_custom_option extension)
  where
    build factory register = bracket (output (c_duckdb_v2_custom_option_create factory)) (destroy c_duckdb_v2_custom_option_destroy) \option ->
        withCallbackResources
            ( \allocate -> case callback of
                Nothing -> pure Nothing
                Just update -> Just . DuckDBV2CustomOptionUpdateCallbackFn <$> allocate (toFunPtr (DuckDBV2CustomOptionUpdateCallbackFn_Aux (\info context err -> runCallback err (update info context))))
            )
            (attachOpaque (c_duckdb_v2_custom_option_set_user_data option))
            \update -> do
                configure option
                mapM_ (checked . c_duckdb_v2_custom_option_set_update_callback option) update
                register option

-- | List built-in configuration flags using a temporary connection.
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
    when (Text.any (== '\0') name) $ throwRegistrationError "config option name contains NUL"
    options <- listConfigOptions conn
    case find (\value -> configOptionName value == name || name `elem` configOptionAliases value) options of
        Nothing -> pure Nothing
        Just _ -> withConfigOptionValue conn name \value scope -> Just . (`ConfigValue` scope) <$> valueText value

-- | Write an option at its default, global, or session scope.
setConfigOption :: Connection -> DuckDBV2SettingScope -> Text -> Text -> IO ()
setConfigOption conn scope name value = withConnectionHandle conn \handle ->
    do
        config <- output (c_duckdb_v2_connection_get_config handle)
        withIdentifier name \key -> withText value \setting ->
            checked (c_duckdb_v2_config_set_option_text config key setting scope)

-- | Read a typed option value during one action. The action borrows the value.
withConfigOptionValue :: Connection -> Text -> (DuckDBV2ValueHandle -> DuckDBV2SettingScope -> IO a) -> IO a
withConfigOptionValue conn name action = withConnectionHandle conn \handle -> do
    config <- output (c_duckdb_v2_connection_get_config handle)
    withOptionValue config name action

-- | Borrow the typed static default of an option during one action.
withConfigOptionDefaultValue :: Connection -> Text -> (DuckDBV2ValueHandle -> IO a) -> IO a
withConfigOptionDefaultValue conn name action = withConnectionHandle conn \handle -> do
    config <- output (c_duckdb_v2_connection_get_config handle)
    withIdentifier name \key ->
        bracket (output (c_duckdb_v2_config_get_option_by_name config key)) (destroy c_duckdb_v2_option_destroy) \option ->
            bracket (output (c_duckdb_v2_option_get_default_value option)) (destroy c_duckdb_v2_value_destroy) action

-- | Set an option from a typed Haskell value. Native code casts it to the option type.
setConfigValue :: (ToDuckValue a) => Connection -> DuckDBV2SettingScope -> Text -> a -> IO ()
setConfigValue conn scope name value = withConnectionHandle conn \handle -> do
    config <- output (c_duckdb_v2_connection_get_config handle)
    bracket (toDuckValue handle value) (destroy c_duckdb_v2_value_destroy) \native ->
        withIdentifier name \key -> checked (c_duckdb_v2_config_set_option_value config key native scope)

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
    scope <- output (c_duckdb_v2_option_get_default_scope handle)
    scopes <- filterM (\candidate -> (/= 0) <$> output (c_duckdb_v2_option_supports_scope handle candidate)) [DuckDBV2SettingScopeGlobal, DuckDBV2SettingScopeSession]
    count <- output (c_duckdb_v2_option_get_alias_count handle)
    aliases <- forM (indices count) \index -> output (c_duckdb_v2_option_get_alias handle index) >>= readIdentifier
    pure (ConfigOption name description setting defaultSetting scope scopes aliases)

-- | Release each owned option value after its action.
withOptionValue :: DuckDBV2ConfigHandle -> Text -> (DuckDBV2ValueHandle -> DuckDBV2SettingScope -> IO a) -> IO a
withOptionValue config name action = withIdentifier name \key -> alloca \scope ->
    bracket
        (output (\value -> c_duckdb_v2_config_get_option_value config key value scope))
        (destroy c_duckdb_v2_value_destroy)
        \value -> peek scope >>= action value

-- | Copy the native diagnostic text of a configuration value.
valueText :: DuckDBV2ValueHandle -> IO Text
valueText value = do
    count <- output (c_duckdb_v2_value_to_string value nullPtr 0)
    when (toInteger count >= toInteger (maxBound :: Int)) $ throwRegistrationError "configuration value exceeds Int range"
    allocaBytes (fromIntegral count + 1) \buffer -> do
        _ <- output (c_duckdb_v2_value_to_string value buffer (count + 1))
        Text.decodeUtf8 <$> BS.packCStringLen (buffer, fromIntegral count)

-- | Enumerate a native count without unsigned underflow for empty input.
indices :: DuckDBIdx -> [DuckDBIdx]
indices count = if count == 0 then [] else [0 .. count - 1]
