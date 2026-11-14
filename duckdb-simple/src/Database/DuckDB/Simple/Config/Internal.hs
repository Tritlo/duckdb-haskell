{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Database.DuckDB.Simple.Config.Internal
-- Description : Share the typed custom option registration for connections and extensions.
--
-- 'registerCustomOptionOn' takes a registration target.
-- "Database.DuckDB.Simple.Config" gives the connection form.
-- "Database.DuckDB.Simple.Extension" gives the extension form.
module Database.DuckDB.Simple.Config.Internal (
  CustomOption (..),
  registerCustomOptionOn,
  unexpectedVariant,
) where

import Control.Exception (bracket, throwIO)
import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.DuckDB.Simple.FromField (FieldValue)
import Database.DuckDB.Simple.Materialize (VectorReader, materializeValue, rejectRows)
import Database.DuckDB.Simple.Native
import Database.DuckDB.Simple.V2.Config (registerCustomOptionWith)
import Database.DuckDB.Simple.V2.Registration (
  ContextCallback,
  DefaultValue (..),
  RegistrationTarget (..),
  TypeDeclaration,
  withRegistrationFactory,
  withTypeDeclaration,
 )

-- | A custom setting. The optional update function gets each new value of the
-- option before DuckDB stores it. The value has the type of the option. An
-- exception in the function rejects the value. The text of the exception is the
-- SQL error. A RESET gives the default value to the function, or
-- 'Database.DuckDB.Simple.FromField.FieldNull' for an option without a default.
-- Thus the function must accept the default value.
data CustomOption = CustomOption
  { customOptionName :: !Text
  , customOptionType :: !TypeDeclaration
  , customOptionDefault :: Maybe DefaultValue
  , customOptionDescription :: !Text
  , customOptionDefaultScope :: !SettingScope
  , customOptionScopes :: ![SettingScope]
  , customOptionUpdate :: Maybe (FieldValue -> IO ())
  }

-- | Register a declared option on a connection or loading extension.
registerCustomOptionOn :: RegistrationTarget -> CustomOption -> IO ()
registerCustomOptionOn target option = do
  when (Text.null (customOptionName option) || Text.any (== '\0') (customOptionName option)) $
    throwInvalidArgument "custom option name must be nonempty and must not contain NUL"
  registerCustomOptionWith
    target
    ( \handle -> withRegistrationFactory target \factory -> do
        withIdentifier (customOptionName option) $ checked . c_duckdb_v2_custom_option_set_name handle
        withTypeDeclaration factory (customOptionType option) $ checked . c_duckdb_v2_custom_option_set_type handle
        case customOptionDefault option of
          Nothing -> pure ()
          Just (DefaultValue make) -> bracket (make factory) (destroy c_duckdb_v2_value_destroy) \native -> checked (c_duckdb_v2_custom_option_set_default_value handle native)
        withText (customOptionDescription option) $ checked . c_duckdb_v2_custom_option_set_description handle
        mapM_
          ( \scope ->
              checked
                (c_duckdb_v2_custom_option_set_supports_scope handle (settingScopeToNative scope) (if scope `elem` customOptionScopes option then 1 else 0))
          )
          [SettingScopeGlobal, SettingScopeSession]
        checked (c_duckdb_v2_custom_option_set_default_scope handle (settingScopeToNative (customOptionDefaultScope option)))
    )
    (updateWithValue <$> customOptionUpdate option)

-- | Copy the new value of an option update. Then give it to the update function.
updateWithValue :: (FieldValue -> IO ()) -> ContextCallback DuckDBV2CustomOptionUpdateInfoHandle
updateWithValue update info context = withContextFactory context \factory ->
  bracket
    (output (c_duckdb_v2_custom_option_update_get_value info))
    (destroy c_duckdb_v2_value_destroy)
    (materializeValue unexpectedVariant factory)
    >>= update

-- | Raise an @SQLError@ for a VARIANT value. Configuration reads do not decode VARIANT.
unexpectedVariant :: VectorReader
unexpectedVariant = rejectRows (throwIO (SQLError "duckdb-simple: configuration values must not contain VARIANT" Nothing Nothing))
