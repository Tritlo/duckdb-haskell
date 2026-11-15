{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Database.DuckDB.Simple.Config.Internal
-- Description : Share the typed custom option registration for connections and extensions.
--
-- This module is not exposed: the cabal file lists it in @other-modules@. It
-- holds the implementation. 'registerCustomOptionOn' takes a registration
-- target. "Database.DuckDB.Simple.Config" gives the connection form.
-- "Database.DuckDB.Simple.Extension" gives the extension form.
module Database.DuckDB.Simple.Config.Internal (
  CustomOption (..),
  registerCustomOptionOn,
) where

import Control.Exception (bracket)
import Control.Monad (when)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.DuckDB.Simple.Callback (targetCastFactory)
import Database.DuckDB.Simple.FromField (FieldValue)
import Database.DuckDB.Simple.Materialize (VectorReader, callbackReadFactory, materializeValue, rejectRows)
import Database.DuckDB.Simple.Native
import Database.DuckDB.Simple.Typed (checkArgumentType)
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
-- Thus the function must accept the default value. For an option with an update
-- function, a type that is or contains VARIANT is an error at registration,
-- because the library cannot read a VARIANT value in a callback (see the
-- section /Typed registration/ of the README). An option without an update
-- function can have such a type, and
-- 'Database.DuckDB.Simple.Config.getConfigValue' reads its value.
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
  -- The update function reads no VARIANT. Thus reject such a type before the
  -- registration.
  when (isJust (customOptionUpdate option)) $
    withRegistrationFactory target \factory ->
      withTypeDeclaration factory (customOptionType option) (checkArgumentType True (customOptionName option) "the update function")
  casts <- targetCastFactory target
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
    (updateWithValue casts <$> customOptionUpdate option)

-- | Copy the new value of an option update. Then give it to the update function.
updateWithValue :: Maybe DuckDBV2FactoryHandle -> (FieldValue -> IO ()) -> ContextCallback DuckDBV2CustomOptionUpdateInfoHandle
updateWithValue casts update info context = withContextFactory context \factory ->
  bracket
    (output (c_duckdb_v2_custom_option_update_get_value info))
    (destroy c_duckdb_v2_value_destroy)
    (materializeValue unexpectedVariant (callbackReadFactory casts factory))
    >>= update

-- | Raise an @SQLError@ for a VARIANT value. The update function of a custom
-- option gets its value in a callback. The callback has a context and no
-- helper connection, so it cannot decode VARIANT.
unexpectedVariant :: VectorReader
unexpectedVariant = rejectRows (throwInvalidArgument "configuration values must not contain VARIANT")
