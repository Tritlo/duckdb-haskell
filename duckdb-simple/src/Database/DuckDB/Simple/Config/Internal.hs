{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Database.DuckDB.Simple.Config.Internal
-- Description : Share the typed custom option registration for connections and extensions.
--
-- The cabal file lists this hidden module in @other-modules@.
-- 'registerCustomOptionOn' takes a registration target.
-- "Database.DuckDB.Simple.Config" exports the connection form.
-- "Database.DuckDB.Simple.Extension" exports the extension form.
module Database.DuckDB.Simple.Config.Internal (
  CustomOption (..),
  registerCustomOptionOn,
) where

import Control.Exception (bracket)
import Control.Monad (forM, forM_, when)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.DuckDB.Simple.Callback (targetCastFactory)
import Database.DuckDB.Simple.FromField (FieldValue)
import Database.DuckDB.Simple.Materialize (callbackReadFactory, materializeValue, readCallbackVariant)
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
-- Thus the function must accept the default value. A VARIANT gives its
-- contained value, as in 'Database.DuckDB.Simple.Config.getConfigValue'.
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
  update <-
    forM (customOptionUpdate option) \callback -> do
      casts <- targetCastFactory target
      pure (updateWithValue casts callback)
  registerCustomOptionWith
    target
    ( \handle -> withRegistrationFactory target \factory -> do
        withIdentifier (customOptionName option) $ checked . c_duckdb_v2_custom_option_set_name handle
        withTypeDeclaration factory (customOptionType option) $ checked . c_duckdb_v2_custom_option_set_type handle
        forM_ (customOptionDefault option) \(DefaultValue make) ->
          bracket (make factory) destroyValue \native -> checked (c_duckdb_v2_custom_option_set_default_value handle native)
        withText (customOptionDescription option) $ checked . c_duckdb_v2_custom_option_set_description handle
        mapM_
          ( \scope ->
              checked
                (c_duckdb_v2_custom_option_set_supports_scope handle (settingScopeToNative scope) (if scope `elem` customOptionScopes option then 1 else 0))
          )
          [SettingScopeGlobal, SettingScopeSession]
        checked (c_duckdb_v2_custom_option_set_default_scope handle (settingScopeToNative (customOptionDefaultScope option)))
    )
    update

-- | Copy the new value of an option update. Then give it to the update function.
updateWithValue :: Maybe DuckDBV2FactoryHandle -> (FieldValue -> IO ()) -> ContextCallback DuckDBV2CustomOptionUpdateInfoHandle
updateWithValue casts update info context = withContextFactory context \factory ->
  bracket
    (output (c_duckdb_v2_custom_option_update_get_value info))
    destroyValue
    (materializeValue readCallbackVariant (callbackReadFactory casts factory))
    >>= update
