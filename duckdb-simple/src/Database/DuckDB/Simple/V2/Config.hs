{-# LANGUAGE BlockArguments #-}

{- |
Module      : Database.DuckDB.Simple.V2.Config
Description : Register custom options with native builders and update callbacks.

The functions of this module give native handles to their actions.
"Database.DuckDB.Simple.Config" has the typed custom options.
-}
module Database.DuckDB.Simple.V2.Config (
    registerCustomOptionWith,
) where

import Database.DuckDB.Simple.Callback (attachOpaque, runCallback, withCallbackResources, withTarget)
import Database.DuckDB.Simple.Native
import Database.DuckDB.Simple.V2.Registration (ContextCallback, RegistrationTarget (..))
import HsBindgen.Runtime.Support.FunPtr (toFunPtr)

{- | Configure and register a custom option with managed callback cleanup.
The configuration action borrows the builder. Do not set user data or callbacks.
An extension target includes the borrowed initialization context.
-}
registerCustomOptionWith :: RegistrationTarget -> (DuckDBV2CustomOptionHandle -> IO ()) -> Maybe (ContextCallback DuckDBV2CustomOptionUpdateInfoHandle) -> IO ()
registerCustomOptionWith target configure callback =
    withTarget target c_duckdb_v2_custom_option_create c_duckdb_v2_custom_option_destroy c_duckdb_v2_connection_register_custom_option c_duckdb_v2_extension_register_custom_option \option register ->
        withCallbackResources
            ( \allocate -> case callback of
                Nothing -> pure Nothing
                Just update -> Just . DuckDBV2CustomOptionUpdateCallbackFn <$> allocate (toFunPtr (DuckDBV2CustomOptionUpdateCallbackFn_Aux (\info context err -> runCallback err (update info context))))
            )
            (attachOpaque (c_duckdb_v2_custom_option_set_user_data option))
            \update -> do
                configure option
                mapM_ (checked . c_duckdb_v2_custom_option_set_update_callback option) update
                register
