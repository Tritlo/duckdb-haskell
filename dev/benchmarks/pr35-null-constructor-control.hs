{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Exception (SomeException, bracket, try)
import Control.Monad (forM, unless, void)
import Data.List (isInfixOf)
import Database.DuckDB.FFI.V2
import Database.DuckDB.Simple
import Database.DuckDB.Simple.Internal (
  destroy,
  withConnectionFactory,
  withConnectionHandle,
  withDatabaseHandle,
  withIdentifier,
  withInstanceFactory,
  withText,
 )
import Database.DuckDB.Simple.LogicalRep
import Database.DuckDB.Simple.V2.LogicalRep
import qualified Database.DuckDB.Simple.V2.Registration as R
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.Ptr (nullPtr)

-- | Compare named, quoted, and unquoted NULL constructors.
inspect :: String -> DuckDBV2FactoryHandle -> IO Bool
inspect label factory = do
  let byText = withText "\"NULL\"" (R.output . c_duckdb_v2_factory_create_type_from_text factory)
      byUnquotedText = withText "NULL" (R.output . c_duckdb_v2_factory_create_type_from_text factory)
      byName = withIdentifier "NULL" \part ->
        bracket (R.output (c_duckdb_v2_qname_create part 1)) (destroy c_duckdb_v2_qname_destroy) \name ->
          R.output (c_duckdb_v2_factory_create_type_from_name factory name (ConstPtr nullPtr) (ConstPtr nullPtr) 0)
      run action = try (bracket action destroyLogicalType (logicalTypeToRep factory)) :: IO (Either SomeException LogicalTypeRep)
  text <- run byText
  name <- run byName
  unquoted <- run byUnquotedText
  print (label, fmap show text, fmap show name, fmap show unquoted)
  pure
    ( case (text, name) of
        (Right left, Right right) -> left == right && right == LogicalTypeScalar LogicalTypeIdSQLNull
        _ -> False
    )

-- | Check both factory kinds with catalog and search-path controls.
main :: IO ()
main = do
  checks <- forM ["baseline", "default-schema", "search-path", "registered"] \scenario ->
    withConnectionWithConfig ":memory:" [("threads", "1"), ("async_threads", "0")] \conn -> do
      setup <-
        try
          ( case scenario of
              "baseline" -> pure ()
              "default-schema" -> void $ execute_ conn "CREATE TYPE \"NULL\" AS VARCHAR"
              "search-path" -> do
                void $ execute_ conn "CREATE SCHEMA custom"
                void $ execute_ conn "CREATE TYPE custom.\"NULL\" AS VARCHAR"
                void $ execute_ conn "SET search_path='custom,main'"
              _ -> R.registerCustomType conn "NULL" (R.StructuredType (LogicalTypeScalar LogicalTypeIdVarchar))
          )
          :: IO (Either SomeException ())
      print (scenario, fmap (const "setup ok") setup)
      connected <- withConnectionHandle conn \native -> withConnectionFactory native (inspect (scenario <> ":connection"))
      instanceOnly <- withDatabaseHandle conn \native -> withInstanceFactory native (inspect (scenario <> ":instance"))
      let setupOk = case setup of
            Right () -> True
            Left problem -> scenario == "default-schema" && "already exists" `isInfixOf` show problem
      pure (setupOk && connected && instanceOnly)
  unless (and checks) (fail "named and quoted NULL must agree and remain unaliased SQLNULL")
