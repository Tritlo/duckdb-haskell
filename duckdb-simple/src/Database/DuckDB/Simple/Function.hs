{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Database.DuckDB.Simple.Function
Description : Register scalar Haskell functions with DuckDB connections.

Register pure and IO functions with the managed argument and result conversions.
-}
module Database.DuckDB.Simple.Function (
    ScalarType,
    ScalarValue,
    FunctionArg (),
    FunctionResult (),
    Function (..),
    createFunction,
    createFunctionWithState,
    deleteFunction,
) where

import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.DuckDB.Simple.Function.Internal (
    Function (..),
    FunctionArg,
    FunctionResult,
    ScalarType,
    ScalarValue,
    createFunctionUsing,
    createFunctionWithStateUsing,
 )
import Database.DuckDB.Simple.Internal (
    Connection,
    executeParsedResult,
    runInterruptibleQuery,
    withConnectionHandle,
    withParsedStatements,
 )
import Database.DuckDB.Simple.Native
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.Ptr (nullPtr)

-- | Register a Haskell function under the supplied name.
createFunction :: forall f. (Function f) => Connection -> Text -> f -> IO ()
createFunction conn name fn =
    withConnectionHandle conn \connection -> withConnectionFactory connection \factory ->
        createFunctionUsing
            (output (c_duckdb_v2_scalar_function_create factory))
            (\dtype -> output (c_duckdb_v2_factory_create_type_from_id factory dtype (ConstPtr nullPtr) (ConstPtr nullPtr) 0))
            (checked . c_duckdb_v2_connection_register_scalar_function connection)
            name
            fn

-- | Register a scalar function with per-worker thread-local state.
createFunctionWithState :: forall s f. (Function f) => Connection -> Text -> IO s -> (s -> f) -> IO ()
createFunctionWithState conn name initState mkFn =
    withConnectionHandle conn \connection -> withConnectionFactory connection \factory ->
        createFunctionWithStateUsing
            (output (c_duckdb_v2_scalar_function_create factory))
            (\dtype -> output (c_duckdb_v2_factory_create_type_from_id factory dtype (ConstPtr nullPtr) (ConstPtr nullPtr) 0))
            (checked . c_duckdb_v2_connection_register_scalar_function connection)
            name
            initState
            mkFn

{- | Run @DROP FUNCTION IF EXISTS@ and report native errors.
DuckDB does not permit deletion of functions registered through the C API.
-}
deleteFunction :: Connection -> Text -> IO ()
deleteFunction conn name = do
    let dropQuery = Query ("DROP FUNCTION IF EXISTS " <> qualifyIdentifier name)
    withParsedStatements conn dropQuery $ \parsed ->
        forM_ parsed $ \statement ->
            bracket
                (executeParsedResult conn dropQuery statement)
                (destroy c_duckdb_v2_result_destroy)
                \result -> do
                    _ <- runInterruptibleQuery conn (output (c_duckdb_v2_result_drain result))
                    pure ()

qualifyIdentifier :: Text -> Text
qualifyIdentifier rawName =
    let parts = Text.splitOn "." rawName
     in Text.intercalate (Text.pack ".") (map quoteIdent parts)

quoteIdent :: Text -> Text
quoteIdent ident =
    Text.concat
        [ Text.pack "\""
        , Text.replace (Text.pack "\"") (Text.pack "\"\"") ident
        , Text.pack "\""
        ]
