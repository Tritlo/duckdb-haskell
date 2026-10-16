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

import Control.Exception (bracket, throwIO, try)
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
    Statement (..),
    closeStatementState,
    createStatementFromParsed,
    runInterruptibleQuery,
    withConnectionHandle,
    withParsedStatements,
    withResult,
 )
import Database.DuckDB.Simple.Native
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.Ptr (nullPtr)

-- | Register a Haskell function under the supplied name.
createFunction :: forall f. (Function f) => Connection -> Text -> f -> IO ()
createFunction conn name fn =
    withConnectionHandle conn \connection ->
        createFunctionUsing
            (output (c_duckdb_v2_scalar_function_create_with_connection connection))
            (\dtype -> output (c_duckdb_v2_connection_create_type_from_id connection dtype (ConstPtr nullPtr) (ConstPtr nullPtr) 0))
            name
            fn

-- | Register a scalar function with per-worker thread-local state.
createFunctionWithState :: forall s f. (Function f) => Connection -> Text -> IO s -> (s -> f) -> IO ()
createFunctionWithState conn name initState mkFn =
    withConnectionHandle conn \connection ->
        createFunctionWithStateUsing
            (output (c_duckdb_v2_scalar_function_create_with_connection connection))
            (\dtype -> output (c_duckdb_v2_connection_create_type_from_id connection dtype (ConstPtr nullPtr) (ConstPtr nullPtr) 0))
            name
            initState
            mkFn

-- | Drop a previously registered scalar function with a DROP FUNCTION statement.
deleteFunction :: Connection -> Text -> IO ()
deleteFunction conn name = do
    let dropQuery = Query ("DROP FUNCTION IF EXISTS " <> qualifyIdentifier name)
    outcome <- try $
        withParsedStatements conn dropQuery $ \parsed ->
            forM_ parsed $ \statement ->
                bracket
                    (createStatementFromParsed conn dropQuery statement)
                    (closeStatementState . statementState)
                    \prepared -> withResult prepared $ \result -> do
                        _ <- runInterruptibleQuery conn (output (c_duckdb_v2_result_drain result))
                        pure ()
    case outcome of
        Right () -> pure ()
        Left err
            -- Native scalar functions are internal catalog entries and cannot be dropped.
            | Text.isInfixOf "Cannot drop internal catalog entry" (sqlErrorMessage err) -> pure ()
            | otherwise -> throwIO err

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
