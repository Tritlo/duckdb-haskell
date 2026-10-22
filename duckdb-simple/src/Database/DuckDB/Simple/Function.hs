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

import Data.Text (Text)
import qualified Data.Text as Text
import Database.DuckDB.Simple.Callback (RegistrationTarget (ConnectionTarget))
import Database.DuckDB.Simple.Function.Internal (
    Function (..),
    FunctionArg,
    FunctionResult,
    ScalarType,
    ScalarValue,
    createFunctionOn,
    createFunctionWithStateOn,
 )
import Database.DuckDB.Simple.Internal (
    Connection,
    executeParsedStatement,
    withParsedStatements,
 )
import Database.DuckDB.Simple.Native

-- | Register a Haskell function under the supplied name.
createFunction :: (Function f) => Connection -> Text -> f -> IO ()
createFunction conn = createFunctionOn (ConnectionTarget conn)

-- | Register a scalar function with per-worker thread-local state.
createFunctionWithState :: (Function f) => Connection -> Text -> IO s -> (s -> f) -> IO ()
createFunctionWithState conn = createFunctionWithStateOn (ConnectionTarget conn)

{- | Run @DROP FUNCTION IF EXISTS@ and report native errors.
DuckDB does not permit deletion of functions registered through the C API.
-}
deleteFunction :: Connection -> Text -> IO ()
deleteFunction conn name = do
    let dropQuery = Query ("DROP FUNCTION IF EXISTS " <> qualifyIdentifier name)
    withParsedStatements conn dropQuery (mapM_ (executeParsedStatement conn dropQuery))

-- | Quote each dot-separated part of a function name.
qualifyIdentifier :: Text -> Text
qualifyIdentifier = Text.intercalate "." . map quoteIdent . Text.splitOn "."

-- | Quote one SQL identifier.
quoteIdent :: Text -> Text
quoteIdent ident = "\"" <> Text.replace "\"" "\"\"" ident <> "\""
