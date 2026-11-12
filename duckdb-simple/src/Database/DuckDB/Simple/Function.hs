{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Database.DuckDB.Simple.Function
Description : Register scalar Haskell functions and casts with DuckDB connections.

Register pure and IO functions with the managed argument and result conversions.
Register pure casts with the same argument conversion.
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
    TypeDeclaration (..),
    createCast,
    TypedRow,
    createTableFunction,
) where

import Data.Text (Text)
import qualified Data.Text as Text
import Database.DuckDB.Simple.Callback (RegistrationTarget (ConnectionTarget))
import Database.DuckDB.Simple.FromField (FromField)
import Database.DuckDB.Simple.FromRow (FromRow)
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
import Database.DuckDB.Simple.ToField (ToDuckValue)
import Database.DuckDB.Simple.Typed (TypedRow, createCastOn)
import Database.DuckDB.Simple.Typed.Table (createTableFunctionOn)
import Database.DuckDB.Simple.V2.Registration (TypeDeclaration (..))

-- | Register a Haskell function under the supplied name.
createFunction :: (Function f) => Connection -> Text -> f -> IO ()
createFunction conn = createFunctionOn (ConnectionTarget conn)

-- | Register a scalar function with per-worker thread-local state.
createFunctionWithState :: (Function f) => Connection -> Text -> IO s -> (s -> f) -> IO ()
createFunctionWithState conn = createFunctionWithStateOn (ConnectionTarget conn)

{- | Register an explicit cast from the source type (the first type) to the
target type (the second type). DuckDB uses the cast for @CAST@ and @TRY_CAST@.
It does not use the cast implicitly. Register a cast again to replace it.

The function converts one value. 'Right' gives the result. 'Left' gives a
failure message. A NULL value gives NULL. The function does not run for NULL.

A row fails when its value does not convert to @a@, when the function gives
'Left', or when the function or the result conversion raises an exception.
@CAST@ then fails with the message of the first failed row. @TRY_CAST@ gives
NULL for each failed row and the results for the other rows.

DuckDB casts each result to the target type with its default cast. The target
type can be a custom type whose base type is the type of @b@, for example a
type from 'Database.DuckDB.Simple.V2.Registration.registerCustomType'.

> createCast conn (TypeExpression "BIGINT") (TypeExpression "shifted") \(value :: Int64) ->
>     if value > maxBound - 100 then Left "BIGINT overflow" else Right (value + 100)
-}
createCast :: (FromField a, ToDuckValue b) => Connection -> TypeDeclaration -> TypeDeclaration -> (a -> Either Text b) -> IO ()
createCast conn = createCastOn (ConnectionTarget conn)

{- | Register a table function. The function gives the rows of one scan as a
list. The list of names gives the names of the result columns. 'TypedRow' of
@row@ gives the column types. A different number of names and columns, or a
row with no column, is an error at registration. A @Maybe@ column gives NULL
for 'Nothing'.

'TypedRow' of @params@ gives the parameter types. Use @()@ for no parameter.
The parameters are positional only. DuckDB casts each argument to its
parameter type. Then 'FromRow' converts the arguments. An argument that does
not convert is an SQL error that names the function. A NULL argument converts
only to a @Maybe@ field.

DuckDB converts the arguments when it plans the query. Thus an argument that
does not convert also fails the preparation of a statement. The function runs
one time for each scan, when the execution of the statement starts. It does not
run at preparation. A query that reads the function two times runs it two
times. A prepared statement runs it at each execution.

DuckDB reads the list one chunk at a time, in one thread. It reads only the
rows that the query needs, so the list can be long or infinite, for example
with @LIMIT@. It reads all columns of each row that it reads, also the columns
that the query does not use. An exception in the function or in a row is an SQL
error. DuckDB releases the list when the scan ends.

> createTableFunction conn "haskell_squares" ["n", "square"] \(Only count) ->
>     pure [(n, n * n) | n <- [0 .. count - 1 :: Int64]]
-}
createTableFunction :: (FromRow params, TypedRow params, TypedRow row) => Connection -> Text -> [Text] -> (params -> IO [row]) -> IO ()
createTableFunction conn = createTableFunctionOn (ConnectionTarget conn)

{- | Run @DROP FUNCTION IF EXISTS@ and report native errors.
DuckDB does not permit deletion of functions registered through the C API.
For a function from 'createFunction', this call raises 'SQLError' and the
function stays registered. Register the name again to replace the function.
GHC reports a warning at each use of this function. A build with @-Werror@
must allow that warning, for example with @-Wno-error=deprecations@.
-}
deleteFunction :: Connection -> Text -> IO ()
deleteFunction conn name = do
    let dropQuery = Query ("DROP FUNCTION IF EXISTS " <> qualifyIdentifier name)
    withParsedStatements conn dropQuery (mapM_ (executeParsedStatement conn dropQuery))
{-# WARNING deleteFunction "DuckDB cannot drop a function that its C API registered. deleteFunction raises SQLError for a function from createFunction. Register the name again to replace the function." #-}

-- | Quote each dot-separated part of a function name.
qualifyIdentifier :: Text -> Text
qualifyIdentifier = Text.intercalate "." . map quoteIdent . Text.splitOn "."

-- | Quote one SQL identifier.
quoteIdent :: Text -> Text
quoteIdent ident = "\"" <> Text.replace "\"" "\"\"" ident <> "\""
