{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Database.DuckDB.Simple.Function
Description : Register Haskell scalar functions, casts, table functions, and aggregate functions with DuckDB connections.

Register pure and IO scalar functions with the managed argument and result
conversions. Register pure casts with the same argument conversion. Register
table functions and aggregate functions that convert their arguments with
'FromRow'.
-}
module Database.DuckDB.Simple.Function (
    ScalarType,
    ScalarValue,
    FunctionArg (),
    FunctionResult (),
    Function (..),
    LogicalTypeId (..),
    createFunction,
    createFunctionWithState,
    deleteFunction,
    TypeDeclaration (..),
    createCast,
    TypedRow,
    createTableFunction,
    Aggregate (..),
    createAggregate,
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
import Database.DuckDB.Simple.Typed.Aggregate (Aggregate (..), createAggregateOn)
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
type from 'Database.DuckDB.Simple.Registration.registerCustomType'.

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

DuckDB reads the list one chunk at a time, in one thread. It stops when the
query needs no more rows, so the list can be long or infinite, for example with
@LIMIT@. It reads a full chunk, and it can read more chunks than the query
needs. Thus it can read rows after the @LIMIT@. It reads all columns of each
row that it reads, also the columns that the query does not use. An exception
in the function or in a row is an SQL error. DuckDB releases the list when the
scan ends.

> createTableFunction conn "haskell_squares" ["n", "square"] \(Only count) ->
>     pure [(n, n * n) | n <- [0 .. count - 1 :: Int64]]
-}
createTableFunction :: (FromRow params, TypedRow params, TypedRow row) => Connection -> Text -> [Text] -> (params -> IO [row]) -> IO ()
createTableFunction conn = createTableFunctionOn (ConnectionTarget conn)

{- | Register an aggregate function. The function folds the rows of each group
into a state.

'aggregateStep' adds one row to a state. The step does not run for a row that
has NULL in an argument. A group with no other rows gets the result of
'aggregateInitial'. DuckDB can split a group into partial states, for example
over threads or for a window frame. 'aggregateCombine' merges two partial
states. DuckDB also merges a partial state into a new state, so
'aggregateCombine' must give the other state when one state is
'aggregateInitial'. Each new state is evaluated to weak head normal form.

The arguments decode with 'Database.DuckDB.Simple.FromRow.FromRow'. The SQL
type of each argument is the 'Database.DuckDB.Simple.ToField.duckdbColumnType'
of its column in 'TypedRow'. The SQL return type comes from 'FunctionResult':
BIGINT for 'Int', 'Data.Int.Int16', 'Data.Int.Int32', 'Data.Int.Int64',
'Data.Word.Word16', and 'Data.Word.Word32'; UBIGINT for 'Word' and
'Data.Word.Word64'; DOUBLE for 'Double' and 'Float'; BOOLEAN for 'Bool'; and
VARCHAR for 'Text' and 'String'. A 'Maybe' result has the SQL type of its
value, and 'Nothing' gives NULL. Thus an @Int32@ argument is INTEGER, but an
@Int32@ result is BIGINT.

An exception in a function fails the query. DuckDB can run the functions on
more than one thread at one time, but not on the same state. The aggregate
function works with GROUP BY, with DISTINCT, FILTER, and ORDER BY, and as a
window function.

> createAggregate conn "haskell_sum" Aggregate
>     { aggregateInitial = 0 :: Int64
>     , aggregateStep = \total (Only value) -> pure (total + value)
>     , aggregateCombine = \a b -> pure (a + b)
>     , aggregateResult = pure
>     }
-}
createAggregate :: (FromRow args, TypedRow args, FunctionResult result) => Connection -> Text -> Aggregate state args result -> IO ()
createAggregate conn = createAggregateOn (ConnectionTarget conn)

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
