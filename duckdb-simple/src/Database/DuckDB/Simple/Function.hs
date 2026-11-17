{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Database.DuckDB.Simple.Function
-- Description : Register Haskell scalar functions, table functions, and aggregate functions with DuckDB connections.
--
-- Register pure and IO scalar functions with the managed argument and result
-- conversions. Register table functions and aggregate functions that convert
-- their arguments with 'FromRow'.
--
-- A program that registers a callback must be linked with @-threaded@. DuckDB
-- can call a callback from several of its threads at the same time, and the
-- runtime of GHC without @-threaded@ does not support that: the program can
-- crash. Without @-threaded@, the registration of a function in this module
-- raises an t'Database.DuckDB.Simple.SQLError' of
-- the type 'Database.DuckDB.Simple.ErrorInputInvalid' when the setting
-- @threads@ is above 1. The section \"User-Defined Functions\" of the README
-- gives the rule.
--
-- A callback must not run a statement on the connection that runs the callback.
-- Use another connection of the same instance. This rule applies to each
-- registered callback, for example of a scalar function, a cast, an aggregate
-- function, a table function, a COPY format, a replacement scan, a pragma, or a
-- log sink. The library holds the lock of a connection while it prepares a
-- statement, while 'Database.DuckDB.Simple.query',
-- 'Database.DuckDB.Simple.execute', or a default fold runs a statement, and
-- while it fetches a chunk of an explicit stream. DuckDB can run a callback in
-- each of these steps. The lock is not reentrant. Thus a statement in the
-- callback waits for the lock. With more than one DuckDB thread, DuckDB can also
-- run a callback of an explicit stream between two fetches, while no fetch holds
-- the lock. Then a statement in the callback on the same connection fails at
-- once with the error of DuckDB for a connection with an open result.
--
-- The wait has the bound 'Database.DuckDB.Simple.connectionCallbackLockWait' of
-- the connection: 10 seconds by default, and no bound for 'Nothing'. After the
-- bound, the statement raises an t'Database.DuckDB.Simple.SQLError' of the type
-- 'Database.DuckDB.Simple.ErrorResourceInUse'. For most callbacks, the outer
-- statement then fails. DuckDB ignores the error of a log sink. The bound
-- applies to each wait of a callback for a connection lock, also for a second
-- connection that another statement uses. The bound does not apply to
-- a callback that DuckDB runs while the library holds no lock of the
-- connection, for example a log sink for a message of
-- 'Database.DuckDB.Simple.Logging.logConnectionMessage'. It also does not apply
-- to a statement in a thread that the callback starts, to a statement inside
-- 'Control.Exception.uninterruptibleMask', and to a statement in the flush
-- function of a log sink when DuckDB destroys the sink.
--
-- A callback that refers to a connection keeps that connection and its instance
-- reachable while DuckDB keeps the callback, possibly until the database closes.
-- Thus the garbage collector does not close them, and the program must close
-- them.
module Database.DuckDB.Simple.Function (
  ScalarValue,
  FunctionResult (),
  Function (..),
  LogicalTypeId (..),
  createFunction,
  createFunctionWithState,
  deleteFunction,
  TypeDeclaration (..),
  TypedRow,
  createTableFunction,
  Aggregate (..),
  createAggregate,
) where

import Control.Exception (bracket)
import Control.Monad (forM)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.DuckDB.Simple.Callback (RegistrationTarget (ConnectionTarget))
import Database.DuckDB.Simple.FromRow (FromRow)
import Database.DuckDB.Simple.Function.Internal (
  Function (..),
  FunctionResult,
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
import Database.DuckDB.Simple.Typed (TypedRow)
import Database.DuckDB.Simple.Typed.Aggregate (Aggregate (..), createAggregateOn)
import Database.DuckDB.Simple.Typed.Table (createTableFunctionOn)
import Database.DuckDB.Simple.V2.Registration (TypeDeclaration (..))

-- | Register a Haskell function under the supplied name.
--
-- The result type needs a t'Database.DuckDB.Simple.ToField.ToDuckValue'
-- instance, and the SQL type of the result is its
-- 'Database.DuckDB.Simple.ToField.duckdbTypeDeclaration'. Thus an
-- 'Data.Int.Int16' result is SMALLINT and a 'Float' result is FLOAT. DuckDB
-- computes with that type: the product of two SMALLINT values is SMALLINT, so
-- @f(x) * f(y)@ of an 'Data.Int.Int16' function raises an Out of Range Error
-- when the product does not fit. Cast a result to a wider type first, for
-- example @f(x)::INTEGER * f(y)@. A 'Bool', an integer of 8 to 64 bits, a
-- 'Float', a 'Double', a 'Text', or a 'String' result goes to the memory of the
-- result vector. Each other result type gives a native value for each row
-- through the context of the call. DuckDB casts that value to the declared
-- type, as a SQL @CAST@ does: it rounds a value to the scale of a DECIMAL type
-- text, and it raises an error for a value that does not fit.
--
-- Each argument type needs t'Database.DuckDB.Simple.FromField.FromField' and
-- t'Database.DuckDB.Simple.ToField.DuckDBColumnType' instances. The SQL type of
-- an argument is the 'Database.DuckDB.Simple.ToField.duckdbTypeDeclaration' of
-- its type. An argument type with no declaration raises an @SQLError@ at
-- registration. A type that is or contains VARIANT is an error at
-- registration, because the library cannot read a VARIANT value in a callback
-- (see the section /Typed registration/ of the README). A GEOMETRY argument
-- reads as a GEOMETRY result does, also inside a list or a STRUCT, for example
-- as a @Geometry@.
createFunction :: (Function f) => Connection -> Text -> f -> IO ()
createFunction conn = createFunctionOn (ConnectionTarget conn)

-- | Register a scalar function with per-worker thread-local state. The
-- arguments have the rules of 'createFunction'. A type that is or contains
-- VARIANT is an error at registration, because the library cannot read a
-- VARIANT value in a callback (see the section /Typed registration/ of the
-- README).
createFunctionWithState :: (Function f) => Connection -> Text -> IO s -> (s -> f) -> IO ()
createFunctionWithState conn = createFunctionWithStateOn (ConnectionTarget conn)

-- | Register a table function. The function gives the rows of one scan as a
-- list. The list of names gives the names of the result columns. 'TypedRow' of
-- @row@ gives the column types. A different number of names and columns, or a
-- row with no column, is an error at registration. A @Maybe@ column gives NULL
-- for 'Nothing'. The SQL type of each parameter and each column is the
-- 'Database.DuckDB.Simple.ToField.duckdbTypeDeclaration' of its type. A record
-- or a sum type with 'Database.DuckDB.Simple.Generic.ViaDuckDB' instances is a
-- STRUCT or a UNION column. A type with no declaration, for example
-- 'Database.DuckDB.Simple.LogicalRep.StructValue', is an error at registration.
-- A parameter type that is or contains VARIANT is an error at registration,
-- because the library cannot read a VARIANT value in a callback (see the
-- section /Typed registration/ of the README). A GEOMETRY parameter works.
--
-- 'TypedRow' of @params@ gives the parameter types. Use @()@ for no parameter.
-- The parameters are positional only. DuckDB casts each argument to its
-- parameter type. Then 'FromRow' converts the arguments. An argument that does
-- not convert is an SQL error that names the function. A NULL argument converts
-- only to a @Maybe@ field.
--
-- DuckDB converts the arguments when it plans the query. Thus an argument that
-- does not convert also fails the preparation of a statement. The function runs
-- one time for each scan, when the execution of the statement starts. It does not
-- run at preparation. A query that reads the function two times runs it two
-- times. A prepared statement runs it at each execution.
--
-- DuckDB reads the list one chunk at a time. It can run two exec callbacks of
-- one scan at the same time, for example for a multi-file function. Each exec
-- callback takes its rows from the list under a lock, so two exec callbacks never
-- read the list at the same time. DuckDB stops when the query needs no more rows,
-- so the list can be long or infinite, for example with @LIMIT@. It reads a full
-- chunk, and it can read more chunks than the query needs. Thus it can read rows
-- after the @LIMIT@. DuckDB asks only for the columns that the query uses, also
-- in @WHERE@ and @ORDER BY@. When the query uses no column, as in
-- @SELECT count(*)@, DuckDB asks for the first column. The library evaluates
-- only the columns that DuckDB asks for, so an exception in another column does
-- not fail the query. But the library evaluates each row that it writes to its
-- constructor, and this evaluates each strict field of the row (a field with
-- @!@, or each field with @StrictData@). Thus an exception in a strict field
-- fails the query, also when the query does not use the column. An exception
-- in the function, in the list, in a row, or in a column that the query uses
-- is an SQL error. DuckDB releases the list when the scan ends.
--
-- > createTableFunction conn "haskell_squares" ["n", "square"] \(Only count) ->
-- >     pure [(n, n * n) | n <- [0 .. count - 1 :: Int64]]
createTableFunction :: (FromRow params, TypedRow params, TypedRow row) => Connection -> Text -> [Text] -> (params -> IO [row]) -> IO ()
createTableFunction conn = createTableFunctionOn (ConnectionTarget conn)

-- | Register an aggregate function. The function folds the rows of each group
-- into a state.
--
-- 'aggregateStep' adds one row to a state. The step does not run for a row that
-- has NULL in an argument. A group with no other rows gets the result of
-- 'aggregateInitial'. DuckDB can split a group into partial states, for example
-- over threads or for a window frame. 'aggregateCombine' merges two partial
-- states. DuckDB also merges a partial state into a new state, so
-- 'aggregateCombine' must give the other state when one state is
-- 'aggregateInitial'. Each new state is evaluated to weak head normal form.
--
-- The arguments decode with t'Database.DuckDB.Simple.FromRow.FromRow'. The SQL
-- type of each argument is the
-- 'Database.DuckDB.Simple.ToField.duckdbTypeDeclaration' of its column in
-- 'TypedRow'. A type with no declaration is an error at registration, as for
-- 'createTableFunction'. A type that is or contains VARIANT is an error at
-- registration, because the library cannot read a VARIANT value in a callback
-- (see the section /Typed registration/ of the README). A GEOMETRY argument reads
-- as for 'createFunction', also when DuckDB runs the update callback on several
-- threads. An aggregate function of an extension refuses a GEOMETRY argument
-- at registration, because its update callback has no factory to read the
-- value. The SQL return type is the
-- 'Database.DuckDB.Simple.ToField.duckdbTypeDeclaration' of the result type,
-- for example SMALLINT for 'Data.Int.Int16' and FLOAT for 'Float'. A 'Maybe'
-- result has the SQL type of its value, and 'Nothing' gives NULL.
--
-- The result type must be BOOLEAN, TINYINT to BIGINT, UTINYINT to UBIGINT,
-- FLOAT, DOUBLE, or VARCHAR: 'Bool', 'Int', 'Data.Int.Int8' to
-- 'Data.Int.Int64', 'Word', 'Data.Word.Word8' to 'Data.Word.Word64', 'Float',
-- 'Double', 'Text', 'String', or 'Maybe' of one of them. A newtype over one of
-- them gets its writer with @deriving newtype (FunctionResult)@. Another result
-- type is an error at registration. The finalize callback of DuckDB has no
-- factory, so it cannot make a value of another type. The library writes these
-- types to the memory of the result vector. It does not use the size of the
-- vector: in an ordered window, DuckDB gives a result vector of the size 0.
--
-- An exception in a function fails the query. DuckDB can run the functions on
-- more than one thread at one time, but not on the same state. The aggregate
-- function works with GROUP BY, with DISTINCT, FILTER, and ORDER BY, and as a
-- window function.
--
-- > createAggregate conn "haskell_sum" Aggregate
-- >     { aggregateInitial = 0 :: Int64
-- >     , aggregateStep = \total (Only value) -> pure (total + value)
-- >     , aggregateCombine = \a b -> pure (a + b)
-- >     , aggregateResult = pure
-- >     }
createAggregate :: (FromRow args, TypedRow args, FunctionResult result) => Connection -> Text -> Aggregate state args result -> IO ()
createAggregate conn = createAggregateOn (ConnectionTarget conn)

-- | Run @DROP FUNCTION IF EXISTS@ and report native errors.
-- DuckDB does not permit deletion of functions registered through the C API.
-- For a function from 'createFunction', this call raises t'SQLError' and the
-- function stays registered. Register the name again to replace the function.
-- GHC reports a warning at each use of this function. A build with @-Werror@
-- must allow that warning, for example with @-Wno-error=deprecations@.
--
-- The name follows the rules of DuckDB for a qualified name, as in
-- 'Database.DuckDB.Simple.Catalog.describeTable'. A dot separates two parts,
-- and a part in double quotes can contain a dot. Thus @\"a.b\"@ names the
-- function @a.b@, and @s.f@ names the function @f@ in the schema @s@. A name
-- with a quote inside a part that has no quotes, with an unterminated quote, or
-- with more than three parts raises the t'SQLError' of the parser.
deleteFunction :: Connection -> Text -> IO ()
deleteFunction conn name = do
  qualified <- qualifyIdentifier name
  -- See Note [SQL text of the library] in "Database.DuckDB.Simple.Native".
  let dropQuery = Query ("DROP FUNCTION IF EXISTS " <> qualified)
  withParsedStatements conn dropQuery (mapM_ (executeParsedStatement conn dropQuery))
{-# WARNING
  deleteFunction
  "DuckDB cannot drop a function that its C API registered. deleteFunction raises SQLError for a function from createFunction. Register the name again to replace the function."
  #-}

-- | Parse a qualified function name with @duckdb_v2_qname_parse@, and quote
-- each part with 'quoteIdentifier'.
qualifyIdentifier :: Text -> IO Text
qualifyIdentifier name = withText name \text ->
  bracket (output (c_duckdb_v2_qname_parse text)) (destroy c_duckdb_v2_qname_destroy) \qname -> do
    count <- output (c_duckdb_v2_qname_get_part_count qname)
    parts <- forM (indices count) \index -> output (c_duckdb_v2_qname_get_part qname index) >>= readIdentifier
    pure (Text.intercalate "." (map quoteIdentifier parts))
