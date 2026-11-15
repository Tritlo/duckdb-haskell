{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Database.DuckDB.Simple.Typed.Table
-- Description : Register typed table functions and write typed rows to output chunks.
--
-- A typed table function converts its arguments with 'FromRow'. It gives the rows
-- of a scan as a Haskell list of 'TypedRow' values. The exec callback writes the
-- list to DuckDB one output chunk at a time. See Note [Row lists of typed scans].
module Database.DuckDB.Simple.Typed.Table (
  createTableFunctionOn,
  writeRows,
) where

import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar)
import Control.Exception (bracket, throwIO)
import Control.Monad (forM, forM_, when, zipWithM_)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Database.DuckDB.Simple.Callback (attachOpaque, readCallbackState, targetCastFactory, transferCallbackState)
import Database.DuckDB.Simple.FromField (Field (..))
import Database.DuckDB.Simple.FromRow (FromRow (..), parseRow)
import Database.DuckDB.Simple.Materialize (materializeValue, rejectRows)
import Database.DuckDB.Simple.Native
import Database.DuckDB.Simple.Ok (Ok (..))
import Database.DuckDB.Simple.Typed (
  ColumnValue (..),
  TypedRow (..),
  argumentName,
  checkArgumentType,
  conversionMessage,
  rowDeclarations,
 )
import Database.DuckDB.Simple.V2.Registration (
  FunctionParameter (..),
  FunctionSignature (..),
  RegistrationTarget,
  TableCallbacks (..),
  configureFunctionSignature,
  parameter,
  registerTableFunctionWith,
  tableCallbacks,
  withRegistrationFactory,
  withTypeDeclaration,
 )

{- Note [Row lists of typed scans]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A typed table function (createTableFunctionOn) and the COPY FROM side of a
typed format (copyFromSide) give the rows of a scan as a Haskell list of
TypedRow values. The two use the same callbacks.

- The bind callback reads the inputs of the call. Its bind data is the action
  that gives the list.

- The global init callback runs the action, and keeps the list in an MVar in
  the global state. The action runs one time for each scan.

- The exec callback takes the list from the MVar, writes the first rows to the
  output chunk (writeRows), and puts the other rows back. In the exec callback
  of a table function, a call that writes no row ends the scan.

DuckDB can run exec callbacks of one scan at the same time. For a plain table
function, the maximum number of threads of a scan "Defaults to 1, a
single-threaded scan" (duckdb_v2_table_function_init_global_set_max_threads in
the header), and the typed scans do not change it. But a multi-file function
(registerMultiFileFunction) over the table function "claims the batches of the
function itself, so that the rows of a file keep their order also when several
threads scan it" (duckdb_v2_table_function_set_claim_batch_callback in the
header). Then DuckDB 2.0.0 runs several exec callbacks of the scan of one file
at the same time. The lock of the MVar gives each row to one call only.

A typed table function declares projection pushdown. DuckDB then asks the exec
callback for only the columns that the query uses. When the query uses no
column, for example in SELECT count(*), DuckDB 2.0.0 asks for the first column:
the function declares no virtual column, so LogicalGet::GetAnyColumn gives
column 0 (logical_get.cpp), and CV2TableScanColumns gives column 0 for an empty
projection (capi_v2_func_table.cpp). writeRows makes the values of the
requested columns only, so it does not evaluate the other columns of a row. But
it evaluates each row that it writes to its constructor, and this evaluates
each strict field of the row. The COPY FROM side asks writeRows for all
columns.

writeRows evaluates only the spine of the rows that fit in the chunk
(splitRows). It does not evaluate the other rows. The list that it gives back
is the list after the written rows, not a reference to the whole list. DuckDB
takes the rows one chunk at a time, so the list can be lazy. DuckDB stops when
the query needs no more rows, so the list can be long or infinite.
-}

-- | Register a typed table function on a connection or a loading extension.
-- 'Database.DuckDB.Simple.Function.createTableFunction' gives the rules.
--
-- The bind callback converts the arguments and declares the result columns.
-- See Note [Row lists of typed scans]. A parameter type or a column type with
-- no declaration raises an error at registration. See
-- Note [Type declarations] in "Database.DuckDB.Simple.ToField".
createTableFunctionOn
  :: forall params row. (FromRow params, TypedRow params, TypedRow row) => RegistrationTarget -> Text -> [Text] -> (params -> IO [row]) -> IO ()
createTableFunctionOn target name columns function = do
  when (Text.null name || Text.any (== '\0') name) $
    throwInvalidArgument "table function name must be nonempty and must not contain NUL"
  when (null columnDeclarations) $
    throwInvalidArgument (name <> ": a table function must have at least one result column")
  when (length columns /= length columnDeclarations) $
    throwInvalidArgument (name <> ": " <> count columns <> " column names for " <> count columnDeclarations <> " result columns")
  parameterTypes <-
    rowDeclarations name ["parameter " <> argumentName index | index <- [0 ..]] (rowColumnDeclarations (Proxy @params))
  columnTypes <- rowDeclarations name (map ("column " <>) columns) columnDeclarations
  casts <- targetCastFactory target
  let signature =
        FunctionSignature
          [ (parameter (argumentName index) dtype){parameterKind = DuckDBV2FunctionParameterKindPositionalOnly}
          | (index, dtype) <- zip [0 ..] parameterTypes
          ]
          Nothing
      configure factory tableFunction = do
        withIdentifier name (checked . c_duckdb_v2_table_function_set_name tableFunction)
        output (c_duckdb_v2_table_function_get_signature tableFunction) >>= configureFunctionSignature factory signature
      bind input result context = withContextFactory context \factory -> do
        params <- readArguments name (length parameterTypes) (fromMaybe factory casts) input
        forM_ (zip columns columnTypes) \(column, dtype) ->
          withTypeDeclaration factory dtype \logical ->
            withIdentifier column \identifier -> checked (c_duckdb_v2_table_function_bind_add_result_column result identifier logical)
        transferCallbackState (attachOpaque (c_duckdb_v2_function_bind_set_bind_data input)) (function params)
  withRegistrationFactory target \factory -> do
    -- The argument reader reads no VARIANT. Thus reject such a type before the
    -- registration.
    forM_ (zip [0 ..] parameterTypes) \(index, dtype) ->
      withTypeDeclaration factory dtype (checkArgumentType True name ("parameter " <> argumentName index))
    registerTableFunctionWith
      target
      (configure factory)
      (tableCallbacks bind execute){tableInitGlobalCallback = Just initialize, tableProjectionPushdown = True}
  where
    count = Text.pack . show . length
    columnDeclarations = rowColumnDeclarations (Proxy @row)
    initialize info _ = do
      scan <- readCallbackState (c_duckdb_v2_table_function_init_global_get_bind_data info)
      remaining <- scan >>= newMVar :: IO (MVar [row])
      transferCallbackState (attachOpaque (c_duckdb_v2_table_function_init_global_set_global_state info)) remaining
    execute info context = withContextFactory context \factory -> do
      remaining <- readCallbackState (c_duckdb_v2_table_function_exec_get_global_state info) :: IO (MVar [row])
      chunk <- output (c_duckdb_v2_table_function_exec_get_output_chunk info)
      -- The declared column of each vector. See Note [Row lists of typed scans].
      requested <- output (c_duckdb_v2_table_function_exec_get_column_count info)
      selected <- forM (takeWhile (< requested) [0 ..]) \index ->
        fromIntegral <$> output (c_duckdb_v2_table_function_exec_get_column_index info index)
      -- The lock gives each row to one call only. See
      -- Note [Row lists of typed scans].
      modifyMVar_ remaining (writeRows factory chunk selected)

-- | Write the first rows of a list to the vectors of an output chunk. Return
-- the rows that do not fit in the chunk. The function does not evaluate those
-- rows. The list of columns gives the column of the row for each vector of the
-- chunk, in the order of the vectors. The function makes the values of these
-- columns only, so it does not evaluate the other columns. It evaluates each
-- written row to its constructor, so it evaluates each strict field of the row.
-- The values come from the borrowed factory. The function destroys each value
-- after it writes it. The size of each vector becomes the number of written
-- rows. An empty list writes no row. In the exec callback of a table function,
-- no row ends the scan. See Note [Row lists of typed scans].
writeRows :: forall row. (TypedRow row) => DuckDBV2FactoryHandle -> DuckDBV2DataChunkHandle -> [Int] -> [row] -> IO [row]
writeRows factory chunk columns rows = do
  capacity <- fromIntegral <$> output (c_duckdb_v2_data_chunk_get_capacity chunk)
  vectorCount <- output (c_duckdb_v2_data_chunk_get_vector_count chunk)
  let width = length (rowColumnDeclarations (Proxy @row))
  when (fromIntegral vectorCount /= length columns || any (\column -> column < 0 || column >= width) columns) $
    throwInternalError
      ( "the output chunk has "
          <> Text.pack (show vectorCount)
          <> " vectors for the columns "
          <> Text.pack (show columns)
          <> " of a row of "
          <> Text.pack (show width)
          <> " columns"
      )
  case splitRows capacity rows of
    (now, later) -> do
      let size = fromIntegral (length now)
      vectors <- forM (take (length columns) [0 ..]) \index -> do
        vector <- output (c_duckdb_v2_data_chunk_get_vector chunk index)
        -- Flatten after the size changes. See Note [Vector flatten] in
        -- "Database.DuckDB.Simple.Materialize".
        checked (c_duckdb_v2_vector_set_size vector size)
        checked (c_duckdb_v2_vector_flatten vector)
        pure vector
      -- Use loops with a constant stack. See Note [Constant stack at safe calls]
      -- in "Database.DuckDB.Simple.Native".
      forM_ (zip [0 ..] now) \(index, row) -> do
        let values = rowColumnValues row
        zipWithM_
          (\vector (ColumnValue make) -> bracket (make factory) destroyValue (checked . c_duckdb_v2_vector_set_value vector index))
          vectors
          (map (values !!) columns)
      pure later

-- | Split a list after at most the given number of elements. Evaluate the
-- spine of the first part only. The second part is the list after those
-- elements, not a reference to the whole list. See
-- Note [Row lists of typed scans].
--
-- In GHC 9.14.1, 'splitAt' of base gives its second part through a lazy
-- pattern binding on the pair of each recursive call, so that part is a thunk
-- on the inner pairs. This function gives the second part as the plain rest of
-- the list.
splitRows :: Int -> [a] -> ([a], [a])
splitRows = go []
  where
    go taken 0 rest = (reverse taken, rest)
    go taken _ [] = (reverse taken, [])
    go taken limit (row : rest) = go (row : taken) (limit - 1) rest

-- | Read the constant arguments of one call and convert them with 'FromRow'.
-- A NULL argument is @FieldNull@. An argument that does not convert raises an
-- error that names the table function.
readArguments :: (FromRow params) => Text -> Int -> DuckDBV2FactoryHandle -> DuckDBV2FunctionBindInfoHandle -> IO params
readArguments name count factory input = do
  fields <- forM (take count [0 ..]) \index ->
    bracket (output (c_duckdb_v2_function_bind_get_arg_value input (fromIntegral index))) destroyValue \value ->
      Field (argumentName index) index <$> materializeValue unexpectedVariant factory value
  case parseRow fromRow fields of
    Ok params -> pure params
    Errors failures -> throwIO (userError ("duckdb-simple: " <> Text.unpack name <> ": " <> conversionMessage failures))
  where
    unexpectedVariant = rejectRows (throwInvalidArgument "the type of a table function argument cannot be or contain VARIANT")
