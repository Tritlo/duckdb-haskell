{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Database.DuckDB.Simple.Typed.Table
Description : Register typed table functions and write typed rows to output chunks.

A typed table function converts its arguments with 'FromRow'. It gives the rows
of a scan as a Haskell list of 'TypedRow' values. The exec callback writes the
list to DuckDB one output chunk at a time.
-}
module Database.DuckDB.Simple.Typed.Table (
    createTableFunctionOn,
    writeRows,
) where

import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar)
import Control.Exception (bracket, throwIO)
import Control.Monad (forM, forM_, when, zipWithM_)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Database.DuckDB.Simple.Callback (attachOpaque, readCallbackState, transferCallbackState)
import Database.DuckDB.Simple.FromField (Field (..), FieldValue (..))
import Database.DuckDB.Simple.FromRow (FromRow (..), parseRow)
import Database.DuckDB.Simple.Materialize (materializeValue)
import Database.DuckDB.Simple.Native
import Database.DuckDB.Simple.Ok (Ok (..))
import Database.DuckDB.Simple.Typed (ColumnValue (..), TypedRow (..), argumentName, conversionMessage)
import Database.DuckDB.Simple.V2.Registration (
    FunctionParameter (..),
    FunctionSignature (..),
    RegistrationTarget,
    TableCallbacks (..),
    TypeDeclaration (..),
    configureFunctionSignature,
    parameter,
    registerTableFunctionWith,
    tableCallbacks,
    withRegistrationFactory,
    withTypeDeclaration,
 )

{- | Register a typed table function on a connection or a loading extension.
'Database.DuckDB.Simple.Function.createTableFunction' gives the rules.

The bind callback converts the arguments and declares the result columns. Its
bind data is the scan action of the call. The global init callback runs the
scan action and keeps the list in the global state. The exec callback writes
the next rows of the list.
-}
createTableFunctionOn :: forall params row. (FromRow params, TypedRow params, TypedRow row) => RegistrationTarget -> Text -> [Text] -> (params -> IO [row]) -> IO ()
createTableFunctionOn target name columns function = do
    when (Text.null name || Text.any (== '\0') name) $
        throwInvalidArgument "table function name must be nonempty and must not contain NUL"
    when (null columnTypes) $
        throwInvalidArgument (name <> ": a table function must have at least one result column")
    when (length columns /= length columnTypes) $
        throwInvalidArgument (name <> ": " <> count columns <> " column names for " <> count columnTypes <> " result columns")
    withRegistrationFactory target \factory ->
        registerTableFunctionWith target (configure factory) (tableCallbacks bind execute){tableInitGlobalCallback = Just initialize}
  where
    count = Text.pack . show . length
    parameterTypes = rowColumnTypes (Proxy :: Proxy params)
    columnTypes = rowColumnTypes (Proxy :: Proxy row)
    signature =
        FunctionSignature
            [ (parameter (argumentName index) (TypeExpression dtype)){parameterKind = DuckDBV2FunctionParameterKindPositionalOnly}
            | (index, dtype) <- zip [0 ..] parameterTypes
            ]
            Nothing
    configure factory tableFunction = do
        withIdentifier name (checked . c_duckdb_v2_table_function_set_name tableFunction)
        output (c_duckdb_v2_table_function_get_signature tableFunction) >>= configureFunctionSignature factory signature
    bind input result context = withContextFactory context \factory -> do
        params <- readArguments name (length parameterTypes) factory input
        forM_ (zip columns columnTypes) \(column, dtype) ->
            withTypeDeclaration factory (TypeExpression dtype) \logical ->
                withIdentifier column \identifier -> checked (c_duckdb_v2_table_function_bind_add_result_column result identifier logical)
        transferCallbackState (attachOpaque (c_duckdb_v2_function_bind_set_bind_data input)) (function params)
    initialize info _ = do
        scan <- readCallbackState (c_duckdb_v2_table_function_init_global_get_bind_data info)
        remaining <- scan >>= newMVar :: IO (MVar [row])
        transferCallbackState (attachOpaque (c_duckdb_v2_table_function_init_global_set_global_state info)) remaining
    execute info context = withContextFactory context \factory -> do
        remaining <- readCallbackState (c_duckdb_v2_table_function_exec_get_global_state info) :: IO (MVar [row])
        chunk <- output (c_duckdb_v2_table_function_exec_get_output_chunk info)
        -- DuckDB can run exec callbacks of one scan at the same time, for example for a
        -- multi-file function. The lock gives each row to one call only.
        modifyMVar_ remaining (writeRows factory chunk)

{- | Write the first rows of a list to the vectors of an output chunk. Return
the rows that do not fit in the chunk. The function does not evaluate those
rows. The chunk must have one vector for each column of the row, in order. The
values come from the borrowed factory. The function destroys each value after
it writes it. The size of each vector becomes the number of written rows. An
empty list writes no row. In the exec callback of a table function, no row ends
the scan.
-}
writeRows :: forall row. (TypedRow row) => DuckDBV2FactoryHandle -> DuckDBV2DataChunkHandle -> [row] -> IO [row]
writeRows factory chunk rows = do
    capacity <- fromIntegral <$> output (c_duckdb_v2_data_chunk_get_capacity chunk)
    vectorCount <- output (c_duckdb_v2_data_chunk_get_vector_count chunk)
    let width = length (rowColumnTypes (Proxy :: Proxy row))
    when (fromIntegral vectorCount /= width) $
        throwInvalidArgument ("the output chunk has " <> Text.pack (show vectorCount) <> " vectors for " <> Text.pack (show width) <> " row columns")
    case splitRows capacity rows of
        (now, later) -> do
            let size = fromIntegral (length now)
            vectors <- forM (take width [0 ..]) \index -> do
                vector <- output (c_duckdb_v2_data_chunk_get_vector chunk index)
                -- The set operation checks the row against the size. Flatten after the size changes.
                checked (c_duckdb_v2_vector_set_size vector size)
                checked (c_duckdb_v2_vector_flatten vector)
                pure vector
            -- Use loops with a constant stack. The runtime reads the stack of
            -- the thread at each native call, so a deep stack makes each call slow.
            forM_ (zip [0 ..] now) \(index, row) ->
                zipWithM_ (\vector (ColumnValue make) -> bracket (make factory) destroyValue (checked . c_duckdb_v2_vector_set_value vector index)) vectors (rowColumnValues row)
            pure later

{- | Split a list after at most the given number of elements. Evaluate the
spine of the first part only. The second part is the list after those
elements, not a reference to the whole list.
-}
splitRows :: Int -> [a] -> ([a], [a])
splitRows = go []
  where
    go taken 0 rest = (reverse taken, rest)
    go taken _ [] = (reverse taken, [])
    go taken limit (row : rest) = go (row : taken) (limit - 1) rest

{- | Read the constant arguments of one call and convert them with 'FromRow'.
A NULL argument is 'FieldNull'. An argument that does not convert raises an
error that names the table function.
-}
readArguments :: (FromRow params) => Text -> Int -> DuckDBV2FactoryHandle -> DuckDBV2FunctionBindInfoHandle -> IO params
readArguments name count factory input = do
    fields <- forM (take count [0 ..]) \index ->
        bracket (output (c_duckdb_v2_function_bind_get_arg_value input (fromIntegral index))) destroyValue \value -> do
            isNull <- output (c_duckdb_v2_value_is_null value)
            Field (argumentName index) index
                <$> if isNull /= 0 then pure FieldNull else materializeValue unexpectedVariant factory value
    case parseRow fromRow fields of
        Ok params -> pure params
        Errors failures -> throwIO (userError ("duckdb-simple: " <> Text.unpack name <> ": " <> conversionMessage failures))
  where
    unexpectedVariant _ = pure \_ -> throwIO (userError "duckdb-simple: the type of a table function argument cannot be or contain VARIANT")
