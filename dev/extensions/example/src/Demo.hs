{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Register Haskell callbacks through the loading DuckDB host's V2 API.
module Demo where

import Control.Exception (bracket, throwIO)
import Control.Monad (forM_)
import Data.Text (Text)
import Database.DuckDB.FFI.V2
import Database.DuckDB.Simple.Extension (runExtension)
import qualified Database.DuckDB.Simple.Registration as R
import Foreign.Ptr (Ptr)

-- | Export the registration action called by the C load entrypoint.
initialize :: DuckDBV2ExtensionHandle -> DuckDBV2ContextHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()
initialize = runExtension register

foreign export ccall "duckdb_haskell_demo_register"
    initialize :: DuckDBV2ExtensionHandle -> DuckDBV2ContextHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()

-- | Register a result callback and a callback that raises a SQL error.
register :: R.RegistrationTarget -> DuckDBV2ContextHandle -> IO ()
register target context = do
    scalar "haskell_answer" [] answer
    scalar "haskell_add_one" ["value"] addOne
    scalar "haskell_fail" [] (\_ _ -> throwIO (userError "Haskell callback failure"))
  where
    scalar :: Text -> [Text] -> R.ContextCallback DuckDBV2ScalarFunctionExecInfoHandle -> IO ()
    scalar name parameters callback =
        R.registerScalarFunctionWith target configure (R.scalarCallbacks callback)
      where
        configure function = do
            R.withIdentifier name (R.checked . c_duckdb_v2_scalar_function_set_name function)
            signature <- R.output (c_duckdb_v2_scalar_function_get_signature function)
            bracket (R.withText "BIGINT" (R.output . c_duckdb_v2_context_create_type_from_text context)) (R.destroy c_duckdb_v2_logical_type_destroy) \logical -> do
                R.checked (c_duckdb_v2_function_signature_set_return_type signature logical)
                forM_ parameters \parameter ->
                    R.withIdentifier parameter \identifier ->
                        R.checked (c_duckdb_v2_function_signature_add_parameter signature identifier logical R.nullHandle DuckDBV2FunctionParameterKindStandard)

-- | Fill a native batch with values created in its borrowed execution context.
answer :: R.ContextCallback DuckDBV2ScalarFunctionExecInfoHandle
answer info context = do
    count <- R.output (c_duckdb_v2_scalar_function_exec_get_row_count info)
    vector <- R.output (c_duckdb_v2_scalar_function_exec_get_result info)
    R.checked (c_duckdb_v2_vector_set_size vector count)
    bracket (R.output (c_duckdb_v2_value_create_bigint_with_context context 42)) (R.destroy c_duckdb_v2_value_destroy) \value ->
        forM_ (if count == 0 then [] else [0 .. count - 1]) \row ->
            R.checked (c_duckdb_v2_vector_set_value vector row value)

-- | Increment a BIGINT argument. Keep NULL values and report overflow.
addOne :: R.ContextCallback DuckDBV2ScalarFunctionExecInfoHandle
addOne info context = do
    count <- R.output (c_duckdb_v2_scalar_function_exec_get_row_count info)
    input <- R.output (c_duckdb_v2_scalar_function_exec_get_arg info 0)
    result <- R.output (c_duckdb_v2_scalar_function_exec_get_result info)
    R.checked (c_duckdb_v2_vector_set_size result count)
    forM_ (if count == 0 then [] else [0 .. count - 1]) \row ->
        bracket (R.output (c_duckdb_v2_vector_get_value input row)) (R.destroy c_duckdb_v2_value_destroy) \value -> do
            isNull <- R.output (c_duckdb_v2_value_is_null value)
            if isNull /= 0
                then R.checked (c_duckdb_v2_vector_set_value result row value)
                else do
                    integer <- R.output (c_duckdb_v2_value_get_bigint value)
                    if integer == maxBound
                        then throwIO (userError "Haskell BIGINT overflow")
                        else bracket (R.output (c_duckdb_v2_value_create_bigint_with_context context (integer + 1))) (R.destroy c_duckdb_v2_value_destroy) \incremented ->
                            R.checked (c_duckdb_v2_vector_set_value result row incremented)
