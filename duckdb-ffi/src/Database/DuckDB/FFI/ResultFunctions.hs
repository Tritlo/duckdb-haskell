module Database.DuckDB.FFI.ResultFunctions (
    c_duckdb_result_return_type,
) where

import Database.DuckDB.FFI.Types
import Foreign.C.Types (CInt (..))
import Foreign.Ptr (Ptr)

{- | Returns the return_type of the given result, or DUCKDB_RETURN_TYPE_INVALID on
error

Parameters:
* @result@: The result object

Returns The return_type

These bindings call the wrapper symbol
@wrapped_duckdb_result_return_type@ but mirror the DuckDB C API semantics of
@duckdb_result_return_type@.
-}
foreign import ccall safe "wrapped_duckdb_result_return_type"
    c_duckdb_result_return_type :: Ptr DuckDBResult -> IO DuckDBResultType
