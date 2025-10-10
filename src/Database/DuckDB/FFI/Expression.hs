module Database.DuckDB.FFI.Expression (
    c_duckdb_destroy_expression,
    c_duckdb_expression_return_type,
    c_duckdb_expression_is_foldable,
    c_duckdb_expression_fold,
) where

import Database.DuckDB.FFI.Types
import Foreign.C.Types (CBool (..))
import Foreign.Ptr (Ptr)

{- | Destroys the expression and de-allocates its memory.

Parameters:
* @expr@: A pointer to the expression.
-}
foreign import ccall unsafe "duckdb_destroy_expression"
    c_duckdb_destroy_expression :: Ptr DuckDBExpression -> IO ()

{- | Returns the return type of an expression.

Parameters:
* @expr@: The expression.

Returns The return type. Must be destroyed with @duckdb_destroy_logical_type@.
-}
foreign import ccall unsafe "duckdb_expression_return_type"
    c_duckdb_expression_return_type :: DuckDBExpression -> IO DuckDBLogicalType

{- | Returns whether the expression is foldable into a value or not.

Parameters:
* @expr@: The expression.

Returns True, if the expression is foldable, else false.
-}
foreign import ccall unsafe "duckdb_expression_is_foldable"
    c_duckdb_expression_is_foldable :: DuckDBExpression -> IO CBool

{- | Folds an expression creating a folded value.

Parameters:
* @context@: The client context.
* @expr@: The expression. Must be foldable.
* @out_value@: The folded value, if folding was successful. Must be destroyed
  with @duckdb_destroy_value@.

Returns The error data. Must be destroyed with @duckdb_destroy_error_data@.
-}
foreign import ccall safe "duckdb_expression_fold"
    c_duckdb_expression_fold :: DuckDBClientContext -> DuckDBExpression -> Ptr DuckDBValue -> IO DuckDBErrorData
