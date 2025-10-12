module Database.DuckDB.FFI.ExtractStatements (
    c_duckdb_extract_statements,
    c_duckdb_prepare_extracted_statement,
    c_duckdb_extract_statements_error,
    c_duckdb_destroy_extracted,
) where

import Database.DuckDB.FFI.Types
import Foreign.C.String (CString)
import Foreign.C.Types (CInt (..))
import Foreign.Ptr (Ptr)

{- | Extract all statements from a query. Note that after calling
@duckdb_extract_statements@, the extracted statements should always be
destroyed using @duckdb_destroy_extracted@, even if no statements were
extracted.

If the extract fails, @duckdb_extract_statements_error@ can be called to
obtain the reason why the extract failed.

Parameters:
* @connection@: The connection object
* @query@: The SQL query to extract
* @out_extracted_statements@: The resulting extracted statements object

Returns The number of extracted statements or 0 on failure.
-}
foreign import ccall safe "duckdb_extract_statements"
    c_duckdb_extract_statements :: DuckDBConnection -> CString -> Ptr DuckDBExtractedStatements -> IO DuckDBIdx

{- | Prepare an extracted statement. Note that after calling
@duckdb_prepare_extracted_statement@, the prepared statement should always be
destroyed using @duckdb_destroy_prepare@, even if the prepare fails.

If the prepare fails, @duckdb_prepare_error@ can be called to obtain the
reason why the prepare failed.

Parameters:
* @connection@: The connection object
* @extracted_statements@: The extracted statements object
* @index@: The index of the extracted statement to prepare
* @out_prepared_statement@: The resulting prepared statement object

Returns @DuckDBSuccess@ on success or @DuckDBError@ on failure.
-}
foreign import ccall safe "duckdb_prepare_extracted_statement"
    c_duckdb_prepare_extracted_statement :: DuckDBConnection -> DuckDBExtractedStatements -> DuckDBIdx -> Ptr DuckDBPreparedStatement -> IO DuckDBState

{- | Returns the error message contained within the extracted statements. The
result of this function must not be freed. It will be cleaned up when
@duckdb_destroy_extracted@ is called.

Parameters:
* @extracted_statements@: The extracted statements to fetch the error from.

Returns The error of the extracted statements.
-}
foreign import ccall safe "duckdb_extract_statements_error"
    c_duckdb_extract_statements_error :: DuckDBExtractedStatements -> IO CString

{- | De-allocates all memory allocated for the extracted statements.

Parameters:
* @extracted_statements@: The extracted statements to destroy.
-}
foreign import ccall safe "duckdb_destroy_extracted"
    c_duckdb_destroy_extracted :: Ptr DuckDBExtractedStatements -> IO ()
