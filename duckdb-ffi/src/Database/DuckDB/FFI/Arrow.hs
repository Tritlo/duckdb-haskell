module Database.DuckDB.FFI.Arrow (
    c_duckdb_to_arrow_schema,
    c_duckdb_data_chunk_to_arrow,
    c_duckdb_schema_from_arrow,
    c_duckdb_data_chunk_from_arrow,
    c_duckdb_destroy_arrow_converted_schema,
) where

import Database.DuckDB.FFI.Types
import Foreign.C.String (CString)
import Foreign.Ptr (Ptr)

{- | Transforms a DuckDB Schema into an Arrow Schema

Parameters:
* @arrow_options@: The Arrow settings used to produce arrow.
* @types@: The DuckDB logical types for each column in the schema.
* @names@: The names for each column in the schema.
* @column_count@: The number of columns that exist in the schema.
* @out_schema@: The resulting arrow schema. Must be destroyed with
  @out_schema->release(out_schema)@.

Returns The error data. Must be destroyed with @duckdb_destroy_error_data@.
-}
foreign import ccall safe "duckdb_to_arrow_schema"
    c_duckdb_to_arrow_schema :: DuckDBArrowOptions -> Ptr DuckDBLogicalType -> Ptr CString -> DuckDBIdx -> Ptr ArrowSchema -> IO DuckDBErrorData

{- | Transforms a DuckDB data chunk into an Arrow array.

Parameters:
* @arrow_options@: The Arrow settings used to produce arrow.
* @chunk@: The DuckDB data chunk to convert.
* @out_arrow_array@: The output Arrow structure that will hold the converted
  data. Must be released with @out_arrow_array->release(out_arrow_array)@

Returns The error data. Must be destroyed with @duckdb_destroy_error_data@.
-}
foreign import ccall safe "duckdb_data_chunk_to_arrow"
    c_duckdb_data_chunk_to_arrow :: DuckDBArrowOptions -> DuckDBDataChunk -> Ptr ArrowArray -> IO DuckDBErrorData

{- | Transforms an Arrow Schema into a DuckDB Schema.

Parameters:
* @connection@: The connection to get the transformation settings from.
* @schema@: The input Arrow schema. Must be released with
  @schema->release(schema)@.
* @out_types@: The Arrow converted schema with extra information about the
  arrow types. Must be destroyed with @duckdb_destroy_arrow_converted_schema@.

Returns The error data. Must be destroyed with @duckdb_destroy_error_data@.
-}
foreign import ccall safe "duckdb_schema_from_arrow"
    c_duckdb_schema_from_arrow :: DuckDBConnection -> Ptr ArrowSchema -> Ptr DuckDBArrowConvertedSchema -> IO DuckDBErrorData

{- | Transforms an Arrow array into a DuckDB data chunk. The data chunk will retain
ownership of the underlying Arrow data.

Parameters:
* @connection@: The connection to get the transformation settings from.
* @arrow_array@: The input Arrow array. Data ownership is passed on to
  DuckDB's DataChunk, the underlying object does not need to be released and
  won't have ownership of the data.
* @converted_schema@: The Arrow converted schema with extra information about
  the arrow types.
* @out_chunk@: The resulting DuckDB data chunk. Must be destroyed by
  duckdb_destroy_data_chunk.

Returns The error data. Must be destroyed with @duckdb_destroy_error_data@.
-}
foreign import ccall safe "duckdb_data_chunk_from_arrow"
    c_duckdb_data_chunk_from_arrow :: DuckDBConnection -> Ptr ArrowArray -> DuckDBArrowConvertedSchema -> Ptr DuckDBDataChunk -> IO DuckDBErrorData

{- | Destroys the arrow converted schema and de-allocates all memory allocated for
that arrow converted schema.

Parameters:
* @arrow_converted_schema@: The arrow converted schema to destroy.
-}
foreign import ccall safe "duckdb_destroy_arrow_converted_schema"
    c_duckdb_destroy_arrow_converted_schema :: Ptr DuckDBArrowConvertedSchema -> IO ()
