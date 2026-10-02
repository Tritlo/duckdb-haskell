{- | Arrow C Data Interface conversion and callbacks.

The @mkArrow...@ functions invoke existing callback pointers. The
@wrapArrow...@ functions allocate callback pointers for Haskell functions.
Keep each allocated pointer alive while native code can call it. Free it with
@freeHaskellFunPtr@ after its last use. Catch all callback exceptions before
they can return to C.
-}
module Database.DuckDB.FFI.Arrow (
    c_duckdb_to_arrow_schema,
    c_duckdb_data_chunk_to_arrow,
    c_duckdb_schema_from_arrow,
    c_duckdb_data_chunk_from_arrow,
    c_duckdb_destroy_arrow_converted_schema,
    mkArrowSchemaRelease,
    mkArrowArrayRelease,
    mkArrowStreamGetSchema,
    mkArrowStreamGetNext,
    mkArrowStreamGetLastError,
    mkArrowStreamRelease,
    wrapArrowSchemaRelease,
    wrapArrowArrayRelease,
    wrapArrowStreamGetSchema,
    wrapArrowStreamGetNext,
    wrapArrowStreamGetLastError,
    wrapArrowStreamRelease,
    releaseArrowSchema,
    releaseArrowArray,
    releaseArrowStream,
) where

import Control.Exception (mask_)
import Control.Monad (when)
import Database.DuckDB.FFI.Types
import Foreign.C.String (CString)
import Foreign.C.Types (CInt (..))
import Foreign.Ptr (FunPtr, Ptr, nullFunPtr)
import Foreign.Storable (peek)

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

-- | Invoke a non-null Arrow schema release callback.
foreign import ccall safe "dynamic"
    mkArrowSchemaRelease :: FunPtr (Ptr ArrowSchema -> IO ()) -> Ptr ArrowSchema -> IO ()

-- | Invoke a non-null Arrow array release callback.
foreign import ccall safe "dynamic"
    mkArrowArrayRelease :: FunPtr (Ptr ArrowArray -> IO ()) -> Ptr ArrowArray -> IO ()

-- | Invoke the schema callback of an unreleased Arrow stream.
foreign import ccall safe "dynamic"
    mkArrowStreamGetSchema :: FunPtr (Ptr ArrowArrayStream -> Ptr ArrowSchema -> IO CInt) -> Ptr ArrowArrayStream -> Ptr ArrowSchema -> IO CInt

-- | Invoke the next-array callback of an unreleased Arrow stream.
foreign import ccall safe "dynamic"
    mkArrowStreamGetNext :: FunPtr (Ptr ArrowArrayStream -> Ptr ArrowArray -> IO CInt) -> Ptr ArrowArrayStream -> Ptr ArrowArray -> IO CInt

-- | Read a stream error after a failed callback. Copy it before the next callback.
foreign import ccall safe "dynamic"
    mkArrowStreamGetLastError :: FunPtr (Ptr ArrowArrayStream -> IO CString) -> Ptr ArrowArrayStream -> IO CString

-- | Invoke a non-null Arrow stream release callback.
foreign import ccall safe "dynamic"
    mkArrowStreamRelease :: FunPtr (Ptr ArrowArrayStream -> IO ()) -> Ptr ArrowArrayStream -> IO ()

-- | Allocate a C-callable Arrow schema release callback.
foreign import ccall "wrapper"
    wrapArrowSchemaRelease :: (Ptr ArrowSchema -> IO ()) -> IO (FunPtr (Ptr ArrowSchema -> IO ()))

-- | Allocate a C-callable Arrow array release callback.
foreign import ccall "wrapper"
    wrapArrowArrayRelease :: (Ptr ArrowArray -> IO ()) -> IO (FunPtr (Ptr ArrowArray -> IO ()))

-- | Allocate a C-callable Arrow stream schema callback.
foreign import ccall "wrapper"
    wrapArrowStreamGetSchema :: (Ptr ArrowArrayStream -> Ptr ArrowSchema -> IO CInt) -> IO (FunPtr (Ptr ArrowArrayStream -> Ptr ArrowSchema -> IO CInt))

-- | Allocate a C-callable Arrow stream next-array callback.
foreign import ccall "wrapper"
    wrapArrowStreamGetNext :: (Ptr ArrowArrayStream -> Ptr ArrowArray -> IO CInt) -> IO (FunPtr (Ptr ArrowArrayStream -> Ptr ArrowArray -> IO CInt))

-- | Allocate a C-callable Arrow stream last-error callback.
foreign import ccall "wrapper"
    wrapArrowStreamGetLastError :: (Ptr ArrowArrayStream -> IO CString) -> IO (FunPtr (Ptr ArrowArrayStream -> IO CString))

-- | Allocate a C-callable Arrow stream release callback.
foreign import ccall "wrapper"
    wrapArrowStreamRelease :: (Ptr ArrowArrayStream -> IO ()) -> IO (FunPtr (Ptr ArrowArrayStream -> IO ()))

{- | Release an Arrow schema unless its release callback is null.
The pointer must address an initialized structure. The structure itself stays
allocated. Asynchronous exceptions are masked during release.
-}
releaseArrowSchema :: Ptr ArrowSchema -> IO ()
releaseArrowSchema ptr = mask_ $ do
    schema <- peek ptr
    when (arrowSchemaRelease schema /= nullFunPtr) $
        mkArrowSchemaRelease (arrowSchemaRelease schema) ptr

{- | Release an Arrow array unless its release callback is null.
The pointer must address an initialized structure. The structure itself stays
allocated. Asynchronous exceptions are masked during release.
-}
releaseArrowArray :: Ptr ArrowArray -> IO ()
releaseArrowArray ptr = mask_ $ do
    array <- peek ptr
    when (arrowArrayRelease array /= nullFunPtr) $
        mkArrowArrayRelease (arrowArrayRelease array) ptr

{- | Release an Arrow stream unless its release callback is null.
The pointer must address an initialized structure. The structure itself stays
allocated. Asynchronous exceptions are masked during release.
-}
releaseArrowStream :: Ptr ArrowArrayStream -> IO ()
releaseArrowStream ptr = mask_ $ do
    stream <- peek ptr
    when (arrowStreamRelease stream /= nullFunPtr) $
        mkArrowStreamRelease (arrowStreamRelease stream) ptr
