{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

{- | Raw types for the DuckDB V2 C API.

Generated from the pinned header by @scripts/gen_ffi_v2.py@.
Shared C layouts use the types from @Database.DuckDB.FFI.Types@.
Other layouts and enum values come from the header through hsc2hs.
Keep this module and the native library at the same upstream revision.
Source documentation records the upstream API lifecycle status.
-}
module Database.DuckDB.FFI.V2.Types (
    module Database.DuckDB.FFI.V2.Types,
    ArrowSchema (..),
    ArrowArray (..),
    ArrowArrayStream (..),
    DuckDBListEntry (..),
    DuckDBHugeInt (..),
    DuckDBUHugeInt (..),
    DuckDBInterval (..),
    DuckDBIdx,
    DuckDBSel,
    DuckDBDeleteCallback,
) where

#define DUCKDB_V2_API_ALLOW_UNSTABLE 1
#include "duckdb_v2.h"

import Data.Word (Word32, Word64)
import Database.DuckDB.FFI.Types (
    ArrowArray (..),
    ArrowArrayStream (..),
    ArrowSchema (..),
    DuckDBDeleteCallback,
    DuckDBHugeInt (..),
    DuckDBIdx,
    DuckDBInterval (..),
    DuckDBListEntry (..),
    DuckDBSel,
    DuckDBUHugeInt (..),
 )
import Foreign.C.Types (CBool (..), CChar (..), CInt (..))
import Foreign.Ptr (FunPtr, Ptr, castPtr, plusPtr)
import Foreign.Storable (Storable (..), peekByteOff, pokeByteOff)

-- | DuckDB's unsigned index type.
type DuckDBV2Idx = DuckDBIdx

{- | Opaque target of @duckdb_v2_environment_handle@.

Do not read or write the target storage.
-}
data DuckDBV2Environment

{- | An opaque, owned handle to the V2 environment: the required root through which instance handles are created. All
instances under one environment share an instance cache, so file-level conflicts — the same database file opened
twice — are detected across them. Destroying the environment refuses with ERROR_RESOURCE_IN_USE while any instance
created through it is still alive; destroy those first.
-}
type DuckDBV2EnvironmentHandle = Ptr DuckDBV2Environment

{- | Opaque target of @duckdb_v2_instance_handle@.

Do not read or write the target storage.
-}
data DuckDBV2Instance

{- | An opaque, owned handle to a DuckDB database instance: a buffer pool, a scheduler, a set of GLOBAL settings, and the
databases attached to it. Created empty by @duckdb_v2_instance_create()@; databases are attached with
@duckdb_v2_instance_attach()@, made the default with @duckdb_v2_instance_set_default()@, and detached with
@duckdb_v2_instance_detach()@. Always destroy via @duckdb_v2_instance_destroy()@.
-}
type DuckDBV2InstanceHandle = Ptr DuckDBV2Instance

{- | Opaque target of @duckdb_v2_connection_handle@.

Do not read or write the target storage.
-}
data DuckDBV2Connection

{- | An opaque, owned handle to a DuckDB connection: a session on a database instance with its own LOCAL settings and
transaction state. Always destroy via @duckdb_v2_connection_destroy()@.
-}
type DuckDBV2ConnectionHandle = Ptr DuckDBV2Connection

{- | Opaque target of @duckdb_v2_option_handle@.

Do not read or write the target storage.
-}
data DuckDBV2Option

{- | An opaque, owned, read-only descriptor of a single config option as seen from an instance, connection, or context:
its canonical name, current setting, default setting, description, target scope, and aliases. Returned by the
*_get_option and *_get_option_by_index functions; the caller always destroys it via @duckdb_v2_option_destroy()@.
Options are written with the *_set_option functions, which take a name and a setting directly.
-}
type DuckDBV2OptionHandle = Ptr DuckDBV2Option

{- | Opaque target of @duckdb_v2_error_info_handle@.

Do not read or write the target storage.
-}
data DuckDBV2ErrorInfo

-- | Opaque detail for a failed call; destroy with @duckdb_v2_error_info_destroy()@.
type DuckDBV2ErrorInfoHandle = Ptr DuckDBV2ErrorInfo

{- | Opaque target of @duckdb_v2_logical_type_handle@.

Do not read or write the target storage.
-}
data DuckDBV2LogicalType

{- | An opaque, owned handle to a logical type. Carries a type id plus any kind-specific metadata: decimal width and
scale, enum dictionary, list / array / struct / map / union child types, alias. A type is read-only once constructed;
instances arrive from a query result schema, or from one of the create_type_* constructors. Always destroy via
@duckdb_v2_logical_type_destroy()@. Borrowed strings returned by the getters are valid only until then.
-}
type DuckDBV2LogicalTypeHandle = Ptr DuckDBV2LogicalType

{- | Opaque target of @duckdb_v2_value_handle@.

Do not read or write the target storage.
-}
data DuckDBV2Value

{- | An opaque, owned handle to a single SQL value: a logical type plus a typed payload. Always destroy via
@duckdb_v2_value_destroy()@. The string returned by @duckdb_v2_value_get_varchar()@ / @duckdb_v2_value_get_blob()@ is
borrowed and valid only until then. That holds for BIGNUM too, where what is borrowed is the opaque storage form;
@duckdb_v2_bignum_decode()@ translates it into a magnitude and a sign flag, writing into a buffer the caller
supplies.
-}
type DuckDBV2ValueHandle = Ptr DuckDBV2Value

{- | Opaque target of @duckdb_v2_result_handle@.

Do not read or write the target storage.
-}
data DuckDBV2Result

{- | An opaque, owned handle to the streaming result of a query. Carries the schema (column names and logical types), the
statement type, and the result type from prepare time; row data is produced incrementally by stepping
(@duckdb_v2_result_step()@) or draining (@duckdb_v2_result_fetch_chunk()@). Single consumer: step from one thread at
a time.

A result is a cursor on the connection's execution, not a box of data. While it is live — not finished, cancelled,
errored, or destroyed — the connection refuses new queries with ERROR_RESOURCE_IN_USE, and the query's transaction
stays open, deferring version cleanup and checkpointing, so drain or destroy it promptly. Side-effecting statements
(PRAGMA, ALTER, ...) take effect only once the result is drained. Always destroy via @duckdb_v2_result_destroy()@,
which is safe even on a partially consumed stream.
-}
type DuckDBV2ResultHandle = Ptr DuckDBV2Result

{- | Opaque target of @duckdb_v2_data_chunk_handle@.

Do not read or write the target storage.
-}
data DuckDBV2DataChunk

{- | An opaque, owned handle to a data chunk: a set of vectors of equal logical length plus a cardinality (row count).

A data chunk is the main "unit of execution" in DuckDB, and the main unit of data transfer in the API. It may be
owned or borrowed, depending on the context.
-}
type DuckDBV2DataChunkHandle = Ptr DuckDBV2DataChunk

{- | Opaque target of @duckdb_v2_vector_handle@.

Do not read or write the target storage.
-}
data DuckDBV2Vector

{- | A borrowed handle to a vector within a data chunk, carrying a part of a column's values. Vectors are always borrowed
from a data chunk, and are valid only as long as their parent data chunk is alive.
-}
type DuckDBV2VectorHandle = Ptr DuckDBV2Vector

{- | Opaque target of @duckdb_v2_arena_handle@.

Do not read or write the target storage.
-}
data DuckDBV2Arena

{- | A borrowed handle to a "arena allocator".

Bytes handed out by @duckdb_v2_arena_allocate()@ share the same lifetime as the arena itself.
@duckdb_v2_vector_get_arena()@ yields the arena backing the out-of-line bytes of non-inlined VARCHAR / BLOB / BIT /
BIGNUM values, valid until the owning vector is flattened, reallocated, or destroyed.
-}
type DuckDBV2ArenaHandle = Ptr DuckDBV2Arena

{- | Opaque target of @duckdb_v2_context_handle@.

Do not read or write the target storage.
-}
data DuckDBV2Context

{- | A borrowed handle to a client context: a connection seen from inside DuckDB. Handed out within DuckDB-managed scopes
— function bind / init / exec callbacks, replacement scans, and the extension entrypoint — and valid only for the
duration of that scope; the caller never destroys it. A context is the scope for reading (settings, the file system)
and for constructing values and types, not for registration: catalog entries and instance-level hooks are installed
through an extension or a connection. A transaction is always active while a context is handed out, which is what the
context-scoped constructors assume.
-}
type DuckDBV2ContextHandle = Ptr DuckDBV2Context

{- | Opaque target of @duckdb_v2_cast_function_handle@.

Do not read or write the target storage.
-}
data DuckDBV2CastFunction

{- | An owned opaque handle to a cast function being built. Created with
@duckdb_v2_cast_function_create_with_connection()@ or @duckdb_v2_cast_function_create_with_extension()@, configured
with the setter functions (e.g. @duckdb_v2_cast_function_set_source_type()@,
@duckdb_v2_cast_function_set_exec_callback()@, etc.), made available with @duckdb_v2_cast_function_register()@, and
destroyed with @duckdb_v2_cast_function_destroy()@.
-}
type DuckDBV2CastFunctionHandle = Ptr DuckDBV2CastFunction

{- | Opaque target of @duckdb_v2_cast_function_exec_info_handle@.

Do not read or write the target storage.
-}
data DuckDBV2CastFunctionExecInfo

{- | A borrowed opaque handle to the arguments supplied to a cast function during the execution "exec" phase. The "exec"
callback receives this handle and can use it to access the input vector, the output vector to write into, the number
of rows to convert, and the mode the cast is running in.
-}
type DuckDBV2CastFunctionExecInfoHandle = Ptr DuckDBV2CastFunctionExecInfo

{- | Opaque target of @duckdb_v2_column_data_collection_handle@.

Do not read or write the target storage.
-}
data DuckDBV2ColumnDataCollection

{- | A column data collection represents a set of (buffer-managed) data chunks.

You can append chunks to the collection iteratively, and then scan the collection back to retrieve the chunks in
order. DuckDB will manage the memory for the chunks, and transparently offload chunks to disk if necessary. A
collection shall not be scanned while it is being appended to, and vice versa. A collection shall not be appended to
concurrently, but it can be scanned concurrently by multiple threads using a shared scan state and per-thread worker
scan states. For concurrent appends, consider creating multiple collections and "combining" them into one collection
at the end.
-}
type DuckDBV2ColumnDataCollectionHandle = Ptr DuckDBV2ColumnDataCollection

{- | Opaque target of @duckdb_v2_column_data_collection_shared_scan_state_handle@.

Do not read or write the target storage.
-}
data DuckDBV2ColumnDataCollectionSharedScanState

{- | Opaque state handle for shared state across multiple threads scanning a column data collection. This is used to
coordinate work distribution, track global progress, or store any other state that needs to be shared across threads
during the scan of the collection.
-}
type DuckDBV2ColumnDataCollectionSharedScanStateHandle = Ptr DuckDBV2ColumnDataCollectionSharedScanState

{- | Opaque target of @duckdb_v2_column_data_collection_worker_scan_state_handle@.

Do not read or write the target storage.
-}
data DuckDBV2ColumnDataCollectionWorkerScanState

{- | Opaque state handle for local state while scanning a column data collection. This is used to track progress and any
other state that needs to be maintained on a per-thread basis across calls while scanning the collection. It also
keeps the buffers backing the most recently scanned chunk alive: chunks are scanned zero-copy, so a scanned chunk's
data is only valid until the worker state's next scan call or its destruction.
-}
type DuckDBV2ColumnDataCollectionWorkerScanStateHandle = Ptr DuckDBV2ColumnDataCollectionWorkerScanState

{- | Opaque target of @duckdb_v2_column_data_collection_append_state_handle@.

Do not read or write the target storage.
-}
data DuckDBV2ColumnDataCollectionAppendState

{- | Opaque state handle for appending to a column data collection. This is used to track progress and any other state
that needs to be maintained across calls while appending to the collection.
-}
type DuckDBV2ColumnDataCollectionAppendStateHandle = Ptr DuckDBV2ColumnDataCollectionAppendState

{- | Opaque target of @duckdb_v2_custom_type_handle@.

Do not read or write the target storage.
-}
data DuckDBV2CustomType

{- | An owned opaque handle to a custom type being built. Created with @duckdb_v2_custom_type_create_with_connection()@ or
@duckdb_v2_custom_type_create_with_extension()@, configured with @duckdb_v2_custom_type_set_name()@ and
@duckdb_v2_custom_type_set_base_type()@, made available with @duckdb_v2_custom_type_register()@, and destroyed with
@duckdb_v2_custom_type_destroy()@.
-}
type DuckDBV2CustomTypeHandle = Ptr DuckDBV2CustomType

{- | Opaque target of @duckdb_v2_file_system_handle@.

Do not read or write the target storage.
-}
data DuckDBV2FileSystem

{- | A borrowed opaque handle to a file system. Obtained from @duckdb_v2_file_system_get_from_context()@ or
@duckdb_v2_file_system_get_from_connection()@, and used to open files with @duckdb_v2_file_system_open()@. Borrowed:
the handle belongs to the context or connection it came from, is valid only for as long as that is, and must not be
destroyed.
-}
type DuckDBV2FileSystemHandle = Ptr DuckDBV2FileSystem

{- | Opaque target of @duckdb_v2_file_open_options_handle@.

Do not read or write the target storage.
-}
data DuckDBV2FileOpenOptions

{- | An owned opaque handle to the options a file is opened with. Created with @duckdb_v2_file_open_options_create()@,
configured with @duckdb_v2_file_open_options_set_flag()@ and @duckdb_v2_file_open_options_set_value()@, passed to
@duckdb_v2_file_system_open()@, and destroyed with @duckdb_v2_file_open_options_destroy()@. One options object can
open any number of files, and destroying it does not affect files already opened with it.
-}
type DuckDBV2FileOpenOptionsHandle = Ptr DuckDBV2FileOpenOptions

{- | Opaque target of @duckdb_v2_file_handle@.

Do not read or write the target storage.
-}
data DuckDBV2File

{- | An owned opaque handle to an open file, produced by @duckdb_v2_file_system_open()@ and destroyed with
@duckdb_v2_file_destroy()@. Read, write, seek and sync through the @file_*@ functions. Only usable while the file
system it was opened through is still valid.
-}
type DuckDBV2FileHandle = Ptr DuckDBV2File

{- | Opaque target of @duckdb_v2_function_bind_info_handle@.

Do not read or write the target storage.
-}
data DuckDBV2FunctionBindInfo

{- | A borrowed opaque handle to the call site a bind callback is binding. Every function family's bind callback receives
this handle next to its own bind info. It gives access to the arguments of the call, the function's user data, and
the bind data.

The arguments form one list in four parts, in this order: one argument per positional-only and standard parameter, in
declaration order; the arguments @*args@ received, in call order; one argument per named-only parameter, in
declaration order; and the arguments @**kwargs@ received, in call order. How the caller passed an argument does not
matter: a standard parameter passed by name is at its declared position. A parameter the call omitted is present with
its default value. See @duckdb_v2_function_bind_get_arg_count()@ for the size of each part.
-}
type DuckDBV2FunctionBindInfoHandle = Ptr DuckDBV2FunctionBindInfo

{- | Opaque target of @duckdb_v2_function_signature_handle@.

Do not read or write the target storage.
-}
data DuckDBV2FunctionSignature

-- | An opaque handle to a function signature. Carries the function's parameters and return type.
type DuckDBV2FunctionSignatureHandle = Ptr DuckDBV2FunctionSignature

{- | Opaque target of @duckdb_v2_attach_options_handle@.

Do not read or write the target storage.
-}
data DuckDBV2AttachOptions

{- | An opaque, owned handle to the @(KEY value)@ options of one @duckdb_v2_instance_attach()@ call, as SQL @ATTACH@
accepts them. Created from the instance handle it will be used with via @duckdb_v2_attach_options_create()@; destroy
it via @duckdb_v2_attach_options_destroy()@.
-}
type DuckDBV2AttachOptionsHandle = Ptr DuckDBV2AttachOptions

{- | Opaque target of @duckdb_v2_qname_handle@.

Do not read or write the target storage.
-}
data DuckDBV2Qname

{- | An owned qualified name: an ordered path of one to three non-empty identifier parts whose last element is the object
name. Construct with @duckdb_v2_qname_parse()@ or @duckdb_v2_qname_create()@, read with
@duckdb_v2_qname_get_part_count()@ and @duckdb_v2_qname_get_part()@, render back to SQL with
@duckdb_v2_qname_render()@, compare with @duckdb_v2_qname_equals()@, and destroy with @duckdb_v2_qname_destroy()@.
Two handles are compared with @duckdb_v2_qname_equals()@, never by pointer.
-}
type DuckDBV2QnameHandle = Ptr DuckDBV2Qname

{- | Opaque target of @duckdb_v2_schema_handle@.

Do not read or write the target storage.
-}
data DuckDBV2Schema

{- | An owned, ordered list of (name, type) fields. Read with schema_get_count and schema_get_field. Destroy via
schema_destroy.
-}
type DuckDBV2SchemaHandle = Ptr DuckDBV2Schema

{- | Opaque target of @duckdb_v2_token_iterator_handle@.

Do not read or write the target storage.
-}
data DuckDBV2TokenIterator

-- | An opaque, owned handle to an iterator over the tokens of a SQL string, produced by tokenize_sql.
type DuckDBV2TokenIteratorHandle = Ptr DuckDBV2TokenIterator

{- | Opaque target of @duckdb_v2_aggregate_function_handle@.

Do not read or write the target storage.
-}
data DuckDBV2AggregateFunction

{- | An owned opaque handle to a custom aggregate function being built. Created with
@duckdb_v2_aggregate_function_create_with_connection()@ or @duckdb_v2_aggregate_function_create_with_extension()@,
configured with the setter functions (e.g. @duckdb_v2_aggregate_function_set_name()@,
@duckdb_v2_aggregate_function_set_update_callback()@, etc.) and the signature obtained via
@duckdb_v2_aggregate_function_get_signature()@, made available with @duckdb_v2_aggregate_function_register()@, and
destroyed with @duckdb_v2_aggregate_function_destroy()@.
-}
type DuckDBV2AggregateFunctionHandle = Ptr DuckDBV2AggregateFunction

{- | Opaque target of @duckdb_v2_aggregate_function_bind_info_handle@.

Do not read or write the target storage.
-}
data DuckDBV2AggregateFunctionBindInfo

{- | A borrowed opaque handle to the result of an aggregate function's "bind" phase. The "bind" callback receives this
handle next to a @duckdb_v2_function_bind_info_handle@, which gives access to the arguments, the user data and the
bind data, and can use it to set the return type of the call site being bound.
-}
type DuckDBV2AggregateFunctionBindInfoHandle = Ptr DuckDBV2AggregateFunctionBindInfo

{- | Opaque target of @duckdb_v2_aggregate_function_size_info_handle@.

Do not read or write the target storage.
-}
data DuckDBV2AggregateFunctionSizeInfo

{- | A borrowed opaque handle to the arguments supplied to an aggregate function during the state sizing "size" phase. The
"size" callback receives this handle and must use it to report the size of a single aggregate state in bytes.
-}
type DuckDBV2AggregateFunctionSizeInfoHandle = Ptr DuckDBV2AggregateFunctionSizeInfo

{- | Opaque target of @duckdb_v2_aggregate_function_init_info_handle@.

Do not read or write the target storage.
-}
data DuckDBV2AggregateFunctionInitInfo

{- | A borrowed opaque handle to the arguments supplied to an aggregate function during the state initialization "init"
phase. The "init" callback receives this handle and can use it to access the array of aggregate states it must
initialize in place.
-}
type DuckDBV2AggregateFunctionInitInfoHandle = Ptr DuckDBV2AggregateFunctionInitInfo

{- | Opaque target of @duckdb_v2_aggregate_function_update_info_handle@.

Do not read or write the target storage.
-}
data DuckDBV2AggregateFunctionUpdateInfo

{- | A borrowed opaque handle to the arguments supplied to an aggregate function during the "update" phase. The "update"
callback receives this handle and can use it to access the input argument vectors, the number of rows to process, and
the aggregate state each row must be aggregated into.
-}
type DuckDBV2AggregateFunctionUpdateInfoHandle = Ptr DuckDBV2AggregateFunctionUpdateInfo

{- | Opaque target of @duckdb_v2_aggregate_function_combine_info_handle@.

Do not read or write the target storage.
-}
data DuckDBV2AggregateFunctionCombineInfo

{- | A borrowed opaque handle to the arguments supplied to an aggregate function during the "combine" phase. The "combine"
callback receives this handle and can use it to access the source and target aggregate state arrays to merge.
-}
type DuckDBV2AggregateFunctionCombineInfoHandle = Ptr DuckDBV2AggregateFunctionCombineInfo

{- | Opaque target of @duckdb_v2_aggregate_function_finalize_info_handle@.

Do not read or write the target storage.
-}
data DuckDBV2AggregateFunctionFinalizeInfo

{- | A borrowed opaque handle to the arguments supplied to an aggregate function during the "finalize" phase. The
"finalize" callback receives this handle and can use it to access the aggregate states and the result vector and
offset to write their final values to.
-}
type DuckDBV2AggregateFunctionFinalizeInfoHandle = Ptr DuckDBV2AggregateFunctionFinalizeInfo

{- | Opaque target of @duckdb_v2_aggregate_function_destroy_info_handle@.

Do not read or write the target storage.
-}
data DuckDBV2AggregateFunctionDestroyInfo

{- | A borrowed opaque handle to the arguments supplied to an aggregate function during the state destruction "destroy"
phase. The "destroy" callback receives this handle and can use it to access the array of aggregate states whose
resources must be released.
-}
type DuckDBV2AggregateFunctionDestroyInfoHandle = Ptr DuckDBV2AggregateFunctionDestroyInfo

{- | Opaque target of @duckdb_v2_table_description_handle@.

Do not read or write the target storage.
-}
data DuckDBV2TableDescription

{- | An owned snapshot of one base table taken at creation: where the name resolved, the table's columns, and per-column
catalog facts. Later DDL does not update it. Create with @duckdb_v2_connection_describe_table()@, read the resolved
location via @duckdb_v2_table_description_get_qname()@, and the columns via
@duckdb_v2_table_description_get_column_count()@ and @duckdb_v2_table_description_get_column()@. Destroy via
@duckdb_v2_table_description_destroy()@.
-}
type DuckDBV2TableDescriptionHandle = Ptr DuckDBV2TableDescription

{- | Opaque target of @duckdb_v2_column_description_handle@.

Do not read or write the target storage.
-}
data DuckDBV2ColumnDescription

{- | An owned snapshot of one column of a described table: its name, type, and catalog facts. Obtain with
@duckdb_v2_table_description_get_column()@, read with @duckdb_v2_column_description_get_name()@,
@duckdb_v2_column_description_get_type()@, @duckdb_v2_column_description_has_default()@ and
@duckdb_v2_column_description_has_generated()@, and destroy with @duckdb_v2_column_description_destroy()@.
-}
type DuckDBV2ColumnDescriptionHandle = Ptr DuckDBV2ColumnDescription

{- | Opaque target of @duckdb_v2_copy_function_handle@.

Do not read or write the target storage.
-}
data DuckDBV2CopyFunction

{- | An owned opaque handle to a custom copy function being built: an output format for @COPY ... TO@, an input format for
@COPY ... FROM@, or both. Created with @duckdb_v2_copy_function_create_with_connection()@ or
@duckdb_v2_copy_function_create_with_extension()@, configured with the setter functions (e.g.
@duckdb_v2_copy_function_set_name()@, @duckdb_v2_copy_to_set_batch_callback()@,
@duckdb_v2_copy_from_set_exec_callback()@, etc.), made available with @duckdb_v2_copy_function_register()@, and
destroyed with @duckdb_v2_copy_function_destroy()@.
-}
type DuckDBV2CopyFunctionHandle = Ptr DuckDBV2CopyFunction

{- | Opaque target of @duckdb_v2_copy_to_bind_info_handle@.

Do not read or write the target storage.
-}
data DuckDBV2CopyToBindInfo

{- | A borrowed opaque handle to the arguments supplied to a copy function during the query preparation "bind" phase of a
@COPY ... TO@ statement. The "bind" callback receives this handle and can use it to e.g. inspect the names and types
of the columns being written, read the statement's options and initialize some constant state.
-}
type DuckDBV2CopyToBindInfoHandle = Ptr DuckDBV2CopyToBindInfo

{- | Opaque target of @duckdb_v2_copy_to_batch_size_info_handle@.

Do not read or write the target storage.
-}
data DuckDBV2CopyToBatchSizeInfo

{- | A borrowed opaque handle to the arguments supplied to a copy function during the "batch size" phase of a @COPY ...
TO@ statement. The "batch size" callback receives this handle and must use it to report how many rows a batch should
carry.
-}
type DuckDBV2CopyToBatchSizeInfoHandle = Ptr DuckDBV2CopyToBatchSizeInfo

{- | Opaque target of @duckdb_v2_copy_to_init_info_handle@.

Do not read or write the target storage.
-}
data DuckDBV2CopyToInitInfo

{- | A borrowed opaque handle to the arguments supplied to a copy function during the per-file "init" phase of a @COPY ...
TO@ statement. The "init" callback receives this handle and can use it to e.g. read the path of the file being
written and set up the state shared by every batch written to that file.
-}
type DuckDBV2CopyToInitInfoHandle = Ptr DuckDBV2CopyToInitInfo

{- | Opaque target of @duckdb_v2_copy_to_batch_info_handle@.

Do not read or write the target storage.
-}
data DuckDBV2CopyToBatchInfo

{- | A borrowed opaque handle to the arguments supplied to a copy function during the "batch" phase of a @COPY ... TO@
statement. The "batch" callback receives this handle and can use it to take ownership of the rows of the batch and to
set the prepared form of the batch handed to the "flush" callback.
-}
type DuckDBV2CopyToBatchInfoHandle = Ptr DuckDBV2CopyToBatchInfo

{- | Opaque target of @duckdb_v2_copy_to_flush_info_handle@.

Do not read or write the target storage.
-}
data DuckDBV2CopyToFlushInfo

{- | A borrowed opaque handle to the arguments supplied to a copy function during the "flush" phase of a @COPY ... TO@
statement. The "flush" callback receives this handle and can use it to access the prepared batch and write it to the
output.
-}
type DuckDBV2CopyToFlushInfoHandle = Ptr DuckDBV2CopyToFlushInfo

{- | Opaque target of @duckdb_v2_copy_to_finalize_info_handle@.

Do not read or write the target storage.
-}
data DuckDBV2CopyToFinalizeInfo

{- | A borrowed opaque handle to the arguments supplied to a copy function during the per-file "finalize" phase of a @COPY
... TO@ statement. The "finalize" callback receives this handle and can use it to e.g. close the file after every
batch has been flushed to it.
-}
type DuckDBV2CopyToFinalizeInfoHandle = Ptr DuckDBV2CopyToFinalizeInfo

{- | Opaque target of @duckdb_v2_copy_to_statistics_info_handle@.

Do not read or write the target storage.
-}
data DuckDBV2CopyToStatisticsInfo

{- | A borrowed opaque handle to the arguments supplied to a copy function when a @COPY ... TO@ statement reports
statistics about the files it wrote (e.g. with @RETURN_STATS@). The "statistics" callback receives this handle and
uses it to report the statistics of one written file.
-}
type DuckDBV2CopyToStatisticsInfoHandle = Ptr DuckDBV2CopyToStatisticsInfo

{- | Opaque target of @duckdb_v2_copy_from_bind_info_handle@.

Do not read or write the target storage.
-}
data DuckDBV2CopyFromBindInfo

{- | A borrowed opaque handle to the arguments supplied to a copy function during the query preparation "bind" phase of a
@COPY ... FROM@ statement. The "bind" callback receives this handle and can use it to e.g. read the path of the file
to read, inspect the names and types of the columns the target table expects, read the statement's options, hint at
the number of rows the read will produce and initialize some constant state.
-}
type DuckDBV2CopyFromBindInfoHandle = Ptr DuckDBV2CopyFromBindInfo

{- | Opaque target of @duckdb_v2_copy_from_init_global_info_handle@.

Do not read or write the target storage.
-}
data DuckDBV2CopyFromInitGlobalInfo

{- | A borrowed opaque handle to the arguments supplied to a copy function during the global state initialization "init
global" phase of a @COPY ... FROM@ statement. The "init global" callback receives this handle and can use it to e.g.
set up the state shared by every thread reading the file, and to declare how many threads may read it.
-}
type DuckDBV2CopyFromInitGlobalInfoHandle = Ptr DuckDBV2CopyFromInitGlobalInfo

{- | Opaque target of @duckdb_v2_copy_from_init_local_info_handle@.

Do not read or write the target storage.
-}
data DuckDBV2CopyFromInitLocalInfo

{- | A borrowed opaque handle to the arguments supplied to a copy function during the local state initialization "init
local" phase of a @COPY ... FROM@ statement. The "init local" callback receives this handle and can use it to e.g.
set up worker-local state, typically by claiming work from the shared global state.
-}
type DuckDBV2CopyFromInitLocalInfoHandle = Ptr DuckDBV2CopyFromInitLocalInfo

{- | Opaque target of @duckdb_v2_copy_from_exec_info_handle@.

Do not read or write the target storage.
-}
data DuckDBV2CopyFromExecInfo

{- | A borrowed opaque handle to the arguments supplied to a copy function during the execution "exec" phase of a @COPY
... FROM@ statement. The "exec" callback receives this handle and can use it to e.g. access the global and local
state and write the next batch of rows to the output chunk.
-}
type DuckDBV2CopyFromExecInfoHandle = Ptr DuckDBV2CopyFromExecInfo

{- | Opaque target of @duckdb_v2_copy_from_progress_info_handle@.

Do not read or write the target storage.
-}
data DuckDBV2CopyFromProgressInfo

{- | A borrowed opaque handle to the arguments supplied to a copy function during the "progress" phase of a @COPY ...
FROM@ statement. The "progress" callback receives this handle and can use it to report how far the read has advanced.
-}
type DuckDBV2CopyFromProgressInfoHandle = Ptr DuckDBV2CopyFromProgressInfo

{- | Opaque target of @duckdb_v2_expression_handle@.

Do not read or write the target storage.
-}
data DuckDBV2Expression

{- | A borrowed opaque handle to a node of a bound expression tree. Read-only and owned by the engine: valid only for the
duration of the callback that handed it out, and never destroyed by the caller. Child nodes borrowed via
@duckdb_v2_expression_get_child()@ share their parent's lifetime.
-}
type DuckDBV2ExpressionHandle = Ptr DuckDBV2Expression

{- | Opaque target of @duckdb_v2_replacement_scan_handle@.

Do not read or write the target storage.
-}
data DuckDBV2ReplacementScan

{- | An owned opaque handle to a replacement scan being built. Created with
@duckdb_v2_replacement_scan_create_with_connection()@, @duckdb_v2_replacement_scan_create_with_instance()@ or
@duckdb_v2_replacement_scan_create_with_extension()@, configured with @duckdb_v2_replacement_scan_set_callback()@ and
@duckdb_v2_replacement_scan_set_user_data()@, made available with @duckdb_v2_replacement_scan_register()@, and
destroyed with @duckdb_v2_replacement_scan_destroy()@.
-}
type DuckDBV2ReplacementScanHandle = Ptr DuckDBV2ReplacementScan

{- | Opaque target of @duckdb_v2_replacement_scan_info_handle@.

Do not read or write the target storage.
-}
data DuckDBV2ReplacementScanInfo

{- | A borrowed opaque handle to the arguments supplied to a replacement scan when the binder consults it. The callback
receives this handle and can use it to inspect the unresolved name and to claim it by naming what to read instead.
Valid only for the duration of the callback; the name it hands out is owned separately and outlives it.
-}
type DuckDBV2ReplacementScanInfoHandle = Ptr DuckDBV2ReplacementScanInfo

{- | Opaque target of @duckdb_v2_scalar_function_handle@.

Do not read or write the target storage.
-}
data DuckDBV2ScalarFunction

{- | An owned opaque handle to a custom scalar function being built. Created with
@duckdb_v2_scalar_function_create_with_connection()@ or @duckdb_v2_scalar_function_create_with_extension()@,
configured with the setter functions (e.g. @duckdb_v2_scalar_function_set_name()@,
@duckdb_v2_scalar_function_set_exec_callback()@, etc.) and the signature obtained via
@duckdb_v2_scalar_function_get_signature()@, made available with @duckdb_v2_scalar_function_register()@, and
destroyed with @duckdb_v2_scalar_function_destroy()@.
-}
type DuckDBV2ScalarFunctionHandle = Ptr DuckDBV2ScalarFunction

{- | Opaque target of @duckdb_v2_scalar_function_bind_info_handle@.

Do not read or write the target storage.
-}
data DuckDBV2ScalarFunctionBindInfo

{- | A borrowed opaque handle to the result of a scalar function's "bind" phase. The "bind" callback receives this handle
next to a @duckdb_v2_function_bind_info_handle@, which gives access to the arguments, the user data and the bind
data, and can use it to set the return type of the call site being bound.
-}
type DuckDBV2ScalarFunctionBindInfoHandle = Ptr DuckDBV2ScalarFunctionBindInfo

{- | Opaque target of @duckdb_v2_scalar_function_init_info_handle@.

Do not read or write the target storage.
-}
data DuckDBV2ScalarFunctionInitInfo

{- | A borrowed opaque handle to the arguments supplied to a scalar function during the local state initialization "init"
phase. The "init" callback receives this handle and can use it to e.g. set up local state reusable across invocations
of the "exec" execution callback.
-}
type DuckDBV2ScalarFunctionInitInfoHandle = Ptr DuckDBV2ScalarFunctionInitInfo

{- | Opaque target of @duckdb_v2_scalar_function_exec_info_handle@.

Do not read or write the target storage.
-}
data DuckDBV2ScalarFunctionExecInfo

{- | A borrowed opaque handle to the arguments supplied to a scalar function during the execution "exec" phase. The "exec"
callback receives this handle and can use it to e.g. access the input argument vectors, retrieve the number of rows
to process, and write results to the output vector.
-}
type DuckDBV2ScalarFunctionExecInfoHandle = Ptr DuckDBV2ScalarFunctionExecInfo

{- | Opaque target of @duckdb_v2_sql_statement_handle@.

Do not read or write the target storage.
-}
data DuckDBV2SqlStatement

{- | An opaque, owned handle to a single parsed SQL statement, produced by statement_iterator_next. statement_execute runs
it without consuming it; the caller always destroys it via sql_statement_destroy.
-}
type DuckDBV2SqlStatementHandle = Ptr DuckDBV2SqlStatement

{- | Opaque target of @duckdb_v2_statement_iterator_handle@.

Do not read or write the target storage.
-}
data DuckDBV2StatementIterator

{- | An opaque, owned handle to an iterator over the statements of a SQL string, produced by parse_sql. Destroy it via
statement_iterator_destroy; statements it already yielded are independently owned and unaffected.
-}
type DuckDBV2StatementIteratorHandle = Ptr DuckDBV2StatementIterator

{- | Opaque target of @duckdb_v2_prepared_statement_handle@.

Do not read or write the target storage.
-}
data DuckDBV2PreparedStatement

{- | An owned handle to a statement bound and planned once, executable repeatedly via
@duckdb_v2_prepared_statement_execute()@. Construct with @duckdb_v2_prepared_statement_create()@ and destroy with
@duckdb_v2_prepared_statement_destroy()@. It keeps its connection's session alive, so it stays usable across
executions and even after the connection is disconnected.
-}
type DuckDBV2PreparedStatementHandle = Ptr DuckDBV2PreparedStatement

{- | Opaque target of @duckdb_v2_table_function_handle@.

Do not read or write the target storage.
-}
data DuckDBV2TableFunction

{- | An owned opaque handle to a custom table function being built. Created with
@duckdb_v2_table_function_create_with_connection()@ or @duckdb_v2_table_function_create_with_extension()@, configured
with the setter functions (e.g. @duckdb_v2_table_function_set_name()@,
@duckdb_v2_table_function_set_exec_callback()@, etc.) and the signature obtained via
@duckdb_v2_table_function_get_signature()@, made available with @duckdb_v2_table_function_register()@, and destroyed
with @duckdb_v2_table_function_destroy()@.
-}
type DuckDBV2TableFunctionHandle = Ptr DuckDBV2TableFunction

{- | Opaque target of @duckdb_v2_table_function_bind_info_handle@.

Do not read or write the target storage.
-}
data DuckDBV2TableFunctionBindInfo

{- | A borrowed opaque handle to the result of a table function's "bind" phase. The "bind" callback receives this handle
next to a @duckdb_v2_function_bind_info_handle@, which gives access to the arguments, the user data and the bind
data. It must use this handle to declare the columns the function returns, and can use it to hint at the number of
rows the scan will produce.
-}
type DuckDBV2TableFunctionBindInfoHandle = Ptr DuckDBV2TableFunctionBindInfo

{- | Opaque target of @duckdb_v2_table_function_init_global_info_handle@.

Do not read or write the target storage.
-}
data DuckDBV2TableFunctionInitGlobalInfo

{- | A borrowed opaque handle to the arguments supplied to a table function during the global state initialization "init
global" phase. The "init global" callback receives this handle and can use it to e.g. set up the state shared by
every thread scanning the function, and to declare how many threads may scan it.
-}
type DuckDBV2TableFunctionInitGlobalInfoHandle = Ptr DuckDBV2TableFunctionInitGlobalInfo

{- | Opaque target of @duckdb_v2_table_function_init_local_info_handle@.

Do not read or write the target storage.
-}
data DuckDBV2TableFunctionInitLocalInfo

{- | A borrowed opaque handle to the arguments supplied to a table function during the local state initialization "init
local" phase. The "init local" callback receives this handle and can use it to e.g. set up worker-local state,
typically by claiming work from the shared global state.
-}
type DuckDBV2TableFunctionInitLocalInfoHandle = Ptr DuckDBV2TableFunctionInitLocalInfo

{- | Opaque target of @duckdb_v2_table_function_exec_info_handle@.

Do not read or write the target storage.
-}
data DuckDBV2TableFunctionExecInfo

{- | A borrowed opaque handle to the arguments supplied to a table function during the execution "exec" phase. The "exec"
callback receives this handle and can use it to e.g. access the global and local state and write the next batch of
rows to the output chunk.
-}
type DuckDBV2TableFunctionExecInfoHandle = Ptr DuckDBV2TableFunctionExecInfo

{- | Opaque target of @duckdb_v2_table_function_progress_info_handle@.

Do not read or write the target storage.
-}
data DuckDBV2TableFunctionProgressInfo

{- | A borrowed opaque handle to the arguments supplied to a table function during the "progress" phase. The "progress"
callback receives this handle and can use it to report how far the scan has advanced.
-}
type DuckDBV2TableFunctionProgressInfoHandle = Ptr DuckDBV2TableFunctionProgressInfo

{- | Opaque target of @duckdb_v2_table_function_filter_pushdown_info_handle@.

Do not read or write the target storage.
-}
data DuckDBV2TableFunctionFilterPushdownInfo

{- | A borrowed opaque handle to the arguments supplied to a table function during the "filter pushdown" phase of query
optimization. The "filter pushdown" callback receives this handle and can use it to inspect the filter predicates the
query applies to the function's rows, and to accept the ones it will apply itself.
-}
type DuckDBV2TableFunctionFilterPushdownInfoHandle = Ptr DuckDBV2TableFunctionFilterPushdownInfo

{- | Opaque target of @duckdb_v2_table_function_partition_data_info_handle@.

Do not read or write the target storage.
-}
data DuckDBV2TableFunctionPartitionDataInfo

{- | A borrowed opaque handle to the arguments supplied to a table function during the "partition data" phase. The engine
invokes the callback on the worker thread that just produced a batch of rows, once per batch, only when a downstream
operator needs to know the batch's ordering position, the values of a set of partitioning columns, or both; the
handle reports which of those were actually requested and receives the answer.
-}
type DuckDBV2TableFunctionPartitionDataInfoHandle = Ptr DuckDBV2TableFunctionPartitionDataInfo

{- | Opaque target of @duckdb_v2_table_function_partitioning_info_handle@.

Do not read or write the target storage.
-}
data DuckDBV2TableFunctionPartitioningInfo

{- | A borrowed opaque handle to the arguments supplied to a table function during the "partitioning" phase of query
optimization. The engine invokes the callback once per candidate @GROUP BY@ column set, before execution starts, to
decide whether the scan can feed a partitioned aggregate directly instead of hashing.
-}
type DuckDBV2TableFunctionPartitioningInfoHandle = Ptr DuckDBV2TableFunctionPartitioningInfo

{- | Opaque target of @duckdb_v2_table_function_claim_batch_info_handle@.

Do not read or write the target storage.
-}
data DuckDBV2TableFunctionClaimBatchInfo

{- | A borrowed opaque handle to the arguments supplied to a table function when a scanning thread claims its next batch
of work. The "claim batch" callback receives this handle and can use it to claim the next part of the scan from the
shared global state for the thread's local state, and to report whether it claimed one.
-}
type DuckDBV2TableFunctionClaimBatchInfoHandle = Ptr DuckDBV2TableFunctionClaimBatchInfo

{- | Opaque target of @duckdb_v2_multi_file_function_handle@.

Do not read or write the target storage.
-}
data DuckDBV2MultiFileFunction

{- | An owned opaque handle to a multi-file table function being built. A multi-file function wraps an already registered
table function that reads a single file, and adds everything that is needed to read many files at once on top of it:
globbing, lists of files, hive partitioning, the @filename@ column, @union_by_name@ and the like. Created with
@duckdb_v2_multi_file_function_create_with_connection()@ or @duckdb_v2_multi_file_function_create_with_extension()@,
configured with the setter functions (e.g. @duckdb_v2_multi_file_function_set_name()@,
@duckdb_v2_multi_file_function_set_single_file_function()@), made available with
@duckdb_v2_multi_file_function_register()@, and destroyed with @duckdb_v2_multi_file_function_destroy()@.
-}
type DuckDBV2MultiFileFunctionHandle = Ptr DuckDBV2MultiFileFunction

{- | Opaque target of @duckdb_v2_arrow_importer_handle@.

Do not read or write the target storage.
-}
data DuckDBV2ArrowImporter

{- | Converts Arrow arrays into DuckDB data chunks, against one resolved ArrowSchema. Create it with
@duckdb_v2_arrow_importer_create()@, which resolves every column's DuckDB type once, so the same importer serves any
number of arrays of that shape. @duckdb_v2_arrow_importer_get_schema()@ reports the resolved DuckDB schema.
@duckdb_v2_arrow_importer_append()@ takes an array, @duckdb_v2_arrow_importer_next_chunk()@ produces chunks from it,
and @duckdb_v2_arrow_importer_destroy()@ frees the importer.

The importer borrows the context it was created with and must not outlive it. One array is in flight at a time, and
an importer must not be used from two threads at once.
-}
type DuckDBV2ArrowImporterHandle = Ptr DuckDBV2ArrowImporter

{- | Opaque target of @duckdb_v2_arrow_exporter_handle@.

Do not read or write the target storage.
-}
data DuckDBV2ArrowExporter

{- | Converts DuckDB data chunks into Arrow arrays, for one fixed list of columns. Create it with
@duckdb_v2_arrow_exporter_create()@, which captures the session's Arrow settings and resolves the extension types
once. @duckdb_v2_arrow_exporter_get_schema()@ reports the Arrow schema. @duckdb_v2_arrow_exporter_append()@ takes a
chunk, @duckdb_v2_arrow_exporter_next_array()@ produces arrays from it, and @duckdb_v2_arrow_exporter_destroy()@
frees the exporter.

An exporter must not be used from two threads at once.
-}
type DuckDBV2ArrowExporterHandle = Ptr DuckDBV2ArrowExporter

{- | Opaque target of @duckdb_v2_arrow_result_handle@.

Do not read or write the target storage.
-}
data DuckDBV2ArrowResult

{- | A query result that produces Arrow arrays. Works like a result, which produces data chunks. Create it with
@duckdb_v2_statement_execute_arrow()@ or @duckdb_v2_prepared_statement_execute_arrow()@.

- Use it from one thread at a time.
- @duckdb_v2_connection_interrupt()@ cancels it: @duckdb_v2_arrow_result_step()@ reports @CANCELLED@, and
@duckdb_v2_arrow_result_fetch_array()@ and a stream's @get_next@ fail.
- Arrays and schemas it hands out belong to the caller and stay valid after the result is destroyed.
-}
type DuckDBV2ArrowResultHandle = Ptr DuckDBV2ArrowResult

{- | Opaque target of @duckdb_v2_extension_handle@.

Do not read or write the target storage.
-}
data DuckDBV2Extension

-- | The @duckdb_v2_extension_handle@ handle.
type DuckDBV2ExtensionHandle = Ptr DuckDBV2Extension

{- | An entry in a selection-vector.

A selection-vector is a "dictionary" represented as an array of indices (sel_t's) mapping "logical" row indices to
the "physical" offsets in a vectors primary data buffer. Used by "dictionary vectors" to represent a filtered or
"sparse" view of the vector's data.
-}
type DuckDBV2SelT = DuckDBSel

-- | VARCHAR storage. The bytes must contain valid UTF-8. Read the transparent bytes fields directly.
type DuckDBV2VarcharT = DuckDBV2Bytes

-- | BLOB storage. Read the transparent bytes fields directly.
type DuckDBV2BlobT = DuckDBV2Bytes

-- | BIT storage. Byte 0 is the padding-bit count, bytes 1.. the data.
type DuckDBV2BitT = DuckDBV2Bytes

-- | BIGNUM storage. Decode via @duckdb_v2_bignum_decode()@.
type DuckDBV2BignumT = DuckDBV2Bytes

{- | A borrowed name view with the same layout as str, marking a string DuckDB treats as a SQL identifier: one matched
case-insensitively. Compare two identifiers case-insensitively rather than byte for byte, and render one into SQL
through the identifier-quoting entry point rather than embedding it raw. The catalog preserves casing; some
registries (config settings) canonicalize to lowercase.

An identifier passed into the API must be valid UTF-8; otherwise the call fails with @ERROR_INPUT_INVALID@.
-}
type DuckDBV2IdentifierT = DuckDBV2Str

{- | The order guarantee a producer of rows makes about its output, which decides whether the rows it produces must be
kept in order downstream.
-}
newtype DuckDBV2OrderPreservation = DuckDBV2OrderPreservation (#{type DUCKDB_V2_ORDER_PRESERVATION})
    deriving (Eq, Ord, Show, Read, Storable)

{- | The rows have no meaningful order. The engine may reorder them freely, and consumers that otherwise keep
insertion order, such as query results, INSERT and COPY, run in parallel without ordering them.
-}
pattern DuckDBV2OrderPreservationNoOrder :: DuckDBV2OrderPreservation
pattern DuckDBV2OrderPreservationNoOrder = DuckDBV2OrderPreservation (#{const DUCKDB_V2_ORDER_PRESERVATION_NO_ORDER})

{- | The rows are produced in insertion order, which the engine keeps unless the @preserve_insertion_order@ setting is
disabled.
-}
pattern DuckDBV2OrderPreservationInsertionOrder :: DuckDBV2OrderPreservation
pattern DuckDBV2OrderPreservationInsertionOrder = DuckDBV2OrderPreservation (#{const DUCKDB_V2_ORDER_PRESERVATION_INSERTION_ORDER})

{- | The rows are produced in an order that must be kept, as if sorted by an @ORDER BY@, even when the
@preserve_insertion_order@ setting is disabled.
-}
pattern DuckDBV2OrderPreservationFixedOrder :: DuckDBV2OrderPreservation
pattern DuckDBV2OrderPreservationFixedOrder = DuckDBV2OrderPreservation (#{const DUCKDB_V2_ORDER_PRESERVATION_FIXED_ORDER})

-- | The @DUCKDB_V2_ORDER_PRESERVATION_MAX_ENUM@ constant.
pattern DuckDBV2OrderPreservationMaxEnum :: DuckDBV2OrderPreservation
pattern DuckDBV2OrderPreservationMaxEnum = DuckDBV2OrderPreservation (#{const DUCKDB_V2_ORDER_PRESERVATION_MAX_ENUM})

-- | Error codes for API calls.
newtype DuckDBV2Error = DuckDBV2Error (#{type DUCKDB_V2_ERROR})
    deriving (Eq, Ord, Show, Read, Storable)

-- | Success.
pattern DuckDBV2ErrorNone :: DuckDBV2Error
pattern DuckDBV2ErrorNone = DuckDBV2Error (#{const DUCKDB_V2_ERROR_NONE})

-- | Generic API error.
pattern DuckDBV2ErrorApi :: DuckDBV2Error
pattern DuckDBV2ErrorApi = DuckDBV2Error (#{const DUCKDB_V2_ERROR_API})

-- | The specified file could not be found.
pattern DuckDBV2ErrorIoFileNotFound :: DuckDBV2Error
pattern DuckDBV2ErrorIoFileNotFound = DuckDBV2Error (#{const DUCKDB_V2_ERROR_IO_FILE_NOT_FOUND})

-- | Failed to read from the storage device.
pattern DuckDBV2ErrorIoReadFailure :: DuckDBV2Error
pattern DuckDBV2ErrorIoReadFailure = DuckDBV2Error (#{const DUCKDB_V2_ERROR_IO_READ_FAILURE})

-- | Unexpected end of file reached.
pattern DuckDBV2ErrorIoEof :: DuckDBV2Error
pattern DuckDBV2ErrorIoEof = DuckDBV2Error (#{const DUCKDB_V2_ERROR_IO_EOF})

-- | A generic I/O error occurred while reading or writing data.
pattern DuckDBV2ErrorIoGeneral :: DuckDBV2Error
pattern DuckDBV2ErrorIoGeneral = DuckDBV2Error (#{const DUCKDB_V2_ERROR_IO_GENERAL})

-- | A network-level failure occurred during an I/O operation.
pattern DuckDBV2ErrorIoNetwork :: DuckDBV2Error
pattern DuckDBV2ErrorIoNetwork = DuckDBV2Error (#{const DUCKDB_V2_ERROR_IO_NETWORK})

-- | An HTTP request issued by the database failed.
pattern DuckDBV2ErrorIoHttp :: DuckDBV2Error
pattern DuckDBV2ErrorIoHttp = DuckDBV2Error (#{const DUCKDB_V2_ERROR_IO_HTTP})

-- | General invalid input error.
pattern DuckDBV2ErrorInputInvalid :: DuckDBV2Error
pattern DuckDBV2ErrorInputInvalid = DuckDBV2Error (#{const DUCKDB_V2_ERROR_INPUT_INVALID})

-- | A specific function parameter is malformed.
pattern DuckDBV2ErrorInputParameterInvalid :: DuckDBV2Error
pattern DuckDBV2ErrorInputParameterInvalid = DuckDBV2Error (#{const DUCKDB_V2_ERROR_INPUT_PARAMETER_INVALID})

-- | A provided value is outside the acceptable range.
pattern DuckDBV2ErrorInputOutOfRange :: DuckDBV2Error
pattern DuckDBV2ErrorInputOutOfRange = DuckDBV2Error (#{const DUCKDB_V2_ERROR_INPUT_OUT_OF_RANGE})

-- | An object exceeded its maximum permitted size.
pattern DuckDBV2ErrorInputObjectSize :: DuckDBV2Error
pattern DuckDBV2ErrorInputObjectSize = DuckDBV2Error (#{const DUCKDB_V2_ERROR_INPUT_OBJECT_SIZE})

-- | The requested resource is already in use.
pattern DuckDBV2ErrorResourceInUse :: DuckDBV2Error
pattern DuckDBV2ErrorResourceInUse = DuckDBV2Error (#{const DUCKDB_V2_ERROR_RESOURCE_IN_USE})

-- | An allocation failed because the system ran out of memory.
pattern DuckDBV2ErrorResourceOutOfMemory :: DuckDBV2Error
pattern DuckDBV2ErrorResourceOutOfMemory = DuckDBV2Error (#{const DUCKDB_V2_ERROR_RESOURCE_OUT_OF_MEMORY})

-- | A connection-level failure occurred (e.g. the connection has been closed or invalidated).
pattern DuckDBV2ErrorResourceConnection :: DuckDBV2Error
pattern DuckDBV2ErrorResourceConnection = DuckDBV2Error (#{const DUCKDB_V2_ERROR_RESOURCE_CONNECTION})

-- | An operation failed because of an unresolved dependency between catalog objects.
pattern DuckDBV2ErrorResourceDependency :: DuckDBV2Error
pattern DuckDBV2ErrorResourceDependency = DuckDBV2Error (#{const DUCKDB_V2_ERROR_RESOURCE_DEPENDENCY})

-- | An extension required by the operation is not loaded.
pattern DuckDBV2ErrorResourceMissingExtension :: DuckDBV2Error
pattern DuckDBV2ErrorResourceMissingExtension = DuckDBV2Error (#{const DUCKDB_V2_ERROR_RESOURCE_MISSING_EXTENSION})

-- | Autoloading an extension failed.
pattern DuckDBV2ErrorResourceAutoload :: DuckDBV2Error
pattern DuckDBV2ErrorResourceAutoload = DuckDBV2Error (#{const DUCKDB_V2_ERROR_RESOURCE_AUTOLOAD})

-- | A value could not be converted to the requested type.
pattern DuckDBV2ErrorTypeConversion :: DuckDBV2Error
pattern DuckDBV2ErrorTypeConversion = DuckDBV2Error (#{const DUCKDB_V2_ERROR_TYPE_CONVERSION})

-- | An unknown or unsupported type was encountered.
pattern DuckDBV2ErrorTypeUnknown :: DuckDBV2Error
pattern DuckDBV2ErrorTypeUnknown = DuckDBV2Error (#{const DUCKDB_V2_ERROR_TYPE_UNKNOWN})

-- | A type was used in a context where it is not valid.
pattern DuckDBV2ErrorTypeInvalid :: DuckDBV2Error
pattern DuckDBV2ErrorTypeInvalid = DuckDBV2Error (#{const DUCKDB_V2_ERROR_TYPE_INVALID})

-- | Two values or expressions have incompatible types.
pattern DuckDBV2ErrorTypeMismatch :: DuckDBV2Error
pattern DuckDBV2ErrorTypeMismatch = DuckDBV2Error (#{const DUCKDB_V2_ERROR_TYPE_MISMATCH})

-- | A decimal value is out of range or otherwise invalid.
pattern DuckDBV2ErrorTypeDecimal :: DuckDBV2Error
pattern DuckDBV2ErrorTypeDecimal = DuckDBV2Error (#{const DUCKDB_V2_ERROR_TYPE_DECIMAL})

-- | Division by zero was attempted.
pattern DuckDBV2ErrorTypeDivideByZero :: DuckDBV2Error
pattern DuckDBV2ErrorTypeDivideByZero = DuckDBV2Error (#{const DUCKDB_V2_ERROR_TYPE_DIVIDE_BY_ZERO})

-- | The query could not be parsed.
pattern DuckDBV2ErrorQueryParser :: DuckDBV2Error
pattern DuckDBV2ErrorQueryParser = DuckDBV2Error (#{const DUCKDB_V2_ERROR_QUERY_PARSER})

-- | The query contains a syntax error.
pattern DuckDBV2ErrorQuerySyntax :: DuckDBV2Error
pattern DuckDBV2ErrorQuerySyntax = DuckDBV2Error (#{const DUCKDB_V2_ERROR_QUERY_SYNTAX})

-- | Binding the query against the catalog failed (e.g. unknown column or table).
pattern DuckDBV2ErrorQueryBinder :: DuckDBV2Error
pattern DuckDBV2ErrorQueryBinder = DuckDBV2Error (#{const DUCKDB_V2_ERROR_QUERY_BINDER})

-- | The query could not be translated into a logical plan.
pattern DuckDBV2ErrorQueryPlanner :: DuckDBV2Error
pattern DuckDBV2ErrorQueryPlanner = DuckDBV2Error (#{const DUCKDB_V2_ERROR_QUERY_PLANNER})

-- | An error occurred during query optimization.
pattern DuckDBV2ErrorQueryOptimizer :: DuckDBV2Error
pattern DuckDBV2ErrorQueryOptimizer = DuckDBV2Error (#{const DUCKDB_V2_ERROR_QUERY_OPTIMIZER})

-- | An expression in the query is invalid or could not be evaluated.
pattern DuckDBV2ErrorQueryExpression :: DuckDBV2Error
pattern DuckDBV2ErrorQueryExpression = DuckDBV2Error (#{const DUCKDB_V2_ERROR_QUERY_EXPRESSION})

-- | An error occurred while executing the physical plan.
pattern DuckDBV2ErrorQueryExecutor :: DuckDBV2Error
pattern DuckDBV2ErrorQueryExecutor = DuckDBV2Error (#{const DUCKDB_V2_ERROR_QUERY_EXECUTOR})

-- | The task scheduler reported an error while running the query.
pattern DuckDBV2ErrorQueryScheduler :: DuckDBV2Error
pattern DuckDBV2ErrorQueryScheduler = DuckDBV2Error (#{const DUCKDB_V2_ERROR_QUERY_SCHEDULER})

-- | The requested feature or operation is not implemented.
pattern DuckDBV2ErrorQueryNotImplemented :: DuckDBV2Error
pattern DuckDBV2ErrorQueryNotImplemented = DuckDBV2Error (#{const DUCKDB_V2_ERROR_QUERY_NOT_IMPLEMENTED})

-- | A prepared-statement parameter has not been bound to a value.
pattern DuckDBV2ErrorQueryParameterNotResolved :: DuckDBV2Error
pattern DuckDBV2ErrorQueryParameterNotResolved = DuckDBV2Error (#{const DUCKDB_V2_ERROR_QUERY_PARAMETER_NOT_RESOLVED})

-- | A prepared-statement parameter was used in a position where it is not allowed.
pattern DuckDBV2ErrorQueryParameterNotAllowed :: DuckDBV2Error
pattern DuckDBV2ErrorQueryParameterNotAllowed = DuckDBV2Error (#{const DUCKDB_V2_ERROR_QUERY_PARAMETER_NOT_ALLOWED})

-- | A catalog operation failed (e.g. object not found or already exists).
pattern DuckDBV2ErrorDatabaseCatalog :: DuckDBV2Error
pattern DuckDBV2ErrorDatabaseCatalog = DuckDBV2Error (#{const DUCKDB_V2_ERROR_DATABASE_CATALOG})

-- | A transaction-level error occurred (e.g. conflict or aborted transaction).
pattern DuckDBV2ErrorDatabaseTransaction :: DuckDBV2Error
pattern DuckDBV2ErrorDatabaseTransaction = DuckDBV2Error (#{const DUCKDB_V2_ERROR_DATABASE_TRANSACTION})

-- | A constraint (primary key, unique, foreign key, NOT NULL, check) was violated.
pattern DuckDBV2ErrorDatabaseConstraint :: DuckDBV2Error
pattern DuckDBV2ErrorDatabaseConstraint = DuckDBV2Error (#{const DUCKDB_V2_ERROR_DATABASE_CONSTRAINT})

-- | An index operation failed.
pattern DuckDBV2ErrorDatabaseIndex :: DuckDBV2Error
pattern DuckDBV2ErrorDatabaseIndex = DuckDBV2Error (#{const DUCKDB_V2_ERROR_DATABASE_INDEX})

-- | A sequence operation failed (e.g. overflow or invalid usage).
pattern DuckDBV2ErrorDatabaseSequence :: DuckDBV2Error
pattern DuckDBV2ErrorDatabaseSequence = DuckDBV2Error (#{const DUCKDB_V2_ERROR_DATABASE_SEQUENCE})

-- | An error related to catalog statistics occurred.
pattern DuckDBV2ErrorDatabaseStatistics :: DuckDBV2Error
pattern DuckDBV2ErrorDatabaseStatistics = DuckDBV2Error (#{const DUCKDB_V2_ERROR_DATABASE_STATISTICS})

-- | Serializing or deserializing a database object failed.
pattern DuckDBV2ErrorDatabaseSerialization :: DuckDBV2Error
pattern DuckDBV2ErrorDatabaseSerialization = DuckDBV2Error (#{const DUCKDB_V2_ERROR_DATABASE_SERIALIZATION})

-- | A settings-related error occurred (e.g. setting an unknown option).
pattern DuckDBV2ErrorConfigurationSettings :: DuckDBV2Error
pattern DuckDBV2ErrorConfigurationSettings = DuckDBV2Error (#{const DUCKDB_V2_ERROR_CONFIGURATION_SETTINGS})

-- | The database configuration is invalid.
pattern DuckDBV2ErrorConfigurationInvalid :: DuckDBV2Error
pattern DuckDBV2ErrorConfigurationInvalid = DuckDBV2Error (#{const DUCKDB_V2_ERROR_CONFIGURATION_INVALID})

-- | The operation is not permitted under the current configuration.
pattern DuckDBV2ErrorConfigurationPermission :: DuckDBV2Error
pattern DuckDBV2ErrorConfigurationPermission = DuckDBV2Error (#{const DUCKDB_V2_ERROR_CONFIGURATION_PERMISSION})

-- | An internal invariant was violated; this indicates a bug in DuckDB.
pattern DuckDBV2ErrorRuntimeInternal :: DuckDBV2Error
pattern DuckDBV2ErrorRuntimeInternal = DuckDBV2Error (#{const DUCKDB_V2_ERROR_RUNTIME_INTERNAL})

-- | A fatal error occurred; the database is no longer usable.
pattern DuckDBV2ErrorRuntimeFatal :: DuckDBV2Error
pattern DuckDBV2ErrorRuntimeFatal = DuckDBV2Error (#{const DUCKDB_V2_ERROR_RUNTIME_FATAL})

-- | The operation was interrupted (e.g. by a cancel request).
pattern DuckDBV2ErrorRuntimeInterrupt :: DuckDBV2Error
pattern DuckDBV2ErrorRuntimeInterrupt = DuckDBV2Error (#{const DUCKDB_V2_ERROR_RUNTIME_INTERRUPT})

-- | A required pointer was unexpectedly null.
pattern DuckDBV2ErrorRuntimeNullPointer :: DuckDBV2Error
pattern DuckDBV2ErrorRuntimeNullPointer = DuckDBV2Error (#{const DUCKDB_V2_ERROR_RUNTIME_NULL_POINTER})

-- | The @DUCKDB_V2_ERROR_MAX_ENUM@ constant.
pattern DuckDBV2ErrorMaxEnum :: DuckDBV2Error
pattern DuckDBV2ErrorMaxEnum = DuckDBV2Error (#{const DUCKDB_V2_ERROR_MAX_ENUM})

{- | Identifies a configurable function property. Pass one of these to @duckdb_v2_scalar_function_set_property()@ or
@duckdb_v2_aggregate_function_set_property()@ together with a matching @DUCKDB_V2_FUNCTION_PROPERTY_VALUE@. The high
byte is the function-type group: COMMON (@0x01xxxx@) keys apply to all function types, while group-specific keys
(e.g. aggregate @0x03xxxx@) are only valid for that function type and are rejected with an error otherwise.
-}
newtype DuckDBV2FunctionPropertyKey = DuckDBV2FunctionPropertyKey (#{type DUCKDB_V2_FUNCTION_PROPERTY_KEY})
    deriving (Eq, Ord, Show, Read, Storable)

{- | How stable/deterministic the function's result is across rows and queries. Accepts the
@FUNCTION_PROPERTY_STABILITY_*@ values. Defaults to @CONSISTENT@.
-}
pattern DuckDBV2FunctionPropertyStability :: DuckDBV2FunctionPropertyKey
pattern DuckDBV2FunctionPropertyStability = DuckDBV2FunctionPropertyKey (#{const DUCKDB_V2_FUNCTION_PROPERTY_STABILITY})

{- | Whether the function handles NULL inputs itself. Accepts the @FUNCTION_PROPERTY_NULL_HANDLING_*@ values. Defaults
to @DEFAULT@ (NULL in, NULL out).
-}
pattern DuckDBV2FunctionPropertyNullHandling :: DuckDBV2FunctionPropertyKey
pattern DuckDBV2FunctionPropertyNullHandling = DuckDBV2FunctionPropertyKey (#{const DUCKDB_V2_FUNCTION_PROPERTY_NULL_HANDLING})

{- | Whether the function can raise a runtime error. Accepts the @FUNCTION_PROPERTY_FALLIBILITY_*@ values. Functions
created through this API default to @FALLIBLE@, since their callbacks can report errors.
-}
pattern DuckDBV2FunctionPropertyFallibility :: DuckDBV2FunctionPropertyKey
pattern DuckDBV2FunctionPropertyFallibility = DuckDBV2FunctionPropertyKey (#{const DUCKDB_V2_FUNCTION_PROPERTY_FALLIBILITY})

{- | How the function interacts with collations on its arguments. Accepts the @FUNCTION_PROPERTY_COLLATION_HANDLING_*@
values. Defaults to @PROPAGATE@.
-}
pattern DuckDBV2FunctionPropertyCollationHandling :: DuckDBV2FunctionPropertyKey
pattern DuckDBV2FunctionPropertyCollationHandling = DuckDBV2FunctionPropertyKey (#{const DUCKDB_V2_FUNCTION_PROPERTY_COLLATION_HANDLING})

{- | Aggregate only. Whether the aggregate's result depends on the order in which rows are aggregated. Accepts the
@FUNCTION_PROPERTY_AGG_ORDER_DEPENDENT_*@ values. Defaults to @YES@.
-}
pattern DuckDBV2FunctionPropertyAggOrderDependent :: DuckDBV2FunctionPropertyKey
pattern DuckDBV2FunctionPropertyAggOrderDependent = DuckDBV2FunctionPropertyKey (#{const DUCKDB_V2_FUNCTION_PROPERTY_AGG_ORDER_DEPENDENT})

{- | Aggregate only. Whether the aggregate's result is affected by a DISTINCT modifier. Accepts the
@FUNCTION_PROPERTY_AGG_DISTINCT_DEPENDENT_*@ values. Defaults to @YES@.
-}
pattern DuckDBV2FunctionPropertyAggDistinctDependent :: DuckDBV2FunctionPropertyKey
pattern DuckDBV2FunctionPropertyAggDistinctDependent = DuckDBV2FunctionPropertyKey (#{const DUCKDB_V2_FUNCTION_PROPERTY_AGG_DISTINCT_DEPENDENT})

-- | The @DUCKDB_V2_FUNCTION_PROPERTY_KEY_MAX_ENUM@ constant.
pattern DuckDBV2FunctionPropertyKeyMaxEnum :: DuckDBV2FunctionPropertyKey
pattern DuckDBV2FunctionPropertyKeyMaxEnum = DuckDBV2FunctionPropertyKey (#{const DUCKDB_V2_FUNCTION_PROPERTY_KEY_MAX_ENUM})

{- | A value for a @DUCKDB_V2_FUNCTION_PROPERTY_KEY@. Each key owns a 256-value block: a value @V@ is valid for key @K@
iff @(V & 0xFFFF00) == K@, and shares the key's function-type group (@V & 0xFF0000@) in its high byte. Passing a
value that does not match the key is rejected with an error. Boolean-like properties are expressed as named values so
that additional values can be added later without breaking the ABI.
-}
newtype DuckDBV2FunctionPropertyValue = DuckDBV2FunctionPropertyValue (#{type DUCKDB_V2_FUNCTION_PROPERTY_VALUE})
    deriving (Eq, Ord, Show, Read, Storable)

-- | The function always returns the same result given the same input.
pattern DuckDBV2FunctionPropertyStabilityConsistent :: DuckDBV2FunctionPropertyValue
pattern DuckDBV2FunctionPropertyStabilityConsistent = DuckDBV2FunctionPropertyValue (#{const DUCKDB_V2_FUNCTION_PROPERTY_STABILITY_CONSISTENT})

-- | The result may differ per row (e.g. random()).
pattern DuckDBV2FunctionPropertyStabilityVolatile :: DuckDBV2FunctionPropertyValue
pattern DuckDBV2FunctionPropertyStabilityVolatile = DuckDBV2FunctionPropertyValue (#{const DUCKDB_V2_FUNCTION_PROPERTY_STABILITY_VOLATILE})

-- | The result is stable within a single query/transaction but may change across queries (e.g. now()).
pattern DuckDBV2FunctionPropertyStabilityConsistentWithinQuery :: DuckDBV2FunctionPropertyValue
pattern DuckDBV2FunctionPropertyStabilityConsistentWithinQuery = DuckDBV2FunctionPropertyValue (#{const DUCKDB_V2_FUNCTION_PROPERTY_STABILITY_CONSISTENT_WITHIN_QUERY})

-- | Default NULL handling: if any argument is NULL the result is NULL and the function is not invoked for that row.
pattern DuckDBV2FunctionPropertyNullHandlingDefault :: DuckDBV2FunctionPropertyValue
pattern DuckDBV2FunctionPropertyNullHandlingDefault = DuckDBV2FunctionPropertyValue (#{const DUCKDB_V2_FUNCTION_PROPERTY_NULL_HANDLING_DEFAULT})

-- | The function handles NULL inputs itself and is invoked even when arguments are NULL.
pattern DuckDBV2FunctionPropertyNullHandlingSpecial :: DuckDBV2FunctionPropertyValue
pattern DuckDBV2FunctionPropertyNullHandlingSpecial = DuckDBV2FunctionPropertyValue (#{const DUCKDB_V2_FUNCTION_PROPERTY_NULL_HANDLING_SPECIAL})

{- | The function never raises a runtime error. Declaring this promises that the callbacks never report an error; an
error reported anyway becomes an internal error.
-}
pattern DuckDBV2FunctionPropertyFallibilityInfallible :: DuckDBV2FunctionPropertyValue
pattern DuckDBV2FunctionPropertyFallibilityInfallible = DuckDBV2FunctionPropertyValue (#{const DUCKDB_V2_FUNCTION_PROPERTY_FALLIBILITY_INFALLIBLE})

-- | The function may raise a runtime error for some inputs.
pattern DuckDBV2FunctionPropertyFallibilityFallible :: DuckDBV2FunctionPropertyValue
pattern DuckDBV2FunctionPropertyFallibilityFallible = DuckDBV2FunctionPropertyValue (#{const DUCKDB_V2_FUNCTION_PROPERTY_FALLIBILITY_FALLIBLE})

-- | The function combines collations from its inputs and propagates them to its result (default).
pattern DuckDBV2FunctionPropertyCollationHandlingPropagate :: DuckDBV2FunctionPropertyValue
pattern DuckDBV2FunctionPropertyCollationHandlingPropagate = DuckDBV2FunctionPropertyValue (#{const DUCKDB_V2_FUNCTION_PROPERTY_COLLATION_HANDLING_PROPAGATE})

-- | Combinable collations are executed on the input arguments before the function runs.
pattern DuckDBV2FunctionPropertyCollationHandlingPushCombinable :: DuckDBV2FunctionPropertyValue
pattern DuckDBV2FunctionPropertyCollationHandlingPushCombinable = DuckDBV2FunctionPropertyValue (#{const DUCKDB_V2_FUNCTION_PROPERTY_COLLATION_HANDLING_PUSH_COMBINABLE})

-- | Collations are ignored by the function.
pattern DuckDBV2FunctionPropertyCollationHandlingIgnore :: DuckDBV2FunctionPropertyValue
pattern DuckDBV2FunctionPropertyCollationHandlingIgnore = DuckDBV2FunctionPropertyValue (#{const DUCKDB_V2_FUNCTION_PROPERTY_COLLATION_HANDLING_IGNORE})

-- | The aggregate's result depends on the order in which rows are aggregated (default).
pattern DuckDBV2FunctionPropertyAggOrderDependentYes :: DuckDBV2FunctionPropertyValue
pattern DuckDBV2FunctionPropertyAggOrderDependentYes = DuckDBV2FunctionPropertyValue (#{const DUCKDB_V2_FUNCTION_PROPERTY_AGG_ORDER_DEPENDENT_YES})

-- | The aggregate's result does not depend on input order.
pattern DuckDBV2FunctionPropertyAggOrderDependentNo :: DuckDBV2FunctionPropertyValue
pattern DuckDBV2FunctionPropertyAggOrderDependentNo = DuckDBV2FunctionPropertyValue (#{const DUCKDB_V2_FUNCTION_PROPERTY_AGG_ORDER_DEPENDENT_NO})

-- | The aggregate's result is affected by a DISTINCT modifier (default).
pattern DuckDBV2FunctionPropertyAggDistinctDependentYes :: DuckDBV2FunctionPropertyValue
pattern DuckDBV2FunctionPropertyAggDistinctDependentYes = DuckDBV2FunctionPropertyValue (#{const DUCKDB_V2_FUNCTION_PROPERTY_AGG_DISTINCT_DEPENDENT_YES})

-- | The aggregate's result is not affected by a DISTINCT modifier.
pattern DuckDBV2FunctionPropertyAggDistinctDependentNo :: DuckDBV2FunctionPropertyValue
pattern DuckDBV2FunctionPropertyAggDistinctDependentNo = DuckDBV2FunctionPropertyValue (#{const DUCKDB_V2_FUNCTION_PROPERTY_AGG_DISTINCT_DEPENDENT_NO})

-- | The @DUCKDB_V2_FUNCTION_PROPERTY_VALUE_MAX_ENUM@ constant.
pattern DuckDBV2FunctionPropertyValueMaxEnum :: DuckDBV2FunctionPropertyValue
pattern DuckDBV2FunctionPropertyValueMaxEnum = DuckDBV2FunctionPropertyValue (#{const DUCKDB_V2_FUNCTION_PROPERTY_VALUE_MAX_ENUM})

{- | The mode a cast is being executed in. A normal cast must either succeed for every row or report an error, which
aborts the query. A "try" cast (SQL TRY_CAST, and implicit casts the engine probes speculatively) tolerates per-row
failures: the callback writes NULL for the rows it could not convert instead of aborting. Read with
@duckdb_v2_cast_function_exec_get_mode()@.
-}
newtype DuckDBV2CastMode = DuckDBV2CastMode (#{type DUCKDB_V2_CAST_MODE})
    deriving (Eq, Ord, Show, Read, Storable)

-- | A regular cast. A conversion failure reported through the error slot aborts the query.
pattern DuckDBV2CastModeNormal :: DuckDBV2CastMode
pattern DuckDBV2CastModeNormal = DuckDBV2CastMode (#{const DUCKDB_V2_CAST_MODE_NORMAL})

{- | A "try" cast. Conversion failures should be written as NULLs into the output vector; an error reported through
the slot is swallowed and the affected rows are left NULL.
-}
pattern DuckDBV2CastModeTry :: DuckDBV2CastMode
pattern DuckDBV2CastModeTry = DuckDBV2CastMode (#{const DUCKDB_V2_CAST_MODE_TRY})

-- | The @DUCKDB_V2_CAST_MODE_MAX_ENUM@ constant.
pattern DuckDBV2CastModeMaxEnum :: DuckDBV2CastMode
pattern DuckDBV2CastModeMaxEnum = DuckDBV2CastMode (#{const DUCKDB_V2_CAST_MODE_MAX_ENUM})

{- | The scope target of an option: where DuckDB permits its setting to be written. UNKNOWN is reported for an option
whose declaration carries no explicit scope target, which includes every extension option.
-}
newtype DuckDBV2OptionTargetScope = DuckDBV2OptionTargetScope (#{type DUCKDB_V2_OPTION_TARGET_SCOPE})
    deriving (Eq, Ord, Show, Read, Storable)

-- | Target scope is not known.
pattern DuckDBV2OptionTargetScopeUnknown :: DuckDBV2OptionTargetScope
pattern DuckDBV2OptionTargetScopeUnknown = DuckDBV2OptionTargetScope (#{const DUCKDB_V2_OPTION_TARGET_SCOPE_UNKNOWN})

-- | May only be written at GLOBAL (instance) scope.
pattern DuckDBV2OptionTargetScopeGlobalOnly :: DuckDBV2OptionTargetScope
pattern DuckDBV2OptionTargetScopeGlobalOnly = DuckDBV2OptionTargetScope (#{const DUCKDB_V2_OPTION_TARGET_SCOPE_GLOBAL_ONLY})

-- | May only be written at LOCAL (session) scope.
pattern DuckDBV2OptionTargetScopeLocalOnly :: DuckDBV2OptionTargetScope
pattern DuckDBV2OptionTargetScopeLocalOnly = DuckDBV2OptionTargetScope (#{const DUCKDB_V2_OPTION_TARGET_SCOPE_LOCAL_ONLY})

-- | May be written at either scope; defaults to GLOBAL when unspecified.
pattern DuckDBV2OptionTargetScopeGlobalDefault :: DuckDBV2OptionTargetScope
pattern DuckDBV2OptionTargetScopeGlobalDefault = DuckDBV2OptionTargetScope (#{const DUCKDB_V2_OPTION_TARGET_SCOPE_GLOBAL_DEFAULT})

-- | May be written at either scope; defaults to LOCAL when unspecified.
pattern DuckDBV2OptionTargetScopeLocalDefault :: DuckDBV2OptionTargetScope
pattern DuckDBV2OptionTargetScopeLocalDefault = DuckDBV2OptionTargetScope (#{const DUCKDB_V2_OPTION_TARGET_SCOPE_LOCAL_DEFAULT})

-- | The @DUCKDB_V2_OPTION_TARGET_SCOPE_MAX_ENUM@ constant.
pattern DuckDBV2OptionTargetScopeMaxEnum :: DuckDBV2OptionTargetScope
pattern DuckDBV2OptionTargetScopeMaxEnum = DuckDBV2OptionTargetScope (#{const DUCKDB_V2_OPTION_TARGET_SCOPE_MAX_ENUM})

{- | Destination scope for a connection-side option write, as taken by connection_option_set. AUTOMATIC defers to the
option's target scope, like SQL @SET name = value@. GLOBAL writes through to the instance, visible to all
connections, like @SET GLOBAL@. LOCAL writes to the connection's session only, like @SET LOCAL@ / @SET SESSION@.
-}
newtype DuckDBV2SettingScope = DuckDBV2SettingScope (#{type DUCKDB_V2_SETTING_SCOPE})
    deriving (Eq, Ord, Show, Read, Storable)

-- | Resolve from the option's target scope.
pattern DuckDBV2SettingScopeAutomatic :: DuckDBV2SettingScope
pattern DuckDBV2SettingScopeAutomatic = DuckDBV2SettingScope (#{const DUCKDB_V2_SETTING_SCOPE_AUTOMATIC})

-- | Write through to the instance (visible to all connections).
pattern DuckDBV2SettingScopeGlobal :: DuckDBV2SettingScope
pattern DuckDBV2SettingScopeGlobal = DuckDBV2SettingScope (#{const DUCKDB_V2_SETTING_SCOPE_GLOBAL})

-- | Write to the connection's session only.
pattern DuckDBV2SettingScopeLocal :: DuckDBV2SettingScope
pattern DuckDBV2SettingScopeLocal = DuckDBV2SettingScope (#{const DUCKDB_V2_SETTING_SCOPE_LOCAL})

-- | The @DUCKDB_V2_SETTING_SCOPE_MAX_ENUM@ constant.
pattern DuckDBV2SettingScopeMaxEnum :: DuckDBV2SettingScope
pattern DuckDBV2SettingScopeMaxEnum = DuckDBV2SettingScope (#{const DUCKDB_V2_SETTING_SCOPE_MAX_ENUM})

{- | How @duckdb_v2_file_system_open()@ opens a file. These are not a bitmask -- apply them one at a time with
@duckdb_v2_file_open_options_set_flag()@, calling it once per behaviour you want, e.g. @FILE_FLAG_WRITE@ then
@FILE_FLAG_CREATE@ to write to a file and create it when it does not exist.
-}
newtype DuckDBV2FileFlag = DuckDBV2FileFlag (#{type DUCKDB_V2_FILE_FLAG})
    deriving (Eq, Ord, Show, Read, Storable)

-- | error.
pattern DuckDBV2FileFlagInvalid :: DuckDBV2FileFlag
pattern DuckDBV2FileFlagInvalid = DuckDBV2FileFlag (#{const DUCKDB_V2_FILE_FLAG_INVALID})

-- | Open the file with "read" capabilities.
pattern DuckDBV2FileFlagRead :: DuckDBV2FileFlag
pattern DuckDBV2FileFlagRead = DuckDBV2FileFlag (#{const DUCKDB_V2_FILE_FLAG_READ})

-- | Open the file with "write" capabilities.
pattern DuckDBV2FileFlagWrite :: DuckDBV2FileFlag
pattern DuckDBV2FileFlagWrite = DuckDBV2FileFlag (#{const DUCKDB_V2_FILE_FLAG_WRITE})

-- | Create the file if it does not exist, and open it as it is if it does.
pattern DuckDBV2FileFlagCreate :: DuckDBV2FileFlag
pattern DuckDBV2FileFlagCreate = DuckDBV2FileFlag (#{const DUCKDB_V2_FILE_FLAG_CREATE})

{- | Create the file if it does not exist, and truncate it to empty if it does. To fail instead of truncating, combine
@FILE_FLAG_CREATE@ with @FILE_FLAG_EXCLUSIVE_CREATE@.
-}
pattern DuckDBV2FileFlagCreateNew :: DuckDBV2FileFlag
pattern DuckDBV2FileFlagCreateNew = DuckDBV2FileFlag (#{const DUCKDB_V2_FILE_FLAG_CREATE_NEW})

-- | Open the file in "append" mode.
pattern DuckDBV2FileFlagAppend :: DuckDBV2FileFlag
pattern DuckDBV2FileFlagAppend = DuckDBV2FileFlag (#{const DUCKDB_V2_FILE_FLAG_APPEND})

-- | Fail if the file already exists. A modifier on @FILE_FLAG_CREATE@, and meaningless without it.
pattern DuckDBV2FileFlagExclusiveCreate :: DuckDBV2FileFlag
pattern DuckDBV2FileFlagExclusiveCreate = DuckDBV2FileFlag (#{const DUCKDB_V2_FILE_FLAG_EXCLUSIVE_CREATE})

{- | The file will be read and written at explicit offsets from several threads at once. Pass it whenever
@duckdb_v2_file_read_at()@ or @duckdb_v2_file_write_at()@ are used concurrently -- a file system that would
otherwise assume sequential access, by caching, buffering, or keeping a single cursor, needs to know not to.
-}
pattern DuckDBV2FileFlagParallelAccess :: DuckDBV2FileFlag
pattern DuckDBV2FileFlagParallelAccess = DuckDBV2FileFlag (#{const DUCKDB_V2_FILE_FLAG_PARALLEL_ACCESS})

{- | Read the file through DuckDB's external file cache, which keeps the bytes that were read in memory, so that later
reads of the same file - also by later queries - do not have to read them again. Whether a local file is cached
is decided by the @cache_local_files@ setting. Only meaningful for reading.
-}
pattern DuckDBV2FileFlagExternalFileCache :: DuckDBV2FileFlag
pattern DuckDBV2FileFlagExternalFileCache = DuckDBV2FileFlag (#{const DUCKDB_V2_FILE_FLAG_EXTERNAL_FILE_CACHE})

-- | The @DUCKDB_V2_FILE_FLAG_MAX_ENUM@ constant.
pattern DuckDBV2FileFlagMaxEnum :: DuckDBV2FileFlag
pattern DuckDBV2FileFlagMaxEnum = DuckDBV2FileFlag (#{const DUCKDB_V2_FILE_FLAG_MAX_ENUM})

{- | How a caller passes the argument for a parameter, following Python's parameter kinds. A signature orders its
parameters by kind, in the order the values are listed here. Pass one of these to
@duckdb_v2_function_signature_add_parameter()@.
-}
newtype DuckDBV2FunctionParameterKind = DuckDBV2FunctionParameterKind (#{type DUCKDB_V2_FUNCTION_PARAMETER_KIND})
    deriving (Eq, Ord, Show, Read, Storable)

{- | Passed by position only. The name is invisible to a caller: a named argument with the same name is received by
@**kwargs@, or rejected when the signature has none.
-}
pattern DuckDBV2FunctionParameterKindPositionalOnly :: DuckDBV2FunctionParameterKind
pattern DuckDBV2FunctionParameterKindPositionalOnly = DuckDBV2FunctionParameterKind (#{const DUCKDB_V2_FUNCTION_PARAMETER_KIND_POSITIONAL_ONLY})

-- | Passed by position or by name.
pattern DuckDBV2FunctionParameterKindStandard :: DuckDBV2FunctionParameterKind
pattern DuckDBV2FunctionParameterKindStandard = DuckDBV2FunctionParameterKind (#{const DUCKDB_V2_FUNCTION_PARAMETER_KIND_STANDARD})

{- | @*args@: receives the positional arguments left over after the positional-only and standard parameters, each cast
to the parameter type. ANY leaves them un-cast.
-}
pattern DuckDBV2FunctionParameterKindPositionalVariadic :: DuckDBV2FunctionParameterKind
pattern DuckDBV2FunctionParameterKindPositionalVariadic = DuckDBV2FunctionParameterKind (#{const DUCKDB_V2_FUNCTION_PARAMETER_KIND_POSITIONAL_VARIADIC})

-- | Passed by name only.
pattern DuckDBV2FunctionParameterKindNamedOnly :: DuckDBV2FunctionParameterKind
pattern DuckDBV2FunctionParameterKindNamedOnly = DuckDBV2FunctionParameterKind (#{const DUCKDB_V2_FUNCTION_PARAMETER_KIND_NAMED_ONLY})

{- | @**kwargs@: receives the named arguments that match no other parameter, each cast to the parameter type. ANY
leaves them un-cast.
-}
pattern DuckDBV2FunctionParameterKindNamedVariadic :: DuckDBV2FunctionParameterKind
pattern DuckDBV2FunctionParameterKindNamedVariadic = DuckDBV2FunctionParameterKind (#{const DUCKDB_V2_FUNCTION_PARAMETER_KIND_NAMED_VARIADIC})

-- | The @DUCKDB_V2_FUNCTION_PARAMETER_KIND_MAX_ENUM@ constant.
pattern DuckDBV2FunctionParameterKindMaxEnum :: DuckDBV2FunctionParameterKind
pattern DuckDBV2FunctionParameterKindMaxEnum = DuckDBV2FunctionParameterKind (#{const DUCKDB_V2_FUNCTION_PARAMETER_KIND_MAX_ENUM})

{- | Severity of a log message. Mirrors DuckDB's own log levels, and is compared against the configured threshold: an
entry below it is dropped.
-}
newtype DuckDBV2LogLevel = DuckDBV2LogLevel (#{type DUCKDB_V2_LOG_LEVEL})
    deriving (Eq, Ord, Show, Read, Storable)

-- | Finest detail; off in any default configuration.
pattern DuckDBV2LogLevelTrace :: DuckDBV2LogLevel
pattern DuckDBV2LogLevelTrace = DuckDBV2LogLevel (#{const DUCKDB_V2_LOG_LEVEL_TRACE})

-- | Diagnostic detail.
pattern DuckDBV2LogLevelDebug :: DuckDBV2LogLevel
pattern DuckDBV2LogLevelDebug = DuckDBV2LogLevel (#{const DUCKDB_V2_LOG_LEVEL_DEBUG})

-- | Ordinary progress information.
pattern DuckDBV2LogLevelInfo :: DuckDBV2LogLevel
pattern DuckDBV2LogLevelInfo = DuckDBV2LogLevel (#{const DUCKDB_V2_LOG_LEVEL_INFO})

-- | A recoverable problem.
pattern DuckDBV2LogLevelWarning :: DuckDBV2LogLevel
pattern DuckDBV2LogLevelWarning = DuckDBV2LogLevel (#{const DUCKDB_V2_LOG_LEVEL_WARNING})

-- | An operation failed.
pattern DuckDBV2LogLevelError :: DuckDBV2LogLevel
pattern DuckDBV2LogLevelError = DuckDBV2LogLevel (#{const DUCKDB_V2_LOG_LEVEL_ERROR})

-- | An unrecoverable failure.
pattern DuckDBV2LogLevelFatal :: DuckDBV2LogLevel
pattern DuckDBV2LogLevelFatal = DuckDBV2LogLevel (#{const DUCKDB_V2_LOG_LEVEL_FATAL})

-- | The @DUCKDB_V2_LOG_LEVEL_MAX_ENUM@ constant.
pattern DuckDBV2LogLevelMaxEnum :: DuckDBV2LogLevel
pattern DuckDBV2LogLevelMaxEnum = DuckDBV2LogLevel (#{const DUCKDB_V2_LOG_LEVEL_MAX_ENUM})

-- | The lexical class of a token.
newtype DuckDBV2TokenType = DuckDBV2TokenType (#{type DUCKDB_V2_TOKEN_TYPE})
    deriving (Eq, Ord, Show, Read, Storable)

-- | Not an actual token class; out_type is set to this when @duckdb_v2_token_iterator_next()@ fails.
pattern DuckDBV2TokenTypeInvalid :: DuckDBV2TokenType
pattern DuckDBV2TokenTypeInvalid = DuckDBV2TokenType (#{const DUCKDB_V2_TOKEN_TYPE_INVALID})

-- | A keyword of the connection's grammar.
pattern DuckDBV2TokenTypeKeyword :: DuckDBV2TokenType
pattern DuckDBV2TokenTypeKeyword = DuckDBV2TokenType (#{const DUCKDB_V2_TOKEN_TYPE_KEYWORD})

-- | A bare or double-quoted identifier, quotes included.
pattern DuckDBV2TokenTypeIdentifier :: DuckDBV2TokenType
pattern DuckDBV2TokenTypeIdentifier = DuckDBV2TokenType (#{const DUCKDB_V2_TOKEN_TYPE_IDENTIFIER})

-- | A quoted or dollar-quoted string, delimiters included.
pattern DuckDBV2TokenTypeStringLiteral :: DuckDBV2TokenType
pattern DuckDBV2TokenTypeStringLiteral = DuckDBV2TokenType (#{const DUCKDB_V2_TOKEN_TYPE_STRING_LITERAL})

-- | A numeric literal.
pattern DuckDBV2TokenTypeNumberLiteral :: DuckDBV2TokenType
pattern DuckDBV2TokenTypeNumberLiteral = DuckDBV2TokenType (#{const DUCKDB_V2_TOKEN_TYPE_NUMBER_LITERAL})

-- | An operator or punctuation run other than the statement terminator.
pattern DuckDBV2TokenTypeOperator :: DuckDBV2TokenType
pattern DuckDBV2TokenTypeOperator = DuckDBV2TokenType (#{const DUCKDB_V2_TOKEN_TYPE_OPERATOR})

-- | A line or block comment, delimiters included.
pattern DuckDBV2TokenTypeComment :: DuckDBV2TokenType
pattern DuckDBV2TokenTypeComment = DuckDBV2TokenType (#{const DUCKDB_V2_TOKEN_TYPE_COMMENT})

-- | A statement-terminating semicolon.
pattern DuckDBV2TokenTypeTerminator :: DuckDBV2TokenType
pattern DuckDBV2TokenTypeTerminator = DuckDBV2TokenType (#{const DUCKDB_V2_TOKEN_TYPE_TERMINATOR})

{- | Not a token; reported by token_iterator_next once the input is exhausted, with start equal to the input length
and length 0.
-}
pattern DuckDBV2TokenTypeEndOfInput :: DuckDBV2TokenType
pattern DuckDBV2TokenTypeEndOfInput = DuckDBV2TokenType (#{const DUCKDB_V2_TOKEN_TYPE_END_OF_INPUT})

-- | The @DUCKDB_V2_TOKEN_TYPE_MAX_ENUM@ constant.
pattern DuckDBV2TokenTypeMaxEnum :: DuckDBV2TokenType
pattern DuckDBV2TokenTypeMaxEnum = DuckDBV2TokenType (#{const DUCKDB_V2_TOKEN_TYPE_MAX_ENUM})

{- | Internal representation of a vector, with FSST / SEQUENCE / SHREDDED collapsed into OTHER: the view getter rejects
those and requires an explicit vector_flatten first. OTHER is the 0-value, so a zero-initialized out-param reads as
"unspecified, needs flatten" rather than silently looking like FLAT.
-}
newtype DuckDBV2VectorType = DuckDBV2VectorType (#{type DUCKDB_V2_VECTOR_TYPE})
    deriving (Eq, Ord, Show, Read, Storable)

-- | Default for zero-init. Covers FSST / SEQUENCE / SHREDDED — call vector_flatten first.
pattern DuckDBV2VectorTypeOther :: DuckDBV2VectorType
pattern DuckDBV2VectorTypeOther = DuckDBV2VectorType (#{const DUCKDB_V2_VECTOR_TYPE_OTHER})

-- | Standard per-row storage.
pattern DuckDBV2VectorTypeFlat :: DuckDBV2VectorType
pattern DuckDBV2VectorTypeFlat = DuckDBV2VectorType (#{const DUCKDB_V2_VECTOR_TYPE_FLAT})

-- | A single value applies to every row in the vector.
pattern DuckDBV2VectorTypeConstant :: DuckDBV2VectorType
pattern DuckDBV2VectorTypeConstant = DuckDBV2VectorType (#{const DUCKDB_V2_VECTOR_TYPE_CONSTANT})

-- | Data + selection vector indirection into another vector.
pattern DuckDBV2VectorTypeDictionary :: DuckDBV2VectorType
pattern DuckDBV2VectorTypeDictionary = DuckDBV2VectorType (#{const DUCKDB_V2_VECTOR_TYPE_DICTIONARY})

-- | The @DUCKDB_V2_VECTOR_TYPE_MAX_ENUM@ constant.
pattern DuckDBV2VectorTypeMaxEnum :: DuckDBV2VectorType
pattern DuckDBV2VectorTypeMaxEnum = DuckDBV2VectorType (#{const DUCKDB_V2_VECTOR_TYPE_MAX_ENUM})

{- | The type of a bound expression node. The values mirror the engine's own expression types, restricted to the node
types a filter predicate can contain; every other node type is reported as @EXPRESSION_TYPE_INVALID@.

Comparisons, @BETWEEN@ and casts are regular scalar function calls once bound: they have the function's children and
name, and only their type tells them apart from any other function. A @CAST@ therefore has one child, the value being
cast, and its target type is the node's return type.
-}
newtype DuckDBV2ExpressionType = DuckDBV2ExpressionType (#{type DUCKDB_V2_EXPRESSION_TYPE})
    deriving (Eq, Ord, Show, Read, Storable)

-- | A node type this API does not model. Its children can still be walked.
pattern DuckDBV2ExpressionTypeInvalid :: DuckDBV2ExpressionType
pattern DuckDBV2ExpressionTypeInvalid = DuckDBV2ExpressionType (#{const DUCKDB_V2_EXPRESSION_TYPE_INVALID})

{- | A cast. One child; the target type is the node's return type and @duckdb_v2_expression_cast_get_mode()@ tells a
@TRY_CAST@ apart.
-}
pattern DuckDBV2ExpressionTypeOperatorCast :: DuckDBV2ExpressionType
pattern DuckDBV2ExpressionTypeOperatorCast = DuckDBV2ExpressionType (#{const DUCKDB_V2_EXPRESSION_TYPE_OPERATOR_CAST})

-- | Logical @NOT@. One child.
pattern DuckDBV2ExpressionTypeOperatorNot :: DuckDBV2ExpressionType
pattern DuckDBV2ExpressionTypeOperatorNot = DuckDBV2ExpressionType (#{const DUCKDB_V2_EXPRESSION_TYPE_OPERATOR_NOT})

-- | @IS NULL@. One child.
pattern DuckDBV2ExpressionTypeOperatorIsNull :: DuckDBV2ExpressionType
pattern DuckDBV2ExpressionTypeOperatorIsNull = DuckDBV2ExpressionType (#{const DUCKDB_V2_EXPRESSION_TYPE_OPERATOR_IS_NULL})

-- | @IS NOT NULL@. One child.
pattern DuckDBV2ExpressionTypeOperatorIsNotNull :: DuckDBV2ExpressionType
pattern DuckDBV2ExpressionTypeOperatorIsNotNull = DuckDBV2ExpressionType (#{const DUCKDB_V2_EXPRESSION_TYPE_OPERATOR_IS_NOT_NULL})

-- | @=@. Two children.
pattern DuckDBV2ExpressionTypeCompareEqual :: DuckDBV2ExpressionType
pattern DuckDBV2ExpressionTypeCompareEqual = DuckDBV2ExpressionType (#{const DUCKDB_V2_EXPRESSION_TYPE_COMPARE_EQUAL})

-- | @<>@. Two children.
pattern DuckDBV2ExpressionTypeCompareNotequal :: DuckDBV2ExpressionType
pattern DuckDBV2ExpressionTypeCompareNotequal = DuckDBV2ExpressionType (#{const DUCKDB_V2_EXPRESSION_TYPE_COMPARE_NOTEQUAL})

-- | @<@. Two children.
pattern DuckDBV2ExpressionTypeCompareLessthan :: DuckDBV2ExpressionType
pattern DuckDBV2ExpressionTypeCompareLessthan = DuckDBV2ExpressionType (#{const DUCKDB_V2_EXPRESSION_TYPE_COMPARE_LESSTHAN})

-- | @>@. Two children.
pattern DuckDBV2ExpressionTypeCompareGreaterthan :: DuckDBV2ExpressionType
pattern DuckDBV2ExpressionTypeCompareGreaterthan = DuckDBV2ExpressionType (#{const DUCKDB_V2_EXPRESSION_TYPE_COMPARE_GREATERTHAN})

-- | @<=@. Two children.
pattern DuckDBV2ExpressionTypeCompareLessthanorequalto :: DuckDBV2ExpressionType
pattern DuckDBV2ExpressionTypeCompareLessthanorequalto = DuckDBV2ExpressionType (#{const DUCKDB_V2_EXPRESSION_TYPE_COMPARE_LESSTHANOREQUALTO})

-- | @>=@. Two children.
pattern DuckDBV2ExpressionTypeCompareGreaterthanorequalto :: DuckDBV2ExpressionType
pattern DuckDBV2ExpressionTypeCompareGreaterthanorequalto = DuckDBV2ExpressionType (#{const DUCKDB_V2_EXPRESSION_TYPE_COMPARE_GREATERTHANOREQUALTO})

-- | @IN@. The first child is the value tested, the remaining children are the candidates.
pattern DuckDBV2ExpressionTypeCompareIn :: DuckDBV2ExpressionType
pattern DuckDBV2ExpressionTypeCompareIn = DuckDBV2ExpressionType (#{const DUCKDB_V2_EXPRESSION_TYPE_COMPARE_IN})

-- | @NOT IN@. The first child is the value tested, the remaining children are the candidates.
pattern DuckDBV2ExpressionTypeCompareNotIn :: DuckDBV2ExpressionType
pattern DuckDBV2ExpressionTypeCompareNotIn = DuckDBV2ExpressionType (#{const DUCKDB_V2_EXPRESSION_TYPE_COMPARE_NOT_IN})

-- | @IS DISTINCT FROM@. Two children.
pattern DuckDBV2ExpressionTypeCompareDistinctFrom :: DuckDBV2ExpressionType
pattern DuckDBV2ExpressionTypeCompareDistinctFrom = DuckDBV2ExpressionType (#{const DUCKDB_V2_EXPRESSION_TYPE_COMPARE_DISTINCT_FROM})

-- | @BETWEEN@. Three children: the value tested, the lower bound and the upper bound.
pattern DuckDBV2ExpressionTypeCompareBetween :: DuckDBV2ExpressionType
pattern DuckDBV2ExpressionTypeCompareBetween = DuckDBV2ExpressionType (#{const DUCKDB_V2_EXPRESSION_TYPE_COMPARE_BETWEEN})

-- | @IS NOT DISTINCT FROM@. Two children.
pattern DuckDBV2ExpressionTypeCompareNotDistinctFrom :: DuckDBV2ExpressionType
pattern DuckDBV2ExpressionTypeCompareNotDistinctFrom = DuckDBV2ExpressionType (#{const DUCKDB_V2_EXPRESSION_TYPE_COMPARE_NOT_DISTINCT_FROM})

-- | Logical @AND@. Two or more children.
pattern DuckDBV2ExpressionTypeConjunctionAnd :: DuckDBV2ExpressionType
pattern DuckDBV2ExpressionTypeConjunctionAnd = DuckDBV2ExpressionType (#{const DUCKDB_V2_EXPRESSION_TYPE_CONJUNCTION_AND})

-- | Logical @OR@. Two or more children.
pattern DuckDBV2ExpressionTypeConjunctionOr :: DuckDBV2ExpressionType
pattern DuckDBV2ExpressionTypeConjunctionOr = DuckDBV2ExpressionType (#{const DUCKDB_V2_EXPRESSION_TYPE_CONJUNCTION_OR})

-- | A constant. No children; read it with @duckdb_v2_expression_constant_get_value()@.
pattern DuckDBV2ExpressionTypeValueConstant :: DuckDBV2ExpressionType
pattern DuckDBV2ExpressionTypeValueConstant = DuckDBV2ExpressionType (#{const DUCKDB_V2_EXPRESSION_TYPE_VALUE_CONSTANT})

-- | A prepared statement parameter whose value is not known yet. No children.
pattern DuckDBV2ExpressionTypeValueParameter :: DuckDBV2ExpressionType
pattern DuckDBV2ExpressionTypeValueParameter = DuckDBV2ExpressionType (#{const DUCKDB_V2_EXPRESSION_TYPE_VALUE_PARAMETER})

{- | A call to a scalar function other than the ones listed above. The children are its arguments; read the name with
@duckdb_v2_expression_function_get_name()@.
-}
pattern DuckDBV2ExpressionTypeBoundFunction :: DuckDBV2ExpressionType
pattern DuckDBV2ExpressionTypeBoundFunction = DuckDBV2ExpressionType (#{const DUCKDB_V2_EXPRESSION_TYPE_BOUND_FUNCTION})

-- | result.
pattern DuckDBV2ExpressionTypeCaseExpr :: DuckDBV2ExpressionType
pattern DuckDBV2ExpressionTypeCaseExpr = DuckDBV2ExpressionType (#{const DUCKDB_V2_EXPRESSION_TYPE_CASE_EXPR})

-- | @COALESCE@. One or more children.
pattern DuckDBV2ExpressionTypeOperatorCoalesce :: DuckDBV2ExpressionType
pattern DuckDBV2ExpressionTypeOperatorCoalesce = DuckDBV2ExpressionType (#{const DUCKDB_V2_EXPRESSION_TYPE_OPERATOR_COALESCE})

-- | A reference to a column. No children; read it with @duckdb_v2_expression_column_ref_get_index()@.
pattern DuckDBV2ExpressionTypeBoundColumnRef :: DuckDBV2ExpressionType
pattern DuckDBV2ExpressionTypeBoundColumnRef = DuckDBV2ExpressionType (#{const DUCKDB_V2_EXPRESSION_TYPE_BOUND_COLUMN_REF})

-- | The @DUCKDB_V2_EXPRESSION_TYPE_MAX_ENUM@ constant.
pattern DuckDBV2ExpressionTypeMaxEnum :: DuckDBV2ExpressionType
pattern DuckDBV2ExpressionTypeMaxEnum = DuckDBV2ExpressionType (#{const DUCKDB_V2_EXPRESSION_TYPE_MAX_ENUM})

{- | Logical type identifier. The values are the same integers DuckDB uses internally, so round-tripping is lossless. The
bind- and UDF-only ids (UNKNOWN, ANY, TEMPLATE) appear here for completeness; they do not show up in result column
types in practice.
-}
newtype DuckDBV2LogicalTypeId = DuckDBV2LogicalTypeId (#{type DUCKDB_V2_LOGICAL_TYPE_ID})
    deriving (Eq, Ord, Show, Read, Storable)

-- | Invalid / unset.
pattern DuckDBV2LogicalTypeIdInvalid :: DuckDBV2LogicalTypeId
pattern DuckDBV2LogicalTypeIdInvalid = DuckDBV2LogicalTypeId (#{const DUCKDB_V2_LOGICAL_TYPE_ID_INVALID})

-- | NULL constant type.
pattern DuckDBV2LogicalTypeIdSqlnull :: DuckDBV2LogicalTypeId
pattern DuckDBV2LogicalTypeIdSqlnull = DuckDBV2LogicalTypeId (#{const DUCKDB_V2_LOGICAL_TYPE_ID_SQLNULL})

-- | Unknown — used for unresolved parameter expressions.
pattern DuckDBV2LogicalTypeIdUnknown :: DuckDBV2LogicalTypeId
pattern DuckDBV2LogicalTypeIdUnknown = DuckDBV2LogicalTypeId (#{const DUCKDB_V2_LOGICAL_TYPE_ID_UNKNOWN})

-- | ANY — used for functions that accept any type.
pattern DuckDBV2LogicalTypeIdAny :: DuckDBV2LogicalTypeId
pattern DuckDBV2LogicalTypeIdAny = DuckDBV2LogicalTypeId (#{const DUCKDB_V2_LOGICAL_TYPE_ID_ANY})

{- | A type carried as a value (type parameters). Values of this type are built via value_create_type_with_context /
_with_connection.
-}
pattern DuckDBV2LogicalTypeIdType :: DuckDBV2LogicalTypeId
pattern DuckDBV2LogicalTypeIdType = DuckDBV2LogicalTypeId (#{const DUCKDB_V2_LOGICAL_TYPE_ID_TYPE})

-- | The @DUCKDB_V2_LOGICAL_TYPE_ID_BOOLEAN@ constant.
pattern DuckDBV2LogicalTypeIdBoolean :: DuckDBV2LogicalTypeId
pattern DuckDBV2LogicalTypeIdBoolean = DuckDBV2LogicalTypeId (#{const DUCKDB_V2_LOGICAL_TYPE_ID_BOOLEAN})

-- | The @DUCKDB_V2_LOGICAL_TYPE_ID_TINYINT@ constant.
pattern DuckDBV2LogicalTypeIdTinyint :: DuckDBV2LogicalTypeId
pattern DuckDBV2LogicalTypeIdTinyint = DuckDBV2LogicalTypeId (#{const DUCKDB_V2_LOGICAL_TYPE_ID_TINYINT})

-- | The @DUCKDB_V2_LOGICAL_TYPE_ID_SMALLINT@ constant.
pattern DuckDBV2LogicalTypeIdSmallint :: DuckDBV2LogicalTypeId
pattern DuckDBV2LogicalTypeIdSmallint = DuckDBV2LogicalTypeId (#{const DUCKDB_V2_LOGICAL_TYPE_ID_SMALLINT})

-- | The @DUCKDB_V2_LOGICAL_TYPE_ID_INTEGER@ constant.
pattern DuckDBV2LogicalTypeIdInteger :: DuckDBV2LogicalTypeId
pattern DuckDBV2LogicalTypeIdInteger = DuckDBV2LogicalTypeId (#{const DUCKDB_V2_LOGICAL_TYPE_ID_INTEGER})

-- | The @DUCKDB_V2_LOGICAL_TYPE_ID_BIGINT@ constant.
pattern DuckDBV2LogicalTypeIdBigint :: DuckDBV2LogicalTypeId
pattern DuckDBV2LogicalTypeIdBigint = DuckDBV2LogicalTypeId (#{const DUCKDB_V2_LOGICAL_TYPE_ID_BIGINT})

-- | 32-bit days since epoch.
pattern DuckDBV2LogicalTypeIdDate :: DuckDBV2LogicalTypeId
pattern DuckDBV2LogicalTypeIdDate = DuckDBV2LogicalTypeId (#{const DUCKDB_V2_LOGICAL_TYPE_ID_DATE})

-- | 64-bit microseconds since midnight.
pattern DuckDBV2LogicalTypeIdTime :: DuckDBV2LogicalTypeId
pattern DuckDBV2LogicalTypeIdTime = DuckDBV2LogicalTypeId (#{const DUCKDB_V2_LOGICAL_TYPE_ID_TIME})

-- | 64-bit seconds since epoch.
pattern DuckDBV2LogicalTypeIdTimestampSec :: DuckDBV2LogicalTypeId
pattern DuckDBV2LogicalTypeIdTimestampSec = DuckDBV2LogicalTypeId (#{const DUCKDB_V2_LOGICAL_TYPE_ID_TIMESTAMP_SEC})

-- | 64-bit milliseconds since epoch.
pattern DuckDBV2LogicalTypeIdTimestampMs :: DuckDBV2LogicalTypeId
pattern DuckDBV2LogicalTypeIdTimestampMs = DuckDBV2LogicalTypeId (#{const DUCKDB_V2_LOGICAL_TYPE_ID_TIMESTAMP_MS})

-- | 64-bit seconds since epoch.
pattern DuckDBV2LogicalTypeIdTimestamp :: DuckDBV2LogicalTypeId
pattern DuckDBV2LogicalTypeIdTimestamp = DuckDBV2LogicalTypeId (#{const DUCKDB_V2_LOGICAL_TYPE_ID_TIMESTAMP})

-- | 64-bit nanoseconds since epoch.
pattern DuckDBV2LogicalTypeIdTimestampNs :: DuckDBV2LogicalTypeId
pattern DuckDBV2LogicalTypeIdTimestampNs = DuckDBV2LogicalTypeId (#{const DUCKDB_V2_LOGICAL_TYPE_ID_TIMESTAMP_NS})

-- | Decimal with width and scale parameters.
pattern DuckDBV2LogicalTypeIdDecimal :: DuckDBV2LogicalTypeId
pattern DuckDBV2LogicalTypeIdDecimal = DuckDBV2LogicalTypeId (#{const DUCKDB_V2_LOGICAL_TYPE_ID_DECIMAL})

-- | The @DUCKDB_V2_LOGICAL_TYPE_ID_FLOAT@ constant.
pattern DuckDBV2LogicalTypeIdFloat :: DuckDBV2LogicalTypeId
pattern DuckDBV2LogicalTypeIdFloat = DuckDBV2LogicalTypeId (#{const DUCKDB_V2_LOGICAL_TYPE_ID_FLOAT})

-- | The @DUCKDB_V2_LOGICAL_TYPE_ID_DOUBLE@ constant.
pattern DuckDBV2LogicalTypeIdDouble :: DuckDBV2LogicalTypeId
pattern DuckDBV2LogicalTypeIdDouble = DuckDBV2LogicalTypeId (#{const DUCKDB_V2_LOGICAL_TYPE_ID_DOUBLE})

-- | The @DUCKDB_V2_LOGICAL_TYPE_ID_VARCHAR@ constant.
pattern DuckDBV2LogicalTypeIdVarchar :: DuckDBV2LogicalTypeId
pattern DuckDBV2LogicalTypeIdVarchar = DuckDBV2LogicalTypeId (#{const DUCKDB_V2_LOGICAL_TYPE_ID_VARCHAR})

-- | The @DUCKDB_V2_LOGICAL_TYPE_ID_BLOB@ constant.
pattern DuckDBV2LogicalTypeIdBlob :: DuckDBV2LogicalTypeId
pattern DuckDBV2LogicalTypeIdBlob = DuckDBV2LogicalTypeId (#{const DUCKDB_V2_LOGICAL_TYPE_ID_BLOB})

-- | The @DUCKDB_V2_LOGICAL_TYPE_ID_INTERVAL@ constant.
pattern DuckDBV2LogicalTypeIdInterval :: DuckDBV2LogicalTypeId
pattern DuckDBV2LogicalTypeIdInterval = DuckDBV2LogicalTypeId (#{const DUCKDB_V2_LOGICAL_TYPE_ID_INTERVAL})

-- | The @DUCKDB_V2_LOGICAL_TYPE_ID_UTINYINT@ constant.
pattern DuckDBV2LogicalTypeIdUtinyint :: DuckDBV2LogicalTypeId
pattern DuckDBV2LogicalTypeIdUtinyint = DuckDBV2LogicalTypeId (#{const DUCKDB_V2_LOGICAL_TYPE_ID_UTINYINT})

-- | The @DUCKDB_V2_LOGICAL_TYPE_ID_USMALLINT@ constant.
pattern DuckDBV2LogicalTypeIdUsmallint :: DuckDBV2LogicalTypeId
pattern DuckDBV2LogicalTypeIdUsmallint = DuckDBV2LogicalTypeId (#{const DUCKDB_V2_LOGICAL_TYPE_ID_USMALLINT})

-- | The @DUCKDB_V2_LOGICAL_TYPE_ID_UINTEGER@ constant.
pattern DuckDBV2LogicalTypeIdUinteger :: DuckDBV2LogicalTypeId
pattern DuckDBV2LogicalTypeIdUinteger = DuckDBV2LogicalTypeId (#{const DUCKDB_V2_LOGICAL_TYPE_ID_UINTEGER})

-- | The @DUCKDB_V2_LOGICAL_TYPE_ID_UBIGINT@ constant.
pattern DuckDBV2LogicalTypeIdUbigint :: DuckDBV2LogicalTypeId
pattern DuckDBV2LogicalTypeIdUbigint = DuckDBV2LogicalTypeId (#{const DUCKDB_V2_LOGICAL_TYPE_ID_UBIGINT})

-- | 64-bit microseconds since epoch, timezone-aware.
pattern DuckDBV2LogicalTypeIdTimestampTz :: DuckDBV2LogicalTypeId
pattern DuckDBV2LogicalTypeIdTimestampTz = DuckDBV2LogicalTypeId (#{const DUCKDB_V2_LOGICAL_TYPE_ID_TIMESTAMP_TZ})

-- | 64-bit nanoseconds since epoch, timezone-aware.
pattern DuckDBV2LogicalTypeIdTimestampTzNs :: DuckDBV2LogicalTypeId
pattern DuckDBV2LogicalTypeIdTimestampTzNs = DuckDBV2LogicalTypeId (#{const DUCKDB_V2_LOGICAL_TYPE_ID_TIMESTAMP_TZ_NS})

-- | 64-bit microseconds since midnight + 32-bit offset.
pattern DuckDBV2LogicalTypeIdTimeTz :: DuckDBV2LogicalTypeId
pattern DuckDBV2LogicalTypeIdTimeTz = DuckDBV2LogicalTypeId (#{const DUCKDB_V2_LOGICAL_TYPE_ID_TIME_TZ})

-- | 64-bit nanoseconds since midnight.
pattern DuckDBV2LogicalTypeIdTimeNs :: DuckDBV2LogicalTypeId
pattern DuckDBV2LogicalTypeIdTimeNs = DuckDBV2LogicalTypeId (#{const DUCKDB_V2_LOGICAL_TYPE_ID_TIME_NS})

-- | The @DUCKDB_V2_LOGICAL_TYPE_ID_BIT@ constant.
pattern DuckDBV2LogicalTypeIdBit :: DuckDBV2LogicalTypeId
pattern DuckDBV2LogicalTypeIdBit = DuckDBV2LogicalTypeId (#{const DUCKDB_V2_LOGICAL_TYPE_ID_BIT})

-- | Arbitrary-precision integer (VARINT-encoded).
pattern DuckDBV2LogicalTypeIdBignum :: DuckDBV2LogicalTypeId
pattern DuckDBV2LogicalTypeIdBignum = DuckDBV2LogicalTypeId (#{const DUCKDB_V2_LOGICAL_TYPE_ID_BIGNUM})

-- | The @DUCKDB_V2_LOGICAL_TYPE_ID_UHUGEINT@ constant.
pattern DuckDBV2LogicalTypeIdUhugeint :: DuckDBV2LogicalTypeId
pattern DuckDBV2LogicalTypeIdUhugeint = DuckDBV2LogicalTypeId (#{const DUCKDB_V2_LOGICAL_TYPE_ID_UHUGEINT})

-- | The @DUCKDB_V2_LOGICAL_TYPE_ID_HUGEINT@ constant.
pattern DuckDBV2LogicalTypeIdHugeint :: DuckDBV2LogicalTypeId
pattern DuckDBV2LogicalTypeIdHugeint = DuckDBV2LogicalTypeId (#{const DUCKDB_V2_LOGICAL_TYPE_ID_HUGEINT})

-- | The @DUCKDB_V2_LOGICAL_TYPE_ID_UUID@ constant.
pattern DuckDBV2LogicalTypeIdUuid :: DuckDBV2LogicalTypeId
pattern DuckDBV2LogicalTypeIdUuid = DuckDBV2LogicalTypeId (#{const DUCKDB_V2_LOGICAL_TYPE_ID_UUID})

-- | Geometry (spatial extension).
pattern DuckDBV2LogicalTypeIdGeometry :: DuckDBV2LogicalTypeId
pattern DuckDBV2LogicalTypeIdGeometry = DuckDBV2LogicalTypeId (#{const DUCKDB_V2_LOGICAL_TYPE_ID_GEOMETRY})

-- | The @DUCKDB_V2_LOGICAL_TYPE_ID_STRUCT@ constant.
pattern DuckDBV2LogicalTypeIdStruct :: DuckDBV2LogicalTypeId
pattern DuckDBV2LogicalTypeIdStruct = DuckDBV2LogicalTypeId (#{const DUCKDB_V2_LOGICAL_TYPE_ID_STRUCT})

-- | The @DUCKDB_V2_LOGICAL_TYPE_ID_LIST@ constant.
pattern DuckDBV2LogicalTypeIdList :: DuckDBV2LogicalTypeId
pattern DuckDBV2LogicalTypeIdList = DuckDBV2LogicalTypeId (#{const DUCKDB_V2_LOGICAL_TYPE_ID_LIST})

-- | The @DUCKDB_V2_LOGICAL_TYPE_ID_MAP@ constant.
pattern DuckDBV2LogicalTypeIdMap :: DuckDBV2LogicalTypeId
pattern DuckDBV2LogicalTypeIdMap = DuckDBV2LogicalTypeId (#{const DUCKDB_V2_LOGICAL_TYPE_ID_MAP})

-- | The @DUCKDB_V2_LOGICAL_TYPE_ID_ENUM@ constant.
pattern DuckDBV2LogicalTypeIdEnum :: DuckDBV2LogicalTypeId
pattern DuckDBV2LogicalTypeIdEnum = DuckDBV2LogicalTypeId (#{const DUCKDB_V2_LOGICAL_TYPE_ID_ENUM})

-- | The @DUCKDB_V2_LOGICAL_TYPE_ID_UNION@ constant.
pattern DuckDBV2LogicalTypeIdUnion :: DuckDBV2LogicalTypeId
pattern DuckDBV2LogicalTypeIdUnion = DuckDBV2LogicalTypeId (#{const DUCKDB_V2_LOGICAL_TYPE_ID_UNION})

-- | The @DUCKDB_V2_LOGICAL_TYPE_ID_ARRAY@ constant.
pattern DuckDBV2LogicalTypeIdArray :: DuckDBV2LogicalTypeId
pattern DuckDBV2LogicalTypeIdArray = DuckDBV2LogicalTypeId (#{const DUCKDB_V2_LOGICAL_TYPE_ID_ARRAY})

-- | The @DUCKDB_V2_LOGICAL_TYPE_ID_VARIANT@ constant.
pattern DuckDBV2LogicalTypeIdVariant :: DuckDBV2LogicalTypeId
pattern DuckDBV2LogicalTypeIdVariant = DuckDBV2LogicalTypeId (#{const DUCKDB_V2_LOGICAL_TYPE_ID_VARIANT})

-- | Unnamed struct; shares the physical representation of STRUCT.
pattern DuckDBV2LogicalTypeIdTuple :: DuckDBV2LogicalTypeId
pattern DuckDBV2LogicalTypeIdTuple = DuckDBV2LogicalTypeId (#{const DUCKDB_V2_LOGICAL_TYPE_ID_TUPLE})

-- | The @DUCKDB_V2_LOGICAL_TYPE_ID_MAX_ENUM@ constant.
pattern DuckDBV2LogicalTypeIdMaxEnum :: DuckDBV2LogicalTypeId
pattern DuckDBV2LogicalTypeIdMaxEnum = DuckDBV2LogicalTypeId (#{const DUCKDB_V2_LOGICAL_TYPE_ID_MAX_ENUM})

-- | SQL statement type of a parsed statement or an executed query.
newtype DuckDBV2StatementType = DuckDBV2StatementType (#{type DUCKDB_V2_STATEMENT_TYPE})
    deriving (Eq, Ord, Show, Read, Storable)

-- | The @DUCKDB_V2_STATEMENT_TYPE_INVALID@ constant.
pattern DuckDBV2StatementTypeInvalid :: DuckDBV2StatementType
pattern DuckDBV2StatementTypeInvalid = DuckDBV2StatementType (#{const DUCKDB_V2_STATEMENT_TYPE_INVALID})

-- | The @DUCKDB_V2_STATEMENT_TYPE_SELECT@ constant.
pattern DuckDBV2StatementTypeSelect :: DuckDBV2StatementType
pattern DuckDBV2StatementTypeSelect = DuckDBV2StatementType (#{const DUCKDB_V2_STATEMENT_TYPE_SELECT})

-- | The @DUCKDB_V2_STATEMENT_TYPE_INSERT@ constant.
pattern DuckDBV2StatementTypeInsert :: DuckDBV2StatementType
pattern DuckDBV2StatementTypeInsert = DuckDBV2StatementType (#{const DUCKDB_V2_STATEMENT_TYPE_INSERT})

-- | The @DUCKDB_V2_STATEMENT_TYPE_UPDATE@ constant.
pattern DuckDBV2StatementTypeUpdate :: DuckDBV2StatementType
pattern DuckDBV2StatementTypeUpdate = DuckDBV2StatementType (#{const DUCKDB_V2_STATEMENT_TYPE_UPDATE})

-- | The @DUCKDB_V2_STATEMENT_TYPE_CREATE@ constant.
pattern DuckDBV2StatementTypeCreate :: DuckDBV2StatementType
pattern DuckDBV2StatementTypeCreate = DuckDBV2StatementType (#{const DUCKDB_V2_STATEMENT_TYPE_CREATE})

-- | The @DUCKDB_V2_STATEMENT_TYPE_DELETE@ constant.
pattern DuckDBV2StatementTypeDelete :: DuckDBV2StatementType
pattern DuckDBV2StatementTypeDelete = DuckDBV2StatementType (#{const DUCKDB_V2_STATEMENT_TYPE_DELETE})

-- | The @DUCKDB_V2_STATEMENT_TYPE_PREPARE@ constant.
pattern DuckDBV2StatementTypePrepare :: DuckDBV2StatementType
pattern DuckDBV2StatementTypePrepare = DuckDBV2StatementType (#{const DUCKDB_V2_STATEMENT_TYPE_PREPARE})

-- | The @DUCKDB_V2_STATEMENT_TYPE_EXECUTE@ constant.
pattern DuckDBV2StatementTypeExecute :: DuckDBV2StatementType
pattern DuckDBV2StatementTypeExecute = DuckDBV2StatementType (#{const DUCKDB_V2_STATEMENT_TYPE_EXECUTE})

-- | The @DUCKDB_V2_STATEMENT_TYPE_ALTER@ constant.
pattern DuckDBV2StatementTypeAlter :: DuckDBV2StatementType
pattern DuckDBV2StatementTypeAlter = DuckDBV2StatementType (#{const DUCKDB_V2_STATEMENT_TYPE_ALTER})

-- | The @DUCKDB_V2_STATEMENT_TYPE_TRANSACTION@ constant.
pattern DuckDBV2StatementTypeTransaction :: DuckDBV2StatementType
pattern DuckDBV2StatementTypeTransaction = DuckDBV2StatementType (#{const DUCKDB_V2_STATEMENT_TYPE_TRANSACTION})

-- | The @DUCKDB_V2_STATEMENT_TYPE_COPY@ constant.
pattern DuckDBV2StatementTypeCopy :: DuckDBV2StatementType
pattern DuckDBV2StatementTypeCopy = DuckDBV2StatementType (#{const DUCKDB_V2_STATEMENT_TYPE_COPY})

-- | The @DUCKDB_V2_STATEMENT_TYPE_ANALYZE@ constant.
pattern DuckDBV2StatementTypeAnalyze :: DuckDBV2StatementType
pattern DuckDBV2StatementTypeAnalyze = DuckDBV2StatementType (#{const DUCKDB_V2_STATEMENT_TYPE_ANALYZE})

-- | The @DUCKDB_V2_STATEMENT_TYPE_VARIABLE_SET@ constant.
pattern DuckDBV2StatementTypeVariableSet :: DuckDBV2StatementType
pattern DuckDBV2StatementTypeVariableSet = DuckDBV2StatementType (#{const DUCKDB_V2_STATEMENT_TYPE_VARIABLE_SET})

-- | The @DUCKDB_V2_STATEMENT_TYPE_CREATE_FUNC@ constant.
pattern DuckDBV2StatementTypeCreateFunc :: DuckDBV2StatementType
pattern DuckDBV2StatementTypeCreateFunc = DuckDBV2StatementType (#{const DUCKDB_V2_STATEMENT_TYPE_CREATE_FUNC})

-- | The @DUCKDB_V2_STATEMENT_TYPE_EXPLAIN@ constant.
pattern DuckDBV2StatementTypeExplain :: DuckDBV2StatementType
pattern DuckDBV2StatementTypeExplain = DuckDBV2StatementType (#{const DUCKDB_V2_STATEMENT_TYPE_EXPLAIN})

-- | The @DUCKDB_V2_STATEMENT_TYPE_DROP@ constant.
pattern DuckDBV2StatementTypeDrop :: DuckDBV2StatementType
pattern DuckDBV2StatementTypeDrop = DuckDBV2StatementType (#{const DUCKDB_V2_STATEMENT_TYPE_DROP})

-- | The @DUCKDB_V2_STATEMENT_TYPE_EXPORT@ constant.
pattern DuckDBV2StatementTypeExport :: DuckDBV2StatementType
pattern DuckDBV2StatementTypeExport = DuckDBV2StatementType (#{const DUCKDB_V2_STATEMENT_TYPE_EXPORT})

-- | The @DUCKDB_V2_STATEMENT_TYPE_PRAGMA@ constant.
pattern DuckDBV2StatementTypePragma :: DuckDBV2StatementType
pattern DuckDBV2StatementTypePragma = DuckDBV2StatementType (#{const DUCKDB_V2_STATEMENT_TYPE_PRAGMA})

-- | The @DUCKDB_V2_STATEMENT_TYPE_VACUUM@ constant.
pattern DuckDBV2StatementTypeVacuum :: DuckDBV2StatementType
pattern DuckDBV2StatementTypeVacuum = DuckDBV2StatementType (#{const DUCKDB_V2_STATEMENT_TYPE_VACUUM})

-- | The @DUCKDB_V2_STATEMENT_TYPE_CALL@ constant.
pattern DuckDBV2StatementTypeCall :: DuckDBV2StatementType
pattern DuckDBV2StatementTypeCall = DuckDBV2StatementType (#{const DUCKDB_V2_STATEMENT_TYPE_CALL})

-- | The @DUCKDB_V2_STATEMENT_TYPE_SET@ constant.
pattern DuckDBV2StatementTypeSet :: DuckDBV2StatementType
pattern DuckDBV2StatementTypeSet = DuckDBV2StatementType (#{const DUCKDB_V2_STATEMENT_TYPE_SET})

-- | The @DUCKDB_V2_STATEMENT_TYPE_LOAD@ constant.
pattern DuckDBV2StatementTypeLoad :: DuckDBV2StatementType
pattern DuckDBV2StatementTypeLoad = DuckDBV2StatementType (#{const DUCKDB_V2_STATEMENT_TYPE_LOAD})

-- | The @DUCKDB_V2_STATEMENT_TYPE_RELATION@ constant.
pattern DuckDBV2StatementTypeRelation :: DuckDBV2StatementType
pattern DuckDBV2StatementTypeRelation = DuckDBV2StatementType (#{const DUCKDB_V2_STATEMENT_TYPE_RELATION})

-- | The @DUCKDB_V2_STATEMENT_TYPE_EXTENSION@ constant.
pattern DuckDBV2StatementTypeExtension :: DuckDBV2StatementType
pattern DuckDBV2StatementTypeExtension = DuckDBV2StatementType (#{const DUCKDB_V2_STATEMENT_TYPE_EXTENSION})

-- | The @DUCKDB_V2_STATEMENT_TYPE_LOGICAL_PLAN@ constant.
pattern DuckDBV2StatementTypeLogicalPlan :: DuckDBV2StatementType
pattern DuckDBV2StatementTypeLogicalPlan = DuckDBV2StatementType (#{const DUCKDB_V2_STATEMENT_TYPE_LOGICAL_PLAN})

-- | The @DUCKDB_V2_STATEMENT_TYPE_ATTACH@ constant.
pattern DuckDBV2StatementTypeAttach :: DuckDBV2StatementType
pattern DuckDBV2StatementTypeAttach = DuckDBV2StatementType (#{const DUCKDB_V2_STATEMENT_TYPE_ATTACH})

-- | The @DUCKDB_V2_STATEMENT_TYPE_DETACH@ constant.
pattern DuckDBV2StatementTypeDetach :: DuckDBV2StatementType
pattern DuckDBV2StatementTypeDetach = DuckDBV2StatementType (#{const DUCKDB_V2_STATEMENT_TYPE_DETACH})

-- | The @DUCKDB_V2_STATEMENT_TYPE_MULTI@ constant.
pattern DuckDBV2StatementTypeMulti :: DuckDBV2StatementType
pattern DuckDBV2StatementTypeMulti = DuckDBV2StatementType (#{const DUCKDB_V2_STATEMENT_TYPE_MULTI})

-- | The @DUCKDB_V2_STATEMENT_TYPE_COPY_DATABASE@ constant.
pattern DuckDBV2StatementTypeCopyDatabase :: DuckDBV2StatementType
pattern DuckDBV2StatementTypeCopyDatabase = DuckDBV2StatementType (#{const DUCKDB_V2_STATEMENT_TYPE_COPY_DATABASE})

-- | The @DUCKDB_V2_STATEMENT_TYPE_UPDATE_EXTENSIONS@ constant.
pattern DuckDBV2StatementTypeUpdateExtensions :: DuckDBV2StatementType
pattern DuckDBV2StatementTypeUpdateExtensions = DuckDBV2StatementType (#{const DUCKDB_V2_STATEMENT_TYPE_UPDATE_EXTENSIONS})

-- | The @DUCKDB_V2_STATEMENT_TYPE_MERGE_INTO@ constant.
pattern DuckDBV2StatementTypeMergeInto :: DuckDBV2StatementType
pattern DuckDBV2StatementTypeMergeInto = DuckDBV2StatementType (#{const DUCKDB_V2_STATEMENT_TYPE_MERGE_INTO})

-- | The @DUCKDB_V2_STATEMENT_TYPE_CONNECT@ constant.
pattern DuckDBV2StatementTypeConnect :: DuckDBV2StatementType
pattern DuckDBV2StatementTypeConnect = DuckDBV2StatementType (#{const DUCKDB_V2_STATEMENT_TYPE_CONNECT})

-- | The @DUCKDB_V2_STATEMENT_TYPE_DISCONNECT@ constant.
pattern DuckDBV2StatementTypeDisconnect :: DuckDBV2StatementType
pattern DuckDBV2StatementTypeDisconnect = DuckDBV2StatementType (#{const DUCKDB_V2_STATEMENT_TYPE_DISCONNECT})

-- | The @DUCKDB_V2_STATEMENT_TYPE_EXTERNAL_RESOURCE@ constant.
pattern DuckDBV2StatementTypeExternalResource :: DuckDBV2StatementType
pattern DuckDBV2StatementTypeExternalResource = DuckDBV2StatementType (#{const DUCKDB_V2_STATEMENT_TYPE_EXTERNAL_RESOURCE})

-- | The @DUCKDB_V2_STATEMENT_TYPE_PASSTHROUGH@ constant.
pattern DuckDBV2StatementTypePassthrough :: DuckDBV2StatementType
pattern DuckDBV2StatementTypePassthrough = DuckDBV2StatementType (#{const DUCKDB_V2_STATEMENT_TYPE_PASSTHROUGH})

-- | The @DUCKDB_V2_STATEMENT_TYPE_MAX_ENUM@ constant.
pattern DuckDBV2StatementTypeMaxEnum :: DuckDBV2StatementType
pattern DuckDBV2StatementTypeMaxEnum = DuckDBV2StatementType (#{const DUCKDB_V2_STATEMENT_TYPE_MAX_ENUM})

{- | Shape of a query result. QUERY_RESULT carries rows and columns; CHANGED_ROWS carries an affected row count, as an
INSERT, UPDATE, or DELETE without RETURNING produces; NOTHING covers DDL and other statements with no row output.
-}
newtype DuckDBV2ResultType = DuckDBV2ResultType (#{type DUCKDB_V2_RESULT_TYPE})
    deriving (Eq, Ord, Show, Read, Storable)

-- | The @DUCKDB_V2_RESULT_TYPE_QUERY_RESULT@ constant.
pattern DuckDBV2ResultTypeQueryResult :: DuckDBV2ResultType
pattern DuckDBV2ResultTypeQueryResult = DuckDBV2ResultType (#{const DUCKDB_V2_RESULT_TYPE_QUERY_RESULT})

-- | The @DUCKDB_V2_RESULT_TYPE_CHANGED_ROWS@ constant.
pattern DuckDBV2ResultTypeChangedRows :: DuckDBV2ResultType
pattern DuckDBV2ResultTypeChangedRows = DuckDBV2ResultType (#{const DUCKDB_V2_RESULT_TYPE_CHANGED_ROWS})

-- | The @DUCKDB_V2_RESULT_TYPE_NOTHING@ constant.
pattern DuckDBV2ResultTypeNothing :: DuckDBV2ResultType
pattern DuckDBV2ResultTypeNothing = DuckDBV2ResultType (#{const DUCKDB_V2_RESULT_TYPE_NOTHING})

-- | The @DUCKDB_V2_RESULT_TYPE_MAX_ENUM@ constant.
pattern DuckDBV2ResultTypeMaxEnum :: DuckDBV2ResultType
pattern DuckDBV2ResultTypeMaxEnum = DuckDBV2ResultType (#{const DUCKDB_V2_RESULT_TYPE_MAX_ENUM})

{- | Outcome of a result_step call. WAITING is the 0-value, so a zero-initialized out-param reads as "no work product yet"
rather than CHUNK, the same convention VECTOR_TYPE_OTHER follows. The four states are the ones a consumer acts on;
they are deliberately not a projection of any internal enum.
-}
newtype DuckDBV2ResultStepStatus = DuckDBV2ResultStepStatus (#{type DUCKDB_V2_RESULT_STEP_STATUS})
    deriving (Eq, Ord, Show, Read, Storable)

-- | No chunk yet; step again, or block in result_wait.
pattern DuckDBV2ResultStepStatusWaiting :: DuckDBV2ResultStepStatus
pattern DuckDBV2ResultStepStatusWaiting = DuckDBV2ResultStepStatus (#{const DUCKDB_V2_RESULT_STEP_STATUS_WAITING})

-- | A caller-owned chunk was written to *out_chunk, or an array to *out_array.
pattern DuckDBV2ResultStepStatusChunk :: DuckDBV2ResultStepStatus
pattern DuckDBV2ResultStepStatusChunk = DuckDBV2ResultStepStatus (#{const DUCKDB_V2_RESULT_STEP_STATUS_CHUNK})

-- | Stream exhausted. Sticky.
pattern DuckDBV2ResultStepStatusFinished :: DuckDBV2ResultStepStatus
pattern DuckDBV2ResultStepStatusFinished = DuckDBV2ResultStepStatus (#{const DUCKDB_V2_RESULT_STEP_STATUS_FINISHED})

-- | Query was interrupted. Sticky.
pattern DuckDBV2ResultStepStatusCancelled :: DuckDBV2ResultStepStatus
pattern DuckDBV2ResultStepStatusCancelled = DuckDBV2ResultStepStatus (#{const DUCKDB_V2_RESULT_STEP_STATUS_CANCELLED})

-- | The @DUCKDB_V2_RESULT_STEP_STATUS_MAX_ENUM@ constant.
pattern DuckDBV2ResultStepStatusMaxEnum :: DuckDBV2ResultStepStatus
pattern DuckDBV2ResultStepStatusMaxEnum = DuckDBV2ResultStepStatus (#{const DUCKDB_V2_RESULT_STEP_STATUS_MAX_ENUM})

{- | Whether a table function's scan is partitioned by a given set of columns, and how. Reported by
@duckdb_v2_table_function_set_partitioning_callback()@ via
@duckdb_v2_table_function_partitioning_set_partition_info()@.
-}
newtype DuckDBV2TablePartitionInfo = DuckDBV2TablePartitionInfo (#{type DUCKDB_V2_TABLE_PARTITION_INFO})
    deriving (Eq, Ord, Show, Read, Storable)

-- | The scan is not known to be partitioned by the requested columns.
pattern DuckDBV2TablePartitionInfoNotPartitioned :: DuckDBV2TablePartitionInfo
pattern DuckDBV2TablePartitionInfoNotPartitioned = DuckDBV2TablePartitionInfo (#{const DUCKDB_V2_TABLE_PARTITION_INFO_NOT_PARTITIONED})

-- | Each partition the scan produces carries exactly one distinct value for the requested columns.
pattern DuckDBV2TablePartitionInfoSingleValuePartitions :: DuckDBV2TablePartitionInfo
pattern DuckDBV2TablePartitionInfoSingleValuePartitions = DuckDBV2TablePartitionInfo (#{const DUCKDB_V2_TABLE_PARTITION_INFO_SINGLE_VALUE_PARTITIONS})

-- | The partitions the scan produces overlap only at their boundaries.
pattern DuckDBV2TablePartitionInfoOverlappingPartitions :: DuckDBV2TablePartitionInfo
pattern DuckDBV2TablePartitionInfoOverlappingPartitions = DuckDBV2TablePartitionInfo (#{const DUCKDB_V2_TABLE_PARTITION_INFO_OVERLAPPING_PARTITIONS})

-- | The partitions the scan produces are disjoint ranges.
pattern DuckDBV2TablePartitionInfoDisjointPartitions :: DuckDBV2TablePartitionInfo
pattern DuckDBV2TablePartitionInfoDisjointPartitions = DuckDBV2TablePartitionInfo (#{const DUCKDB_V2_TABLE_PARTITION_INFO_DISJOINT_PARTITIONS})

-- | The @DUCKDB_V2_TABLE_PARTITION_INFO_MAX_ENUM@ constant.
pattern DuckDBV2TablePartitionInfoMaxEnum :: DuckDBV2TablePartitionInfo
pattern DuckDBV2TablePartitionInfoMaxEnum = DuckDBV2TablePartitionInfo (#{const DUCKDB_V2_TABLE_PARTITION_INFO_MAX_ENUM})

{- | Receives text produced by DuckDB

Invoked exactly once per producing call, with the complete text in a single view.

The view is borrowed for the duration of the call only. Copy what you need before returning, and do not retain @text@
or @text->ptr@. The bytes are NOT guaranteed to be null-terminated.

@err@ is a live error slot, never NULL. Populate it with @duckdb_v2_error_info_set_code()@ /
@duckdb_v2_error_info_set_text()@ to signal failure to DuckDB. Do not destroy it yourself.

The sink runs inside DuckDB's call frame: it must not throw or unwind across the boundary, and it must not re-enter
the API on the handle being operated on.
-}
type DuckDBV2TextSinkFn = Ptr DuckDBV2Str -> Ptr () -> Ptr DuckDBV2ErrorInfoHandle -> IO ()

{- | Compares two caller-defined resources for equality.

Invoked when DuckDB needs to compare two opaque handles. The callback is responsible for interpreting the pointers
and comparing the underlying resources. Return true if they are equal, false otherwise.

The callback runs inside DuckDB's call frame: it must not throw or unwind across the boundary, and it must not
re-enter the API on the handle being operated on.
-}
type DuckDBV2OpaqueEqualsFn = Ptr () -> Ptr () -> IO CBool

{- | Destroys a caller-defined resource.

Invoked when DuckDB needs to destroy an opaque handle. The callback is responsible for interpreting the pointer and
freeing the underlying resource.

The callback runs inside DuckDB's call frame: it must not throw or unwind across the boundary, and it must not
re-enter the API on the handle being operated on.
-}
type DuckDBV2OpaqueDestroyFn = Ptr () -> IO ()

-- | Callback for @duckdb_v2_cast_function_exec_callback_fn@.
type DuckDBV2CastFunctionExecCallbackFn = DuckDBV2CastFunctionExecInfoHandle -> DuckDBV2ContextHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()

{- | Hands back the function-pointer table for the API version the extension was built against, or NULL if this DuckDB
cannot serve it.

The extension calls it exactly once, first, through DUCKDB_EXTENSION_API_INIT; nothing else in the API may be called
before it succeeds. A NULL return means the load has already failed and DuckDB has recorded why, so the entrypoint
must return without touching the error slot. Statically linked extensions resolve DuckDB's symbols at link time and
never call it, so for those the field is NULL.
-}
type DuckDBV2ExtensionGetApiFn = DuckDBV2ExtensionHandle -> Ptr CChar -> IO (Ptr ())

-- | Callback for @duckdb_v2_aggregate_function_bind_callback_fn@.
type DuckDBV2AggregateFunctionBindCallbackFn = DuckDBV2FunctionBindInfoHandle -> DuckDBV2AggregateFunctionBindInfoHandle -> DuckDBV2ContextHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()

-- | Callback for @duckdb_v2_aggregate_function_size_callback_fn@.
type DuckDBV2AggregateFunctionSizeCallbackFn = DuckDBV2AggregateFunctionSizeInfoHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()

-- | Callback for @duckdb_v2_aggregate_function_init_callback_fn@.
type DuckDBV2AggregateFunctionInitCallbackFn = DuckDBV2AggregateFunctionInitInfoHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()

-- | Callback for @duckdb_v2_aggregate_function_update_callback_fn@.
type DuckDBV2AggregateFunctionUpdateCallbackFn = DuckDBV2AggregateFunctionUpdateInfoHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()

-- | Callback for @duckdb_v2_aggregate_function_combine_callback_fn@.
type DuckDBV2AggregateFunctionCombineCallbackFn = DuckDBV2AggregateFunctionCombineInfoHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()

-- | Callback for @duckdb_v2_aggregate_function_finalize_callback_fn@.
type DuckDBV2AggregateFunctionFinalizeCallbackFn = DuckDBV2AggregateFunctionFinalizeInfoHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()

-- | Callback for @duckdb_v2_aggregate_function_destroy_callback_fn@.
type DuckDBV2AggregateFunctionDestroyCallbackFn = DuckDBV2AggregateFunctionDestroyInfoHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()

-- | Callback for @duckdb_v2_copy_to_bind_callback_fn@.
type DuckDBV2CopyToBindCallbackFn = DuckDBV2CopyToBindInfoHandle -> DuckDBV2ContextHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()

-- | Callback for @duckdb_v2_copy_to_batch_size_callback_fn@.
type DuckDBV2CopyToBatchSizeCallbackFn = DuckDBV2CopyToBatchSizeInfoHandle -> DuckDBV2ContextHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()

-- | Callback for @duckdb_v2_copy_to_init_callback_fn@.
type DuckDBV2CopyToInitCallbackFn = DuckDBV2CopyToInitInfoHandle -> DuckDBV2ContextHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()

-- | Callback for @duckdb_v2_copy_to_batch_callback_fn@.
type DuckDBV2CopyToBatchCallbackFn = DuckDBV2CopyToBatchInfoHandle -> DuckDBV2ContextHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()

-- | Callback for @duckdb_v2_copy_to_flush_callback_fn@.
type DuckDBV2CopyToFlushCallbackFn = DuckDBV2CopyToFlushInfoHandle -> DuckDBV2ContextHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()

-- | Callback for @duckdb_v2_copy_to_finalize_callback_fn@.
type DuckDBV2CopyToFinalizeCallbackFn = DuckDBV2CopyToFinalizeInfoHandle -> DuckDBV2ContextHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()

-- | Callback for @duckdb_v2_copy_to_statistics_callback_fn@.
type DuckDBV2CopyToStatisticsCallbackFn = DuckDBV2CopyToStatisticsInfoHandle -> DuckDBV2ContextHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()

-- | Callback for @duckdb_v2_copy_from_bind_callback_fn@.
type DuckDBV2CopyFromBindCallbackFn = DuckDBV2CopyFromBindInfoHandle -> DuckDBV2ContextHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()

-- | Callback for @duckdb_v2_copy_from_init_global_callback_fn@.
type DuckDBV2CopyFromInitGlobalCallbackFn = DuckDBV2CopyFromInitGlobalInfoHandle -> DuckDBV2ContextHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()

-- | Callback for @duckdb_v2_copy_from_init_local_callback_fn@.
type DuckDBV2CopyFromInitLocalCallbackFn = DuckDBV2CopyFromInitLocalInfoHandle -> DuckDBV2ContextHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()

-- | Callback for @duckdb_v2_copy_from_exec_callback_fn@.
type DuckDBV2CopyFromExecCallbackFn = DuckDBV2CopyFromExecInfoHandle -> DuckDBV2ContextHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()

-- | Callback for @duckdb_v2_copy_from_progress_callback_fn@.
type DuckDBV2CopyFromProgressCallbackFn = DuckDBV2CopyFromProgressInfoHandle -> DuckDBV2ContextHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()

-- | Callback for @duckdb_v2_replacement_scan_callback_fn@.
type DuckDBV2ReplacementScanCallbackFn = DuckDBV2ReplacementScanInfoHandle -> DuckDBV2ContextHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()

-- | Callback for @duckdb_v2_scalar_function_bind_callback_fn@.
type DuckDBV2ScalarFunctionBindCallbackFn = DuckDBV2FunctionBindInfoHandle -> DuckDBV2ScalarFunctionBindInfoHandle -> DuckDBV2ContextHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()

-- | Callback for @duckdb_v2_scalar_function_init_callback_fn@.
type DuckDBV2ScalarFunctionInitCallbackFn = DuckDBV2ScalarFunctionInitInfoHandle -> DuckDBV2ContextHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()

-- | Callback for @duckdb_v2_scalar_function_exec_callback_fn@.
type DuckDBV2ScalarFunctionExecCallbackFn = DuckDBV2ScalarFunctionExecInfoHandle -> DuckDBV2ContextHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()

-- | Callback for @duckdb_v2_table_function_bind_callback_fn@.
type DuckDBV2TableFunctionBindCallbackFn = DuckDBV2FunctionBindInfoHandle -> DuckDBV2TableFunctionBindInfoHandle -> DuckDBV2ContextHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()

-- | Callback for @duckdb_v2_table_function_init_global_callback_fn@.
type DuckDBV2TableFunctionInitGlobalCallbackFn = DuckDBV2TableFunctionInitGlobalInfoHandle -> DuckDBV2ContextHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()

-- | Callback for @duckdb_v2_table_function_init_local_callback_fn@.
type DuckDBV2TableFunctionInitLocalCallbackFn = DuckDBV2TableFunctionInitLocalInfoHandle -> DuckDBV2ContextHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()

-- | Callback for @duckdb_v2_table_function_exec_callback_fn@.
type DuckDBV2TableFunctionExecCallbackFn = DuckDBV2TableFunctionExecInfoHandle -> DuckDBV2ContextHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()

-- | Callback for @duckdb_v2_table_function_progress_callback_fn@.
type DuckDBV2TableFunctionProgressCallbackFn = DuckDBV2TableFunctionProgressInfoHandle -> DuckDBV2ContextHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()

-- | Callback for @duckdb_v2_table_function_filter_pushdown_callback_fn@.
type DuckDBV2TableFunctionFilterPushdownCallbackFn = DuckDBV2TableFunctionFilterPushdownInfoHandle -> DuckDBV2ContextHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()

-- | Callback for @duckdb_v2_table_function_partition_data_callback_fn@.
type DuckDBV2TableFunctionPartitionDataCallbackFn = DuckDBV2TableFunctionPartitionDataInfoHandle -> DuckDBV2ContextHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()

-- | Callback for @duckdb_v2_table_function_partitioning_callback_fn@.
type DuckDBV2TableFunctionPartitioningCallbackFn = DuckDBV2TableFunctionPartitioningInfoHandle -> DuckDBV2ContextHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()

-- | Callback for @duckdb_v2_table_function_claim_batch_callback_fn@.
type DuckDBV2TableFunctionClaimBatchCallbackFn = DuckDBV2TableFunctionClaimBatchInfoHandle -> DuckDBV2ContextHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()

-- | The @ArrowSchema.release@ callback.
type DuckDBV2ArrowSchemaReleaseFn = Ptr DuckDBV2ArrowSchema -> IO ()

-- | The @ArrowArray.release@ callback.
type DuckDBV2ArrowArrayReleaseFn = Ptr DuckDBV2ArrowArray -> IO ()

-- | The @ArrowArrayStream.get_schema@ callback.
type DuckDBV2ArrowArrayStreamGetSchemaFn = Ptr DuckDBV2ArrowArrayStream -> Ptr DuckDBV2ArrowSchema -> IO CInt

-- | The @ArrowArrayStream.get_next@ callback.
type DuckDBV2ArrowArrayStreamGetNextFn = Ptr DuckDBV2ArrowArrayStream -> Ptr DuckDBV2ArrowArray -> IO CInt

-- | The @ArrowArrayStream.get_last_error@ callback.
type DuckDBV2ArrowArrayStreamGetLastErrorFn = Ptr DuckDBV2ArrowArrayStream -> IO (Ptr CChar)

-- | The @ArrowArrayStream.release@ callback.
type DuckDBV2ArrowArrayStreamReleaseFn = Ptr DuckDBV2ArrowArrayStream -> IO ()

-- | Maximum inline byte count in @duckdb_v2_bytes@.
duckdbV2BytesInlineLength :: Word32
duckdbV2BytesInlineLength = #{const DUCKDB_V2_BYTES_INLINE_LENGTH}

-- | The @DUCKDB_V2_API_VERSION_MAJOR@ constant from the pinned header.
duckdbV2ApiVersionMajor :: Word32
duckdbV2ApiVersionMajor = #{const DUCKDB_V2_API_VERSION_MAJOR}

-- | The @DUCKDB_V2_API_VERSION_MINOR@ constant from the pinned header.
duckdbV2ApiVersionMinor :: Word32
duckdbV2ApiVersionMinor = #{const DUCKDB_V2_API_VERSION_MINOR}

-- | The @DUCKDB_V2_API_VERSION_PATCH@ constant from the pinned header.
duckdbV2ApiVersionPatch :: Word32
duckdbV2ApiVersionPatch = #{const DUCKDB_V2_API_VERSION_PATCH}

-- | The @ARROW_FLAG_DICTIONARY_ORDERED@ constant from the pinned header.
duckdbV2ArrowFlagDictionaryOrdered :: Word32
duckdbV2ArrowFlagDictionaryOrdered = #{const ARROW_FLAG_DICTIONARY_ORDERED}

-- | The @ARROW_FLAG_NULLABLE@ constant from the pinned header.
duckdbV2ArrowFlagNullable :: Word32
duckdbV2ArrowFlagNullable = #{const ARROW_FLAG_NULLABLE}

-- | The @ARROW_FLAG_MAP_KEYS_SORTED@ constant from the pinned header.
duckdbV2ArrowFlagMapKeysSorted :: Word32
duckdbV2ArrowFlagMapKeysSorted = #{const ARROW_FLAG_MAP_KEYS_SORTED}

{- | The @ArrowSchema@ structure.

Uses the shared @ArrowSchema@ storage and constructors.
-}
type DuckDBV2ArrowSchema = ArrowSchema

{- | The @ArrowArray@ structure.

Uses the shared @ArrowArray@ storage and constructors.
-}
type DuckDBV2ArrowArray = ArrowArray

{- | The @ArrowArrayStream@ structure.

Uses the shared @ArrowArrayStream@ storage and constructors.
-}
type DuckDBV2ArrowArrayStream = ArrowArrayStream

{- | A borrowed, length-delimited string view: @ptr@ points at @len@ bytes of character data. The bytes are NOT guaranteed
to be null-terminated and may contain interior null bytes, so always honor @len@ rather than scanning for a
terminator. The view never owns its bytes; the function that produced it documents how long they live (typically
"valid until the owning handle is destroyed"). @{NULL, 0}@ is the canonical empty view, and @ptr@ must not be
dereferenced when @len@ is 0. Not to be confused with @bytes@, the transparent 16-byte *storage* format for a
variable-size value in a vector.

Functions take an input view as a @const str *@, which must not be NULL; a NULL pointer, or a view whose @ptr@ is
NULL while @len@ is nonzero, fails with ERROR_INPUT_INVALID. Point at a @{NULL, 0}@ view to pass an empty string.

Text inputs, such as VARCHAR values and names, must contain valid UTF-8. The caller is responsible for ensuring this;
API functions do not necessarily validate the input. Binary inputs, such as BLOB values, do not require valid UTF-8.
-}
data DuckDBV2Str = DuckDBV2Str
    { duckdbV2StrPtr :: Ptr CChar
    , duckdbV2StrLen :: DuckDBV2Idx
    }
    deriving (Eq, Show)

instance Storable DuckDBV2Str where
    sizeOf _ = #{size duckdb_v2_str}
    alignment _ = #{alignment duckdb_v2_str}
    peek ptr =
        DuckDBV2Str
            <$> peekByteOff ptr #{offset duckdb_v2_str, ptr}
            <*> peekByteOff ptr #{offset duckdb_v2_str, len}
    poke ptr DuckDBV2Str{..} = do
        pokeByteOff ptr #{offset duckdb_v2_str, ptr} duckdbV2StrPtr
        pokeByteOff ptr #{offset duckdb_v2_str, len} duckdbV2StrLen

{- | 16-byte storage for a variable-size, byte-backed value: VARCHAR, BLOB, BIT, or BIGNUM. The bytes are inlined when the
length is at most BYTES_INLINE_LENGTH, in value.inlined.inlined; otherwise value.pointer.ptr holds them and
value.pointer.prefix repeats the first
4. The union is transparent, so read the fields directly. VARCHAR and
BLOB carry no further encoding; BIT and BIGNUM do. For BIT, byte 0 is the padding-bit count and bytes 1.. are the
data; BIGNUM is decoded via @duckdb_v2_bignum_decode()@.

The three storage words preserve the union bytes. Read the pointer arm
with 'duckdbV2BytesPointer', or access the inline bytes with
'duckdbV2BytesInlinePointer'. These functions borrow the supplied storage.
-}
data DuckDBV2Bytes = DuckDBV2Bytes
    { duckdbV2BytesLength :: Word32
    , duckdbV2BytesStorage0 :: Word32
    , duckdbV2BytesStorage1 :: Word32
    , duckdbV2BytesStorage2 :: Word32
    }
    deriving (Eq, Show)

instance Storable DuckDBV2Bytes where
    sizeOf _ = #{size duckdb_v2_bytes}
    alignment _ = #{alignment duckdb_v2_bytes}
    peek ptr =
        DuckDBV2Bytes
            <$> peekByteOff ptr #{offset duckdb_v2_bytes, value.inlined.length}
            <*> peekByteOff ptr (#{offset duckdb_v2_bytes, value.inlined.inlined} + 0)
            <*> peekByteOff ptr (#{offset duckdb_v2_bytes, value.inlined.inlined} + 4)
            <*> peekByteOff ptr (#{offset duckdb_v2_bytes, value.inlined.inlined} + 8)
    poke ptr DuckDBV2Bytes{..} = do
        pokeByteOff ptr #{offset duckdb_v2_bytes, value.inlined.length} duckdbV2BytesLength
        pokeByteOff ptr (#{offset duckdb_v2_bytes, value.inlined.inlined} + 0) duckdbV2BytesStorage0
        pokeByteOff ptr (#{offset duckdb_v2_bytes, value.inlined.inlined} + 4) duckdbV2BytesStorage1
        pokeByteOff ptr (#{offset duckdb_v2_bytes, value.inlined.inlined} + 8) duckdbV2BytesStorage2

{- | Return the borrowed payload pointer of the non-inline union arm.

Call this only when the stored length exceeds 'duckdbV2BytesInlineLength'.
The pointer remains valid only while the owning vector or value is alive.
-}
duckdbV2BytesPointer :: Ptr DuckDBV2Bytes -> IO (Ptr CChar)
duckdbV2BytesPointer ptr = peekByteOff ptr #{offset duckdb_v2_bytes, value.pointer.ptr}

{- | Return a pointer to the inline bytes of the supplied storage.

Call this only when the stored length is at most 'duckdbV2BytesInlineLength'.
The pointer remains valid only while the supplied storage is alive.
-}
duckdbV2BytesInlinePointer :: Ptr DuckDBV2Bytes -> Ptr CChar
duckdbV2BytesInlinePointer ptr = castPtr ptr `plusPtr` #{offset duckdb_v2_bytes, value.inlined.inlined}

{- | An opaque, owned handle to a caller-defined resource. Bundles the pointer with optional callbacks that destroy and
compare the resource.
-}
data DuckDBV2Opaque = DuckDBV2Opaque
    { duckdbV2OpaquePtr :: Ptr ()
    , duckdbV2OpaqueDestroy :: DuckDBDeleteCallback
    , duckdbV2OpaqueEquals :: FunPtr DuckDBV2OpaqueEqualsFn
    }
    deriving (Eq, Show)

instance Storable DuckDBV2Opaque where
    sizeOf _ = #{size duckdb_v2_opaque}
    alignment _ = #{alignment duckdb_v2_opaque}
    peek ptr =
        DuckDBV2Opaque
            <$> peekByteOff ptr #{offset duckdb_v2_opaque, ptr}
            <*> peekByteOff ptr #{offset duckdb_v2_opaque, destroy}
            <*> peekByteOff ptr #{offset duckdb_v2_opaque, equals}
    poke ptr DuckDBV2Opaque{..} = do
        pokeByteOff ptr #{offset duckdb_v2_opaque, ptr} duckdbV2OpaquePtr
        pokeByteOff ptr #{offset duckdb_v2_opaque, destroy} duckdbV2OpaqueDestroy
        pokeByteOff ptr #{offset duckdb_v2_opaque, equals} duckdbV2OpaqueEquals

{- | The single argument to a V2 C extension entrypoint. Borrowed for the duration of the call: none of its fields outlive
the entrypoint, and the extension destroys none of them.
-}
data DuckDBV2ExtensionInput = DuckDBV2ExtensionInput
    { duckdbV2ExtensionInputGetApi :: FunPtr DuckDBV2ExtensionGetApiFn
    , duckdbV2ExtensionInputExtension :: DuckDBV2ExtensionHandle
    , duckdbV2ExtensionInputContext :: DuckDBV2ContextHandle
    , duckdbV2ExtensionInputErr :: Ptr DuckDBV2ErrorInfoHandle
    }
    deriving (Eq, Show)

instance Storable DuckDBV2ExtensionInput where
    sizeOf _ = #{size duckdb_v2_extension_input}
    alignment _ = #{alignment duckdb_v2_extension_input}
    peek ptr =
        DuckDBV2ExtensionInput
            <$> peekByteOff ptr #{offset duckdb_v2_extension_input, get_api}
            <*> peekByteOff ptr #{offset duckdb_v2_extension_input, extension}
            <*> peekByteOff ptr #{offset duckdb_v2_extension_input, context}
            <*> peekByteOff ptr #{offset duckdb_v2_extension_input, err}
    poke ptr DuckDBV2ExtensionInput{..} = do
        pokeByteOff ptr #{offset duckdb_v2_extension_input, get_api} duckdbV2ExtensionInputGetApi
        pokeByteOff ptr #{offset duckdb_v2_extension_input, extension} duckdbV2ExtensionInputExtension
        pokeByteOff ptr #{offset duckdb_v2_extension_input, context} duckdbV2ExtensionInputContext
        pokeByteOff ptr #{offset duckdb_v2_extension_input, err} duckdbV2ExtensionInputErr

-- | The @duckdb_v2_vector_view@ structure.
data DuckDBV2VectorView = DuckDBV2VectorView
    { duckdbV2VectorViewData :: Ptr ()
    , duckdbV2VectorViewValidity :: Ptr Word64
    , duckdbV2VectorViewSel :: Ptr DuckDBV2SelT
    , duckdbV2VectorViewCount :: DuckDBV2Idx
    }
    deriving (Eq, Show)

instance Storable DuckDBV2VectorView where
    sizeOf _ = #{size duckdb_v2_vector_view}
    alignment _ = #{alignment duckdb_v2_vector_view}
    peek ptr =
        DuckDBV2VectorView
            <$> peekByteOff ptr #{offset duckdb_v2_vector_view, data}
            <*> peekByteOff ptr #{offset duckdb_v2_vector_view, validity}
            <*> peekByteOff ptr #{offset duckdb_v2_vector_view, sel}
            <*> peekByteOff ptr #{offset duckdb_v2_vector_view, count}
    poke ptr DuckDBV2VectorView{..} = do
        pokeByteOff ptr #{offset duckdb_v2_vector_view, data} duckdbV2VectorViewData
        pokeByteOff ptr #{offset duckdb_v2_vector_view, validity} duckdbV2VectorViewValidity
        pokeByteOff ptr #{offset duckdb_v2_vector_view, sel} duckdbV2VectorViewSel
        pokeByteOff ptr #{offset duckdb_v2_vector_view, count} duckdbV2VectorViewCount

{- | The @duckdb_v2_list_entry@ structure.

Uses the shared @DuckDBListEntry@ storage and constructors.
-}
type DuckDBV2ListEntry = DuckDBListEntry

{- | The @duckdb_v2_hugeint_t@ structure.

Uses the shared @DuckDBHugeInt@ storage and constructors.
-}
type DuckDBV2HugeintT = DuckDBHugeInt

{- | The @duckdb_v2_uhugeint_t@ structure.

Uses the shared @DuckDBUHugeInt@ storage and constructors.
-}
type DuckDBV2UhugeintT = DuckDBUHugeInt

{- | The @duckdb_v2_interval_t@ structure.

Uses the shared @DuckDBInterval@ storage and constructors.
-}
type DuckDBV2IntervalT = DuckDBInterval
