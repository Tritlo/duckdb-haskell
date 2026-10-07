{-# LANGUAGE ForeignFunctionInterface #-}

{- | Complete raw DuckDB V2 preview C API.

Generated from the pinned header by @scripts/gen_ffi_v2.py@.
Every import calls the C function directly with its native signature.
Every import is safe because calls can run registered Haskell callbacks.
Source documentation records the upstream API lifecycle status.
Callers must pin the matching native library.
-}
module Database.DuckDB.FFI.V2.Functions where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)
import Database.DuckDB.FFI.V2.Types
import Foreign.C.Types (CBool (..), CChar (..), CDouble (..), CFloat (..), CInt (..))
import Foreign.Ptr (FunPtr, Ptr)

{- | Allocate @byte_len@ bytes from the arena.

Returns a writable pointer to @byte_len@ uninitialized bytes, valid for as long as the arena is. There is no minimum
or maximum allocation size, and the arena may return a pointer to a block of memory that is larger than @byte_len@.
The arena will free all allocated memory when it is destroyed, so the caller should not attempt to free the returned
pointer themselves.

history:
- stable: v2.0.0


@arena@: The arena to allocate from.

@byte_len@: Number of bytes to reserve. May be 0.

@out_ptr@: Receives a writable pointer to byte_len arena-owned bytes.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_arena_allocate"
    c_duckdb_v2_arena_allocate :: DuckDBV2ArenaHandle -> DuckDBV2Idx -> Ptr (Ptr Word8) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a new cast function that will be registered on the connection's database.

The function starts out empty: configure it with the setter functions (e.g.
@duckdb_v2_cast_function_set_source_type()@, @duckdb_v2_cast_function_set_exec_callback()@, etc.), then make it
available with @duckdb_v2_cast_function_register()@. The caller owns the returned handle and must destroy it with
@duckdb_v2_cast_function_destroy()@, also after registration.

history:
- stable: v2.0.0


@connection@: The connection to create the function in.

@function@: On success, receives the newly created cast function. Owned by the caller.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_cast_function_create_with_connection"
    c_duckdb_v2_cast_function_create_with_connection :: DuckDBV2ConnectionHandle -> Ptr DuckDBV2CastFunctionHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a new cast function that will be registered on the loading extension's database.

Use this from an extension load callback, where an extension handle is available. The function starts out empty:
configure it with the setter functions (e.g. @duckdb_v2_cast_function_set_source_type()@,
@duckdb_v2_cast_function_set_exec_callback()@, etc.), then make it available with
@duckdb_v2_cast_function_register()@. The caller owns the returned handle and must destroy it with
@duckdb_v2_cast_function_destroy()@, also after registration.

history:
- stable: v2.0.0


@extension@: The extension to create the function in.

@function@: On success, receives the newly created cast function. Owned by the caller.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_cast_function_create_with_extension"
    c_duckdb_v2_cast_function_create_with_extension :: DuckDBV2ExtensionHandle -> Ptr DuckDBV2CastFunctionHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets the type the cast converts from.

The type is borrowed and copied. Calling this again replaces the previous source type. A source type must be set
before registration, and it must be a fully defined concrete type.

history:
- stable: v2.0.0


@function@: The function to set the source type of.

@source_type@: The type to cast from. Borrowed for the call only.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_cast_function_set_source_type"
    c_duckdb_v2_cast_function_set_source_type :: DuckDBV2CastFunctionHandle -> DuckDBV2LogicalTypeHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets the type the cast converts to.

The type is borrowed and copied. Calling this again replaces the previous target type. A target type must be set
before registration, and it must be a fully defined concrete type.

history:
- stable: v2.0.0


@function@: The function to set the target type of.

@target_type@: The type to cast to. Borrowed for the call only.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_cast_function_set_target_type"
    c_duckdb_v2_cast_function_set_target_type :: DuckDBV2CastFunctionHandle -> DuckDBV2LogicalTypeHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets what it costs to apply this cast implicitly.

The binder uses the cost to choose between candidate implicit casts: a lower non-negative cost makes the cast more
likely to be picked, a higher one less likely. Built-in widening casts sit in the [0, 20] range, so a cost above 100
effectively puts this cast last. A negative cost -- the default -- means the cast is never applied implicitly and is
reached only through an explicit CAST or TRY_CAST.

history:
- stable: v2.0.0


@function@: The function to set the implicit cast cost of.

@cost@: The cost. Negative disables implicit casting.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_cast_function_set_implicit_cast_cost"
    c_duckdb_v2_cast_function_set_implicit_cast_cost :: DuckDBV2CastFunctionHandle -> Int64 -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets arbitrary user data on the cast function.

Associates an opaque pointer with the function, retrievable from the exec callback via
@duckdb_v2_cast_function_exec_get_user_data()@. The opaque handle bundles the pointer with an optional destructor,
invoked when the data is no longer needed. The data is read-only during execution: the same pointer is shared by
every thread running the cast.

history:
- stable: v2.0.0


@function@: The function to set the user data of.

@data@: Opaque handle bundling the user data pointer plus an optional destructor.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_cast_function_set_user_data"
    c_duckdb_v2_cast_function_set_user_data :: DuckDBV2CastFunctionHandle -> Ptr DuckDBV2Opaque -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets the exec callback of the cast function.

The exec callback implements the conversion: it is invoked during query execution with a batch of input values and
must fill the output vector. An exec callback must be set before registration.

history:
- stable: v2.0.0


@function@: The function to set the exec callback of.

@callback@: The exec callback to set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_cast_function_set_exec_callback"
    c_duckdb_v2_cast_function_set_exec_callback :: DuckDBV2CastFunctionHandle -> FunPtr DuckDBV2CastFunctionExecCallbackFn -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the user data set via @duckdb_v2_cast_function_set_user_data()@.

history:
- stable: v2.0.0


@info@: The exec info handle.

@data@: Receives the user data pointer, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_cast_function_exec_get_user_data"
    c_duckdb_v2_cast_function_exec_get_user_data :: DuckDBV2CastFunctionExecInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns how many rows this execution must convert.

The number of rows held by the input vector, and the number of entries the callback must write to the output vector.
Note that this may be less than a full vector: a constant input is converted as a single row and the result is
expanded by the engine.

history:
- stable: v2.0.0


@info@: The exec info handle.

@count@: Receives the number of rows.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_cast_function_exec_get_row_count"
    c_duckdb_v2_cast_function_exec_get_row_count :: DuckDBV2CastFunctionExecInfoHandle -> Ptr DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the input vector holding the values to convert.

The vector holds the source type's values for the current batch; use @duckdb_v2_cast_function_exec_get_row_count()@
for the number of rows. Borrowed; valid only for the duration of the callback.

history:
- stable: v2.0.0


@info@: The exec info handle.

@vector@: Receives the borrowed input vector.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_cast_function_exec_get_input"
    c_duckdb_v2_cast_function_exec_get_input :: DuckDBV2CastFunctionExecInfoHandle -> Ptr DuckDBV2VectorHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the output vector the exec callback must write into.

The callback must write one entry per input row; use @duckdb_v2_cast_function_exec_get_row_count()@ for the number of
rows. Borrowed; valid only for the duration of the callback.

history:
- stable: v2.0.0


@info@: The exec info handle.

@vector@: Receives the borrowed output vector to write into.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_cast_function_exec_get_output"
    c_duckdb_v2_cast_function_exec_get_output :: DuckDBV2CastFunctionExecInfoHandle -> Ptr DuckDBV2VectorHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the mode the cast is being executed in.

In @CAST_MODE_TRY@ a conversion failure should be written as a NULL into the output vector rather than reported
through the error slot, since the engine discards the error and keeps whatever the callback left in the output.

history:
- stable: v2.0.0


@info@: The exec info handle.

@mode@: Receives the cast mode.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_cast_function_exec_get_mode"
    c_duckdb_v2_cast_function_exec_get_mode :: DuckDBV2CastFunctionExecInfoHandle -> Ptr DuckDBV2CastMode -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Registers the cast function, making it available to CAST and TRY_CAST.

The function is registered on the target given at creation: the connection's database or the loading extension.
Registration requires a source type, a target type and an exec callback; both types must be fully defined concrete
types. Registering a cast for a pair that already has one replaces it. The caller still owns the handle after
registration and must destroy it with @duckdb_v2_cast_function_destroy()@, which does not affect the registered
function.

history:
- stable: v2.0.0


@function@: The function to register.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_cast_function_register"
    c_duckdb_v2_cast_function_register :: DuckDBV2CastFunctionHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Destroys the cast function, releasing its resources.

Null-safe: passing a null pointer or null handle is a no-op. The handle is set to null on return to prevent
double-destruction. Destroying the handle after registration does not affect the registered function.

history:
- stable: v2.0.0


@function@: The function to destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_cast_function_destroy"
    c_duckdb_v2_cast_function_destroy :: Ptr DuckDBV2CastFunctionHandle -> IO DuckDBV2Error

{- | Creates an empty column data collection from a connection.

Allocates a new, empty column data collection, drawing its buffer allocator from the connection's context. The
collection starts with no chunks and is ready to have chunks appended to it. A collection must have at least one
column; an empty types array is rejected with INVALID_INPUT.

history:
- stable: v2.0.0


@conn@: The connection whose context supplies the collection's allocator.

@types_array@: Pointer to an array of logical_type handles, one per column. This defines the schema for the
chunks that will be stored in the collection. All chunks appended to this collection must have vectors that conform
to these types.

@types_count@: Number of elements in the types_array.

@out_collection@: Receives the new collection handle.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_column_data_collection_create_with_connection"
    c_duckdb_v2_column_data_collection_create_with_connection :: DuckDBV2ConnectionHandle -> Ptr DuckDBV2LogicalTypeHandle -> DuckDBV2Idx -> Ptr DuckDBV2ColumnDataCollectionHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates an empty column data collection from a context.

Allocates a new, empty column data collection, drawing its buffer allocator from the context. Use this from inside a
callback or extension where a context is already in hand. The collection starts with no chunks and is ready to have
chunks appended to it. A collection must have at least one column; an empty types array is rejected with
INVALID_INPUT.

history:
- stable: v2.0.0


@context@: The context whose allocator will be used for the collection.

@types_array@: Pointer to an array of logical_type handles, one per column. This defines the schema for the
chunks that will be stored in the collection. All chunks appended to this collection must have vectors that conform
to these types.

@types_count@: Number of elements in the types_array.

@out_collection@: Receives the new collection handle.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_column_data_collection_create_with_context"
    c_duckdb_v2_column_data_collection_create_with_context :: DuckDBV2ContextHandle -> Ptr DuckDBV2LogicalTypeHandle -> DuckDBV2Idx -> Ptr DuckDBV2ColumnDataCollectionHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Drops all buffered rows, keeping the column types.

Drops all buffered rows and releases their memory. The column types are unchanged and the collection is immediately
appendable again. Existing append and scan states are invalidated. Use @duckdb_v2_column_data_collection_clear()@
instead to keep the memory for the next appends.

history:
- stable: v2.0.0


@collection@: The collection to reset.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_column_data_collection_reset"
    c_duckdb_v2_column_data_collection_reset :: DuckDBV2ColumnDataCollectionHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Drops all buffered rows, keeping the column types and the memory.

Like @duckdb_v2_column_data_collection_reset()@, but the buffers are retained rather than released, so the next
appends write into memory that is already allocated. Use this when the collection is refilled repeatedly. The column
types are unchanged and the collection is immediately appendable again. Existing append and scan states are
invalidated.

history:
- stable: v2.0.0


@collection@: The collection to clear.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_column_data_collection_clear"
    c_duckdb_v2_column_data_collection_clear :: DuckDBV2ColumnDataCollectionHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Destroys a column data collection and all its chunks.

Cleans up the collection and all resources associated with it, including all contained chunks. Sets the collection
handle to NULL.

history:
- stable: v2.0.0


@collection@: The collection to destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_column_data_collection_destroy"
    c_duckdb_v2_column_data_collection_destroy :: Ptr DuckDBV2ColumnDataCollectionHandle -> IO DuckDBV2Error

{- | Merges the source collection into the target collection, consuming the source in the process.

Transfers all chunks from the source collection to the target collection. This destroys the source collection,
setting the source handle to NULL. The two collections must have the same column types, and must have been created
from the same context (or connections to the same database): the transferred chunks keep their original buffers, so
combining collections from different databases would tie the target to a buffer manager it does not own.

history:
- stable: v2.0.0


@target@: The collection to merge into. After the call, this collection will contain all chunks from both
collections.

@source@: The collection to merge from. This collection will be destroyed by this operation and its handle set to
NULL.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_column_data_collection_combine"
    c_duckdb_v2_column_data_collection_combine :: DuckDBV2ColumnDataCollectionHandle -> Ptr DuckDBV2ColumnDataCollectionHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the total number of rows across all chunks in the collection.

Sums the row counts of all chunks in the collection. This is the total number of rows represented by the collection,
which may be more than the number of rows in any individual chunk.

history:
- stable: v2.0.0


@collection@: The collection to inspect.

@out_row_count@: Receives the total row count.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_column_data_collection_row_count"
    c_duckdb_v2_column_data_collection_row_count :: DuckDBV2ColumnDataCollectionHandle -> Ptr DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Initializes an append state for the collection.

Returns an append state handle that can be used to append chunks to the collection while maintaining any necessary
state across calls.

history:
- stable: v2.0.0


@collection@: The collection to prepare for appending.

@out_state@: Receives the new append state handle.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_column_data_collection_append_state_create"
    c_duckdb_v2_column_data_collection_append_state_create :: DuckDBV2ColumnDataCollectionHandle -> Ptr DuckDBV2ColumnDataCollectionAppendStateHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Destroys an append state handle.

Cleans up any resources associated with the append state. After this call, the state handle must not be used again.

history:
- stable: v2.0.0


@state@: The append state to destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_column_data_collection_append_state_destroy"
    c_duckdb_v2_column_data_collection_append_state_destroy :: Ptr DuckDBV2ColumnDataCollectionAppendStateHandle -> IO DuckDBV2Error

{- | Appends a data chunk to the collection.

Appends a copy of the given chunk to the end of the collection. The chunk's column count and types must equal the
collection's exactly; a mismatch is rejected with INVALID_INPUT before anything is copied. VARCHAR values must
already contain valid UTF-8; this function does not validate text. Complex-typed vectors may be flattened in place by
the copy.

history:
- stable: v2.0.0


@collection@: The collection to append to.

@state@: The append state to use this append operation.

@chunk@: The chunk to append.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_column_data_collection_append"
    c_duckdb_v2_column_data_collection_append :: DuckDBV2ColumnDataCollectionHandle -> DuckDBV2ColumnDataCollectionAppendStateHandle -> DuckDBV2DataChunkHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Initializes a shared scan state for the collection.

Returns a shared scan state handle that can be used by multiple threads to coordinate scanning of the collection.
Caller owns the returned state and must destroy it via column_data_collection_shared_scan_state_destroy when done.

history:
- stable: v2.0.0


@collection@: The collection to prepare for shared scanning.

@out_state@: Receives the new shared scan state handle.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_column_data_collection_shared_scan_state_create"
    c_duckdb_v2_column_data_collection_shared_scan_state_create :: DuckDBV2ColumnDataCollectionHandle -> Ptr DuckDBV2ColumnDataCollectionSharedScanStateHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Destroys a shared scan state handle.

Cleans up any resources associated with the shared scan state. After this call, the state handle must not be used
again.

history:
- stable: v2.0.0


@state@: The shared scan state to destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_column_data_collection_shared_scan_state_destroy"
    c_duckdb_v2_column_data_collection_shared_scan_state_destroy :: Ptr DuckDBV2ColumnDataCollectionSharedScanStateHandle -> IO DuckDBV2Error

{- | Initializes a worker scan state for the collection.

Returns a "local" scan state handle for a worker thread to use while scanning the collection. This allows each thread
to maintain its own progress and any other necessary information across multiple scan calls, while still coordinating
with other threads via the shared scan state.

history:
- stable: v2.0.0


@collection@: The collection to prepare for worker scanning.

@out_state@: Receives the new worker scan state handle.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_column_data_collection_worker_scan_state_create"
    c_duckdb_v2_column_data_collection_worker_scan_state_create :: DuckDBV2ColumnDataCollectionHandle -> Ptr DuckDBV2ColumnDataCollectionWorkerScanStateHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Destroys a worker scan state handle.

Cleans up any resources associated with the worker scan state. After this call, the state handle must not be used
again.

history:
- stable: v2.0.0


@state@: The worker scan state to destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_column_data_collection_worker_scan_state_destroy"
    c_duckdb_v2_column_data_collection_worker_scan_state_destroy :: Ptr DuckDBV2ColumnDataCollectionWorkerScanStateHandle -> IO DuckDBV2Error

{- | Scans the collection to retrieve the next data chunk, using shared state for coordination across threads and
worker-local state for individual progress tracking.

Retrieves the next chunk of data from the collection based on the provided shared scan state and worker-local scan
state. The output chunk's column count and types must equal the collection's exactly; a mismatch is rejected with
INVALID_INPUT.

The scan is zero-copy where possible: the output chunk's vectors may reference the collection's buffers, kept alive
by the worker scan state. The chunk's data is therefore only valid until the next scan call with the same worker
state, or until the worker state or the collection is destroyed, whichever comes first. Copy the data out (or append
it to another collection) before continuing the scan if it must outlive that window.

history:
- stable: v2.0.0


@collection@: The collection to scan.

@shared_state@: The shared scan state to use for coordinating across threads.

@worker_state@: The worker-local scan state to use for this thread's scan operation.

@out_chunk@: The output chunk to write to.

@did_produce_chunk@: Receives true when a chunk was produced by this call, or false when the scan has completed
(the chunk is then reset to empty).

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_column_data_collection_scan"
    c_duckdb_v2_column_data_collection_scan :: DuckDBV2ColumnDataCollectionHandle -> DuckDBV2ColumnDataCollectionSharedScanStateHandle -> DuckDBV2ColumnDataCollectionWorkerScanStateHandle -> DuckDBV2DataChunkHandle -> Ptr CBool -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Destroys an option descriptor.

Frees the handle along with the strings it owns. On success the slot is set to null. Safe to call on an already-null
slot.

history:
- stable: v2.0.0


@option@: The option handle to destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_option_destroy"
    c_duckdb_v2_option_destroy :: Ptr DuckDBV2OptionHandle -> IO DuckDBV2Error

{- | Borrows the option's canonical name.

When the option was resolved from an alias, this still reports the canonical name; the alias is reachable via
@duckdb_v2_option_get_alias()@.

history:
- stable: v2.0.0


@option@: The option.

@out_name@: Receives a borrowed view of the option name. Valid until the option is destroyed.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_option_get_name"
    c_duckdb_v2_option_get_name :: DuckDBV2OptionHandle -> Ptr DuckDBV2IdentifierT -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Borrows the option's current setting (its string-encoded value).

The effective setting at the scope of the instance, connection, or context the descriptor was read from.

history:
- stable: v2.0.0


@option@: The option.

@out_setting@: Receives a borrowed view of the setting. Valid until the option is destroyed.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_option_get_setting"
    c_duckdb_v2_option_get_setting :: DuckDBV2OptionHandle -> Ptr DuckDBV2Str -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Borrows the option's static default setting.

The empty view for an option whose declaration has no default.

history:
- stable: v2.0.0


@option@: The option.

@out_default_setting@: Receives a borrowed view of the default setting. Valid until the option is destroyed.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_option_get_default_setting"
    c_duckdb_v2_option_get_default_setting :: DuckDBV2OptionHandle -> Ptr DuckDBV2Str -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Borrows the option's human-readable description.

history:
- stable: v2.0.0


@option@: The option.

@out_description@: Receives a borrowed view of the description. Valid until the option is destroyed.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_option_get_description"
    c_duckdb_v2_option_get_description :: DuckDBV2OptionHandle -> Ptr DuckDBV2Str -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the option's target scope.

OPTION_TARGET_SCOPE_UNKNOWN for an option whose declaration carries no explicit scope target, which includes every
extension option.

history:
- stable: v2.0.0


@option@: The option.

@out_target_scope@: Receives the target scope.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_option_get_target_scope"
    c_duckdb_v2_option_get_target_scope :: DuckDBV2OptionHandle -> Ptr DuckDBV2OptionTargetScope -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the number of aliases registered for this option.

Zero for an extension option; extension options carry no aliases.

history:
- stable: v2.0.0


@option@: The option.

@out_count@: Receives the alias count.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_option_get_alias_count"
    c_duckdb_v2_option_get_alias_count :: DuckDBV2OptionHandle -> Ptr DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Borrows the alias name at the given index.

An out-of-range index returns ERROR_INPUT_INVALID.

history:
- stable: v2.0.0


@option@: The option.

@index@: The alias index, in [0, alias_count).

@out_alias@: Receives a borrowed view of the alias. Valid until the option is destroyed.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_option_get_alias"
    c_duckdb_v2_option_get_alias :: DuckDBV2OptionHandle -> DuckDBV2Idx -> Ptr DuckDBV2IdentifierT -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Reads a config option through a context.

The context is a connection seen from inside DuckDB, so this reads the same cascade
@duckdb_v2_connection_get_option_by_name()@ does: the LOCAL override if the connection set one, otherwise the GLOBAL
value, otherwise the static default. Aliases resolve transparently, and an unknown name returns ERROR_INPUT_INVALID.
The caller destroys the returned option. A context is a read scope: options are written through an instance or
connection.

history:
- stable: v2.0.0


@ctx@: The context.

@name@: Option name (canonical or alias).

@out_option@: Receives the populated option handle.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_context_get_option_by_name"
    c_duckdb_v2_context_get_option_by_name :: DuckDBV2ContextHandle -> Ptr DuckDBV2IdentifierT -> Ptr DuckDBV2OptionHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the number of config options visible to this context.

The same count @duckdb_v2_instance_get_option_count()@ reports for the underlying instance: core plus extension
options, aliases excluded.

history:
- stable: v2.0.0


@ctx@: The context.

@out_count@: Receives the option count.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_context_get_option_count"
    c_duckdb_v2_context_get_option_count :: DuckDBV2ContextHandle -> Ptr DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Reads the config option at the given index visible to this context.

Index space: [0, core_count) addresses core options, [core_count, total) the extension options visible from this
context's instance. An out-of-range index returns ERROR_INPUT_INVALID. The caller destroys the returned option.

history:
- stable: v2.0.0


@ctx@: The context.

@index@: The option index, in [0, option_count).

@out_option@: Receives the populated option handle.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_context_get_option_by_index"
    c_duckdb_v2_context_get_option_by_index :: DuckDBV2ContextHandle -> DuckDBV2Idx -> Ptr DuckDBV2OptionHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a new custom type that will be registered on the connection's database.

The type starts out empty: configure it with @duckdb_v2_custom_type_set_name()@ and
@duckdb_v2_custom_type_set_base_type()@, then make it available with @duckdb_v2_custom_type_register()@. The caller
owns the returned handle and must destroy it with @duckdb_v2_custom_type_destroy()@, also after registration.

history:
- stable: v2.0.0


@connection@: The connection to create the type in.

@type@: On success, receives the newly created custom type. Owned by the caller.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_custom_type_create_with_connection"
    c_duckdb_v2_custom_type_create_with_connection :: DuckDBV2ConnectionHandle -> Ptr DuckDBV2CustomTypeHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a new custom type that will be registered on the loading extension's database.

Use this from an extension load callback, where an extension handle is available. The type starts out empty:
configure it with @duckdb_v2_custom_type_set_name()@ and @duckdb_v2_custom_type_set_base_type()@, then make it
available with @duckdb_v2_custom_type_register()@. The caller owns the returned handle and must destroy it with
@duckdb_v2_custom_type_destroy()@, also after registration.

history:
- stable: v2.0.0


@extension@: The extension to create the type in.

@type@: On success, receives the newly created custom type. Owned by the caller.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_custom_type_create_with_extension"
    c_duckdb_v2_custom_type_create_with_extension :: DuckDBV2ExtensionHandle -> Ptr DuckDBV2CustomTypeHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets the name of the custom type.

This is the name the type is referred to by in SQL, and the alias carried by every logical type instance of it. The
name is borrowed and copied. Calling this again replaces the previous name. A name must be set before registration.

history:
- stable: v2.0.0


@type@: The type to set the name of.

@name@: The name to set. Borrowed and copied.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_custom_type_set_name"
    c_duckdb_v2_custom_type_set_name :: DuckDBV2CustomTypeHandle -> Ptr DuckDBV2IdentifierT -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets the base type of the custom type.

The custom type shares the base type's internal representation but is logically distinct, so it can carry its own
cast functions. The type is borrowed and copied. Calling this again replaces the previous base type. A base type must
be set before registration, and it must be a fully defined concrete type -- ANY is a signature wildcard, not
something a registered type can be built on.

history:
- stable: v2.0.0


@type@: The type to set the base type of.

@base_type@: The logical type to base the custom type on. Borrowed for the call only.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_custom_type_set_base_type"
    c_duckdb_v2_custom_type_set_base_type :: DuckDBV2CustomTypeHandle -> DuckDBV2LogicalTypeHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Registers the custom type, making it available for use in SQL queries.

The type is registered on the target given at creation: the connection's database or the loading extension.
Registration requires a name and a complete base type. The caller still owns the handle after registration and must
destroy it with @duckdb_v2_custom_type_destroy()@, which does not affect the registered type.

history:
- stable: v2.0.0


@type@: The type to register.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_custom_type_register"
    c_duckdb_v2_custom_type_register :: DuckDBV2CustomTypeHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Destroys the custom type, releasing its resources.

Null-safe: passing a null pointer or null handle is a no-op. The handle is set to null on return to prevent
double-destruction. Destroying the handle after registration does not affect the registered type.

history:
- stable: v2.0.0


@type@: The type to destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_custom_type_destroy"
    c_duckdb_v2_custom_type_destroy :: Ptr DuckDBV2CustomTypeHandle -> IO DuckDBV2Error

{- | Creates an empty data chunk with the given column types.

Allocates one FLAT vector per element of the types array, each at default capacity. Every vector starts at size 0;
set it with vector_set_size once the vector is populated. The chunk is caller-owned and must be destroyed via
data_chunk_destroy.

history:
- stable: v2.0.0


@types@: Pointer to an array of logical_type handles, one per column.

@column_count@: Number of elements in the types array.

@out_chunk@: Receives the new chunk handle.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_data_chunk_create"
    c_duckdb_v2_data_chunk_create :: Ptr DuckDBV2LogicalTypeHandle -> DuckDBV2Idx -> Ptr DuckDBV2DataChunkHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates an empty data chunk with the given column types, drawing its allocator from the connection's context.

Like data_chunk_create, but the chunk's vectors are allocated through the connection's context instead of the default
allocator, so the memory is accounted to that database. Allocates one FLAT vector per element of the types array,
each at default capacity. Every vector starts at size 0; set it with vector_set_size once the vector is populated.
The chunk is caller-owned and must be destroyed via data_chunk_destroy.

history:
- stable: v2.0.0


@conn@: The connection whose context supplies the chunk's allocator.

@types@: Pointer to an array of logical_type handles, one per column.

@column_count@: Number of elements in the types array.

@out_chunk@: Receives the new chunk handle.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_data_chunk_create_with_connection"
    c_duckdb_v2_data_chunk_create_with_connection :: DuckDBV2ConnectionHandle -> Ptr DuckDBV2LogicalTypeHandle -> DuckDBV2Idx -> Ptr DuckDBV2DataChunkHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates an empty data chunk with the given column types, drawing its allocator from a context.

Like data_chunk_create, but the chunk's vectors are allocated through the given context instead of the default
allocator, so the memory is accounted to that database. Use this from inside a callback or extension where a context
is already in hand. Allocates one FLAT vector per element of the types array, each at default capacity. Every vector
starts at size 0; set it with vector_set_size once the vector is populated. The chunk is caller-owned and must be
destroyed via data_chunk_destroy.

history:
- stable: v2.0.0


@context@: The context whose allocator will be used for the chunk.

@types@: Pointer to an array of logical_type handles, one per column.

@column_count@: Number of elements in the types array.

@out_chunk@: Receives the new chunk handle.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_data_chunk_create_with_context"
    c_duckdb_v2_data_chunk_create_with_context :: DuckDBV2ContextHandle -> Ptr DuckDBV2LogicalTypeHandle -> DuckDBV2Idx -> Ptr DuckDBV2DataChunkHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a deep copy of a data chunk, drawing its allocator from the connection's context.

Allocates a new chunk with the source chunk's column types and copies all rows into it. The copy is flattened and
owns all its data, so it stays valid after the source chunk or whatever backs it (such as a column data collection
scan state) is destroyed. The returned chunk is caller-owned and must be destroyed via data_chunk_destroy.

history:
- stable: v2.0.0


@conn@: The connection whose context supplies the copy's allocator.

@chunk@: The chunk to copy. Left unchanged.

@out_chunk@: Receives the new chunk handle.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_data_chunk_copy_with_connection"
    c_duckdb_v2_data_chunk_copy_with_connection :: DuckDBV2ConnectionHandle -> DuckDBV2DataChunkHandle -> Ptr DuckDBV2DataChunkHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a deep copy of a data chunk, drawing its allocator from a context.

Allocates a new chunk with the source chunk's column types and copies all rows into it. Use this from inside a
callback or extension where a context is already in hand. The copy is flattened and owns all its data, so it stays
valid after the source chunk or whatever backs it (such as a column data collection scan state) is destroyed. The
returned chunk is caller-owned and must be destroyed via data_chunk_destroy.

history:
- stable: v2.0.0


@context@: The context whose allocator will be used for the copy.

@chunk@: The chunk to copy. Left unchanged.

@out_chunk@: Receives the new chunk handle.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_data_chunk_copy_with_context"
    c_duckdb_v2_data_chunk_copy_with_context :: DuckDBV2ContextHandle -> DuckDBV2DataChunkHandle -> Ptr DuckDBV2DataChunkHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Destroys a data chunk handle.

Null-safe: passing nullptr or a slot already set to nullptr is a no-op. On success the slot is set to nullptr. Any
vector previously borrowed from this chunk becomes invalid.

history:
- stable: v2.0.0


@chunk@: The chunk to destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_data_chunk_destroy"
    c_duckdb_v2_data_chunk_destroy :: Ptr DuckDBV2DataChunkHandle -> IO DuckDBV2Error

{- | Returns the row count of a data chunk.

history:
- stable: v2.0.0


@chunk@: The chunk.

@out_size@: Receives the row count.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_data_chunk_get_size"
    c_duckdb_v2_data_chunk_get_size :: DuckDBV2DataChunkHandle -> Ptr DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the number of rows a data chunk can hold, e.g. how many rows the exec callback of a table function may write
into its output chunk in one call.

history:
- unstable: v2.0.0


@chunk@: The chunk.

@out_capacity@: Receives the number of rows the chunk can hold.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_data_chunk_get_capacity"
    c_duckdb_v2_data_chunk_get_capacity :: DuckDBV2DataChunkHandle -> Ptr DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the number of vectors in a data chunk.

Equals the column count of the producing result.

history:
- stable: v2.0.0


@chunk@: The chunk.

@out_count@: Receives the vector count.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_data_chunk_get_vector_count"
    c_duckdb_v2_data_chunk_get_vector_count :: DuckDBV2DataChunkHandle -> Ptr DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Borrows the vector at the given index in a data chunk.

The returned handle is borrowed and valid until the chunk is destroyed; do not destroy it. An out-of-range index
returns ERROR_INPUT_INVALID.

history:
- stable: v2.0.0


@chunk@: The chunk.

@index@: The vector index, in [0, vector_count).

@out_vector@: Receives the borrowed vector handle.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_data_chunk_get_vector"
    c_duckdb_v2_data_chunk_get_vector :: DuckDBV2DataChunkHandle -> DuckDBV2Idx -> Ptr DuckDBV2VectorHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates the V2 environment. Call once at program start.

history:
- stable: v2.0.0


@out_env@: Receives the new environment handle.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_environment_create"
    c_duckdb_v2_environment_create :: Ptr DuckDBV2EnvironmentHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Destroys the environment.

Refuses with ERROR_RESOURCE_IN_USE while any instance created through this environment is still alive; destroy those
instances first, then retry. On success the handle is set to null.

history:
- stable: v2.0.0


@env@: The environment.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_environment_destroy"
    c_duckdb_v2_environment_destroy :: Ptr DuckDBV2EnvironmentHandle -> IO DuckDBV2Error

{- | Returns the number of instances currently alive under the environment.

A diagnostic accessor, for tracking down leaked instances when @duckdb_v2_environment_destroy()@ returns
ERROR_RESOURCE_IN_USE. The count is a snapshot and may change before the next call.

history:
- stable: v2.0.0


@env@: The environment.

@out_count@: Receives the number of live instances.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_environment_get_instance_count"
    c_duckdb_v2_environment_get_instance_count :: DuckDBV2EnvironmentHandle -> Ptr DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the error code associated with an error info handle.

On success, writes the info's error code to @*out_code@. On failure (e.g. if @info@ is invalid), leaves @*out_code@
unchanged and returns a non-success error code.

history:
- stable: v2.0.0


@info@: The error info handle to query.

@out_code@: The error code associated with the info, if the call succeeds.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_error_info_get_code"
    c_duckdb_v2_error_info_get_code :: DuckDBV2ErrorInfoHandle -> Ptr DuckDBV2Error -> IO DuckDBV2Error

{- | Retrieves the error text associated with an error info handle.

Returns a borrowed pointer to the info's null-terminated error text. The pointer is owned by DuckDB and is valid
until the info is destroyed or a new text is set; callers must not free it, and must not read it once the info has
been destroyed.

history:
- stable: v2.0.0


@info@: The error info handle to query.

@out_text@: Receives a borrowed view of the text. Owned by DuckDB; valid until the info handle is destroyed.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_error_info_get_text"
    c_duckdb_v2_error_info_get_text :: DuckDBV2ErrorInfoHandle -> Ptr DuckDBV2Str -> IO DuckDBV2Error

{- | Retrieves the error text body without the leading category prefix.

Returns a borrowed view of the same text error_info_get_text reports, minus the leading "<Type> Error: " prefix. This
is the authoritative unprefixed body: the API exposes no type name, so the prefix cannot be reconstructed and the
body cannot be derived from error_info_get_text. Unprefixed does not mean plain — the body keeps whatever form it was
rendered in (a LINE/caret block by default, JSON under errors_as_json). @{NULL, 0}@ when there is no text. The
pointer is owned by DuckDB and valid until the info handle is destroyed.

history:
- stable: v2.0.0


@info@: The error info handle to query.

@out_raw_text@: Receives a borrowed view of the raw text. Owned by DuckDB; valid until the info handle is
destroyed.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_error_info_get_raw_text"
    c_duckdb_v2_error_info_get_raw_text :: DuckDBV2ErrorInfoHandle -> Ptr DuckDBV2Str -> IO DuckDBV2Error

{- | Sets the error code for an error info handle.

On success, replaces the info's error code with the provided one. On failure, nothing is changed. Accepts a @nullptr@
info handle, in which case the call is a no-op and returns ERROR_NONE.

history:
- stable: v2.0.0


@info@: The error info handle to set. On success, updated with the provided code.

@code@: The error code to set in the info.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_error_info_set_code"
    c_duckdb_v2_error_info_set_code :: DuckDBV2ErrorInfoHandle -> DuckDBV2Error -> IO DuckDBV2Error

{- | Sets the error text for an error info handle.

On success, replaces the info's text with the provided one; DuckDB allocates its own copy of the string. On failure,
nothing is changed. Accepts a @nullptr@ info handle, in which case the call is a no-op and returns ERROR_NONE.

history:
- stable: v2.0.0


@info@: The error info handle to set. On success, updated with the provided text.

@text@: The error text to set in the info.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_error_info_set_text"
    c_duckdb_v2_error_info_set_text :: DuckDBV2ErrorInfoHandle -> Ptr DuckDBV2Str -> IO DuckDBV2Error

{- | Destroys an error info handle and frees its resources.

Null-safe: calling with a null handle, or a null pointer-to-handle, is a no-op and returns ERROR_NONE. On return
@*info@ is set to nullptr. Safe to call on any info DuckDB returned.

history:
- stable: v2.0.0


@info@: The error info handle to destroy. Set to nullptr on return.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_error_info_destroy"
    c_duckdb_v2_error_info_destroy :: Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Borrows the file system of a context.

Use this from inside a callback, where a context is in hand. The returned handle is borrowed: it is valid only for as
long as the context is, and must not be destroyed.

history:
- stable: v2.0.0


@context@: The context to take the file system from.

@file_system@: Receives the borrowed file system.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_file_system_get_from_context"
    c_duckdb_v2_file_system_get_from_context :: DuckDBV2ContextHandle -> Ptr DuckDBV2FileSystemHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Borrows the file system of a connection.

The connection form of @duckdb_v2_file_system_get_from_context()@, for callers outside a callback. The returned
handle is borrowed: it is valid only for as long as the connection is, and must not be destroyed.

history:
- stable: v2.0.0


@connection@: The connection to take the file system from.

@file_system@: Receives the borrowed file system.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_file_system_get_from_connection"
    c_duckdb_v2_file_system_get_from_connection :: DuckDBV2ConnectionHandle -> Ptr DuckDBV2FileSystemHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a set of open options for a file system.

The options start out empty: give them flags with @duckdb_v2_file_open_options_set_flag()@, which is required, and
optionally attach values with @duckdb_v2_file_open_options_set_value()@. They are then passed to
@duckdb_v2_file_system_open()@, and can be reused for as many opens as you like. The caller owns the returned handle
and must destroy it with @duckdb_v2_file_open_options_destroy()@.

The options belong to the file system they were created from, since which values mean anything depends on which file
system ends up handling the path.

history:
- stable: v2.0.0


@file_system@: The file system the options are for.

@options@: On success, receives the new options. Owned by the caller; destroy via
@duckdb_v2_file_open_options_destroy()@.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_file_open_options_create"
    c_duckdb_v2_file_open_options_create :: DuckDBV2FileSystemHandle -> Ptr DuckDBV2FileOpenOptionsHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Applies one flag to the options.

Additive: call it once per behaviour you want, and applying the same flag twice is harmless. At least one flag must
be applied before the options can open anything, since the flags are what say whether the file is being read or
written. There is no way to take a flag back -- build a fresh set of options instead.

@FILE_FLAG_INVALID@ names no behaviour and is rejected, as is any value that is not a @DUCKDB_V2_FILE_FLAG@.

history:
- stable: v2.0.0


@options@: The options to apply the flag to.

@flag@: The flag to apply.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_file_open_options_set_flag"
    c_duckdb_v2_file_open_options_set_flag :: DuckDBV2FileOpenOptionsHandle -> DuckDBV2FileFlag -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Attaches a named value to the options.

These are hints for whichever file system ends up handling the path, and what they mean is that file system's
business: a value it does not recognise is ignored rather than rejected, and the same name can mean different things
to different file systems. They are the same values a file system reports when listing files, so a known file size or
modification time learned from a listing can be handed straight back to avoid re-reading it.

The name and the value are borrowed and copied, so the caller may destroy the value immediately after. Setting the
same name again replaces the previous value. Names are case-sensitive.

history:
- stable: v2.0.0


@options@: The options to set the value on.

@name@: The name of the value. Borrowed and copied.

@value@: The value. Borrowed and copied.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_file_open_options_set_value"
    c_duckdb_v2_file_open_options_set_value :: DuckDBV2FileOpenOptionsHandle -> Ptr DuckDBV2Str -> DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Destroys the options, releasing their resources.

Null-safe: passing a null pointer or null handle is a no-op. The handle is set to null on return to prevent
double-destruction. Files already opened with these options are unaffected.

history:
- stable: v2.0.0


@options@: The options to destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_file_open_options_destroy"
    c_duckdb_v2_file_open_options_destroy :: Ptr DuckDBV2FileOpenOptionsHandle -> IO DuckDBV2Error

{- | Opens a file.

Opens the file at the given path through the file system, which routes it the way the engine would -- a path handled
by a registered virtual or remote file system goes there rather than to local disk. The returned handle is owned by
the caller and must be destroyed with @duckdb_v2_file_destroy()@.

The options carry the flags and any file-system-specific values; see @duckdb_v2_file_open_options_create()@. Opening
without having set flags fails, since the flags are what say whether the file is being read or written.

Failure to open -- a missing file without @FILE_FLAG_CREATE@, insufficient permissions, an existing file under
@FILE_FLAG_EXCLUSIVE_CREATE@ -- is reported as an error.

Flag combinations that contradict each other, such as naming neither read nor write or combining @FILE_FLAG_CREATE@
with @FILE_FLAG_CREATE_NEW@, are a programming error rather than a supported input. An assertion build catches them;
elsewhere the behaviour is whatever the underlying file system does with them.

history:
- stable: v2.0.0


@file_system@: The file system to open the file through.

@file_path@: The path of the file to open. Borrowed for the call only.

@options@: How to open the file. Borrowed for the call only, and reusable across opens.

@file@: On success, receives the open file. Owned by the caller; destroy via @duckdb_v2_file_destroy()@.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_file_system_open"
    c_duckdb_v2_file_system_open :: DuckDBV2FileSystemHandle -> Ptr DuckDBV2Str -> DuckDBV2FileOpenOptionsHandle -> Ptr DuckDBV2FileHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Reads from the file into a caller-supplied buffer.

Reads up to @buffer_size@ bytes from the file's current position, advancing it by however many were read. Fewer bytes
than asked for is normal at the end of the file, and zero means there is nothing left; neither is an error. The file
must have been opened with @FILE_FLAG_READ@.

Use @duckdb_v2_file_read_at()@ to read at an explicit offset instead, which leaves the position alone and so can run
on several threads at once.

history:
- stable: v2.0.0


@file@: The file to read from.

@buffer@: A caller-owned buffer of at least @buffer_size@ bytes, receiving what was read.

@buffer_size@: The maximum number of bytes to read.

@bytes_read@: Receives how many bytes were actually read, which may be fewer than @buffer_size@ at the end of the
file.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_file_read"
    c_duckdb_v2_file_read :: DuckDBV2FileHandle -> Ptr () -> DuckDBV2Idx -> Ptr DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Writes a caller-supplied buffer to the file.

Writes up to @buffer_size@ bytes at the file's current position, advancing it by however many were written. The file
must have been opened with @FILE_FLAG_WRITE@ or @FILE_FLAG_APPEND@. Writes may be buffered; use
@duckdb_v2_file_sync()@ to force them out.

Use @duckdb_v2_file_write_at()@ to write at an explicit offset instead, which leaves the position alone and so can
run on several threads at once.

history:
- stable: v2.0.0


@file@: The file to write to.

@buffer@: A caller-owned buffer of at least @buffer_size@ bytes, holding what to write.

@buffer_size@: The number of bytes to write.

@bytes_written@: Receives how many bytes were actually written.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_file_write"
    c_duckdb_v2_file_write :: DuckDBV2FileHandle -> Ptr () -> DuckDBV2Idx -> Ptr DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Reads from a fixed offset, without moving the file's position.

Reads exactly @buffer_size@ bytes starting at @location@. Unlike @duckdb_v2_file_read()@, a short read is an error
rather than a result: reaching the end of the file before @buffer_size@ bytes fails, so there is no count to report
back. The file's read/write position is untouched, which is what makes this safe to call from several threads at once
-- provided the file was opened with @FILE_FLAG_PARALLEL_ACCESS@.

The file must have been opened with @FILE_FLAG_READ@.

history:
- stable: v2.0.0


@file@: The file to read from.

@buffer@: A caller-owned buffer of at least @buffer_size@ bytes, receiving what was read.

@buffer_size@: The number of bytes to read. All of them are read, or the call fails.

@location@: The absolute byte offset to read from, measured from the start of the file.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_file_read_at"
    c_duckdb_v2_file_read_at :: DuckDBV2FileHandle -> Ptr () -> DuckDBV2Idx -> DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Writes at a fixed offset, without moving the file's position.

Writes exactly @buffer_size@ bytes starting at @location@, extending the file when the offset is past its end. Unlike
@duckdb_v2_file_write()@ there is no count to report back: all of the bytes are written, or the call fails. The
file's read/write position is untouched, which is what makes this safe to call from several threads at once --
provided the file was opened with @FILE_FLAG_PARALLEL_ACCESS@, and that the threads write disjoint ranges.

The file must have been opened with @FILE_FLAG_WRITE@. Writes may be buffered; use @duckdb_v2_file_sync()@ to force
them out.

history:
- stable: v2.0.0


@file@: The file to write to.

@buffer@: A caller-owned buffer of at least @buffer_size@ bytes, holding what to write.

@buffer_size@: The number of bytes to write. All of them are written, or the call fails.

@location@: The absolute byte offset to write at, measured from the start of the file.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_file_write_at"
    c_duckdb_v2_file_write_at :: DuckDBV2FileHandle -> Ptr () -> DuckDBV2Idx -> DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the file's current read/write position, as a byte offset from the start of the file.

history:
- stable: v2.0.0


@file@: The file to query.

@position@: Receives the current position, as a byte offset from the start of the file.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_file_tell"
    c_duckdb_v2_file_tell :: DuckDBV2FileHandle -> Ptr DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the total size of the file in bytes.

history:
- stable: v2.0.0


@file@: The file to query.

@size@: Receives the total size of the file in bytes.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_file_size"
    c_duckdb_v2_file_size :: DuckDBV2FileHandle -> Ptr DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Moves the file's read/write position.

Sets the position to an absolute byte offset from the start of the file; subsequent reads and writes start there.
Seeking past the end is allowed, and reading from there yields nothing.

history:
- stable: v2.0.0


@file@: The file to seek within.

@position@: The absolute byte offset to seek to, measured from the start of the file.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_file_seek"
    c_duckdb_v2_file_seek :: DuckDBV2FileHandle -> DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Flushes buffered writes to persistent storage.

Forces anything still buffered out to storage, which is what makes writes durable across a crash or a process exit.
Closing or destroying the handle flushes as well.

history:
- stable: v2.0.0


@file@: The file to synchronize.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_file_sync"
    c_duckdb_v2_file_sync :: DuckDBV2FileHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Closes the file without destroying the handle.

Releases the operating-system resources behind the file, such as its descriptor. The handle itself stays valid and
must still be destroyed with @duckdb_v2_file_destroy()@, but it can no longer read, write or seek.

history:
- stable: v2.0.0


@file@: The file to close.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_file_close"
    c_duckdb_v2_file_close :: DuckDBV2FileHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Destroys the file, closing it if it is still open.

Null-safe: passing a null pointer or null handle is a no-op. The handle is set to null on return to prevent
double-destruction.

history:
- stable: v2.0.0


@file@: The file to destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_file_destroy"
    c_duckdb_v2_file_destroy :: Ptr DuckDBV2FileHandle -> IO DuckDBV2Error

{- | Retrieves the user data set on the function being bound, e.g. via @duckdb_v2_scalar_function_set_user_data()@.

history:
- stable: v2.0.0


@info@: The bind info handle.

@data@: Receives the user data pointer, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_function_bind_get_user_data"
    c_duckdb_v2_function_bind_get_user_data :: DuckDBV2FunctionBindInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets the function's "bind data" from the bind callback.

The bind data is stored with the bound call site and retrievable from every later callback. The opaque handle bundles
the pointer with an optional destructor, invoked when the bind data is no longer needed, and an optional equality
callback used when comparing two bound call sites; without one, pointer equality is used.

history:
- stable: v2.0.0


@info@: The bind info handle.

@data@: Opaque handle bundling the bind data pointer plus optional destructor and equality callbacks.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_function_bind_set_bind_data"
    c_duckdb_v2_function_bind_set_bind_data :: DuckDBV2FunctionBindInfoHandle -> Ptr DuckDBV2Opaque -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the number of arguments of the call site being bound, split into the four parts of the argument list.

The parts follow each other in this order: the positional-only and standard parameters, the arguments @*args@
received, the named-only parameters, and the arguments @**kwargs@ received. The argument at index @i@ of the
named-only part is therefore at index @positional_fixed + positional_variadic + i@. Valid indices for the other
argument functions are [0, the sum of the four counts). Every out-parameter may be NULL, in which case nothing is
written to it.

history:
- stable: v2.0.0


@info@: The bind info handle.

@positional_fixed@: Optional. Receives the number of positional-only and standard parameters.

@positional_variadic@: Optional. Receives the number of arguments @*args@ received, 0 when the signature has
none.

@named_fixed@: Optional. Receives the number of named-only parameters.

@named_variadic@: Optional. Receives the number of arguments @**kwargs@ received, 0 when the signature has none.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_function_bind_get_arg_count"
    c_duckdb_v2_function_bind_get_arg_count :: DuckDBV2FunctionBindInfoHandle -> Ptr DuckDBV2Idx -> Ptr DuckDBV2Idx -> Ptr DuckDBV2Idx -> Ptr DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the type of the argument at the given index.

Fails if the index is out of bounds. The returned type is owned by the caller and must be destroyed via
@duckdb_v2_logical_type_destroy()@.

history:
- stable: v2.0.0


@info@: The bind info handle.

@index@: The index of the argument.

@type@: Receives the argument type. Owned by the caller; destroy via @duckdb_v2_logical_type_destroy()@.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_function_bind_get_arg_type"
    c_duckdb_v2_function_bind_get_arg_type :: DuckDBV2FunctionBindInfoHandle -> DuckDBV2Idx -> Ptr DuckDBV2LogicalTypeHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Folds the argument at the given index to a constant value.

Fails if the argument is not constant, e.g. a column reference, or if the index is out of bounds. The arguments of a
table function are always constant. The resulting value may be NULL. The returned value is owned by the caller and
must be destroyed via @duckdb_v2_value_destroy()@.

history:
- stable: v2.0.0


@info@: The bind info handle.

@index@: The index of the argument.

@value@: Receives the constant value. Owned by the caller; destroy via @duckdb_v2_value_destroy()@.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_function_bind_get_arg_value"
    c_duckdb_v2_function_bind_get_arg_value :: DuckDBV2FunctionBindInfoHandle -> DuckDBV2Idx -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the name of the argument at the given index.

For an argument of a declared parameter, this is the parameter name. For an argument @**kwargs@ received, it is the
name the caller passed, which may be the name of a positional-only parameter. An argument @*args@ received has no
name, and yields an empty name. Fails if the index is out of bounds. The name is borrowed and valid only for the
duration of the callback.

history:
- stable: v2.0.0


@info@: The bind info handle.

@index@: The index of the argument.

@name@: Receives a borrowed view of the argument name. Valid only for the duration of the callback.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_function_bind_get_arg_name"
    c_duckdb_v2_function_bind_get_arg_name :: DuckDBV2FunctionBindInfoHandle -> DuckDBV2Idx -> Ptr DuckDBV2IdentifierT -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Looks up the index of an argument by name.

Names are matched case-insensitively, against the names a caller can pass an argument by: those of the standard and
named-only parameters, and those @**kwargs@ received. Positional-only parameters are skipped, so @f(1, x := 2)@ with
a positional-only @x@ finds the argument @**kwargs@ received. A name the call did not pass is not an error: @found@
receives false and @index@ is left untouched. A standard or named-only parameter is always found, as the call either
passed it or it carries its default value.

history:
- stable: v2.0.0


@info@: The bind info handle.

@name@: The name to look up. Borrowed for the call only.

@index@: Receives the index of the argument, if found.

@found@: Receives whether the call has an argument of that name.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_function_bind_get_arg_index"
    c_duckdb_v2_function_bind_get_arg_index :: DuckDBV2FunctionBindInfoHandle -> Ptr DuckDBV2IdentifierT -> Ptr DuckDBV2Idx -> Ptr CBool -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Adds a parameter to a signature.

The kind decides how a caller passes the argument; see @DUCKDB_V2_FUNCTION_PARAMETER_KIND@. The signature keeps its
parameters ordered by kind, and parameters of the same kind in the order they were added, so parameters of different
kinds can be added in any order. A default value makes the parameter optional; @*args@ and @**kwargs@ cannot have
one.

Registration fails when two parameters share a name, when the signature has more than one @*args@ or @**kwargs@
parameter, or when a standard parameter without a default value follows one with a default value.

history:
- stable: v2.0.0


@sig@: The signature to configure.

@name@: The parameter name. Borrowed and copied.

@type@: The parameter type. ANY accepts an argument of any type without casting it. Borrowed and copied.

@value@: Optional default value, borrowed and copied. Will be cast to the parameter type.

@kind@: How a caller passes the argument for the parameter.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_function_signature_add_parameter"
    c_duckdb_v2_function_signature_add_parameter :: DuckDBV2FunctionSignatureHandle -> Ptr DuckDBV2IdentifierT -> DuckDBV2LogicalTypeHandle -> DuckDBV2ValueHandle -> DuckDBV2FunctionParameterKind -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets the return type of a signature.

Sets the signature's return type. The type is borrowed and copied. Calling this again overwrites the previous return
type. An INVALID type is rejected with ERROR_INPUT_INVALID. Whether a concrete return type is required, and whether
ANY is accepted, depends on the function family and is enforced when the signature is registered with a builder.

history:
- stable: v2.0.0


@sig@: The signature to configure.

@type@: The return type. Borrowed and copied.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_function_signature_set_return_type"
    c_duckdb_v2_function_signature_set_return_type :: DuckDBV2FunctionSignatureHandle -> DuckDBV2LogicalTypeHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Renders a name as a SQL identifier, quoting and escaping only when required.

Produces the SQL text for the name: the name itself when it is already a legal bare identifier, or the name
double-quoted with interior double quotes doubled when it is a keyword or contains characters that require quoting.
This is the engine's own identifier rendering, so the result parses back to a name equal to the input.

Writes into a caller-supplied buffer, so nothing is allocated on the caller's behalf and nothing has to be freed.
Pass out_text = NULL to size the buffer without rendering into it: out_length then receives the length, and
out_capacity is ignored. With out_text != NULL, out_capacity must be at least out_length + 1, or the call returns
ERROR_INPUT_OBJECT_SIZE with out_length set to the required length and out_text left untouched.

out_length never counts the terminator, but a successful write always appends one, so the buffer is usable as a C
string.

history:
- stable: v2.0.0


@name@: The name to render. Borrowed for the call only.

@out_text@: Caller-owned buffer receiving the text plus a null terminator, or NULL to only report the required
length in out_length.

@out_capacity@: Bytes available in out_text, terminator included. Ignored when out_text is NULL.

@out_length@: Receives the text length excluding the null terminator — written on success and on
ERROR_INPUT_OBJECT_SIZE.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_identifier_render_quoted"
    c_duckdb_v2_identifier_render_quoted :: Ptr DuckDBV2IdentifierT -> Ptr CChar -> DuckDBV2Idx -> Ptr DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates an instance handle under the environment.

The handle starts with no database attached and with its instance not yet started; set startup options with
@duckdb_v2_instance_set_option()@, then attach a database with @duckdb_v2_instance_attach()@ and pick the default
with @duckdb_v2_instance_set_default()@, or connect with @duckdb_v2_connection_create()@. The caller destroys it via
@duckdb_v2_instance_destroy()@.

history:
- stable: v2.0.0


@env@: The environment.

@out_instance@: Receives the new instance handle.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_instance_create"
    c_duckdb_v2_instance_create :: DuckDBV2EnvironmentHandle -> Ptr DuckDBV2InstanceHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Destroys the instance handle.

Always succeeds. Every attached database is closed with the instance. The instance itself stays alive for as long as
any connection still references it; when the last reference drops, it is destroyed and its database files become
available to open again. On success the handle is set to null. Safe to call on an already-null slot.

history:
- stable: v2.0.0


@instance@: The instance handle to destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_instance_destroy"
    c_duckdb_v2_instance_destroy :: Ptr DuckDBV2InstanceHandle -> IO DuckDBV2Error

{- | Attaches a database to the instance, starting it if it has not started yet.

Attaches the database at @path@ exactly like @ATTACH \'path\'@:
  - @:memory:@ or an empty view attaches a fresh in-memory database named @memory@.
  - any other path attaches that file, creating it if it does not exist, under the name derived from the file's base
name. @name@ is the name to attach under, like @ATTACH ... AS name@; null, or an empty view, uses the name derived
from the path. Naming is how two files with the same base name, or @:memory:@ twice, attach side by side. @options@
may be null; it carries the @(KEY value)@ options of SQL @ATTACH@, for per-database options such as READ_ONLY,
BLOCK_SIZE or ENCRYPTION_KEY, and must have been created from this instance handle. @make_default@ makes the attached
database the default for connections created afterwards, exactly as a @duckdb_v2_instance_set_default()@ call right
after the attach would; otherwise the default is left alone. A path that is already attached on this or any other
instance of the environment returns ERROR_RESOURCE_IN_USE, and a name that is already attached fails. Options that
only apply at startup must be set before the first attach or connection.

history:
- stable: v2.0.0


@instance@: The instance handle.

@path@: Path to the database file, or @:memory:@ / an empty view for an in-memory database.

@name@: Optional. The name to attach under; null or an empty view for the name derived from the path. Borrowed
and copied.

@options@: Optional attach options created from @instance@, or null to attach with defaults under the derived
name.

@make_default@: Whether to make the attached database the default for connections created afterwards.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_instance_attach"
    c_duckdb_v2_instance_attach :: DuckDBV2InstanceHandle -> Ptr DuckDBV2Str -> Ptr DuckDBV2IdentifierT -> DuckDBV2AttachOptionsHandle -> CBool -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Detaches the database that was attached from @path@.

Detaches it like @DETACH@, checkpointing a file database first. Unlike SQL, the default database may be detached too;
new connections then start without a default until @duckdb_v2_instance_set_default()@ names another, and existing
connections bound to it keep it alive until their transaction on it ends and fail unqualified DDL afterwards. The
path is matched against the attached databases as @duckdb_v2_instance_attach()@ recorded it, so pass the path that
was passed to @duckdb_v2_instance_attach()@. Returns ERROR_INPUT_INVALID when no database attached from that path
exists. Connections that still reference the database keep it alive until they release it.

history:
- stable: v2.0.0


@instance@: The instance handle.

@path@: The path the database was attached from, or the name it is attached under.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_instance_detach"
    c_duckdb_v2_instance_detach :: DuckDBV2InstanceHandle -> Ptr DuckDBV2Str -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Makes the database that was attached from @path@ the default database for connections created from now on.

A connection binds to the default database when it is created, so this does not retarget existing connections: their
default stays what it was when they connected, or what they chose with @USE@. The default is where unqualified DDL
and unqualified table lookups that miss the temporary catalog go. It stays the default for new connections until
another @duckdb_v2_instance_set_default()@ call or until it is detached; a connection whose default has been detached
fails unqualified DDL with a message saying so until it selects another with @USE@. @path@ is either the path that
was passed to @duckdb_v2_instance_attach()@ or the name the database is attached under; a name match wins. Returns
ERROR_INPUT_INVALID when neither matches an attached database.

history:
- stable: v2.0.0


@instance@: The instance handle.

@path@: The path the database was attached from, or the name it is attached under.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_instance_set_default"
    c_duckdb_v2_instance_set_default :: DuckDBV2InstanceHandle -> Ptr DuckDBV2Str -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates an empty set of attach options for use with @duckdb_v2_instance_attach()@ on @instance@.

Starts empty. The caller destroys it via @duckdb_v2_attach_options_destroy()@; the options may be destroyed as soon
as the attach call returns, and reused for further attaches before that.

history:
- stable: v2.0.0


@instance@: The instance handle the options will be used with.

@out_options@: Receives the new options handle.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_attach_options_create"
    c_duckdb_v2_attach_options_create :: DuckDBV2InstanceHandle -> Ptr DuckDBV2AttachOptionsHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets one attach option, like a @(KEY value)@ entry of SQL @ATTACH@.

The key is matched case-insensitively, as an unquoted SQL identifier is. The setting is passed on as the text a
quoted SQL literal would produce: the engine casts the options it knows (READ_ONLY, RECOVERY_MODE, TYPE,
DEFAULT_TABLE, VACUUM_REBUILD_INDEXES, BLOCK_SIZE, ENCRYPTION_KEY, ...) and hands the rest to the storage extension
that ends up owning the database, which decides what they mean. Keys must be valid UTF-8; an unknown or ill-typed
option fails the attach. Both views are borrowed and copied. Setting the same key again replaces it.

history:
- stable: v2.0.0


@options@: The attach options.

@key@: The option name, e.g. READ_ONLY.

@setting@: The option value, in the textual form a quoted SQL literal would carry.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_attach_options_set"
    c_duckdb_v2_attach_options_set :: DuckDBV2AttachOptionsHandle -> Ptr DuckDBV2IdentifierT -> Ptr DuckDBV2Str -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Destroys an attach options handle.

On success the slot is set to null. Safe to call on an already-null slot.

history:
- stable: v2.0.0


@options@: The attach options to destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_attach_options_destroy"
    c_duckdb_v2_attach_options_destroy :: Ptr DuckDBV2AttachOptionsHandle -> IO DuckDBV2Error

{- | Sets a config option on the instance (GLOBAL scope).

Before the instance has started (no @duckdb_v2_instance_attach()@ or @duckdb_v2_connection_create()@ yet), the option
goes into the startup configuration: this is the only way to set an option that can only be chosen at startup, such
as access_mode or enable_external_access. Unknown names are kept for an extension to consume at startup; if none
does, startup fails with the unrecognized names. After startup, this is @SET GLOBAL name = setting@ and an unknown
name is rejected unless an extension that defines it can be autoloaded. Returns ERROR_INPUT_INVALID for an option
declared LOCAL_ONLY, and for a legacy option with no global setter.

history:
- stable: v2.0.0


@instance@: The instance handle.

@name@: Option name (canonical or alias).

@setting@: The setting, in the textual form SQL @SET@ accepts.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_instance_set_option"
    c_duckdb_v2_instance_set_option :: DuckDBV2InstanceHandle -> Ptr DuckDBV2IdentifierT -> Ptr DuckDBV2Str -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Reads a config option from the instance (GLOBAL scope) by name.

Allocates a fully populated option: canonical name, current GLOBAL setting, default setting, description, target
scope, and aliases. Before startup the current setting is the staged startup value, or the default. Aliases resolve
transparently — passing an alias returns the canonical option, with the alias listed in its alias array. An unknown
name returns ERROR_INPUT_INVALID; before startup that includes an option of an extension that has not loaded yet,
even if a setting for it has been staged. The caller destroys the returned option.

history:
- stable: v2.0.0


@instance@: The instance handle.

@name@: Option name (canonical or alias).

@out_option@: Receives the populated option handle.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_instance_get_option_by_name"
    c_duckdb_v2_instance_get_option_by_name :: DuckDBV2InstanceHandle -> Ptr DuckDBV2IdentifierT -> Ptr DuckDBV2OptionHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the number of config options registered on the instance.

Counts core options plus the extension options registered on this instance. Aliases are NOT counted separately; each
one is reachable through its canonical option's alias array.

history:
- stable: v2.0.0


@instance@: The instance handle.

@out_count@: Receives the option count.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_instance_get_option_count"
    c_duckdb_v2_instance_get_option_count :: DuckDBV2InstanceHandle -> Ptr DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Reads the config option at the given index from the instance.

Index space: [0, core_count) addresses core options, [core_count, total) extension options. The mapping is stable for
as long as no extension registers new options. An out-of-range index returns ERROR_INPUT_INVALID. The caller destroys
the returned option.

history:
- stable: v2.0.0


@instance@: The instance handle.

@index@: The option index, in [0, option_count).

@out_option@: Receives the populated option handle.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_instance_get_option_by_index"
    c_duckdb_v2_instance_get_option_by_index :: DuckDBV2InstanceHandle -> DuckDBV2Idx -> Ptr DuckDBV2OptionHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the version of the linked DuckDB library.

history:
- stable: v2.0.0


@out_version@: The version string.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_library_version"
    c_duckdb_v2_library_version :: Ptr DuckDBV2Str -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Writes a message to DuckDB's log.

The entry is attributed to the context's connection scope, so it carries that connection's ids. Whether it is
recorded at all is up to the database's log configuration: if logging is off, or level is below the configured
threshold, or log_type is not among the enabled types, the call succeeds and writes nothing. An empty log_type
selects the default type. Read entries back with @SELECT * FROM duckdb_logs@.

history:
- stable: v2.0.0


@ctx@: The context to log through.

@level@: Severity of the message.

@log_type@: The log type to record under, matched case-sensitively. Empty selects the default type.

@message@: The message body. Borrowed for the call only.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_context_log"
    c_duckdb_v2_context_log :: DuckDBV2ContextHandle -> DuckDBV2LogLevel -> Ptr DuckDBV2Str -> Ptr DuckDBV2Str -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Parses SQL text into a qualified name.

Applies the engine's qualified-name rules: dots separate parts, and a double-quoted part may contain dots and doubled
interior quotes. More than three parts and an unterminated quote are rejected with the parser's own error. Invalid
UTF-8 and text without at least one non-empty part are rejected with @ERROR_INPUT_INVALID@. When the parts are
already separate, build the name with @duckdb_v2_qname_create()@ rather than joining them and parsing the result.

history:
- stable: v2.0.0


@text@: The name text to parse. Borrowed for the call only.

@name@: On success, receives the qualified name. Owned by the caller; destroy via @duckdb_v2_qname_destroy()@.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_qname_parse"
    c_duckdb_v2_qname_parse :: Ptr DuckDBV2Str -> Ptr DuckDBV2QnameHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a qualified name from its parts.

The parts are ordered outermost first, so the last one is the object name. Between one and three parts are accepted
-- the engine qualifies at most catalog.schema.name today -- and every part must be non-empty: partial qualification
is expressed by passing fewer parts, never by empty placeholders. Zero parts, more than three, and an empty part are
all rejected with @ERROR_INPUT_INVALID@. The parts are borrowed and copied.

history:
- stable: v2.0.0


@parts@: An array of @part_count@ non-empty identifier views, outermost first.

@part_count@: The number of parts, between one and three.

@name@: On success, receives the qualified name. Owned by the caller; destroy via @duckdb_v2_qname_destroy()@.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_qname_create"
    c_duckdb_v2_qname_create :: Ptr DuckDBV2IdentifierT -> DuckDBV2Idx -> Ptr DuckDBV2QnameHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns how many parts the qualified name has.

Always at least one. Valid indices for @duckdb_v2_qname_get_part()@ are [0, count), and the object name is the part
at count - 1.

history:
- stable: v2.0.0


@name@: The qualified name.

@count@: Receives the number of parts.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_qname_get_part_count"
    c_duckdb_v2_qname_get_part_count :: DuckDBV2QnameHandle -> Ptr DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Borrows one part of the qualified name.

Parts are ordered outermost first, so the object name is the part at @duckdb_v2_qname_get_part_count()@ - 1. The view
is valid until the qualified name is destroyed. An index outside [0, count) is rejected with
@ERROR_INPUT_OUT_OF_RANGE@.

history:
- stable: v2.0.0


@name@: The qualified name.

@index@: Zero-based part index.

@part@: Receives a borrowed view of the part, valid until the qualified name is destroyed.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_qname_get_part"
    c_duckdb_v2_qname_get_part :: DuckDBV2QnameHandle -> DuckDBV2Idx -> Ptr DuckDBV2IdentifierT -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Renders the qualified name as SQL text.

Joins the parts with dots, quoting and escaping each one only where the identifier requires it, so the result parses
back through @duckdb_v2_qname_parse()@ to an equal name.

Writes into a caller-supplied buffer, so nothing is allocated on the caller's behalf and nothing has to be freed.
Pass out_text = NULL to size the buffer without rendering into it: out_length then receives the length, and
out_capacity is ignored. With out_text != NULL, out_capacity must be at least out_length + 1, or the call returns
ERROR_INPUT_OBJECT_SIZE with out_length set to the required length and out_text left untouched.

out_length never counts the terminator, but a successful write always appends one, so the buffer is usable as a C
string.

history:
- stable: v2.0.0


@name@: The qualified name to render.

@out_text@: Caller-owned buffer receiving the text plus a null terminator, or NULL to only report the required
length in out_length.

@out_capacity@: Bytes available in out_text, terminator included. Ignored when out_text is NULL.

@out_length@: Receives the text length excluding the null terminator — written on success and on
ERROR_INPUT_OBJECT_SIZE.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_qname_render"
    c_duckdb_v2_qname_render :: DuckDBV2QnameHandle -> Ptr CChar -> DuckDBV2Idx -> Ptr DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Compares two qualified names.

True when both have the same number of parts and every part matches case-insensitively, which is the engine's own
identifier equality: casing never distinguishes two names. This is the only way to compare names; two handles holding
equal names are still distinct pointers.

history:
- stable: v2.0.0


@left@: The first qualified name.

@right@: The second qualified name.

@result@: Receives whether the two names are equal.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_qname_equals"
    c_duckdb_v2_qname_equals :: DuckDBV2QnameHandle -> DuckDBV2QnameHandle -> Ptr CBool -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Hashes a qualified name.

Consistent with @duckdb_v2_qname_equals()@: names that compare equal hash equal, casing differences included. The
value is not stable across processes or library versions, so use it for in-process lookup tables only and never
persist it.

history:
- stable: v2.0.0


@name@: The qualified name to hash.

@hash@: Receives the hash value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_qname_hash"
    c_duckdb_v2_qname_hash :: DuckDBV2QnameHandle -> Ptr Word64 -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Destroys the qualified name, releasing its resources.

Null-safe: passing a null pointer or null handle is a no-op. The handle is set to null on return to prevent
double-destruction. Any part views borrowed from it become dangling.

history:
- stable: v2.0.0


@name@: The qualified name to destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_qname_destroy"
    c_duckdb_v2_qname_destroy :: Ptr DuckDBV2QnameHandle -> IO DuckDBV2Error

{- | Returns the number of fields in a schema.

history:
- stable: v2.0.0


@schema@: The schema.

@out_count@: Receives the field count.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_schema_get_count"
    c_duckdb_v2_schema_get_count :: DuckDBV2SchemaHandle -> Ptr DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Borrows the name and type of the field at the given index.

out_name and out_type are valid only until the schema is destroyed, and out_type must not be destroyed. An
out-of-range index is rejected with ERROR_INPUT_INVALID.

history:
- stable: v2.0.0


@schema@: The schema.

@index@: Zero-based field index.

@out_name@: Receives a borrowed view of the field name. Valid until the schema is destroyed.

@out_type@: Receives a borrowed field type. Valid until the schema is destroyed; do not destroy it.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_schema_get_field"
    c_duckdb_v2_schema_get_field :: DuckDBV2SchemaHandle -> DuckDBV2Idx -> Ptr DuckDBV2IdentifierT -> Ptr DuckDBV2LogicalTypeHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Destroys a schema handle.

Frees the handle along with the names and types it owns. On success the slot is set to null. Safe to call on an
already-null slot.

history:
- stable: v2.0.0


@schema@: The schema handle to destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_schema_destroy"
    c_duckdb_v2_schema_destroy :: Ptr DuckDBV2SchemaHandle -> IO DuckDBV2Error

{- | Tokenizes a SQL string into an iterator over its tokens.

Lexical tokenization, in the context of whatever grammar extensions are loaded on the given connection. Closing the
connection or changing settings afterwards does not affect the tokens. The SQL string is borrowed for the call only,
the caller may free it once this call returns. Whitespace is not a token. Malformed input is not an error;
@duckdb_v2_token_iterator_ends_unterminated()@ reports whether the input ended inside an open token.

*out_iterator is set to NULL on failure.

history:
- stable: v2.0.0


@conn@: The connection supplying the grammar.

@sql@: The SQL text. Borrowed for the call only; may contain interior null bytes. {NULL, 0} is the empty input.

@out_iterator@: Receives the new iterator handle. Destroy via token_iterator_destroy.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_tokenize_sql"
    c_duckdb_v2_tokenize_sql :: DuckDBV2ConnectionHandle -> Ptr DuckDBV2Str -> Ptr DuckDBV2TokenIteratorHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Yields the next token, or END_OF_INPUT once exhausted.

On success writes the token's class, byte offset and byte length into the out-parameters. The lexeme is the input
bytes [start, start + length). Once the input is exhausted, every call gives TOKEN_TYPE_END_OF_INPUT with start equal
to the input length and length 0. On failure out params are set to TOKEN_TYPE_INVALID, 0, 0.

history:
- stable: v2.0.0


@iterator@: The iterator to advance.

@out_type@: Receives the token's class, or TOKEN_TYPE_END_OF_INPUT once exhausted.

@out_start@: Receives the token's byte offset into the input; the input length for END_OF_INPUT.

@out_length@: Receives the token's byte length; 0 for END_OF_INPUT.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_token_iterator_next"
    c_duckdb_v2_token_iterator_next :: DuckDBV2TokenIteratorHandle -> Ptr DuckDBV2TokenType -> Ptr DuckDBV2Idx -> Ptr DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Reports whether the input ended before the closing delimiter of its last token.

True when the input ends inside an open string, quoted identifier, block comment or dollar-quoted string, or in a
line comment with no trailing newline; false otherwise, including for empty or whitespace-only input. Equivalently,
the last token @duckdb_v2_token_iterator_next()@ yields before TOKEN_TYPE_END_OF_INPUT is the open one, so its class
and lexeme say which delimiter is missing. A property of the input, not of the iterator position: the answer is the
same before, during and after draining. The only failure is a null argument.

history:
- stable: v2.0.0


@iterator@: The iterator.

@out_ends_unterminated@: Receives whether the input ended inside an open token.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_token_iterator_ends_unterminated"
    c_duckdb_v2_token_iterator_ends_unterminated :: DuckDBV2TokenIteratorHandle -> Ptr CBool -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Destroys a token iterator handle.

history:
- stable: v2.0.0


@iterator@: The iterator to destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_token_iterator_destroy"
    c_duckdb_v2_token_iterator_destroy :: Ptr DuckDBV2TokenIteratorHandle -> IO DuckDBV2Error

{- | Returns the internal representation kind: flat, constant, or dictionary.

Reports VECTOR_TYPE_OTHER for FSST / SEQUENCE / SHREDDED vectors, which must be passed through vector_flatten before
vector_get_view will read them.

history:
- stable: v2.0.0


@vector@: The vector.

@out_type@: Receives the representation kind.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_vector_get_vector_type"
    c_duckdb_v2_vector_get_vector_type :: DuckDBV2VectorHandle -> Ptr DuckDBV2VectorType -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the vector's logical type.

The returned logical type is the same type that was passed to vector_reference, or the type that was set on creation.
It is caller-owned; destroy it via logical_type_destroy.

history:
- stable: v2.0.0


@vector@: The vector.

@out_type@: Receives the caller-owned logical type handle.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_vector_get_logical_type"
    c_duckdb_v2_vector_get_logical_type :: DuckDBV2VectorHandle -> Ptr DuckDBV2LogicalTypeHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Reads a vector as a unified view of data, validity, selection, and count.

Populates out_view with borrowed pointers, valid until the owning chunk is destroyed. view.data addresses the
vector's elements, to be cast to the layout its logical type implies: a typed leaf array for the primitives,
hugeint_t / uhugeint_t / interval_t for those kinds, bytes for VARCHAR / BLOB / BIT / BIGNUM, list_entry for LIST and
MAP, and nothing at all for ARRAY / STRUCT / UNION, whose data lives in their children.

DECIMAL and ENUM are typed leaf arrays too, over a storage tier that follows from the type; both mappings are
contract. A DECIMAL of width <= 4 stores int16, <= 9 int32, <= 18 int64, and <= 38 int128, boundaries inclusive, with
the width and scale available from logical_type_get_param. An ENUM stores its dictionary indices as uint8 for a
dictionary of at most 255 entries, uint16 for at most 65535, and uint32 beyond that, boundaries inclusive and an
empty dictionary uint8; logical_type_get_param resolves an index to its entry.

Rejects VECTOR_TYPE_OTHER; call vector_flatten first.

Reading a DICTIONARY vector has one side effect: the dictionary's underlying child is flattened in place if it is not
already FLAT. The parent vector stays DICTIONARY, and only the child's storage shape changes, but any pointer
previously borrowed into that child — from an earlier vector_get_view on a vector aliasing the same buffer, say — is
invalidated. This is the only mutation vector_get_view performs; compare vector_flatten, which materializes the whole
vector to FLAT.

history:
- stable: v2.0.0


@vector@: The vector to read.

@out_view@: Receives the view. Its pointers are borrowed and valid until the owning chunk is destroyed.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_vector_get_view"
    c_duckdb_v2_vector_get_view :: DuckDBV2VectorHandle -> Ptr DuckDBV2VectorView -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the number of elements in the vector.

The size is the number of logical elements the vector represents, which for a nested kind can differ from the size of
its children. An ARRAY vector of size 10 whose array size is 3, for instance, has 10 logical elements while its
single child vector has 30.

history:
- stable: v2.0.0


@vector@: The vector.

@out_size@: Receives the number of elements.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_vector_get_size"
    c_duckdb_v2_vector_get_size :: DuckDBV2VectorHandle -> Ptr DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets the number of elements in the vector.

The counterpart of vector_get_size: it declares how many logical elements the vector now holds. It reserves enough
space for size logical elements.

history:
- stable: v2.0.0


@vector@: The vector.

@size@: The new number of logical elements.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_vector_set_size"
    c_duckdb_v2_vector_set_size :: DuckDBV2VectorHandle -> DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Reads one cell of a vector as an owned value.

The total fallback reader. It reads any logical row of any vector representation without flattening — FLAT, CONSTANT,
DICTIONARY, and the compressed kinds the view getter rejects — and covers every type kind, including those with no
committed view layout (today VARIANT and GEOMETRY), where it is the only way to reach a cell.

Each call allocates one owned value, which makes this a single-cell bridge rather than a per-row loop primitive; use
vector_get_view on hot paths. The row index is logical and bounds-checked against vector_get_size. The returned value
is caller-owned; destroy it via value_destroy.

history:
- stable: v2.0.0


@vector@: The vector to read from.

@row@: The logical row index, in [0, vector_get_size).

@out_value@: Receives the owned cell value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_vector_get_value"
    c_duckdb_v2_vector_get_value :: DuckDBV2VectorHandle -> DuckDBV2Idx -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Writes one cell of a FLAT vector from a value.

The total fallback writer. It covers every type kind, including those with no committed view layout (today VARIANT
and GEOMETRY), where it is the only way to write a cell. The value is borrowed and copied in, then cast to the
vector's type through the non-strict default cast; a cast failure surfaces from the call. A NULL value clears the
row's validity.

Requires a FLAT vector: constant, dictionary, and compressed representations are not row-addressable, so call
vector_flatten first. Each call performs one value write, which makes this a single-cell bridge rather than a per-row
loop primitive; use the typed mutable-data paths on hot paths. The row index is logical and bounds-checked against
vector_get_size.

history:
- stable: v2.0.0


@vector@: The vector to write to.

@row@: The logical row index, in [0, vector_get_size).

@value@: The borrowed value to write. Copied in; cast to the vector's type.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_vector_set_value"
    c_duckdb_v2_vector_set_value :: DuckDBV2VectorHandle -> DuckDBV2Idx -> DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns a mutable pointer to the data of a FLAT or CONSTANT vector.

Any other representation returns ERROR_INPUT_INVALID. The pointer is valid until the owning chunk is destroyed, and
the vector's storage shape must not change — through a flatten, say — while it is in use.

When writing VARCHAR values, the caller must ensure that the bytes contain valid UTF-8.

history:
- stable: v2.0.0


@vector@: The vector.

@out_data@: Receives a mutable pointer to the data.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_vector_get_data_mutable"
    c_duckdb_v2_vector_get_data_mutable :: DuckDBV2VectorHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Forces a vector into FLAT representation, in place.

A no-op for a vector that is already FLAT. For a CONSTANT, DICTIONARY, FSST, SEQUENCE, or SHREDDED vector it
materializes the per-row data and switches the representation to FLAT. Call it before vector_get_view if you would
rather not handle CONSTANT and DICTIONARY semantics in the view; it is required for any vector reporting
VECTOR_TYPE_OTHER.

history:
- stable: v2.0.0


@vector@: The vector to flatten.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_vector_flatten"
    c_duckdb_v2_vector_flatten :: DuckDBV2VectorHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Repoints a vector at another vector's data, without copying.

@vector@ takes on the storage of @source@: no data moves, and the two alias the same buffers until one of them is
reset or re-referenced. Works for any type, including nested types. The source's logical type must equal the
vector's; a mismatch returns ERROR_INPUT_INVALID. The source's data must outlive every read of @vector@. Use it to
hand an already-materialized vector straight to an output vector without a per-row copy.

history:
- stable: v2.0.0


@vector@: The vector to repoint at the source's data.

@source@: The vector whose data to reference. Its logical type must equal the vector's.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_vector_reference"
    c_duckdb_v2_vector_reference :: DuckDBV2VectorHandle -> DuckDBV2VectorHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Turns the vector into a CONSTANT vector holding the given value.

Afterwards the vector holds a single element that applies to every logical row; write it through
vector_get_data_mutable, which returns a pointer to that one element. For a STRUCT vector the change propagates to
every child. The value's logical type must equal the vector's; a mismatch returns ERROR_INPUT_INVALID.

history:
- stable: v2.0.0


@vector@: The vector to make constant.

@value@: The value every row takes. Its logical type must equal the vector's.

@count@: The number of elements in the vector.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_vector_make_constant"
    c_duckdb_v2_vector_make_constant :: DuckDBV2VectorHandle -> DuckDBV2ValueHandle -> DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets a vector to a SEQUENCE representation.

Afterwards the vector represents the arithmetic sequence start, start+increment, start+2*increment, ... over count
elements. No data pointer is involved; the three parameters describe the sequence completely.

history:
- stable: v2.0.0


@vector@: The vector to turn into a sequence.

@start@: The first value of the sequence.

@increment@: The step between consecutive values.

@count@: The number of elements in the sequence.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_vector_make_sequence"
    c_duckdb_v2_vector_make_sequence :: DuckDBV2VectorHandle -> Int64 -> Int64 -> DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets one row of a FLAT vector to NULL, nested children included.

Clears the row's validity and broadcasts the NULL recursively to every descendant element under that row — STRUCT
fields, and ARRAY elements strided by the array size — maintaining DuckDB's nested NULL invariant: when a STRUCT or
ARRAY row is NULL, every descendant element under it must be NULL too. LIST children are left untouched, since their
consumers gate on the list's own validity. Which kinds propagate is normative DuckDB behavior that evolves with the
type system, which makes this the total NULL write path: equivalent to vector_set_value with a NULL value, without
the per-call value allocation.

Requires a FLAT vector: constant, dictionary, and compressed representations are not row-addressable, so call
vector_flatten first. The row index is logical and bounds-checked against vector_get_size.

history:
- stable: v2.0.0


@vector@: The vector to write to.

@row@: The logical row index, in [0, vector_get_size).

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_vector_set_null"
    c_duckdb_v2_vector_set_null :: DuckDBV2VectorHandle -> DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns a mutable pointer to the validity mask of a FLAT vector.

Allocates the mask on first use, so the returned pointer is always non-null. Each uint64_t word covers 64 rows: bit N
of word W is row W*64+N. A set bit means valid; a cleared bit means NULL.

A raw mask write marks only this vector's rows. Clearing a STRUCT or ARRAY parent bit leaves the descendant elements
untouched, which violates DuckDB's nested NULL invariant — a NULL STRUCT or ARRAY row requires NULL descendants — so
set nested rows NULL through vector_set_null instead. When writing nested masks raw anyway, invalidate the child
masks up front and mark elements valid as the values are written.

history:
- stable: v2.0.0


@vector@: The vector.

@out_validity@: Receives a mutable pointer to the validity mask.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_vector_flat_get_validity_mutable"
    c_duckdb_v2_vector_flat_get_validity_mutable :: DuckDBV2VectorHandle -> Ptr (Ptr Word64) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets the validity of a CONSTANT vector.

A constant vector holds a single element, so one flag decides whether every row reads as the value or as NULL.

history:
- stable: v2.0.0


@vector@: The constant vector.

@validity@: True to mark the vector's single element valid, false to mark it NULL.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_vector_constant_set_valid"
    c_duckdb_v2_vector_constant_set_valid :: DuckDBV2VectorHandle -> CBool -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Borrows a string-backed vector's arena for writing.

Allocates the arena on first use. Valid for VARCHAR, BLOB, BIT, and BIGNUM vectors, and returns ERROR_INPUT_INVALID
for anything else. This call is the single string-ness check, which is why arena_allocate itself performs none. The
arena is borrowed — never destroy it — and stays valid until the vector is flattened, reallocated, or destroyed.

history:
- stable: v2.0.0


@vector@: The string-backed vector whose arena to borrow.

@out_arena@: Receives the borrowed arena handle.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_vector_get_arena"
    c_duckdb_v2_vector_get_arena :: DuckDBV2VectorHandle -> Ptr DuckDBV2ArenaHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the number of child vectors a nested vector exposes.

1 for LIST, MAP and ARRAY; the field count for STRUCT; member_count + 1 for UNION, the tag plus the members; and 0
for any non-nested kind. vector_get_child documents what each index addresses.

history:
- stable: v2.0.0


@vector@: The vector.

@out_count@: Receives the number of child vectors.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_vector_get_child_count"
    c_duckdb_v2_vector_get_child_count :: DuckDBV2VectorHandle -> Ptr DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Borrows a child vector by index.

What each index addresses, by kind: LIST and ARRAY expose [0] = the elements; MAP [0] = the entries, a STRUCT(key,
value) whose children are the keys and the values; STRUCT and TUPLE [i] = field i; and UNION [0] = the tag with
[1..N] = the members. Note that value_get_child diverges on UNION, exposing only the active member.

The returned child is borrowed and lives as long as the owning chunk. Returns ERROR_INPUT_INVALID if the vector has
no children, or if the index is out of range.

history:
- stable: v2.0.0


@vector@: The nested vector to descend into.

@index@: The child index, in [0, vector_get_child_count).

@out_child@: Receives the borrowed child vector.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_vector_get_child"
    c_duckdb_v2_vector_get_child :: DuckDBV2VectorHandle -> DuckDBV2Idx -> Ptr DuckDBV2VectorHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Decodes BIGNUM storage bytes into a caller-supplied magnitude buffer + sign flag.

in_data and in_length are the value's raw storage bytes, exactly as they sit in a vector's bignum payload or as
value_get_blob reports them for a BIGNUM value. Those bytes are opaque: the storage encoding is not a committed part
of this API, and this function is the only thing that interprets them.

Writes the big-endian magnitude bytes into out_data and the sign flag into out_is_negative; reconstruct the integer
as (-1)**out_is_negative * unsigned_big_endian(out_data[0..out_length]). Materializing the bytes is unavoidable,
since a negative bignum stores its magnitude bit-inverted, but the buffer belongs to the caller: this function never
allocates and hands back nothing to free.

Pass out_data = NULL to size the buffer without decoding. out_length then receives the exact number of magnitude
bytes and out_is_negative the sign, and out_capacity is ignored. The size comes straight out of the storage header,
so the query is O(1) and exact — a size call followed by a decode call never needs a retry loop.

With out_data != NULL, out_capacity must be at least out_length bytes, or the call returns ERROR_INPUT_OBJECT_SIZE
with out_length set to the required size and out_data left untouched. On success exactly out_length bytes are
written, and the rest of the buffer is not modified.

Returns ERROR_INPUT_INVALID if in_data is NULL, or if in_length is too short to hold a bignum header.

history:
- stable: v2.0.0


@in_data@: The value's raw BIGNUM storage bytes.

@in_length@: Size of in_data in bytes.

@out_data@: Caller-owned buffer receiving the magnitude bytes, or NULL to only report the required size in
out_length.

@out_capacity@: Bytes available in out_data. Ignored when out_data is NULL.

@out_length@: Receives the magnitude size in bytes, written on success and on ERROR_INPUT_OBJECT_SIZE.

@out_is_negative@: Receives true if the value is negative, false otherwise.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_bignum_decode"
    c_duckdb_v2_bignum_decode :: Ptr Word8 -> DuckDBV2Idx -> Ptr Word8 -> DuckDBV2Idx -> Ptr DuckDBV2Idx -> Ptr CBool -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Encodes a magnitude + sign flag into BIGNUM storage bytes.

The inverse of bignum_decode. in_data and in_length are the big-endian magnitude bytes and is_negative the sign;
out_data receives the raw storage bytes for that integer, in the form a BIGNUM vector payload expects. Those bytes
are opaque: the storage encoding is not a committed part of this API.

Pass out_data = NULL to size the buffer without encoding. out_length then receives the exact number of storage bytes,
and out_capacity is ignored. The size is a fixed function of in_length, so the query is O(1) and exact.

With out_data != NULL, out_capacity must be at least out_length bytes, or the call returns ERROR_INPUT_OBJECT_SIZE
with out_length set to the required size and out_data left untouched. On success exactly out_length bytes are
written, and the rest of the buffer is not modified.

The magnitude must be canonical, matching what bignum_decode produces: in_length >= 1 with no leading zero bytes, so
the value zero is the single byte 0x00 with is_negative = false. A NULL in_data, an in_length of 0, a leading zero
byte, or a negative zero returns ERROR_INPUT_INVALID; a magnitude beyond the maximum bignum width returns
ERROR_INPUT_OUT_OF_RANGE.

history:
- stable: v2.0.0


@in_data@: Big-endian magnitude bytes, with no leading zero bytes.

@in_length@: Size of in_data in bytes. Must be >= 1.

@is_negative@: True to encode a negative value.

@out_data@: Caller-owned buffer receiving the storage bytes, or NULL to only report the required size in
out_length.

@out_capacity@: Bytes available in out_data. Ignored when out_data is NULL.

@out_length@: Receives the storage size in bytes — written on success and on ERROR_INPUT_OBJECT_SIZE.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_bignum_encode"
    c_duckdb_v2_bignum_encode :: Ptr Word8 -> DuckDBV2Idx -> CBool -> Ptr Word8 -> DuckDBV2Idx -> Ptr DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Validates all text->len bytes as UTF-8, including bytes after embedded NUL characters.

Returns ERROR_INPUT_INVALID if any of:
- text is NULL.
- text->ptr is NULL and text->len is nonzero.
- The input contains malformed UTF-8.

A view with a NULL ptr and zero length is valid.

history:
- stable: v2.0.0


@text@: The bytes to validate.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_validate_utf8"
    c_duckdb_v2_validate_utf8 :: Ptr DuckDBV2Str -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a new aggregate function that will be registered on the connection's database.

The function starts out empty: configure it with the setter functions (e.g.
@duckdb_v2_aggregate_function_set_name()@, @duckdb_v2_aggregate_function_set_update_callback()@, etc.) and the
signature obtained via @duckdb_v2_aggregate_function_get_signature()@, then make it available with
@duckdb_v2_aggregate_function_register()@. The caller owns the returned handle and must destroy it with
@duckdb_v2_aggregate_function_destroy()@, also after registration.

history:
- stable: v2.0.0


@connection@: The connection to create the function in.

@function@: On success, receives the newly created aggregate function. Owned by the caller.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_aggregate_function_create_with_connection"
    c_duckdb_v2_aggregate_function_create_with_connection :: DuckDBV2ConnectionHandle -> Ptr DuckDBV2AggregateFunctionHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a new aggregate function that will be registered on the loading extension's database.

Use this from an extension load callback, where an extension handle is available. The function starts out empty:
configure it with the setter functions (e.g. @duckdb_v2_aggregate_function_set_name()@,
@duckdb_v2_aggregate_function_set_update_callback()@, etc.) and the signature obtained via
@duckdb_v2_aggregate_function_get_signature()@, then make it available with
@duckdb_v2_aggregate_function_register()@. The caller owns the returned handle and must destroy it with
@duckdb_v2_aggregate_function_destroy()@, also after registration.

history:
- stable: v2.0.0


@extension@: The extension to create the function in.

@function@: On success, receives the newly created aggregate function. Owned by the caller.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_aggregate_function_create_with_extension"
    c_duckdb_v2_aggregate_function_create_with_extension :: DuckDBV2ExtensionHandle -> Ptr DuckDBV2AggregateFunctionHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets the name of the aggregate function.

The name is borrowed and copied. Calling this again replaces the previous name. A name must be set before
registration.

history:
- stable: v2.0.0


@function@: The function to set the name of.

@name@: The name to set. Borrowed and copied.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_aggregate_function_set_name"
    c_duckdb_v2_aggregate_function_set_name :: DuckDBV2AggregateFunctionHandle -> Ptr DuckDBV2IdentifierT -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the function's signature so it can be configured.

Add parameters with @duckdb_v2_function_signature_add_parameter()@ and set the return type with
@duckdb_v2_function_signature_set_return_type()@. The signature is modified in place; the function must be given a
signature with a return type before registration.

history:
- stable: v2.0.0


@function@: The function to get the signature of.

@sig@: The returned signature. Borrowed and valid for the lifetime of the function handle.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_aggregate_function_get_signature"
    c_duckdb_v2_aggregate_function_get_signature :: DuckDBV2AggregateFunctionHandle -> Ptr DuckDBV2FunctionSignatureHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets arbitrary user data on the aggregate function.

Associates an opaque pointer with the function, retrievable from each callback via its user data accessor (e.g.
@duckdb_v2_function_bind_get_user_data()@, @duckdb_v2_aggregate_function_update_get_user_data()@, etc.). The opaque
handle bundles the pointer with an optional destructor, invoked when the data is no longer needed.

history:
- stable: v2.0.0


@function@: The function to set the user data of.

@data@: Opaque handle bundling the user data pointer plus an optional destructor.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_aggregate_function_set_user_data"
    c_duckdb_v2_aggregate_function_set_user_data :: DuckDBV2AggregateFunctionHandle -> Ptr DuckDBV2Opaque -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets a property on the aggregate function.

Configures a function property that influences planning and execution, such as stability, NULL handling, fallibility,
collation handling, or the aggregate-specific order/DISTINCT dependence. The value must be one of the
@DUCKDB_V2_FUNCTION_PROPERTY_VALUE@ entries belonging to @key@; passing a value that does not belong to the key, or a
key that is not valid for aggregate functions, results in an error.

history:
- stable: v2.0.0


@function@: The function to set the property of.

@key@: The property to set.

@value@: The value to set for the property. Must belong to @key@.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_aggregate_function_set_property"
    c_duckdb_v2_aggregate_function_set_property :: DuckDBV2AggregateFunctionHandle -> DuckDBV2FunctionPropertyKey -> DuckDBV2FunctionPropertyValue -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets the optional bind callback of the aggregate function.

The bind callback is invoked during query planning for each call site of the function. Through its
@duckdb_v2_function_bind_info_handle@ it can inspect the argument types and constant argument values and set "bind
data" that is shared with the other callbacks. Through its @duckdb_v2_aggregate_function_bind_info_handle@ it can set
a concrete return type.

history:
- stable: v2.0.0


@function@: The function to set the bind callback of.

@callback@: The bind callback to set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_aggregate_function_set_bind_callback"
    c_duckdb_v2_aggregate_function_set_bind_callback :: DuckDBV2AggregateFunctionHandle -> FunPtr DuckDBV2AggregateFunctionBindCallbackFn -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets the size callback of the aggregate function.

The size callback reports the size of a single aggregate state in bytes via
@duckdb_v2_aggregate_function_size_set_state_size()@; DuckDB uses it to allocate state memory. A size callback must
be set before registration.

history:
- stable: v2.0.0


@function@: The function to set the size callback of.

@callback@: The size callback to set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_aggregate_function_set_size_callback"
    c_duckdb_v2_aggregate_function_set_size_callback :: DuckDBV2AggregateFunctionHandle -> FunPtr DuckDBV2AggregateFunctionSizeCallbackFn -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets the init callback of the aggregate function.

The init callback initializes freshly allocated aggregate states in place, obtained via
@duckdb_v2_aggregate_function_init_get_states()@. An init callback must be set before registration.

history:
- stable: v2.0.0


@function@: The function to set the init callback of.

@callback@: The init callback to set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_aggregate_function_set_init_callback"
    c_duckdb_v2_aggregate_function_set_init_callback :: DuckDBV2AggregateFunctionHandle -> FunPtr DuckDBV2AggregateFunctionInitCallbackFn -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets the update callback of the aggregate function.

The update callback is invoked during query execution with a batch of input rows: row i of every argument vector must
be aggregated into state i of the state array. An update callback must be set before registration.

history:
- stable: v2.0.0


@function@: The function to set the update callback of.

@callback@: The update callback to set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_aggregate_function_set_update_callback"
    c_duckdb_v2_aggregate_function_set_update_callback :: DuckDBV2AggregateFunctionHandle -> FunPtr DuckDBV2AggregateFunctionUpdateCallbackFn -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets the combine callback of the aggregate function.

The combine callback merges partial aggregate states computed in parallel: source state i must be combined into
target state i. A combine callback must be set before registration.

history:
- stable: v2.0.0


@function@: The function to set the combine callback of.

@callback@: The combine callback to set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_aggregate_function_set_combine_callback"
    c_duckdb_v2_aggregate_function_set_combine_callback :: DuckDBV2AggregateFunctionHandle -> FunPtr DuckDBV2AggregateFunctionCombineCallbackFn -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets the finalize callback of the aggregate function.

The finalize callback produces the function's result: state i must be finalized into result row offset + i of the
result vector. A finalize callback must be set before registration.

history:
- stable: v2.0.0


@function@: The function to set the finalize callback of.

@callback@: The finalize callback to set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_aggregate_function_set_finalize_callback"
    c_duckdb_v2_aggregate_function_set_finalize_callback :: DuckDBV2AggregateFunctionHandle -> FunPtr DuckDBV2AggregateFunctionFinalizeCallbackFn -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets the destroy callback for the aggregate function. Optional: only needed when aggregate states own resources that
must be released. The callback runs on states that are discarded without being finalized; it must not fail, an error
reported through its error slot is ignored.

history:
- stable: v2.0.0


@function@: The function to set the destroy callback of.

@callback@: The destroy callback to set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_aggregate_function_set_destroy_callback"
    c_duckdb_v2_aggregate_function_set_destroy_callback :: DuckDBV2AggregateFunctionHandle -> FunPtr DuckDBV2AggregateFunctionDestroyCallbackFn -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets the concrete return type of the call site being bound.

Overrides the return type declared in the signature. Register the function with an ANY return type and set the
concrete type here, derived from the argument types, to build a function whose result type depends on its input. The
type is borrowed and copied.

history:
- stable: v2.0.0


@info@: The bind info handle.

@return_type@: The return type to set. Borrowed for the call only.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_aggregate_function_bind_set_return_type"
    c_duckdb_v2_aggregate_function_bind_set_return_type :: DuckDBV2AggregateFunctionBindInfoHandle -> DuckDBV2LogicalTypeHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the user data set via @duckdb_v2_aggregate_function_set_user_data()@.

history:
- stable: v2.0.0


@info@: The size info handle.

@data@: Receives the user data pointer, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_aggregate_function_size_get_user_data"
    c_duckdb_v2_aggregate_function_size_get_user_data :: DuckDBV2AggregateFunctionSizeInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the bind data set by the function's bind callback.

history:
- stable: v2.0.0


@info@: The size info handle.

@data@: Receives the bind data pointer, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_aggregate_function_size_get_bind_data"
    c_duckdb_v2_aggregate_function_size_get_bind_data :: DuckDBV2AggregateFunctionSizeInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets the size of a single aggregate state, in bytes. The size callback must set this; DuckDB uses it to allocate
state memory.

history:
- stable: v2.0.0


@info@: The size info handle.

@size@: The size of a single aggregate state, in bytes.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_aggregate_function_size_set_state_size"
    c_duckdb_v2_aggregate_function_size_set_state_size :: DuckDBV2AggregateFunctionSizeInfoHandle -> DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the user data set via @duckdb_v2_aggregate_function_set_user_data()@.

history:
- stable: v2.0.0


@info@: The init info handle.

@data@: Receives the user data pointer, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_aggregate_function_init_get_user_data"
    c_duckdb_v2_aggregate_function_init_get_user_data :: DuckDBV2AggregateFunctionInitInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the bind data set by the function's bind callback.

history:
- stable: v2.0.0


@info@: The init info handle.

@data@: Receives the bind data pointer, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_aggregate_function_init_get_bind_data"
    c_duckdb_v2_aggregate_function_init_get_bind_data :: DuckDBV2AggregateFunctionInitInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns how many aggregate states this invocation must initialize. This is the length of the array returned by
@duckdb_v2_aggregate_function_init_get_states()@.

history:
- stable: v2.0.0


@info@: The init info handle.

@count@: Receives the number of states.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_aggregate_function_init_get_state_count"
    c_duckdb_v2_aggregate_function_init_get_state_count :: DuckDBV2AggregateFunctionInitInfoHandle -> Ptr DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the array of aggregate states to initialize, one pointer per state. Each state points to uninitialized memory
of the size reported by the size callback; the init callback must initialize all of them in place.

history:
- stable: v2.0.0


@info@: The init info handle.

@states@: Receives the array of aggregate state pointers. Borrowed; valid only for the duration of the callback.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_aggregate_function_init_get_states"
    c_duckdb_v2_aggregate_function_init_get_states :: DuckDBV2AggregateFunctionInitInfoHandle -> Ptr (Ptr (Ptr ())) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the user data set via @duckdb_v2_aggregate_function_set_user_data()@.

history:
- stable: v2.0.0


@info@: The update info handle.

@data@: Receives the user data pointer, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_aggregate_function_update_get_user_data"
    c_duckdb_v2_aggregate_function_update_get_user_data :: DuckDBV2AggregateFunctionUpdateInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the bind data set by the function's bind callback.

history:
- stable: v2.0.0


@info@: The update info handle.

@data@: Receives the bind data pointer, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_aggregate_function_update_get_bind_data"
    c_duckdb_v2_aggregate_function_update_get_bind_data :: DuckDBV2AggregateFunctionUpdateInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns how many input rows this invocation carries. This is the length of the argument vectors and of the state
array: row i of every argument vector must be aggregated into state i.

history:
- stable: v2.0.0


@info@: The update info handle.

@count@: Receives the number of rows.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_aggregate_function_update_get_row_count"
    c_duckdb_v2_aggregate_function_update_get_row_count :: DuckDBV2AggregateFunctionUpdateInfoHandle -> Ptr DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns how many argument vectors this invocation carries, split into the four parts of the argument list.

The parts and their order are those the bind callback saw through @duckdb_v2_function_bind_get_arg_count()@, and the
vectors are at the same indices. Valid indices for @duckdb_v2_aggregate_function_update_get_arg()@ are [0, the sum of
the four counts). Every out-parameter may be NULL, in which case nothing is written to it.

history:
- stable: v2.0.0


@info@: The update info handle.

@positional_fixed@: Optional. Receives the number of positional-only and standard parameters.

@positional_variadic@: Optional. Receives the number of arguments @*args@ received, 0 when the signature has
none.

@named_fixed@: Optional. Receives the number of named-only parameters.

@named_variadic@: Optional. Receives the number of arguments @**kwargs@ received, 0 when the signature has none.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_aggregate_function_update_get_arg_count"
    c_duckdb_v2_aggregate_function_update_get_arg_count :: DuckDBV2AggregateFunctionUpdateInfoHandle -> Ptr DuckDBV2Idx -> Ptr DuckDBV2Idx -> Ptr DuckDBV2Idx -> Ptr DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the argument vector at the given index.

The index is the one the bind callback used for the argument, e.g. the index
@duckdb_v2_function_bind_get_arg_index()@ found for a name. The vector holds the argument's values for the current
batch; use @duckdb_v2_aggregate_function_update_get_row_count()@ for the number of rows. Fails if the index is out of
bounds. Borrowed; valid only for the duration of the callback.

history:
- stable: v2.0.0


@info@: The update info handle.

@index@: The index of the argument.

@vector@: Receives the borrowed argument vector.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_aggregate_function_update_get_arg"
    c_duckdb_v2_aggregate_function_update_get_arg :: DuckDBV2AggregateFunctionUpdateInfoHandle -> DuckDBV2Idx -> Ptr DuckDBV2VectorHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the array of aggregate states to update, one pointer per input row: row i of every argument vector must be
aggregated into state i. Different rows may point to the same state.

history:
- stable: v2.0.0


@info@: The update info handle.

@states@: Receives the array of aggregate state pointers. Borrowed; valid only for the duration of the callback.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_aggregate_function_update_get_states"
    c_duckdb_v2_aggregate_function_update_get_states :: DuckDBV2AggregateFunctionUpdateInfoHandle -> Ptr (Ptr (Ptr ())) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the user data set via @duckdb_v2_aggregate_function_set_user_data()@.

history:
- stable: v2.0.0


@info@: The combine info handle.

@data@: Receives the user data pointer, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_aggregate_function_combine_get_user_data"
    c_duckdb_v2_aggregate_function_combine_get_user_data :: DuckDBV2AggregateFunctionCombineInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the bind data set by the function's bind callback.

history:
- stable: v2.0.0


@info@: The combine info handle.

@data@: Receives the bind data pointer, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_aggregate_function_combine_get_bind_data"
    c_duckdb_v2_aggregate_function_combine_get_bind_data :: DuckDBV2AggregateFunctionCombineInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns how many source/target state pairs this invocation must combine. This is the length of both the sources and
the targets array.

history:
- stable: v2.0.0


@info@: The combine info handle.

@count@: Receives the number of state pairs.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_aggregate_function_combine_get_state_count"
    c_duckdb_v2_aggregate_function_combine_get_state_count :: DuckDBV2AggregateFunctionCombineInfoHandle -> Ptr DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the array of source aggregate states, one pointer per pair. Source i must be combined into target i; the
source must not be modified.

history:
- stable: v2.0.0


@info@: The combine info handle.

@states@: Receives the array of source aggregate state pointers. Borrowed; valid only for the duration of the
callback.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_aggregate_function_combine_get_sources"
    c_duckdb_v2_aggregate_function_combine_get_sources :: DuckDBV2AggregateFunctionCombineInfoHandle -> Ptr (Ptr (Ptr ())) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the array of target aggregate states, one pointer per pair. Source i must be combined into target i.

history:
- stable: v2.0.0


@info@: The combine info handle.

@states@: Receives the array of target aggregate state pointers. Borrowed; valid only for the duration of the
callback.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_aggregate_function_combine_get_targets"
    c_duckdb_v2_aggregate_function_combine_get_targets :: DuckDBV2AggregateFunctionCombineInfoHandle -> Ptr (Ptr (Ptr ())) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the user data set via @duckdb_v2_aggregate_function_set_user_data()@.

history:
- stable: v2.0.0


@info@: The finalize info handle.

@data@: Receives the user data pointer, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_aggregate_function_finalize_get_user_data"
    c_duckdb_v2_aggregate_function_finalize_get_user_data :: DuckDBV2AggregateFunctionFinalizeInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the bind data set by the function's bind callback.

history:
- stable: v2.0.0


@info@: The finalize info handle.

@data@: Receives the bind data pointer, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_aggregate_function_finalize_get_bind_data"
    c_duckdb_v2_aggregate_function_finalize_get_bind_data :: DuckDBV2AggregateFunctionFinalizeInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns how many aggregate states this invocation must finalize. This is the length of the state array and the number
of rows to write to the result vector.

history:
- stable: v2.0.0


@info@: The finalize info handle.

@count@: Receives the number of states.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_aggregate_function_finalize_get_state_count"
    c_duckdb_v2_aggregate_function_finalize_get_state_count :: DuckDBV2AggregateFunctionFinalizeInfoHandle -> Ptr DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the array of aggregate states to finalize, one pointer per state. State i must be finalized into result row
offset + i.

history:
- stable: v2.0.0


@info@: The finalize info handle.

@states@: Receives the array of aggregate state pointers. Borrowed; valid only for the duration of the callback.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_aggregate_function_finalize_get_states"
    c_duckdb_v2_aggregate_function_finalize_get_states :: DuckDBV2AggregateFunctionFinalizeInfoHandle -> Ptr (Ptr (Ptr ())) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the result vector the finalize callback must write into.

State i must be finalized into result row offset + i, with the offset from
@duckdb_v2_aggregate_function_finalize_get_result_offset()@. Borrowed; valid only for the duration of the callback.

history:
- stable: v2.0.0


@info@: The finalize info handle.

@vector@: Receives the borrowed result vector to write into.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_aggregate_function_finalize_get_result"
    c_duckdb_v2_aggregate_function_finalize_get_result :: DuckDBV2AggregateFunctionFinalizeInfoHandle -> Ptr DuckDBV2VectorHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the offset in the result vector at which to start writing: state i must be finalized into result row offset +
i.

history:
- stable: v2.0.0


@info@: The finalize info handle.

@offset@: Receives the result offset.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_aggregate_function_finalize_get_result_offset"
    c_duckdb_v2_aggregate_function_finalize_get_result_offset :: DuckDBV2AggregateFunctionFinalizeInfoHandle -> Ptr DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the user data set via @duckdb_v2_aggregate_function_set_user_data()@.

history:
- stable: v2.0.0


@info@: The destroy info handle.

@data@: Receives the user data pointer, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_aggregate_function_destroy_get_user_data"
    c_duckdb_v2_aggregate_function_destroy_get_user_data :: DuckDBV2AggregateFunctionDestroyInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the bind data set by the function's bind callback.

history:
- stable: v2.0.0


@info@: The destroy info handle.

@data@: Receives the bind data pointer, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_aggregate_function_destroy_get_bind_data"
    c_duckdb_v2_aggregate_function_destroy_get_bind_data :: DuckDBV2AggregateFunctionDestroyInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns how many aggregate states this invocation must destroy. This is the length of the state array.

history:
- stable: v2.0.0


@info@: The destroy info handle.

@count@: Receives the number of states.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_aggregate_function_destroy_get_state_count"
    c_duckdb_v2_aggregate_function_destroy_get_state_count :: DuckDBV2AggregateFunctionDestroyInfoHandle -> Ptr DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the array of aggregate states to destroy, one pointer per state. The callback must release any resources the
states own.

history:
- stable: v2.0.0


@info@: The destroy info handle.

@states@: Receives the array of aggregate state pointers. Borrowed; valid only for the duration of the callback.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_aggregate_function_destroy_get_states"
    c_duckdb_v2_aggregate_function_destroy_get_states :: DuckDBV2AggregateFunctionDestroyInfoHandle -> Ptr (Ptr (Ptr ())) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Registers the aggregate function, making it available for use in SQL queries.

The function is registered on the target given at creation: the connection's database or the loading extension.
Registration requires a name, the size, init, update, combine and finalize callbacks, and a signature with a complete
return type; an ANY return type is accepted only together with a bind callback that sets the concrete type per call
site. The caller still owns the handle after registration and must destroy it with
@duckdb_v2_aggregate_function_destroy()@, which does not affect the registered function.

history:
- stable: v2.0.0


@function@: The function to register.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_aggregate_function_register"
    c_duckdb_v2_aggregate_function_register :: DuckDBV2AggregateFunctionHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Destroys the aggregate function, releasing its resources.

Null-safe: passing a null pointer or null handle is a no-op. The handle is set to null on return to prevent
double-destruction. Destroying the handle after registration does not affect the registered function.

history:
- stable: v2.0.0


@function@: The function to destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_aggregate_function_destroy"
    c_duckdb_v2_aggregate_function_destroy :: Ptr DuckDBV2AggregateFunctionHandle -> IO DuckDBV2Error

{- | Resolves a table name and snapshots its description.

Resolves name in the connection's catalogs and returns an owned description of the table it names. name may be
partial: an unqualified or schema-qualified name resolves through the connection's search path, exactly as the same
name resolves in SQL. A two-part name tries the first part as a schema and as an attached database, as SQL does, and
is rejected when both readings exist. A name that resolves to nothing is rejected with the engine's missing-table
error; a name that resolves to a view is rejected with the engine's not-a-table error, since a description snapshots
a base table.

history:
- stable: v2.0.0


@conn@: The connection whose catalogs and search path resolve the name.

@name@: The possibly partial table name to resolve. Borrowed for the call only.

@desc@: On success, receives the owned description. Destroy via @duckdb_v2_table_description_destroy()@.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_connection_describe_table"
    c_duckdb_v2_connection_describe_table :: DuckDBV2ConnectionHandle -> DuckDBV2QnameHandle -> Ptr DuckDBV2TableDescriptionHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the resolved qualified name of the described table.

Returns an owned copy of the fully resolved name: the catalog, schema, and table the lookup landed on, with the
casing the table was created with, never an echo of the requested name. Rendering it produces SQL that pins the
described table regardless of search path.

history:
- stable: v2.0.0


@desc@: The description.

@name@: Receives the owned resolved name. Destroy via @duckdb_v2_qname_destroy()@.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_description_get_qname"
    c_duckdb_v2_table_description_get_qname :: DuckDBV2TableDescriptionHandle -> Ptr DuckDBV2QnameHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns whether the described table's catalog is read-only.

True when the catalog the name resolved into was attached read-only, in which case no write to the table can succeed.
False does not by itself prove a write will succeed; it clears the catalog-level check only.

history:
- stable: v2.0.0


@desc@: The description.

@readonly@: Receives whether the catalog is read-only.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_description_is_readonly"
    c_duckdb_v2_table_description_is_readonly :: DuckDBV2TableDescriptionHandle -> Ptr CBool -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the number of columns of the described table.

Counts every column in declared order, generated columns included. Valid indices for
@duckdb_v2_table_description_get_column()@ are [0, count).

history:
- stable: v2.0.0


@desc@: The description.

@count@: Receives the column count.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_description_get_column_count"
    c_duckdb_v2_table_description_get_column_count :: DuckDBV2TableDescriptionHandle -> Ptr DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns an owned description of the column at index.

Columns are numbered in declared order, generated columns included. An out-of-range index is rejected with
@ERROR_INPUT_OUT_OF_RANGE@.

history:
- stable: v2.0.0


@desc@: The description.

@index@: Zero-based column index.

@column@: Receives the owned column description. Destroy via @duckdb_v2_column_description_destroy()@.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_description_get_column"
    c_duckdb_v2_table_description_get_column :: DuckDBV2TableDescriptionHandle -> DuckDBV2Idx -> Ptr DuckDBV2ColumnDescriptionHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Destroys the table description, releasing its resources.

Null-safe: passing a null pointer or null handle is a no-op. The handle is set to null on return to prevent
double-destruction. Column descriptions obtained from it are independent and stay valid.

history:
- stable: v2.0.0


@desc@: The description to destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_description_destroy"
    c_duckdb_v2_table_description_destroy :: Ptr DuckDBV2TableDescriptionHandle -> IO DuckDBV2Error

{- | Borrows the name of the column.

The name carries the casing the column was declared with.

history:
- stable: v2.0.0


@column@: The column description.

@name@: Receives a borrowed view of the column name. Valid until the column description is destroyed.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_column_description_get_name"
    c_duckdb_v2_column_description_get_name :: DuckDBV2ColumnDescriptionHandle -> Ptr DuckDBV2IdentifierT -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Borrows the type of the column.

history:
- stable: v2.0.0


@column@: The column description.

@type@: Receives the borrowed column type. Valid until the column description is destroyed; do not destroy it.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_column_description_get_type"
    c_duckdb_v2_column_description_get_type :: DuckDBV2ColumnDescriptionHandle -> Ptr DuckDBV2LogicalTypeHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns whether the column has a default value.

True when the column declares a default expression; the engine evaluates it for rows that omit the column. Generated
columns report false.

history:
- stable: v2.0.0


@column@: The column description.

@has_default@: Receives whether the column has a default value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_column_description_has_default"
    c_duckdb_v2_column_description_has_default :: DuckDBV2ColumnDescriptionHandle -> Ptr CBool -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns whether the column is generated.

True when the column is a generated column, computed by the engine from a generation expression and not writable.

history:
- stable: v2.0.0


@column@: The column description.

@has_generated@: Receives whether the column is generated.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_column_description_has_generated"
    c_duckdb_v2_column_description_has_generated :: DuckDBV2ColumnDescriptionHandle -> Ptr CBool -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Destroys the column description, releasing its resources.

Null-safe: passing a null pointer or null handle is a no-op. The handle is set to null on return to prevent
double-destruction. Any name or type borrowed from it becomes dangling.

history:
- stable: v2.0.0


@column@: The column description to destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_column_description_destroy"
    c_duckdb_v2_column_description_destroy :: Ptr DuckDBV2ColumnDescriptionHandle -> IO DuckDBV2Error

{- | Opens a connection to an instance, starting it if it has not started yet.

Each connection carries its own client context and session-scoped (LOCAL) settings. Connections to the same instance
share its catalog, buffer pool, and transaction manager. A connection may be created before any database is attached
to the instance; until one is, only the system catalog and the connection's temporary catalog are visible. The caller
destroys it via @duckdb_v2_connection_destroy()@.

history:
- stable: v2.0.0


@instance@: The instance to connect to.

@out_conn@: Receives the new connection handle.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_connection_create"
    c_duckdb_v2_connection_create :: DuckDBV2InstanceHandle -> Ptr DuckDBV2ConnectionHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Destroys the connection.

Always succeeds. Releases the connection's reference on the underlying database instance, which survives for as long
as anything else still references it. On success the handle is set to null. Safe to call on an already-null slot.

history:
- stable: v2.0.0


@conn@: The connection to destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_connection_destroy"
    c_duckdb_v2_connection_destroy :: Ptr DuckDBV2ConnectionHandle -> IO DuckDBV2Error

{- | Sets a config option through the connection.

@scope@ chooses the destination, mirroring SQL:
  - AUTOMATIC resolves it from the option's target scope, like a bare @SET name = setting@.
  - GLOBAL writes through to the instance, visible to all connections, like @SET GLOBAL@.
  - LOCAL writes to this connection's session only, like @SET LOCAL@ / @SET SESSION@.
A disallowed combination returns ERROR_INPUT_INVALID: GLOBAL against a LOCAL_ONLY option, LOCAL against a GLOBAL_ONLY
one, and the legacy analogues. An unknown name is rejected unless an extension that defines it can be autoloaded; to
stage a setting for an extension before startup, use @duckdb_v2_instance_set_option()@.

history:
- stable: v2.0.0


@conn@: The connection.

@name@: Option name (canonical or alias).

@setting@: The setting, in the textual form SQL @SET@ accepts.

@scope@: Target scope: AUTOMATIC, GLOBAL, or LOCAL.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_connection_set_option"
    c_duckdb_v2_connection_set_option :: DuckDBV2ConnectionHandle -> Ptr DuckDBV2IdentifierT -> Ptr DuckDBV2Str -> DuckDBV2SettingScope -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Reads a config option through the connection.

Returns the option's effective setting at the connection's scope: the LOCAL override if this connection set one,
otherwise the GLOBAL value, otherwise the static default. The remaining fields are populated exactly as by
@duckdb_v2_instance_get_option_by_name()@. Aliases resolve transparently, and an unknown name returns
ERROR_INPUT_INVALID. The caller destroys the returned option.

history:
- stable: v2.0.0


@conn@: The connection.

@name@: Option name (canonical or alias).

@out_option@: Receives the populated option handle.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_connection_get_option_by_name"
    c_duckdb_v2_connection_get_option_by_name :: DuckDBV2ConnectionHandle -> Ptr DuckDBV2IdentifierT -> Ptr DuckDBV2OptionHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the number of config options visible to this connection.

The same count @duckdb_v2_instance_get_option_count()@ reports for the underlying instance: core plus extension
options, aliases excluded.

history:
- stable: v2.0.0


@conn@: The connection.

@out_count@: Receives the option count.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_connection_get_option_count"
    c_duckdb_v2_connection_get_option_count :: DuckDBV2ConnectionHandle -> Ptr DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Reads the config option at the given index visible to this connection.

Index space: [0, core_count) addresses core options, [core_count, total) the extension options visible from this
connection's instance. An out-of-range index returns ERROR_INPUT_INVALID. The caller destroys the returned option.

history:
- stable: v2.0.0


@conn@: The connection.

@index@: The option index, in [0, option_count).

@out_option@: Receives the populated option handle.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_connection_get_option_by_index"
    c_duckdb_v2_connection_get_option_by_index :: DuckDBV2ConnectionHandle -> DuckDBV2Idx -> Ptr DuckDBV2OptionHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Interrupts the query currently executing on the connection.

The cross-thread (but not cross-connection) cancellation entry point for streaming results: safe to call from any
thread within the execution of a query through a connection, including while another thread steps the query's result.
A no-op when no query is active. Cancellation surfaces on the consuming side as step status CANCELLED
(@duckdb_v2_result_step()@), or as ERROR_RUNTIME_INTERRUPT (@duckdb_v2_result_fetch_chunk()@).

history:
- stable: v2.0.0


@conn@: The connection whose active query to interrupt.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_connection_interrupt"
    c_duckdb_v2_connection_interrupt :: DuckDBV2ConnectionHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Captures a snapshot of the active query's execution progress.

Reads the percentage and row counts from one consistent snapshot of the query currently executing on the connection.
Safe to call from any thread, including while another thread steps the query's result.

Progress is published only when the enable_progress_bar option is set; the bridge does not enable tracking itself.
Both row counts are 0 when no information is available. The percentage is -1 when tracking is disabled, no query is
active, or no progress has been published yet.

history:
- stable: v2.0.0


@conn@: The connection.

@out_percentage@: Receives the percentage complete in [0, 100], -1 if tracking is disabled, no query is active,
or no progress has been published yet.

@out_rows_processed@: Receives the number of rows processed so far.

@out_total_rows_to_process@: Receives the total number of rows the query will process.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_connection_progress_get"
    c_duckdb_v2_connection_progress_get :: DuckDBV2ConnectionHandle -> Ptr CDouble -> Ptr Word64 -> Ptr Word64 -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a new copy function that will be registered on the connection's database.

A copy function implements a file format for @COPY@: once registered, SQL reaches it with @COPY ... TO \'path\' (FORMAT
name)@ and @COPY table FROM \'path\' (FORMAT name)@. The function starts out empty: configure it with the setter
functions (e.g. @duckdb_v2_copy_function_set_name()@, the @copy_to_set_*@ callbacks for writing, the
@copy_from_set_*@ callbacks for reading, or both), then make it available with @duckdb_v2_copy_function_register()@.
The caller owns the returned handle and must destroy it with @duckdb_v2_copy_function_destroy()@, also after
registration.

history:
- stable: v2.0.0


@connection@: The connection to create the function in.

@function@: On success, receives the newly created copy function. Owned by the caller.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_function_create_with_connection"
    c_duckdb_v2_copy_function_create_with_connection :: DuckDBV2ConnectionHandle -> Ptr DuckDBV2CopyFunctionHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a new copy function that will be registered on the loading extension's database.

A copy function implements a file format for @COPY@: once registered, SQL reaches it with @COPY ... TO \'path\' (FORMAT
name)@ and @COPY table FROM \'path\' (FORMAT name)@. Use this from an extension load callback, where an extension
handle is available. The function starts out empty: configure it with the setter functions (e.g.
@duckdb_v2_copy_function_set_name()@, the @copy_to_set_*@ callbacks for writing, the @copy_from_set_*@ callbacks for
reading, or both), then make it available with @duckdb_v2_copy_function_register()@. The caller owns the returned
handle and must destroy it with @duckdb_v2_copy_function_destroy()@, also after registration.

history:
- stable: v2.0.0


@extension@: The extension to create the function in.

@function@: On success, receives the newly created copy function. Owned by the caller.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_function_create_with_extension"
    c_duckdb_v2_copy_function_create_with_extension :: DuckDBV2ExtensionHandle -> Ptr DuckDBV2CopyFunctionHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets the name of the copy function.

The name is the format SQL selects the function with: @COPY ... TO \'path\' (FORMAT name)@ or @COPY table FROM \'path\'
(FORMAT name)@. It is borrowed and copied. Calling this again replaces the previous name. A name must be set before
registration.

history:
- stable: v2.0.0


@function@: The function to set the name of.

@name@: The name to set. Borrowed and copied.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_function_set_name"
    c_duckdb_v2_copy_function_set_name :: DuckDBV2CopyFunctionHandle -> Ptr DuckDBV2IdentifierT -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets arbitrary user data on the copy function.

Associates an opaque pointer with the function, retrievable from each callback of either side via its user data
accessor (e.g. @duckdb_v2_copy_to_bind_get_user_data()@, @duckdb_v2_copy_from_exec_get_user_data()@, etc.). The
opaque handle bundles the pointer with an optional destructor, invoked when the data is no longer needed.

history:
- stable: v2.0.0


@function@: The function to set the user data of.

@data@: Opaque handle bundling the user data pointer plus an optional destructor.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_function_set_user_data"
    c_duckdb_v2_copy_function_set_user_data :: DuckDBV2CopyFunctionHandle -> Ptr DuckDBV2Opaque -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets the optional bind callback of the @COPY ... TO@ side.

The bind callback is invoked during query planning for each @COPY ... TO@ statement that uses the function. It can
inspect the names and types of the columns being written, read the statement's options and set "bind data" that is
shared with the other @COPY ... TO@ callbacks.

history:
- stable: v2.0.0


@function@: The function to set the callback of.

@callback@: The callback to set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_to_set_bind_callback"
    c_duckdb_v2_copy_to_set_bind_callback :: DuckDBV2CopyFunctionHandle -> FunPtr DuckDBV2CopyToBindCallbackFn -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets the optional batch size callback of the @COPY ... TO@ side.

The batch size callback is invoked during query planning, after the bind callback, for each @COPY ... TO@ statement
that does not set @BATCH_SIZE@ itself. It should report how many rows a batch should carry via
@duckdb_v2_copy_to_batch_size_set_target()@; the engine then cuts the rows being written into batches of that size
and hands each to the batch callback. Without a batch size from either the statement or the callback, a batch is cut
for every chunk of rows sunk, i.e. a vector at a time. A batch may still be smaller than the reported size (the last
one of a file, or when @BATCH_SIZE_BYTES@ cuts it first).

history:
- stable: v2.0.0


@function@: The function to set the callback of.

@callback@: The callback to set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_to_set_batch_size_callback"
    c_duckdb_v2_copy_to_set_batch_size_callback :: DuckDBV2CopyFunctionHandle -> FunPtr DuckDBV2CopyToBatchSizeCallbackFn -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets the optional init callback of the @COPY ... TO@ side.

The init callback is invoked once per output file, before any batch destined for that file is prepared. It can read
the path of the file via @duckdb_v2_copy_to_init_get_file_path()@ and set "init data" that is shared with the batch,
flush and finalize callbacks of that file. A statement may write several files, e.g. when its output is partitioned;
each gets its own init data.

history:
- stable: v2.0.0


@function@: The function to set the callback of.

@callback@: The callback to set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_to_set_init_callback"
    c_duckdb_v2_copy_to_set_init_callback :: DuckDBV2CopyFunctionHandle -> FunPtr DuckDBV2CopyToInitCallbackFn -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets the batch callback of the @COPY ... TO@ side.

The batch callback is invoked during query execution with a batch of the rows being written, taken via
@duckdb_v2_copy_to_batch_take_input()@. It prepares the batch for writing, e.g. by encoding it into the output
format, and sets "batch data" via @duckdb_v2_copy_to_batch_set_batch_data()@ that is handed to the flush callback.
Batches may be prepared by several threads at once, so the callback must synchronize its own access to the init data.
A batch callback must be set for the @COPY ... TO@ side to be registered.

history:
- stable: v2.0.0


@function@: The function to set the callback of.

@callback@: The callback to set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_to_set_batch_callback"
    c_duckdb_v2_copy_to_set_batch_callback :: DuckDBV2CopyFunctionHandle -> FunPtr DuckDBV2CopyToBatchCallbackFn -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets the flush callback of the @COPY ... TO@ side.

The flush callback is invoked once per prepared batch to write its batch data, available via
@duckdb_v2_copy_to_flush_get_batch_data()@, to the output. Flushes of the same file never run concurrently. A flush
callback must be set for the @COPY ... TO@ side to be registered.

history:
- stable: v2.0.0


@function@: The function to set the callback of.

@callback@: The callback to set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_to_set_flush_callback"
    c_duckdb_v2_copy_to_set_flush_callback :: DuckDBV2CopyFunctionHandle -> FunPtr DuckDBV2CopyToFlushCallbackFn -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets the optional finalize callback of the @COPY ... TO@ side.

The finalize callback is invoked once per output file after the last batch destined for that file has been flushed.
It can e.g. write a footer and close the file.

history:
- stable: v2.0.0


@function@: The function to set the callback of.

@callback@: The callback to set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_to_set_finalize_callback"
    c_duckdb_v2_copy_to_set_finalize_callback :: DuckDBV2CopyFunctionHandle -> FunPtr DuckDBV2CopyToFinalizeCallbackFn -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the user data set via @duckdb_v2_copy_function_set_user_data()@.

history:
- stable: v2.0.0


@info@: The bind info handle.

@data@: Receives the user data pointer, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_to_bind_get_user_data"
    c_duckdb_v2_copy_to_bind_get_user_data :: DuckDBV2CopyToBindInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets the function's "bind data" from the bind callback.

The bind data is stored with the bound statement and retrievable from the other callbacks of the same side. The
opaque handle bundles the pointer with an optional destructor, invoked when the bind data is no longer needed, and an
optional equality callback used when comparing two bound statements; without one, pointer equality is used.

history:
- stable: v2.0.0


@info@: The bind info handle.

@data@: Opaque handle bundling the bind data pointer plus optional destructor and equality callbacks.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_to_bind_set_bind_data"
    c_duckdb_v2_copy_to_bind_set_bind_data :: DuckDBV2CopyToBindInfoHandle -> Ptr DuckDBV2Opaque -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the path the @COPY ... TO@ statement writes to, as written in the statement.

This is the target before the engine's own rewrites: the path of each file actually being written, e.g. a temporary
name or a per-partition path, is only known to the init callback via @duckdb_v2_copy_to_init_get_file_path()@. The
path is borrowed and valid only for the duration of the callback.

history:
- stable: v2.0.0


@info@: The bind info handle.

@path@: Receives a borrowed view of the file path. Valid only for the duration of the callback.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_to_bind_get_file_path"
    c_duckdb_v2_copy_to_bind_get_file_path :: DuckDBV2CopyToBindInfoHandle -> Ptr DuckDBV2Str -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the number of columns being written, i.e. the columns of the rows every batch carries.

Valid indices for @duckdb_v2_copy_to_bind_get_column_type()@ and @duckdb_v2_copy_to_bind_get_column_name()@ are [0,
count).

history:
- stable: v2.0.0


@info@: The bind info handle.

@count@: Receives the number of columns.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_to_bind_get_column_count"
    c_duckdb_v2_copy_to_bind_get_column_count :: DuckDBV2CopyToBindInfoHandle -> Ptr DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the type of the column at the given index.

Fails if the index is out of bounds. The returned type is owned by the caller and must be destroyed via
@duckdb_v2_logical_type_destroy()@.

history:
- stable: v2.0.0


@info@: The bind info handle.

@index@: The index of the column to get the type of.

@type@: Receives the column type. Owned by the caller; destroy via @duckdb_v2_logical_type_destroy()@.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_to_bind_get_column_type"
    c_duckdb_v2_copy_to_bind_get_column_type :: DuckDBV2CopyToBindInfoHandle -> DuckDBV2Idx -> Ptr DuckDBV2LogicalTypeHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the name of the column at the given index.

Fails if the index is out of bounds. The name is borrowed and valid only for the duration of the callback.

history:
- stable: v2.0.0


@info@: The bind info handle.

@index@: The index of the column to get the name of.

@name@: Receives a borrowed view of the column name. Valid only for the duration of the callback.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_to_bind_get_column_name"
    c_duckdb_v2_copy_to_bind_get_column_name :: DuckDBV2CopyToBindInfoHandle -> DuckDBV2Idx -> Ptr DuckDBV2IdentifierT -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the number of options the @COPY@ statement passed to the function.

The engine's own options (e.g. @USE_TMP_FILE@ or @BATCH_SIZE@) are handled before the function sees the statement and
are not included. Valid indices for @duckdb_v2_copy_to_bind_get_option_name()@ and
@duckdb_v2_copy_to_bind_get_option_value()@ are [0, count). The options are ordered by name.

history:
- stable: v2.0.0


@info@: The bind info handle.

@count@: Receives the number of options.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_to_bind_get_option_count"
    c_duckdb_v2_copy_to_bind_get_option_count :: DuckDBV2CopyToBindInfoHandle -> Ptr DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the name of the option at the given index.

Option names are SQL identifiers, matched case-insensitively. Fails if the index is out of bounds. The name is
borrowed and valid only for the duration of the callback.

history:
- stable: v2.0.0


@info@: The bind info handle.

@index@: The index of the option to get the name of.

@name@: Receives a borrowed view of the option name. Valid only for the duration of the callback.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_to_bind_get_option_name"
    c_duckdb_v2_copy_to_bind_get_option_name :: DuckDBV2CopyToBindInfoHandle -> DuckDBV2Idx -> Ptr DuckDBV2IdentifierT -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the value of the option at the given index.

An option written with a single value (e.g. @DELIM \',\'@) yields that value. An option written as a bare name (e.g.
@HEADER@) yields the BOOLEAN @true@. An option written with a parenthesized list (e.g. @KEYS (a, b)@) yields a tuple:
an unnamed STRUCT with one field per element, in order. Fails if the index is out of bounds. The returned value is
owned by the caller and must be destroyed via @duckdb_v2_value_destroy()@.

history:
- stable: v2.0.0


@info@: The bind info handle.

@index@: The index of the option.

@value@: Receives the option's value. Owned by the caller; destroy via @duckdb_v2_value_destroy()@.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_to_bind_get_option_value"
    c_duckdb_v2_copy_to_bind_get_option_value :: DuckDBV2CopyToBindInfoHandle -> DuckDBV2Idx -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the user data set via @duckdb_v2_copy_function_set_user_data()@.

history:
- stable: v2.0.0


@info@: The batch size info handle.

@data@: Receives the user data pointer, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_to_batch_size_get_user_data"
    c_duckdb_v2_copy_to_batch_size_get_user_data :: DuckDBV2CopyToBatchSizeInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the bind data set by the function's @COPY ... TO@ bind callback.

history:
- stable: v2.0.0


@info@: The batch size info handle.

@data@: Receives the bind data pointer, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_to_batch_size_get_bind_data"
    c_duckdb_v2_copy_to_batch_size_get_bind_data :: DuckDBV2CopyToBatchSizeInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets the number of rows a batch should carry, as the target the engine cuts batches at. The batch size callback must
set this to a value greater than 0; the statement fails otherwise. DuckDB defines the @BATCH_SIZE@ on omission of
calling the function.

history:
- stable: v2.0.0


@info@: The batch size info handle.

@rows@: The number of rows a batch should carry. Must be greater than 0.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_to_batch_size_set_target"
    c_duckdb_v2_copy_to_batch_size_set_target :: DuckDBV2CopyToBatchSizeInfoHandle -> DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the user data set via @duckdb_v2_copy_function_set_user_data()@.

history:
- stable: v2.0.0


@info@: The init info handle.

@data@: Receives the user data pointer, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_to_init_get_user_data"
    c_duckdb_v2_copy_to_init_get_user_data :: DuckDBV2CopyToInitInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the bind data set by the function's @COPY ... TO@ bind callback.

history:
- stable: v2.0.0


@info@: The init info handle.

@data@: Receives the bind data pointer, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_to_init_get_bind_data"
    c_duckdb_v2_copy_to_init_get_bind_data :: DuckDBV2CopyToInitInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the path of the file the init callback is preparing.

This is the path the batches of this file are to be written to, after the engine has applied its own rewrites (e.g. a
temporary name while the file is being written, or a per-partition path). The path is borrowed and valid only for the
duration of the callback.

history:
- stable: v2.0.0


@info@: The init info handle.

@path@: Receives a borrowed view of the file path. Valid only for the duration of the callback.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_to_init_get_file_path"
    c_duckdb_v2_copy_to_init_get_file_path :: DuckDBV2CopyToInitInfoHandle -> Ptr DuckDBV2Str -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets the function's "init data" from the init callback.

The init data lives for the duration of the file being written and is retrievable from the batch, flush and finalize
callbacks of that file. Batches may be prepared by several threads at once, so the function must synchronize its own
access to it from the batch callback. The opaque handle bundles the pointer with an optional destructor, invoked when
the file is done with the data.

history:
- stable: v2.0.0


@info@: The init info handle.

@data@: Opaque handle bundling the init data pointer plus an optional destructor.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_to_init_set_init_data"
    c_duckdb_v2_copy_to_init_set_init_data :: DuckDBV2CopyToInitInfoHandle -> Ptr DuckDBV2Opaque -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the user data set via @duckdb_v2_copy_function_set_user_data()@.

history:
- stable: v2.0.0


@info@: The batch info handle.

@data@: Receives the user data pointer, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_to_batch_get_user_data"
    c_duckdb_v2_copy_to_batch_get_user_data :: DuckDBV2CopyToBatchInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the bind data set by the function's @COPY ... TO@ bind callback.

history:
- stable: v2.0.0


@info@: The batch info handle.

@data@: Receives the bind data pointer, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_to_batch_get_bind_data"
    c_duckdb_v2_copy_to_batch_get_bind_data :: DuckDBV2CopyToBatchInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the init data set by the function's @COPY ... TO@ init callback for the file this batch belongs to.

history:
- stable: v2.0.0


@info@: The batch info handle.

@data@: Receives the init data pointer, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_to_batch_get_init_data"
    c_duckdb_v2_copy_to_batch_get_init_data :: DuckDBV2CopyToBatchInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Takes ownership of the rows of the batch to prepare.

The collection holds one column per column reported by @duckdb_v2_copy_to_bind_get_column_count()@, in the same
order, and can be scanned with @duckdb_v2_column_data_collection_scan()@. The batch can only be taken once: a second
call fails. Once taken, the caller owns the collection and must destroy it via
@duckdb_v2_column_data_collection_destroy()@, e.g. by keeping it as the batch data with that destructor. A batch that
is never taken is destroyed when the callback returns.

history:
- stable: v2.0.0


@info@: The batch info handle.

@collection@: Receives the collection holding the rows of the batch. Owned by the caller; destroy via
@duckdb_v2_column_data_collection_destroy()@.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_to_batch_take_input"
    c_duckdb_v2_copy_to_batch_take_input :: DuckDBV2CopyToBatchInfoHandle -> Ptr DuckDBV2ColumnDataCollectionHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets the prepared "batch data" from the batch callback.

The batch data is the prepared form of the batch and is handed to the flush callback via
@duckdb_v2_copy_to_flush_get_batch_data()@. The opaque handle bundles the pointer with an optional destructor,
invoked once the batch has been flushed.

history:
- stable: v2.0.0


@info@: The batch info handle.

@data@: Opaque handle bundling the batch data pointer plus an optional destructor.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_to_batch_set_batch_data"
    c_duckdb_v2_copy_to_batch_set_batch_data :: DuckDBV2CopyToBatchInfoHandle -> Ptr DuckDBV2Opaque -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the user data set via @duckdb_v2_copy_function_set_user_data()@.

history:
- stable: v2.0.0


@info@: The flush info handle.

@data@: Receives the user data pointer, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_to_flush_get_user_data"
    c_duckdb_v2_copy_to_flush_get_user_data :: DuckDBV2CopyToFlushInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the bind data set by the function's @COPY ... TO@ bind callback.

history:
- stable: v2.0.0


@info@: The flush info handle.

@data@: Receives the bind data pointer, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_to_flush_get_bind_data"
    c_duckdb_v2_copy_to_flush_get_bind_data :: DuckDBV2CopyToFlushInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the init data set by the function's @COPY ... TO@ init callback for the file this batch belongs to.

history:
- stable: v2.0.0


@info@: The flush info handle.

@data@: Receives the init data pointer, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_to_flush_get_init_data"
    c_duckdb_v2_copy_to_flush_get_init_data :: DuckDBV2CopyToFlushInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the batch data set by the function's batch callback for the batch being flushed.

history:
- stable: v2.0.0


@info@: The flush info handle.

@data@: Receives the batch data pointer, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_to_flush_get_batch_data"
    c_duckdb_v2_copy_to_flush_get_batch_data :: DuckDBV2CopyToFlushInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the user data set via @duckdb_v2_copy_function_set_user_data()@.

history:
- stable: v2.0.0


@info@: The finalize info handle.

@data@: Receives the user data pointer, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_to_finalize_get_user_data"
    c_duckdb_v2_copy_to_finalize_get_user_data :: DuckDBV2CopyToFinalizeInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the bind data set by the function's @COPY ... TO@ bind callback.

history:
- stable: v2.0.0


@info@: The finalize info handle.

@data@: Receives the bind data pointer, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_to_finalize_get_bind_data"
    c_duckdb_v2_copy_to_finalize_get_bind_data :: DuckDBV2CopyToFinalizeInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the init data set by the function's @COPY ... TO@ init callback for the file being finalized.

history:
- stable: v2.0.0


@info@: The finalize info handle.

@data@: Receives the init data pointer, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_to_finalize_get_init_data"
    c_duckdb_v2_copy_to_finalize_get_init_data :: DuckDBV2CopyToFinalizeInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets the bind callback of the @COPY ... FROM@ side.

The bind callback is invoked during query planning for each @COPY ... FROM@ statement that uses the function. It can
read the path of the file to read and the statement's options, inspect the names and types of the columns the target
table expects, hint at the number of rows the read will produce, and set "bind data" that is shared with the other
@COPY ... FROM@ callbacks. The columns are fixed by the target table: the function must produce them as they are. A
bind callback must be set for the @COPY ... FROM@ side to be registered.

history:
- stable: v2.0.0


@function@: The function to set the callback of.

@callback@: The callback to set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_from_set_bind_callback"
    c_duckdb_v2_copy_from_set_bind_callback :: DuckDBV2CopyFunctionHandle -> FunPtr DuckDBV2CopyFromBindCallbackFn -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets the optional global init callback of the @COPY ... FROM@ side.

The global init callback is invoked once per statement, at the start of execution. It can set "global state" shared
by every thread reading the file, and declare how many threads may read it in parallel via
@duckdb_v2_copy_from_init_global_set_max_threads()@.

history:
- stable: v2.0.0


@function@: The function to set the callback of.

@callback@: The callback to set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_from_set_init_global_callback"
    c_duckdb_v2_copy_from_set_init_global_callback :: DuckDBV2CopyFunctionHandle -> FunPtr DuckDBV2CopyFromInitGlobalCallbackFn -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets the optional local init callback of the @COPY ... FROM@ side.

The local init callback is invoked once per thread that will read the file. It can set worker-local "local state",
retrievable from the exec callback, typically derived from the shared global state.

history:
- stable: v2.0.0


@function@: The function to set the callback of.

@callback@: The callback to set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_from_set_init_local_callback"
    c_duckdb_v2_copy_from_set_init_local_callback :: DuckDBV2CopyFunctionHandle -> FunPtr DuckDBV2CopyFromInitLocalCallbackFn -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets the exec callback of the @COPY ... FROM@ side.

The exec callback implements the read: it is invoked repeatedly during query execution and writes the next batch of
rows to the output chunk, until it produces an empty batch to signal the end of the read. An exec callback must be
set for the @COPY ... FROM@ side to be registered.

history:
- stable: v2.0.0


@function@: The function to set the callback of.

@callback@: The callback to set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_from_set_exec_callback"
    c_duckdb_v2_copy_from_set_exec_callback :: DuckDBV2CopyFunctionHandle -> FunPtr DuckDBV2CopyFromExecCallbackFn -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets the optional progress callback of the @COPY ... FROM@ side.

The progress callback is invoked on demand during execution to report how far the read has advanced, which the engine
surfaces as the query's progress. It is invoked concurrently with the exec callback, so it must read the global state
in a thread-safe way.

history:
- stable: v2.0.0


@function@: The function to set the callback of.

@callback@: The callback to set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_from_set_progress_callback"
    c_duckdb_v2_copy_from_set_progress_callback :: DuckDBV2CopyFunctionHandle -> FunPtr DuckDBV2CopyFromProgressCallbackFn -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the user data set via @duckdb_v2_copy_function_set_user_data()@.

history:
- stable: v2.0.0


@info@: The bind info handle.

@data@: Receives the user data pointer, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_from_bind_get_user_data"
    c_duckdb_v2_copy_from_bind_get_user_data :: DuckDBV2CopyFromBindInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets the function's "bind data" from the bind callback.

The bind data is stored with the bound statement and retrievable from the other callbacks of the same side. The
opaque handle bundles the pointer with an optional destructor, invoked when the bind data is no longer needed, and an
optional equality callback used when comparing two bound statements; without one, pointer equality is used.

history:
- stable: v2.0.0


@info@: The bind info handle.

@data@: Opaque handle bundling the bind data pointer plus optional destructor and equality callbacks.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_from_bind_set_bind_data"
    c_duckdb_v2_copy_from_bind_set_bind_data :: DuckDBV2CopyFromBindInfoHandle -> Ptr DuckDBV2Opaque -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the path the @COPY ... FROM@ statement reads from, as written in the statement.

The engine does not expand globs or check that the file exists; both are left to the function. The path is borrowed
and valid only for the duration of the callback.

history:
- stable: v2.0.0


@info@: The bind info handle.

@path@: Receives a borrowed view of the file path. Valid only for the duration of the callback.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_from_bind_get_file_path"
    c_duckdb_v2_copy_from_bind_get_file_path :: DuckDBV2CopyFromBindInfoHandle -> Ptr DuckDBV2Str -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the number of columns the target table expects, i.e. the columns every batch the exec callback produces must
carry, in this order.

Valid indices for @duckdb_v2_copy_from_bind_get_column_type()@ and @duckdb_v2_copy_from_bind_get_column_name()@ are
[0, count).

history:
- stable: v2.0.0


@info@: The bind info handle.

@count@: Receives the number of columns.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_from_bind_get_column_count"
    c_duckdb_v2_copy_from_bind_get_column_count :: DuckDBV2CopyFromBindInfoHandle -> Ptr DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the type of the column at the given index.

Fails if the index is out of bounds. The returned type is owned by the caller and must be destroyed via
@duckdb_v2_logical_type_destroy()@.

history:
- stable: v2.0.0


@info@: The bind info handle.

@index@: The index of the column to get the type of.

@type@: Receives the column type. Owned by the caller; destroy via @duckdb_v2_logical_type_destroy()@.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_from_bind_get_column_type"
    c_duckdb_v2_copy_from_bind_get_column_type :: DuckDBV2CopyFromBindInfoHandle -> DuckDBV2Idx -> Ptr DuckDBV2LogicalTypeHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the name of the column at the given index.

Fails if the index is out of bounds. The name is borrowed and valid only for the duration of the callback.

history:
- stable: v2.0.0


@info@: The bind info handle.

@index@: The index of the column to get the name of.

@name@: Receives a borrowed view of the column name. Valid only for the duration of the callback.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_from_bind_get_column_name"
    c_duckdb_v2_copy_from_bind_get_column_name :: DuckDBV2CopyFromBindInfoHandle -> DuckDBV2Idx -> Ptr DuckDBV2IdentifierT -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the number of options the @COPY@ statement passed to the function.

Every option other than @FORMAT@ is included. Valid indices for @duckdb_v2_copy_from_bind_get_option_name()@ and
@duckdb_v2_copy_from_bind_get_option_value()@ are [0, count). The options are ordered by name.

history:
- stable: v2.0.0


@info@: The bind info handle.

@count@: Receives the number of options.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_from_bind_get_option_count"
    c_duckdb_v2_copy_from_bind_get_option_count :: DuckDBV2CopyFromBindInfoHandle -> Ptr DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the name of the option at the given index.

Option names are SQL identifiers, matched case-insensitively. Fails if the index is out of bounds. The name is
borrowed and valid only for the duration of the callback.

history:
- stable: v2.0.0


@info@: The bind info handle.

@index@: The index of the option to get the name of.

@name@: Receives a borrowed view of the option name. Valid only for the duration of the callback.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_from_bind_get_option_name"
    c_duckdb_v2_copy_from_bind_get_option_name :: DuckDBV2CopyFromBindInfoHandle -> DuckDBV2Idx -> Ptr DuckDBV2IdentifierT -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the value of the option at the given index.

An option written with a single value (e.g. @DELIM \',\'@) yields that value. An option written as a bare name (e.g.
@HEADER@) yields the BOOLEAN @true@. An option written with a parenthesized list (e.g. @KEYS (a, b)@) yields a tuple:
an unnamed STRUCT with one field per element, in order. Fails if the index is out of bounds. The returned value is
owned by the caller and must be destroyed via @duckdb_v2_value_destroy()@.

history:
- stable: v2.0.0


@info@: The bind info handle.

@index@: The index of the option.

@value@: Receives the option's value. Owned by the caller; destroy via @duckdb_v2_value_destroy()@.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_from_bind_get_option_value"
    c_duckdb_v2_copy_from_bind_get_option_value :: DuckDBV2CopyFromBindInfoHandle -> DuckDBV2Idx -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Reports the estimated number of rows the read will produce.

The estimate is a hint for the optimizer, not a limit: producing a different number of rows is not an error. Without
it, the optimizer falls back on its own defaults.

history:
- stable: v2.0.0


@info@: The bind info handle.

@cardinality@: The estimated number of rows.

@is_exact@: Whether the estimate is exact, which also makes it an upper bound on the row count.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_from_bind_set_cardinality"
    c_duckdb_v2_copy_from_bind_set_cardinality :: DuckDBV2CopyFromBindInfoHandle -> DuckDBV2Idx -> CBool -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the user data set via @duckdb_v2_copy_function_set_user_data()@.

history:
- stable: v2.0.0


@info@: The global init info handle.

@data@: Receives the user data pointer, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_from_init_global_get_user_data"
    c_duckdb_v2_copy_from_init_global_get_user_data :: DuckDBV2CopyFromInitGlobalInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the bind data set by the function's @COPY ... FROM@ bind callback.

history:
- stable: v2.0.0


@info@: The global init info handle.

@data@: Receives the bind data pointer, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_from_init_global_get_bind_data"
    c_duckdb_v2_copy_from_init_global_get_bind_data :: DuckDBV2CopyFromInitGlobalInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets the function's "global state" from the global init callback.

The global state lives for the duration of the read and is retrievable from the local init, exec and progress
callbacks. Every thread reading the file shares it, so the function must synchronize its own access to it. The opaque
handle bundles the pointer with an optional destructor, invoked when the read is done with the state.

history:
- stable: v2.0.0


@info@: The global init info handle.

@data@: Opaque handle bundling the global state pointer plus an optional destructor.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_from_init_global_set_global_state"
    c_duckdb_v2_copy_from_init_global_set_global_state :: DuckDBV2CopyFromInitGlobalInfoHandle -> Ptr DuckDBV2Opaque -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets how many threads may read the file in parallel.

Defaults to 1, a single-threaded read. The engine creates at most this many local states, and therefore runs at most
this many exec callbacks concurrently. It is an upper bound, not a request: the engine may use fewer threads.

history:
- stable: v2.0.0


@info@: The global init info handle.

@max_threads@: The maximum number of threads. Must be at least 1.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_from_init_global_set_max_threads"
    c_duckdb_v2_copy_from_init_global_set_max_threads :: DuckDBV2CopyFromInitGlobalInfoHandle -> DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the user data set via @duckdb_v2_copy_function_set_user_data()@.

history:
- stable: v2.0.0


@info@: The local init info handle.

@data@: Receives the user data pointer, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_from_init_local_get_user_data"
    c_duckdb_v2_copy_from_init_local_get_user_data :: DuckDBV2CopyFromInitLocalInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the bind data set by the function's @COPY ... FROM@ bind callback.

history:
- stable: v2.0.0


@info@: The local init info handle.

@data@: Receives the bind data pointer, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_from_init_local_get_bind_data"
    c_duckdb_v2_copy_from_init_local_get_bind_data :: DuckDBV2CopyFromInitLocalInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the global state set by the function's global init callback.

Shared with every other thread reading the file; access to it must be synchronized by the function.

history:
- stable: v2.0.0


@info@: The local init info handle.

@data@: Receives the global state pointer, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_from_init_local_get_global_state"
    c_duckdb_v2_copy_from_init_local_get_global_state :: DuckDBV2CopyFromInitLocalInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets the function's worker-local "local state" from the local init callback.

The local state is associated with the executing thread for the duration of the read and retrievable from the exec
callback via @duckdb_v2_copy_from_exec_get_local_state()@. No other thread observes it, so it needs no
synchronization. The opaque handle bundles the pointer with an optional destructor, invoked when the local state is
no longer needed.

history:
- stable: v2.0.0


@info@: The local init info handle.

@data@: Opaque handle bundling the local state pointer plus an optional destructor.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_from_init_local_set_local_state"
    c_duckdb_v2_copy_from_init_local_set_local_state :: DuckDBV2CopyFromInitLocalInfoHandle -> Ptr DuckDBV2Opaque -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the user data set via @duckdb_v2_copy_function_set_user_data()@.

history:
- stable: v2.0.0


@info@: The exec info handle.

@data@: Receives the user data pointer, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_from_exec_get_user_data"
    c_duckdb_v2_copy_from_exec_get_user_data :: DuckDBV2CopyFromExecInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the bind data set by the function's @COPY ... FROM@ bind callback.

history:
- stable: v2.0.0


@info@: The exec info handle.

@data@: Receives the bind data pointer, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_from_exec_get_bind_data"
    c_duckdb_v2_copy_from_exec_get_bind_data :: DuckDBV2CopyFromExecInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the global state set by the function's global init callback.

Shared with every other thread reading the file; access to it must be synchronized by the function.

history:
- stable: v2.0.0


@info@: The exec info handle.

@data@: Receives the global state pointer, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_from_exec_get_global_state"
    c_duckdb_v2_copy_from_exec_get_global_state :: DuckDBV2CopyFromExecInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the worker-local local state set by the function's local init callback.

history:
- stable: v2.0.0


@info@: The exec info handle.

@data@: Receives the local state pointer for the executing thread, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_from_exec_get_local_state"
    c_duckdb_v2_copy_from_exec_get_local_state :: DuckDBV2CopyFromExecInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the output chunk the exec callback must write the next batch of rows into.

The chunk holds one vector per column reported by @duckdb_v2_copy_from_bind_get_column_count()@, in the same order;
reach them with @duckdb_v2_data_chunk_get_vector()@. The chunk starts out empty on every invocation: write the rows,
then declare how many there are with @duckdb_v2_vector_set_size()@ on the first vector, which the engine takes as the
batch's row count and propagates to the other vectors. Producing an empty batch signals the end of the read, after
which the callback is not invoked again on that thread. Borrowed; valid only for the duration of the callback.

history:
- stable: v2.0.0


@info@: The exec info handle.

@chunk@: Receives the borrowed output chunk to write into.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_from_exec_get_output_chunk"
    c_duckdb_v2_copy_from_exec_get_output_chunk :: DuckDBV2CopyFromExecInfoHandle -> Ptr DuckDBV2DataChunkHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the user data set via @duckdb_v2_copy_function_set_user_data()@.

history:
- stable: v2.0.0


@info@: The progress info handle.

@data@: Receives the user data pointer, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_from_progress_get_user_data"
    c_duckdb_v2_copy_from_progress_get_user_data :: DuckDBV2CopyFromProgressInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the bind data set by the function's @COPY ... FROM@ bind callback.

history:
- stable: v2.0.0


@info@: The progress info handle.

@data@: Receives the bind data pointer, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_from_progress_get_bind_data"
    c_duckdb_v2_copy_from_progress_get_bind_data :: DuckDBV2CopyFromProgressInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the global state set by the function's global init callback.

The progress callback runs concurrently with the exec callbacks reading the file, so it must read the global state in
a thread-safe way.

history:
- stable: v2.0.0


@info@: The progress info handle.

@data@: Receives the global state pointer, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_from_progress_get_global_state"
    c_duckdb_v2_copy_from_progress_get_global_state :: DuckDBV2CopyFromProgressInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Reports how far the read has advanced.

A fraction between 0.0 (nothing read yet) and 1.0 (done); values outside that range are clamped. A progress callback
that returns without calling this reports no progress.

history:
- stable: v2.0.0


@info@: The progress info handle.

@progress@: The fraction of the read that is complete, in [0.0, 1.0].

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_from_progress_set_progress"
    c_duckdb_v2_copy_from_progress_set_progress :: DuckDBV2CopyFromProgressInfoHandle -> CDouble -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Registers the copy function, making it available as a @COPY@ format.

The function is registered on the target given at creation: the connection's database or the loading extension.
Registration requires a name and at least one configured side: a @COPY ... TO@ side needs its batch and flush
callbacks, a @COPY ... FROM@ side needs its bind and exec callbacks. A statement in a direction the function does not
implement fails with an error. The caller still owns the handle after registration and must destroy it with
@duckdb_v2_copy_function_destroy()@, which does not affect the registered function.

history:
- stable: v2.0.0


@function@: The function to register.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_function_register"
    c_duckdb_v2_copy_function_register :: DuckDBV2CopyFunctionHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Destroys the copy function, releasing its resources.

Null-safe: passing a null pointer or null handle is a no-op. The handle is set to null on return to prevent
double-destruction. Destroying the handle after registration does not affect the registered function.

history:
- stable: v2.0.0


@function@: The function to destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_function_destroy"
    c_duckdb_v2_copy_function_destroy :: Ptr DuckDBV2CopyFunctionHandle -> IO DuckDBV2Error

{- | Sets the optional statistics callback of the @COPY ... TO@ side.

The statistics callback reports statistics about a written file, such as the number of rows and bytes it holds, which
@COPY ... TO@ returns when the statement asks for them with @RETURN_STATS@. It is invoked once per output file, after
the finalize callback of that file, and only when the statement asks for the statistics. Without a statistics
callback, @RETURN_STATS@ is rejected for the function.

history:
- unstable: v2.0.0


@function@: The function to set the callback of.

@callback@: The callback to set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_to_set_statistics_callback"
    c_duckdb_v2_copy_to_set_statistics_callback :: DuckDBV2CopyFunctionHandle -> FunPtr DuckDBV2CopyToStatisticsCallbackFn -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the user data set on the copy function via @duckdb_v2_copy_function_set_user_data()@.

history:
- unstable: v2.0.0


@info@: The statistics info handle.

@data@: Receives the user data pointer, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_to_statistics_get_user_data"
    c_duckdb_v2_copy_to_statistics_get_user_data :: DuckDBV2CopyToStatisticsInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the bind data set by the function's @COPY ... TO@ bind callback.

history:
- unstable: v2.0.0


@info@: The statistics info handle.

@data@: Receives the bind data pointer, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_to_statistics_get_bind_data"
    c_duckdb_v2_copy_to_statistics_get_bind_data :: DuckDBV2CopyToStatisticsInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the init data set by the function's @COPY ... TO@ init callback for the file being reported.

history:
- unstable: v2.0.0


@info@: The statistics info handle.

@data@: Receives the init data pointer, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_to_statistics_get_init_data"
    c_duckdb_v2_copy_to_statistics_get_init_data :: DuckDBV2CopyToStatisticsInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Reports how many rows were written to the file. Reported as 0 when the callback does not set it.

history:
- unstable: v2.0.0


@info@: The statistics info handle.

@row_count@: The number of rows written to the file.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_to_statistics_set_row_count"
    c_duckdb_v2_copy_to_statistics_set_row_count :: DuckDBV2CopyToStatisticsInfoHandle -> DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Reports the size of the written file in bytes. Reported as 0 when the callback does not set it.

history:
- unstable: v2.0.0


@info@: The statistics info handle.

@file_size_bytes@: The size of the file in bytes.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_copy_to_statistics_set_file_size"
    c_duckdb_v2_copy_to_statistics_set_file_size :: DuckDBV2CopyToStatisticsInfoHandle -> DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the type of an expression node.

The type decides which of the type-specific accessors apply, and what the node's children mean; see
@EXPRESSION_TYPE@. A node of a type this API does not model reports @EXPRESSION_TYPE_INVALID@.

history:
- stable: v2.0.0


@expression@: The expression node.

@type@: Receives the node type.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_expression_get_type"
    c_duckdb_v2_expression_get_type :: DuckDBV2ExpressionHandle -> Ptr DuckDBV2ExpressionType -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the logical type the expression node evaluates to.

For a cast this is the target type. The returned type is owned by the caller and must be destroyed via
@duckdb_v2_logical_type_destroy()@.

history:
- stable: v2.0.0


@expression@: The expression node.

@type@: Receives the owned logical type.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_expression_get_return_type"
    c_duckdb_v2_expression_get_return_type :: DuckDBV2ExpressionHandle -> Ptr DuckDBV2LogicalTypeHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the number of child nodes of an expression node.

Works for every node type, including @EXPRESSION_TYPE_INVALID@. Constants, parameters and column references have no
children.

history:
- stable: v2.0.0


@expression@: The expression node.

@count@: Receives the number of children.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_expression_get_child_count"
    c_duckdb_v2_expression_get_child_count :: DuckDBV2ExpressionHandle -> Ptr DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves a child node of an expression node by index.

Fails if the index is out of bounds. Children are ordered as the node type describes (see @EXPRESSION_TYPE@). The
child is borrowed and shares the lifetime of its parent.

history:
- stable: v2.0.0


@expression@: The expression node.

@index@: The index of the child to retrieve.

@child@: Receives the borrowed child node.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_expression_get_child"
    c_duckdb_v2_expression_get_child :: DuckDBV2ExpressionHandle -> DuckDBV2Idx -> Ptr DuckDBV2ExpressionHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the value of a constant node.

Fails if the node is not of type @EXPRESSION_TYPE_VALUE_CONSTANT@. The returned value is owned by the caller and must
be destroyed via @duckdb_v2_value_destroy()@.

history:
- stable: v2.0.0


@expression@: The constant node.

@value@: Receives the owned constant value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_expression_constant_get_value"
    c_duckdb_v2_expression_constant_get_value :: DuckDBV2ExpressionHandle -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the column a column reference node points at.

Fails if the node is not of type @EXPRESSION_TYPE_BOUND_COLUMN_REF@. The index counts the columns of the operator the
predicate is evaluated against, which is not necessarily the full set of columns that operator can produce: a
callback resolves it through whatever handed it the expression, e.g.
@duckdb_v2_table_function_filter_pushdown_get_column_index()@ for a filter offered to a table function.

history:
- stable: v2.0.0


@expression@: The column reference node.

@index@: Receives the column index.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_expression_column_ref_get_index"
    c_duckdb_v2_expression_column_ref_get_index :: DuckDBV2ExpressionHandle -> Ptr DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the name of the scalar function a function node calls.

Applies to @EXPRESSION_TYPE_BOUND_FUNCTION@ and to the node types that are function calls underneath: the
comparisons, @EXPRESSION_TYPE_COMPARE_BETWEEN@ and @EXPRESSION_TYPE_OPERATOR_CAST@. Fails for any other node type.
For a comparison the name is its operator, e.g. @<@; for @BETWEEN@ and casts it is an internal name, so dispatch on
the type rather than the name for those. The name is borrowed and shares the lifetime of the node.

history:
- stable: v2.0.0


@expression@: The function node.

@name@: Receives a borrowed view of the function name.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_expression_function_get_name"
    c_duckdb_v2_expression_function_get_name :: DuckDBV2ExpressionHandle -> Ptr DuckDBV2IdentifierT -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the qualified name of the scalar function a function node calls.

Applies to the same node types as @duckdb_v2_expression_function_get_name()@, and fails for any other. The name is
qualified with the catalog and schema the function was resolved in, where known. The returned name is owned by the
caller and must be destroyed via @duckdb_v2_qname_destroy()@.

history:
- stable: v2.0.0


@expression@: The function node.

@name@: Receives the owned qualified name.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_expression_function_get_qname"
    c_duckdb_v2_expression_function_get_qname :: DuckDBV2ExpressionHandle -> Ptr DuckDBV2QnameHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns whether a cast node is a regular @CAST@ or a @TRY_CAST@.

Fails if the node is not of type @EXPRESSION_TYPE_OPERATOR_CAST@. A @TRY_CAST@ yields NULL for a value it cannot
convert where a regular @CAST@ would fail the query.

history:
- stable: v2.0.0


@expression@: The cast node.

@mode@: Receives the cast mode.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_expression_cast_get_mode"
    c_duckdb_v2_expression_cast_get_mode :: DuckDBV2ExpressionHandle -> Ptr DuckDBV2CastMode -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a logical type from a type id plus value parameters.

The id-keyed twin of context_create_type_from_name: the type id names the kind, and the parameters bind it. With
param_count 0 it instantiates a primitive directly, without touching the catalog: BOOLEAN, TINYINT..BIGINT,
UTINYINT..UBIGINT, HUGEINT, UHUGEINT, FLOAT, DOUBLE, DATE, every TIME and TIMESTAMP variant, INTERVAL, VARCHAR, BLOB,
BIT, BIGNUM, and UUID. ANY is accepted as well.

ANY is a function-signature wildcard, constructible here so it can be passed to the function parameter and varargs
setters, as a fixed-arity ANY parameter or an ANY varargs type. Data-creating surfaces reject it: value and data
chunk creation, scalar and aggregate return types, table function result columns, cast source and target types, and
custom type registration.

With parameters, the id resolves to its canonical type name and binds through the same path as
context_create_type_from_name, so the parameterized kinds construct here too: decimal(width, scale); list(T);
array(T, size); map(K, V); struct(fields); union(members); enum(entries); and varchar with a named "collation"
parameter. Parameters are (name, value) pairs in two parallel arrays, exactly as for context_create_type_from_name.

Returns ERROR_INPUT_INVALID when param_count is 0 and the id needs parameters (DECIMAL, LIST, STRUCT, TUPLE, MAP,
ARRAY, UNION, ENUM, VARIANT, GEOMETRY), for the bind-time-only ids (SQLNULL, UNKNOWN), for TYPE — construct that via
context_create_type_from_text — and for INVALID.

history:
- stable: v2.0.0


@ctx@: The context supplying the catalog and active transaction.

@type_id@: The type id to instantiate.

@param_names@: Optional. An array of param_count parameter names; a {NULL, 0} entry is positional. Pass NULL for
all-positional parameters.

@param_values@: An array of param_count parameter values. Borrowed (copied in). Pass NULL when param_count is 0.

@param_count@: The number of parameters. 0 instantiates a parameterless primitive.

@out_type@: Receives the new logical type handle.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_context_create_type_from_id"
    c_duckdb_v2_context_create_type_from_id :: DuckDBV2ContextHandle -> DuckDBV2LogicalTypeId -> Ptr DuckDBV2IdentifierT -> Ptr DuckDBV2ValueHandle -> DuckDBV2Idx -> Ptr DuckDBV2LogicalTypeHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a logical type from a type name plus value parameters.

The generic constructor: resolves the name in the context's catalog and binds it with the given parameters, exactly
as SQL binds a type expression. Built-in parameterized kinds and registered extension types construct through this
same call.

An unqualified name is resolved along the search path first and then in the system catalog, which is where the
built-in kinds live. A qualified name is resolved exactly as written, with no system-catalog fallback, so it names
the type in that catalog or schema or fails.

Parameters are (name, value) pairs in two parallel arrays. param_names may be NULL to make every parameter
positional, and a {NULL, 0} entry makes that one parameter positional. Child types cross as TYPE values, built with
value_create_type_with_context / _with_connection. The built-in shapes are: decimal(width, scale); list(T); array(T,
size); map(K, V); struct(fields, as named or all-positional TYPE values); union(members, as named TYPE values);
enum(entries, as VARCHAR values); and varchar with a named "collation" VARCHAR parameter.

A name that resolves to a type with no bind function takes no parameters, and passing any fails. Bind errors —
unknown name, wrong parameter count or types — surface from the call.

Runs in the caller's context scope, as create_type_from_text does: reach it from a bind-phase callback or another
context-holding scope, not from an exec-phase worker callback. External callers holding only a connection use
connection_create_type_from_name instead.

The returned logical type is caller-owned and must be destroyed via logical_type_destroy. A type resolved from the
catalog shares database-owned storage, such as an ENUM dictionary, so destroy it before closing the database. This is
the inverse of logical_type_get_param_count / logical_type_get_param.

history:
- stable: v2.0.0


@ctx@: The context supplying the catalog and active transaction.

@name@: The type name to resolve. Parts are matched case-insensitively; qualify it to name a type in a particular
catalog or schema.

@param_names@: Optional. An array of param_count parameter names; a {NULL, 0} entry is positional. Pass NULL for
all-positional parameters.

@param_values@: An array of param_count parameter values. Borrowed (copied in). Pass NULL when param_count is 0.

@param_count@: The number of parameters.

@out_type@: Receives the new logical type handle.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_context_create_type_from_name"
    c_duckdb_v2_context_create_type_from_name :: DuckDBV2ContextHandle -> DuckDBV2QnameHandle -> Ptr DuckDBV2IdentifierT -> Ptr DuckDBV2ValueHandle -> DuckDBV2Idx -> Ptr DuckDBV2LogicalTypeHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a logical type by parsing SQL text.

Parses a SQL type expression in the given context and returns the bound logical type. It accepts primitives
("INTEGER"), parameterized kinds ("DECIMAL(18,3)", "INTEGER[]", "STRUCT(a INTEGER, b VARCHAR)", "MAP(VARCHAR,
INTEGER)", "INTEGER[3]", "UNION(i INTEGER, s VARCHAR)", "ENUM(@a@, @b@)"), and catalog-registered type names, both
user-defined and from extensions. A catalog type name binds to its structural type, and the name is not preserved as
an alias. Names are case-insensitive. Parse and bind errors surface from the call.

Runs in the caller's context scope: a context handle arrives with the context lock held and a transaction active, as
in a function bind callback or custom type registration. Catalog-touching context calls belong in bind-phase
callbacks and other context-holding scopes, not in exec-phase worker callbacks. External callers holding only a
connection use connection_create_type_from_text instead.

The returned logical type is caller-owned and must be destroyed via logical_type_destroy. A type resolved from the
catalog shares database-owned storage, such as an ENUM dictionary, so destroy it before closing the database. This is
the inverse of logical_type_to_text for every constructible kind.

history:
- stable: v2.0.0


@ctx@: The context supplying the catalog and active transaction.

@text@: View of the SQL type expression to parse.

@out_type@: Receives the new logical type handle.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_context_create_type_from_text"
    c_duckdb_v2_context_create_type_from_text :: DuckDBV2ContextHandle -> Ptr DuckDBV2Str -> Ptr DuckDBV2LogicalTypeHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a logical type from a type id plus value parameters.

The id-keyed twin of connection_create_type_from_name: the type id names the kind, and the parameters bind it. With
param_count 0 it instantiates a primitive directly, without touching the catalog: BOOLEAN, TINYINT..BIGINT,
UTINYINT..UBIGINT, HUGEINT, UHUGEINT, FLOAT, DOUBLE, DATE, every TIME and TIMESTAMP variant, INTERVAL, VARCHAR, BLOB,
BIT, BIGNUM, and UUID. ANY is accepted as well.

ANY is a function-signature wildcard, constructible here so it can be passed to the function parameter and varargs
setters, as a fixed-arity ANY parameter or an ANY varargs type. Data-creating surfaces reject it: value and data
chunk creation, scalar and aggregate return types, table function result columns, cast source and target types, and
custom type registration.

With parameters, the id resolves to its canonical type name and binds through the same path as
connection_create_type_from_name, so the parameterized kinds construct here too: decimal(width, scale); list(T);
array(T, size); map(K, V); struct(fields); union(members); enum(entries); and varchar with a named "collation"
parameter. Parameters are (name, value) pairs in two parallel arrays, exactly as for
connection_create_type_from_name.

Returns ERROR_INPUT_INVALID when param_count is 0 and the id needs parameters (DECIMAL, LIST, STRUCT, TUPLE, MAP,
ARRAY, UNION, ENUM, VARIANT, GEOMETRY), for the bind-time-only ids (SQLNULL, UNKNOWN), for TYPE — construct that via
connection_create_type_from_text — and for INVALID.

history:
- stable: v2.0.0


@conn@: The connection supplying the catalog and active transaction.

@type_id@: The type id to instantiate.

@param_names@: Optional. An array of param_count parameter names; a {NULL, 0} entry is positional. Pass NULL for
all-positional parameters.

@param_values@: An array of param_count parameter values. Borrowed (copied in). Pass NULL when param_count is 0.

@param_count@: The number of parameters. 0 instantiates a parameterless primitive.

@out_type@: Receives the new logical type handle.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_connection_create_type_from_id"
    c_duckdb_v2_connection_create_type_from_id :: DuckDBV2ConnectionHandle -> DuckDBV2LogicalTypeId -> Ptr DuckDBV2IdentifierT -> Ptr DuckDBV2ValueHandle -> DuckDBV2Idx -> Ptr DuckDBV2LogicalTypeHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a logical type from a type name plus value parameters, using a connection.

The same as context_create_type_from_name, except that the catalog and transaction come from a connection: the bind
runs in its own transaction on that connection's context. Use it from outside DuckDB, where a connection — but no
context — is in hand.

Parameters are (name, value) pairs in two parallel arrays, exactly as for context_create_type_from_name.

The returned logical type is caller-owned and must be destroyed via logical_type_destroy. A type resolved from the
catalog shares database-owned storage, such as an ENUM dictionary, so destroy it before closing the database.

history:
- stable: v2.0.0


@conn@: The connection supplying the catalog and active transaction.

@name@: The type name to resolve. Parts are matched case-insensitively; qualify it to name a type in a particular
catalog or schema.

@param_names@: Optional. An array of param_count parameter names; a {NULL, 0} entry is positional. Pass NULL for
all-positional parameters.

@param_values@: An array of param_count parameter values. Borrowed (copied in). Pass NULL when param_count is 0.

@param_count@: The number of parameters.

@out_type@: Receives the new logical type handle.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_connection_create_type_from_name"
    c_duckdb_v2_connection_create_type_from_name :: DuckDBV2ConnectionHandle -> DuckDBV2QnameHandle -> Ptr DuckDBV2IdentifierT -> Ptr DuckDBV2ValueHandle -> DuckDBV2Idx -> Ptr DuckDBV2LogicalTypeHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a logical type by parsing SQL text, using a connection.

The same as context_create_type_from_text, except that the catalog and transaction come from a connection: the parse
and bind run in their own transaction on that connection's context. Use it from outside DuckDB, where a connection —
but no context — is in hand.

The returned logical type is caller-owned and must be destroyed via logical_type_destroy. A type resolved from the
catalog shares database-owned storage, such as an ENUM dictionary, so destroy it before closing the database.

history:
- stable: v2.0.0


@conn@: The connection supplying the catalog and active transaction.

@text@: View of the SQL type expression to parse.

@out_type@: Receives the new logical type handle.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_connection_create_type_from_text"
    c_duckdb_v2_connection_create_type_from_text :: DuckDBV2ConnectionHandle -> Ptr DuckDBV2Str -> Ptr DuckDBV2LogicalTypeHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a copy of a logical type.

On success, writes the new caller-owned handle into *out_type; destroy it via logical_type_destroy.

history:
- stable: v2.0.0


@type@: The logical type to copy.

@out_type@: Receives the new logical type handle.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_logical_type_copy"
    c_duckdb_v2_logical_type_copy :: DuckDBV2LogicalTypeHandle -> Ptr DuckDBV2LogicalTypeHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Destroys a logical type handle.

Null-safe: passing nullptr or a slot already set to nullptr is a no-op. On success the slot is set to nullptr.

history:
- stable: v2.0.0


@type@: The logical type to destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_logical_type_destroy"
    c_duckdb_v2_logical_type_destroy :: Ptr DuckDBV2LogicalTypeHandle -> IO DuckDBV2Error

{- | Compares two logical types for deep equality.

Two types are equal when they agree in kind and in every parameter, recursively. DECIMAL(10, 2) equals DECIMAL(10,
2), but not DECIMAL(10, 3) and not FLOAT; two STRUCTs are equal when they have the same field names in the same order
and equal field types.

history:
- stable: v2.0.0


@left@: The first logical type.

@right@: The second logical type.

@result@: Receives the result of the comparison.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_logical_type_is_equal"
    c_duckdb_v2_logical_type_is_equal :: DuckDBV2LogicalTypeHandle -> DuckDBV2LogicalTypeHandle -> Ptr CBool -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the logical type id.

history:
- stable: v2.0.0


@type@: The logical type.

@out_id@: Receives the type id.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_logical_type_get_id"
    c_duckdb_v2_logical_type_get_id :: DuckDBV2LogicalTypeHandle -> Ptr DuckDBV2LogicalTypeId -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Borrows the logical type's name.

The alias when one is set — an extension or user-defined name such as "POINT_2D" — otherwise the canonical name of
the type id, such as "INTEGER", "DECIMAL", or "TIMESTAMP WITH TIME ZONE". Never the empty view. This is exactly the
name vocabulary create_type_from_name accepts. The view is valid until the logical type is destroyed; a canonical
name points at static storage.

history:
- stable: v2.0.0


@type@: The logical type.

@out_name@: Receives a borrowed view of the name (alias when set, else the id's canonical name).

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_logical_type_get_name"
    c_duckdb_v2_logical_type_get_name :: DuckDBV2LogicalTypeHandle -> Ptr DuckDBV2IdentifierT -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Renders a logical type as SQL text.

An aliased type renders as its alias, and create_type_from_text resolves that spelling only when the name is
registered in the connection's catalog. The text round-trips through create_type_from_text for every constructible
kind, with one exception: ANY renders as "ANY", but create_type_from_text cannot parse it back, since ANY is a
signature wildcard rather than a parseable SQL type.

Writes into a caller-supplied buffer, so nothing is allocated on the caller's behalf and nothing has to be freed.
Pass out_text = NULL to size the buffer without rendering into it: out_length then receives the length, and
out_capacity is ignored. With out_text != NULL, out_capacity must be at least out_length + 1, or the call returns
ERROR_INPUT_OBJECT_SIZE with out_length set to the required length and out_text left untouched.

out_length never counts the terminator, but a successful write always appends one, so the buffer is usable as a C
string.

history:
- stable: v2.0.0


@type@: The logical type.

@out_text@: Caller-owned buffer receiving the text plus a null terminator, or NULL to only report the required
length in out_length.

@out_capacity@: Bytes available in out_text, terminator included. Ignored when out_text is NULL.

@out_length@: Receives the text length excluding the null terminator — written on success and on
ERROR_INPUT_OBJECT_SIZE.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_logical_type_to_text"
    c_duckdb_v2_logical_type_to_text :: DuckDBV2LogicalTypeHandle -> Ptr CChar -> DuckDBV2Idx -> Ptr DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the number of value parameters of a logical type.

The inspection dual of create_type_from_name: these are the parameters that reconstruct the type through it. Per
kind: DECIMAL 2 (width, scale); LIST 1 (element type); ARRAY 2 (element type, size); MAP 2 (key type, value type);
STRUCT and TUPLE one per field; UNION one per member; ENUM one per dictionary entry; VARCHAR 1 when a collation is
set, else 0; GEOMETRY 1 when a coordinate system is set, else 0; everything else 0. A bound type reports only what it
actually carries, so a bind-time modifier that is not retained — an ignored VARCHAR length, say — does not reappear.

history:
- stable: v2.0.0


@type@: The logical type.

@out_count@: Receives the number of parameters.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_logical_type_get_param_count"
    c_duckdb_v2_logical_type_get_param_count :: DuckDBV2LogicalTypeHandle -> Ptr DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns one value parameter of a logical type.

out_name receives a borrowed view of the parameter name — a STRUCT field name, a UNION member name, "collation" — or
the empty view {NULL, 0} for a positional parameter. A non-empty view is valid until the logical type is destroyed.
out_value receives an owned value, destroyed via value_destroy: child types come back as TYPE values (unwrap them
with value_get_type), DECIMAL width and scale as UTINYINT, ARRAY size as BIGINT, and ENUM dictionary entries and
collations as VARCHAR. An out-of-range index returns ERROR_INPUT_INVALID. Each call allocates one owned value.

history:
- stable: v2.0.0


@type@: The logical type.

@index@: The parameter index, in [0, param_count).

@out_name@: Receives a borrowed view of the parameter name, or the empty view {NULL, 0} for a positional
parameter.

@out_value@: Receives the owned parameter value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_logical_type_get_param"
    c_duckdb_v2_logical_type_get_param :: DuckDBV2LogicalTypeHandle -> DuckDBV2Idx -> Ptr DuckDBV2IdentifierT -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a logical type that is an alias of another logical type.

The alias keeps the base type's internal representation, so executing against it needs no special handling, while
remaining logically distinct from the base type. Intended for custom type bind callbacks, where both the base type
and the name come from the bind info.

Scoped like the rest of the create_type family: the alias is resolved against the catalog reachable from the context.
An empty alias name returns ERROR_INPUT_INVALID.

history:
- stable: v2.0.0


@ctx@: The context to resolve the alias against.

@base_type@: The logical type to alias. Typically the base type supplied in the custom type bind info.

@alias_name@: The name for the resulting type. Typically the name of the custom type being constructed, also
available from the bind info.

@out_type@: Receives the new type: the base type's internal representation under the given alias name.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_context_create_type_with_alias"
    c_duckdb_v2_context_create_type_with_alias :: DuckDBV2ContextHandle -> DuckDBV2LogicalTypeHandle -> Ptr DuckDBV2IdentifierT -> Ptr DuckDBV2LogicalTypeHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a logical type that is an alias of another logical type.

The alias keeps the base type's internal representation, so executing against it needs no special handling, while
remaining logically distinct from the base type. Intended for custom type bind callbacks, where both the base type
and the name come from the bind info.

Scoped like the rest of the create_type family: the alias is resolved against the catalog reachable from the
connection. An empty alias name returns ERROR_INPUT_INVALID.

history:
- stable: v2.0.0


@conn@: The connection to resolve the alias against.

@base_type@: The logical type to alias. Typically the base type supplied in the custom type bind info.

@alias_name@: The name for the resulting type. Typically the name of the custom type being constructed, also
available from the bind info.

@out_type@: Receives the new type: the base type's internal representation under the given alias name.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_connection_create_type_with_alias"
    c_duckdb_v2_connection_create_type_with_alias :: DuckDBV2ConnectionHandle -> DuckDBV2LogicalTypeHandle -> Ptr DuckDBV2IdentifierT -> Ptr DuckDBV2LogicalTypeHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a new replacement scan that will be registered on the connection.

The scan is visible only to queries on this connection and is released when the connection is destroyed. It is
consulted before every instance-wide scan, so it can claim a name that a built-in scan would otherwise take. The scan
starts out empty: configure it with @duckdb_v2_replacement_scan_set_callback()@ and optionally
@duckdb_v2_replacement_scan_set_user_data()@, then make it available with @duckdb_v2_replacement_scan_register()@.
The caller owns the returned handle and must destroy it with @duckdb_v2_replacement_scan_destroy()@, also after
registration.

history:
- stable: v2.0.0


@connection@: The connection to create the scan on.

@scan@: On success, receives the newly created replacement scan. Owned by the caller.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_replacement_scan_create_with_connection"
    c_duckdb_v2_replacement_scan_create_with_connection :: DuckDBV2ConnectionHandle -> Ptr DuckDBV2ReplacementScanHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a new replacement scan that will be registered on the instance.

The scan is visible to every connection to the instance and lives until the instance is destroyed. The scan starts
out empty: configure it with @duckdb_v2_replacement_scan_set_callback()@ and optionally
@duckdb_v2_replacement_scan_set_user_data()@, then make it available with @duckdb_v2_replacement_scan_register()@.
The caller owns the returned handle and must destroy it with @duckdb_v2_replacement_scan_destroy()@, also after
registration.

history:
- stable: v2.0.0


@instance@: The instance to create the scan on.

@scan@: On success, receives the newly created replacement scan. Owned by the caller.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_replacement_scan_create_with_instance"
    c_duckdb_v2_replacement_scan_create_with_instance :: DuckDBV2InstanceHandle -> Ptr DuckDBV2ReplacementScanHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a new replacement scan that will be registered on the loading extension's instance.

Use this from an extension load callback, where an extension handle is available. The scan is visible to every
connection to that instance and lives until the instance is destroyed. The scan starts out empty: configure it with
@duckdb_v2_replacement_scan_set_callback()@ and optionally @duckdb_v2_replacement_scan_set_user_data()@, then make it
available with @duckdb_v2_replacement_scan_register()@. The caller owns the returned handle and must destroy it with
@duckdb_v2_replacement_scan_destroy()@, also after registration.

history:
- stable: v2.0.0


@extension@: The extension to create the scan on.

@scan@: On success, receives the newly created replacement scan. Owned by the caller.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_replacement_scan_create_with_extension"
    c_duckdb_v2_replacement_scan_create_with_extension :: DuckDBV2ExtensionHandle -> Ptr DuckDBV2ReplacementScanHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets the callback of the replacement scan.

The callback is invoked during query planning for each table reference the catalog could not resolve. It can inspect
the unresolved name via @duckdb_v2_replacement_scan_get_name()@, and claim the reference via
@duckdb_v2_replacement_scan_set_function_name()@, @duckdb_v2_replacement_scan_set_collection()@ or
@duckdb_v2_replacement_scan_set_subquery()@; returning without claiming declines it. The context passed to the
callback may be used to read settings, but not to run queries. A callback must be set before registration.

history:
- stable: v2.0.0


@scan@: The scan to set the callback of.

@callback@: The callback to set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_replacement_scan_set_callback"
    c_duckdb_v2_replacement_scan_set_callback :: DuckDBV2ReplacementScanHandle -> FunPtr DuckDBV2ReplacementScanCallbackFn -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets arbitrary user data on the replacement scan.

Associates an opaque pointer with the scan, retrievable from the callback via
@duckdb_v2_replacement_scan_get_user_data()@. The opaque handle bundles the pointer with an optional destructor,
invoked when the data is no longer needed, at the latest when the scan's scope ends. The callback may be invoked from
several connections at once, so the data must be safe to read concurrently.

history:
- stable: v2.0.0


@scan@: The scan to set the user data of.

@data@: Opaque handle bundling the user data pointer plus an optional destructor.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_replacement_scan_set_user_data"
    c_duckdb_v2_replacement_scan_set_user_data :: DuckDBV2ReplacementScanHandle -> Ptr DuckDBV2Opaque -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the user data set via @duckdb_v2_replacement_scan_set_user_data()@.

history:
- stable: v2.0.0


@info@: The info handle.

@data@: Receives the user data pointer, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_replacement_scan_get_user_data"
    c_duckdb_v2_replacement_scan_get_user_data :: DuckDBV2ReplacementScanInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the name the catalog could not resolve.

The name as written in the query, as a path: an unqualified reference has a single part, and a qualified one carries
its catalog and schema before it -- see @duckdb_v2_qname_get_part()@. For a file-backed reference the single part is
the path with the quotes stripped. The returned name is owned by the caller and must be destroyed via
@duckdb_v2_qname_destroy()@; being owned, it may outlive the callback.

history:
- stable: v2.0.0


@info@: The info handle.

@name@: Receives the unresolved name. Owned by the caller; destroy via @duckdb_v2_qname_destroy()@.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_replacement_scan_get_name"
    c_duckdb_v2_replacement_scan_get_name :: DuckDBV2ReplacementScanInfoHandle -> Ptr DuckDBV2QnameHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Claims the reference by naming a table function to read instead.

Arguments to the function are added with @duckdb_v2_replacement_scan_add_argument()@ and
@duckdb_v2_replacement_scan_add_named_argument()@. The name is borrowed and copied, and its parts are matched
case-insensitively. A qualified name targets a function in a particular schema or catalog, exactly as writing it out
in SQL would. The name is not resolved here: an unknown function fails later, when the replacement is bound. Calling
this again replaces the previous name and keeps the arguments added so far.

The three claim forms, @duckdb_v2_replacement_scan_set_function_name()@,
@duckdb_v2_replacement_scan_set_collection()@ and @duckdb_v2_replacement_scan_set_subquery()@, are mutually
exclusive: claiming the reference through a second, different form results in an error.

history:
- stable: v2.0.0


@info@: The info handle.

@name@: The table function to read instead. Borrowed and copied.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_replacement_scan_set_function_name"
    c_duckdb_v2_replacement_scan_set_function_name :: DuckDBV2ReplacementScanInfoHandle -> DuckDBV2QnameHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Appends a positional argument to the claimed table function.

Positional arguments are passed in the order they are added, before any named ones. The value is borrowed and copied,
so the caller may destroy it after the call. Requires @duckdb_v2_replacement_scan_set_function_name()@ to have been
called first; otherwise the call results in an error.

history:
- stable: v2.0.0


@info@: The info handle.

@value@: The argument value. Borrowed and copied.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_replacement_scan_add_argument"
    c_duckdb_v2_replacement_scan_add_argument :: DuckDBV2ReplacementScanInfoHandle -> DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Appends a named argument to the claimed table function.

Equivalent to writing @name := value@ at the call site. The name and the value are borrowed and copied, so the caller
may destroy the value after the call. Requires @duckdb_v2_replacement_scan_set_function_name()@ to have been called
first; otherwise the call results in an error.

history:
- stable: v2.0.0


@info@: The info handle.

@name@: The name of the parameter to bind the value to. Borrowed and copied.

@value@: The argument value. Borrowed and copied.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_replacement_scan_add_named_argument"
    c_duckdb_v2_replacement_scan_add_named_argument :: DuckDBV2ReplacementScanInfoHandle -> Ptr DuckDBV2IdentifierT -> DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Claims the reference by naming a column data collection to read instead.

The collection is borrowed, not copied: the caller keeps ownership and must keep it alive, and must not clear, reset
or destroy it, for as long as any result reading it is live, since the result scans its buffers directly.

A prepared statement extends that lifetime well beyond its own results. Preparing a statement over a claimed name
captures the borrow in the plan, and since a collection claim reads no database there is nothing to invalidate that
plan: @duckdb_v2_prepared_statement_reuses_plan()@ reports true, and every later execution reuses the captured borrow
without consulting the callback again. So dropping the name from whatever the callback resolves against does not
release the collection, and releasing it while such a statement is live leaves the next execution reading freed
memory. Destroy every prepared statement over a claimed name before clearing, resetting or destroying the collection
behind it.

By default the collection's columns are named col1..colN. Pass @column_names@ to name them instead. When supplied,
@column_count@ must equal the collection's column count and no name may be empty; pass NULL and 0 for the default
names.

The three claim forms, @duckdb_v2_replacement_scan_set_function_name()@,
@duckdb_v2_replacement_scan_set_collection()@ and @duckdb_v2_replacement_scan_set_subquery()@, are mutually
exclusive: claiming the reference through a second, different form results in an error.

history:
- stable: v2.0.0


@info@: The info handle.

@collection@: The collection to read instead. Borrowed; the caller keeps ownership and must keep it alive.

@column_names@: Optional. An array of @column_count@ column names, in order. Pass NULL for the default names
col1..colN.

@column_count@: The number of names in @column_names@. Must equal the collection's column count, or 0 for the
default names.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_replacement_scan_set_collection"
    c_duckdb_v2_replacement_scan_set_collection :: DuckDBV2ReplacementScanInfoHandle -> DuckDBV2ColumnDataCollectionHandle -> Ptr DuckDBV2IdentifierT -> DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Claims the reference by naming a query to read instead.

The text is parsed at the call and must contain exactly one SELECT statement: a syntax error, several statements or a
statement of another kind results in an error. The text is borrowed for the call only. Prefer
@duckdb_v2_replacement_scan_set_function_name()@ when a single table function call suffices, as it avoids the parse
and keeps the plan flatter.

The three claim forms, @duckdb_v2_replacement_scan_set_function_name()@,
@duckdb_v2_replacement_scan_set_collection()@ and @duckdb_v2_replacement_scan_set_subquery()@, are mutually
exclusive: claiming the reference through a second, different form results in an error.

history:
- stable: v2.0.0


@info@: The info handle.

@sql@: The SELECT statement to read instead. Borrowed for the call only.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_replacement_scan_set_subquery"
    c_duckdb_v2_replacement_scan_set_subquery :: DuckDBV2ReplacementScanInfoHandle -> Ptr DuckDBV2Str -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets the alias the claimed replacement is bound under.

Optional, and independent of the claim form. An alias written in the query takes precedence over this one; without
either, the table name of the reference is used, which for a file-backed reference is the path. The alias is borrowed
and copied.

history:
- stable: v2.0.0


@info@: The info handle.

@alias@: The alias to bind the replacement under. Borrowed and copied.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_replacement_scan_set_alias"
    c_duckdb_v2_replacement_scan_set_alias :: DuckDBV2ReplacementScanInfoHandle -> Ptr DuckDBV2IdentifierT -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Registers the replacement scan, making it consulted for names the catalog cannot resolve.

The scan is registered on the target given at creation: the connection, the instance or the loading extension's
instance. Registration requires a callback. Scans are consulted in registration order within their scope,
connection-scoped ones before instance-wide ones, and the first to claim a name wins. A scan cannot be registered
twice, and a registered scan cannot be unregistered: it lives until its scope ends. Registering an instance-wide scan
while queries are binding on other connections is not thread-safe; register from an extension load callback or before
issuing queries. The caller still owns the handle after registration and must destroy it with
@duckdb_v2_replacement_scan_destroy()@, which does not affect the registered scan.

history:
- stable: v2.0.0


@scan@: The scan to register.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_replacement_scan_register"
    c_duckdb_v2_replacement_scan_register :: DuckDBV2ReplacementScanHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Destroys the replacement scan, releasing its resources.

Null-safe: passing a null pointer or null handle is a no-op. The handle is set to null on return to prevent
double-destruction. Destroying the handle after registration does not affect the registered scan.

history:
- stable: v2.0.0


@scan@: The scan to destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_replacement_scan_destroy"
    c_duckdb_v2_replacement_scan_destroy :: Ptr DuckDBV2ReplacementScanHandle -> IO DuckDBV2Error

{- | Creates a new scalar function that will be registered on the connection's database.

The function starts out empty: configure it with the setter functions (e.g. @duckdb_v2_scalar_function_set_name()@,
@duckdb_v2_scalar_function_set_exec_callback()@, etc.) and the signature obtained via
@duckdb_v2_scalar_function_get_signature()@, then make it available with @duckdb_v2_scalar_function_register()@. The
caller owns the returned handle and must destroy it with @duckdb_v2_scalar_function_destroy()@, also after
registration.

history:
- stable: v2.0.0


@connection@: The connection to create the function in.

@function@: On success, receives the newly created scalar function. Owned by the caller.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_scalar_function_create_with_connection"
    c_duckdb_v2_scalar_function_create_with_connection :: DuckDBV2ConnectionHandle -> Ptr DuckDBV2ScalarFunctionHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a new scalar function that will be registered on the loading extension's database.

Use this from an extension load callback, where an extension handle is available. The function starts out empty:
configure it with the setter functions (e.g. @duckdb_v2_scalar_function_set_name()@,
@duckdb_v2_scalar_function_set_exec_callback()@, etc.) and the signature obtained via
@duckdb_v2_scalar_function_get_signature()@, then make it available with @duckdb_v2_scalar_function_register()@. The
caller owns the returned handle and must destroy it with @duckdb_v2_scalar_function_destroy()@, also after
registration.

history:
- stable: v2.0.0


@extension@: The extension to create the function in.

@function@: On success, receives the newly created scalar function. Owned by the caller.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_scalar_function_create_with_extension"
    c_duckdb_v2_scalar_function_create_with_extension :: DuckDBV2ExtensionHandle -> Ptr DuckDBV2ScalarFunctionHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets the name of the scalar function.

The name is borrowed and copied. Calling this again replaces the previous name. A name must be set before
registration.

history:
- stable: v2.0.0


@function@: The function to set the name of.

@name@: The name to set. Borrowed and copied.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_scalar_function_set_name"
    c_duckdb_v2_scalar_function_set_name :: DuckDBV2ScalarFunctionHandle -> Ptr DuckDBV2IdentifierT -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the function's signature so it can be configured.

Add parameters with @duckdb_v2_function_signature_add_parameter()@ and set the return type with
@duckdb_v2_function_signature_set_return_type()@. The signature is modified in place; the function must be given a
signature with a return type before registration.

history:
- stable: v2.0.0


@function@: The function to get the signature of.

@sig@: The returned signature. Borrowed and valid for the lifetime of the function handle.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_scalar_function_get_signature"
    c_duckdb_v2_scalar_function_get_signature :: DuckDBV2ScalarFunctionHandle -> Ptr DuckDBV2FunctionSignatureHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets arbitrary user data on the scalar function.

Associates an opaque pointer with the function, retrievable from the callbacks via
@duckdb_v2_function_bind_get_user_data()@, @duckdb_v2_scalar_function_init_get_user_data()@ and
@duckdb_v2_scalar_function_exec_get_user_data()@. The opaque handle bundles the pointer with an optional destructor,
invoked when the data is no longer needed.

history:
- stable: v2.0.0


@function@: The function to set the user data of.

@data@: Opaque handle bundling the user data pointer plus an optional destructor.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_scalar_function_set_user_data"
    c_duckdb_v2_scalar_function_set_user_data :: DuckDBV2ScalarFunctionHandle -> Ptr DuckDBV2Opaque -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets a property on the scalar function.

Configures a function property that influences planning and execution, such as stability, NULL handling, fallibility
or collation handling. The value must be one of the @DUCKDB_V2_FUNCTION_PROPERTY_VALUE@ entries belonging to @key@;
passing a value that does not belong to the key, or a key that is not valid for scalar functions, results in an
error.

history:
- stable: v2.0.0


@function@: The function to set the property of.

@key@: The property to set.

@value@: The value to set for the property. Must belong to @key@.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_scalar_function_set_property"
    c_duckdb_v2_scalar_function_set_property :: DuckDBV2ScalarFunctionHandle -> DuckDBV2FunctionPropertyKey -> DuckDBV2FunctionPropertyValue -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets the optional bind callback of the scalar function.

The bind callback is invoked during query planning for each call site of the function. Through its
@duckdb_v2_function_bind_info_handle@ it can inspect the argument types and constant argument values and set "bind
data" that is shared with the init and exec callbacks. Through its @duckdb_v2_scalar_function_bind_info_handle@ it
can set a concrete return type.

history:
- stable: v2.0.0


@function@: The function to set the bind callback of.

@callback@: The bind callback to set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_scalar_function_set_bind_callback"
    c_duckdb_v2_scalar_function_set_bind_callback :: DuckDBV2ScalarFunctionHandle -> FunPtr DuckDBV2ScalarFunctionBindCallbackFn -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets the optional init callback of the scalar function.

The init callback is invoked at the start of execution for each thread that will execute the function. It can set
worker-local "init data", retrievable from the exec callback, to keep mutable state across invocations of the
function.

history:
- stable: v2.0.0


@function@: The function to set the init callback of.

@callback@: The init callback to set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_scalar_function_set_init_callback"
    c_duckdb_v2_scalar_function_set_init_callback :: DuckDBV2ScalarFunctionHandle -> FunPtr DuckDBV2ScalarFunctionInitCallbackFn -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets the exec callback of the scalar function.

The exec callback implements the function's logic: it is invoked during query execution with a batch of input rows
and must fill the result vector. An exec callback must be set before registration.

history:
- stable: v2.0.0


@function@: The function to set the exec callback of.

@callback@: The exec callback to set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_scalar_function_set_exec_callback"
    c_duckdb_v2_scalar_function_set_exec_callback :: DuckDBV2ScalarFunctionHandle -> FunPtr DuckDBV2ScalarFunctionExecCallbackFn -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets the concrete return type of the call site being bound.

Overrides the return type declared in the signature. Register the function with an ANY return type and set the
concrete type here, derived from the argument types, to build a function whose result type depends on its input. The
type is borrowed and copied.

history:
- stable: v2.0.0


@info@: The bind info handle.

@return_type@: The return type to set. Borrowed for the call only.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_scalar_function_bind_set_return_type"
    c_duckdb_v2_scalar_function_bind_set_return_type :: DuckDBV2ScalarFunctionBindInfoHandle -> DuckDBV2LogicalTypeHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the user data set via @duckdb_v2_scalar_function_set_user_data()@.

history:
- stable: v2.0.0


@info@: The init info handle.

@data@: Receives the user data pointer, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_scalar_function_init_get_user_data"
    c_duckdb_v2_scalar_function_init_get_user_data :: DuckDBV2ScalarFunctionInitInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the bind data set by the function's bind callback.

history:
- stable: v2.0.0


@info@: The init info handle.

@data@: Receives the bind data pointer, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_scalar_function_init_get_bind_data"
    c_duckdb_v2_scalar_function_init_get_bind_data :: DuckDBV2ScalarFunctionInitInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets the function's worker-local "init data" from the init callback.

The init data is associated with the executing thread for the duration of the query and retrievable from the exec
callback via @duckdb_v2_scalar_function_exec_get_init_data()@. The opaque handle bundles the pointer with an optional
destructor, invoked when the init data is no longer needed.

history:
- stable: v2.0.0


@info@: The init info handle.

@data@: Opaque handle bundling the init data pointer plus an optional destructor.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_scalar_function_init_set_init_data"
    c_duckdb_v2_scalar_function_init_set_init_data :: DuckDBV2ScalarFunctionInitInfoHandle -> Ptr DuckDBV2Opaque -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the user data set via @duckdb_v2_scalar_function_set_user_data()@.

history:
- stable: v2.0.0


@info@: The exec info handle.

@data@: Receives the user data pointer, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_scalar_function_exec_get_user_data"
    c_duckdb_v2_scalar_function_exec_get_user_data :: DuckDBV2ScalarFunctionExecInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the bind data set by the function's bind callback.

history:
- stable: v2.0.0


@info@: The exec info handle.

@data@: Receives the bind data pointer, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_scalar_function_exec_get_bind_data"
    c_duckdb_v2_scalar_function_exec_get_bind_data :: DuckDBV2ScalarFunctionExecInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the worker-local init data set by the function's init callback.

history:
- stable: v2.0.0


@info@: The exec info handle.

@data@: Receives the init data pointer for the executing thread, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_scalar_function_exec_get_init_data"
    c_duckdb_v2_scalar_function_exec_get_init_data :: DuckDBV2ScalarFunctionExecInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns how many rows this execution must produce. The exec callback must write exactly this many rows to the result
vector. Note that this may be less than a full vector: with all-constant arguments the function is invoked for a
single row and the result is expanded by the engine.

history:
- stable: v2.0.0


@info@: The exec info handle.

@count@: Receives the number of rows.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_scalar_function_exec_get_row_count"
    c_duckdb_v2_scalar_function_exec_get_row_count :: DuckDBV2ScalarFunctionExecInfoHandle -> Ptr DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns how many argument vectors this invocation carries, split into the four parts of the argument list.

The parts and their order are those the bind callback saw through @duckdb_v2_function_bind_get_arg_count()@, and the
vectors are at the same indices. Valid indices for @duckdb_v2_scalar_function_exec_get_arg()@ are [0, the sum of the
four counts). Every out-parameter may be NULL, in which case nothing is written to it.

history:
- stable: v2.0.0


@info@: The exec info handle.

@positional_fixed@: Optional. Receives the number of positional-only and standard parameters.

@positional_variadic@: Optional. Receives the number of arguments @*args@ received, 0 when the signature has
none.

@named_fixed@: Optional. Receives the number of named-only parameters.

@named_variadic@: Optional. Receives the number of arguments @**kwargs@ received, 0 when the signature has none.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_scalar_function_exec_get_arg_count"
    c_duckdb_v2_scalar_function_exec_get_arg_count :: DuckDBV2ScalarFunctionExecInfoHandle -> Ptr DuckDBV2Idx -> Ptr DuckDBV2Idx -> Ptr DuckDBV2Idx -> Ptr DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the argument vector at the given index.

The index is the one the bind callback used for the argument, e.g. the index
@duckdb_v2_function_bind_get_arg_index()@ found for a name. The vector holds the argument's values for the current
batch; use @duckdb_v2_scalar_function_exec_get_row_count()@ for the number of rows. Fails if the index is out of
bounds. Borrowed; valid only for the duration of the callback.

history:
- stable: v2.0.0


@info@: The exec info handle.

@index@: The index of the argument.

@vector@: Receives the borrowed argument vector.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_scalar_function_exec_get_arg"
    c_duckdb_v2_scalar_function_exec_get_arg :: DuckDBV2ScalarFunctionExecInfoHandle -> DuckDBV2Idx -> Ptr DuckDBV2VectorHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the result vector the exec callback must write into.

The callback must write one entry per input row; use @duckdb_v2_scalar_function_exec_get_row_count()@ for the number
of rows. Borrowed; valid only for the duration of the callback.

history:
- stable: v2.0.0


@info@: The exec info handle.

@vector@: Receives the borrowed result vector to write into.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_scalar_function_exec_get_result"
    c_duckdb_v2_scalar_function_exec_get_result :: DuckDBV2ScalarFunctionExecInfoHandle -> Ptr DuckDBV2VectorHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Registers the scalar function, making it available for use in SQL queries.

The function is registered on the target given at creation: the connection's database or the loading extension.
Registration requires a name, an exec callback, and a signature with a complete return type; an ANY return type is
accepted only together with a bind callback that sets the concrete type per call site. The caller still owns the
handle after registration and must destroy it with @duckdb_v2_scalar_function_destroy()@, which does not affect the
registered function.

history:
- stable: v2.0.0


@function@: The function to register.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_scalar_function_register"
    c_duckdb_v2_scalar_function_register :: DuckDBV2ScalarFunctionHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Destroys the scalar function, releasing its resources.

Null-safe: passing a null pointer or null handle is a no-op. The handle is set to null on return to prevent
double-destruction. Destroying the handle after registration does not affect the registered function.

history:
- stable: v2.0.0


@function@: The function to destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_scalar_function_destroy"
    c_duckdb_v2_scalar_function_destroy :: Ptr DuckDBV2ScalarFunctionHandle -> IO DuckDBV2Error

{- | Parses a SQL string into an iterator over its statements.

Parses and nothing more: no binding, no catalog access, no transaction. The connection supplies the parser options
and parser extensions, and is not otherwise touched. Statements are raw parser output; statement-level rewrites
happen inside statement_execute. The SQL string is copied, so the caller may free it once this call returns. An input
with no statements — empty, whitespace, or separators only — yields an iterator that is immediately exhausted.

history:
- stable: v2.0.0


@conn@: The connection supplying the parser configuration.

@sql@: Null-terminated SQL string; may contain any number of statements.

@out_iterator@: Receives the new iterator handle.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_parse_sql"
    c_duckdb_v2_parse_sql :: DuckDBV2ConnectionHandle -> Ptr CChar -> Ptr DuckDBV2StatementIteratorHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Yields the next statement, or NULL when exhausted.

On success *out_statement receives the next owned statement, or NULL once the iterator is exhausted — repeatedly, so
calling again is harmless. A parse error within the input surfaces no later than the call that reaches the failing
statement: an implementation that parses eagerly reports it from parse_sql instead and yields no statements at all,
while an incremental one yields the statements ahead of the failure first. On failure *out_statement is set to NULL.

history:
- stable: v2.0.0


@iterator@: The iterator to advance.

@out_statement@: Receives an owned statement, or NULL when the iterator is exhausted.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_statement_iterator_next"
    c_duckdb_v2_statement_iterator_next :: DuckDBV2StatementIteratorHandle -> Ptr DuckDBV2SqlStatementHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Binds a parsed statement without executing, yielding its schema signature.

Preprocesses and binds the statement exactly as execution would, but runs nothing: the result is the statement's
signature as two schemas, not a query result. The statement is borrowed, not consumed, so it can be bound as often as
you like and executed later. out_schema receives the output schema (result columns) and is never empty, since a
non-SELECT reports a single status column: a BIGINT changed-rows count, or a BOOLEAN success. out_parameters, when
non-NULL, receives the input schema (parameter types, ordered by binding index). Both are owned; destroy them via
schema_destroy.

Binding is read-only and does not disturb a live result: it begins no query, claims no cursor, reuses an active
transaction read-only, and runs alongside a paused stream. It is single-consumer like the stepping functions, so bind
concurrently only on a second connection. Prepare-time errors — binder, catalog, preprocessing — surface here. A
statement that preprocessing expands into a group (a dynamic PIVOT, or statement-expanding DDL such as ALTER ADD
COLUMN with a non-constant DEFAULT) cannot be bound and is rejected with ERROR_INPUT_INVALID; execute it instead.

*out_schema and *out_parameters are set to NULL on failure.

history:
- stable: v2.0.0


@conn@: The connection supplying the catalog, transaction, and parser configuration.

@statement@: The statement to bind. Borrowed; not consumed.

@out_schema@: Receives the owned output schema (result columns). Destroy via schema_destroy.

@out_parameters@: Optional. When non-NULL, receives the owned input schema (parameter types, ordered by binding
index). Destroy via schema_destroy.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_statement_bind"
    c_duckdb_v2_statement_bind :: DuckDBV2ConnectionHandle -> DuckDBV2SqlStatementHandle -> Ptr DuckDBV2SchemaHandle -> Ptr DuckDBV2SchemaHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the statement's type as classified by the parser.

This gives the type before the statement-level rewrites that @duckdb_v2_statement_execute()@ applies, if any. So a
PRAGMA reports PRAGMA even where execution rewrites it into a SELECT or a CALL.
@duckdb_v2_result_get_statement_type()@ on the executed result reports the rewritten type. A statement the parser
expands into a group reports MULTI. Its parts are not visible here, and statement_bind rejects it.

history:
- stable: v2.0.0


@statement@: The statement.

@out_type@: Receives the statement type.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_sql_statement_get_type"
    c_duckdb_v2_sql_statement_get_type :: DuckDBV2SqlStatementHandle -> Ptr DuckDBV2StatementType -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Borrows the statement's own SQL text.

The slice of the parsed string that belongs to this statement. A trailing terminator and the whitespace after it are
included, whitespace and comments before the first token are not. The statement holds its own copy, so the view
outlives the SQL string passed to parse_sql and the iterator, and stays valid until the statement is destroyed. A
statement produced by a parser extension that overrides parsing carries whatever text the extension recorded.

history:
- stable: v2.0.0


@statement@: The statement.

@out_text@: Receives a borrowed view of the statement text, valid until the statement is destroyed.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_sql_statement_get_text"
    c_duckdb_v2_sql_statement_get_text :: DuckDBV2SqlStatementHandle -> Ptr DuckDBV2Str -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the number of distinct parameters the statement declares.

This counts the parameters the parser found ($1, ?, $name, ...), so it needs no catalog and no binding; only the
parameter types wait for @duckdb_v2_statement_bind()@. Repeated uses of one parameter count once.

history:
- stable: v2.0.0


@statement@: The statement.

@out_count@: Receives the number of distinct parameters.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_sql_statement_get_parameter_count"
    c_duckdb_v2_sql_statement_get_parameter_count :: DuckDBV2SqlStatementHandle -> Ptr DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Borrows the name of one parameter, in binding order.

Parse-time metadata. Positions follow the parameters' binding indices, the order of @duckdb_v2_statement_bind()@'s
parameter schema, so position i here names field i there. The name is the binding key that
@duckdb_v2_statement_execute()@ accepts: "1", "2", ... for a positional parameter ($1 or ?), the identifier for a
named one ($name). Positional indices may be gapped ($1 and $3 without $2), in which case the names are "1" and "3"
at positions 0 and 1. The view is valid until the statement is destroyed. An index outside [0, count) is rejected
with ERROR_INPUT_OUT_OF_RANGE.

history:
- stable: v2.0.0


@statement@: The statement.

@index@: Zero-based position in binding order.

@out_name@: Receives a borrowed view of the parameter name, valid until the statement is destroyed.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_sql_statement_get_parameter_name"
    c_duckdb_v2_sql_statement_get_parameter_name :: DuckDBV2SqlStatementHandle -> DuckDBV2Idx -> Ptr DuckDBV2IdentifierT -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Destroys a statement handle.

Null-safe: passing nullptr or a slot already set to nullptr is a no-op. statement_execute does not consume a
statement, so every statement is destroyed here once it is no longer needed. On success the slot is set to nullptr.

history:
- stable: v2.0.0


@statement@: The statement to destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_sql_statement_destroy"
    c_duckdb_v2_sql_statement_destroy :: Ptr DuckDBV2SqlStatementHandle -> IO DuckDBV2Error

{- | Destroys a statement iterator handle.

Null-safe: passing nullptr or a slot already set to nullptr is a no-op. Statements already yielded are independently
owned and stay valid. On success the slot is set to nullptr.

history:
- stable: v2.0.0


@iterator@: The iterator to destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_statement_iterator_destroy"
    c_duckdb_v2_statement_iterator_destroy :: Ptr DuckDBV2StatementIteratorHandle -> IO DuckDBV2Error

{- | Destroys a value handle.

Null-safe: passing nullptr or a slot already set to nullptr is a no-op. On success the slot is set to nullptr.

history:
- stable: v2.0.0


@value@: The value to destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_destroy"
    c_duckdb_v2_value_destroy :: Ptr DuckDBV2ValueHandle -> IO DuckDBV2Error

{- | Creates a NULL value of the given logical type.

The logical type is borrowed and copied into the value, so the caller can destroy it independently.

history:
- stable: v2.0.0


@type@: The borrowed logical type to attach to the NULL value.

@out_value@: Receives the new NULL value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_null"
    c_duckdb_v2_value_create_null :: DuckDBV2LogicalTypeHandle -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the value as a BOOLEAN.

A value of a different type is converted through the default cast set, on a copy, so reading never alters the value.
An unsupported conversion returns ERROR_INPUT_INVALID, as does a NULL value.

history:
- stable: v2.0.0


@value@: The value to read.

@out@: Receives the payload as a bool.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_get_bool"
    c_duckdb_v2_value_get_bool :: DuckDBV2ValueHandle -> Ptr CBool -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the value as a UTINYINT.

A value of a different type is converted through the default cast set, on a copy, so reading never alters the value.
An unsupported conversion returns ERROR_INPUT_INVALID, as does a NULL value.

history:
- stable: v2.0.0


@value@: The value to read.

@out@: Receives the payload as a uint8_t.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_get_utinyint"
    c_duckdb_v2_value_get_utinyint :: DuckDBV2ValueHandle -> Ptr Word8 -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the value as a USMALLINT.

A value of a different type is converted through the default cast set, on a copy, so reading never alters the value.
An unsupported conversion returns ERROR_INPUT_INVALID, as does a NULL value.

history:
- stable: v2.0.0


@value@: The value to read.

@out@: Receives the payload as a uint16_t.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_get_usmallint"
    c_duckdb_v2_value_get_usmallint :: DuckDBV2ValueHandle -> Ptr Word16 -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the value as a UINTEGER.

A value of a different type is converted through the default cast set, on a copy, so reading never alters the value.
An unsupported conversion returns ERROR_INPUT_INVALID, as does a NULL value.

history:
- stable: v2.0.0


@value@: The value to read.

@out@: Receives the payload as a uint32_t.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_get_uint"
    c_duckdb_v2_value_get_uint :: DuckDBV2ValueHandle -> Ptr Word32 -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the value as a UBIGINT.

A value of a different type is converted through the default cast set, on a copy, so reading never alters the value.
An unsupported conversion returns ERROR_INPUT_INVALID, as does a NULL value.

history:
- stable: v2.0.0


@value@: The value to read.

@out@: Receives the payload as a uint64_t.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_get_ubigint"
    c_duckdb_v2_value_get_ubigint :: DuckDBV2ValueHandle -> Ptr Word64 -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the value as a UHUGEINT.

A value of a different type is converted through the default cast set, on a copy, so reading never alters the value.
An unsupported conversion returns ERROR_INPUT_INVALID, as does a NULL value.

history:
- stable: v2.0.0


@value@: The value to read.

@out@: Receives the payload as a uint128_t.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_get_uhugeint"
    c_duckdb_v2_value_get_uhugeint :: DuckDBV2ValueHandle -> Ptr DuckDBV2UhugeintT -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the value as a TINYINT.

A value of a different type is converted through the default cast set, on a copy, so reading never alters the value.
An unsupported conversion returns ERROR_INPUT_INVALID, as does a NULL value.

history:
- stable: v2.0.0


@value@: The value to read.

@out@: Receives the payload as an int8_t.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_get_tinyint"
    c_duckdb_v2_value_get_tinyint :: DuckDBV2ValueHandle -> Ptr Int8 -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the value as a SMALLINT.

A value of a different type is converted through the default cast set, on a copy, so reading never alters the value.
An unsupported conversion returns ERROR_INPUT_INVALID, as does a NULL value.

history:
- stable: v2.0.0


@value@: The value to read.

@out@: Receives the payload as an int16_t.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_get_smallint"
    c_duckdb_v2_value_get_smallint :: DuckDBV2ValueHandle -> Ptr Int16 -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the value as an INTEGER.

A value of a different type is converted through the default cast set, on a copy, so reading never alters the value.
An unsupported conversion returns ERROR_INPUT_INVALID, as does a NULL value.

history:
- stable: v2.0.0


@value@: The value to read.

@out@: Receives the payload as an int32_t.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_get_int"
    c_duckdb_v2_value_get_int :: DuckDBV2ValueHandle -> Ptr Int32 -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the value as a BIGINT.

A value of a different type is converted through the default cast set, on a copy, so reading never alters the value.
An unsupported conversion returns ERROR_INPUT_INVALID, as does a NULL value.

history:
- stable: v2.0.0


@value@: The value to read.

@out@: Receives the payload as an int64_t.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_get_bigint"
    c_duckdb_v2_value_get_bigint :: DuckDBV2ValueHandle -> Ptr Int64 -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the value as a HUGEINT.

A value of a different type is converted through the default cast set, on a copy, so reading never alters the value.
An unsupported conversion returns ERROR_INPUT_INVALID, as does a NULL value.

history:
- stable: v2.0.0


@value@: The value to read.

@out@: Receives the payload as an int128_t.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_get_hugeint"
    c_duckdb_v2_value_get_hugeint :: DuckDBV2ValueHandle -> Ptr DuckDBV2HugeintT -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the value as a VARCHAR.

The string is borrowed from the value and stays valid until the value is destroyed. That borrow is why this getter
does not convert: a converted copy would not outlive the call. A value of any other type id, and a NULL value, return
ERROR_INPUT_INVALID.

history:
- stable: v2.0.0


@value@: The value to read.

@out@: Receives the payload as a UTF-8 string. The returned string is borrowed and valid until the value handle
is destroyed.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_get_varchar"
    c_duckdb_v2_value_get_varchar :: DuckDBV2ValueHandle -> Ptr DuckDBV2Str -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the value as a byte string: a BLOB, or the storage bytes of a BIT or BIGNUM.

The string is borrowed from the value and stays valid until the value is destroyed. That borrow is why this getter
does not convert: a converted copy would not outlive the call. A value of any other type id, and a NULL value, return
ERROR_INPUT_INVALID.

history:
- stable: v2.0.0


@value@: The value to read.

@out@: Receives the payload as a byte string. The returned string is borrowed and valid until the value handle is
destroyed.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_get_blob"
    c_duckdb_v2_value_get_blob :: DuckDBV2ValueHandle -> Ptr DuckDBV2Str -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the value as a FLOAT.

A value of a different type is converted through the default cast set, on a copy, so reading never alters the value.
An unsupported conversion returns ERROR_INPUT_INVALID, as does a NULL value.

history:
- stable: v2.0.0


@value@: The value to read.

@out@: Receives the payload as a float.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_get_float"
    c_duckdb_v2_value_get_float :: DuckDBV2ValueHandle -> Ptr CFloat -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the value as a DOUBLE.

A value of a different type is converted through the default cast set, on a copy, so reading never alters the value.
An unsupported conversion returns ERROR_INPUT_INVALID, as does a NULL value.

history:
- stable: v2.0.0


@value@: The value to read.

@out@: Receives the payload as a double.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_get_double"
    c_duckdb_v2_value_get_double :: DuckDBV2ValueHandle -> Ptr CDouble -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the value as a DATE.

The payload is days since 1970-01-01, the same unit a vector of this type exposes.

A value of a different type is converted through the default cast set, on a copy, so reading never alters the value.
An unsupported conversion returns ERROR_INPUT_INVALID, as does a NULL value.

history:
- stable: v2.0.0


@value@: The value to read.

@out@: Receives the payload.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_get_date"
    c_duckdb_v2_value_get_date :: DuckDBV2ValueHandle -> Ptr Int32 -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the value as a TIME.

The payload is microseconds since midnight, the same unit a vector of this type exposes.

A value of a different type is converted through the default cast set, on a copy, so reading never alters the value.
An unsupported conversion returns ERROR_INPUT_INVALID, as does a NULL value.

history:
- stable: v2.0.0


@value@: The value to read.

@out@: Receives the payload.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_get_time"
    c_duckdb_v2_value_get_time :: DuckDBV2ValueHandle -> Ptr Int64 -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the value as a TIME_NS.

The payload is nanoseconds since midnight, the same unit a vector of this type exposes.

A value of a different type is converted through the default cast set, on a copy, so reading never alters the value.
An unsupported conversion returns ERROR_INPUT_INVALID, as does a NULL value.

history:
- stable: v2.0.0


@value@: The value to read.

@out@: Receives the payload.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_get_time_ns"
    c_duckdb_v2_value_get_time_ns :: DuckDBV2ValueHandle -> Ptr Int64 -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the value as a TIME_TZ.

The payload packs the time of day and the UTC offset into one integer, in the committed layout: micros in the high 40
bits, a biased offset in the low 24. The same form a vector of this type exposes.

A value of a different type is converted through the default cast set, on a copy, so reading never alters the value.
An unsupported conversion returns ERROR_INPUT_INVALID, as does a NULL value.

history:
- stable: v2.0.0


@value@: The value to read.

@out@: Receives the payload.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_get_time_tz"
    c_duckdb_v2_value_get_time_tz :: DuckDBV2ValueHandle -> Ptr Word64 -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the value as a TIMESTAMP.

The payload is microseconds since 1970-01-01, the same unit a vector of this type exposes.

A value of a different type is converted through the default cast set, on a copy, so reading never alters the value.
An unsupported conversion returns ERROR_INPUT_INVALID, as does a NULL value.

history:
- stable: v2.0.0


@value@: The value to read.

@out@: Receives the payload.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_get_timestamp"
    c_duckdb_v2_value_get_timestamp :: DuckDBV2ValueHandle -> Ptr Int64 -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the value as a TIMESTAMP_SEC.

The payload is seconds since 1970-01-01, the same unit a vector of this type exposes.

A value of a different type is converted through the default cast set, on a copy, so reading never alters the value.
An unsupported conversion returns ERROR_INPUT_INVALID, as does a NULL value.

history:
- stable: v2.0.0


@value@: The value to read.

@out@: Receives the payload.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_get_timestamp_sec"
    c_duckdb_v2_value_get_timestamp_sec :: DuckDBV2ValueHandle -> Ptr Int64 -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the value as a TIMESTAMP_MS.

The payload is milliseconds since 1970-01-01, the same unit a vector of this type exposes.

A value of a different type is converted through the default cast set, on a copy, so reading never alters the value.
An unsupported conversion returns ERROR_INPUT_INVALID, as does a NULL value.

history:
- stable: v2.0.0


@value@: The value to read.

@out@: Receives the payload.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_get_timestamp_ms"
    c_duckdb_v2_value_get_timestamp_ms :: DuckDBV2ValueHandle -> Ptr Int64 -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the value as a TIMESTAMP_NS.

The payload is nanoseconds since 1970-01-01, the same unit a vector of this type exposes.

A value of a different type is converted through the default cast set, on a copy, so reading never alters the value.
An unsupported conversion returns ERROR_INPUT_INVALID, as does a NULL value.

history:
- stable: v2.0.0


@value@: The value to read.

@out@: Receives the payload.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_get_timestamp_ns"
    c_duckdb_v2_value_get_timestamp_ns :: DuckDBV2ValueHandle -> Ptr Int64 -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the value as a TIMESTAMP_TZ.

The payload is microseconds since 1970-01-01, in UTC, the same unit a vector of this type exposes.

A value of a different type is converted through the default cast set, on a copy, so reading never alters the value.
An unsupported conversion returns ERROR_INPUT_INVALID, as does a NULL value.

history:
- stable: v2.0.0


@value@: The value to read.

@out@: Receives the payload.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_get_timestamp_tz"
    c_duckdb_v2_value_get_timestamp_tz :: DuckDBV2ValueHandle -> Ptr Int64 -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the value as a TIMESTAMP_TZ_NS.

The payload is nanoseconds since 1970-01-01, in UTC, the same unit a vector of this type exposes.

A value of a different type is converted through the default cast set, on a copy, so reading never alters the value.
An unsupported conversion returns ERROR_INPUT_INVALID, as does a NULL value.

history:
- stable: v2.0.0


@value@: The value to read.

@out@: Receives the payload.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_get_timestamp_tz_ns"
    c_duckdb_v2_value_get_timestamp_tz_ns :: DuckDBV2ValueHandle -> Ptr Int64 -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the value as an INTERVAL.

The payload is the (months, days, micros) triple, the same unit a vector of this type exposes.

A value of a different type is converted through the default cast set, on a copy, so reading never alters the value.
An unsupported conversion returns ERROR_INPUT_INVALID, as does a NULL value.

history:
- stable: v2.0.0


@value@: The value to read.

@out@: Receives the payload.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_get_interval"
    c_duckdb_v2_value_get_interval :: DuckDBV2ValueHandle -> Ptr DuckDBV2IntervalT -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the value as a UUID, in its internal 128-bit form.

The dual of value_create_uuid: the storage form a vector element holds, with the high bit flipped so the integer
sorts, rather than the canonical byte order. A value of any other type id, and a NULL value, return
ERROR_INPUT_INVALID.

history:
- stable: v2.0.0


@value@: The value to read.

@out@: Receives the internal 128-bit storage form.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_get_uuid"
    c_duckdb_v2_value_get_uuid :: DuckDBV2ValueHandle -> Ptr DuckDBV2HugeintT -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the value as a DECIMAL: its backing integer plus the width and scale of its type.

The integer is the value scaled by 10^scale — the dual of value_create_decimal — widened to 128 bits from whatever
storage tier the width selects. The integer getters convert, so a DECIMAL read through value_get_bigint is its
numeric value with the fraction dropped; this one always reports storage instead, together with the scale needed to
interpret it. A value of any other type id, and a NULL value, return ERROR_INPUT_INVALID.

history:
- stable: v2.0.0


@value@: The value to read.

@out@: Receives the backing integer, scaled by 10^scale.

@out_width@: Receives the total digit count of the value's type.

@out_scale@: Receives the digits after the decimal point.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_get_decimal"
    c_duckdb_v2_value_get_decimal :: DuckDBV2ValueHandle -> Ptr DuckDBV2HugeintT -> Ptr Word8 -> Ptr Word8 -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Unwraps the logical type carried by a TYPE value.

Returns ERROR_INPUT_INVALID unless the value is a non-NULL TYPE value. The returned logical type is caller-owned and
must be destroyed via logical_type_destroy.

history:
- stable: v2.0.0


@value@: The value to read.

@out_type@: Receives the owned logical type.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_get_type"
    c_duckdb_v2_value_get_type :: DuckDBV2ValueHandle -> Ptr DuckDBV2LogicalTypeHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a NULL value of the given logical type.

The logical type is borrowed and copied into the value, so the caller can destroy it independently.

history:
- stable: v2.0.0


@ctx@: The context to use for the construction.

@type@: The borrowed logical type to attach to the NULL value.

@out_value@: Receives the new NULL value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_null_with_context"
    c_duckdb_v2_value_create_null_with_context :: DuckDBV2ContextHandle -> DuckDBV2LogicalTypeHandle -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a BOOLEAN value from a bool.

The input is copied in. The returned value is caller-owned; destroy it via value_destroy.

history:
- stable: v2.0.0


@ctx@: The context to use for the construction.

@in_value@: The bool to wrap.

@out_value@: Receives the new BOOLEAN value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_bool_with_context"
    c_duckdb_v2_value_create_bool_with_context :: DuckDBV2ContextHandle -> CBool -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a UTINYINT value from a uint8_t.

The input is copied in. The returned value is caller-owned; destroy it via value_destroy.

history:
- stable: v2.0.0


@ctx@: The context to use for the construction.

@in_value@: The uint8_t to wrap.

@out_value@: Receives the new UTINYINT value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_utinyint_with_context"
    c_duckdb_v2_value_create_utinyint_with_context :: DuckDBV2ContextHandle -> Word8 -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a USMALLINT value from a uint16_t.

The input is copied in. The returned value is caller-owned; destroy it via value_destroy.

history:
- stable: v2.0.0


@ctx@: The context to use for the construction.

@in_value@: The uint16_t to wrap.

@out_value@: Receives the new USMALLINT value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_usmallint_with_context"
    c_duckdb_v2_value_create_usmallint_with_context :: DuckDBV2ContextHandle -> Word16 -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a UINTEGER value from a uint32_t.

The input is copied in. The returned value is caller-owned; destroy it via value_destroy.

history:
- stable: v2.0.0


@ctx@: The context to use for the construction.

@in_value@: The uint32_t to wrap.

@out_value@: Receives the new UINTEGER value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_uint_with_context"
    c_duckdb_v2_value_create_uint_with_context :: DuckDBV2ContextHandle -> Word32 -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a UBIGINT value from a uint64_t.

The input is copied in. The returned value is caller-owned; destroy it via value_destroy.

history:
- stable: v2.0.0


@ctx@: The context to use for the construction.

@in_value@: The uint64_t to wrap.

@out_value@: Receives the new UBIGINT value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_ubigint_with_context"
    c_duckdb_v2_value_create_ubigint_with_context :: DuckDBV2ContextHandle -> Word64 -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a UHUGEINT value from a uint128_t.

The input is copied in. The returned value is caller-owned; destroy it via value_destroy.

history:
- stable: v2.0.0


@ctx@: The context to use for the construction.

@in_value@: The uint128_t to wrap.

@out_value@: Receives the new UHUGEINT value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_uhugeint_with_context"
    c_duckdb_v2_value_create_uhugeint_with_context :: DuckDBV2ContextHandle -> Ptr DuckDBV2UhugeintT -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a TINYINT value from an int8_t.

The input is copied in. The returned value is caller-owned; destroy it via value_destroy.

history:
- stable: v2.0.0


@ctx@: The context to use for the construction.

@in_value@: The int8_t to wrap.

@out_value@: Receives the new TINYINT value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_tinyint_with_context"
    c_duckdb_v2_value_create_tinyint_with_context :: DuckDBV2ContextHandle -> Int8 -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a SMALLINT value from an int16_t.

The input is copied in. The returned value is caller-owned; destroy it via value_destroy.

history:
- stable: v2.0.0


@ctx@: The context to use for the construction.

@in_value@: The int16_t to wrap.

@out_value@: Receives the new SMALLINT value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_smallint_with_context"
    c_duckdb_v2_value_create_smallint_with_context :: DuckDBV2ContextHandle -> Int16 -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates an INTEGER value from an int32_t.

The input is copied in. The returned value is caller-owned; destroy it via value_destroy.

history:
- stable: v2.0.0


@ctx@: The context to use for the construction.

@in_value@: The int32_t to wrap.

@out_value@: Receives the new INTEGER value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_int_with_context"
    c_duckdb_v2_value_create_int_with_context :: DuckDBV2ContextHandle -> Int32 -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a BIGINT value from an int64_t.

The input is copied in. The returned value is caller-owned; destroy it via value_destroy.

history:
- stable: v2.0.0


@ctx@: The context to use for the construction.

@in_value@: The int64_t to wrap.

@out_value@: Receives the new BIGINT value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_bigint_with_context"
    c_duckdb_v2_value_create_bigint_with_context :: DuckDBV2ContextHandle -> Int64 -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a HUGEINT value from an int128_t.

The input is copied in. The returned value is caller-owned; destroy it via value_destroy.

history:
- stable: v2.0.0


@ctx@: The context to use for the construction.

@in_value@: The int128_t to wrap.

@out_value@: Receives the new HUGEINT value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_hugeint_with_context"
    c_duckdb_v2_value_create_hugeint_with_context :: DuckDBV2ContextHandle -> Ptr DuckDBV2HugeintT -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a VARCHAR value from a UTF-8 string.

The input is copied in. The returned value is caller-owned; destroy it via value_destroy.

history:
- stable: v2.0.0


@ctx@: The context to use for the construction.

@in_value@: The UTF-8 string to wrap. May be null only when len is 0 (empty string).

@out_value@: Receives the new VARCHAR value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_varchar_with_context"
    c_duckdb_v2_value_create_varchar_with_context :: DuckDBV2ContextHandle -> Ptr DuckDBV2Str -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a BLOB value from a byte string.

The input is copied in. The returned value is caller-owned; destroy it via value_destroy.

history:
- stable: v2.0.0


@ctx@: The context to use for the construction.

@in_value@: The byte string to wrap. May be null only when len is 0 (empty string).

@out_value@: Receives the new BLOB value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_blob_with_context"
    c_duckdb_v2_value_create_blob_with_context :: DuckDBV2ContextHandle -> Ptr DuckDBV2Str -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a FLOAT value from a float.

The input is copied in. The returned value is caller-owned; destroy it via value_destroy.

history:
- stable: v2.0.0


@ctx@: The context to use for the construction.

@in_value@: The float to wrap.

@out_value@: Receives the new FLOAT value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_float_with_context"
    c_duckdb_v2_value_create_float_with_context :: DuckDBV2ContextHandle -> CFloat -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a DOUBLE value from a double.

The input is copied in. The returned value is caller-owned; destroy it via value_destroy.

history:
- stable: v2.0.0


@ctx@: The context to use for the construction.

@in_value@: The double to wrap.

@out_value@: Receives the new DOUBLE value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_double_with_context"
    c_duckdb_v2_value_create_double_with_context :: DuckDBV2ContextHandle -> CDouble -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a TYPE value from a logical type.

The logical type is borrowed and copied into the value, so the caller can destroy it independently.

history:
- stable: v2.0.0


@ctx@: The context to use for the construction.

@in_type@: The borrowed logical type to wrap.

@out_value@: Receives the new TYPE value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_type_with_context"
    c_duckdb_v2_value_create_type_with_context :: DuckDBV2ContextHandle -> DuckDBV2LogicalTypeHandle -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a NULL value of the given logical type.

The logical type is borrowed and copied into the value, so the caller can destroy it independently.

history:
- stable: v2.0.0


@conn@: The connection to use for the construction.

@type@: The borrowed logical type to attach to the NULL value.

@out_value@: Receives the new NULL value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_null_with_connection"
    c_duckdb_v2_value_create_null_with_connection :: DuckDBV2ConnectionHandle -> DuckDBV2LogicalTypeHandle -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a BOOLEAN value from a bool.

The input is copied in. The returned value is caller-owned; destroy it via value_destroy.

history:
- stable: v2.0.0


@conn@: The connection to use for the construction.

@in_value@: The bool to wrap.

@out_value@: Receives the new BOOLEAN value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_bool_with_connection"
    c_duckdb_v2_value_create_bool_with_connection :: DuckDBV2ConnectionHandle -> CBool -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a UTINYINT value from a uint8_t.

The input is copied in. The returned value is caller-owned; destroy it via value_destroy.

history:
- stable: v2.0.0


@conn@: The connection to use for the construction.

@in_value@: The uint8_t to wrap.

@out_value@: Receives the new UTINYINT value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_utinyint_with_connection"
    c_duckdb_v2_value_create_utinyint_with_connection :: DuckDBV2ConnectionHandle -> Word8 -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a USMALLINT value from a uint16_t.

The input is copied in. The returned value is caller-owned; destroy it via value_destroy.

history:
- stable: v2.0.0


@conn@: The connection to use for the construction.

@in_value@: The uint16_t to wrap.

@out_value@: Receives the new USMALLINT value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_usmallint_with_connection"
    c_duckdb_v2_value_create_usmallint_with_connection :: DuckDBV2ConnectionHandle -> Word16 -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a UINTEGER value from a uint32_t.

The input is copied in. The returned value is caller-owned; destroy it via value_destroy.

history:
- stable: v2.0.0


@conn@: The connection to use for the construction.

@in_value@: The uint32_t to wrap.

@out_value@: Receives the new UINTEGER value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_uint_with_connection"
    c_duckdb_v2_value_create_uint_with_connection :: DuckDBV2ConnectionHandle -> Word32 -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a UBIGINT value from a uint64_t.

The input is copied in. The returned value is caller-owned; destroy it via value_destroy.

history:
- stable: v2.0.0


@conn@: The connection to use for the construction.

@in_value@: The uint64_t to wrap.

@out_value@: Receives the new UBIGINT value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_ubigint_with_connection"
    c_duckdb_v2_value_create_ubigint_with_connection :: DuckDBV2ConnectionHandle -> Word64 -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a UHUGEINT value from a uint128_t.

The input is copied in. The returned value is caller-owned; destroy it via value_destroy.

history:
- stable: v2.0.0


@conn@: The connection to use for the construction.

@in_value@: The uint128_t to wrap.

@out_value@: Receives the new UHUGEINT value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_uhugeint_with_connection"
    c_duckdb_v2_value_create_uhugeint_with_connection :: DuckDBV2ConnectionHandle -> Ptr DuckDBV2UhugeintT -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a TINYINT value from an int8_t.

The input is copied in. The returned value is caller-owned; destroy it via value_destroy.

history:
- stable: v2.0.0


@conn@: The connection to use for the construction.

@in_value@: The int8_t to wrap.

@out_value@: Receives the new TINYINT value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_tinyint_with_connection"
    c_duckdb_v2_value_create_tinyint_with_connection :: DuckDBV2ConnectionHandle -> Int8 -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a SMALLINT value from an int16_t.

The input is copied in. The returned value is caller-owned; destroy it via value_destroy.

history:
- stable: v2.0.0


@conn@: The connection to use for the construction.

@in_value@: The int16_t to wrap.

@out_value@: Receives the new SMALLINT value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_smallint_with_connection"
    c_duckdb_v2_value_create_smallint_with_connection :: DuckDBV2ConnectionHandle -> Int16 -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates an INTEGER value from an int32_t.

The input is copied in. The returned value is caller-owned; destroy it via value_destroy.

history:
- stable: v2.0.0


@conn@: The connection to use for the construction.

@in_value@: The int32_t to wrap.

@out_value@: Receives the new INTEGER value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_int_with_connection"
    c_duckdb_v2_value_create_int_with_connection :: DuckDBV2ConnectionHandle -> Int32 -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a BIGINT value from an int64_t.

The input is copied in. The returned value is caller-owned; destroy it via value_destroy.

history:
- stable: v2.0.0


@conn@: The connection to use for the construction.

@in_value@: The int64_t to wrap.

@out_value@: Receives the new BIGINT value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_bigint_with_connection"
    c_duckdb_v2_value_create_bigint_with_connection :: DuckDBV2ConnectionHandle -> Int64 -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a HUGEINT value from an int128_t.

The input is copied in. The returned value is caller-owned; destroy it via value_destroy.

history:
- stable: v2.0.0


@conn@: The connection to use for the construction.

@in_value@: The int128_t to wrap.

@out_value@: Receives the new HUGEINT value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_hugeint_with_connection"
    c_duckdb_v2_value_create_hugeint_with_connection :: DuckDBV2ConnectionHandle -> Ptr DuckDBV2HugeintT -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a VARCHAR value from a UTF-8 string.

The input is copied in. The returned value is caller-owned; destroy it via value_destroy.

history:
- stable: v2.0.0


@conn@: The connection to use for the construction.

@in_value@: The UTF-8 string to wrap. May be null only when len is 0 (empty string).

@out_value@: Receives the new VARCHAR value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_varchar_with_connection"
    c_duckdb_v2_value_create_varchar_with_connection :: DuckDBV2ConnectionHandle -> Ptr DuckDBV2Str -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a BLOB value from a byte string.

The input is copied in. The returned value is caller-owned; destroy it via value_destroy.

history:
- stable: v2.0.0


@conn@: The connection to use for the construction.

@in_value@: The byte string to wrap. May be null only when len is 0 (empty string).

@out_value@: Receives the new BLOB value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_blob_with_connection"
    c_duckdb_v2_value_create_blob_with_connection :: DuckDBV2ConnectionHandle -> Ptr DuckDBV2Str -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a FLOAT value from a float.

The input is copied in. The returned value is caller-owned; destroy it via value_destroy.

history:
- stable: v2.0.0


@conn@: The connection to use for the construction.

@in_value@: The float to wrap.

@out_value@: Receives the new FLOAT value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_float_with_connection"
    c_duckdb_v2_value_create_float_with_connection :: DuckDBV2ConnectionHandle -> CFloat -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a DOUBLE value from a double.

The input is copied in. The returned value is caller-owned; destroy it via value_destroy.

history:
- stable: v2.0.0


@conn@: The connection to use for the construction.

@in_value@: The double to wrap.

@out_value@: Receives the new DOUBLE value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_double_with_connection"
    c_duckdb_v2_value_create_double_with_connection :: DuckDBV2ConnectionHandle -> CDouble -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a TYPE value wrapping the given logical type.

The input logical type is borrowed; the value internally copies the type so the caller can destroy the logical type
independently.

history:
- stable: v2.0.0


@conn@: The connection to use for the construction.

@type@: The borrowed logical type to wrap.

@out_value@: Receives the new value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_type_with_connection"
    c_duckdb_v2_value_create_type_with_connection :: DuckDBV2ConnectionHandle -> DuckDBV2LogicalTypeHandle -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a DATE value.

The payload is days since 1970-01-01, the same unit value_get_date reports. It is not range-checked here; value_cast
is the validating path. The returned value is caller-owned; destroy it via value_destroy.

history:
- stable: v2.0.0


@ctx@: The context to use for the construction.

@in_value@: The payload to wrap.

@out_value@: Receives the new DATE value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_date_with_context"
    c_duckdb_v2_value_create_date_with_context :: DuckDBV2ContextHandle -> Int32 -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a DATE value.

The payload is days since 1970-01-01, the same unit value_get_date reports. It is not range-checked here; value_cast
is the validating path. The returned value is caller-owned; destroy it via value_destroy.

history:
- stable: v2.0.0


@conn@: The connection to use for the construction.

@in_value@: The payload to wrap.

@out_value@: Receives the new DATE value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_date_with_connection"
    c_duckdb_v2_value_create_date_with_connection :: DuckDBV2ConnectionHandle -> Int32 -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a TIME value.

The payload is microseconds since midnight, the same unit value_get_time reports. It is not range-checked here;
value_cast is the validating path. The returned value is caller-owned; destroy it via value_destroy.

history:
- stable: v2.0.0


@ctx@: The context to use for the construction.

@in_value@: The payload to wrap.

@out_value@: Receives the new TIME value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_time_with_context"
    c_duckdb_v2_value_create_time_with_context :: DuckDBV2ContextHandle -> Int64 -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a TIME value.

The payload is microseconds since midnight, the same unit value_get_time reports. It is not range-checked here;
value_cast is the validating path. The returned value is caller-owned; destroy it via value_destroy.

history:
- stable: v2.0.0


@conn@: The connection to use for the construction.

@in_value@: The payload to wrap.

@out_value@: Receives the new TIME value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_time_with_connection"
    c_duckdb_v2_value_create_time_with_connection :: DuckDBV2ConnectionHandle -> Int64 -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a TIME_NS value.

The payload is nanoseconds since midnight, the same unit value_get_time_ns reports. It is not range-checked here;
value_cast is the validating path. The returned value is caller-owned; destroy it via value_destroy.

history:
- stable: v2.0.0


@ctx@: The context to use for the construction.

@in_value@: The payload to wrap.

@out_value@: Receives the new TIME_NS value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_time_ns_with_context"
    c_duckdb_v2_value_create_time_ns_with_context :: DuckDBV2ContextHandle -> Int64 -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a TIME_NS value.

The payload is nanoseconds since midnight, the same unit value_get_time_ns reports. It is not range-checked here;
value_cast is the validating path. The returned value is caller-owned; destroy it via value_destroy.

history:
- stable: v2.0.0


@conn@: The connection to use for the construction.

@in_value@: The payload to wrap.

@out_value@: Receives the new TIME_NS value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_time_ns_with_connection"
    c_duckdb_v2_value_create_time_ns_with_connection :: DuckDBV2ConnectionHandle -> Int64 -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a TIME_TZ value.

The payload packs the time of day and the UTC offset into one integer, in the committed layout: micros in the high 40
bits, a biased offset in the low 24. The same form value_get_time_tz reports. It is not range-checked here;
value_cast is the validating path. The returned value is caller-owned; destroy it via value_destroy.

history:
- stable: v2.0.0


@ctx@: The context to use for the construction.

@in_value@: The payload to wrap.

@out_value@: Receives the new TIME_TZ value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_time_tz_with_context"
    c_duckdb_v2_value_create_time_tz_with_context :: DuckDBV2ContextHandle -> Word64 -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a TIME_TZ value.

The payload packs the time of day and the UTC offset into one integer, in the committed layout: micros in the high 40
bits, a biased offset in the low 24. The same form value_get_time_tz reports. It is not range-checked here;
value_cast is the validating path. The returned value is caller-owned; destroy it via value_destroy.

history:
- stable: v2.0.0


@conn@: The connection to use for the construction.

@in_value@: The payload to wrap.

@out_value@: Receives the new TIME_TZ value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_time_tz_with_connection"
    c_duckdb_v2_value_create_time_tz_with_connection :: DuckDBV2ConnectionHandle -> Word64 -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a TIMESTAMP value.

The payload is microseconds since 1970-01-01, the same unit value_get_timestamp reports. It is not range-checked
here; value_cast is the validating path. The returned value is caller-owned; destroy it via value_destroy.

history:
- stable: v2.0.0


@ctx@: The context to use for the construction.

@in_value@: The payload to wrap.

@out_value@: Receives the new TIMESTAMP value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_timestamp_with_context"
    c_duckdb_v2_value_create_timestamp_with_context :: DuckDBV2ContextHandle -> Int64 -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a TIMESTAMP value.

The payload is microseconds since 1970-01-01, the same unit value_get_timestamp reports. It is not range-checked
here; value_cast is the validating path. The returned value is caller-owned; destroy it via value_destroy.

history:
- stable: v2.0.0


@conn@: The connection to use for the construction.

@in_value@: The payload to wrap.

@out_value@: Receives the new TIMESTAMP value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_timestamp_with_connection"
    c_duckdb_v2_value_create_timestamp_with_connection :: DuckDBV2ConnectionHandle -> Int64 -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a TIMESTAMP_SEC value.

The payload is seconds since 1970-01-01, the same unit value_get_timestamp_sec reports. It is not range-checked here;
value_cast is the validating path. The returned value is caller-owned; destroy it via value_destroy.

history:
- stable: v2.0.0


@ctx@: The context to use for the construction.

@in_value@: The payload to wrap.

@out_value@: Receives the new TIMESTAMP_SEC value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_timestamp_sec_with_context"
    c_duckdb_v2_value_create_timestamp_sec_with_context :: DuckDBV2ContextHandle -> Int64 -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a TIMESTAMP_SEC value.

The payload is seconds since 1970-01-01, the same unit value_get_timestamp_sec reports. It is not range-checked here;
value_cast is the validating path. The returned value is caller-owned; destroy it via value_destroy.

history:
- stable: v2.0.0


@conn@: The connection to use for the construction.

@in_value@: The payload to wrap.

@out_value@: Receives the new TIMESTAMP_SEC value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_timestamp_sec_with_connection"
    c_duckdb_v2_value_create_timestamp_sec_with_connection :: DuckDBV2ConnectionHandle -> Int64 -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a TIMESTAMP_MS value.

The payload is milliseconds since 1970-01-01, the same unit value_get_timestamp_ms reports. It is not range-checked
here; value_cast is the validating path. The returned value is caller-owned; destroy it via value_destroy.

history:
- stable: v2.0.0


@ctx@: The context to use for the construction.

@in_value@: The payload to wrap.

@out_value@: Receives the new TIMESTAMP_MS value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_timestamp_ms_with_context"
    c_duckdb_v2_value_create_timestamp_ms_with_context :: DuckDBV2ContextHandle -> Int64 -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a TIMESTAMP_MS value.

The payload is milliseconds since 1970-01-01, the same unit value_get_timestamp_ms reports. It is not range-checked
here; value_cast is the validating path. The returned value is caller-owned; destroy it via value_destroy.

history:
- stable: v2.0.0


@conn@: The connection to use for the construction.

@in_value@: The payload to wrap.

@out_value@: Receives the new TIMESTAMP_MS value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_timestamp_ms_with_connection"
    c_duckdb_v2_value_create_timestamp_ms_with_connection :: DuckDBV2ConnectionHandle -> Int64 -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a TIMESTAMP_NS value.

The payload is nanoseconds since 1970-01-01, the same unit value_get_timestamp_ns reports. It is not range-checked
here; value_cast is the validating path. The returned value is caller-owned; destroy it via value_destroy.

history:
- stable: v2.0.0


@ctx@: The context to use for the construction.

@in_value@: The payload to wrap.

@out_value@: Receives the new TIMESTAMP_NS value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_timestamp_ns_with_context"
    c_duckdb_v2_value_create_timestamp_ns_with_context :: DuckDBV2ContextHandle -> Int64 -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a TIMESTAMP_NS value.

The payload is nanoseconds since 1970-01-01, the same unit value_get_timestamp_ns reports. It is not range-checked
here; value_cast is the validating path. The returned value is caller-owned; destroy it via value_destroy.

history:
- stable: v2.0.0


@conn@: The connection to use for the construction.

@in_value@: The payload to wrap.

@out_value@: Receives the new TIMESTAMP_NS value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_timestamp_ns_with_connection"
    c_duckdb_v2_value_create_timestamp_ns_with_connection :: DuckDBV2ConnectionHandle -> Int64 -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a TIMESTAMP_TZ value.

The payload is microseconds since 1970-01-01, in UTC, the same unit value_get_timestamp_tz reports. It is not
range-checked here; value_cast is the validating path. The returned value is caller-owned; destroy it via
value_destroy.

history:
- stable: v2.0.0


@ctx@: The context to use for the construction.

@in_value@: The payload to wrap.

@out_value@: Receives the new TIMESTAMP_TZ value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_timestamp_tz_with_context"
    c_duckdb_v2_value_create_timestamp_tz_with_context :: DuckDBV2ContextHandle -> Int64 -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a TIMESTAMP_TZ value.

The payload is microseconds since 1970-01-01, in UTC, the same unit value_get_timestamp_tz reports. It is not
range-checked here; value_cast is the validating path. The returned value is caller-owned; destroy it via
value_destroy.

history:
- stable: v2.0.0


@conn@: The connection to use for the construction.

@in_value@: The payload to wrap.

@out_value@: Receives the new TIMESTAMP_TZ value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_timestamp_tz_with_connection"
    c_duckdb_v2_value_create_timestamp_tz_with_connection :: DuckDBV2ConnectionHandle -> Int64 -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a TIMESTAMP_TZ_NS value.

The payload is nanoseconds since 1970-01-01, in UTC, the same unit value_get_timestamp_tz_ns reports. It is not
range-checked here; value_cast is the validating path. The returned value is caller-owned; destroy it via
value_destroy.

history:
- stable: v2.0.0


@ctx@: The context to use for the construction.

@in_value@: The payload to wrap.

@out_value@: Receives the new TIMESTAMP_TZ_NS value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_timestamp_tz_ns_with_context"
    c_duckdb_v2_value_create_timestamp_tz_ns_with_context :: DuckDBV2ContextHandle -> Int64 -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a TIMESTAMP_TZ_NS value.

The payload is nanoseconds since 1970-01-01, in UTC, the same unit value_get_timestamp_tz_ns reports. It is not
range-checked here; value_cast is the validating path. The returned value is caller-owned; destroy it via
value_destroy.

history:
- stable: v2.0.0


@conn@: The connection to use for the construction.

@in_value@: The payload to wrap.

@out_value@: Receives the new TIMESTAMP_TZ_NS value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_timestamp_tz_ns_with_connection"
    c_duckdb_v2_value_create_timestamp_tz_ns_with_connection :: DuckDBV2ConnectionHandle -> Int64 -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a INTERVAL value.

The payload is the (months, days, micros) triple, the same unit value_get_interval reports. It is not range-checked
here; value_cast is the validating path. The returned value is caller-owned; destroy it via value_destroy.

history:
- stable: v2.0.0


@ctx@: The context to use for the construction.

@in_value@: The payload to wrap.

@out_value@: Receives the new INTERVAL value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_interval_with_context"
    c_duckdb_v2_value_create_interval_with_context :: DuckDBV2ContextHandle -> Ptr DuckDBV2IntervalT -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a INTERVAL value.

The payload is the (months, days, micros) triple, the same unit value_get_interval reports. It is not range-checked
here; value_cast is the validating path. The returned value is caller-owned; destroy it via value_destroy.

history:
- stable: v2.0.0


@conn@: The connection to use for the construction.

@in_value@: The payload to wrap.

@out_value@: Receives the new INTERVAL value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_interval_with_connection"
    c_duckdb_v2_value_create_interval_with_connection :: DuckDBV2ConnectionHandle -> Ptr DuckDBV2IntervalT -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a DECIMAL value from its backing integer.

The value is scaled by 10^scale, so (18500, 18, 3) is 18.500. width is the total digit count and must be 1..38; scale
is the number of digits after the point and must not exceed width. Violating either returns ERROR_INPUT_INVALID, as
does a value too wide for the storage tier the width selects. The value itself is not range-checked against the
width; value_cast is the validating path.

history:
- stable: v2.0.0


@ctx@: The context to use for the construction.

@in_value@: The backing integer, scaled by 10^scale.

@width@: Total digit count, 1..38.

@scale@: Digits after the decimal point; must not exceed width.

@out_value@: Receives the new DECIMAL value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_decimal_with_context"
    c_duckdb_v2_value_create_decimal_with_context :: DuckDBV2ContextHandle -> Ptr DuckDBV2HugeintT -> Word8 -> Word8 -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a DECIMAL value from its backing integer.

The value is scaled by 10^scale, so (18500, 18, 3) is 18.500. width is the total digit count and must be 1..38; scale
is the number of digits after the point and must not exceed width. Violating either returns ERROR_INPUT_INVALID, as
does a value too wide for the storage tier the width selects. The value itself is not range-checked against the
width; value_cast is the validating path.

history:
- stable: v2.0.0


@conn@: The connection to use for the construction.

@in_value@: The backing integer, scaled by 10^scale.

@width@: Total digit count, 1..38.

@scale@: Digits after the decimal point; must not exceed width.

@out_value@: Receives the new DECIMAL value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_decimal_with_connection"
    c_duckdb_v2_value_create_decimal_with_connection :: DuckDBV2ConnectionHandle -> Ptr DuckDBV2HugeintT -> Word8 -> Word8 -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a UUID value from its internal 128-bit form.

The payload is the storage form a vector element holds, not the canonical byte order: the high bit is flipped so the
integer sorts. To build one from canonical text, cast a VARCHAR with value_cast instead, which is also how a UUID
renders back.

history:
- stable: v2.0.0


@ctx@: The context to use for the construction.

@in_value@: The internal 128-bit storage form.

@out_value@: Receives the new UUID value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_uuid_with_context"
    c_duckdb_v2_value_create_uuid_with_context :: DuckDBV2ContextHandle -> Ptr DuckDBV2HugeintT -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a UUID value from its internal 128-bit form.

The payload is the storage form a vector element holds, not the canonical byte order: the high bit is flipped so the
integer sorts. To build one from canonical text, cast a VARCHAR with value_cast instead, which is also how a UUID
renders back.

history:
- stable: v2.0.0


@conn@: The connection to use for the construction.

@in_value@: The internal 128-bit storage form.

@out_value@: Receives the new UUID value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_uuid_with_connection"
    c_duckdb_v2_value_create_uuid_with_connection :: DuckDBV2ConnectionHandle -> Ptr DuckDBV2HugeintT -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a BIT value from its wire bytes.

The wire form is a mandatory padding-header byte — the count of leading bits in the first data byte that are not part
of the bit string — followed by the data bytes, so the input must be at least 1 byte. These are the same bytes
value_get_blob reports back.

history:
- stable: v2.0.0


@ctx@: The context to use for the construction.

@in_value@: The wire bytes, a padding header byte followed by data. Must be at least 1 byte.

@out_value@: Receives the new BIT value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_bit_with_context"
    c_duckdb_v2_value_create_bit_with_context :: DuckDBV2ContextHandle -> Ptr DuckDBV2Str -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a BIT value from its wire bytes.

The wire form is a mandatory padding-header byte — the count of leading bits in the first data byte that are not part
of the bit string — followed by the data bytes, so the input must be at least 1 byte. These are the same bytes
value_get_blob reports back.

history:
- stable: v2.0.0


@conn@: The connection to use for the construction.

@in_value@: The wire bytes, a padding header byte followed by data. Must be at least 1 byte.

@out_value@: Receives the new BIT value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_bit_with_connection"
    c_duckdb_v2_value_create_bit_with_connection :: DuckDBV2ConnectionHandle -> Ptr DuckDBV2Str -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a BIGNUM value from its storage bytes.

The bytes are opaque storage, as produced by bignum_encode from a magnitude and a sign flag; a negative is stored
bit-inverted behind a header, so these are not the magnitude bytes. The header plus at least one magnitude byte means
the input must exceed 3 bytes. These are the same bytes value_get_blob reports back, for bignum_decode to translate.

history:
- stable: v2.0.0


@ctx@: The context to use for the construction.

@in_value@: The opaque storage bytes from bignum_encode. Must exceed 3 bytes.

@out_value@: Receives the new BIGNUM value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_bignum_with_context"
    c_duckdb_v2_value_create_bignum_with_context :: DuckDBV2ContextHandle -> Ptr DuckDBV2Str -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a BIGNUM value from its storage bytes.

The bytes are opaque storage, as produced by bignum_encode from a magnitude and a sign flag; a negative is stored
bit-inverted behind a header, so these are not the magnitude bytes. The header plus at least one magnitude byte means
the input must exceed 3 bytes. These are the same bytes value_get_blob reports back, for bignum_decode to translate.

history:
- stable: v2.0.0


@conn@: The connection to use for the construction.

@in_value@: The opaque storage bytes from bignum_encode. Must exceed 3 bytes.

@out_value@: Receives the new BIGNUM value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_bignum_with_connection"
    c_duckdb_v2_value_create_bignum_with_connection :: DuckDBV2ConnectionHandle -> Ptr DuckDBV2Str -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a LIST value from its elements.

The child type is the common type of the elements, resolved by the same rule a SQL list literal follows, and every
element is cast to it; a set with no common type surfaces the cast error. Pass child_type to name the type instead,
which is also how an empty list is built: with child_type NULL, an empty element array has no type to resolve and
returns ERROR_INPUT_INVALID.

The elements are borrowed and copied in, and a NULL element becomes a typed NULL. The type is rebuilt from its child,
so an alias on the outer LIST type is not preserved; value_cast is the alias-preserving path.

history:
- stable: v2.0.0


@ctx@: The context to use for the construction.

@child_type@: Optional. The element type. Pass NULL to resolve it from the elements.

@children@: An array of child_count elements. Borrowed (copied in). Pass NULL when child_count is 0.

@child_count@: The number of elements.

@out_value@: Receives the new composite value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_list_with_context"
    c_duckdb_v2_value_create_list_with_context :: DuckDBV2ContextHandle -> DuckDBV2LogicalTypeHandle -> Ptr DuckDBV2ValueHandle -> DuckDBV2Idx -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a LIST value from its elements.

The child type is the common type of the elements, resolved by the same rule a SQL list literal follows, and every
element is cast to it; a set with no common type surfaces the cast error. Pass child_type to name the type instead,
which is also how an empty list is built: with child_type NULL, an empty element array has no type to resolve and
returns ERROR_INPUT_INVALID.

The elements are borrowed and copied in, and a NULL element becomes a typed NULL. The type is rebuilt from its child,
so an alias on the outer LIST type is not preserved; value_cast is the alias-preserving path.

history:
- stable: v2.0.0


@conn@: The connection to use for the construction.

@child_type@: Optional. The element type. Pass NULL to resolve it from the elements.

@children@: An array of child_count elements. Borrowed (copied in). Pass NULL when child_count is 0.

@child_count@: The number of elements.

@out_value@: Receives the new composite value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_list_with_connection"
    c_duckdb_v2_value_create_list_with_connection :: DuckDBV2ConnectionHandle -> DuckDBV2LogicalTypeHandle -> Ptr DuckDBV2ValueHandle -> DuckDBV2Idx -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates an ARRAY value from its elements, sized by their count.

The child type is resolved exactly as for value_create_list, and child_type names it explicitly the same way. The
minimum array size is 1, so an empty element array returns ERROR_INPUT_INVALID whether or not child_type is given.

The elements are borrowed and copied in, and a NULL element becomes a typed NULL.

history:
- stable: v2.0.0


@ctx@: The context to use for the construction.

@child_type@: Optional. The element type. Pass NULL to resolve it from the elements.

@children@: An array of child_count elements. Borrowed (copied in). Pass NULL when child_count is 0.

@child_count@: The number of elements.

@out_value@: Receives the new composite value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_array_with_context"
    c_duckdb_v2_value_create_array_with_context :: DuckDBV2ContextHandle -> DuckDBV2LogicalTypeHandle -> Ptr DuckDBV2ValueHandle -> DuckDBV2Idx -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates an ARRAY value from its elements, sized by their count.

The child type is resolved exactly as for value_create_list, and child_type names it explicitly the same way. The
minimum array size is 1, so an empty element array returns ERROR_INPUT_INVALID whether or not child_type is given.

The elements are borrowed and copied in, and a NULL element becomes a typed NULL.

history:
- stable: v2.0.0


@conn@: The connection to use for the construction.

@child_type@: Optional. The element type. Pass NULL to resolve it from the elements.

@children@: An array of child_count elements. Borrowed (copied in). Pass NULL when child_count is 0.

@child_count@: The number of elements.

@out_value@: Receives the new composite value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_array_with_connection"
    c_duckdb_v2_value_create_array_with_connection :: DuckDBV2ConnectionHandle -> DuckDBV2LogicalTypeHandle -> Ptr DuckDBV2ValueHandle -> DuckDBV2Idx -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a STRUCT value from named fields, in order.

Each field keeps its own child's type, so there is nothing to resolve across them; names and children are parallel
arrays of field_count entries. An empty field list builds the empty struct, which is a real type.

The names and children are borrowed and copied in, and a NULL child becomes a typed NULL. Duplicate field names are
rejected.

history:
- stable: v2.0.0


@ctx@: The context to use for the construction.

@names@: An array of field_count field names. Pass NULL when field_count is 0.

@children@: An array of field_count field values. Borrowed (copied in). Pass NULL when field_count is 0.

@field_count@: The number of fields.

@out_value@: Receives the new composite value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_struct_with_context"
    c_duckdb_v2_value_create_struct_with_context :: DuckDBV2ContextHandle -> Ptr DuckDBV2IdentifierT -> Ptr DuckDBV2ValueHandle -> DuckDBV2Idx -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a STRUCT value from named fields, in order.

Each field keeps its own child's type, so there is nothing to resolve across them; names and children are parallel
arrays of field_count entries. An empty field list builds the empty struct, which is a real type.

The names and children are borrowed and copied in, and a NULL child becomes a typed NULL. Duplicate field names are
rejected.

history:
- stable: v2.0.0


@conn@: The connection to use for the construction.

@names@: An array of field_count field names. Pass NULL when field_count is 0.

@children@: An array of field_count field values. Borrowed (copied in). Pass NULL when field_count is 0.

@field_count@: The number of fields.

@out_value@: Receives the new composite value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_struct_with_connection"
    c_duckdb_v2_value_create_struct_with_connection :: DuckDBV2ConnectionHandle -> Ptr DuckDBV2IdentifierT -> Ptr DuckDBV2ValueHandle -> DuckDBV2Idx -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a TUPLE value from positional fields.

TUPLE is the unnamed struct: the same positional children as value_create_struct, under a distinct type id and with
no names. Each field keeps its own child's type. An empty field list builds the empty tuple, which is a real type.

The children are borrowed and copied in, and a NULL child becomes a typed NULL.

history:
- stable: v2.0.0


@ctx@: The context to use for the construction.

@children@: An array of field_count field values. Borrowed (copied in). Pass NULL when field_count is 0.

@field_count@: The number of fields.

@out_value@: Receives the new composite value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_tuple_with_context"
    c_duckdb_v2_value_create_tuple_with_context :: DuckDBV2ContextHandle -> Ptr DuckDBV2ValueHandle -> DuckDBV2Idx -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a TUPLE value from positional fields.

TUPLE is the unnamed struct: the same positional children as value_create_struct, under a distinct type id and with
no names. Each field keeps its own child's type. An empty field list builds the empty tuple, which is a real type.

The children are borrowed and copied in, and a NULL child becomes a typed NULL.

history:
- stable: v2.0.0


@conn@: The connection to use for the construction.

@children@: An array of field_count field values. Borrowed (copied in). Pass NULL when field_count is 0.

@field_count@: The number of fields.

@out_value@: Receives the new composite value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_tuple_with_connection"
    c_duckdb_v2_value_create_tuple_with_connection :: DuckDBV2ConnectionHandle -> Ptr DuckDBV2ValueHandle -> DuckDBV2Idx -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a MAP value from parallel key and value arrays.

The key and value types are the common types of the keys and of the values, resolved as for value_create_list, and
every entry is cast to them. Pass key_type and value_type to name them instead, which is also how an empty map is
built: with both NULL, empty key and value arrays have no types to resolve and return ERROR_INPUT_INVALID.

Keys and values are borrowed and copied in, and the two arrays must be the same length. Keys must be non-NULL and
unique; both are enforced.

history:
- stable: v2.0.0


@ctx@: The context to use for the construction.

@key_type@: Optional. The key type. Pass NULL to resolve it from the keys.

@value_type@: Optional. The value type. Pass NULL to resolve it from the values.

@keys@: An array of entry_count keys. Borrowed (copied in). Pass NULL when entry_count is 0.

@values@: An array of entry_count values, parallel to keys. Borrowed (copied in). Pass NULL when entry_count is
0.

@entry_count@: The number of entries.

@out_value@: Receives the new composite value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_map_with_context"
    c_duckdb_v2_value_create_map_with_context :: DuckDBV2ContextHandle -> DuckDBV2LogicalTypeHandle -> DuckDBV2LogicalTypeHandle -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ValueHandle -> DuckDBV2Idx -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a MAP value from parallel key and value arrays.

The key and value types are the common types of the keys and of the values, resolved as for value_create_list, and
every entry is cast to them. Pass key_type and value_type to name them instead, which is also how an empty map is
built: with both NULL, empty key and value arrays have no types to resolve and return ERROR_INPUT_INVALID.

Keys and values are borrowed and copied in, and the two arrays must be the same length. Keys must be non-NULL and
unique; both are enforced.

history:
- stable: v2.0.0


@conn@: The connection to use for the construction.

@key_type@: Optional. The key type. Pass NULL to resolve it from the keys.

@value_type@: Optional. The value type. Pass NULL to resolve it from the values.

@keys@: An array of entry_count keys. Borrowed (copied in). Pass NULL when entry_count is 0.

@values@: An array of entry_count values, parallel to keys. Borrowed (copied in). Pass NULL when entry_count is
0.

@entry_count@: The number of entries.

@out_value@: Receives the new composite value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_create_map_with_connection"
    c_duckdb_v2_value_create_map_with_connection :: DuckDBV2ConnectionHandle -> DuckDBV2LogicalTypeHandle -> DuckDBV2LogicalTypeHandle -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ValueHandle -> DuckDBV2Idx -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the number of child values of a value.

Per type kind: the element count for LIST and ARRAY; the field count for STRUCT and TUPLE; twice the entry count for
MAP, whose children alternate key, value; and 2 for UNION, the tag plus the active member. Everything else reports 0,
NULL values of any nested type included.

history:
- stable: v2.0.0


@value@: The value to read.

@out_count@: Receives the number of children.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_get_child_count"
    c_duckdb_v2_value_get_child_count :: DuckDBV2ValueHandle -> Ptr DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns one child of a composite value as an owned copy.

LIST and ARRAY children are the elements. STRUCT and TUPLE children are the fields in declared order, and a STRUCT's
field names come from its type, via logical_type_get_param. MAP children alternate key, value, symmetric with
value_create_map. UNION children are [0] the tag, as a UTINYINT value, and [1] the active member, because a union
value holds only that one member.

Note the divergence from vector_get_child, which descends structurally and so exposes [0] = tag and [1..N] = ALL
members. Code written to descend generically over both must account for that difference.

An out-of-range index returns ERROR_INPUT_INVALID, as does any index on a non-composite or NULL value. The returned
value is caller-owned; destroy it via value_destroy.

history:
- stable: v2.0.0


@value@: The value to read.

@index@: The child index, in [0, child_count).

@out_child@: Receives the owned child value.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_get_child"
    c_duckdb_v2_value_get_child :: DuckDBV2ValueHandle -> DuckDBV2Idx -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Casts a value to a target type, using a connection.

The same as value_cast_with_context, except that the cast function set comes from a connection: the cast runs in its
own transaction on that connection's context. Use it from outside DuckDB, where a connection — but no context — is in
hand.

The conversion is the SQL-faithful, non-strict one, registered custom casts included, and a cast failure surfaces
from the call. Together with a VARCHAR built through value_create_varchar_with_context / _with_connection, this
constructs any value from text, extension values included; casting a member value to a union type, or a VARCHAR to an
enum type, is the sanctioned way to build UNION and ENUM values.

The input value and target type are borrowed. The returned value is caller-owned; destroy it via value_destroy.

history:
- stable: v2.0.0


@conn@: The connection whose context supplies the cast function set.

@value@: The borrowed value to cast.

@target_type@: The borrowed target logical type.

@out_value@: Receives the owned cast result.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_cast_with_connection"
    c_duckdb_v2_value_cast_with_connection :: DuckDBV2ConnectionHandle -> DuckDBV2ValueHandle -> DuckDBV2LogicalTypeHandle -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Casts a value to a target type, using a context.

The conversion is the SQL-faithful, non-strict one, registered custom casts included, and a cast failure surfaces
from the call. Together with a VARCHAR built through value_create_varchar_with_context / _with_connection, this
constructs any value from text, extension values included; casting a member value to a union type, or a VARCHAR to an
enum type, is the sanctioned way to build UNION and ENUM values.

Runs in the caller's context scope, as create_type_from_text does: reach it from a bind-phase callback or another
context-holding scope, not from an exec-phase worker callback. From outside DuckDB use value_cast_with_connection.

The input value and target type are borrowed. The returned value is caller-owned; destroy it via value_destroy.

history:
- stable: v2.0.0


@ctx@: The context supplying the cast function set.

@value@: The borrowed value to cast.

@target_type@: The borrowed target logical type.

@out_value@: Receives the owned cast result.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_cast_with_context"
    c_duckdb_v2_value_cast_with_context :: DuckDBV2ContextHandle -> DuckDBV2ValueHandle -> DuckDBV2LogicalTypeHandle -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns whether the value is NULL.

history:
- stable: v2.0.0


@value@: The value to read.

@out_is_null@: Receives true if the value is NULL, false otherwise.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_is_null"
    c_duckdb_v2_value_is_null :: DuckDBV2ValueHandle -> Ptr CBool -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the logical type of the value.

The returned logical type is caller-owned; destroy it via logical_type_destroy.

history:
- stable: v2.0.0


@value@: The value to read.

@out_type@: Receives the owned logical type.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_get_logical_type"
    c_duckdb_v2_value_get_logical_type :: DuckDBV2ValueHandle -> Ptr DuckDBV2LogicalTypeHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Renders the value as a human-readable string. Diagnostic only.

Writes into a caller-supplied buffer, so nothing is allocated on the caller's behalf and nothing has to be freed.

Pass out_string = NULL to size the buffer without rendering into it: out_length then receives the length, and
out_capacity is ignored. With out_string != NULL, out_capacity must be at least out_length + 1, or the call returns
ERROR_INPUT_OBJECT_SIZE with out_length set to the required length and out_string left untouched.

out_length never counts the terminator, but a successful write always appends one, so the buffer is usable as a C
string.

history:
- stable: v2.0.0


@value@: The value to read.

@out_string@: Caller-owned buffer receiving the text plus a null terminator, or NULL to only report the required
length in out_length.

@out_capacity@: Bytes available in out_string, terminator included. Ignored when out_string is NULL.

@out_length@: Receives the text length excluding the null terminator — written on success and on
ERROR_INPUT_OBJECT_SIZE.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_value_to_string"
    c_duckdb_v2_value_to_string :: DuckDBV2ValueHandle -> Ptr CChar -> DuckDBV2Idx -> Ptr DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Prepares a parsed statement into a reusable handle. Non-consuming.

Copies the statement's AST, then binds and plans it once. Binder and catalog errors surface here, exactly as they
would from @duckdb_v2_statement_execute()@. The statement is borrowed rather than consumed, since a copy is what gets
prepared, so it can be prepared again or executed directly; the caller destroys it with
@duckdb_v2_sql_statement_destroy()@.

By default this succeeds for any preparable statement, whether or not its plan will be reused; ask
@duckdb_v2_prepared_statement_reuses_plan()@ which one you got. Setting @require_cacheable@ instead fails with
@ERROR_INPUT_INVALID@ when the plan would not be reused, so a caller who wants the handle only for the speedup finds
out here rather than after silently taking the slow path.

Refuses with @ERROR_RESOURCE_IN_USE@ while the connection has a live result. Drain, destroy, or interrupt that result
first, or prepare on another connection. @*out_prepared@ is set to NULL on failure.

history:
- stable: v2.0.0


@conn@: The connection supplying the catalog, transaction, and parser state. The prepared statement belongs to
it.

@statement@: The statement to prepare. Borrowed and copied, not consumed; destroy it with
@duckdb_v2_sql_statement_destroy()@.

@require_cacheable@: When true, fail with @ERROR_INPUT_INVALID@ unless the prepared plan will be reused across
executions, as @duckdb_v2_prepared_statement_reuses_plan()@ would report it.

@out_prepared@: On success, receives the new prepared statement. Owned by the caller; destroy via
@duckdb_v2_prepared_statement_destroy()@.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_prepared_statement_create"
    c_duckdb_v2_prepared_statement_create :: DuckDBV2ConnectionHandle -> DuckDBV2SqlStatementHandle -> CBool -> Ptr DuckDBV2PreparedStatementHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Executes a prepared statement, streaming its result. Non-consuming.

Returns a result without executing anything: execution happens incrementally as the result is stepped or drained,
exactly as with @duckdb_v2_statement_execute()@. The handle returned is an ordinary result, with identical behaviour
throughout -- streaming, draining, the changed-row count of a DML statement, the output schema, the statement type,
and the result type.

@parameter_values@ binds the statement's parameters as constants for this execution. Binding is positional by default
($1 = element 0); supply @parameter_names@ to bind by name instead, where a non-empty entry binds its value to that
named parameter ($name, matched case-insensitively) and a {NULL, 0} entry stays positional. Pass NULL for both
arrays, or a count of 0, for a statement without parameters. Both arrays are borrowed and copied in, so the caller
still owns and destroys them. A key set that does not match the statement's parameters is rejected with
@ERROR_INPUT_INVALID@, with one exception: a named parameter left without a value reads the session variable of the
same name (@SET VARIABLE@) when one exists.

Not consumed: execute the same handle again, with the same values or different ones, as often as you like. Values are
bound per execution and nothing carries over between them. A catalog change since the statement was prepared, or a
parameter type that differs from the one the cached plan assumed, triggers a re-bind that is invisible apart from its
cost.

Refuses with @ERROR_RESOURCE_IN_USE@ while the connection has a live result; drain, destroy, or interrupt it first,
or execute on another connection. A failed execution, at any stage, leaves the prepared statement usable.
@*out_result@ is set to NULL on failure.

history:
- stable: v2.0.0


@prepared@: The prepared statement to execute. Borrowed; not consumed.

@parameter_names@: Optional. An array of @parameter_count@ parameter names; a non-empty entry binds its value to
the named parameter ($name, case-insensitive), a {NULL, 0} entry keeps it positional ($1 = element 0). Pass NULL to
bind everything positionally.

@parameter_values@: Optional. An array of @parameter_count@ values. Each binds by name when @parameter_names@
supplies one, and positionally ($1 = element 0) otherwise. Borrowed and copied in. Pass NULL for a statement without
parameters.

@parameter_count@: The number of entries in @parameter_names@ and @parameter_values@. Pass 0 for a statement
without parameters.

@out_result@: On success, receives the new result. Owned by the caller; destroy via @duckdb_v2_result_destroy()@.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_prepared_statement_execute"
    c_duckdb_v2_prepared_statement_execute :: DuckDBV2PreparedStatementHandle -> Ptr DuckDBV2IdentifierT -> Ptr DuckDBV2ValueHandle -> DuckDBV2Idx -> Ptr DuckDBV2ResultHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Reports whether the prepared statement reuses its compiled plan across executions.

True when executions reuse the plan built at prepare time, provided the supplied values match the planned parameter
types and nothing the plan depends on has changed; false when the statement re-binds on every execution, making it no
faster than @duckdb_v2_statement_execute()@. A plan is reused only when all parameter types were resolved at prepare
time and the plan is cacheable: @SELECT 42@ reuses, @SELECT $1::INTEGER + 1@ reuses, a statement reading a base table
does not (it re-binds so a catalog change is picked up), and @SELECT $1 + $2@ does not (the types are unknown until
values arrive).

A static property of the built plan, fixed when the statement was prepared and independent of the values later passed
to @duckdb_v2_prepared_statement_execute()@.

history:
- stable: v2.0.0


@prepared@: The prepared statement to inspect.

@out_reuses@: Receives true when the compiled plan is reused across executions, false when the statement re-binds
each time.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_prepared_statement_reuses_plan"
    c_duckdb_v2_prepared_statement_reuses_plan :: DuckDBV2PreparedStatementHandle -> Ptr CBool -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Destroys a prepared statement.

Null-safe: passing NULL, or a slot already set to NULL, is a no-op. A result produced by
@duckdb_v2_prepared_statement_execute()@ is independently owned and keeps the session alive itself, so the prepared
statement may be destroyed while results made from it are still live. On success the slot is set to NULL.

history:
- stable: v2.0.0


@prepared@: The prepared statement to destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_prepared_statement_destroy"
    c_duckdb_v2_prepared_statement_destroy :: Ptr DuckDBV2PreparedStatementHandle -> IO DuckDBV2Error

{- | Executes a parsed statement on the connection, streaming its result. Non-consuming.

Takes a statement produced by the sql_statement module (parse_sql / statement_iterator_next), preprocesses and
prepares it, and returns a result handle without executing anything: execution happens incrementally as the result is
stepped (result_step) or drained (result_fetch_chunk). This call reports only the errors detectable at prepare time —
binder, catalog, pragma preprocessing — while errors raised during execution surface from the stepping functions.

The statement is borrowed, not consumed, since a copy is what executes. It can be executed again, for example with a
different set of values, and the caller destroys it with sql_statement_destroy.

parameter_values binds the statement's parameters as constants for this execution. Binding is positional by default:
the i-th value binds $(i+1), matching SQL's own convention, so dense $1..$N and ? placeholders work directly. Supply
parameter_names to bind by name instead — a non-empty name binds its value to that named parameter ($name, matched
case-insensitively), while a {NULL, 0} entry leaves that value positional. The parameter schema from statement_bind
lists the names to use. Both arrays are borrowed and copied in; the caller still owns and destroys them. Pass NULL
for both, or a count of 0, for an unparameterized statement. A key set that does not match the statement's parameters
— names for a positional statement, or the reverse — is rejected with ERROR_INPUT_INVALID, and named and positional
parameters cannot be mixed within one statement. Parameters and statement expansion are mutually exclusive: passing
values for a statement that preprocesses into a group is rejected with ERROR_INPUT_INVALID.

Preprocessing can expand one statement into a group — a dynamic PIVOT, or statement-expanding DDL such as ALTER ...
ADD COLUMN with a non-constant DEFAULT. The group executes as one result through the same steps, and the stream
surfaces the first row-producing statement of the group, or the last statement when none produces rows. An expansion
with more than one row-producing statement cannot be streamed as a single result and reports
ERROR_QUERY_NOT_IMPLEMENTED; no known expansion produces one.

One live result per connection: this refuses with ERROR_RESOURCE_IN_USE while the connection already has a live
result. Drain, destroy, or interrupt that one first, or open another connection.

Schema metadata — result type, statement type, column count, names, logical types — is available on the returned
handle immediately, before the first step.

*out_result is set to nullptr on failure.

history:
- stable: v2.0.0


@conn@: The connection on which to execute the statement.

@statement@: The statement to execute. Borrowed and copied; not consumed. Destroy it with sql_statement_destroy.

@parameter_names@: Optional. An array of parameter_count parameter names; a non-empty entry binds its value to
the named parameter ($name, case-insensitive), a {NULL, 0} entry keeps it positional ($1 = element 0). Pass NULL for
all-positional binding. Named and positional parameters cannot be mixed within one statement.

@parameter_values@: Optional. An array of parameter_count value handles. Each binds by name when parameter_names
supplies one, otherwise positionally ($1 = element 0). Borrowed (copied in). Pass NULL for an unparameterized
statement.

@parameter_count@: The number of parameters in parameter_names and parameter_values. Pass 0 for an
unparameterized statement.

@out_result@: Receives the new result handle.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_statement_execute"
    c_duckdb_v2_statement_execute :: DuckDBV2ConnectionHandle -> DuckDBV2SqlStatementHandle -> Ptr DuckDBV2IdentifierT -> Ptr DuckDBV2ValueHandle -> DuckDBV2Idx -> Ptr DuckDBV2ResultHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Destroys a result handle.

Null-safe: passing nullptr or a slot already set to nullptr is a no-op. Frees the memory the result owns and releases
the connection for its next query. Safe at any point in the stream's life, though destroying a partially consumed
result abandons the remaining execution, including side effects not yet applied. Chunks already fetched are
caller-owned and stay valid. On success the slot is set to nullptr.

history:
- stable: v2.0.0


@result@: The result to destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_result_destroy"
    c_duckdb_v2_result_destroy :: Ptr DuckDBV2ResultHandle -> IO DuckDBV2Error

{- | Runs one bounded unit of query execution and returns without blocking.

The streaming primitive: a step does a bounded amount of execution work, mostly without blocking, and returns control
to the caller, so an event loop stays responsive and can interleave other work between steps. The conveniences —
result_wait, result_fetch_chunk, result_drain — block, and are for synchronous callers. out_status reports the
outcome:

- CHUNK: *out_chunk receives a caller-owned chunk (destroy via data_chunk_destroy). Written only for this status;
nullptr otherwise.
- WAITING: no chunk yet, but work was done. Transient: keep stepping and it resolves to CHUNK, FINISHED, CANCELLED,
or an error. Block in result_wait rather than busy-stepping.
- FINISHED: stream exhausted. Sticky.
- CANCELLED: query interrupted via connection_interrupt. Sticky. Cancellation is a status here, not an error;
result_fetch_chunk, which has no status out-param, reports it as ERROR_RUNTIME_INTERRUPT.

Execution errors come back as the return code plus err, never as a status; out_status is then unspecified and
*out_chunk is nullptr. Errors are sticky, so later steps report the same code.

history:
- stable: v2.0.0


@result@: The result to step.

@out_chunk@: Receives an owned chunk if *out_status is CHUNK; set to nullptr otherwise.

@out_status@: Receives the step status.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_result_step"
    c_duckdb_v2_result_step :: DuckDBV2ResultHandle -> Ptr DuckDBV2DataChunkHandle -> Ptr DuckDBV2ResultStepStatus -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Blocks until the next chunk is available and returns it.

A convenience over result_step: blocks until a chunk is produced or the stream ends. On success *out_chunk receives a
caller-owned chunk (destroy via data_chunk_destroy), or nullptr at end-of-stream. End-of-stream is sticky, so later
calls keep succeeding with *out_chunk set to nullptr.

An interrupted query returns ERROR_RUNTIME_INTERRUPT — the same event result_step reports as status CANCELLED,
carried on the error channel because this function has no status out-param.

On failure *out_chunk is set to nullptr. Errors are sticky, so later calls report the same code.

history:
- stable: v2.0.0


@result@: The result to fetch from.

@out_chunk@: Receives an owned chunk, or nullptr at end-of-stream.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_result_fetch_chunk"
    c_duckdb_v2_result_fetch_chunk :: DuckDBV2ResultHandle -> Ptr DuckDBV2DataChunkHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Blocks until result_step can make progress.

A convenience over result_step: blocks until a step is worth issuing again, that is, until a unit of execution work
can run on the calling thread. Never produces or consumes chunks. Waiting on a terminal result — FINISHED, CANCELLED,
or a sticky error — returns immediately; it is a no-op, never an error.

history:
- stable: v2.0.0


@result@: The result to wait on.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_result_wait"
    c_duckdb_v2_result_wait :: DuckDBV2ResultHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Renders the result as a box table, consuming it.

Drains the result into a column data collection and renders it with the same renderer the DuckDB CLI uses, so every
client displays results identically without reimplementing table formatting. The result is consumed by transfer: the
slot is set to NULL on success and on failure alike. A partially consumed result is accepted, and the remainder is
what gets rendered.

The whole remaining result materializes in memory before rendering. max_rows bounds what is DISPLAYED, not what is
read, so with limit 0 the footer's row count is exact. A caller who cannot afford full materialization should bound
the query itself (e.g. LIMIT n) and pass n as limit; the footer then renders "? rows" whenever the result fills that
bound, since the true total is unknown at that point.

Zero selects the renderer default for each sizing knob: max_rows 20, max_width the probed terminal width or 80 when
that is unavailable, max_col_width 20. An empty null_value renders NULL cells as the default "NULL" text.

The rendered text goes to sink rather than being returned: a box can be large, and this way nothing allocates a
buffer the caller has to free. The sink is called exactly once with the whole box, so its view carries the full
length — write it straight to a stream, or copy it into your own string. sink must not be NULL.

history:
- stable: v2.0.0


@result@: The result to render; consumed and set to NULL. Left intact only when the call rejects null arguments.

@max_rows@: Maximum rows displayed; 0 selects the renderer default (20).

@max_width@: Maximum total width in characters; 0 selects the renderer default.

@max_col_width@: Maximum width of one column; 0 selects the renderer default (20).

@null_value@: Text rendered for NULL cells; empty selects "NULL".

@render_mode@: 0 renders rows (records down the page); 1 renders columns; other values are rejected with
INVALID_INPUT.

@limit@: The row limit the caller applied to the query before rendering; 0 means none. When the materialized
result holds exactly this many rows the true total is unknown, so the footer renders "? rows" instead of an exact
count.

@sink@: Receives the whole rendered box in a single call. Borrowed for the duration of that call; see text_sink
for the full contract.

@user_data@: Opaque pointer passed through to sink untouched. May be NULL.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_result_render_box"
    c_duckdb_v2_result_render_box :: Ptr DuckDBV2ResultHandle -> DuckDBV2Idx -> DuckDBV2Idx -> DuckDBV2Idx -> Ptr DuckDBV2Str -> DuckDBV2Idx -> DuckDBV2Idx -> FunPtr DuckDBV2TextSinkFn -> Ptr () -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Runs the result to completion and reports the changed-row count.

A convenience over result_step: blocks until the stream is fully consumed, so every side effect is applied. Rows of a
row-producing result are discarded. For a CHANGED_ROWS result *out_rows_changed receives the affected row count; for
every other result type, and for a stream whose Count chunk was already consumed, it receives 0.

The result type (result_get_result_type) is prepare-time metadata, so a caller can choose between consuming rows and
draining without inspecting the SQL. Draining an already FINISHED result succeeds. Cancellation surfaces as
ERROR_RUNTIME_INTERRUPT; errors are sticky, and on failure *out_rows_changed is unspecified.

history:
- stable: v2.0.0


@result@: The result to drain.

@out_rows_changed@: Receives the affected row count for CHANGED_ROWS results, 0 otherwise.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_result_drain"
    c_duckdb_v2_result_drain :: DuckDBV2ResultHandle -> Ptr DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the shape of the result: query, changed rows, or nothing.

QUERY_RESULT for statements that produce rows (SELECT, RETURNING, EXPLAIN), CHANGED_ROWS for an INSERT, UPDATE, or
DELETE without RETURNING, NOTHING for DDL and other statements with no row output.

Prepare-time metadata: available from statement_execute on, except for a statement that preprocessing expands into a
group, where it fails with ERROR_INPUT_INVALID until stepping has prepared the row-producing fragment.

history:
- stable: v2.0.0


@result@: The result.

@out_type@: Receives the result shape.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_result_get_result_type"
    c_duckdb_v2_result_get_result_type :: DuckDBV2ResultHandle -> Ptr DuckDBV2ResultType -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the SQL statement type that produced the result.

Prepare-time metadata: available from statement_execute on, except for a statement that preprocessing expands into a
group, where it fails with ERROR_INPUT_INVALID until stepping has prepared the row-producing fragment.

history:
- stable: v2.0.0


@result@: The result.

@out_type@: Receives the statement type.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_result_get_statement_type"
    c_duckdb_v2_result_get_statement_type :: DuckDBV2ResultHandle -> Ptr DuckDBV2StatementType -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the result's output schema as a single schema handle.

Builds an owned schema of the result's column names and types into *out_schema; destroy it via schema_destroy.
Prepare-time metadata, available before the first step, or for an expanding statement once stepping has prepared the
row-producing fragment. Never empty: a non-SELECT reports a single status column, either a BIGINT changed-rows count
or a BOOLEAN success.

This schema is the authoritative source for the column types of the chunks the result produces; vectors do not carry
their own type.

*out_schema is set to NULL on failure.

history:
- stable: v2.0.0


@result@: The result.

@out_schema@: Receives the owned output schema. Destroy via schema_destroy.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via error_info_destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_result_get_schema"
    c_duckdb_v2_result_get_schema :: DuckDBV2ResultHandle -> Ptr DuckDBV2SchemaHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a new table function that will be registered on the connection's database.

The function starts out empty: configure it with the setter functions (e.g. @duckdb_v2_table_function_set_name()@,
@duckdb_v2_table_function_set_exec_callback()@, etc.) and the signature obtained via
@duckdb_v2_table_function_get_signature()@, then make it available with @duckdb_v2_table_function_register()@. The
caller owns the returned handle and must destroy it with @duckdb_v2_table_function_destroy()@, also after
registration.

history:
- stable: v2.0.0


@connection@: The connection to create the function in.

@function@: On success, receives the newly created table function. Owned by the caller.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_create_with_connection"
    c_duckdb_v2_table_function_create_with_connection :: DuckDBV2ConnectionHandle -> Ptr DuckDBV2TableFunctionHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a new table function that will be registered on the loading extension's database.

Use this from an extension load callback, where an extension handle is available. The function starts out empty:
configure it with the setter functions (e.g. @duckdb_v2_table_function_set_name()@,
@duckdb_v2_table_function_set_exec_callback()@, etc.) and the signature obtained via
@duckdb_v2_table_function_get_signature()@, then make it available with @duckdb_v2_table_function_register()@. The
caller owns the returned handle and must destroy it with @duckdb_v2_table_function_destroy()@, also after
registration.

history:
- stable: v2.0.0


@extension@: The extension to create the function in.

@function@: On success, receives the newly created table function. Owned by the caller.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_create_with_extension"
    c_duckdb_v2_table_function_create_with_extension :: DuckDBV2ExtensionHandle -> Ptr DuckDBV2TableFunctionHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets the name of the table function.

The name is borrowed and copied. Calling this again replaces the previous name. A name must be set before
registration.

history:
- stable: v2.0.0


@function@: The function to set the name of.

@name@: The name to set. Borrowed and copied.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_set_name"
    c_duckdb_v2_table_function_set_name :: DuckDBV2TableFunctionHandle -> Ptr DuckDBV2IdentifierT -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the function's signature so it can be configured.

Add parameters with @duckdb_v2_function_signature_add_parameter()@. The signature is modified in place. A table
function declares the columns it returns from its bind callback instead of through a return type, so registration
rejects a signature whose return type was set with @duckdb_v2_function_signature_set_return_type()@.

history:
- stable: v2.0.0


@function@: The function to get the signature of.

@sig@: The returned signature. Borrowed and valid for the lifetime of the function handle.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_get_signature"
    c_duckdb_v2_table_function_get_signature :: DuckDBV2TableFunctionHandle -> Ptr DuckDBV2FunctionSignatureHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets arbitrary user data on the table function.

Associates an opaque pointer with the function, retrievable from the callbacks via
@duckdb_v2_function_bind_get_user_data()@, @duckdb_v2_table_function_init_global_get_user_data()@,
@duckdb_v2_table_function_exec_get_user_data()@ and their counterparts on the other phases. The opaque handle bundles
the pointer with an optional destructor, invoked when the data is no longer needed.

history:
- stable: v2.0.0


@function@: The function to set the user data of.

@data@: Opaque handle bundling the user data pointer plus an optional destructor.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_set_user_data"
    c_duckdb_v2_table_function_set_user_data :: DuckDBV2TableFunctionHandle -> Ptr DuckDBV2Opaque -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets the bind callback of the table function.

The bind callback is invoked during query planning for each call site of the function. It must declare the columns
the function returns via @duckdb_v2_table_function_bind_add_result_column()@. Through its
@duckdb_v2_function_bind_info_handle@ it can also inspect the constant argument values and set "bind data" that is
shared with all later callbacks. A bind callback must be set before registration.

history:
- stable: v2.0.0


@function@: The function to set the bind callback of.

@callback@: The bind callback to set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_set_bind_callback"
    c_duckdb_v2_table_function_set_bind_callback :: DuckDBV2TableFunctionHandle -> FunPtr DuckDBV2TableFunctionBindCallbackFn -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets the optional global init callback of the table function.

The global init callback is invoked once per scan, at the start of execution. It can set "global state" shared by
every thread scanning the function, and declare how many threads may scan it in parallel via
@duckdb_v2_table_function_init_global_set_max_threads()@.

history:
- stable: v2.0.0


@function@: The function to set the global init callback of.

@callback@: The global init callback to set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_set_init_global_callback"
    c_duckdb_v2_table_function_set_init_global_callback :: DuckDBV2TableFunctionHandle -> FunPtr DuckDBV2TableFunctionInitGlobalCallbackFn -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets the optional local init callback of the table function.

The local init callback is invoked once per thread that will scan the function. It can set worker-local "local
state", retrievable from the exec callback, typically derived from the shared global state.

history:
- stable: v2.0.0


@function@: The function to set the local init callback of.

@callback@: The local init callback to set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_set_init_local_callback"
    c_duckdb_v2_table_function_set_init_local_callback :: DuckDBV2TableFunctionHandle -> FunPtr DuckDBV2TableFunctionInitLocalCallbackFn -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets the exec callback of the table function.

The exec callback implements the function's logic: it is invoked repeatedly during query execution and writes the
next batch of rows to the output chunk, until it produces an empty batch to signal the end of the scan. An exec
callback must be set before registration.

history:
- stable: v2.0.0


@function@: The function to set the exec callback of.

@callback@: The exec callback to set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_set_exec_callback"
    c_duckdb_v2_table_function_set_exec_callback :: DuckDBV2TableFunctionHandle -> FunPtr DuckDBV2TableFunctionExecCallbackFn -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets the optional progress callback of the table function.

The progress callback is invoked on demand during execution to report how far the scan has advanced, which the engine
surfaces as the query's progress. It is invoked concurrently with the exec callback, so it must read the global state
in a thread-safe way.

history:
- stable: v2.0.0


@function@: The function to set the progress callback of.

@callback@: The progress callback to set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_set_progress_callback"
    c_duckdb_v2_table_function_set_progress_callback :: DuckDBV2TableFunctionHandle -> FunPtr DuckDBV2TableFunctionProgressCallbackFn -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets whether the table function supports projection pushdown. Defaults to false.

With projection pushdown, the engine asks the function for only the columns a query actually uses: the exec
callback's output chunk holds one vector per requested column rather than one per column declared in bind, and the
global init, local init and exec callbacks can look up which declared column each vector stands for via e.g.
@duckdb_v2_table_function_exec_get_column_index()@. Without it the output chunk always holds every declared column,
and the engine drops the unused ones itself.

history:
- stable: v2.0.0


@function@: The function to configure.

@enable@: Whether the function supports projection pushdown.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_set_projection_pushdown"
    c_duckdb_v2_table_function_set_projection_pushdown :: DuckDBV2TableFunctionHandle -> CBool -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets the optional filter pushdown callback of the table function.

The filter pushdown callback is invoked while the query is optimized, after the bind callback and before any init
callback, with the filter predicates the query applies to the function's rows. Each predicate is a bound expression
the callback can inspect with the @expression@ functions; the callback accepts the predicates it will apply itself
via @duckdb_v2_table_function_filter_pushdown_accept()@, typically after recording what they select in the bind data.
The engine then stops applying the accepted predicates and keeps applying the rest above the scan, so accepting a
predicate is a promise to filter the rows exactly as it would have. The optimizer may invoke the callback more than
once for the same query, each time with the predicates not yet accepted; it is not invoked when there are none.

history:
- stable: v2.0.0


@function@: The function to set the filter pushdown callback of.

@callback@: The filter pushdown callback to set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_set_filter_pushdown_callback"
    c_duckdb_v2_table_function_set_filter_pushdown_callback :: DuckDBV2TableFunctionHandle -> FunPtr DuckDBV2TableFunctionFilterPushdownCallbackFn -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets the optional "partition data" callback of the table function.

The callback reports, for the batch the exec callback just produced, an ordering batch index, the values of a set of
partitioning columns, or both, depending on what a downstream operator requires; it runs on the worker thread that
produced the batch. Set @duckdb_v2_table_function_set_partitioning_callback()@ too when the function can answer
partitioning-column requests, since the engine only asks a scan for those when that callback reports
@TABLE_PARTITION_INFO_SINGLE_VALUE_PARTITIONS@.

history:
- stable: v2.0.0


@function@: The function to set the partition data callback of.

@callback@: The partition data callback to set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_set_partition_data_callback"
    c_duckdb_v2_table_function_set_partition_data_callback :: DuckDBV2TableFunctionHandle -> FunPtr DuckDBV2TableFunctionPartitionDataCallbackFn -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets the optional "partitioning" callback of the table function.

The callback tells the query optimizer, for a candidate @GROUP BY@ column set, whether every partition the scan
produces carries exactly one distinct value for those columns; only @TABLE_PARTITION_INFO_SINGLE_VALUE_PARTITIONS@
unlocks the partitioned aggregate optimization; any other result, or leaving this callback unset, keeps the regular
hash aggregate. It runs on the planning thread, before execution starts, and must be deterministic for a given column
set since it may be called on a plan the optimizer later discards. Registration fails unless
@duckdb_v2_table_function_set_partition_data_callback()@ is set too, since the engine asks that callback for the
partitioning column values once this one claims single-value partitions.

history:
- stable: v2.0.0


@function@: The function to set the partitioning callback of.

@callback@: The partitioning callback to set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_set_partitioning_callback"
    c_duckdb_v2_table_function_set_partitioning_callback :: DuckDBV2TableFunctionHandle -> FunPtr DuckDBV2TableFunctionPartitioningCallbackFn -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Declares one of the columns the function returns.

Call this once per column, in order: the columns declared here are the columns of the table the function produces,
and the vectors of the output chunk the exec callback fills follow the same order. At least one column must be
declared. The type must be a fully defined concrete type; ANY is rejected, as a result column carries data. The name
and type are borrowed and copied.

history:
- stable: v2.0.0


@info@: The bind info handle.

@name@: The name of the column. Borrowed and copied.

@type@: The type of the column. Borrowed and copied.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_bind_add_result_column"
    c_duckdb_v2_table_function_bind_add_result_column :: DuckDBV2TableFunctionBindInfoHandle -> Ptr DuckDBV2IdentifierT -> DuckDBV2LogicalTypeHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets the estimated number of rows the scan will produce.

The estimate is a hint for the optimizer, not a limit: producing a different number of rows is not an error. Without
it, the optimizer falls back on its own defaults.

history:
- stable: v2.0.0


@info@: The bind info handle.

@cardinality@: The estimated number of rows.

@is_exact@: Whether the estimate is exact, which also makes it an upper bound on the row count.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_bind_set_cardinality"
    c_duckdb_v2_table_function_bind_set_cardinality :: DuckDBV2TableFunctionBindInfoHandle -> DuckDBV2Idx -> CBool -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets the order guarantee of the rows this call of the function produces. Defaults to
@ORDER_PRESERVATION_INSERTION_ORDER@.

With @ORDER_PRESERVATION_NO_ORDER@, a scan that reports more than one thread via
@duckdb_v2_table_function_init_global_set_max_threads()@ runs in parallel into query results, INSERT and COPY without
a @duckdb_v2_table_function_set_partition_data_callback()@, and the rows arrive in no particular order. With
insertion order kept, those consumers need the partition data callback to run in parallel. Fails with
@ERROR_INPUT_INVALID@ when order is not one of the enum's declared values.

history:
- stable: v2.0.0


@info@: The bind info handle.

@order@: The order guarantee of the produced rows.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_bind_set_order_preservation"
    c_duckdb_v2_table_function_bind_set_order_preservation :: DuckDBV2TableFunctionBindInfoHandle -> DuckDBV2OrderPreservation -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the user data set via @duckdb_v2_table_function_set_user_data()@.

history:
- stable: v2.0.0


@info@: The global init info handle.

@data@: Receives the user data pointer, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_init_global_get_user_data"
    c_duckdb_v2_table_function_init_global_get_user_data :: DuckDBV2TableFunctionInitGlobalInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the bind data set by the function's bind callback.

history:
- stable: v2.0.0


@info@: The global init info handle.

@data@: Receives the bind data pointer, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_init_global_get_bind_data"
    c_duckdb_v2_table_function_init_global_get_bind_data :: DuckDBV2TableFunctionInitGlobalInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets the function's "global state" from the global init callback.

The global state lives for the duration of the scan and is retrievable from the local init, exec and progress
callbacks. Every thread scanning the function shares it, so the function must synchronize its own access to it. The
opaque handle bundles the pointer with an optional destructor, invoked when the scan is done with the state.

history:
- stable: v2.0.0


@info@: The global init info handle.

@data@: Opaque handle bundling the global state pointer plus an optional destructor.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_init_global_set_global_state"
    c_duckdb_v2_table_function_init_global_set_global_state :: DuckDBV2TableFunctionInitGlobalInfoHandle -> Ptr DuckDBV2Opaque -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets how many threads may scan the function in parallel.

Defaults to 1, a single-threaded scan. The engine creates at most this many local states, and therefore runs at most
this many exec callbacks concurrently. It is an upper bound, not a request: the engine may use fewer threads.

history:
- stable: v2.0.0


@info@: The global init info handle.

@max_threads@: The maximum number of threads. Must be at least 1.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_init_global_set_max_threads"
    c_duckdb_v2_table_function_init_global_set_max_threads :: DuckDBV2TableFunctionInitGlobalInfoHandle -> DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the number of columns the scan produces.

With projection pushdown (see @duckdb_v2_table_function_set_projection_pushdown()@) this is the number of columns the
query uses, which is the number of vectors in the exec callback's output chunk; without it, it is the number of
columns declared in bind. Valid indices for @duckdb_v2_table_function_init_global_get_column_index()@ are @0@ up to
(but excluding) this count.

history:
- stable: v2.0.0


@info@: The global init info handle.

@count@: Receives the number of columns the scan produces.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_init_global_get_column_count"
    c_duckdb_v2_table_function_init_global_get_column_count :: DuckDBV2TableFunctionInitGlobalInfoHandle -> Ptr DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns which declared column the scan's column at the given index stands for.

The result indexes the columns declared with @duckdb_v2_table_function_bind_add_result_column()@, in declaration
order: the exec callback fills the output chunk's vector at @index@ with that column's data. Without projection
pushdown the mapping is the identity. Fails if the index is out of bounds.

history:
- stable: v2.0.0


@info@: The global init info handle.

@index@: The index of the column in the scan's output.

@column_index@: Receives the index of the declared column.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_init_global_get_column_index"
    c_duckdb_v2_table_function_init_global_get_column_index :: DuckDBV2TableFunctionInitGlobalInfoHandle -> DuckDBV2Idx -> Ptr DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the user data set via @duckdb_v2_table_function_set_user_data()@.

history:
- stable: v2.0.0


@info@: The local init info handle.

@data@: Receives the user data pointer, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_init_local_get_user_data"
    c_duckdb_v2_table_function_init_local_get_user_data :: DuckDBV2TableFunctionInitLocalInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the bind data set by the function's bind callback.

history:
- stable: v2.0.0


@info@: The local init info handle.

@data@: Receives the bind data pointer, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_init_local_get_bind_data"
    c_duckdb_v2_table_function_init_local_get_bind_data :: DuckDBV2TableFunctionInitLocalInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the global state set by the function's global init callback.

Shared with every other thread scanning the function; access to it must be synchronized by the function.

history:
- stable: v2.0.0


@info@: The local init info handle.

@data@: Receives the global state pointer, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_init_local_get_global_state"
    c_duckdb_v2_table_function_init_local_get_global_state :: DuckDBV2TableFunctionInitLocalInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets the function's worker-local "local state" from the local init callback.

The local state is associated with the executing thread for the duration of the scan and retrievable from the exec
callback via @duckdb_v2_table_function_exec_get_local_state()@. No other thread observes it, so it needs no
synchronization. The opaque handle bundles the pointer with an optional destructor, invoked when the local state is
no longer needed.

history:
- stable: v2.0.0


@info@: The local init info handle.

@data@: Opaque handle bundling the local state pointer plus an optional destructor.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_init_local_set_local_state"
    c_duckdb_v2_table_function_init_local_set_local_state :: DuckDBV2TableFunctionInitLocalInfoHandle -> Ptr DuckDBV2Opaque -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the number of columns the scan produces.

With projection pushdown (see @duckdb_v2_table_function_set_projection_pushdown()@) this is the number of columns the
query uses, which is the number of vectors in the exec callback's output chunk; without it, it is the number of
columns declared in bind. Valid indices for @duckdb_v2_table_function_init_local_get_column_index()@ are @0@ up to
(but excluding) this count.

history:
- stable: v2.0.0


@info@: The local init info handle.

@count@: Receives the number of columns the scan produces.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_init_local_get_column_count"
    c_duckdb_v2_table_function_init_local_get_column_count :: DuckDBV2TableFunctionInitLocalInfoHandle -> Ptr DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns which declared column the scan's column at the given index stands for.

The result indexes the columns declared with @duckdb_v2_table_function_bind_add_result_column()@, in declaration
order: the exec callback fills the output chunk's vector at @index@ with that column's data. Without projection
pushdown the mapping is the identity. Fails if the index is out of bounds.

history:
- stable: v2.0.0


@info@: The local init info handle.

@index@: The index of the column in the scan's output.

@column_index@: Receives the index of the declared column.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_init_local_get_column_index"
    c_duckdb_v2_table_function_init_local_get_column_index :: DuckDBV2TableFunctionInitLocalInfoHandle -> DuckDBV2Idx -> Ptr DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the user data set via @duckdb_v2_table_function_set_user_data()@.

history:
- stable: v2.0.0


@info@: The exec info handle.

@data@: Receives the user data pointer, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_exec_get_user_data"
    c_duckdb_v2_table_function_exec_get_user_data :: DuckDBV2TableFunctionExecInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the bind data set by the function's bind callback.

history:
- stable: v2.0.0


@info@: The exec info handle.

@data@: Receives the bind data pointer, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_exec_get_bind_data"
    c_duckdb_v2_table_function_exec_get_bind_data :: DuckDBV2TableFunctionExecInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the global state set by the function's global init callback.

Shared with every other thread scanning the function; access to it must be synchronized by the function.

history:
- stable: v2.0.0


@info@: The exec info handle.

@data@: Receives the global state pointer, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_exec_get_global_state"
    c_duckdb_v2_table_function_exec_get_global_state :: DuckDBV2TableFunctionExecInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the worker-local local state set by the function's local init callback.

history:
- stable: v2.0.0


@info@: The exec info handle.

@data@: Receives the local state pointer for the executing thread, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_exec_get_local_state"
    c_duckdb_v2_table_function_exec_get_local_state :: DuckDBV2TableFunctionExecInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the output chunk the exec callback must write the next batch of rows into.

The chunk holds one vector per column declared with @duckdb_v2_table_function_bind_add_result_column()@, in the same
order; reach them with @duckdb_v2_data_chunk_get_vector()@. The chunk starts out empty on every invocation: write the
rows, then declare how many there are with @duckdb_v2_vector_set_size()@ on the first vector, which the engine takes
as the batch's row count and propagates to the other vectors. Producing an empty batch signals the end of the scan,
after which the callback is not invoked again on that thread. Borrowed; valid only for the duration of the callback.

history:
- stable: v2.0.0


@info@: The exec info handle.

@chunk@: Receives the borrowed output chunk to write into.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_exec_get_output_chunk"
    c_duckdb_v2_table_function_exec_get_output_chunk :: DuckDBV2TableFunctionExecInfoHandle -> Ptr DuckDBV2DataChunkHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the number of columns the scan produces.

With projection pushdown (see @duckdb_v2_table_function_set_projection_pushdown()@) this is the number of columns the
query uses, which is the number of vectors in the exec callback's output chunk; without it, it is the number of
columns declared in bind. Valid indices for @duckdb_v2_table_function_exec_get_column_index()@ are @0@ up to (but
excluding) this count.

history:
- stable: v2.0.0


@info@: The exec info handle.

@count@: Receives the number of columns the scan produces.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_exec_get_column_count"
    c_duckdb_v2_table_function_exec_get_column_count :: DuckDBV2TableFunctionExecInfoHandle -> Ptr DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns which declared column the scan's column at the given index stands for.

The result indexes the columns declared with @duckdb_v2_table_function_bind_add_result_column()@, in declaration
order: the exec callback fills the output chunk's vector at @index@ with that column's data. Without projection
pushdown the mapping is the identity. Fails if the index is out of bounds.

history:
- stable: v2.0.0


@info@: The exec info handle.

@index@: The index of the column in the scan's output.

@column_index@: Receives the index of the declared column.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_exec_get_column_index"
    c_duckdb_v2_table_function_exec_get_column_index :: DuckDBV2TableFunctionExecInfoHandle -> DuckDBV2Idx -> Ptr DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the user data set via @duckdb_v2_table_function_set_user_data()@.

history:
- stable: v2.0.0


@info@: The progress info handle.

@data@: Receives the user data pointer, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_progress_get_user_data"
    c_duckdb_v2_table_function_progress_get_user_data :: DuckDBV2TableFunctionProgressInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the bind data set by the function's bind callback.

history:
- stable: v2.0.0


@info@: The progress info handle.

@data@: Receives the bind data pointer, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_progress_get_bind_data"
    c_duckdb_v2_table_function_progress_get_bind_data :: DuckDBV2TableFunctionProgressInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the global state set by the function's global init callback.

The progress callback runs concurrently with the exec callbacks scanning the function, so it must read the global
state in a thread-safe way.

history:
- stable: v2.0.0


@info@: The progress info handle.

@data@: Receives the global state pointer, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_progress_get_global_state"
    c_duckdb_v2_table_function_progress_get_global_state :: DuckDBV2TableFunctionProgressInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Reports how far the scan has advanced.

A fraction between 0.0 (nothing scanned yet) and 1.0 (done); values outside that range are clamped. A progress
callback that returns without calling this reports no progress.

history:
- stable: v2.0.0


@info@: The progress info handle.

@progress@: The fraction of the scan that is complete, in [0.0, 1.0].

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_progress_set_progress"
    c_duckdb_v2_table_function_progress_set_progress :: DuckDBV2TableFunctionProgressInfoHandle -> CDouble -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the user data set via @duckdb_v2_table_function_set_user_data()@, or null if none was set.

history:
- stable: v2.0.0


@info@: The filter pushdown info handle.

@data@: Receives the user data pointer.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_filter_pushdown_get_user_data"
    c_duckdb_v2_table_function_filter_pushdown_get_user_data :: DuckDBV2TableFunctionFilterPushdownInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the bind data set via @duckdb_v2_function_bind_set_bind_data()@, or null if none was set.

This is the same object the init and exec callbacks later receive, so a predicate the callback accepts can be
recorded in it for the scan to apply.

history:
- stable: v2.0.0


@info@: The filter pushdown info handle.

@data@: Receives the bind data pointer.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_filter_pushdown_get_bind_data"
    c_duckdb_v2_table_function_filter_pushdown_get_bind_data :: DuckDBV2TableFunctionFilterPushdownInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the number of filter predicates offered to the function.

The predicates are combined with @AND@: every row the scan produces must satisfy all of them. Valid indices for
@duckdb_v2_table_function_filter_pushdown_get_filter()@ and @duckdb_v2_table_function_filter_pushdown_accept()@ are
@0@ up to (but excluding) this count.

history:
- stable: v2.0.0


@info@: The filter pushdown info handle.

@count@: Receives the number of predicates.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_filter_pushdown_get_filter_count"
    c_duckdb_v2_table_function_filter_pushdown_get_filter_count :: DuckDBV2TableFunctionFilterPushdownInfoHandle -> Ptr DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the filter predicate at the given index.

Fails if the index is out of bounds. The predicate is a bound expression evaluating to @BOOLEAN@; inspect it with
@duckdb_v2_expression_get_type()@ and the other @expression@ functions, and resolve the column references it contains
via @duckdb_v2_table_function_filter_pushdown_get_column_index()@. Borrowed; valid only for the duration of the
callback.

history:
- stable: v2.0.0


@info@: The filter pushdown info handle.

@index@: The index of the predicate to retrieve.

@filter@: Receives the borrowed predicate.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_filter_pushdown_get_filter"
    c_duckdb_v2_table_function_filter_pushdown_get_filter :: DuckDBV2TableFunctionFilterPushdownInfoHandle -> DuckDBV2Idx -> Ptr DuckDBV2ExpressionHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Accepts the filter predicate at the given index: the function will apply it itself.

The engine stops applying an accepted predicate, so the scan must produce only rows that satisfy it, or the query
returns rows it should not. Predicates left unaccepted are applied by the engine as usual, so a callback that
recognizes nothing can simply return. Fails if the index is out of bounds.

history:
- stable: v2.0.0


@info@: The filter pushdown info handle.

@index@: The index of the predicate to accept.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_filter_pushdown_accept"
    c_duckdb_v2_table_function_filter_pushdown_accept :: DuckDBV2TableFunctionFilterPushdownInfoHandle -> DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the number of columns the predicates can refer to.

A column reference inside a predicate (@duckdb_v2_expression_column_ref_get_index()@) indexes the columns the query
reads from the function, not the columns declared in bind. Valid indices for
@duckdb_v2_table_function_filter_pushdown_get_column_index()@ are @0@ up to (but excluding) this count.

history:
- stable: v2.0.0


@info@: The filter pushdown info handle.

@count@: Receives the number of referable columns.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_filter_pushdown_get_column_count"
    c_duckdb_v2_table_function_filter_pushdown_get_column_count :: DuckDBV2TableFunctionFilterPushdownInfoHandle -> Ptr DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Resolves a column reference found in a predicate to the declared column it refers to.

Takes the index a @EXPRESSION_TYPE_BOUND_COLUMN_REF@ node reports and returns the index of the column among the ones
declared with @duckdb_v2_table_function_bind_add_result_column()@, in declaration order. Fails if the index is out of
bounds.

history:
- stable: v2.0.0


@info@: The filter pushdown info handle.

@index@: The column index reported by a column reference node.

@column_index@: Receives the index of the declared column.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_filter_pushdown_get_column_index"
    c_duckdb_v2_table_function_filter_pushdown_get_column_index :: DuckDBV2TableFunctionFilterPushdownInfoHandle -> DuckDBV2Idx -> Ptr DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the user data set via @duckdb_v2_table_function_set_user_data()@.

history:
- stable: v2.0.0


@info@: The partition data info handle.

@data@: Receives the user data pointer, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_partition_data_get_user_data"
    c_duckdb_v2_table_function_partition_data_get_user_data :: DuckDBV2TableFunctionPartitionDataInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the bind data set by the function's bind callback.

history:
- stable: v2.0.0


@info@: The partition data info handle.

@data@: Receives the bind data pointer, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_partition_data_get_bind_data"
    c_duckdb_v2_table_function_partition_data_get_bind_data :: DuckDBV2TableFunctionPartitionDataInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the global state set by the function's global init callback.

Shared with every other thread scanning the function; access to it must be synchronized by the function.

history:
- stable: v2.0.0


@info@: The partition data info handle.

@data@: Receives the global state pointer, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_partition_data_get_global_state"
    c_duckdb_v2_table_function_partition_data_get_global_state :: DuckDBV2TableFunctionPartitionDataInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the worker-local local state set by the function's local init callback, for the thread that produced the
batch this call reports on. No other thread observes it, so it needs no synchronization.

history:
- stable: v2.0.0


@info@: The partition data info handle.

@data@: Receives the local state pointer, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_partition_data_get_local_state"
    c_duckdb_v2_table_function_partition_data_get_local_state :: DuckDBV2TableFunctionPartitionDataInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns whether a downstream operator needs this batch's ordering position.

@duckdb_v2_table_function_partition_data_set_batch_index()@ must still be called even when this is false: the engine
validates the reported value regardless of whether it is actually used for ordering, so @0@ is always a safe answer
when this reports false.

history:
- stable: v2.0.0


@info@: The partition data info handle.

@required@: Receives whether a batch index is required.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_partition_data_requires_batch_index"
    c_duckdb_v2_table_function_partition_data_requires_batch_index :: DuckDBV2TableFunctionPartitionDataInfoHandle -> Ptr CBool -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns whether a downstream operator needs this batch's values for a set of partitioning columns.

When true, call @duckdb_v2_table_function_partition_data_set_partition_value()@ once for every index in @[0, count)@,
where @count@ comes from @duckdb_v2_table_function_partition_data_get_partition_column_count()@.

history:
- stable: v2.0.0


@info@: The partition data info handle.

@required@: Receives whether partitioning column values are required.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_partition_data_requires_partition_columns"
    c_duckdb_v2_table_function_partition_data_requires_partition_columns :: DuckDBV2TableFunctionPartitionDataInfoHandle -> Ptr CBool -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the number of partitioning columns a downstream operator is requesting values for.

@0@ when @duckdb_v2_table_function_partition_data_requires_partition_columns()@ is false. Valid indices for
@duckdb_v2_table_function_partition_data_get_partition_column_index()@ and
@duckdb_v2_table_function_partition_data_set_partition_value()@ are @0@ up to (but excluding) this count.

history:
- stable: v2.0.0


@info@: The partition data info handle.

@count@: Receives the number of requested partitioning columns.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_partition_data_get_partition_column_count"
    c_duckdb_v2_table_function_partition_data_get_partition_column_count :: DuckDBV2TableFunctionPartitionDataInfoHandle -> Ptr DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns which declared column the requested partitioning column at the given index stands for.

The result indexes the columns declared with @duckdb_v2_table_function_bind_add_result_column()@, in declaration
order. Fails if the index is out of bounds.

history:
- stable: v2.0.0


@info@: The partition data info handle.

@index@: The index of the requested partitioning column.

@column_index@: Receives the index of the declared column.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_partition_data_get_partition_column_index"
    c_duckdb_v2_table_function_partition_data_get_partition_column_index :: DuckDBV2TableFunctionPartitionDataInfoHandle -> DuckDBV2Idx -> Ptr DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Reports the ordering position of the batch the exec callback just produced.

Required on every call, whether or not @duckdb_v2_table_function_partition_data_requires_batch_index()@ is true: the
engine validates the reported value regardless. Not calling this before the callback returns fails the query; @0@ is
always a safe answer when the batch index is not actually required. Must not decrease across successive calls on the
same thread, must be unique across threads for the ordering to be meaningful, must be less than roughly @10^13@, and,
when partitioning column values are also being reported, must change whenever those values change: the engine only
re-reads them when the batch index changes, so reporting a new value under an unchanged batch index is treated as a
caller error rather than applied. Fails with @ERROR_INPUT_INVALID@ for an out-of-range value; a decreasing value or
an unchanged value paired with changed partitioning columns fails the query once the callback returns.

history:
- stable: v2.0.0


@info@: The partition data info handle.

@batch_index@: The batch's ordering position.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_partition_data_set_batch_index"
    c_duckdb_v2_table_function_partition_data_set_batch_index :: DuckDBV2TableFunctionPartitionDataInfoHandle -> DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Reports the single value of the partitioning column at the given index, for the batch the exec callback just
produced.

Every partition reported this way carries exactly one distinct value for the column, so the value serves as both the
minimum and maximum of the partition's range. The value is borrowed and copied, and must be of the declared type of
the corresponding result column. Required once for every index in @[0,
@duckdb_v2_table_function_partition_data_get_partition_column_count()@)@ before the callback returns; calling it
again for the same index overwrites the previous value. The reported value only takes effect together with a changed
@duckdb_v2_table_function_partition_data_set_batch_index()@: reporting a different value under an unchanged batch
index fails the query once the callback returns. Fails with @ERROR_INPUT_INVALID@ when the index is out of bounds or
the value's type does not match.

history:
- stable: v2.0.0


@info@: The partition data info handle.

@index@: The index of the partitioning column being reported.

@value@: The single value of the partitioning column. Borrowed and copied.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_partition_data_set_partition_value"
    c_duckdb_v2_table_function_partition_data_set_partition_value :: DuckDBV2TableFunctionPartitionDataInfoHandle -> DuckDBV2Idx -> DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the user data set via @duckdb_v2_table_function_set_user_data()@.

history:
- stable: v2.0.0


@info@: The partitioning info handle.

@data@: Receives the user data pointer, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_partitioning_get_user_data"
    c_duckdb_v2_table_function_partitioning_get_user_data :: DuckDBV2TableFunctionPartitioningInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the bind data set by the function's bind callback.

history:
- stable: v2.0.0


@info@: The partitioning info handle.

@data@: Receives the bind data pointer, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_partitioning_get_bind_data"
    c_duckdb_v2_table_function_partitioning_get_bind_data :: DuckDBV2TableFunctionPartitioningInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the number of columns the optimizer is asking about: the candidate @GROUP BY@ column set.

Valid indices for @duckdb_v2_table_function_partitioning_get_partition_column_index()@ are @0@ up to (but excluding)
this count.

history:
- stable: v2.0.0


@info@: The partitioning info handle.

@count@: Receives the number of columns in the candidate set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_partitioning_get_partition_column_count"
    c_duckdb_v2_table_function_partitioning_get_partition_column_count :: DuckDBV2TableFunctionPartitioningInfoHandle -> Ptr DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns which declared column the candidate @GROUP BY@ column at the given index stands for.

The result indexes the columns declared with @duckdb_v2_table_function_bind_add_result_column()@, in declaration
order. Fails if the index is out of bounds.

history:
- stable: v2.0.0


@info@: The partitioning info handle.

@index@: The index of the candidate column.

@column_index@: Receives the index of the declared column.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_partitioning_get_partition_column_index"
    c_duckdb_v2_table_function_partitioning_get_partition_column_index :: DuckDBV2TableFunctionPartitioningInfoHandle -> DuckDBV2Idx -> Ptr DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Reports whether every partition the scan produces carries exactly one distinct value for the candidate @GROUP BY@
column set.

Only @TABLE_PARTITION_INFO_SINGLE_VALUE_PARTITIONS@ unlocks the partitioned aggregate optimization; any other value
keeps the regular hash aggregate. A callback that never calls this is treated as reporting
@TABLE_PARTITION_INFO_NOT_PARTITIONED@. Fails with @ERROR_INPUT_INVALID@ when partition_info is not one of the enum's
declared values.

history:
- stable: v2.0.0


@info@: The partitioning info handle.

@partition_info@: Whether, and how, the scan is partitioned by the candidate column set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_partitioning_set_partition_info"
    c_duckdb_v2_table_function_partitioning_set_partition_info :: DuckDBV2TableFunctionPartitioningInfoHandle -> DuckDBV2TablePartitionInfo -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Registers the table function, making it available for use in SQL queries.

The function is registered on the target given at creation: the connection's database or the loading extension.
Registration requires a name, a bind callback and an exec callback, and rejects a signature that declares a return
type, since a table function declares the columns it returns from its bind callback. The caller still owns the handle
after registration and must destroy it with @duckdb_v2_table_function_destroy()@, which does not affect the
registered function.

history:
- stable: v2.0.0


@function@: The function to register.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_register"
    c_duckdb_v2_table_function_register :: DuckDBV2TableFunctionHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Destroys the table function, releasing its resources.

Null-safe: passing a null pointer or null handle is a no-op. The handle is set to null on return to prevent
double-destruction. Destroying the handle after registration does not affect the registered function.

history:
- stable: v2.0.0


@function@: The function to destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_destroy"
    c_duckdb_v2_table_function_destroy :: Ptr DuckDBV2TableFunctionHandle -> IO DuckDBV2Error

{- | Sets the optional claim batch callback of the table function.

With a claim batch callback, a scanning thread works through the scan one batch at a time: the callback claims the
next batch of work (e.g. the next block of a file) for the thread's local state, typically from the shared global
state, and the exec callback then produces the rows of that batch only, leaving the output chunk empty once the batch
is exhausted. A batch is claimed before the exec callback first runs on a thread, and again whenever the exec
callback leaves the output chunk empty; the scan ends for a thread once the claim batch callback does not claim a
batch.

Telling the batches apart lets a caller that scans several batches in parallel put their rows back in the order of
the batches, e.g. a multi-file function registered with @duckdb_v2_multi_file_function_register()@ claims the batches
of the function itself, so that the rows of a file keep their order also when several threads scan it.

The callback runs while the work of the scan is handed out to the threads, possibly while every other scanning thread
waits for it. It should therefore only claim the work - e.g. reserve the number of the next block - and leave reading
it to the exec callback.

history:
- unstable: v2.0.0


@function@: The function to set the claim batch callback of.

@callback@: The claim batch callback to set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_set_claim_batch_callback"
    c_duckdb_v2_table_function_set_claim_batch_callback :: DuckDBV2TableFunctionHandle -> FunPtr DuckDBV2TableFunctionClaimBatchCallbackFn -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the user data set on the table function via @duckdb_v2_table_function_set_user_data()@.

history:
- unstable: v2.0.0


@info@: The claim batch info handle.

@data@: Receives the user data pointer, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_claim_batch_get_user_data"
    c_duckdb_v2_table_function_claim_batch_get_user_data :: DuckDBV2TableFunctionClaimBatchInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the bind data set by the function's bind callback.

history:
- unstable: v2.0.0


@info@: The claim batch info handle.

@data@: Receives the bind data pointer, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_claim_batch_get_bind_data"
    c_duckdb_v2_table_function_claim_batch_get_bind_data :: DuckDBV2TableFunctionClaimBatchInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the global state set by the function's global init callback.

Shared with every other thread scanning the function; access to it must be synchronized by the function.

history:
- unstable: v2.0.0


@info@: The claim batch info handle.

@data@: Receives the global state pointer, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_claim_batch_get_global_state"
    c_duckdb_v2_table_function_claim_batch_get_global_state :: DuckDBV2TableFunctionClaimBatchInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Retrieves the local state set by the function's local init callback, for the thread claiming a batch.

history:
- unstable: v2.0.0


@info@: The claim batch info handle.

@data@: Receives the local state pointer for the claiming thread, or null if none was set.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_claim_batch_get_local_state"
    c_duckdb_v2_table_function_claim_batch_get_local_state :: DuckDBV2TableFunctionClaimBatchInfoHandle -> Ptr (Ptr ()) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Reports whether the callback claimed a batch for the thread.

A callback that returns without calling this claimed nothing, which ends the scan for the thread.

history:
- unstable: v2.0.0


@info@: The claim batch info handle.

@claimed@: Whether a batch was claimed.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_claim_batch_set_claimed"
    c_duckdb_v2_table_function_claim_batch_set_claimed :: DuckDBV2TableFunctionClaimBatchInfoHandle -> CBool -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Attaches an identifier to a result column, or to a field nested inside one.

Identifiers are only consulted when the function reads a file as part of a multi-file function registered with
@duckdb_v2_multi_file_function_register()@, and are ignored otherwise. A multi-file reader can then map the columns
of every file onto the columns of the scan by identifier rather than by name - e.g. by the field ids of a file format
that carries them, so that renamed columns and fields are still found. An identifier is an INTEGER field id or a
VARCHAR name.

The nested field is addressed by a path of child indexes, starting at the result column at @column_index@: a STRUCT
field by its index, the elements of a LIST or ARRAY by @0@, the keys of a MAP by @0@ and its values by @1@, and a
UNION member by its index. An empty path addresses the result column itself. Setting an identifier again for the same
column or field replaces it. Fails with @ERROR_INPUT_INVALID@ when the column index is out of range, or the path does
not address a nested field of the column's type.

history:
- unstable: v2.0.0


@info@: The bind info handle.

@column_index@: The index of the result column, in the order the columns were declared.

@child_path@: The path of child indexes addressing the nested field, or null for the column itself.

@child_path_length@: The number of entries in the path, 0 for the column itself.

@identifier@: The identifier, an INTEGER or VARCHAR value. Borrowed and copied.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_bind_set_result_column_identifier"
    c_duckdb_v2_table_function_bind_set_result_column_identifier :: DuckDBV2TableFunctionBindInfoHandle -> DuckDBV2Idx -> Ptr DuckDBV2Idx -> DuckDBV2Idx -> DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Adds an entry to the metadata of the file the function reads.

File metadata is only consulted when the function reads a file as part of a multi-file function registered with
@duckdb_v2_multi_file_function_register()@, and is ignored otherwise: it is the key-value metadata the reader of the
file exposes to the multi-file reader, e.g. to a custom multi-file reader of another extension. Entries keep the
order in which they were added; adding a key again replaces its value.

history:
- unstable: v2.0.0


@info@: The bind info handle.

@key@: The key of the entry. Borrowed and copied.

@value@: The value of the entry. Borrowed and copied.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_table_function_bind_add_file_metadata"
    c_duckdb_v2_table_function_bind_add_file_metadata :: DuckDBV2TableFunctionBindInfoHandle -> Ptr DuckDBV2Str -> DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a new multi-file function that will be registered on the connection's database.

The function starts out empty: give it a name with @duckdb_v2_multi_file_function_set_name()@ and the single-file
function it wraps with @duckdb_v2_multi_file_function_set_single_file_function()@, then make it available with
@duckdb_v2_multi_file_function_register()@. The caller owns the returned handle and must destroy it with
@duckdb_v2_multi_file_function_destroy()@, also after registration.

history:
- unstable: v2.0.0


@connection@: The connection to create the function in.

@function@: On success, receives the newly created multi-file function. Owned by the caller.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_multi_file_function_create_with_connection"
    c_duckdb_v2_multi_file_function_create_with_connection :: DuckDBV2ConnectionHandle -> Ptr DuckDBV2MultiFileFunctionHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Creates a new multi-file function that will be registered on the loading extension's database.

Use this from an extension load callback, where an extension handle is available. The function starts out empty: give
it a name with @duckdb_v2_multi_file_function_set_name()@ and the single-file function it wraps with
@duckdb_v2_multi_file_function_set_single_file_function()@, then make it available with
@duckdb_v2_multi_file_function_register()@. The caller owns the returned handle and must destroy it with
@duckdb_v2_multi_file_function_destroy()@, also after registration.

history:
- unstable: v2.0.0


@extension@: The extension to create the function in.

@function@: On success, receives the newly created multi-file function. Owned by the caller.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_multi_file_function_create_with_extension"
    c_duckdb_v2_multi_file_function_create_with_extension :: DuckDBV2ExtensionHandle -> Ptr DuckDBV2MultiFileFunctionHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets the name of the multi-file function, as SQL will call it.

The name is borrowed and copied. Calling this again replaces the previous name. A name must be set before
registration.

history:
- unstable: v2.0.0


@function@: The function to set the name of.

@name@: The name to set. Borrowed and copied.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_multi_file_function_set_name"
    c_duckdb_v2_multi_file_function_set_name :: DuckDBV2MultiFileFunctionHandle -> Ptr DuckDBV2Str -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets the name of the table function that reads a single file, which the multi-file function wraps.

The single-file function must be registered before the multi-file function is, and must take the path of the file to
read as its only positional VARCHAR parameter. It is bound once for every file that is read, and every named
parameter it declares is also a named parameter of the multi-file function, forwarded to it as given. Everything that
involves several files - globbing, lists of files, hive partitioning, the @filename@ column, @union_by_name@ and the
like - is provided by the multi-file function on top of it. The name is borrowed and copied. A single-file function
must be set before registration.

history:
- unstable: v2.0.0


@function@: The function to set the single-file function of.

@name@: The name of the registered single-file table function. Borrowed and copied.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_multi_file_function_set_single_file_function"
    c_duckdb_v2_multi_file_function_set_single_file_function :: DuckDBV2MultiFileFunctionHandle -> Ptr DuckDBV2Str -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets how the files the function reads are referred to in messages, e.g. "Avro" or "JSON".

Optional: without it, the files are referred to by the name of the multi-file function. The name is borrowed and
copied.

history:
- unstable: v2.0.0


@function@: The function to set the reader type of.

@reader_type@: The reader type. Borrowed and copied.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_multi_file_function_set_reader_type"
    c_duckdb_v2_multi_file_function_set_reader_type :: DuckDBV2MultiFileFunctionHandle -> Ptr DuckDBV2Str -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Sets the file extension of the files the function reads, e.g. "avro".

Optional. With a file extension, a path that names a directory rather than a file reads the files with that extension
inside it. Without one, the paths passed to the function must match at least one file. The extension is given without
a leading dot, and is borrowed and copied.

history:
- unstable: v2.0.0


@function@: The function to set the file extension of.

@extension@: The file extension, without a leading dot. Borrowed and copied.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_multi_file_function_set_file_extension"
    c_duckdb_v2_multi_file_function_set_file_extension :: DuckDBV2MultiFileFunctionHandle -> Ptr DuckDBV2Str -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Registers the multi-file function, making it available for use in SQL queries.

The function is registered on the target given at creation: the connection's database or the loading extension. It
can then be called with the path of a single file, a glob pattern, or a list of either, and accepts the options every
multi-file function accepts (e.g. @filename@, @hive_partitioning@, @union_by_name@) next to the named parameters of
the single-file function. Registration requires a name and a single-file function, and fails when no table function
by that name is registered or it does not take the path of a file as its only positional VARCHAR parameter. The
caller still owns the handle after registration and must destroy it with @duckdb_v2_multi_file_function_destroy()@,
which does not affect the registered function.

history:
- unstable: v2.0.0


@function@: The function to register.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_multi_file_function_register"
    c_duckdb_v2_multi_file_function_register :: DuckDBV2MultiFileFunctionHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Destroys the multi-file function, releasing its resources.

Null-safe: passing a null pointer or null handle is a no-op. The handle is set to null on return to prevent
double-destruction. Destroying the handle after registration does not affect the registered function.

history:
- unstable: v2.0.0


@function@: The function to destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_multi_file_function_destroy"
    c_duckdb_v2_multi_file_function_destroy :: Ptr DuckDBV2MultiFileFunctionHandle -> IO DuckDBV2Error

{- | Resolves an Arrow schema into a reusable importer.

Works out every column's DuckDB logical type and the Arrow type information the conversion needs, once, so any number
of arrays of that shape can be imported without re-reading the schema. @schema@ is read, not consumed: the caller
keeps ownership and still releases it.

@batch_size@ caps the rows per produced chunk. A long array is split across several chunks. Rows left over that do
not fill a batch are held back and joined with the next array, unless the append asked to flush. Pass 0 for no
maximum: each array becomes one chunk, however long it is.

Resolving reads the catalog for extension types, so @context@ must have an active transaction. The importer keeps
using that context for every conversion and must not outlive it. Within one connection the context is the same
throughout, so an importer created in a bind callback is usable from the matching exec callback. @*out_importer@ is
set to NULL on failure.

history:
- stable: v2.0.0


@context@: The context used to resolve the Arrow types, extension types included.

@schema@: The schema to resolve. Read, not consumed; the caller keeps ownership.

@batch_size@: Maximum rows per produced chunk, or 0 for no maximum.

@out_importer@: On success, receives the new importer. Owned by the caller; destroy via
@duckdb_v2_arrow_importer_destroy()@.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_arrow_importer_create"
    c_duckdb_v2_arrow_importer_create :: DuckDBV2ContextHandle -> Ptr DuckDBV2ArrowSchema -> DuckDBV2Idx -> Ptr DuckDBV2ArrowImporterHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the resolved DuckDB schema.

Writes an owned schema handle with the DuckDB name and logical type of every column the importer resolved. This is
how a caller learns the DuckDB shape of an ArrowSchema, to declare a table function's result columns for instance,
without reimplementing the mapping from Arrow format strings to logical types.

The fields were resolved at creation, so this reads no catalog and needs no transaction. @*out_schema@ is set to NULL
on failure.

history:
- stable: v2.0.0


@importer@: The importer to read.

@out_schema@: On success, receives an owned schema. Destroy via @duckdb_v2_schema_destroy()@.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_arrow_importer_get_schema"
    c_duckdb_v2_arrow_importer_get_schema :: DuckDBV2ArrowImporterHandle -> Ptr DuckDBV2SchemaHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Gives the importer one array to convert.

Take the chunks with @duckdb_v2_arrow_importer_next_chunk()@ until that returns NULL. Appending while the previous
array still has rows left is rejected with @ERROR_INPUT_INVALID@. An array whose shape does not match the resolved
schema -- a different child count, a child whose length differs from the array's, a null or already-released child --
is rejected the same way, before anything is read.

@flush@ marks the end of the input: rows that do not fill a batch then come out as a final short chunk instead of
being held back for the next array. Pass NULL for @array@ with @flush@ set to release the held rows without supplying
more input.

@consume@ decides what happens to the caller's array.

When true, the importer takes over the array and sets its @release@ to NULL; the caller must not release it
afterwards. The produced chunks reference the Arrow buffers directly, without copying, and keep them alive, so the
chunks stay valid after the importer is destroyed. Prefer this path.

When false, the caller keeps the array and must keep it valid until the drain finishes. The produced chunks are
copies, so they do not depend on the array, at the cost of one copy per chunk.

Either way, a chunk that joins rows held back from the previous array is a copy, since it cannot reference two
arrays.

Only the default, dictionary-encoded and run-end-encoded Arrow layouts are supported. Any other layout reports
@ERROR_QUERY_NOT_IMPLEMENTED@ when the column is converted.

history:
- stable: v2.0.0


@importer@: The importer to feed.

@array@: The array to convert. Its @release@ is set to NULL when @consume@ is true.

@consume@: True to hand the array over for a zero-copy import; false to keep it, in which case every produced
chunk is a copy.

@flush@: True to mark the end of the input, releasing held rows as a final short chunk. Pass NULL for @array@
with this set to flush without supplying more input.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_arrow_importer_append"
    c_duckdb_v2_arrow_importer_append :: DuckDBV2ArrowImporterHandle -> Ptr DuckDBV2ArrowArray -> CBool -> CBool -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Produces the next chunk of the appended array, or NULL once the array is drained.

Call it in a loop until @*out_chunk@ is NULL. An importer with no array appended also returns NULL. Each chunk holds
at most the importer's @batch_size@ rows, or the whole array when that is 0. A chunk may start with rows held back
from the previous array, and rows that do not fill a batch are held back in turn unless the append asked to flush. So
NULL means the array has been read, not that all of its rows have come out.

The conversion runs under the context the importer was created with, which must still be alive. @*out_chunk@ is set
to NULL on failure.

history:
- stable: v2.0.0


@importer@: The importer to drain.

@out_chunk@: On success, receives the next chunk, or NULL once the array is drained. Destroy via
@duckdb_v2_data_chunk_destroy()@.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_arrow_importer_next_chunk"
    c_duckdb_v2_arrow_importer_next_chunk :: DuckDBV2ArrowImporterHandle -> Ptr DuckDBV2DataChunkHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Destroys an importer.

Null-safe: passing NULL, or a slot already set to NULL, is a no-op. Chunks already produced stay valid, including the
zero-copy ones, which keep the Arrow buffers alive themselves. An array appended with @consume@ true and not fully
drained is released here. Rows held back for a next array are dropped. On success the slot is set to NULL.

history:
- stable: v2.0.0


@importer@: The importer to destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_arrow_importer_destroy"
    c_duckdb_v2_arrow_importer_destroy :: Ptr DuckDBV2ArrowImporterHandle -> IO DuckDBV2Error

{- | Creates an exporter for one fixed list of columns.

Resolves the extension types and captures the session's Arrow settings, so every array this exporter produces matches
the schema @duckdb_v2_arrow_exporter_get_schema()@ reports, even if a setting changes afterwards.

@batch_size@ caps the rows per produced array. A long chunk is split across several arrays. Rows left over that do
not fill a batch are held back and joined with the next chunk, unless the append asked to flush. Pass 0 for no
maximum: each chunk becomes one array, however long it is.

@types@ and @names@ are parallel arrays of @count@ entries, borrowed and copied; they may be NULL only when @count@
is 0. Resolving reads the catalog, so @context@ must have an active transaction. @*out_exporter@ is set to NULL on
failure.

history:
- stable: v2.0.0


@context@: The context whose Arrow settings are captured and whose transaction resolves the types.

@types@: An array of @count@ column types. May be NULL only when @count@ is 0.

@names@: An array of @count@ column names, parallel to @types@. Each name must be valid UTF-8. May be NULL only
when @count@ is 0.

@count@: The number of columns, being the length of both @types@ and @names@.

@batch_size@: Maximum rows per produced array, or 0 for no maximum.

@out_exporter@: On success, receives the new exporter. Owned by the caller; destroy via
@duckdb_v2_arrow_exporter_destroy()@.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_arrow_exporter_create"
    c_duckdb_v2_arrow_exporter_create :: DuckDBV2ContextHandle -> Ptr DuckDBV2LogicalTypeHandle -> Ptr DuckDBV2Str -> DuckDBV2Idx -> DuckDBV2Idx -> Ptr DuckDBV2ArrowExporterHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the Arrow schema of the arrays this exporter produces.

Fills the caller-allocated @out_schema@. The caller owns the result and releases it with
@out_schema->release(out_schema)@. Callable at any point, and always returns the same schema, since it is built from
the settings captured at creation.

history:
- stable: v2.0.0


@exporter@: The exporter to read.

@out_schema@: Caller-allocated schema the library fills. Release it with @out_schema->release(out_schema)@.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_arrow_exporter_get_schema"
    c_duckdb_v2_arrow_exporter_get_schema :: DuckDBV2ArrowExporterHandle -> Ptr DuckDBV2ArrowSchema -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Gives the exporter one chunk to convert.

The chunk is converted in full before this returns. The conversion copies into freshly allocated Arrow buffers, so
nothing of the caller's is retained. Every array completed by this chunk becomes available from
@duckdb_v2_arrow_exporter_next_array()@; rows that do not complete a batch are held back and finished by the next
chunk. Completed arrays queue up, so appending again before they are taken is allowed.

@flush@ marks the end of the input: the held rows are then finished as a final short array. Pass NULL for @chunk@
with @flush@ set to release the held rows without supplying more input.

The chunk's types must match the ones the exporter was created with, or the call is rejected with
@ERROR_INPUT_INVALID@ before anything is read.

@consume@ decides only what happens to the caller's handle, since the data is copied either way. When true the chunk
is destroyed and the slot set to NULL, saving a @duckdb_v2_data_chunk_destroy()@ for a chunk the caller owns. When
false the chunk is left untouched, which is what a chunk borrowed from a callback needs, such as the output chunk of
a table function's exec callback, which the caller does not own and must not destroy.

history:
- stable: v2.0.0


@exporter@: The exporter to feed.

@chunk@: The chunk to convert. Destroyed and set to NULL only when @consume@ is true.

@consume@: True to hand the chunk over, destroying it; false to leave the caller's handle untouched.

@flush@: True to mark the end of the input, releasing the held rows as a final short array. Pass NULL for @chunk@
with this set to flush without supplying more input.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_arrow_exporter_append"
    c_duckdb_v2_arrow_exporter_append :: DuckDBV2ArrowExporterHandle -> Ptr DuckDBV2DataChunkHandle -> CBool -> CBool -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Takes the next completed array, or reports that none is ready.

Call it in a loop after every @duckdb_v2_arrow_exporter_append()@ until @out_array->release@ is NULL, which is how
the Arrow C Data Interface signals "no array" and what a stream's @get_next@ does at end of input. Rows held back
towards an unfinished batch are not an array yet; they come out after a further append or a flush.

Each array is owned by the caller and released with @out_array->release(out_array)@, independently of the exporter
and of every other array.

history:
- stable: v2.0.0


@exporter@: The exporter to drain.

@out_array@: Caller-allocated array the library fills. Left released -- @release@ NULL -- when none is ready.
Release a filled one with @out_array->release(out_array)@.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_arrow_exporter_next_array"
    c_duckdb_v2_arrow_exporter_next_array :: DuckDBV2ArrowExporterHandle -> Ptr DuckDBV2ArrowArray -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Destroys an exporter.

Null-safe: passing NULL, or a slot already set to NULL, is a no-op. Arrays already taken stay valid. Arrays still
queued inside are released here, and rows held back towards an unfinished batch are dropped. On success the slot is
set to NULL.

history:
- stable: v2.0.0


@exporter@: The exporter to destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_arrow_exporter_destroy"
    c_duckdb_v2_arrow_exporter_destroy :: Ptr DuckDBV2ArrowExporterHandle -> IO DuckDBV2Error

{- | Executes a parsed statement. The result produces Arrow arrays.

Works like @duckdb_v2_statement_execute()@. @*out_result@ is set to NULL on failure.

history:
- stable: v2.0.0


@conn@: The connection to run the statement on.

@statement@: The statement to execute. Not consumed.

@parameter_names@: Optional. As in @duckdb_v2_statement_execute()@.

@parameter_values@: Optional. As in @duckdb_v2_statement_execute()@.

@parameter_count@: The number of parameters. 0 for none.

@batch_size@: Maximum rows per array. Arrays can be shorter. 0 means 131072.

@out_result@: Receives the new result. Destroy via @duckdb_v2_arrow_result_destroy()@.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_statement_execute_arrow"
    c_duckdb_v2_statement_execute_arrow :: DuckDBV2ConnectionHandle -> DuckDBV2SqlStatementHandle -> Ptr DuckDBV2IdentifierT -> Ptr DuckDBV2ValueHandle -> DuckDBV2Idx -> DuckDBV2Idx -> Ptr DuckDBV2ArrowResultHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Executes a prepared statement. The result produces Arrow arrays.

Works like @duckdb_v2_prepared_statement_execute()@. @*out_result@ is set to NULL on failure.

history:
- stable: v2.0.0


@prepared@: The prepared statement to execute. Not consumed.

@parameter_names@: Optional. As in @duckdb_v2_prepared_statement_execute()@.

@parameter_values@: Optional. As in @duckdb_v2_prepared_statement_execute()@.

@parameter_count@: The number of parameters. 0 for none.

@batch_size@: Maximum rows per array. Arrays can be shorter. 0 means 131072.

@out_result@: Receives the new result. Destroy via @duckdb_v2_arrow_result_destroy()@.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_prepared_statement_execute_arrow"
    c_duckdb_v2_prepared_statement_execute_arrow :: DuckDBV2PreparedStatementHandle -> Ptr DuckDBV2IdentifierT -> Ptr DuckDBV2ValueHandle -> DuckDBV2Idx -> DuckDBV2Idx -> Ptr DuckDBV2ArrowResultHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Destroys the result. Works like @duckdb_v2_result_destroy()@.

history:
- stable: v2.0.0


@result@: The result to destroy.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_arrow_result_destroy"
    c_duckdb_v2_arrow_result_destroy :: Ptr DuckDBV2ArrowResultHandle -> IO DuckDBV2Error

{- | Runs a bounded amount of the query and returns without blocking.

Works like @duckdb_v2_result_step()@. On status @CHUNK@, @out_array@ holds the next array. Otherwise its @release@ is
NULL.

history:
- stable: v2.0.0


@result@: The result to step.

@out_array@: Caller-allocated array the library fills. Release it with @out_array->release(out_array)@.

@out_status@: Receives the step status.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_arrow_result_step"
    c_duckdb_v2_arrow_result_step :: DuckDBV2ArrowResultHandle -> Ptr DuckDBV2ArrowArray -> Ptr DuckDBV2ResultStepStatus -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Blocks until the next array is ready.

Works like @duckdb_v2_result_fetch_chunk()@. At the end of the result, @out_array->release@ is NULL.

history:
- stable: v2.0.0


@result@: The result to fetch from.

@out_array@: Caller-allocated array the library fills. Release it with @out_array->release(out_array)@.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_arrow_result_fetch_array"
    c_duckdb_v2_arrow_result_fetch_array :: DuckDBV2ArrowResultHandle -> Ptr DuckDBV2ArrowArray -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Blocks until @duckdb_v2_arrow_result_step()@ can make progress. Works like @duckdb_v2_result_wait()@.

history:
- stable: v2.0.0


@result@: The result to wait on.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_arrow_result_wait"
    c_duckdb_v2_arrow_result_wait :: DuckDBV2ArrowResultHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Runs the query to the end and reports the changed-row count. Works like @duckdb_v2_result_drain()@.

history:
- stable: v2.0.0


@result@: The result to drain.

@out_rows_changed@: Receives the changed-row count for an INSERT, UPDATE or DELETE, 0 otherwise.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_arrow_result_drain"
    c_duckdb_v2_arrow_result_drain :: DuckDBV2ArrowResultHandle -> Ptr DuckDBV2Idx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the shape of the result. Works like @duckdb_v2_result_get_result_type()@.

history:
- stable: v2.0.0


@result@: The result.

@out_type@: Receives the result shape.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_arrow_result_get_result_type"
    c_duckdb_v2_arrow_result_get_result_type :: DuckDBV2ArrowResultHandle -> Ptr DuckDBV2ResultType -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the type of statement that produced the result. Works like @duckdb_v2_result_get_statement_type()@.

history:
- stable: v2.0.0


@result@: The result.

@out_type@: Receives the statement type.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_arrow_result_get_statement_type"
    c_duckdb_v2_arrow_result_get_statement_type :: DuckDBV2ArrowResultHandle -> Ptr DuckDBV2StatementType -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Returns the Arrow schema of the result's arrays.

Available whenever @duckdb_v2_result_get_schema()@ would be.

history:
- stable: v2.0.0


@result@: The result.

@out_schema@: Caller-allocated schema the library fills. Release it with @out_schema->release(out_schema)@.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_arrow_result_get_schema"
    c_duckdb_v2_arrow_result_get_schema :: DuckDBV2ArrowResultHandle -> Ptr DuckDBV2ArrowSchema -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Turns the result into an ArrowArrayStream.

On success the stream takes over the result and @*result@ is set to NULL. Each @get_next@ returns the next array.
Arrays already fetched are not repeated. Releasing the stream destroys the result.

Converting runs no part of the query. The stream's @get_schema@ can: for a statement that expands into several
statements, such as PIVOT, it runs the ones before the statement that returns rows. A failing @get_next@ or
@get_schema@ returns @EIO@; @get_last_error@ has the message.

history:
- stable: v2.0.0


@result@: The result to take over. Set to NULL on success.

@out_stream@: Caller-allocated stream the library fills. Release it with @out_stream->release(out_stream)@.

@err@: Optional. On failure, receives an opaque info handle the caller must destroy via
@duckdb_v2_error_info_destroy()@.

Returns DUCKDB_V2_ERROR
-}
foreign import ccall safe "duckdb_v2_arrow_result_to_arrow_c_stream"
    c_duckdb_v2_arrow_result_to_arrow_c_stream :: Ptr DuckDBV2ArrowResultHandle -> Ptr DuckDBV2ArrowArrayStream -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

{- | Create a function pointer for 'DuckDBV2TextSinkFn'.

Keep the pointer alive while DuckDB can invoke it. Free the pointer with
@freeHaskellFunPtr@ after its final possible invocation.
The callback must not throw an exception across the C boundary.
-}
foreign import ccall "wrapper"
    mkDuckDBV2TextSinkFn :: DuckDBV2TextSinkFn -> IO (FunPtr DuckDBV2TextSinkFn)

-- | Call a function pointer with the 'DuckDBV2TextSinkFn' signature.
foreign import ccall safe "dynamic"
    callDuckDBV2TextSinkFn :: FunPtr DuckDBV2TextSinkFn -> Ptr DuckDBV2Str -> Ptr () -> Ptr DuckDBV2ErrorInfoHandle -> IO ()

{- | Create a function pointer for 'DuckDBV2OpaqueEqualsFn'.

Keep the pointer alive while DuckDB can invoke it. Free the pointer with
@freeHaskellFunPtr@ after its final possible invocation.
The callback must not throw an exception across the C boundary.
-}
foreign import ccall "wrapper"
    mkDuckDBV2OpaqueEqualsFn :: DuckDBV2OpaqueEqualsFn -> IO (FunPtr DuckDBV2OpaqueEqualsFn)

-- | Call a function pointer with the 'DuckDBV2OpaqueEqualsFn' signature.
foreign import ccall safe "dynamic"
    callDuckDBV2OpaqueEqualsFn :: FunPtr DuckDBV2OpaqueEqualsFn -> Ptr () -> Ptr () -> IO CBool

{- | Create a function pointer for 'DuckDBV2OpaqueDestroyFn'.

Keep the pointer alive while DuckDB can invoke it. Free the pointer with
@freeHaskellFunPtr@ after its final possible invocation.
The callback must not throw an exception across the C boundary.
-}
foreign import ccall "wrapper"
    mkDuckDBV2OpaqueDestroyFn :: DuckDBV2OpaqueDestroyFn -> IO (FunPtr DuckDBV2OpaqueDestroyFn)

-- | Call a function pointer with the 'DuckDBV2OpaqueDestroyFn' signature.
foreign import ccall safe "dynamic"
    callDuckDBV2OpaqueDestroyFn :: FunPtr DuckDBV2OpaqueDestroyFn -> Ptr () -> IO ()

{- | Create a function pointer for 'DuckDBV2CastFunctionExecCallbackFn'.

Keep the pointer alive while DuckDB can invoke it. Free the pointer with
@freeHaskellFunPtr@ after its final possible invocation.
The callback must not throw an exception across the C boundary.
-}
foreign import ccall "wrapper"
    mkDuckDBV2CastFunctionExecCallbackFn :: DuckDBV2CastFunctionExecCallbackFn -> IO (FunPtr DuckDBV2CastFunctionExecCallbackFn)

-- | Call a function pointer with the 'DuckDBV2CastFunctionExecCallbackFn' signature.
foreign import ccall safe "dynamic"
    callDuckDBV2CastFunctionExecCallbackFn :: FunPtr DuckDBV2CastFunctionExecCallbackFn -> DuckDBV2CastFunctionExecInfoHandle -> DuckDBV2ContextHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()

{- | Create a function pointer for 'DuckDBV2ExtensionGetApiFn'.

Keep the pointer alive while DuckDB can invoke it. Free the pointer with
@freeHaskellFunPtr@ after its final possible invocation.
The callback must not throw an exception across the C boundary.
-}
foreign import ccall "wrapper"
    mkDuckDBV2ExtensionGetApiFn :: DuckDBV2ExtensionGetApiFn -> IO (FunPtr DuckDBV2ExtensionGetApiFn)

-- | Call a function pointer with the 'DuckDBV2ExtensionGetApiFn' signature.
foreign import ccall safe "dynamic"
    callDuckDBV2ExtensionGetApiFn :: FunPtr DuckDBV2ExtensionGetApiFn -> DuckDBV2ExtensionHandle -> Ptr CChar -> IO (Ptr ())

{- | Create a function pointer for 'DuckDBV2AggregateFunctionBindCallbackFn'.

Keep the pointer alive while DuckDB can invoke it. Free the pointer with
@freeHaskellFunPtr@ after its final possible invocation.
The callback must not throw an exception across the C boundary.
-}
foreign import ccall "wrapper"
    mkDuckDBV2AggregateFunctionBindCallbackFn :: DuckDBV2AggregateFunctionBindCallbackFn -> IO (FunPtr DuckDBV2AggregateFunctionBindCallbackFn)

-- | Call a function pointer with the 'DuckDBV2AggregateFunctionBindCallbackFn' signature.
foreign import ccall safe "dynamic"
    callDuckDBV2AggregateFunctionBindCallbackFn :: FunPtr DuckDBV2AggregateFunctionBindCallbackFn -> DuckDBV2FunctionBindInfoHandle -> DuckDBV2AggregateFunctionBindInfoHandle -> DuckDBV2ContextHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()

{- | Create a function pointer for 'DuckDBV2AggregateFunctionSizeCallbackFn'.

Keep the pointer alive while DuckDB can invoke it. Free the pointer with
@freeHaskellFunPtr@ after its final possible invocation.
The callback must not throw an exception across the C boundary.
-}
foreign import ccall "wrapper"
    mkDuckDBV2AggregateFunctionSizeCallbackFn :: DuckDBV2AggregateFunctionSizeCallbackFn -> IO (FunPtr DuckDBV2AggregateFunctionSizeCallbackFn)

-- | Call a function pointer with the 'DuckDBV2AggregateFunctionSizeCallbackFn' signature.
foreign import ccall safe "dynamic"
    callDuckDBV2AggregateFunctionSizeCallbackFn :: FunPtr DuckDBV2AggregateFunctionSizeCallbackFn -> DuckDBV2AggregateFunctionSizeInfoHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()

{- | Create a function pointer for 'DuckDBV2AggregateFunctionInitCallbackFn'.

Keep the pointer alive while DuckDB can invoke it. Free the pointer with
@freeHaskellFunPtr@ after its final possible invocation.
The callback must not throw an exception across the C boundary.
-}
foreign import ccall "wrapper"
    mkDuckDBV2AggregateFunctionInitCallbackFn :: DuckDBV2AggregateFunctionInitCallbackFn -> IO (FunPtr DuckDBV2AggregateFunctionInitCallbackFn)

-- | Call a function pointer with the 'DuckDBV2AggregateFunctionInitCallbackFn' signature.
foreign import ccall safe "dynamic"
    callDuckDBV2AggregateFunctionInitCallbackFn :: FunPtr DuckDBV2AggregateFunctionInitCallbackFn -> DuckDBV2AggregateFunctionInitInfoHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()

{- | Create a function pointer for 'DuckDBV2AggregateFunctionUpdateCallbackFn'.

Keep the pointer alive while DuckDB can invoke it. Free the pointer with
@freeHaskellFunPtr@ after its final possible invocation.
The callback must not throw an exception across the C boundary.
-}
foreign import ccall "wrapper"
    mkDuckDBV2AggregateFunctionUpdateCallbackFn :: DuckDBV2AggregateFunctionUpdateCallbackFn -> IO (FunPtr DuckDBV2AggregateFunctionUpdateCallbackFn)

-- | Call a function pointer with the 'DuckDBV2AggregateFunctionUpdateCallbackFn' signature.
foreign import ccall safe "dynamic"
    callDuckDBV2AggregateFunctionUpdateCallbackFn :: FunPtr DuckDBV2AggregateFunctionUpdateCallbackFn -> DuckDBV2AggregateFunctionUpdateInfoHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()

{- | Create a function pointer for 'DuckDBV2AggregateFunctionCombineCallbackFn'.

Keep the pointer alive while DuckDB can invoke it. Free the pointer with
@freeHaskellFunPtr@ after its final possible invocation.
The callback must not throw an exception across the C boundary.
-}
foreign import ccall "wrapper"
    mkDuckDBV2AggregateFunctionCombineCallbackFn :: DuckDBV2AggregateFunctionCombineCallbackFn -> IO (FunPtr DuckDBV2AggregateFunctionCombineCallbackFn)

-- | Call a function pointer with the 'DuckDBV2AggregateFunctionCombineCallbackFn' signature.
foreign import ccall safe "dynamic"
    callDuckDBV2AggregateFunctionCombineCallbackFn :: FunPtr DuckDBV2AggregateFunctionCombineCallbackFn -> DuckDBV2AggregateFunctionCombineInfoHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()

{- | Create a function pointer for 'DuckDBV2AggregateFunctionFinalizeCallbackFn'.

Keep the pointer alive while DuckDB can invoke it. Free the pointer with
@freeHaskellFunPtr@ after its final possible invocation.
The callback must not throw an exception across the C boundary.
-}
foreign import ccall "wrapper"
    mkDuckDBV2AggregateFunctionFinalizeCallbackFn :: DuckDBV2AggregateFunctionFinalizeCallbackFn -> IO (FunPtr DuckDBV2AggregateFunctionFinalizeCallbackFn)

-- | Call a function pointer with the 'DuckDBV2AggregateFunctionFinalizeCallbackFn' signature.
foreign import ccall safe "dynamic"
    callDuckDBV2AggregateFunctionFinalizeCallbackFn :: FunPtr DuckDBV2AggregateFunctionFinalizeCallbackFn -> DuckDBV2AggregateFunctionFinalizeInfoHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()

{- | Create a function pointer for 'DuckDBV2AggregateFunctionDestroyCallbackFn'.

Keep the pointer alive while DuckDB can invoke it. Free the pointer with
@freeHaskellFunPtr@ after its final possible invocation.
The callback must not throw an exception across the C boundary.
-}
foreign import ccall "wrapper"
    mkDuckDBV2AggregateFunctionDestroyCallbackFn :: DuckDBV2AggregateFunctionDestroyCallbackFn -> IO (FunPtr DuckDBV2AggregateFunctionDestroyCallbackFn)

-- | Call a function pointer with the 'DuckDBV2AggregateFunctionDestroyCallbackFn' signature.
foreign import ccall safe "dynamic"
    callDuckDBV2AggregateFunctionDestroyCallbackFn :: FunPtr DuckDBV2AggregateFunctionDestroyCallbackFn -> DuckDBV2AggregateFunctionDestroyInfoHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()

{- | Create a function pointer for 'DuckDBV2CopyToBindCallbackFn'.

Keep the pointer alive while DuckDB can invoke it. Free the pointer with
@freeHaskellFunPtr@ after its final possible invocation.
The callback must not throw an exception across the C boundary.
-}
foreign import ccall "wrapper"
    mkDuckDBV2CopyToBindCallbackFn :: DuckDBV2CopyToBindCallbackFn -> IO (FunPtr DuckDBV2CopyToBindCallbackFn)

-- | Call a function pointer with the 'DuckDBV2CopyToBindCallbackFn' signature.
foreign import ccall safe "dynamic"
    callDuckDBV2CopyToBindCallbackFn :: FunPtr DuckDBV2CopyToBindCallbackFn -> DuckDBV2CopyToBindInfoHandle -> DuckDBV2ContextHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()

{- | Create a function pointer for 'DuckDBV2CopyToBatchSizeCallbackFn'.

Keep the pointer alive while DuckDB can invoke it. Free the pointer with
@freeHaskellFunPtr@ after its final possible invocation.
The callback must not throw an exception across the C boundary.
-}
foreign import ccall "wrapper"
    mkDuckDBV2CopyToBatchSizeCallbackFn :: DuckDBV2CopyToBatchSizeCallbackFn -> IO (FunPtr DuckDBV2CopyToBatchSizeCallbackFn)

-- | Call a function pointer with the 'DuckDBV2CopyToBatchSizeCallbackFn' signature.
foreign import ccall safe "dynamic"
    callDuckDBV2CopyToBatchSizeCallbackFn :: FunPtr DuckDBV2CopyToBatchSizeCallbackFn -> DuckDBV2CopyToBatchSizeInfoHandle -> DuckDBV2ContextHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()

{- | Create a function pointer for 'DuckDBV2CopyToInitCallbackFn'.

Keep the pointer alive while DuckDB can invoke it. Free the pointer with
@freeHaskellFunPtr@ after its final possible invocation.
The callback must not throw an exception across the C boundary.
-}
foreign import ccall "wrapper"
    mkDuckDBV2CopyToInitCallbackFn :: DuckDBV2CopyToInitCallbackFn -> IO (FunPtr DuckDBV2CopyToInitCallbackFn)

-- | Call a function pointer with the 'DuckDBV2CopyToInitCallbackFn' signature.
foreign import ccall safe "dynamic"
    callDuckDBV2CopyToInitCallbackFn :: FunPtr DuckDBV2CopyToInitCallbackFn -> DuckDBV2CopyToInitInfoHandle -> DuckDBV2ContextHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()

{- | Create a function pointer for 'DuckDBV2CopyToBatchCallbackFn'.

Keep the pointer alive while DuckDB can invoke it. Free the pointer with
@freeHaskellFunPtr@ after its final possible invocation.
The callback must not throw an exception across the C boundary.
-}
foreign import ccall "wrapper"
    mkDuckDBV2CopyToBatchCallbackFn :: DuckDBV2CopyToBatchCallbackFn -> IO (FunPtr DuckDBV2CopyToBatchCallbackFn)

-- | Call a function pointer with the 'DuckDBV2CopyToBatchCallbackFn' signature.
foreign import ccall safe "dynamic"
    callDuckDBV2CopyToBatchCallbackFn :: FunPtr DuckDBV2CopyToBatchCallbackFn -> DuckDBV2CopyToBatchInfoHandle -> DuckDBV2ContextHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()

{- | Create a function pointer for 'DuckDBV2CopyToFlushCallbackFn'.

Keep the pointer alive while DuckDB can invoke it. Free the pointer with
@freeHaskellFunPtr@ after its final possible invocation.
The callback must not throw an exception across the C boundary.
-}
foreign import ccall "wrapper"
    mkDuckDBV2CopyToFlushCallbackFn :: DuckDBV2CopyToFlushCallbackFn -> IO (FunPtr DuckDBV2CopyToFlushCallbackFn)

-- | Call a function pointer with the 'DuckDBV2CopyToFlushCallbackFn' signature.
foreign import ccall safe "dynamic"
    callDuckDBV2CopyToFlushCallbackFn :: FunPtr DuckDBV2CopyToFlushCallbackFn -> DuckDBV2CopyToFlushInfoHandle -> DuckDBV2ContextHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()

{- | Create a function pointer for 'DuckDBV2CopyToFinalizeCallbackFn'.

Keep the pointer alive while DuckDB can invoke it. Free the pointer with
@freeHaskellFunPtr@ after its final possible invocation.
The callback must not throw an exception across the C boundary.
-}
foreign import ccall "wrapper"
    mkDuckDBV2CopyToFinalizeCallbackFn :: DuckDBV2CopyToFinalizeCallbackFn -> IO (FunPtr DuckDBV2CopyToFinalizeCallbackFn)

-- | Call a function pointer with the 'DuckDBV2CopyToFinalizeCallbackFn' signature.
foreign import ccall safe "dynamic"
    callDuckDBV2CopyToFinalizeCallbackFn :: FunPtr DuckDBV2CopyToFinalizeCallbackFn -> DuckDBV2CopyToFinalizeInfoHandle -> DuckDBV2ContextHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()

{- | Create a function pointer for 'DuckDBV2CopyToStatisticsCallbackFn'.

Keep the pointer alive while DuckDB can invoke it. Free the pointer with
@freeHaskellFunPtr@ after its final possible invocation.
The callback must not throw an exception across the C boundary.
-}
foreign import ccall "wrapper"
    mkDuckDBV2CopyToStatisticsCallbackFn :: DuckDBV2CopyToStatisticsCallbackFn -> IO (FunPtr DuckDBV2CopyToStatisticsCallbackFn)

-- | Call a function pointer with the 'DuckDBV2CopyToStatisticsCallbackFn' signature.
foreign import ccall safe "dynamic"
    callDuckDBV2CopyToStatisticsCallbackFn :: FunPtr DuckDBV2CopyToStatisticsCallbackFn -> DuckDBV2CopyToStatisticsInfoHandle -> DuckDBV2ContextHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()

{- | Create a function pointer for 'DuckDBV2CopyFromBindCallbackFn'.

Keep the pointer alive while DuckDB can invoke it. Free the pointer with
@freeHaskellFunPtr@ after its final possible invocation.
The callback must not throw an exception across the C boundary.
-}
foreign import ccall "wrapper"
    mkDuckDBV2CopyFromBindCallbackFn :: DuckDBV2CopyFromBindCallbackFn -> IO (FunPtr DuckDBV2CopyFromBindCallbackFn)

-- | Call a function pointer with the 'DuckDBV2CopyFromBindCallbackFn' signature.
foreign import ccall safe "dynamic"
    callDuckDBV2CopyFromBindCallbackFn :: FunPtr DuckDBV2CopyFromBindCallbackFn -> DuckDBV2CopyFromBindInfoHandle -> DuckDBV2ContextHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()

{- | Create a function pointer for 'DuckDBV2CopyFromInitGlobalCallbackFn'.

Keep the pointer alive while DuckDB can invoke it. Free the pointer with
@freeHaskellFunPtr@ after its final possible invocation.
The callback must not throw an exception across the C boundary.
-}
foreign import ccall "wrapper"
    mkDuckDBV2CopyFromInitGlobalCallbackFn :: DuckDBV2CopyFromInitGlobalCallbackFn -> IO (FunPtr DuckDBV2CopyFromInitGlobalCallbackFn)

-- | Call a function pointer with the 'DuckDBV2CopyFromInitGlobalCallbackFn' signature.
foreign import ccall safe "dynamic"
    callDuckDBV2CopyFromInitGlobalCallbackFn :: FunPtr DuckDBV2CopyFromInitGlobalCallbackFn -> DuckDBV2CopyFromInitGlobalInfoHandle -> DuckDBV2ContextHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()

{- | Create a function pointer for 'DuckDBV2CopyFromInitLocalCallbackFn'.

Keep the pointer alive while DuckDB can invoke it. Free the pointer with
@freeHaskellFunPtr@ after its final possible invocation.
The callback must not throw an exception across the C boundary.
-}
foreign import ccall "wrapper"
    mkDuckDBV2CopyFromInitLocalCallbackFn :: DuckDBV2CopyFromInitLocalCallbackFn -> IO (FunPtr DuckDBV2CopyFromInitLocalCallbackFn)

-- | Call a function pointer with the 'DuckDBV2CopyFromInitLocalCallbackFn' signature.
foreign import ccall safe "dynamic"
    callDuckDBV2CopyFromInitLocalCallbackFn :: FunPtr DuckDBV2CopyFromInitLocalCallbackFn -> DuckDBV2CopyFromInitLocalInfoHandle -> DuckDBV2ContextHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()

{- | Create a function pointer for 'DuckDBV2CopyFromExecCallbackFn'.

Keep the pointer alive while DuckDB can invoke it. Free the pointer with
@freeHaskellFunPtr@ after its final possible invocation.
The callback must not throw an exception across the C boundary.
-}
foreign import ccall "wrapper"
    mkDuckDBV2CopyFromExecCallbackFn :: DuckDBV2CopyFromExecCallbackFn -> IO (FunPtr DuckDBV2CopyFromExecCallbackFn)

-- | Call a function pointer with the 'DuckDBV2CopyFromExecCallbackFn' signature.
foreign import ccall safe "dynamic"
    callDuckDBV2CopyFromExecCallbackFn :: FunPtr DuckDBV2CopyFromExecCallbackFn -> DuckDBV2CopyFromExecInfoHandle -> DuckDBV2ContextHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()

{- | Create a function pointer for 'DuckDBV2CopyFromProgressCallbackFn'.

Keep the pointer alive while DuckDB can invoke it. Free the pointer with
@freeHaskellFunPtr@ after its final possible invocation.
The callback must not throw an exception across the C boundary.
-}
foreign import ccall "wrapper"
    mkDuckDBV2CopyFromProgressCallbackFn :: DuckDBV2CopyFromProgressCallbackFn -> IO (FunPtr DuckDBV2CopyFromProgressCallbackFn)

-- | Call a function pointer with the 'DuckDBV2CopyFromProgressCallbackFn' signature.
foreign import ccall safe "dynamic"
    callDuckDBV2CopyFromProgressCallbackFn :: FunPtr DuckDBV2CopyFromProgressCallbackFn -> DuckDBV2CopyFromProgressInfoHandle -> DuckDBV2ContextHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()

{- | Create a function pointer for 'DuckDBV2ReplacementScanCallbackFn'.

Keep the pointer alive while DuckDB can invoke it. Free the pointer with
@freeHaskellFunPtr@ after its final possible invocation.
The callback must not throw an exception across the C boundary.
-}
foreign import ccall "wrapper"
    mkDuckDBV2ReplacementScanCallbackFn :: DuckDBV2ReplacementScanCallbackFn -> IO (FunPtr DuckDBV2ReplacementScanCallbackFn)

-- | Call a function pointer with the 'DuckDBV2ReplacementScanCallbackFn' signature.
foreign import ccall safe "dynamic"
    callDuckDBV2ReplacementScanCallbackFn :: FunPtr DuckDBV2ReplacementScanCallbackFn -> DuckDBV2ReplacementScanInfoHandle -> DuckDBV2ContextHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()

{- | Create a function pointer for 'DuckDBV2ScalarFunctionBindCallbackFn'.

Keep the pointer alive while DuckDB can invoke it. Free the pointer with
@freeHaskellFunPtr@ after its final possible invocation.
The callback must not throw an exception across the C boundary.
-}
foreign import ccall "wrapper"
    mkDuckDBV2ScalarFunctionBindCallbackFn :: DuckDBV2ScalarFunctionBindCallbackFn -> IO (FunPtr DuckDBV2ScalarFunctionBindCallbackFn)

-- | Call a function pointer with the 'DuckDBV2ScalarFunctionBindCallbackFn' signature.
foreign import ccall safe "dynamic"
    callDuckDBV2ScalarFunctionBindCallbackFn :: FunPtr DuckDBV2ScalarFunctionBindCallbackFn -> DuckDBV2FunctionBindInfoHandle -> DuckDBV2ScalarFunctionBindInfoHandle -> DuckDBV2ContextHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()

{- | Create a function pointer for 'DuckDBV2ScalarFunctionInitCallbackFn'.

Keep the pointer alive while DuckDB can invoke it. Free the pointer with
@freeHaskellFunPtr@ after its final possible invocation.
The callback must not throw an exception across the C boundary.
-}
foreign import ccall "wrapper"
    mkDuckDBV2ScalarFunctionInitCallbackFn :: DuckDBV2ScalarFunctionInitCallbackFn -> IO (FunPtr DuckDBV2ScalarFunctionInitCallbackFn)

-- | Call a function pointer with the 'DuckDBV2ScalarFunctionInitCallbackFn' signature.
foreign import ccall safe "dynamic"
    callDuckDBV2ScalarFunctionInitCallbackFn :: FunPtr DuckDBV2ScalarFunctionInitCallbackFn -> DuckDBV2ScalarFunctionInitInfoHandle -> DuckDBV2ContextHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()

{- | Create a function pointer for 'DuckDBV2ScalarFunctionExecCallbackFn'.

Keep the pointer alive while DuckDB can invoke it. Free the pointer with
@freeHaskellFunPtr@ after its final possible invocation.
The callback must not throw an exception across the C boundary.
-}
foreign import ccall "wrapper"
    mkDuckDBV2ScalarFunctionExecCallbackFn :: DuckDBV2ScalarFunctionExecCallbackFn -> IO (FunPtr DuckDBV2ScalarFunctionExecCallbackFn)

-- | Call a function pointer with the 'DuckDBV2ScalarFunctionExecCallbackFn' signature.
foreign import ccall safe "dynamic"
    callDuckDBV2ScalarFunctionExecCallbackFn :: FunPtr DuckDBV2ScalarFunctionExecCallbackFn -> DuckDBV2ScalarFunctionExecInfoHandle -> DuckDBV2ContextHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()

{- | Create a function pointer for 'DuckDBV2TableFunctionBindCallbackFn'.

Keep the pointer alive while DuckDB can invoke it. Free the pointer with
@freeHaskellFunPtr@ after its final possible invocation.
The callback must not throw an exception across the C boundary.
-}
foreign import ccall "wrapper"
    mkDuckDBV2TableFunctionBindCallbackFn :: DuckDBV2TableFunctionBindCallbackFn -> IO (FunPtr DuckDBV2TableFunctionBindCallbackFn)

-- | Call a function pointer with the 'DuckDBV2TableFunctionBindCallbackFn' signature.
foreign import ccall safe "dynamic"
    callDuckDBV2TableFunctionBindCallbackFn :: FunPtr DuckDBV2TableFunctionBindCallbackFn -> DuckDBV2FunctionBindInfoHandle -> DuckDBV2TableFunctionBindInfoHandle -> DuckDBV2ContextHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()

{- | Create a function pointer for 'DuckDBV2TableFunctionInitGlobalCallbackFn'.

Keep the pointer alive while DuckDB can invoke it. Free the pointer with
@freeHaskellFunPtr@ after its final possible invocation.
The callback must not throw an exception across the C boundary.
-}
foreign import ccall "wrapper"
    mkDuckDBV2TableFunctionInitGlobalCallbackFn :: DuckDBV2TableFunctionInitGlobalCallbackFn -> IO (FunPtr DuckDBV2TableFunctionInitGlobalCallbackFn)

-- | Call a function pointer with the 'DuckDBV2TableFunctionInitGlobalCallbackFn' signature.
foreign import ccall safe "dynamic"
    callDuckDBV2TableFunctionInitGlobalCallbackFn :: FunPtr DuckDBV2TableFunctionInitGlobalCallbackFn -> DuckDBV2TableFunctionInitGlobalInfoHandle -> DuckDBV2ContextHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()

{- | Create a function pointer for 'DuckDBV2TableFunctionInitLocalCallbackFn'.

Keep the pointer alive while DuckDB can invoke it. Free the pointer with
@freeHaskellFunPtr@ after its final possible invocation.
The callback must not throw an exception across the C boundary.
-}
foreign import ccall "wrapper"
    mkDuckDBV2TableFunctionInitLocalCallbackFn :: DuckDBV2TableFunctionInitLocalCallbackFn -> IO (FunPtr DuckDBV2TableFunctionInitLocalCallbackFn)

-- | Call a function pointer with the 'DuckDBV2TableFunctionInitLocalCallbackFn' signature.
foreign import ccall safe "dynamic"
    callDuckDBV2TableFunctionInitLocalCallbackFn :: FunPtr DuckDBV2TableFunctionInitLocalCallbackFn -> DuckDBV2TableFunctionInitLocalInfoHandle -> DuckDBV2ContextHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()

{- | Create a function pointer for 'DuckDBV2TableFunctionExecCallbackFn'.

Keep the pointer alive while DuckDB can invoke it. Free the pointer with
@freeHaskellFunPtr@ after its final possible invocation.
The callback must not throw an exception across the C boundary.
-}
foreign import ccall "wrapper"
    mkDuckDBV2TableFunctionExecCallbackFn :: DuckDBV2TableFunctionExecCallbackFn -> IO (FunPtr DuckDBV2TableFunctionExecCallbackFn)

-- | Call a function pointer with the 'DuckDBV2TableFunctionExecCallbackFn' signature.
foreign import ccall safe "dynamic"
    callDuckDBV2TableFunctionExecCallbackFn :: FunPtr DuckDBV2TableFunctionExecCallbackFn -> DuckDBV2TableFunctionExecInfoHandle -> DuckDBV2ContextHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()

{- | Create a function pointer for 'DuckDBV2TableFunctionProgressCallbackFn'.

Keep the pointer alive while DuckDB can invoke it. Free the pointer with
@freeHaskellFunPtr@ after its final possible invocation.
The callback must not throw an exception across the C boundary.
-}
foreign import ccall "wrapper"
    mkDuckDBV2TableFunctionProgressCallbackFn :: DuckDBV2TableFunctionProgressCallbackFn -> IO (FunPtr DuckDBV2TableFunctionProgressCallbackFn)

-- | Call a function pointer with the 'DuckDBV2TableFunctionProgressCallbackFn' signature.
foreign import ccall safe "dynamic"
    callDuckDBV2TableFunctionProgressCallbackFn :: FunPtr DuckDBV2TableFunctionProgressCallbackFn -> DuckDBV2TableFunctionProgressInfoHandle -> DuckDBV2ContextHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()

{- | Create a function pointer for 'DuckDBV2TableFunctionFilterPushdownCallbackFn'.

Keep the pointer alive while DuckDB can invoke it. Free the pointer with
@freeHaskellFunPtr@ after its final possible invocation.
The callback must not throw an exception across the C boundary.
-}
foreign import ccall "wrapper"
    mkDuckDBV2TableFunctionFilterPushdownCallbackFn :: DuckDBV2TableFunctionFilterPushdownCallbackFn -> IO (FunPtr DuckDBV2TableFunctionFilterPushdownCallbackFn)

-- | Call a function pointer with the 'DuckDBV2TableFunctionFilterPushdownCallbackFn' signature.
foreign import ccall safe "dynamic"
    callDuckDBV2TableFunctionFilterPushdownCallbackFn :: FunPtr DuckDBV2TableFunctionFilterPushdownCallbackFn -> DuckDBV2TableFunctionFilterPushdownInfoHandle -> DuckDBV2ContextHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()

{- | Create a function pointer for 'DuckDBV2TableFunctionPartitionDataCallbackFn'.

Keep the pointer alive while DuckDB can invoke it. Free the pointer with
@freeHaskellFunPtr@ after its final possible invocation.
The callback must not throw an exception across the C boundary.
-}
foreign import ccall "wrapper"
    mkDuckDBV2TableFunctionPartitionDataCallbackFn :: DuckDBV2TableFunctionPartitionDataCallbackFn -> IO (FunPtr DuckDBV2TableFunctionPartitionDataCallbackFn)

-- | Call a function pointer with the 'DuckDBV2TableFunctionPartitionDataCallbackFn' signature.
foreign import ccall safe "dynamic"
    callDuckDBV2TableFunctionPartitionDataCallbackFn :: FunPtr DuckDBV2TableFunctionPartitionDataCallbackFn -> DuckDBV2TableFunctionPartitionDataInfoHandle -> DuckDBV2ContextHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()

{- | Create a function pointer for 'DuckDBV2TableFunctionPartitioningCallbackFn'.

Keep the pointer alive while DuckDB can invoke it. Free the pointer with
@freeHaskellFunPtr@ after its final possible invocation.
The callback must not throw an exception across the C boundary.
-}
foreign import ccall "wrapper"
    mkDuckDBV2TableFunctionPartitioningCallbackFn :: DuckDBV2TableFunctionPartitioningCallbackFn -> IO (FunPtr DuckDBV2TableFunctionPartitioningCallbackFn)

-- | Call a function pointer with the 'DuckDBV2TableFunctionPartitioningCallbackFn' signature.
foreign import ccall safe "dynamic"
    callDuckDBV2TableFunctionPartitioningCallbackFn :: FunPtr DuckDBV2TableFunctionPartitioningCallbackFn -> DuckDBV2TableFunctionPartitioningInfoHandle -> DuckDBV2ContextHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()

{- | Create a function pointer for 'DuckDBV2TableFunctionClaimBatchCallbackFn'.

Keep the pointer alive while DuckDB can invoke it. Free the pointer with
@freeHaskellFunPtr@ after its final possible invocation.
The callback must not throw an exception across the C boundary.
-}
foreign import ccall "wrapper"
    mkDuckDBV2TableFunctionClaimBatchCallbackFn :: DuckDBV2TableFunctionClaimBatchCallbackFn -> IO (FunPtr DuckDBV2TableFunctionClaimBatchCallbackFn)

-- | Call a function pointer with the 'DuckDBV2TableFunctionClaimBatchCallbackFn' signature.
foreign import ccall safe "dynamic"
    callDuckDBV2TableFunctionClaimBatchCallbackFn :: FunPtr DuckDBV2TableFunctionClaimBatchCallbackFn -> DuckDBV2TableFunctionClaimBatchInfoHandle -> DuckDBV2ContextHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()

{- | Create a function pointer for 'DuckDBV2ArrowSchemaReleaseFn'.

Keep the pointer alive while DuckDB can invoke it. Free the pointer with
@freeHaskellFunPtr@ after its final possible invocation.
The callback must not throw an exception across the C boundary.
-}
foreign import ccall "wrapper"
    mkDuckDBV2ArrowSchemaReleaseFn :: DuckDBV2ArrowSchemaReleaseFn -> IO (FunPtr DuckDBV2ArrowSchemaReleaseFn)

-- | Call a function pointer with the 'DuckDBV2ArrowSchemaReleaseFn' signature.
foreign import ccall safe "dynamic"
    callDuckDBV2ArrowSchemaReleaseFn :: FunPtr DuckDBV2ArrowSchemaReleaseFn -> Ptr DuckDBV2ArrowSchema -> IO ()

{- | Create a function pointer for 'DuckDBV2ArrowArrayReleaseFn'.

Keep the pointer alive while DuckDB can invoke it. Free the pointer with
@freeHaskellFunPtr@ after its final possible invocation.
The callback must not throw an exception across the C boundary.
-}
foreign import ccall "wrapper"
    mkDuckDBV2ArrowArrayReleaseFn :: DuckDBV2ArrowArrayReleaseFn -> IO (FunPtr DuckDBV2ArrowArrayReleaseFn)

-- | Call a function pointer with the 'DuckDBV2ArrowArrayReleaseFn' signature.
foreign import ccall safe "dynamic"
    callDuckDBV2ArrowArrayReleaseFn :: FunPtr DuckDBV2ArrowArrayReleaseFn -> Ptr DuckDBV2ArrowArray -> IO ()

{- | Create a function pointer for 'DuckDBV2ArrowArrayStreamGetSchemaFn'.

Keep the pointer alive while DuckDB can invoke it. Free the pointer with
@freeHaskellFunPtr@ after its final possible invocation.
The callback must not throw an exception across the C boundary.
-}
foreign import ccall "wrapper"
    mkDuckDBV2ArrowArrayStreamGetSchemaFn :: DuckDBV2ArrowArrayStreamGetSchemaFn -> IO (FunPtr DuckDBV2ArrowArrayStreamGetSchemaFn)

-- | Call a function pointer with the 'DuckDBV2ArrowArrayStreamGetSchemaFn' signature.
foreign import ccall safe "dynamic"
    callDuckDBV2ArrowArrayStreamGetSchemaFn :: FunPtr DuckDBV2ArrowArrayStreamGetSchemaFn -> Ptr DuckDBV2ArrowArrayStream -> Ptr DuckDBV2ArrowSchema -> IO CInt

{- | Create a function pointer for 'DuckDBV2ArrowArrayStreamGetNextFn'.

Keep the pointer alive while DuckDB can invoke it. Free the pointer with
@freeHaskellFunPtr@ after its final possible invocation.
The callback must not throw an exception across the C boundary.
-}
foreign import ccall "wrapper"
    mkDuckDBV2ArrowArrayStreamGetNextFn :: DuckDBV2ArrowArrayStreamGetNextFn -> IO (FunPtr DuckDBV2ArrowArrayStreamGetNextFn)

-- | Call a function pointer with the 'DuckDBV2ArrowArrayStreamGetNextFn' signature.
foreign import ccall safe "dynamic"
    callDuckDBV2ArrowArrayStreamGetNextFn :: FunPtr DuckDBV2ArrowArrayStreamGetNextFn -> Ptr DuckDBV2ArrowArrayStream -> Ptr DuckDBV2ArrowArray -> IO CInt

{- | Create a function pointer for 'DuckDBV2ArrowArrayStreamGetLastErrorFn'.

Keep the pointer alive while DuckDB can invoke it. Free the pointer with
@freeHaskellFunPtr@ after its final possible invocation.
The callback must not throw an exception across the C boundary.
-}
foreign import ccall "wrapper"
    mkDuckDBV2ArrowArrayStreamGetLastErrorFn :: DuckDBV2ArrowArrayStreamGetLastErrorFn -> IO (FunPtr DuckDBV2ArrowArrayStreamGetLastErrorFn)

-- | Call a function pointer with the 'DuckDBV2ArrowArrayStreamGetLastErrorFn' signature.
foreign import ccall safe "dynamic"
    callDuckDBV2ArrowArrayStreamGetLastErrorFn :: FunPtr DuckDBV2ArrowArrayStreamGetLastErrorFn -> Ptr DuckDBV2ArrowArrayStream -> IO (Ptr CChar)

{- | Create a function pointer for 'DuckDBV2ArrowArrayStreamReleaseFn'.

Keep the pointer alive while DuckDB can invoke it. Free the pointer with
@freeHaskellFunPtr@ after its final possible invocation.
The callback must not throw an exception across the C boundary.
-}
foreign import ccall "wrapper"
    mkDuckDBV2ArrowArrayStreamReleaseFn :: DuckDBV2ArrowArrayStreamReleaseFn -> IO (FunPtr DuckDBV2ArrowArrayStreamReleaseFn)

-- | Call a function pointer with the 'DuckDBV2ArrowArrayStreamReleaseFn' signature.
foreign import ccall safe "dynamic"
    callDuckDBV2ArrowArrayStreamReleaseFn :: FunPtr DuckDBV2ArrowArrayStreamReleaseFn -> Ptr DuckDBV2ArrowArrayStream -> IO ()
