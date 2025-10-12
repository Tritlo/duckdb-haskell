module Database.DuckDB.FFI.Vector (
    c_duckdb_create_vector,
    c_duckdb_destroy_vector,
    c_duckdb_vector_get_column_type,
    c_duckdb_vector_get_data,
    c_duckdb_vector_get_validity,
    c_duckdb_vector_ensure_validity_writable,
    c_duckdb_vector_assign_string_element,
    c_duckdb_vector_assign_string_element_len,
    c_duckdb_list_vector_get_child,
    c_duckdb_list_vector_get_size,
    c_duckdb_list_vector_set_size,
    c_duckdb_list_vector_reserve,
    c_duckdb_struct_vector_get_child,
    c_duckdb_array_vector_get_child,
    c_duckdb_slice_vector,
    c_duckdb_vector_copy_sel,
    c_duckdb_vector_reference_value,
    c_duckdb_vector_reference_vector,
) where

import Data.Word (Word64)
import Database.DuckDB.FFI.Types
import Foreign.C.String (CString)
import Foreign.C.Types (CInt (..))
import Foreign.Ptr (Ptr)

{- | Creates a flat vector. Must be destroyed with @duckdb_destroy_vector@.

Parameters:
* @type@: The logical type of the vector.
* @capacity@: The capacity of the vector.

Returns The vector.
-}
foreign import ccall safe "duckdb_create_vector"
    c_duckdb_create_vector :: DuckDBLogicalType -> DuckDBIdx -> IO DuckDBVector

{- | Destroys the vector and de-allocates its memory.

Parameters:
* @vector@: A pointer to the vector.
-}
foreign import ccall safe "duckdb_destroy_vector"
    c_duckdb_destroy_vector :: Ptr DuckDBVector -> IO ()

{- | Retrieves the column type of the specified vector.

The result must be destroyed with @duckdb_destroy_logical_type@.

Parameters:
* @vector@: The vector get the data from

Returns The type of the vector
-}
foreign import ccall safe "duckdb_vector_get_column_type"
    c_duckdb_vector_get_column_type :: DuckDBVector -> IO DuckDBLogicalType

{- | Retrieves the data pointer of the vector.

The data pointer can be used to read or write values from the vector. How to
read or write values depends on the type of the vector.

Parameters:
* @vector@: The vector to get the data from

Returns The data pointer
-}
foreign import ccall safe "duckdb_vector_get_data"
    c_duckdb_vector_get_data :: DuckDBVector -> IO (Ptr ())

{- | Retrieves the validity mask pointer of the specified vector.

If all values are valid, this function MIGHT return NULL!

The validity mask is a bitset that signifies null-ness within the data chunk.
It is a series of uint64_t values, where each uint64_t value contains validity
for 64 tuples. The bit is set to 1 if the value is valid (i.e., not NULL) or 0
if the value is invalid (i.e., NULL).

Validity of a specific value can be obtained like this:

idx_t entry_idx = row_idx / 64; idx_t idx_in_entry = row_idx % 64; bool
is_valid = validity_mask[entry_idx] & (1 << idx_in_entry);

Alternatively, the (slower) duckdb_validity_row_is_valid function can be used.

Parameters:
* @vector@: The vector to get the data from

Returns The pointer to the validity mask, or NULL if no validity mask is
present
-}
foreign import ccall safe "duckdb_vector_get_validity"
    c_duckdb_vector_get_validity :: DuckDBVector -> IO (Ptr Word64)

{- | Ensures the validity mask is writable by allocating it.

After this function is called, @duckdb_vector_get_validity@ will ALWAYS return
non-NULL. This allows NULL values to be written to the vector, regardless of
whether a validity mask was present before.

Parameters:
* @vector@: The vector to alter
-}
foreign import ccall safe "duckdb_vector_ensure_validity_writable"
    c_duckdb_vector_ensure_validity_writable :: DuckDBVector -> IO ()

{- | Assigns a string element in the vector at the specified location.

Parameters:
* @vector@: The vector to alter
* @index@: The row position in the vector to assign the string to
* @str@: The null-terminated string
-}
foreign import ccall safe "duckdb_vector_assign_string_element"
    c_duckdb_vector_assign_string_element :: DuckDBVector -> DuckDBIdx -> CString -> IO ()

{- | Assigns a string element in the vector at the specified location. You may also
use this function to assign BLOBs.

Parameters:
* @vector@: The vector to alter
* @index@: The row position in the vector to assign the string to
* @str@: The string
* @str_len@: The length of the string (in bytes)
-}
foreign import ccall safe "duckdb_vector_assign_string_element_len"
    c_duckdb_vector_assign_string_element_len :: DuckDBVector -> DuckDBIdx -> CString -> DuckDBIdx -> IO ()

{- | Retrieves the child vector of a list vector.

The resulting vector is valid as long as the parent vector is valid.

Parameters:
* @vector@: The vector

Returns The child vector
-}
foreign import ccall safe "duckdb_list_vector_get_child"
    c_duckdb_list_vector_get_child :: DuckDBVector -> IO DuckDBVector

{- | Returns the size of the child vector of the list.

Parameters:
* @vector@: The vector

Returns The size of the child list
-}
foreign import ccall safe "duckdb_list_vector_get_size"
    c_duckdb_list_vector_get_size :: DuckDBVector -> IO DuckDBIdx

{- | Sets the total size of the underlying child-vector of a list vector.

Parameters:
* @vector@: The list vector.
* @size@: The size of the child list.

Returns The duckdb state. Returns DuckDBError if the vector is nullptr.
-}
foreign import ccall safe "duckdb_list_vector_set_size"
    c_duckdb_list_vector_set_size :: DuckDBVector -> DuckDBIdx -> IO DuckDBState

{- | Sets the total capacity of the underlying child-vector of a list.

After calling this method, you must call @duckdb_vector_get_validity@ and
@duckdb_vector_get_data@ to obtain current data and validity pointers

Parameters:
* @vector@: The list vector.
* @required_capacity@: the total capacity to reserve.

Returns The duckdb state. Returns DuckDBError if the vector is nullptr.
-}
foreign import ccall safe "duckdb_list_vector_reserve"
    c_duckdb_list_vector_reserve :: DuckDBVector -> DuckDBIdx -> IO DuckDBState

{- | Retrieves the child vector of a struct vector. The resulting vector is valid
as long as the parent vector is valid.

Parameters:
* @vector@: The vector
* @index@: The child index

Returns The child vector
-}
foreign import ccall safe "duckdb_struct_vector_get_child"
    c_duckdb_struct_vector_get_child :: DuckDBVector -> DuckDBIdx -> IO DuckDBVector

{- | Retrieves the child vector of an array vector. The resulting vector is valid
as long as the parent vector is valid. The resulting vector has the size of
the parent vector multiplied by the array size.

Parameters:
* @vector@: The vector

Returns The child vector
-}
foreign import ccall safe "duckdb_array_vector_get_child"
    c_duckdb_array_vector_get_child :: DuckDBVector -> IO DuckDBVector

{- | Slice a vector with a selection vector. The length of the selection vector
must be less than or equal to the length of the vector. Turns the vector into
a dictionary vector.

Parameters:
* @vector@: The vector to slice.
* @sel@: The selection vector.
* @len@: The length of the selection vector.
-}
foreign import ccall safe "duckdb_slice_vector"
    c_duckdb_slice_vector :: DuckDBVector -> DuckDBSelectionVector -> DuckDBIdx -> IO ()

{- | Copy the src vector to the dst with a selection vector that identifies which
indices to copy.

Parameters:
* @src@: The vector to copy from.
* @dst@: The vector to copy to.
* @sel@: The selection vector. The length of the selection vector should not
  be more than the length of the src vector
* @src_count@: The number of entries from selection vector to copy. Think of
  this as the effective length of the selection vector starting from index 0
* @src_offset@: The offset in the selection vector to copy from (important:
  actual number of items copied = src_count - src_offset).
* @dst_offset@: The offset in the dst vector to start copying to.
-}
foreign import ccall safe "duckdb_vector_copy_sel"
    c_duckdb_vector_copy_sel :: DuckDBVector -> DuckDBVector -> DuckDBSelectionVector -> DuckDBIdx -> DuckDBIdx -> DuckDBIdx -> IO ()

{- | Copies the value from @value@ to @vector@.

Parameters:
* @vector@: The receiving vector.
* @value@: The value to copy into the vector.
-}
foreign import ccall safe "duckdb_vector_reference_value"
    c_duckdb_vector_reference_value :: DuckDBVector -> DuckDBValue -> IO ()

{- | Changes @to_vector@ to reference `from_vector. After, the vectors share
ownership of the data.

Parameters:
* @to_vector@: The receiving vector.
* @from_vector@: The vector to reference.
-}
foreign import ccall safe "duckdb_vector_reference_vector"
    c_duckdb_vector_reference_vector :: DuckDBVector -> DuckDBVector -> IO ()
