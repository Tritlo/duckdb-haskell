module Database.DuckDB.FFI.LogicalTypes (
    c_duckdb_create_logical_type,
    c_duckdb_logical_type_get_alias,
    c_duckdb_logical_type_set_alias,
    c_duckdb_create_list_type,
    c_duckdb_create_array_type,
    c_duckdb_create_map_type,
    c_duckdb_create_union_type,
    c_duckdb_create_struct_type,
    c_duckdb_create_enum_type,
    c_duckdb_create_decimal_type,
    c_duckdb_get_type_id,
    c_duckdb_decimal_width,
    c_duckdb_decimal_scale,
    c_duckdb_decimal_internal_type,
    c_duckdb_enum_internal_type,
    c_duckdb_enum_dictionary_size,
    c_duckdb_enum_dictionary_value,
    c_duckdb_list_type_child_type,
    c_duckdb_array_type_child_type,
    c_duckdb_array_type_array_size,
    c_duckdb_map_type_key_type,
    c_duckdb_map_type_value_type,
    c_duckdb_struct_type_child_count,
    c_duckdb_struct_type_child_name,
    c_duckdb_struct_type_child_type,
    c_duckdb_union_type_member_count,
    c_duckdb_union_type_member_name,
    c_duckdb_union_type_member_type,
    c_duckdb_destroy_logical_type,
    c_duckdb_register_logical_type,
) where

import Data.Word (Word32, Word8)
import Database.DuckDB.FFI.Types
import Foreign.C.String (CString)
import Foreign.C.Types (CInt (..))
import Foreign.Ptr (Ptr)

{- | Creates a @duckdb_logical_type@ from a primitive type. The resulting logical
type must be destroyed with @duckdb_destroy_logical_type@.

Returns an invalid logical type, if type is: @DUCKDB_TYPE_INVALID@,
@DUCKDB_TYPE_DECIMAL@, @DUCKDB_TYPE_ENUM@, @DUCKDB_TYPE_LIST@,
@DUCKDB_TYPE_STRUCT@, @DUCKDB_TYPE_MAP@, @DUCKDB_TYPE_ARRAY@, or
@DUCKDB_TYPE_UNION@.

Parameters:
* @type@: The primitive type to create.

Returns The logical type.
-}
foreign import ccall safe "duckdb_create_logical_type"
    c_duckdb_create_logical_type :: DuckDBType -> IO DuckDBLogicalType

{- | Returns the alias of a duckdb_logical_type, if set, else @nullptr@. The result
must be destroyed with @duckdb_free@.

Parameters:
* @type@: The logical type

Returns The alias or @nullptr@
-}
foreign import ccall safe "duckdb_logical_type_get_alias"
    c_duckdb_logical_type_get_alias :: DuckDBLogicalType -> IO CString

{- | Sets the alias of a duckdb_logical_type.

Parameters:
* @type@: The logical type
* @alias@: The alias to set
-}
foreign import ccall safe "duckdb_logical_type_set_alias"
    c_duckdb_logical_type_set_alias :: DuckDBLogicalType -> CString -> IO ()

{- | Creates a LIST type from its child type. The return type must be destroyed
with @duckdb_destroy_logical_type@.

Parameters:
* @type@: The child type of the list

Returns The logical type.
-}
foreign import ccall safe "duckdb_create_list_type"
    c_duckdb_create_list_type :: DuckDBLogicalType -> IO DuckDBLogicalType

{- | Creates an ARRAY type from its child type. The return type must be destroyed
with @duckdb_destroy_logical_type@.

Parameters:
* @type@: The child type of the array.
* @array_size@: The number of elements in the array.

Returns The logical type.
-}
foreign import ccall safe "duckdb_create_array_type"
    c_duckdb_create_array_type :: DuckDBLogicalType -> DuckDBIdx -> IO DuckDBLogicalType

{- | Creates a MAP type from its key type and value type. The return type must be
destroyed with @duckdb_destroy_logical_type@.

Parameters:
* @key_type@: The map's key type.
* @value_type@: The map's value type.

Returns The logical type.
-}
foreign import ccall safe "duckdb_create_map_type"
    c_duckdb_create_map_type :: DuckDBLogicalType -> DuckDBLogicalType -> IO DuckDBLogicalType

{- | Creates a UNION type from the passed arrays. The return type must be destroyed
with @duckdb_destroy_logical_type@.

Parameters:
* @member_types@: The array of union member types.
* @member_names@: The union member names.
* @member_count@: The number of union members.

Returns The logical type.
-}
foreign import ccall safe "duckdb_create_union_type"
    c_duckdb_create_union_type :: Ptr DuckDBLogicalType -> Ptr CString -> DuckDBIdx -> IO DuckDBLogicalType

{- | Creates a STRUCT type based on the member types and names. The resulting type
must be destroyed with @duckdb_destroy_logical_type@.

Parameters:
* @member_types@: The array of types of the struct members.
* @member_names@: The array of names of the struct members.
* @member_count@: The number of members of the struct.

Returns The logical type.
-}
foreign import ccall safe "duckdb_create_struct_type"
    c_duckdb_create_struct_type :: Ptr DuckDBLogicalType -> Ptr CString -> DuckDBIdx -> IO DuckDBLogicalType

{- | Creates an ENUM type from the passed member name array. The resulting type
should be destroyed with @duckdb_destroy_logical_type@.

Parameters:
* @member_names@: The array of names that the enum should consist of.
* @member_count@: The number of elements that were specified in the array.

Returns The logical type.
-}
foreign import ccall safe "duckdb_create_enum_type"
    c_duckdb_create_enum_type :: Ptr CString -> DuckDBIdx -> IO DuckDBLogicalType

{- | Creates a DECIMAL type with the specified width and scale. The resulting type
should be destroyed with @duckdb_destroy_logical_type@.

Parameters:
* @width@: The width of the decimal type
* @scale@: The scale of the decimal type

Returns The logical type.
-}
foreign import ccall safe "duckdb_create_decimal_type"
    c_duckdb_create_decimal_type :: Word8 -> Word8 -> IO DuckDBLogicalType

{- | Retrieves the enum @duckdb_type@ of a @duckdb_logical_type@.

Parameters:
* @type@: The logical type.

Returns The @duckdb_type@ id.
-}
foreign import ccall safe "duckdb_get_type_id"
    c_duckdb_get_type_id :: DuckDBLogicalType -> IO DuckDBType

{- | Retrieves the width of a decimal type.

Parameters:
* @type@: The logical type object

Returns The width of the decimal type
-}
foreign import ccall safe "duckdb_decimal_width"
    c_duckdb_decimal_width :: DuckDBLogicalType -> IO Word8

{- | Retrieves the scale of a decimal type.

Parameters:
* @type@: The logical type object

Returns The scale of the decimal type
-}
foreign import ccall safe "duckdb_decimal_scale"
    c_duckdb_decimal_scale :: DuckDBLogicalType -> IO Word8

{- | Retrieves the internal storage type of a decimal type.

Parameters:
* @type@: The logical type object

Returns The internal type of the decimal type
-}
foreign import ccall safe "duckdb_decimal_internal_type"
    c_duckdb_decimal_internal_type :: DuckDBLogicalType -> IO DuckDBType

{- | Retrieves the internal storage type of an enum type.

Parameters:
* @type@: The logical type object

Returns The internal type of the enum type
-}
foreign import ccall safe "duckdb_enum_internal_type"
    c_duckdb_enum_internal_type :: DuckDBLogicalType -> IO DuckDBType

{- | Retrieves the dictionary size of the enum type.

Parameters:
* @type@: The logical type object

Returns The dictionary size of the enum type
-}
foreign import ccall safe "duckdb_enum_dictionary_size"
    c_duckdb_enum_dictionary_size :: DuckDBLogicalType -> IO Word32

{- | Retrieves the dictionary value at the specified position from the enum.

The result must be freed with @duckdb_free@.

Parameters:
* @type@: The logical type object
* @index@: The index in the dictionary

Returns The string value of the enum type. Must be freed with @duckdb_free@.
-}
foreign import ccall safe "duckdb_enum_dictionary_value"
    c_duckdb_enum_dictionary_value :: DuckDBLogicalType -> DuckDBIdx -> IO CString

{- | Retrieves the child type of the given LIST type. Also accepts MAP types. The
result must be freed with @duckdb_destroy_logical_type@.

Parameters:
* @type@: The logical type, either LIST or MAP.

Returns The child type of the LIST or MAP type.
-}
foreign import ccall safe "duckdb_list_type_child_type"
    c_duckdb_list_type_child_type :: DuckDBLogicalType -> IO DuckDBLogicalType

{- | Retrieves the child type of the given ARRAY type.

The result must be freed with @duckdb_destroy_logical_type@.

Parameters:
* @type@: The logical type. Must be ARRAY.

Returns The child type of the ARRAY type.
-}
foreign import ccall safe "duckdb_array_type_child_type"
    c_duckdb_array_type_child_type :: DuckDBLogicalType -> IO DuckDBLogicalType

{- | Retrieves the array size of the given array type.

Parameters:
* @type@: The logical type object

Returns The fixed number of elements the values of this array type can store.
-}
foreign import ccall safe "duckdb_array_type_array_size"
    c_duckdb_array_type_array_size :: DuckDBLogicalType -> IO DuckDBIdx

{- | Retrieves the key type of the given map type.

The result must be freed with @duckdb_destroy_logical_type@.

Parameters:
* @type@: The logical type object

Returns The key type of the map type. Must be destroyed with
@duckdb_destroy_logical_type@.
-}
foreign import ccall safe "duckdb_map_type_key_type"
    c_duckdb_map_type_key_type :: DuckDBLogicalType -> IO DuckDBLogicalType

{- | Retrieves the value type of the given map type.

The result must be freed with @duckdb_destroy_logical_type@.

Parameters:
* @type@: The logical type object

Returns The value type of the map type. Must be destroyed with
@duckdb_destroy_logical_type@.
-}
foreign import ccall safe "duckdb_map_type_value_type"
    c_duckdb_map_type_value_type :: DuckDBLogicalType -> IO DuckDBLogicalType

{- | Returns the number of children of a struct type.

Parameters:
* @type@: The logical type object

Returns The number of children of a struct type.
-}
foreign import ccall safe "duckdb_struct_type_child_count"
    c_duckdb_struct_type_child_count :: DuckDBLogicalType -> IO DuckDBIdx

{- | Retrieves the name of the struct child.

The result must be freed with @duckdb_free@.

Parameters:
* @type@: The logical type object
* @index@: The child index

Returns The name of the struct type. Must be freed with @duckdb_free@.
-}
foreign import ccall safe "duckdb_struct_type_child_name"
    c_duckdb_struct_type_child_name :: DuckDBLogicalType -> DuckDBIdx -> IO CString

{- | Retrieves the child type of the given struct type at the specified index.

The result must be freed with @duckdb_destroy_logical_type@.

Parameters:
* @type@: The logical type object
* @index@: The child index

Returns The child type of the struct type. Must be destroyed with
@duckdb_destroy_logical_type@.
-}
foreign import ccall safe "duckdb_struct_type_child_type"
    c_duckdb_struct_type_child_type :: DuckDBLogicalType -> DuckDBIdx -> IO DuckDBLogicalType

{- | Returns the number of members that the union type has.

Parameters:
* @type@: The logical type (union) object

Returns The number of members of a union type.
-}
foreign import ccall safe "duckdb_union_type_member_count"
    c_duckdb_union_type_member_count :: DuckDBLogicalType -> IO DuckDBIdx

{- | Retrieves the name of the union member.

The result must be freed with @duckdb_free@.

Parameters:
* @type@: The logical type object
* @index@: The child index

Returns The name of the union member. Must be freed with @duckdb_free@.
-}
foreign import ccall safe "duckdb_union_type_member_name"
    c_duckdb_union_type_member_name :: DuckDBLogicalType -> DuckDBIdx -> IO CString

{- | Retrieves the child type of the given union member at the specified index.

The result must be freed with @duckdb_destroy_logical_type@.

Parameters:
* @type@: The logical type object
* @index@: The child index

Returns The child type of the union member. Must be destroyed with
@duckdb_destroy_logical_type@.
-}
foreign import ccall safe "duckdb_union_type_member_type"
    c_duckdb_union_type_member_type :: DuckDBLogicalType -> DuckDBIdx -> IO DuckDBLogicalType

{- | Destroys the logical type and de-allocates all memory allocated for that type.

Parameters:
* @type@: The logical type to destroy.
-}
foreign import ccall safe "duckdb_destroy_logical_type"
    c_duckdb_destroy_logical_type :: Ptr DuckDBLogicalType -> IO ()

{- | Registers a custom type within the given connection. The type must have an
alias

Parameters:
* @con@: The connection to use
* @type@: The custom type to register

Returns Whether or not the registration was successful.
-}
foreign import ccall safe "duckdb_register_logical_type"
    c_duckdb_register_logical_type :: DuckDBConnection -> DuckDBLogicalType -> DuckDBCreateTypeInfo -> IO DuckDBState
