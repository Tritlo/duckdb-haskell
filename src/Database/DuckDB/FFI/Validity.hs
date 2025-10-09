module Database.DuckDB.FFI.Validity (
  c_duckdb_validity_row_is_valid,
  c_duckdb_validity_set_row_validity,
  c_duckdb_validity_set_row_invalid,
  c_duckdb_validity_set_row_valid
) where

import Database.DuckDB.FFI.Types
import Foreign.C.Types (CBool (..))
import Data.Word (Word64)
import Foreign.Ptr (Ptr)

-- | Returns whether or not a row is valid (i.e., not NULL) in the given validity
-- mask.
--
-- Parameters:
-- * @validity@: The validity mask, as obtained through
--   @duckdb_vector_get_validity@
-- * @row@: The row index
--
-- Returns true if the row is valid, false otherwise
foreign import ccall safe "duckdb_validity_row_is_valid"
  c_duckdb_validity_row_is_valid :: Ptr Word64 -> DuckDBIdx -> IO CBool

-- | In a validity mask, sets a specific row to either valid or invalid.
--
-- Note that @duckdb_vector_ensure_validity_writable@ should be called before
-- calling @duckdb_vector_get_validity@, to ensure that there is a validity mask
-- to write to.
--
-- Parameters:
-- * @validity@: The validity mask, as obtained through
--   @duckdb_vector_get_validity@.
-- * @row@: The row index
-- * @valid@: Whether or not to set the row to valid, or invalid
foreign import ccall unsafe "duckdb_validity_set_row_validity"
  c_duckdb_validity_set_row_validity :: Ptr Word64 -> DuckDBIdx -> CBool -> IO ()

-- | In a validity mask, sets a specific row to invalid.
--
-- Equivalent to @duckdb_validity_set_row_validity@ with valid set to false.
--
-- Parameters:
-- * @validity@: The validity mask
-- * @row@: The row index
foreign import ccall safe "duckdb_validity_set_row_invalid"
  c_duckdb_validity_set_row_invalid :: Ptr Word64 -> DuckDBIdx -> IO ()

-- | In a validity mask, sets a specific row to valid.
--
-- Equivalent to @duckdb_validity_set_row_validity@ with valid set to true.
--
-- Parameters:
-- * @validity@: The validity mask
-- * @row@: The row index
foreign import ccall unsafe "duckdb_validity_set_row_valid"
  c_duckdb_validity_set_row_valid :: Ptr Word64 -> DuckDBIdx -> IO ()
