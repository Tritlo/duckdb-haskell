{-# LANGUAGE BlockArguments #-}

module SelectionVectorTest (tests) where

import Control.Monad (forM_)
import Data.Int (Int32)
import Data.Word (Word32)
import Database.DuckDB.FFI
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (peekElemOff, pokeElemOff)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Utils (withLogicalType, withSelectionVector, withVector)

tests :: TestTree
tests =
    testGroup
        "Selection Vector Interface"
        [ selectionVectorPointerWritable
        , selectionVectorCopySelection
        ]

selectionVectorPointerWritable :: TestTree
selectionVectorPointerWritable =
    testCase "selection vector exposes writable data pointer" $ do
        withSelectionVector 4 \selVec -> do
            dataPtr <- c_duckdb_selection_vector_get_data_ptr selVec
            assertBool "data pointer should be non-null" (dataPtr /= nullPtr)
            forM_ (zip [0 ..] [0, 2, 4, 6 :: Word32]) (uncurry (pokeElemOff dataPtr))
            fetched <- mapM (peekElemOff dataPtr) [0 .. 3]
            fetched @?= [0, 2, 4, 6]

selectionVectorCopySelection :: TestTree
selectionVectorCopySelection =
    testCase "vector_copy_sel copies selected rows" $ do
        withLogicalType (c_duckdb_create_logical_type DuckDBTypeInteger) \intType -> do
            withVector (c_duckdb_create_vector intType 4) \srcVec -> do
                srcPtr <- vectorDataPtr srcVec
                forM_ (zip [0 ..] [10, 20, 30, 40 :: Int32]) (uncurry (pokeElemOff srcPtr))

                withSelectionVector 2 \selVec -> do
                    selPtr <- c_duckdb_selection_vector_get_data_ptr selVec
                    pokeElemOff selPtr 0 1
                    pokeElemOff selPtr 1 3

                    withVector (c_duckdb_create_vector intType 2) \dstVec -> do
                        c_duckdb_vector_copy_sel srcVec dstVec selVec 2 0 0
                        dstPtr <- vectorDataPtr dstVec
                        val0 <- peekElemOff dstPtr 0
                        val1 <- peekElemOff dstPtr 1
                        val0 @?= 20
                        val1 @?= 40

-- Helpers -------------------------------------------------------------------

vectorDataPtr :: DuckDBVector -> IO (Ptr Int32)
vectorDataPtr vec = do
    raw <- c_duckdb_vector_get_data vec
    pure (castPtr raw)
