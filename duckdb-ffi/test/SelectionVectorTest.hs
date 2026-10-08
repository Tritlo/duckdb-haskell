{-# LANGUAGE BlockArguments #-}

module SelectionVectorTest (tests) where

import Control.Monad (forM_)
import Data.Coerce (coerce)
import Data.Int (Int32)
import Data.Void (Void)
import Database.DuckDB.FFI
import Foreign.Ptr (Ptr, nullPtr)
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
            dataPtr <- duckdb_selection_vector_get_data_ptr selVec
            assertBool "data pointer should be non-null" (dataPtr /= (coerce (nullPtr :: Ptr Void)))
            forM_ (zip [0 ..] [0, 2, 4, 6 :: Sel_t]) (uncurry (pokeElemOff dataPtr))
            fetched <- mapM (peekElemOff dataPtr) [0 .. 3]
            fetched @?= [0, 2, 4, 6]

selectionVectorCopySelection :: TestTree
selectionVectorCopySelection =
    testCase "vector_copy_sel copies selected rows" $ do
        withLogicalType (duckdb_create_logical_type (Duckdb_type DUCKDB_TYPE_INTEGER)) \intType -> do
            withVector (duckdb_create_vector intType 4) \srcVec -> do
                srcPtr <- vectorDataPtr srcVec
                forM_ (zip [0 ..] [10, 20, 30, 40 :: Int32]) (uncurry (pokeElemOff srcPtr))

                withSelectionVector 2 \selVec -> do
                    selPtr <- duckdb_selection_vector_get_data_ptr selVec
                    pokeElemOff selPtr 0 1
                    pokeElemOff selPtr 1 3

                    withVector (duckdb_create_vector intType 2) \dstVec -> do
                        duckdb_vector_copy_sel srcVec dstVec selVec 2 0 0
                        dstPtr <- vectorDataPtr dstVec
                        val0 <- peekElemOff dstPtr 0
                        val1 <- peekElemOff dstPtr 1
                        val0 @?= 20
                        val1 @?= 40

-- Helpers -------------------------------------------------------------------

vectorDataPtr :: Duckdb_vector -> IO (Ptr Int32)
vectorDataPtr vec = do
    raw <- duckdb_vector_get_data vec
    pure (coerce raw)
