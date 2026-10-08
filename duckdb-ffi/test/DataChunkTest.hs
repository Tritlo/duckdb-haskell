{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

module DataChunkTest (tests) where

import Control.Exception (bracket)
import Control.Monad (forM_, void, when, (>=>))
import Data.Coerce (coerce)
import Data.Int (Int32)
import Data.Void (Void)
import Database.DuckDB.FFI
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (Ptr, nullPtr, plusPtr)
import Foreign.Storable (peek, poke, sizeOf)
import GHC.Records (getField)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Utils (setAllValid, withLogicalType)

-- | Entry point for the Data Chunk focused tests.
tests :: TestTree
tests =
    testGroup
        "Data Chunk Interface"
        [ dataChunkLifecycle
        , dataChunkReset
        ]

-- | Populate an integer chunk and verify the stored sequence.
dataChunkLifecycle :: TestTree
dataChunkLifecycle =
    testCase "create chunk, fill via vector, and read back" $ do
        withIntegerLogicalType \intType ->
            withArray [intType] \typeArray ->
                withDataChunk (duckdb_create_data_chunk typeArray 1) \chunk -> do
                    duckdb_data_chunk_get_column_count chunk >>= (@?= 1)

                    vec <- duckdb_data_chunk_get_vector chunk 0
                    fillVectorWithSequence vec [1 .. 4]
                    duckdb_data_chunk_set_size chunk 4

                    verifyChunkContents chunk [1 .. 4]

-- | Resetting should zero the size yet keep buffers reusable.
dataChunkReset :: TestTree
dataChunkReset =
    testCase "reset clears size and keeps vectors reusable" $ do
        withIntegerLogicalType \intType ->
            withArray [intType] \typeArray ->
                withDataChunk (duckdb_create_data_chunk typeArray 1) \chunk -> do
                    vec <- duckdb_data_chunk_get_vector chunk 0
                    fillVectorWithSequence vec [10, 20, 30]
                    duckdb_data_chunk_set_size chunk 3

                    duckdb_data_chunk_reset chunk
                    duckdb_data_chunk_get_size chunk >>= (@?= 0)

                    fillVectorWithSequence vec [7, 8]
                    duckdb_data_chunk_set_size chunk 2
                    verifyChunkContents chunk [7, 8]

-- Helpers -------------------------------------------------------------------

withIntegerLogicalType :: (Duckdb_logical_type -> IO a) -> IO a
withIntegerLogicalType = withLogicalType (duckdb_create_logical_type (Duckdb_type DUCKDB_TYPE_INTEGER))

withDataChunk :: IO Duckdb_data_chunk -> (Duckdb_data_chunk -> IO a) -> IO a
withDataChunk acquire = bracket acquire destroyChunk
  where
    destroyChunk chunk = alloca \ptr -> poke ptr chunk >> duckdb_destroy_data_chunk ptr

fillVectorWithSequence :: Duckdb_vector -> [Int32] -> IO ()
fillVectorWithSequence vec values = do
    colType <- duckdb_vector_get_column_type vec
    withLogicalType (pure colType) ((fmap (getField @"unwrap") . duckdb_get_type_id) >=> (@?= DUCKDB_TYPE_INTEGER))
    void (duckdb_vector_ensure_validity_writable vec)
    dataPtrRaw <- duckdb_vector_get_data vec
    let dataPtr = coerce dataPtrRaw :: Ptr Int32
    validity <- duckdb_vector_get_validity vec
    when (validity /= (coerce (nullPtr :: Ptr Void))) $ setAllValid validity (length values)
    forM_ (zip [0 ..] values) (uncurry (pokeElem dataPtr))

verifyChunkContents :: Duckdb_data_chunk -> [Int32] -> IO ()
verifyChunkContents chunk expected = do
    sz <- duckdb_data_chunk_get_size chunk
    sz @?= fromIntegral (length expected)
    vec <- duckdb_data_chunk_get_vector chunk 0
    dataPtrRaw <- duckdb_vector_get_data vec
    let dataPtr = coerce dataPtrRaw :: Ptr Int32
    forM_ (zip [0 ..] expected) \(idx, val) -> peekElem dataPtr idx >>= (@?= val)

-- validity helpers ----------------------------------------------------------

-- pointer utilities ---------------------------------------------------------

pokeElem :: Ptr Int32 -> Int -> Int32 -> IO ()
pokeElem base idx = poke (base `plusElem` idx)

peekElem :: Ptr Int32 -> Int -> IO Int32
peekElem base idx = peek (base `plusElem` idx)

plusElem :: Ptr a -> Int -> Ptr a
plusElem base idx = base `plusPtr` (idx * sizeOf (undefined :: Int32))
