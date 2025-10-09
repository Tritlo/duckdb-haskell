{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}

module DataChunkTest (tests) where

import Control.Exception (bracket)
import Control.Monad (forM_, void, when)
import Data.Bits (complement)
import Data.Int (Int32)
import Data.Word (Word64)
import Database.DuckDB.FFI
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (Ptr, castPtr, plusPtr, nullPtr)
import Foreign.Storable (peek, poke, sizeOf)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

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
        withDataChunk (c_duckdb_create_data_chunk typeArray 1) \chunk -> do
          c_duckdb_data_chunk_get_column_count chunk >>= (@?= 1)

          vec <- c_duckdb_data_chunk_get_vector chunk 0
          fillVectorWithSequence vec [1 .. 4]
          c_duckdb_data_chunk_set_size chunk 4

          verifyChunkContents chunk [1 .. 4]

-- | Resetting should zero the size yet keep buffers reusable.
dataChunkReset :: TestTree
dataChunkReset =
  testCase "reset clears size and keeps vectors reusable" $ do
    withIntegerLogicalType \intType ->
      withArray [intType] \typeArray ->
        withDataChunk (c_duckdb_create_data_chunk typeArray 1) \chunk -> do
          vec <- c_duckdb_data_chunk_get_vector chunk 0
          fillVectorWithSequence vec [10, 20, 30]
          c_duckdb_data_chunk_set_size chunk 3

          c_duckdb_data_chunk_reset chunk
          c_duckdb_data_chunk_get_size chunk >>= (@?= 0)

          fillVectorWithSequence vec [7, 8]
          c_duckdb_data_chunk_set_size chunk 2
          verifyChunkContents chunk [7, 8]

-- Helpers -------------------------------------------------------------------

withIntegerLogicalType :: (DuckDBLogicalType -> IO a) -> IO a
withIntegerLogicalType = withLogicalType (c_duckdb_create_logical_type DuckDBTypeInteger)

withLogicalType :: IO DuckDBLogicalType -> (DuckDBLogicalType -> IO a) -> IO a
withLogicalType acquire action = bracket acquire destroyLogicalType action

withDataChunk :: IO DuckDBDataChunk -> (DuckDBDataChunk -> IO a) -> IO a
withDataChunk acquire action = bracket acquire destroyChunk action
  where
    destroyChunk chunk = alloca \ptr -> poke ptr chunk >> c_duckdb_destroy_data_chunk ptr

destroyLogicalType :: DuckDBLogicalType -> IO ()
destroyLogicalType lt = alloca \ptr -> poke ptr lt >> c_duckdb_destroy_logical_type ptr

fillVectorWithSequence :: DuckDBVector -> [Int32] -> IO ()
fillVectorWithSequence vec values = do
  colType <- c_duckdb_vector_get_column_type vec
  withLogicalType (pure colType) \inner -> c_duckdb_get_type_id inner >>= (@?= DuckDBTypeInteger)
  void (c_duckdb_vector_ensure_validity_writable vec)
  dataPtrRaw <- c_duckdb_vector_get_data vec
  let dataPtr = castPtr dataPtrRaw :: Ptr Int32
  validity <- c_duckdb_vector_get_validity vec
  when (validity /= nullPtr) $ setAllValid validity (length values)
  forM_ (zip [0 ..] values) \(idx, val) -> pokeElem dataPtr idx val

verifyChunkContents :: DuckDBDataChunk -> [Int32] -> IO ()
verifyChunkContents chunk expected = do
  sz <- c_duckdb_data_chunk_get_size chunk
  sz @?= fromIntegral (length expected)
  vec <- c_duckdb_data_chunk_get_vector chunk 0
  dataPtrRaw <- c_duckdb_vector_get_data vec
  let dataPtr = castPtr dataPtrRaw :: Ptr Int32
  forM_ (zip [0 ..] expected) \(idx, val) -> peekElem dataPtr idx >>= (@?= val)

-- validity helpers ----------------------------------------------------------

setAllValid :: Ptr Word64 -> Int -> IO ()
setAllValid mask count =
  let wordsNeeded = (count + 63) `div` 64
   in forM_ [0 .. wordsNeeded - 1] \wordIdx -> do
        let ptr = mask `plusWord` wordIdx
        poke ptr (complement 0)

-- pointer utilities ---------------------------------------------------------

pokeElem :: Ptr Int32 -> Int -> Int32 -> IO ()
pokeElem base idx val = poke (base `plusElem` idx) val

peekElem :: Ptr Int32 -> Int -> IO Int32
peekElem base idx = peek (base `plusElem` idx)

plusElem :: Ptr a -> Int -> Ptr a
plusElem base idx = base `plusPtr` (idx * sizeOf (undefined :: Int32))

plusWord :: Ptr Word64 -> Int -> Ptr Word64
plusWord base idx = base `plusPtr` (idx * sizeOf (undefined :: Word64))
