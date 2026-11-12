module V2ChunkTest (tests) where

import Control.Exception (finally)
import Control.Monad (forM, forM_, unless, void, zipWithM_)
import Data.Coerce (coerce)
import Data.Int (Int64)
import Database.DuckDB.FFI.V2
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.C.Types (CBool (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (Storable (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import V2Test (checked, output, readView, withConnection, withFactory, withHandle, withType, withView)

tests :: TestTree
tests =
    testGroup
        "DuckDB V2 chunks and vectors"
        [ testCase "chunk copies are flat and outlive their source" testChunkCopy
        , testCase "cleared chunks are empty, flat and writable again" testChunkClear
        , testCase "vectors report their representation and constant validity" testVectorType
        , testCase "nested vectors report their child count" testChildCount
        , testCase "referenced vectors alias the data of their source" testReference
        , testCase "column data collections combine, reset and clear their rows" testCollection
        ]

withChunk :: DuckDBV2FactoryHandle -> [DuckDBV2LogicalTypeHandle] -> (DuckDBV2DataChunkHandle -> IO a) -> IO a
withChunk factory types action = withArray types $ \array ->
    withHandle (c_duckdb_v2_data_chunk_create factory (ConstPtr array) (fromIntegral (length types))) c_duckdb_v2_data_chunk_destroy action

vectorAt :: DuckDBV2DataChunkHandle -> Int -> IO DuckDBV2VectorHandle
vectorAt chunk index = output (c_duckdb_v2_data_chunk_get_vector chunk (fromIntegral index))

-- | Write the rows into a FLAT BIGINT vector and set its size.
writeBigints :: DuckDBV2VectorHandle -> [Int64] -> IO ()
writeBigints vector rows = do
    raw <- output (c_duckdb_v2_vector_get_data_mutable vector)
    zipWithM_ (pokeElemOff (coerce raw)) [0 ..] rows
    checked (c_duckdb_v2_vector_set_size vector (fromIntegral (length rows)))

-- | Read every row of a BIGINT vector of any representation.
readBigints :: DuckDBV2VectorHandle -> IO [Int64]
readBigints vector = do
    size <- output (c_duckdb_v2_vector_get_size vector)
    forM [0 .. fromIntegral size - 1 :: Int] $ \row ->
        withHandle (c_duckdb_v2_vector_get_value vector (fromIntegral row)) c_duckdb_v2_value_destroy $ \value ->
            output (c_duckdb_v2_value_get_bigint value)

vectorType :: DuckDBV2VectorHandle -> IO DuckDBV2VectorType
vectorType vector = output (c_duckdb_v2_vector_get_vector_type vector)

testChunkCopy :: IO ()
testChunkCopy = withConnection $ \conn -> withFactory conn $ \factory ->
    withType factory "BIGINT" $ \integer -> withType factory "VARCHAR" $ \text -> alloca $ \copySlot -> do
        poke copySlot (coerce nullPtr)
        flip finally (c_duckdb_v2_data_chunk_destroy copySlot >>= (@?= DuckDBV2ErrorNone)) $ do
            withChunk factory [integer, text] $ \source -> do
                capacity <- output (c_duckdb_v2_data_chunk_get_capacity source)
                assertBool "the chunk holds the rows that the test writes" (capacity >= 3)
                constant <- vectorAt source 0
                withHandle (c_duckdb_v2_value_create_bigint factory 7) c_duckdb_v2_value_destroy $ \value ->
                    checked (c_duckdb_v2_vector_make_constant constant value 3)
                strings <- vectorAt source 1
                checked (c_duckdb_v2_vector_set_size strings 3)
                forM_ (zip [0 ..] ["a", "long enough to be stored outside the inline buffer", "c"]) $ \(row, string) ->
                    withView string $ \view -> withHandle (c_duckdb_v2_value_create_varchar factory view) c_duckdb_v2_value_destroy $ \value ->
                        checked (c_duckdb_v2_vector_set_value strings row value)
                checked (c_duckdb_v2_data_chunk_copy factory source copySlot)
                vectorType constant >>= (@?= DuckDBV2VectorTypeConstant)
            -- The source chunk is destroyed now. The copy owns all of its data.
            copy <- peek copySlot
            output (c_duckdb_v2_data_chunk_get_size copy) >>= (@?= 3)
            integers <- vectorAt copy 0
            vectorType integers >>= (@?= DuckDBV2VectorTypeFlat)
            readBigints integers >>= (@?= [7, 7, 7])
            strings <- vectorAt copy 1
            forM [0 .. 2] (\row -> withHandle (c_duckdb_v2_vector_get_value strings row) c_duckdb_v2_value_destroy (\value -> output (c_duckdb_v2_value_get_varchar value) >>= readView))
                >>= (@?= ["a", "long enough to be stored outside the inline buffer", "c"])

testChunkClear :: IO ()
testChunkClear = withConnection $ \conn -> withFactory conn $ \factory ->
    withType factory "BIGINT" $ \integer -> withChunk factory [integer] $ \chunk -> do
        vector <- vectorAt chunk 0
        checked (c_duckdb_v2_vector_make_sequence vector 1 1 5)
        output (c_duckdb_v2_data_chunk_get_size chunk) >>= (@?= 5)
        checked (c_duckdb_v2_data_chunk_clear chunk)
        output (c_duckdb_v2_data_chunk_get_size chunk) >>= (@?= 0)
        vectorType vector >>= (@?= DuckDBV2VectorTypeFlat)
        writeBigints vector [4, 5]
        output (c_duckdb_v2_data_chunk_get_size chunk) >>= (@?= 2)
        readBigints vector >>= (@?= [4, 5])

testVectorType :: IO ()
testVectorType = withConnection $ \conn -> withFactory conn $ \factory ->
    withType factory "BIGINT" $ \integer -> withChunk factory [integer] $ \chunk -> do
        vector <- vectorAt chunk 0
        vectorType vector >>= (@?= DuckDBV2VectorTypeFlat)
        checked (c_duckdb_v2_vector_make_sequence vector 10 2 3)
        vectorType vector >>= (@?= DuckDBV2VectorTypeOther)
        checked (c_duckdb_v2_vector_flatten vector)
        vectorType vector >>= (@?= DuckDBV2VectorTypeFlat)
        withHandle (c_duckdb_v2_value_create_bigint factory 7) c_duckdb_v2_value_destroy $ \value ->
            checked (c_duckdb_v2_vector_make_constant vector value 3)
        vectorType vector >>= (@?= DuckDBV2VectorTypeConstant)
        let rowIsNull row = withHandle (c_duckdb_v2_vector_get_value vector row) c_duckdb_v2_value_destroy $ \value ->
                (/= 0) <$> output (c_duckdb_v2_value_is_null value)
        checked (c_duckdb_v2_vector_constant_set_valid vector (CBool 0))
        forM [0 .. 2] rowIsNull >>= (@?= [True, True, True])
        checked (c_duckdb_v2_vector_constant_set_valid vector (CBool 1))
        readBigints vector >>= (@?= [7, 7, 7])

testChildCount :: IO ()
testChildCount = withConnection $ \conn -> withFactory conn $ \factory -> do
    let kinds =
            [ ("BIGINT", 0)
            , ("VARCHAR", 0)
            , ("INTEGER[]", 1)
            , ("MAP(VARCHAR, INTEGER)", 1)
            , ("INTEGER[3]", 1)
            , ("STRUCT(a INTEGER, b VARCHAR, c DOUBLE)", 3)
            , ("UNION(i INTEGER, s VARCHAR)", 3)
            ]
    forM_ kinds $ \(text, count) -> withType factory text $ \ty -> withChunk factory [ty] $ \chunk -> do
        vector <- vectorAt chunk 0
        output (c_duckdb_v2_vector_get_child_count vector) >>= (@?= count)

testReference :: IO ()
testReference = withConnection $ \conn -> withFactory conn $ \factory ->
    withType factory "BIGINT" $ \integer -> withType factory "VARCHAR" $ \text ->
        withChunk factory [integer, integer, text] $ \chunk -> do
            target <- vectorAt chunk 0
            source <- vectorAt chunk 1
            writeBigints source [5, 6, 7]
            checked (c_duckdb_v2_vector_reference target source)
            readBigints target >>= (@?= [5, 6, 7])
            -- No data moves, so a write through the source shows through the target.
            raw <- output (c_duckdb_v2_vector_get_data_mutable source)
            pokeElemOff (coerce raw) 1 (60 :: Int64)
            readBigints target >>= (@?= [5, 60, 7])
            strings <- vectorAt chunk 2
            c_duckdb_v2_vector_reference target strings nullPtr >>= (@?= DuckDBV2ErrorInputInvalid)

testCollection :: IO ()
testCollection = withConnection $ \conn -> withFactory conn $ \factory -> withType factory "BIGINT" $ \integer -> withArray [integer] $ \types -> do
    let create = c_duckdb_v2_column_data_collection_create factory (ConstPtr types) 1
        rowCount collection = output (c_duckdb_v2_column_data_collection_row_count collection)
        append collection rows = withChunk factory [integer] $ \chunk -> do
            vectorAt chunk 0 >>= \vector -> writeBigints vector rows
            withHandle (c_duckdb_v2_column_data_collection_append_state_create collection) c_duckdb_v2_column_data_collection_append_state_destroy $ \state ->
                checked (c_duckdb_v2_column_data_collection_append collection state chunk)
    withHandle create c_duckdb_v2_column_data_collection_destroy $ \target -> do
        append target [1, 2, 3]
        -- Combine consumes the source and sets its slot to null.
        alloca $ \sourceSlot -> do
            poke sourceSlot (coerce nullPtr)
            checked (create sourceSlot)
            let destroyLeft = peek sourceSlot >>= \left -> unless (left == coerce nullPtr) (void (c_duckdb_v2_column_data_collection_destroy sourceSlot))
            flip finally destroyLeft $ do
                source <- peek sourceSlot
                append source [4, 5]
                checked (c_duckdb_v2_column_data_collection_combine target sourceSlot)
                peek sourceSlot >>= (@?= coerce nullPtr)
        rowCount target >>= (@?= 5)
        checked (c_duckdb_v2_column_data_collection_reset target)
        rowCount target >>= (@?= 0)
        append target [6, 7, 8]
        rowCount target >>= (@?= 3)
        checked (c_duckdb_v2_column_data_collection_clear target)
        rowCount target >>= (@?= 0)
        append target [9]
        rowCount target >>= (@?= 1)
