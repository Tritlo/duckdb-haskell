{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module ArrowInterfaceTest (tests) where

import Control.Exception (bracket)
import Control.Monad (when)
import Data.Bits (testBit)
import Data.Int (Int32)
import Database.DuckDB.FFI
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (peekArray, withArray)
import Foreign.Marshal.Utils (withMany)
import Foreign.Ptr (FunPtr, Ptr, castPtr, nullFunPtr, nullPtr)
import Foreign.Storable (Storable (..), peek, peekElemOff, poke, pokeElemOff)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import Utils (withConnection, withDatabase)

tests :: TestTree
tests =
    testGroup
        "Arrow Interface"
        [ arrowSchemaRoundtrip
        , arrowChunkRoundtrip
        ]

arrowSchemaRoundtrip :: TestTree
arrowSchemaRoundtrip =
    testCase "to_arrow_schema exposes children and converts back" $
        withDatabase \db ->
            withConnection db \conn ->
                withArrowOptions conn \arrowOpts ->
                    withLogicalTypes [DuckDBTypeInteger, DuckDBTypeVarchar] \logicalTypes ->
                        withArray logicalTypes \logicalArray ->
                            withColumnNames ["id", "label"] \nameArray ->
                                withArrowSchema \schemaPtr -> do
                                    errData <- c_duckdb_to_arrow_schema arrowOpts logicalArray nameArray (fromIntegral (length logicalTypes)) (castPtr schemaPtr)
                                    assertNoError errData

                                    schema <- peek schemaPtr
                                    formatStr <- peekCString (arrowSchemaFormat schema)
                                    formatStr @?= "+s"
                                    arrowSchemaChildCount schema @?= fromIntegral (length logicalTypes)

                                    let childCount = fromIntegral (arrowSchemaChildCount schema)
                                    childArrayPtr <- pure (arrowSchemaChildren schema)
                                    assertBool "children pointer should not be null" (childArrayPtr /= nullPtr)
                                    childPtrs <- peekArray childCount childArrayPtr

                                    firstChild <- peek (childPtrs !! 0)
                                    secondChild <- peek (childPtrs !! 1)
                                    peekCString (arrowSchemaName firstChild) >>= (@?= "id")
                                    peekCString (arrowSchemaName secondChild) >>= (@?= "label")
                                    assertBool "schema release pointer should be set" (arrowSchemaRelease schema /= nullFunPtr)

                                    withConvertedSchema conn schemaPtr (const (pure ()))

                                    releaseArrowSchema schemaPtr

arrowChunkRoundtrip :: TestTree
arrowChunkRoundtrip =
    testCase "data_chunk_to_arrow and from_arrow preserve values and nulls" $
        withDatabase \db ->
            withConnection db \conn ->
                withArrowOptions conn \arrowOpts ->
                    withLogicalTypes [DuckDBTypeInteger] \logicalTypes ->
                        withArray logicalTypes \logicalArray ->
                            withColumnNames ["val"] \nameArray ->
                                withArrowSchema \schemaPtr -> do
                                    errSchema <- c_duckdb_to_arrow_schema arrowOpts logicalArray nameArray 1 (castPtr schemaPtr)
                                    assertNoError errSchema

                                    withConvertedSchema conn schemaPtr \convertedSchema ->
                                        do
                                            chunk <- c_duckdb_create_data_chunk logicalArray 1
                                            assertBool "create_data_chunk should return chunk" (chunk /= nullPtr)

                                            withOwnedChunk chunk \ownedChunk -> do
                                                c_duckdb_data_chunk_set_size ownedChunk 2
                                                vector <- c_duckdb_data_chunk_get_vector ownedChunk 0
                                                dataPtr <- c_duckdb_vector_get_data vector
                                                let intPtr = castPtr dataPtr :: Ptr Int32
                                                pokeElemOff intPtr 0 42
                                                pokeElemOff intPtr 1 0

                                                c_duckdb_vector_ensure_validity_writable vector
                                                maskPtr <- c_duckdb_vector_get_validity vector
                                                assertBool "validity mask pointer should not be null" (maskPtr /= nullPtr)
                                                c_duckdb_validity_set_row_valid maskPtr 0
                                                c_duckdb_validity_set_row_invalid maskPtr 1

                                                withArrowArray \arrayPtr ->
                                                    do
                                                        errArray <- c_duckdb_data_chunk_to_arrow arrowOpts ownedChunk (castPtr arrayPtr)
                                                        assertNoError errArray

                                                        array <- peek arrayPtr
                                                        arrowArrayLength array @?= 2
                                                        assertBool "release pointer should not be null before transfer" (arrowArrayRelease array /= nullFunPtr)

                                                        alloca \outChunkPtr -> do
                                                            poke outChunkPtr nullPtr
                                                            errFromArrow <- c_duckdb_data_chunk_from_arrow conn (castPtr arrayPtr) convertedSchema outChunkPtr
                                                            assertNoError errFromArrow
                                                            restoredChunk <- peek outChunkPtr
                                                            assertBool "restored chunk should not be null" (restoredChunk /= nullPtr)

                                                            withOwnedChunk restoredChunk $ \restored -> do
                                                                restoredSize <- c_duckdb_data_chunk_get_size restored
                                                                restoredSize @?= 2

                                                                restoredVector <- c_duckdb_data_chunk_get_vector restored 0
                                                                restoredDataPtr <- c_duckdb_vector_get_data restoredVector
                                                                let restoredIntPtr = castPtr restoredDataPtr :: Ptr Int32
                                                                restoredVal <- peekElemOff restoredIntPtr 0
                                                                restoredVal @?= 42

                                                                restoredMaskPtr <- c_duckdb_vector_get_validity restoredVector
                                                                assertBool "restored validity mask pointer should not be null" (restoredMaskPtr /= nullPtr)
                                                                restoredMaskWord <- peek restoredMaskPtr
                                                                assertBool "first row should be valid" (testBit restoredMaskWord 0)
                                                                assertBool "second row should be null" (not (testBit restoredMaskWord 1))

                                                            arrayAfter <- peek arrayPtr
                                                            arrowArrayRelease arrayAfter @?= nullFunPtr

                                    releaseArrowSchema schemaPtr

withArrowOptions :: DuckDBConnection -> (DuckDBArrowOptions -> IO a) -> IO a
withArrowOptions conn action =
    alloca \optsPtr -> do
        let acquire = do
                poke optsPtr nullPtr
                c_duckdb_connection_get_arrow_options conn optsPtr
                opts <- peek optsPtr
                when (opts == nullPtr) $ assertFailure "duckdb_connection_get_arrow_options returned null"
                pure opts
            release _ = c_duckdb_destroy_arrow_options optsPtr
        bracket acquire release action

withLogicalTypes :: [DuckDBType] -> ([DuckDBLogicalType] -> IO a) -> IO a
withLogicalTypes [] action = action []
withLogicalTypes (t : ts) action =
    bracket (c_duckdb_create_logical_type t) destroyLogicalType \lt ->
        withLogicalTypes ts \rest -> action (lt : rest)

withColumnNames :: [String] -> (Ptr CString -> IO a) -> IO a
withColumnNames names action =
    withMany withCString names \cNames -> withArray cNames action

withArrowSchema :: (Ptr ArrowSchema -> IO a) -> IO a
withArrowSchema = withStruct zeroArrowSchema

withArrowArray :: (Ptr ArrowArray -> IO a) -> IO a
withArrowArray action =
    withStruct zeroArrowArray \ptr -> do
        result <- action ptr
        releaseArrowArray ptr
        pure result

withStruct :: (Storable a) => a -> (Ptr a -> IO b) -> IO b
withStruct initial action =
    alloca \ptr -> do
        poke ptr initial
        action ptr

withOwnedChunk :: DuckDBDataChunk -> (DuckDBDataChunk -> IO a) -> IO a
withOwnedChunk chunk = bracket (pure chunk) destroyChunk

withConvertedSchema :: DuckDBConnection -> Ptr ArrowSchema -> (DuckDBArrowConvertedSchema -> IO a) -> IO a
withConvertedSchema conn schemaPtr action =
    alloca \convertedPtr -> do
        poke convertedPtr nullPtr
        err <- c_duckdb_schema_from_arrow conn (castPtr schemaPtr) convertedPtr
        assertNoError err
        converted <- peek convertedPtr
        assertBool "converted schema pointer should not be null" (converted /= nullPtr)
        bracket (pure converted) destroyArrowConvertedSchema action

destroyLogicalType :: DuckDBLogicalType -> IO ()
destroyLogicalType lt =
    alloca \ptr -> do
        poke ptr lt
        c_duckdb_destroy_logical_type ptr

destroyChunk :: DuckDBDataChunk -> IO ()
destroyChunk chunk =
    alloca \ptr -> do
        poke ptr chunk
        c_duckdb_destroy_data_chunk ptr

destroyArrowConvertedSchema :: DuckDBArrowConvertedSchema -> IO ()
destroyArrowConvertedSchema schema =
    alloca \ptr -> do
        poke ptr schema
        c_duckdb_destroy_arrow_converted_schema ptr

destroyErrorData :: DuckDBErrorData -> IO ()
destroyErrorData err =
    alloca \ptr -> do
        poke ptr err
        c_duckdb_destroy_error_data ptr

assertNoError :: DuckDBErrorData -> IO ()
assertNoError err =
    when (err /= nullPtr) $ do
        msgPtr <- c_duckdb_error_data_message err
        msg <- peekCString msgPtr
        destroyErrorData err
        assertFailure ("DuckDB reported error: " <> msg)

releaseArrowSchema :: Ptr ArrowSchema -> IO ()
releaseArrowSchema schemaPtr = do
    schema <- peek schemaPtr
    let releaseFun = arrowSchemaRelease schema
    when (releaseFun /= nullFunPtr) $ do
        let release = mkArrowSchemaRelease releaseFun
        release schemaPtr

releaseArrowArray :: Ptr ArrowArray -> IO ()
releaseArrowArray arrayPtr = do
    array <- peek arrayPtr
    let releaseFun = arrowArrayRelease array
    when (releaseFun /= nullFunPtr) $ do
        let release = mkArrowArrayRelease releaseFun
        release arrayPtr

zeroArrowSchema :: ArrowSchema
zeroArrowSchema =
    ArrowSchema
        { arrowSchemaFormat = nullPtr
        , arrowSchemaName = nullPtr
        , arrowSchemaMetadata = nullPtr
        , arrowSchemaFlags = 0
        , arrowSchemaChildCount = 0
        , arrowSchemaChildren = nullPtr
        , arrowSchemaDictionary = nullPtr
        , arrowSchemaRelease = nullFunPtr
        , arrowSchemaPrivateData = nullPtr
        }

zeroArrowArray :: ArrowArray
zeroArrowArray =
    ArrowArray
        { arrowArrayLength = 0
        , arrowArrayNullCount = 0
        , arrowArrayOffset = 0
        , arrowArrayBufferCount = 0
        , arrowArrayChildCount = 0
        , arrowArrayBuffers = nullPtr
        , arrowArrayChildren = nullPtr
        , arrowArrayDictionary = nullPtr
        , arrowArrayRelease = nullFunPtr
        , arrowArrayPrivateData = nullPtr
        }

foreign import ccall "dynamic"
    mkArrowSchemaRelease :: FunPtr (Ptr ArrowSchema -> IO ()) -> Ptr ArrowSchema -> IO ()

foreign import ccall "dynamic"
    mkArrowArrayRelease :: FunPtr (Ptr ArrowArray -> IO ()) -> Ptr ArrowArray -> IO ()
