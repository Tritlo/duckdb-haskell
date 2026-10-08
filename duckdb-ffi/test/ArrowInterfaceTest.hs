{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

module ArrowInterfaceTest (tests) where

import Control.Exception (bracket, finally)
import Control.Monad (when)
import Data.Bits (testBit)
import Data.Coerce (coerce)
import Data.Int (Int32)
import Data.Proxy (Proxy (..))
import Data.Void (Void)
import Database.DuckDB.FFI
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.C.String (peekCString)
import Foreign.C.Types (CChar)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (peekArray, withArray)
import Foreign.Marshal.Utils (withMany)
import Foreign.Ptr (FunPtr, Ptr, freeHaskellFunPtr, nullFunPtr, nullPtr)
import Foreign.Storable (Storable (..), peek, peekElemOff, poke, pokeElemOff)
import GHC.Records (getField)
import HsBindgen.Runtime.HasCField (fromPtr)
import HsBindgen.Runtime.Support.FunPtr (fromFunPtr, toFunPtr)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import Utils (releaseArrowArray, releaseArrowSchema, releaseArrowStream, withConnection, withConstCString, withDatabase)

tests :: TestTree
tests =
    testGroup
        "Arrow Interface"
        [ arrowSchemaRoundtrip
        , arrowChunkRoundtrip
        , arrowStreamErrorCallbacks
        ]

-- | Invoke a producer's error callback after its next-array callback fails.
arrowStreamErrorCallbacks :: TestTree
arrowStreamErrorCallbacks =
    testCase "stream callbacks report a producer error" $
        withConstCString "Arrow producer failed" \message ->
            bracket (toFunPtr (\_ _ -> pure 5)) (freeHaskellFunPtr . coerce) \next ->
                bracket (toFunPtr (const (pure message))) (freeHaskellFunPtr . coerce) \lastError ->
                    bracket
                        ( toFunPtr \stream -> do
                            poke (fromPtr (Proxy @"release") (stream :: Ptr ArrowArrayStream)) (coerce (nullFunPtr :: FunPtr Void))
                        )
                        (freeHaskellFunPtr . coerce)
                        \release ->
                            alloca \stream -> do
                                poke stream (ArrowArrayStream (coerce (nullFunPtr :: FunPtr Void)) next lastError release (coerce (nullPtr :: Ptr Void)))
                                alloca \array -> do
                                    poke array zeroArrowArray
                                    fromFunPtr next stream array >>= (@?= 5)
                                    fromFunPtr lastError stream >>= (peekCString . coerce) >>= (@?= "Arrow producer failed")
                                releaseArrowStream stream
                                value <- peek stream
                                (getField @"release") value @?= (coerce (nullFunPtr :: FunPtr Void))
                                releaseArrowStream stream

arrowSchemaRoundtrip :: TestTree
arrowSchemaRoundtrip =
    testCase "to_arrow_schema exposes children and converts back" $
        withDatabase \db ->
            withConnection db \conn ->
                withArrowOptions conn \arrowOpts ->
                    withLogicalTypes [DUCKDB_TYPE_INTEGER, DUCKDB_TYPE_VARCHAR] \logicalTypes ->
                        withArray logicalTypes \logicalArray ->
                            withColumnNames ["id", "label"] \nameArray ->
                                withArrowSchema \schemaPtr -> do
                                    errData <- duckdb_to_arrow_schema arrowOpts logicalArray nameArray (fromIntegral (length logicalTypes)) (coerce schemaPtr)
                                    assertNoError errData

                                    schema <- peek schemaPtr
                                    formatStr <- (peekCString . coerce) ((getField @"format") schema)
                                    formatStr @?= "+s"
                                    (getField @"n_children") schema @?= fromIntegral (length logicalTypes)

                                    let childCount = fromIntegral ((getField @"n_children") schema)
                                    let childArrayPtr = (getField @"children") schema
                                    assertBool "children pointer should not be null" (childArrayPtr /= (coerce (nullPtr :: Ptr Void)))
                                    [fc, sc] <- peekArray childCount childArrayPtr >>= mapM peek
                                    (peekCString . coerce) ((getField @"name") fc) >>= (@?= "id")
                                    (peekCString . coerce) ((getField @"name") sc) >>= (@?= "label")
                                    assertBool "schema release pointer should be set" ((getField @"release") schema /= (coerce (nullFunPtr :: FunPtr Void)))

                                    withConvertedSchema conn schemaPtr (const (pure ()))

arrowChunkRoundtrip :: TestTree
arrowChunkRoundtrip =
    testCase "data_chunk_to_arrow and from_arrow preserve values and nulls" $
        withDatabase \db ->
            withConnection db \conn ->
                withArrowOptions conn \arrowOpts ->
                    withLogicalTypes [DUCKDB_TYPE_INTEGER] \logicalTypes ->
                        withArray logicalTypes \logicalArray ->
                            withColumnNames ["val"] \nameArray ->
                                withArrowSchema \schemaPtr -> do
                                    errSchema <- duckdb_to_arrow_schema arrowOpts logicalArray nameArray 1 (coerce schemaPtr)
                                    assertNoError errSchema

                                    withConvertedSchema conn schemaPtr \convertedSchema ->
                                        do
                                            chunk <- duckdb_create_data_chunk logicalArray 1
                                            assertBool "create_data_chunk should return chunk" (chunk /= (coerce (nullPtr :: Ptr Void)))

                                            withOwnedChunk chunk \ownedChunk -> do
                                                duckdb_data_chunk_set_size ownedChunk 2
                                                vector <- duckdb_data_chunk_get_vector ownedChunk 0
                                                dataPtr <- duckdb_vector_get_data vector
                                                let intPtr = coerce dataPtr :: Ptr Int32
                                                pokeElemOff intPtr 0 42
                                                pokeElemOff intPtr 1 0

                                                duckdb_vector_ensure_validity_writable vector
                                                maskPtr <- duckdb_vector_get_validity vector
                                                assertBool "validity mask pointer should not be null" (maskPtr /= (coerce (nullPtr :: Ptr Void)))
                                                duckdb_validity_set_row_valid maskPtr 0
                                                duckdb_validity_set_row_invalid maskPtr 1

                                                withArrowArray \arrayPtr ->
                                                    do
                                                        errArray <- duckdb_data_chunk_to_arrow arrowOpts ownedChunk (coerce arrayPtr)
                                                        assertNoError errArray

                                                        array <- peek arrayPtr
                                                        (getField @"length") array @?= 2
                                                        assertBool "release pointer should not be null before transfer" ((getField @"release") array /= (coerce (nullFunPtr :: FunPtr Void)))

                                                        alloca \outChunkPtr -> do
                                                            poke outChunkPtr (coerce (nullPtr :: Ptr Void))
                                                            errFromArrow <- duckdb_data_chunk_from_arrow conn (coerce arrayPtr) convertedSchema outChunkPtr
                                                            assertNoError errFromArrow
                                                            restoredChunk <- peek outChunkPtr
                                                            assertBool "restored chunk should not be null" (restoredChunk /= (coerce (nullPtr :: Ptr Void)))

                                                            withOwnedChunk restoredChunk $ \restored -> do
                                                                restoredSize <- duckdb_data_chunk_get_size restored
                                                                restoredSize @?= 2

                                                                restoredVector <- duckdb_data_chunk_get_vector restored 0
                                                                restoredDataPtr <- duckdb_vector_get_data restoredVector
                                                                let restoredIntPtr = coerce restoredDataPtr :: Ptr Int32
                                                                restoredVal <- peekElemOff restoredIntPtr 0
                                                                restoredVal @?= 42

                                                                restoredMaskPtr <- duckdb_vector_get_validity restoredVector
                                                                assertBool "restored validity mask pointer should not be null" (restoredMaskPtr /= (coerce (nullPtr :: Ptr Void)))
                                                                restoredMaskWord <- peek restoredMaskPtr
                                                                assertBool "first row should be valid" (testBit restoredMaskWord 0)
                                                                assertBool "second row should be null" (not (testBit restoredMaskWord 1))

                                                            arrayAfter <- peek arrayPtr
                                                            (getField @"release") arrayAfter @?= (coerce (nullFunPtr :: FunPtr Void))

withArrowOptions :: Duckdb_connection -> (Duckdb_arrow_options -> IO a) -> IO a
withArrowOptions conn action =
    alloca \optsPtr -> do
        let acquire = do
                poke optsPtr (coerce (nullPtr :: Ptr Void))
                duckdb_connection_get_arrow_options conn optsPtr
                opts <- peek optsPtr
                when (opts == (coerce (nullPtr :: Ptr Void))) $ assertFailure "duckdb_connection_get_arrow_options returned null"
                pure opts
            release _ = duckdb_destroy_arrow_options optsPtr
        bracket acquire release action

withLogicalTypes :: [DUCKDB_TYPE] -> ([Duckdb_logical_type] -> IO a) -> IO a
withLogicalTypes [] action = action []
withLogicalTypes (t : ts) action =
    bracket (duckdb_create_logical_type (Duckdb_type t)) destroyLogicalType \lt ->
        withLogicalTypes ts \rest -> action (lt : rest)

withColumnNames :: [String] -> (Ptr (ConstPtr CChar) -> IO a) -> IO a
withColumnNames names action =
    withMany withConstCString names $ \cNames -> withArray cNames action

withArrowSchema :: (Ptr ArrowSchema -> IO a) -> IO a
withArrowSchema action =
    withStruct zeroArrowSchema \ptr -> action ptr `finally` releaseArrowSchema ptr

withArrowArray :: (Ptr ArrowArray -> IO a) -> IO a
withArrowArray action =
    withStruct zeroArrowArray \ptr -> action ptr `finally` releaseArrowArray ptr

withStruct :: (Storable a) => a -> (Ptr a -> IO b) -> IO b
withStruct initial action =
    alloca \ptr -> do
        poke ptr initial
        action ptr

withOwnedChunk :: Duckdb_data_chunk -> (Duckdb_data_chunk -> IO a) -> IO a
withOwnedChunk chunk = bracket (pure chunk) destroyChunk

withConvertedSchema :: Duckdb_connection -> Ptr ArrowSchema -> (Duckdb_arrow_converted_schema -> IO a) -> IO a
withConvertedSchema conn schemaPtr action =
    alloca \convertedPtr -> do
        poke convertedPtr (coerce (nullPtr :: Ptr Void))
        err <- duckdb_schema_from_arrow conn (coerce schemaPtr) convertedPtr
        assertNoError err
        converted <- peek convertedPtr
        assertBool "converted schema pointer should not be null" (converted /= (coerce (nullPtr :: Ptr Void)))
        bracket (pure converted) destroyArrowConvertedSchema action

destroyLogicalType :: Duckdb_logical_type -> IO ()
destroyLogicalType lt =
    alloca \ptr -> do
        poke ptr lt
        duckdb_destroy_logical_type ptr

destroyChunk :: Duckdb_data_chunk -> IO ()
destroyChunk chunk =
    alloca \ptr -> do
        poke ptr chunk
        duckdb_destroy_data_chunk ptr

destroyArrowConvertedSchema :: Duckdb_arrow_converted_schema -> IO ()
destroyArrowConvertedSchema schema =
    alloca \ptr -> do
        poke ptr schema
        duckdb_destroy_arrow_converted_schema ptr

destroyErrorData :: Duckdb_error_data -> IO ()
destroyErrorData err =
    alloca \ptr -> do
        poke ptr err
        duckdb_destroy_error_data ptr

assertNoError :: Duckdb_error_data -> IO ()
assertNoError err =
    when (err /= (coerce (nullPtr :: Ptr Void))) $ do
        msgPtr <- duckdb_error_data_message err
        msg <- (peekCString . coerce) msgPtr
        destroyErrorData err
        assertFailure ("DuckDB reported error: " <> msg)

zeroArrowSchema :: ArrowSchema
zeroArrowSchema =
    ArrowSchema
        { format = (coerce (nullPtr :: Ptr Void))
        , name = (coerce (nullPtr :: Ptr Void))
        , metadata = (coerce (nullPtr :: Ptr Void))
        , flags = 0
        , n_children = 0
        , children = (coerce (nullPtr :: Ptr Void))
        , dictionary = (coerce (nullPtr :: Ptr Void))
        , release = (coerce (nullFunPtr :: FunPtr Void))
        , private_data = (coerce (nullPtr :: Ptr Void))
        }

zeroArrowArray :: ArrowArray
zeroArrowArray =
    ArrowArray
        { length = 0
        , null_count = 0
        , offset = 0
        , n_buffers = 0
        , n_children = 0
        , buffers = (coerce (nullPtr :: Ptr Void))
        , children = (coerce (nullPtr :: Ptr Void))
        , dictionary = (coerce (nullPtr :: Ptr Void))
        , release = (coerce (nullFunPtr :: FunPtr Void))
        , private_data = (coerce (nullPtr :: Ptr Void))
        }
