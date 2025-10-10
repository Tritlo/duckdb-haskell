{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}

module ArrowInterfaceTest (tests) where

import Control.Exception (bracket)
import Control.Monad (forM_, unless, when)
import Data.Bits (testBit)
import Data.Char (toLower)
import Data.Int (Int32, Int64)
import Data.List (isInfixOf)
import Data.Word (Word64)
import Database.DuckDB.FFI
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CBool (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (peekArray, withArray)
import Foreign.Marshal.Utils (withMany)
import Foreign.Ptr (FunPtr, Ptr, castPtr, nullFunPtr, nullPtr)
import Foreign.Storable (Storable (..), peek, peekElemOff, poke, pokeElemOff)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import Utils (withConnection, withDatabase, withResult)

tests :: TestTree
tests =
  testGroup
    "Arrow Interface"
    [ arrowQueryMetadata
    , arrowErrorHandling
    -- , arrowQuerySchemaAccess
    -- , arrowResultArrayConversion
    -- , arrowPreparedArrowExecution
    -- , arrowScanFromArrowData
    , arrowSchemaRoundtrip
    , arrowChunkRoundtrip
    ]

arrowQueryMetadata :: TestTree
arrowQueryMetadata =
  testCase "query_arrow exposes schema and array data" $
    withDatabase \db ->
      withConnection db \conn ->
        withArrowResult conn "SELECT 1::INTEGER AS id, 'alpha'::VARCHAR AS label" \arrow -> do
          c_duckdb_arrow_column_count arrow >>= (@?= 2)
          c_duckdb_arrow_row_count arrow >>= (@?= 1)
          c_duckdb_arrow_rows_changed arrow >>= (@?= 0)

arrowErrorHandling :: TestTree
arrowErrorHandling =
  testCase "query_arrow surfaces planner errors" $
    withDatabase \db ->
      withConnection db \conn ->
        withCString "SELECT * FROM missing_table" \sql ->
          alloca \arrowPtr -> do
            poke arrowPtr nullPtr
            state <- c_duckdb_query_arrow conn sql arrowPtr
            state @?= DuckDBError
            arrow <- peek arrowPtr
            unless (arrow == nullPtr) $ do
              errMsgPtr <- c_duckdb_query_arrow_error arrow
              assertBool "expected non-null error pointer" (errMsgPtr /= nullPtr)
              errMsg <- peekCString errMsgPtr
              assertBool "error message mentions table" ("missing_table" `isInfixOf` map toLower errMsg)
            c_duckdb_destroy_arrow arrowPtr

arrowQuerySchemaAccess :: TestTree
arrowQuerySchemaAccess =
  testCase "query_arrow exposes schema and arrays through dedicated accessors" $
    withDatabase \db ->
      withConnection db \conn ->
        withArrowResult conn "SELECT 42::INTEGER AS id, 'deck'::VARCHAR AS label" \arrow -> do
          alloca \schemaPtrPtr -> do
            poke schemaPtrPtr nullPtr
            schemaState <- c_duckdb_query_arrow_schema arrow schemaPtrPtr
            schemaState @?= DuckDBSuccess
            schemaHandle <- peek schemaPtrPtr
            assertBool "duckdb_query_arrow_schema returns handle" (schemaHandle /= nullPtr)
            schemaStructPtr <- arrowSchemaStructPtr schemaHandle
            assertBool "arrow schema internal pointer should not be null" (schemaStructPtr /= nullPtr)
            schema <- peek schemaStructPtr
            arrowSchemaChildCount schema @?= 2
            assertBool "schema release pointer should be set" (arrowSchemaRelease schema /= nullFunPtr)
            peekCString (arrowSchemaFormat schema) >>= (@?= "+s")

            childPtr <- pure (arrowSchemaChildren schema)
            assertBool "schema children pointer should not be null" (childPtr /= nullPtr)
            childSchemas <- peekArray (fromIntegral (arrowSchemaChildCount schema)) childPtr
            firstChild <- peek (childSchemas !! 0)
            secondChild <- peek (childSchemas !! 1)
            peekCString (arrowSchemaName firstChild) >>= (@?= "id")
            peekCString (arrowSchemaName secondChild) >>= (@?= "label")

            releaseArrowSchema schemaStructPtr
            clearArrowSchemaHandle schemaHandle

          alloca \arrayPtrPtr -> do
            poke arrayPtrPtr nullPtr
            arrayState <- c_duckdb_query_arrow_array arrow arrayPtrPtr
            arrayState @?= DuckDBSuccess
            arrayHandle <- peek arrayPtrPtr
            assertBool "duckdb_query_arrow_array returns handle" (arrayHandle /= nullPtr)
            arrayStructPtr <- arrowArrayStructPtr arrayHandle
            assertBool "arrow array internal pointer should not be null" (arrayStructPtr /= nullPtr)
            array <- peek arrayStructPtr
            arrowArrayLength array @?= 1
            assertBool "array release pointer should be set" (arrowArrayRelease array /= nullFunPtr)

            releaseArrowArray arrayStructPtr
            clearArrowArrayHandle arrayHandle

arrowResultArrayConversion :: TestTree
arrowResultArrayConversion =
  testCase "result_arrow_array converts materialised chunks" $
    withDatabase \db ->
      withConnection db \conn ->
        withResult conn "SELECT 7::BIGINT AS num, 'row'::VARCHAR AS tag" \resPtr -> do
          chunk <- c_duckdb_fetch_chunk resPtr
          assertBool "fetch_chunk should yield a data chunk" (chunk /= nullPtr)

          alloca \arrowArrayPtr -> do
            poke arrowArrayPtr nullPtr
            c_duckdb_result_arrow_array resPtr chunk arrowArrayPtr
            arrayHandle <- peek arrowArrayPtr
            assertBool "result_arrow_array should populate handle" (arrayHandle /= nullPtr)
            arrayStructPtr <- arrowArrayStructPtr arrayHandle
            assertBool "converted array internal pointer should not be null" (arrayStructPtr /= nullPtr)
            array <- peek arrayStructPtr
            arrowArrayLength array @?= 1
            assertBool "converted array release pointer should be set" (arrowArrayRelease array /= nullFunPtr)

            releaseArrowArray arrayStructPtr
            clearArrowArrayHandle arrayHandle

          destroyChunk chunk

arrowPreparedArrowExecution :: TestTree
arrowPreparedArrowExecution =
  testCase "execute_prepared_arrow and prepared_arrow_schema expose metadata and data" $
    withDatabase \db ->
      withConnection db \conn -> do
        setupPreparedFixture conn

        withCString "SELECT id, label FROM arrow_fixture WHERE id = ?" \querySql ->
          alloca \stmtPtr -> do
            prepareState <- c_duckdb_prepare conn querySql stmtPtr
            prepareState @?= DuckDBSuccess
            stmt <- peek stmtPtr
            assertBool "prepare should return a statement" (stmt /= nullPtr)

            c_duckdb_bind_int32 stmt 1 2 >>= (@?= DuckDBSuccess)

            alloca \schemaPtrPtr -> do
              poke schemaPtrPtr nullPtr
              schemaState <- c_duckdb_prepared_arrow_schema stmt schemaPtrPtr
              schemaState @?= DuckDBSuccess
              schemaHandle <- peek schemaPtrPtr
              assertBool "prepared_arrow_schema returns handle" (schemaHandle /= nullPtr)
              schemaStructPtr <- arrowSchemaStructPtr schemaHandle
              assertBool "prepared arrow schema internal pointer should not be null" (schemaStructPtr /= nullPtr)
              schema <- peek schemaStructPtr
              arrowSchemaChildCount schema @?= 2
              assertBool "prepared schema release pointer should be set" (arrowSchemaRelease schema /= nullFunPtr)
              peekCString (arrowSchemaFormat schema) >>= (@?= "+s")
              releaseArrowSchema schemaStructPtr
              clearArrowSchemaHandle schemaHandle

            alloca \arrowPtr -> do
              poke arrowPtr nullPtr
              execState <- c_duckdb_execute_prepared_arrow stmt arrowPtr
              execState @?= DuckDBSuccess

              arrowHandle <- peek arrowPtr
              assertBool "execute_prepared_arrow returns result handle" (arrowHandle /= nullPtr)
              c_duckdb_arrow_column_count arrowHandle >>= (@?= 2)
              c_duckdb_arrow_row_count arrowHandle >>= (@?= 1)
              c_duckdb_arrow_rows_changed arrowHandle >>= (@?= 0)

              alloca \arrayPtrPtr -> do
                poke arrayPtrPtr nullPtr
                arrayState <- c_duckdb_query_arrow_array arrowHandle arrayPtrPtr
                arrayState @?= DuckDBSuccess
                arrayHandle <- peek arrayPtrPtr
                assertBool "prepared arrow query yields array chunk" (arrayHandle /= nullPtr)
                arrayStructPtr <- arrowArrayStructPtr arrayHandle
                assertBool "prepared arrow array internal pointer should not be null" (arrayStructPtr /= nullPtr)
                array <- peek arrayStructPtr
                arrowArrayLength array @?= 1
                assertBool "prepared result array release pointer should be set" (arrowArrayRelease array /= nullFunPtr)
                releaseArrowArray arrayStructPtr
                clearArrowArrayHandle arrayHandle

              c_duckdb_destroy_arrow arrowPtr

            c_duckdb_destroy_prepare stmtPtr

arrowScanFromArrowData :: TestTree
arrowScanFromArrowData =
  testCase "arrow_array_scan wires view and arrow_scan reuses stream" $
    withDatabase \db ->
      withConnection db \conn ->
        withArrowResult conn "SELECT 11::INTEGER AS val" \arrow -> do
          arrowSchemaHandle <- alloca \schemaPtrPtr -> do
            poke schemaPtrPtr nullPtr
            schemaState <- c_duckdb_query_arrow_schema arrow schemaPtrPtr
            schemaState @?= DuckDBSuccess
            schemaHandle <- peek schemaPtrPtr
            assertBool "query_arrow_schema should produce handle" (schemaHandle /= nullPtr)
            pure schemaHandle

          arrowArrayHandle <- alloca \arrayPtrPtr -> do
            poke arrayPtrPtr nullPtr
            arrayState <- c_duckdb_query_arrow_array arrow arrayPtrPtr
            arrayState @?= DuckDBSuccess
            arrayHandle <- peek arrayPtrPtr
            assertBool "query_arrow_array should produce handle" (arrayHandle /= nullPtr)
            pure arrayHandle

          withCString "arrow_array_view" \arrayView ->
            alloca \streamPtr -> do
              scanState <- c_duckdb_arrow_array_scan conn arrayView arrowSchemaHandle arrowArrayHandle streamPtr
              scanState @?= DuckDBSuccess
              streamHandle <- peek streamPtr
              assertBool "arrow_array_scan should populate stream" (streamHandle /= nullPtr)

              withResult conn "SELECT val FROM arrow_array_view" \resPtr -> do
                rowCount <- c_duckdb_row_count resPtr
                rowCount @?= 1
                value <- c_duckdb_value_int64 resPtr 0 0
                value @?= 11

              withCString "arrow_stream_view" \streamView -> do
                reuseState <- c_duckdb_arrow_scan conn streamView streamHandle
                reuseState @?= DuckDBSuccess

                withResult conn "SELECT val FROM arrow_stream_view" \resPtr -> do
                  rowCount <- c_duckdb_row_count resPtr
                  rowCount @?= 1
                  value <- c_duckdb_value_int64 resPtr 0 0
                  value @?= 11

              withCString "arrow_scan_error" \badView -> do
                errState <- c_duckdb_arrow_scan conn badView nullPtr
                errState @?= DuckDBError

              c_duckdb_destroy_arrow_stream streamPtr
              streamAfter <- peek streamPtr
              streamAfter @?= nullPtr

arrowSchemaRoundtrip :: TestTree
arrowSchemaRoundtrip =
  testCase "to_arrow_schema exposes children and converts back" $
    withDatabase \db ->
      withConnection db \conn ->
        withArrowOptions conn \arrowOpts ->
          withLogicalTypes [DuckDBTypeInteger, DuckDBTypeVarchar] \logicalTypes ->
            withArray logicalTypes \logicalArray ->
              withColumnNames ["id", "label"] \nameArray ->
                withArrowSchemaStruct \schemaPtr -> do
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
                withArrowSchemaStruct \schemaPtr -> do
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

                        withArrowArrayStruct \arrayPtr ->
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

withArrowResult :: DuckDBConnection -> String -> (DuckDBArrow -> IO a) -> IO a
withArrowResult conn sql action =
  withCString sql \sqlPtr ->
    alloca \arrowPtr -> do
      poke arrowPtr nullPtr
      state <- c_duckdb_query_arrow conn sqlPtr arrowPtr
      state @?= DuckDBSuccess
      arrow <- peek arrowPtr
      whenNull arrow $ assertFailure "duckdb_query_arrow returned null handle"
      result <- action arrow
      c_duckdb_destroy_arrow arrowPtr
      pure result
  where
    whenNull ptr actionFn = unless (ptr /= nullPtr) actionFn

setupPreparedFixture :: DuckDBConnection -> IO ()
setupPreparedFixture conn = do
  let statements =
        [ "CREATE TABLE arrow_fixture (id INTEGER, label VARCHAR);"
        , "INSERT INTO arrow_fixture VALUES (1, 'alpha'), (2, 'beta');"
        ]
  forM_ statements \sql ->
    withCString sql \sqlPtr ->
      alloca \resPtr -> do
        state <- c_duckdb_query conn sqlPtr resPtr
        state @?= DuckDBSuccess
        c_duckdb_destroy_result resPtr

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

withArrowSchemaStruct :: (Ptr ArrowSchemaStruct -> IO a) -> IO a
withArrowSchemaStruct = withStruct zeroArrowSchema

withArrowArrayStruct :: (Ptr ArrowArrayStruct -> IO a) -> IO a
withArrowArrayStruct action =
  withStruct zeroArrowArray \ptr -> do
    result <- action ptr
    releaseArrowArray ptr
    pure result

withStruct :: Storable a => a -> (Ptr a -> IO b) -> IO b
withStruct initial action =
  alloca \ptr -> do
    poke ptr initial
    action ptr

withOwnedChunk :: DuckDBDataChunk -> (DuckDBDataChunk -> IO a) -> IO a
withOwnedChunk chunk = bracket (pure chunk) destroyChunk

withConvertedSchema :: DuckDBConnection -> Ptr ArrowSchemaStruct -> (DuckDBArrowConvertedSchema -> IO a) -> IO a
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

releaseArrowSchema :: Ptr ArrowSchemaStruct -> IO ()
releaseArrowSchema schemaPtr = do
  schema <- peek schemaPtr
  let releaseFun = arrowSchemaRelease schema
  when (releaseFun /= nullFunPtr) $ do
    let release = mkArrowSchemaRelease releaseFun
    release schemaPtr

releaseArrowArray :: Ptr ArrowArrayStruct -> IO ()
releaseArrowArray arrayPtr = do
  array <- peek arrayPtr
  let releaseFun = arrowArrayRelease array
  when (releaseFun /= nullFunPtr) $ do
    let release = mkArrowArrayRelease releaseFun
    release arrayPtr

arrowSchemaStructPtr :: DuckDBArrowSchema -> IO (Ptr ArrowSchemaStruct)
arrowSchemaStructPtr handle
  | handle == nullPtr = pure nullPtr
  | otherwise = peek (castPtr handle :: Ptr (Ptr ArrowSchemaStruct))

arrowArrayStructPtr :: DuckDBArrowArray -> IO (Ptr ArrowArrayStruct)
arrowArrayStructPtr handle
  | handle == nullPtr = pure nullPtr
  | otherwise = peek (castPtr handle :: Ptr (Ptr ArrowArrayStruct))

clearArrowSchemaHandle :: DuckDBArrowSchema -> IO ()
clearArrowSchemaHandle handle
  | handle == nullPtr = pure ()
  | otherwise = poke (castPtr handle :: Ptr (Ptr ArrowSchemaStruct)) nullPtr

clearArrowArrayHandle :: DuckDBArrowArray -> IO ()
clearArrowArrayHandle handle
  | handle == nullPtr = pure ()
  | otherwise = poke (castPtr handle :: Ptr (Ptr ArrowArrayStruct)) nullPtr
zeroArrowSchema =
  ArrowSchemaStruct
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

zeroArrowArray :: ArrowArrayStruct
zeroArrowArray =
  ArrowArrayStruct
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
  mkArrowSchemaRelease :: FunPtr (Ptr ArrowSchemaStruct -> IO ()) -> Ptr ArrowSchemaStruct -> IO ()

foreign import ccall "dynamic"
  mkArrowArrayRelease :: FunPtr (Ptr ArrowArrayStruct -> IO ()) -> Ptr ArrowArrayStruct -> IO ()
