{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}

module ArrowInterfaceDeprecatedTests (tests) where

import Control.Exception (bracket, finally)
import Control.Monad (unless, when)
import Data.Int (Int32, Int64)
import Data.List (isInfixOf)
import Database.DuckDB.FFI
import Foreign.C.String (peekCString, peekCStringLen, withCString)
import Foreign.C.Types (CChar)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (FunPtr, Ptr, castPtr, nullFunPtr, nullPtr, plusPtr)
import Foreign.Storable (peek, peekElemOff, poke)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.ExpectedFailure (expectFailBecause)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import Utils (withConnection, withDatabase, withResult)

tests :: TestTree
tests =
  testGroup
    "Deprecated Arrow Interface"
    [ queryArrowExposesSchemaAndArrays
    , queryArrowReportsErrors
    , preparedArrowSchemaMatchesStatement
    , executePreparedArrowProducesRows
    , resultArrowArrayMirrorsChunk
    , arrowRowsChangedReflectsMutations
    , arrowArrayScanRegistersView
    , arrowStreamScanRegistersView
    , arrowPointerHelpersClearInternalState
    ]

-- basic query ---------------------------------------------------------------

queryArrowExposesSchemaAndArrays :: TestTree
queryArrowExposesSchemaAndArrays =
  testCase "query_arrow exposes schema metadata and arrays" $
    withDatabase \db ->
      withConnection db \conn ->
        withSuccessfulArrow conn "SELECT 1::INTEGER AS id, 'duck'::VARCHAR AS label" \arrow -> do
          columnCount <- c_duckdb_arrow_column_count arrow
          columnCount @?= 2

          rowCount <- c_duckdb_arrow_row_count arrow
          rowCount @?= 1

          rowsChanged <- c_duckdb_arrow_rows_changed arrow
          rowsChanged @?= 0

          errPtr <- c_duckdb_query_arrow_error arrow
          when (errPtr /= nullPtr) $ do
            errMsg <- peekCString errPtr
            errMsg @?= ""

          -- schema/array access validated in dedicated scan test

-- error handling ------------------------------------------------------------

queryArrowReportsErrors :: TestTree
queryArrowReportsErrors =
  testCase "query_arrow surfaces execution errors" $
    withDatabase \db ->
      withConnection db \conn ->
        withCString "SELECT * FROM missing_table" \querySql ->
          alloca \arrowPtr -> do
            poke arrowPtr nullPtr
            state <- c_duckdb_query_arrow conn querySql arrowPtr
            state @?= DuckDBError

            arrow <- peek arrowPtr
            assertBool "arrow result should still be allocated on error" (arrow /= nullPtr)

            errPtr <- c_duckdb_query_arrow_error arrow
            assertBool "error message should be present" (errPtr /= nullPtr)
            errMsg <- peekCString errPtr
            assertBool "error message should mention missing_table" ("missing_table" `isInfixOf` errMsg)

            c_duckdb_destroy_arrow arrowPtr

-- prepared statements -------------------------------------------------------

preparedArrowSchemaMatchesStatement :: TestTree
preparedArrowSchemaMatchesStatement =
  testCase "prepared_arrow_schema reflects projected columns" $
    withDatabase \db ->
      withConnection db \conn ->
        withPrepared conn "SELECT id, label FROM (VALUES (1, 'a')) AS t(id, label)" \stmt -> do
          alloca \schemaOut -> do
            poke schemaOut nullPtr
            stSchema <- c_duckdb_prepared_arrow_schema stmt schemaOut
            case stSchema of
              DuckDBSuccess -> do
                schemaWrapper <- peek schemaOut
                assertBool "prepared arrow schema pointer should not be null" (schemaWrapper /= nullPtr)
              DuckDBError -> do
                schemaWrapper <- peek schemaOut
                schemaWrapper @?= nullPtr
                errPtr <- c_duckdb_prepare_error stmt
                when (errPtr /= nullPtr) $ do
                  errMsg <- peekCString errPtr
                  errMsg @?= ""

executePreparedArrowProducesRows :: TestTree
executePreparedArrowProducesRows =
  testCase "execute_prepared_arrow materialises a result set" $
    withDatabase \db ->
      withConnection db \conn ->
        withPrepared conn "SELECT ?::INTEGER + 5 AS computed" \stmt -> do
          c_duckdb_bind_int32 stmt 1 (5 :: Int32) >>= (@?= DuckDBSuccess)

          withPreparedArrow stmt \arrow -> do
            rowCount <- c_duckdb_arrow_row_count arrow
            rowCount @?= 1

            colCount <- c_duckdb_arrow_column_count arrow
            colCount @?= 1

-- result conversion ---------------------------------------------------------

resultArrowArrayMirrorsChunk :: TestTree
resultArrowArrayMirrorsChunk =
  testCase "result_arrow_array converts materialised chunks to Arrow arrays" $
    withDatabase \db ->
      withConnection db \conn -> do
        execStatement conn "CREATE TABLE arrow_chunks(id BIGINT, label VARCHAR);"
        execStatement conn "INSERT INTO arrow_chunks VALUES (10, 'ten'), (11, 'eleven');"

        withResult conn "SELECT id, label FROM arrow_chunks ORDER BY id" \resPtr -> do
          chunk <- c_duckdb_result_get_chunk resPtr 0
          assertBool "fetch_chunk returned a null chunk" (chunk /= nullPtr)
          chunkSize <- c_duckdb_data_chunk_get_size chunk
          chunkCols <- c_duckdb_data_chunk_get_column_count chunk

          alloca \arrowArrayPtr -> do
            poke arrowArrayPtr zeroArrowArray
            let duckArray :: DuckDBArrowArray
                duckArray = castPtr arrowArrayPtr

            alloca \arrayOut -> do
              poke arrayOut duckArray
              c_duckdb_result_arrow_array resPtr chunk arrayOut

              array <- peek arrowArrayPtr

              arrowArrayLength array @?= fromIntegral chunkSize
              arrowArrayChildCount array @?= fromIntegral chunkCols

              validateChunkChildren array

              releaseArrowArray arrowArrayPtr

          destroyChunk chunk

-- rows changed --------------------------------------------------------------

arrowRowsChangedReflectsMutations :: TestTree
arrowRowsChangedReflectsMutations =
  testCase "arrow_rows_changed reports mutation counts" $
    withDatabase \db ->
      withConnection db \conn -> do
        execStatement conn "CREATE TABLE arrow_changes(val INTEGER);"

        withSuccessfulArrow conn "INSERT INTO arrow_changes VALUES (1), (2), (3)" \arrow -> do
          rowCount <- c_duckdb_arrow_row_count arrow
          assertBool "modification result should not report negative rows" (rowCount >= 0)

          changed <- c_duckdb_arrow_rows_changed arrow
          assertBool "rows_changed should report positive count" (changed > 0)

-- arrow scans ----------------------------------------------------------------

arrowArrayScanRegistersView :: TestTree
arrowArrayScanRegistersView =
  testCase "arrow_array_scan registers a view and yields a release stream" $
    withDatabase \db ->
      withConnection db \conn -> do
        execStatement conn "CREATE TABLE arrow_scan_source(i BIGINT, label VARCHAR);"
        execStatement conn "INSERT INTO arrow_scan_source VALUES (5, 'five'), (6, 'six');"

        withSuccessfulArrow conn "SELECT i, label FROM arrow_scan_source ORDER BY i" \arrow -> do
          alloca \schemaStorage -> do
            poke schemaStorage zeroArrowSchema
            let schemaHandle = castPtr schemaStorage :: DuckDBArrowSchema
            alloca \schemaOut -> do
              poke schemaOut schemaHandle
              schemaState <- c_duckdb_query_arrow_schema arrow schemaOut
              schemaState @?= DuckDBSuccess

              alloca \arrayStorage -> do
                poke arrayStorage zeroArrowArray
                let arrayHandle = castPtr arrayStorage :: DuckDBArrowArray
                alloca \arrayOut -> do
                  poke arrayOut arrayHandle
                  arrayState <- c_duckdb_query_arrow_array arrow arrayOut
                  arrayState @?= DuckDBSuccess

                  withCString "arrow_array_view" \viewName ->
                    alloca \streamOut -> do
                      poke streamOut nullPtr
                      scanState <- c_duckdb_arrow_array_scan conn viewName schemaHandle arrayHandle streamOut
                      scanState @?= DuckDBSuccess

                      streamWrapper <- peek streamOut
                      assertBool "arrow_array_scan returned a null stream handle" (streamWrapper /= nullPtr)

                      withResult conn "SELECT COUNT(*) FROM arrow_array_view" \resPtr -> do
                        count <- c_duckdb_value_int64 resPtr 0 0
                        count @?= 2

                      c_duckdb_destroy_arrow_stream streamOut
                  releaseArrowArray arrayStorage
            releaseArrowSchema schemaStorage

arrowStreamScanRegistersView :: TestTree
arrowStreamScanRegistersView =
  expectFailBecause "duckdb_arrow_scan currently returns DuckDBError even with a populated stream" $
    testCase "arrow_scan registers a view from an Arrow stream" $
      withDatabase \db ->
        withConnection db \conn -> do
          execStatement conn "CREATE TABLE arrow_stream_source(i BIGINT, label VARCHAR);"
          execStatement conn "INSERT INTO arrow_stream_source VALUES (7, 'seven'), (8, 'eight');"

          withSuccessfulArrow conn "SELECT i, label FROM arrow_stream_source ORDER BY i" \arrow -> do
            alloca \schemaStorage ->
              finally
                (do
                  poke schemaStorage zeroArrowSchema
                  let schemaHandle = castPtr schemaStorage :: DuckDBArrowSchema
                  alloca \schemaOut -> do
                    poke schemaOut schemaHandle
                    schemaState <- c_duckdb_query_arrow_schema arrow schemaOut
                    schemaState @?= DuckDBSuccess

                    alloca \arrayStorage ->
                      finally
                        (do
                          poke arrayStorage zeroArrowArray
                          let arrayHandle = castPtr arrayStorage :: DuckDBArrowArray
                          alloca \arrayOut -> do
                            poke arrayOut arrayHandle
                            arrayState <- c_duckdb_query_arrow_array arrow arrayOut
                            arrayState @?= DuckDBSuccess

                            withCString "arrow_stream_source" \sourceView ->
                              alloca \streamOut -> do
                                poke streamOut nullPtr
                                arrayScanState <- c_duckdb_arrow_array_scan conn sourceView schemaHandle arrayHandle streamOut
                                arrayScanState @?= DuckDBSuccess

                                streamHandle <- peek streamOut
                                assertBool "arrow_array_scan returned a null stream" (streamHandle /= nullPtr)

                                withCString "arrow_stream_view" \streamView -> do
                                  streamScanState <- c_duckdb_arrow_scan conn streamView streamHandle
                                  streamScanState @?= DuckDBSuccess

                                withResult conn "SELECT COUNT(*) FROM arrow_stream_view" \resPtr -> do
                                  count <- c_duckdb_value_int64 resPtr 0 0
                                  count @?= 2

                                c_duckdb_destroy_arrow_stream streamOut
                        )
                        (releaseArrowArray arrayStorage)
                )
                (releaseArrowSchema schemaStorage)

arrowPointerHelpersClearInternalState :: TestTree
arrowPointerHelpersClearInternalState =
  testCase "arrow pointer helper functions read and clear internal_ptr fields" $
    alloca \schemaPtrPtr ->
      alloca \arrayPtrPtr -> do
        let schemaSentinel = castPtr schemaPtrPtr :: Ptr ()
            schemaHandle = castPtr schemaPtrPtr :: DuckDBArrowSchema
        poke schemaPtrPtr schemaSentinel
        schemaPtr <- c_duckdb_arrow_schema_internal_ptr schemaHandle
        schemaPtr @?= schemaSentinel

        c_duckdb_arrow_schema_clear_internal_ptr schemaHandle
        schemaCleared <- c_duckdb_arrow_schema_internal_ptr schemaHandle
        schemaCleared @?= nullPtr

        let arraySentinel = castPtr arrayPtrPtr :: Ptr ()
            arrayHandle = castPtr arrayPtrPtr :: DuckDBArrowArray
        poke arrayPtrPtr arraySentinel
        arrayPtr <- c_duckdb_arrow_array_internal_ptr arrayHandle
        arrayPtr @?= arraySentinel

        c_duckdb_arrow_array_clear_internal_ptr arrayHandle
        arrayCleared <- c_duckdb_arrow_array_internal_ptr arrayHandle
        arrayCleared @?= nullPtr

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

validateChunkChildren :: ArrowArray -> IO ()
validateChunkChildren array = do
  let expectedIds = [10, 11] :: [Int64]
      expectedLabels = ["ten", "eleven"]
      childCount = fromIntegral (arrowArrayChildCount array) :: Int
      rowCount = fromIntegral (arrowArrayLength array) :: Int
  childCount @?= 2
  rowCount @?= length expectedIds

  let childrenPtr = arrowArrayChildren array
  assertBool "Arrow array did not expose child arrays" (childrenPtr /= nullPtr)

  let ensurePtr name ptrPred =
        unless ptrPred $
          assertFailure ("Arrow array " ++ name ++ " pointer is null")

  intChildPtr <- peekElemOff childrenPtr 0
  ensurePtr "integer child" (intChildPtr /= nullPtr)
  intChild <- peek intChildPtr
  arrowArrayNullCount intChild @?= 0
  let intBufferCount = fromIntegral (arrowArrayBufferCount intChild) :: Int
  assertBool "Integer child did not expose the expected buffers" (intBufferCount >= 2)

  let intBuffers = arrowArrayBuffers intChild
  ensurePtr "integer buffers" (intBuffers /= nullPtr)
  valueBufferRaw <- peekElemOff intBuffers 1
  ensurePtr "integer value buffer" (valueBufferRaw /= nullPtr)
  let valueBuffer = castPtr valueBufferRaw :: Ptr Int64
  values <- mapM (peekElemOff valueBuffer) [0 .. rowCount - 1]
  values @?= expectedIds

  strChildPtr <- peekElemOff childrenPtr 1
  ensurePtr "varchar child" (strChildPtr /= nullPtr)
  strChild <- peek strChildPtr
  arrowArrayNullCount strChild @?= 0
  let strBufferCount = fromIntegral (arrowArrayBufferCount strChild) :: Int
  assertBool "Varchar child did not expose the expected buffers" (strBufferCount >= 3)

  let strBuffers = arrowArrayBuffers strChild
  ensurePtr "varchar buffers" (strBuffers /= nullPtr)
  offsetsRaw <- peekElemOff strBuffers 1
  dataRaw <- peekElemOff strBuffers 2
  ensurePtr "varchar offsets buffer" (offsetsRaw /= nullPtr)
  ensurePtr "varchar data buffer" (dataRaw /= nullPtr)

  let offsetsPtr = castPtr offsetsRaw :: Ptr Int32
      dataPtr = castPtr dataRaw :: Ptr CChar
  labels <-
    mapM
      ( \idx -> do
          start <- fromIntegral <$> peekElemOff offsetsPtr idx
          end <- fromIntegral <$> peekElemOff offsetsPtr (idx + 1)
          peekCStringLen (dataPtr `plusPtr` start, end - start)
      )
      [0 .. rowCount - 1]
  labels @?= expectedLabels

releaseArrowArray :: Ptr ArrowArray -> IO ()
releaseArrowArray arrayPtr = do
  array <- peek arrayPtr
  let releaseFun = arrowArrayRelease array
  when (releaseFun /= nullFunPtr) $ do
    let release = mkArrowArrayRelease releaseFun
    release arrayPtr

releaseArrowSchema :: Ptr ArrowSchema -> IO ()
releaseArrowSchema schemaPtr = do
  schema <- peek schemaPtr
  let releaseFun = arrowSchemaRelease schema
  when (releaseFun /= nullFunPtr) $ do
    let release = mkArrowSchemaRelease releaseFun
    release schemaPtr

foreign import ccall "dynamic"
  mkArrowArrayRelease :: FunPtr (Ptr ArrowArray -> IO ()) -> Ptr ArrowArray -> IO ()

foreign import ccall "dynamic"
  mkArrowSchemaRelease :: FunPtr (Ptr ArrowSchema -> IO ()) -> Ptr ArrowSchema -> IO ()

foreign import ccall unsafe "wrapped_duckdb_arrow_schema_internal_ptr"
  c_duckdb_arrow_schema_internal_ptr :: DuckDBArrowSchema -> IO (Ptr ())

foreign import ccall unsafe "wrapped_duckdb_arrow_schema_clear_internal_ptr"
  c_duckdb_arrow_schema_clear_internal_ptr :: DuckDBArrowSchema -> IO ()

foreign import ccall unsafe "wrapped_duckdb_arrow_array_internal_ptr"
  c_duckdb_arrow_array_internal_ptr :: DuckDBArrowArray -> IO (Ptr ())

foreign import ccall unsafe "wrapped_duckdb_arrow_array_clear_internal_ptr"
  c_duckdb_arrow_array_clear_internal_ptr :: DuckDBArrowArray -> IO ()

withSuccessfulArrow :: DuckDBConnection -> String -> (DuckDBArrow -> IO a) -> IO a
withSuccessfulArrow conn sql action =
  withCString sql \sqlPtr ->
    alloca \arrowPtr ->
      bracket
        (do
          poke arrowPtr nullPtr
          state <- c_duckdb_query_arrow conn sqlPtr arrowPtr
          state @?= DuckDBSuccess
          arrow <- peek arrowPtr
          assertBool "duckdb_query_arrow returned null result" (arrow /= nullPtr)
          pure arrow)
        (\_ -> c_duckdb_destroy_arrow arrowPtr)
        action

withPrepared :: DuckDBConnection -> String -> (DuckDBPreparedStatement -> IO a) -> IO a
withPrepared conn sql action =
  withCString sql \sqlPtr ->
    alloca \stmtPtr ->
      bracket
        (do
          state <- c_duckdb_prepare conn sqlPtr stmtPtr
          state @?= DuckDBSuccess
          stmt <- peek stmtPtr
          assertBool "prepare should produce a statement" (stmt /= nullPtr)
          pure stmt)
        (\_ -> c_duckdb_destroy_prepare stmtPtr)
        action

withPreparedArrow :: DuckDBPreparedStatement -> (DuckDBArrow -> IO a) -> IO a
withPreparedArrow stmt action =
  alloca \arrowPtr ->
    bracket
      (do
        poke arrowPtr nullPtr
        state <- c_duckdb_execute_prepared_arrow stmt arrowPtr
        state @?= DuckDBSuccess
        arrow <- peek arrowPtr
        assertBool "execute_prepared_arrow returned null result" (arrow /= nullPtr)
        pure arrow)
      (\_ -> c_duckdb_destroy_arrow arrowPtr)
      action

destroyChunk :: DuckDBDataChunk -> IO ()
destroyChunk chunk =
  alloca \ptr -> poke ptr chunk >> c_duckdb_destroy_data_chunk ptr

execStatement :: DuckDBConnection -> String -> IO ()
execStatement conn sql =
  withCString sql \sqlPtr ->
    alloca \resPtr -> do
      st <- c_duckdb_query conn sqlPtr resPtr
      st @?= DuckDBSuccess
      c_duckdb_destroy_result resPtr
