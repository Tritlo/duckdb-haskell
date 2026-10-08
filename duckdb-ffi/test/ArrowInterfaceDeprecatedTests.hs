{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module ArrowInterfaceDeprecatedTests (tests) where

import Control.Exception (bracket, finally)
import Control.Monad (unless, when, (>=>))
import Data.Coerce (coerce)
import Data.Int (Int32, Int64)
import Data.List (isInfixOf)
import Data.Proxy (Proxy (..))
import Data.Void (Void)
import Database.DuckDB.FFI
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.C.String (peekCString, peekCStringLen)
import Foreign.C.Types (CChar)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (FunPtr, Ptr, nullFunPtr, nullPtr, plusPtr)
import Foreign.Storable (peek, peekElemOff, poke)
import GHC.Records (getField)
import HsBindgen.Runtime.HasCField (fromPtr)
import HsBindgen.Runtime.Support.FunPtr (fromFunPtr)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import Utils (releaseArrowArray, releaseArrowSchema, withConnection, withConstCString, withDatabase, withResult)

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
        , arrowStructMovesPreserveOwnership
        ]

-- basic query ---------------------------------------------------------------

queryArrowExposesSchemaAndArrays :: TestTree
queryArrowExposesSchemaAndArrays =
    testCase "query_arrow exposes schema metadata and arrays" $
        withDatabase \db ->
            withConnection db \conn ->
                withSuccessfulArrow conn "SELECT 1::INTEGER AS id, 'duck'::VARCHAR AS label" \arrow -> do
                    columnCount <- duckdb_arrow_column_count arrow
                    columnCount @?= 2

                    rowCount <- duckdb_arrow_row_count arrow
                    rowCount @?= 1

                    rowsChanged <- duckdb_arrow_rows_changed arrow
                    rowsChanged @?= 0

                    errPtr <- duckdb_query_arrow_error arrow
                    when (errPtr /= (coerce (nullPtr :: Ptr Void))) $ do
                        errMsg <- (peekCString . coerce) errPtr
                        errMsg @?= ""

-- schema/array access validated in dedicated scan test

-- error handling ------------------------------------------------------------

queryArrowReportsErrors :: TestTree
queryArrowReportsErrors =
    testCase "query_arrow surfaces execution errors" $
        withDatabase \db ->
            withConnection db \conn ->
                withConstCString "SELECT * FROM missing_table" \querySql ->
                    alloca \arrowPtr -> do
                        poke arrowPtr (coerce (nullPtr :: Ptr Void))
                        state <- duckdb_query_arrow conn querySql arrowPtr
                        state @?= DuckDBError

                        arrow <- peek arrowPtr
                        assertBool "arrow result should still be allocated on error" (arrow /= (coerce (nullPtr :: Ptr Void)))

                        errPtr <- duckdb_query_arrow_error arrow
                        assertBool "error message should be present" (errPtr /= (coerce (nullPtr :: Ptr Void)))
                        errMsg <- (peekCString . coerce) errPtr
                        assertBool "error message should mention missing_table" ("missing_table" `isInfixOf` errMsg)

                        duckdb_destroy_arrow arrowPtr

-- prepared statements -------------------------------------------------------

preparedArrowSchemaMatchesStatement :: TestTree
preparedArrowSchemaMatchesStatement =
    testCase "prepared_arrow_schema describes statement parameters" $
        withDatabase \db ->
            withConnection db \conn ->
                withPrepared conn "SELECT ?::INTEGER, ?::VARCHAR" \stmt ->
                    alloca \schemaStorage -> do
                        poke schemaStorage zeroArrowSchema
                        alloca \schemaOut -> do
                            poke schemaOut (coerce schemaStorage)
                            duckdb_prepared_arrow_schema stmt schemaOut >>= (@?= DuckDBSuccess)
                            schema <- peek schemaStorage
                            (getField @"n_children") schema @?= 2
                            children <- mapM (peekElemOff ((getField @"children") schema) >=> peek) [0, 1]
                            mapM ((peekCString . coerce) . (getField @"name")) children >>= (@?= ["0", "1"])
                            mapM ((peekCString . coerce) . (getField @"format")) children >>= (@?= ["n", "n"])
                            releaseArrowSchema schemaStorage

executePreparedArrowProducesRows :: TestTree
executePreparedArrowProducesRows =
    testCase "execute_prepared_arrow materialises a result set" $
        withDatabase \db ->
            withConnection db \conn ->
                withPrepared conn "SELECT ?::INTEGER + 5 AS computed" \stmt -> do
                    duckdb_bind_int32 stmt 1 (5 :: Int32) >>= (@?= DuckDBSuccess)

                    withPreparedArrow stmt \arrow -> do
                        rowCount <- duckdb_arrow_row_count arrow
                        rowCount @?= 1

                        colCount <- duckdb_arrow_column_count arrow
                        colCount @?= 1

-- result conversion ---------------------------------------------------------

resultArrowArrayMirrorsChunk :: TestTree
resultArrowArrayMirrorsChunk =
    -- NOTE: duckdb_result_arrow_array writes release callbacks into the
    -- provided ArrowArray storage. Always zero-initialise the struct
    -- (see zeroArrowArray) and honour the release function before the
    -- stack memory goes out of scope.
    testCase "result_arrow_array converts materialised chunks to Arrow arrays" $
        withDatabase \db ->
            withConnection db \conn -> do
                execStatement conn "CREATE TABLE arrow_chunks(id BIGINT, label VARCHAR);"
                execStatement conn "INSERT INTO arrow_chunks VALUES (10, 'ten'), (11, 'eleven');"

                withResult conn "SELECT id, label FROM arrow_chunks ORDER BY id" \resPtr -> do
                    chunk <- (peek resPtr >>= \rawValue -> duckdb_result_get_chunk rawValue 0)
                    assertBool "fetch_chunk returned a null chunk" (chunk /= (coerce (nullPtr :: Ptr Void)))
                    chunkSize <- duckdb_data_chunk_get_size chunk
                    chunkCols <- duckdb_data_chunk_get_column_count chunk

                    alloca \arrowArrayPtr -> do
                        poke arrowArrayPtr zeroArrowArray
                        let duckArray :: Duckdb_arrow_array
                            duckArray = coerce arrowArrayPtr

                        alloca \arrayOut -> do
                            poke arrayOut duckArray
                            (peek resPtr >>= \rawValue -> duckdb_result_arrow_array rawValue chunk arrayOut)

                            array <- peek arrowArrayPtr

                            (getField @"length") array @?= fromIntegral chunkSize
                            (getField @"n_children") array @?= fromIntegral chunkCols

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
                    rowCount <- duckdb_arrow_row_count arrow
                    assertBool "modification result should not report negative rows" (rowCount >= 0)

                    changed <- duckdb_arrow_rows_changed arrow
                    assertBool "rows_changed should report positive count" (changed > 0)

-- arrow scans ----------------------------------------------------------------

arrowArrayScanRegistersView :: TestTree
arrowArrayScanRegistersView =
    -- NOTE: Both the schema and array buffers must be initialised to
    -- zeroed Arrow structures before calling the query helpers. DuckDB
    -- fills in release callbacks and expects us to invoke
    -- duckdb_destroy_arrow_stream on the out stream.
    testCase "arrow_array_scan registers a view and yields a release stream" $
        withDatabase \db ->
            withConnection db \conn -> do
                execStatement conn "CREATE TABLE arrow_scan_source(i BIGINT, label VARCHAR);"
                execStatement conn "INSERT INTO arrow_scan_source VALUES (5, 'five'), (6, 'six');"

                withSuccessfulArrow conn "SELECT i, label FROM arrow_scan_source ORDER BY i" \arrow -> do
                    alloca \schemaStorage -> do
                        poke schemaStorage zeroArrowSchema
                        let schemaHandle = coerce schemaStorage :: Duckdb_arrow_schema
                        alloca \schemaOut -> do
                            poke schemaOut schemaHandle
                            schemaState <- duckdb_query_arrow_schema arrow schemaOut
                            schemaState @?= DuckDBSuccess

                            alloca \arrayStorage -> do
                                poke arrayStorage zeroArrowArray
                                let arrayHandle = coerce arrayStorage :: Duckdb_arrow_array
                                alloca \arrayOut -> do
                                    poke arrayOut arrayHandle
                                    arrayState <- duckdb_query_arrow_array arrow arrayOut
                                    arrayState @?= DuckDBSuccess

                                    withConstCString "arrow_array_view" \viewName ->
                                        alloca \streamOut -> do
                                            poke streamOut (coerce (nullPtr :: Ptr Void))
                                            scanState <- duckdb_arrow_array_scan conn viewName schemaHandle arrayHandle streamOut
                                            scanState @?= DuckDBSuccess

                                            streamWrapper <- peek streamOut
                                            assertBool "arrow_array_scan returned a null stream handle" (streamWrapper /= (coerce (nullPtr :: Ptr Void)))

                                            withResult conn "SELECT COUNT(*) FROM arrow_array_view" \resPtr -> do
                                                count <- duckdb_value_int64 resPtr 0 0
                                                count @?= 2

                                            duckdb_destroy_arrow_stream streamOut
                                    releaseArrowArray arrayStorage
                        releaseArrowSchema schemaStorage

arrowStreamScanRegistersView :: TestTree
arrowStreamScanRegistersView =
    -- NOTE: duckdb_arrow_array_scan returns DuckDBError if the target
    -- view name already exists (even if it is a table). Use a fresh
    -- view name that will be dropped implicitly when the connection
    -- closes.
    testCase "arrow_scan registers a view from an Arrow stream" $
        withDatabase \db ->
            withConnection db \conn -> do
                execStatement conn "CREATE TABLE arrow_stream_source(i BIGINT, label VARCHAR);"
                execStatement conn "INSERT INTO arrow_stream_source VALUES (7, 'seven'), (8, 'eight');"

                withSuccessfulArrow conn "SELECT i, label FROM arrow_stream_source ORDER BY i" \arrow -> do
                    alloca \schemaStorage ->
                        finally
                            ( do
                                poke schemaStorage zeroArrowSchema
                                let schemaHandle = coerce schemaStorage :: Duckdb_arrow_schema
                                alloca \schemaOut -> do
                                    poke schemaOut schemaHandle
                                    schemaState <- duckdb_query_arrow_schema arrow schemaOut
                                    schemaState @?= DuckDBSuccess

                                    alloca \arrayStorage ->
                                        finally
                                            ( do
                                                poke arrayStorage zeroArrowArray
                                                let arrayHandle = coerce arrayStorage :: Duckdb_arrow_array
                                                alloca \arrayOut -> do
                                                    poke arrayOut arrayHandle
                                                    arrayState <- duckdb_query_arrow_array arrow arrayOut
                                                    arrayState @?= DuckDBSuccess

                                                    withConstCString "arrow_stream_array_view" \sourceView ->
                                                        alloca \streamOut -> do
                                                            poke streamOut (coerce (nullPtr :: Ptr Void))
                                                            arrayScanState <- duckdb_arrow_array_scan conn sourceView schemaHandle arrayHandle streamOut
                                                            arrayScanState @?= DuckDBSuccess

                                                            streamHandle <- peek streamOut
                                                            assertBool "arrow_array_scan returned a null stream" (streamHandle /= (coerce (nullPtr :: Ptr Void)))

                                                            withConstCString "arrow_stream_view" \streamView -> do
                                                                streamScanState <- duckdb_arrow_scan conn streamView streamHandle
                                                                streamScanState @?= DuckDBSuccess

                                                            withResult conn "SELECT COUNT(*) FROM arrow_stream_view" \resPtr -> do
                                                                count <- duckdb_value_int64 resPtr 0 0
                                                                count @?= 2

                                                            duckdb_destroy_arrow_stream streamOut
                                            )
                                            (releaseArrowArray arrayStorage)
                            )
                            (releaseArrowSchema schemaStorage)

arrowStructMovesPreserveOwnership :: TestTree
arrowStructMovesPreserveOwnership =
    testCase "Arrow structs move real DuckDB buffers without releasing them" $ do
        withDatabase \db ->
            withConnection db \conn ->
                withSuccessfulArrow conn "SELECT 42::BIGINT AS id, 'duck' AS label" \arrow ->
                    alloca \schemaStorage -> do
                        poke schemaStorage zeroArrowSchema
                        let schemaHandle = coerce schemaStorage :: Duckdb_arrow_schema
                        alloca \schemaOut -> do
                            poke schemaOut schemaHandle
                            duckdb_query_arrow_schema arrow schemaOut >>= (@?= DuckDBSuccess)

                        alloca \arrayStorage -> do
                            poke arrayStorage zeroArrowArray
                            let arrayHandle = coerce arrayStorage :: Duckdb_arrow_array
                            alloca \arrayOut -> do
                                poke arrayOut arrayHandle
                                duckdb_query_arrow_array arrow arrayOut >>= (@?= DuckDBSuccess)

                            withConstCString "arrow_helper_view" \viewName ->
                                alloca \streamOut -> do
                                    poke streamOut (coerce (nullPtr :: Ptr Void))
                                    duckdb_arrow_array_scan conn viewName schemaHandle arrayHandle streamOut >>= (@?= DuckDBSuccess)
                                    stream <- peek streamOut

                                    original <- peek (coerce stream :: Ptr ArrowArrayStream)
                                    alloca \movedStream -> do
                                        poke movedStream original
                                        poke (fromPtr (Proxy @"release") ((coerce stream :: Ptr ArrowArrayStream) :: Ptr ArrowArrayStream)) (coerce (nullFunPtr :: FunPtr Void))
                                        source <- peek (coerce stream :: Ptr ArrowArrayStream)
                                        (getField @"release") source @?= (coerce (nullFunPtr :: FunPtr Void))
                                        (getField @"get_schema") source @?= (getField @"get_schema") original
                                        alloca \movedSchema -> do
                                            poke movedSchema zeroArrowSchema
                                            fromFunPtr ((getField @"get_schema") original) movedStream movedSchema >>= (@?= 0)
                                            schema <- peek movedSchema
                                            (peekCString . coerce) ((getField @"format") schema) >>= (@?= "+s")
                                            releaseArrowSchema movedSchema
                                        alloca \movedArray -> do
                                            poke movedArray zeroArrowArray
                                            fromFunPtr ((getField @"get_next") original) movedStream movedArray >>= (@?= 0)
                                            batch <- peek movedArray
                                            (getField @"length") batch @?= 1
                                            releaseArrowArray movedArray
                                            fromFunPtr ((getField @"get_next") original) movedStream movedArray >>= (@?= 0)
                                            exhausted <- peek movedArray
                                            (getField @"release") exhausted @?= (coerce (nullFunPtr :: FunPtr Void))
                                        fromFunPtr ((getField @"release") original) movedStream
                                        moved <- peek movedStream
                                        (getField @"release") moved @?= (coerce (nullFunPtr :: FunPtr Void))
                                    duckdb_destroy_arrow_stream streamOut
                                    peek streamOut >>= (@?= (coerce (nullPtr :: Ptr Void)))

                            originalArray <- peek arrayStorage
                            alloca \movedArray -> do
                                poke movedArray originalArray
                                poke (fromPtr (Proxy @"release") (arrayStorage :: Ptr ArrowArray)) (coerce (nullFunPtr :: FunPtr Void))
                                source <- peek arrayStorage
                                (getField @"length") source @?= (getField @"length") originalArray
                                (getField @"buffers") source @?= (getField @"buffers") originalArray
                                (getField @"release") source @?= (coerce (nullFunPtr :: FunPtr Void))
                                moved <- peek movedArray
                                (getField @"length") moved @?= 1
                                child <- peekElemOff ((getField @"children") moved) 0 >>= peek
                                buffer <- peekElemOff ((getField @"buffers") child) 1
                                peek (coerce buffer :: Ptr Int64) >>= (@?= 42)
                                releaseArrowArray movedArray
                                poke (fromPtr (Proxy @"release") (arrayStorage :: Ptr ArrowArray)) (coerce (nullFunPtr :: FunPtr Void))

                        originalSchema <- peek schemaStorage
                        alloca \movedSchema -> do
                            poke movedSchema originalSchema
                            poke (fromPtr (Proxy @"release") (schemaStorage :: Ptr ArrowSchema)) (coerce (nullFunPtr :: FunPtr Void))
                            source <- peek schemaStorage
                            (getField @"format") source @?= (getField @"format") originalSchema
                            (getField @"release") source @?= (coerce (nullFunPtr :: FunPtr Void))
                            moved <- peek movedSchema
                            (peekCString . coerce) ((getField @"format") moved) >>= (@?= "+s")
                            child <- peekElemOff ((getField @"children") moved) 0 >>= peek
                            (peekCString . coerce) ((getField @"name") child) >>= (@?= "id")
                            releaseArrowSchema movedSchema
                            poke (fromPtr (Proxy @"release") (schemaStorage :: Ptr ArrowSchema)) (coerce (nullFunPtr :: FunPtr Void))

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

validateChunkChildren :: ArrowArray -> IO ()
validateChunkChildren array = do
    let expectedIds = [10, 11] :: [Int64]
        expectedLabels = ["ten", "eleven"]
        childCount = fromIntegral ((getField @"n_children") array) :: Int
        rowCount = fromIntegral ((getField @"length") array) :: Int
    childCount @?= 2
    rowCount @?= length expectedIds

    let childrenPtr = (getField @"children") array
    assertBool "Arrow array did not expose child arrays" (childrenPtr /= (coerce (nullPtr :: Ptr Void)))

    let ensurePtr name ptrPred =
            unless ptrPred $
                assertFailure ("Arrow array " ++ name ++ " pointer is null")

    intChildPtr <- peekElemOff childrenPtr 0
    ensurePtr "integer child" (intChildPtr /= (coerce (nullPtr :: Ptr Void)))
    intChild <- peek intChildPtr
    (getField @"null_count") intChild @?= 0
    let intBufferCount = fromIntegral ((getField @"n_buffers") intChild) :: Int
    assertBool "Integer child did not expose the expected buffers" (intBufferCount >= 2)

    let intBuffers = (getField @"buffers") intChild
    ensurePtr "integer buffers" (intBuffers /= (coerce (nullPtr :: Ptr Void)))
    valueBufferRaw <- peekElemOff intBuffers 1
    ensurePtr "integer value buffer" (valueBufferRaw /= (coerce (nullPtr :: Ptr Void)))
    let valueBuffer = coerce valueBufferRaw :: Ptr Int64
    values <- mapM (peekElemOff valueBuffer) [0 .. rowCount - 1]
    values @?= expectedIds

    strChildPtr <- peekElemOff childrenPtr 1
    ensurePtr "varchar child" (strChildPtr /= (coerce (nullPtr :: Ptr Void)))
    strChild <- peek strChildPtr
    (getField @"null_count") strChild @?= 0
    let strBufferCount = fromIntegral ((getField @"n_buffers") strChild) :: Int
    assertBool "Varchar child did not expose the expected buffers" (strBufferCount >= 3)

    let strBuffers = (getField @"buffers") strChild
    ensurePtr "varchar buffers" (strBuffers /= (coerce (nullPtr :: Ptr Void)))
    offsetsRaw <- peekElemOff strBuffers 1
    dataRaw <- peekElemOff strBuffers 2
    ensurePtr "varchar offsets buffer" (offsetsRaw /= (coerce (nullPtr :: Ptr Void)))
    ensurePtr "varchar data buffer" (dataRaw /= (coerce (nullPtr :: Ptr Void)))

    let offsetsPtr = coerce offsetsRaw :: Ptr Int32
        dataPtr = coerce dataRaw :: Ptr CChar
    labels <-
        mapM
            ( \idx -> do
                start <- fromIntegral <$> peekElemOff offsetsPtr idx
                end <- fromIntegral <$> peekElemOff offsetsPtr (idx + 1)
                peekCStringLen (dataPtr `plusPtr` start, end - start)
            )
            [0 .. rowCount - 1]
    labels @?= expectedLabels

withSuccessfulArrow :: Duckdb_connection -> String -> (Duckdb_arrow -> IO a) -> IO a
withSuccessfulArrow conn sql action =
    withConstCString sql \sqlPtr ->
        alloca \arrowPtr ->
            bracket
                ( do
                    poke arrowPtr (coerce (nullPtr :: Ptr Void))
                    state <- duckdb_query_arrow conn sqlPtr arrowPtr
                    state @?= DuckDBSuccess
                    arrow <- peek arrowPtr
                    assertBool "duckdb_query_arrow returned null result" (arrow /= (coerce (nullPtr :: Ptr Void)))
                    pure arrow
                )
                (\_ -> duckdb_destroy_arrow arrowPtr)
                action

withPrepared :: Duckdb_connection -> String -> (Duckdb_prepared_statement -> IO a) -> IO a
withPrepared conn sql action =
    withConstCString sql \sqlPtr ->
        alloca \stmtPtr ->
            bracket
                ( do
                    state <- duckdb_prepare conn sqlPtr stmtPtr
                    state @?= DuckDBSuccess
                    stmt <- peek stmtPtr
                    assertBool "prepare should produce a statement" (stmt /= (coerce (nullPtr :: Ptr Void)))
                    pure stmt
                )
                (\_ -> duckdb_destroy_prepare stmtPtr)
                action

withPreparedArrow :: Duckdb_prepared_statement -> (Duckdb_arrow -> IO a) -> IO a
withPreparedArrow stmt action =
    alloca \arrowPtr ->
        bracket
            ( do
                poke arrowPtr (coerce (nullPtr :: Ptr Void))
                state <- duckdb_execute_prepared_arrow stmt arrowPtr
                state @?= DuckDBSuccess
                arrow <- peek arrowPtr
                assertBool "execute_prepared_arrow returned null result" (arrow /= (coerce (nullPtr :: Ptr Void)))
                pure arrow
            )
            (\_ -> duckdb_destroy_arrow arrowPtr)
            action

destroyChunk :: Duckdb_data_chunk -> IO ()
destroyChunk chunk =
    alloca \ptr -> poke ptr chunk >> duckdb_destroy_data_chunk ptr

execStatement :: Duckdb_connection -> String -> IO ()
execStatement conn sql =
    withConstCString sql \sqlPtr ->
        alloca \resPtr -> do
            st <- duckdb_query conn sqlPtr resPtr
            st @?= DuckDBSuccess
            duckdb_destroy_result resPtr
