{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module ExecutePreparedStatementsTest (tests) where

import Control.Monad (forM_)
import Data.Coerce (coerce)
import Data.Void (Void)
import Database.DuckDB.FFI
import Foreign.C.String (peekCString)
import Foreign.C.Types (CBool (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek, poke)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Utils (withConnection, withConstCString, withDatabase)

tests :: TestTree
tests =
    testGroup
        "Execute Prepared Statements"
        [ executePreparedProducesResult
        , executePreparedStreamingProducesChunks
        ]

setupTable :: DuckDBConnection -> IO ()
setupTable conn = do
    let statements =
            [ "CREATE TABLE exec_prepared (id INTEGER, name VARCHAR);"
            , "INSERT INTO exec_prepared VALUES (1, 'alpha'), (2, 'beta');"
            ]
    forM_ statements \sql ->
        withConstCString sql \cSql ->
            alloca \resPtr -> do
                st <- c_duckdb_query conn cSql resPtr
                st @?= DuckDBSuccess
                c_duckdb_destroy_result resPtr

executePreparedProducesResult :: TestTree
executePreparedProducesResult =
    testCase "execute_prepared returns regular result" $
        withDatabase \db ->
            withConnection db \conn -> do
                setupTable conn

                withConstCString "SELECT name FROM exec_prepared WHERE id = ?" \querySql ->
                    alloca \stmtPtr -> do
                        st <- c_duckdb_prepare conn querySql stmtPtr
                        st @?= DuckDBSuccess
                        stmt <- peek stmtPtr
                        assertBool "prepared statement should not be null" (stmt /= (coerce (nullPtr :: Ptr Void)))

                        c_duckdb_bind_int32 stmt 1 2 >>= (@?= DuckDBSuccess)

                        alloca \resPtr -> do
                            execState <- c_duckdb_execute_prepared stmt resPtr
                            execState @?= DuckDBSuccess

                            streamingFlag <- (peek resPtr >>= \rawValue -> c_duckdb_result_is_streaming rawValue)
                            streamingFlag @?= CBool 0

                            rowCount <- c_duckdb_row_count resPtr
                            rowCount @?= 1

                            varcharPtr <- c_duckdb_value_varchar resPtr 0 0
                            (peekCString . coerce) varcharPtr >>= (@?= "beta")
                            c_duckdb_free (coerce varcharPtr)

                            c_duckdb_destroy_result resPtr

                        c_duckdb_destroy_prepare stmtPtr

executePreparedStreamingProducesChunks :: TestTree
executePreparedStreamingProducesChunks =
    testCase "execute_prepared_streaming produces streaming result" $
        withDatabase \db ->
            withConnection db \conn -> do
                setupTable conn

                withConstCString "SELECT id, name FROM exec_prepared ORDER BY id" \querySql ->
                    alloca \stmtPtr -> do
                        st <- c_duckdb_prepare conn querySql stmtPtr
                        st @?= DuckDBSuccess
                        stmt <- peek stmtPtr
                        assertBool "streaming prepared statement should not be null" (stmt /= (coerce (nullPtr :: Ptr Void)))

                        alloca \resPtr -> do
                            execState <- c_duckdb_execute_prepared_streaming stmt resPtr
                            execState @?= DuckDBSuccess

                            streamingFlag <- (peek resPtr >>= \rawValue -> c_duckdb_result_is_streaming rawValue)
                            streamingFlag @?= CBool 1

                            chunk <- (peek resPtr >>= \rawValue -> c_duckdb_stream_fetch_chunk rawValue)
                            assertBool "streaming fetch chunk returns data" (chunk /= (coerce (nullPtr :: Ptr Void)))

                            chunkSize <- c_duckdb_data_chunk_get_size chunk
                            assertBool "streamed chunk should contain rows" (chunkSize > 0)

                            alloca \chunkPtr -> do
                                poke chunkPtr chunk
                                c_duckdb_destroy_data_chunk chunkPtr

                            c_duckdb_destroy_result resPtr

                        c_duckdb_destroy_prepare stmtPtr
