{-# LANGUAGE OverloadedRecordDot #-}

-- | Run database operations through the generated API.
module Main (main) where

import Control.Exception (SomeException, bracket, bracket_, catch, displayException, finally)
import Control.Monad (forM_, unless, when)
import Data.Int (Int64)
import DuckDB qualified as C
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.C.String (peekCString, peekCStringLen, withCString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, castPtr, freeHaskellFunPtr, nullFunPtr, nullPtr)
import Foreign.Storable (alignment, peek, peekElemOff, poke, pokeElemOff, sizeOf)
import HsBindgen.Runtime.Struct qualified as Struct
import HsBindgen.Runtime.Support.FunPtr (fromFunPtr, toFunPtr)

-- | Check a probe result.
check :: String -> Bool -> IO ()
check label ok = unless ok (fail label)

-- | Open a connection and close both native handles after the action.
withConnection :: (C.Duckdb_connection -> IO a) -> IO a
withConnection action = alloca $ \database -> do
    poke database (C.Duckdb_database nullPtr)
    bracket_ (pure ()) (C.duckdb_close database) $ do
        C.duckdb_open (ConstPtr nullPtr) database >>= check "open" . (== C.DuckDBSuccess)
        alloca $ \connection -> do
            poke connection (C.Duckdb_connection nullPtr)
            bracket_ (pure ()) (C.duckdb_disconnect connection) $ do
                db <- peek database
                C.duckdb_connect db connection >>= check "connect" . (== C.DuckDBSuccess)
                peek connection >>= action

-- | Destroy a query result after success or failure.
withResult :: C.Duckdb_connection -> String -> (Ptr C.Duckdb_result -> IO a) -> IO a
withResult connection sql action = alloca $ \result -> do
    poke result Struct.zero
    bracket_ (pure ()) (C.duckdb_destroy_result result) $ do
        withCString sql $ \text -> do
            state <- C.duckdb_query connection (ConstPtr text) result
            check ("query: " ++ sql) (state == C.DuckDBSuccess)
        action result

-- | Write a BIGINT result and contain callback exceptions.
scalarCallback :: C.Duckdb_function_info -> C.Duckdb_data_chunk -> C.Duckdb_vector -> IO ()
scalarCallback info input output = run `catch` report
  where
    run = do
        C.Idx_t count <- C.duckdb_data_chunk_get_size input
        values <- C.duckdb_vector_get_data output
        forM_ [0 .. fromIntegral count - 1] $ \row ->
            pokeElemOff (castPtr values :: Ptr Int64) row 42
    report (err :: SomeException) = withCString (displayException err) $ \message ->
        C.duckdb_scalar_function_set_error info (ConstPtr message)

-- | Register a generated callback pointer and query its result.
checkCallback :: C.Duckdb_connection -> C.Duckdb_scalar_function_t -> IO ()
checkCallback connection callback =
    bracket C.duckdb_create_scalar_function (\fn -> with fn C.duckdb_destroy_scalar_function) $ \fn -> do
        withCString "hs_bindgen_answer" $ C.duckdb_scalar_function_set_name fn . ConstPtr
        bracket (C.duckdb_create_logical_type (C.Duckdb_type C.DUCKDB_TYPE_BIGINT)) (\ty -> with ty C.duckdb_destroy_logical_type) $ \ty -> do
            C.duckdb_scalar_function_set_return_type fn ty
            C.duckdb_scalar_function_set_function fn callback
            C.duckdb_register_scalar_function connection fn >>= check "register callback" . (== C.DuckDBSuccess)
        withResult connection "SELECT hs_bindgen_answer()" $ \result -> do
            value <- C.duckdb_value_int64 result (C.Idx_t 0) (C.Idx_t 0)
            check "native callback" (value == 42)

-- | Read inline and allocated VARCHAR values through the generated union.
checkStrings :: C.Duckdb_connection -> IO ()
checkStrings connection = withResult connection "SELECT 'abc', 'abcdefghijklmnopqrst'" $ \result -> do
    raw <- peek result
    bracket (C.duckdb_result_get_chunk raw (C.Idx_t 0)) (\chunk -> with chunk C.duckdb_destroy_data_chunk) $ \chunk ->
        forM_ [(0, "abc"), (1, "abcdefghijklmnopqrst")] $ \(column, expected) -> do
            vector <- C.duckdb_data_chunk_get_vector chunk (C.Idx_t column)
            values <- C.duckdb_vector_get_data vector
            let string = castPtr values :: Ptr C.Duckdb_string_t
            value <- peek string
            count <- C.duckdb_string_t_length value
            ConstPtr bytes <- C.duckdb_string_t_data string
            actual <- peekCStringLen (bytes, fromIntegral count)
            check "string union" (fromIntegral count == length expected && actual == expected)
    check "string layout" (sizeOf (undefined :: C.Duckdb_string_t) == 16 && alignment (undefined :: C.Duckdb_string_t) == 8)

-- | Export a real Arrow array and invoke its generated release callback.
checkArrow :: C.Duckdb_connection -> IO ()
checkArrow connection = alloca $ \options -> do
    poke options (C.Duckdb_arrow_options nullPtr)
    bracket_ (pure ()) (C.duckdb_destroy_arrow_options options) $ do
        C.duckdb_connection_get_arrow_options connection options
        settings <- peek options
        withResult connection "SELECT 42::BIGINT" $ \result -> do
            raw <- peek result
            bracket (C.duckdb_result_get_chunk raw (C.Idx_t 0)) (\chunk -> with chunk C.duckdb_destroy_data_chunk) $ \chunk ->
                alloca $ \array -> do
                    poke array Struct.zero
                    let release = do
                            value <- peek array
                            when (value.release /= nullFunPtr) (fromFunPtr value.release array)
                    flip finally release $ do
                        bracket (C.duckdb_data_chunk_to_arrow settings chunk array) (\err -> with err C.duckdb_destroy_error_data) $ \err -> do
                            failed <- C.duckdb_error_data_has_error err
                            check "Arrow export" (failed == 0)
                        value <- peek array
                        check "Arrow array" (value.length == 1 && value.n_children == 1 && value.release /= nullFunPtr)
                        child <- peek value.children >>= peek
                        ConstPtr buffer <- peekElemOff child.buffers 1
                        number <- peek (castPtr buffer :: Ptr Int64)
                        check "Arrow data" (number == 42)
                        release
                        cleared <- peek array
                        check "Arrow release" (cleared.release == nullFunPtr)

-- | Check prepared statements and native result cleanup on errors.
checkPrepared :: C.Duckdb_connection -> IO ()
checkPrepared connection = alloca $ \statement -> do
    poke statement (C.Duckdb_prepared_statement nullPtr)
    bracket_ (pure ()) (C.duckdb_destroy_prepare statement) $ do
        withCString "SELECT ?::BIGINT" $ \sql ->
            C.duckdb_prepare connection (ConstPtr sql) statement >>= check "prepare" . (== C.DuckDBSuccess)
        prepared <- peek statement
        C.duckdb_bind_int64 prepared (C.Idx_t 1) 123 >>= check "bind" . (== C.DuckDBSuccess)
        alloca $ \result -> do
            poke result Struct.zero
            bracket_ (pure ()) (C.duckdb_destroy_result result) $ do
                C.duckdb_execute_prepared prepared result >>= check "execute" . (== C.DuckDBSuccess)
                value <- C.duckdb_value_int64 result (C.Idx_t 0) (C.Idx_t 0)
                check "prepared result" (value == 123)
    alloca $ \result -> do
        poke result Struct.zero
        bracket_ (pure ()) (C.duckdb_destroy_result result) $
            withCString "SELECT * FROM hs_bindgen_missing_table" $ \sql -> do
                state <- C.duckdb_query connection (ConstPtr sql) result
                check "query failure" (state == C.DuckDBError)
                message <- C.duckdb_result_error result
                check "query error message" (message /= ConstPtr nullPtr)

-- | Fetch a streaming result through a struct passed by value.
checkStreaming :: C.Duckdb_connection -> IO ()
checkStreaming connection = alloca $ \statement -> do
    poke statement (C.Duckdb_prepared_statement nullPtr)
    bracket_ (pure ()) (C.duckdb_destroy_prepare statement) $ do
        withCString "SELECT i::BIGINT FROM range(5000) t(i)" $ \sql ->
            C.duckdb_prepare connection (ConstPtr sql) statement >>= check "stream prepare" . (== C.DuckDBSuccess)
        prepared <- peek statement
        alloca $ \result -> do
            poke result Struct.zero
            bracket_ (pure ()) (C.duckdb_destroy_result result) $ do
                C.duckdb_execute_prepared_streaming prepared result >>= check "stream execute" . (== C.DuckDBSuccess)
                raw <- peek result
                let consume count = do
                        chunk <- C.duckdb_fetch_chunk raw
                        if chunk == C.Duckdb_data_chunk nullPtr
                            then check "stream rows" (count == 5000)
                            else do
                                rows <- bracket_ (pure ()) (with chunk C.duckdb_destroy_data_chunk) $ do
                                    C.Idx_t rows <- C.duckdb_data_chunk_get_size chunk
                                    check "nonempty stream chunk" (rows > 0)
                                    vector <- C.duckdb_data_chunk_get_vector chunk (C.Idx_t 0)
                                    values <- C.duckdb_vector_get_data vector
                                    forM_ [0 .. fromIntegral rows - 1] $ \index -> do
                                        value <- peekElemOff (castPtr values :: Ptr Int64) index
                                        check "stream order" (value == count + fromIntegral index)
                                    pure rows
                                consume (count + fromIntegral rows)
                consume 0
                C.duckdb_result_error result >>= check "stream error" . (== ConstPtr nullPtr)

-- | Exercise generated functions, layouts, and callback conversions.
main :: IO ()
main = do
    ConstPtr version <- C.duckdb_library_version
    name <- peekCString version
    check "DuckDB 1.5.6" (name == "v1.5.6")
    let date = C.Duckdb_date_struct 2026 10 8
    encoded <- C.duckdb_to_date date
    decoded <- C.duckdb_from_date encoded
    check "date structs by value" (decoded == date)
    decimal <- C.duckdb_double_to_decimal 12.34 8 2
    number <- C.duckdb_decimal_to_double decimal
    check "DECIMAL structs by value" (abs (number - 12.34) < 0.000001)
    bracket (toFunPtr (C.Duckdb_scalar_function_t_Aux scalarCallback)) freeHaskellFunPtr $ \callback ->
        withConnection $ \connection -> do
            checkPrepared connection
            checkStreaming connection
            checkStrings connection
            checkCallback connection (C.Duckdb_scalar_function_t callback)
            checkArrow connection
    putStrLn "DuckDB 1.5.6: generated API, structs, unions, prepared statements, streaming, callbacks, Arrow, and error cleanup passed."
