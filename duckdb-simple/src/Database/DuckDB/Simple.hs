{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}

{- |
Module      : Database.DuckDB.Simple
Description : High-level DuckDB API in the duckdb-simple style.

The API implemented here mirrors the ergonomics of @sqlite-simple@ while being
backed by the DuckDB C API.  Phase 1 focuses on connection management and
prepared statement lifecycles.
-}
module Database.DuckDB.Simple (
    -- * Connections
    Connection,
    open,
    close,
    withConnection,

    -- * Queries and statements
    Query (..),
    Statement,
    openStatement,
    closeStatement,
    withStatement,
    clearStatementBindings,
    namedParameterIndex,
    executeStatement,
    execute,
    executeMany,
    execute_,
    bind,
    query,
    query_,

    -- * Errors and conversions
    SQLError (..),
    ResultError (..),
    FromField (..),
    FromRow (..),
    -- Re-export parameter helper types.
    ToField (..),
    ToRow (..),
    FieldBinding,
    Null (..),
    Only (..),
) where

import Control.Exception (bracket, mask, onException, throwIO)
import Control.Monad (void, when, zipWithM_)
import Data.IORef (atomicModifyIORef', mkWeakIORef, newIORef)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Foreign as TextForeign
import Database.DuckDB.FFI
import Database.DuckDB.Simple.FromField (
    Field (..),
    FieldValue (..),
    FromField (..),
    ResultError (..),
 )
import Database.DuckDB.Simple.FromRow (FromRow (..), resultErrorToSqlError)
import Database.DuckDB.Simple.Internal (
    Connection (..),
    ConnectionState (..),
    Query (..),
    SQLError (..),
    Statement (..),
    StatementState (..),
    withConnectionHandle,
    withQueryCString,
    withStatementHandle,
 )
import Database.DuckDB.Simple.ToField (FieldBinding, ToField (..), bindFieldBinding)
import Database.DuckDB.Simple.ToRow (ToRow (..))
import Database.DuckDB.Simple.Types (Null (..), Only (..))
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CBool (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (peek, poke)

-- | Open a DuckDB database located at the supplied path.
open :: FilePath -> IO Connection
open path =
    mask \restore -> do
        db <- restore (openDatabase path)
        conn <-
            restore (connectDatabase db)
                `onException` closeDatabaseHandle db
        connection <-
            createConnection db conn
                `onException` do
                    closeConnectionHandle conn
                    closeDatabaseHandle db
        pure connection

-- | Close a connection.  The operation is idempotent.
close :: Connection -> IO ()
close Connection{connectionState} =
    void $
        atomicModifyIORef' connectionState \case
            ConnectionClosed -> (ConnectionClosed, pure ())
            openState@(ConnectionOpen{}) ->
                (ConnectionClosed, closeHandles openState)

-- | Run an action with a freshly opened connection, closing it afterwards.
withConnection :: FilePath -> (Connection -> IO a) -> IO a
withConnection path = bracket (open path) close

-- | Prepare a SQL statement for execution.
openStatement :: Connection -> Query -> IO Statement
openStatement conn query =
    mask \restore -> do
        handle <-
            restore $
                withConnectionHandle conn \connPtr ->
                    withQueryCString query \sql ->
                        alloca \stmtPtr -> do
                            rc <- c_duckdb_prepare connPtr sql stmtPtr
                            stmt <- peek stmtPtr
                            if rc == DuckDBSuccess
                                then pure stmt
                                else do
                                    errMsg <- fetchPrepareError stmt
                                    c_duckdb_destroy_prepare stmtPtr
                                    throwIO $ mkPrepareError query errMsg
        createStatement conn handle query
            `onException` destroyPrepared handle

-- | Finalise a prepared statement.  The operation is idempotent.
closeStatement :: Statement -> IO ()
closeStatement Statement{statementState} =
    void $
        atomicModifyIORef' statementState \case
            StatementClosed -> (StatementClosed, pure ())
            StatementOpen{statementHandle} ->
                (StatementClosed, destroyPrepared statementHandle)

-- | Run an action with a prepared statement, closing it afterwards.
withStatement :: Connection -> Query -> (Statement -> IO a) -> IO a
withStatement conn sql = bracket (openStatement conn sql) closeStatement

-- | Bind positional parameters to a prepared statement.
bind :: Statement -> [FieldBinding] -> IO ()
bind stmt fields = do
    clearStatementBindings stmt
    zipWithM_ apply [1 ..] fields
  where
    apply :: Int -> FieldBinding -> IO ()
    apply idx field =
        bindFieldBinding stmt (fromIntegral idx :: DuckDBIdx) field

-- | Remove all parameter bindings associated with a prepared statement.
clearStatementBindings :: Statement -> IO ()
clearStatementBindings stmt =
    withStatementHandle stmt \handle -> do
        rc <- c_duckdb_clear_bindings handle
        when (rc /= DuckDBSuccess) $ do
            err <- fetchPrepareError handle
            throwIO $ mkPrepareError (statementQuery stmt) err

{- | Look up the 1-based index of a named placeholder.

Returns 'Nothing' when the name is not present in the statement.
-}
namedParameterIndex :: Statement -> Text -> IO (Maybe Int)
namedParameterIndex stmt name =
    withStatementHandle stmt \handle ->
        TextForeign.withCString name \cName ->
            alloca \idxPtr -> do
                rc <- c_duckdb_bind_parameter_index handle idxPtr cName
                if rc == DuckDBSuccess
                    then do
                        idx <- peek idxPtr
                        if idx == 0
                            then pure Nothing
                            else pure (Just (fromIntegral idx))
                    else pure Nothing

-- | Execute a prepared statement and discard the materialised result.
executeStatement :: Statement -> IO ()
executeStatement stmt =
    withStatementHandle stmt \handle ->
        alloca \resPtr -> do
            rc <- c_duckdb_execute_prepared handle resPtr
            if rc == DuckDBSuccess
                then c_duckdb_destroy_result resPtr
                else do
                    err <- fetchResultError resPtr
                    c_duckdb_destroy_result resPtr
                    throwIO $ mkPrepareError (statementQuery stmt) err

-- | Execute a query with positional parameters supplied via 'ToRow'.
execute :: ToRow q => Connection -> Query -> q -> IO ()
execute conn query params =
    withStatement conn query \stmt -> do
        bind stmt (toRow params)
        executeStatement stmt

-- | Execute a query multiple times with different parameters.
executeMany :: ToRow q => Connection -> Query -> [q] -> IO ()
executeMany conn query rows =
    withStatement conn query \stmt ->
        mapM_ (\row -> bind stmt (toRow row) >> executeStatement stmt) rows

-- | Execute an ad-hoc query without parameters.
execute_ :: Connection -> Query -> IO ()
execute_ conn query =
    withConnectionHandle conn \connPtr ->
        withQueryCString query \sql ->
            alloca \resPtr -> do
                rc <- c_duckdb_query connPtr sql resPtr
                if rc == DuckDBSuccess
                    then c_duckdb_destroy_result resPtr
                    else do
                        err <- fetchResultError resPtr
                        c_duckdb_destroy_result resPtr
                        throwIO $ mkExecuteError query err

-- | Run a query with positional parameters and return all rows.
query :: (ToRow q, FromRow r) => Connection -> Query -> q -> IO [r]
query conn queryText params =
    withStatement conn queryText \stmt -> do
        bind stmt (toRow params)
        withStatementHandle stmt \handle ->
            alloca \resPtr -> do
                rc <- c_duckdb_execute_prepared handle resPtr
                if rc == DuckDBSuccess
                    then do
                        rows <- collectRows resPtr
                        c_duckdb_destroy_result resPtr
                        convertRows queryText rows
                    else do
                        err <- fetchResultError resPtr
                        c_duckdb_destroy_result resPtr
                        throwIO $ mkPrepareError queryText err

-- | Run a query without supplying parameters.
query_ :: (FromRow r) => Connection -> Query -> IO [r]
query_ conn queryText =
    withConnectionHandle conn \connPtr ->
        withQueryCString queryText \sql ->
            alloca \resPtr -> do
                rc <- c_duckdb_query connPtr sql resPtr
                if rc == DuckDBSuccess
                    then do
                        rows <- collectRows resPtr
                        c_duckdb_destroy_result resPtr
                        convertRows queryText rows
                    else do
                        err <- fetchResultError resPtr
                        c_duckdb_destroy_result resPtr
                        throwIO $ mkExecuteError queryText err

-- Internal helpers -----------------------------------------------------------

createConnection :: DuckDBDatabase -> DuckDBConnection -> IO Connection
createConnection db conn = do
    ref <- newIORef (ConnectionOpen db conn)
    _ <-
        mkWeakIORef ref $
            void $
                atomicModifyIORef' ref \case
                    ConnectionClosed -> (ConnectionClosed, pure ())
                    openState@(ConnectionOpen{}) ->
                        (ConnectionClosed, closeHandles openState)
    pure Connection{connectionState = ref}

createStatement :: Connection -> DuckDBPreparedStatement -> Query -> IO Statement
createStatement parent handle query = do
    ref <- newIORef (StatementOpen handle)
    _ <-
        mkWeakIORef ref $
            void $
                atomicModifyIORef' ref \case
                    StatementClosed -> (StatementClosed, pure ())
                    StatementOpen{statementHandle} ->
                        (StatementClosed, destroyPrepared statementHandle)
    pure
        Statement
            { statementState = ref
            , statementConnection = parent
            , statementQuery = query
            }

openDatabase :: FilePath -> IO DuckDBDatabase
openDatabase path =
    alloca \dbPtr ->
        alloca \errPtr -> do
            poke errPtr nullPtr
            withCString path \cPath -> do
                rc <- c_duckdb_open_ext cPath dbPtr nullPtr errPtr
                if rc == DuckDBSuccess
                    then do
                        db <- peek dbPtr
                        maybeFreeErr errPtr
                        pure db
                    else do
                        errMsg <- peekError errPtr
                        maybeFreeErr errPtr
                        throwIO $ mkOpenError errMsg

connectDatabase :: DuckDBDatabase -> IO DuckDBConnection
connectDatabase db =
    alloca \connPtr -> do
        rc <- c_duckdb_connect db connPtr
        if rc == DuckDBSuccess
            then peek connPtr
            else throwIO mkConnectError

closeHandles :: ConnectionState -> IO ()
closeHandles ConnectionClosed = pure ()
closeHandles ConnectionOpen{connectionDatabase, connectionHandle} = do
    closeConnectionHandle connectionHandle
    closeDatabaseHandle connectionDatabase

closeConnectionHandle :: DuckDBConnection -> IO ()
closeConnectionHandle conn =
    alloca \ptr -> poke ptr conn >> c_duckdb_disconnect ptr

closeDatabaseHandle :: DuckDBDatabase -> IO ()
closeDatabaseHandle db =
    alloca \ptr -> poke ptr db >> c_duckdb_close ptr

destroyPrepared :: DuckDBPreparedStatement -> IO ()
destroyPrepared stmt =
    alloca \ptr -> poke ptr stmt >> c_duckdb_destroy_prepare ptr

fetchPrepareError :: DuckDBPreparedStatement -> IO Text
fetchPrepareError stmt = do
    msgPtr <- c_duckdb_prepare_error stmt
    if msgPtr == nullPtr
        then pure (Text.pack "duckdb-simple: prepare failed")
        else Text.pack <$> peekCString msgPtr

fetchResultError :: Ptr DuckDBResult -> IO Text
fetchResultError resultPtr = do
    msgPtr <- c_duckdb_result_error resultPtr
    if msgPtr == nullPtr
        then pure (Text.pack "duckdb-simple: query failed")
        else Text.pack <$> peekCString msgPtr

mkOpenError :: Text -> SQLError
mkOpenError msg =
    SQLError
        { sqlErrorMessage = msg
        , sqlErrorType = Nothing
        , sqlErrorQuery = Nothing
        }

mkConnectError :: SQLError
mkConnectError =
    SQLError
        { sqlErrorMessage = Text.pack "duckdb-simple: failed to create connection handle"
        , sqlErrorType = Nothing
        , sqlErrorQuery = Nothing
        }

mkPrepareError :: Query -> Text -> SQLError
mkPrepareError query msg =
    SQLError
        { sqlErrorMessage = msg
        , sqlErrorType = Nothing
        , sqlErrorQuery = Just query
        }

mkExecuteError :: Query -> Text -> SQLError
mkExecuteError query msg =
    SQLError
        { sqlErrorMessage = msg
        , sqlErrorType = Nothing
        , sqlErrorQuery = Just query
        }

convertRows :: (FromRow r) => Query -> [[Field]] -> IO [r]
convertRows queryText rows =
    case traverse fromRow rows of
        Left err -> throwIO (resultErrorToSqlError queryText err)
        Right ok -> pure ok

collectRows :: Ptr DuckDBResult -> IO [[Field]]
collectRows resPtr = do
    columnCount <- fromIntegral <$> c_duckdb_column_count resPtr
    rowCount <- fromIntegral <$> c_duckdb_row_count resPtr
    mapM (collectRow resPtr columnCount) [0 .. rowCount - 1]

collectRow :: Ptr DuckDBResult -> Int -> Int -> IO [Field]
collectRow resPtr columnCount rowIndex =
    mapM (collectField resPtr rowIndex) [0 .. columnCount - 1]

collectField :: Ptr DuckDBResult -> Int -> Int -> IO Field
collectField resPtr rowIndex columnIndex = do
    namePtr <- c_duckdb_column_name resPtr (fromIntegral columnIndex)
    name <-
        if namePtr == nullPtr
            then pure (Text.pack ("column" <> show columnIndex))
            else Text.pack <$> peekCString namePtr
    dtype <- c_duckdb_column_type resPtr (fromIntegral columnIndex)
    value <- fetchFieldValue resPtr dtype columnIndex rowIndex
    pure
        Field
            { fieldName = name
            , fieldIndex = columnIndex
            , fieldValue = value
            }

fetchFieldValue :: Ptr DuckDBResult -> DuckDBType -> Int -> Int -> IO FieldValue
fetchFieldValue resPtr dtype columnIndex rowIndex = do
    let duckCol = fromIntegral columnIndex
        duckRow = fromIntegral rowIndex
    isNull <- c_duckdb_value_is_null resPtr duckCol duckRow
    if isNull /= CBool 0
        then pure FieldNull
        else case dtype of
            DuckDBTypeBoolean -> do
                CBool b <- c_duckdb_value_boolean resPtr duckCol duckRow
                pure (FieldBool (b /= 0))
            DuckDBTypeTinyInt -> FieldInt <$> c_duckdb_value_int64 resPtr duckCol duckRow
            DuckDBTypeSmallInt -> FieldInt <$> c_duckdb_value_int64 resPtr duckCol duckRow
            DuckDBTypeInteger -> FieldInt <$> c_duckdb_value_int64 resPtr duckCol duckRow
            DuckDBTypeBigInt -> FieldInt <$> c_duckdb_value_int64 resPtr duckCol duckRow
            DuckDBTypeHugeInt -> FieldInt <$> c_duckdb_value_int64 resPtr duckCol duckRow
            DuckDBTypeUTinyInt -> FieldInt <$> c_duckdb_value_int64 resPtr duckCol duckRow
            DuckDBTypeUSmallInt -> FieldInt <$> c_duckdb_value_int64 resPtr duckCol duckRow
            DuckDBTypeUInteger -> FieldInt <$> c_duckdb_value_int64 resPtr duckCol duckRow
            DuckDBTypeUBigInt -> FieldInt <$> c_duckdb_value_int64 resPtr duckCol duckRow
            DuckDBTypeFloat -> FieldDouble . realToFrac <$> c_duckdb_value_double resPtr duckCol duckRow
            DuckDBTypeDouble -> FieldDouble . realToFrac <$> c_duckdb_value_double resPtr duckCol duckRow
            DuckDBTypeVarchar -> FieldText <$> fetchTextValue resPtr duckCol duckRow
            _ -> FieldText <$> fetchTextValue resPtr duckCol duckRow

fetchTextValue :: Ptr DuckDBResult -> DuckDBIdx -> DuckDBIdx -> IO Text
fetchTextValue resPtr columnIndex rowIndex = do
    strPtr <- c_duckdb_value_varchar resPtr columnIndex rowIndex
    if strPtr == nullPtr
        then pure Text.empty
        else do
            value <- peekCString strPtr
            c_duckdb_free (castPtr strPtr)
            pure (Text.pack value)

peekError :: Ptr CString -> IO Text
peekError ptr = do
    errPtr <- peek ptr
    if errPtr == nullPtr
        then pure (Text.pack "duckdb-simple: failed to open database")
        else do
            message <- peekCString errPtr
            pure (Text.pack message)

maybeFreeErr :: Ptr CString -> IO ()
maybeFreeErr ptr = do
    errPtr <- peek ptr
    when (errPtr /= nullPtr) $ c_duckdb_free (castPtr errPtr)
