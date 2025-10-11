{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}

{- |
Module      : Database.DuckDB.Simple
Description : High-level DuckDB API in the duckdb-simple style.

The API mirrors the ergonomics of @sqlite-simple@ while being backed by the
DuckDB C API. It supports connection management, parameter binding, execution,
and typed result decoding. See 'README.md' for usage examples.
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
    columnCount,
    columnName,
    executeStatement,
    execute,
    executeMany,
    execute_,
    bind,
    bindNamed,
    executeNamed,
    queryNamed,
    withTransaction,
    withImmediateTransaction,
    withExclusiveTransaction,
    withSavepoint,
    query,
    query_,

    -- * Metadata
    rowsChanged,

    -- * Errors and conversions
    SQLError (..),
    FormatError (..),
    ResultError (..),
    FromField (..),
    FromRow (..),
    RowParser,
    field,
    fieldWith,
    numFieldsRemaining,
    -- Re-export parameter helper types.
    ToField (..),
    ToRow (..),
    FieldBinding,
    NamedParam (..),
    Null (..),
    Only (..),
    (:.) (..),

    -- * User-defined scalar functions
    Function,
    createFunction,
    deleteFunction,
) where

import Control.Exception (bracket, mask, onException, throwIO, try)
import Control.Monad (forM, void, when, zipWithM_)
import Data.IORef (atomicModifyIORef', mkWeakIORef, newIORef, readIORef)
import Data.Maybe (isJust, isNothing)
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
import Database.DuckDB.Simple.FromRow (
    FromRow (..),
    RowParser,
    field,
    fieldWith,
    numFieldsRemaining,
    parseRow,
    resultErrorToSqlError,
 )
import Database.DuckDB.Simple.Function (Function, createFunction, deleteFunction)
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
import Database.DuckDB.Simple.ToField (FieldBinding, NamedParam (..), ToField (..), bindFieldBinding, renderFieldBinding)
import Database.DuckDB.Simple.ToRow (ToRow (..))
import Database.DuckDB.Simple.Types (FormatError (..), Null (..), Only (..), (:.) (..))
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CBool (..), CDouble (..))
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

-- | Bind positional parameters to a prepared statement, replacing any previous bindings.
bind :: Statement -> [FieldBinding] -> IO ()
bind stmt fields = do
    withStatementHandle stmt \handle -> do
        let actual = length fields
        expected <- fmap fromIntegral (c_duckdb_nparams handle)
        when (actual /= expected) $
            throwFormatErrorBindings stmt (parameterCountMessage expected actual) fields
        parameterNames <- fetchParameterNames handle expected
        when (any isJust parameterNames) $
            throwFormatErrorBindings stmt (Text.pack "duckdb-simple: statement defines named parameters; use executeNamed or bindNamed") fields
    clearStatementBindings stmt
    zipWithM_ apply [1 ..] fields
  where
    parameterCountMessage expected actual =
        Text.pack $
            "duckdb-simple: SQL query contains "
                <> show expected
                <> " parameter(s), but "
                <> show actual
                <> " argument(s) were supplied"

    apply :: Int -> FieldBinding -> IO ()
    apply idx field = bindFieldBinding stmt (fromIntegral idx :: DuckDBIdx) field

-- | Bind named parameters to a prepared statement, preserving any positional bindings.
bindNamed :: Statement -> [NamedParam] -> IO ()
bindNamed stmt params =
    let bindings = fmap (\(name := value) -> (name, toField value)) params
        parameterCountMessage expected actual =
            Text.pack $
                "duckdb-simple: SQL query contains "
                    <> show expected
                    <> " named parameter(s), but "
                    <> show actual
                    <> " argument(s) were supplied"
        unknownNameMessage name =
            Text.concat
                [ Text.pack "duckdb-simple: unknown named parameter "
                , name
                ]
        apply (name, binding) = do
            mIdx <- namedParameterIndex stmt name
            case mIdx of
                Nothing ->
                    throwFormatErrorNamed stmt (unknownNameMessage name) bindings
                Just idx -> bindFieldBinding stmt (fromIntegral idx :: DuckDBIdx) binding
     in do
            withStatementHandle stmt \handle -> do
                let actual = length bindings
                expected <- fmap fromIntegral (c_duckdb_nparams handle)
                when (actual /= expected) $
                    throwFormatErrorNamed stmt (parameterCountMessage expected actual) bindings
                parameterNames <- fetchParameterNames handle expected
                when (all isNothing parameterNames && expected > 0) $
                    throwFormatErrorNamed stmt (Text.pack "duckdb-simple: statement does not define named parameters; use positional bindings or adjust the SQL") bindings
            clearStatementBindings stmt
            mapM_ apply bindings

fetchParameterNames :: DuckDBPreparedStatement -> Int -> IO [Maybe Text]
fetchParameterNames handle count =
    forM [1 .. count] \idx -> do
        namePtr <- c_duckdb_parameter_name handle (fromIntegral idx)
        if namePtr == nullPtr
            then pure Nothing
            else do
                name <- Text.pack <$> peekCString namePtr
                c_duckdb_free (castPtr namePtr)
                let normalized = normalizeName name
                if normalized == Text.pack (show idx)
                    then pure Nothing
                    else pure (Just name)

-- | Remove all parameter bindings associated with a prepared statement.
clearStatementBindings :: Statement -> IO ()
clearStatementBindings stmt =
    withStatementHandle stmt \handle -> do
        rc <- c_duckdb_clear_bindings handle
        when (rc /= DuckDBSuccess) $ do
            err <- fetchPrepareError handle
            throwIO $ mkPrepareError (statementQuery stmt) err

-- | Look up the 1-based index of a named placeholder.
namedParameterIndex :: Statement -> Text -> IO (Maybe Int)
namedParameterIndex stmt name =
    withStatementHandle stmt \handle ->
        let normalized = normalizeName name
         in TextForeign.withCString normalized \cName ->
                alloca \idxPtr -> do
                    rc <- c_duckdb_bind_parameter_index handle idxPtr cName
                    if rc == DuckDBSuccess
                        then do
                            idx <- peek idxPtr
                            if idx == 0
                                then pure Nothing
                                else pure (Just (fromIntegral idx))
                        else pure Nothing

{- | Return the number of rows affected by the most recent data-changing statement
executed on the supplied connection.

The counter updates when functions such as 'execute', 'executeMany',
'executeNamed', and 'execute_' complete successfully. DuckDB does not expose a
stable @last_insert_rowid@ equivalent; prefer SQL @RETURNING@ clauses when you
need to capture generated identifiers.
-}
rowsChanged :: Connection -> IO Int
rowsChanged Connection{connectionRowsChanged} = readIORef connectionRowsChanged

-- | Retrieve the number of columns produced by the supplied prepared statement.
columnCount :: Statement -> IO Int
columnCount stmt =
    withStatementHandle stmt \handle ->
        fmap fromIntegral (c_duckdb_prepared_statement_column_count handle)

-- | Look up the zero-based column name exposed by a prepared statement result.
columnName :: Statement -> Int -> IO Text
columnName stmt columnIndex
    | columnIndex < 0 = throwIO (columnIndexError stmt columnIndex Nothing)
    | otherwise =
        withStatementHandle stmt \handle -> do
            total <- fmap fromIntegral (c_duckdb_prepared_statement_column_count handle)
            when (columnIndex >= total) $
                throwIO (columnIndexError stmt columnIndex (Just total))
            namePtr <- c_duckdb_prepared_statement_column_name handle (fromIntegral columnIndex)
            if namePtr == nullPtr
                then throwIO (columnNameUnavailableError stmt columnIndex)
                else do
                    name <- Text.pack <$> peekCString namePtr
                    c_duckdb_free (castPtr namePtr)
                    pure name

executeStatement :: Statement -> IO Int
executeStatement stmt@Statement{statementConnection} =
    withStatementHandle stmt \handle ->
        alloca \resPtr -> do
            rc <- c_duckdb_execute_prepared handle resPtr
            if rc == DuckDBSuccess
                then do
                    changed <- resultRowsChanged resPtr
                    c_duckdb_destroy_result resPtr
                    recordRowsChanged statementConnection changed
                    pure changed
                else do
                    (errMsg, _) <- fetchResultError resPtr
                    c_duckdb_destroy_result resPtr
                    throwIO $ mkPrepareError (statementQuery stmt) errMsg

-- | Execute a query with positional parameters and return the affected row count.
execute :: (ToRow q) => Connection -> Query -> q -> IO Int
execute conn queryText params =
    withStatement conn queryText \stmt -> do
        bind stmt (toRow params)
        executeStatement stmt

-- | Execute the same query multiple times with different parameter sets.
executeMany :: (ToRow q) => Connection -> Query -> [q] -> IO Int
executeMany conn queryText rows =
    withStatement conn queryText \stmt -> do
        changes <- mapM (\row -> bind stmt (toRow row) >> executeStatement stmt) rows
        let total = sum changes
        recordRowsChanged conn total
        pure total

-- | Execute an ad-hoc query without parameters and return the affected row count.
execute_ :: Connection -> Query -> IO Int
execute_ conn queryText =
    withConnectionHandle conn \connPtr ->
        withQueryCString queryText \sql ->
            alloca \resPtr -> do
                rc <- c_duckdb_query connPtr sql resPtr
                if rc == DuckDBSuccess
                    then do
                        changed <- resultRowsChanged resPtr
                        c_duckdb_destroy_result resPtr
                        recordRowsChanged conn changed
                        pure changed
                    else do
                        (errMsg, errType) <- fetchResultError resPtr
                        c_duckdb_destroy_result resPtr
                        throwIO $ mkExecuteError queryText errMsg errType

-- | Execute a query that uses named parameters.
executeNamed :: Connection -> Query -> [NamedParam] -> IO Int
executeNamed conn queryText params =
    withStatement conn queryText \stmt -> do
        bindNamed stmt params
        executeStatement stmt

-- | Run a parameterised query and decode all rows eagerly.
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
                        (errMsg, errType) <- fetchResultError resPtr
                        c_duckdb_destroy_result resPtr
                        throwIO $ mkExecuteError queryText errMsg errType

-- | Run a query that uses named parameters and decode all rows eagerly.
queryNamed :: (FromRow r) => Connection -> Query -> [NamedParam] -> IO [r]
queryNamed conn queryText params =
    withStatement conn queryText \stmt -> do
        bindNamed stmt params
        withStatementHandle stmt \handle ->
            alloca \resPtr -> do
                rc <- c_duckdb_execute_prepared handle resPtr
                if rc == DuckDBSuccess
                    then do
                        rows <- collectRows resPtr
                        c_duckdb_destroy_result resPtr
                        convertRows queryText rows
                    else do
                        (errMsg, errType) <- fetchResultError resPtr
                        c_duckdb_destroy_result resPtr
                        throwIO $ mkExecuteError queryText errMsg errType

-- | Run a query without supplying parameters and decode all rows eagerly.
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
                        (errMsg, errType) <- fetchResultError resPtr
                        c_duckdb_destroy_result resPtr
                        throwIO $ mkExecuteError queryText errMsg errType

-- | Run an action inside a deferred transaction.
withTransaction :: Connection -> IO a -> IO a
withTransaction =
    withTransactionMode
        (Query (Text.pack "BEGIN TRANSACTION"))
        (Query (Text.pack "COMMIT"))
        (Query (Text.pack "ROLLBACK"))

-- | Run an action inside an immediate transaction.
withImmediateTransaction :: Connection -> IO a -> IO a
withImmediateTransaction =
    withTransactionMode
        (Query (Text.pack "BEGIN TRANSACTION"))
        (Query (Text.pack "COMMIT"))
        (Query (Text.pack "ROLLBACK"))

-- | Run an action inside an exclusive transaction.
withExclusiveTransaction :: Connection -> IO a -> IO a
withExclusiveTransaction =
    withTransactionMode
        (Query (Text.pack "BEGIN TRANSACTION"))
        (Query (Text.pack "COMMIT"))
        (Query (Text.pack "ROLLBACK"))

{- | Run an action within a named savepoint.
  Throws an 'SQLError' when the underlying DuckDB build does not support savepoints.
-}
withSavepoint :: Connection -> Text -> IO a -> IO a
withSavepoint conn label action =
    mask \restore -> do
        let begin = Query (Text.concat [Text.pack "SAVEPOINT ", label])
        beginResult <- try (execute_ conn begin)
        case beginResult of
            Left err
                | isSavepointUnsupported err -> throwIO (savepointUnsupportedError begin)
                | otherwise -> throwIO err
            Right _ -> do
                let commit = Query (Text.concat [Text.pack "RELEASE ", label])
                    rollback = Query (Text.concat [Text.pack "ROLLBACK TO ", label])
                    rollbackAction = executeIgnore conn rollback
                result <- restore action `onException` rollbackAction
                void (execute_ conn commit)
                pure result

withTransactionMode :: Query -> Query -> Query -> Connection -> IO a -> IO a
withTransactionMode begin commit rollback conn action =
    mask \restore -> do
        void (execute_ conn begin)
        let rollbackAction = executeIgnore conn rollback
        result <- restore action `onException` rollbackAction
        void (execute_ conn commit)
        pure result

executeIgnore :: Connection -> Query -> IO ()
executeIgnore conn q = void (execute_ conn q)

recordRowsChanged :: Connection -> Int -> IO ()
recordRowsChanged Connection{connectionRowsChanged} value =
    atomicModifyIORef'
        connectionRowsChanged
        (const (value, ()))

-- Internal helpers -----------------------------------------------------------

createConnection :: DuckDBDatabase -> DuckDBConnection -> IO Connection
createConnection db conn = do
    ref <- newIORef (ConnectionOpen db conn)
    rowsChangedRef <- newIORef 0
    _ <-
        mkWeakIORef ref $
            void $
                atomicModifyIORef' ref \case
                    ConnectionClosed -> (ConnectionClosed, pure ())
                    openState@(ConnectionOpen{}) ->
                        (ConnectionClosed, closeHandles openState)
    pure Connection{connectionState = ref, connectionRowsChanged = rowsChangedRef}

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

fetchResultError :: Ptr DuckDBResult -> IO (Text, Maybe DuckDBErrorType)
fetchResultError resultPtr = do
    msgPtr <- c_duckdb_result_error resultPtr
    msg <-
        if msgPtr == nullPtr
            then pure (Text.pack "duckdb-simple: query failed")
            else Text.pack <$> peekCString msgPtr
    errType <- c_duckdb_result_error_type resultPtr
    let classified =
            if errType == DuckDBErrorInvalid
                then Nothing
                else Just errType
    pure (msg, classified)

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

mkExecuteError :: Query -> Text -> Maybe DuckDBErrorType -> SQLError
mkExecuteError query msg errType =
    SQLError
        { sqlErrorMessage = msg
        , sqlErrorType = errType
        , sqlErrorQuery = Just query
        }

throwFormatError :: Statement -> Text -> [String] -> IO a
throwFormatError Statement{statementQuery} message params =
    throwIO
        FormatError
            { formatErrorMessage = message
            , formatErrorQuery = statementQuery
            , formatErrorParams = params
            }

throwFormatErrorBindings :: Statement -> Text -> [FieldBinding] -> IO a
throwFormatErrorBindings stmt message bindings =
    throwFormatError stmt message (map renderFieldBinding bindings)

throwFormatErrorNamed :: Statement -> Text -> [(Text, FieldBinding)] -> IO a
throwFormatErrorNamed stmt message bindings =
    throwFormatError stmt message (map renderNamed bindings)
  where
    renderNamed (name, binding) =
        Text.unpack name <> " := " <> renderFieldBinding binding

columnIndexError :: Statement -> Int -> Maybe Int -> SQLError
columnIndexError stmt idx total =
    let base =
            Text.concat
                [ Text.pack "duckdb-simple: column index "
                , Text.pack (show idx)
                , Text.pack " out of bounds"
                ]
        message =
            case total of
                Nothing -> base
                Just count ->
                    Text.concat
                        [ base
                        , Text.pack " (column count: "
                        , Text.pack (show count)
                        , Text.pack ")"
                        ]
     in SQLError
            { sqlErrorMessage = message
            , sqlErrorType = Nothing
            , sqlErrorQuery = Just (statementQuery stmt)
            }

columnNameUnavailableError :: Statement -> Int -> SQLError
columnNameUnavailableError stmt idx =
    SQLError
        { sqlErrorMessage =
            Text.concat
                [ Text.pack "duckdb-simple: column name unavailable for index "
                , Text.pack (show idx)
                ]
        , sqlErrorType = Nothing
        , sqlErrorQuery = Just (statementQuery stmt)
        }

isSavepointUnsupported :: SQLError -> Bool
isSavepointUnsupported SQLError{sqlErrorMessage} =
    Text.isInfixOf (Text.pack "SAVEPOINT") sqlErrorMessage

savepointUnsupportedError :: Query -> SQLError
savepointUnsupportedError query =
    SQLError
        { sqlErrorMessage = Text.pack "duckdb-simple: savepoints are not supported by this DuckDB build"
        , sqlErrorType = Nothing
        , sqlErrorQuery = Just query
        }

normalizeName :: Text -> Text
normalizeName name =
    case Text.uncons name of
        Just (prefix, rest)
            | prefix == ':' || prefix == '$' || prefix == '@' -> rest
        _ -> name

resultRowsChanged :: Ptr DuckDBResult -> IO Int
resultRowsChanged resPtr = fromIntegral <$> c_duckdb_rows_changed resPtr

convertRows :: (FromRow r) => Query -> [[Field]] -> IO [r]
convertRows queryText rows =
    case traverse (parseRow fromRow) rows of
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
            DuckDBTypeFloat -> do
                CDouble d <- c_duckdb_value_double resPtr duckCol duckRow
                pure (FieldDouble (realToFrac d))
            DuckDBTypeDouble -> do
                CDouble d <- c_duckdb_value_double resPtr duckCol duckRow
                pure (FieldDouble (realToFrac d))
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
