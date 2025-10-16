{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

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
    fold,
    fold_,
    foldNamed,
    withTransaction,
    query,
    queryWith,
    query_,
    queryWith_,
    nextRow,
    nextRowWith,

    -- * Errors and conversions
    SQLError (..),
    FormatError (..),
    ResultError (..),
    FieldParser,
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
    DuckDBColumnType (..),
    duckdbColumnType,
    Null (..),
    Only (..),
    (:.) (..),

    -- * User-defined scalar functions
    Function,
    createFunction,
    deleteFunction,
) where

import Control.Monad (forM, join, void, when, zipWithM, zipWithM_)
import Data.IORef (IORef, atomicModifyIORef', mkWeakIORef, newIORef, readIORef, writeIORef)
import Control.Exception (SomeException, bracket, finally, mask, onException, throwIO, try)
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Foreign as TextForeign
import Database.DuckDB.FFI
import Database.DuckDB.Simple.FromField
    ( Field (..)
    , FieldParser
    , FromField (..)
    , ResultError (..)
    )
import Database.DuckDB.Simple.FromRow
    ( FromRow (..)
    , RowParser
    , field
    , fieldWith
    , numFieldsRemaining
    , parseRow
    , rowErrorsToSqlError
    )
import Database.DuckDB.Simple.Function (Function, createFunction, deleteFunction)
import Database.DuckDB.Simple.Materialize
    ( materializeValue )
import Database.DuckDB.Simple.Internal
    ( Connection (..)
    , ConnectionState (..)
    , Query (..)
    , SQLError (..)
    , Statement (..)
    , StatementState (..)
    , StatementStream (..)
    , StatementStreamChunk (..)
    , StatementStreamChunkVector (..)
    , StatementStreamColumn (..)
    , StatementStreamState (..)
    , withConnectionHandle
    , withQueryCString
    , withStatementHandle
    )
import Database.DuckDB.Simple.ToField (DuckDBColumnType (..), FieldBinding, NamedParam (..), ToField (..), bindFieldBinding, duckdbColumnType, renderFieldBinding)
import Database.DuckDB.Simple.ToRow (ToRow (..))
import Database.DuckDB.Simple.Types (FormatError (..), Null (..), Only (..), (:.) (..))
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.Marshal.Alloc (alloca, free, malloc)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (peek, poke)
import Database.DuckDB.Simple.Ok (Ok(..))

-- | Open a DuckDB database located at the supplied path.
open :: FilePath -> IO Connection
open path =
    mask \restore -> do
        db <- restore (openDatabase path)
        conn <-
            restore (connectDatabase db)
                `onException` closeDatabaseHandle db
        createConnection db conn
            `onException` do
                closeConnectionHandle conn
                closeDatabaseHandle db

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
openStatement conn queryText =
    mask \restore -> do
        handle <-
            restore $
                withConnectionHandle conn \connPtr ->
                    withQueryCString queryText \sql ->
                        alloca \stmtPtr -> do
                            rc <- c_duckdb_prepare connPtr sql stmtPtr
                            stmt <- peek stmtPtr
                            if rc == DuckDBSuccess
                                then pure stmt
                                else do
                                    errMsg <- fetchPrepareError stmt
                                    c_duckdb_destroy_prepare stmtPtr
                                    throwIO $ mkPrepareError queryText errMsg
        createStatement conn handle queryText
            `onException` destroyPrepared handle

-- | Finalise a prepared statement.  The operation is idempotent.
closeStatement :: Statement -> IO ()
closeStatement stmt@Statement{statementState} = do
    resetStatementStream stmt
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
    resetStatementStream stmt
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
    apply idx = bindFieldBinding stmt (fromIntegral idx :: DuckDBIdx)

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
            resetStatementStream stmt
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

-- | Retrieve the number of columns produced by the supplied prepared statement.
columnCount :: Statement -> IO Int
columnCount stmt =
    withStatementHandle stmt $ \handle ->
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

-- | Execute a prepared statement and return the number of affected rows.
--   Resets any active result stream before running and raises a 'SqlError'
--   if DuckDB reports a failure.
executeStatement :: Statement -> IO Int
executeStatement stmt =
    withStatementHandle stmt \handle -> do
        resetStatementStream stmt
        alloca \resPtr -> do
            rc <- c_duckdb_execute_prepared handle resPtr
            if rc == DuckDBSuccess
                then do
                    changed <- resultRowsChanged resPtr
                    c_duckdb_destroy_result resPtr
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
        sum <$> mapM (\row -> bind stmt (toRow row) >> executeStatement stmt) rows

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

-- | Run a parameterised query and decode every resulting row eagerly.
query :: (ToRow q, FromRow r) => Connection -> Query -> q -> IO [r]
query = queryWith fromRow

-- | Run a parameterised query with a custom row parser.
queryWith :: (ToRow q) => RowParser r -> Connection -> Query -> q -> IO [r]
queryWith parser conn queryText params =
    withStatement conn queryText \stmt -> do
        bind stmt (toRow params)
        withStatementHandle stmt \handle ->
            alloca \resPtr -> do
                rc <- c_duckdb_execute_prepared handle resPtr
                if rc == DuckDBSuccess
                    then do
                        rows <- collectRows resPtr
                        c_duckdb_destroy_result resPtr
                        convertRowsWith parser queryText rows
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
query_ = queryWith_ fromRow

-- | Run a query without parameters using a custom row parser.
queryWith_ :: RowParser r -> Connection -> Query -> IO [r]
queryWith_ parser conn queryText =
    withConnectionHandle conn \connPtr ->
        withQueryCString queryText \sql ->
            alloca \resPtr -> do
                rc <- c_duckdb_query connPtr sql resPtr
                if rc == DuckDBSuccess
                    then do
                        rows <- collectRows resPtr
                        c_duckdb_destroy_result resPtr
                        convertRowsWith parser queryText rows
                    else do
                        (errMsg, errType) <- fetchResultError resPtr
                        c_duckdb_destroy_result resPtr
                        throwIO $ mkExecuteError queryText errMsg errType

-- Streaming folds -----------------------------------------------------------

-- | Stream a parameterised query through an accumulator without loading all rows.
--   Bind the supplied parameters, start a streaming result, and apply the step
--   function row by row to produce a final accumulator value.
fold :: (FromRow row, ToRow params) => Connection -> Query -> params -> a -> (a -> row -> IO a) -> IO a
fold conn queryText params initial step =
    withStatement conn queryText \stmt -> do
        resetStatementStream stmt
        bind stmt (toRow params)
        foldStatementWith fromRow stmt initial step

-- | Stream a parameterless query through an accumulator without loading all rows.
fold_ :: (FromRow row) => Connection -> Query -> a -> (a -> row -> IO a) -> IO a
fold_ conn queryText initial step =
    withStatement conn queryText \stmt -> do
        resetStatementStream stmt
        foldStatementWith fromRow stmt initial step

-- | Stream a query that uses named parameters through an accumulator.
foldNamed :: (FromRow row) => Connection -> Query -> [NamedParam] -> a -> (a -> row -> IO a) -> IO a
foldNamed conn queryText params initial step =
    withStatement conn queryText \stmt -> do
        resetStatementStream stmt
        bindNamed stmt params
        foldStatementWith fromRow stmt initial step

foldStatementWith :: RowParser row -> Statement -> a -> (a -> row -> IO a) -> IO a
foldStatementWith parser stmt initial step =
    let loop acc = do
            nextVal <- nextRowWith parser stmt
            case nextVal of
                Nothing -> pure acc
                Just row -> do
                    acc' <- step acc row
                    acc' `seq` loop acc'
     in loop initial `finally` resetStatementStream stmt

-- | Fetch the next row from a streaming statement, stopping when no rows remain.
nextRow :: (FromRow r) => Statement -> IO (Maybe r)
nextRow = nextRowWith fromRow

-- | Fetch the next row using a custom parser, returning 'Nothing' once exhausted.
nextRowWith :: RowParser r -> Statement -> IO (Maybe r)
nextRowWith parser stmt@Statement{statementStream} =
    mask \restore -> do
        state <- readIORef statementStream
        case state of
            StatementStreamIdle -> do
                newStream <- restore (startStatementStream stmt)
                case newStream of
                    Nothing -> pure Nothing
                    Just stream -> restore (consumeStream statementStream parser stmt stream)
            StatementStreamActive stream ->
                restore (consumeStream statementStream parser stmt stream)

resetStatementStream :: Statement -> IO ()
resetStatementStream Statement{statementStream} =
    cleanupStatementStreamRef statementStream

consumeStream :: IORef StatementStreamState -> RowParser r -> Statement -> StatementStream -> IO (Maybe r)
consumeStream streamRef parser stmt stream = do
    result <-
        ( try (streamNextRow (statementQuery stmt) stream)
            :: IO (Either SomeException (Maybe [Field], StatementStream))
        )
    case result of
        Left err -> do
            finalizeStream stream
            writeIORef streamRef StatementStreamIdle
            throwIO err
        Right (maybeFields, updatedStream) ->
            case maybeFields of
                Nothing -> do
                    finalizeStream updatedStream
                    writeIORef streamRef StatementStreamIdle
                    pure Nothing
                Just fields ->
                    case parseRow parser fields of
                        Errors rowErr -> do
                            finalizeStream updatedStream
                            writeIORef streamRef StatementStreamIdle
                            throwIO $ rowErrorsToSqlError (statementQuery stmt) rowErr
                        Ok value -> do
                            writeIORef streamRef (StatementStreamActive updatedStream)
                            pure (Just value)

startStatementStream :: Statement -> IO (Maybe StatementStream)
startStatementStream stmt =
    withStatementHandle stmt \handle -> do
        columns <- collectStreamColumns handle
        resultPtr <- malloc
        rc <- c_duckdb_execute_prepared handle resultPtr
        if rc /= DuckDBSuccess
            then do
                (errMsg, errType) <- fetchResultError resultPtr
                c_duckdb_destroy_result resultPtr
                free resultPtr
                throwIO $ mkExecuteError (statementQuery stmt) errMsg errType
            else do
                resultType <- c_duckdb_result_return_type resultPtr
                if resultType /= DuckDBResultTypeQueryResult
                    then do
                        c_duckdb_destroy_result resultPtr
                        free resultPtr
                        pure Nothing
                    else pure (Just (StatementStream resultPtr columns Nothing))

collectStreamColumns :: DuckDBPreparedStatement -> IO [StatementStreamColumn]
collectStreamColumns handle = do
    rawCount <- c_duckdb_prepared_statement_column_count handle
    let cc = fromIntegral rawCount :: Int
    forM [0 .. cc - 1] \idx -> do
        namePtr <- c_duckdb_prepared_statement_column_name handle (fromIntegral idx)
        name <-
            if namePtr == nullPtr
                then pure (Text.pack ("column" <> show idx))
                else Text.pack <$> peekCString namePtr
        dtype <- c_duckdb_prepared_statement_column_type handle (fromIntegral idx)
        pure
            StatementStreamColumn
                { statementStreamColumnIndex = idx
                , statementStreamColumnName = name
                , statementStreamColumnType = dtype
                }

streamNextRow :: Query -> StatementStream -> IO (Maybe [Field], StatementStream)
streamNextRow queryText stream@StatementStream{statementStreamChunk = Nothing} = do
    refreshed <- fetchChunk stream
    case statementStreamChunk refreshed of
        Nothing -> pure (Nothing, refreshed)
        Just chunk -> emitRow queryText refreshed chunk
streamNextRow queryText stream@StatementStream{statementStreamChunk = Just chunk} =
    emitRow queryText stream chunk

fetchChunk :: StatementStream -> IO StatementStream
fetchChunk stream@StatementStream{statementStreamResult} = do
    chunk <- c_duckdb_fetch_chunk statementStreamResult
    if chunk == nullPtr
        then pure stream
        else do
            rawSize <- c_duckdb_data_chunk_get_size chunk
            let rowCount = fromIntegral rawSize :: Int
            if rowCount <= 0
                then do
                    destroyDataChunk chunk
                    fetchChunk stream
                else do
                    vectors <- prepareChunkVectors chunk (statementStreamColumns stream)
                    let chunkState =
                            StatementStreamChunk
                                { statementStreamChunkPtr = chunk
                                , statementStreamChunkSize = rowCount
                                , statementStreamChunkIndex = 0
                                , statementStreamChunkVectors = vectors
                                }
                    pure stream{statementStreamChunk = Just chunkState}

prepareChunkVectors :: DuckDBDataChunk -> [StatementStreamColumn] -> IO [StatementStreamChunkVector]
prepareChunkVectors chunk columns =
    forM columns \StatementStreamColumn{statementStreamColumnIndex} -> do
        vector <- c_duckdb_data_chunk_get_vector chunk (fromIntegral statementStreamColumnIndex)
        dataPtr <- c_duckdb_vector_get_data vector
        validity <- c_duckdb_vector_get_validity vector
        pure
            StatementStreamChunkVector
                { statementStreamChunkVectorHandle = vector
                , statementStreamChunkVectorData = dataPtr
                , statementStreamChunkVectorValidity = validity
                }

emitRow :: Query -> StatementStream -> StatementStreamChunk -> IO (Maybe [Field], StatementStream)
emitRow queryText stream chunk@StatementStreamChunk{statementStreamChunkIndex, statementStreamChunkSize} = do
    fields <-
        buildRow
            queryText
            (statementStreamColumns stream)
            (statementStreamChunkVectors chunk)
            statementStreamChunkIndex
    let nextIndex = statementStreamChunkIndex + 1
    if nextIndex < statementStreamChunkSize
        then
            let updatedChunk = chunk{statementStreamChunkIndex = nextIndex}
             in pure (Just fields, stream{statementStreamChunk = Just updatedChunk})
        else do
            destroyDataChunk (statementStreamChunkPtr chunk)
            pure (Just fields, stream{statementStreamChunk = Nothing})

buildRow :: Query -> [StatementStreamColumn] -> [StatementStreamChunkVector] -> Int -> IO [Field]
buildRow queryText columns vectors rowIdx =
    zipWithM (buildField queryText rowIdx) columns vectors

buildField :: Query -> Int -> StatementStreamColumn -> StatementStreamChunkVector -> IO Field
buildField queryText rowIdx column StatementStreamChunkVector{statementStreamChunkVectorHandle, statementStreamChunkVectorData, statementStreamChunkVectorValidity} = do
    let dtype = statementStreamColumnType column
    value <-
        case dtype of
            DuckDBTypeStruct ->
                throwIO (streamingUnsupportedTypeError queryText column)
            DuckDBTypeUnion ->
                throwIO (streamingUnsupportedTypeError queryText column)
            _ ->
                materializeValue
                    dtype
                    statementStreamChunkVectorHandle
                    statementStreamChunkVectorData
                    statementStreamChunkVectorValidity
                    rowIdx
    pure
        Field
            { fieldName = statementStreamColumnName column
            , fieldIndex = statementStreamColumnIndex column
            , fieldValue = value
            }

cleanupStatementStreamRef :: IORef StatementStreamState -> IO ()
cleanupStatementStreamRef ref = do
    state <- atomicModifyIORef' ref (StatementStreamIdle,)
    finalizeStreamState state

finalizeStreamState :: StatementStreamState -> IO ()
finalizeStreamState = \case
    StatementStreamIdle -> pure ()
    StatementStreamActive stream -> finalizeStream stream

finalizeStream :: StatementStream -> IO ()
finalizeStream StatementStream{statementStreamResult, statementStreamChunk} = do
    maybe (pure ()) finalizeChunk statementStreamChunk
    c_duckdb_destroy_result statementStreamResult
    free statementStreamResult

finalizeChunk :: StatementStreamChunk -> IO ()
finalizeChunk StatementStreamChunk{statementStreamChunkPtr} =
    destroyDataChunk statementStreamChunkPtr

destroyDataChunk :: DuckDBDataChunk -> IO ()
destroyDataChunk chunk =
    alloca \ptr -> do
        poke ptr chunk
        c_duckdb_destroy_data_chunk ptr

streamingUnsupportedTypeError :: Query -> StatementStreamColumn -> SQLError
streamingUnsupportedTypeError queryText StatementStreamColumn{statementStreamColumnName, statementStreamColumnType} =
    SQLError
        { sqlErrorMessage =
            Text.concat
                [ Text.pack "duckdb-simple: streaming does not yet support column "
                , statementStreamColumnName
                , Text.pack " with DuckDB type "
                , Text.pack (show statementStreamColumnType)
                ]
        , sqlErrorType = Nothing
        , sqlErrorQuery = Just queryText
        }

-- | Run an action inside a transaction.
withTransaction :: Connection -> IO a -> IO a
withTransaction conn action =
    mask \restore -> do
        void (execute_ conn begin)
        let rollbackAction = void (execute_ conn rollback)
        result <- restore action `onException` rollbackAction
        void (execute_ conn commit)
        pure result

  where begin = Query (Text.pack "BEGIN TRANSACTION")
        commit = Query (Text.pack "COMMIT")
        rollback = Query (Text.pack "ROLLBACK")


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
createStatement parent handle queryText = do
    ref <- newIORef (StatementOpen handle)
    streamRef <- newIORef StatementStreamIdle
    _ <-
        mkWeakIORef ref $
          do join $
               atomicModifyIORef' ref $ \case
                        StatementClosed -> (StatementClosed, pure ())
                        StatementOpen{statementHandle} ->
                            ( StatementClosed
                            , do
                                cleanupStatementStreamRef streamRef
                                destroyPrepared statementHandle
                            )
    pure
        Statement
            { statementState = ref
            , statementConnection = parent
            , statementQuery = queryText
            , statementStream = streamRef
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
mkPrepareError queryText msg =
    SQLError
        { sqlErrorMessage = msg
        , sqlErrorType = Nothing
        , sqlErrorQuery = Just queryText
        }

mkExecuteError :: Query -> Text -> Maybe DuckDBErrorType -> SQLError
mkExecuteError queryText msg errType =
    SQLError
        { sqlErrorMessage = msg
        , sqlErrorType = errType
        , sqlErrorQuery = Just queryText
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


normalizeName :: Text -> Text
normalizeName name =
    case Text.uncons name of
        Just (prefix, rest)
            | prefix == ':' || prefix == '$' || prefix == '@' -> rest
        _ -> name

resultRowsChanged :: Ptr DuckDBResult -> IO Int
resultRowsChanged resPtr = fromIntegral <$> c_duckdb_rows_changed resPtr

convertRows :: (FromRow r) => Query -> [[Field]] -> IO [r]
convertRows = convertRowsWith fromRow

convertRowsWith :: RowParser r -> Query -> [[Field]] -> IO [r]
convertRowsWith parser queryText rows =
    case traverse (parseRow parser) rows of
        Errors err -> throwIO (rowErrorsToSqlError queryText err)
        Ok ok -> pure ok

collectRows :: Ptr DuckDBResult -> IO [[Field]]
collectRows resPtr = do
    columns <- collectResultColumns resPtr
    collectChunks columns []
  where
    collectChunks columns acc = do
        chunk <- c_duckdb_fetch_chunk resPtr
        if chunk == nullPtr
            then pure (concat (reverse acc))
            else do
                rows <-
                    finally
                        (decodeChunk columns chunk)
                        (destroyDataChunk chunk)
                let acc' = maybe acc (: acc) rows
                collectChunks columns acc'

    decodeChunk columns chunk = do
        rawSize <- c_duckdb_data_chunk_get_size chunk
        let rowCount = fromIntegral rawSize :: Int
        if rowCount <= 0
            then pure Nothing
            else
                if null columns
                    then pure (Just (replicate rowCount []))
                    else do
                        vectors <- prepareChunkVectors chunk columns
                        rows <- mapM (buildMaterializedRow columns vectors) [0 .. rowCount - 1]
                        pure (Just rows)

collectResultColumns :: Ptr DuckDBResult -> IO [StatementStreamColumn]
collectResultColumns resPtr = do
    rawCount <- c_duckdb_column_count resPtr
    let cc = fromIntegral rawCount :: Int
    forM [0 .. cc - 1] \columnIndex -> do
        namePtr <- c_duckdb_column_name resPtr (fromIntegral columnIndex)
        name <-
            if namePtr == nullPtr
                then pure (Text.pack ("column" <> show columnIndex))
                else Text.pack <$> peekCString namePtr
        dtype <- c_duckdb_column_type resPtr (fromIntegral columnIndex)
        pure
            StatementStreamColumn
                { statementStreamColumnIndex = columnIndex
                , statementStreamColumnName = name
                , statementStreamColumnType = dtype
                }

buildMaterializedRow :: [StatementStreamColumn] -> [StatementStreamChunkVector] -> Int -> IO [Field]
buildMaterializedRow columns vectors rowIdx =
    zipWithM (buildMaterializedField rowIdx) columns vectors

buildMaterializedField :: Int -> StatementStreamColumn -> StatementStreamChunkVector -> IO Field
buildMaterializedField rowIdx column StatementStreamChunkVector{statementStreamChunkVectorHandle, statementStreamChunkVectorData, statementStreamChunkVectorValidity} = do
    value <-
        materializeValue
            (statementStreamColumnType column)
            statementStreamChunkVectorHandle
            statementStreamChunkVectorData
            statementStreamChunkVectorValidity
            rowIdx
    pure
        Field
            { fieldName = statementStreamColumnName column
            , fieldIndex = statementStreamColumnIndex column
            , fieldValue = value
            }

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
