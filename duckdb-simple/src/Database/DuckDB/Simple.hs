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

import Control.Monad (forM, void, when, zipWithM, zipWithM_)
import Data.IORef (IORef, atomicModifyIORef', mkWeakIORef, newIORef, readIORef, writeIORef)
import Control.Exception (SomeException, bracket, finally, mask, onException, throwIO, try)
import qualified Data.ByteString as BS
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Foreign as TextForeign
import qualified Data.Text.Encoding as TextEncoding
import Data.Time.Calendar (Day, fromGregorian)
import Data.Time.LocalTime (LocalTime (..), TimeOfDay (..))
import Data.Word (Word16, Word32, Word64, Word8)
import Data.Ratio ((%))
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
    StatementStream (..),
    StatementStreamChunk (..),
    StatementStreamChunkVector (..),
    StatementStreamColumn (..),
    StatementStreamState (..),
    withConnectionHandle,
    withQueryCString,
    withStatementHandle,
 )
import Database.DuckDB.Simple.ToField (FieldBinding, NamedParam (..), ToField (..), bindFieldBinding, renderFieldBinding)
import Database.DuckDB.Simple.ToRow (ToRow (..))
import Database.DuckDB.Simple.Types (FormatError (..), Null (..), Only (..), (:.) (..))
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CBool (..), CDouble (..), CChar)
import Foreign.Marshal.Alloc (alloca, free, malloc)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Storable (peek, peekElemOff, poke)

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
    withStatementHandle stmt \handle -> do
        resetStatementStream stmt
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

fold :: (FromRow row, ToRow params) => Connection -> Query -> params -> a -> (a -> row -> IO a) -> IO a
fold conn queryText params initial step =
    withStatement conn queryText \stmt -> do
        resetStatementStream stmt
        bind stmt (toRow params)
        foldStatementWith fromRow stmt initial step

fold_ :: (FromRow row) => Connection -> Query -> a -> (a -> row -> IO a) -> IO a
fold_ conn queryText initial step =
    withStatement conn queryText \stmt -> do
        resetStatementStream stmt
        foldStatementWith fromRow stmt initial step

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

nextRow :: (FromRow r) => Statement -> IO (Maybe r)
nextRow = nextRowWith fromRow

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
                        Left rowErr -> do
                            finalizeStream updatedStream
                            writeIORef streamRef StatementStreamIdle
                            throwIO (resultErrorToSqlError (statementQuery stmt) rowErr)
                        Right value -> do
                            writeIORef streamRef (StatementStreamActive updatedStream)
                            pure (Just value)

startStatementStream :: Statement -> IO (Maybe StatementStream)
startStatementStream stmt =
    withStatementHandle stmt \handle -> do
        columns <- collectStreamColumns handle
        resultPtr <- malloc
        rc <- c_duckdb_execute_prepared_streaming handle resultPtr
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
    let columnCount = fromIntegral rawCount :: Int
    forM [0 .. columnCount - 1] \idx -> do
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
streamNextRow query stream@StatementStream{statementStreamChunk = Nothing} = do
    refreshed <- fetchChunk stream
    case statementStreamChunk refreshed of
        Nothing -> pure (Nothing, refreshed)
        Just chunk -> emitRow query refreshed chunk
streamNextRow query stream@StatementStream{statementStreamChunk = Just chunk} =
    emitRow query stream chunk

fetchChunk :: StatementStream -> IO StatementStream
fetchChunk stream@StatementStream{statementStreamResult} = do
    chunk <- c_duckdb_stream_fetch_chunk statementStreamResult
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
emitRow query stream chunk@StatementStreamChunk{statementStreamChunkIndex, statementStreamChunkSize} = do
    fields <-
        buildRow
            query
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
buildRow query columns vectors rowIdx =
    zipWithM (buildField query rowIdx) columns vectors

buildField :: Query -> Int -> StatementStreamColumn -> StatementStreamChunkVector -> IO Field
buildField query rowIdx column StatementStreamChunkVector{statementStreamChunkVectorData, statementStreamChunkVectorValidity} = do
    let duckIdx = fromIntegral rowIdx
        dtype = statementStreamColumnType column
    valid <- chunkIsRowValid statementStreamChunkVectorValidity duckIdx
    value <-
        if not valid
            then pure FieldNull
            else case dtype of
                DuckDBTypeBoolean -> do
                    raw <- peekElemOff (castPtr statementStreamChunkVectorData :: Ptr Word8) rowIdx
                    pure (FieldBool (raw /= 0))
                DuckDBTypeTinyInt -> do
                    raw <- peekElemOff (castPtr statementStreamChunkVectorData :: Ptr Int8) rowIdx
                    pure (FieldInt (fromIntegral raw))
                DuckDBTypeSmallInt -> do
                    raw <- peekElemOff (castPtr statementStreamChunkVectorData :: Ptr Int16) rowIdx
                    pure (FieldInt (fromIntegral raw))
                DuckDBTypeInteger -> do
                    raw <- peekElemOff (castPtr statementStreamChunkVectorData :: Ptr Int32) rowIdx
                    pure (FieldInt (fromIntegral raw))
                DuckDBTypeBigInt -> do
                    raw <- peekElemOff (castPtr statementStreamChunkVectorData :: Ptr Int64) rowIdx
                    pure (FieldInt raw)
                DuckDBTypeHugeInt -> do
                    raw <- peekElemOff (castPtr statementStreamChunkVectorData :: Ptr Int64) rowIdx
                    pure (FieldInt raw)
                DuckDBTypeUTinyInt -> do
                    raw <- peekElemOff (castPtr statementStreamChunkVectorData :: Ptr Word8) rowIdx
                    pure (FieldInt (fromIntegral raw))
                DuckDBTypeUSmallInt -> do
                    raw <- peekElemOff (castPtr statementStreamChunkVectorData :: Ptr Word16) rowIdx
                    pure (FieldInt (fromIntegral raw))
                DuckDBTypeUInteger -> do
                    raw <- peekElemOff (castPtr statementStreamChunkVectorData :: Ptr Word32) rowIdx
                    pure (FieldInt (fromIntegral raw))
                DuckDBTypeUBigInt -> do
                    raw <- peekElemOff (castPtr statementStreamChunkVectorData :: Ptr Word64) rowIdx
                    pure (FieldInt (fromIntegral raw))
                DuckDBTypeUHugeInt -> do
                    raw <- peekElemOff (castPtr statementStreamChunkVectorData :: Ptr Word64) rowIdx
                    pure (FieldInt (fromIntegral raw))
                DuckDBTypeBlob -> do
                    blob <- chunkDecodeBlob statementStreamChunkVectorData duckIdx
                    pure (FieldBlob blob)
                DuckDBTypeDate -> do
                    raw <- peekElemOff (castPtr statementStreamChunkVectorData :: Ptr Int32) rowIdx
                    day <- decodeDuckDBDate (DuckDBDate raw)
                    pure (FieldDate day)
                DuckDBTypeTime -> do
                    raw <- peekElemOff (castPtr statementStreamChunkVectorData :: Ptr DuckDBTime) rowIdx
                    tod <- decodeDuckDBTime raw
                    pure (FieldTime tod)
                DuckDBTypeTimestamp -> do
                    raw <- peekElemOff (castPtr statementStreamChunkVectorData :: Ptr DuckDBTimestamp) rowIdx
                    ts <- decodeDuckDBTimestamp raw
                    pure (FieldTimestamp ts)
                DuckDBTypeFloat -> do
                    raw <- peekElemOff (castPtr statementStreamChunkVectorData :: Ptr Float) rowIdx
                    pure (FieldDouble (realToFrac raw))
                DuckDBTypeDouble -> do
                    raw <- peekElemOff (castPtr statementStreamChunkVectorData :: Ptr Double) rowIdx
                    pure (FieldDouble raw)
                DuckDBTypeVarchar -> FieldText <$> chunkDecodeText statementStreamChunkVectorData duckIdx
                _ ->
                    throwIO (streamingUnsupportedTypeError query column)
    pure
        Field
            { fieldName = statementStreamColumnName column
            , fieldIndex = statementStreamColumnIndex column
            , fieldValue = value
            }

chunkIsRowValid :: Ptr Word64 -> DuckDBIdx -> IO Bool
chunkIsRowValid validity rowIdx
    | validity == nullPtr = pure True
    | otherwise = do
        CBool flag <- c_duckdb_validity_row_is_valid validity rowIdx
        pure (flag /= 0)

chunkDecodeText :: Ptr () -> DuckDBIdx -> IO Text
chunkDecodeText dataPtr rowIdx = do
    let base = castPtr dataPtr :: Ptr Word8
        offset = fromIntegral rowIdx * duckdbStringTSize
        stringPtr = castPtr (base `plusPtr` offset) :: Ptr DuckDBStringT
    len <- c_duckdb_string_t_length stringPtr
    if len == 0
        then pure Text.empty
        else do
            cstr <- c_duckdb_string_t_data stringPtr
            bytes <- BS.packCStringLen (cstr, fromIntegral len)
            pure (TextEncoding.decodeUtf8 bytes)

chunkDecodeBlob :: Ptr () -> DuckDBIdx -> IO BS.ByteString
chunkDecodeBlob dataPtr rowIdx = do
    let base = castPtr dataPtr :: Ptr Word8
        offset = fromIntegral rowIdx * duckdbStringTSize
        stringPtr = castPtr (base `plusPtr` offset) :: Ptr DuckDBStringT
    len <- c_duckdb_string_t_length stringPtr
    if len == 0
        then pure BS.empty
        else do
            ptr <- c_duckdb_string_t_data stringPtr
            BS.packCStringLen (ptr, fromIntegral len)

duckdbStringTSize :: Int
duckdbStringTSize = 16

cleanupStatementStreamRef :: IORef StatementStreamState -> IO ()
cleanupStatementStreamRef ref = do
    state <- atomicModifyIORef' ref \current -> (StatementStreamIdle, current)
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
streamingUnsupportedTypeError query StatementStreamColumn{statementStreamColumnName, statementStreamColumnType} =
    SQLError
        { sqlErrorMessage =
            Text.concat
                [ Text.pack "duckdb-simple: streaming does not yet support column "
                , statementStreamColumnName
                , Text.pack " with DuckDB type "
                , Text.pack (show statementStreamColumnType)
                ]
        , sqlErrorType = Nothing
        , sqlErrorQuery = Just query
        }

-- | Run an action inside a transaction.
withTransaction :: Connection -> IO a -> IO a
withTransaction conn action =
    mask \restore -> do
        void (execute_ conn begin)
        let rollbackAction = executeIgnore conn rollback
        result <- restore action `onException` rollbackAction
        void (execute_ conn commit)
        pure result

  where begin = Query (Text.pack "BEGIN TRANSACTION")
        commit = Query (Text.pack "COMMIT")
        rollback = Query (Text.pack "ROLLBACK")


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
    streamRef <- newIORef StatementStreamIdle
    _ <-
        mkWeakIORef ref $
            do
                finalizer <-
                    atomicModifyIORef' ref \case
                        StatementClosed -> (StatementClosed, pure ())
                        StatementOpen{statementHandle} ->
                            ( StatementClosed
                            , do
                                cleanupStatementStreamRef streamRef
                                destroyPrepared statementHandle
                            )
                finalizer
    pure
        Statement
            { statementState = ref
            , statementConnection = parent
            , statementQuery = query
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
            DuckDBTypeBlob -> FieldBlob <$> fetchBlobValue resPtr duckCol duckRow
            DuckDBTypeDate -> FieldDate <$> fetchDateValue resPtr duckCol duckRow
            DuckDBTypeTime -> FieldTime <$> fetchTimeValue resPtr duckCol duckRow
            DuckDBTypeTimestamp -> FieldTimestamp <$> fetchTimestampValue resPtr duckCol duckRow
            DuckDBTypeUUID -> FieldText <$> fetchTextValue resPtr duckCol duckRow
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

fetchBlobValue :: Ptr DuckDBResult -> DuckDBIdx -> DuckDBIdx -> IO BS.ByteString
fetchBlobValue resPtr columnIndex rowIndex = do
    alloca \ptr -> do
        c_duckdb_value_blob resPtr columnIndex rowIndex ptr
        DuckDBBlob{duckDBBlobData = dat, duckDBBlobSize = len} <- peek ptr
        if dat == nullPtr
            then pure BS.empty
            else do
                bs <- BS.packCStringLen (castPtr dat :: Ptr CChar, fromIntegral len)
                c_duckdb_free dat
                pure bs

fetchDateValue :: Ptr DuckDBResult -> DuckDBIdx -> DuckDBIdx -> IO Day
fetchDateValue resPtr columnIndex rowIndex = do
    raw <- c_duckdb_value_date resPtr columnIndex rowIndex
    decodeDuckDBDate raw

fetchTimeValue :: Ptr DuckDBResult -> DuckDBIdx -> DuckDBIdx -> IO TimeOfDay
fetchTimeValue resPtr columnIndex rowIndex = do
    raw <- c_duckdb_value_time resPtr columnIndex rowIndex
    decodeDuckDBTime raw

fetchTimestampValue :: Ptr DuckDBResult -> DuckDBIdx -> DuckDBIdx -> IO LocalTime
fetchTimestampValue resPtr columnIndex rowIndex = do
    raw <- c_duckdb_value_timestamp resPtr columnIndex rowIndex
    decodeDuckDBTimestamp raw

decodeDuckDBDate :: DuckDBDate -> IO Day
decodeDuckDBDate raw =
    alloca \ptr -> do
        c_duckdb_from_date raw ptr
        dateStruct <- peek ptr
        pure (dateStructToDay dateStruct)

decodeDuckDBTime :: DuckDBTime -> IO TimeOfDay
decodeDuckDBTime raw =
    alloca \ptr -> do
        c_duckdb_from_time raw ptr
        timeStruct <- peek ptr
        pure (timeStructToTimeOfDay timeStruct)

decodeDuckDBTimestamp :: DuckDBTimestamp -> IO LocalTime
decodeDuckDBTimestamp raw =
    alloca \ptr -> do
        c_duckdb_from_timestamp raw ptr
        DuckDBTimestampStruct{duckDBTimestampStructDate = dateStruct, duckDBTimestampStructTime = timeStruct} <- peek ptr
        pure
            LocalTime
                { localDay = dateStructToDay dateStruct
                , localTimeOfDay = timeStructToTimeOfDay timeStruct
                }

dateStructToDay :: DuckDBDateStruct -> Day
dateStructToDay DuckDBDateStruct{duckDBDateStructYear, duckDBDateStructMonth, duckDBDateStructDay} =
    fromGregorian (fromIntegral duckDBDateStructYear) (fromIntegral duckDBDateStructMonth) (fromIntegral duckDBDateStructDay)

timeStructToTimeOfDay :: DuckDBTimeStruct -> TimeOfDay
timeStructToTimeOfDay DuckDBTimeStruct{duckDBTimeStructHour, duckDBTimeStructMinute, duckDBTimeStructSecond, duckDBTimeStructMicros} =
    let secondsInt = fromIntegral duckDBTimeStructSecond :: Integer
        micros = fromIntegral duckDBTimeStructMicros :: Integer
        fractional = fromRational (micros % 1000000)
        totalSeconds = fromInteger secondsInt + fractional
     in TimeOfDay
            (fromIntegral duckDBTimeStructHour)
            (fromIntegral duckDBTimeStructMinute)
            totalSeconds

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
