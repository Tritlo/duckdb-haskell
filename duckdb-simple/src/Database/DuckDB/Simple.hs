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
import qualified Data.ByteString as BS
import Data.Bits ((.|.), shiftL)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Foreign as TextForeign
import qualified Data.Text.Encoding as TextEncoding
import Data.Time.Calendar (Day, fromGregorian)
import Data.Time.Clock (UTCTime (..))
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.LocalTime
    ( LocalTime (..)
    , TimeOfDay (..)
    , minutesToTimeZone
    , utcToLocalTime
    , utc
    )
import Data.Word (Word16, Word32, Word64, Word8)
import Data.Ratio ((%))
import Database.DuckDB.FFI
import Database.DuckDB.Simple.FromField
    ( BigNum (..)
    , BitString (..)
    , DecimalValue (..)
    , Field (..)
    , FieldValue (..)
    , FieldParser
    , FromField (..)
    , IntervalValue (..)
    , ResultError (..)
    , TimeWithZone (..)
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
import Database.DuckDB.Simple.ToField (FieldBinding, NamedParam (..), ToField (..), bindFieldBinding, renderFieldBinding)
import Database.DuckDB.Simple.ToRow (ToRow (..))
import Database.DuckDB.Simple.Types (FormatError (..), Null (..), Only (..), (:.) (..))
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CBool (..))
import Foreign.Marshal.Alloc (alloca, free, malloc)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Storable (Storable (..), peekElemOff, peekByteOff, pokeByteOff)
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
                    pure (FieldInt8 (fromIntegral raw))
                DuckDBTypeSmallInt -> do
                    raw <- peekElemOff (castPtr statementStreamChunkVectorData :: Ptr Int16) rowIdx
                    pure (FieldInt16 (fromIntegral raw))
                DuckDBTypeInteger -> do
                    raw <- peekElemOff (castPtr statementStreamChunkVectorData :: Ptr Int32) rowIdx
                    pure (FieldInt32 (fromIntegral raw))
                DuckDBTypeBigInt -> do
                    raw <- peekElemOff (castPtr statementStreamChunkVectorData :: Ptr Int64) rowIdx
                    pure (FieldInt64 raw)
                DuckDBTypeUTinyInt -> do
                    raw <- peekElemOff (castPtr statementStreamChunkVectorData :: Ptr Word8) rowIdx
                    pure (FieldWord8 (fromIntegral raw))
                DuckDBTypeUSmallInt -> do
                    raw <- peekElemOff (castPtr statementStreamChunkVectorData :: Ptr Word16) rowIdx
                    pure (FieldWord16 (fromIntegral raw))
                DuckDBTypeUInteger -> do
                    raw <- peekElemOff (castPtr statementStreamChunkVectorData :: Ptr Word32) rowIdx
                    pure (FieldWord32 (fromIntegral raw))
                DuckDBTypeUBigInt -> do
                    raw <- peekElemOff (castPtr statementStreamChunkVectorData :: Ptr Word64) rowIdx
                    pure (FieldWord64 (fromIntegral raw))
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
                DuckDBTypeList -> FieldList <$> decodeListElements statementStreamChunkVectorHandle statementStreamChunkVectorData rowIdx
                DuckDBTypeMap -> FieldMap <$> decodeMapPairs statementStreamChunkVectorHandle statementStreamChunkVectorData rowIdx
                DuckDBTypeStruct -> error "duckdb-simple: STRUCT columns are not supported"
                DuckDBTypeUnion -> error "duckdb-simple: UNION columns are not supported"
                DuckDBTypeHugeInt -> do
                    raw <- peekElemOff (castPtr statementStreamChunkVectorData :: Ptr DuckDBHugeInt) rowIdx
                    pure (FieldHugeInt (duckDBHugeIntToInteger raw))
                DuckDBTypeUHugeInt -> do
                    raw <- peekElemOff (castPtr statementStreamChunkVectorData :: Ptr DuckDBUHugeInt) rowIdx
                    pure (FieldUHugeInt (duckDBUHugeIntToInteger raw))
                _ ->
                    throwIO (streamingUnsupportedTypeError queryText column)
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

destroyLogicalType :: DuckDBLogicalType -> IO ()
destroyLogicalType logicalType =
    alloca \ptr -> do
        poke ptr logicalType
        c_duckdb_destroy_logical_type ptr

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
        materializedValueFromPointers
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

data DuckDBListEntry = DuckDBListEntry
    { duckDBListEntryOffset :: !Word64
    , duckDBListEntryLength :: !Word64
    }
    deriving (Eq, Show)

instance Storable DuckDBListEntry where
    sizeOf _ = listEntryWordSize * 2
    alignment _ = alignment (0 :: Word64)
    peek ptr = do
        offset <- peekByteOff ptr 0
        len <- peekByteOff ptr listEntryWordSize
        pure DuckDBListEntry{duckDBListEntryOffset = offset, duckDBListEntryLength = len}
    poke ptr DuckDBListEntry{duckDBListEntryOffset, duckDBListEntryLength} = do
        pokeByteOff ptr 0 duckDBListEntryOffset
        pokeByteOff ptr listEntryWordSize duckDBListEntryLength

listEntryWordSize :: Int
listEntryWordSize = sizeOf (0 :: Word64)

decodeListElements :: DuckDBVector -> Ptr () -> Int -> IO [FieldValue]
decodeListElements vector dataPtr rowIdx = do
    entry <- peekElemOff (castPtr dataPtr :: Ptr DuckDBListEntry) rowIdx
    (baseIdx, len) <- listEntryBounds (Text.pack "list") entry
    childVec <- c_duckdb_list_vector_get_child vector
    when (childVec == nullPtr) $
        throwIO (userError "duckdb-simple: list child vector is null")
    childType <- vectorElementType childVec
    childData <- c_duckdb_vector_get_data childVec
    childValidity <- c_duckdb_vector_get_validity childVec
    forM [0 .. len - 1] \delta ->
        materializedValueFromPointers childType childVec childData childValidity (baseIdx + delta)

decodeMapPairs :: DuckDBVector -> Ptr () -> Int -> IO [(FieldValue, FieldValue)]
decodeMapPairs vector dataPtr rowIdx = do
    entry <- peekElemOff (castPtr dataPtr :: Ptr DuckDBListEntry) rowIdx
    (baseIdx, len) <- listEntryBounds (Text.pack "map") entry
    structVec <- c_duckdb_list_vector_get_child vector
    when (structVec == nullPtr) $
        throwIO (userError "duckdb-simple: map struct vector is null")
    keyVec <- c_duckdb_struct_vector_get_child structVec 0
    valueVec <- c_duckdb_struct_vector_get_child structVec 1
    when (keyVec == nullPtr || valueVec == nullPtr) $
        throwIO (userError "duckdb-simple: map child vectors are null")
    keyType <- vectorElementType keyVec
    valueType <- vectorElementType valueVec
    keyData <- c_duckdb_vector_get_data keyVec
    valueData <- c_duckdb_vector_get_data valueVec
    keyValidity <- c_duckdb_vector_get_validity keyVec
    valueValidity <- c_duckdb_vector_get_validity valueVec
    forM [0 .. len - 1] \delta -> do
        let childIdx = baseIdx + delta
        keyValue <- materializedValueFromPointers keyType keyVec keyData keyValidity childIdx
        valueValue <- materializedValueFromPointers valueType valueVec valueData valueValidity childIdx
        pure (keyValue, valueValue)

vectorElementType :: DuckDBVector -> IO DuckDBType
vectorElementType vec = do
    logical <- c_duckdb_vector_get_column_type vec
    dtype <- c_duckdb_get_type_id logical
    destroyLogicalType logical
    pure dtype

listEntryBounds :: Text -> DuckDBListEntry -> IO (Int, Int)
listEntryBounds context DuckDBListEntry{duckDBListEntryOffset, duckDBListEntryLength} = do
    base <- ensureWithinIntRange (context <> Text.pack " offset") duckDBListEntryOffset
    len <- ensureWithinIntRange (context <> Text.pack " length") duckDBListEntryLength
    let maxInt = toInteger (maxBound :: Int)
        upperBound = toInteger base + toInteger len - 1
    when (len > 0 && upperBound > maxInt) $
        throwIO (userError ("duckdb-simple: " <> Text.unpack context <> " bounds exceed Int range"))
    pure (base, len)

ensureWithinIntRange :: Text -> Word64 -> IO Int
ensureWithinIntRange context value =
    let actual = toInteger value
        limit = toInteger (maxBound :: Int)
     in if actual <= limit
            then pure (fromInteger actual)
            else throwIO (userError ("duckdb-simple: " <> Text.unpack context <> " exceeds Int range"))

materializedValueFromPointers :: DuckDBType -> DuckDBVector -> Ptr () -> Ptr Word64 -> Int -> IO FieldValue
materializedValueFromPointers dtype vector dataPtr validity rowIdx = do
    let duckIdx = fromIntegral rowIdx :: DuckDBIdx
    valid <- chunkIsRowValid validity duckIdx
    if not valid
        then pure FieldNull
        else do
            case dtype of
                DuckDBTypeBoolean -> do
                    raw <- peekElemOff (castPtr dataPtr :: Ptr Word8) rowIdx
                    pure (FieldBool (raw /= 0))
                DuckDBTypeTinyInt -> do
                    raw <- peekElemOff (castPtr dataPtr :: Ptr Int8) rowIdx
                    pure (FieldInt8 raw)
                DuckDBTypeSmallInt -> do
                    raw <- peekElemOff (castPtr dataPtr :: Ptr Int16) rowIdx
                    pure (FieldInt16 raw)
                DuckDBTypeInteger -> do
                    raw <- peekElemOff (castPtr dataPtr :: Ptr Int32) rowIdx
                    pure (FieldInt32 raw)
                DuckDBTypeBigInt -> do
                    raw <- peekElemOff (castPtr dataPtr :: Ptr Int64) rowIdx
                    pure (FieldInt64 raw)
                DuckDBTypeUTinyInt -> do
                    raw <- peekElemOff (castPtr dataPtr :: Ptr Word8) rowIdx
                    pure (FieldWord8 raw)
                DuckDBTypeUSmallInt -> do
                    raw <- peekElemOff (castPtr dataPtr :: Ptr Word16) rowIdx
                    pure (FieldWord16 raw)
                DuckDBTypeUInteger -> do
                    raw <- peekElemOff (castPtr dataPtr :: Ptr Word32) rowIdx
                    pure (FieldWord32 raw)
                DuckDBTypeUBigInt -> do
                    raw <- peekElemOff (castPtr dataPtr :: Ptr Word64) rowIdx
                    pure (FieldWord64 raw)
                DuckDBTypeFloat -> do
                    raw <- peekElemOff (castPtr dataPtr :: Ptr Float) rowIdx
                    pure (FieldFloat raw)
                DuckDBTypeDouble -> do
                    raw <- peekElemOff (castPtr dataPtr :: Ptr Double) rowIdx
                    pure (FieldDouble raw)
                DuckDBTypeVarchar -> FieldText <$> chunkDecodeText dataPtr duckIdx
                DuckDBTypeUUID -> FieldText <$> chunkDecodeText dataPtr duckIdx
                DuckDBTypeBlob -> FieldBlob <$> chunkDecodeBlob dataPtr duckIdx
                DuckDBTypeDate -> do
                    raw <- peekElemOff (castPtr dataPtr :: Ptr Int32) rowIdx
                    FieldDate <$> decodeDuckDBDate (DuckDBDate raw)
                DuckDBTypeTime -> do
                    raw <- peekElemOff (castPtr dataPtr :: Ptr DuckDBTime) rowIdx
                    FieldTime <$> decodeDuckDBTime raw
                DuckDBTypeTimeNs -> do
                    raw <- peekElemOff (castPtr dataPtr :: Ptr DuckDBTimeNs) rowIdx
                    pure (FieldTime (decodeDuckDBTimeNs raw))
                DuckDBTypeTimeTz -> do
                    raw <- peekElemOff (castPtr dataPtr :: Ptr DuckDBTimeTz) rowIdx
                    FieldTimeTZ <$> decodeDuckDBTimeTz raw
                DuckDBTypeTimestamp -> do
                    raw <- peekElemOff (castPtr dataPtr :: Ptr DuckDBTimestamp) rowIdx
                    FieldTimestamp <$> decodeDuckDBTimestamp raw
                DuckDBTypeTimestampS -> do
                    raw <- peekElemOff (castPtr dataPtr :: Ptr DuckDBTimestampS) rowIdx
                    FieldTimestamp <$> decodeDuckDBTimestampSeconds raw
                DuckDBTypeTimestampMs -> do
                    raw <- peekElemOff (castPtr dataPtr :: Ptr DuckDBTimestampMs) rowIdx
                    FieldTimestamp <$> decodeDuckDBTimestampMilliseconds raw
                DuckDBTypeTimestampNs -> do
                    raw <- peekElemOff (castPtr dataPtr :: Ptr DuckDBTimestampNs) rowIdx
                    FieldTimestamp <$> decodeDuckDBTimestampNanoseconds raw
                DuckDBTypeTimestampTz -> do
                    raw <- peekElemOff (castPtr dataPtr :: Ptr DuckDBTimestamp) rowIdx
                    FieldTimestampTZ <$> decodeDuckDBTimestampUTCTime raw
                DuckDBTypeInterval -> do
                    raw <- peekElemOff (castPtr dataPtr :: Ptr DuckDBInterval) rowIdx
                    pure (FieldInterval (intervalValueFromDuckDB raw))
                DuckDBTypeDecimal -> do
                    bracket (c_duckdb_vector_get_column_type vector)
                            (\lty -> alloca $ \ptr -> poke ptr lty >> c_duckdb_destroy_logical_type ptr) $
                              \logical -> do
                                 width <- c_duckdb_decimal_width logical
                                 scale <- c_duckdb_decimal_scale logical
                                 internal_ty <- c_duckdb_decimal_internal_type logical
                                 raw <- case internal_ty of
                                     DuckDBTypeSmallInt ->
                                         toInteger <$> peekElemOff (castPtr dataPtr :: Ptr Int16) rowIdx
                                     DuckDBTypeInteger ->
                                         toInteger <$> peekElemOff (castPtr dataPtr :: Ptr Int32) rowIdx
                                     DuckDBTypeBigInt ->
                                         toInteger <$> peekElemOff (castPtr dataPtr :: Ptr Int64) rowIdx
                                     DuckDBTypeHugeInt ->
                                         toInteger . duckDBHugeIntToInteger <$>
                                            peekElemOff (castPtr dataPtr :: Ptr DuckDBHugeInt) rowIdx
                                     _ -> error "duckdb-simple: unsupported decimal internal storage type"
                                 pure (FieldDecimal (DecimalValue width scale raw))
                DuckDBTypeHugeInt -> do
                    raw <- peekElemOff (castPtr dataPtr :: Ptr DuckDBHugeInt) rowIdx
                    pure (FieldHugeInt (duckDBHugeIntToInteger raw))
                DuckDBTypeUHugeInt -> do
                    raw <- peekElemOff (castPtr dataPtr :: Ptr DuckDBUHugeInt) rowIdx
                    pure (FieldUHugeInt (duckDBUHugeIntToInteger raw))
                DuckDBTypeBit -> do
                    raw <- peekElemOff (castPtr dataPtr :: Ptr DuckDBBit) rowIdx
                    FieldBit <$> decodeDuckDBBit raw
                DuckDBTypeBigNum -> do
                    raw <- peekElemOff (castPtr dataPtr :: Ptr DuckDBBignum) rowIdx
                    FieldBigNum <$> decodeDuckDBBigNum raw
                DuckDBTypeList -> FieldList <$> decodeListElements vector dataPtr rowIdx
                DuckDBTypeMap -> FieldMap <$> decodeMapPairs vector dataPtr rowIdx
                DuckDBTypeStruct -> error "duckdb-simple: STRUCT columns are not supported"
                DuckDBTypeUnion -> error "duckdb-simple: UNION columns are not supported"
                DuckDBTypeEnum -> do
                    bracket (c_duckdb_vector_get_column_type vector)
                            (\lty -> alloca $ \ptr -> poke ptr lty >> c_duckdb_destroy_logical_type ptr) $
                            \logical -> do
                              enumInternal <- c_duckdb_enum_internal_type logical
                              FieldEnum <$>
                                case enumInternal of
                                    DuckDBTypeUTinyInt -> do
                                        fromIntegral <$> peekElemOff (castPtr dataPtr :: Ptr Word8) rowIdx
                                    DuckDBTypeUSmallInt -> do
                                        fromIntegral <$> peekElemOff (castPtr dataPtr :: Ptr Word16) rowIdx
                                    DuckDBTypeUInteger -> do
                                        fromIntegral <$> peekElemOff (castPtr dataPtr :: Ptr Word32) rowIdx
                                    _ -> error "duckdb-simple: unsupported enum internal storage type"
                DuckDBTypeSQLNull ->
                    pure FieldNull
                DuckDBTypeStringLiteral -> FieldText <$> chunkDecodeText dataPtr duckIdx
                DuckDBTypeIntegerLiteral -> do
                    raw <- peekElemOff (castPtr dataPtr :: Ptr Int64) rowIdx
                    pure (FieldInt64 raw)
                other ->
                    error ("duckdb-simple: unsupported DuckDB type in eager result: " <> show other)

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

decodeDuckDBTimeNs :: DuckDBTimeNs -> TimeOfDay
decodeDuckDBTimeNs (DuckDBTimeNs nanos) =
    let (hours, remainderHours) = nanos `divMod` (60 * 60 * 1000000000)
        (minutes, remainderMinutes) = remainderHours `divMod` (60 * 1000000000)
        (seconds, fractionalNanos) = remainderMinutes `divMod` 1000000000
        fractional = fromRational (toInteger fractionalNanos % 1000000000)
        totalSeconds = fromIntegral seconds + fractional
     in TimeOfDay
            (fromIntegral hours)
            (fromIntegral minutes)
            totalSeconds

decodeDuckDBTimeTz :: DuckDBTimeTz -> IO TimeWithZone
decodeDuckDBTimeTz raw =
    alloca \ptr -> do
        c_duckdb_from_time_tz raw ptr
        DuckDBTimeTzStruct{duckDBTimeTzStructTime = timeStruct, duckDBTimeTzStructOffset = offset} <- peek ptr
        let timeOfDay = timeStructToTimeOfDay timeStruct
            minutes = fromIntegral offset `div` 60
            zone = minutesToTimeZone minutes
        pure TimeWithZone{timeWithZoneTime = timeOfDay, timeWithZoneZone = zone}

decodeDuckDBTimestampSeconds :: DuckDBTimestampS -> IO LocalTime
decodeDuckDBTimestampSeconds (DuckDBTimestampS seconds) =
    decodeDuckDBTimestamp (DuckDBTimestamp (seconds * 1000000))

decodeDuckDBTimestampMilliseconds :: DuckDBTimestampMs -> IO LocalTime
decodeDuckDBTimestampMilliseconds (DuckDBTimestampMs millis) =
    decodeDuckDBTimestamp (DuckDBTimestamp (millis * 1000))

decodeDuckDBTimestampNanoseconds :: DuckDBTimestampNs -> IO LocalTime
decodeDuckDBTimestampNanoseconds (DuckDBTimestampNs nanos) = do
    let utcTime = posixSecondsToUTCTime (fromRational (toInteger nanos % 1000000000))
    pure (utcToLocalTime utc utcTime)

decodeDuckDBTimestampUTCTime :: DuckDBTimestamp -> IO UTCTime
decodeDuckDBTimestampUTCTime (DuckDBTimestamp micros) =
    pure (posixSecondsToUTCTime (fromRational (toInteger micros % 1000000)))

intervalValueFromDuckDB :: DuckDBInterval -> IntervalValue
intervalValueFromDuckDB DuckDBInterval{duckDBIntervalMonths, duckDBIntervalDays, duckDBIntervalMicros} =
    IntervalValue
        { intervalMonths = duckDBIntervalMonths
        , intervalDays = duckDBIntervalDays
        , intervalMicros = duckDBIntervalMicros
        }


duckDBHugeIntToInteger :: DuckDBHugeInt -> Integer
duckDBHugeIntToInteger DuckDBHugeInt{duckDBHugeIntLower, duckDBHugeIntUpper} =
    (fromIntegral duckDBHugeIntUpper `shiftL` 64) .|. fromIntegral duckDBHugeIntLower

duckDBUHugeIntToInteger :: DuckDBUHugeInt -> Integer
duckDBUHugeIntToInteger DuckDBUHugeInt{duckDBUHugeIntLower, duckDBUHugeIntUpper} =
    (fromIntegral duckDBUHugeIntUpper `shiftL` 64) .|. fromIntegral duckDBUHugeIntLower

decodeDuckDBBit :: DuckDBBit -> IO BitString
decodeDuckDBBit DuckDBBit{duckDBBitData, duckDBBitSize}
    | duckDBBitData == nullPtr || duckDBBitSize == 0 =
        pure BitString{bitStringLength = 0, bitStringBytes = BS.empty}
    | otherwise = do
        let bitLength = duckDBBitSize
            byteLength = fromIntegral ((bitLength + 7) `div` 8) :: Int
        bytes <- BS.packCStringLen (castPtr duckDBBitData, byteLength)
        pure BitString{bitStringLength = bitLength, bitStringBytes = bytes}

decodeDuckDBBigNum :: DuckDBBignum -> IO BigNum
decodeDuckDBBigNum DuckDBBignum{duckDBBignumData, duckDBBignumSize, duckDBBignumIsNegative}
    | duckDBBignumData == nullPtr || duckDBBignumSize == 0 =
        pure BigNum{bigNumIsNegative = duckDBBignumIsNegative /= 0, bigNumMagnitude = BS.empty}
    | otherwise = do
        let byteLength = fromIntegral duckDBBignumSize :: Int
        bytes <- BS.packCStringLen (castPtr duckDBBignumData, byteLength)
        pure BigNum{bigNumIsNegative = duckDBBignumIsNegative /= 0, bigNumMagnitude = bytes}

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
