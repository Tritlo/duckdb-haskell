{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

{- |
Module      : Database.DuckDB.Simple
Description : High-level DuckDB API in the duckdb-simple style.

The API mirrors the ergonomics of @sqlite-simple@ while being backed by the
DuckDB C API. It supports connection management, parameter binding, execution,
and typed result decoding. See @README.md@ for usage examples.
-}
module Database.DuckDB.Simple (
    -- * Connections
    Connection,
    open,
    openWithConfig,
    close,
    withConnection,
    withConnectionWithConfig,
    DuckDBDatabase,

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
    Function (..),
    createFunction,
    createFunctionWithState,
    deleteFunction,
    withDatabase,
    withDatabaseConnection,
) where

import Control.Exception (SomeException, bracket, finally, mask, mask_, onException, throwIO, try)
import Control.Monad (forM, forM_, join, void, when, zipWithM_)
import Data.IORef (atomicModifyIORef', mkWeakIORef, newIORef)
import Data.Maybe (isJust, isNothing)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Foreign as TextForeign
import Database.DuckDB.FFI
import Database.DuckDB.Simple.FromField (
    Field (..),
    FieldParser,
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
    rowErrorsToSqlError,
 )
import Database.DuckDB.Simple.Function (Function (..), createFunction, createFunctionWithState, deleteFunction)
import Database.DuckDB.Simple.Internal (
    Connection (..),
    ConnectionState (..),
    Query (..),
    ResultMode (..),
    SQLError (..),
    Statement (..),
    StatementState (..),
    StatementStreamState (..),
    keepAlive,
    peekUtf8CString,
    runInterruptibleQuery,
    withConnectionHandle,
    withQueryCString,
    withResult,
    withStatementHandle,
 )
import Database.DuckDB.Simple.Ok (Ok (..))
import Database.DuckDB.Simple.Result (cleanupStatementStreamRef, collectRows, resetStatementStream)
import qualified Database.DuckDB.Simple.Result as Result
import Database.DuckDB.Simple.ToField (DuckDBColumnType (..), FieldBinding, NamedParam (..), ToField (..), bindFieldBinding, duckdbColumnType, renderFieldBinding)
import Database.DuckDB.Simple.ToRow (ToRow (..))
import Database.DuckDB.Simple.Types (FormatError (..), Null (..), Only (..), (:.) (..))
import Foreign.C.String (CString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (peek, poke)
import GHC.Stack (HasCallStack, callStack)

-- | Open a DuckDB database located at the supplied path.
open :: FilePath -> IO Connection
open path = openWithConfig path []

-- | Open a DuckDB database with configuration flags applied before startup.
openWithConfig :: FilePath -> [(Text, Text)] -> IO Connection
openWithConfig path settings =
    mask_ do
        db <- openDatabaseWithConfig path settings
        conn <-
            connectDatabase db
                `onException` closeDatabaseHandle db
        createConnection db conn
            `onException` do
                closeConnectionHandle conn
                closeDatabaseHandle db

-- | Close a connection.  The operation is idempotent.
close :: Connection -> IO ()
close Connection{connectionState} =
    mask_ $
        join $
            atomicModifyIORef' connectionState \case
                ConnectionClosed -> (ConnectionClosed, pure ())
                openState@(ConnectionOpen{}) ->
                    (ConnectionClosed, closeHandles openState)

closeConnection :: Connection -> IO ()
closeConnection Connection{connectionState} =
    void $
        atomicModifyIORef' connectionState \case
            ConnectionClosed -> (ConnectionClosed, pure ())
            ConnectionOpen{connectionHandle} ->
                (ConnectionClosed, closeConnectionHandle connectionHandle)


-- | Run an action with a freshly opened connection, closing it afterwards.
withConnection :: FilePath -> (Connection -> IO a) -> IO a
withConnection path = bracket (open path) close

withDatabase :: FilePath -> [(Text, Text)] -> (DuckDBDatabase -> IO a) -> IO a
withDatabase path opts = bracket (openDatabaseWithConfig path opts) closeDatabaseHandle

withDatabaseConnection :: DuckDBDatabase -> (Connection -> IO a) -> IO a
withDatabaseConnection db = bracket (connectDatabase db >>= createConnection db) closeConnection

-- | Run an action with a freshly opened configured connection, closing it afterwards.
withConnectionWithConfig :: FilePath -> [(Text, Text)] -> (Connection -> IO a) -> IO a
withConnectionWithConfig path settings = bracket (openWithConfig path settings) close

-- | Prepare a SQL statement for execution.
openStatement :: Connection -> Query -> IO Statement
openStatement conn queryText =
    mask_ do
        handle <-
            withConnectionHandle conn \connPtr ->
                withQueryCString queryText \sql ->
                    alloca \stmtPtr -> do
                        poke stmtPtr nullPtr
                        flip onException (c_duckdb_destroy_prepare stmtPtr) do
                            rc <- runInterruptibleQuery conn (c_duckdb_prepare connPtr sql stmtPtr)
                            stmt <- peek stmtPtr
                            if rc == DuckDBSuccess
                                then pure stmt
                                else do
                                    errMsg <- fetchPrepareError stmt
                                    throwIO $ mkPrepareError queryText errMsg
        createStatement conn handle queryText
            `onException` destroyPrepared handle

-- | Finalise a prepared statement.  The operation is idempotent.
closeStatement :: Statement -> IO ()
closeStatement stmt@Statement{statementState} = mask_ do
    resetStatementStream stmt
    finish <- atomicModifyIORef' statementState \case
        StatementClosed -> (StatementClosed, pure ())
        StatementOpen{statementHandle} ->
            (StatementClosed, destroyPrepared statementHandle)
    finish

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
            let names = map (normalizeName . fst) bindings
            when (Set.size (Set.fromList names) /= length names) $
                throwFormatErrorNamed stmt (Text.pack "duckdb-simple: duplicate named parameter") bindings
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
    forM [1 .. count] \idx ->
        bracket (c_duckdb_parameter_name handle (fromIntegral idx)) (c_duckdb_free . castPtr) \namePtr ->
            if namePtr == nullPtr
                then pure Nothing
                else do
                    name <- peekUtf8CString namePtr
                    let normalized = normalizeName name
                    if normalized == Text.pack (show idx)
                        then pure Nothing
                        else pure (Just name)

-- | Remove all parameter bindings associated with a prepared statement.
clearStatementBindings :: Statement -> IO ()
clearStatementBindings stmt =
    withStatementHandle stmt \handle -> do
        resetStatementStream stmt
        rc <- c_duckdb_clear_bindings handle
        when (rc /= DuckDBSuccess) $ do
            err <- fetchPrepareError handle
            throwIO $ mkPrepareError (statementQuery stmt) err

-- | Look up the 1-based index of a named placeholder.
namedParameterIndex :: Statement -> Text -> IO (Maybe Int)
namedParameterIndex stmt name =
    withStatementHandle stmt \handle -> do
        when (Text.any (== '\0') name) $
            throwIO (mkPrepareError (statementQuery stmt) (Text.pack "duckdb-simple: parameter name contains NUL"))
        let normalized = normalizeName name
        TextForeign.withCString normalized \cName ->
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
            bracket (c_duckdb_prepared_statement_column_name handle (fromIntegral columnIndex)) (c_duckdb_free . castPtr) \namePtr ->
                if namePtr == nullPtr
                    then throwIO (columnNameUnavailableError stmt columnIndex)
                    else peekUtf8CString namePtr

{- | Execute a prepared statement and return the number of affected rows.
  Resets any active result stream before running and raises an @SQLError@
  if DuckDB reports a failure.
-}
executeStatement :: Statement -> IO Int
executeStatement stmt =
    withStatementHandle stmt \handle -> do
        resetStatementStream stmt
        withResult (statementConnection stmt) (statementQuery stmt) (c_duckdb_execute_prepared handle) resultRowsChanged

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
            withResult conn queryText (c_duckdb_query connPtr sql) resultRowsChanged

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
            withResult conn queryText (c_duckdb_execute_prepared handle) \resPtr ->
                collectRows queryText resPtr >>= convertRowsWith parser queryText

-- | Run a query that uses named parameters and decode all rows eagerly.
queryNamed :: (FromRow r) => Connection -> Query -> [NamedParam] -> IO [r]
queryNamed conn queryText params =
    withStatement conn queryText \stmt -> do
        bindNamed stmt params
        withStatementHandle stmt \handle ->
            withResult conn queryText (c_duckdb_execute_prepared handle) \resPtr ->
                collectRows queryText resPtr >>= convertRows queryText

-- | Run a query without supplying parameters and decode all rows eagerly.
query_ :: (FromRow r) => Connection -> Query -> IO [r]
query_ = queryWith_ fromRow

-- | Run a query without parameters using a custom row parser.
queryWith_ :: RowParser r -> Connection -> Query -> IO [r]
queryWith_ parser conn queryText =
    withConnectionHandle conn \connPtr ->
        withQueryCString queryText \sql ->
            withResult conn queryText (c_duckdb_query connPtr sql) \resPtr ->
                collectRows queryText resPtr >>= convertRowsWith parser queryText

-- Cursors and folds ---------------------------------------------------------

{- | Fold a parameterised result without constructing a complete Haskell row list.
  DuckDB materializes the native result before decoding starts. Native memory
  use depends on the result size; the step function controls Haskell memory use.
-}
fold :: (FromRow row, ToRow params) => Connection -> Query -> params -> a -> (a -> row -> IO a) -> IO a
fold conn queryText params initial step =
    withStatement conn queryText \stmt -> do
        resetStatementStream stmt
        bind stmt (toRow params)
        Result.foldStatementWith MaterializedResult fromRow stmt initial step

-- | Fold a parameterless result. Native materialization follows 'fold'.
fold_ :: (FromRow row) => Connection -> Query -> a -> (a -> row -> IO a) -> IO a
fold_ conn queryText initial step =
    withStatement conn queryText \stmt -> do
        resetStatementStream stmt
        Result.foldStatementWith MaterializedResult fromRow stmt initial step

-- | Fold a result with named parameters. Native materialization follows 'fold'.
foldNamed :: (FromRow row) => Connection -> Query -> [NamedParam] -> a -> (a -> row -> IO a) -> IO a
foldNamed conn queryText params initial step =
    withStatement conn queryText \stmt -> do
        resetStatementStream stmt
        bindNamed stmt params
        Result.foldStatementWith MaterializedResult fromRow stmt initial step

-- | Fetch the next row. The first call materializes the native result.
nextRow :: (FromRow r) => Statement -> IO (Maybe r)
nextRow = nextRowWith fromRow

-- | Fetch the next row using a custom parser, returning @Nothing@ once exhausted.
nextRowWith :: RowParser r -> Statement -> IO (Maybe r)
nextRowWith = Result.nextRowWith MaterializedResult

-- | Run an action inside a transaction.
withTransaction :: Connection -> IO a -> IO a
withTransaction conn action =
    mask \restore -> do
        void (execute_ conn begin)
        let rollbackAction = void (try (execute_ conn rollback) :: IO (Either SomeException Int))
        result <- restore action `onException` rollbackAction
        void (execute_ conn commit) `onException` rollbackAction
        pure result
  where
    begin = Query (Text.pack "BEGIN TRANSACTION")
    commit = Query (Text.pack "COMMIT")
    rollback = Query (Text.pack "ROLLBACK")

-- Internal helpers -----------------------------------------------------------

createConnection :: DuckDBDatabase -> DuckDBConnection -> IO Connection
createConnection db conn = do
    ref <- newIORef (ConnectionOpen db conn)
    _ <-
        mkWeakIORef ref $
            join $
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
            keepAlive parent do
                join $
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

openDatabaseWithConfig :: FilePath -> [(Text, Text)] -> IO DuckDBDatabase
openDatabaseWithConfig path settings = do
    when ('\0' `elem` path || any (\(name, value) -> Text.any (== '\0') name || Text.any (== '\0') value) settings) $
        throwIO (mkOpenError (Text.pack "duckdb-simple: database path or configuration contains NUL"))
    alloca \dbPtr ->
        alloca \configPtr ->
            alloca \errPtr -> do
                rcConfig <- c_duckdb_create_config configPtr
                when (rcConfig /= DuckDBSuccess) $
                    throwIO (mkOpenError (Text.pack "duckdb-simple: failed to allocate DuckDB config"))
                config <- peek configPtr
                poke errPtr nullPtr
                let destroyConfig =
                        when (config /= nullPtr) $
                            alloca \cfgPtr -> poke cfgPtr config >> c_duckdb_destroy_config cfgPtr
                flip finally destroyConfig $
                    do
                        forM_ settings \(name, value) ->
                            TextForeign.withCString name \cName ->
                                TextForeign.withCString value \cValue -> do
                                    rcSet <- c_duckdb_set_config config cName cValue
                                    when (rcSet /= DuckDBSuccess)
                                        $ throwIO
                                        $ mkOpenError
                                        $ Text.concat
                                            [ Text.pack "duckdb-simple: failed to set config option "
                                            , name
                                            ]
                        TextForeign.withCString (Text.pack path) \cPath -> do
                            rc <- c_duckdb_open_ext cPath dbPtr config errPtr
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
        else peekUtf8CString msgPtr

mkOpenError :: HasCallStack => Text -> SQLError
mkOpenError msg =
    SQLError
        { sqlErrorMessage = msg
        , sqlErrorType = Nothing
        , sqlErrorQuery = Nothing
        , sqlErrorCallStack = callStack
        }

mkConnectError :: HasCallStack => SQLError
mkConnectError =
    SQLError
        { sqlErrorMessage = Text.pack "duckdb-simple: failed to create connection handle"
        , sqlErrorType = Nothing
        , sqlErrorQuery = Nothing
        , sqlErrorCallStack = callStack
        }

mkPrepareError :: HasCallStack => Query -> Text -> SQLError
mkPrepareError queryText msg =
    SQLError
        { sqlErrorMessage = msg
        , sqlErrorType = Nothing
        , sqlErrorQuery = Just queryText
        , sqlErrorCallStack = callStack
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
            , sqlErrorCallStack = callStack
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
        , sqlErrorCallStack = callStack
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

peekError :: Ptr CString -> IO Text
peekError ptr = do
    errPtr <- peek ptr
    if errPtr == nullPtr
        then pure (Text.pack "duckdb-simple: failed to open database")
        else do
            peekUtf8CString errPtr

maybeFreeErr :: Ptr CString -> IO ()
maybeFreeErr ptr = do
    errPtr <- peek ptr
    when (errPtr /= nullPtr) $ c_duckdb_free (castPtr errPtr)
