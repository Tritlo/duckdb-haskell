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
    Environment,
    createEnvironment,
    closeEnvironment,
    withEnvironment,
    Instance,
    createInstance,
    closeInstance,
    withInstance,
    attachDatabase,
    detachDatabase,
    setDefaultDatabase,
    connect,
    connectWithOptions,
    withConnectionOn,
    Connection,
    open,
    openWithConfig,
    ConnectionOptions (..),
    defaultConnectionOptions,
    openWithOptions,
    close,
    withConnection,
    withConnectionWithConfig,
    withConnectionWithOptions,

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
) where

import Control.Exception (SomeException, bracket, finally, mask, mask_, onException, throwIO, try)
import Control.Monad (forM_, void, when, zipWithM_)
import Data.Char (toLower)
import Data.List (findIndex)
import Data.Maybe (isJust, isNothing)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Database.DuckDB.Simple.FromField (Field (..), FieldParser, FromField (..), ResultError (..))
import Database.DuckDB.Simple.FromRow (FromRow (..), RowParser, field, fieldWith, numFieldsRemaining, parseRow, rowErrorsToSqlError)
import Database.DuckDB.Simple.Function (Function (..), createFunction, createFunctionWithState, deleteFunction)
import Database.DuckDB.Simple.Internal
import Database.DuckDB.Simple.Ok (Ok (..))
import Database.DuckDB.Simple.Result (collectRows, resetStatementStream)
import qualified Database.DuckDB.Simple.Result as Result
import Database.DuckDB.Simple.ToField (DuckDBColumnType (..), FieldBinding, NamedParam (..), ToField (..), bindFieldBinding, duckdbColumnType, renderFieldBinding)
import Database.DuckDB.Simple.ToRow (ToRow (..))
import Database.DuckDB.Simple.TypeCache (defaultGeometryCRS)
import Database.DuckDB.Simple.Types (FormatError (..), Null (..), Only (..), (:.) (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)

-- | Run an action with a shared native environment.
withEnvironment :: (Environment -> IO a) -> IO a
withEnvironment = bracket createEnvironment closeEnvironment

-- | Run an action with a configured instance in a shared environment.
withInstance :: Environment -> [(Text, Text)] -> (Instance -> IO a) -> IO a
withInstance environment settings = bracket (createInstance environment settings) closeInstance

-- | Attach a catalog. The final argument selects it as the default catalog.
attachDatabase :: Instance -> FilePath -> Text -> [(Text, Text)] -> Bool -> IO ()
attachDatabase instance_ path alias settings makeDefault = withInstanceHandle instance_ \raw -> do
    when ('\0' `elem` path || Text.any (== '\0') alias) $
        throwIO (SQLError (Text.pack "duckdb-simple: database path or alias contains NUL") Nothing Nothing)
    bracket (output (c_duckdb_v2_attach_options_create raw)) (destroy c_duckdb_v2_attach_options_destroy) \options -> do
        forM_ settings \(key, value) -> withIdentifier key \name -> withText value \text ->
            checked (c_duckdb_v2_attach_options_set options name text)
        withText (Text.pack path) \file -> withIdentifier alias \name ->
            checked (c_duckdb_v2_instance_attach raw file name options (if makeDefault then 1 else 0))

-- | Detach a catalog from an instance.
detachDatabase :: Instance -> Text -> IO ()
detachDatabase instance_ alias = withInstanceHandle instance_ \raw -> withText alias \name ->
    checked (c_duckdb_v2_instance_detach raw name)

-- | Select the default catalog for new connections to an instance.
setDefaultDatabase :: Instance -> Text -> IO ()
setDefaultDatabase instance_ alias = withInstanceHandle instance_ \raw -> withText alias \name ->
    checked (c_duckdb_v2_instance_set_default raw name)

-- | Open a connection to a shared instance.
connect :: Instance -> IO Connection
connect instance_ = connectWithOptions instance_ defaultConnectionOptions

-- | Open a connection with options. Configuration uses DuckDB's automatic scope.
connectWithOptions :: Instance -> ConnectionOptions -> IO Connection
connectWithOptions instance_ ConnectionOptions{connectionConfig, connectionGeometryCRS} = mask_ do
    when (any (\(name, value) -> Text.any (== '\0') name || Text.any (== '\0') value) connectionConfig) $
        throwIO (SQLError (Text.pack "duckdb-simple: configuration contains NUL") Nothing Nothing)
    conn <- createConnection instance_ connectionGeometryCRS (pure ())
    flip onException (close conn) do
        withConnectionHandle conn \raw -> forM_ connectionConfig \(key, value) ->
            withIdentifier key \name -> withText value \text ->
                checked (c_duckdb_v2_connection_set_option raw name text DuckDBV2SettingScopeAutomatic)
        pure conn

-- | Run an action with a connection to a shared instance.
withConnectionOn :: Instance -> (Connection -> IO a) -> IO a
withConnectionOn instance_ = bracket (connect instance_) close

-- | Open a DuckDB database located at the supplied path.
open :: FilePath -> IO Connection
open path = openWithConfig path []

-- | Open a DuckDB database with configuration flags applied before startup.
openWithConfig :: FilePath -> [(Text, Text)] -> IO Connection
openWithConfig path settings = openWithOptions path defaultConnectionOptions{connectionConfig = settings}

-- | Settings that duckdb-simple applies when it opens a connection.
data ConnectionOptions = ConnectionOptions
    { connectionConfig :: [(Text, Text)]
    -- ^ DuckDB configuration flags, as for 'openWithConfig'.
    , connectionGeometryCRS :: [Text]
    {- ^ CRS definitions to validate at connection creation. V2 constructs
    GEOMETRY types directly. It does not require a separate type query.
    -}
    }
    deriving (Eq, Show)

-- | No configuration flags, and the CRS @OGC:CRS84@.
defaultConnectionOptions :: ConnectionOptions
defaultConnectionOptions =
    ConnectionOptions
        { connectionConfig = []
        , connectionGeometryCRS = defaultGeometryCRS
        }

-- | Open a database with a private environment and instance.
openWithOptions :: FilePath -> ConnectionOptions -> IO Connection
openWithOptions path ConnectionOptions{connectionConfig, connectionGeometryCRS} = mask_ do
    environment <- createEnvironment
    instance_ <- createInstance environment connectionConfig `onException` closeEnvironment environment
    let release = closeInstance instance_ `finally` closeEnvironment environment
    flip onException release do
        attachDatabase instance_ (if null path then ":memory:" else path) Text.empty [] True
        createConnection instance_ connectionGeometryCRS release

-- | Close a connection and its statements. The operation is idempotent.
close :: Connection -> IO ()
close = closeConnection

-- | Run an action with a freshly opened connection, closing it afterwards.
withConnection :: FilePath -> (Connection -> IO a) -> IO a
withConnection path = bracket (open path) close

-- | Run an action with a freshly opened configured connection, closing it afterwards.
withConnectionWithConfig :: FilePath -> [(Text, Text)] -> (Connection -> IO a) -> IO a
withConnectionWithConfig path settings = bracket (openWithConfig path settings) close

-- | Run an action with a connection opened with options, closing it afterwards.
withConnectionWithOptions :: FilePath -> ConnectionOptions -> (Connection -> IO a) -> IO a
withConnectionWithOptions path options = bracket (openWithOptions path options) close

-- | Prepare a SQL statement for execution.
openStatement :: Connection -> Query -> IO Statement
openStatement conn queryText = withParsedStatements conn queryText \parsed ->
    case parsed of
        [statement] -> createStatementFromParsed conn queryText statement
        [] -> throwIO (mkPrepareError queryText (Text.pack "duckdb-simple: SQL query contains no statement"))
        _ -> throwIO (mkPrepareError queryText (Text.pack "duckdb-simple: prepared SQL contains more than one statement"))

-- | Finalize a prepared statement. The operation is idempotent.
closeStatement :: Statement -> IO ()
closeStatement = closeStatementState . statementState

-- | Run an action with a prepared statement, closing it afterwards.
withStatement :: Connection -> Query -> (Statement -> IO a) -> IO a
withStatement conn sql = bracket (openStatement conn sql) closeStatement

-- | Bind positional parameters to a prepared statement, replacing any previous bindings.
bind :: Statement -> [FieldBinding] -> IO ()
bind stmt fields = do
    resetStatementStream stmt
    withStatementHandle stmt \_ -> do
        let actual = length fields
            expected = length (statementParameterNames stmt)
        when (actual /= expected) $
            throwFormatErrorBindings stmt (parameterCountMessage expected actual) fields
        let parameterNames = fetchParameterNames stmt
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

{- | Bind named parameters to a prepared statement.
Session variables can supply omitted named parameters.
-}
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
            withStatementHandle stmt \_ -> do
                let actual = length bindings
                    expected = length (statementParameterNames stmt)
                when (actual > expected) $
                    throwFormatErrorNamed stmt (parameterCountMessage expected actual) bindings
                let parameterNames = fetchParameterNames stmt
                when (all isNothing parameterNames && expected > 0) $
                    throwFormatErrorNamed stmt (Text.pack "duckdb-simple: statement does not define named parameters; use positional bindings or adjust the SQL") bindings
            clearStatementBindings stmt
            mapM_ apply bindings

fetchParameterNames :: Statement -> [Maybe Text]
fetchParameterNames stmt = zipWith (\idx name -> if normalizeName name == Text.pack (show idx) then Nothing else Just name) [1 :: Int ..] (statementParameterNames stmt)

-- | Remove all parameter bindings and release an active result.
clearStatementBindings :: Statement -> IO ()
clearStatementBindings stmt = withStatementHandle stmt \_ -> do
    resetStatementStream stmt
    clearBoundValues (statementBindings stmt)

-- | Look up the 1-based index of a named placeholder.
namedParameterIndex :: Statement -> Text -> IO (Maybe Int)
namedParameterIndex stmt name = withStatementHandle stmt \_ -> do
    when (Text.any (== '\0') name) $
        throwIO (mkPrepareError (statementQuery stmt) (Text.pack "duckdb-simple: parameter name contains NUL"))
    pure ((+ 1) <$> findIndex ((== normalizeName name) . normalizeName) (statementParameterNames stmt))

-- | Retrieve the number of columns produced by the supplied prepared statement.
columnCount :: Statement -> IO Int
columnCount stmt = do
    (schema, _) <- statementSchemas stmt
    fromIntegral <$> output (c_duckdb_v2_schema_get_count schema)

-- | Look up the zero-based column name of a prepared statement result.
columnName :: Statement -> Int -> IO Text
columnName stmt columnIndex
    | columnIndex < 0 = throwIO (columnIndexError stmt columnIndex Nothing)
    | otherwise = do
        total <- columnCount stmt
        when (columnIndex >= total) $ throwIO (columnIndexError stmt columnIndex (Just total))
        (schema, _) <- statementSchemas stmt
        alloca \name -> do
            _ <- output (c_duckdb_v2_schema_get_field schema (fromIntegral columnIndex) name)
            peek name >>= readIdentifier

{- | Execute a prepared statement and return the number of affected rows.
  Resets any active result stream before running and raises an @SQLError@
  if DuckDB reports a failure.
-}
executeStatement :: Statement -> IO Int
executeStatement stmt = do
    resetStatementStream stmt
    withQueryError (statementQuery stmt) $ withResult stmt \result ->
        fromIntegral <$> runInterruptibleQuery (statementConnection stmt) (output (c_duckdb_v2_result_drain result))

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
execute_ conn queryText = withAdHocStatement conn queryText (executeParsedStatement conn queryText) (pure 0)

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
        collectRows stmt >>= convertRowsWith parser queryText

-- | Run a query that uses named parameters and decode all rows eagerly.
queryNamed :: (FromRow r) => Connection -> Query -> [NamedParam] -> IO [r]
queryNamed conn queryText params =
    withStatement conn queryText \stmt -> do
        bindNamed stmt params
        collectRows stmt >>= convertRows queryText

-- | Run a query without supplying parameters and decode all rows eagerly.
query_ :: (FromRow r) => Connection -> Query -> IO [r]
query_ = queryWith_ fromRow

-- | Run a query without parameters using a custom row parser.
queryWith_ :: RowParser r -> Connection -> Query -> IO [r]
queryWith_ parser conn queryText =
    withAdHocStatement
        conn
        queryText
        (\parsed -> Result.collectParsedRows conn queryText parsed >>= convertRowsWith parser queryText)
        (pure [])

-- | Execute preceding statements before returning the final statement's result.
withAdHocStatement :: Connection -> Query -> (DuckDBV2SqlStatementHandle -> IO a) -> IO a -> IO a
withAdHocStatement conn queryText action empty = withParsedStatements conn queryText \statements ->
    let run [] = empty
        run [parsed] = action parsed
        run (parsed : rest) = do
            void (executeParsedStatement conn queryText parsed)
            run rest
     in run statements

-- | Execute a parsed statement directly and drain all its side effects.
executeParsedStatement :: Connection -> Query -> DuckDBV2SqlStatementHandle -> IO Int
executeParsedStatement conn queryText parsed = withQueryError queryText $
    bracket (executeParsedResult conn queryText parsed) (destroy c_duckdb_v2_result_destroy) \result ->
        fromIntegral <$> runInterruptibleQuery conn (output (c_duckdb_v2_result_drain result))

-- Cursors and folds ---------------------------------------------------------

{- | Fold a parameterised result without constructing a complete Haskell row list.
  The fold collects all owned native chunks before decoding starts. Native
  memory use depends on the result size. The step function controls Haskell memory use.
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

-- | Fetch the next row. The first call collects all owned native chunks.
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

mkPrepareError :: Query -> Text -> SQLError
mkPrepareError queryText msg =
    SQLError
        { sqlErrorMessage = msg
        , sqlErrorType = Nothing
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

-- | Use DuckDB's ASCII case rules for parameter names.
normalizeName :: Text -> Text
normalizeName name =
    Text.map asciiLower $
        case Text.uncons name of
            Just (prefix, rest)
                | prefix == ':' || prefix == '$' || prefix == '@' -> rest
            _ -> name
  where
    asciiLower char
        | char >= 'A' && char <= 'Z' = toLower char
        | otherwise = char

convertRows :: (FromRow r) => Query -> [[Field]] -> IO [r]
convertRows = convertRowsWith fromRow

convertRowsWith :: RowParser r -> Query -> [[Field]] -> IO [r]
convertRowsWith parser queryText rows =
    case traverse (parseRow parser) rows of
        Errors err -> throwIO (rowErrorsToSqlError queryText err)
        Ok ok -> pure ok
