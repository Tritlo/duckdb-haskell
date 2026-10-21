{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | Native V2 ownership and execution used by the public API.
module Database.DuckDB.Simple.Internal (
    module Database.DuckDB.Simple.Native,
    Environment (..),
    EnvironmentState (..),
    Instance (..),
    InstanceState (..),
    Connection (..),
    ConnectionState (..),
    Statement (..),
    StatementState (..),
    StatementStreamState (..),
    StatementStream (..),
    ResultMode (..),
    StatementStreamColumn (..),
    StatementStreamChunk (..),
    createEnvironment,
    closeEnvironment,
    createInstance,
    closeInstance,
    createConnection,
    closeConnection,
    closeStatementState,
    createStatementFromParsed,
    createStatementFromParsedWithCollections,
    withParsedStatements,
    statementSchemas,
    clearStatementSchemas,
    preparedParameterType,
    storeBoundValue,
    clearBoundValues,
    cleanupStatementStreamRef,
    connectionClosedError,
    statementClosedError,
    keepAlive,
    withInstanceHandle,
    withDatabaseHandle,
    withConnectionHandle,
    withTypeCache,
    withStatementHandle,
    withQueryCString,
    withQueryError,
    peekUtf8CString,
    duckDBTypeFromName,
    duckDBTypeToName,
    withResult,
    runInterruptibleQuery,
    executePreparedResult,
    executeParsedResult,
    executeParsedStatement,
    executePreparedArrowResult,
    withArrowResult,
    clearArrowResult,
    fetchResultChunk,
    destroyDataChunk,
) where

import Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar, readMVar, threadDelay, tryReadMVar)
import Control.Exception (SomeException, bracket, catch, finally, mask, mask_, onException, throwIO, try, uninterruptibleMask_)
import Control.Monad (forM, forM_, join, unless, void, when)
import qualified Data.ByteString as BS
import Data.Coerce (Coercible, coerce)
import Data.IORef (IORef, atomicModifyIORef', mkWeakIORef, newIORef, readIORef, writeIORef)
import qualified Data.IntMap.Strict as IntMap
import Data.List (find, partition)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Foreign as TextForeign
import Database.DuckDB.Simple.FromField (FieldValue)
import Database.DuckDB.Simple.LogicalRep (LogicalTypeRep, logicalTypeToRep)
import Database.DuckDB.Simple.Native
import Database.DuckDB.Simple.TypeCache (TypeCache, createTypeCache, destroyTypeCache)
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.C.String (CString)
import Foreign.C.Types (CChar)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (peek, poke)
import GHC.Exts (keepAlive#)
import GHC.IO (IO (..))
import System.Mem.Weak (Weak, deRefWeak, finalize)

-- | Track child finalizers without retaining live children.
newtype Children a = Children (IORef [(Weak (IORef a), MVar ())])

-- | Create an empty child registry.
newChildren :: IO (Children a)
newChildren = Children <$> newIORef []

-- | Register a child and remove completed entries.
registerChild :: (a -> Bool) -> Children a -> IORef a -> IO () -> IO ()
registerChild isOpen (Children children) child cleanup = mask_ do
    previous <- readIORef children
    finished <- fmap concat $ forM previous \entry@(weak, done) ->
        deRefWeak weak >>= \case
            Nothing -> finishChild entry >> pure [done]
            Just ref -> do
                state <- readIORef ref
                if isOpen state then pure [] else finishChild entry >> pure [done]
    -- Finish stale cleanup before transferring ownership of the new child.
    done <- newEmptyMVar
    weak <- mkWeakIORef child (cleanup `finally` putMVar done ())
    -- Remove only finished snapshot entries. Keep concurrent registrations.
    atomicModifyIORef' children $ \current ->
        ((weak, done) : filter (\(_, completed) -> completed `notElem` finished) current, ())

-- | Run a child finalizer and wait for its completion.
finishChild :: (Weak (IORef a), MVar ()) -> IO ()
finishChild (weak, done) = uninterruptibleMask_ (finalize weak `finally` readMVar done)

-- | Complete child finalizers before their parent closes.
closeChildren :: Children a -> IO ()
closeChildren (Children children) = atomicModifyIORef' children ([],) >>= cleanupAll . map finishChild

-- | Run every cleanup action in order, even when an earlier action throws.
cleanupAll :: [IO ()] -> IO ()
cleanupAll = foldr finally (pure ())

-- | A native environment shared by one or more instances.
newtype Environment = Environment {environmentState :: IORef EnvironmentState}

-- | Internal environment state.
data EnvironmentState = EnvironmentClosed | EnvironmentOpen DuckDBV2EnvironmentHandle (Children InstanceState)

-- | An instance with catalogs and configuration shared by its connections.
data Instance = Instance {instanceState :: IORef InstanceState, instanceEnvironment :: Environment}

-- | Internal instance state.
data InstanceState = InstanceClosed | InstanceOpen DuckDBV2InstanceHandle (Children ConnectionState)

-- | A connection that retains its instance until it closes.
data Connection = Connection {connectionState :: IORef ConnectionState, connectionInstance :: Instance}

-- | Internal connection state.
data ConnectionState
    = ConnectionClosed
    | ConnectionOpen
        { connectionHandle :: DuckDBV2ConnectionHandle
        , connectionTypeCache :: TypeCache
        , connectionStatements :: Children StatementState
        , connectionReleaseParent :: IO ()
        }

-- | A prepared statement and its owned bindings and cursor.
data Statement = Statement
    { statementState :: IORef StatementState
    , statementConnection :: Connection
    , statementQuery :: Query
    , statementStream :: IORef StatementStreamState
    , statementArrowResult :: IORef DuckDBV2ArrowResultHandle
    , statementBindings :: IORef (IntMap.IntMap DuckDBV2ValueHandle)
    , statementParameterNames :: [Text]
    }

-- | Internal prepared statement state.
data StatementState
    = StatementClosed
    | StatementOpen
        { statementHandle :: DuckDBV2PreparedStatementHandle
        , statementParsed :: DuckDBV2SqlStatementHandle
        , statementSchemaCache :: IORef (Maybe (DuckDBV2SchemaHandle, DuckDBV2SchemaHandle))
        }

-- | Cursor state. Exhaustion persists until the caller resets the statement.
data StatementStreamState = StatementStreamIdle | StatementStreamExhausted | StatementStreamActive StatementStream

-- | An execution cursor or an owned collection of materialized chunks.
data StatementStream = StatementStream
    { statementStreamResult :: DuckDBV2ResultHandle
    , statementStreamColumns :: [StatementStreamColumn]
    , statementStreamPendingChunks :: [DuckDBV2DataChunkHandle]
    , statementStreamChunk :: Maybe StatementStreamChunk
    }

-- | Select materialization before decoding or incremental native execution.
data ResultMode = MaterializedResult | StreamingResult deriving (Eq, Show)

-- | Metadata for one result column.
data StatementStreamColumn = StatementStreamColumn
    { statementStreamColumnIndex :: Int
    , statementStreamColumnName :: Text
    }

-- | One owned chunk and the readers that borrow its vectors.
data StatementStreamChunk = StatementStreamChunk
    { statementStreamChunkPtr :: DuckDBV2DataChunkHandle
    , statementStreamChunkSize :: Int
    , statementStreamChunkIndex :: Int
    , statementStreamChunkReaders :: [Int -> IO FieldValue]
    }

-- | Create an environment. Close it after all related work finishes.
createEnvironment :: IO Environment
createEnvironment = mask_ do
    raw <- output c_duckdb_v2_environment_create
    children <- newChildren
    ref <- newIORef (EnvironmentOpen raw children)
    _ <- mkWeakIORef ref (closeEnvironmentState ref)
    pure (Environment ref)

{- | Close an environment and its instances and connections. This is idempotent.
Wait for child creation and all descendant operations to finish before closing.
-}
closeEnvironment :: Environment -> IO ()
closeEnvironment Environment{environmentState} = closeEnvironmentState environmentState

-- | Mark the environment closed before releasing its native state.
closeEnvironmentState :: IORef EnvironmentState -> IO ()
closeEnvironmentState ref = mask_ $ joinCleanup ref \case
    EnvironmentClosed -> (EnvironmentClosed, pure ())
    EnvironmentOpen raw children -> (EnvironmentClosed, cleanupAll [closeChildren children, destroy c_duckdb_v2_environment_destroy raw])

-- | Create an instance with configuration applied before its first connection.
createInstance :: Environment -> [(Text, Text)] -> IO Instance
createInstance environment@Environment{environmentState} settings = keepAlive environment $ mask_ do
    (rawEnvironment, children) <-
        readIORef environmentState >>= \case
            EnvironmentClosed -> throwIO (SQLError "duckdb-simple: environment is closed" Nothing Nothing)
            EnvironmentOpen raw children -> pure (raw, children)
    when (any (\(name, value) -> Text.any (== '\0') name || Text.any (== '\0') value) settings) $
        throwIO (SQLError "duckdb-simple: configuration contains NUL" Nothing Nothing)
    let hasWhitelist = any ((`elem` ["allowed_directories", "allowed_paths"]) . Text.toLower . fst) settings
        needsInstance (name, _) = Text.toLower name `elem` runtimeOptions || (hasWhitelist && Text.toLower name == "enable_external_access")
        (deferredSettings, startupSettings) = partition needsInstance settings
        (runtimeSettings, externalAccessSettings) = partition ((/= "enable_external_access") . Text.toLower . fst) deferredSettings
    raw <- withTextArray (map fst startupSettings) \names -> withTextArray (map snd startupSettings) \values ->
        output (c_duckdb_v2_instance_create_with_options rawEnvironment (ConstPtr names) (ConstPtr values) (fromIntegral (length startupSettings)))
    flip onException (destroy c_duckdb_v2_instance_destroy raw) do
        config <- output (c_duckdb_v2_instance_get_config raw)
        -- Set the whitelist before external access can be disabled.
        forM_ (runtimeSettings ++ externalAccessSettings) \(name, value) -> withIdentifier name \key -> withText value \text ->
            checked (c_duckdb_v2_config_set_option_text config key text DuckDBV2SettingScopeDefault)
        connections <- newChildren
        ref <- newIORef (InstanceOpen raw connections)
        registerChild (\case InstanceOpen{} -> True; _ -> False) children ref (keepAlive environment (closeInstanceState ref))
        pure (Instance ref environment)
  where
    -- These options require the native instance and its file system or logger.
    runtimeOptions =
        [ "enable_logging"
        , "logging_mode"
        , "logging_level"
        , "logging_storage"
        , "enabled_log_types"
        , "disabled_log_types"
        , "allowed_directories"
        , "allowed_paths"
        , "disabled_filesystems"
        ]

{- | Close an instance and all its connections. This is idempotent.
Wait for connection creation and all descendant operations to finish before closing.
-}
closeInstance :: Instance -> IO ()
closeInstance Instance{instanceState} = closeInstanceState instanceState

-- | Mark the instance closed before releasing its native state.
closeInstanceState :: IORef InstanceState -> IO ()
closeInstanceState ref = mask_ $ joinCleanup ref \case
    InstanceClosed -> (InstanceClosed, pure ())
    InstanceOpen raw children -> (InstanceClosed, cleanupAll [closeChildren children, destroy c_duckdb_v2_instance_destroy raw])

-- | Create a connection. The final action releases private convenience parents.
createConnection :: Instance -> [Text] -> IO () -> IO Connection
createConnection parent@Instance{instanceState} crss releaseParent = keepAlive parent $ mask_ do
    (rawInstance, children) <-
        readIORef instanceState >>= \case
            InstanceClosed -> throwIO (SQLError "duckdb-simple: instance is closed" Nothing Nothing)
            InstanceOpen raw children -> pure (raw, children)
    raw <- output (c_duckdb_v2_connection_create rawInstance)
    cache <- createTypeCache rawInstance raw crss `onException` destroy c_duckdb_v2_connection_destroy raw
    flip onException (destroyTypeCache cache `finally` destroy c_duckdb_v2_connection_destroy raw) do
        statements <- newChildren
        ref <- newIORef (ConnectionOpen raw cache statements releaseParent)
        registerChild (\case ConnectionOpen{} -> True; _ -> False) children ref (keepAlive parent (closeConnectionState False ref))
        pure (Connection ref parent)

-- | Close a connection and every prepared statement that belongs to it.
closeConnection :: Connection -> IO ()
closeConnection Connection{connectionState} = closeConnectionState True connectionState

-- | GC releases the child first. Parent finalizers run after their references drop.
closeConnectionState :: Bool -> IORef ConnectionState -> IO ()
closeConnectionState releaseParents ref = mask_ $ joinCleanup ref \case
    ConnectionClosed -> (ConnectionClosed, pure ())
    ConnectionOpen{connectionHandle, connectionTypeCache, connectionStatements, connectionReleaseParent} ->
        ( ConnectionClosed
        , cleanupAll
            [ closeChildren connectionStatements
            , destroyTypeCache connectionTypeCache
            , destroy c_duckdb_v2_connection_destroy connectionHandle
            , when releaseParents connectionReleaseParent
            ]
        )

-- | Replace lifecycle state before running its cleanup action.
joinCleanup :: IORef a -> (a -> (a, IO ())) -> IO ()
joinCleanup ref action = join (atomicModifyIORef' ref action)

-- | Close all native state owned by one statement.
closeStatementState :: Statement -> IO ()
closeStatementState Statement{statementState, statementStream, statementArrowResult, statementBindings} = mask_ $ joinCleanup statementState \case
    StatementClosed -> (StatementClosed, pure ())
    StatementOpen{statementHandle, statementParsed, statementSchemaCache} ->
        ( StatementClosed
        , cleanupAll
            [ cleanupStatementStreamRef statementStream
            , clearArrowResultRef statementArrowResult
            , clearBoundValues statementBindings
            , releaseSchemas statementSchemaCache
            , destroy c_duckdb_v2_prepared_statement_destroy statementHandle
            , destroy c_duckdb_v2_sql_statement_destroy statementParsed
            ]
        )

-- | Release both owned statement schemas.
releaseSchemas :: IORef (Maybe (DuckDBV2SchemaHandle, DuckDBV2SchemaHandle)) -> IO ()
releaseSchemas ref =
    atomicModifyIORef' ref (Nothing,) >>= mapM_ \(result, parameters) ->
        destroy c_duckdb_v2_schema_destroy result `finally` destroy c_duckdb_v2_schema_destroy parameters

-- | Release cached metadata before binding or executing the statement again.
clearStatementSchemas :: Statement -> IO ()
clearStatementSchemas Statement{statementState} = mask_ do
    readIORef statementState >>= \case
        StatementClosed -> pure ()
        StatementOpen{statementSchemaCache} -> releaseSchemas statementSchemaCache

-- | Parse SQL and borrow the resulting statements for one action.
withParsedStatements :: Connection -> Query -> ([DuckDBV2SqlStatementHandle] -> IO a) -> IO a
withParsedStatements conn query action = withQueryError query $ withConnectionHandle conn \raw ->
    withQueryCString query \sql -> bracket (output (c_duckdb_v2_parse_sql raw sql)) (destroy c_duckdb_v2_statement_iterator_destroy) \iterator ->
        let gather acc = bracket (output (c_duckdb_v2_statement_iterator_next iterator)) (destroy c_duckdb_v2_sql_statement_destroy) \parsed ->
                if parsed == nullHandle then action (reverse acc) else gather (parsed : acc)
         in gather []

-- | Prepare a borrowed parsed statement. The native handle copies its SQL.
createStatementFromParsed :: Connection -> Query -> DuckDBV2SqlStatementHandle -> IO Statement
createStatementFromParsed parent query parsed = createStatementFromParsedWithCollections parent query parsed []

-- | Prepare SQL and retain its collection bindings in the metadata copy.
createStatementFromParsedWithCollections :: Connection -> Query -> DuckDBV2SqlStatementHandle -> [(Text, DuckDBV2ColumnDataCollectionHandle, [Text])] -> IO Statement
createStatementFromParsedWithCollections parent query parsed collections = withQueryError query $ withConnectionHandle parent \raw -> mask_ do
    addCollections parsed
    handle <- alloca \slot -> do
        poke slot nullHandle
        flip onException (peek slot >>= destroy c_duckdb_v2_prepared_statement_destroy) do
            runInterruptibleQuery parent (checked (c_duckdb_v2_prepared_statement_create raw parsed 0 slot))
            peek slot
    flip onException (destroy c_duckdb_v2_prepared_statement_destroy handle) do
        -- Keep a separate parsed handle for metadata after the input iterator closes.
        text <- output (c_duckdb_v2_sql_statement_get_text parsed) >>= readText
        copied <- withQueryCString (Query text) \sql -> bracket (output (c_duckdb_v2_parse_sql raw sql)) (destroy c_duckdb_v2_statement_iterator_destroy) \iterator ->
            output (c_duckdb_v2_statement_iterator_next iterator)
        flip onException (destroy c_duckdb_v2_sql_statement_destroy copied) do
            addCollections copied
            count <- output (c_duckdb_v2_sql_statement_get_parameter_count copied)
            names <- forM [0 .. fromIntegral count - 1 :: Int] \idx ->
                output (c_duckdb_v2_sql_statement_get_parameter_name copied (fromIntegral idx)) >>= readIdentifier
            schemas <- newIORef Nothing
            bindings <- newIORef IntMap.empty
            stream <- newIORef StatementStreamIdle
            arrow <- newIORef nullHandle
            ref <- newIORef (StatementOpen handle copied schemas)
            let statement = Statement ref parent query stream arrow bindings names
            children <-
                readIORef (connectionState parent) >>= \case
                    ConnectionClosed -> throwIO connectionClosedError
                    ConnectionOpen{connectionStatements} -> pure connectionStatements
            -- The finalizer does not keep its weak key alive through this closure.
            registerChild (\case StatementOpen{} -> True; _ -> False) children ref (keepAlive parent (closeStatementState statement))
            pure statement
  where
    addCollections statement = forM_ collections \(name, collection, columns) -> do
        when (any (Text.any (== '\0')) (name : columns)) $
            throwIO (SQLError "duckdb-simple: collection name or column name contains NUL" Nothing (Just query))
        withIdentifier name \nativeName -> withIdentifierArray columns \nativeColumns ->
            checked (c_duckdb_v2_statement_add_collection statement nativeName collection (ConstPtr (if null columns then nullPtr else nativeColumns)) (fromIntegral (length columns)))

-- | Error raised when an operation targets a closed connection.
connectionClosedError :: SQLError
connectionClosedError = SQLError "duckdb-simple: connection is closed" Nothing Nothing

-- | Error raised when an operation targets a closed statement.
statementClosedError :: Statement -> SQLError
statementClosedError Statement{statementQuery} = SQLError "duckdb-simple: statement is closed" Nothing (Just statementQuery)

-- | Keep a Haskell owner reachable until an action finishes.
keepAlive :: a -> IO b -> IO b
keepAlive owner (IO action) = IO \state -> keepAlive# owner state action

-- | Borrow a live instance handle.
withInstanceHandle :: Instance -> (DuckDBV2InstanceHandle -> IO a) -> IO a
withInstanceHandle owner@Instance{instanceState} action =
    keepAlive owner $
        readIORef instanceState >>= \case
            InstanceClosed -> throwIO (SQLError "duckdb-simple: instance is closed" Nothing Nothing)
            InstanceOpen raw _ -> action raw

-- | Borrow the instance that owns a live connection.
withDatabaseHandle :: Connection -> (DuckDBV2InstanceHandle -> IO a) -> IO a
withDatabaseHandle conn action = withConnectionHandle conn \_ -> withInstanceHandle (connectionInstance conn) action

-- | Borrow a live connection handle.
withConnectionHandle :: Connection -> (DuckDBV2ConnectionHandle -> IO a) -> IO a
withConnectionHandle owner@Connection{connectionState} action =
    keepAlive owner $
        readIORef connectionState >>= \case
            ConnectionClosed -> throwIO connectionClosedError
            ConnectionOpen{connectionHandle} -> action connectionHandle

-- | Borrow a connection's logical type cache.
withTypeCache :: Connection -> (TypeCache -> IO a) -> IO a
withTypeCache owner@Connection{connectionState} action =
    keepAlive owner $
        readIORef connectionState >>= \case
            ConnectionClosed -> throwIO connectionClosedError
            ConnectionOpen{connectionTypeCache} -> action connectionTypeCache

-- | Borrow a live prepared statement handle.
withStatementHandle :: Statement -> (DuckDBV2PreparedStatementHandle -> IO a) -> IO a
withStatementHandle owner@Statement{statementState, statementConnection, statementQuery} action = keepAlive owner $
    withQueryError statementQuery $ withConnectionHandle statementConnection \_ ->
        readIORef statementState >>= \case
            StatementClosed -> throwIO (statementClosedError owner)
            StatementOpen{statementHandle} -> action statementHandle

-- | Attach the SQL text to errors from native calls.
withQueryError :: Query -> IO a -> IO a
withQueryError query action = action `catch` \err -> throwIO err{sqlErrorQuery = Just query}

-- | Borrow SQL as a NUL-terminated UTF-8 string.
withQueryCString :: Query -> (ConstPtr CChar -> IO a) -> IO a
withQueryCString query@(Query text) action = do
    when (Text.any (== '\0') text) $ throwIO (SQLError "duckdb-simple: query contains NUL" Nothing (Just query))
    TextForeign.withCString text (action . ConstPtr)

-- | Copy a UTF-8 C string.
peekUtf8CString :: (Coercible a CString) => a -> IO Text
peekUtf8CString ptr = Text.decodeUtf8 <$> BS.packCString (coerce ptr)

-- | Run native work with cancellation and wait for it before releasing inputs.
runInterruptibleQuery :: forall a. Connection -> IO a -> IO a
runInterruptibleQuery conn action = withConnectionHandle conn \handle -> mask \restore -> do
    done <- newEmptyMVar
    _ <- forkIO do
        outcome <- try action :: IO (Either SomeException a)
        putMVar done outcome
    let wait =
            tryReadMVar done >>= \case
                Just _ -> pure ()
                Nothing -> void (c_duckdb_v2_connection_interrupt handle nullPtr) >> threadDelay 10000 >> wait
        stop = uninterruptibleMask_ wait
    outcome <- restore (readMVar done) `onException` stop
    either throwIO pure outcome

-- | Execute a borrowed parsed statement. The caller owns the returned cursor.
executeParsedResult :: Connection -> Query -> DuckDBV2SqlStatementHandle -> IO DuckDBV2ResultHandle
executeParsedResult conn query parsed = withQueryError query $ withConnectionHandle conn \raw -> mask_ $ alloca \slot -> do
    poke slot nullHandle
    flip onException (peek slot >>= destroy c_duckdb_v2_result_destroy) do
        runInterruptibleQuery conn $
            checked (c_duckdb_v2_statement_execute raw parsed (ConstPtr nullPtr) (ConstPtr nullPtr) 0 slot)
        peek slot

-- | Execute a parsed statement directly and drain all its side effects.
executeParsedStatement :: Connection -> Query -> DuckDBV2SqlStatementHandle -> IO Int
executeParsedStatement conn queryText parsed = withQueryError queryText $
    bracket (executeParsedResult conn queryText parsed) (destroy c_duckdb_v2_result_destroy) \result ->
        fromIntegral <$> runInterruptibleQuery conn (output (c_duckdb_v2_result_drain result))

-- | Execute a prepared statement. The caller owns the returned result.
executePreparedResult :: Statement -> IO DuckDBV2ResultHandle
executePreparedResult stmt = withStatementHandle stmt \handle -> mask_ $ alloca \slot -> do
    poke slot nullHandle
    flip onException (peek slot >>= destroy c_duckdb_v2_result_destroy) $
        withBoundValues stmt \names values count -> do
            runInterruptibleQuery (statementConnection stmt) $
                checked (c_duckdb_v2_prepared_statement_execute handle names values count slot)
            peek slot

-- | Borrow parameter names and values until native execution returns.
withBoundValues :: Statement -> (ConstPtr DuckDBV2IdentifierT -> ConstPtr DuckDBV2ValueHandle -> DuckDBIdx -> IO a) -> IO a
withBoundValues stmt action = do
    bindings <- IntMap.toAscList <$> readIORef (statementBindings stmt)
    let names = map (\(idx, _) -> statementParameterNames stmt !! (idx - 1)) bindings
        values = map snd bindings
    withIdentifierArray names \namesPtr -> withArray values \valuesPtr ->
        action (ConstPtr namesPtr) (ConstPtr valuesPtr) (fromIntegral (length bindings))

-- | Execute a prepared Arrow result and register it for statement cleanup.
executePreparedArrowResult :: Statement -> IO DuckDBV2ArrowResultHandle
executePreparedArrowResult stmt = withStatementHandle stmt \handle -> mask_ $ alloca \slot -> do
    poke slot nullHandle
    flip onException (peek slot >>= destroy c_duckdb_v2_arrow_result_destroy) $
        withBoundValues stmt \names values count -> do
            runInterruptibleQuery (statementConnection stmt) $
                checked (c_duckdb_v2_prepared_statement_execute_arrow handle names values count 2048 slot)
            result <- peek slot
            writeIORef (statementArrowResult stmt) result
            pure result

-- | Execute an Arrow result and release it on every exit path.
withArrowResult :: Statement -> (DuckDBV2ArrowResultHandle -> IO a) -> IO a
withArrowResult stmt = bracket (executePreparedArrowResult stmt) (const (clearArrowResult stmt))

-- | Release an Arrow result before callbacks use materialized batches.
clearArrowResult :: Statement -> IO ()
clearArrowResult = clearArrowResultRef . statementArrowResult

-- | Remove the owned Arrow cursor before releasing it.
clearArrowResultRef :: IORef DuckDBV2ArrowResultHandle -> IO ()
clearArrowResultRef ref =
    mask_ $
        atomicModifyIORef' ref (nullHandle,) >>= destroy c_duckdb_v2_arrow_result_destroy

-- | Execute and release one result on every exit path.
withResult :: Statement -> (DuckDBV2ResultHandle -> IO a) -> IO a
withResult stmt = bracket (executePreparedResult stmt) (destroy c_duckdb_v2_result_destroy)

-- | Fetch one owned chunk. A null handle denotes completion.
fetchResultChunk :: Connection -> DuckDBV2ResultHandle -> IO DuckDBV2DataChunkHandle
fetchResultChunk conn result = mask_ $ alloca \slot -> do
    poke slot nullHandle
    flip onException (peek slot >>= destroyDataChunk) do
        runInterruptibleQuery conn (checked (c_duckdb_v2_result_fetch_chunk result slot))
        peek slot

-- | Release an owned native data chunk.
destroyDataChunk :: DuckDBV2DataChunkHandle -> IO ()
destroyDataChunk = destroy c_duckdb_v2_data_chunk_destroy

-- | Release the active cursor and return to the idle state.
cleanupStatementStreamRef :: IORef StatementStreamState -> IO ()
cleanupStatementStreamRef ref = mask_ do
    state <- atomicModifyIORef' ref (StatementStreamIdle,)
    case state of
        StatementStreamActive StatementStream{statementStreamResult, statementStreamPendingChunks, statementStreamChunk} ->
            maybe (pure ()) (destroyDataChunk . statementStreamChunkPtr) statementStreamChunk
                `finally` (mapM_ destroyDataChunk statementStreamPendingChunks `finally` destroy c_duckdb_v2_result_destroy statementStreamResult)
        _ -> pure ()

-- | Transfer ownership of one successfully constructed parameter value.
storeBoundValue :: Statement -> DuckDBIdx -> DuckDBV2ValueHandle -> IO ()
storeBoundValue stmt index value = withStatementHandle stmt \_ -> mask_ do
    let idx = fromIntegral index
    unless (idx >= 1 && idx <= length (statementParameterNames stmt)) $
        throwIO (SQLError "duckdb-simple: parameter index is out of bounds" Nothing (Just (statementQuery stmt)))
    previous <- IntMap.lookup idx <$> readIORef (statementBindings stmt)
    mapM_ destroyValue previous
    atomicModifyIORef' (statementBindings stmt) \values -> (IntMap.insert idx value values, ())

-- | Release all owned parameter values.
clearBoundValues :: IORef (IntMap.IntMap DuckDBV2ValueHandle) -> IO ()
clearBoundValues ref = atomicModifyIORef' ref (IntMap.empty,) >>= mapM_ destroyValue

{- | Bind SQL metadata and borrow the cached output and parameter schemas.
The handles remain valid until the statement resets, rebinds, or closes.
-}
statementSchemas :: Statement -> IO (DuckDBV2SchemaHandle, DuckDBV2SchemaHandle)
statementSchemas stmt = withStatementHandle stmt \_ -> mask_ do
    state <- readIORef (statementState stmt)
    case state of
        StatementClosed -> throwIO (statementClosedError stmt)
        StatementOpen{statementParsed, statementSchemaCache} ->
            readIORef statementSchemaCache >>= \case
                Just schemas -> pure schemas
                Nothing -> withConnectionHandle (statementConnection stmt) \conn -> alloca \result -> alloca \parameters -> do
                    poke result nullHandle
                    poke parameters nullHandle
                    let release = (peek result >>= destroy c_duckdb_v2_schema_destroy) `finally` (peek parameters >>= destroy c_duckdb_v2_schema_destroy)
                    flip onException release do
                        runInterruptibleQuery (statementConnection stmt) (checked (c_duckdb_v2_statement_bind conn statementParsed result parameters))
                        schemas <- (,) <$> peek result <*> peek parameters
                        writeIORef statementSchemaCache (Just schemas)
                        pure schemas

-- | Read a parameter type when the binder can determine it without values.
preparedParameterType :: Statement -> DuckDBIdx -> IO (Maybe LogicalTypeRep)
preparedParameterType stmt idx = do
    (_, schema) <- statementSchemas stmt
    alloca \name -> do
        dtype <- output (c_duckdb_v2_schema_get_field schema (idx - 1) name)
        ident <- output (c_duckdb_v2_logical_type_get_id dtype)
        if ident == DuckDBV2LogicalTypeIdUnknown || ident == DuckDBV2LogicalTypeIdSQLNull
            then pure Nothing
            else Just <$> logicalTypeToRep dtype

-- | Column type names and the logical types that they denote.
duckDBTypeNames :: [(Text, DuckDBV2LogicalTypeId)]
duckDBTypeNames =
    [ ("BOOLEAN", DuckDBV2LogicalTypeIdBoolean)
    , ("TINYINT", DuckDBV2LogicalTypeIdTinyInt)
    , ("SMALLINT", DuckDBV2LogicalTypeIdSmallInt)
    , ("INTEGER", DuckDBV2LogicalTypeIdInteger)
    , ("BIGINT", DuckDBV2LogicalTypeIdBigInt)
    , ("HUGEINT", DuckDBV2LogicalTypeIdHugeInt)
    , ("UTINYINT", DuckDBV2LogicalTypeIdUTinyInt)
    , ("USMALLINT", DuckDBV2LogicalTypeIdUSmallInt)
    , ("UINTEGER", DuckDBV2LogicalTypeIdUInteger)
    , ("UBIGINT", DuckDBV2LogicalTypeIdUBigInt)
    , ("UHUGEINT", DuckDBV2LogicalTypeIdUHugeInt)
    , ("FLOAT", DuckDBV2LogicalTypeIdFloat)
    , ("DOUBLE", DuckDBV2LogicalTypeIdDouble)
    , ("DATE", DuckDBV2LogicalTypeIdDate)
    , ("TIME", DuckDBV2LogicalTypeIdTime)
    , ("TIMETZ", DuckDBV2LogicalTypeIdTimeTz)
    , ("TIMESTAMP", DuckDBV2LogicalTypeIdTimestamp)
    , ("TIMESTAMPTZ", DuckDBV2LogicalTypeIdTimestampTz)
    , ("TIMESTAMPTZ_NS", DuckDBV2LogicalTypeIdTimestampTzNs)
    , ("TIMESTAMP_NS WITH TIME ZONE", DuckDBV2LogicalTypeIdTimestampTzNs)
    , ("INTERVAL", DuckDBV2LogicalTypeIdInterval)
    , ("TEXT", DuckDBV2LogicalTypeIdVarchar)
    , ("BLOB", DuckDBV2LogicalTypeIdBlob)
    , ("GEOMETRY", DuckDBV2LogicalTypeIdGeometry)
    , ("VARIANT", DuckDBV2LogicalTypeIdVariant)
    , ("UUID", DuckDBV2LogicalTypeIdUUID)
    , ("BIT", DuckDBV2LogicalTypeIdBit)
    , ("BIGNUM", DuckDBV2LogicalTypeIdBigNum)
    , -- NULL gives an element type to Maybe values without data.
      ("NULL", DuckDBV2LogicalTypeIdSQLNull)
    ]

-- | Find the type that a column type name denotes.
duckDBTypeFromName :: Text -> Maybe DuckDBV2LogicalTypeId
duckDBTypeFromName name = lookup name duckDBTypeNames

-- | Find the column type name of a type. Other types use their 'Show' text.
duckDBTypeToName :: DuckDBV2LogicalTypeId -> Text
duckDBTypeToName dtype =
    maybe (Text.pack (show dtype)) fst (find ((== dtype) . snd) duckDBTypeNames)
