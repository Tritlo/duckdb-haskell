{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      : Database.DuckDB.Simple.Internal
-- Description : Native V2 ownership and execution used by the public API.
module Database.DuckDB.Simple.Internal (
  module Database.DuckDB.Simple.Native,
  Environment (..),
  EnvironmentState (..),
  Instance (..),
  InstanceState (..),
  Connection (..),
  defaultAggregateStripes,
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
  withCollectionStatement,
  statementSchemas,
  clearStatementSchemas,
  withKeptParameterTypes,
  preparedParameterType,
  storeBoundValue,
  clearBoundValues,
  cleanupStatementStreamRef,
  finalizeStream,
  finalizeChunk,
  connectionClosedError,
  statementClosedError,
  keepAlive,
  withInstanceHandle,
  withDatabaseHandle,
  withConnectionHandle,
  withConnectionLock,
  withResultWindow,
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

import Control.Concurrent (
  MVar,
  ThreadId,
  forkIO,
  myThreadId,
  newEmptyMVar,
  newMVar,
  putMVar,
  readMVar,
  takeMVar,
  threadDelay,
  tryReadMVar,
  tryTakeMVar,
  withMVar,
 )
import Control.Exception (SomeException, bracket, bracket_, catch, finally, mask, mask_, onException, throwIO, try, uninterruptibleMask_)
import Control.Monad (forM, forM_, join, unless, void, when)
import Data.Array (Array, bounds, inRange, listArray, (!))
import qualified Data.ByteString as BS
import Data.Coerce (Coercible, coerce)
import Data.IORef (IORef, atomicModifyIORef', atomicWriteIORef, mkWeakIORef, modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.IntMap.Strict as IntMap
import Data.List (dropWhileEnd, find, intercalate, partition)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Foreign as TextForeign
import Database.DuckDB.Simple.CallbackMark (hasCallbackMark)
import Database.DuckDB.Simple.FromField (FieldValue)
import Database.DuckDB.Simple.LogicalRep (LogicalTypeRep)
import Database.DuckDB.Simple.Native
import Database.DuckDB.Simple.TypeCache (TypeCache, createTypeCache, destroyTypeCache)
import Database.DuckDB.Simple.V2.LogicalRep (logicalTypeToRep)
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.C.String (CString)
import Foreign.C.Types (CChar)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (peek, poke)
import GHC.Exts (keepAlive#)
import GHC.IO (IO (..))
import GHC.Weak.Finalize (getFinalizerExceptionHandler)
import System.Mem.Weak (Weak, deRefWeak, finalize)
import System.Timeout (timeout)

{- Note [Native handle ownership]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The native handles form a tree. An environment owns its instances, an instance
owns its connections, and a connection owns its prepared statements. The open
state of each parent holds a registry of its children (Children). Each child
value also holds its parent: instanceEnvironment, connectionInstance, and
statementConnection.

A registry entry is a weak pointer and an MVar. The key of the weak pointer is
the state IORef of the child, so the registry does not keep a live child
reachable. The finalizer of the weak pointer runs the cleanup of the child, and
then it puts the MVar (registerChild). The cleanup closes the state of the
child, and it keeps the parent alive until the cleanup ends (keepAlive). A
child thus closes in one of three ways: by an explicit close, by the close of
its parent, or by the finalizer after the child becomes unreachable. A close
first replaces the state with the closed state
(joinCleanup). A close of a closed state has no effect. Thus a finalizer after
an explicit close has no effect.

A parent closes its children before it destroys its own native handle
(closeChildren). Thus the close order is statement, connection, instance, and
environment. finishChild runs the finalizer of one child, and it waits for the
MVar of that child. The MVar tells that the cleanup is complete, also when the
cleanup runs in another thread. A parent marks its state closed before it
closes its children. A create function raises an error when the state of its
parent is closed. A close must not start while the parent creates a child (see
closeEnvironment and closeInstance).

The garbage collector releases a child first. The finalizer of a parent runs
after the references to the parent drop. The collector runs the finalizers of
one collection in sequence in one thread, and a parent waits for its children.
A cleanup in that thread can wait for a child finalizer that is behind it.
Thus the finalizers of instances and connections run in new threads
(forkFinalizer). A statement has no children, so its finalizer runs in the
thread of the collector.

The environment has no parent and no registry entry. createEnvironment gives
its state a weak pointer of its own, with a finalizer in a new thread.

openWithOptions makes a private environment and instance for one connection.
The connection keeps an action that closes them (connectionReleaseParent). An
explicit close of the connection runs this action, and the finalizer of the
connection does not. Then the finalizers of the private instance and
environment close them.

A function that borrows a native handle, for example withConnectionHandle, runs
its action under keepAlive of the owner. Thus the finalizer of the owner does
not destroy the handle while the action uses it.
-}

-- | Track child finalizers without retaining live children. See
-- Note [Native handle ownership].
newtype Children a = Children (IORef [(Weak (IORef a), MVar ())])

-- | Create an empty child registry.
newChildren :: IO (Children a)
newChildren = Children <$> newIORef []

-- | Register a child and remove completed entries. The first argument runs
-- the finalizer of the child. Use 'forkFinalizer' for a child that has children.
-- See Note [Native handle ownership].
registerChild :: (IO () -> IO ()) -> (a -> Bool) -> Children a -> IORef a -> IO () -> IO ()
registerChild runFinalizer isOpen (Children children) child cleanup = mask_ do
  previous <- readIORef children
  finished <- fmap concat $ forM previous \entry@(weak, done) ->
    deRefWeak weak >>= \case
      Nothing -> finishChild entry >> pure [done]
      Just ref -> do
        state <- readIORef ref
        if isOpen state then pure [] else finishChild entry >> pure [done]
  -- Finish stale cleanup before transferring ownership of the new child.
  done <- newEmptyMVar
  weak <- mkWeakIORef child (runFinalizer (cleanup `finally` putMVar done ()))
  -- Remove only finished snapshot entries. Keep concurrent registrations.
  atomicModifyIORef' children $ \current ->
    ((weak, done) : filter (\(_, completed) -> completed `notElem` finished) current, ())

-- | Run a finalizer in a new thread. Report an exception as the collector
-- reports it for a finalizer. See Note [Native handle ownership].
forkFinalizer :: IO () -> IO ()
forkFinalizer cleanup =
  void . forkIO $
    cleanup `catch` \(err :: SomeException) -> getFinalizerExceptionHandler >>= ($ err)

-- | Run a child finalizer and wait for its completion. See
-- Note [Native handle ownership].
finishChild :: (Weak (IORef a), MVar ()) -> IO ()
finishChild (weak, done) = uninterruptibleMask_ (finalize weak `finally` readMVar done)

-- | Complete child finalizers before their parent closes. See
-- Note [Native handle ownership].
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
data Connection = Connection
  { connectionState :: IORef ConnectionState
  , connectionInstance :: Instance
  , connectionLock :: MVar ()
  -- ^ Held while a native result of this connection runs, and for each fetch
  -- of an explicit stream. A DuckDB callback waits for it at most
  -- 'connectionLockBound'. See 'withConnectionLock' and Note [Connection lock].
  , connectionWindow :: IORef (Maybe ThreadId)
  -- ^ The helper thread that runs the current result. See 'withResultWindow'
  -- and Note [Result window].
  , connectionLockBound :: Maybe Int
  -- ^ The longest wait in microseconds of a DuckDB callback for
  -- 'connectionLock'. 'Nothing' sets no bound.
  , connectionStripes :: Int
  -- ^ The number of stripes in the state table of a typed aggregate function.
  -- See Note [State table stripes] in "Database.DuckDB.Simple.Typed.Aggregate".
  , connectionArrowBatch :: Int
  -- ^ The largest number of rows in one Arrow array of a result. 0 means
  -- 131072. See 'executePreparedArrowResult'.
  }

-- | The default of @connectionAggregateStripes@ in @ConnectionOptions@: 64
-- stripes. See Note [State table stripes] in
-- "Database.DuckDB.Simple.Typed.Aggregate".
defaultAggregateStripes :: Int
defaultAggregateStripes = 64

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
  , statementParameterNames :: Array Int Text
  -- ^ The parameter names in binding order. The first index is 1.
  , statementParameterTypes :: IORef (Maybe (IntMap.IntMap (Maybe LogicalTypeRep)))
  -- ^ The kept parameter types while 'withKeptParameterTypes' runs. Otherwise
  -- Nothing. See Note [Statement schemas].
  }

-- | Internal prepared statement state.
data StatementState
  = StatementClosed
  | StatementOpen
      { statementHandle :: DuckDBV2PreparedStatementHandle
      , statementParsed :: DuckDBV2SqlStatementHandle
      , statementSchemaCache :: IORef (Maybe (DuckDBV2SchemaHandle, DuckDBV2SchemaHandle))
      }

{- Note [Statement cursor]
~~~~~~~~~~~~~~~~~~~~~~~~~~
The cursor of a statement (statementStream) is in one of three states.

- StatementStreamIdle. The statement has no result. The next fetch
  (nextRowWith in "Database.DuckDB.Simple.Result") executes the statement in
  the mode of that fetch, and the cursor becomes active. A result that is not a
  query result or a count of changed rows gives no rows, and the cursor becomes
  exhausted.

- StatementStreamActive. The cursor owns a result. Each next fetch reads from
  this result in the mode of the result, also when the fetch asks for the
  other mode. A materialized result has all its chunks in
  statementStreamPendingChunks, and its result handle is null. A streaming
  result keeps its result handle open, and it fetches each next chunk from
  DuckDB.

- StatementStreamExhausted. The cursor has no result, and each fetch gives no
  row. A fetch that raises an error while it reads from an active cursor also
  makes the cursor exhausted.

Only a reset makes the cursor idle again: resetStatementStream, or the close of
the statement (closeStatementState). Both release an active result with
cleanupStatementStreamRef.

The cursor owns each native chunk that it holds. fetchChunk writes a new chunk
to the cursor before it reads the metadata of the chunk or makes the readers.
Thus a release of the cursor also releases the chunk. finalizeStream releases
the current chunk, the pending chunks, and the result, in that order.
-}

-- | Cursor state. Exhaustion persists until the caller resets the statement.
-- See Note [Statement cursor].
data StatementStreamState = StatementStreamIdle | StatementStreamExhausted | StatementStreamActive StatementStream

-- | An execution cursor or an owned collection of materialized chunks. See
-- Note [Statement cursor].
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
  , statementStreamColumnTree :: IORef (Maybe LogicalTypeRep)
  -- ^ The type tree of the column, converted from the vector of the first chunk
  -- that needs it. Each execution makes new columns, so the tree lives for one
  -- execution. See Note [Type trees] in "Database.DuckDB.Simple.Materialize".
  , statementStreamColumnLabels :: IORef (IntMap.IntMap Text)
  -- ^ The ENUM labels that the readers of the column read, by index. They live
  -- for one execution, as the tree. See Note [ENUM labels] in
  -- "Database.DuckDB.Simple.Materialize".
  }

-- | One owned chunk and the readers that borrow its vectors.
data StatementStreamChunk = StatementStreamChunk
  { statementStreamChunkPtr :: DuckDBV2DataChunkHandle
  , statementStreamChunkSize :: Int
  , statementStreamChunkIndex :: Int
  , statementStreamChunkReaders :: [Int -> IO FieldValue]
  }

-- | Create an environment. Close it after all related work finishes. See
-- Note [Native handle ownership].
createEnvironment :: IO Environment
createEnvironment = mask_ do
  checkLibraryVersion
  raw <- output c_duckdb_v2_environment_create
  children <- newChildren
  ref <- newIORef (EnvironmentOpen raw children)
  _ <- mkWeakIORef ref (forkFinalizer (closeEnvironmentState ref))
  pure (Environment ref)

-- | Require the DuckDB release that the V2 bindings describe. Upstream marks
-- the V2 API as unstable, so another release can change its functions.
checkLibraryVersion :: IO ()
checkLibraryVersion = do
  actual <- output c_duckdb_v2_library_version >>= readText
  unless (actual == expected) $
    throwIO (SQLError ("duckdb-simple: the V2 API requires DuckDB " <> expected <> ", but the loaded library is " <> actual) Nothing Nothing)
  where
    expected = Text.pack ('v' : intercalate "." (map show [dUCKDB_V2_API_VERSION_MAJOR, dUCKDB_V2_API_VERSION_MINOR, dUCKDB_V2_API_VERSION_PATCH]))

-- | Close an environment and its instances and connections.
-- Wait for child creation and all descendant operations to finish before closing.
-- DuckDB refuses to destroy the environment while an instance is alive that
-- 'createInstance' did not create. In this case, this function closes the
-- instances from 'createInstance' and their connections, raises an 'SQLError'
-- with the type 'ErrorResourceInUse', and keeps the environment open.
-- Destroy the other instance, then close the environment again.
-- After a successful close, a second close has no effect.
closeEnvironment :: Environment -> IO ()
closeEnvironment Environment{environmentState} = closeEnvironmentState environmentState

-- | Mark the environment closed before releasing its native state, so that a
-- 'createInstance' that starts after the mark raises an error. A
-- 'createInstance' that read the open state before the mark can still register
-- its instance. Thus the caller must wait for child creation before the close
-- (see 'closeEnvironment'). If the native destroy refuses with
-- @ERROR_RESOURCE_IN_USE@, the native environment is still alive. In this case,
-- put back the open state with the same child registry, and raise the error.
-- 'closeChildren' empties the registry, and a later 'createInstance' can register
-- in it. After a different error, the state stays closed, because the header does
-- not say that the handle stays valid.
closeEnvironmentState :: IORef EnvironmentState -> IO ()
closeEnvironmentState ref = mask_ $ joinCleanup ref \case
  EnvironmentClosed -> (EnvironmentClosed, pure ())
  EnvironmentOpen raw children ->
    ( EnvironmentClosed
    , cleanupAll
        [ closeChildren children
        , destroy c_duckdb_v2_environment_destroy raw `catch` \(err :: SQLError) -> do
            when (sqlErrorType err == Just ErrorResourceInUse) (atomicWriteIORef ref (EnvironmentOpen raw children))
            throwIO err
        ]
    )

-- | Create an instance with configuration applied before its first connection.
createInstance :: Environment -> [(Text, Text)] -> IO Instance
createInstance environment@Environment{environmentState} settings = keepAlive environment $ mask_ do
  (rawEnvironment, children) <-
    readIORef environmentState >>= \case
      EnvironmentClosed -> throwIO (SQLError "duckdb-simple: environment is closed" Nothing Nothing)
      EnvironmentOpen raw children -> pure (raw, children)
  when (any (\(name, value) -> Text.any (== '\0') name || Text.any (== '\0') value) settings) $
    throwInvalidArgument "configuration contains NUL"
  let hasWhitelist = any ((`elem` ["allowed_directories", "allowed_paths"]) . Text.toLower . fst) settings
      needsInstance (name, _) = Text.toLower name `elem` runtimeOptions || (hasWhitelist && Text.toLower name == "enable_external_access")
      (deferredSettings, startupSettings) = partition needsInstance settings
      (runtimeSettings, externalAccessSettings) = partition ((/= "enable_external_access") . Text.toLower . fst) deferredSettings
  raw <- withTextArray (map fst startupSettings) \names -> withTextArray (map snd startupSettings) \values ->
    output
      ( c_duckdb_v2_instance_create_with_options
          rawEnvironment
          (ConstPtr (if null startupSettings then nullPtr else names))
          (ConstPtr (if null startupSettings then nullPtr else values))
          (fromIntegral (length startupSettings))
      )
  flip onException (destroy c_duckdb_v2_instance_destroy raw) do
    config <- output (c_duckdb_v2_instance_get_config raw)
    -- Set the whitelist before external access can be disabled.
    forM_ (runtimeSettings ++ externalAccessSettings) \(name, value) -> withIdentifier name \key -> withText value \text ->
      checked (c_duckdb_v2_config_set_option_text config key text DuckDBV2SettingScopeDefault)
    connections <- newChildren
    ref <- newIORef (InstanceOpen raw connections)
    registerChild forkFinalizer (\case InstanceOpen{} -> True; _ -> False) children ref (keepAlive environment (closeInstanceState ref))
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

-- | Close an instance and all its connections. This is idempotent.
-- Wait for connection creation and all descendant operations to finish before closing.
closeInstance :: Instance -> IO ()
closeInstance Instance{instanceState} = closeInstanceState instanceState

-- | Mark the instance closed before releasing its native state.
closeInstanceState :: IORef InstanceState -> IO ()
closeInstanceState ref = mask_ $ joinCleanup ref \case
  InstanceClosed -> (InstanceClosed, pure ())
  InstanceOpen raw children -> (InstanceClosed, cleanupAll [closeChildren children, destroy c_duckdb_v2_instance_destroy raw])

-- | Create a connection with its CRS names, the bound of a callback that waits
-- for its lock, the number of stripes of a typed aggregate, and the Arrow batch
-- size. The final action releases private convenience parents.
createConnection :: Instance -> [Text] -> Maybe Int -> Int -> Int -> IO () -> IO Connection
createConnection parent@Instance{instanceState} crss callbackLockWait stripes arrowBatch releaseParent = keepAlive parent $ mask_ do
  (rawInstance, children) <-
    readIORef instanceState >>= \case
      InstanceClosed -> throwIO (SQLError "duckdb-simple: instance is closed" Nothing Nothing)
      InstanceOpen raw children -> pure (raw, children)
  raw <- output (c_duckdb_v2_connection_create rawInstance)
  -- See Note [CRS types of a connection] in "Database.DuckDB.Simple.TypeCache".
  cache <- createTypeCache rawInstance raw crss `onException` destroy c_duckdb_v2_connection_destroy raw
  flip onException (destroyTypeCache cache `finally` destroy c_duckdb_v2_connection_destroy raw) do
    statements <- newChildren
    ref <- newIORef (ConnectionOpen raw cache statements releaseParent)
    registerChild forkFinalizer (\case ConnectionOpen{} -> True; _ -> False) children ref (keepAlive parent (closeConnectionState False ref))
    Connection ref parent <$> newMVar () <*> newIORef Nothing <*> pure callbackLockWait <*> pure stripes <*> pure arrowBatch

-- | Close a connection and every prepared statement that belongs to it.
closeConnection :: Connection -> IO ()
closeConnection Connection{connectionState} = closeConnectionState True connectionState

-- | Close the state of a connection. The first argument tells whether to run
-- 'connectionReleaseParent'. An explicit close gives 'True', and the finalizer
-- gives 'False'. See Note [Native handle ownership].
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

-- | Replace lifecycle state before running its cleanup action. See
-- Note [Native handle ownership].
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
-- See Note [Statement schemas].
clearStatementSchemas :: Statement -> IO ()
clearStatementSchemas Statement{statementState} = mask_ do
  readIORef statementState >>= \case
    StatementClosed -> pure ()
    StatementOpen{statementSchemaCache} -> releaseSchemas statementSchemaCache

-- | Keep the parameter types that the action reads until the action ends. The
-- first read of a parameter type runs the binder. Each later read of that type
-- in the action uses the kept type, also after a column type changes. See
-- Note [Statement schemas].
withKeptParameterTypes :: Statement -> IO a -> IO a
withKeptParameterTypes Statement{statementParameterTypes} =
  bracket_ (writeIORef statementParameterTypes (Just IntMap.empty)) (writeIORef statementParameterTypes Nothing)

-- | Parse SQL and borrow the resulting statements for one action.
withParsedStatements :: Connection -> Query -> ([DuckDBV2SqlStatementHandle] -> IO a) -> IO a
withParsedStatements conn query action = withQueryError query $ withConnectionHandle conn \raw ->
  withQueryCString query \sql -> bracket (output (c_duckdb_v2_parse_sql raw sql)) (destroy c_duckdb_v2_statement_iterator_destroy) \iterator ->
    let gather acc = bracket (output (c_duckdb_v2_statement_iterator_next iterator)) (destroy c_duckdb_v2_sql_statement_destroy) \parsed ->
          if parsed == nullHandle then action (reverse acc) else gather (parsed : acc)
    in  gather []

-- | Prepare a borrowed parsed statement. The native handle copies its SQL.
createStatementFromParsed :: Connection -> Query -> DuckDBV2SqlStatementHandle -> IO Statement
createStatementFromParsed parent query parsed = createStatementFromParsedWithCollections parent query parsed []

-- | Prepare a borrowed parsed statement with named input collections. A
-- collection is a DuckDB column data collection: rows in memory that the caller
-- owns. @duckdb_v2_statement_add_collection@ binds a collection to a statement
-- under a name, and the SQL of the statement reads it as a table with that name.
-- Each triple is the name, the collection handle, and the column names. An empty
-- list of column names selects the default names @col1@ to @colN@.
-- The function binds the collections to the parsed statement that it prepares,
-- and to the copy that it keeps for parameter names and schemas. The caller must
-- keep each collection alive and unchanged until the statement closes and the
-- caller destroys the parsed statement.
createStatementFromParsedWithCollections
  :: Connection -> Query -> DuckDBV2SqlStatementHandle -> [(Text, DuckDBV2ColumnDataCollectionHandle, [Text])] -> IO Statement
createStatementFromParsedWithCollections parent query parsed collections = withQueryError query $ withConnectionHandle parent \raw -> mask_ do
  addCollections parsed
  handle <- alloca \slot -> do
    poke slot nullHandle
    flip onException (peek slot >>= destroy c_duckdb_v2_prepared_statement_destroy) do
      -- DuckDB also rejects preparation while another result is open. See
      -- Note [Connection lock].
      withConnectionLock parent (runInterruptibleQuery parent (checked (c_duckdb_v2_prepared_statement_create raw parsed 0 slot)))
      peek slot
  flip onException (destroy c_duckdb_v2_prepared_statement_destroy handle) do
    -- Keep a separate parsed handle for metadata after the input iterator closes.
    text <- output (c_duckdb_v2_sql_statement_get_text parsed) >>= readText
    copied <- withQueryCString (Query text) \sql -> bracket (output (c_duckdb_v2_parse_sql raw sql)) (destroy c_duckdb_v2_statement_iterator_destroy) \iterator ->
      output (c_duckdb_v2_statement_iterator_next iterator)
    flip onException (destroy c_duckdb_v2_sql_statement_destroy copied) do
      addCollections copied
      count <- fromIntegral <$> output (c_duckdb_v2_sql_statement_get_parameter_count copied)
      names <- parameterNames copied count
      schemas <- newIORef Nothing
      bindings <- newIORef IntMap.empty
      stream <- newIORef StatementStreamIdle
      arrow <- newIORef nullHandle
      kept <- newIORef Nothing
      ref <- newIORef (StatementOpen handle copied schemas)
      let statement = Statement ref parent query stream arrow bindings (listArray (1, count) names) kept
      children <-
        readIORef (connectionState parent) >>= \case
          ConnectionClosed -> throwIO connectionClosedError
          ConnectionOpen{connectionStatements} -> pure connectionStatements
      -- The finalizer does not keep its weak key alive through this closure.
      -- See Note [Native handle ownership].
      registerChild id (\case StatementOpen{} -> True; _ -> False) children ref (keepAlive parent (closeStatementState statement))
      pure statement
  where
    addCollections statement = forM_ collections \(name, collection, columns) -> do
      when (any (Text.any (== '\0')) (name : columns)) $
        withQueryError query (throwInvalidArgument "collection name or column name contains NUL")
      withIdentifier name \nativeName -> withIdentifierArray columns \nativeColumns ->
        checked
          ( c_duckdb_v2_statement_add_collection
              statement
              nativeName
              collection
              (ConstPtr (if null columns then nullPtr else nativeColumns))
              (fromIntegral (length columns))
          )

-- | Prepare SQL with borrowed column collections as named input tables.
-- The caller must keep each collection alive and unchanged until the callback
-- returns. An empty column-name list selects DuckDB's default column names.
-- The statement and all its results close before this function returns.
withCollectionStatement :: Connection -> Query -> [(Text, DuckDBV2ColumnDataCollectionHandle, [Text])] -> (Statement -> IO a) -> IO a
withCollectionStatement conn queryText collections action = withParsedStatements conn queryText \parsed ->
  case parsed of
    [statement] -> bracket (createStatementFromParsedWithCollections conn queryText statement collections) closeStatementState action
    [] -> throwInvalidArgument "SQL query contains no statement"
    _ -> throwInvalidArgument "prepared SQL contains more than one statement"

-- | Read the parameter names in binding order. DuckDB sorts all names again
-- for each name that it returns. DuckDB rejects a mix of named and numbered
-- parameters, and numbered names are distinct and sorted. So @count@ names that
-- end with @count@ are the positions 1 to @count@. Read only the last name then.
parameterNames :: DuckDBV2SqlStatementHandle -> Int -> IO [Text]
parameterNames _ 0 = pure []
parameterNames statement count = do
  let name index = output (c_duckdb_v2_sql_statement_get_parameter_name statement (fromIntegral index)) >>= readIdentifier
      positions = map (Text.pack . show) [1 .. count]
  final <- name (count - 1)
  if final == last positions then pure positions else forM [0 .. count - 1] name

-- | Error raised when an operation targets a closed connection.
connectionClosedError :: SQLError
connectionClosedError = SQLError "duckdb-simple: connection is closed" Nothing Nothing

-- | Error raised when an operation targets a closed statement.
statementClosedError :: Statement -> SQLError
statementClosedError Statement{statementQuery} = SQLError "duckdb-simple: statement is closed" Nothing (Just statementQuery)

-- | Keep a Haskell owner reachable until an action finishes. See
-- Note [Native handle ownership].
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

{- Note [Connection lock]
~~~~~~~~~~~~~~~~~~~~~~~~~
DuckDB permits one open result for each connection, and it rejects a second
execution. DuckDB also rejects a preparation while another result is open.
Thus connectionLock serializes these native calls of a connection, and the
threads that share a connection wait for each other. withConnectionLock takes
the lock.

These calls take the lock:

- The preparation of a statement (createStatementFromParsedWithCollections).

- The metadata bind of a statement (statementSchemas).

- The native calls of one result in the result window (withResultWindow). A
  materialized result holds the lock from its execution until it closes. See
  Note [Result window].

- Each fetch of an explicit stream after the result window (fetchChunk in
  "Database.DuckDB.Simple.Result").

- The execution of an explicit Arrow stream, and each fetch of a batch
  (foldArrowWith in "Database.DuckDB.Simple.Arrow.Internal").

An explicit stream keeps its result open between two fetches. It holds the
lock for its execution and for each fetch, but not between two fetches. A
statement of another thread on the connection can then take the lock, and
DuckDB rejects that statement while the result is open.

Each fetch of an explicit stream holds the lock for this reason. The header
says that a result has a single consumer, and that a bind runs at the same time
only on a second connection. Without the lock, another thread can start a
statement when the stream is finished in DuckDB but still has buffered chunks.
A cancellation of the fetch then interrupts the query that runs on the
connection, which is the query of the other thread.

The lock is not reentrant. A thread that holds the lock must not take it again.
Thus:

- A call in the result window does not take the lock. A fetch of an explicit
  stream after the result window takes the lock, because its thread does not
  hold the lock then.

- No step function runs under the lock. A step function is a function of the
  user that a fold calls for each row or for each Arrow batch. It can run a
  statement on the same connection, and that statement takes the lock. The
  folds call a step function after the fetch, or after the result closes.

DuckDB callbacks run while a native call holds the lock. withConnectionLock
gives a bound to the wait of a thread that is inside a callback. See
Note [Callback marks] in "Database.DuckDB.Simple.CallbackMark".
-}

-- | Run an action with the lock of the connection. The lock is not reentrant.
-- Do not run a function of the user, such as the step of a fold, in the
-- action. A thread with a callback mark waits for the lock at most the bound
-- of the connection ('connectionLockBound'), and then raises an 'SQLError' of
-- the type 'ErrorResourceInUse'. Each other thread waits without a bound. See
-- Note [Connection lock], and Note [Callback marks] in
-- "Database.DuckDB.Simple.CallbackMark".
--
-- The take of the lock is masked. The timeout of the bound can thus arrive only
-- while 'takeMVar' blocks, and then the take has not occurred. After the take,
-- the same handlers as in 'withMVar' put the lock back.
withConnectionLock :: Connection -> IO a -> IO a
withConnectionLock Connection{connectionLock, connectionLockBound} action = do
  bound <- case connectionLockBound of
    Nothing -> pure Nothing
    Just micros -> do
      inside <- hasCallbackMark
      pure (if inside then Just micros else Nothing)
  case bound of
    Nothing -> withMVar connectionLock (const action)
    Just micros -> mask \restore -> do
      -- A free lock needs no timer. 'timeout' with a bound of 0 does not run
      -- its action, so the bound 0 also takes a free lock.
      taken <-
        tryTakeMVar connectionLock >>= \case
          Just () -> pure True
          Nothing -> isJust <$> timeout micros (takeMVar connectionLock)
      unless taken $ throwIO (callbackLockWaitError micros)
      result <- restore action `onException` putMVar connectionLock ()
      putMVar connectionLock ()
      pure result

-- | The error of a callback that waited for a connection lock for its bound.
callbackLockWaitError :: Int -> SQLError
callbackLockWaitError micros =
  SQLError
    ( Text.pack
        ( "duckdb-simple: a callback waited "
            <> seconds
            <> " s for a connection that runs a statement. A callback must not run a statement on the connection that runs the callback. Use another connection of the same instance, or change connectionCallbackLockWait."
        )
    )
    (Just ErrorResourceInUse)
    Nothing
  where
    (whole, fraction) = micros `quotRem` 1000000
    digits = dropWhileEnd (== '0') (let text = show fraction in replicate (6 - length text) '0' <> text)
    seconds = show whole <> (if null digits then "" else '.' : digits)

{- Note [Result window]
~~~~~~~~~~~~~~~~~~~~~~~
Each interruptible native call uses a helper thread. A bound caller, such as
the main thread, changes operating system threads to wait for one. A result
needs several native calls, for example its execution and its fetches. Thus
withResultWindow runs the native calls of one result as one unit, in one helper
thread. The caller changes threads once for the result, and not once for each
call. The result window is the time in which this helper thread runs the
action.

withResultWindow takes the connection lock, and it runs the action with
runInterruptible. The helper thread writes its ThreadId to connectionWindow
before the action, and it writes Nothing after the action. runInterruptible
compares connectionWindow with the current thread. If they are equal, it runs
the work directly. Thus each interruptible call in the action, for example in
executePreparedResult or fetchResultChunk, runs in the helper thread of the
window. The connection lock lets a connection have at most one window at a
time.

The helper thread is interruptible as one unit. A cancellation interrupts
DuckDB until the whole action returns. The caller can be cancelled after the
helper thread has made its result. The release action of withResultWindow then
releases that result. See Note [Interruptible native calls].

These functions use a result window:

- executeParsedStatement.

- executeStatement in "Database.DuckDB.Simple".

- startStatementStream and collectParsedRows in "Database.DuckDB.Simple.Result".

- foldArrowWith in "Database.DuckDB.Simple.Arrow.Internal", for a materialized
  result.

A row stream runs its execution and its first fetch in the window. Each later
fetch is outside the window: it takes the connection lock and a helper thread
of its own. An Arrow stream does not use a window. See Note [Connection lock].

The action holds the connection lock. Thus no step function runs in the action.
-}

-- | Run the native calls of one result in one helper thread, and hold the
-- connection lock. The first argument releases the result of the action when
-- the caller is cancelled after the helper thread has made that result. Do not
-- run a function of the user in the action. See Note [Result window].
withResultWindow :: Connection -> (a -> IO ()) -> IO a -> IO a
withResultWindow conn@Connection{connectionWindow} release action =
  withConnectionLock conn . runInterruptible conn release $ do
    self <- myThreadId
    bracket_ (writeIORef connectionWindow (Just self)) (writeIORef connectionWindow Nothing) action

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

-- | Attach the SQL text to errors from native calls. An error that already has
-- SQL text keeps it, so an error from a nested query reports that query.
withQueryError :: Query -> IO a -> IO a
withQueryError query action = action `catch` \err -> throwIO err{sqlErrorQuery = Just (fromMaybe query (sqlErrorQuery err))}

-- | Borrow SQL as a NUL-terminated UTF-8 string.
withQueryCString :: Query -> (ConstPtr CChar -> IO a) -> IO a
withQueryCString query@(Query text) action = do
  when (Text.any (== '\0') text) $ withQueryError query (throwInvalidArgument "query contains NUL")
  TextForeign.withCString text (action . ConstPtr)

-- | Copy a UTF-8 C string.
peekUtf8CString :: (Coercible a CString) => a -> IO Text
peekUtf8CString ptr = Text.decodeUtf8 <$> BS.packCString (coerce ptr)

{- Note [Interruptible native calls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Native work that can run for a long time runs in a helper thread, so that an
asynchronous exception can interrupt it. An asynchronous exception does not
interrupt a thread that is in a foreign call: the documentation of throwTo says
that the exception is not raised until the call has completed. Thus the caller
does not make the call itself. runInterruptible starts the helper thread, and
the caller waits for the outcome of the work. An asynchronous exception that
the caller gets during this wait is a cancellation.

On a cancellation, the caller does these steps:

- It interrupts DuckDB with duckdb_v2_connection_interrupt until the work
  returns. The header says that the interrupt is "A no-op when no query is
  active". An interrupt that arrives before the query starts is thus lost, and
  the caller repeats the interrupt.

- It waits interruptRetryDelayMicros between two interrupts, so that the loop
  is not a busy loop.

- If the work gives a result, the caller releases it with the release action
  of runInterruptible, because the caller does not return that result. If the
  work raises an error, the caller ignores that error.

- It raises the cancellation again.

These steps are uninterruptible (uninterruptibleMask_). Thus the caller
releases its inputs, such as the arrays that withBoundValues lends, only after
the native work returns. Cancellation is cooperative: the native code, and each
DuckDB callback that it runs, must return before the cleanup can finish.

runInterruptibleQuery gives no release action. Its work must keep each native
result in storage that the caller releases. For example, executeParsedResult
gives the work a slot for the result handle. If an exception occurs, the caller
destroys the handle in the slot.

A call from the helper thread of the result window runs the work directly,
without a new helper thread. See Note [Result window].
-}

-- | Run native work with cancellation and wait for it before releasing inputs.
-- The work must keep its native results in storage that the caller releases.
-- See Note [Interruptible native calls].
runInterruptibleQuery :: Connection -> IO a -> IO a
runInterruptibleQuery conn = runInterruptible conn (const (pure ()))

-- | Run native work in a helper thread. On cancellation, interrupt DuckDB
-- until the work returns, and release a result that the caller does not receive.
-- A call from the helper thread of 'withResultWindow' runs the work directly.
-- See Note [Interruptible native calls] and Note [Result window].
runInterruptible :: forall a. Connection -> (a -> IO ()) -> IO a -> IO a
runInterruptible conn release action = do
  self <- myThreadId
  window <- readIORef (connectionWindow conn)
  if window == Just self then action else interruptible
  where
    interruptible = withConnectionHandle conn \handle -> mask \restore -> do
      done <- newEmptyMVar
      _ <- forkIO do
        outcome <- try action :: IO (Either SomeException a)
        putMVar done outcome
      let wait =
            tryReadMVar done >>= \case
              Just outcome -> pure outcome
              -- Repeat the interrupt until the work returns. See
              -- Note [Interruptible native calls].
              Nothing -> void (c_duckdb_v2_connection_interrupt handle nullPtr) >> threadDelay interruptRetryDelayMicros >> wait
          stop = uninterruptibleMask_ (wait >>= either (const (pure ())) release)
      outcome <- restore (readMVar done) `onException` stop
      either throwIO pure outcome

-- | Repeat the interrupt at most 100 times each second while the native work
-- finishes. The interval prevents a busy loop. It can add up to one interval to
-- the time at which the caller sees the end of the work. See
-- Note [Interruptible native calls].
interruptRetryDelayMicros :: Int
interruptRetryDelayMicros = 10 * 1000

-- | Execute a borrowed parsed statement. The caller owns the returned cursor.
-- See Note [Interruptible native calls].
executeParsedResult :: Connection -> Query -> DuckDBV2SqlStatementHandle -> IO DuckDBV2ResultHandle
executeParsedResult conn query parsed = withQueryError query $ withConnectionHandle conn \raw -> mask_ $ alloca \slot -> do
  poke slot nullHandle
  flip onException (peek slot >>= destroy c_duckdb_v2_result_destroy) do
    runInterruptibleQuery conn $
      checked (c_duckdb_v2_statement_execute raw parsed (ConstPtr nullPtr) (ConstPtr nullPtr) 0 slot)
    peek slot

-- | Execute a parsed statement directly and drain all its side effects. See
-- Note [Result window].
executeParsedStatement :: Connection -> Query -> DuckDBV2SqlStatementHandle -> IO Int
executeParsedStatement conn queryText parsed = withQueryError queryText . withResultWindow conn (const (pure ())) $
  bracket (executeParsedResult conn queryText parsed) (destroy c_duckdb_v2_result_destroy) \result ->
    fromIntegral <$> runInterruptibleQuery conn (output (c_duckdb_v2_result_drain result))

-- | Execute a prepared statement. The caller owns the returned result. See
-- Note [Interruptible native calls].
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
  let names = map (\(idx, _) -> statementParameterNames stmt ! idx) bindings
      values = map snd bindings
  withIdentifierArray names \namesPtr -> withArray values \valuesPtr ->
    action (ConstPtr namesPtr) (ConstPtr valuesPtr) (fromIntegral (length bindings))

-- | Execute a prepared Arrow result and register it for statement cleanup.
-- Each array of the result has at most 'connectionArrowBatch' rows of the
-- connection. See Note [Interruptible native calls].
executePreparedArrowResult :: Statement -> IO DuckDBV2ArrowResultHandle
executePreparedArrowResult stmt = withStatementHandle stmt \handle -> mask_ $ alloca \slot -> do
  poke slot nullHandle
  flip onException (peek slot >>= destroy c_duckdb_v2_arrow_result_destroy) $
    withBoundValues stmt \names values count -> do
      let batch = fromIntegral (connectionArrowBatch (statementConnection stmt))
      runInterruptibleQuery (statementConnection stmt) $
        checked (c_duckdb_v2_prepared_statement_execute_arrow handle names values count batch slot)
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

-- | Fetch one owned chunk. A null handle denotes completion. See
-- Note [Interruptible native calls].
fetchResultChunk :: Connection -> DuckDBV2ResultHandle -> IO DuckDBV2DataChunkHandle
fetchResultChunk conn result = mask_ $ alloca \slot -> do
  poke slot nullHandle
  flip onException (peek slot >>= destroyDataChunk) do
    runInterruptibleQuery conn (checked (c_duckdb_v2_result_fetch_chunk result slot))
    peek slot

-- | Release an owned native data chunk.
destroyDataChunk :: DuckDBV2DataChunkHandle -> IO ()
destroyDataChunk = destroy c_duckdb_v2_data_chunk_destroy

-- | Release the active cursor and return to the idle state. See
-- Note [Statement cursor].
cleanupStatementStreamRef :: IORef StatementStreamState -> IO ()
cleanupStatementStreamRef ref = mask_ do
  state <- atomicModifyIORef' ref (StatementStreamIdle,)
  case state of
    StatementStreamActive stream -> finalizeStream stream
    _ -> pure ()

-- | Release the native state of a stream in this order: the current chunk, the
-- pending chunks, and the result. Each release runs also when an earlier
-- release raises an error.
finalizeStream :: StatementStream -> IO ()
finalizeStream StatementStream{statementStreamResult, statementStreamChunk, statementStreamPendingChunks} =
  maybe (pure ()) finalizeChunk statementStreamChunk
    `finally` (releaseItems destroyDataChunk statementStreamPendingChunks `finally` destroy c_duckdb_v2_result_destroy statementStreamResult)

-- | Release the native chunk of a stream chunk.
finalizeChunk :: StatementStreamChunk -> IO ()
finalizeChunk StatementStreamChunk{statementStreamChunkPtr} = destroyDataChunk statementStreamChunkPtr

-- | Transfer ownership of one successfully constructed parameter value.
storeBoundValue :: Statement -> DuckDBIdx -> DuckDBV2ValueHandle -> IO ()
storeBoundValue stmt index value = withStatementHandle stmt \_ -> mask_ do
  let idx = fromIntegral index
  unless (inRange (bounds (statementParameterNames stmt)) idx) $
    withQueryError (statementQuery stmt) (throwInvalidArgument "parameter index is out of bounds")
  previous <- IntMap.lookup idx <$> readIORef (statementBindings stmt)
  mapM_ destroyValue previous
  atomicModifyIORef' (statementBindings stmt) \values -> (IntMap.insert idx value values, ())

-- | Release all owned parameter values.
clearBoundValues :: IORef (IntMap.IntMap DuckDBV2ValueHandle) -> IO ()
clearBoundValues ref = atomicModifyIORef' ref (IntMap.empty,) >>= releaseItems destroyValue . IntMap.elems

{- Note [Statement schemas]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
An open statement keeps a cache of two schemas (statementSchemaCache): the
schema of its result and the schema of its parameters. statementSchemas fills
the cache on its first call. It runs the metadata bind
(duckdb_v2_statement_bind) on the parsed copy of the statement under the
connection lock, and it stores both handles. The next calls give the same
handles until the cache is empty again. columnCount and columnName in
"Database.DuckDB.Simple" read the result schema, and preparedParameterType
reads the type of one parameter.

clearStatementSchemas releases both handles and empties the cache.
resetStatementStream in "Database.DuckDB.Simple.Result" calls it. Thus each
call of bind, bindNamed, clearStatementBindings, and executeStatement clears
the cache first. Each fold of "Database.DuckDB.Simple" clears it before its
first row and after its last row. closeStatementState releases the handles
when the statement closes.

- Why each bind clears the cache. A statement that the caller keeps must see a
  change of the catalog between two binds, for example an ALTER TABLE that
  changes the type of a column. A binding that depends on the type of its
  parameter then builds its value for the new type. With a type from before the
  change, a UTCTime for a column that became TIMESTAMPTZ_NS loses its
  nanoseconds.

- The cost. The first read of a parameter type after a bind runs the binder of
  DuckDB again. Only a binding that reads the type of its parameter has this
  cost: a TimeOfDay, a LocalTime, or a UTCTime (or an Unbounded form) with
  digits below a microsecond (see Note [Nanosecond parameters] in
  "Database.DuckDB.Simple.ToField"), an Array, a list, a Map, a manual STRUCT,
  TUPLE, or UNION value, a FieldList, a FieldArray, or a FieldMap value, and an
  Integer or a Natural outside the HUGEINT range
  (see Note [Integer parameters] in "Database.DuckDB.Simple.ToField").

- The exception is executeMany. One executeMany call binds many rows on one
  statement that no caller can reach. withKeptParameterTypes keeps the decoded
  type of each parameter (statementParameterTypes) from the first row that
  reads it to the end of the call. Thus the binder runs one time for the call,
  and not one time for each row. The schemas are still cleared for each row.
  If the type of a column changes during the call, the later rows of the call
  bind for the old type. Another connection can make such a change, and also
  another thread that uses the same connection.
-}

-- | Bind SQL metadata and borrow the cached output and parameter schemas.
-- The handles remain valid until the statement resets, rebinds, or closes. See
-- Note [Statement schemas].
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
            withConnectionLock
              (statementConnection stmt)
              (runInterruptibleQuery (statementConnection stmt) (checked (c_duckdb_v2_statement_bind conn statementParsed result parameters)))
            schemas <- (,) <$> peek result <*> peek parameters
            writeIORef statementSchemaCache (Just schemas)
            pure schemas

-- | Read a parameter type when the binder can determine it without values.
-- While 'withKeptParameterTypes' runs, read each type one time. See
-- Note [Statement schemas].
preparedParameterType :: Statement -> DuckDBIdx -> IO (Maybe LogicalTypeRep)
preparedParameterType stmt idx = do
  kept <- readIORef (statementParameterTypes stmt)
  case kept >>= IntMap.lookup (fromIntegral idx) of
    Just rep -> pure rep
    Nothing -> do
      rep <- readParameterType
      -- Outside 'withKeptParameterTypes' the value is Nothing, and the insert does nothing.
      modifyIORef' (statementParameterTypes stmt) (fmap (IntMap.insert (fromIntegral idx) rep))
      pure rep
  where
    readParameterType = do
      (_, schema) <- statementSchemas stmt
      alloca \name -> do
        dtype <- output (c_duckdb_v2_schema_get_field schema (idx - 1) name)
        ident <- output (c_duckdb_v2_logical_type_get_id dtype)
        if ident == DuckDBV2LogicalTypeIdUnknown || ident == DuckDBV2LogicalTypeIdSQLNull
          then pure Nothing
          else Just <$> logicalTypeToRep dtype

-- | Column type names and the logical types that they denote. When a type has
-- more than one name, 'duckDBTypeToName' gives the first name.
duckDBTypeNames :: [(Text, LogicalTypeId)]
duckDBTypeNames =
  [ ("BOOLEAN", LogicalTypeIdBoolean)
  , ("TINYINT", LogicalTypeIdTinyInt)
  , ("SMALLINT", LogicalTypeIdSmallInt)
  , ("INTEGER", LogicalTypeIdInteger)
  , ("BIGINT", LogicalTypeIdBigInt)
  , ("HUGEINT", LogicalTypeIdHugeInt)
  , ("UTINYINT", LogicalTypeIdUTinyInt)
  , ("USMALLINT", LogicalTypeIdUSmallInt)
  , ("UINTEGER", LogicalTypeIdUInteger)
  , ("UBIGINT", LogicalTypeIdUBigInt)
  , ("UHUGEINT", LogicalTypeIdUHugeInt)
  , ("FLOAT", LogicalTypeIdFloat)
  , ("DOUBLE", LogicalTypeIdDouble)
  , ("DATE", LogicalTypeIdDate)
  , ("TIME", LogicalTypeIdTime)
  , ("TIMETZ", LogicalTypeIdTimeTz)
  , ("TIMESTAMP", LogicalTypeIdTimestamp)
  , ("TIMESTAMPTZ", LogicalTypeIdTimestampTz)
  , ("TIMESTAMPTZ_NS", LogicalTypeIdTimestampTzNs)
  , ("TIMESTAMP_NS WITH TIME ZONE", LogicalTypeIdTimestampTzNs)
  , ("INTERVAL", LogicalTypeIdInterval)
  , ("VARCHAR", LogicalTypeIdVarchar)
  , ("TEXT", LogicalTypeIdVarchar)
  , ("BLOB", LogicalTypeIdBlob)
  , ("GEOMETRY", LogicalTypeIdGeometry)
  , ("VARIANT", LogicalTypeIdVariant)
  , ("UUID", LogicalTypeIdUUID)
  , ("BIT", LogicalTypeIdBit)
  , ("BIGNUM", LogicalTypeIdBigNum)
  , -- NULL gives an element type to Maybe values without data.
    ("NULL", LogicalTypeIdSQLNull)
  ]

-- | Find the type that a column type name denotes.
duckDBTypeFromName :: Text -> Maybe LogicalTypeId
duckDBTypeFromName name = lookup name duckDBTypeNames

-- | Find the column type name of a type. Other types use their 'Show' text.
duckDBTypeToName :: LogicalTypeId -> Text
duckDBTypeToName dtype =
  maybe (Text.pack (show dtype)) fst (find ((== dtype) . snd) duckDBTypeNames)
