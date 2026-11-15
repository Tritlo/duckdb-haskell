{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Database.DuckDB.Simple.Callback
-- Description : Manage callback resources and registration builders at the DuckDB ownership boundary.
module Database.DuckDB.Simple.Callback (
  RegistrationTarget (..),
  withRegistrationFactory,
  targetCastFactory,
  targetConfig,
  withTarget,
  Allocator,
  install,
  installOptional,
  installAll,
  guardContext,
  withCallbackResources,
  withCallbackResourcesFinally,
  transferCallbackState,
  readCallbackState,
  runCallback,
  runCallbackWith,
  ignoreCallbackExceptions,
  attachOpaque,
) where

import Control.Concurrent (rtsSupportsBoundThreads)
import Control.Exception (SomeException, bracket, catch, displayException, finally, fromException, mask, mask_, onException, try)
import Control.Monad (unless, when)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import Database.DuckDB.Simple.CallbackMark (withCallbackMark)
import Database.DuckDB.Simple.Internal (Connection, withConnectionHandle, withTypeCache)
import Database.DuckDB.Simple.Native
import Database.DuckDB.Simple.TypeCache (typeCacheCasts)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (FunPtr, Ptr, castFunPtr, castPtr, freeHaskellFunPtr, nullFunPtr, nullPtr)
import Foreign.StablePtr (StablePtr, castPtrToStablePtr, castStablePtrToPtr, deRefStablePtr, freeStablePtr, newStablePtr)
import Foreign.Storable (Storable, peek)
import HsBindgen.Runtime.Support.FunPtr (ToFunPtr, toFunPtr)
import System.IO.Error (ioeGetErrorString, isUserError)

-- | A registration target. Extension handles are borrowed from an extension load
-- callback and must not escape it. Configure extension types and values with
-- the context supplied to that callback.
data RegistrationTarget = ConnectionTarget Connection | ExtensionTarget DuckDBV2ExtensionHandle DuckDBV2ContextHandle

-- | Borrow the factory of a registration target. The factory must not escape
-- this action. Use it to construct signature types and values in a configuration action.
withRegistrationFactory :: RegistrationTarget -> (DuckDBV2FactoryHandle -> IO a) -> IO a
withRegistrationFactory target action = case target of
  ConnectionTarget connection -> withConnectionHandle connection $ \native -> withConnectionFactory native action
  ExtensionTarget _ context -> withContextFactory context action

-- | The factory for the casts that the library runs in the callbacks of a
-- target. A connection target gives the factory of its instance. An extension
-- target gives 'Nothing', because an extension has no instance. A callback with
-- a context then reads GEOMETRY with the factory of the context. A registration
-- takes the factory one time, and its callbacks keep it. The factory is valid
-- until the instance handle is destroyed. The library destroys that handle only
-- after it closes each child of the instance (Note [Native handle ownership] in
-- "Database.DuckDB.Simple.Internal"), and a callback runs only in a statement on
-- a connection of the instance. A callback keeps the factory and not the connection, for the
-- reason that withVariantReader in "Database.DuckDB.Simple.Copy.Internal" gives.
-- See Note [User casts and the casts of the library] in
-- "Database.DuckDB.Simple.Materialize".
targetCastFactory :: RegistrationTarget -> IO (Maybe DuckDBV2FactoryHandle)
targetCastFactory target = case target of
  ConnectionTarget connection -> Just <$> withTypeCache connection (pure . typeCacheCasts)
  ExtensionTarget _ _ -> pure Nothing

-- | Borrow the config of a registration target: the config of the connection,
-- or the config of the context of an extension. The handle is valid while the
-- target is.
targetConfig :: RegistrationTarget -> IO DuckDBV2ConfigHandle
targetConfig target = case target of
  ConnectionTarget connection -> withConnectionHandle connection (output . c_duckdb_v2_connection_get_config)
  ExtensionTarget _ context -> output (c_duckdb_v2_context_get_config context)

-- | Keep the target alive while borrowing its factory and registration builder.
withTarget
  :: (Storable handle)
  => RegistrationTarget
  -> (DuckDBV2FactoryHandle -> Ptr handle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error)
  -> (Ptr handle -> IO DuckDBV2Error)
  -> (DuckDBV2ConnectionHandle -> handle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error)
  -> (DuckDBV2ExtensionHandle -> handle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error)
  -> (handle -> IO () -> IO a)
  -> IO a
withTarget target create release registerConnection registerExtension action = case target of
  ConnectionTarget connection -> withConnectionHandle connection $ \native ->
    withConnectionFactory native $ \factory ->
      bracket (output (create factory)) (destroy release) $ \handle -> action handle (checked (registerConnection native handle))
  ExtensionTarget extension context -> withContextFactory context $ \factory ->
    bracket (output (create factory)) (destroy release) $ \handle -> action handle (checked (registerExtension extension handle))

-- | Allocate a callback pointer tracked by the native registration owner.
type Allocator = forall a. IO (FunPtr a) -> IO (FunPtr a)

-- | Allocate one generated callback and defer its checked setter.
install
  :: (ToFunPtr auxiliary)
  => Allocator -> (FunPtr auxiliary -> callback) -> auxiliary -> (callback -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error) -> IO (IO ())
install allocate wrap callback setter = do
  pointer <- wrap <$> allocate (toFunPtr callback)
  pure (checked (setter pointer))

-- | Allocate an optional callback when it is present.
installOptional :: Maybe a -> (a -> IO (IO ())) -> IO (IO ())
installOptional value action = maybe (pure (pure ())) action value

-- | Run each install action now, and return one action that runs their setters
-- in order. A registration has two phases. First the install actions allocate
-- each function pointer, in the acquire step of 'withCallbackResources', so
-- that its cleanup frees each pointer. Then the setters run after the
-- configuration action of the registration, so a callback of the library
-- replaces a callback that the configuration action set.
installAll :: [IO (IO ())] -> IO (IO ())
installAll installers = sequence_ <$> sequence installers

-- | Translate a callback exception through its native error channel.
guardContext :: (info -> DuckDBV2ContextHandle -> IO ()) -> info -> DuckDBV2ContextHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()
guardContext callback info context err = runCallback err (callback info context)

{- Note [Callback ownership]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The library gives two kinds of Haskell values to DuckDB. DuckDB holds each
value as a stable pointer with a destructor, in an opaque structure
(attachOpaque). After a setter takes the pointer, DuckDB owns it, and only the
destructor frees it.

- Callback resources. withCallbackResourcesFinally collects the cleanup of the
  function pointers of one registration, and its final action, in one IO
  action. It gives a stable pointer to this action to the user data of the
  registered object, with the destructor
  duckdb_simple_release_callback_resources. This destructor runs the action and
  frees the stable pointer.

- Callback state. transferCallbackState gives a stable pointer to one value to
  a state slot, for example the bind data, the init data, or the global state.
  The destructor duckdb_simple_release_callback_state frees only the stable
  pointer. readCallbackState reads the slot and dereferences the pointer. The
  caller supplies the type, so it must be the type of the value in the slot.

These two rules are the behaviour of DuckDB 2.0.0. The header does not state
them.

- A setter that fails has not taken the pointer. Thus the handlers of
  withCallbackResourcesFinally and transferCallbackState free it.

- A setter for init data, batch data, global state, or local state does not
  release a value that it replaces. Thus give one value to each slot.

The two destructors are foreign exports, and DuckDB gets their static
addresses. A static address needs no release. A function pointer of a wrapper
import must be freed with freeHaskellFunPtr after its last call. The destructor
is the last call for its object, so no later call could free such a pointer.
The destructors mask asynchronous exceptions (mask_). They have no error
channel, so they contain each exception (ignoreCallbackExceptions). Other
modules attach destructors of their own with attachOpaque, for example
duckdb_simple_release_aggregate_states in
"Database.DuckDB.Simple.Typed.Aggregate" and duckdb_simple_release_copy_binding
in "Database.DuckDB.Simple.Copy.Internal".
-}

-- | Acquire callbacks and transfer their cleanup to a DuckDB object.
-- The object must be non-null. Its destructor must run after the action. The
-- first argument reads the config of the database that runs the callbacks. See
-- Note [Callback ownership] and Note [Callbacks without the threaded runtime].
withCallbackResources
  :: IO DuckDBV2ConfigHandle
  -> (Allocator -> IO r)
  -> (Ptr Void -> DuckDBV2OpaqueDestroyFn -> IO ())
  -> (r -> IO b)
  -> IO b
withCallbackResources config = withCallbackResourcesFinally config (pure ())

-- | Run a final action before the callback resources are released.
-- This includes acquisition and registration failure. Contain final-action errors.
-- A registration that acquires a callback checks the runtime before it
-- registers. See Note [Callbacks without the threaded runtime].
withCallbackResourcesFinally
  :: IO DuckDBV2ConfigHandle
  -> IO ()
  -> (Allocator -> IO r)
  -> (Ptr Void -> DuckDBV2OpaqueDestroyFn -> IO ())
  -> (r -> IO b)
  -> IO b
withCallbackResourcesFinally config finalizer acquire attach action = mask \restore -> do
  cleanups <- newIORef []
  let cleanup = ignoreCallbackExceptions finalizer `finally` (readIORef cleanups >>= sequence_)
      allocate make = do
        ptr <- make
        modifyIORef' cleanups (freeHaskellFunPtr ptr :)
          `onException` freeHaskellFunPtr ptr
        pure ptr
  resources <- acquire allocate `onException` cleanup
  acquired <- readIORef cleanups
  unless (null acquired) (requireThreadedRuntime config) `onException` cleanup
  stable <- newStablePtr cleanup `onException` cleanup
  -- A setter that fails has not taken the pointer, so this handler frees it.
  -- See Note [Callback ownership].
  attach (castPtr (castStablePtrToPtr stable)) callbackResourcesDestructor
    `onException` (freeStablePtr stable >> cleanup)
  restore (action resources)

{- Note [Callbacks without the threaded runtime]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
DuckDB calls a callback from its own threads. The GHC runtime without -threaded
cannot take a call from a foreign thread. The GHC User's Guide says: "In order
to use the FFI in a multi-threaded setting, you must use the -threaded option."
A program without -threaded crashed in 20 of 20 runs when a scalar function ran
over 2,000,000 rows with the setting threads = 4 ("schedule: re-entered
unsafely", a segmentation fault, or a heap corruption). With threads = 1, or
with -threaded, it crashed in none of 20 runs.

Thus each registration that acquires a callback calls requireThreadedRuntime
before it registers (withCallbackResourcesFinally). With the threaded runtime
the check is one Bool test and no native call. Without it, the check reads the
setting threads of the database that runs the callbacks (four native calls: the
config, the option value, its BIGINT, and the release of the value) and raises
the input error of the library when the setting is above 1. A registration with
no callback, for example a custom type, a macro, or a custom option with no
update function, does not check. The setting threads has only the GLOBAL scope
("cannot be set locally" in DuckDB 2.0.0), so each connection of an instance and
the instance itself give the same value.

The check cannot see a later change. A statement SET threads = 4 after the
registration, also on another connection of the instance, raises the count
again, and the library does not read the SQL text of a statement. The check is
a guard for the common mistake, not a proof that no callback runs on a second
thread.
-}

-- | Raise the input error of the library when the GHC runtime is not threaded
-- and the database that the config belongs to runs more than one thread. See
-- Note [Callbacks without the threaded runtime].
requireThreadedRuntime :: IO DuckDBV2ConfigHandle -> IO ()
requireThreadedRuntime config = unless rtsSupportsBoundThreads do
  handle <- config
  threads <- withIdentifier (Text.pack "threads") \key -> alloca \scope ->
    bracket
      (output (\value -> c_duckdb_v2_config_get_option_value handle key value scope))
      (destroy c_duckdb_v2_value_destroy)
      (output . c_duckdb_v2_value_get_bigint)
  when (threads > 1) $
    throwInvalidArgument
      ( Text.pack
          ( "this program is not linked with -threaded, and the database runs "
              <> show threads
              <> " threads: link the program with -threaded, or set the DuckDB setting threads to 1 before you register a callback"
          )
      )

-- | Transfer one state value to a DuckDB callback state slot.
-- Use each slot once. See Note [Callback ownership].
transferCallbackState :: (Ptr Void -> DuckDBV2OpaqueDestroyFn -> IO ()) -> a -> IO ()
transferCallbackState attach state = mask_ do
  stable <- newStablePtr state
  -- A setter that fails has not taken the pointer, so this handler frees it.
  -- See Note [Callback ownership].
  attach (castPtr (castStablePtrToPtr stable)) callbackStateDestructor
    `onException` freeStablePtr stable

-- | Read state stored with 'transferCallbackState'. The caller supplies its type.
-- See Note [Callback ownership].
readCallbackState :: (Ptr (Ptr Void) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error) -> IO a
readCallbackState getter = do
  ptr <- output getter
  when (ptr == nullPtr) $ throwInternalError (Text.pack "callback state is not initialized")
  deRefStablePtr (castPtrToStablePtr (castPtr ptr))

-- | Convert callback exceptions to native V2 diagnostics. Mark the thread as a
-- thread inside a DuckDB callback for the time of the action, so that its
-- waits for a connection lock have the bound of the connection. See
-- 'Database.DuckDB.Simple.Internal.withConnectionLock' and
-- Note [Callback marks] in "Database.DuckDB.Simple.CallbackMark". Each DuckDB
-- callback with user code runs through this function or 'runCallbackWith',
-- except the flush function of a log sink when DuckDB destroys the sink. See
-- Note [Errors that leave a callback] for the code and the text of an error.
runCallback :: Ptr DuckDBV2ErrorInfoHandle -> IO () -> IO ()
runCallback = runCallbackAs callbackError

-- | 'runCallback' with another native error code for each failure. The code
-- replaces the type of each exception, also the type of an 'SQLError', and the
-- text is the 'failureText' of the exception. A cast callback uses it. See
-- Note [The error code of a failed cast] in
-- "Database.DuckDB.Simple.V2.Registration".
runCallbackWith :: DuckDBV2Error -> Ptr DuckDBV2ErrorInfoHandle -> IO () -> IO ()
runCallbackWith code = runCallbackAs \failure -> (code, Text.pack (failureText failure))

-- | Run a callback action. For an exception, write the code and the text that
-- the first argument gives into the error slot.
runCallbackAs :: (SomeException -> (DuckDBV2Error, Text)) -> Ptr DuckDBV2ErrorInfoHandle -> IO () -> IO ()
runCallbackAs describe err action = mask \restore -> do
  outcome <- try (withCallbackMark (restore action))
  case outcome of
    Right () -> pure ()
    Left (failure :: SomeException) ->
      ignoreCallbackExceptions $
        if err == nullPtr
          then pure ()
          else do
            info <- peek err
            if info == nullHandle
              then pure ()
              else do
                let (code, text) = describe failure
                c_duckdb_v2_error_info_set_code info code >>= checkCode
                withText (Text.pack "Haskell callback failed") $ \message ->
                  c_duckdb_v2_error_info_set_text info message >>= checkCode
                withText text $ \message ->
                  c_duckdb_v2_error_info_set_text info message >>= checkCode

{- Note [Errors that leave a callback]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
runCallback writes an exception of a callback into the error slot of DuckDB as
a code and a text. The statement that runs the callback then fails with an
error of the type of the code. DuckDB puts the prefix of that type before the
text, for example "Conversion Error: " for the code of ErrorTypeConversion, and
"Invalid Input Error: " for DuckDBV2ErrorApi. The section "Errors" of the README
lists the types that do not pass for users.

- An SQLError with a type writes the code of its type. The text is the body of
  the error (sqlErrorBody), the text of DuckDB without the prefix. An error of
  duckdb-simple has no body, so the text is its message. Thus the error of a
  statement in the callback reaches the outer statement with its type and with
  one prefix. An error that passes through two callbacks keeps its type and
  its body in the same way.
- DuckDB 2.0.0 cannot raise some codes again with their own type. Six codes
  have no exception type in the map of the C API (TryGetExceptionTypeFromErrorCode
  in capi_v2.cpp): the codes of ErrorApi, ErrorIOFileNotFound,
  ErrorIOReadFailure, ErrorIOEof, ErrorInputParameterInvalid, and
  ErrorInputMetadataDeferred. DuckDB raises them as Invalid Input, so the outer
  statement fails with ErrorInputInvalid and "Invalid Input Error: ".
- ErrorIONetwork does not pass. Its exception type has no name in DuckDB 2.0.0
  (EXCEPTION_MAP in exception.cpp), so with its code the outer statement fails
  with ErrorApi and the prefix "Invalid Error: " of a type with no name. Such
  an error writes the code DuckDBV2ErrorApi and its message, so the outer
  statement fails with ErrorInputInvalid, as for an SQLError without a type.
- Seven types do not pass, because DuckDB expects them before the execution
  of a statement: ErrorQueryBinder, ErrorDatabaseCatalog,
  ErrorResourceConnection, ErrorQueryParameterNotAllowed, ErrorQueryParser,
  ErrorConfigurationPermission, and ErrorResourceInUse. Under the DuckDB
  setting SYNTACTIC_ERRORS_DO_NOT_INVALIDATE of
  default_transaction_invalidation_policy, DuckDB keeps the transaction after
  an error of these types (Exception::InvalidatesTransaction in exception.cpp).
  A callback raises during the execution, for example after an INSERT has
  written rows. With the code of such a type, these rows would stay in the
  transaction, and COMMIT would store them. Thus such an error writes the code
  DuckDBV2ErrorApi and its message. DuckDB then invalidates the explicit
  transaction, and COMMIT stores none of its rows.
- ErrorRuntimeInternal and ErrorRuntimeFatal do not pass. For these codes,
  DuckDB 2.0.0 invalidates the database of the outer statement, also when the
  error came from another database. Such an error writes the code
  DuckDBV2ErrorApi and its message.
- Each other exception writes the code DuckDBV2ErrorApi and failureText: an
  SQLError without a type, a userError, or another exception.
- A cast callback runs through runCallbackWith and not through this rule. It
  writes the code DuckDBV2ErrorTypeMismatch and failureText for each
  exception, also for an SQLError with a type. See
  Note [The error code of a failed cast] in
  "Database.DuckDB.Simple.V2.Registration".
-}

-- | The code and the text that a callback writes into its error slot for an
-- exception. See Note [Errors that leave a callback].
callbackError :: SomeException -> (DuckDBV2Error, Text)
callbackError failure = case fromException failure of
  Just SQLError{sqlErrorType = Just errorType, sqlErrorMessage = message, sqlErrorBody = body}
    | errorType `notElem` heldBack
    , Just code <- lookup errorType errorTypeCodes ->
        (code, fromMaybe message body)
  _ -> (DuckDBV2ErrorApi, Text.pack (failureText failure))
  where
    -- See Note [Errors that leave a callback].
    heldBack =
      [ ErrorQueryBinder
      , ErrorDatabaseCatalog
      , ErrorResourceConnection
      , ErrorQueryParameterNotAllowed
      , ErrorQueryParser
      , ErrorConfigurationPermission
      , ErrorResourceInUse
      , ErrorIONetwork
      , ErrorRuntimeInternal
      , ErrorRuntimeFatal
      ]

-- | The text of a callback exception in the SQL error. An 'SQLError' gives its
-- message, and a 'userError' gives its text. Thus the SQL error does not show a
-- Haskell record or the words \"user error\". Each other exception gives its
-- 'displayException' text.
failureText :: SomeException -> String
failureText failure
  | Just sqlError <- fromException failure = Text.unpack (sqlErrorMessage sqlError)
  | Just ioError' <- fromException failure, isUserError ioError' = ioeGetErrorString ioError'
  | otherwise = displayException failure

-- | Transfer an opaque resource to a checked V2 setter. See
-- Note [Callback ownership].
attachOpaque
  :: (Ptr DuckDBV2Opaque -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error)
  -> Ptr Void
  -> DuckDBV2OpaqueDestroyFn
  -> IO ()
attachOpaque setter ptr release =
  with (DuckDBV2Opaque ptr release (DuckDBV2OpaqueEqualsFn nullFunPtr)) $ \opaque ->
    checked (setter opaque)

-- | Contain exceptions in callbacks which have no error channel.
ignoreCallbackExceptions :: IO () -> IO ()
ignoreCallbackExceptions action = mask \restore ->
  restore action `catch` \(_ :: SomeException) -> pure ()

-- | Release object-owned callbacks through a static C entry point. See
-- Note [Callback ownership].
releaseCallbackResources :: Ptr Void -> IO ()
releaseCallbackResources raw =
  mask_
    $ ignoreCallbackExceptions
    $ if raw == nullPtr
      then pure ()
      else do
        let stable = castPtrToStablePtr (castPtr raw) :: StablePtr (IO ())
        (deRefStablePtr stable >>= id) `finally` freeStablePtr stable

-- | Release callback state independently of the registered function. See
-- Note [Callback ownership].
releaseCallbackState :: Ptr Void -> IO ()
releaseCallbackState raw =
  mask_
    $ ignoreCallbackExceptions
    $ if raw == nullPtr then pure () else freeStablePtr (castPtrToStablePtr (castPtr raw))

foreign export ccall "duckdb_simple_release_callback_resources"
  releaseCallbackResources :: Ptr Void -> IO ()

foreign import ccall "&duckdb_simple_release_callback_resources"
  callbackResourcesDestructorAddress :: FunPtr (Ptr Void -> IO ())

foreign export ccall "duckdb_simple_release_callback_state"
  releaseCallbackState :: Ptr Void -> IO ()

foreign import ccall "&duckdb_simple_release_callback_state"
  callbackStateDestructorAddress :: FunPtr (Ptr Void -> IO ())

-- | Use the generated callback type for the static destructor address.
callbackResourcesDestructor :: DuckDBV2OpaqueDestroyFn
callbackResourcesDestructor = DuckDBV2OpaqueDestroyFn (castFunPtr callbackResourcesDestructorAddress)

-- | Use the generated callback type for the static state destructor address.
callbackStateDestructor :: DuckDBV2OpaqueDestroyFn
callbackStateDestructor = DuckDBV2OpaqueDestroyFn (castFunPtr callbackStateDestructorAddress)
