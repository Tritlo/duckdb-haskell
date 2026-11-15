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
  ignoreCallbackExceptions,
  attachOpaque,
) where

import Control.Exception (SomeException, bracket, catch, displayException, finally, fromException, mask, mask_, onException, throwIO, try)
import Control.Monad (when)
import Data.IORef (modifyIORef', newIORef, readIORef)
import qualified Data.Text as Text
import Data.Void (Void)
import Database.DuckDB.Simple.CallbackMark (withCallbackMark)
import Database.DuckDB.Simple.Internal (Connection, withConnectionHandle, withTypeCache)
import Database.DuckDB.Simple.Native
import Database.DuckDB.Simple.TypeCache (typeCacheCasts)
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
-- The object must be non-null. Its destructor must run after the action. See
-- Note [Callback ownership].
withCallbackResources
  :: (Allocator -> IO r)
  -> (Ptr Void -> DuckDBV2OpaqueDestroyFn -> IO ())
  -> (r -> IO b)
  -> IO b
withCallbackResources = withCallbackResourcesFinally (pure ())

-- | Run a final action before the callback resources are released.
-- This includes acquisition and registration failure. Contain final-action errors.
withCallbackResourcesFinally
  :: IO ()
  -> (Allocator -> IO r)
  -> (Ptr Void -> DuckDBV2OpaqueDestroyFn -> IO ())
  -> (r -> IO b)
  -> IO b
withCallbackResourcesFinally finalizer acquire attach action = mask \restore -> do
  cleanups <- newIORef []
  let cleanup = ignoreCallbackExceptions finalizer `finally` (readIORef cleanups >>= sequence_)
      allocate make = do
        ptr <- make
        modifyIORef' cleanups (freeHaskellFunPtr ptr :)
          `onException` freeHaskellFunPtr ptr
        pure ptr
  resources <- acquire allocate `onException` cleanup
  stable <- newStablePtr cleanup `onException` cleanup
  -- A setter that fails has not taken the pointer, so this handler frees it.
  -- See Note [Callback ownership].
  attach (castPtr (castStablePtrToPtr stable)) callbackResourcesDestructor
    `onException` (freeStablePtr stable >> cleanup)
  restore (action resources)

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
  when (ptr == nullPtr) $ throwIO (SQLError (Text.pack "duckdb-simple: callback state is not initialized") Nothing Nothing)
  deRefStablePtr (castPtrToStablePtr (castPtr ptr))

-- | Convert callback exceptions to native V2 diagnostics. Mark the thread as a
-- thread inside a DuckDB callback for the time of the action, so that its
-- waits for a connection lock have the bound of the connection. See
-- 'Database.DuckDB.Simple.Internal.withConnectionLock' and
-- Note [Callback marks] in "Database.DuckDB.Simple.CallbackMark". Each DuckDB
-- callback with user code runs through this function, except the flush function
-- of a log sink when DuckDB destroys the sink.
runCallback :: Ptr DuckDBV2ErrorInfoHandle -> IO () -> IO ()
runCallback err action = mask \restore -> do
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
                c_duckdb_v2_error_info_set_code info DuckDBV2ErrorApi >>= checkCode
                withText (Text.pack "Haskell callback failed") $ \message ->
                  c_duckdb_v2_error_info_set_text info message >>= checkCode
                withText (Text.pack (failureText failure)) $ \message ->
                  c_duckdb_v2_error_info_set_text info message >>= checkCode

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
