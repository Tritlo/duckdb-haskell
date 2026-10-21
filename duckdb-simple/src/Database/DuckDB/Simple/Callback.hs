{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Manage callback resources at the DuckDB ownership boundary.
module Database.DuckDB.Simple.Callback (
    withCallbackResources,
    withCallbackResourcesFinally,
    transferCallbackState,
    runCallback,
    ignoreCallbackExceptions,
    attachOpaque,
) where

import Control.Exception (SomeException, catch, displayException, finally, mask, mask_, onException, try)
import Data.IORef (modifyIORef', newIORef, readIORef)
import qualified Data.Text as Text
import Data.Void (Void)
import Database.DuckDB.Simple.Native
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (FunPtr, Ptr, castFunPtr, castPtr, freeHaskellFunPtr, nullFunPtr, nullPtr)
import Foreign.StablePtr (StablePtr, castPtrToStablePtr, castStablePtrToPtr, deRefStablePtr, freeStablePtr, newStablePtr)
import Foreign.Storable (peek)

{- | Acquire callbacks and transfer their cleanup to a DuckDB object.
The object must be non-null. Its destructor must run after the action.
-}
withCallbackResources ::
    ((forall a. IO (FunPtr a) -> IO (FunPtr a)) -> IO r) ->
    (Ptr Void -> DuckDBV2OpaqueDestroyFn -> IO ()) ->
    (r -> IO b) ->
    IO b
withCallbackResources = withCallbackResourcesFinally (pure ())

{- | Run a final action before the callback resources are released.
This includes acquisition and registration failure. Contain final-action errors.
-}
withCallbackResourcesFinally ::
    IO () ->
    ((forall a. IO (FunPtr a) -> IO (FunPtr a)) -> IO r) ->
    (Ptr Void -> DuckDBV2OpaqueDestroyFn -> IO ()) ->
    (r -> IO b) ->
    IO b
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
    attach (castPtr (castStablePtrToPtr stable)) callbackResourcesDestructor
        `onException` (freeStablePtr stable >> cleanup)
    restore (action resources)

{- | Transfer one state value to a DuckDB callback state slot.
Use each slot once. Native init and batch setters do not release replaced values.
-}
transferCallbackState :: (Ptr Void -> DuckDBV2OpaqueDestroyFn -> IO ()) -> a -> IO ()
transferCallbackState attach state = mask_ do
    stable <- newStablePtr state
    attach (castPtr (castStablePtrToPtr stable)) callbackStateDestructor
        `onException` freeStablePtr stable

-- | Convert callback exceptions to native V2 diagnostics.
runCallback :: Ptr DuckDBV2ErrorInfoHandle -> IO () -> IO ()
runCallback err action = mask \restore -> do
    outcome <- try (restore action)
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
                                withText (Text.pack (displayException failure)) $ \message ->
                                    c_duckdb_v2_error_info_set_text info message >>= checkCode

-- | Transfer an opaque resource to a checked V2 setter.
attachOpaque ::
    (Ptr DuckDBV2Opaque -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error) ->
    Ptr Void ->
    DuckDBV2OpaqueDestroyFn ->
    IO ()
attachOpaque setter ptr release =
    with (DuckDBV2Opaque ptr release (DuckDBV2OpaqueEqualsFn nullFunPtr)) $ \opaque ->
        checked (setter opaque)

-- | Contain exceptions in callbacks which have no error channel.
ignoreCallbackExceptions :: IO () -> IO ()
ignoreCallbackExceptions action = mask \restore ->
    restore action `catch` \(_ :: SomeException) -> pure ()

-- | Release object-owned callbacks through a static C entry point.
releaseCallbackResources :: Ptr Void -> IO ()
releaseCallbackResources raw =
    mask_
        $ ignoreCallbackExceptions
        $ if raw == nullPtr
            then pure ()
            else do
                let stable = castPtrToStablePtr (castPtr raw) :: StablePtr (IO ())
                (deRefStablePtr stable >>= id) `finally` freeStablePtr stable

-- | Release callback state independently of the registered function.
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
