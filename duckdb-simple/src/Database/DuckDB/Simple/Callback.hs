{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Manage callback resources at the DuckDB ownership boundary.
module Database.DuckDB.Simple.Callback (
    withCallbackResources,
    transferCallbackState,
    runCallback,
    ignoreCallbackExceptions,
) where

import Control.Exception (SomeException, catch, displayException, finally, mask, mask_, onException, try)
import Data.IORef (modifyIORef', newIORef, readIORef)
import qualified Data.Text as Text
import qualified Data.Text.Foreign as TextForeign
import Database.DuckDB.FFI (DuckDBDeleteCallback)
import Foreign.C.String (CString, withCString)
import Foreign.Ptr (FunPtr, Ptr, freeHaskellFunPtr, nullPtr)
import Foreign.StablePtr (StablePtr, castPtrToStablePtr, castStablePtrToPtr, deRefStablePtr, freeStablePtr, newStablePtr)

{- | Acquire callbacks and transfer their cleanup to a DuckDB object.
The object must be non-null. Its destructor must run after the action.
-}
withCallbackResources ::
    ((forall a. IO (FunPtr a) -> IO (FunPtr a)) -> IO r) ->
    (Ptr () -> DuckDBDeleteCallback -> IO ()) ->
    (r -> IO b) ->
    IO b
withCallbackResources acquire attach action = mask \restore -> do
    cleanups <- newIORef []
    let cleanup = readIORef cleanups >>= sequence_
        allocate make = do
            ptr <- make
            modifyIORef' cleanups (freeHaskellFunPtr ptr :)
                `onException` freeHaskellFunPtr ptr
            pure ptr
    resources <- acquire allocate `onException` cleanup
    stable <- newStablePtr cleanup `onException` cleanup
    attach (castStablePtrToPtr stable) callbackResourcesDestructor
        `onException` (freeStablePtr stable >> cleanup)
    restore (action resources)

-- | Transfer one state value to a DuckDB callback state slot.
transferCallbackState :: (Ptr () -> DuckDBDeleteCallback -> IO ()) -> a -> IO ()
transferCallbackState attach state = mask_ do
    stable <- newStablePtr state
    attach (castStablePtrToPtr stable) callbackStateDestructor
        `onException` freeStablePtr stable

-- | Convert callback exceptions to DuckDB errors.
runCallback :: (CString -> IO ()) -> IO () -> IO ()
runCallback setError action = mask \restore -> do
    outcome <- try (restore action)
    case outcome of
        Right () -> pure ()
        Left (err :: SomeException) ->
            TextForeign.withCString (Text.pack (displayException err)) setError
                `catch` \(_ :: SomeException) ->
                    ignoreCallbackExceptions $
                        withCString "duckdb-simple: Haskell callback failed" setError

-- | Contain exceptions in callbacks which have no error channel.
ignoreCallbackExceptions :: IO () -> IO ()
ignoreCallbackExceptions action = mask \restore ->
    restore action `catch` \(_ :: SomeException) -> pure ()

-- | Release object-owned callbacks through a static C entry point.
releaseCallbackResources :: Ptr () -> IO ()
releaseCallbackResources raw =
    mask_
        $ ignoreCallbackExceptions
        $ if raw == nullPtr
            then pure ()
            else do
                let stable = castPtrToStablePtr raw :: StablePtr (IO ())
                (deRefStablePtr stable >>= id) `finally` freeStablePtr stable

-- | Release callback state independently of the registered function.
releaseCallbackState :: Ptr () -> IO ()
releaseCallbackState raw =
    mask_
        $ ignoreCallbackExceptions
        $ if raw == nullPtr then pure () else freeStablePtr (castPtrToStablePtr raw)

foreign export ccall "duckdb_simple_release_callback_resources"
    releaseCallbackResources :: Ptr () -> IO ()

foreign import ccall "&duckdb_simple_release_callback_resources"
    callbackResourcesDestructor :: DuckDBDeleteCallback

foreign export ccall "duckdb_simple_release_callback_state"
    releaseCallbackState :: Ptr () -> IO ()

foreign import ccall "&duckdb_simple_release_callback_state"
    callbackStateDestructor :: DuckDBDeleteCallback
