{-# LANGUAGE ForeignFunctionInterface #-}

{- | Shared callback imports for the DuckDB C APIs.

Each pointer type remains a separate type parameter. Public callback signatures
select the required types. These functions do not read pointer targets or manage
their ownership.

Keep each allocated callback pointer alive while native code can invoke it.
Free it with @freeHaskellFunPtr@ after its last possible invocation. Catch all
callback exceptions before they return to C.
-}
module Database.DuckDB.FFI.Callbacks (
    wrapVoid1,
    callVoid1,
    wrapVoid2,
    callVoid2,
    wrapVoid3,
    callVoid3,
    wrapVoid4,
    callVoid4,
    wrapBool2,
    callBool2,
    wrapInt2,
    callInt2,
    wrapPtr1,
    callPtr1,
    wrapPtr2,
    callPtr2,
) where

import Foreign.C.Types (CBool (..), CInt (..))
import Foreign.Ptr (FunPtr, Ptr)

-- | Allocate a void callback with one pointer argument.
foreign import ccall "wrapper"
    wrapVoid1 :: (Ptr a -> IO ()) -> IO (FunPtr (Ptr a -> IO ()))

-- | Invoke a void callback with one pointer argument.
foreign import ccall safe "dynamic"
    callVoid1 :: FunPtr (Ptr a -> IO ()) -> Ptr a -> IO ()

-- | Allocate a void callback with two pointer arguments.
foreign import ccall "wrapper"
    wrapVoid2 :: (Ptr a -> Ptr b -> IO ()) -> IO (FunPtr (Ptr a -> Ptr b -> IO ()))

-- | Invoke a void callback with two pointer arguments.
foreign import ccall safe "dynamic"
    callVoid2 :: FunPtr (Ptr a -> Ptr b -> IO ()) -> Ptr a -> Ptr b -> IO ()

-- | Allocate a void callback with three pointer arguments.
foreign import ccall "wrapper"
    wrapVoid3 :: (Ptr a -> Ptr b -> Ptr c -> IO ()) -> IO (FunPtr (Ptr a -> Ptr b -> Ptr c -> IO ()))

-- | Invoke a void callback with three pointer arguments.
foreign import ccall safe "dynamic"
    callVoid3 :: FunPtr (Ptr a -> Ptr b -> Ptr c -> IO ()) -> Ptr a -> Ptr b -> Ptr c -> IO ()

-- | Allocate a void callback with four pointer arguments.
foreign import ccall "wrapper"
    wrapVoid4 :: (Ptr a -> Ptr b -> Ptr c -> Ptr d -> IO ()) -> IO (FunPtr (Ptr a -> Ptr b -> Ptr c -> Ptr d -> IO ()))

-- | Invoke a void callback with four pointer arguments.
foreign import ccall safe "dynamic"
    callVoid4 :: FunPtr (Ptr a -> Ptr b -> Ptr c -> Ptr d -> IO ()) -> Ptr a -> Ptr b -> Ptr c -> Ptr d -> IO ()

-- | Allocate a C boolean callback with two pointer arguments.
foreign import ccall "wrapper"
    wrapBool2 :: (Ptr a -> Ptr b -> IO CBool) -> IO (FunPtr (Ptr a -> Ptr b -> IO CBool))

-- | Invoke a C boolean callback with two pointer arguments.
foreign import ccall safe "dynamic"
    callBool2 :: FunPtr (Ptr a -> Ptr b -> IO CBool) -> Ptr a -> Ptr b -> IO CBool

-- | Allocate a C integer callback with two pointer arguments.
foreign import ccall "wrapper"
    wrapInt2 :: (Ptr a -> Ptr b -> IO CInt) -> IO (FunPtr (Ptr a -> Ptr b -> IO CInt))

-- | Invoke a C integer callback with two pointer arguments.
foreign import ccall safe "dynamic"
    callInt2 :: FunPtr (Ptr a -> Ptr b -> IO CInt) -> Ptr a -> Ptr b -> IO CInt

-- | Allocate a pointer callback with one pointer argument.
foreign import ccall "wrapper"
    wrapPtr1 :: (Ptr a -> IO (Ptr b)) -> IO (FunPtr (Ptr a -> IO (Ptr b)))

-- | Invoke a pointer callback with one pointer argument.
foreign import ccall safe "dynamic"
    callPtr1 :: FunPtr (Ptr a -> IO (Ptr b)) -> Ptr a -> IO (Ptr b)

-- | Allocate a pointer callback with two pointer arguments.
foreign import ccall "wrapper"
    wrapPtr2 :: (Ptr a -> Ptr b -> IO (Ptr c)) -> IO (FunPtr (Ptr a -> Ptr b -> IO (Ptr c)))

-- | Invoke a pointer callback with two pointer arguments.
foreign import ccall safe "dynamic"
    callPtr2 :: FunPtr (Ptr a -> Ptr b -> IO (Ptr c)) -> Ptr a -> Ptr b -> IO (Ptr c)
