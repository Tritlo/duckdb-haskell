{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}

-- |
-- Module      : Database.DuckDB.Simple.CallbackMark
-- Description : Mark the threads that are inside a DuckDB callback.
--
-- 'withCallbackMark' marks the current thread for the time of an action.
-- 'hasCallbackMark' tells if the current thread has a mark. See
-- Note [Callback marks].
module Database.DuckDB.Simple.CallbackMark (
  withCallbackMark,
  hasCallbackMark,
) where

import Control.Concurrent (myThreadId)
import Control.Exception (evaluate, finally, mask)
import Control.Monad (unless)
import Data.IORef (newIORef, readIORef)
import qualified Data.IntMap.Strict as IntMap
import GHC.Exts (casMutVar#)
import GHC.IO (IO (..))
import GHC.IORef (IORef (..))
import GHC.STRef (STRef (..))
import System.IO.Unsafe (unsafePerformIO)
#if MIN_VERSION_base(4,19,0)
import GHC.Conc.Sync (ThreadId, fromThreadId)
#else
import Foreign.C.Types (CULLong (..))
import GHC.Conc.Sync (ThreadId (..))
import GHC.Exts (ThreadId#)
#endif

{- Note [Callback marks]
~~~~~~~~~~~~~~~~~~~~~~~~
DuckDB runs a callback while a native call of the library holds the lock of a
connection. A statement that the callback runs on that connection waits for
the lock, and the holder of the lock waits for the callback. Thus
withConnectionLock gives a bound to the wait of a thread that is inside a
callback. A mark tells it which threads these are. runCallback marks its thread
with withCallbackMark, and withConnectionLock asks hasCallbackMark.

The mark of a thread is an entry in callbackMarks, one table for the process.
These are the reasons for its form.

- The table is a top-level value. A callback does not always have a connection.
  For example, an aggregate callback gets no context. Thus the table cannot be
  a field of a connection.

- The mark is not a thread label. In GHC 9.6 to 9.14.1, threadLabel gives a
  null pointer for a thread without a label, and the garbage collector can
  crash on that pointer (GHC issue 27618). DuckDB calls each callback in a new
  thread without a label.

- The key is the number of the thread, not its ThreadId. A ThreadId in the
  table keeps its thread reachable. Then the GHC runtime does not raise
  BlockedIndefinitelyOnMVar in a callback that waits for a value that no
  thread can supply, and the statement does not return. The runtime gives each
  thread a different number and does not use a number again.

- The value is a count of marks. A thread that already has a mark thus keeps
  it until the outer withCallbackMark ends.

- A change is a compare-and-swap (changeCallbackMarks). atomicModifyIORef'
  stores a value that is not evaluated, and it allocates more. If the runtime
  stops a thread during that evaluation, each other thread that needs the value
  waits. With one capability, each such wait of a callback thread is a context
  switch of the operating system. With compare-and-swap, the table holds only
  evaluated maps, and no thread waits for another thread.

- The table is one IORef. Only the threads that are inside a callback at the
  same time have an entry, so the map is small. Two threads that change the
  table at the same time cost one more attempt for one of them.
-}

-- | The threads inside a DuckDB callback, with the number of marks of each
-- thread. The key is the number of the thread. See Note [Callback marks].
callbackMarks :: IORef (IntMap.IntMap Int)
callbackMarks = unsafePerformIO (newIORef IntMap.empty)
{-# NOINLINE callbackMarks #-}

-- | The number of a Haskell thread. See Note [Callback marks].
threadNumber :: ThreadId -> Int
#if MIN_VERSION_base(4,19,0)
threadNumber = fromIntegral . fromThreadId
#else
-- base 4.18 has no fromThreadId. This is the import that base 4.19 uses for it.
threadNumber (ThreadId thread) = fromIntegral (rts_getThreadId thread)

foreign import ccall unsafe "rts_getThreadId" rts_getThreadId :: ThreadId# -> CULLong
#endif

-- | Mark the current thread as a thread inside a DuckDB callback for the time
-- of the action. The entry adds 1 to the marks of the thread, and each exit
-- subtracts 1. A thread that the action starts has no mark. See
-- Note [Callback marks].
withCallbackMark :: IO a -> IO a
withCallbackMark action = mask \restore -> do
  key <- threadNumber <$> myThreadId
  changeCallbackMarks (IntMap.insertWith (+) key 1)
  restore action `finally` changeCallbackMarks (IntMap.update unmark key)
  where
    unmark :: Int -> Maybe Int
    unmark count = if count > 1 then Just (count - 1) else Nothing

-- | Change 'callbackMarks' with compare-and-swap. Read the map, evaluate the
-- new map, and store it if the table still holds the map that was read. If
-- another thread changed the table, do the change again. See
-- Note [Callback marks].
--
-- The swap compares pointers. A map is a value of a type with more than one
-- constructor, so GHC keeps its pointer and does not make a new box for it.
changeCallbackMarks :: (IntMap.IntMap Int -> IntMap.IntMap Int) -> IO ()
changeCallbackMarks change = do
  old <- readIORef callbackMarks
  new <- evaluate (change old)
  swapped <- case callbackMarks of
    IORef (STRef var) -> IO \s -> case casMutVar# var old new s of
      (# s', 0#, _ #) -> (# s', True #)
      (# s', _, _ #) -> (# s', False #)
  unless swapped (changeCallbackMarks change)

-- | Tell if 'withCallbackMark' marks the current thread. Only the thread
-- itself adds and removes its key, so the answer is exact without a lock.
hasCallbackMark :: IO Bool
hasCallbackMark = do
  key <- threadNumber <$> myThreadId
  IntMap.member key <$> readIORef callbackMarks
