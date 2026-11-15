{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : TestUtils
-- Description : Assertions that several test modules share.
module TestUtils (
  NativeTimestamp (..),
  assertFailureIO,
  assertInputError,
  assertInputErrorContaining,
  assertReleasedAfter,
  assertSqlErrorType,
  filteredQueries,
  nativeLocalTime,
  raiseCapabilities,
  withConstCString,
  writeIntegers,
) where

import Control.Concurrent (getNumCapabilities, setNumCapabilities)
import Control.Exception (SomeException, try)
import Control.Monad (forM_, void, when)
import Data.IORef (IORef, mkWeakIORef, newIORef)
import Data.Int (Int64)
import Data.Maybe (isNothing)
import Data.Ratio ((%))
import qualified Data.Text as Text
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.LocalTime (LocalTime, utc, utcToLocalTime)
import Database.DuckDB.FFI.V2 (
  DuckDBV2VectorHandle,
  c_duckdb_v2_value_create_timestamp,
  c_duckdb_v2_value_create_timestamp_ms,
  c_duckdb_v2_value_create_timestamp_ns,
  c_duckdb_v2_value_create_timestamp_sec,
  c_duckdb_v2_vector_flatten,
  c_duckdb_v2_vector_get_data_mutable,
  c_duckdb_v2_vector_set_size,
 )
import Database.DuckDB.Simple (ErrorType (..), Query, SQLError (..))
import Database.DuckDB.Simple.ToField (DuckDBColumnType (..), ToDuckValue (..), ToField)
import Database.DuckDB.Simple.V2.Registration (checked, output)
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.C.String (withCString)
import Foreign.C.Types (CChar)
import Foreign.Ptr (castPtr)
import Foreign.Storable (pokeElemOff)
import System.Mem (performMajorGC)
import System.Mem.Weak (deRefWeak)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, (@?=))

{- Note [Capabilities of the tests]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Some cases need more than one capability of the GHC runtime, for example to
run several Haskell threads on one connection at the same time. Such a case
raises the capability count with raiseCapabilities. No case lowers the count,
also not to restore it after the case.

The reason is a defect of the GHC runtime, seen with GHC 9.14.1. After
setNumCapabilities lowers the count, the runtime keeps the other capabilities
as disabled capabilities. A callback from a foreign thread, for example from
a DuckDB worker thread, can then run on a disabled capability. A garbage
collection can then lose the wake-up of that callback. The callback then
waits for ever, and the test run does not end.
-}

-- | Raise the capability count of the GHC runtime to the given count. Do not
-- lower it. See Note [Capabilities of the tests].
raiseCapabilities :: Int -> IO ()
raiseCapabilities wanted = do
  current <- getNumCapabilities
  when (wanted > current) (setNumCapabilities wanted)

-- | Require an exception without depending on native error text.
assertFailureIO :: IO a -> Assertion
assertFailureIO action = do
  result <- try (action >> pure ()) :: IO (Either SomeException ())
  case result of
    Left _ -> pure ()
    Right () -> assertFailure "expected an exception"

-- | A timestamp in the raw unit of one of the four TIMESTAMP types. A
-- parameter of this type stores the raw value, also one that no Haskell time
-- type can bind, such as a value of seconds outside the range of microseconds.
data NativeTimestamp = Seconds Int64 | Milliseconds Int64 | Microseconds Int64 | Nanoseconds Int64
  deriving (Show)

instance DuckDBColumnType NativeTimestamp where
  duckdbColumnTypeFor _ = "TIMESTAMP"

instance ToDuckValue NativeTimestamp where
  toDuckValue factory (Seconds value) = output (c_duckdb_v2_value_create_timestamp_sec factory value)
  toDuckValue factory (Milliseconds value) = output (c_duckdb_v2_value_create_timestamp_ms factory value)
  toDuckValue factory (Microseconds value) = output (c_duckdb_v2_value_create_timestamp factory value)
  toDuckValue factory (Nanoseconds value) = output (c_duckdb_v2_value_create_timestamp_ns factory value)

instance ToField NativeTimestamp

-- | The local time in UTC of a raw timestamp. The computation uses neither
-- DuckDB nor duckdb-simple.
nativeLocalTime :: NativeTimestamp -> LocalTime
nativeLocalTime timestamp = utcToLocalTime utc (posixSecondsToUTCTime (fromRational (toInteger value % units)))
  where
    (value, units) = case timestamp of
      Seconds raw -> (raw, 1)
      Milliseconds raw -> (raw, 1000)
      Microseconds raw -> (raw, 1000000)
      Nanoseconds raw -> (raw, 1000000000)

-- | Require the error of an input check of the library: an 'SQLError' of the
-- type 'ErrorInputInvalid' whose message starts with "duckdb-simple: ".
assertInputError :: IO a -> Assertion
assertInputError = assertInputErrorContaining Text.empty

-- | Require the error of an input check of the library, of the type
-- ErrorInputInvalid, which includes the expected message.
assertInputErrorContaining :: Text.Text -> IO a -> Assertion
assertInputErrorContaining expected action = do
  outcome <- try (void action)
  case outcome of
    Left err -> do
      sqlErrorType err @?= Just ErrorInputInvalid
      -- The prefix shows that a check of the library raised the error, not DuckDB.
      assertBool ("expected an input error containing " <> show expected <> ", got " <> show err) $
        ("duckdb-simple: " `Text.isPrefixOf` sqlErrorMessage err) && expected `Text.isInfixOf` sqlErrorMessage err
    Right () -> assertFailure "expected the error of an input check"

-- | Require an 'SQLError' of DuckDB with the given type. An error of
-- duckdb-simple, whose message starts with "duckdb-simple: ", fails the
-- assertion, also when it has the same type.
assertSqlErrorType :: ErrorType -> IO a -> Assertion
assertSqlErrorType expected action = do
  result <- try action
  case result of
    Left err -> do
      assertBool
        ("expected an error of DuckDB, got " <> Text.unpack (sqlErrorMessage err))
        (not ("duckdb-simple: " `Text.isPrefixOf` sqlErrorMessage err))
      sqlErrorType err @?= Just expected
    Right _ -> assertFailure ("expected an SQLError of the type " <> show expected)

-- | Run an action with a new token, and require that no value keeps the token
-- after the action and two major collections. The action gives the token to the
-- callbacks of a registration that fails, is replaced, or ends with its database.
assertReleasedAfter :: String -> (IORef Int64 -> IO ()) -> Assertion
assertReleasedAfter label action = do
  weak <- do
    token <- newIORef 1
    weak <- mkWeakIORef token (pure ())
    action token
    pure weak
  performMajorGC
  performMajorGC
  deRefWeak weak >>= assertBool (label <> " keeps its callback after the case") . isNothing

-- | Supply a constant C string for one native call.
withConstCString :: String -> (ConstPtr CChar -> IO a) -> IO a
withConstCString text action = withCString text (action . ConstPtr)

-- | Set the size of a BIGINT vector and write the values to its rows. The
-- function flattens the vector first, so that each row has its own slot: the
-- header gives the data pointer of a FLAT or CONSTANT vector, and a CONSTANT
-- vector has one value for all rows. A flatten does nothing for a FLAT vector.
writeIntegers :: DuckDBV2VectorHandle -> [Int64] -> IO ()
writeIntegers vector values = do
  checked (c_duckdb_v2_vector_set_size vector (fromIntegral (length values)))
  checked (c_duckdb_v2_vector_flatten vector)
  raw <- output (c_duckdb_v2_vector_get_data_mutable vector)
  forM_ (zip [0 ..] values) $ \(index, value) -> pokeElemOff (castPtr raw) index value

-- | Filtered queries over more than one chunk, each with the rows that DuckDB
-- gives. After the filter, most chunks of the second and the third query have
-- no row.
filteredQueries :: [(Query, [Int64])]
filteredQueries =
  [ ("SELECT i FROM range(100000) t(i) WHERE i < 3 OR i >= 99997", [0, 1, 2, 99997, 99998, 99999])
  , ("SELECT i FROM range(1000000) t(i) WHERE i % 100000 = 99999", [99999, 199999 .. 999999])
  , ("SELECT i FROM range(1000000) t(i) WHERE i = 999999", [999999])
  ]
