-- | Assertions that several test modules share.
module TestUtils (assertFailureIO) where

import Control.Exception (SomeException, try)
import Test.Tasty.HUnit (Assertion, assertFailure)

-- | Require an exception without depending on native error text.
assertFailureIO :: IO a -> Assertion
assertFailureIO action = do
    result <- try (action >> pure ()) :: IO (Either SomeException ())
    case result of
        Left _ -> pure ()
        Right () -> assertFailure "expected an exception"
