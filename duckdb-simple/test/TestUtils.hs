-- | Assertions that several test modules share.
module TestUtils (assertFailureIO, withConstCString) where

import Control.Exception (SomeException, try)
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.C.String (withCString)
import Foreign.C.Types (CChar)
import Test.Tasty.HUnit (Assertion, assertFailure)

-- | Require an exception without depending on native error text.
assertFailureIO :: IO a -> Assertion
assertFailureIO action = do
    result <- try (action >> pure ()) :: IO (Either SomeException ())
    case result of
        Left _ -> pure ()
        Right () -> assertFailure "expected an exception"

-- | Supply a constant C string for one native call.
withConstCString :: String -> (ConstPtr CChar -> IO a) -> IO a
withConstCString text action = withCString text (action . ConstPtr)
