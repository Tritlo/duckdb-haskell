module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import qualified V2Test
import qualified V2ValueTest

main :: IO ()
main =
    defaultMain $
        testGroup
            "DuckDB V2 C API"
            [ V2Test.tests
            , V2ValueTest.tests
            ]
