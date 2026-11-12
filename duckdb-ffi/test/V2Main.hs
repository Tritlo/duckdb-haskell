module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import qualified V2ChunkTest
import qualified V2StatementTest
import qualified V2Test
import qualified V2TypeTest
import qualified V2ValueTest

main :: IO ()
main =
    defaultMain $
        testGroup
            "DuckDB V2 C API"
            [ V2Test.tests
            , V2ValueTest.tests
            , V2TypeTest.tests
            , V2ChunkTest.tests
            , V2StatementTest.tests
            ]
