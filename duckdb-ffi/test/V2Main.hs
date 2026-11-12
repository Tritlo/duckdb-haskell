module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import qualified V2AggregateTest
import qualified V2CallbackTest
import qualified V2ChunkTest
import qualified V2FileSystemTest
import qualified V2FunctionDocsTest
import qualified V2MacroTest
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
            , V2FileSystemTest.tests
            , V2FunctionDocsTest.tests
            , V2CallbackTest.tests
            , V2AggregateTest.tests
            , V2MacroTest.tests
            ]
