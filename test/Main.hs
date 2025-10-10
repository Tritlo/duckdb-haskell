module Main (main) where

import qualified AggregateFunctionsTest
import qualified AppenderTest
import qualified ArrowInterfaceDeprecatedTests
import qualified ArrowInterfaceTest
import qualified BindValuesTest
import qualified CastFunctionsTest
import qualified ConfigurationTest
import qualified DataChunkTest
import qualified ErrorDataTest
import qualified ExecutePreparedStatementsTest
import qualified ExpressionTest
import qualified ExtractStatementsTest
import qualified HelpersTest
import qualified LogicalTypesTest
import qualified OpenConnectTest
import qualified PendingResultTest
import qualified PreparedStatementsTest
import qualified ProfilingInfoTest
import qualified QueryExecutionTest
import qualified ReplacementScansTest
import qualified ResultFunctionsTest
import qualified SafeFetchTest
import qualified ScalarFunctionsTest
import qualified SelectionVectorTest
import qualified StreamingResultTest
import qualified TableDescriptionTest
import qualified TableFunctionsTest
import Test.Tasty (defaultMain, testGroup)
import qualified ThreadingTest
import qualified ValidityMaskTest
import qualified ValueInterfaceTest
import qualified VectorTest

main :: IO ()
main =
    defaultMain $
        testGroup
            "DuckDB FFI"
            [ OpenConnectTest.tests
            , ErrorDataTest.tests
            , ConfigurationTest.tests
            , PreparedStatementsTest.tests
            , BindValuesTest.tests
            , ExecutePreparedStatementsTest.tests
            , ExtractStatementsTest.tests
            , LogicalTypesTest.tests
            , DataChunkTest.tests
            , VectorTest.tests
            , ValidityMaskTest.tests
            , ScalarFunctionsTest.tests
            , AggregateFunctionsTest.tests
            , TableFunctionsTest.tests
            , ProfilingInfoTest.tests
            , SelectionVectorTest.tests
            , PendingResultTest.tests
            , ValueInterfaceTest.tests
            , QueryExecutionTest.tests
            , ResultFunctionsTest.tests
            , SafeFetchTest.tests
            , HelpersTest.tests
            , TableDescriptionTest.tests
            , AppenderTest.tests
            , CastFunctionsTest.tests
            , ExpressionTest.tests
            , ReplacementScansTest.tests
            , ArrowInterfaceDeprecatedTests.tests
            , ArrowInterfaceTest.tests
            , StreamingResultTest.tests
            , ThreadingTest.tests
            ]
