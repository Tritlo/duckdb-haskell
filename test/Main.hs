module Main (main) where

import qualified ConfigurationTest
import qualified ErrorDataTest
import qualified OpenConnectTest
import qualified PreparedStatementsTest
import qualified BindValuesTest
import qualified ExecutePreparedStatementsTest
import qualified ExtractStatementsTest
import qualified DataChunkTest
import qualified LogicalTypesTest
import qualified PendingResultTest
import qualified ValueInterfaceTest
import qualified VectorTest
import qualified ValidityMaskTest
import qualified ScalarFunctionsTest
import qualified AggregateFunctionsTest
import qualified SelectionVectorTest
import qualified QueryExecutionTest
import qualified ResultFunctionsTest
import qualified SafeFetchTest
import qualified HelpersTest
import qualified TableFunctionsTest
import qualified ProfilingInfoTest
import qualified AppenderTest
import qualified TableDescriptionTest
import qualified CastFunctionsTest
import qualified ExpressionTest
import qualified ReplacementScansTest
import qualified ArrowInterfaceTest
import qualified ArrowInterfaceDeprecatedTests
import qualified StreamingResultTest
import qualified ThreadingTest
import Test.Tasty (defaultMain, testGroup)

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
