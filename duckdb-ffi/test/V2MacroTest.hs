module V2MacroTest (tests) where

import Control.Exception (finally)
import Control.Monad (forM_)
import Data.Coerce (coerce)
import Database.DuckDB.FFI.V2
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (Storable (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import V2Test (checked, output, withInstance, withType, withView)

tests :: TestTree
tests =
  testGroup
    "DuckDB V2 macro functions"
    [ testCase "scalar and table macros take a name, a body and parameters, and destruction clears the handle" testMacroObjects
    ]

-- | Only an extension can register a macro. Thus this test cannot read the name, the body or the parameters back
-- from the catalog.
testMacroObjects :: IO ()
testMacroObjects = withInstance $ \instanceHandle -> do
  factory <- output (c_duckdb_v2_instance_get_factory instanceHandle)
  forM_ [(DuckDBV2MacroFunctionTypeScalar, "x + 1"), (DuckDBV2MacroFunctionTypeTable, "SELECT x AS y")] $ \(kind, body) -> alloca $ \slot -> do
    poke slot (coerce nullPtr)
    flip finally (c_duckdb_v2_macro_function_destroy slot >>= (@?= DuckDBV2ErrorNone)) $ do
      checked (c_duckdb_v2_macro_function_create factory kind slot)
      function <- peek slot
      assertBool "the macro function is non-null" (function /= coerce nullPtr)
      -- A second call replaces the name and the body.
      forM_ ["v2_first_name", "v2_macro"] $ \name -> withView name $ \view ->
        checked (c_duckdb_v2_macro_function_set_name function (coerce view))
      forM_ ["NULL", body] $ \text -> withView text $ \view -> checked (c_duckdb_v2_macro_function_set_body function view)
      signature <- output (c_duckdb_v2_macro_function_get_signature function)
      assertBool "the signature is non-null" (signature /= coerce nullPtr)
      withType factory "BIGINT" $ \ty -> withView "x" $ \name ->
        checked (c_duckdb_v2_function_signature_add_parameter signature (coerce name) ty (coerce nullPtr) DuckDBV2FunctionParameterKindStandard)
    peek slot >>= (@?= coerce nullPtr)
    -- A second destruction and the destruction of a null pointer do nothing.
    c_duckdb_v2_macro_function_destroy slot >>= (@?= DuckDBV2ErrorNone)
    c_duckdb_v2_macro_function_destroy nullPtr >>= (@?= DuckDBV2ErrorNone)
