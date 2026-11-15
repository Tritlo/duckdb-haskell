{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : UserCastTests
-- Description : Check that a cast that a user registers does not change what the library reads.
--
-- Each case registers a cast that writes NULL for each row and counts its
-- calls. Then the case reads a value whose reader uses the built-in cast for
-- the same pair of types. The value must be correct, and the user cast must
-- not run. See Note [User casts and the casts of the library] in
-- "Database.DuckDB.Simple.Materialize".
module UserCastTests (userCastTests) where

import Control.Exception (try)
import Control.Monad (forM_, void)
import Data.Array (listArray)
import qualified Data.ByteString as BS
import qualified Data.Geometry as G
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Database.DuckDB.FFI.V2
import Database.DuckDB.Simple
import qualified Database.DuckDB.Simple.Config as Config
import qualified Database.DuckDB.Simple.Copy as Copy
import Database.DuckDB.Simple.FromField (Field (..), FieldValue (..))
import Database.DuckDB.Simple.Geometry (RawGeometry (..))
import Database.DuckDB.Simple.LogicalRep (LogicalTypeRep (..), StructField (..), StructValue (..))
import Database.DuckDB.Simple.Pragma (PragmaCallback (..), registerPragmaFunction)
import qualified Database.DuckDB.Simple.Streaming as Streaming
import qualified Database.DuckDB.Simple.V2.Registration as R
import Database.DuckDB.Simple.Variant (Variant (..), variantObject)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

-- | Check the readers and binders of the library under user casts.
userCastTests :: TestTree
userCastTests =
  testGroup
    "user casts"
    [ testCase "a cast from GEOMETRY to BLOB does not change a GEOMETRY result" $ withConnection ":memory:" \conn -> do
        calls <- nullCast conn (R.TypeExpression "GEOMETRY") (R.TypeExpression "BLOB")
        (query_ conn "SELECT 'POINT (1 2)'::GEOMETRY" :: IO [Only (Maybe RawGeometry)]) >>= (@?= [Only (Just point)])
        (query_ conn "SELECT 'POINT (1 2)'::GEOMETRY" :: IO [Only (Maybe G.Geometry)]) >>= (@?= [Only (Just shape)])
        (query_ conn "SELECT {'g': 'POINT (1 2)'::GEOMETRY}" :: IO [Only FieldValue])
          >>= (@?= [Only (FieldStruct (oneField "g" (LogicalTypeGeometry Nothing) (FieldGeometry point)))])
        Streaming.fold_ conn "SELECT 'POINT (1 2)'::GEOMETRY FROM range(3)" [] (\seen (Only value) -> pure (value : seen))
          >>= (@?= replicate 3 point)
        -- A GEOMETRY with a CRS has another type, so this cast is not used for it.
        (query conn "SELECT ST_SetCRS(ST_GeomFromWKB(?), 'local grid')" (Only pointWKB) :: IO [Only RawGeometry])
          >>= (@?= [Only point{rawGeometryCRS = Just "local grid"}])
        readIORef calls >>= (@?= 0)
    , testCase "a cast from GEOMETRY('OGC:CRS84') to BLOB does not change a GEOMETRY('OGC:CRS84') result" $
        withConnection ":memory:" \conn -> do
          calls <- nullCast conn (R.StructuredType (LogicalTypeGeometry (Just "OGC:CRS84"))) (R.TypeExpression "BLOB")
          (query_ conn "SELECT 'POINT (1 2)'::GEOMETRY('OGC:CRS84')" :: IO [Only RawGeometry])
            >>= (@?= [Only point{rawGeometryCRS = Just "OGC:CRS84"}])
          readIORef calls >>= (@?= 0)
    , testCase "a cast from GEOMETRY to BLOB does not change the value of a GEOMETRY option" $
        withConnection ":memory:" \conn -> do
          Config.registerCustomOption
            conn
            (Config.customOption "user_cast_shape" (R.TypeExpression "GEOMETRY")){Config.customOptionDefault = Just (R.defaultValue shape)}
          calls <- nullCast conn (R.TypeExpression "GEOMETRY") (R.TypeExpression "BLOB")
          Config.getConfigDefaultValue conn "user_cast_shape" >>= (@?= FieldGeometry point)
          (fst <$> Config.getConfigValue conn "user_cast_shape") >>= (@?= FieldGeometry point)
          readIORef calls >>= (@?= 0)
    , testCase "a cast from GEOMETRY to BLOB does not change a GEOMETRY argument of a table function" $
        withConnection ":memory:" \conn -> do
          createTableFunction conn "user_cast_shapes" ["same"] \(Only argument) -> pure [Only (argument == Just shape)]
          calls <- nullCast conn (R.TypeExpression "GEOMETRY") (R.TypeExpression "BLOB")
          query_ conn "SELECT same FROM user_cast_shapes('POINT (1 2)'::GEOMETRY)" >>= (@?= [Only True])
          readIORef calls >>= (@?= 0)
    , testCase "a cast from GEOMETRY to BLOB does not change a GEOMETRY row of a COPY TO function" $
        withConnection ":memory:" \conn -> do
          rows <- newIORef []
          Copy.registerCopyToFunction conn "user_cast_copy" (\_ -> pure ()) (\_ -> pure ()) (\_ batch -> modifyIORef' rows (<> batch)) (\_ -> pure ())
          calls <- nullCast conn (R.TypeExpression "GEOMETRY") (R.TypeExpression "BLOB")
          void (execute_ conn "COPY (SELECT 'POINT (1 2)'::GEOMETRY AS g) TO 'unused' (FORMAT user_cast_copy)")
          (map (map fieldValue) <$> readIORef rows) >>= (@?= [[FieldGeometry point]])
          readIORef calls >>= (@?= 0)
    , testCase "a cast from GEOMETRY to BLOB does not change a GEOMETRY argument of a pragma" $
        withConnection ":memory:" \conn -> do
          seen <- newIORef Nothing
          registerPragmaFunction
            conn
            "user_cast_pragma"
            [R.parameter "shape" (R.TypeExpression "GEOMETRY")]
            (ExecPragma \(Only argument) -> writeIORef seen argument)
          calls <- nullCast conn (R.TypeExpression "GEOMETRY") (R.TypeExpression "BLOB")
          void (execute_ conn "PRAGMA user_cast_pragma('POINT (1 2)')")
          readIORef seen >>= (@?= Just shape)
          readIORef calls >>= (@?= 0)
    , testCase "a cast from GEOMETRY to BLOB does not change the value of a GEOMETRY option update" $
        withConnection ":memory:" \conn -> do
          seen <- newIORef []
          Config.registerCustomOption
            conn
            (Config.customOption "user_cast_update" (R.TypeExpression "GEOMETRY"))
              { Config.customOptionUpdate = Just \value -> modifyIORef' seen (value :)
              }
          calls <- nullCast conn (R.TypeExpression "GEOMETRY") (R.TypeExpression "BLOB")
          void (execute_ conn "SET user_cast_update = 'POINT (1 2)'")
          readIORef seen >>= (@?= [FieldGeometry point])
          readIORef calls >>= (@?= 0)
    , testCase "a cast from VARIANT to BIGINT does not change a BIGINT payload of a VARIANT result" $
        withConnection ":memory:" \conn -> do
          calls <- nullCast conn (R.TypeExpression "VARIANT") (R.TypeExpression "BIGINT")
          query_ conn "SELECT 42::BIGINT::VARIANT" >>= (@?= [Only (Variant (FieldInt64 42))])
          query_ conn "SELECT {'n': 42::BIGINT}::VARIANT" >>= (@?= [Only (Variant (variantObject [("n", FieldInt64 42)]))])
          query_ conn "SELECT [1, 2]::BIGINT[]::VARIANT" >>= (@?= [Only (Variant (FieldList [FieldInt64 1, FieldInt64 2]))])
          readIORef calls >>= (@?= 0)
    , testCase "a cast from GEOMETRY to BLOB does not change a GEOMETRY payload of a VARIANT result" $
        withConnection ":memory:" \conn -> do
          calls <- nullCast conn (R.TypeExpression "GEOMETRY") (R.TypeExpression "BLOB")
          query_ conn "SELECT 'POINT (1 2)'::GEOMETRY::VARIANT" >>= (@?= [Only (Variant (FieldGeometry point))])
          readIORef calls >>= (@?= 0)
    , testCase "a cast from a VARIANT list to a STRUCT list does not change a VARIANT object" $
        withConnection ":memory:" \conn -> do
          let variant = LogicalTypeScalar LogicalTypeIdVariant
              objects = LogicalTypeList (LogicalTypeStruct (listArray (0, 0) [StructField "a" variant]))
          calls <- nullCast conn (R.StructuredType (LogicalTypeList variant)) (R.StructuredType objects)
          query_ conn "SELECT {'a': 1}::VARIANT" >>= (@?= [Only (Variant (variantObject [("a", FieldInt32 1)]))])
          readIORef calls >>= (@?= 0)
    , testCase "a cast from VARIANT to BIGINT does not change a VARIANT row of a COPY TO function" $
        withConnection ":memory:" \conn -> do
          rows <- newIORef []
          Copy.registerCopyToFunction
            conn
            "user_cast_variant_copy"
            (\_ -> pure ())
            (\_ -> pure ())
            (\_ batch -> modifyIORef' rows (<> batch))
            (\_ -> pure ())
          calls <- nullCast conn (R.TypeExpression "VARIANT") (R.TypeExpression "BIGINT")
          void (execute_ conn "COPY (SELECT 42::BIGINT::VARIANT AS v) TO 'unused' (FORMAT user_cast_variant_copy)")
          (map (map fieldValue) <$> readIORef rows) >>= (@?= [[FieldInt64 42]])
          readIORef calls >>= (@?= 0)
    , testCase "a cast from VARCHAR to GEOMETRY does not run when a connection with a custom CRS opens" $
        withEnvironment \environment -> withInstance environment [] \instance' -> withConnectionOn instance' \conn -> do
          calls <- nullCast conn (R.TypeExpression "VARCHAR") (R.TypeExpression "GEOMETRY")
          -- The open reads the GEOMETRY type of the custom CRS with a query.
          other <- connectWithOptions instance' defaultConnectionOptions{connectionGeometryCRS = ["local grid"]}
          readIORef calls >>= (@?= 0)
          let grid = point{rawGeometryCRS = Just "local grid"}
              struct = oneField "g" (LogicalTypeGeometry (Just "local grid")) (FieldGeometry grid)
          query other "SELECT ?" (Only struct) >>= (@?= [Only struct])
          close other
    , testCase "a cast from BLOB to GEOMETRY does not change a bound raw geometry" $ withConnection ":memory:" \conn -> do
        calls <- nullCast conn (R.TypeExpression "BLOB") (R.TypeExpression "GEOMETRY")
        let struct = oneField "g" (LogicalTypeGeometry Nothing) (FieldGeometry point)
        query conn "SELECT ?" (Only point) >>= (@?= [Only point])
        query conn "SELECT ?" (Only struct) >>= (@?= [Only struct])
        query conn "SELECT ?" (Only (Variant (FieldGeometry point))) >>= (@?= [Only (Variant (FieldGeometry point))])
        readIORef calls >>= (@?= 0)
    , testCase "a cast from BLOB to GEOMETRY('OGC:CRS84') does not change a bound raw geometry with that CRS" $
        withConnection ":memory:" \conn -> do
          let crs84 = LogicalTypeGeometry (Just "OGC:CRS84")
              struct = oneField "g" crs84 (FieldGeometry point{rawGeometryCRS = Just "OGC:CRS84"})
          calls <- nullCast conn (R.TypeExpression "BLOB") (R.StructuredType crs84)
          query conn "SELECT ?" (Only struct) >>= (@?= [Only struct])
          readIORef calls >>= (@?= 0)
    , testCase "a cast from VARCHAR to GEOMETRY does not change a bound Geometry" $ withConnection ":memory:" \conn -> do
        calls <- nullCast conn (R.TypeExpression "VARCHAR") (R.TypeExpression "GEOMETRY")
        query conn "SELECT ?" (Only shape) >>= (@?= [Only shape])
        query conn "SELECT ?" (Only [shape, shape]) >>= (@?= [Only [shape, shape]])
        readIORef calls >>= (@?= 0)
    , testCase "a cast from VARCHAR to an ENUM type does not change a bound ENUM value" $ withConnection ":memory:" \conn -> do
        let entries = LogicalTypeEnum (listArray (0, 1) ["x", "y"])
            struct = oneField "e" entries (FieldEnum 1)
        calls <- nullCast conn (R.TypeExpression "VARCHAR") (R.StructuredType entries)
        query conn "SELECT ?" (Only struct) >>= (@?= [Only struct])
        readIORef calls >>= (@?= 0)
    , testCase "a cast from BIGINT to VARIANT does not change a bound Variant" $ withConnection ":memory:" \conn -> do
        calls <- nullCast conn (R.TypeExpression "BIGINT") (R.TypeExpression "VARIANT")
        let list = Variant (FieldList [FieldInt64 1, FieldInt64 2])
        query conn "SELECT ?" (Only (Variant (FieldInt64 5))) >>= (@?= [Only (Variant (FieldInt64 5))])
        query conn "SELECT ?" (Only list) >>= (@?= [Only list])
        readIORef calls >>= (@?= 0)
    , testCase "a cast from VARCHAR to GEOMETRY does not change a Geometry row of a table function" $
        withConnection ":memory:" \conn -> do
          createTableFunction conn "user_cast_rows" ["g"] \() -> pure [Only shape]
          calls <- nullCast conn (R.TypeExpression "VARCHAR") (R.TypeExpression "GEOMETRY")
          query_ conn "SELECT g FROM user_cast_rows()" >>= (@?= [Only (Just shape)])
          readIORef calls >>= (@?= 0)
    , testCase "a bound raw geometry keeps its typed NULL and its error for bytes that are not WKB" $
        withConnection ":memory:" \conn -> do
          let crs84 = LogicalTypeGeometry (Just "OGC:CRS84")
              struct = oneField "g" crs84 FieldNull
          query conn "SELECT ?" (Only struct) >>= (@?= [Only struct])
          try (query conn "SELECT ?" (Only (RawGeometry (BS.pack [1, 2, 3]) Nothing)) :: IO [Only RawGeometry]) >>= \case
            Left SQLError{sqlErrorMessage} ->
              assertBool (show sqlErrorMessage) ("Could not convert BLOB to GEOMETRY" `Text.isInfixOf` sqlErrorMessage)
            Right rows -> assertFailure ("expected an error, got " <> show rows)
    ]

-- | Register a cast that writes NULL for each row, and give its call count.
nullCast :: Connection -> R.TypeDeclaration -> R.TypeDeclaration -> IO (IORef Int)
nullCast conn source target = do
  calls <- newIORef 0
  R.registerCast conn source target Nothing \info _ -> do
    atomicModifyIORef' calls (\count -> (count + 1, ()))
    rows <- R.output (c_duckdb_v2_cast_function_exec_get_row_count info)
    vector <- R.output (c_duckdb_v2_cast_function_exec_get_output info)
    R.checked (c_duckdb_v2_vector_set_size vector rows)
    R.checked (c_duckdb_v2_vector_flatten vector)
    forM_ [0 .. fromIntegral rows - 1 :: Int] \row -> R.checked (c_duckdb_v2_vector_set_null vector (fromIntegral row))
  pure calls

-- | WKB of POINT (1 2).
pointWKB :: BS.ByteString
pointWKB = BS.pack [1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 240, 63, 0, 0, 0, 0, 0, 0, 0, 64]

point :: RawGeometry
point = RawGeometry pointWKB Nothing

shape :: G.Geometry
shape = G.PointGeometry (G.PointXY (G.XY 1 2))

oneField :: Text -> LogicalTypeRep -> FieldValue -> StructValue FieldValue
oneField name rep value =
  StructValue
    { structValueFields = listArray (0, 0) [StructField name value]
    , structValueTypes = listArray (0, 0) [StructField name rep]
    , structValueIndex = Map.singleton name 0
    }
