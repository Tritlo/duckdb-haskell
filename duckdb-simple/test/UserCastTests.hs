{-# LANGUAGE BlockArguments #-}
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

import Control.Monad (forM_)
import Data.Array (listArray)
import qualified Data.ByteString as BS
import qualified Data.Geometry as G
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Database.DuckDB.FFI.V2
import Database.DuckDB.Simple
import qualified Database.DuckDB.Simple.Config as Config
import Database.DuckDB.Simple.FromField (FieldValue (..))
import Database.DuckDB.Simple.Geometry (RawGeometry (..))
import Database.DuckDB.Simple.LogicalRep (LogicalTypeRep (..), StructField (..), StructValue (..))
import qualified Database.DuckDB.Simple.Streaming as Streaming
import qualified Database.DuckDB.Simple.V2.Registration as R
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
