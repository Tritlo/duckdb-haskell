{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Check the private codec's validation without constructing invalid native vectors.
module Main (main) where

import Control.Exception (IOException, try)
import qualified Data.ByteString as BS
import Data.Text (Text)
import Data.Word (Word32, Word8)
import Database.DuckDB.Simple.FromField (FieldValue (..))
import Database.DuckDB.Simple.Variant (variantObject)
import Database.DuckDB.Simple.VariantCodec (decodeVariantPayload)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit

-- | Run malformed payload checks and boundary cases for the native format.
main :: IO ()
main =
    defaultMain $
        testGroup
            "VARIANT payload validation"
            [ rejected "missing root" [] [] [] []
            , rejected "unknown tag" [(34, 0)] [] [] []
            , rejected "offset outside blob" [(1, 1)] [] [] []
            , rejected "truncated BIGINT" [(6, 0)] [] [] (replicate 7 0)
            , rejected "truncated varint" [(16, 0)] [] [] [128]
            , rejected "varint overflows uint32" [(16, 0)] [] [] [255, 255, 255, 255, 16]
            , rejected "unterminated varint" [(16, 0)] [] [] [255, 255, 255, 255, 255, 0]
            , rejected "string outside blob" [(16, 0)] [] [] [3, 97, 98]
            , rejected "invalid UTF-8" [(16, 0)] [] [] [1, 255]
            , rejected "decimal scale exceeds precision" [(15, 0)] [] [] [2, 3, 1, 0]
            , rejected "decimal value exceeds precision" [(15, 0)] [] [] [1, 0, 100, 0]
            , rejected "BIGNUM header is truncated" [(31, 0)] [] [] [2, 128, 0]
            , rejected "BIGNUM length mismatch" [(31, 0)] [] [] [4, 128, 0, 2, 1]
            , rejected "BIT padding exceeds seven" [(32, 0)] [] [] [2, 8, 255]
            , rejected "BIT padding bits are absent" [(32, 0)] [] [] [2, 3, 21]
            , rejected "child value out of range" [(30, 0)] [(Nothing, 1)] [] [1, 0]
            , rejected "container range out of bounds" [(30, 0)] [] [] [1, 0]
            , rejected "object key is missing" [(29, 0), (1, 2)] [(Just 0, 1)] [] [1, 0]
            , rejected "array child has a key" [(30, 0), (1, 2)] [(Just 0, 1)] [(0, "x")] [1, 0]
            , rejected "object child has no key" [(29, 0), (1, 2)] [(Nothing, 1)] [] [1, 0]
            , rejected "duplicate dictionary index" [(1, 0)] [] [(0, "x"), (0, "y")] []
            , rejected "duplicate object key" [(29, 0), (1, 2)] [(Just 0, 1), (Just 1, 1)] [(0, "x"), (1, "x")] [2, 0]
            , rejected "cyclic child reference" [(30, 0)] [(Nothing, 0)] [] [1, 0]
            , testCase "empty values need no payload bytes" $
                decodeVariantPayload [(30, 0)] [] [] (BS.singleton 0) >>= (@?= FieldList [])
            , testCase "shared children decode without duplicate traversal" $
                decodeVariantPayload [(30, 0), (3, 2)] [(Nothing, 1), (Nothing, 1)] [] (BS.pack [2, 0, 7])
                    >>= (@?= FieldList [FieldInt8 7, FieldInt8 7])
            , testCase "length-aware keys preserve embedded NUL" $
                decodeVariantPayload [(29, 0), (1, 2)] [(Just 0, 1)] [(0, "a\0b")] (BS.pack [1, 0])
                    >>= (@?= variantObject [("a\0b", FieldBool True)])
            , testCase "128 levels are accepted" $
                let (values, children, bytes) = nested 128
                 in decodeVariantPayload values children [] bytes >>= (@?= foldr (const (FieldList . pure)) (FieldInt8 7) [1 .. 127 :: Int])
            , testCase "129 levels are rejected" $
                let (values, children, bytes) = nested 129
                 in assertRejected (decodeVariantPayload values children [] bytes)
            ]

-- | Require a validation error for copied native payload data.
rejected :: String -> [(Word8, Word32)] -> [(Maybe Word32, Word32)] -> [(Word32, Text)] -> [Word8] -> TestTree
rejected label values children keys bytes =
    testCase label $ assertRejected (decodeVariantPayload values children keys (BS.pack bytes))

-- | Require a codec error.
assertRejected :: IO FieldValue -> Assertion
assertRejected action = do
    result <- try action
    case result of
        Left (_ :: IOException) -> pure ()
        Right value -> assertFailure ("expected payload rejection, got " <> show value)

-- | Construct a chain of arrays with one scalar leaf.
nested :: Int -> ([(Word8, Word32)], [(Maybe Word32, Word32)], BS.ByteString)
nested count =
    ( [(30, fromIntegral (2 * n)) | n <- [0 .. count - 2]] <> [(3, fromIntegral (2 * (count - 1)))]
    , [(Nothing, fromIntegral (n + 1)) | n <- [0 .. count - 2]]
    , BS.pack (concat [[1, fromIntegral n] | n <- [0 .. count - 2]] <> [7])
    )
