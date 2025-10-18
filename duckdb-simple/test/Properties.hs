{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Properties (roundTripTests) where

import Control.Monad (forM)
import Data.Array (Array, elems, listArray)
import qualified Data.ByteString as BS
import Data.Fixed (Fixed (MkFixed))
import Data.Int (Int16, Int32, Int64, Int8)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import qualified Data.Text as Text
import Data.Time.Calendar (Day (..))
import Data.Time.Clock (UTCTime (..), diffTimeToPicoseconds, picosecondsToDiffTime)
import Data.Time.LocalTime (LocalTime (..), TimeOfDay (..), timeOfDayToTime)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Data.Word (Word16, Word32, Word64, Word8)
import Database.DuckDB.FFI (
    pattern DuckDBTypeBlob,
    pattern DuckDBTypeBoolean,
    pattern DuckDBTypeDouble,
    pattern DuckDBTypeInteger,
    pattern DuckDBTypeVarchar,
 )
import Database.DuckDB.Simple (Connection, close, open, query)
import Database.DuckDB.Simple.FromField (
    BigNum (..),
    BitString (..),
    FieldValue (..),
    FromField,
    LogicalTypeRep (..),
    StructField (..),
    StructValue (..),
    UnionMemberType (..),
    UnionValue (..)
 )
import Database.DuckDB.Simple.ToField (ToField)
import Database.DuckDB.Simple.Types (Null (..), Only (..))
import Numeric.Natural (Natural)
import Test.QuickCheck (
    Gen,
    Property,
    arbitrary,
    arbitrarySizedNatural,
    chooseInt,
    chooseInteger,
    counterexample,
    elements,
    frequency,
    forAllShrink,
    ioProperty,
    scale,
    shrink,
    shrinkIntegral,
    sized,
    suchThat,
    vectorOf,
    (===),
 )
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.QuickCheck (testProperty)

data SomeRoundTrip
    = forall a.
      (Eq a, Show a, ToField a, FromField a) =>
      SomeRoundTrip String (Proxy a) (Gen a) (a -> [a])

roundTripTests :: TestTree
roundTripTests =
    withResource
        (open ":memory:")
        close
        \getConn ->
            testGroup
                "round-trip properties"
                (fmap (mkRoundTripTest getConn) specs)

mkRoundTripTest :: IO Connection -> SomeRoundTrip -> TestTree
mkRoundTripTest getConn (SomeRoundTrip label proxy gen shrinker) =
            testProperty label $
                forAllShrink gen shrinker (roundTripProperty getConn proxy)

roundTripProperty ::
    forall a.
    (Eq a, Show a, ToField a, FromField a) =>
    IO Connection ->
    Proxy a ->
    a ->
    Property
roundTripProperty getConn _ value =
    ioProperty do
        conn <- getConn
        rows <- query conn "SELECT ?" (Only value)
        case rows of
            [Only out] ->
                pure $
                    counterexample
                        ("expected " ++ show value ++ ", got " ++ show out)
                        (out === value)
            other ->
                fail $
                    "unexpected row count during round-trip: "
                        ++ show (length other)

specs :: [SomeRoundTrip]
specs =
    [ SomeRoundTrip "Null" (Proxy :: Proxy Null) (pure Null) shrinkNone
    , SomeRoundTrip "Bool" (Proxy :: Proxy Bool) (arbitrary :: Gen Bool) shrink
    , SomeRoundTrip "Int" (Proxy :: Proxy Int) (arbitrary :: Gen Int) shrink
    , SomeRoundTrip "Int8" (Proxy :: Proxy Int8) (arbitrary :: Gen Int8) shrink
    , SomeRoundTrip "Int16" (Proxy :: Proxy Int16) (arbitrary :: Gen Int16) shrink
    , SomeRoundTrip "Int32" (Proxy :: Proxy Int32) (arbitrary :: Gen Int32) shrink
    , SomeRoundTrip "Int64" (Proxy :: Proxy Int64) (arbitrary :: Gen Int64) shrink
    , SomeRoundTrip "Integer" (Proxy :: Proxy Integer) (arbitrary :: Gen Integer) shrink
    , SomeRoundTrip "BigNum" (Proxy :: Proxy BigNum) genBigNum shrinkBigNum
    , SomeRoundTrip "Natural" (Proxy :: Proxy Natural) genNatural shrinkNatural
    , SomeRoundTrip "Word" (Proxy :: Proxy Word) (arbitrary :: Gen Word) shrink
    , SomeRoundTrip "Word8" (Proxy :: Proxy Word8) (arbitrary :: Gen Word8) shrink
    , SomeRoundTrip "Word16" (Proxy :: Proxy Word16) (arbitrary :: Gen Word16) shrink
    , SomeRoundTrip "Word32" (Proxy :: Proxy Word32) (arbitrary :: Gen Word32) shrink
    , SomeRoundTrip "Word64" (Proxy :: Proxy Word64) (arbitrary :: Gen Word64) shrink
    , SomeRoundTrip "Float" (Proxy :: Proxy Float) genFiniteFloat shrink
    , SomeRoundTrip "Double" (Proxy :: Proxy Double) genFiniteDouble shrink
    , SomeRoundTrip "String" (Proxy :: Proxy String) genStringNoNul shrinkStringNoNul
    , SomeRoundTrip "Text" (Proxy :: Proxy Text.Text) genTextNoNul shrinkTextNoNul
    , SomeRoundTrip "ByteString" (Proxy :: Proxy BS.ByteString) genByteString shrinkByteString
    , SomeRoundTrip "BitString" (Proxy :: Proxy BitString) genBitString shrinkBitString
    , SomeRoundTrip "UUID" (Proxy :: Proxy UUID) genUUID shrinkNone
    , SomeRoundTrip "Day" (Proxy :: Proxy Day) genDay shrinkDay
    , SomeRoundTrip "TimeOfDay" (Proxy :: Proxy TimeOfDay) genTimeOfDay shrinkTimeOfDay
    , SomeRoundTrip "LocalTime" (Proxy :: Proxy LocalTime) genLocalTime shrinkLocalTime
    , SomeRoundTrip "UTCTime" (Proxy :: Proxy UTCTime) genUTCTime shrinkUTCTime
    , SomeRoundTrip "Maybe Int" (Proxy :: Proxy (Maybe Int)) genMaybeInt shrinkMaybeInt
    , SomeRoundTrip "Array Int Int" (Proxy :: Proxy (Array Int Int)) genIntArray shrinkIntArray
    , SomeRoundTrip "StructValue FieldValue" (Proxy :: Proxy (StructValue FieldValue)) genStructValue shrinkStructValue
    , SomeRoundTrip "UnionValue FieldValue" (Proxy :: Proxy (UnionValue FieldValue)) genUnionValue shrinkNone
    ]

shrinkNone :: a -> [a]
shrinkNone _ = []

genNatural :: Gen Natural
genNatural = fromInteger <$> arbitrarySizedNatural

shrinkNatural :: Natural -> [Natural]
shrinkNatural n = fromInteger <$> shrinkIntegral (toInteger n)

genBigNum :: Gen BigNum
genBigNum = BigNum <$> scale (\s -> s * 8) (arbitrary :: Gen Integer)

shrinkBigNum :: BigNum -> [BigNum]
shrinkBigNum (BigNum n) = BigNum <$> shrinkIntegral n

genFiniteFloat :: Gen Float
genFiniteFloat =
    (arbitrary :: Gen Float)
        `suchThat` \x -> not (isNaN x || isInfinite x)

genFiniteDouble :: Gen Double
genFiniteDouble =
    (arbitrary :: Gen Double)
        `suchThat` \x -> not (isNaN x || isInfinite x)

genStringNoNul :: Gen String
genStringNoNul =
    sized \n -> do
        len <- chooseInt (0, max 0 (min n 32))
        vectorOf len genCharNoNul

shrinkStringNoNul :: String -> [String]
shrinkStringNoNul =
    filter (all (/= '\0')) . shrink

genCharNoNul :: Gen Char
genCharNoNul =
    (arbitrary :: Gen Char) `suchThat` (/= '\0')

genTextNoNul :: Gen Text.Text
genTextNoNul = Text.pack <$> genStringNoNul

shrinkTextNoNul :: Text.Text -> [Text.Text]
shrinkTextNoNul txt = Text.pack <$> shrinkStringNoNul (Text.unpack txt)

genByteString :: Gen BS.ByteString
genByteString =
    sized \n -> do
        len <- chooseInt (0, max 0 (min n 64))
        BS.pack <$> vectorOf len (arbitrary :: Gen Word8)

shrinkByteString :: BS.ByteString -> [BS.ByteString]
shrinkByteString bs =
    [ BS.pack xs
    | xs <- shrink (BS.unpack bs)
    , length xs <= 64
    ]

genBitString :: Gen BitString
genBitString =
    sized \n -> do
        len <- chooseInt (1, max 1 (min 64 (n * 2)))
        bytes <- vectorOf len (arbitrary :: Gen Word8)
        pure (BitString 0 (BS.pack bytes))

shrinkBitString :: BitString -> [BitString]
shrinkBitString (BitString _ raw) =
    [ BitString 0 (BS.pack xs)
    | xs <- shrink (BS.unpack raw)
    , length xs <= 64
    , not (null xs)
    ]

genUUID :: Gen UUID
genUUID =
    UUID.fromWords
        <$> (arbitrary :: Gen Word32)
        <*> (arbitrary :: Gen Word32)
        <*> (arbitrary :: Gen Word32)
        <*> (arbitrary :: Gen Word32)

genMaybeInt :: Gen (Maybe Int)
genMaybeInt =
    sized \n ->
        let bound = max 1 (min 1000 (n * 10))
         in frequency
                [ (1, pure Nothing)
                , (3, Just <$> chooseInt (-bound, bound))
                ]

shrinkMaybeInt :: Maybe Int -> [Maybe Int]
shrinkMaybeInt Nothing = []
shrinkMaybeInt (Just x) = Nothing : [Just y | y <- shrink x]

genDay :: Gen Day
genDay = ModifiedJulianDay <$> (arbitrary :: Gen Integer)

shrinkDay :: Day -> [Day]
shrinkDay (ModifiedJulianDay d) = ModifiedJulianDay <$> shrink d

genTimeOfDay :: Gen TimeOfDay
genTimeOfDay = do
    micros <- chooseInteger (0, 86400 * 1000000 - 1)
    pure (microsecondsToTimeOfDay micros)

shrinkTimeOfDay :: TimeOfDay -> [TimeOfDay]
shrinkTimeOfDay tod =
    [ microsecondsToTimeOfDay us
    | us <- shrinkIntegral (timeOfDayToMicroseconds tod)
    , us >= 0
    ]

genLocalTime :: Gen LocalTime
genLocalTime = LocalTime <$> genDay <*> genTimeOfDay

shrinkLocalTime :: LocalTime -> [LocalTime]
shrinkLocalTime LocalTime{localDay, localTimeOfDay} =
    [ LocalTime day' localTimeOfDay
    | day' <- shrinkDay localDay
    ]
        ++ [ LocalTime localDay time'
           | time' <- shrinkTimeOfDay localTimeOfDay
           ]

genUTCTime :: Gen UTCTime
genUTCTime = do
    day <- genDay
    micros <- chooseInteger (0, 86400 * 1000000 - 1)
    pure (UTCTime day (picosecondsToDiffTime (micros * 1000000)))

shrinkUTCTime :: UTCTime -> [UTCTime]
shrinkUTCTime (UTCTime day diff) =
    [UTCTime day' diff | day' <- shrinkDay day]
        ++ [UTCTime day diff' | diff' <- shrinkDiff diff]
  where
    shrinkDiff dt =
        [ picosecondsToDiffTime ps
        | ps <- shrinkIntegral (diffTimeToPicoseconds dt)
        , ps >= 0
        , ps `mod` 1000000 == 0
        ]

genIntArray :: Gen (Array Int Int)
genIntArray =
    sized \n -> do
        size <- chooseInt (0, max 0 (min n 8))
        values <- vectorOf size (arbitrary :: Gen Int)
        pure $
            if size == 0
                then listArray (1, 0) []
                else listArray (0, size - 1) values

shrinkIntArray :: Array Int Int -> [Array Int Int]
shrinkIntArray arr =
    let values = elems arr
     in [ listArray (0, length xs - 1) xs
        | xs <- shrink values
        , not (null xs)
        ]
            ++ if null values then [] else [listArray (1, 0) []]

genStructValue :: Gen (StructValue FieldValue)
genStructValue = do
    count <- chooseInt (0, 3)
    if count == 0
        then pure emptyStruct
        else do
            types <- vectorOf count genSimpleType
            values <- forM types simpleTypeValue
            let names = [Text.pack ("field" ++ show i) | i <- [0 .. count - 1]]
                fieldArr =
                    listArray
                        (0, count - 1)
                        [ StructField{structFieldName = name, structFieldValue = val}
                        | (name, val) <- zip names values
                        ]
                typeArr =
                    listArray
                        (0, count - 1)
                        [ StructField{structFieldName = name, structFieldValue = simpleTypeRep ty}
                        | (name, ty) <- zip names types
                        ]
                indexMap = Map.fromList (zip names [0 ..])
            pure
                StructValue
                    { structValueFields = fieldArr
                    , structValueTypes = typeArr
                    , structValueIndex = indexMap
                    }

shrinkStructValue :: StructValue FieldValue -> [StructValue FieldValue]
shrinkStructValue StructValue{structValueFields}
    | null (elems structValueFields) = []
    | otherwise = [emptyStruct]

genUnionValue :: Gen (UnionValue FieldValue)
genUnionValue = do
    memberCount <- chooseInt (1, 4)
    types <- vectorOf memberCount genSimpleType
    let names = [Text.pack ("alt" ++ show i) | i <- [0 .. memberCount - 1]]
        members =
            listArray
                (0, memberCount - 1)
                [ UnionMemberType{unionMemberName = name, unionMemberType = simpleTypeRep ty}
                | (name, ty) <- zip names types
                ]
    tag <- chooseInt (0, memberCount - 1)
    payload <- simpleTypeValue (types !! tag)
    pure
        UnionValue
            { unionValueIndex = fromIntegral tag
            , unionValueLabel = names !! tag
            , unionValuePayload = payload
            , unionValueMembers = members
            }

emptyStruct :: StructValue FieldValue
emptyStruct =
    StructValue
        { structValueFields = listArray (1, 0) []
        , structValueTypes = listArray (1, 0) []
        , structValueIndex = Map.empty
        }

data SimpleType
    = SimpleBool
    | SimpleInt32
    | SimpleDouble
    | SimpleText
    | SimpleBlob
    deriving (Eq, Show)

genSimpleType :: Gen SimpleType
genSimpleType =
    elements [SimpleBool, SimpleInt32, SimpleDouble, SimpleText, SimpleBlob]

simpleTypeRep :: SimpleType -> LogicalTypeRep
simpleTypeRep = \case
    SimpleBool -> LogicalTypeScalar DuckDBTypeBoolean
    SimpleInt32 -> LogicalTypeScalar DuckDBTypeInteger
    SimpleDouble -> LogicalTypeScalar DuckDBTypeDouble
    SimpleText -> LogicalTypeScalar DuckDBTypeVarchar
    SimpleBlob -> LogicalTypeScalar DuckDBTypeBlob

simpleTypeValue :: SimpleType -> Gen FieldValue
simpleTypeValue = \case
    SimpleBool -> FieldBool <$> (arbitrary :: Gen Bool)
    SimpleInt32 -> FieldInt32 <$> (arbitrary :: Gen Int32)
    SimpleDouble -> FieldDouble <$> genFiniteDouble
    SimpleText -> FieldText <$> genTextNoNul
    SimpleBlob -> FieldBlob <$> genByteString

timeOfDayToMicroseconds :: TimeOfDay -> Integer
timeOfDayToMicroseconds tod =
    diffTimeToPicoseconds (timeOfDayToTime tod) `div` 1000000

microsecondsToTimeOfDay :: Integer -> TimeOfDay
microsecondsToTimeOfDay micros =
    let (hours, remHour) = micros `divMod` (3600 * 1000000)
        (minutes, remMinute) = remHour `divMod` (60 * 1000000)
        (seconds, fracMicros) = remMinute `divMod` 1000000
        pico = seconds * 1000000000000 + fracMicros * 1000000
     in TimeOfDay (fromInteger hours) (fromInteger minutes) (MkFixed pico)
