{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Properties
-- Description : Round-trip properties for parameters and results of each column type.
module Properties (roundTripTests) where

import Control.Monad (forM)
import Data.Array (Array, elems, listArray)
import Data.Bits (clearBit)
import qualified Data.ByteString as BS
import Data.Fixed (Fixed (MkFixed))
import Data.Int (Int16, Int32, Int64, Int8)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.Ratio ((%))
import qualified Data.Text as Text
import Data.Time.Calendar (Day (..), addDays, fromGregorian)
import Data.Time.Clock (UTCTime (..), diffTimeToPicoseconds, picosecondsToDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.LocalTime (LocalTime (..), TimeOfDay (..), timeOfDayToTime, utc, utcToLocalTime)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Data.Word (Word16, Word32, Word64, Word8)
import Database.DuckDB.Simple (Connection, close, open, query)
import Database.DuckDB.Simple.FromField (
  BigNum (..),
  BitString (..),
  FieldValue (..),
  FromField,
  LogicalTypeId (..),
  LogicalTypeRep (..),
  StructField (..),
  StructValue (..),
  UnionMemberType (..),
  UnionValue (..),
 )
import Database.DuckDB.Simple.ToField (ToField)
import Database.DuckDB.Simple.Types (Null (..), Only (..))
import GHC.Float (castWord32ToFloat, castWord64ToDouble)
import Numeric.Natural (Natural)
import Test.QuickCheck (
  Gen,
  Property,
  arbitrary,
  chooseInt,
  chooseInteger,
  counterexample,
  elements,
  forAllShrink,
  frequency,
  ioProperty,
  shrink,
  shrinkIntegral,
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
  testGroup
    "round-trip properties"
    (fmap mkRoundTripTest specs)

mkRoundTripTest :: SomeRoundTrip -> TestTree
mkRoundTripTest (SomeRoundTrip label proxy gen shrinker) =
  withResource
    (open ":memory:")
    close
    \getConn ->
      testProperty label $
        forAllShrink gen shrinker (roundTripProperty getConn proxy)

roundTripProperty
  :: forall a
   . (Eq a, Show a, ToField a, FromField a)
  => IO Connection
  -> Proxy a
  -> a
  -> Property
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
  , SomeRoundTrip "Int" (Proxy :: Proxy Int) genBounded shrink
  , SomeRoundTrip "Int8" (Proxy :: Proxy Int8) genBounded shrink
  , SomeRoundTrip "Int16" (Proxy :: Proxy Int16) genBounded shrink
  , SomeRoundTrip "Int32" (Proxy :: Proxy Int32) genBounded shrink
  , SomeRoundTrip "Int64" (Proxy :: Proxy Int64) genBounded shrink
  , SomeRoundTrip "Integer" (Proxy :: Proxy Integer) genInteger shrink
  , SomeRoundTrip "BigNum" (Proxy :: Proxy BigNum) genBigNum shrinkBigNum
  , SomeRoundTrip "Natural" (Proxy :: Proxy Natural) genNatural shrinkNatural
  , SomeRoundTrip "Word" (Proxy :: Proxy Word) genBounded shrink
  , SomeRoundTrip "Word8" (Proxy :: Proxy Word8) genBounded shrink
  , SomeRoundTrip "Word16" (Proxy :: Proxy Word16) genBounded shrink
  , SomeRoundTrip "Word32" (Proxy :: Proxy Word32) genBounded shrink
  , SomeRoundTrip "Word64" (Proxy :: Proxy Word64) genBounded shrink
  , SomeRoundTrip "Float" (Proxy :: Proxy Float) genFiniteFloat shrink
  , SomeRoundTrip "Double" (Proxy :: Proxy Double) genFiniteDouble shrink
  , SomeRoundTrip "String" (Proxy :: Proxy String) genString shrinkString
  , SomeRoundTrip "Text" (Proxy :: Proxy Text.Text) genText shrinkText
  , SomeRoundTrip "ByteString" (Proxy :: Proxy BS.ByteString) genByteString shrinkByteString
  , SomeRoundTrip "BitString" (Proxy :: Proxy BitString) genBitString shrinkBitString
  , SomeRoundTrip "UUID" (Proxy :: Proxy UUID) genUUID shrinkNone
  , SomeRoundTrip "Day" (Proxy :: Proxy Day) genDay shrinkDay
  , SomeRoundTrip "TimeOfDay" (Proxy :: Proxy TimeOfDay) genTimeOfDay shrinkTimeOfDay
  , SomeRoundTrip "LocalTime" (Proxy :: Proxy LocalTime) genLocalTime shrinkLocalTime
  , SomeRoundTrip "UTCTime" (Proxy :: Proxy UTCTime) genUTCTime shrinkUTCTime
  , SomeRoundTrip "Maybe Int" (Proxy :: Proxy (Maybe Int)) genMaybeInt shrinkMaybeInt
  , SomeRoundTrip "nonempty Array Int Int" (Proxy :: Proxy (Array Int Int)) genIntArray shrinkIntArray
  , SomeRoundTrip "StructValue FieldValue" (Proxy :: Proxy (StructValue FieldValue)) genStructValue shrinkStructValue
  , SomeRoundTrip "UnionValue FieldValue" (Proxy :: Proxy (UnionValue FieldValue)) genUnionValue shrinkNone
  ]

shrinkNone :: a -> [a]
shrinkNone _ = []

-- | Choose a value from the whole range of a bounded integral type. One value
-- in four is a boundary of the type: the minimum, the maximum, -1, 0, or 1.
genBounded :: forall a. (Bounded a, Integral a) => Gen a
genBounded =
  frequency
    [ (1, elements [minBound, maxBound, fromInteger (max (-1) (toInteger (minBound :: a))), 0, 1])
    , (3, fromInteger <$> chooseInteger (toInteger (minBound :: a), toInteger (maxBound :: a)))
    ]

-- | Choose an integer of at most 200 bits. The boundaries are those of the
-- 64-bit and 128-bit integer types, where the binding changes its native type.
genInteger :: Gen Integer
genInteger =
  frequency
    [ (1, elements integerBoundaries)
    , (1, chooseInteger (-1000, 1000))
    , (2, chooseInteger (negate (2 ^ (200 :: Int)), 2 ^ (200 :: Int)))
    ]
  where
    integerBoundaries =
      concat [[value, negate value] | bits <- [63, 64, 127, 128 :: Int], value <- [2 ^ bits - 1, 2 ^ bits, 2 ^ bits + 1]] <> [0, 1, -1]

genNatural :: Gen Natural
genNatural = fromInteger . abs <$> genInteger

shrinkNatural :: Natural -> [Natural]
shrinkNatural n = fromInteger <$> shrinkIntegral (toInteger n)

genBigNum :: Gen BigNum
genBigNum = BigNum <$> genInteger

shrinkBigNum :: BigNum -> [BigNum]
shrinkBigNum (BigNum n) = BigNum <$> shrinkIntegral n

-- | Choose a finite Float from each bit pattern. The boundaries are the zeros,
-- the largest finite values, the smallest normal value, and the smallest
-- subnormal values.
genFiniteFloat :: Gen Float
genFiniteFloat =
  frequency
    [ (1, elements [0, -0, 1, -1, 3.4028235e38, -3.4028235e38, 1.1754944e-38, 1.0e-45, -1.0e-45])
    , (3, (castWord32ToFloat <$> arbitrary) `suchThat` \x -> not (isNaN x || isInfinite x))
    ]

-- | Choose a finite Double from each bit pattern, with the boundaries of the
-- same kinds as for 'genFiniteFloat'.
genFiniteDouble :: Gen Double
genFiniteDouble =
  frequency
    [ (1, elements [0, -0, 1, -1, 1.7976931348623157e308, -1.7976931348623157e308, 2.2250738585072014e-308, 5.0e-324, -5.0e-324])
    , (3, (castWord64ToDouble <$> arbitrary) `suchThat` \x -> not (isNaN x || isInfinite x))
    ]

-- | Choose a string of at most 300 characters. The boundaries have 0, 1, 11, 12,
-- and 13 ASCII characters, around the 12 bytes that DuckDB stores inline.
genString :: Gen String
genString =
  frequency
    [ (1, elements [replicate count 'x' | count <- [0, 1, 11, 12, 13]])
    , (3, chooseInt (0, 300) >>= \len -> vectorOf len genChar)
    ]

shrinkString :: String -> [String]
shrinkString = shrink

genChar :: Gen Char
genChar =
  frequency [(1, pure '\0'), (9, arbitrary)]

genText :: Gen Text.Text
genText = Text.pack <$> genString

shrinkText :: Text.Text -> [Text.Text]
shrinkText txt = Text.pack <$> shrinkString (Text.unpack txt)

-- | Choose a byte string of at most 300 bytes. The boundaries have 0, 12, and
-- 13 bytes, around the 12 bytes that DuckDB stores inline.
genByteString :: Gen BS.ByteString
genByteString =
  frequency
    [ (1, elements [BS.replicate count byte | count <- [0, 12, 13], byte <- [0, 255]])
    , (3, chooseInt (0, 300) >>= \len -> BS.pack <$> vectorOf len (arbitrary :: Gen Word8))
    ]

shrinkByteString :: BS.ByteString -> [BS.ByteString]
shrinkByteString bs =
  [ BS.pack xs
  | xs <- shrink (BS.unpack bs)
  , length xs <= 300
  ]

-- | Choose a BIT value of 1 to 64 bytes with 0 to 7 bits of padding. The
-- padding bits of the first byte are 0, as the decoder gives them.
genBitString :: Gen BitString
genBitString = do
  len <- chooseInt (1, 64)
  padding <- chooseInt (0, 7)
  first <- arbitrary :: Gen Word8
  rest <- vectorOf (len - 1) (arbitrary :: Gen Word8)
  pure (BitString (fromIntegral padding) (BS.pack (foldl clearBit first [8 - padding .. 7] : rest)))

shrinkBitString :: BitString -> [BitString]
shrinkBitString (BitString _ raw) =
  [ BitString 0 (BS.pack xs)
  | xs <- shrink (BS.unpack raw)
  , length xs <= 64
  , not (null xs)
  ]

-- | Choose a UUID from each bit pattern. The boundaries are the nil UUID and
-- the UUID with each bit set.
genUUID :: Gen UUID
genUUID =
  frequency
    [ (1, elements [UUID.nil, UUID.fromWords maxBound maxBound maxBound maxBound])
    , (3, UUID.fromWords <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary)
    ]

genMaybeInt :: Gen (Maybe Int)
genMaybeInt =
  frequency
    [ (1, pure Nothing)
    , (3, Just <$> genBounded)
    ]

shrinkMaybeInt :: Maybe Int -> [Maybe Int]
shrinkMaybeInt Nothing = []
shrinkMaybeInt (Just x) = Nothing : [Just y | y <- shrink x]

-- | Choose a day from the finite range of DATE: each 32-bit count of days from
-- the Unix epoch, except the two counts that DuckDB uses for the infinities.
-- The boundaries are the ends of the range and the days next to the epoch.
genDay :: Gen Day
genDay = (`addDays` fromGregorian 1970 1 1) <$> genFiniteCount (toInteger (maxBound :: Int32))

-- | Choose a count from minBound to maxBound - 1 of a signed type whose
-- maximum is the argument, except the negated maximum. DuckDB uses the
-- maximum and the negated maximum for the infinities of DATE.
genFiniteCount :: Integer -> Gen Integer
genFiniteCount largest =
  frequency
    [ (1, elements [negate largest - 1, negate largest + 1, -1, 0, 1, largest - 1])
    , (3, chooseInteger (negate largest - 1, largest - 1) `suchThat` (/= negate largest))
    ]

-- | Choose a count of microseconds from the Unix epoch in the range of TIMESTAMP
-- that DuckDB states: from 290309-12-22 (BC) 00:00:00, which is
-- -9223372022400000000, to 294247-01-10 04:00:54.775806, which is maxBound - 1.
-- DuckDB gives a conversion error for a smaller count.
genTimestampMicros :: Gen Integer
genTimestampMicros =
  frequency
    [ (1, elements [smallest, -1, 0, 1, largest])
    , (3, chooseInteger (smallest, largest))
    ]
  where
    smallest = -9223372022400000000
    largest = toInteger (maxBound :: Int64) - 1

shrinkDay :: Day -> [Day]
shrinkDay (ModifiedJulianDay d) = ModifiedJulianDay <$> shrink d

-- | Choose a time of day in microseconds. The boundaries are midnight, the last
-- microsecond of the day, and 24:00:00, which DuckDB also accepts. A parameter
-- with no known type keeps only microseconds.
genTimeOfDay :: Gen TimeOfDay
genTimeOfDay =
  microsecondsToTimeOfDay
    <$> frequency
      [ (1, elements [0, 86400 * 1000000 - 1, 86400 * 1000000])
      , (3, chooseInteger (0, 86400 * 1000000 - 1))
      ]

shrinkTimeOfDay :: TimeOfDay -> [TimeOfDay]
shrinkTimeOfDay tod =
  [ microsecondsToTimeOfDay us
  | us <- shrinkIntegral (timeOfDayToMicroseconds tod)
  , us >= 0
  ]

-- | Choose a local time from the range of TIMESTAMP, with the boundaries of
-- 'genTimestampMicros'.
genLocalTime :: Gen LocalTime
genLocalTime = utcToLocalTime utc <$> genUTCTime

shrinkLocalTime :: LocalTime -> [LocalTime]
shrinkLocalTime LocalTime{localDay, localTimeOfDay} =
  [ LocalTime day' localTimeOfDay
  | day' <- shrinkDay localDay
  ]
    ++ [ LocalTime localDay time'
       | time' <- shrinkTimeOfDay localTimeOfDay
       ]

-- | Choose a UTC time from the range of TIMESTAMPTZ, with the boundaries of
-- 'genTimestampMicros'.
genUTCTime :: Gen UTCTime
genUTCTime = do
  micros <- genTimestampMicros
  pure (posixSecondsToUTCTime (fromRational (micros % 1000000)))

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
genIntArray = do
  size <- chooseInt (1, 32)
  values <- vectorOf size genBounded
  pure (listArray (0, size - 1) values)

shrinkIntArray :: Array Int Int -> [Array Int Int]
shrinkIntArray arr =
  let values = elems arr
  in  [ listArray (0, length xs - 1) xs
      | xs <- shrink values
      , not (null xs)
      ]

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
  SimpleBool -> LogicalTypeScalar LogicalTypeIdBoolean
  SimpleInt32 -> LogicalTypeScalar LogicalTypeIdInteger
  SimpleDouble -> LogicalTypeScalar LogicalTypeIdDouble
  SimpleText -> LogicalTypeScalar LogicalTypeIdVarchar
  SimpleBlob -> LogicalTypeScalar LogicalTypeIdBlob

simpleTypeValue :: SimpleType -> Gen FieldValue
simpleTypeValue = \case
  SimpleBool -> FieldBool <$> (arbitrary :: Gen Bool)
  SimpleInt32 -> FieldInt32 <$> genBounded
  SimpleDouble -> FieldDouble <$> genFiniteDouble
  SimpleText -> FieldText <$> genText
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
  in  TimeOfDay (fromInteger hours) (fromInteger minutes) (MkFixed pico)
