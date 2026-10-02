-- | Typed values for DuckDB VARIANT.
module Database.DuckDB.Simple.Variant (Variant (..)) where

import Data.ByteString (ByteString)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Text (Text)
import Data.UUID (UUID)
import Data.Word (Word16, Word32, Word64, Word8)

{- | A VARIANT retains each scalar's native type and each object's entry order.
Dates contain days since 1970-01-01. Times contain ticks since midnight.
Timestamps contain ticks since 1970-01-01. Each constructor states its unit.
Date and timestamp integers retain DuckDB's infinity sentinels.
TIME WITH TIME ZONE contains DuckDB's packed native Word64.
TIMESTAMP WITH TIME ZONE contains UTC microseconds since the epoch.
Intervals contain months, days, and microseconds, in that order.
Decimals contain precision, scale, and the signed unscaled integer.
BIT contains a left-padding count and data bytes without the native header.
The unused high bits in its first data byte are zero.
GEOMETRY contains WKB bytes. Native VARIANT does not retain CRS metadata.
Object keys are case-sensitive. Binding rejects duplicate keys, empty keys,
and keys that contain NUL. Result decoding preserves empty keys and NUL keys.
Root 'VariantNull' is SQL NULL. Nested 'VariantNull' is a null container entry.
The codec supports at most 128 value levels, including the root.
Derived equality uses Float and Double equality. NaN does not equal itself.
Positive and negative floating zero compare equal.
-}
data Variant
    = VariantNull
    | VariantBool !Bool
    | VariantInt8 !Int8
    | VariantInt16 !Int16
    | VariantInt32 !Int32
    | VariantInt64 !Int64
    | VariantHugeInt !Integer
    | VariantWord8 !Word8
    | VariantWord16 !Word16
    | VariantWord32 !Word32
    | VariantWord64 !Word64
    | VariantUHugeInt !Integer
    | VariantFloat !Float
    | VariantDouble !Double
    | VariantDecimal !Word8 !Word8 !Integer
    | VariantText !Text
    | VariantBlob !ByteString
    | VariantUUID !UUID
    | VariantDate !Int32
    | VariantTimeMicros !Int64
    | VariantTimeNanos !Int64
    | VariantTimestampSeconds !Int64
    | VariantTimestampMillis !Int64
    | VariantTimestampMicros !Int64
    | VariantTimestampNanos !Int64
    | VariantTimeTZ !Word64
    | VariantTimestampTZ !Int64
    | VariantInterval !Int32 !Int32 !Int64
    | VariantBigNum !Integer
    | VariantBit !Word8 !ByteString
    | VariantGeometry !ByteString
    | VariantArray [Variant]
    | VariantObject [(Text, Variant)]
    deriving (Eq, Show)
