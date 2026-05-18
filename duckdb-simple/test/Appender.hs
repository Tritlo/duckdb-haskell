{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Appender (appenderTests) where

import Data.Proxy (Proxy (..))
import Data.Time.Calendar (pattern YearMonthDay)
import Data.Time.Clock (UTCTime (..))
import Database.DuckDB.Simple
import GHC.Generics (Generic)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ( testCase, (@=?) )
import Data.Text (Text)
import Database.DuckDB.Simple.Appender
import Data.Aeson (Value)
import qualified Data.Aeson as A
import Data.Map ( Map )
import qualified Data.Map as Map

data LogLine =
  LogLine
  { timestamp :: UTCTime
  , message :: Text
  , severity :: Severity
  , payload :: Payload
  }
  deriving stock (Generic, Show)
  deriving anyclass (AppendTableRow)
  deriving (AppenderDuckValue) via (ViaDuckStruct LogLine)

data Payload =
  PayloadJSON { json :: Value }
  | PayloadBinary { summary :: Text, bytes :: Text }
  | PayloadNested { info :: Maybe Text, nested :: Nested }
  | NoPayload
  | PayloadList { list :: [Text], map :: Map Text Int}
  deriving stock (Generic, Show)
  deriving AppenderDuckValue via (ViaDuckUnion Payload)

data Nested = Nested { foo :: Text, bar :: Int }
  deriving stock (Generic, Show)
  deriving AppenderDuckValue via (ViaDuckStruct Nested)


data Severity = Error | Warning | Info
  deriving stock (Generic, Show, Enum)
  deriving AppenderDuckValue via (ViaDuckEnum Severity)

data SimpleUnion = Foo Int | Bar Text | Baz UTCTime
  deriving stock (Generic)
  deriving AppenderDuckValue via (ViaDuckUnion SimpleUnion)

data SimpleUnion2 = Foo2 { foo2 :: Int } | Bar2 { bar2 :: Text } | Baz2 { baz2 :: UTCTime }
  deriving stock (Generic)
  deriving AppenderDuckValue via (ViaDuckUnion SimpleUnion2)

data SimpleUnion3 = Foo3 { foo3 :: Int, foo3Bool :: Bool } | Bar3 { bar3 :: Text, bar3Int :: Int } | Baz3 { baz3 :: UTCTime } | NoFieldsHere
  deriving stock (Generic)
  deriving AppenderDuckValue via (ViaDuckUnion SimpleUnion3)

data SimpleStruct = SimpleStruct { a :: Int, b :: Bool, c :: UTCTime}
  deriving stock (Generic)
  deriving AppenderDuckValue via (ViaDuckStruct SimpleStruct)

data SimpleEnum = A | B | C | D | Other
  deriving stock (Generic, Show, Enum)
  deriving AppenderDuckValue via (ViaDuckEnum SimpleEnum)


sometime :: UTCTime
sometime = UTCTime (YearMonthDay 2026 05 11) 123

appenderTests :: TestTree
appenderTests =
    testGroup
        "appender"
        [
        testCase "generates a proper table schema" $
                  "CREATE TABLE \"logs\" (\"timestamp\" TIMESTAMPTZ, \n\"message\" VARCHAR, \n\"severity\" ENUM('Error', 'Warning', 'Info'), \n\"payload\" UNION(\"PayloadJSON\" JSON, \"PayloadBinary\" STRUCT(\"summary\" VARCHAR, \"bytes\" VARCHAR), \"PayloadNested\" STRUCT(\"info\" VARCHAR, \"nested\" STRUCT(\"foo\" VARCHAR, \"bar\" BIGINT)), \"NoPayload\" TINYINT, \"PayloadList\" STRUCT(\"list\" VARCHAR[], \"map\" MAP(VARCHAR, BIGINT))))"
                  @=?
                  createTableQuery "logs" (Proxy @LogLine)
        , testCase "schema: union unnamed" $
                "UNION(\"Foo\" BIGINT, \"Bar\" VARCHAR, \"Baz\" TIMESTAMPTZ)" @=? renderDuckTypeName (appenderTypeName $ Proxy @SimpleUnion)
        , testCase "schema: union named " $
                "UNION(\"Foo2\" BIGINT, \"Bar2\" VARCHAR, \"Baz2\" TIMESTAMPTZ)" @=? renderDuckTypeName (appenderTypeName $ Proxy @SimpleUnion2)
        , testCase "schema: union structs" $
                 "UNION(\"Foo3\" STRUCT(\"foo3\" BIGINT, \"foo3Bool\" BOOL), \"Bar3\" STRUCT(\"bar3\" VARCHAR, \"bar3Int\" BIGINT), \"Baz3\" TIMESTAMPTZ, \"NoFieldsHere\" TINYINT)" @=? renderDuckTypeName (appenderTypeName $ Proxy @SimpleUnion3)
        , testCase "schema: structs" $
                  "STRUCT(\"a\" BIGINT, \"b\" BOOL, \"c\" TIMESTAMPTZ)" @=? renderDuckTypeName (appenderTypeName $ Proxy @SimpleStruct)
        , testCase "schema: enums" $
                  "ENUM('A', 'B', 'C', 'D', 'Other')" @=? renderDuckTypeName (appenderTypeName $ Proxy @SimpleEnum)
        , testCase "withTableAppender appends" $
            withConnection ":memory:" \conn -> do
                _ <- execute_ conn $ createTableQuery "logs" (Proxy @LogLine)
                withTableAppender conn "logs" $ \app -> do
                  mapM_ (appendTableRow app)
                    [ LogLine sometime "message A" Error  (PayloadJSON $ A.object ["foo" A..= A.Null, "bar" A..= (123 :: Int)])
                    , LogLine sometime "message B" Info (PayloadBinary "descr" "bytes")
                    , LogLine sometime "message C" Warning (PayloadNested Nothing (Nested "foo" 123))
                    , LogLine sometime "message D" Warning NoPayload
                    , LogLine sometime "message E" Warning (PayloadList ["foo", "bar", "baz"] (Map.fromList [("foo", 2), ("bar", 3)]))
                    ]
                rows <- query_ conn "SELECT COUNT(*) FROM logs" :: IO [Only Int]
                [Only 5] @=? rows
        ]
