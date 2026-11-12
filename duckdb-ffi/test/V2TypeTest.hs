module V2TypeTest (tests) where

import Control.Monad (forM_)
import Data.Coerce (coerce)
import Data.Word (Word64)
import Database.DuckDB.FFI.V2
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.C.String (peekCStringLen, withCStringLen)
import Foreign.C.Types (CChar)
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Array (peekArray, withArray)
import Foreign.Marshal.Utils (fillBytes, with)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (Storable (..))
import HsBindgen.Runtime.PtrConst (PtrConst)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import V2Test (checked, consume, output, readView, withBytes, withConnection, withFactory, withHandle, withResult, withView)

tests :: TestTree
tests =
    testGroup
        "DuckDB V2 types and names"
        [ testCase "logical types render as text that parses back to an equal type" testTypeText
        , testCase "type names and parameters resolve back to equal types" testTypeName
        , testCase "common types follow the implicit cast rules" testCommonType
        , testCase "qualified names compare and hash without regard to case" testQualifiedName
        , testCase "identifiers are quoted only where SQL requires it" testIdentifier
        , testCase "UTF-8 validation checks every byte of the view" testUtf8
        ]

withType :: DuckDBV2FactoryHandle -> String -> (DuckDBV2LogicalTypeHandle -> IO a) -> IO a
withType factory text action = withView text $ \view ->
    withHandle (c_duckdb_v2_factory_create_type_from_text factory view) c_duckdb_v2_logical_type_destroy action

isEqual :: DuckDBV2LogicalTypeHandle -> DuckDBV2LogicalTypeHandle -> IO Bool
isEqual left right = (/= 0) <$> output (c_duckdb_v2_logical_type_is_equal left right)

-- | Pass the strings as an array of identifier views.
withParts :: [String] -> (PtrConst DuckDBV2IdentifierT -> IO a) -> IO a
withParts parts action = go parts []
  where
    go [] views = withArray (reverse views) (action . ConstPtr)
    go (part : rest) views = withCStringLen part $ \(ptr, len) ->
        go rest (DuckDBV2IdentifierT (DuckDBV2Str (ConstPtr ptr) (fromIntegral len)) : views)

withQName :: [String] -> (DuckDBV2QnameHandle -> IO a) -> IO a
withQName parts action = withParts parts $ \views ->
    withHandle (c_duckdb_v2_qname_create views (fromIntegral (length parts))) c_duckdb_v2_qname_destroy action

{- | Render text through the buffer protocol of the header.
The call sizes the text first. It then checks that a buffer without room for the terminator is refused and stays unchanged.
-}
render :: (Ptr CChar -> DuckDBIdx -> Ptr DuckDBIdx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error) -> IO String
render renderer = alloca $ \lengthSlot -> do
    checked (renderer nullPtr 0 lengthSlot)
    len <- fromIntegral <$> peek lengthSlot
    allocaBytes (len + 1) $ \buffer -> do
        fillBytes buffer 0x55 (len + 1)
        poke lengthSlot 0
        renderer buffer (fromIntegral len) lengthSlot nullPtr >>= (@?= DuckDBV2ErrorInputObjectSize)
        peek lengthSlot >>= (@?= fromIntegral len)
        peekArray (len + 1) buffer >>= (@?= replicate (len + 1) 0x55)
        checked (renderer buffer (fromIntegral (len + 1)) lengthSlot)
        peek lengthSlot >>= (@?= fromIntegral len)
        peekElemOff buffer len >>= (@?= 0)
        peekCStringLen (buffer, len)

testTypeText :: IO ()
testTypeText = withConnection $ \conn -> withFactory conn $ \factory -> do
    let texts =
            [ "INTEGER"
            , "TIMESTAMP WITH TIME ZONE"
            , "DECIMAL(18,3)"
            , "INTEGER[]"
            , "INTEGER[3]"
            , "STRUCT(a INTEGER, \"b c\" VARCHAR)"
            , "MAP(VARCHAR, INTEGER)"
            , "UNION(i INTEGER, s VARCHAR)"
            , "ENUM('a', 'b''c')"
            ]
    forM_ texts $ \text -> withType factory text $ \ty -> do
        rendered <- render (c_duckdb_v2_logical_type_to_text ty)
        withType factory rendered $ \parsed -> isEqual ty parsed >>= assertBool (text <> " parses back from " <> rendered)
    withType factory "DECIMAL(10, 2)" $ \decimal -> do
        withType factory "DECIMAL(10, 2)" $ \same -> isEqual decimal same >>= (@?= True)
        withType factory "DECIMAL(10, 3)" $ \scale -> isEqual decimal scale >>= (@?= False)
        withType factory "FLOAT" $ \float -> isEqual decimal float >>= (@?= False)
    withType factory "STRUCT(a INTEGER, b VARCHAR)" $ \struct -> do
        withType factory "STRUCT(b VARCHAR, a INTEGER)" $ \reordered -> isEqual struct reordered >>= (@?= False)
        withType factory "STRUCT(a INTEGER, c VARCHAR)" $ \renamed -> isEqual struct renamed >>= (@?= False)
    withHandle (c_duckdb_v2_factory_create_type_from_id factory DuckDBV2LogicalTypeIdAny (coerce nullPtr) (coerce nullPtr) 0) c_duckdb_v2_logical_type_destroy $ \ty ->
        render (c_duckdb_v2_logical_type_to_text ty) >>= (@?= "ANY")

testTypeName :: IO ()
testTypeName = withConnection $ \conn -> withFactory conn $ \factory -> do
    let fromName name values action = withArray values $ \params ->
            withHandle (c_duckdb_v2_factory_create_type_from_name factory name (coerce nullPtr) (ConstPtr params) (fromIntegral (length values))) c_duckdb_v2_logical_type_destroy action
        nameOf ty = output (c_duckdb_v2_logical_type_get_name ty) >>= readView . coerce
    forM_ ["INTEGER", "TIMESTAMP WITH TIME ZONE", "VARCHAR", "UUID", "INTERVAL"] $ \text -> withType factory text $ \ty -> do
        name <- nameOf ty
        assertBool "the name is never empty" (not (null name))
        withQName [name] $ \qname -> fromName qname [] $ \resolved -> isEqual ty resolved >>= assertBool (text <> " resolves from its name " <> name)
    -- The parameters of a parameterized kind cross as values, child types as TYPE values.
    withType factory "DECIMAL(18,3)" $ \decimal -> do
        name <- nameOf decimal
        withHandle (c_duckdb_v2_value_create_utinyint factory 18) c_duckdb_v2_value_destroy $ \width ->
            withHandle (c_duckdb_v2_value_create_utinyint factory 3) c_duckdb_v2_value_destroy $ \scale ->
                withQName [name] $ \qname -> fromName qname [width, scale] $ \resolved -> isEqual decimal resolved >>= (@?= True)
    withType factory "INTEGER[]" $ \list -> withType factory "INTEGER" $ \element -> do
        name <- nameOf list
        withHandle (c_duckdb_v2_value_create_type factory element) c_duckdb_v2_value_destroy $ \child ->
            withQName [name] $ \qname -> fromName qname [child] $ \resolved -> isEqual list resolved >>= (@?= True)
    withType factory "INTEGER" $ \base -> withView "v2_alias" $ \alias ->
        withHandle (c_duckdb_v2_factory_create_type_with_alias factory base (coerce alias)) c_duckdb_v2_logical_type_destroy $ \aliased ->
            nameOf aliased >>= (@?= "v2_alias")
    -- A qualified name resolves in the schema that it names.
    withResult conn "CREATE TYPE v2_mood AS ENUM ('sad', 'ok')" consume
    withType factory "v2_mood" $ \mood ->
        withQName ["main", "v2_mood"] $ \qname -> fromName qname [] $ \resolved -> isEqual mood resolved >>= (@?= True)
    let rejected parts values = withQName parts $ \qname -> withArray values $ \params -> alloca $ \out -> do
            poke out (coerce nullPtr)
            code <- c_duckdb_v2_factory_create_type_from_name factory qname (coerce nullPtr) (ConstPtr params) (fromIntegral (length values)) out nullPtr
            c_duckdb_v2_logical_type_destroy out >>= (@?= DuckDBV2ErrorNone)
            assertBool (show parts <> " does not resolve") (code /= DuckDBV2ErrorNone)
    rejected ["v2_no_such_schema", "integer"] []
    rejected ["v2_no_such_type"] []
    withHandle (c_duckdb_v2_value_create_utinyint factory 1) c_duckdb_v2_value_destroy $ \value -> rejected ["integer"] [value]

testCommonType :: IO ()
testCommonType = withConnection $ \conn -> withFactory conn $ \factory -> do
    let common types = withArray types $ \array -> alloca $ \out -> do
            poke out (coerce nullPtr)
            code <- c_duckdb_v2_factory_create_common_type factory (ConstPtr array) (fromIntegral (length types)) out nullPtr
            c_duckdb_v2_logical_type_destroy out >>= (@?= DuckDBV2ErrorNone)
            pure code
        resolvesTo inputs expected = withTypes inputs $ \types -> withArray types $ \array -> withType factory expected $ \wanted ->
            withHandle (c_duckdb_v2_factory_create_common_type factory (ConstPtr array) (fromIntegral (length types))) c_duckdb_v2_logical_type_destroy $ \resolved ->
                isEqual wanted resolved >>= assertBool (show inputs <> " resolve to " <> expected)
        withTypes [] action = action []
        withTypes (text : rest) action = withType factory text $ \ty -> withTypes rest (action . (ty :))
    resolvesTo ["INTEGER", "BIGINT"] "BIGINT"
    resolvesTo ["DECIMAL(4,1)", "DECIMAL(5,4)"] "DECIMAL(7,4)"
    resolvesTo ["VARCHAR"] "VARCHAR"
    common [] >>= (@?= DuckDBV2ErrorInputInvalid)
    withType factory "INTEGER" $ \integer -> do
        common [integer, coerce nullPtr] >>= (@?= DuckDBV2ErrorInputInvalid)
        withHandle (c_duckdb_v2_factory_create_type_from_id factory DuckDBV2LogicalTypeIdAny (coerce nullPtr) (coerce nullPtr) 0) c_duckdb_v2_logical_type_destroy $ \anyType ->
            common [integer, anyType] >>= (@?= DuckDBV2ErrorInputInvalid)
    withTypes ["STRUCT(a INTEGER)", "INTEGER[]"] $ \types -> common types >>= (@?= DuckDBV2ErrorQueryNotImplemented)

testQualifiedName :: IO ()
testQualifiedName = do
    let equals left right = (/= 0) <$> output (c_duckdb_v2_qname_equals left right)
        hash name = output (c_duckdb_v2_qname_hash name) :: IO Word64
    withQName ["Main", "Users"] $ \mixed -> withQName ["main", "USERS"] $ \upper -> do
        equals mixed upper >>= (@?= True)
        (==) <$> hash mixed <*> hash upper >>= assertBool "equal names hash equal"
        withQName ["users"] $ \short -> equals mixed short >>= (@?= False)
        withQName ["main", "orders"] $ \other -> equals mixed other >>= (@?= False)
    withQName ["main", "users"] $ \plain -> render (c_duckdb_v2_qname_render plain) >>= (@?= "main.users")
    -- Parts with a dot, a keyword and a quote need quoting. The rendered text parses back to an equal name.
    withQName ["my.catalog", "select", "a\"b"] $ \name -> do
        text <- render (c_duckdb_v2_qname_render name)
        withView text $ \view -> withHandle (c_duckdb_v2_qname_parse view) c_duckdb_v2_qname_destroy $ \parsed ->
            equals name parsed >>= assertBool ("the name parses back from " <> text)
    let rejected parts = withParts parts $ \views -> alloca $ \out -> do
            poke out (coerce nullPtr)
            code <- c_duckdb_v2_qname_create views (fromIntegral (length parts)) out nullPtr
            c_duckdb_v2_qname_destroy out >>= (@?= DuckDBV2ErrorNone)
            code @?= DuckDBV2ErrorInputInvalid
    rejected []
    rejected ["a", "b", "c", "d"]
    rejected ["main", ""]

testIdentifier :: IO ()
testIdentifier = do
    let quoted name = withView name $ \view -> render (c_duckdb_v2_identifier_render_quoted (coerce view))
    quoted "users" >>= (@?= "users")
    quoted "select" >>= (@?= "\"select\"")
    quoted "a \"b\"" >>= (@?= "\"a \"\"b\"\"\"")
    -- The rendered identifier parses back to a name with the same single part.
    forM_ ["users", "select", "a \"b\"", "x.y"] $ \name -> do
        text <- quoted name
        withView text $ \view -> withHandle (c_duckdb_v2_qname_parse view) c_duckdb_v2_qname_destroy $ \parsed -> do
            output (c_duckdb_v2_qname_get_part_count parsed) >>= (@?= 1)
            output (c_duckdb_v2_qname_get_part parsed 0) >>= readView . coerce >>= (@?= name)

testUtf8 :: IO ()
testUtf8 = do
    let validates bytes = withBytes bytes $ \view -> c_duckdb_v2_validate_utf8 view nullPtr
    validates [] >>= (@?= DuckDBV2ErrorNone)
    validates [0x68, 0xc3, 0xa9, 0x00, 0xe2, 0x82, 0xac] >>= (@?= DuckDBV2ErrorNone)
    validates [0x61, 0x00, 0xff] >>= (@?= DuckDBV2ErrorInputInvalid)
    validates [0x61, 0xc3] >>= (@?= DuckDBV2ErrorInputInvalid)
    validates [0xc0, 0xaf] >>= (@?= DuckDBV2ErrorInputInvalid)
    with (DuckDBV2Str (ConstPtr nullPtr) 0) $ \view -> c_duckdb_v2_validate_utf8 (ConstPtr view) nullPtr >>= (@?= DuckDBV2ErrorNone)
    with (DuckDBV2Str (ConstPtr nullPtr) 1) $ \view -> c_duckdb_v2_validate_utf8 (ConstPtr view) nullPtr >>= (@?= DuckDBV2ErrorInputInvalid)
    c_duckdb_v2_validate_utf8 (ConstPtr nullPtr) nullPtr >>= (@?= DuckDBV2ErrorInputInvalid)
