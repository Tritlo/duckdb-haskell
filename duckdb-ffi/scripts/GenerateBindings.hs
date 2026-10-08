{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Generate and verify release bindings from typed hs-bindgen declarations.
module Main (main) where

import Control.Monad (forM_, unless)
import Data.Aeson (Value, object, (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Default (def)
import Data.Foldable (toList)
import Data.List (intercalate)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, mapMaybe)
import Data.Set qualified as Set
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as Text
import HsBindgen
import HsBindgen.Artefact
import HsBindgen.Backend.Category
import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.Name qualified as Name
import HsBindgen.Backend.Hs.Origin qualified as Origin
import HsBindgen.Backend.Hs.Translation.Field qualified as Field
import HsBindgen.BindingSpec (BindingSpecConfig (..))
import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Config (Config_ (..), FieldNamingStrategy (..), UniqueId (..), toBindgenConfig)
import HsBindgen.Config.ClangArgs
import HsBindgen.Frontend.Pass.Final (Final)
import HsBindgen.Frontend.Pass.Parse.IsPass (Parse)
import HsBindgen.Frontend.Pass.Parse.Result
import HsBindgen.Frontend.Predicate
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Hs qualified as Type
import HsBindgen.IR.Translation (DeclIdPair (..), ScopedNamePair (..), TranslatedTypes (..))
import HsBindgen.Instances qualified as Inst
import HsBindgen.Language.Haskell (Name (..), SomeName (..))
import HsBindgen.Macro qualified as Macro
import HsBindgen.Util.Tracer
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath ((</>))

-- | Compare target output and write both generated release files.
main :: IO ()
main = do
    arguments <- getArgs
    case arguments of
        [scratchDirectory] -> generate scratchDirectory
        _ -> die "Usage: GenerateBindings SCRATCH_DIR"

-- | Run the library with the DuckDB naming and selection settings.
runBindgen :: String -> Maybe FilePath -> [C.UncheckedRootDirective] -> Artefact Macro.CExpr a -> IO a
runBindgen target specification directives =
    hsBindgenMacroLang (pure . Macro.cExpr) quiet quiet bindgenConfig directives
  where
    config :: Config_ FilePath
    config =
        def
            { clang =
                def
                    { extraIncludeDirs = ["cbits"]
                    , argsAfter =
                        if target == "native"
                            then []
                            else ["--target=" <> target, "-ffreestanding"]
                    }
            , fieldNamingStrategy = OmitFieldPrefixes
            , bindingSpec = def{prescriptiveBindingSpec = specification}
            , selectionPredicate =
                BAnd def
                    $ BNot
                    $ BIf
                    $ SelectDecl
                    $ DeclNameMatches "^macro DUCKDB_API_VERSION_(AT_LEAST|BELOW)$"
            }
    categories =
        useSafeCategory
            { cSafe = IncludeTermCategory (RenameTerm ("c_" <>))
            }
    bindgenConfig = toBindgenConfig config (UniqueId "duckdb-ffi") "Database.DuckDB.FFI" categories
    quiet = def{verbosity = Verbosity Error}

-- | Generate all supported targets and check their native ABI values.
generate :: FilePath -> IO ()
generate scratchDirectory = do
    let target = "x86_64-unknown-linux-gnu"
        directives = map C.DirectiveHashInclude ["duckdb.h", "duckdb_arrow.h"]
        specificationPath = scratchDirectory </> "types.json"
    specification <- runBindgen target Nothing directives generationSpecification
    LBS.writeFile specificationPath (Aeson.encode specification)
    (bindings, result) <- runBindgen target (Just specificationPath) directives $ do
        bindings <- getBindings def
        checks <- getABIChecks
        pure (bindings, checks)
    (checks, functions) <- either die pure result
    forM_ ["aarch64-unknown-linux-gnu", "x86_64-apple-macos10.13", "arm64-apple-macos11", "native"] $ \otherTarget -> do
        otherBindings <- runBindgen otherTarget (Just specificationPath) directives (getBindings def)
        unless (bindings == otherBindings) $
            die ("Generated bindings differ for " <> otherTarget)
    let probePath = scratchDirectory </> "abi.h"
        probe =
            "enum {\n"
                <> intercalate ",\n" [probeName index <> " = " <> expression | (index, (expression, _, _)) <- zip [0 ..] checks]
                <> "\n};\n"
    writeFile probePath probe
    parsed <- runBindgen target Nothing (directives <> [C.DirectiveHashInclude (fromString probePath)]) (FrontendPassA ParsePass)
    (values, nativeFunctions) <- either die pure (parseABIProbe parsed)
    unless (Set.fromList functions == Set.fromList nativeFunctions) $
        die "Generated bindings do not contain every native function."
    assertions <- sequence [assertion values index check | (index, check) <- zip [0 ..] checks]
    createDirectoryIfMissing True "src/Database/DuckDB"
    writeFile "src/Database/DuckDB/FFI.hs" bindings
    writeFile "cbits/abi-checks.c" (abiHeader <> unlines assertions)

-- | Name one enum constant in the native ABI probe.
probeName :: Int -> String
probeName index = "hs_bindgen_abi_" <> show index

-- | Check the generated layout value before writing its C assertion.
assertion :: Map.Map String Integer -> Int -> (String, String, Maybe Integer) -> IO String
assertion values index (expression, label, expected) = do
    value <- maybe (die ("Clang did not evaluate " <> expression)) pure (Map.lookup (probeName index) values)
    unless (maybe True (== value) expected) $
        die ("Clang and generated bindings disagree for " <> label)
    pure ("_Static_assert(" <> expression <> " == " <> show value <> ", \"duckdb-ffi ABI: " <> label <> "\");")

-- | Reject platforms outside the verified ABI before compiling assertions.
abiHeader :: String
abiHeader =
    "/* Generated by scripts/generate-bindings.sh. Do not edit. */\n"
        <> "#include \"duckdb.h\"\n#include \"duckdb_arrow.h\"\n"
        <> "#if !(defined(__linux__) || (defined(__APPLE__) && defined(__ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__))) || !(defined(__x86_64__) || defined(__aarch64__))\n"
        <> "#error \"duckdb-ffi supports Linux and macOS on x86_64 and aarch64.\"\n"
        <> "#endif\n"

-- | Derive the generation policy from parsed C declarations.
generationSpecification :: Artefact l Value
generationSpecification = do
    declarations <- getReifiedC
    aliases <- getSquashedTypes
    mainHeaders <- getGetMainHeaders
    let callbacks =
            Set.fromList
                [ declaration.info.id.hsName.text
                | declaration <- declarations
                , C.DeclTypedef typedef <- [declaration.kind]
                , isJust (C.getFirstFunTypeIndirection typedef.typ.c)
                ]
        pointees =
            Set.fromList
                [ reference.cName
                | declaration <- declarations
                , "duckdb_" `Text.isPrefixOf` declaration.info.id.cName.name.text
                , C.DeclTypedef typedef <- [declaration.kind]
                , C.TypePointers 1 (C.TypeRef reference) <- [C.getCanonicalType typedef.typ.c]
                ]
        opaque =
            Set.fromList
                [ declaration.info.id.cName
                | declaration <- declarations
                , declaration.info.id.cName `Set.member` pointees
                , isHandle declaration
                ]
        types =
            [ (declaration.info.id.cName, map (.path) (NonEmpty.toList header.mainHeaders), declaration.info.id.hsName.text)
            | declaration <- declarations
            , isType declaration
            , C.FromHeader header <- [declaration.info.origin]
            ]
                ++ [ (cName, either error (map (.path) . NonEmpty.toList) (mainHeaders path), hsName.text)
                   | (cName, (path, hsName)) <- aliases
                   ]
        renames =
            Map.fromList $
                mapMaybe
                    ( \(cName, _, hsName) ->
                        (\name -> (hsName, name)) <$> publicTypeName cName (hsName `Set.member` callbacks)
                    )
                    types
        names = Map.elems renames
        entries =
            [ (cName, headers, Map.findWithDefault hsName hsName renames)
            | (cName, headers, hsName) <- types
            , Map.member hsName renames || cName `Set.member` opaque
            ]
    if length names /= Set.size (Set.fromList names)
        then error "The public type naming rules produce duplicate names."
        else
            pure $
                object
                    [ "version"
                        .= object
                            [ "binding_specification" .= show BindingSpec.currentBindingSpecVersion
                            , "hs_bindgen" .= ("1.0.0.0" :: Text)
                            ]
                    , "ctypes"
                        .= [ object ["cname" .= C.renderDeclId cName, "headers" .= headers, "hsname" .= hsName]
                           | (cName, headers, hsName) <- entries
                           ]
                    , "hstypes"
                        .= [ object ["hsname" .= hsName, "representation" .= ("emptydata" :: Text)]
                           | (cName, _, hsName) <- entries
                           , cName `Set.member` opaque
                           ]
                    ]

-- | Keep native handles opaque when their only field is an implementation pointer.
isHandle :: C.Decl l Final -> Bool
isHandle declaration =
    "_duckdb_" `Text.isPrefixOf` declaration.info.id.cName.name.text
        && case declaration.kind of
            C.DeclStruct structure -> case structure.fields of
                [C.FieldRegular field] ->
                    field.info.name.cName.text == "internal_ptr"
                        && case C.getCanonicalType field.typ.c of
                            C.TypePointers 1 C.TypeVoid -> True
                            _ -> False
                _ -> False
            _ -> False

-- | Select declarations that generate Haskell types.
isType :: C.Decl l Final -> Bool
isType declaration = case declaration.kind of
    C.DeclStruct{} -> True
    C.DeclUnion{} -> True
    C.DeclTypedef{} -> True
    C.DeclEnum{} -> True
    C.DeclOpaque{} -> True
    _ -> False

-- | Preserve the public DuckDB type spelling.
publicTypeName :: C.DeclId -> Bool -> Maybe Text
publicTypeName cName callback
    | cName.isUnnamed = Nothing
    | Just name <- lookup original exceptions = Just name
    | Just rest <- Text.stripPrefix "_duckdb_" original = Just (camel rest <> "Struct")
    | Just rest <- Text.stripPrefix "duckdb_" original =
        let base = if callback then maybe rest id (Text.stripSuffix "_t" rest) else rest
            suffix = if callback && not ("_callback" `Text.isSuffixOf` base) then "Fun" else ""
         in Just (camel base <> suffix)
    | otherwise = Nothing
  where
    original = cName.name.text
    camel = ("DuckDB" <>) . Text.concat . map Text.toTitle . Text.splitOn "_"
    exceptions =
        [ ("duckdb_query_progress_type", "DuckDBQueryProgress")
        , ("duckdb_hugeint", "DuckDBHugeInt")
        , ("duckdb_uhugeint", "DuckDBUHugeInt")
        , ("idx_t", "DuckDBIdx")
        , ("sel_t", "DuckDBSel")
        ]

-- | Collect C expressions, optional generated values, and exported functions.
getABIChecks :: Artefact l (Either String ([(String, String, Maybe Integer)], [String]))
getABIChecks = do
    declarations <- concat . toList <$> HsDecls
    aliases <- getSquashedTypes
    pure $ collectChecks declarations [(Text.unpack name.text, cName) | (cName, (_, name)) <- aliases]

-- | Build checks without reading the rendered Haskell source.
collectChecks :: [Hs.Decl l] -> [(String, C.DeclId)] -> Either String ([(String, String, Maybe Integer)], [String])
collectChecks declarations aliases = do
    let descriptions = mapMaybe describeType declarations
        types = Set.fromList [name | (name, _, instances, _) <- descriptions, Inst.StaticSize `Set.member` instances]
        records = Set.fromList [name | (name, _, instances, _) <- descriptions, not (Set.null (Set.fromList [Inst.IsStruct, Inst.IsUnion] `Set.intersection` instances))]
        nativeFields =
            Map.fromList
                [ ((name, Text.unpack fieldInfo.name.hsName.text), Text.unpack fieldInfo.name.cName.text)
                | (name, _, _, fields) <- descriptions
                , name `Set.member` records
                , field <- Field.flattenFields fields
                , let fieldInfo = Field.getFieldInfo field
                ]
        instanceFields =
            [ field
            | Hs.DeclDefineInstance definition <- declarations
            , Hs.InstanceHasCField field <- [definition.instanceDecl]
            , Just name <- [typeName field.parentType]
            , name `Set.member` records
            ]
        fieldKeys = [(name, Text.unpack field.fieldName.text) | field <- instanceFields, Just name <- [typeName field.parentType]]
        layouts = Map.fromList $ mapMaybe staticLayout declarations
        namedTypes =
            foldl
                chooseName
                Map.empty
                [ (name, Text.unpack (C.renderDeclNameC cName.name))
                | (name, cName) <- [(name, info.id.cName) | (name, info, _, _) <- descriptions] ++ aliases
                , name `Set.member` types
                , not cName.isUnnamed
                ]
        functions = [Name.termNameToStr function.name | Hs.DeclFunction function <- declarations, Name.ExportedName _ <- [function.name]]
    unless (Set.fromList fieldKeys == Map.keysSet nativeFields && length fieldKeys == Set.size (Set.fromList fieldKeys)) $
        Left "Cannot map every generated record field to its C declaration."
    fields <- traverse (resolveField nativeFields) instanceFields
    let cTypes = expandNames types fields namedTypes
    unless (Map.keysSet cTypes == types) $
        Left ("Cannot map generated types to C: " <> show (Set.toList (types Set.\\ Map.keysSet cTypes)))
    unless (records `Set.isSubsetOf` Map.keysSet layouts) $
        Left "Cannot read every generated record layout."
    unless (length functions == Set.size (Set.fromList functions)) $
        Left "The generated bindings contain duplicate function names."
    let typeChecks =
            concat
                [ [ ("sizeof(" <> cType <> ")", name <> ": size", fst <$> Map.lookup name layouts)
                  , ("_Alignof(" <> cType <> ")", name <> ": alignment", snd <$> Map.lookup name layouts)
                  ]
                | name <- Set.toAscList types
                , let cType = cTypes Map.! name
                ]
        fieldChecks =
            concat
                [ [ ("__builtin_offsetof(" <> cType <> ", " <> field <> ")", owner <> "." <> field <> ": offset", Just offset)
                  , ("sizeof(((" <> cType <> " *)0)->" <> field <> ")", owner <> "." <> field <> ": size", Nothing)
                  ]
                | (owner, field, _, offset) <- fields
                , let cType = cTypes Map.! owner
                ]
    pure (typeChecks ++ fieldChecks, functions)
  where
    chooseName :: Map String String -> (String, String) -> Map String String
    chooseName names (name, cName)
        | Map.notMember name names || ' ' `notElem` cName = Map.insert name cName names
        | otherwise = names

-- | Read a generated type's origin, instances, and native record fields.
describeType :: Hs.Decl l -> Maybe (String, C.DeclInfo Final, Set.Set Inst.TypeClass, [C.Field Final])
describeType = \case
    Hs.DeclData structure -> do
        origin <- structure.origin
        let Origin.Struct native = origin.kind
        pure (Text.unpack structure.name.text, origin.info, structure.instances, native.fields)
    Hs.DeclEmpty empty ->
        Just (Text.unpack empty.name.text, empty.origin.info, empty.instances, [])
    Hs.DeclNewtype newtype' -> case newtype'.origin.kind of
        Origin.Aux _ -> Nothing
        Origin.Union native -> Just (Text.unpack newtype'.name.text, newtype'.origin.info, newtype'.instances, native.fields)
        _ -> Just (Text.unpack newtype'.name.text, newtype'.origin.info, newtype'.instances, [])
    _ -> Nothing

-- | Read explicit layout constants and layouts derived via byte arrays.
staticLayout :: Hs.Decl l -> Maybe (String, (Integer, Integer))
staticLayout = \case
    Hs.DeclDefineInstance definition -> case definition.instanceDecl of
        Hs.InstanceStaticSize name layout -> Just (Text.unpack name.text, (toInteger layout.staticSizeOf, toInteger layout.staticAlignment))
        _ -> Nothing
    Hs.DeclDeriveInstance instance' -> case (instance'.clss, instance'.strategy) of
        (Inst.StaticSize, Hs.DeriveVia (Type.SizedByteArray size alignment)) -> Just (Text.unpack instance'.name.text, (toInteger size, toInteger alignment))
        _ -> Nothing
    _ -> Nothing

-- | Read a generated type reference.
typeName :: Type.Type -> Maybe String
typeName = \case
    Type.TypRef name _ -> Just (Text.unpack name.text)
    _ -> Nothing

-- | Resolve a generated field to its native field name.
resolveField :: Map (String, String) String -> Hs.HasCFieldInstance -> Either String (String, String, Maybe String, Integer)
resolveField names field = do
    owner <- maybe (Left "A generated field has no parent type reference.") Right (typeName field.parentType)
    native <- maybe (Left ("Cannot read the C field name for " <> owner)) Right (Map.lookup (owner, Text.unpack field.fieldName.text) names)
    pure (owner, native, typeName field.cFieldType, toInteger field.fieldOffset)

-- | Name anonymous nested types through a native record field.
expandNames :: Set.Set String -> [(String, String, Maybe String, Integer)] -> Map String String -> Map String String
expandNames types fields names
    | expanded == names = names
    | otherwise = expandNames types fields expanded
  where
    expanded = foldl addName names fields
    addName current (owner, field, child, _) = case (Map.lookup owner current, child) of
        (Just parent, Just name)
            | name `Set.member` types && Map.notMember name current ->
                Map.insert name ("__typeof__(((" <> parent <> " *)0)->" <> field <> ")") current
        _ -> current

-- | Read evaluated enum constants and native function names from the ABI probe.
parseABIProbe :: [ParseResult l Parse] -> Either String (Map String Integer, [String])
parseABIProbe parsed = do
    unless (null failures) $ Left ("Cannot parse native declarations: " <> show failures)
    unless (length values == Map.size constants) $ Left "The ABI probe contains duplicate constants."
    pure (constants, Set.toAscList (Set.fromList functions))
  where
    declarations = mapMaybe getParseResultMaybeDecl parsed
    failures =
        [ Text.unpack (C.renderDeclNameC name)
        | result <- parsed
        , Just name <- [C.prelimDeclIdSourceName result.id]
        , name.kind == C.NameKindOrdinary
        , "duckdb_" `Text.isPrefixOf` name.text || "hs_bindgen_abi_" `Text.isPrefixOf` name.text
        , case result.classification of
            ParseResultSuccess _ -> False
            _ -> True
        ]
    values =
        [ (Text.unpack constant.info.name.text, constant.value)
        | declaration <- declarations
        , C.DeclEnum enum <- [declaration.kind]
        , constant <- enum.constants
        , "hs_bindgen_abi_" `Text.isPrefixOf` constant.info.name.text
        ]
    constants = Map.fromList values
    functions =
        [ "c_" <> Text.unpack name.text
        | declaration <- declarations
        , C.DeclFunction _ <- [declaration.kind]
        , Just name <- [C.prelimDeclIdSourceName declaration.info.id]
        , "duckdb_" `Text.isPrefixOf` name.text
        ]
