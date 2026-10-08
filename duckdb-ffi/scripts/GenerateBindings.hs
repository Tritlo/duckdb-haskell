{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Generate release sources with the pinned hs-bindgen library.
module Main (main) where

import Data.Default (def)
import System.Environment (getArgs)
import System.Exit (die)

import HsBindgen
import HsBindgen.ArtefactM
import HsBindgen.Backend.Category
import HsBindgen.BindingSpec
import HsBindgen.Config (Config_ (..), FieldNamingStrategy (..), UniqueId (..), toBindgenConfig)
import HsBindgen.Config.ClangArgs
import HsBindgen.Frontend.Predicate
import HsBindgen.IR.C qualified as C
import HsBindgen.Macro qualified as Macro
import HsBindgen.Util.Tracer

-- | Write a module and its type metadata for one Clang target.
main :: IO ()
main = do
    arguments <- getArgs
    case arguments of
        [target, outputDirectory, outputSpecification, inputSpecification] ->
            generate target outputDirectory outputSpecification inputSpecification
        _ ->
            die "Usage: GenerateBindings TARGET OUTPUT_DIR OUTPUT_SPEC INPUT_SPEC"
  where
    generate :: String -> FilePath -> FilePath -> FilePath -> IO ()
    generate target outputDirectory outputSpecification inputSpecification = do
        let config :: Config_ FilePath
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
                    , bindingSpec =
                        def
                            { prescriptiveBindingSpec =
                                if inputSpecification == "-"
                                    then Nothing
                                    else Just inputSpecification
                            }
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
            bindgenConfig =
                toBindgenConfig config (UniqueId "duckdb-ffi") "Database.DuckDB.FFI" categories
            headers = map C.DirectiveHashInclude ["duckdb.h", "duckdb_arrow.h"]
            quiet = def{verbosity = Verbosity Error}
        hsBindgenMacroLang
            (pure . Macro.cExpr)
            quiet
            quiet
            bindgenConfig
            headers
            $ do
                writeBindingsSingle def AllowFileOverwrite CreateOutputDirs outputDirectory
                writeBindingSpec AllowFileOverwrite CreateOutputDirs outputSpecification
