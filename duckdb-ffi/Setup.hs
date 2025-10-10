-- Setup.hs

import Control.Exception (bracket)
import Control.Monad (filterM, forM_)
import Data.List (nub)
import Data.Maybe (catMaybes, isJust, listToMaybe)
import Distribution.PackageDescription (
  GenericPackageDescription (..),
  PackageDescription (..),
  extraLibDirs,
  libBuildInfo,
  library,
  package,
 )
import Distribution.Simple
import Distribution.Simple.Compiler (compilerInfo)
import Distribution.Simple.InstallDirs (absoluteInstallDirs, dynlibdir, libdir)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo (..), localUnitId)
import Distribution.Simple.Setup (configExtraLibDirs, configVerbosity, copyDest, defaultCopyFlags, fromFlag)
import Distribution.Simple.Utils (copyFileVerbose, createDirectoryIfMissingVerbose, createTempDirectory, info)
import Distribution.Verbosity (Verbosity)
import System.Directory (
  doesDirectoryExist,
  doesFileExist,
  findFile,
  getCurrentDirectory,
  getTemporaryDirectory,
  removeDirectoryRecursive,
 )
import System.Environment (lookupEnv)
import System.FilePath ((</>), splitSearchPath)
import System.Process (callProcess)

main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks
      { preConf = \args flags -> do
          installVendoredDuckDB (fromFlag (configVerbosity flags))
          preConf simpleUserHooks args flags
      , confHook = \a f -> do
          lbi <- confHook simpleUserHooks a f
          updateExtraLibDirs lbi
      }

dUCKDBVERSION :: String
dUCKDBVERSION = "v1.4.1"

dUCKDBURL :: String
dUCKDBURL = "https://github.com/duckdb/duckdb/releases/download/" <> dUCKDBVERSION <> "/libduckdb-linux-amd64.zip"

vendoredSharedLibraryCandidates :: [FilePath]
vendoredSharedLibraryCandidates =
  [ "libduckdb.so"
  , "libduckdb.so.1.4"
  , "libduckdb.so.1.4.0"
  ]

vendoredLibraryFiles :: [FilePath]
vendoredLibraryFiles = vendoredSharedLibraryCandidates ++ ["libduckdb.a"]

systemLibraryCandidates :: [FilePath]
systemLibraryCandidates =
  [ "libduckdb.so"
  , "libduckdb.so.0"
  , "libduckdb.so.1"
  , "libduckdb.so.2"
  , "libduckdb.so.1.4"
  , "libduckdb.so.1.4.0"
  , "libduckdb.dylib"
  , "duckdb.dll"
  , "duckdb.lib"
  , "libduckdb.a"
  ]

updateExtraLibDirs :: LocalBuildInfo -> IO LocalBuildInfo
updateExtraLibDirs localBuildInfo = do
  let packageDescription = localPkgDescr localBuildInfo
      Just lib = library packageDescription
      libBuild = libBuildInfo lib
  dir <- getCurrentDirectory
  let vendorLibDir = dir </> "cbits" </> "duckdb"
  hasVendored <- vendoredDuckDBPresentAt vendorLibDir
  if not hasVendored
    then pure localBuildInfo
    else
      return
        localBuildInfo
          { localPkgDescr =
              packageDescription
                { library =
                    Just $
                      lib
                        { libBuildInfo =
                            libBuild
                              { extraLibDirs = vendorLibDir : nub (extraLibDirs libBuild)
                              }
                        }
                }
          }

installVendoredDuckDB :: Verbosity -> IO ()
installVendoredDuckDB verb = do
  available <- systemDuckDBAvailable
  if available
    then info verb "System libduckdb and duckdb.h found; skipping vendored DuckDB install."
    else do
      vendored <- vendoredDuckDBPresent
      if vendored
        then info verb "Vendored DuckDB already present; skipping download."
        else installVendoredDuckDB' verb

installVendoredDuckDB' :: Verbosity -> IO ()
installVendoredDuckDB' verb = do
  let target = "cbits" </> "duckdb"

  tmpDir <- getTemporaryDirectory
  bracket
    (createTempDirectory tmpDir "duckdb")
    removeDirectoryRecursive
    $ \temp -> do
      let zipPath = temp </> "libduckdb.zip"
      info verb $ "Downloading DuckDB from " ++ dUCKDBURL
      callProcess "curl" ["-fsSL", "-o", zipPath, dUCKDBURL]
      info verb $ "Extracting to " ++ temp
      callProcess "unzip" [zipPath, "-d", temp]
      let srcDir' = temp
      existing <- filterM (doesFileExist . (srcDir' </>)) vendoredSharedLibraryCandidates
      case existing of
        [] -> info verb "duckdb-ffi: no libduckdb.so* found after extraction"
        xs -> do
          createDirectoryIfMissingVerbose verb True target
          forM_ xs $ \f -> do
            let src = srcDir' </> f
                out = target </> f
            copyFileVerbose verb src out
          copyFileVerbose verb (srcDir' </> "libduckdb_static.a") (target </> "libduckdb.a")
      return ()

systemDuckDBAvailable :: IO Bool
systemDuckDBAvailable = do
  header <- findDuckDBHeader
  lib <- findDuckDBLibrary
  pure (isJust header && isJust lib)

vendoredDuckDBPresent :: IO Bool
vendoredDuckDBPresent = do
  dir <- getCurrentDirectory
  let vendorLibDir = dir </> "cbits" </> "duckdb"
  vendoredDuckDBPresentAt vendorLibDir

vendoredDuckDBPresentAt :: FilePath -> IO Bool
vendoredDuckDBPresentAt vendorLibDir = do
  exists <- doesDirectoryExist vendorLibDir
  if not exists
    then pure False
    else or <$> mapM (doesFileExist . (vendorLibDir </>)) vendoredLibraryFiles

findDuckDBHeader :: IO (Maybe FilePath)
findDuckDBHeader = do
  paths <- headerSearchPaths
  findFile paths "duckdb.h"

findDuckDBLibrary :: IO (Maybe FilePath)
findDuckDBLibrary = do
  paths <- librarySearchPaths
  pure . listToMaybe . catMaybes =<< mapM (findFile paths) systemLibraryCandidates

headerSearchPaths :: IO [FilePath]
headerSearchPaths = do
  envPaths <- gatherEnvPaths ["C_INCLUDE_PATH", "CPATH", "INCLUDE"]
  let defaults =
        [ "/usr/include"
        , "/usr/local/include"
        , "/usr/include/x86_64-linux-gnu"
        , "/usr/include/aarch64-linux-gnu"
        , "/opt/homebrew/include"
        , "/opt/local/include"
        ]
  pure (nub (envPaths ++ defaults))

librarySearchPaths :: IO [FilePath]
librarySearchPaths = do
  envPaths <- gatherEnvPaths ["LIBRARY_PATH", "LD_LIBRARY_PATH", "DYLD_LIBRARY_PATH", "LIB"]
  let defaults =
        [ "/usr/lib"
        , "/usr/local/lib"
        , "/usr/lib64"
        , "/usr/local/lib64"
        , "/usr/lib/x86_64-linux-gnu"
        , "/usr/lib/aarch64-linux-gnu"
        , "/opt/homebrew/lib"
        , "/opt/local/lib"
        ]
  pure (nub (envPaths ++ defaults))

gatherEnvPaths :: [String] -> IO [FilePath]
gatherEnvPaths names = do
  vals <- mapM lookupEnv names
  let paths = concatMap splitSearchPath (catMaybes vals)
  pure (filter (not . null) paths)
