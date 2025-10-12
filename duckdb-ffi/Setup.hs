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
          ensureDuckDBAvailable (fromFlag (configVerbosity flags))
          preConf simpleUserHooks args flags
      }

systemLibraryCandidates :: [FilePath]
systemLibraryCandidates =
  [ "libduckdb.so"
  , "libduckdb.so.0"
  , "libduckdb.so.1"
  , "libduckdb.so.1.4"
  , "libduckdb.so.1.4.0"
  ]


ensureDuckDBAvailable :: Verbosity -> IO ()
ensureDuckDBAvailable verb = do
  available <- systemDuckDBAvailable
  if available
    then info verb "libduckdb found"
    else error "libduckdb not found, please install libduckdb (available at https://duckdb.org/install/)"


systemDuckDBAvailable :: IO Bool
systemDuckDBAvailable = do
  header <- findDuckDBHeader
  lib <- findDuckDBLibrary
  pure (isJust header && isJust lib)

findDuckDBHeader :: IO (Maybe FilePath)
findDuckDBHeader = do
  paths <- headerSearchPaths
  findFile paths "duckdb.h"

findDuckDBLibrary :: IO (Maybe FilePath)
findDuckDBLibrary = do
  paths <- librarySearchPaths
  listToMaybe . catMaybes <$> mapM (findFile paths) systemLibraryCandidates

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
