-- Setup.hs

import Control.Exception (bracket)
import Control.Monad (filterM, forM_)
import Data.List (nub)
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
import System.Directory (doesFileExist, getCurrentDirectory, getTemporaryDirectory, removeDirectoryRecursive)
import System.FilePath ((</>))
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

dUCKDBURL :: String
dUCKDBURL = "https://github.com/duckdb/duckdb/releases/download/v1.4.0/libduckdb-linux-amd64.zip"

updateExtraLibDirs :: LocalBuildInfo -> IO LocalBuildInfo
updateExtraLibDirs localBuildInfo = do
  let packageDescription = localPkgDescr localBuildInfo
      Just lib = library packageDescription
      libBuild = libBuildInfo lib
  dir <- getCurrentDirectory
  return
    localBuildInfo
      { localPkgDescr =
          packageDescription
            { library =
                Just $
                  lib
                    { libBuildInfo =
                        libBuild
                          { extraLibDirs = (dir </> "cbits" </> "duckdb") : nub (extraLibDirs libBuild)
                          }
                    }
            }
      }

installVendoredDuckDB :: Verbosity -> IO ()
installVendoredDuckDB verb = do
  let target = "cbits" </> "duckdb"
      cand =
        [ "libduckdb.so"
        , "libduckdb.so.1.4"
        , "libduckdb.so.1.4.0"
        ]

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
      existing <- filterM (doesFileExist . (srcDir' </>)) cand
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
