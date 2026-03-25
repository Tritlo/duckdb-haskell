{-# LANGUAGE CPP #-}

import Data.List (isPrefixOf)
import Distribution.Simple
import Distribution.Simple.Program
import Distribution.Simple.Setup
import Distribution.Simple.LocalBuildInfo
import Distribution.Types.LocalBuildInfo
import Distribution.Types.GenericPackageDescription
import Distribution.Types.HookedBuildInfo
import Distribution.PackageDescription
import Distribution.System
import System.Directory
import System.FilePath
import System.IO.Temp
import Network.HTTP.Simple
import qualified Data.ByteString.Lazy as LBS
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
import GHC.IO.Exception
import Control.Exception
import Codec.Archive.Zip

#if MIN_VERSION_Cabal(3,14,0)
import Distribution.Utils.Path (makeSymbolicPath)
#else

makeSymbolicPath :: a -> a
makeSymbolicPath = id
#endif

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks
  { preConf = \_ _ -> do
      pure emptyHookedBuildInfo
  , confHook = \(gpd, hbi) flags -> do
      mDuckDBDir <- ensureDuckDB
      case mDuckDBDir of
        Nothing -> do
          putStrLn "libduckdb not found in cache, skipping extra configuration."
          confHook simpleUserHooks (gpd, hbi) flags
        Just duckdbDir -> do
          let updatedFlags = flags
                { configExtraLibDirs     = makeSymbolicPath duckdbDir : configExtraLibDirs flags
                , configExtraIncludeDirs = makeSymbolicPath duckdbDir : configExtraIncludeDirs flags
                }
          lbi <- confHook simpleUserHooks (gpd, hbi) updatedFlags
          case buildOS of
            OSX   -> return $ lbi { withPrograms = addRPath duckdbDir (withPrograms lbi) }
            Linux -> return $ lbi { withPrograms = addRPath duckdbDir (withPrograms lbi) }
            _     -> return lbi
  }

-- Default to the version of libduckdb recommended in the README
getDuckDBVersion :: IO String
getDuckDBVersion = fromMaybe "1.5.0" <$> lookupEnv "DUCKDB_VERSION"

getLocalUserDuckDBDir :: IO FilePath
getLocalUserDuckDBDir = do
  mHome <- lookupEnv "DUCKDB_HOME"
  version <- getDuckDBVersion
  base <- case mHome of
    Just h  -> pure h
    Nothing -> getXdgDirectory XdgCache "duckdb"
  pure $ base </> version </> platformTag

platformTag :: String
platformTag =
  case (buildOS, buildArch) of
    (Linux, X86_64)  -> "linux-amd64"
    (OSX,   _)       -> "osx-universal"
    _ -> error $ "Unsupported platform: " <> show (buildOS, buildArch)

ensureDuckDB :: IO (Maybe FilePath)
ensureDuckDB = do
  isSandbox <- isNixSandbox
  if isSandbox
    then return Nothing
    else downloadDuckDB

isNixSandbox :: IO Bool
isNixSandbox = do
  nix <- lookupEnv "NIX_BUILD_TOP"
  case nix of
    Just path ->
      if any (`isPrefixOf` path) ["/build", "/private/tmp/nix-build"]
        then do
          putStrLn "Nix sandbox detected; skipping libduckdb download."
          return True
        else return False
    Nothing -> return False

downloadDuckDB :: IO (Maybe FilePath)
downloadDuckDB = do
  skip <- lookupEnv "DUCKDB_SKIP_DOWNLOAD"
  case skip of
    Just _ -> do
      putStrLn "DUCKDB_SKIP_DOWNLOAD set; assuming libduckdb exists globally."
      return Nothing
    Nothing -> do
      dest <- getLocalUserDuckDBDir
      let marker = dest </> ".ok"
      exists <- doesFileExist marker
      present <- doesDirectoryExist dest
      if present && exists
        then pure $ Just dest
        else do
          putStrLn $ "libduckdb not found in local cache, installing to " <> dest
          downloadAndExtractDuckDBTo dest
          writeFile marker ""
          pure $ Just dest

downloadAndExtractDuckDBTo :: FilePath -> IO ()
downloadAndExtractDuckDBTo dest = do
  createDirectoryIfMissing True dest
  (url, fileName) <- computeURL
  putStrLn $ "Downloading libduckdb from: " ++ url
  withSystemTempDirectory "duckdb-download" $ \tmpDir -> do
    let downloadPath = tmpDir </> fileName
    request  <- parseRequest url
    response <- httpLBS request
    LBS.writeFile downloadPath (getResponseBody response)
    putStrLn "Download complete. Extracting..."
    archive <- toArchive <$> LBS.readFile downloadPath
    let extractDir = tmpDir </> "extracted"
    createDirectoryIfMissing True extractDir
    extractFilesFromArchive [OptDestination extractDir] archive
    (renameDirectory extractDir dest) `catch` (\(_ :: IOException) -> copyTree extractDir dest)
    putStrLn "libduckdb extracted successfully (global cache)."

copyTree :: FilePath -> FilePath -> IO ()
copyTree src dest = do
  createDirectoryIfMissing True dest
  entries <- listDirectory src
  mapM_ (\e -> do
          let s = src  </> e
              d = dest </> e
          isDir <- doesDirectoryExist s
          if isDir then copyTree s d else copyFile s d
        ) entries

computeURL :: IO (String, String)
computeURL = do
  v <- getDuckDBVersion
  let base = "https://github.com/duckdb/duckdb/releases/download/v" ++ v ++ "/"
  pure $ case buildOS of
    Linux -> ( base ++ "libduckdb-linux-amd64.zip", "libduckdb-linux-amd64.zip" )
    OSX   -> ( base ++ "libduckdb-osx-universal.zip", "libduckdb-osx-universal.zip" )
    _     -> error "Unsupported OS for libduckdb download"

addRPath :: FilePath -> ProgramDb -> ProgramDb
addRPath libDir progDb =
  userSpecifyArgs (programName ldProgram)
  ["-Wl,-rpath," ++ libDir]
  progDb
