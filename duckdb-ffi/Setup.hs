{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Exception (IOException, catch, throwIO)
import Control.Monad (filterM, unless, when)
import Data.List (intercalate, isPrefixOf, nub, stripPrefix)
import Data.Maybe (isJust, isNothing)
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.Setup (ConfigFlags, configConfigurationsFlags, configConfigureArgs, configExtraLibDirs)
#if MIN_VERSION_Cabal(3,14,0)
import Distribution.Utils.Path (getSymbolicPath, makeSymbolicPath)
#endif
import System.Directory
import System.Environment (lookupEnv)
import System.FilePath (isAbsolute, (</>))
import System.IO.Temp (withTempDirectory)
import System.Info (arch, os)
import System.Process (callProcess, readProcess)

#if !MIN_VERSION_Cabal(3,14,0)
-- | Cabal versions before 3.14 use plain file paths.
makeSymbolicPath :: FilePath -> FilePath
makeSymbolicPath = id

-- | Read a plain file path with Cabal versions before 3.14.
getSymbolicPath :: FilePath -> FilePath
getSymbolicPath = id
#endif

-- | Save the native library directory in the package description.
main :: IO ()
main = defaultMainWithHooks simpleUserHooks{confHook = configure}
  where
    configure (description, hooks) flags = do
        nativeDirs <- nativeLibraryDirs flags
        let addLibrary lib =
                let info = libBuildInfo lib
                 in lib
                        { libBuildInfo =
                            info
                                { ldOptions =
                                    (if os `elem` ["linux", "darwin"] then concatMap (\dir -> ["-Xlinker", "-rpath", "-Xlinker", dir]) nativeDirs else [])
                                        <> ldOptions info
                                }
                        }
            updated = description{condLibrary = fmap (fmap addLibrary) (condLibrary description)}
            updatedFlags =
                if null nativeDirs
                    then flags
                    else flags{configExtraLibDirs = map makeSymbolicPath nativeDirs}
        confHook simpleUserHooks (updated, hooks) updatedFlags

-- | Use a supplied library, or download one to the user cache.
nativeLibraryDirs :: ConfigFlags -> IO [FilePath]
nativeLibraryDirs flags = do
    nixBuild <- lookupEnv "NIX_BUILD_TOP"
    nixShell <- lookupEnv "IN_NIX_SHELL"
    let supplied = map getSymbolicPath (configExtraLibDirs flags)
        system = lookupFlagAssignment (mkFlagName "systemlib") (configConfigurationsFlags flags) == Just True
    if isJust nixBuild || isJust nixShell
        then pure []
        else
            if system || not (null supplied)
                then do
                    unless (all isAbsolute supplied) $
                        fail "Use absolute paths in --extra-lib-dirs so Cabal can track the library location."
                    pure (nub supplied)
                else (: []) <$> installNativeLibrary flags

{- | Pin the download to the release verified by the checksums below.
The Haskell package version can differ from the native library version.
-}
nativeVersion :: String
nativeVersion = "1.5.3"

-- | Select an official archive and its release checksum.
nativeArchive :: IO (String, String, FilePath)
nativeArchive = case (os, arch) of
    ("linux", "x86_64") -> pure ("linux-amd64", "0a926eba5bce0abc0010f4b9109133e4440cb74e97bd10fd2d0fc2a721621b05", "libduckdb.so")
    ("linux", "aarch64") -> pure ("linux-arm64", "162806d591c0431d031d9bdf43dbecc5f00755da01a2064df68f9a69a6f50a10", "libduckdb.so")
    ("darwin", "x86_64") -> mac
    ("darwin", "aarch64") -> mac
    _ -> fail "Automatic DuckDB installation supports glibc Linux and macOS. Supply DuckDB >= 1.5.3 and < 1.6 with -fsystemlib and --extra-lib-dirs."
  where
    mac = pure ("osx-universal", "386f8e8b3b4bc8d128762327121e22065ce45f2ee55ef1b1f412ce11e0e6c51f", "libduckdb.dylib")

-- | Download into a temporary directory. Publish the verified library atomically.
installNativeLibrary :: ConfigFlags -> IO FilePath
installNativeLibrary flags = do
    base <- case filter (isPrefixOf "--duckdb-install-dir") (configConfigureArgs flags) of
        [] -> getXdgDirectory XdgCache "duckdb-haskell" >>= makeAbsolute
        [option]
            | Just directory <- stripPrefix "--duckdb-install-dir=" option
            , isAbsolute directory ->
                pure directory
        _ -> fail "Pass --configure-option=--duckdb-install-dir=/absolute/path once, with an absolute directory."
    (platform, checksum, libraryName) <- nativeArchive
    let root = base </> nativeVersion
        destination = root </> platform
        complete dir = do
            libraryExists <- doesFileExist (dir </> libraryName)
            markerExists <- doesFileExist (dir </> "SHA256")
            marker <- if markerExists then readFile (dir </> "SHA256") else pure ""
            pure (libraryExists && marker == checksum <> "\n")
    -- Check existence first to accept a library installed by another build.
    exists <- doesPathExist destination
    ready <- complete destination
    unless ready $ do
        when exists $ fail ("Incomplete DuckDB installation: remove " <> destination <> " and retry.")
        let programs = ["curl", "unzip", if os == "darwin" then "shasum" else "sha256sum"]
        missing <- filterM (\program -> isNothing <$> findExecutable program) programs
        unless (null missing) $
            fail ("DuckDB installation requires these programs on PATH: " <> intercalate ", " missing <> ". Install them, or supply DuckDB with -fsystemlib and --extra-lib-dirs. See README.md for details.")
        createDirectoryIfMissing True root
        withTempDirectory root ".download-" $ \staging -> do
            let archive = staging </> "duckdb.zip"
                extracted = staging </> "library"
                url = "https://github.com/duckdb/duckdb/releases/download/v" <> nativeVersion <> "/libduckdb-" <> platform <> ".zip"
            putStrLn ("Downloading DuckDB " <> nativeVersion <> " to " <> destination)
            callProcess "curl" ["--fail", "--location", "--proto", "=https", "--proto-redir", "=https", "--tlsv1.2", "--retry", "2", "--output", archive, url]
            digest <-
                if os == "darwin"
                    then readProcess "shasum" ["-a", "256", archive] ""
                    else readProcess "sha256sum" [archive] ""
            unless (takeWhile (/= ' ') digest == checksum) $ fail "DuckDB archive checksum mismatch."
            createDirectory extracted
            callProcess "unzip" ["-q", archive, libraryName, "-d", extracted]
            writeFile (extracted </> "SHA256") (checksum <> "\n")
            -- Another build can publish the same verified archive first.
            renameDirectory extracted destination `catch` \(err :: IOException) -> do
                installed <- complete destination
                unless installed (throwIO err)
    pure destination
