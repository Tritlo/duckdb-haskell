{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Exception (IOException, catch, throwIO)
import Control.Monad (filterM, unless, when)
import Data.List (intercalate, isPrefixOf, isSuffixOf, nub, stripPrefix)
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
import System.Process (CreateProcess (..), callProcess, proc, readCreateProcess)

#if !MIN_VERSION_Cabal(3,14,0)

-- | Cabal versions before 3.14 use plain file paths.
makeSymbolicPath :: FilePath -> FilePath
makeSymbolicPath = id

-- | Read a plain file path with Cabal versions before 3.14.
getSymbolicPath :: FilePath -> FilePath
getSymbolicPath = id

#endif

-- | Set native library paths when Cabal configures the package.
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
            updated = description{condLibrary = fmap (\tree -> tree{condTreeData = addLibrary (condTreeData tree)}) (condLibrary description)}
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
        extension = lookupFlagAssignment (mkFlagName "extension") (configConfigurationsFlags flags) == Just True
    if extension || isJust nixBuild || isJust nixShell
        then pure []
        else
            if system || not (null supplied)
                then do
                    unless (all isAbsolute supplied) $
                        fail "Use absolute paths in --extra-lib-dirs so Cabal can track the library location."
                    pure (nub supplied)
                else (: []) <$> installNativeLibrary flags

-- | Name the release platform and the shared library for this system.
nativePlatform :: IO (String, FilePath)
nativePlatform = case (os, arch) of
    ("linux", "x86_64") -> pure ("linux-amd64", "libduckdb.so")
    ("linux", "aarch64") -> pure ("linux-arm64", "libduckdb.so")
    ("darwin", cpu) | cpu `elem` ["x86_64", "aarch64"] -> pure ("osx-universal", "libduckdb.dylib")
    _ -> fail "duckdb-ffi supports glibc Linux and macOS on x86_64 and aarch64."

{- | Read the release path and checksum for one platform from @native.sha256@.
Each line has a checksum and a release path, for example
@v2.0.0/duckdb-shared-libs-linux-amd64.tar.gz@.
-}
nativeArchive :: String -> IO (FilePath, String)
nativeArchive platform = do
    pinned <- doesFileExist "native.sha256"
    unless pinned $ fail "native.sha256 is missing. The package must list it in extra-source-files."
    pins <- readFile "native.sha256"
    case [(path, checksum) | [checksum, path] <- map words (lines pins), ("/duckdb-shared-libs-" <> platform <> ".tar.gz") `isSuffixOf` path] of
        [archive] -> pure archive
        _ -> fail ("native.sha256 must contain one archive for " <> platform <> ".")

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
    (platform, libraryName) <- nativePlatform
    (release, checksum) <- nativeArchive platform
    let version = drop 1 (takeWhile (/= '/') release)
        root = base </> version
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
        let programs = ["curl", "tar", if os == "darwin" then "shasum" else "sha256sum"]
        missing <- filterM (\program -> isNothing <$> findExecutable program) programs
        unless (null missing) $
            fail ("DuckDB installation requires these programs on PATH: " <> intercalate ", " missing <> ". Install them, or supply DuckDB with -fsystemlib and --extra-lib-dirs. See README.md for details.")
        createDirectoryIfMissing True root
        withTempDirectory root ".download-" $ \staging -> do
            let archiveName = "duckdb.tar.gz"
                archive = staging </> archiveName
                extracted = staging </> "library"
                url = "https://github.com/duckdb/duckdb/releases/download/" <> release
                hash = if os == "darwin" then proc "shasum" ["-a", "256", archiveName] else proc "sha256sum" [archiveName]
            putStrLn ("Downloading DuckDB " <> version <> " to " <> destination)
            callProcess "curl" ["--fail", "--location", "--proto", "=https", "--proto-redir", "=https", "--tlsv1.2", "--retry", "2", "--output", archive, url]
            -- Hash a plain relative name. Hash tools escape other names in their output.
            digest <- readCreateProcess hash{cwd = Just staging} ""
            unless (takeWhile (/= ' ') digest == checksum) $ fail "DuckDB archive checksum mismatch."
            createDirectory extracted
            -- Keep the shared-library symlinks and their versioned targets together.
            callProcess "tar" ["-xzf", archive, "-C", extracted]
            libraryPresent <- doesFileExist (extracted </> libraryName)
            unless libraryPresent $ fail "DuckDB archive does not contain the expected shared library and its symlink targets."
            writeFile (extracted </> "SHA256") (checksum <> "\n")
            -- Another build can publish the same verified archive first.
            renameDirectory extracted destination `catch` \(err :: IOException) -> do
                installed <- complete destination
                unless installed (throwIO err)
    pure destination
