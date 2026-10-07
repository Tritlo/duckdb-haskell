{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Exception (IOException, catch, throwIO)
import Control.Monad (filterM, forM_, unless, when)
import Data.Char (isHexDigit)
import Data.List (intercalate, isPrefixOf, nub, stripPrefix)
import Data.Maybe (isJust, isNothing)
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.InstallDirs (CopyDest (NoCopyDest), includedir)
import qualified Distribution.Simple.LocalBuildInfo as LBI
import Distribution.Simple.Setup (ConfigFlags, configConfigurationsFlags, configConfigureArgs, configDistPref, configExtraLibDirs, copyDest, defaultDistPref, fromFlagOrDefault, installDest, regInPlace)

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

-- | Save the native library and selected header paths in the package description.
main :: IO ()
main =
    defaultMainWithHooks
        simpleUserHooks
            { confHook = configure
            , copyHook = \package local hooks flags ->
                copyHook simpleUserHooks (installDescription package local) local hooks flags
            , instHook = \package local hooks flags ->
                instHook simpleUserHooks (installDescription package local) local hooks flags
            , regHook = \package local hooks flags ->
                regHook
                    simpleUserHooks
                    (if fromFlagOrDefault False (regInPlace flags) then package else installDescription package local)
                    local
                    hooks
                    flags
            , postCopy = \args flags package local -> do
                postCopy simpleUserHooks args flags package local
                copyPreviewHeaders (fromFlagOrDefault NoCopyDest (copyDest flags)) package local
            , postInst = \args flags package local -> do
                postInst simpleUserHooks args flags package local
                copyPreviewHeaders (fromFlagOrDefault NoCopyDest (installDest flags)) package local
            }
  where
    configure (description, hooks) flags = do
        let preview = lookupFlagAssignment (mkFlagName "duckdb-v2") (configConfigurationsFlags flags) == Just True
        headerDirectories <- if preview then (: []) <$> extractPreviewHeaders flags else pure []
        nativeDirs <- nativeLibraryDirs flags
        let addLibrary lib =
                let info = libBuildInfo lib
                 in lib
                        { libBuildInfo =
                            info
                                { ldOptions =
                                    (if os `elem` ["linux", "darwin"] then concatMap (\dir -> ["-Xlinker", "-rpath", "-Xlinker", dir]) nativeDirs else [])
                                        <> ldOptions info
                                , includeDirs = if preview then map makeSymbolicPath headerDirectories else includeDirs info
                                }
                        }
            updated = description{condLibrary = fmap (\tree -> tree{condTreeData = addLibrary (condTreeData tree)}) (condLibrary description)}
            updatedFlags =
                if null nativeDirs
                    then flags
                    else flags{configExtraLibDirs = map makeSymbolicPath nativeDirs}
        confHook simpleUserHooks (updated, hooks) updatedFlags

-- | Verify the preview API archive. Extract its client headers into the build directory.
extractPreviewHeaders :: ConfigFlags -> IO FilePath
extractPreviewHeaders flags = do
    let archive = "vendor" </> "duckdb-api.tar.gz"
        checksumFile = "vendor" </> "duckdb-api.sha256"
        files = ["duckdb.h", "duckdb_v2.h", "LICENSE"]
    forM_ [archive, checksumFile] $ \path -> do
        exists <- doesFileExist path
        unless exists $ fail ("Bundled DuckDB API file is missing: " <> path)
    checksumText <- readFile checksumFile
    let checksum = takeWhile (/= '\n') checksumText
    unless (length checksum == 64 && all isHexDigit checksum && checksumText == checksum <> "\n") $
        fail ("Invalid SHA256 digest in " <> checksumFile <> ". Expected one hexadecimal digest followed by a newline.")
    sha256sum <- findExecutable "sha256sum"
    (checksumProgram, checksumArgs) <- case sha256sum of
        Just program -> pure (program, [])
        Nothing -> do
            shasum <- findExecutable "shasum"
            case shasum of
                Just program -> pure (program, ["-a", "256"])
                Nothing -> fail "DuckDB API archive verification requires sha256sum or shasum on PATH."
    digest <- readProcess checksumProgram (checksumArgs <> [archive]) ""
    unless (takeWhile (/= ' ') digest == checksum) $
        fail ("Bundled DuckDB API archive checksum mismatch: " <> archive)
    tar <- findExecutable "tar"
    tarProgram <- maybe (fail "DuckDB API header extraction requires tar on PATH.") pure tar
    buildDirectory <- makeAbsolute (getSymbolicPath (fromFlagOrDefault defaultDistPref (configDistPref flags)))
    let headerDirectory = buildDirectory </> "duckdb-api" </> "duckdb-2.0"
    createDirectoryIfMissing True headerDirectory
    callProcess tarProgram (["-xzf", archive, "-C", headerDirectory, "--strip-components=1"] <> map ("duckdb-2.0" </>) files)
    renameFile (headerDirectory </> "LICENSE") (headerDirectory </> "duckdb-LICENSE")
    pure headerDirectory

-- | Install the preview headers and DuckDB license after Cabal copies the library.
copyPreviewHeaders :: CopyDest -> PackageDescription -> LBI.LocalBuildInfo -> IO ()
copyPreviewHeaders destination package local =
    when (lookupFlagAssignment (mkFlagName "duckdb-v2") (LBI.flagAssignment local) == Just True) $ do
        source <- extractPreviewHeaders (LBI.configFlags local)
        let target = includedir (LBI.absoluteInstallDirs package local destination)
        createDirectoryIfMissing True target
        forM_ ["duckdb.h", "duckdb_v2.h", "duckdb-LICENSE"] $ \file ->
            copyFile (source </> file) (target </> file)

-- | Use the package header directory when Cabal copies or registers a preview library.
installDescription :: PackageDescription -> LBI.LocalBuildInfo -> PackageDescription
installDescription package local =
    if lookupFlagAssignment (mkFlagName "duckdb-v2") (LBI.flagAssignment local) == Just True
        then package{library = fmap installLibrary (library package)}
        else package
  where
    installLibrary lib =
        lib{libBuildInfo = (libBuildInfo lib){includeDirs = [makeSymbolicPath "cbits"]}}

-- | Use a supplied library, or download one to the user cache.
nativeLibraryDirs :: ConfigFlags -> IO [FilePath]
nativeLibraryDirs flags = do
    nixBuild <- lookupEnv "NIX_BUILD_TOP"
    nixShell <- lookupEnv "IN_NIX_SHELL"
    let supplied = map getSymbolicPath (configExtraLibDirs flags)
        system = lookupFlagAssignment (mkFlagName "systemlib") (configConfigurationsFlags flags) == Just True
        preview = lookupFlagAssignment (mkFlagName "duckdb-v2") (configConfigurationsFlags flags) == Just True
    if isJust nixBuild || isJust nixShell
        then pure []
        else
            if system || not (null supplied)
                then do
                    unless (all isAbsolute supplied) $
                        fail "Use absolute paths in --extra-lib-dirs so Cabal can track the library location."
                    pure (nub supplied)
                else do
                    when preview $
                        fail "The duckdb-v2 flag requires a matching DuckDB 2.0 library. Supply it with -fsystemlib and --extra-lib-dirs=/absolute/path. See docs/duckdb-2.0.md in the repository."
                    (: []) <$> installNativeLibrary flags

{- | Pin the download to the release verified by the checksums below.
The Haskell package version can differ from the native library version.
-}
nativeVersion :: String
nativeVersion = "1.5.6"

-- | Select an official archive and its release checksum.
nativeArchive :: IO (String, String, FilePath)
nativeArchive = case (os, arch) of
    ("linux", "x86_64") -> pure ("linux-amd64", "b845005f5132a7d8180057c35e14a7626632258782f871a90861b19c1c03841b", "libduckdb.so")
    ("linux", "aarch64") -> pure ("linux-arm64", "b72ed9f05003f5e9d2015f7ceada6416b377d9dd33169cdde3c9e33897856eee", "libduckdb.so")
    ("darwin", "x86_64") -> mac
    ("darwin", "aarch64") -> mac
    _ -> fail "Automatic DuckDB installation supports glibc Linux and macOS. Supply DuckDB >= 1.5.3 and < 1.6 with -fsystemlib and --extra-lib-dirs."
  where
    mac = pure ("osx-universal", "e0bc007d9b0094c0970ac1847a8601d10aad07cbd2910ce586ec810f77b638d6", "libduckdb.dylib")

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
