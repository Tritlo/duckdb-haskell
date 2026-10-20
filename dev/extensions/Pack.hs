{-# LANGUAGE OverloadedStrings #-}

-- | Copy the demo library and append unsigned DuckDB V2 metadata.
module Main where

import Control.Exception (IOException, bracket, catch, finally)
import Control.Monad (unless, when)
import Data.Bits ((.&.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Char (ord)
import Data.List (isPrefixOf)
import System.Console.GetOpt
import System.Directory (canonicalizePath, removeFile)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitSuccess, exitWith)
import System.FilePath (takeDirectory, takeFileName)
import System.IO
import System.IO.Error (ioeGetErrorString)
import System.Posix.Files (createLink, fileMode, getFileStatus, isRegularFile, setFileMode)

-- | Command-line metadata. Each required option occurs once.
data Flag = Output FilePath | Platform String | DuckDBVersion String | ExtensionVersion String | Help
    deriving (Eq, Show)

-- | Accepted options match the original packaging CLI.
options :: [OptDescr Flag]
options =
    [ Option [] ["output"] (ReqArg Output "OUTPUT") "new path named haskell_demo.duckdb_extension; must not exist"
    , Option [] ["platform"] (ReqArg Platform "PLATFORM") "exact DuckDB platform, such as linux_amd64 or osx_arm64"
    , Option [] ["duckdb-version"] (ReqArg DuckDBVersion "VERSION") "exact normalized loader version; pinned dev: d54cbbe3df"
    , Option [] ["extension-version"] (ReqArg ExtensionVersion "VERSION") "extension release version to record"
    , Option ['h'] ["help"] (NoArg Help) "show this help"
    ]

-- | Usage text for the explicit input and metadata arguments.
usage :: String
usage = usageInfo "Usage: runghc dev/extensions/Pack.hs LIBRARY [OPTIONS]" options

-- | Report a packaging error without a traceback.
failUsage :: String -> IO a
failUsage message = hPutStrLn stderr ("pack: " <> message <> "\n" <> usage) >> exitWith (ExitFailure 2)

-- | Read one required option and reject repeated values.
required :: String -> [String] -> IO String
required label values = case values of
    [value] -> pure value
    [] -> failUsage ("missing " <> label)
    _ -> failUsage ("repeated " <> label)

-- | Encode a nonempty ASCII field without truncation.
metadataField :: String -> String -> IO BS.ByteString
metadataField label value = do
    when (null value || '\0' `elem` value) $ ioError (userError (label <> " must be nonempty and must not contain NUL"))
    when (any ((> 127) . ord) value) $ ioError (userError (label <> " must contain ASCII characters only"))
    when (length value > 32) $ ioError (userError (label <> " exceeds 32 bytes"))
    pure (BS8.pack value <> BS.replicate (32 - length value) 0)

-- | Match the pinned append_metadata.cmake field order and unsigned footer.
footer :: String -> String -> String -> IO BS.ByteString
footer platform duckdbVersion extensionVersion = do
    fields <-
        sequence
            [ metadataField "metadata magic" "4"
            , metadataField "platform" platform
            , metadataField "DuckDB version" duckdbVersion
            , metadataField "extension version" extensionVersion
            , metadataField "ABI type" "C_STRUCT_UNSTABLE"
            ]
    let prefix = BS.pack [0, 0x93, 4, 0x10] <> "duckdb_signature" <> BS.pack [0x80, 4]
    pure (prefix <> BS.concat (reverse (fields <> replicate 3 (BS.replicate 32 0))) <> BS.replicate 256 0)

-- | Read a short native header word in its declared byte order.
headerWord :: Bool -> Int -> Int -> BS.ByteString -> Int
headerWord little offset count header =
    BS.foldl' (\value byte -> value * 256 + fromIntegral byte) 0 ordered
  where
    bytes = BS.take count (BS.drop offset header)
    ordered = if little then BS.reverse bytes else bytes

-- | Accept current ELF64 and thin Mach-O64 shared libraries.
checkLibrary :: BS.ByteString -> String -> IO ()
checkLibrary header platform
    | BS.length header < 32 = invalid "input is too short to be a supported shared library"
    | magic == BS.pack [0x7f, 0x45, 0x4c, 0x46] = do
        unless (BS.length header >= 64 && BS.index header 4 == 2 && BS.index header 5 `elem` [1, 2]) $
            invalid "input must use the 64-bit ELF format"
        unless (headerWord (BS.index header 5 == 1) 16 2 header == 3) $
            invalid "ELF input must be a shared object (ET_DYN)"
        unless ("linux_" `isPrefixOf` platform) $ invalid "ELF input requires an explicit linux_* platform"
    | magic `elem` [BS.pack [0xcf, 0xfa, 0xed, 0xfe], BS.pack [0xfe, 0xed, 0xfa, 0xcf]] = do
        unless (headerWord (BS.head magic == 0xcf) 12 4 header `elem` [6, 8]) $
            invalid "Mach-O input must be a dynamic library or bundle"
        unless ("osx_" `isPrefixOf` platform) $ invalid "Mach-O input requires an explicit osx_* platform"
    | otherwise = invalid "input must be a 64-bit Linux ELF or thin macOS Mach-O library; Windows is unsupported"
  where
    magic = BS.take 4 header
    invalid = ioError . userError

-- | Copy bytes without retaining the complete shared library in memory.
copyHandle :: Handle -> Handle -> IO ()
copyHandle source target = do
    bytes <- BS.hGetSome source (64 * 1024)
    unless (BS.null bytes) $ BS.hPut target bytes >> copyHandle source target

-- | Write a new extension and preserve the original library.
pack :: FilePath -> FilePath -> String -> String -> String -> IO ()
pack library output platform duckdbVersion extensionVersion = do
    suffix <- footer platform duckdbVersion extensionVersion
    source <- canonicalizePath library
    destination <- canonicalizePath output
    when (source == destination) $ ioError (userError "input and output paths must differ")
    unless (takeFileName output == "haskell_demo.duckdb_extension") $
        ioError (userError "output must be named haskell_demo.duckdb_extension to match the demo entrypoint")
    status <- getFileStatus source
    unless (isRegularFile status) $ ioError (userError "input must be a regular shared-library file")
    withBinaryFile source ReadMode $ \original -> do
        BS.hGet original 64 >>= (`checkLibrary` platform)
        size <- hFileSize original
        when (size >= 512) $ do
            hSeek original SeekFromEnd (-288)
            magic <- BS.hGet original 32
            expected <- metadataField "metadata magic" "4"
            when (magic == expected) $ ioError (userError "input already contains DuckDB extension metadata")
        hSeek original AbsoluteSeek 0
        -- Publish the complete file with an exclusive hard link. Existing outputs stay intact.
        bracket (openBinaryTempFile (takeDirectory output) ".haskell_demo.pack") cleanup $ \(temporary, target) -> do
            copyHandle original target
            BS.hPut target suffix
            hClose target
            setFileMode temporary (fileMode status .&. 0o777)
            createLink temporary output
  where
    cleanup (temporary, target) = hClose target `finally` removeFile temporary

-- | Read explicit metadata and package one foreign-library build output.
main :: IO ()
main = do
    arguments <- getArgs
    let (flags, positional, errors) = getOpt Permute options arguments
    when (Help `elem` flags) $ putStr usage >> exitSuccess
    unless (null errors) $ failUsage (concat errors)
    library <- case positional of
        [path] -> pure path
        _ -> failUsage "supply one explicit shared-library path"
    output <- required "--output" [value | Output value <- flags]
    platform <- required "--platform" [value | Platform value <- flags]
    duckdbVersion <- required "--duckdb-version" [value | DuckDBVersion value <- flags]
    extensionVersion <- required "--extension-version" [value | ExtensionVersion value <- flags]
    (pack library output platform duckdbVersion extensionVersion >> putStrLn output)
        `catch` \exception -> failUsage (ioeGetErrorString (exception :: IOException))
