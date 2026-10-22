{-# LANGUAGE BlockArguments #-}

-- | SQL tokenization using the grammar of a V2 connection.
module Database.DuckDB.Simple.Tokenizer (
    Token (..),
    Tokenization (..),
    tokenize,
) where

import Control.Exception (bracket)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Database.DuckDB.Simple.Internal (Connection, withConnectionHandle)
import Database.DuckDB.Simple.Native
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)

-- | A token and its location in the UTF-8 source bytes.
data Token = Token
    { tokenType :: !DuckDBV2TokenType
    , tokenOffset :: !Int
    , tokenBytes :: !BS.ByteString
    }
    deriving (Eq, Show)

-- | Tokens and the native indication of an unclosed final token.
data Tokenization = Tokenization
    { tokenizationTokens :: ![Token]
    , tokenizationUnterminated :: !Bool
    }
    deriving (Eq, Show)

-- | Tokenize text, including comments and interior NUL bytes.
tokenize :: Connection -> Text -> IO Tokenization
tokenize conn sql = withConnectionHandle conn \handle ->
    let bytes = Text.encodeUtf8 sql
     in withBytes bytes \source ->
            bracket (output (c_duckdb_v2_tokenize_sql handle source)) (destroy c_duckdb_v2_token_iterator_destroy) \iterator -> do
                tokens <- alloca \kind -> alloca \start -> alloca \count ->
                    let collect acc = do
                            checked (c_duckdb_v2_token_iterator_next iterator kind start count)
                            typ <- peek kind
                            if typ == DuckDBV2TokenTypeEndOfInput
                                then pure (reverse acc)
                                else do
                                    offset <- fromIntegral <$> peek start
                                    size <- fromIntegral <$> peek count
                                    collect (Token typ offset (BS.take size (BS.drop offset bytes)) : acc)
                     in collect []
                unfinished <- output (c_duckdb_v2_token_iterator_ends_unterminated iterator)
                pure (Tokenization tokens (unfinished /= 0))
