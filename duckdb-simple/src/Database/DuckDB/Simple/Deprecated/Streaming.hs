-- | Compatibility import for explicit V2 streaming.
module Database.DuckDB.Simple.Deprecated.Streaming
    {-# DEPRECATED "Use Database.DuckDB.Simple.Streaming for V2 streaming." #-} (
    module Database.DuckDB.Simple.Streaming,
) where

import Database.DuckDB.Simple.Streaming
