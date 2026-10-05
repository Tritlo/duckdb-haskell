{-# OPTIONS_GHC -Wno-deprecations #-}

{- |
Module      : Database.DuckDB.FFI.Deprecated
Description : Re-exports of deprecated DuckDB C API bindings.

This module gathers bindings that DuckDB has marked with a deprecation notice
so downstream code can opt-in to the legacy surface separately from the main
@Database.DuckDB.FFI@ module.
-}
module Database.DuckDB.FFI.Deprecated
    {-# DEPRECATED "These DuckDB C APIs are deprecated upstream. Prefer Database.DuckDB.FFI and its nondeprecated modules." #-} (
    module Database.DuckDB.FFI.Deprecated.Appender,
    module Database.DuckDB.FFI.Deprecated.Arrow,
    module Database.DuckDB.FFI.Deprecated.ExecutePrepared,
    module Database.DuckDB.FFI.Deprecated.PendingResult,
    module Database.DuckDB.FFI.Deprecated.QueryExecution,
    module Database.DuckDB.FFI.Deprecated.ResultFunctions,
    module Database.DuckDB.FFI.Deprecated.SafeFetch,
    module Database.DuckDB.FFI.Deprecated.StreamingResult,
) where

import Database.DuckDB.FFI.Deprecated.Appender
import Database.DuckDB.FFI.Deprecated.Arrow
import Database.DuckDB.FFI.Deprecated.ExecutePrepared
import Database.DuckDB.FFI.Deprecated.PendingResult
import Database.DuckDB.FFI.Deprecated.QueryExecution
import Database.DuckDB.FFI.Deprecated.ResultFunctions
import Database.DuckDB.FFI.Deprecated.SafeFetch
import Database.DuckDB.FFI.Deprecated.StreamingResult
