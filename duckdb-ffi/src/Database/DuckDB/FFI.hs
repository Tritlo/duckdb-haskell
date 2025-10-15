{- |
Module      : Database.DuckDB.FFI
Description : Aggregated re-exports for the DuckDB C API bindings

The individual modules under @Database.DuckDB.FFI.*@ group the raw FFI imports
by the sections documented in @api.md@.  This module preserves the historic
surface area by re-exporting all of those symbols.
-}
module Database.DuckDB.FFI (
    module Database.DuckDB.FFI.Types,
    module Database.DuckDB.FFI.OpenConnect,
    module Database.DuckDB.FFI.Configuration,
    module Database.DuckDB.FFI.ErrorData,
    module Database.DuckDB.FFI.QueryExecution,
    module Database.DuckDB.FFI.ResultFunctions,
    module Database.DuckDB.FFI.Helpers,
    module Database.DuckDB.FFI.PreparedStatements,
    module Database.DuckDB.FFI.BindValues,
    module Database.DuckDB.FFI.ExecutePrepared,
    module Database.DuckDB.FFI.ExtractStatements,
    module Database.DuckDB.FFI.PendingResult,
    module Database.DuckDB.FFI.ValueInterface,
    module Database.DuckDB.FFI.LogicalTypes,
    module Database.DuckDB.FFI.DataChunk,
    module Database.DuckDB.FFI.Vector,
    module Database.DuckDB.FFI.Validity,
    module Database.DuckDB.FFI.ScalarFunctions,
    module Database.DuckDB.FFI.TableFunctions,
    module Database.DuckDB.FFI.SelectionVector,
    module Database.DuckDB.FFI.AggregateFunctions,
    module Database.DuckDB.FFI.ReplacementScans,
    module Database.DuckDB.FFI.ProfilingInfo,
    module Database.DuckDB.FFI.Appender,
    module Database.DuckDB.FFI.TableDescription,
    module Database.DuckDB.FFI.Arrow,
    module Database.DuckDB.FFI.Threading,
    module Database.DuckDB.FFI.StreamingResult,
    module Database.DuckDB.FFI.CastFunctions,
    module Database.DuckDB.FFI.Expression,
) where

import Database.DuckDB.FFI.AggregateFunctions
import Database.DuckDB.FFI.Appender
import Database.DuckDB.FFI.Arrow
import Database.DuckDB.FFI.BindValues
import Database.DuckDB.FFI.CastFunctions
import Database.DuckDB.FFI.Configuration
import Database.DuckDB.FFI.DataChunk
import Database.DuckDB.FFI.ErrorData
import Database.DuckDB.FFI.ExecutePrepared
import Database.DuckDB.FFI.Expression
import Database.DuckDB.FFI.ExtractStatements
import Database.DuckDB.FFI.Helpers
import Database.DuckDB.FFI.LogicalTypes
import Database.DuckDB.FFI.OpenConnect
import Database.DuckDB.FFI.PendingResult
import Database.DuckDB.FFI.PreparedStatements
import Database.DuckDB.FFI.ProfilingInfo
import Database.DuckDB.FFI.QueryExecution
import Database.DuckDB.FFI.ReplacementScans
import Database.DuckDB.FFI.ResultFunctions
import Database.DuckDB.FFI.ScalarFunctions
import Database.DuckDB.FFI.SelectionVector
import Database.DuckDB.FFI.StreamingResult
import Database.DuckDB.FFI.TableDescription
import Database.DuckDB.FFI.TableFunctions
import Database.DuckDB.FFI.Threading
import Database.DuckDB.FFI.Types
import Database.DuckDB.FFI.Validity
import Database.DuckDB.FFI.ValueInterface
import Database.DuckDB.FFI.Vector
