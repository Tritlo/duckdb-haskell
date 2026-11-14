-- |
-- Module      : Database.DuckDB.Simple.Copy
-- Description : Register COPY formats and multi-file table functions with Haskell functions.
--
-- The functions get and give Haskell values. Batch preparation can run
-- concurrently. The sink calls for one output file run one at a time. A
-- multi-file function uses the callbacks of a registered single-file table
-- function. "Database.DuckDB.Simple.V2.Copy" has the native callback interface.
module Database.DuckDB.Simple.Copy (
  CopyBindInfo (..),
  LogicalTypeId (..),
  CopyInitInfo (..),
  CopySinkInfo (..),
  CopyFinalizeInfo (..),
  registerCopyToFunction,
  CopyFromInfo (..),
  registerCopyFromFunction,
  CopyTo (..),
  registerCopyFunction,
  TypedRow,
  MultiFileOptions (..),
  defaultMultiFileOptions,
  registerMultiFileFunction,
) where

import Database.DuckDB.Simple.Copy.Internal (
  CopyBindInfo (..),
  CopyFinalizeInfo (..),
  CopyFromInfo (..),
  CopyInitInfo (..),
  CopySinkInfo (..),
  CopyTo (..),
  LogicalTypeId (..),
  MultiFileOptions (..),
  TypedRow,
  defaultMultiFileOptions,
  registerCopyFromFunction,
  registerCopyFunction,
  registerCopyToFunction,
  registerMultiFileFunction,
 )
