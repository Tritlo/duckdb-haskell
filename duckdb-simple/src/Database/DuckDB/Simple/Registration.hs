{- |
Module      : Database.DuckDB.Simple.Registration
Description : Typed registration of custom types and shared declarations.

This module has the declarations that typed registrations share. It also
registers custom types on a connection. Its signatures have no native handle.
"Database.DuckDB.Simple.V2.Registration" has the registrations with native
callbacks.
-}
module Database.DuckDB.Simple.Registration (
    TypeDeclaration (..),
    DefaultValue,
    defaultValue,
    FunctionParameter (..),
    parameter,
    registerCustomType,
) where

import Database.DuckDB.Simple.V2.Registration (DefaultValue, FunctionParameter (..), TypeDeclaration (..), defaultValue, parameter, registerCustomType)
