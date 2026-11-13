{- |
Module      : Database.DuckDB.Simple.Registration
Description : Typed registration of custom types, replacement scans, and shared declarations.

This module has the declarations that typed registrations share. It also
registers custom types and replacement scans on a connection or an instance.
Its signatures have no native handle. A 'FunctionParameter' from 'parameter' is
a required parameter. The V2 module has the field for the other parameter
kinds. "Database.DuckDB.Simple.V2.Registration" has the registrations with
native callbacks.
-}
module Database.DuckDB.Simple.Registration (
    TypeDeclaration (..),
    DefaultValue,
    defaultValue,
    FunctionParameter (parameterName, parameterType, parameterDefault),
    parameter,
    registerCustomType,
    Replacement (..),
    registerReplacementScan,
    registerInstanceReplacementScan,
) where

import Data.Text (Text)
import Database.DuckDB.Simple.Internal (Connection, Instance)
import Database.DuckDB.Simple.Typed (Replacement (..), replacementScanCallback)
import Database.DuckDB.Simple.V2.Registration (DefaultValue, FunctionParameter (..), RegistrationTarget (ConnectionTarget), TypeDeclaration (..), defaultValue, parameter, registerCustomType)
import qualified Database.DuckDB.Simple.V2.Registration as V2

{- | Register a replacement scan on a connection. DuckDB calls the function
while it plans a query on this connection that reads a table that the catalog
does not have. Other connections of the instance do not use the scan.

The function gets the parts of the table name as the query writes them. A name
without a schema has one part. A qualified name has its catalog and schema
before the table name, for example @[\"memory\", \"main\", \"t\"]@. DuckDB
gives the parts also when the catalog or the schema does not exist. Each part
keeps the case of the query text. DuckDB removes the quotes of a quoted part. A
dot in a quoted part stays in that part. A file path in single quotes, for
example @\'data.csv\'@, is one part without the quotes.

'Nothing' declines the name. DuckDB then asks the next scan. When no scan
replaces the name, the query fails with the usual error for an unknown table.
An exception in the function fails the query with an SQL error that contains
the message of the exception.

DuckDB asks the scans of the connection first, in the order of registration.
Then it asks the scans of the instance, which include the built-in scans for
file paths. The first scan that replaces the name wins. Thus a connection scan
can replace a file path. A scan stays registered until the connection closes.
No function removes a scan.
-}
registerReplacementScan :: Connection -> ([Text] -> IO (Maybe Replacement)) -> IO ()
registerReplacementScan connection = V2.registerReplacementScanOn (ConnectionTarget connection) . replacementScanCallback

{- | Register a replacement scan on an instance. Each connection of the
instance uses the scan until the instance closes. No function removes a scan.
DuckDB asks the scans of a connection before the scans of its instance. It asks
the scans of the instance in the order of registration.
'registerReplacementScan' gives the rules for the function.

Register the scan before a connection runs a query. Registration while another
connection plans a query is not thread safe. The function can run on several
connections at the same time.
-}
registerInstanceReplacementScan :: Instance -> ([Text] -> IO (Maybe Replacement)) -> IO ()
registerInstanceReplacementScan instance' = V2.registerInstanceReplacementScan instance' . replacementScanCallback
