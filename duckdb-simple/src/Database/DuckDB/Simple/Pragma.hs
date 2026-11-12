-- | Managed query and execution callbacks for native PRAGMA registration.
module Database.DuckDB.Simple.Pragma (
    PragmaCallback (..),
    registerPragmaFunction,
    registerPragmaFunctionOn,
    registerPragmaFunctionWith,
) where

import Data.Text (Text)
import Database.DuckDB.Simple.Internal (Connection)
import Database.DuckDB.Simple.V2.Pragma (PragmaCallback (..), registerPragmaFunctionOn, registerPragmaFunctionWith)
import Database.DuckDB.Simple.V2.Registration (FunctionParameter, RegistrationTarget (..))

-- | Register a pragma with named, default, or variadic parameters and no return type.
registerPragmaFunction :: Connection -> Text -> [FunctionParameter] -> PragmaCallback -> IO ()
registerPragmaFunction = registerPragmaFunctionOn . ConnectionTarget
