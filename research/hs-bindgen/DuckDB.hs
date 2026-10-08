{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Generate the DuckDB C API and Arrow layouts from their headers.
module DuckDB where

import HsBindgen.Runtime.LibC qualified
import HsBindgen.TH qualified as Bindgen

let config :: Bindgen.Config
    config = Bindgen.def
 in Bindgen.withHsBindgen
        config
            { Bindgen.clang = config.clang{Bindgen.extraIncludeDirs = [Bindgen.PkgDir "../../duckdb-ffi/cbits"]}
            , Bindgen.fieldNamingStrategy = Bindgen.OmitFieldPrefixes
            }
        Bindgen.def
        $ do
            Bindgen.hashInclude "duckdb.h"
            Bindgen.hashInclude "duckdb_arrow.h"
