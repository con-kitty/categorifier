{-# LANGUAGE CPP #-}

module Categorifier.GHC.Runtime
  ( module DynamicLoading,
    module HscTypes,
  )
where

import qualified Categorifier.GHC.Driver as Driver
import qualified Categorifier.GHC.Types as Types
import qualified Categorifier.GHC.Unit as Unit
#if MIN_VERSION_ghc(9, 0, 0)
#if MIN_VERSION_ghc(9, 2, 0)
import GHC.Runtime.Context as HscTypes
#else
import GHC.Driver.Types as HscTypes
#endif
import GHC.Runtime.Loader as DynamicLoading
#else
import DynamicLoading
import HscTypes
#endif
