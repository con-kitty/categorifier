{-# LANGUAGE CPP #-}

module Categorifier.GHC.Runtime
  ( module DynamicLoading,
    module HscTypes,
  )
where

#if MIN_VERSION_ghc(9, 0, 0)
#if MIN_VERSION_ghc(9, 2, 0)
import GHC.Runtime.Context as HscTypes
#else
import GHC.Driver.Types as HscTypes
#endif
import GHC.Runtime.Loader as DynamicLoading
#else
import DynamicLoading
import HscTypes hiding (ModGuts (..))
#endif
