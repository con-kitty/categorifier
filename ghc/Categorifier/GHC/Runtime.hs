{-# LANGUAGE CPP #-}

module Categorifier.GHC.Runtime
  ( module DynamicLoading,
    module HscTypes,
    getValueSafely,
  )
where

import qualified Categorifier.GHC.Core as Core
import qualified Categorifier.GHC.Driver as Driver
import qualified Categorifier.GHC.Types as Types
#if MIN_VERSION_ghc(9, 0, 0)
#if MIN_VERSION_ghc(9, 2, 0)
import GHC.Runtime.Context as HscTypes
#else
import GHC.Driver.Types as HscTypes
#endif
#if MIN_VERSION_ghc(9, 4, 0)
import Data.Bifunctor (Bifunctor (bimap))
import GHC.Runtime.Loader as DynamicLoading hiding (getValueSafely)
import qualified GHC.Runtime.Loader as DynamicLoading
#else
import GHC.Runtime.Loader as DynamicLoading
#endif
#else
import DynamicLoading
import HscTypes hiding (ModGuts (..))
#endif

getValueSafely :: Driver.HscEnv -> Types.Name -> Core.Type -> IO (Either (Maybe Core.Type) a)
#if MIN_VERSION_ghc(9, 4, 0)
getValueSafely env name = fmap (bimap pure (\(a, _, _) -> a)) . DynamicLoading.getValueSafely env name
#else
getValueSafely env name = maybe (Left Nothing) Right DynamicLoading.getValueSafely env name
#endif
