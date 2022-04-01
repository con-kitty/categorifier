{-# LANGUAGE CPP #-}

module Categorifier.GHC.Runtime
  (
#if MIN_VERSION_ghc(9, 2, 0)
    module GHC.Runtime.Context,
#endif
    module DynamicLoading,
    lookupRdrNameInModuleForPlugins,
  )
where

import qualified Categorifier.GHC.Driver as Driver
import qualified Categorifier.GHC.Types as Types
import qualified Categorifier.GHC.Unit as Unit
#if MIN_VERSION_ghc(9, 2, 0)
import GHC.Runtime.Context
#endif
#if MIN_VERSION_ghc(9, 0, 0)
import GHC.Runtime.Loader as DynamicLoading hiding (lookupRdrNameInModuleForPlugins)
import qualified GHC.Runtime.Loader as DynamicLoading
#else
import DynamicLoading hiding (lookupRdrNameInModuleForPlugins)
import qualified DynamicLoading
#endif

lookupRdrNameInModuleForPlugins ::
  Driver.HscEnv -> Unit.ModuleName -> Types.RdrName -> IO (Maybe Types.Name)
#if MIN_VERSION_ghc(8, 6, 0)
lookupRdrNameInModuleForPlugins hscEnv modu =
  fmap (fmap fst) . DynamicLoading.lookupRdrNameInModuleForPlugins hscEnv modu
#else
lookupRdrNameInModuleForPlugins = DynamicLoading.lookupRdrNameInModuleForPlugins
#endif
