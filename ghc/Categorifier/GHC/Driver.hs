{-# LANGUAGE CPP #-}

module Categorifier.GHC.Driver
  ( module DynFlags,
    module HscTypes,
    module Outputable,
    module Plugins,
  )
where

#if MIN_VERSION_ghc(9, 0, 0)
#if MIN_VERSION_ghc(9, 2, 0)
import GHC.Driver.Env.Types as HscTypes
import GHC.Driver.Ppr as Outputable
#else
import GHC.Driver.Types as HscTypes hiding
  ( InteractiveContext (..),
    InteractiveImport (..),
    ModGuts (..),
  )
import GHC.Utils.Outputable as Outputable hiding (Outputable (..), renderWithStyle)
#endif
import GHC.Driver.Plugins as Plugins
import GHC.Driver.Session as DynFlags
#else
import DynFlags
import HscTypes hiding (InteractiveContext (..), InteractiveImport (..), ModGuts (..))
import Outputable hiding (Outputable (..), renderWithStyle)
import Plugins
#endif
