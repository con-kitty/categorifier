{-# LANGUAGE CPP #-}

module Categorifier.GHC.Unit
  ( module Finder,
    module Module,
  )
where

#if MIN_VERSION_ghc(9, 0, 0)
#if MIN_VERSION_ghc(9, 2, 0)
import GHC.Unit.Finder as Finder
import GHC.Unit.Module.Deps as Module
import GHC.Unit.Module.ModGuts as Module
#else
import GHC.Driver.Finder as Finder
#endif
import GHC.Unit.Module.Name as Module
import GHC.Unit.Types as Module
#else
import Finder
import Module
#endif
