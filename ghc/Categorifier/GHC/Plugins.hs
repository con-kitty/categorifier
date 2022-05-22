{-# LANGUAGE CPP #-}

module Categorifier.GHC.Plugins
  ( module Plugins,
  )
where

#if MIN_VERSION_ghc(9, 0, 0)
import GHC.Plugins as Plugins
#else
import GhcPlugins as Plugins
#endif
