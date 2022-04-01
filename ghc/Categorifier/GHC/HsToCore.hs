{-# LANGUAGE CPP #-}

module Categorifier.GHC.HsToCore
  ( module DsBinds,
    module DsMonad,
  )
where

#if MIN_VERSION_ghc(9, 0, 0)
import GHC.HsToCore.Binds as DsBinds
import GHC.HsToCore.Monad as DsMonad
#else
import DsBinds
import DsMonad
#endif
