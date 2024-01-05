{-# LANGUAGE CPP #-}
#if MIN_VERSION_ghc(9, 4, 0)
#else
-- -Wno-orphans is so we can add missing instances to `Bag.Bag`
{-# OPTIONS_GHC -Wno-orphans #-}
#endif

module Categorifier.GHC.Data
  ( module Bag,
    module FastString,
    module Pair,
  )
where

#if MIN_VERSION_ghc(9, 0, 0)
import GHC.Data.Bag as Bag
import GHC.Data.FastString as FastString
import GHC.Data.Pair as Pair
#else
import Bag
import FastString
import Pair
#endif

#if MIN_VERSION_ghc(9, 4, 0)
#else
-- | Need this instance to use a `Bag.Bag` as the output of @RWST@.
instance Semigroup (Bag.Bag a) where
  (<>) = Bag.unionBags

-- | Need this instance to use a `Bag.Bag` as the output of @RWST@.
instance Monoid (Bag.Bag a) where
  mempty = Bag.emptyBag
#endif
