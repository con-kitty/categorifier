-- `PolyKinds` ensures the derived HasRep instances are fully polymorphic. In future, we could try
-- to make this explicit in `deriveHasRep`.
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | The only module that should be needed by code that is in the dependency graph of calls to
--  `Kitty.Plugin.Categorize.expression`. If the module actually /calls/
--  `Kitty.Plugin.Categorize.expression`, then it will also need to
--   @import qualified "Kitty.Plugin.Categorize" as Categorize@.
module Kitty.Plugin.Client
  ( HasRep (..),
    deriveHasRep,

    -- * testing
    abstReturn,
    reprReturn,
  )
where

import Data.Functor.Compose (Compose)
import Data.Functor.Identity (Identity)
import qualified Data.Functor.Product as Functor
import qualified Data.Functor.Sum as Functor
import Data.Complex (Complex)
import Data.List.NonEmpty (NonEmpty)
import Data.Proxy (Proxy (..))
import Data.Ratio (Ratio)
import qualified Data.Semigroup as Semigroup
import qualified GHC.Generics as Generic
import Kitty.Plugin.Client.Internal (HasRep (..), deriveHasRep)

-- | This property should be true for every `HasRep` instance. `r` should be a classifying object,
--   and the first argument a comparison (e.g., `Bool` and `==` or `Hedgehog.Property` and
--  `Hedgehog.===`) to make it easy to test.
abstReturn :: forall a r. HasRep a => Proxy a -> (Rep a -> Rep a -> r) -> Rep a -> r
abstReturn Proxy eq = eq <$> id <*> repr . abst @a

-- | This property should be true for every `HasRep` instance. `r` should be a classifying object,
--   and the first argument a comparison (e.g., `Bool` and `==` or `Hedgehog.Property` and
--  `Hedgehog.===`) to make it easy to test.
reprReturn :: HasRep a => (a -> a -> r) -> a -> r
reprReturn eq = eq <$> id <*> abst . repr

deriveHasRep ''[]
deriveHasRep ''(,,)
deriveHasRep ''(,,,)
deriveHasRep ''(,,,,)
deriveHasRep ''(,,,,,)
deriveHasRep ''(,,,,,,)
deriveHasRep ''(,,,,,,,)
deriveHasRep ''(,,,,,,,,)
deriveHasRep ''(,,,,,,,,,)
deriveHasRep ''(,,,,,,,,,,)
deriveHasRep ''(,,,,,,,,,,,)
deriveHasRep ''(,,,,,,,,,,,,)
deriveHasRep ''(,,,,,,,,,,,,,)
deriveHasRep ''(,,,,,,,,,,,,,,)
deriveHasRep ''(,,,,,,,,,,,,,,,)
deriveHasRep ''(,,,,,,,,,,,,,,,,)
deriveHasRep ''(,,,,,,,,,,,,,,,,,)
deriveHasRep ''(,,,,,,,,,,,,,,,,,,)
deriveHasRep ''(,,,,,,,,,,,,,,,,,,,)
deriveHasRep ''(,,,,,,,,,,,,,,,,,,,,)
deriveHasRep ''(,,,,,,,,,,,,,,,,,,,,,)
deriveHasRep ''(,,,,,,,,,,,,,,,,,,,,,,)
deriveHasRep ''(,,,,,,,,,,,,,,,,,,,,,,,)
deriveHasRep ''(,,,,,,,,,,,,,,,,,,,,,,,,)
deriveHasRep ''(,,,,,,,,,,,,,,,,,,,,,,,,,)
deriveHasRep ''(,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriveHasRep ''Complex
deriveHasRep ''Compose
deriveHasRep ''Functor.Product
deriveHasRep ''Functor.Sum
deriveHasRep ''(Generic.:+:)
deriveHasRep ''(Generic.:*:)
deriveHasRep ''(Generic.:.:)
deriveHasRep ''Generic.K1
deriveHasRep ''Generic.M1
deriveHasRep ''Generic.Par1
deriveHasRep ''Generic.U1
deriveHasRep ''Identity
deriveHasRep ''Maybe
deriveHasRep ''NonEmpty
deriveHasRep ''Ordering
deriveHasRep ''Proxy
deriveHasRep ''Ratio
deriveHasRep ''Semigroup.All
deriveHasRep ''Semigroup.Any
deriveHasRep ''Semigroup.Dual
deriveHasRep ''Semigroup.Endo
deriveHasRep ''Semigroup.Max
deriveHasRep ''Semigroup.Min
deriveHasRep ''Semigroup.Product
deriveHasRep ''Semigroup.Sum
