{-# OPTIONS_GHC -Wno-orphans #-}

-- | Orphans and re-exports combining our type class hierarchies with the plugin test categories.
module Kitty.Plugin.Test.UnconCat.Instances
  ( Hask (..),
    Term (..),
  )
where

import Data.Bifunctor (Bifunctor (..))
import qualified Kitty.Plugin.UnconCat as UnconCat
import Kitty.Plugin.Test.ConCat.Instances (Hask (..), Term (..), binaryZero, unaryZero)

------------------------------------------------------------------------------

-- * Hask UnconCat instances

instance UnconCat.Category Hask where
  id = Hask UnconCat.id
  Hask f . Hask g = Hask (f UnconCat.. g)

instance UnconCat.AssociativePCat Hask

instance UnconCat.ClosedCat Hask where
  apply = Hask (UnconCat.uncurry ($))
  curry (Hask f) = Hask (curry f)
  uncurry (Hask f) = Hask (uncurry f)

instance UnconCat.CoproductCat Hask where
  inl = Hask Left
  inr = Hask Right
  jam =
    Hask
      ( \case
          Left x -> x
          Right y -> y
      )

instance UnconCat.MonoidalPCat Hask where
  Hask f *** Hask g = Hask (bimap f g)

instance UnconCat.ProductCat Hask where
  exl = Hask fst
  exr = Hask snd
  dup = Hask (\x -> (x, x))

------------------------------------------------------------------------------

-- * Term UnconCat instances

instance UnconCat.Category Term where
  id = ZeroId
  (.) = binaryZero

instance UnconCat.AssociativePCat Term

instance UnconCat.ClosedCat Term where
  apply = ZeroId
  curry = unaryZero
  uncurry = unaryZero

instance UnconCat.CoproductCat Term where
  inl = ZeroId
  inr = ZeroId
  jam = ZeroId

instance UnconCat.MonoidalPCat Term where
  (***) = binaryZero

instance UnconCat.ProductCat Term where
  exl = ZeroId
  exr = ZeroId
  dup = ZeroId
