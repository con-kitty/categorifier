{-# OPTIONS_GHC -Wno-orphans #-}

-- | Orphans and re-exports combining our type class hierarchies with the plugin test categories.
module Test.Cat.Categories
  ( Hask (..),
    Term (..),
  )
where

import Data.Bifunctor (Bifunctor (..))
import qualified Kitty.UnconCat
import Test.Plugin.ConCat.Instances (Hask (..), Term (..), binaryZero, unaryZero)

------------------------------------------------------------------------------

-- * Hask UnconCat instances

instance Kitty.UnconCat.Category Hask where
  id = Hask Kitty.UnconCat.id
  Hask f . Hask g = Hask (f Kitty.UnconCat.. g)

instance Kitty.UnconCat.AssociativePCat Hask

instance Kitty.UnconCat.ClosedCat Hask where
  apply = Hask (Kitty.UnconCat.uncurry ($))
  curry (Hask f) = Hask (curry f)
  uncurry (Hask f) = Hask (uncurry f)

instance Kitty.UnconCat.CoproductCat Hask where
  inl = Hask Left
  inr = Hask Right
  jam =
    Hask
      ( \case
          Left x -> x
          Right y -> y
      )

instance Kitty.UnconCat.MonoidalPCat Hask where
  Hask f *** Hask g = Hask (bimap f g)

instance Kitty.UnconCat.ProductCat Hask where
  exl = Hask fst
  exr = Hask snd
  dup = Hask (\x -> (x, x))

------------------------------------------------------------------------------

-- * Term UnconCat instances

instance Kitty.UnconCat.Category Term where
  id = ZeroId
  (.) = binaryZero

instance Kitty.UnconCat.AssociativePCat Term

instance Kitty.UnconCat.ClosedCat Term where
  apply = ZeroId
  curry = unaryZero
  uncurry = unaryZero

instance Kitty.UnconCat.CoproductCat Term where
  inl = ZeroId
  inr = ZeroId
  jam = ZeroId

instance Kitty.UnconCat.MonoidalPCat Term where
  (***) = binaryZero

instance Kitty.UnconCat.ProductCat Term where
  exl = ZeroId
  exr = ZeroId
  dup = ZeroId
