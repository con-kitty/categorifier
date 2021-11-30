{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Plugin.Hask
  ( Hask (..),
  )
where

import qualified ConCat.Category as ConCat
import qualified Control.Arrow as Base
import qualified Control.Categorical.Bifunctor as Categories
import qualified Control.Categorical.Functor as Categories
import qualified Control.Category as Base
import qualified Control.Category.Associative as Categories
import qualified Control.Category.Braided as Categories
import qualified Control.Category.Cartesian as Categories
import qualified Control.Category.Cartesian.Closed as Categories
import qualified Control.Category.Distributive as Categories
import qualified Control.Category.Monoidal as Categories
import Data.Constraint (Dict (..), (:-) (..))
import Kitty.Plugin.Category (ReferenceCat (..))

-- | A trivial wrapper around __Hask__ for testing purposes.
data Hask a b = Hask {runHask :: a -> b}
{-# ANN Hask "Hlint: ignore Use newtype instead of data" #-}

instance Base.Category Hask where
  id = Hask Base.id

  Hask f . Hask g = Hask (f Base.. g)

instance Base.Arrow Hask where
  arr = Hask

  first (Hask f) = Hask (Base.first f)

instance Base.ArrowApply Hask where
  app = Hask (Base.app . Base.first runHask)

instance Base.ArrowChoice Hask where
  Hask f +++ Hask g = Hask (f Base.+++ g)

instance Base.ArrowLoop Hask where
  loop (Hask f) = Hask (Base.loop f)

instance Categories.Functor f (->) (->) => Categories.Functor f Hask Hask where
  fmap (Hask f) = Hask (Categories.fmap f)

instance Categories.PFunctor f (->) (->) => Categories.PFunctor f Hask Hask where
  first (Hask f) = Hask (Categories.first f)

instance Categories.QFunctor f (->) (->) => Categories.QFunctor f Hask Hask where
  second (Hask g) = Hask (Categories.second g)

instance Categories.Bifunctor t (->) (->) (->) => Categories.Bifunctor t Hask Hask Hask where
  bimap (Hask f) (Hask g) = Hask (Categories.bimap f g)

instance Categories.Associative (->) t => Categories.Associative Hask t where
  associate = Hask Categories.associate

  disassociate = Hask Categories.disassociate

instance Categories.Monoidal (->) t => Categories.Monoidal Hask t where
  type Id Hask t = Categories.Id (->) t

  idl = Hask Categories.idl

  idr = Hask Categories.idr

  coidl = Hask Categories.coidl

  coidr = Hask Categories.coidr

instance Categories.Braided (->) t => Categories.Braided Hask t where
  braid = Hask Categories.braid

instance Categories.Symmetric (->) t => Categories.Symmetric Hask t

instance Categories.Cartesian Hask where
  type Product Hask = (,)

  fst = Hask Categories.fst

  snd = Hask Categories.snd

  diag = Hask Categories.diag

instance Categories.CoCartesian Hask where
  type Sum Hask = Either

  inl = Hask Categories.inl

  inr = Hask Categories.inr

  codiag = Hask Categories.codiag

instance Categories.CCC Hask where
  type Exp Hask = (->)

  apply = Hask Categories.apply

  curry (Hask f) = Hask (Categories.curry f)

  uncurry (Hask f) = Hask (Categories.uncurry f)

instance Categories.Distributive Hask where
  distribute = Hask Categories.distribute

instance ConCat.Category Hask where
  id = Hask ConCat.id

  Hask f . Hask g = Hask (f ConCat.. g)

instance ConCat.ConstCat Hask b where
  const a = Hask (const a)

instance ConCat.ProductCat Hask where
  exl = Hask fst

  exr = Hask snd

  dup = Hask (\x -> (x, x))

instance ConCat.CoproductCat Hask where
  inl = Hask Left

  inr = Hask Right

  jam =
    Hask
      ( \case
          Left x -> x
          Right y -> y
      )

instance ConCat.BoolCat Hask where
  notC = Hask not

  andC = Hask $ uncurry (&&)

  orC = Hask $ uncurry (||)

  xorC = Hask $ \(a, b) -> a /= b

instance ConCat.AssociativePCat Hask

instance ConCat.MonoidalPCat Hask where
  Hask f *** Hask g = Hask (f Base.*** g)

instance ConCat.BraidedPCat Hask

instance ConCat.MonoidalSCat Hask where
  Hask f +++ Hask g = Hask (f Base.+++ g)

instance ConCat.DistribCat Hask where
  distl = Hask ConCat.distl

instance ConCat.ClosedCat Hask where
  apply = Hask (ConCat.uncurry ($))

  curry (Hask f) = Hask (curry f)

  uncurry (Hask f) = Hask (uncurry f)

instance ConCat.OkFunctor Hask f where
  okFunctor = ConCat.Entail (Sub Dict)

instance Functor f => ConCat.FunctorCat Hask f where
  fmapC (Hask fn) = Hask (ConCat.fmapC fn)

  unzipC = Hask ConCat.unzipC

instance Functor f => ConCat.Strong Hask f where
  strength = Hask ConCat.strength

instance Eq a => ConCat.EqCat Hask a where
  equal = Hask ConCat.equal

  notEqual = Hask ConCat.notEqual

instance Ord a => ConCat.OrdCat Hask a where
  lessThan = Hask ConCat.lessThan

  greaterThan = Hask ConCat.greaterThan

  lessThanOrEqual = Hask ConCat.lessThanOrEqual

  greaterThanOrEqual = Hask ConCat.greaterThanOrEqual

instance Ord a => ConCat.MinMaxCat Hask a where
  minC = Hask ConCat.minC

  maxC = Hask ConCat.maxC

instance (Integral a, Num b) => ConCat.FromIntegralCat Hask a b where
  fromIntegralC = Hask ConCat.fromIntegralC

instance Num a => ConCat.NumCat Hask a where
  negateC = Hask ConCat.negateC

  addC = Hask ConCat.addC

  subC = Hask ConCat.subC

  mulC = Hask ConCat.mulC

  powIC = Hask ConCat.powIC

instance Floating a => ConCat.FloatingCat Hask a where
  cosC = Hask ConCat.cosC

  expC = Hask ConCat.expC

  logC = Hask ConCat.logC

  sinC = Hask ConCat.sinC

instance Fractional a => ConCat.FractionalCat Hask a where
  divideC = Hask ConCat.divideC

  recipC = Hask ConCat.recipC

instance ConCat.CoerceCat Hask a b where
  coerceC = Hask ConCat.coerceC

instance ConCat.RepCat (->) a r => ConCat.RepCat Hask a r where
  abstC = Hask ConCat.abstC

  reprC = Hask ConCat.reprC

instance ConCat.RepresentableCat (->) f => ConCat.RepresentableCat Hask f where
  tabulateC = Hask ConCat.tabulateC

  indexC = Hask ConCat.indexC

-- | This doesn't use @`ConCat.PointedCat` (->)@ because it brings in an unwanted dependency on the
--   pointed library.
instance Applicative m => ConCat.PointedCat Hask m a where
  pointC = Hask pure

instance ConCat.BottomCat Hask a b where
  bottomC = Hask ConCat.bottomC

instance Integral a => ConCat.IntegralCat Hask a where
  divC = Hask ConCat.divC
  modC = Hask ConCat.modC

instance ConCat.IfCat Hask a where
  ifC = Hask ConCat.ifC

instance ReferenceCat Hask a b
