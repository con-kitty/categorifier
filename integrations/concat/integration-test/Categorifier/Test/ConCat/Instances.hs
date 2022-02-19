{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Categorifier.Test.ConCat.Instances
  ( module Categorifier.Test.Hask,
    module Categorifier.Test.Term,
  )
where

import Categorifier.Category (RepCat (..))
import qualified Categorifier.Client as Client
import Categorifier.Test.Hask (Hask (..))
import Categorifier.Test.Term (Term (..), binaryZero, unaryZero)
import qualified ConCat.Category as ConCat
import ConCat.Circuit ((:>))
import qualified ConCat.Rep as ConCat
import ConCat.Syntactic (Syn, app0')
import qualified Control.Arrow as Base
import Data.Constraint (Dict (..), (:-) (..))

instance
  ( Client.HasRep a,
    r ~ Client.Rep a,
    ConCat.Ok (:>) a,
    ConCat.Ok (:>) r,
    -- __NB__: This constraint is only because "ConCat.Circuit" doesn't export enough for us to
    --         define this instance directly.
    r ~ ConCat.Rep a
  ) =>
  RepCat (:>) a r
  where
  reprC = ConCat.reprC
  abstC = ConCat.abstC

instance (Client.HasRep a, r ~ Client.Rep a) => RepCat Syn a r where
  reprC = app0' "repr"
  abstC = app0' "abst"

-- Term

instance ConCat.Category Term where
  id = ZeroId
  (.) = binaryZero

instance ConCat.BoolCat Term where
  notC = ZeroId
  andC = ZeroId
  orC = ZeroId
  xorC = ZeroId

instance ConCat.ConstCat Term b where
  const _ = ZeroId

instance ConCat.ProductCat Term where
  exl = ZeroId
  exr = ZeroId
  dup = ZeroId

instance ConCat.CoproductCat Term where
  inl = ZeroId
  inr = ZeroId
  jam = ZeroId

instance ConCat.AssociativePCat Term

instance ConCat.MonoidalPCat Term where
  (***) = binaryZero

instance ConCat.BraidedPCat Term

instance ConCat.MonoidalSCat Term where
  (+++) = binaryZero

instance ConCat.DistribCat Term where
  distl = ZeroId

instance ConCat.ClosedCat Term where
  apply = ZeroId
  curry = unaryZero
  uncurry = unaryZero

instance ConCat.OkFunctor Term f where
  okFunctor = ConCat.Entail (Sub Dict)

-- | __TODO__: It seems weird that we need a `Functor` instance here.
instance Functor f => ConCat.FunctorCat Term f where
  fmapC = unaryZero
  unzipC = ZeroId

instance Functor f => ConCat.Strong Term f where
  strength = ZeroId

instance ConCat.TraversableCat Term t f where
  sequenceAC = ZeroId

instance ConCat.EqCat Term a where
  equal = ZeroId

-- | Overconstrained, because the class in ConCat is overconstrained.
instance Ord a => ConCat.OrdCat Term a where
  lessThan = ZeroId

instance ConCat.MinMaxCat Term a where
  minC = ZeroId
  maxC = ZeroId

instance ConCat.NumCat Term a where
  negateC = ZeroId
  addC = ZeroId
  subC = ZeroId
  mulC = ZeroId
  powIC = ZeroId

instance ConCat.IntegralCat Term a where
  divC = ZeroId
  modC = ZeroId

instance ConCat.FromIntegralCat Term a b where
  fromIntegralC = ZeroId

instance ConCat.FractionalCat Term a where
  divideC = ZeroId
  recipC = ZeroId

instance ConCat.RepresentableCat Term f where
  tabulateC = ZeroId
  indexC = ZeroId

instance ConCat.FloatingCat Term a where
  cosC = ZeroId
  expC = ZeroId
  logC = ZeroId
  sinC = ZeroId
  sqrtC = ZeroId

instance ConCat.PointedCat Term m a where
  pointC = ZeroId

instance ConCat.IfCat Term a where
  ifC = ZeroId

instance ConCat.BottomCat Term a b where
  bottomC = ZeroId

instance ConCat.TracedCat Term where
  trace = unaryZero

-- Hask

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

instance ConCat.TraversableCat (->) t f => ConCat.TraversableCat Hask t f where
  sequenceAC = Hask ConCat.sequenceAC

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
  sqrtC = Hask ConCat.sqrtC

instance Fractional a => ConCat.FractionalCat Hask a where
  divideC = Hask ConCat.divideC
  recipC = Hask ConCat.recipC

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

instance ConCat.TracedCat Hask where
  trace (Hask f) = Hask (ConCat.trace f)
