{-# LANGUAGE TypeFamilies #-}

module Test.Plugin.Term
  ( Term (..),
    binaryZero,
    unaryZero,
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
import Kitty.Plugin.Category (RepCat (..))
import qualified Kitty.Plugin.Client as Client

-- | The terminal object in __Cat__ -- a category with only a single object (up to unique
--   isomorphism), whose only arrow is its identity.
data Term a b = ZeroId
  deriving (Show)

unaryZero :: Term a b -> Term c d
unaryZero ZeroId = ZeroId

binaryZero :: Term a b -> Term c d -> Term e f
binaryZero ZeroId ZeroId = ZeroId

instance Base.Category Term where
  id = ZeroId

  (.) = binaryZero

instance Base.Arrow Term where
  arr _ = ZeroId

  first = unaryZero

instance Base.ArrowChoice Term where
  (+++) = binaryZero

instance Base.ArrowApply Term where
  app = ZeroId

instance Base.ArrowLoop Term where
  loop = unaryZero

instance Categories.Functor f Term Term where
  fmap = unaryZero

instance Categories.PFunctor f Term Term where
  first = unaryZero

instance Categories.QFunctor f Term Term where
  second = unaryZero

instance Categories.Bifunctor t Term Term Term where
  bimap = binaryZero

instance Categories.Associative Term t where
  associate = ZeroId

  disassociate = ZeroId

instance Categories.Monoidal Term t where
  type Id Term t = ()

  idl = ZeroId

  idr = ZeroId

  coidl = ZeroId

  coidr = ZeroId

instance Categories.Braided Term t where
  braid = ZeroId

instance Categories.Symmetric Term t

instance Categories.Cartesian Term where
  type Product Term = (,)

  fst = ZeroId

  snd = ZeroId

  diag = ZeroId

instance Categories.CoCartesian Term where
  type Sum Term = Either

  inl = ZeroId

  inr = ZeroId

  codiag = ZeroId

instance Categories.CCC Term where
  type Exp Term = (->)

  apply = ZeroId

  curry = unaryZero

  uncurry = unaryZero

instance Categories.Distributive Term where
  distribute = ZeroId

instance (Client.Rep a ~ r) => RepCat Term a r where
  abstC = ZeroId

  reprC = ZeroId

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

instance ConCat.CoerceCat Term a b where
  coerceC = ZeroId

instance ConCat.RepresentableCat Term f where
  tabulateC = ZeroId

  indexC = ZeroId

instance ConCat.FloatingCat Term a where
  cosC = ZeroId

  expC = ZeroId

  logC = ZeroId

  sinC = ZeroId

instance ConCat.PointedCat Term m a where
  pointC = ZeroId

instance ConCat.IfCat Term a where
  ifC = ZeroId

instance ConCat.BottomCat Term a b where
  bottomC = ZeroId
