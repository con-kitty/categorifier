{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Plugin.TotOrd
  ( TotOrd (..),
    runTotOrd,
  )
where

import qualified ConCat.Category as ConCat
import Kitty.Plugin.Category (RepCat (..))
import qualified Kitty.Plugin.Client as Client

-- | The category of totally ordered sets. This is an example for testing constrained categories
--  (full subcategories). This category is /not/ closed.
--
--   Unfortunately, the definition of constrained categories is tied to the hierarchy, so this is
--   not as general as the other categories we're testing.
newtype TotOrd a b = TotOrd {runTotConstrained :: ConCat.Constrained Ord (->) a b}
  deriving newtype
    ( ConCat.BoolCat,
      ConCat.BraidedPCat,
      ConCat.Category,
      ConCat.CoproductCat,
      ConCat.MonoidalPCat,
      ConCat.MonoidalSCat,
      ConCat.ProductCat
    )

runTotOrd :: TotOrd a b -> a -> b
runTotOrd = (\(ConCat.Constrained f) -> f) . runTotConstrained

-- | Orphan that we should push upstream to ConCat.
instance (Applicative m, con a) => ConCat.PointedCat (ConCat.Constrained con (->)) m a where
  pointC = ConCat.Constrained pure

-- | Orphan that we should push upstream to ConCat.
instance
  (ConCat.EqCat k a, con a, con Bool, ConCat.OpSat (ConCat.Prod k) con) =>
  ConCat.EqCat (ConCat.Constrained con k) a
  where
  equal = ConCat.Constrained ConCat.equal

  notEqual = ConCat.Constrained ConCat.notEqual

-- | Orphan that we should push upstream to ConCat.
instance
  (ConCat.OrdCat k a, con a, con Bool, ConCat.OpSat (ConCat.Prod k) con) =>
  ConCat.OrdCat (ConCat.Constrained con k) a
  where
  lessThan = ConCat.Constrained ConCat.lessThan

  greaterThan = ConCat.Constrained ConCat.greaterThan

  lessThanOrEqual = ConCat.Constrained ConCat.lessThanOrEqual

  greaterThanOrEqual = ConCat.Constrained ConCat.greaterThanOrEqual

instance Ord b => ConCat.ConstCat TotOrd b where
  const = TotOrd . ConCat.const

instance ConCat.OkFunctor TotOrd f where
  okFunctor = ConCat.okFunctor @TotOrd

instance Functor f => ConCat.FunctorCat TotOrd f where
  fmapC (TotOrd fn) = TotOrd $ ConCat.fmapC fn

  unzipC = TotOrd ConCat.unzipC

instance Ord a => ConCat.EqCat TotOrd a where
  equal = TotOrd ConCat.equal

instance Ord a => ConCat.OrdCat TotOrd a where
  lessThan = TotOrd ConCat.lessThan

instance (Num a, Ord a) => ConCat.NumCat TotOrd a where
  negateC = TotOrd ConCat.negateC

  addC = TotOrd ConCat.addC

  subC = TotOrd ConCat.subC

  mulC = TotOrd ConCat.mulC

  powIC = TotOrd ConCat.powIC

instance (Integral a, Num b, Ord b) => ConCat.FromIntegralCat TotOrd a b where
  fromIntegralC = TotOrd ConCat.fromIntegralC

instance (Fractional a, Ord a) => ConCat.FractionalCat TotOrd a where
  divideC = TotOrd ConCat.divideC

  recipC = TotOrd ConCat.recipC

instance (Ord a, Ord b) => ConCat.CoerceCat TotOrd a b where
  coerceC = TotOrd ConCat.coerceC

instance (Client.HasRep a, r ~ Client.Rep a, Ord a, Ord r) => RepCat TotOrd a r where
  abstC = TotOrd abstC

  reprC = TotOrd reprC

instance (Floating a, Ord a) => ConCat.FloatingCat TotOrd a where
  cosC = TotOrd ConCat.cosC

  expC = TotOrd ConCat.expC

  logC = TotOrd ConCat.logC

  sinC = TotOrd ConCat.sinC

instance (Applicative m, Ord a) => ConCat.PointedCat TotOrd m a where
  pointC = TotOrd ConCat.pointC

instance Ord a => ConCat.IfCat TotOrd a where
  ifC = TotOrd ConCat.ifC

instance (Ord a, Ord b) => ConCat.BottomCat TotOrd a b where
  bottomC = TotOrd ConCat.bottomC
