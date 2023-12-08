{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Categorifier.Test.TotOrd
  ( TotOrd (..),
    runTotOrd,
  )
where

import Categorifier.Category (RepCat (..), UnsafeCoerceCat (..))
import qualified Categorifier.Client as Client
import qualified ConCat.Category as ConCat

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

instance (RepCat k a r, con a, con r) => RepCat (ConCat.Constrained con k) a r where
  abstC = ConCat.Constrained abstC
  reprC = ConCat.Constrained reprC

instance (UnsafeCoerceCat k a b) => UnsafeCoerceCat (ConCat.Constrained con k) a b where
  unsafeCoerceK = ConCat.Constrained unsafeCoerceK

instance (Ord b) => ConCat.ConstCat TotOrd b where
  const = TotOrd . ConCat.const

instance ConCat.OkFunctor TotOrd f where
  okFunctor = ConCat.okFunctor @TotOrd

instance (Functor f) => ConCat.FunctorCat TotOrd f where
  fmapC (TotOrd fn) = TotOrd $ ConCat.fmapC fn
  unzipC = TotOrd ConCat.unzipC

instance (Ord a) => ConCat.EqCat TotOrd a where
  equal = TotOrd ConCat.equal

instance (Ord a) => ConCat.OrdCat TotOrd a where
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

instance (Client.HasRep a, r ~ Client.Rep a, Ord a, Ord r) => RepCat TotOrd a r where
  abstC = TotOrd abstC
  reprC = TotOrd reprC

instance UnsafeCoerceCat TotOrd a b where
  unsafeCoerceK = TotOrd unsafeCoerceK

instance (Floating a, Ord a) => ConCat.FloatingCat TotOrd a where
  cosC = TotOrd ConCat.cosC
  expC = TotOrd ConCat.expC
  logC = TotOrd ConCat.logC
  sinC = TotOrd ConCat.sinC
  sqrtC = TotOrd ConCat.sqrtC
  tanhC = TotOrd ConCat.tanhC

instance (Applicative m, Ord a) => ConCat.PointedCat TotOrd m a where
  pointC = TotOrd ConCat.pointC

instance (Ord a) => ConCat.IfCat TotOrd a where
  ifC = TotOrd ConCat.ifC

instance (Ord a, Ord b) => ConCat.BottomCat TotOrd a b where
  bottomC = TotOrd ConCat.bottomC

instance ConCat.TracedCat TotOrd where
  trace (TotOrd fn) = TotOrd $ ConCat.trace fn
