{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Orphan instances for the plugin test categories. This also re-exports the types for the
--   categories to make using it less fraught. I.e., if you need this, you should get the types from
--   here rather than from "Categorifier.Test.Hask", etc.
module Categorifier.Test.ConCatExtensions.Instances (Hask (..), Term (..), TotOrd (..)) where

import Categorifier.ConCatExtensions
  ( ApplicativeCat (..),
    BindableCat (..),
    FixedCat (..),
    FloatingCat' (..),
    FloatingPointClassifyCat (..),
    FloatingPointConvertCat (..),
    IntegralCat' (..),
    LaxMonoidalFunctorCat (..),
    MonadCat (..),
    NumCat' (..),
    OrdCat' (..),
    PowICat (..),
    RealToFracCat (..),
    SemigroupCat (..),
    TranscendentalCat (..),
    TraversableCat' (..),
  )
import Categorifier.Test.ConCat.Instances (Hask (..), Term (..), unaryZero)
import Categorifier.Test.TotOrd (TotOrd (..))
import qualified ConCat.Category as ConCat
import qualified GHC.Float
import qualified GHC.Real

-- Term

instance PowICat Term a where
  powIK _ = ZeroId

instance IntegralCat' Term a where
  quotK = ZeroId
  remK = ZeroId

instance FloatingCat' Term m where
  powK = ZeroId

instance TranscendentalCat Term a where
  tanK = ZeroId
  asinK = ZeroId
  acosK = ZeroId
  atanK = ZeroId
  sinhK = ZeroId
  coshK = ZeroId
  tanhK = ZeroId
  asinhK = ZeroId
  acoshK = ZeroId
  atanhK = ZeroId

instance FloatingPointConvertCat Term where
  floatToDoubleK = ZeroId
  doubleToFloatK = ZeroId

instance FloatingPointClassifyCat Term a where
  isNegativeZeroK = ZeroId
  isInfiniteK = ZeroId
  isFiniteK = ZeroId
  isNaNK = ZeroId
  isDenormalK = ZeroId

instance RealToFracCat Term a b where
  realToFracK = ZeroId

instance LaxMonoidalFunctorCat Term m where
  liftA2K = unaryZero

instance ApplicativeCat Term m where
  apK = ZeroId

instance Functor m => MonadCat Term m where
  joinK = ZeroId
  mmapK = unaryZero

instance Functor m => BindableCat Term m where
  bindK = ZeroId

instance TraversableCat' Term t f where
  traverseK = unaryZero

instance NumCat' Term a where
  absK = ZeroId

  signumK = ZeroId

instance SemigroupCat Term m where
  appendK = ZeroId

instance FixedCat Term where
  fixK = unaryZero

-- Hask

instance Ord a => OrdCat' Hask a where
  compareK = Hask compareK

instance Num a => PowICat Hask a where
  powIK i = Hask (powIK i)

instance LaxMonoidalFunctorCat (->) m => LaxMonoidalFunctorCat Hask m where
  liftA2K (Hask f) = Hask $ liftA2K f

instance ApplicativeCat (->) m => ApplicativeCat Hask m where
  apK = Hask apK

instance MonadCat (->) m => MonadCat Hask m where
  joinK = Hask joinK
  mmapK (Hask fn) = Hask $ mmapK fn

instance BindableCat (->) m => BindableCat Hask m where
  bindK = Hask bindK

instance TraversableCat' (->) t f => TraversableCat' Hask t f where
  traverseK (Hask fn) = Hask (traverseK fn)

instance NumCat' (->) m => NumCat' Hask m where
  absK = Hask absK
  signumK = Hask signumK

instance Integral a => IntegralCat' Hask a where
  quotK = Hask quotK
  remK = Hask remK

instance FloatingCat' (->) m => FloatingCat' Hask m where
  powK = Hask powK

instance (Floating a, TranscendentalCat (->) a) => TranscendentalCat Hask a where
  tanK = Hask tanK
  asinK = Hask asinK
  acosK = Hask acosK
  atanK = Hask atanK
  sinhK = Hask sinhK
  coshK = Hask coshK
  tanhK = Hask tanhK
  asinhK = Hask asinhK
  acoshK = Hask acoshK
  atanhK = Hask atanhK

instance SemigroupCat (->) m => SemigroupCat Hask m where
  appendK = Hask appendK

instance FixedCat Hask where
  fixK (Hask f) = Hask (fixK f)

instance (Real a, Fractional b) => RealToFracCat Hask a b where
  realToFracK = Hask realToFracK

instance FloatingPointConvertCat Hask where
  floatToDoubleK = Hask floatToDoubleK
  doubleToFloatK = Hask doubleToFloatK

instance FloatingPointClassifyCat Hask Double where
  isNegativeZeroK = Hask isNegativeZeroK
  isInfiniteK = Hask isInfiniteK
  isFiniteK = Hask isFiniteK
  isNaNK = Hask isNaNK
  isDenormalK = Hask isDenormalK

instance FloatingPointClassifyCat Hask Float where
  isNegativeZeroK = Hask isNegativeZeroK
  isInfiniteK = Hask isInfiniteK
  isFiniteK = Hask isFiniteK
  isNaNK = Hask isNaNK
  isDenormalK = Hask isDenormalK

-- TotOrd

instance Applicative f => LaxMonoidalFunctorCat TotOrd f where
  liftA2K (TotOrd fn) = TotOrd $ liftA2K fn

instance Monad f => MonadCat TotOrd f where
  joinK = TotOrd joinK
  mmapK (TotOrd fn) = TotOrd $ mmapK fn

instance (Floating a, Ord a) => FloatingCat' TotOrd a where
  powK = TotOrd powK

instance (Floating a, Ord a) => TranscendentalCat TotOrd a where
  tanK = TotOrd tanK
  asinK = TotOrd asinK
  acosK = TotOrd acosK
  atanK = TotOrd atanK
  sinhK = TotOrd sinhK
  coshK = TotOrd coshK
  tanhK = TotOrd tanhK
  asinhK = TotOrd asinhK
  acoshK = TotOrd acoshK
  atanhK = TotOrd atanhK

instance FloatingPointConvertCat TotOrd where
  floatToDoubleK = TotOrd $ ConCat.Constrained GHC.Float.float2Double
  doubleToFloatK = TotOrd $ ConCat.Constrained GHC.Float.double2Float

instance
  (Num a, FloatingPointClassifyCat (->) a) =>
  FloatingPointClassifyCat TotOrd a
  where
  isNegativeZeroK = TotOrd $ ConCat.Constrained isNegativeZeroK
  isInfiniteK = TotOrd $ ConCat.Constrained isInfiniteK
  isFiniteK = TotOrd $ ConCat.Constrained isFiniteK
  isNaNK = TotOrd $ ConCat.Constrained isNaNK
  isDenormalK = TotOrd $ ConCat.Constrained isDenormalK

instance (Real a, Fractional b) => RealToFracCat TotOrd a b where
  realToFracK = TotOrd $ ConCat.Constrained GHC.Real.realToFrac

instance Num a => NumCat' TotOrd a where
  absK = TotOrd $ ConCat.Constrained abs

  signumK = TotOrd $ ConCat.Constrained signum

instance Integral a => IntegralCat' TotOrd a where
  quotK = TotOrd quotK
  remK = TotOrd remK

instance Semigroup m => SemigroupCat TotOrd m where
  appendK = TotOrd . ConCat.Constrained $ uncurry (<>)

-- | This should live in "Categorifier.ConCatExtensions", but can't until
--   @`ConCat.TracedCat` `ConCat.Constrained`@ is moved upstream.
instance
  (FixedCat k, ConCat.OpSat (ConCat.Prod k) con) =>
  FixedCat (ConCat.Constrained con k)
  where
  fixK (ConCat.Constrained fn) = ConCat.Constrained $ fixK fn

instance FixedCat TotOrd where
  fixK (TotOrd fn) = TotOrd $ fixK fn
