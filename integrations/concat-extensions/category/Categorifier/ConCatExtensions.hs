{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | Extensions to the type class hierarchy from concat-classes supporting additional things we use
--   in the controller (although still very general things).
--
--  __TODO__: Submit these upstream.
module Categorifier.ConCatExtensions
  ( IntegralCat' (..),
    OrdCat' (..),
    LaxMonoidalFunctorCat (..),
    ApplicativeCat (..),
    MonadCat (..),
    BindableCat (..),
    TraversableCat' (..),
    NumCat' (..),
    FloatingCat' (..),
    PowICat (..),
    SemigroupCat (..),
    FixedCat (..),
    RealToFracCat (..),
    FloatingPointConvertCat (..),
    FloatingPointClassifyCat (..),
    TranscendentalCat (..),
    ArcTan2Cat (..),
    FModCat (..),
    ConstraintCat (..),
    defaultTrace,
  )
where

import qualified ConCat.Category as ConCat
import Control.Applicative (liftA2)
import qualified Control.Arrow as Arrow
import Control.Monad (join)
import Data.Kind (Constraint, Type)
import qualified GHC.Float as GHC
import qualified GHC.Real

{- We need UndecidableSuperClasses because:
Potential superclass cycle for 'FModCat'
  one of whose superclasses is 'ConCat.FloatingCat'
  one of whose superclass constraints is headed by a type family:
    'ConCat.Ok k a'
-}
{-# ANN module "HLint: ignore Avoid restricted extensions" #-}

-- TODO: Making it (and other classes in this module) poly-kinded, i.e.,
-- `k :: ok -> ok -> Type`. This requires adding `Plugins.typeKind a`
-- to a number of places in Hierarchy.hs.
class OrdCat' (k :: Type -> Type -> Type) a where
  compareK :: ConCat.Prod k a a `k` Ordering

instance (Ord a) => OrdCat' (->) a where
  compareK = uncurry compare

class LaxMonoidalFunctorCat (k :: Type -> Type -> Type) f where
  liftA2K :: forall a b c. ConCat.Prod k a b `k` c -> ConCat.Prod k (f a) (f b) `k` f c

instance (Applicative f) => LaxMonoidalFunctorCat (->) f where
  liftA2K f = uncurry . liftA2 $ curry f

instance (LaxMonoidalFunctorCat k f) => LaxMonoidalFunctorCat (ConCat.Constrained con k) f where
  liftA2K (ConCat.Constrained fn) = ConCat.Constrained $ liftA2K fn

class
  (ConCat.ClosedCat k, LaxMonoidalFunctorCat k f) =>
  ApplicativeCat (k :: Type -> Type -> Type) f
  where
  apK ::
    forall a b. (ConCat.Ok k a, ConCat.Ok k b) => ConCat.Prod k (f (ConCat.Exp k a b)) (f a) `k` f b
  apK = liftA2K (ConCat.uncurry ConCat.id) ConCat.<+ ConCat.okExp @k @a @b

instance (Applicative f) => ApplicativeCat (->) f where
  apK = uncurry (<*>)

class (ConCat.Category k, ConCat.FunctorCat k h) => MonadCat (k :: Type -> Type -> Type) h where
  {-# MINIMAL joinK | mmapK #-}
  joinK :: forall a. (ConCat.Ok k a) => h (h a) `k` h a
  joinK = mmapK ConCat.id ConCat.<+ ConCat.okFunctor @k @h @a

  -- | This is `=<<`.
  mmapK :: forall a b. (ConCat.Ok2 k a b) => a `k` h b -> h a `k` h b
  mmapK fn =
    joinK
      ConCat.. ConCat.fmapC fn
      ConCat.<+ ConCat.okFunctor @k @h @a
      ConCat.<+ ConCat.okFunctor @k @h @(h b)
      ConCat.<+ ConCat.okFunctor @k @h @b

instance (Monad h) => MonadCat (->) h where
  joinK = join
  mmapK = (=<<)

instance (MonadCat k f) => MonadCat (ConCat.Constrained con k) f where
  joinK = ConCat.Constrained joinK
  mmapK (ConCat.Constrained fn) = ConCat.Constrained $ mmapK fn

class (MonadCat k h) => BindableCat (k :: Type -> Type -> Type) h where
  -- | It seems like it /should/ be possible to define a default implementation for this in terms of
  --  `MonadCat` and `ConCat.CCC`, but I think some limitations in how closed categories are modeled
  --  (i.e., they're never really self-enriched, everything is enriched in `(->)`) prevent the
  --   simple definition.
  bindK ::
    forall a b.
    (ConCat.Ok k a, ConCat.Ok k b) =>
    ConCat.Prod k (h a) (ConCat.Exp k a (h b)) `k` h b

instance (Monad h) => BindableCat (->) h where
  bindK = uncurry (>>=)

class (ConCat.TraversableCat k t f) => TraversableCat' k t f where
  traverseK :: forall a b. a `k` f b -> t a `k` f (t b)

instance (Traversable t, Applicative f) => TraversableCat' (->) t f where
  traverseK = traverse

-- | Extends `ConCat.Category.NumCat` with additional operations from `Num`.
class NumCat' (k :: Type -> Type -> Type) a where
  absK :: a `k` a

  signumK :: a `k` a

instance (Num a) => NumCat' (->) a where
  absK = abs

  signumK = signum

instance (NumCat' k a, con a) => NumCat' (ConCat.Constrained con k) a where
  absK = ConCat.Constrained absK

  signumK = ConCat.Constrained signumK

-- | Extension of `ConCat.Category.IntegralCat`.
class IntegralCat' (k :: Type -> Type -> Type) a where
  evenK :: a `k` Bool
  oddK :: a `k` Bool
  quotK :: ConCat.Prod k a a `k` a
  remK :: ConCat.Prod k a a `k` a

instance (Integral a) => IntegralCat' (->) a where
  evenK = even
  oddK = odd
  quotK = uncurry quot
  remK = uncurry rem

instance (IntegralCat' k a, con a) => IntegralCat' (ConCat.Constrained con k) a where
  evenK = ConCat.Constrained evenK
  oddK = ConCat.Constrained oddK
  quotK = ConCat.Constrained quotK
  remK = ConCat.Constrained remK

class FloatingCat' (k :: Type -> Type -> Type) a where
  powK :: ConCat.Prod k a a `k` a

instance (Floating a) => FloatingCat' (->) a where
  powK = uncurry (**)

instance (FloatingCat' k a, con a) => FloatingCat' (ConCat.Constrained con k) a where
  powK = ConCat.Constrained powK

class PowICat k a where
  -- This is defined this way rather than @(a, i) `k` a@, because `^` should be evaluated
  -- Haskell, and the latter requires that `TargetOb i ~ i`, which is only true for non-primitive
  -- types like `Int` and `Integer`. Putting `i` to the left of `->` makes it work for
  -- all `Integral`s.
  powIK :: forall i. (Integral i) => i -> a `k` a

instance (Num a) => PowICat (->) a where
  powIK = flip (^)

class SemigroupCat (k :: Type -> Type -> Type) m where
  appendK :: ConCat.Prod k m m `k` m

instance (Semigroup m) => SemigroupCat (->) m where
  appendK = uncurry (<>)

class (ConCat.MonoidalPCat k, ConCat.TracedCat k) => FixedCat (k :: Type -> Type -> Type) where
  fixK :: forall a x. (ConCat.Ok2 k a x) => ConCat.Prod k a x `k` x -> a `k` x
  fixK f = ConCat.trace (f ConCat.&&& f) ConCat.<+ ConCat.okProd @k @a @x

-- | If `FixedCat` moves up stream, this should be the actual default method for `ConCat.trace`.
defaultTrace ::
  forall k a b x.
  (FixedCat k, ConCat.Ok3 k a b x) =>
  ConCat.Prod k a x `k` ConCat.Prod k b x ->
  a `k` b
defaultTrace f =
  ConCat.exl
    ConCat.. f
    ConCat.. (ConCat.id ConCat.&&& fixK (ConCat.exr ConCat.. f))
    ConCat.<+ ConCat.okProd @k @a @x
    ConCat.<+ ConCat.okProd @k @b @x

instance FixedCat (->) where
  fixK f = f . (ConCat.id Arrow.&&& fixK f)

class RealToFracCat (k :: Type -> Type -> Type) a b where
  realToFracK :: a `k` b

instance (Real a, Fractional b) => RealToFracCat (->) a b where
  realToFracK = GHC.Real.realToFrac

class
  (NumCat' k Double, NumCat' k Float) =>
  FloatingPointConvertCat (k :: Type -> Type -> Type)
  where
  floatToDoubleK :: Float `k` Double
  doubleToFloatK :: Double `k` Float

instance FloatingPointConvertCat (->) where
  floatToDoubleK = GHC.float2Double
  doubleToFloatK = GHC.double2Float

class (NumCat' k a) => FloatingPointClassifyCat (k :: Type -> Type -> Type) a where
  isNegativeZeroK :: a `k` Bool
  isInfiniteK :: a `k` Bool
  isFiniteK :: a `k` Bool
  isNaNK :: a `k` Bool
  isDenormalK :: a `k` Bool

instance FloatingPointClassifyCat (->) Double where
  isNegativeZeroK = (/= 0) . GHC.isDoubleNegativeZero
  isInfiniteK = (/= 0) . GHC.isDoubleInfinite
  isFiniteK = (/= 0) . GHC.isDoubleFinite
  isNaNK = (/= 0) . GHC.isDoubleNaN
  isDenormalK = (/= 0) . GHC.isDoubleDenormalized

instance FloatingPointClassifyCat (->) Float where
  isNegativeZeroK = (/= 0) . GHC.isFloatNegativeZero
  isInfiniteK = (/= 0) . GHC.isFloatInfinite
  isFiniteK = (/= 0) . GHC.isFloatFinite
  isNaNK = (/= 0) . GHC.isFloatNaN
  isDenormalK = (/= 0) . GHC.isFloatDenormalized

instance
  (FloatingPointClassifyCat k Double, con Double) =>
  FloatingPointClassifyCat (ConCat.Constrained con k) Double
  where
  isNegativeZeroK = ConCat.Constrained isNegativeZeroK
  isInfiniteK = ConCat.Constrained isInfiniteK
  isFiniteK = ConCat.Constrained isFiniteK
  isNaNK = ConCat.Constrained isNaNK
  isDenormalK = ConCat.Constrained isDenormalK

-- | This class provides all the usual transcendental functions.  `ConCat.FloatingCat` provides the
-- first four functions, but in order to both define a self-consistent class and not modify
-- `ConCat.FloatingCat`, we provide the whole set here.
class (ConCat.FloatingCat k a) => TranscendentalCat (k :: Type -> Type -> Type) a where
  expK :: a `k` a
  expK = ConCat.expC
  logK :: a `k` a
  logK = ConCat.logC
  sinK :: a `k` a
  sinK = ConCat.sinC
  cosK :: a `k` a
  cosK = ConCat.cosC
  tanK :: a `k` a
  asinK :: a `k` a
  acosK :: a `k` a
  atanK :: a `k` a
  sinhK :: a `k` a
  coshK :: a `k` a
  tanhK :: a `k` a
  asinhK :: a `k` a
  acoshK :: a `k` a
  atanhK :: a `k` a

instance (Floating a) => TranscendentalCat (->) a where
  tanK = tan
  asinK = asin
  acosK = acos
  atanK = atan
  sinhK = sinh
  coshK = cosh
  tanhK = tanh
  asinhK = asinh
  acoshK = acosh
  atanhK = atanh

instance (TranscendentalCat k a, con a) => TranscendentalCat (ConCat.Constrained con k) a where
  tanK = ConCat.Constrained tanK
  asinK = ConCat.Constrained asinK
  acosK = ConCat.Constrained acosK
  atanK = ConCat.Constrained atanK
  sinhK = ConCat.Constrained sinhK
  coshK = ConCat.Constrained coshK
  tanhK = ConCat.Constrained tanhK
  asinhK = ConCat.Constrained asinhK
  acoshK = ConCat.Constrained acoshK
  atanhK = ConCat.Constrained atanhK

class (TranscendentalCat k a) => ArcTan2Cat (k :: Type -> Type -> Type) a where
  arctan2K :: ConCat.Prod k a a `k` a

instance (RealFloat a) => ArcTan2Cat (->) a where
  arctan2K = uncurry atan2

instance (ArcTan2Cat k a, con a) => ArcTan2Cat (ConCat.Constrained con k) a where
  arctan2K = ConCat.Constrained arctan2K

class (ConCat.FloatingCat k a) => FModCat (k :: Type -> Type -> Type) a where
  fmodK :: ConCat.Prod k a a `k` a

instance (FModCat k a, con a) => FModCat (ConCat.Constrained con k) a where
  fmodK = ConCat.Constrained fmodK

-- | Similar to 'ConCat.ConstCat', but for constraints.
class ConstraintCat (k :: kind -> Constraint -> Type) (c :: Constraint) where
  constraintK :: forall a. (c) => a `k` c
