{-# OPTIONS_GHC -Wno-orphans #-}

-- | This is intended to be used /instead/ of "ConCat.Syntactic" when using the "Categorifier"
--   plugin with `Categorifier.Hierarchy.ConCatExtensions.hierarchy`.
--
--   It re-exports the original module, adding instances for classes required by `Categorifier`.
module Categorifier.ConCatExtensions.Examples.Syntactic
  ( module Categorifier.ConCat.Examples.Syntactic,
  )
where

import Categorifier.ConCat.Examples.Syntactic
import qualified Categorifier.ConCatExtensions as Cat

instance Cat.OrdCat' Syn a where
  compareK = app0 "compare"

instance Cat.LaxMonoidalFunctorCat Syn f where
  liftA2K = app1 "liftA2"

instance Cat.ApplicativeCat Syn f where
  apK = app0 "<*>"

-- TODO: Remove `Functor` constraint from upstream class
instance (Functor h) => Cat.MonadCat Syn h where
  joinK = app0 "join"
  mmapK = app1 "=<<"

-- TODO: Remove `Functor` constraint from upstream class
instance (Functor h) => Cat.BindableCat Syn h where
  bindK = app0 ">>="

instance Cat.TraversableCat' Syn t f where
  traverseK = app1 "traverse"

instance Cat.NumCat' Syn a where
  absK = app0 "abs"
  signumK = app0 "signum"

instance Cat.IntegralCat' Syn a where
  evenK = app0 "even"
  oddK = app0 "odd"
  quotK = app0 "quot"
  remK = app0 "rem"

instance Cat.FloatingCat' Syn a where
  powK = app0 "**"

instance Cat.PowICat Syn a where
  powIK = app1 "powI" . app0 . show . toInteger

instance Cat.SemigroupCat Syn m where
  appendK = app0 "<>"

instance Cat.FixedCat Syn where
  fixK = app1 "fix"

instance Cat.RealToFracCat Syn a b where
  realToFracK = app0 "realToFrac"

instance Cat.FloatingPointConvertCat Syn where
  floatToDoubleK = app0 "float2Double"
  doubleToFloatK = app0 "double2Float"

instance Cat.FloatingPointClassifyCat Syn a where
  isNegativeZeroK = app0 "isNegativeZero"
  isInfiniteK = app0 "isInfinite"
  isFiniteK = app0 "isFinite"
  isNaNK = app0 "isNaN"
  isDenormalK = app0 "isDenormal"

instance Cat.TranscendentalCat Syn a where
  tanK = app0 "tan"
  asinK = app0 "asin"
  acosK = app0 "acos"
  atanK = app0 "atan"
  sinhK = app0 "sinh"
  coshK = app0 "cosh"
  tanhK = app0 "tanh"
  asinhK = app0 "asinh"
  acoshK = app0 "acosh"
  atanhK = app0 "atanh"

instance Cat.ArcTan2Cat Syn a where
  arctan2K = app0 "atan2"

instance Cat.FModCat Syn a where
  fmodK = app0 "fmod"
