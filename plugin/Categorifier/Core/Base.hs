{-# LANGUAGE TupleSections #-}

-- | The `Categorifier.Core.Hierarchy.baseHierarchy` can support most of categorification (`arr` is a
--   very powerful operation), but it doesn't provide all of the appropriate functions and some of
--   them are too complicated to encode directly in GHC Core. This module bridges the gap to make
--   the Core definitions more tractable.
module Categorifier.Core.Base
  ( distlB,
    fixB,
    ifThenElseB,
    lassocB,
    rassocB,
    strengthB,
    uncurryB,
  )
where

import Control.Arrow (Arrow (arr, first), ArrowLoop (..))
import Control.Category (Category (..))
import Data.Bifunctor (Bifunctor (bimap))
import Prelude hiding ((.))

distlB :: (Bifunctor f) => (a, f b c) -> f (a, b) (a, c)
distlB (a, fbc) = bimap (a,) (a,) fbc

fixB :: (ArrowLoop k) => k (a, x) x -> k a x
fixB f = loop (arr (\x -> (x, x)) . f)

ifThenElseB :: (Bool, (a, a)) -> a
ifThenElseB (t, (c, a)) = if t then c else a

lassocB :: (a, (b, c)) -> ((a, b), c)
lassocB (a, (b, c)) = ((a, b), c)

rassocB :: ((a, b), c) -> (a, (b, c))
rassocB ((a, b), c) = (a, (b, c))

strengthB :: (Functor f) => (a, f b) -> f (a, b)
strengthB (a, fb) = (a,) <$> fb

uncurryB :: (Arrow k) => k a (b -> c) -> k (a, b) c
uncurryB f = arr (uncurry ($)) . first f
