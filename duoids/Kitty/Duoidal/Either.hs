-- | Operations specific to failure duoids.
module Kitty.Duoidal.Either
  ( noteAccum,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Kitty.Duoidal (Parallel (..))

-- | Converts a maybe-returning function into a failed-input-accumulating one.
--
--   This is particularly useful for traversals, where you want to track _which_ elements of the
--   traversal failed. E.g.,
--
-- > traverse safeHead :: t [a] -> Maybe (t a)
--
--   becomes
--
-- > traverse (noteAccum safeHead) :: t [a] -> Parallel (Either (NonEmpty a)) (t a)
noteAccum :: (a -> Maybe b) -> a -> Parallel (Either (NonEmpty a)) b
noteAccum f a = Parallel . maybe (Left $ pure a) pure $ f a
