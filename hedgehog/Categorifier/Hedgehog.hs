-- | Some useful functions for testing with "Hedgehog".
module Categorifier.Hedgehog
  ( floatingEq,
    genFloating,
    genInteger,
    genIntegralBounded,
    genNatural,
    genNaturalFrom,
  )
where

import qualified GHC.Float
import GHC.Stack (HasCallStack, withFrozenCallStack)
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Numeric.Natural (Natural)

-- | A variant on `Hedgehog.===` that identifies NaNs as equals. It still works for non-FP types.
floatingEq :: (Hedgehog.MonadTest m, Eq a, Show a, HasCallStack) => a -> a -> m ()
floatingEq x y = withFrozenCallStack $ Hedgehog.diff x eq y
  where
    eq x' y' = x' /= x' && y' /= y' || x' == y'

-- | Generate any 'GHC.Float.RealFloat' value.
--
-- This generator chooses a value from
--
--     * small ranges around starting points that often cause problems:
--
--         * 0
--         * 1
--         * @pi / 2@
--         * @pi@
--
--     * a general range spanning all numerical values
--
--     * special IEEE values like infinities and @NaN@.
genFloating :: forall a m. (Hedgehog.MonadGen m, GHC.Float.RealFloat a) => m a
genFloating =
  Gen.choice
    . fmap Gen.realFloat
    $
    -- Search around 0, and don't forget -0.0, as well as 1, pi/2 and
    -- pi, as these are often sensitive values for algorithms and
    -- functions.
    aroundPosNeg 0 1e-6
      <> aroundPosNeg 1 1e-6
      <> aroundPosNeg (pi / 2) 1e-6
      <> aroundPosNeg pi 1e-6
      <>
      -- This limit exceeds the max value of doubles, +-1e308
      [ Range.exponentialFloatFrom 0 (-1e322) 1e322,
        Range.singleton $ 1 / 0, -- Infinity
        Range.singleton $ (-1) / 0, -- -Infinity
        Range.singleton $ 0 / 0 -- NaN
      ]
  where
    aroundFloat :: a -> a -> Range.Range a
    aroundFloat float size = Range.exponentialFloatFrom float (float - size) (float + size)
    aroundPosNeg :: a -> a -> [Range.Range a]
    aroundPosNeg float size = [aroundFloat float size, aroundFloat (negate float) size]

-- | Generate an arbitrary, potentially quite large, integer.
genInteger :: (Hedgehog.MonadGen m) => m Integer
genInteger = Gen.integral $ Range.linearFrom 0 (-maxUnbounded) maxUnbounded

-- | Like `Gen.enumBounded`, but safe for integral types larger than `Int`
--   (which can vary based on the platform).
genIntegralBounded :: (Hedgehog.MonadGen m, Bounded a, Integral a) => m a
genIntegralBounded = Gen.integral Range.linearBounded

-- | Arbitrary large value for bounding unbounded integral types.
maxUnbounded :: (Integral a) => a
maxUnbounded = 10 ^ (100 :: Natural)

-- | Like `genNatural`, but takes a lower bound. This is useful for eliminating invalid cases for
--   things like subtraction.
genNaturalFrom :: (Hedgehog.MonadGen m) => Natural -> m Natural
genNaturalFrom lowerBound = Gen.integral $ Range.linear lowerBound maxUnbounded

-- | Generate an arbitrary, potentially quite large, non-negative number.
genNatural :: (Hedgehog.MonadGen m) => m Natural
genNatural = genNaturalFrom 0
