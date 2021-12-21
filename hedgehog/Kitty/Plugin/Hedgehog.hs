-- | Some useful functions for testing with "Hedgehog".
module Kitty.Plugin.Hedgehog
  ( floatingEq,
    genFloating,
  )
where

import qualified GHC.Float
import GHC.Stack (HasCallStack, withFrozenCallStack)
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

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
