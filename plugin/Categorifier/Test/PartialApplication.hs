module Test.PartialApplication
  ( hprop_partialApplication,
    hprop_functionApplication,
    hprop_functionPartialApplication,
  )
where

import qualified Categorifier.Categorize as Categorize
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import P
import Test.Hask (Hask (..))

testArrow :: Hask (Char, (Char, Char)) Char -> Hedgehog.Property
testArrow arrow = Hedgehog.property $ do
  input <-
    Hedgehog.forAll
      ( (,)
          <$> Gen.enumBounded
          <*> ( (,)
                  <$> Gen.enumBounded
                  <*> Gen.enumBounded
              ) ::
          Hedgehog.Gen (Char, (Char, Char))
      )
  runHask arrow input Hedgehog.=== fst (snd input)

hprop_partialApplication :: Hedgehog.Property
hprop_partialApplication = testArrow (Categorize.expression $ fst . snd)

-- | This, unsurprisingly, has to be inlined for it to work, which perhaps makes it seem like not
--   a good test. But it should at least indicate to us whether inlining is being applied at the
--   correct time before our plugin is triggered.
preApply :: (a -> b) -> a `c` b
preApply = Categorize.expression
{-# INLINE preApply #-}

hprop_functionApplication :: Hedgehog.Property
hprop_functionApplication = testArrow (preApply (fst . snd))

hprop_functionPartialApplication :: Hedgehog.Property
hprop_functionPartialApplication = testArrow (preApply $ fst . snd)
