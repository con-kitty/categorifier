{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

-- | Defines various mappings between categorical representations and the plugin, allowing us to
--   support transformations against different type class hierarchies.
module Kitty.Plugin.Hierarchy.Categories
  ( hierarchy,
  )
where

import qualified GhcPlugins as Plugins
import Kitty.Plugin.Core.Types (CategoryStack, Lookup)
import Kitty.Plugin.Hierarchy
  ( First (..),
    Hierarchy (..),
    baseHierarchy,
    identifier,
    mkMethodApps,
  )

-- | A hierarchy using the type classes available in the
--   [@categories@](https://hackage.haskell.org/package/categories) library. This includes
--  `baseHierarchy`, since this library also builds on that hierarchy.` However, it currently
--   results in mixing stuff from @categories@ and the `Control.Arrow.Arrow` hierarchy, which is
--   /not/ part of the expected @categories@ instances. It does illustrate that mixing hierarchies
--   works, though.
hierarchy :: Lookup (Hierarchy CategoryStack)
hierarchy =
  fmap getFirst $
    (<>) <$> fmap First hierarchy' <*> fmap First baseHierarchy

-- | `hierarchy` without the bits from @base@. This is just separated out for clarity. It shouldn't
--   be public.
hierarchy' :: Monad f => Lookup (Hierarchy f)
hierarchy' = do
  let tensor = Plugins.mkTyConTy (Plugins.tupleTyCon Plugins.Boxed 2)
  let absV = Nothing
  let abstCV = Nothing
  let acosV = Nothing
  let acoshV = Nothing
  let apV = Nothing
  let andV = Nothing
  let appendV = Nothing
  applyV <-
    pure <$> do
      op <- identifier "Control.Category.Cartesian.Closed" "apply"
      pure $ \onDict cat a b -> mkMethodApps onDict op [cat] [a, b] []
  let apply2V = Nothing
  let arctan2V = Nothing
  let asinV = Nothing
  let asinhV = Nothing
  let atanV = Nothing
  let atanhV = Nothing
  let bindV = Nothing
  let bottomV = Nothing
  let coerceV = Nothing
  let compareV = Nothing
  let composeV = Nothing
  let compose2V = Nothing
  let constV = Nothing
  let constraintV = Nothing
  let cosV = Nothing
  let coshV = Nothing
  curryV <-
    pure <$> do
      op <- identifier "Control.Category.Cartesian.Closed" "curry"
      pure $ \onDict cat a b c -> mkMethodApps onDict op [cat] [a, b, c] []
  distlV <-
    pure <$> do
      op <- identifier "Control.Category.Distributive" "distribute"
      pure $ \onDict cat a b c -> mkMethodApps onDict op [cat] [a, b, c] []
  let divV = Nothing
  let divideV = Nothing
  let doubleToFloatV = Nothing
  let equalV = Nothing
  exlV <-
    pure <$> do
      op <- identifier "Control.Category.Cartesian" "fst"
      pure $ \onDict cat a b -> mkMethodApps onDict op [cat] [a, b] []
  let expV = Nothing
  exrV <-
    pure <$> do
      op <- identifier "Control.Category.Cartesian" "snd"
      pure $ \onDict cat a b -> mkMethodApps onDict op [cat] [a, b] []
  let fixV = Nothing
  let floatToDoubleV = Nothing
  let fmodV = Nothing
  forkV <-
    pure <$> do
      op <- identifier "Control.Category.Cartesian" "&&&"
      pure (\onDict cat a b c -> mkMethodApps onDict op [cat] [a, b, c] [])
  let fpIsNegativeZeroV = Nothing
  let fpIsInfiniteV = Nothing
  let fpIsFiniteV = Nothing
  let fpIsNaNV = Nothing
  let fpIsDenormalV = Nothing
  let fromIntegerV = Nothing
  let fromIntegralV = Nothing
  let geV = Nothing
  let gtV = Nothing
  let idV = Nothing
  let ifV = Nothing
  inlV <-
    pure <$> do
      op <- identifier "Control.Category.Cartesian" "inl"
      pure $ \onDict cat a b -> mkMethodApps onDict op [cat] [a, b] []
  inrV <-
    pure <$> do
      op <- identifier "Control.Category.Cartesian" "inr"
      pure $ \onDict cat a b -> mkMethodApps onDict op [cat] [b, a] []
  joinV <-
    pure <$> do
      op <- identifier "Control.Category.Cartesian" "|||"
      pure (\onDict cat a b c -> mkMethodApps onDict op [cat] [a, b, c] [])
  lassocV <-
    pure <$> do
      op <- identifier "Control.Category.Associative" "disassociate"
      pure $ \onDict cat a b c -> mkMethodApps onDict op [cat, tensor] [a, b, c] []
  let leV = Nothing
  let liftA2V = Nothing
  let logV = Nothing
  let ltV = Nothing
  mapV <-
    pure <$> do
      op <- identifier "Control.Categorical.Functor" "fmap"
      pure (\onDict cat cat' f a b -> mkMethodApps onDict op [f, cat, cat'] [a, b] [])
  let maxV = Nothing
  let maximumV = Nothing
  let minV = Nothing
  let minimumV = Nothing
  let minusV = Nothing
  let modV = Nothing
  let nativeV = Nothing
  let negateV = Nothing
  let indexV = Nothing
  let tabulateV = Nothing
  let notV = Nothing
  let notEqualV = Nothing
  let orV = Nothing
  let plusV = Nothing
  let pointV = Nothing
  let powV = Nothing
  let powIV = Nothing
  let powIntV = Nothing
  let quotV = Nothing
  rassocV <-
    pure <$> do
      op <- identifier "Control.Category.Associative" "associate"
      pure $ \onDict cat a b c -> mkMethodApps onDict op [cat, tensor] [a, b, c] []
  let realToFracV = Nothing
  let recipV = Nothing
  let remV = Nothing
  let reprCV = Nothing
  let sequenceAV = Nothing
  let signumV = Nothing
  let sinV = Nothing
  let sinhV = Nothing
  let sqrtV = Nothing
  let strengthV = Nothing
  swapV <-
    pure <$> do
      op <- identifier "Control.Category.Braided" "braid"
      pure $ \onDict cat a b -> mkMethodApps onDict op [cat, tensor] [a, b] []
  let tanV = Nothing
  let tanhV = Nothing
  let timesV = Nothing
  let traverseV = Nothing
  uncurryV <-
    pure <$> do
      op <- identifier "Control.Category.Cartesian.Closed" "uncurry"
      pure $ \onDict cat a b c -> mkMethodApps onDict op [cat] [a, b, c] []
  pure Hierarchy {..}
