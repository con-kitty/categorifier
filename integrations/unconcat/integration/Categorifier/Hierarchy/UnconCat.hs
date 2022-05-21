{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Defines various mappings between categorical representations and the plugin, allowing us to
--   support transformations against different type class hierarchies.
module Categorifier.Hierarchy.UnconCat
  ( hierarchy,
  )
where

import Categorifier.Core.Types (CategoryStack, Lookup)
import Categorifier.Hierarchy
  ( Hierarchy (..),
    emptyHierarchy,
    identifier,
    mkFunctionApps,
    mkMethodApps,
  )
import qualified Categorifier.UnconCat

hierarchy :: Lookup (Hierarchy CategoryStack)
hierarchy = do
  kapplyV <-
    pure <$> do
      fn <- identifier 'Categorifier.UnconCat.apply
      pure (\onDict cat a b -> mkMethodApps onDict fn [cat] [a, b] [])
  kapply2V <-
    pure <$> do
      fn <- identifier 'Categorifier.UnconCat.apply2
      pure (\onDict cat x a b f g -> mkFunctionApps onDict fn [cat, x, a, b] [f, g])
  kcomposeV <-
    pure <$> do
      fn <- identifier '(Categorifier.UnconCat..)
      pure (\onDict cat a b c -> mkMethodApps onDict fn [cat] [b, c, a] [])
  kcompose2V <-
    pure <$> do
      fn <- identifier 'Categorifier.UnconCat.compose2
      pure (\onDict cat x b c a f g -> mkFunctionApps onDict fn [cat, x, b, c, a] [f, g])
  kcurryV <-
    pure <$> do
      fn <- identifier 'Categorifier.UnconCat.curry
      pure (\onDict cat a b c -> mkMethodApps onDict fn [cat] [a, b, c] [])
  kexlV <-
    pure <$> do
      fn <- identifier 'Categorifier.UnconCat.exl
      pure (\onDict cat a b -> mkMethodApps onDict fn [cat] [a, b] [])
  kexrV <-
    pure <$> do
      fn <- identifier 'Categorifier.UnconCat.exr
      pure (\onDict cat a b -> mkMethodApps onDict fn [cat] [a, b] [])
  kforkV <-
    pure <$> do
      fn <- identifier '(Categorifier.UnconCat.&&&)
      pure (\onDict cat a b c -> mkFunctionApps onDict fn [cat, a, b, c] [])
  kidV <-
    pure <$> do
      fn <- identifier 'Categorifier.UnconCat.id
      pure (\onDict cat a -> mkMethodApps onDict fn [cat] [a] [])
  kinlV <-
    pure <$> do
      fn <- identifier 'Categorifier.UnconCat.inl
      pure (\onDict cat a b -> mkMethodApps onDict fn [cat] [a, b] [])
  kinrV <-
    pure <$> do
      fn <- identifier 'Categorifier.UnconCat.inr
      pure (\onDict cat a b -> mkMethodApps onDict fn [cat] [a, b] [])
  klassocV <-
    pure <$> do
      fn <- identifier 'Categorifier.UnconCat.lassocP
      pure (\onDict cat a b c -> mkMethodApps onDict fn [cat] [a, b, c] [])
  krassocV <-
    pure <$> do
      fn <- identifier 'Categorifier.UnconCat.rassocP
      pure (\onDict cat a b c -> mkMethodApps onDict fn [cat] [a, b, c] [])
  kuncurryV <-
    pure <$> do
      fn <- identifier 'Categorifier.UnconCat.uncurry
      pure (\onDict cat a b c -> mkMethodApps onDict fn [cat] [a, b, c] [])
  pure
    emptyHierarchy
      { applyV = kapplyV,
        apply2V = kapply2V,
        composeV = kcomposeV,
        compose2V = kcompose2V,
        curryV = kcurryV,
        exlV = kexlV,
        exrV = kexrV,
        forkV = kforkV,
        idV = kidV,
        inlV = kinlV,
        inrV = kinrV,
        lassocV = klassocV,
        rassocV = krassocV,
        uncurryV = kuncurryV
      }
