{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Defines various mappings between categorical representations and the plugin, allowing us to
--   support transformations against different type class hierarchies.
module Categorifier.Hierarchy.ConCatExtensions
  ( hierarchy,
  )
where

import qualified Categorifier.Category
import qualified Categorifier.ConCatExtensions
import Categorifier.Core.Types (CategoryStack, Lookup)
import qualified Categorifier.GHC.Builtin as Plugins
import qualified Categorifier.GHC.Core as Plugins
import Categorifier.Hierarchy
  ( Hierarchy (..),
    emptyHierarchy,
    identifier,
    mkMethodApps,
    mkMethodApps',
  )

-- | Some locally-defined classes, mostly to add missing pieces to ConCat.
hierarchy :: Lookup (Hierarchy CategoryStack)
hierarchy = do
  kabsV <-
    pure <$> do
      fn <- identifier 'Categorifier.ConCatExtensions.absK
      pure (\onDict cat a -> mkMethodApps onDict fn [cat, a] [] [])
  kacosV <-
    pure <$> do
      fn <- identifier 'Categorifier.ConCatExtensions.acosK
      pure (\onDict cat a -> mkMethodApps onDict fn [cat, a] [] [])
  kacoshV <-
    pure <$> do
      fn <- identifier 'Categorifier.ConCatExtensions.acoshK
      pure (\onDict cat a -> mkMethodApps onDict fn [cat, a] [] [])
  kapV <-
    pure <$> do
      fn <- identifier 'Categorifier.ConCatExtensions.apK
      pure (\onDict cat f a b -> mkMethodApps onDict fn [cat, f] [a, b] [])
  kappendV <-
    pure <$> do
      fn <- identifier 'Categorifier.ConCatExtensions.appendK
      pure (\onDict cat a -> mkMethodApps onDict fn [cat, a] [] [])
  karctan2V <-
    pure <$> do
      op <- identifier 'Categorifier.ConCatExtensions.arctan2K
      pure (\onDict cat a -> mkMethodApps onDict op [cat, a] [] [])
  kasinV <-
    pure <$> do
      fn <- identifier 'Categorifier.ConCatExtensions.asinK
      pure (\onDict cat a -> mkMethodApps onDict fn [cat, a] [] [])
  kasinhV <-
    pure <$> do
      fn <- identifier 'Categorifier.ConCatExtensions.asinhK
      pure (\onDict cat a -> mkMethodApps onDict fn [cat, a] [] [])
  katanV <-
    pure <$> do
      fn <- identifier 'Categorifier.ConCatExtensions.atanK
      pure (\onDict cat a -> mkMethodApps onDict fn [cat, a] [] [])
  katanhV <-
    pure <$> do
      fn <- identifier 'Categorifier.ConCatExtensions.atanhK
      pure (\onDict cat a -> mkMethodApps onDict fn [cat, a] [] [])
  kbindV <-
    pure <$> do
      fn <- identifier 'Categorifier.ConCatExtensions.bindK
      pure (\onDict cat m a b -> mkMethodApps onDict fn [cat, m] [a, b] [])
  kcompareV <-
    pure <$> do
      fn <- identifier 'Categorifier.ConCatExtensions.compareK
      pure (\onDict cat a -> mkMethodApps onDict fn [cat, a] [] [])
  kconstraintV <-
    pure <$> do
      fn <- identifier 'Categorifier.ConCatExtensions.constraintK
      pure $ \onDict cat a b ->
        -- The original kind of `cat` is @* -> * -> *@. We need to change it to
        -- @Plugins.typeKind a -> Constraint -> *@, in order for `buildDictionary` to
        -- find the `ConstraintCat cat` instance.
        --
        -- Currently, in all other `FooCat k` classes, k's kind is always
        -- @* -> * -> *@, so this trick only needs to be applied in this one place.
        let cat'
              | Just (catTyCon, [_, _]) <- Plugins.splitTyConApp_maybe cat =
                  Plugins.mkTyConApp catTyCon [Plugins.typeKind a, Plugins.constraintKind]
              | otherwise = cat
         in -- Here we must avoid building dictionaries on the `c` constraint in
            -- @constraintK :: forall c a. c => a `k` c@ (hence the `pure`), because we are
            -- going to pass `c`'s dictionary as a term argument.
            mkMethodApps' onDict pure fn [Plugins.typeKind a, cat', b] [a] []
  kcoshV <-
    pure <$> do
      fn <- identifier 'Categorifier.ConCatExtensions.coshK
      pure (\onDict cat a -> mkMethodApps onDict fn [cat, a] [] [])
  kevenV <-
    pure <$> do
      fn <- identifier 'Categorifier.ConCatExtensions.evenK
      pure (\onDict cat a -> mkMethodApps onDict fn [cat, a] [] [])
  kfixV <-
    pure <$> do
      op <- identifier 'Categorifier.ConCatExtensions.fixK
      pure (\onDict cat a b -> mkMethodApps onDict op [cat] [a, b] [])
  kfmodV <-
    pure <$> do
      op <- identifier 'Categorifier.ConCatExtensions.fmodK
      pure (\onDict cat a -> mkMethodApps onDict op [cat, a] [] [])
  kliftA2V <-
    pure <$> do
      fn <- identifier 'Categorifier.ConCatExtensions.liftA2K
      pure (\onDict cat f a b c -> mkMethodApps onDict fn [cat, f] [a, b, c] [])
  knativeV <-
    pure <$> do
      fn <- identifier 'Categorifier.Category.nativeK
      pure $ \onDict cat tag a b ->
        mkMethodApps onDict fn [Plugins.typeKind a, Plugins.typeKind b, cat, tag, a, b] [] []
  koddV <-
    pure <$> do
      fn <- identifier 'Categorifier.ConCatExtensions.oddK
      pure (\onDict cat a -> mkMethodApps onDict fn [cat, a] [] [])
  kpowV <-
    pure <$> do
      fn <- identifier 'Categorifier.ConCatExtensions.powK
      pure (\onDict cat a -> mkMethodApps onDict fn [cat, a] [] [])
  kpowIV <-
    pure <$> do
      fn <- identifier 'Categorifier.ConCatExtensions.powIK
      pure (\onDict cat a i iVal -> mkMethodApps onDict fn [Plugins.typeKind a, cat, a] [i] [iVal])
  kquotV <-
    pure <$> do
      fn <- identifier 'Categorifier.ConCatExtensions.quotK
      pure (\onDict cat a -> mkMethodApps onDict fn [cat, a] [] [])
  kremV <-
    pure <$> do
      fn <- identifier 'Categorifier.ConCatExtensions.remK
      pure (\onDict cat a -> mkMethodApps onDict fn [cat, a] [] [])
  ksignumV <-
    pure <$> do
      fn <- identifier 'Categorifier.ConCatExtensions.signumK
      pure (\onDict cat a -> mkMethodApps onDict fn [cat, a] [] [])
  ksinhV <-
    pure <$> do
      fn <- identifier 'Categorifier.ConCatExtensions.sinhK
      pure (\onDict cat a -> mkMethodApps onDict fn [cat, a] [] [])
  ktanV <-
    pure <$> do
      fn <- identifier 'Categorifier.ConCatExtensions.tanK
      pure (\onDict cat a -> mkMethodApps onDict fn [cat, a] [] [])
  ktanhV <-
    pure <$> do
      fn <- identifier 'Categorifier.ConCatExtensions.tanhK
      pure (\onDict cat a -> mkMethodApps onDict fn [cat, a] [] [])
  ktraverseV <-
    pure <$> do
      fn <- identifier 'Categorifier.ConCatExtensions.traverseK
      pure (\onDict cat t f a b -> mkMethodApps onDict fn [cat, t, f] [a, b] [])
  kdoubleToFloatV <-
    pure <$> do
      fn <- identifier 'Categorifier.ConCatExtensions.doubleToFloatK
      pure (\onDict cat -> mkMethodApps onDict fn [cat] [] [])
  kfloatToDoubleV <-
    pure <$> do
      fn <- identifier 'Categorifier.ConCatExtensions.floatToDoubleK
      pure (\onDict cat -> mkMethodApps onDict fn [cat] [] [])
  kfpIsNegativeZeroV <-
    pure <$> do
      fn <- identifier 'Categorifier.ConCatExtensions.isNegativeZeroK
      pure (\onDict cat a -> mkMethodApps onDict fn [cat, a] [] [])
  kfpIsInfiniteV <-
    pure <$> do
      fn <- identifier 'Categorifier.ConCatExtensions.isInfiniteK
      pure (\onDict cat a -> mkMethodApps onDict fn [cat, a] [] [])
  kfpIsFiniteV <-
    pure <$> do
      fn <- identifier 'Categorifier.ConCatExtensions.isFiniteK
      pure (\onDict cat a -> mkMethodApps onDict fn [cat, a] [] [])
  kfpIsNaNV <-
    pure <$> do
      fn <- identifier 'Categorifier.ConCatExtensions.isNaNK
      pure (\onDict cat a -> mkMethodApps onDict fn [cat, a] [] [])
  kfpIsDenormalV <-
    pure <$> do
      fn <- identifier 'Categorifier.ConCatExtensions.isDenormalK
      pure (\onDict cat a -> mkMethodApps onDict fn [cat, a] [] [])
  krealToFracV <-
    pure <$> do
      fn <- identifier 'Categorifier.ConCatExtensions.realToFracK
      pure $ \onDict cat a b -> mkMethodApps onDict fn [cat, a, b] [] []
  pure
    emptyHierarchy
      { absV = kabsV,
        acosV = kacosV,
        acoshV = kacoshV,
        apV = kapV,
        appendV = kappendV,
        arctan2V = karctan2V,
        asinV = kasinV,
        asinhV = kasinhV,
        atanV = katanV,
        atanhV = katanhV,
        bindV = kbindV,
        compareV = kcompareV,
        constraintV = kconstraintV,
        coshV = kcoshV,
        doubleToFloatV = kdoubleToFloatV,
        evenV = kevenV,
        fixV = kfixV,
        floatToDoubleV = kfloatToDoubleV,
        fmodV = kfmodV,
        fpIsNegativeZeroV = kfpIsNegativeZeroV,
        fpIsInfiniteV = kfpIsInfiniteV,
        fpIsFiniteV = kfpIsFiniteV,
        fpIsNaNV = kfpIsNaNV,
        fpIsDenormalV = kfpIsDenormalV,
        liftA2V = kliftA2V,
        nativeV = knativeV,
        oddV = koddV,
        powV = kpowV,
        powIV = kpowIV,
        quotV = kquotV,
        realToFracV = krealToFracV,
        remV = kremV,
        signumV = ksignumV,
        sinhV = ksinhV,
        tanV = ktanV,
        tanhV = ktanhV,
        traverseV = ktraverseV
      }
