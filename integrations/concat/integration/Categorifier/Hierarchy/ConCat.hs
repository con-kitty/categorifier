{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

-- | Defines various mappings between categorical representations and the plugin, allowing us to
--   support transformations against different type class hierarchies.
module Categorifier.Hierarchy.ConCat
  ( classHierarchy,
    functionHierarchy,
  )
where

import Categorifier.Core.Types (CategoryStack, Lookup)
import qualified Categorifier.GHC.Core as Plugins
import Categorifier.Hierarchy
  ( Hierarchy (..),
    findTyCon,
    identifier,
    mkFunctionApps,
    mkMethodApps,
  )

-- | ConCat effectively provides us with two hierarchies. One is standard type classes, the other
--   lowers all of the methods from those classes to functions. These should behave the same for our
--   purposes, except perhaps for optimization. This gives us a common base for most of the
--   definitions.
--
--  __NB__: This uses `mkMethodApps` even though for `functionHierarchy` they're not methods. But,
--          this approach degrades fine -- it just means there will be no dictionaries resolved in
--          the first attempt. If this causes performance problems, we can tighten it up later.
hierarchy' :: Monad f => String -> Lookup (Hierarchy f)
hierarchy' moduleName = do
  let absV = Nothing
  abstCV <- pure <$> repOp "abstC"
  let acosV = Nothing
  let acoshV = Nothing
  andV <-
    pure <$> do
      fn <- identifier' "andC"
      pure (\onDict cat -> mkMethodApps onDict fn [cat] [] [])
  let apV = Nothing
  let appendV = Nothing
  applyV <-
    pure <$> do
      fn <- identifier' "apply"
      pure (\onDict cat a b -> mkMethodApps onDict fn [cat] [a, b] [])
  let apply2V = Nothing
  let arctan2V = Nothing
  let asinV = Nothing
  let asinhV = Nothing
  let atanV = Nothing
  let atanhV = Nothing
  let bindV = Nothing
  bottomV <-
    pure <$> do
      fn <- identifier' "bottomC"
      pure (\onDict cat a b -> mkMethodApps onDict fn [cat, a, b] [] [])
  coerceV <-
    pure <$> do
      -- __NB__: This uses `Categorifier.Category.unsafeCoerceK` instead of
      --        `ConCat.Category.coerceC` because the `Coercible` constraint on the latter requires
      --         imports for an unbounded number of @newtype@ constructors. See
      --         https://github.com/conal/concat/issues/34 for some further discussion.
      fn <- identifier "Categorifier.Category" "unsafeCoerceK"
      pure $ \onDict cat from to ->
        mkMethodApps onDict fn [Plugins.typeKind from, Plugins.typeKind to, cat, from, to] [] []
  let compareV = Nothing
  composeV <-
    pure <$> do
      fn <- identifier' "."
      pure (\onDict cat a b c -> mkMethodApps onDict fn [cat] [b, c, a] [])
  let compose2V = Nothing
  constV <-
    pure <$> do
      fn <- identifier' "const"
      pure (\onDict cat a b -> mkMethodApps onDict fn [cat, b] [a] [])
  let constraintV = Nothing
  cosV <-
    pure <$> do
      fn <- identifier' "cosC"
      pure (\onDict cat a -> mkMethodApps onDict fn [cat, a] [] [])
  let coshV = Nothing
  curryV <-
    pure <$> do
      fn <- identifier' "curry"
      pure (\onDict cat a b c -> mkMethodApps onDict fn [cat] [a, b, c] [])
  distlV <-
    pure <$> do
      fn <- identifier' "distl"
      pure (\onDict cat a b c -> mkMethodApps onDict fn [cat] [a, b, c] [])
  divV <-
    pure <$> do
      fn <- identifier' "divC"
      pure (\onDict cat a -> mkMethodApps onDict fn [cat, a] [] [])
  divideV <-
    pure <$> do
      fn <- identifier' "divideC"
      pure (\onDict cat a -> mkMethodApps onDict fn [cat, a] [] [])
  let doubleToFloatV = Nothing
  equalV <-
    pure <$> do
      fn <- identifier' "equal"
      pure (\onDict cat a -> mkMethodApps onDict fn [cat, a] [] [])
  let evenV = Nothing
  exlV <-
    pure <$> do
      fn <- identifier' "exl"
      pure (\onDict cat a b -> mkMethodApps onDict fn [cat] [a, b] [])
  expV <-
    pure <$> do
      fn <- identifier' "expC"
      pure (\onDict cat a -> mkMethodApps onDict fn [cat, a] [] [])
  exrV <-
    pure <$> do
      fn <- identifier' "exr"
      pure (\onDict cat a b -> mkMethodApps onDict fn [cat] [a, b] [])
  let fixV = Nothing
  let floatToDoubleV = Nothing
  let fmodV = Nothing
  forkV <-
    pure <$> do
      fn <- identifier' "&&&"
      pure (\onDict cat a b c -> mkFunctionApps onDict fn [cat, a, b, c] [])
  let fpIsNegativeZeroV = Nothing
  let fpIsInfiniteV = Nothing
  let fpIsFiniteV = Nothing
  let fpIsNaNV = Nothing
  let fpIsDenormalV = Nothing
  let fromIntegerV = Nothing
  let fromIntegralV = Nothing
  geV <-
    pure <$> do
      fn <- identifier' "greaterThanOrEqual"
      pure (\onDict cat a -> mkMethodApps onDict fn [cat, a] [] [])
  gtV <-
    pure <$> do
      fn <- identifier' "greaterThan"
      pure (\onDict cat a -> mkMethodApps onDict fn [cat, a] [] [])
  idV <-
    pure <$> do
      fn <- identifier' "id"
      pure (\onDict cat a -> mkMethodApps onDict fn [cat] [a] [])
  ifV <-
    pure <$> do
      op <- identifier' "ifC"
      pure (\onDict cat a -> mkMethodApps onDict op [cat, a] [] [])
  indexV <-
    pure <$> do
      fn <- identifier' "indexC"
      pure (\onDict cat f a -> mkMethodApps onDict fn [cat, f] [a] [])
  inlV <-
    pure <$> do
      fn <- identifier' "inl"
      pure (\onDict cat a b -> mkMethodApps onDict fn [cat] [a, b] [])
  inrV <-
    pure <$> do
      fn <- identifier' "inr"
      pure (\onDict cat a b -> mkMethodApps onDict fn [cat] [a, b] [])
  joinV <-
    pure <$> do
      fn <- identifier' "|||"
      pure (\onDict cat a b c -> mkFunctionApps onDict fn [cat, c, a, b] [])
  lassocV <-
    pure <$> do
      fn <- identifier' "lassocP"
      pure (\onDict cat a b c -> mkMethodApps onDict fn [cat] [a, b, c] [])
  leV <-
    pure <$> do
      fn <- identifier' "lessThanOrEqual"
      pure (\onDict cat a -> mkMethodApps onDict fn [cat, a] [] [])
  let liftA2V = Nothing
  logV <-
    pure <$> do
      fn <- identifier' "logC"
      pure (\onDict cat a -> mkMethodApps onDict fn [cat, a] [] [])
  ltV <-
    pure <$> do
      fn <- identifier' "lessThan"
      pure (\onDict cat a -> mkMethodApps onDict fn [cat, a] [] [])
  mapV <-
    pure <$> do
      op <- identifier' "fmapC"
      -- __FIXME__: We currently ignore @cat'@ here and hope @cat' `==` cat@.
      pure (\onDict cat _cat' f a b -> mkMethodApps onDict op [cat, f] [a, b] [])
  maxV <-
    pure <$> do
      fn <- identifier' "maxC"
      pure (\onDict cat a -> mkMethodApps onDict fn [cat, a] [] [])
  maximumV <-
    pure <$> do
      fn <- identifier' "maximumC"
      pure (\onDict cat f a -> mkMethodApps onDict fn [cat, f, a] [] [])
  minV <-
    pure <$> do
      fn <- identifier' "minC"
      pure (\onDict cat a -> mkMethodApps onDict fn [cat, a] [] [])
  minimumV <-
    pure <$> do
      fn <- identifier' "minimumC"
      pure (\onDict cat f a -> mkMethodApps onDict fn [cat, f, a] [] [])
  minusV <-
    pure <$> do
      fn <- identifier' "subC"
      pure (\onDict cat a -> mkMethodApps onDict fn [cat, a] [] [])
  modV <-
    pure <$> do
      fn <- identifier' "modC"
      pure (\onDict cat a -> mkMethodApps onDict fn [cat, a] [] [])
  let nativeV = Nothing
  negateV <-
    pure <$> do
      fn <- identifier' "negateC"
      pure (\onDict cat a -> mkMethodApps onDict fn [cat, a] [] [])
  notV <-
    pure <$> do
      fn <- identifier' "notC"
      pure (\onDict cat -> mkMethodApps onDict fn [cat] [] [])
  notEqualV <-
    pure <$> do
      fn <- identifier' "notEqual"
      pure (\onDict cat a -> mkMethodApps onDict fn [cat, a] [] [])
  let oddV = Nothing
  orV <-
    pure <$> do
      fn <- identifier' "orC"
      pure (\onDict cat -> mkMethodApps onDict fn [cat] [] [])
  plusV <-
    pure <$> do
      fn <- identifier' "addC"
      pure (\onDict cat a -> mkMethodApps onDict fn [cat, a] [] [])
  pointV <-
    pure <$> do
      fn <- identifier' "pointC"
      pure (\onDict cat f a -> mkMethodApps onDict fn [cat, f, a] [] [])
  let powV = Nothing
  let powIV = Nothing
  powIntV <-
    pure <$> do
      fn <- identifier' "powIC"
      pure (\onDict cat a -> mkMethodApps onDict fn [cat, a] [] [])
  let quotV = Nothing
  rassocV <-
    pure <$> do
      fn <- identifier' "rassocP"
      pure (\onDict cat a b c -> mkMethodApps onDict fn [cat] [a, b, c] [])
  let realToFracV = Nothing
  recipV <-
    pure <$> do
      fn <- identifier' "recipC"
      pure (\onDict cat a -> mkMethodApps onDict fn [cat, a] [] [])
  let remV = Nothing
  reprCV <- pure <$> repOp "reprC"
  sequenceAV <-
    pure <$> do
      fn <- identifier' "sequenceAC"
      pure (\onDict cat t f a -> mkMethodApps onDict fn [cat, t, f] [a] [])
  let signumV = Nothing
  sinV <-
    pure <$> do
      fn <- identifier' "sinC"
      pure (\onDict cat a -> mkMethodApps onDict fn [cat, a] [] [])
  let sinhV = Nothing
  sqrtV <-
    pure <$> do
      fn <- identifier' "sqrtC"
      pure (\onDict cat a -> mkMethodApps onDict fn [cat, a] [] [])
  strengthV <-
    pure <$> do
      fn <- identifier' "strength"
      pure (\onDict cat f a b -> mkMethodApps onDict fn [cat, f] [a, b] [])
  sumV <-
    pure <$> do
      fn <- identifier' "sumAC"
      pure (\onDict cat f a -> mkMethodApps onDict fn [cat, f, a] [] [])
  swapV <-
    pure <$> do
      fn <- identifier' "swapP"
      pure (\onDict cat a b -> mkMethodApps onDict fn [cat] [a, b] [])
  tabulateV <-
    pure <$> do
      fn <- identifier' "tabulateC"
      pure (\onDict cat f a -> mkMethodApps onDict fn [cat, f] [a] [])
  let tanV = Nothing
  let tanhV = Nothing
  timesV <-
    pure <$> do
      fn <- identifier' "mulC"
      pure (\onDict cat a -> mkMethodApps onDict fn [cat, a] [] [])
  let traverseV = Nothing
  uncurryV <-
    pure <$> do
      fn <- identifier' "uncurry"
      pure (\onDict cat a b c -> mkMethodApps onDict fn [cat] [a, b, c] [])
  pure Hierarchy {..}
  where
    identifier' = identifier moduleName
    repOp ::
      Monad f =>
      String ->
      Lookup
        ( (Plugins.CoreExpr -> f Plugins.CoreExpr) ->
          Plugins.Type ->
          Plugins.Type ->
          f Plugins.CoreExpr
        )
    repOp name = do
      op <- identifier "Categorifier.Category" name
      rep <- findTyCon "Categorifier.Client" "Rep"
      pure $ \onDict cat a -> mkMethodApps onDict op [cat, a, Plugins.mkTyConApp rep [a]] [] []

-- | A hierarchy using the type classes provided by Conal Eliot's @concat@ library.
--
--  __NB__: This uses "ConCat.Category" directly, and ignores the existence of "ConCat.AltCat".
classHierarchy :: Lookup (Hierarchy CategoryStack)
classHierarchy = do
  hierarchy <- hierarchy' moduleName
  fromIntegerV <-
    pure <$> do
      fn <- identifier moduleName "fromIntegralC"
      int <- Plugins.mkTyConTy <$> findTyCon "Prelude" "Integer"
      pure $ \onDict cat a ->
        mkMethodApps onDict fn [Plugins.typeKind int, Plugins.typeKind a, cat, int, a] [] []
  fromIntegralV <-
    pure <$> do
      fn <- identifier moduleName "fromIntegralC"
      pure $ \onDict cat a b ->
        mkMethodApps onDict fn [Plugins.typeKind a, Plugins.typeKind b, cat, a, b] [] []
  pure
    hierarchy
      { fromIntegerV = fromIntegerV,
        fromIntegralV = fromIntegralV
      }
  where
    moduleName = "ConCat.Category"

-- | A hierarchy using the functions from Conal's ConCat library. These are the same operations used
--   by Conal's original implementation.
functionHierarchy :: Lookup (Hierarchy CategoryStack)
functionHierarchy = do
  hierarchy <- hierarchy' moduleName
  fromIntegerV <-
    pure <$> do
      fn <- identifier moduleName "fromIntegralC"
      int <- Plugins.mkTyConTy <$> findTyCon "Prelude" "Integer"
      pure $ \onDict cat a -> mkMethodApps onDict fn [cat, int, a] [] []
  fromIntegralV <-
    pure <$> do
      fn <- identifier moduleName "fromIntegralC"
      pure $ \onDict cat a b -> mkMethodApps onDict fn [cat, a, b] [] []
  pure
    hierarchy
      { fromIntegerV = fromIntegerV,
        fromIntegralV = fromIntegralV
      }
  where
    moduleName = "ConCat.AltCat"
