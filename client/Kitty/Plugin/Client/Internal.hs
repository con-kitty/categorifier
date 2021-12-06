{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- | This is separate from "Kitty.Plugin.Client" because we can't define Template Haskell and use it in
--   the same module. This module provides `deriveHasRep`, but it's private because importing it
--   would mean you miss the instances.
module Kitty.Plugin.Client.Internal
  ( HasRep (..),
    deriveHasRep,
  )
where

import Data.Bifunctor (bimap)
import Data.Constraint (Dict (..))
import Data.Foldable (foldl', toList)
import Data.List.Extra (groupSort)
import Data.Maybe (listToMaybe)
import Data.Tuple.Extra (fst3, snd3, thd3)
import Data.Void (Void)
import qualified Kitty.Common.IO.Exception as Exception
import qualified Language.Haskell.TH as TH

-- | Convert to and from standard representations. Used for transforming case
-- expression scrutinees and constructor applications. The 'repr' method should
-- convert to a standard representation (unit, products, sums), or closer to
-- such a representation, via another type with a 'HasRep' instance. The 'abst'
-- method should reveal a constructor so that we can perform the
-- case-of-known-constructor transformation.
--
-- It is very important to give @INLINE@ pragmas for 'repr' and 'abst' definitions.
class HasRep a where
  type Rep a
  repr :: a -> Rep a
  abst :: Rep a -> a

data DeriveCallFailure
  = GadtMissingName TH.Type
  | InvalidName TH.Info
  | MisquotedName

-- | Defines a `HasRep` instance, needed for pretty much any type that ends up passing through the
--  "Kitty.Plugin" plugin.
--
--   Almost any `HasRep` instance can be created with this. The one exception is GADTs that have
--  "overlapping" constructor types. That is, a type like
--
-- > data AltVec n a where
-- >   AVNil :: AltVec 'Z a
-- >   ACons :: a -> AltVec n a -> AltVec ('S n) a
-- >   BCons :: a -> AltVec n a -> AltVec ('S n) a
--
--   should work just fine, because the constructors can be split into disjoint groups, one
--   containing the constructors with type @AltVec 'Z a@ (@AVNil@) and one containing the
--   constructors with type @AltVec ('S n) a@ (@ACons@ and @BCons@). There is no way for the two
--   groups to overlap. However, a type like
--
-- > data SomeExpr a where
-- >   IntLit :: Int -> SomeExpr Int
-- >   DubLit :: Double -> SomeExpr Double
-- >   Add :: SomeExpr a -> SomeExpr a -> SomeExpr a
--
--   will fail because @Add@ is must be in both the @SomeExpr Int@ and @SomeExpr Double@ groups,
--   which is not yet supported.
--
--   The following documentation should be written on `HasRep`, but that's not managed by us, so we'll explain it here for now:
--
--   There are some expectations for a `HasRep` instance. This function ensures that they hold, but
--   in the case you have to write an instance by hand, these guidelines are important to
--   follow. The plugin needs the implementations to be completely inlined easily, so
--
-- - always add @INLINE@ pragmas for both the `abst` and `repr` implementations, write them using
-- - /only/ @case@ expressions and data constructors, alternatives should be combined using `Either`
-- - and fields with @(,)@ (no larger tuples, only -- 2-tuples), `Dict` should be used to reify
-- - constraints into the set of fields, and try to make the nesting as even as possible (i.e.,
-- - @((a, b), (c, d))@ is better than -- @(a, (b, (c, d)))@.
deriveHasRep :: TH.Name -> TH.DecsQ
deriveHasRep name =
  either (Exception.throwIOAsException explainDeriveCallFailure) sequenceA . deriveHasRep'
    =<< TH.reify name
  where
    explainDeriveCallFailure = \case
      GadtMissingName ty ->
        "a GADT constructor with type "
          <> show ty
          <> " has no constructor name. This should be impossible."
      InvalidName info -> "expected type constructor for " <> show name <> " but got " <> show info
      MisquotedName ->
        "expected type constructor for "
          <> show name
          <> " but got Data Constructor. Did you only put one apostrophe on your thingy?"

deriveHasRep' :: TH.Info -> Either DeriveCallFailure [TH.DecQ]
deriveHasRep' = \case
  TH.DataConI {} -> Left MisquotedName
  TH.TyConI (TH.DataD ctx name tyVarBndrs _ dataCons _) ->
    hasRep (applyType name tyVarBndrs) ctx dataCons
  TH.TyConI (TH.NewtypeD ctx name tyVarBndrs _ dataCon _) ->
    hasRep (applyType name tyVarBndrs) ctx $ pure dataCon
  info -> Left $ InvalidName info
  where
    applyType name = foldl' TH.AppT (TH.ConT name) . fmap (TH.VarT . nameOfBinder)
    nameOfBinder (TH.PlainTV n) = n
    nameOfBinder (TH.KindedTV n _) = n

    hasRep :: TH.Type -> TH.Cxt -> [TH.Con] -> Either DeriveCallFailure [TH.DecQ]
    hasRep type0 ctx = fmap (fmap (uncurry sums) . groupSort) . traverse (processCon type0 ctx)

    processCon ::
      TH.Type ->
      TH.Cxt ->
      TH.Con ->
      Either DeriveCallFailure (TH.Type, (TH.TypeQ, (TH.PatQ, TH.ExpQ), (TH.PatQ, TH.ExpQ)))
    processCon type0 ctx = \case
      TH.ForallC _ ctx' con -> processCon type0 (ctx <> ctx') con
      TH.GadtC names fieldTypes type1 ->
        maybe
          (Left $ GadtMissingName type1)
          (\conName -> pure (type1, hasRep' conName ctx $ fmap snd fieldTypes))
          $ listToMaybe names
      TH.InfixC fstField conName sndField ->
        pure (type0, hasRep' conName ctx $ fmap snd [fstField, sndField])
      TH.NormalC conName fieldTypes -> pure (type0, hasRep' conName ctx $ fmap snd fieldTypes)
      TH.RecC conName fieldTypes -> pure (type0, hasRep' conName ctx $ fmap thd3 fieldTypes)
      TH.RecGadtC names fieldTypes type1 ->
        maybe
          (Left $ GadtMissingName type1)
          (\conName -> pure (type1, hasRep' conName ctx $ fmap thd3 fieldTypes))
          $ listToMaybe names

    sums :: TH.Type -> [(TH.TypeQ, (TH.PatQ, TH.ExpQ), (TH.PatQ, TH.ExpQ))] -> TH.DecQ
    sums type0 cons =
      hasRepInstD
        (pure type0)
        (mkNestedPairs (\x y -> [t|Either $x $y|]) [t|Void|] =<< traverse fst3 cons)
        (buildClauses (mkNestedSums (\x -> [p|Left $x|]) (\x -> [p|Right $x|])) id $ fmap snd3 cons)
        (buildClauses id (mkNestedSums (\x -> [|Left $x|]) (\x -> [|Right $x|])) $ fmap thd3 cons)

    buildClauses ::
      ([TH.PatQ] -> [TH.PatQ]) -> ([TH.ExpQ] -> [TH.ExpQ]) -> [(TH.PatQ, TH.ExpQ)] -> [TH.ClauseQ]
    buildClauses patFn expFn =
      fmap (flip (uncurry TH.clause) [])
        . uncurry zip
        . bimap (fmap pure . patFn) (fmap (fmap TH.NormalB) . expFn)
        . unzip

    hasRepInstD :: TH.TypeQ -> TH.TypeQ -> [TH.ClauseQ] -> [TH.ClauseQ] -> TH.DecQ
    hasRepInstD type0 repTy abstClauses reprClauses =
      TH.instanceD
        (pure [])
        [t|HasRep $type0|]
        [ TH.tySynInstD $ TH.tySynEqn Nothing [t|Rep $type0|] repTy,
          TH.funD 'abst abstClauses,
          TH.pragInlD 'abst TH.Inline TH.FunLike TH.AllPhases,
          TH.funD 'repr reprClauses,
          TH.pragInlD 'repr TH.Inline TH.FunLike TH.AllPhases
        ]

    hasRep' :: TH.Name -> TH.Cxt -> [TH.Type] -> (TH.TypeQ, (TH.PatQ, TH.ExpQ), (TH.PatQ, TH.ExpQ))
    hasRep' conName predTypes fieldTypes =
      let vars = take (length fieldTypes) varSupply
          dicts = replicate (length predTypes) 'Dict
       in ( mkNestedPairs (\x y -> [t|($x, $y)|]) [t|()|] $
              fmap (TH.AppT $ TH.ConT ''Dict) predTypes <> fieldTypes,
            ( mkNestedPairs (\x y -> [p|($x, $y)|]) [p|()|] $
                fmap (`TH.ConP` []) dicts <> fmap TH.VarP vars,
              pure $ foldl' (\e -> TH.AppE e . TH.VarE) (TH.ConE conName) vars
            ),
            ( pure $ TH.ConP conName . fmap TH.VarP $ toList vars,
              mkNestedPairs (\x y -> [|($x, $y)|]) [|()|] $ fmap TH.ConE dicts <> fmap TH.VarE vars
            )
          )

    mkNestedPairs :: forall a. (TH.Q a -> TH.Q a -> TH.Q a) -> TH.Q a -> [a] -> TH.Q a
    mkNestedPairs mkPair unit = \case
      [] -> unit
      [x] -> pure x
      xs ->
        let (ys, zs) = splitAt (length xs `div` 2) xs
         in mkPair (mkNestedPairs mkPair unit ys) (mkNestedPairs mkPair unit zs)

    mkNestedSums :: forall a. (TH.Q a -> TH.Q a) -> (TH.Q a -> TH.Q a) -> [TH.Q a] -> [TH.Q a]
    mkNestedSums mkL mkR = \case
      [] -> []
      [x] -> [x]
      xs ->
        let (ys, zs) = splitAt (length xs `div` 2) xs
         in (mkL <$> mkNestedSums mkL mkR ys) <> (mkR <$> mkNestedSums mkL mkR zs)

    varSupply :: [TH.Name]
    varSupply = TH.mkName <$> ["hasrep_" <> i | i <- show <$> [0 :: Int ..]]
