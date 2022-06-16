{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- | This is separate from "Categorifier.Client" because we can't define Template Haskell and use it
--   in the same module. This module provides `deriveHasRep`, but the module is private because
--   importing it directly would mean you miss the instances.
module Categorifier.Client.Internal
  ( HasRep (..),
    Rep,
    deriveHasRep,
  )
where

import qualified Categorifier.Common.IO.Exception as Exception
import Categorifier.Duoidal (Parallel (..), traverseD)
import Categorifier.Duoidal.Either (noteAccum)
import qualified Categorifier.TH as TH
import Control.Monad ((<=<))
import Data.Bifunctor (Bifunctor (..))
import Data.Constraint (Dict (..))
import Data.Foldable (foldl', toList)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (isJust, listToMaybe)
import Data.Semigroup (Any (..))
import Data.Tuple.Extra (fst3, snd3, thd3)
import Data.Void (Void)
import PyF (fmt)

-- | The "standard representation" that forms an isomorphism with the underlying type, @a@.
--   @`Rep` a@ should roughly replace all the constructors of the underlying type with a combination
--   of `Either`, `Void`, `()`, `(,)` (strictly 2-tuples), and `Dict`; leaving the types of the
--   fields untouched.
--
--   E.g., the type
-- > data Foo a = Num a => Bar a Int String | Baz (a -> a)
--   would have an instance like the following
-- > type instance Rep (Foo a) = Either (Dict (Num a), ((a, Int), String)) (a -> a)
--   The specific tuple groupings and orderings are not important, so long as the isomorphism holds.
--   However, making more balanced shallow structures, like @((a, b), (c, d))@, rather than biased
--   deep structures, like @(a, (b, (c, d)))@, helps with performance.
--
--   Existentials are not supported (as they're not supported by type families in general), but many
--   GADTs can be handled by creating multiple `Rep` instances, at least one for each "return type".
--   If the return types overlap, there may need to be additional `Rep` instances defined.
--
--  __NB__: The actual type here is considered an implementation detail, and should not be relied on
--         (beyond the expectations provided above). Minor changes will not be considered breaking.
--
--  __NB__: This is intentionally not an associated type family of `HasRep` in order to avoid some
--          overlapping instance issues with GADTs. E.g., given a type like
--        > data Bar a where
--        >   BNat :: Natural -> Bar Natural
--        >   BDub :: Double -> Bar Double
--        >   BEmpty :: Bar a
--        >   BCombine :: Bar a -> Bar a -> Bar a
--          there are three distinct `HasRep` instances, @Bar Natural@ and @Bar Double@ each have
--          three constructors to handle, and @Bar a@ has two constructors to handle. However,
--          @Bar a@ must be declared @overappable@, and that isn't allowed if the class has an
--          associated type family.
--
--          We could replace @Bar a@ with separate instances for each concrete @a@ we need to
--          support, but that introduces a lot of duplication. To avoid that, we keep the type
--          family separate, allowing the overlappable instance, then only duplicate the
--          @type instance `Rep`@ declaration for each concrete @a@ we need to support.
type family Rep a

-- | Convert to and from standard representations. Used for transforming case expression scrutinees
--   and constructor applications. The 'repr' method should convert to a standard representation
--  (unit, products, sums). The 'abst' method should reveal a constructor so that we can perform the
--   case-of-known-constructor transformation.
--
--  __IMPORTANT__: 99.9% of the time, `deriveHasRep` is the best way to define any `HasRep` instance
--                 you need. If you _do_ need to define your own, it is very important to give
--                 @INLINE@ pragmas for 'repr' and 'abst' definitions. You may also want to use
--                 @-ddump-splices@ along with `deriveHasRep` to give you a starting point. If you
--                 do define one manually, please open an issue including your type and manual
--                 instance, so we can try to improve the `deriveHasRep` mechanism.
--
--   This is similar to `Generic`, but for the plugin to work, this needs to deterministically
--   reduce to simple pattern matching and data constructor application, which we can't always
--   expect from `Generic` instances (at least in GHC 8.10). If this changes in future, it would be
--   good to support `Generic` as an alternative representation.
class HasRep a where
  -- | Convert a value in the standard represetation to a value of the underlying type.
  --
  --  __NB__: The implementation should contain only pattern matching and data constructor
  --          application. All instances _must_ @INLINE@ this implementation.
  abst :: Rep a -> a

  -- | Convert a value of the underlying type to a value in the standard representation.
  --
  --  __NB__: The implementation should contain only pattern matching and data constructor
  --          application. All instances _must_ @INLINE@ this implementation.
  repr :: a -> Rep a

data GadtProcessingFailure
  = GadtMissingName TH.Con
  | -- | __TODO__: This should also carry the `TH.Con`.
    UnableToAlphaRename (NonEmpty TH.Name)

data DeriveCallFailure
  = GadtProcessingFailures (NonEmpty GadtProcessingFailure)
  | InvalidName TH.Info
  | MisquotedName

-- | Defines a `HasRep` instance, needed for pretty much any type that ends up passing through the
--  "Categorifier" plugin.
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
-- >   DubLit :: Double -> SomeExpr Double
-- >   IntLit :: Int -> SomeExpr Int
-- >   Add :: SomeExpr a -> SomeExpr a -> SomeExpr a
--
--   will fail because @Add@ must be in both the @SomeExpr `Double`@ and @SomeExpr `Int`@ groups,
--   which is not yet supported. The two instances should be able to be hand-written, because an @a@
--   other than `Double` or `Int` can never be introduced, so this is distinct from the more
--   problematic overlapping case that is currently impossible to support at all.
--
-- > type instance Rep (SomeExpr Double) = Either Double (SomeExpr Double, SomeExpr Double)
-- > type instance Rep (SomeExpr Int) = Either Int (SomeExpr Int, SomeExpr Int)
-- > instance HasRep (SomeExpr Double) where
-- >   abst = \case
-- >     Left d -> DubLit d
-- >     Right (x, y) -> Add x y
-- >   {-# INLINE abst #-}
-- >   repr = \case
-- >     DubLit d -> Left d
-- >     Add x y -> Right (x, y)
-- >   {-# INLINE repr #-}
-- > instance HasRep (SomeExpr Int) where
-- >   abst = \case
-- >     Left i -> IntLit i
-- >     Right (x, y) -> Add x y
-- >   {-# INLINE abst #-}
-- >   repr = \case
-- >     IntLit i -> Left i
-- >     Add x y -> Right (x, y)
-- >   {-# INLINE repr #-}
deriveHasRep :: TH.Name -> TH.DecsQ
deriveHasRep name =
  either (Exception.throwIOAsException explainDeriveCallFailure) sequenceA . deriveHasRep'
    =<< TH.reify name
  where
    explainDeriveCallFailure = \case
      GadtProcessingFailures failures ->
        [fmt|Some GADT alternatives had errors while processing: \
{mconcat (toList $ fmap (("\n- " <>) . explainGadtProcessingFailure) failures)}|]
      InvalidName info -> [fmt|expected type constructor for {show name} but got {show info}|]
      MisquotedName ->
        [fmt|expected type constructor for {show name} but got data constructor. Did you only put\
one apostrophe on your thingy?|]

explainGadtProcessingFailure :: GadtProcessingFailure -> String
explainGadtProcessingFailure = \case
  GadtMissingName ty ->
    [fmt|a GADT constructor with type {show ty} has no constructor name. \
This should be impossible.|]
  UnableToAlphaRename _ -> ""

findDistinctTypes :: [TH.Type] -> [TH.Type]
findDistinctTypes = foldr gbt []
  where
    gbt ty existing = if any (isJust . TH.alphaEquiv ty) existing then existing else ty : existing

groupByType :: [(TH.Type, (TH.TypeQ, a, b))] -> [(TH.Type, (Bool, [(TH.TypeQ, a, b)]))]
groupByType tys = foldr gbt distinctTys tys
  where
    distinctTys = fmap (,(False, [])) . findDistinctTypes $ fst <$> tys
    gbt (ty, (tq, p, e)) existing =
      let rewrite m =
            either
              -- __TODO__: This throws too soon, but it's complicated to propagate this
              --           particular failure. We need to build the `Rep` outside of `Q`.
              (Exception.throwIOAsException explainGadtProcessingFailure . UnableToAlphaRename)
              pure
              . TH.rewriteType m
              =<< tq
          (Any hasMatched, updatedMap) =
            traverse
              ( \(t, (overlappable, es)) ->
                  -- if the types are alpha equivalent, then alpha rename the `Rep` of the type to
                  -- match the variables in the group key.
                  maybe
                    (Any False, (t, (overlappable, es)))
                    ( \case
                        (EQ, m) -> (Any True, (t, (overlappable, (rewrite m, p, e) : es)))
                        (GT, m) -> (Any False, (t, (overlappable, (rewrite m, p, e) : es)))
                        (LT, _) -> (Any False, (t, (True, es)))
                    )
                    $ TH.compareTypes ty t
              )
              existing
       in if hasMatched then updatedMap else (ty, (False, pure (tq, p, e))) : updatedMap

deriveHasRep' :: TH.Info -> Either DeriveCallFailure [TH.DecQ]
deriveHasRep' = \case
  TH.DataConI {} -> Left MisquotedName
  TH.TyConI (TH.DataD ctx name tyVarBndrs _ dataCons _) ->
    first GadtProcessingFailures $ hasReps (applyType name tyVarBndrs) ctx dataCons
  TH.TyConI (TH.NewtypeD ctx name tyVarBndrs _ dataCon _) ->
    first GadtProcessingFailures $ hasReps (applyType name tyVarBndrs) ctx $ pure dataCon
  info -> Left $ InvalidName info
  where
    applyType name = foldl' TH.AppT (TH.ConT name) . fmap (TH.VarT . TH.tyVarBndrName)

    -- Produces one or more `HasRep` instances for the given `TH.Type`. It can be more than one in
    -- the case of GADTs.
    hasReps :: TH.Type -> TH.Cxt -> [TH.Con] -> Either (NonEmpty GadtProcessingFailure) [TH.DecQ]
    hasReps type0 ctx =
      fmap (uncurry sums <=< groupByType)
        . traverseD (((first (fmap GadtMissingName) . getParallel) .) . noteAccum $ processCon type0 ctx)

    processCon ::
      TH.Type ->
      TH.Cxt ->
      TH.Con ->
      Maybe (TH.Type, (TH.TypeQ, (TH.PatQ, TH.ExpQ), (TH.PatQ, TH.ExpQ)))
    processCon type0 ctx = \case
      TH.ForallC _ ctx' con -> processCon type0 (ctx <> ctx') con
      TH.GadtC names fieldTypes type1 ->
        (\conName -> (type1, hasRep' conName ctx $ fmap snd fieldTypes)) <$> listToMaybe names
      TH.InfixC fstField conName sndField ->
        pure (type0, hasRep' conName ctx $ fmap snd [fstField, sndField])
      TH.NormalC conName fieldTypes -> pure (type0, hasRep' conName ctx $ fmap snd fieldTypes)
      TH.RecC conName fieldTypes -> pure (type0, hasRep' conName ctx $ fmap thd3 fieldTypes)
      TH.RecGadtC names fieldTypes type1 ->
        (\conName -> (type1, hasRep' conName ctx $ fmap thd3 fieldTypes)) <$> listToMaybe names

    sums :: TH.Type -> (Bool, [(TH.TypeQ, (TH.PatQ, TH.ExpQ), (TH.PatQ, TH.ExpQ))]) -> [TH.DecQ]
    sums type0 (overlappable, cons) =
      let repTy = mkNestedPairs (\x y -> [t|Either $x $y|]) [t|Void|] =<< traverse fst3 cons
       in hasRepInstD
            (if overlappable then pure repTy else Nothing)
            (pure type0)
            ( buildClauses (mkNestedSums (\x -> [p|Left $x|]) (\x -> [p|Right $x|])) id $
                fmap snd3 cons
            )
            ( buildClauses id (mkNestedSums (\x -> [|Left $x|]) (\x -> [|Right $x|])) $
                fmap thd3 cons
            ) :
          if overlappable then mempty else pure $ repInstD (pure type0) repTy

    buildClauses ::
      ([TH.PatQ] -> [TH.PatQ]) -> ([TH.ExpQ] -> [TH.ExpQ]) -> [(TH.PatQ, TH.ExpQ)] -> [TH.ClauseQ]
    buildClauses patFn expFn =
      fmap (flip (uncurry TH.clause) [])
        . uncurry zip
        . bimap (fmap pure . patFn) (fmap (fmap TH.NormalB) . expFn)
        . unzip

    repInstD :: TH.TypeQ -> TH.TypeQ -> TH.DecQ
    repInstD type0 = TH.tySynInstD . TH.tySynEqn Nothing (foldl' TH.appT (TH.conT ''Rep) [type0])

    hasRepInstD :: Maybe TH.TypeQ -> TH.TypeQ -> [TH.ClauseQ] -> [TH.ClauseQ] -> TH.DecQ
    hasRepInstD mRepTy type0 abstClauses reprClauses =
      TH.instanceWithOverlapD
        (TH.Overlappable <$ mRepTy)
        (maybe (pure []) (fmap pure . TH.appT (TH.appT TH.equalityT (TH.appT (TH.conT ''Rep) type0))) mRepTy)
        [t|HasRep $type0|]
        [ TH.funD 'abst abstClauses,
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
                fmap (`TH.conP` []) dicts <> fmap TH.VarP vars,
              pure $ foldl' (\e -> TH.AppE e . TH.VarE) (TH.ConE conName) vars
            ),
            ( pure $ TH.conP conName . fmap TH.VarP $ toList vars,
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
    varSupply = TH.mkName <$> [[fmt|hasrep_{i}|] | i <- show <$> [0 :: Int ..]]
