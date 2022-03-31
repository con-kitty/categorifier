{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- | This is separate from "Categorifier.Client" because we can't define Template Haskell and use it in
--   the same module. This module provides `deriveHasRep`, but it's private because importing it
--   would mean you miss the instances.
module Categorifier.Client.Internal
  ( HasRep (..),
    deriveHasRep,
  )
where

import qualified Categorifier.Common.IO.Exception as Exception
import Categorifier.Duoidal (Parallel (..), traverseD)
import Categorifier.Duoidal.Either (noteAccum)
import qualified Categorifier.TH as TH
import Data.Bifunctor (Bifunctor (..))
import Data.Constraint (Dict (..))
import Data.Foldable (foldl', toList)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (listToMaybe)
import Data.Semigroup (Any (..))
import Data.Tuple.Extra (fst3, snd3, thd3)
import Data.Void (Void)
import PyF (fmt)

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
  -- | The "standard representation" that forms an isomorphism with the underlying type. This type
  --   should only modify the current type (not the types contained within this one), and should
  --   consist only of `Either`, `Void`, `()`, `(,)` (strictly 2-tuples), and `Dict`.
  --
  --   E.g., the type
  -- > data Foo a = Num a => Bar a Int String | Baz (a -> a)
  --   would have an instance like the following
  -- > type Rep (Foo a) = Either (Dict (Num a), ((a, Int), String)) (a -> a)
  --  (the specific tuple groupings and orderings are not important, so long as the isomporphism
  --   holds, but making the structure as shallow as possible helps with performance).
  --
  --   Existentials are not supported (as they're not supported by type families in general), but
  --   some GADTs can be handled by creating multiple `HasRep` instances, one for each "return type"
  --   so long as all the return types are disjoint (again, as type families don't allow
  --   overlapping).
  --
  --  __NB__: The actual type here is considered an implementation detail, and should not be relied
  --          on (beyond the expectations provided above). Minor changes will not be considered
  --          breaking.
  type Rep a

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
-- > instance HasRep (SomeExpr Double) where
-- >   type Rep (SomeExpr Double) = Either Double (SomeExpr Double, SomeExpr Double)
-- >   abst = \case
-- >     Left d -> DubLit d
-- >     Right (x, y) -> Add x y
-- >   {-# INLINE abst #-}
-- >   repr = \case
-- >     DubLit d -> Left d
-- >     Add x y -> Right (x, y)
-- >   {-# INLINE repr #-}
-- > instance HasRep (SomeExpr Int) where
-- >   type Rep (SomeExpr Int) = Either Int (SomeExpr Int, SomeExpr Int)
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

groupByType :: [(TH.Type, (TH.TypeQ, a, b))] -> [(TH.Type, NonEmpty (TH.TypeQ, a, b))]
groupByType = foldr gbt []
  where
    gbt (ty, (tq, p, e)) existing =
      let (Any hasMatched, updatedMap) =
            traverse
              ( \(t, es) ->
                  -- if the types are alpha equivalent, then alpha rename the `Rep` of the type to
                  -- match the variables in the group key.
                  maybe
                    (Any False, (t, es))
                    ( \m ->
                        ( Any True,
                          ( t,
                            NE.cons
                              ( either
                                  -- __TODO__: This throws too soon, but it's complicated to
                                  --           propagate this particular failure. We need to build
                                  --           the `Rep` outised of `Q`.
                                  ( Exception.throwIOAsException explainGadtProcessingFailure
                                      . UnableToAlphaRename
                                  )
                                  pure
                                  . TH.alphaRename m
                                  =<< tq,
                                p,
                                e
                              )
                              es
                          )
                        )
                    )
                    $ TH.alphaEquiv ty t
              )
              existing
       in if hasMatched then updatedMap else (ty, pure (tq, p, e)) : updatedMap

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
      fmap (fmap (uncurry sums) . groupByType)
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

    sums :: TH.Type -> NonEmpty (TH.TypeQ, (TH.PatQ, TH.ExpQ), (TH.PatQ, TH.ExpQ)) -> TH.DecQ
    sums type0 cons =
      hasRepInstD
        (pure type0)
        (mkNestedPairs (\x y -> [t|Either $x $y|]) [t|Void|] . toList =<< traverse fst3 cons)
        (buildClauses (mkNestedSums (\x -> [p|Left $x|]) (\x -> [p|Right $x|])) id . toList $ fmap snd3 cons)
        (buildClauses id (mkNestedSums (\x -> [|Left $x|]) (\x -> [|Right $x|])) . toList $ fmap thd3 cons)

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
        [
          TH.tySynInstD' ''Rep [type0] repTy,
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
