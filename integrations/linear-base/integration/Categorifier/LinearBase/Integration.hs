{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Categorifier.LinearBase.Integration
  ( makerMapFun,
    symbolLookup,
  )
where

import Categorifier.Core.MakerMap
  ( MakerMapFun,
    SymbolLookup (..),
    applyEnrichedCat,
    applyEnrichedCat',
    composeCat,
    curryCat,
    forkCat,
    handleAdditionalArgs,
    makeLookupMap,
    makeMaker1,
    makeMaker2,
    makeTupleTyWithVar,
    uncurryCat,
  )
import Categorifier.Core.Makers (Makers (..))
import Categorifier.Core.Types (Lookup)
import Categorifier.Duoidal (joinD, (<*\>), (<=\<), (=<\<))
import qualified Categorifier.GHC.Builtin as Plugins
import qualified Categorifier.GHC.Core as Plugins
import qualified Control.Functor.Linear
import qualified Data.Array.Mutable.Linear
import qualified Data.Array.Mutable.Unlifted.Linear
import qualified Data.Bool.Linear
import qualified Data.Either.Linear
import qualified Data.Functor.Linear
import qualified Data.List.Linear
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Monoid.Linear
import qualified Data.Num.Linear
import qualified Data.Ord.Linear
import qualified Data.Replicator.Linear
import qualified Data.Tuple.Linear
import qualified Data.V.Linear
import qualified Prelude.Linear
import qualified Streaming.Prelude.Linear
import qualified Unsafe.Linear

symbolLookup :: Lookup SymbolLookup
symbolLookup =
  makeLookupMap
    [ ''Data.Array.Mutable.Linear.Array,
      ''Data.Array.Mutable.Unlifted.Linear.Array#,
      ''Data.Replicator.Linear.Replicator,
      ''Data.V.Linear.V,
      ''Streaming.Prelude.Linear.Stream
    ]

makerMapFun :: MakerMapFun
makerMapFun
  symLookup
  _dflags
  _logger
  m@Makers {..}
  n
  _target
  expr
  _cat
  _var
  _args
  _modu
  _categorifyFun
  categorifyLambda =
    makerMap
    where
      makerMap =
        Map.fromListWith
          const
          [ ( '(Control.Functor.Linear.<$>),
              \case
                f : a : b : functor : rest ->
                  ($ (f : functor : a : b : rest)) =<< Map.lookup 'Control.Functor.Linear.fmap makerMap
                _ -> Nothing
            ),
            ( '(Control.Functor.Linear.<*>),
              \case
                Plugins.Type f : _ap : Plugins.Type a : Plugins.Type b : rest ->
                  pure $ maker2 rest =<\< mkAp f a b
                _ -> Nothing
            ),
            ( '(Control.Functor.Linear.>>=),
              \case
                Plugins.Type mty : _monad : Plugins.Type a : Plugins.Type b : rest ->
                  pure $ maker2 rest =<\< mkBind mty a b
                _ -> Nothing
            ),
            ( 'Control.Functor.Linear.ap,
              \case
                mty : a : b : monad : rest ->
                  ($ (mty : monad : a : b : rest)) =<< Map.lookup '(Control.Functor.Linear.<*>) makerMap
                _ -> Nothing
            ),
            ( 'Control.Functor.Linear.fmap,
              \case
                Plugins.Type f : _functor : Plugins.Type a : Plugins.Type b : u : rest ->
                  pure $ mkMap' f a b u rest
                _ -> Nothing
            ),
            ( 'Control.Functor.Linear.liftA2,
              \case
                Plugins.Type f : _ap : Plugins.Type a : Plugins.Type b : Plugins.Type c
                  : u
                  : rest ->
                    pure $ mkLiftA2' f a b c u rest
                _ -> Nothing
            ),
            ( 'Control.Functor.Linear.pure,
              \case
                Plugins.Type f : _applicative : Plugins.Type a : rest ->
                  pure $ maker1 rest =<\< mkPoint f a
                _ -> Nothing
            ),
            ( 'Control.Functor.Linear.return,
              \case
                mty : a : monad : rest ->
                  ($ mty : monad : a : rest) =<< Map.lookup 'Control.Functor.Linear.pure makerMap
                _ -> Nothing
            ),
            ( 'Data.Array.Mutable.Linear.map,
              \case
                Plugins.Type a : Plugins.Type b : u : rest -> do
                  array <- Map.lookup ''Data.Array.Mutable.Linear.Array (tyConLookup symLookup)
                  pure $ mkMap' (Plugins.mkTyConTy array) a b u rest
                _ -> Nothing
            ),
            ( 'Data.Array.Mutable.Unlifted.Linear.map,
              \case
                Plugins.Type a : Plugins.Type b : u : rest -> do
                  array <- Map.lookup ''Data.Array.Mutable.Unlifted.Linear.Array# (tyConLookup symLookup)
                  pure $ mkMap' (Plugins.mkTyConTy array) a b u rest
                _ -> Nothing
            ),
            ('(Data.Bool.Linear.&&), \rest -> pure $ maker2 rest =<\< mkAnd),
            ('(Data.Bool.Linear.||), \rest -> pure $ maker2 rest =<\< mkOr),
            ('Data.Bool.Linear.not, \rest -> pure $ maker1 rest =<\< mkNot),
            ( 'Data.Either.Linear.either,
              \case
                Plugins.Type a1 : Plugins.Type b : Plugins.Type a2 : u : v : rest ->
                  -- As in §8:
                  -- (λx → U ▽ V) ≡ curry ((uncurry (λx → U) ▽ uncurry (λx → V)) ◦ distl)
                  pure . joinD $
                    applyEnriched' [u, v] rest
                      <$> mkJoin (nameTuple a1) (nameTuple a2) b
                        <*\> mkDistl (Plugins.varType n) a1 a2
                _ -> Nothing
            ),
            ( '(Data.Functor.Linear.<*>),
              \case
                Plugins.Type f : _ap : Plugins.Type a : Plugins.Type b : rest ->
                  pure $ maker2 rest =<\< mkAp f a b
                _ -> Nothing
            ),
            ( '(Data.Functor.Linear.<$>),
              \case
                f : a : b : functor : rest ->
                  ($ (f : functor : a : b : rest)) =<< Map.lookup 'Data.Functor.Linear.fmap makerMap
                _ -> Nothing
            ),
            ( 'Data.Functor.Linear.fmap,
              \case
                Plugins.Type f : _functor : Plugins.Type a : Plugins.Type b : u : rest ->
                  pure $ mkMap' f a b u rest
                _ -> Nothing
            ),
            ( 'Data.Functor.Linear.liftA2,
              \case
                Plugins.Type f : _ap : Plugins.Type a : Plugins.Type b : Plugins.Type c
                  : u
                  : rest ->
                    pure $ mkLiftA2' f a b c u rest
                _ -> Nothing
            ),
            ( 'Data.Functor.Linear.pure,
              \case
                Plugins.Type f : _applicative : Plugins.Type a : rest ->
                  pure $ maker1 rest =<\< mkPoint f a
                _ -> Nothing
            ),
            ( 'Data.Functor.Linear.sequence,
              \case
                Plugins.Type t
                  : _traversable
                  : Plugins.Type f
                  : Plugins.Type a
                  : _applicative
                  : rest ->
                    pure $ maker1 rest =<\< mkSequenceA t f a
                _ -> Nothing
            ),
            ( 'Data.Functor.Linear.sequenceA,
              \case
                Plugins.Type t
                  : Plugins.Type f
                  : Plugins.Type a
                  : _traversable
                  : _applicative
                  : rest ->
                    pure $ maker1 rest =<\< mkSequenceA t f a
                _ -> Nothing
            ),
            ( 'Data.Functor.Linear.traverse,
              \case
                Plugins.Type t : _traversable : Plugins.Type f : Plugins.Type a : Plugins.Type b
                  : _applicative
                  : u
                  : rest ->
                    pure . joinD $
                      applyEnriched' [u] rest
                        <$> mkTraverse t f (nameTuple a) b
                          <*\> mkStrength t (Plugins.varType n) a
                _ -> Nothing
            ),
            ( '(Data.List.Linear.++),
              \case
                Plugins.Type a : rest ->
                  pure . (maker2 rest <=\< mkAppend) $ Plugins.mkTyConApp Plugins.listTyCon [a]
                _ -> Nothing
            ),
            ( 'Data.List.Linear.map,
              \case
                Plugins.Type a : Plugins.Type b : u : rest ->
                  pure $ mkMap' (Plugins.mkTyConTy Plugins.listTyCon) a b u rest
                _ -> Nothing
            ),
            ( 'Data.List.Linear.sum,
              \case
                Plugins.Type a : _num : rest ->
                  pure $ maker1 rest =<\< mkSum (Plugins.mkTyConTy Plugins.listTyCon) a
                _ -> Nothing
            ),
            ( 'Data.List.Linear.traverse',
              \case
                Plugins.Type f : Plugins.Type a : Plugins.Type b
                  : _applicative
                  : u
                  : rest ->
                    let list = Plugins.mkTyConTy Plugins.listTyCon
                     in pure . joinD $
                          applyEnriched' [u] rest
                            <$> mkTraverse list f (nameTuple a) b
                              <*\> mkStrength list (Plugins.varType n) a
                _ -> Nothing
            ),
            ( '(Data.Monoid.Linear.<>),
              \case
                Plugins.Type a : _semigroup : rest -> pure $ maker2 rest =<\< mkAppend a
                _ -> Nothing
            ),
            ( 'Data.Monoid.Linear.mappend,
              fromMaybe (const Nothing) $ Map.lookup '(Data.Monoid.Linear.<>) makerMap
            ),
            ( '(Data.Num.Linear.+),
              \case
                Plugins.Type ty : _additive : rest -> pure $ maker2 rest =<\< mkPlus ty
                _ -> Nothing
            ),
            -- TODO: Catch cases on Natural/Word to turn into Monus
            ( '(Data.Num.Linear.-),
              \case
                Plugins.Type ty : _additive : rest -> pure $ maker2 rest =<\< mkMinus ty
                _ -> Nothing
            ),
            ( '(Data.Num.Linear.*),
              \case
                Plugins.Type ty : _multiplicative : rest -> pure $ maker2 rest =<\< mkTimes ty
                _ -> Nothing
            ),
            ( 'Data.Num.Linear.abs,
              \case
                Plugins.Type ty : _num : rest -> pure $ maker1 rest =<\< mkAbs ty
                _ -> Nothing
            ),
            ( 'Data.Num.Linear.fromInteger,
              \case
                Plugins.Type ty : _fromInteger : rest -> pure $ maker1 rest =<\< mkFromInteger ty
                _ -> Nothing
            ),
            ( 'Data.Num.Linear.negate,
              \case
                Plugins.Type ty : _additive : rest -> pure $ maker1 rest =<\< mkNegate ty
                _ -> Nothing
            ),
            ( 'Data.Num.Linear.signum,
              \case
                Plugins.Type ty : _num : rest -> pure $ maker1 rest =<\< mkSignum ty
                _ -> Nothing
            ),
            ( '(Data.Ord.Linear.==),
              \case
                Plugins.Type ty : _eq : rest -> pure $ maker2 rest =<\< mkEqual ty
                _ -> Nothing
            ),
            ( '(Data.Ord.Linear./=),
              \case
                Plugins.Type ty : _eq : rest -> pure $ maker2 rest =<\< mkNotEqual ty
                _ -> Nothing
            ),
            ( '(Data.Ord.Linear.<),
              \case
                Plugins.Type ty : _ord : rest -> pure $ maker2 rest =<\< mkLT ty
                _ -> Nothing
            ),
            ( '(Data.Ord.Linear.<=),
              \case
                Plugins.Type ty : _ord : rest -> pure $ maker2 rest =<\< mkLE ty
                _ -> Nothing
            ),
            ( '(Data.Ord.Linear.>),
              \case
                Plugins.Type ty : _ord : rest -> pure $ maker2 rest =<\< mkGT ty
                _ -> Nothing
            ),
            ( '(Data.Ord.Linear.>=),
              \case
                Plugins.Type ty : _ord : rest -> pure $ maker2 rest =<\< mkGE ty
                _ -> Nothing
            ),
            ( 'Data.Ord.Linear.compare,
              \case
                Plugins.Type ty : _ord : rest -> pure $ maker2 rest =<\< mkCompare ty
                _ -> Nothing
            ),
            ( 'Data.Ord.Linear.max,
              \case
                Plugins.Type ty : _dupable : _ord : rest -> pure $ maker2 rest =<\< mkMax ty
                _ -> Nothing
            ),
            ( 'Data.Ord.Linear.min,
              \case
                Plugins.Type ty : _dupable : _ord : rest -> pure $ maker2 rest =<\< mkMin ty
                _ -> Nothing
            ),
            ( '(Data.Replicator.Linear.<*>),
              \case
                Plugins.Type a : Plugins.Type b : rest -> do
                  replicator <- Map.lookup ''Data.Replicator.Linear.Replicator (tyConLookup symLookup)
                  pure $ maker2 rest =<\< mkAp (Plugins.mkTyConTy replicator) a b
                _ -> Nothing
            ),
            ( 'Data.Replicator.Linear.map,
              \case
                Plugins.Type a : Plugins.Type b : u : rest -> do
                  replicator <- Map.lookup ''Data.Replicator.Linear.Replicator (tyConLookup symLookup)
                  pure $ mkMap' (Plugins.mkTyConTy replicator) a b u rest
                _ -> Nothing
            ),
            ( 'Data.Replicator.Linear.pure,
              \case
                Plugins.Type a : rest -> do
                  replicator <- Map.lookup ''Data.Replicator.Linear.Replicator (tyConLookup symLookup)
                  pure $ maker1 rest =<\< mkPoint (Plugins.mkTyConTy replicator) a
                _ -> Nothing
            ),
            ( 'Data.Tuple.Linear.curry,
              \case
                Plugins.Type a1 : Plugins.Type a2 : Plugins.Type b : Plugins.Type _q : Plugins.Type _q' : u : rest ->
                  -- from: (\n -> curry {{u}}) :: n -> a1 -> a2 -> b
                  -- to:   curry (curry (uncurry (categorify n {{u}}) . assoc))
                  --         :: n `k` (a1 -> a2 -> b)
                  pure . joinD $
                    applyEnriched rest
                      <$> mkCurry (nameTuple a1) a2 b
                        <*\> mkId (nameTuple a1)
                        <*\> sequenceA
                          [ joinD $
                              composeCat m
                                <$> (uncurryCat m =<\< categorifyLambda u)
                                  <*\> mkRAssoc (Plugins.varType n) a1 a2
                          ]
                _ -> Nothing
            ),
            ( 'Data.Tuple.Linear.fst,
              \case
                Plugins.Type fTy : Plugins.Type sTy : _consumable : rest ->
                  pure $ maker1 rest =<\< mkExl fTy sTy
                _ -> Nothing
            ),
            ( 'Data.Tuple.Linear.snd,
              \case
                Plugins.Type fTy : Plugins.Type sTy : _consumable : rest ->
                  pure $ maker1 rest =<\< mkExr fTy sTy
                _ -> Nothing
            ),
            ( 'Data.Tuple.Linear.swap,
              \case
                Plugins.Type a : Plugins.Type b : rest -> pure $ maker1 rest =<\< mkSwap a b
                _ -> Nothing
            ),
            ( 'Data.Tuple.Linear.uncurry,
              \case
                Plugins.Type a1
                  : Plugins.Type a2
                  : Plugins.Type b
                  : Plugins.Type _q
                  : Plugins.Type _q'
                  : u
                  : rest ->
                    -- from: (\n -> uncurry {{u}}) :: n -> (a1, a2) -> b
                    -- to:   curry (uncurry (uncurry (categorify n {{u}})) . unassoc)
                    --         :: n `k` ((a1, a2) -> b)
                    pure . joinD $
                      applyEnriched' [u] rest
                        <$> mkUncurry (nameTuple a1) a2 b
                          <*\> mkLAssoc (Plugins.varType n) a1 a2
                _ -> Nothing
            ),
            ( '(Data.V.Linear.<*>),
              \case
                Plugins.Type n' : Plugins.Type a : Plugins.Type b : rest -> do
                  v <- Map.lookup ''Data.V.Linear.V (tyConLookup symLookup)
                  pure $ maker2 rest =<\< mkAp (Plugins.mkTyConApp v [n']) a b
                _ -> Nothing
            ),
            ( 'Data.V.Linear.map,
              \case
                Plugins.Type a : Plugins.Type b : Plugins.Type n' : u : rest -> do
                  v <- Map.lookup ''Data.V.Linear.V (tyConLookup symLookup)
                  pure $ mkMap' (Plugins.mkTyConApp v [n']) a b u rest
                _ -> Nothing
            ),
            ( 'Data.V.Linear.pure,
              \case
                Plugins.Type n' : Plugins.Type a : _knownNat : rest -> do
                  v <- Map.lookup ''Data.V.Linear.V (tyConLookup symLookup)
                  pure $ maker1 rest =<\< mkPoint (Plugins.mkTyConApp v [n']) a
                _ -> Nothing
            ),
            ( '(Prelude.Linear.$),
              \case
                Plugins.Type _rep : Plugins.Type a : Plugins.Type b : Plugins.Type _q : Plugins.Type _q' : rest ->
                  pure $ maker2 rest =<\< mkApply a b
                _ -> Nothing
            ),
            ( '(Prelude.Linear..),
              \case
                Plugins.Type _rep
                  : Plugins.Type b
                  : Plugins.Type c
                  : Plugins.Type a
                  : Plugins.Type _q
                  : Plugins.Type _q'
                  : Plugins.Type _q''
                  : f
                  : g
                  : rest ->
                    pure $
                      case mkCompose2 of
                        Nothing ->
                          -- from: (\n -> {{u}} . {{v}}) :: n -> a -> c
                          -- to:   curry
                          --         (compose
                          --           (uncurry (categorify n {{u}}))
                          --           (exl &&& uncurry (categorify n {{v}})))
                          --         :: n `k` (a -> c)
                          joinD $
                            applyEnriched rest
                              <$> mkCompose (nameTuple a) (nameTuple b) c
                                <*\> mkId (nameTuple a)
                                <*\> sequenceA
                                  [ uncurryCat m =<\< categorifyLambda f,
                                    joinD $
                                      forkCat m
                                        <$> mkExl (Plugins.varType n) a
                                          <*\> (uncurryCat m =<\< categorifyLambda g)
                                  ]
                        Just fn ->
                          handleExtraArgs rest
                            =<\< joinD
                              ( fn (Plugins.varType n) b c a
                                  <$> categorifyLambda f
                                    <*\> categorifyLambda g
                              )
                _ -> Nothing
            ),
            ( 'Prelude.Linear.const,
              \case
                Plugins.Type _b : Plugins.Type a : Plugins.Type _q : u : rest ->
                  -- __NB__: this doesn't use `applyEnriched` because @u@ isn't a function.
                  --
                  -- from: (\n -> const {{u}}) :: n -> a -> b
                  -- to:   curry (categorify n {{u}} . exl) :: n `k` (a -> b)
                  pure . (handleExtraArgs rest <=\< curryCat m <=\< joinD) $
                    composeCat m <$> categorifyLambda u <*\> mkExl (Plugins.varType n) a
                _ -> Nothing
            ),
            ( 'Prelude.Linear.id,
              \case
                Plugins.Type ty : Plugins.Type _q : rest -> pure $ maker1 rest =<\< mkId ty
                _ -> Nothing
            ),
            ( 'Unsafe.Linear.coerce,
              \case
                Plugins.Type from : Plugins.Type to : rest ->
                  pure $ maker1 rest =<\< mkCoerce from to
                _ -> Nothing
            )
          ]
      maker1 = makeMaker1 m categorifyLambda
      maker2 = makeMaker2 m categorifyLambda expr
      handleExtraArgs = handleAdditionalArgs m categorifyLambda
      -- from: (\n -> liftA2 {{u}}) :: n -> f a -> f b -> f c
      -- to:   curry (curry (liftA2 (uncurry (uncurry (categorify n {{u}})))) . strength) ::
      --         n `k` (f a -> f b -> f c)
      mkLiftA2' f a b c u rest =
        handleExtraArgs rest
          =<\< curryCat m
          =<\< joinD
            ( composeCat m
                <$> ( curryCat m
                        =<\< ( Plugins.App
                                 <$> mkLiftA2 f (nameTuple a) b c
                                   <*\> ( uncurryCat m
                                            =<\< uncurryCat m
                                            =<\< categorifyLambda u
                                        )
                             )
                    )
                  <*\> mkStrength f (Plugins.varType n) a
            )
      -- from: (\n -> fmap {{u}}) :: n -> f a -> f b
      -- to:   curry (fmap (uncurry (categorifyLambda n {{u}})) . strength) ::
      --         n `k` (f a -> f b)
      mkMap' f a b u rest =
        joinD $
          applyEnriched' [u] rest
            <$> mkMap f (nameTuple a) b
              <*\> mkStrength f (Plugins.varType n) a
      applyEnriched = applyEnrichedCat m categorifyLambda
      applyEnriched' = applyEnrichedCat' m categorifyLambda
      nameTuple = makeTupleTyWithVar n
