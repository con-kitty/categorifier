{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Categorifier.Core.MakerMap
  ( MakerMap,
    MakerMapFun,
    combineMakerMapFuns,
    baseMakerMapFun,
    adjunctionsMakerMapFun,

    -- * Helper functions for combining categorified expressions
    applyCat,
    composeCat,
    forkCat,
    curryCat,
    uncurryCat,

    -- * Other helper functions
    applyEnrichedCat,
    applyEnrichedCat',
    handleAdditionalArgs,
    isHeadVarId,
    makeMaker1,
    makeMaker2,
    makeTupleTyWithVar,
    splitNameString,
  )
where

import qualified Categorifier.Core.Functions
import Categorifier.Core.Makers (Makers (..), extract2TypeArgs, getMorphismType)
import Categorifier.Core.Types (CategoricalFailure (..), CategoryStack)
import Categorifier.Duoidal (joinD, traverseD, (<*\>), (<=\<), (=<\<))
import Categorifier.Hierarchy (properFunTy)
import qualified Control.Arrow
import qualified Control.Category
import Control.Monad.Trans.Except (throwE)
import CoreArity (etaExpand)
import Data.Bitraversable (bitraverse)
import qualified Data.Bool
import qualified Data.Coerce
import qualified Data.Either
import Data.Foldable (foldlM)
import qualified Data.Foldable
import qualified Data.Function
import qualified Data.Functor
import qualified Data.Functor.Rep
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Traversable
import qualified Data.Tuple
import qualified GHC.Base
import qualified GHC.Classes
import qualified GHC.Err
import qualified GHC.Float
import qualified GHC.Num
import qualified GHC.Real
import qualified GHC.Word
import qualified GhcPlugins as Plugins
import qualified Language.Haskell.TH as TH
import qualified TysWiredIn
import qualified Unsafe.Coerce

-- For Unsafe.Coerce
{-# ANN module ("HLint: ignore Avoid restricted module" :: String) #-}

-- | A map of functions for interpreting names.
--
-- The key is the name, and the value is a function that takes the arguments applied
-- to that name, and returns @Just@ if the application is successfully interpreted into
-- a `Plugins.CoreExpr` in the target category, otherwise @Nothing@.
--
-- When a name is encountered that is not in the map, or the interpreting function
-- returns @Nothing@, the name will be inlined.
type MakerMap = Map TH.Name ([Plugins.CoreExpr] -> Maybe (CategoryStack Plugins.CoreExpr))

-- | Extending support in the source language ... if you have operations that should map more
--   directly than simply being inlined.
type MakerMapFun =
  Plugins.DynFlags ->
  Makers ->
  -- | Lambda-bound var
  Plugins.Var ->
  -- | The var being interpreted
  Plugins.Var ->
  -- | The expression where the var being interpreted is the head
  Plugins.CoreExpr ->
  -- | Target category
  Plugins.Type ->
  -- | The name of the var being interpreted
  String ->
  -- | The args applied to the var being interpreted
  [Plugins.CoreExpr] ->
  -- | The module of the var being interpreted
  String ->
  -- | A function for categorifying functions
  (Plugins.CoreExpr -> CategoryStack Plugins.CoreExpr) ->
  -- | A function for categorifying lambdas
  (Plugins.CoreExpr -> CategoryStack Plugins.CoreExpr) ->
  MakerMap

-- | Right-based combination.
combineMakerMapFuns :: [MakerMapFun] -> MakerMapFun
combineMakerMapFuns fs dflags m n target expr cat var args modu catFun catLambda =
  Map.unionsWith (\_ x -> x) maps
  where
    maps = fmap (\f -> f dflags m n target expr cat var args modu catFun catLambda) fs

adjunctionsMakerMapFun :: MakerMapFun
adjunctionsMakerMapFun
  dflags
  m@Makers {..}
  n
  target
  expr
  cat
  var
  args
  modu
  categorifyFun
  categorifyLambda =
    Map.fromListWith
      const
      [ ( 'Data.Functor.Rep.apRep,
          \case
            f : a : b : representable : rest ->
              ($ (f : representable : a : b : rest)) =<< Map.lookup '(GHC.Base.<*>) baseMakerMap
            _ -> Nothing
        ),
        ( 'Data.Functor.Rep.bindRep,
          \case
            f : a : b : representable : rest ->
              ($ (f : representable : a : b : rest)) =<< Map.lookup '(GHC.Base.>>=) baseMakerMap
            _ -> Nothing
        ),
        ( 'Data.Functor.Rep.fmapRep,
          \case
            f : a : b : representable : rest ->
              ($ (f : representable : a : b : rest)) =<< Map.lookup 'GHC.Base.fmap baseMakerMap
            _ -> Nothing
        ),
        ( 'Data.Functor.Rep.index,
          \case
            Plugins.Type f : _representable : Plugins.Type a : rest ->
              pure $ maker1 rest =<\< mkIndex f a
            _ -> Nothing
        ),
        ( 'Data.Functor.Rep.liftR2,
          \case
            f : a : b : c : representable : rest ->
              ($ (f : representable : a : b : c : rest))
                =<< Map.lookup 'GHC.Base.liftA2 baseMakerMap
            _ -> Nothing
        ),
        ( 'Data.Functor.Rep.pureRep,
          \case
            f : a : representable : rest ->
              ($ (f : representable : a : rest)) =<< Map.lookup 'GHC.Base.pure baseMakerMap
            _ -> Nothing
        ),
        ( 'Data.Functor.Rep.tabulate,
          \case
            Plugins.Type f : _representable : Plugins.Type a : rest ->
              pure $ maker1 rest =<\< mkTabulate f a
            _ -> Nothing
        )
      ]
    where
      baseMakerMap =
        baseMakerMapFun
          dflags
          m
          n
          target
          expr
          cat
          var
          args
          modu
          categorifyFun
          categorifyLambda
      maker1 = makeMaker1 m categorifyLambda

baseMakerMapFun :: MakerMapFun
baseMakerMapFun
  _dflags
  m@Makers {..}
  n
  _target
  expr
  _cat
  _var
  _args
  _modu
  categorifyFun
  categorifyLambda =
    makerMap
    where
      makerMap =
        Map.fromListWith
          const
          [ ( '(Control.Arrow.&&&),
              \case
                Plugins.Type c
                  : _arr
                  : Plugins.Type a
                  : Plugins.Type b1
                  : Plugins.Type b2
                  : u
                  : v
                  : rest
                    | Plugins.eqType c properFunTy ->
                        -- from: (\n -> {{u}} &&& {{v}}) :: n -> a -> (b1, b2)
                        -- to:   curry (uncurry (categorifyLambda n {{u}})
                        --         &&& uncurry (categorifyLambda n {{v}})) ::
                        --         n `k` (a -> (b1, b2))
                        pure . joinD $
                          applyEnriched' [u, v] rest
                            <$> mkFork (nameTuple a) b1 b2
                            <*\> mkId (nameTuple a)
                _ -> Nothing
            ),
            ( '(Control.Arrow.|||),
              \case
                Plugins.Type c : _arr : rest
                  | Plugins.eqType c properFunTy ->
                      ($ rest) =<< Map.lookup 'Data.Either.either makerMap
                _ -> Nothing
            ),
            ( 'Control.Arrow.arr,
              \case
                Plugins.Type c : _arrow : Plugins.Type _a : Plugins.Type _b : fn : rest
                  | Plugins.eqType c properFunTy -> pure $ maker1 rest =<\< categorifyFun fn
                _ -> Nothing
            ),
            ( '(Control.Category..),
              \case
                Plugins.Type _ : Plugins.Type k : _category : rest
                  | Plugins.eqType k properFunTy -> ($ rest) =<< Map.lookup '(GHC.Base..) makerMap
                _ -> Nothing
            ),
            ( 'Control.Category.id,
              \case
                Plugins.Type _ : Plugins.Type c : _category : rest
                  | Plugins.eqType c properFunTy -> ($ rest) =<< Map.lookup 'GHC.Base.id makerMap
                _ -> Nothing
            ),
            -- __TODO__: This is not partially-applicable enough
            ( 'Data.Bool.bool,
              \case
                [Plugins.Type a, false, true, test] ->
                  pure . joinD $
                    composeCat m
                      <$> mkIf a
                      <*\> joinD
                        ( forkCat m
                            <$> categorifyLambda test
                            <*\> joinD
                              ( forkCat m
                                  <$> categorifyLambda true
                                  <*\> categorifyLambda false
                              )
                        )
                _ -> Nothing
            ),
            ( 'Data.Coerce.coerce,
              \case
                Plugins.Type from : Plugins.Type to : _coercible : rest ->
                  pure $ maker1 rest =<\< mkCoerce from to
                _ -> Nothing
            ),
            ( 'Data.Either.either,
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
            ( 'Data.Foldable.maximum,
              \case
                Plugins.Type t : _foldable : Plugins.Type a : _ord : rest ->
                  pure $ maker1 rest =<\< mkMaximum t a
                _ -> Nothing
            ),
            ( 'Data.Foldable.minimum,
              \case
                Plugins.Type t : _foldable : Plugins.Type a : _ord : rest ->
                  pure $ maker1 rest =<\< mkMinimum t a
                _ -> Nothing
            ),
            ( 'Data.Function.fix,
              \case
                Plugins.Type a : u : rest ->
                  -- from: (\n -> fix {{u}}) :: n -> a
                  -- to:   fix (uncurry (categorifyLambda n {{u}})) :: n `k` a
                  pure $
                    handleExtraArgs rest
                      =<\< ( Plugins.App
                               <$> mkFix (Plugins.varType n) a
                               <*\> (uncurryCat m =<\< categorifyLambda u)
                           )
                _ -> Nothing
            ),
            ( '(Data.Functor.<$>),
              \case
                f : a : b : functor : rest ->
                  ($ (f : functor : a : b : rest)) =<< Map.lookup 'GHC.Base.fmap makerMap
                _ -> Nothing
            ),
            ( 'Data.Traversable.sequenceA,
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
            ( 'Data.Traversable.traverse,
              \case
                Plugins.Type t : _traversable : Plugins.Type f : Plugins.Type a : Plugins.Type b
                  : _applicative
                  : u
                  : rest -> pure $ mkTraverse' t f a b u rest
                _ -> Nothing
            ),
            ( 'Data.Tuple.curry,
              \case
                Plugins.Type a1 : Plugins.Type a2 : Plugins.Type b : u : rest ->
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
            ( 'Data.Tuple.fst,
              \case
                Plugins.Type fTy : Plugins.Type sTy : rest ->
                  pure $ maker1 rest =<\< mkExl fTy sTy
                _ -> Nothing
            ),
            ( 'Data.Tuple.snd,
              \case
                Plugins.Type fTy : Plugins.Type sTy : rest ->
                  pure $ maker1 rest =<\< mkExr fTy sTy
                _ -> Nothing
            ),
            ( 'Data.Tuple.swap,
              \case
                Plugins.Type a : Plugins.Type b : rest -> pure $ maker1 rest =<\< mkSwap a b
                _ -> Nothing
            ),
            ( 'Data.Tuple.uncurry,
              \case
                Plugins.Type a1 : Plugins.Type a2 : Plugins.Type b : u : rest ->
                  -- from: (\n -> uncurry {{u}}) :: n -> (a1, a2) -> b
                  -- to:   curry (uncurry (uncurry (categorify n {{u}})) . unassoc)
                  --         :: n `k` ((a1, a2) -> b)
                  pure . joinD $
                    applyEnriched' [u] rest
                      <$> mkUncurry (nameTuple a1) a2 b
                      <*\> mkLAssoc (Plugins.varType n) a1 a2
                _ -> Nothing
            ),
            ( '(GHC.Base.$),
              \case
                Plugins.Type _rep : Plugins.Type a : Plugins.Type b : rest ->
                  pure $ maker2 rest =<\< mkApply a b
                _ -> Nothing
            ),
            ( '(GHC.Base..),
              \case
                Plugins.Type b : Plugins.Type c : Plugins.Type a : f : g : rest -> pure $
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
                          ( fn (Plugins.varType n) b c a <$> categorifyLambda f
                              <*\> categorifyLambda g
                          )
                _ -> Nothing
            ),
            ( '(GHC.Base.<*>),
              \case
                Plugins.Type f : _ap : Plugins.Type a : Plugins.Type b : rest ->
                  pure $ maker2 rest =<\< mkAp f a b
                _ -> Nothing
            ),
            ( '(GHC.Base.>>=),
              \case
                Plugins.Type mty : _monad : Plugins.Type a : Plugins.Type b : rest ->
                  pure $ maker2 rest =<\< mkBind mty a b
                _ -> Nothing
            ),
            ( '(GHC.Base.<>),
              \case
                Plugins.Type a : _semigroup : rest -> pure $ maker2 rest =<\< mkAppend a
                _ -> Nothing
            ),
            ( '(GHC.Base.++),
              \case
                Plugins.Type a : rest ->
                  pure $ maker2 rest <=\< mkAppend $ Plugins.mkTyConApp Plugins.listTyCon [a]
                _ -> Nothing
            ),
            ('GHC.Base.ap, fromMaybe (const Nothing) $ Map.lookup '(GHC.Base.<*>) makerMap),
            ( 'GHC.Base.const,
              \case
                Plugins.Type _b : Plugins.Type a : u : rest ->
                  -- __NB__: this doesn't use `applyEnriched` because @u@ isn't a function.
                  --
                  -- from: (\n -> const {{u}}) :: n -> a -> b
                  -- to:   curry (categorify n {{u}} . exl) :: n `k` (a -> b)
                  pure $
                    handleExtraArgs rest <=\< curryCat m <=\< joinD $
                      composeCat m <$> categorifyLambda u <*\> mkExl (Plugins.varType n) a
                _ -> Nothing
            ),
            ( 'GHC.Base.fmap,
              \case
                Plugins.Type f : _functor : Plugins.Type a : Plugins.Type b : u : rest ->
                  pure $ mkMap' f a b u rest
                _ -> Nothing
            ),
            ( 'GHC.Base.id,
              \case
                Plugins.Type ty : rest -> pure $ maker1 rest =<\< mkId ty
                _ -> Nothing
            ),
            ( 'GHC.Base.liftA2,
              \case
                Plugins.Type f : _ap : Plugins.Type a : Plugins.Type b : Plugins.Type c
                  : u
                  : rest ->
                    pure $ mkLiftA2' f a b c u rest
                _ -> Nothing
            ),
            ( 'GHC.Base.map,
              \case
                Plugins.Type a : Plugins.Type b : u : rest ->
                  -- from: (\n -> map {{u}}) :: n -> [a] -> [b]
                  -- to:   curry (map (uncurry (categorifyLambda n {{u}})) . strength)
                  --         :: n `k` ([a] -> [b])
                  let f = Plugins.mkTyConTy Plugins.listTyCon
                   in pure . joinD $
                        applyEnriched' [u] rest
                          <$> mkMap f (nameTuple a) b
                          <*\> mkStrength f (Plugins.varType n) a
                _ -> Nothing
            ),
            ('GHC.Base.mappend, fromMaybe (const Nothing) $ Map.lookup '(GHC.Base.<>) makerMap),
            ( 'GHC.Base.pure,
              \case
                Plugins.Type f : _applicative : Plugins.Type a : rest ->
                  pure $ maker1 rest =<\< mkPoint f a
                _ -> Nothing
            ),
            ('GHC.Base.return, fromMaybe (const Nothing) $ Map.lookup 'GHC.Base.pure makerMap),
            ( '(GHC.Classes.==),
              \case
                Plugins.Type ty : _eq : rest -> pure $ maker2 rest =<\< mkEqual ty
                _ -> Nothing
            ),
            ( '(GHC.Classes./=),
              \case
                Plugins.Type ty : _eq : rest -> pure $ maker2 rest =<\< mkNotEqual ty
                _ -> Nothing
            ),
            ( '(GHC.Classes.<),
              \case
                Plugins.Type ty : _ord : rest -> pure $ maker2 rest =<\< mkLT ty
                _ -> Nothing
            ),
            ( '(GHC.Classes.<=),
              \case
                Plugins.Type ty : _ord : rest -> pure $ maker2 rest =<\< mkLE ty
                _ -> Nothing
            ),
            ( '(GHC.Classes.>),
              \case
                Plugins.Type ty : _ord : rest -> pure $ maker2 rest =<\< mkGT ty
                _ -> Nothing
            ),
            ( '(GHC.Classes.>=),
              \case
                Plugins.Type ty : _ord : rest -> pure $ maker2 rest =<\< mkGE ty
                _ -> Nothing
            ),
            ('(GHC.Classes.&&), \rest -> pure $ maker2 rest =<\< mkAnd),
            ('(GHC.Classes.||), \rest -> pure $ maker2 rest =<\< mkOr),
            ( 'GHC.Classes.compare,
              \case
                Plugins.Type ty : _ord : rest -> pure $ maker2 rest =<\< mkCompare ty
                _ -> Nothing
            ),
            ('GHC.Classes.eqDouble, \rest -> pure $ maker2 rest =<\< mkEqual Plugins.doubleTy),
            ( 'GHC.Classes.max,
              \case
                Plugins.Type ty : _ord : rest -> pure $ maker2 rest =<\< mkMax ty
                _ -> Nothing
            ),
            ( 'GHC.Classes.min,
              \case
                Plugins.Type ty : _ord : rest -> pure $ maker2 rest =<\< mkMin ty
                _ -> Nothing
            ),
            ('GHC.Classes.not, \rest -> pure $ maker1 rest =<\< mkNot),
            -- We currently ignore the `CallStack`, since `error` doesn't use `throw`, but instead
            -- calls `raise#` directly for some reason (which is too low-level for us to handle).
            ( 'GHC.Err.error,
              \case
                Plugins.Type _rep : Plugins.Type b : _callStack : rest ->
                  let str =
                        Plugins.mkAppTy
                          (Plugins.mkTyConTy Plugins.listTyCon)
                          (Plugins.mkTyConTy Plugins.charTyCon)
                   in pure $ maker1 rest =<\< mkBottom str b
                _ -> Nothing
            ),
            ( '(GHC.Float.**),
              \case
                Plugins.Type a : _floating : rest -> pure $ maker2 rest =<\< mkPow a
                _ -> Nothing
            ),
            ( 'GHC.Float.acos,
              \case
                Plugins.Type a : _floating : rest -> pure $ maker1 rest =<\< mkACos a
                _ -> Nothing
            ),
            ( 'GHC.Float.acosh,
              \case
                Plugins.Type a : _floating : rest -> pure $ maker1 rest =<\< mkACosh a
                _ -> Nothing
            ),
            ( 'GHC.Float.asin,
              \case
                Plugins.Type a : _floating : rest -> pure $ maker1 rest =<\< mkASin a
                _ -> Nothing
            ),
            ( 'GHC.Float.asinh,
              \case
                Plugins.Type a : _floating : rest -> pure $ maker1 rest =<\< mkASinh a
                _ -> Nothing
            ),
            ( 'GHC.Float.atan,
              \case
                Plugins.Type a : _floating : rest -> pure $ maker1 rest =<\< mkATan a
                _ -> Nothing
            ),
            ( 'GHC.Float.atan2,
              \case
                Plugins.Type a : _realFloat : rest -> pure $ maker2 rest =<\< mkArcTan2 a
                _ -> Nothing
            ),
            ( 'GHC.Float.atanh,
              \case
                Plugins.Type a : _floating : rest -> pure $ maker1 rest =<\< mkATanh a
                _ -> Nothing
            ),
            ( 'GHC.Float.cos,
              \case
                Plugins.Type a : _floating : rest -> pure $ maker1 rest =<\< mkCos a
                _ -> Nothing
            ),
            ( 'GHC.Float.cosh,
              \case
                Plugins.Type a : _floating : rest -> pure $ maker1 rest =<\< mkCosh a
                _ -> Nothing
            ),
            ('GHC.Float.divideDouble, \rest -> pure $ maker2 rest =<\< mkDivide Plugins.doubleTy),
            ('GHC.Float.double2Float, \rest -> pure $ maker1 rest =<\< mkDoubleToFloat),
            ( 'GHC.Float.exp,
              \case
                Plugins.Type a : _floating : rest -> pure $ maker1 rest =<\< mkExp a
                _ -> Nothing
            ),
            ('GHC.Float.fabsDouble, \rest -> pure $ maker1 rest =<\< mkAbs Plugins.doubleTy),
            ('GHC.Float.float2Double, \rest -> pure $ maker1 rest =<\< mkFloatToDouble),
            ( 'GHC.Float.isDenormalized,
              \case
                Plugins.Type a : _realFloat : rest -> pure $ maker1 rest =<\< mkFPIsDenormal a
                _ -> Nothing
            ),
            ( 'GHC.Float.isInfinite,
              \case
                Plugins.Type a : _realFloat : rest -> pure $ maker1 rest =<\< mkFPIsInfinite a
                _ -> Nothing
            ),
            ( 'GHC.Float.isNaN,
              \case
                Plugins.Type a : _realFloat : rest -> pure $ maker1 rest =<\< mkFPIsNaN a
                _ -> Nothing
            ),
            ( 'GHC.Float.isNegativeZero,
              \case
                Plugins.Type a : _realFloat : rest -> pure $ maker1 rest =<\< mkFPIsNegativeZero a
                _ -> Nothing
            ),
            ( 'GHC.Float.log,
              \case
                Plugins.Type a : _floating : rest -> pure $ maker1 rest =<\< mkLog a
                _ -> Nothing
            ),
            ('GHC.Float.negateDouble, \rest -> pure $ maker1 rest =<\< mkNegate Plugins.doubleTy),
            ('GHC.Float.minusDouble, \rest -> pure $ maker2 rest =<\< mkMinus Plugins.doubleTy),
            ('GHC.Float.plusDouble, \rest -> pure $ maker2 rest =<\< mkPlus Plugins.doubleTy),
            ( 'GHC.Float.sin,
              \case
                Plugins.Type a : _floating : rest -> pure $ maker1 rest =<\< mkSin a
                _ -> Nothing
            ),
            ( 'GHC.Float.sinh,
              \case
                Plugins.Type a : _floating : rest -> pure $ maker1 rest =<\< mkSinh a
                _ -> Nothing
            ),
            ( 'GHC.Float.sqrt,
              \case
                Plugins.Type a : _floating : rest -> pure $ maker1 rest =<\< mkSqrt a
                _ -> Nothing
            ),
            ('GHC.Float.sqrtDouble, \rest -> pure $ maker1 rest =<\< mkSqrt Plugins.doubleTy),
            ( 'GHC.Float.tan,
              \case
                Plugins.Type a : _floating : rest -> pure $ maker1 rest =<\< mkTan a
                _ -> Nothing
            ),
            ( 'GHC.Float.tanh,
              \case
                Plugins.Type a : _floating : rest -> pure $ maker1 rest =<\< mkTanh a
                _ -> Nothing
            ),
            ('GHC.Float.timesDouble, \rest -> pure $ maker2 rest =<\< mkTimes Plugins.doubleTy),
            ( '(GHC.Num.+),
              \case
                Plugins.Type ty : _num : rest -> pure $ maker2 rest =<\< mkPlus ty
                _ -> Nothing
            ),
            -- TODO: Catch cases on Natural/Word to turn into Monus
            ( '(GHC.Num.-),
              \case
                Plugins.Type ty : _num : rest -> pure $ maker2 rest =<\< mkMinus ty
                _ -> Nothing
            ),
            ( '(GHC.Num.*),
              \case
                Plugins.Type ty : _num : rest -> pure $ maker2 rest =<\< mkTimes ty
                _ -> Nothing
            ),
            ( 'GHC.Num.abs,
              \case
                Plugins.Type ty : _num : rest -> pure $ maker1 rest =<\< mkAbs ty
                _ -> Nothing
            ),
            ( 'GHC.Num.fromInteger,
              \case
                Plugins.Type ty : _num : rest -> pure $ maker1 rest =<\< mkFromInteger ty
                _ -> Nothing
            ),
            ( 'GHC.Num.negate,
              \case
                Plugins.Type ty : _num : rest -> pure $ maker1 rest =<\< mkNegate ty
                _ -> Nothing
            ),
            ( 'GHC.Num.signum,
              \case
                Plugins.Type ty : _num : rest -> pure $ maker1 rest =<\< mkSignum ty
                _ -> Nothing
            ),
            ( '(GHC.Real.^),
              \case
                Plugins.Type aTy : Plugins.Type bTy : _num : _integral : rest
                  | Plugins.eqType bTy Plugins.intTy -> pure $ maker2 rest =<\< mkPowInt aTy
                  | a : b : rest' <- rest -> pure $ maker1 (a : rest') =<\< mkPowI aTy bTy b
                _ -> Nothing
            ),
            ( '(GHC.Real./),
              \case
                Plugins.Type ty : _fractional : rest -> pure $ maker2 rest =<\< mkDivide ty
                _ -> Nothing
            ),
            ( 'GHC.Real.div,
              \case
                Plugins.Type ty : _integral : rest -> pure $ maker2 rest =<\< mkDiv ty
                _ -> Nothing
            ),
            ( 'GHC.Real.fromIntegral,
              \case
                Plugins.Type a : Plugins.Type b : _real : _frac : rest ->
                  pure $ maker1 rest =<\< mkFromIntegral a b
                _ -> Nothing
            ),
            ( 'GHC.Real.mod,
              \case
                Plugins.Type ty : _integral : rest -> pure $ maker2 rest =<\< mkMod ty
                _ -> Nothing
            ),
            ( 'GHC.Real.quot,
              \case
                Plugins.Type ty : _integral : rest -> pure $ maker2 rest =<\< mkQuot ty
                _ -> Nothing
            ),
            ( 'GHC.Real.realToFrac,
              \case
                Plugins.Type a : Plugins.Type b : _integral : _num : rest ->
                  pure $ maker1 rest =<\< mkRealToFrac a b
                _ -> Nothing
            ),
            ( 'GHC.Real.recip,
              \case
                Plugins.Type ty : _num : rest -> pure $ maker1 rest =<\< mkRecip ty
                _ -> Nothing
            ),
            ( 'GHC.Real.rem,
              \case
                Plugins.Type ty : _integral : rest -> pure $ maker2 rest =<\< mkRem ty
                _ -> Nothing
            ),
            ('GHC.Word.eqWord8, \rest -> pure $ maker2 rest =<\< mkEqual Plugins.word8Ty),
            ('GHC.Word.neWord8, \rest -> pure $ maker2 rest =<\< mkNotEqual Plugins.word8Ty),
            ( 'Categorifier.Core.Functions.abst,
              \case
                Plugins.Type a : _hasRep : rest -> pure $ maker1 rest =<\< mkAbstC a
                _ -> Nothing
            ),
            ( 'Categorifier.Core.Functions.repr,
              \case
                Plugins.Type a : _hasRep : rest -> pure $ maker1 rest =<\< mkReprC a
                _ -> Nothing
            ),
            ( 'Unsafe.Coerce.unsafeCoerce,
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
      -- from: (\n -> traverse {{u}}) :: n -> t a -> f (t b)
      -- to:   curry (traverse (uncurry (categorifyLambda n {{u}})) . strength) ::
      --         n `k` (t a -> f (t b))
      mkTraverse' t f a b u rest =
        joinD $
          applyEnriched' [u] rest
            <$> mkTraverse t f (nameTuple a) b
            <*\> mkStrength t (Plugins.varType n) a
      applyEnriched = applyEnrichedCat m categorifyLambda
      applyEnriched' = applyEnrichedCat' m categorifyLambda
      nameTuple = makeTupleTyWithVar n

applyCat ::
  Makers ->
  -- | cat x (a -> b)
  Plugins.CoreExpr ->
  -- | cat x a
  Plugins.CoreExpr ->
  -- | cat x b
  CategoryStack Plugins.CoreExpr
applyCat m f a = do
  let curriedTy = Plugins.exprType f
  (xTy, fTy) <-
    either (throwE . pure . NotEnoughTypeArgs "applyCat 1" f curriedTy) pure $
      extract2TypeArgs curriedTy
  (aTy, bTy) <-
    either (throwE . pure . NotEnoughTypeArgs "applyCat 2" f fTy) pure $
      extract2TypeArgs fTy
  maybe
    (joinD $ composeCat m <$> mkApply m aTy bTy <*\> forkCat m f a)
    (\fn -> fn xTy aTy bTy f a)
    (mkApply2 m)

composeCat ::
  Makers ->
  -- | cat b c
  Plugins.CoreExpr ->
  -- | cat a b
  Plugins.CoreExpr ->
  -- | cat a c
  CategoryStack Plugins.CoreExpr
composeCat m f g = do
  (b, c) <- getMorphismType f
  (a, _b') <- getMorphismType g
  -- TODO(ian): As we apply more rules for simplification, the rules should be
  --            organized in a separate function and managed by a structured map.
  --            See #25
  -- __FIXME__: This _should_ check @`Plugins.eqType` b b'@ and fail with `TypeMismatch`
  --            otherwise, but `Plugins.eqType` doesn't see through all synonyms somehow
  --            (maybe limited to associated types?), so it fails in some cases where the
  --            types /are/ equal.
  case (isHeadVarId f, isHeadVarId g) of
    (True, _) -> pure g
    (False, True) -> pure f
    (False, False) ->
      Plugins.mkCoreApps <$> mkCompose m a b c <*\> pure [f, g]

forkCat ::
  Makers ->
  -- | cat a b1
  Plugins.CoreExpr ->
  -- | cat a b2
  Plugins.CoreExpr ->
  -- | cat a (b1, b2)
  CategoryStack Plugins.CoreExpr
forkCat m f g = do
  (a, b1) <- getMorphismType f
  (_a', b2) <- getMorphismType g
  -- __FIXME__: This _should_ check @`Plugins.eqType` a a'@ and fail with `TypeMismatch`
  --            otherwise, but `Plugins.eqType` doesn't see through all synonyms somehow
  --            (maybe limited to associated types?), so it fails in some cases where the
  --            types /are/ equal.
  --            For example, @Product Hask@ and @(,)@ are equal, but their @eqType@ is
  --            False. This is the same issue described in @composeCat@.
  Plugins.mkCoreApps <$> mkFork m a b1 b2 <*\> pure [f, g]

curryCat :: Makers -> Plugins.CoreExpr -> CategoryStack Plugins.CoreExpr
curryCat m e = do
  let eTy = Plugins.exprType e
  ((a1, a2), b) <-
    either
      (throwE . pure . NotEnoughTypeArgs "curryCat" e eTy)
      pure
      (bitraverse extract2TypeArgs pure =<< extract2TypeArgs eTy)
  Plugins.App <$> mkCurry m a1 a2 b <*\> pure e

uncurryCat :: Makers -> Plugins.CoreExpr -> CategoryStack Plugins.CoreExpr
uncurryCat m e = do
  let eTy = Plugins.exprType e
  (a1, (a2, b)) <-
    either
      (throwE . pure . NotEnoughTypeArgs "uncurryCat" e eTy)
      pure
      (bitraverse pure extract2TypeArgs =<< extract2TypeArgs eTy)
  Plugins.App <$> mkUncurry m a1 a2 b <*\> pure e

-- | After fully applying an operation, there may be additional arguments applied to it. This
-- usually means that the result of the operation was a function itself, so continue to apply
-- the remaining arguments.
handleAdditionalArgs ::
  Makers ->
  (Plugins.CoreExpr -> CategoryStack Plugins.CoreExpr) ->
  [Plugins.CoreExpr] ->
  Plugins.CoreExpr ->
  CategoryStack Plugins.CoreExpr
handleAdditionalArgs m categorifyLambda xs core =
  foldlM (\x -> applyCat m x <=\< categorifyLambda) core xs

makeMaker1 ::
  Makers ->
  (Plugins.CoreExpr -> CategoryStack Plugins.CoreExpr) ->
  [Plugins.CoreExpr] ->
  Plugins.CoreExpr ->
  CategoryStack Plugins.CoreExpr
makeMaker1 m categorifyLambda rest op =
  case rest of
    [] -> pure op
    (v : xs) ->
      handleAdditionalArgs m categorifyLambda xs =<\< composeCat m op =<\< categorifyLambda v

makeMaker2 ::
  Makers ->
  (Plugins.CoreExpr -> CategoryStack Plugins.CoreExpr) ->
  Plugins.CoreExpr ->
  [Plugins.CoreExpr] ->
  Plugins.CoreExpr ->
  CategoryStack Plugins.CoreExpr
makeMaker2 m categorifyLambda e rest op =
  case rest of
    [] -> curryCat m op
    [_] -> categorifyLambda $ etaExpand 1 e -- NON-INDUCTIVE
    (u : v : xs) ->
      handleAdditionalArgs m categorifyLambda xs
        =<\< composeCat m op
        =<\< joinD (forkCat m <$> categorifyLambda u <*\> categorifyLambda v)

-- | The general pattern for applying a morphism in the enriching category. There is an
-- example at the very end of §8 in the paper:
--
-- > (λx → U ▽ V) ≡ curry ((uncurry (λx → U) ▽ uncurry (λx → V)) ◦ distl)
--
-- It's not spelled out there, but this can be generalized to cover a number of cases, as
--
-- > curry ($op (uncurry (λx → U)) ...) ◦ $dist)
--
-- where @$op@ is the operation we're mapping to the target category, @...@ means that any
-- number of arguments can be applied, and @$dist@ is /some/ distribing operation that
-- relates to it. As above the @$dist@ for @▽@ is @distl@. Others are @strength@ for @fmap@
-- and @leftAssoc@ for @uncurry@. There are some cases where the @uncurry@ needs to be
-- handled differently, however.
--
-- __NB__: The provided @morphs@ should be pre-categorified, since in some cases (e.g.,
--         @compose@) they need to be processed a bit more than this function does. see
--        `applyEnrichedCat'` for a helper.
applyEnrichedCat ::
  Makers ->
  (Plugins.CoreExpr -> CategoryStack Plugins.CoreExpr) ->
  [Plugins.CoreExpr] ->
  Plugins.CoreExpr ->
  Plugins.CoreExpr ->
  [Plugins.CoreExpr] ->
  CategoryStack Plugins.CoreExpr
applyEnrichedCat m categorifyLambda rest op dist morphs =
  handleAdditionalArgs m categorifyLambda rest
    =<\< curryCat m
    =<\< composeCat m (Plugins.mkCoreApps op morphs) dist

applyEnrichedCat' ::
  Makers ->
  (Plugins.CoreExpr -> CategoryStack Plugins.CoreExpr) ->
  [Plugins.CoreExpr] ->
  [Plugins.CoreExpr] ->
  Plugins.CoreExpr ->
  Plugins.CoreExpr ->
  CategoryStack Plugins.CoreExpr
applyEnrichedCat' m categorifyLambda morphs rest op dist =
  applyEnrichedCat m categorifyLambda rest op dist
    =<\< traverseD (uncurryCat m <=\< categorifyLambda) morphs

makeTupleTyWithVar :: Plugins.Var -> Plugins.Type -> Plugins.Type
makeTupleTyWithVar n a = TysWiredIn.mkBoxedTupleTy [Plugins.varType n, a]

-- TODO(ian): The check whether the function is id or not should be generic.
--            A safer way to do this at this moment would be to use temporarily
--            made id function using @mkId@ and check both function and module name.
--            However, Such temporary creation incurs pretty high performance cost
--            in bulidDictionary. So we do this in an ad hoc way for now.
--            See #26
--
-- NOTE:      CoreExpr for id is always of the form
--            @id (Type:Cat) $fCategoryCat (TYPE:Double)@.
--            so we check if expression has 3 arguments and head is matched with
--            known names and modules for the identity.
isHeadVarId :: Plugins.CoreExpr -> Bool
isHeadVarId e =
  case Plugins.collectArgs e of
    (Plugins.Var ident, [Plugins.Type _cat, _category, Plugins.Type _a]) ->
      case splitNameString $ Plugins.getName ident of
        (Just modu, "id") ->
          modu
            `elem` [ "Categorifier.UnconCat",
                     "ConCat.Category",
                     "Control.Category",
                     "Haskerwaul.Semigroupoid"
                   ]
        (_, _) -> False
    (_, _) -> False

splitNameString :: Plugins.Name -> (Maybe String, String)
splitNameString name = (modu, occ)
  where
    modu = fmap (Plugins.moduleNameString . Plugins.moduleName) (Plugins.nameModule_maybe name)
    occ = Plugins.occNameString (Plugins.nameOccName name)
