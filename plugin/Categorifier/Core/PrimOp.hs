{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

-- | Primitive operation handling.
--
-- GHC's simplifier often replaces normal functions with equivalents using primitive operations and
-- unboxed variables.  To correctly translate these to the target category, we have to more or less
-- recover the original (boxed) expression they came from.
module Categorifier.Core.PrimOp
  ( -- * Replacement
    replace,
    Boxer (..),
    noAdditionalBoxers,

    -- ** Sanity-checking
    checkForUnboxedVars,

    -- ** Search for triggering primop replacement
    matchOnUniverse,
    matchBoxingApp,
    matchToIntegerApp,
    matchIntegerToIntApp,
    matchFloatFromIntegralApp,
    matchTagToEnumApp,

    -- * Constructor map table construction
    defaultConMap,
    mkConMap,
  )
where

import Categorifier.Core.Makers (Makers (..), isFreeIn)
import Categorifier.Core.Trace (addIdInfo, maybeTraceWith, renderSDoc)
import Categorifier.Core.Types (CategoricalFailure (..), CategoryStack)
import Categorifier.Duoidal (sequenceD, traverseD, (<*\>))
import qualified Categorifier.GHC.Builtin as Plugins
import qualified Categorifier.GHC.Core as Plugins
import qualified Categorifier.GHC.Data as Plugins
import qualified Categorifier.GHC.Driver as Plugins
import qualified Categorifier.GHC.Types as Plugins
import qualified Categorifier.GHC.Utils as Plugins
import Categorifier.Hierarchy
  ( BaseIdentifiers (..),
    GetTagInfo (..),
    IntConstructor (..),
    intConstructorToBoxer,
    intConstructorToOpTyPair,
  )
import Control.Applicative (Alternative (..))
import Control.Monad (when, (<=<))
import Control.Monad.Extra (whenJust)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (catchE, throwE)
import Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.Reader as Reader
import Data.Bifunctor (Bifunctor (..))
import Data.Foldable (asum, foldl', foldrM)
import Data.Generics.Uniplate.Data (universe, universeBi)
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Monoid (Any (..))
import qualified Data.Tuple as Tuple (swap)
import PyF (fmt)

-- Need Uniplate for traversals on GHC-provided recursive types
{-# ANN module ("HLint: ignore Avoid restricted module" :: String) #-}

data Boxer = Boxer
  { _modu :: String,
    _ident :: String,
    _argDataCons :: [Plugins.DataCon],
    _maker :: CategoryStack Plugins.CoreExpr
  }

type OpMap opType = Map opType [(([Plugins.TyCon], Plugins.TyCon), Boxer)]

type PrimOpMap = OpMap Plugins.PrimOp

-- |
--
--  __TODO__: This should perhaps eventually become `baseBoxers`, and have none hardcoded
noAdditionalBoxers ::
  Makers -> [(Plugins.CLabelString, (Boxer, [Plugins.Type], Plugins.Type))]
noAdditionalBoxers = const []

constructPrimOpMap :: Makers -> [IntConstructor] -> PrimOpMap
constructPrimOpMap makers intCons =
  Map.unionWith (<>) (mkFloatingPrimOps makers) $
    foldl' (\acc -> Map.unionWith (<>) acc . go) Map.empty intCons
  where
    go intCon =
      ( if intConstructorSigned intCon
          then mkIntPrimOps
          else mkWordPrimOps
      )
        makers
        (intConstructorTyCon intCon)
        (intConstructorDataCon intCon)

type NoInlinePrimFunctionMap = OpMap Plugins.Name

constructNoInlinePrimFunctionMap :: Makers -> [IntConstructor] -> NoInlinePrimFunctionMap
constructNoInlinePrimFunctionMap makers =
  foldl' (\acc -> Map.unionWith (<>) acc . go) Map.empty . filter intConstructorSigned
  where
    go intCon =
      mkNoInlinePrimIntFunctions
        makers
        (intConstructorTyCon intCon)
        (intConstructorDataCon intCon)

type DeleteOpMap = Map Plugins.PrimOp Plugins.Type

constructDeleteOpMap :: [IntConstructor] -> DeleteOpMap
constructDeleteOpMap =
  Map.fromListWith const . mapMaybe (fmap (fmap Plugins.mkTyConTy) . intConstructorToOpTyPair)

-- `Plugins.TyCon`s don't have an `Ord` instance, so we have to use `List.nub`.
{-# ANN ensureUnique ("HLint: ignore Avoid restricted function" :: String) #-}
ensureUnique :: Eq a => [a] -> Maybe a
ensureUnique elems = case List.nub elems of
  [x] -> pure x
  _ -> empty

fromIntegralNames :: [(Plugins.Name, Plugins.Type)]
fromIntegralNames =
  [ (Plugins.integerToDoubleName, Plugins.doubleTy),
    (Plugins.integerToFloatName, Plugins.floatTy)
  ]

-- | If the given primitive operation always corresponds to a fixed result type, return that type.
getInvariantResultType ::
  PrimOpMap ->
  Plugins.PrimOp ->
  Maybe Plugins.Type
getInvariantResultType opMap =
  fmap Plugins.mkTyConTy . ensureUnique . fmap (snd . fst) <=< flip Map.lookup opMap

-- | Given a primitive operation and an argument type, return the result type if possible.
--
-- See @deduceArgumentType@ for a note on the types involved with multi-argument primops.
deduceResultType ::
  PrimOpMap ->
  -- | Variable referring to the primop
  Plugins.Var ->
  -- | Argument type
  Plugins.Type ->
  Maybe Plugins.Type
deduceResultType opMap var argType = do
  primOp <- Plugins.isPrimOpId_maybe var
  getInvariantResultType opMap primOp <|> deduceResultType' opMap primOp argType

deduceResultType' ::
  PrimOpMap ->
  Plugins.PrimOp ->
  Plugins.Type ->
  Maybe Plugins.Type
deduceResultType' opMap primOp argType = do
  argTyCon <- Plugins.tyConAppTyCon_maybe argType
  ops <- Map.lookup primOp opMap
  Plugins.mkTyConTy <$> case filter ((argTyCon `elem`) . fst) $ fmap fst ops of
    [(_argTyCons, resultTyCon)] -> pure resultTyCon
    [] -> Nothing
    signatures -> ensureUnique $ fmap snd signatures

-- | Given a primitive operation and its result type, return the argument type if possible.
--
-- We don't plan to support any multi-argument primops that have differing argument types, so for
-- now, this function only returns a single type.  It could easily be generalized to return a list
-- of argument types if needed (e.g. if we need to start doing actual unification).  For now, if you
-- add a multi-argument primop with differing argument types, this function will return `Nothing`.
deduceArgumentType ::
  PrimOpMap ->
  -- | Variable referring to the primop
  Plugins.Var ->
  -- | Result type
  Plugins.Type ->
  Maybe Plugins.Type
deduceArgumentType opMap var resultType = do
  primOp <- Plugins.isPrimOpId_maybe var
  resultTyCon <- Plugins.tyConAppTyCon_maybe resultType
  ops <- Map.lookup primOp opMap
  fmap Plugins.mkTyConTy . ensureUnique
    =<< case filter ((resultTyCon ==) . snd) $ fmap fst ops of
      [(argTyCons, _resultTyCon)] -> pure argTyCons
      [] -> Nothing
      signatures -> ensureUnique $ fmap fst signatures

lookupPrimOp ::
  Plugins.DynFlags ->
  Bool ->
  PrimOpMap ->
  (Plugins.Var, [Plugins.Type], Plugins.Type) ->
  Maybe Boxer
lookupPrimOp dflags debug opMap (var, argTys, resTy) = do
  (argTyCons, resTyCon) <-
    (,)
      <$> traverse Plugins.tyConAppTyCon_maybe argTys
      <*> Plugins.tyConAppTyCon_maybe resTy
  lookupPrimOpTyCons dflags debug opMap (var, argTyCons, resTyCon)

isSupportedPrimOp :: PrimOpMap -> Plugins.Var -> Bool
isSupportedPrimOp opMap = maybe False (`Map.member` opMap) . Plugins.isPrimOpId_maybe

lookupPrimOpTyCons ::
  Plugins.DynFlags ->
  Bool ->
  PrimOpMap ->
  -- | Variable, arg types, result type
  (Plugins.Var, [Plugins.TyCon], Plugins.TyCon) ->
  Maybe Boxer
lookupPrimOpTyCons dflags debug opMap (var, argCons, resCon) =
  lookupOpTyCons dflags debug Plugins.isPrimOpId_maybe opMap (var, argCons, resCon)

lookupNoInlinePrimitive ::
  Plugins.DynFlags ->
  Bool ->
  NoInlinePrimFunctionMap ->
  (Plugins.Var, [Plugins.Type], Plugins.Type) ->
  Maybe Boxer
lookupNoInlinePrimitive dflags debug opMap (var, argTys, resTy) = do
  (argTyCons, resTyCon) <-
    (,)
      <$> traverse Plugins.tyConAppTyCon_maybe argTys
      <*> Plugins.tyConAppTyCon_maybe resTy
  lookupNoInlinePrimitiveTyCons dflags debug opMap (var, argTyCons, resTyCon)

lookupNoInlinePrimitiveTyCons ::
  Plugins.DynFlags ->
  Bool ->
  NoInlinePrimFunctionMap ->
  -- | Variable, arg types, result type
  (Plugins.Var, [Plugins.TyCon], Plugins.TyCon) ->
  Maybe Boxer
lookupNoInlinePrimitiveTyCons dflags debug opMap (var, argTyCons, resTyCon) =
  lookupOpTyCons dflags debug (pure . Plugins.varName) opMap (var, argTyCons, resTyCon)

lookupOpTyCons ::
  Ord opType =>
  Plugins.DynFlags ->
  Bool ->
  (Plugins.Var -> Maybe opType) ->
  OpMap opType ->
  (Plugins.Var, [Plugins.TyCon], Plugins.TyCon) ->
  Maybe Boxer
lookupOpTyCons dflags debug convVar opMap (var, argCons, resCon) =
  go
    <|> maybeTraceWith
      debug
      ( const
          [fmt|"lookupOpTyCons: boxed equivalent for
  {dbg var} :: {dbg $ Plugins.mkTyConTy <$> argCons} -> {dbg $ Plugins.mkTyConTy resCon}
not found.|]
      )
      Nothing
  where
    go =
      List.lookup (argCons, resCon) =<< flip Map.lookup opMap =<< convVar var

    dbg :: Plugins.Outputable a => a -> String
    dbg = renderSDoc dflags . Plugins.ppr

isNoInlinePrimitive :: NoInlinePrimFunctionMap -> Plugins.Var -> Bool
isNoInlinePrimitive funMap var = Map.member (Plugins.varName var) funMap

-- | @NOINLINE@ primitive functions are handled similarly to primops.
deduceResultTypeNIP ::
  NoInlinePrimFunctionMap ->
  Plugins.Var ->
  Plugins.Type ->
  Maybe Plugins.Type
deduceResultTypeNIP opMap var argType = do
  argTyCon <- Plugins.tyConAppTyCon_maybe argType
  ops <- Map.lookup (Plugins.varName var) opMap
  Plugins.mkTyConTy <$> case filter ((argTyCon `elem`) . fst) $ fmap fst ops of
    [(_argTyCons, resultTyCon)] -> pure resultTyCon
    [] -> Nothing
    signatures -> ensureUnique $ fmap snd signatures

-- | @NOINLINE@ primitive functions are handled similarly to primops.
deduceArgumentTypeNIP ::
  NoInlinePrimFunctionMap ->
  -- | Variable referring to the primop
  Plugins.Var ->
  -- | Result type
  Plugins.Type ->
  Maybe Plugins.Type
deduceArgumentTypeNIP opMap var resultType = do
  resultTyCon <- Plugins.tyConAppTyCon_maybe resultType
  ops <- Map.lookup (Plugins.varName var) opMap
  fmap Plugins.mkTyConTy . ensureUnique
    =<< case filter ((resultTyCon ==) . snd) $ fmap fst ops of
      [(argTyCons, _resultTyCon)] -> pure argTyCons
      [] -> Nothing
      signatures -> ensureUnique $ fmap fst signatures

-- | Produce a primitive constructor mapping, incorporating info about the fixed-size integer types
-- available in the provided `IntConstructor`s.
mkConMap :: [IntConstructor] -> [(Plugins.TyCon, Plugins.DataCon)]
mkConMap intCons = defaultConMap <> fmap intConstructorToBoxer intCons

-- | Default primitive constructor mapping (does not include fixed-size integer types).
defaultConMap :: [(Plugins.TyCon, Plugins.DataCon)]
defaultConMap =
  [ (Plugins.doubleTyCon, Plugins.doubleDataCon),
    (Plugins.floatTyCon, Plugins.floatDataCon),
    (Plugins.intTyCon, Plugins.intDataCon),
    (Plugins.boolTyCon, Plugins.intDataCon)
  ]

{- All the functions below here are related to primitive handling. -}

-- | `checkForUnboxedVars` is responsible for detecting unboxed variables after `replace` has run.
checkForUnboxedVars ::
  Plugins.CoreExpr ->
  Plugins.CoreExpr ->
  CategoryStack [()]
checkForUnboxedVars rhs =
  traverseD raise
    . filter (Plugins.isPrimitiveType . Plugins.varType)
    . mapMaybe matchVars
    . universe
  where
    raise :: Plugins.Var -> CategoryStack a
    raise = throwE . pure . flip BareUnboxedVar (addIdInfo rhs)

    matchVars = \case
      Plugins.Var v -> Just v
      _ -> Nothing

-- | This contains all the data we need to get from `Categorifier.Core.Categorify.categorifyLambda`.
--   It gets computed once in `replace` and reused across recursive calls in @replacePrimOps@.
data ReplacePrimOpsContext = ReplacePrimOpsContext
  { rpoCtxDynFlags :: Plugins.DynFlags,
    rpoCtxDebugPrint :: forall a. Plugins.Outputable a => a -> String,
    rpoCtxMakers :: Makers,
    rpoCtxShouldTrace :: Bool,
    rpoCtxIntConstructors :: [IntConstructor],
    rpoCtxBoxers :: [(Plugins.TyCon, Plugins.DataCon)],
    rpoCtxCCallBoxers :: [(Plugins.CLabelString, (Boxer, [Plugins.Type], Plugins.Type))],
    rpoCtxPrimOpMap :: PrimOpMap,
    rpoCtxNoInlinePrimFunctionmap :: NoInlinePrimFunctionMap,
    rpoCtxDeleteOpMap :: DeleteOpMap,
    rpoCtxFindBoxedPrim :: (Plugins.Var, [Plugins.Type], Plugins.Type) -> Maybe Boxer,
    rpoCtxGetTag :: GetTagInfo,
    rpoCtxGetUnique :: CategoryStack Plugins.Unique
  }

-- | Dual-purpose `Reader.Reader` monad. The context provides all the information available from
--  `Categorifier.Core.Categorify.categorifyLambda` that we use in replacement. The map lets us look
--   up the boxed replacements for unboxed variables as we come across them.
type PrimOpT = ReaderT (ReplacePrimOpsContext, Map Plugins.Var Plugins.Var)

-- | Replace all primitive operations, unboxed variables etc. in the given expression with their
-- standard boxed equivalents.
replace ::
  Plugins.DynFlags ->
  Bool ->
  Makers ->
  (Makers -> [(Plugins.CLabelString, (Boxer, [Plugins.Type], Plugins.Type))]) ->
  BaseIdentifiers ->
  CategoryStack Plugins.Unique ->
  Plugins.Type ->
  Plugins.CoreExpr ->
  CategoryStack Plugins.CoreExpr
replace dflags debug makers additionalBoxers baseIds getUnique resultType =
  flip Reader.runReaderT (ctx, mempty) . replacePrimOps (pure resultType)
  where
    ctx =
      ReplacePrimOpsContext
        { rpoCtxDynFlags = dflags,
          rpoCtxDebugPrint = renderSDoc dflags . Plugins.ppr,
          rpoCtxMakers = makers,
          rpoCtxShouldTrace = debug,
          rpoCtxIntConstructors = intCons,
          rpoCtxBoxers = mkConMap intCons,
          rpoCtxCCallBoxers = additionalBoxers makers,
          rpoCtxPrimOpMap = constructPrimOpMap makers intCons,
          rpoCtxNoInlinePrimFunctionmap = constructNoInlinePrimFunctionMap makers intCons,
          rpoCtxDeleteOpMap = constructDeleteOpMap intCons,
          rpoCtxFindBoxedPrim = \vs ->
            lookupPrimOp dflags debug (rpoCtxPrimOpMap ctx) vs
              <|> lookupNoInlinePrimitive dflags debug (rpoCtxNoInlinePrimFunctionmap ctx) vs,
          rpoCtxGetTag = getTagInfo,
          rpoCtxGetUnique = getUnique
        }
    BaseIdentifiers {baseIntConstructors = intCons, baseGetTag = getTagInfo} = baseIds

withNewBinding ::
  Plugins.Var ->
  Plugins.Var ->
  PrimOpT m a ->
  PrimOpT m a
withNewBinding unboxedBinder boxedBinder =
  Reader.local $ fmap (Map.insert unboxedBinder boxedBinder)

-- | This is only extracted in order to work with Ormolu's CPP support.
extractCallSpecTarget :: Plugins.IdDetails -> Maybe Plugins.CCallTarget
extractCallSpecTarget (Plugins.FCallId (Plugins.CCall (Plugins.CCallSpec target _ _))) = pure target
extractCallSpecTarget _ = Nothing

-- | @replacePrimOps@ is responsible for replacing occurrences of primitive operations with their
-- non-primitive counterparts.  Here are the situations in which this can occur, each of which needs
-- to be handled in its own way:
--
--  1. ordinary function application
--  2. inside the scrutinee of a case with only @DEFAULT@
--  3. as a C FFI call within the @State#@ monad
--  4. as a relational operation inside the scrutinee of a case with two branches
--
-- @replacePrimOps@ also accumulates a map of variable replacements when it processes
-- `case`-expressions.
--
-- === What is a primitive operation?
--
-- Currently we replace 3 kinds of operation:
--
--  * Actual compiler primops, `Plugins.PrimOp`
--
--  * C FFI calls to basic mathematical operations (e.g. @fmod@)
--
--  * Primitive-related functions which are defined but marked @NOINLINE@ by GHC (e.g. `modInt#`).
--
-- Unfortunately we must handle these all slightly differently from one another.
--
-- === Type deduction
--
-- We expect to always recover floating-point and boolean variables by type deduction.
--
-- Cases where we expect to be told the boxed result type:
--
--  - at the top level
--  - nested function applications (case 1)
--
-- Cases where we cannot expect to be told the boxed result type:
--
--  - when replacing the scrutinee of a case statement
--
-- WARNING: within @replacePrimOps@: please do not use `Plugins.mkCoreConApps` as recommended by
-- the GHC API docs!  @`Plugins.mkCoreConApps` `Plugins.doubleDataCon`@ will rewrite (for
-- example)
--
--    D# (-## x y)
--
-- into
--
--    case -## x y of w { __DEFAULT -> D# w }
--
-- essentially using this case in place of a lambda (to preserve the _CoreSyn let/app
-- invariant_).  We have logic below to handle this crazy move, but if you don't stick with
-- `Plugins.mkConApp`, you might cause an infinite loop.
replacePrimOps ::
  Maybe Plugins.Type ->
  Plugins.CoreExpr ->
  PrimOpT CategoryStack Plugins.CoreExpr
replacePrimOps resultType replaceExpr = do
  ( ReplacePrimOpsContext
      { rpoCtxDynFlags = dflags,
        rpoCtxShouldTrace = debug,
        rpoCtxDebugPrint = dbg,
        rpoCtxMakers = makers,
        rpoCtxBoxers = boxers,
        rpoCtxCCallBoxers = additionalBoxers,
        rpoCtxDeleteOpMap = deleteOpMap,
        rpoCtxPrimOpMap = primOpMap,
        rpoCtxNoInlinePrimFunctionmap = noInlinePrimFunctionMap,
        rpoCtxIntConstructors = intCons,
        rpoCtxGetTag = getTagInfo
      },
    replacements
    ) <-
    Reader.ask
  let rpoLabel :: (Plugins.Outputable a, Plugins.Outputable b) => String -> a -> b -> b
      rpoLabel which before =
        maybeTraceWith debug (\x -> [fmt|replacePrimOps {which}> {dbg before} ---> {dbg x}|])
  whenJust resultType $ \ty ->
    when (containsPrimitiveType ty) $
      lift . throwE . pure $ UnexpectedUnboxedType "replacePrimOps" ty replaceExpr
  maybeTraceWith debug (\x -> [fmt|replacePrimOps> {dbg x}|]) <$> case replaceExpr of
    -- Case 14. Enum comparisons
    --
    -- GHC will unbox enum representations via "tags" to compare them.  We will only encounter this
    -- if the comparison has been excessively specialized; in this case, our job is to
    -- undo this specialization.
    tagToEnumCase@(Plugins.collectArgs -> (Plugins.Var f, [Plugins.Type hopeIt'sBool, cmpExpr]))
      | hopeIt'sBool `Plugins.eqType` Plugins.boolTy,
        Just Plugins.TagToEnumOp <- Plugins.isPrimOpId_maybe f,
        (Plugins.Var mbCmpPrimOp, args) <- Plugins.collectArgs cmpExpr,
        -- Remove `getTag` calls from all arguments.
        Just [enumA, enumB] <- traverse (stripGetTag getTagInfo) args ->
          rpoLabel "14a" tagToEnumCase
            <$> reboxEnumCmp mbCmpPrimOp (Plugins.exprType enumA) enumA enumB
    -- Case 4. Gnarly comparison stuff
    --
    -- Here is the strategy for handling what GHC generates for comparisons:
    --
    --     a. Remove references to `tagToEnum#`, which is no longer needed once the primop is
    --        replaced
    --
    --     b. Replace the case on matches `__DEFAULT` and `1#` in the unboxed case-of-primop
    --        with a call to the `if` operation for the target category.
    --
    -- 4a. `tagToEnum#` deletion
    tagToEnumCase@(Plugins.App (Plugins.App (Plugins.Var f) (Plugins.Type ty)) unboxedRelation)
      | Just Plugins.TagToEnumOp <- Plugins.isPrimOpId_maybe f,
        pure Plugins.boolTyCon == Plugins.tyConAppTyCon_maybe ty ->
          rpoLabel "4a" tagToEnumCase <$> replacePrimOps (pure Plugins.boolTy) unboxedRelation
    appCase@(Plugins.App (Plugins.Var f) v)
      -- Here we remove integer narrowing operations, which are meaningless once the underlying
      -- operation is reboxed.
      --
      -- If we encounter a narrowing operation, this also gives us some useful type information
      -- about the resulting boxed type.
      | Just primOp <- Plugins.isPrimOpId_maybe f,
        Just toType <- Map.lookup primOp deleteOpMap ->
          rpoLabel "7a" appCase <$> replacePrimOps (pure toType) v
      -- Here we handle `fromInteger` by turning `doubleFromInteger` or some similar function
      -- into a boxed `fromInteger`.
      | Just boxedConvertedType <- lookup (Plugins.varName f) fromIntegralNames ->
          rpoLabel "7b" appCase <$> do
            arg <- replacePrimOps Nothing v
            let argType = Plugins.exprType arg
            op <-
              lift $
                if any (`Plugins.eqType` argType) $
                  fmap (Plugins.mkTyConTy . intConstructorTyCon) intCons
                  then -- We are dealing with a `fromIntegral` conversion
                    mkFromIntegral makers argType boxedConvertedType
                  else -- We are probably dealing with an integer literal of type `Integer` -- use
                  -- `fromInteger` and let it get processed at the top level
                    mkFromInteger makers boxedConvertedType
            pure $ Plugins.App op arg
      -- Here we replace primitive integer conversion with a direct `fromIntegral`
      | Plugins.varName f == Plugins.integerToIntName,
        (Plugins.Var toInt, args@[Plugins.Type fromType, _f, arg]) <- Plugins.collectArgs v,
        Plugins.varName toInt == Plugins.toIntegerName,
        Just toType <- resultType ->
          rpoLabel [fmt|"7c ({dbg args})|] appCase
            <$> ( Plugins.App
                    <$> lift (mkFromIntegral makers fromType toType)
                    <*\> replacePrimOps (pure fromType) arg
                )
    -- We're inside the case that unboxed this variable, so we can simply replace it with the
    -- original boxed variable.
    old@(Plugins.Var v)
      | Just v' <- Map.lookup v replacements -> pure . rpoLabel "6" old $ Plugins.Var v'
    -- Case 1. ordinary function application
    e
      -- This match detects occurrences of @`recip` @Double x@, which have been expanded by GHC
      -- into `1 /## x`, and replaces them with boxed `recip` calls.
      --
      -- So far this is the only place we have needed to handle literal floating-point values in
      -- any special way, so we handle `recip` as its own function.  If we need to generalize
      -- support to reboxing other floating-point literals, we could probably remove this and
      -- just let it become a boxed division operation via the existing function application
      -- support.
      | (Plugins.Var f, arguments) <- Plugins.collectArgs e,
        Just mbDiv <- Plugins.isPrimOpId_maybe f,
        Plugins.DoubleDivOp == mbDiv,
        [Plugins.Lit (Plugins.LitDouble 1), x] <- arguments -> do
          rcp <- lift (mkRecip makers Plugins.doubleTy)
          arg <- replacePrimOps (pure Plugins.doubleTy) x
          pure $ Plugins.mkCoreApps rcp [arg]
      -- This match detects occurrences of @`recip` @Float x@, which have been expanded by GHC
      -- into `1 /## x`, and replaces them with boxed `recip` calls.
      | (Plugins.Var f, arguments) <- Plugins.collectArgs e,
        Just mbDiv <- Plugins.isPrimOpId_maybe f,
        Plugins.FloatDivOp == mbDiv,
        [Plugins.Lit (Plugins.LitFloat 1), x] <- arguments -> do
          rcp <- lift (mkRecip makers Plugins.floatTy)
          arg <- replacePrimOps (pure Plugins.floatTy) x
          pure $ Plugins.mkCoreApps rcp [arg]
      | (Plugins.Var f, arguments) <- Plugins.collectArgs e,
        -- Make sure we've got some flavor of primop on our hands at least
        isSupportedPrimOp primOpMap f ->
          rpoLabel "1" e <$> replacePrimOpFunCall e resultType f arguments
      | (Plugins.Var f, arguments) <- Plugins.collectArgs e,
        -- Perhaps we are dealing with a GHC-provided primitive (unboxed) function marked @NOINLINE@
        -- in the compiler, like `modInt#`.
        isNoInlinePrimitive noInlinePrimFunctionMap f ->
          rpoLabel "1a" e <$> replaceNoInlinePrimFunctionCall e resultType f arguments
    -- All other case-expressions are handled here.
    caseExpr@(Plugins.Case sc bndr ty alts) -> do
      -- In general we don't know the equivalent boxed type without doing unification, but in
      -- common situations, we can work it out when we spot a situation wherein a boxing
      -- constructor is applied to the binder directly.
      --
      -- TODO(MP): this might benefit from a "retry"-type scenario like in standard application.
      let predictedBinderBoxedType = deduceEBTFrom boxers bndr $ universeBi alts
      -- We will inevitably need to use the boxed version of the scrutinee in the resulting
      -- expression, and furthermore, if we box it before continuing with any further logic, we
      -- can rely on the resulting equivalent boxed type!
      sc' <- replacePrimOps predictedBinderBoxedType sc
      let newApp b v = Plugins.Let (Plugins.NonRec b v)
      case Plugins.exprType sc' of
        newScTy
          -- This case looks for the GMP primitive `smallInteger`, which GHC uses for converting
          -- from some finite machine integral type to `Integer`, and removes it.
          -- @replacePrimOps@ will convert GHC's original conversion sequence of
          --
          --   Int64 -> Int64# -> Integer -> Double# -> Double
          --
          -- to
          --
          --   Int64 -> Double
          --
          -- and this case removes the second step in the original conversion.
          | [Plugins.Alt (Plugins.DataAlt con) [_unb] (Plugins.App (Plugins.Var si) _unbv)] <- alts,
            con `elem` fmap snd boxers,
            Plugins.varName si == Plugins.integerFromInt64Name ->
              pure sc
          -- This is a case statement that unboxes an existing variable.  Replace it with a
          -- `let`-binding, extend the replacement table and continue descending.
          | [Plugins.Alt (Plugins.DataAlt con) [unb] e] <- alts,
            con `elem` fmap snd boxers ->
              let -- The type of the binder does not change (it's already boxed if we are unboxing
                  -- in the sole alternative); we just reset the occurrence info before using it
                  -- to replace the unboxed thing.
                  bndr' = Plugins.setIdOccInfo bndr Plugins.noOccInfo
               in -- newReplacements = Map.insert unb bndr' replacements
                  fmap
                    ( maybeTraceWith
                        debug
                        ( \x ->
                            [fmt|replacePrimOps 5>
[ {dbg unb} : {dbg bndr'} :: {dbg (Plugins.varType bndr')} ]
{dbg caseExpr} ---> {dbg x}|]
                        )
                    )
                    . withNewBinding unb bndr'
                    $ newApp bndr' sc' <$> replacePrimOps resultType e
          -- 4b. Primitive case reboxing
          {- Rewrite

             ```
             case x ># 3# of bndr@
               1# -> trueExpr
               _ -> falseExpr
             ```

             into

             ```
             case x > 3 of bndr'@
               True -> trueExpr'
               False -> falseExpr'
             ```

             where `trueExpr' = replacePrimOps resultType trueExpr`. Same for `falseExpr'`.
          -}
          | Plugins.boolTy `Plugins.eqType` newScTy,
            [Plugins.Alt Plugins.DEFAULT [] falseExpr, Plugins.Alt (Plugins.LitAlt one) [] trueExpr] <- alts,
            Plugins.LitNumber 1 <- one ->
              fmap
                (rpoLabel "4b Bool" caseExpr)
                $ do
                  bndr' <- mkBoxedVarFrom "ccc_boxed_binder" newScTy bndr
                  withNewBinding bndr bndr' $
                    Plugins.Case sc' bndr' ty
                      <$> sequenceD
                        [ Plugins.Alt (Plugins.DataAlt Plugins.falseDataCon) []
                            <$> replacePrimOps resultType falseExpr,
                          Plugins.Alt (Plugins.DataAlt Plugins.trueDataCon) []
                            <$> replacePrimOps resultType trueExpr
                        ]
          {- Rewrite

             ```
             case x ># 3# of bndr@
               0# -> False
               _ -> True
             ```

             into `x > 3`.
          -}
          | [ Plugins.Alt Plugins.DEFAULT [] (Plugins.Var t),
              Plugins.Alt (Plugins.LitAlt zero) [] (Plugins.Var f)
              ] <-
              alts,
            Plugins.varType bndr `Plugins.eqType` Plugins.intPrimTy,
            ty `Plugins.eqType` Plugins.boolTy,
            Plugins.LitNumber 0 <- zero,
            Just tdc <- Plugins.isDataConId_maybe t,
            tdc == Plugins.trueDataCon,
            Just fdc <- Plugins.isDataConId_maybe f,
            fdc == Plugins.falseDataCon,
            newScTy `Plugins.eqType` Plugins.boolTy ->
              pure $ rpoLabel "4b Int->Bool" caseExpr sc'
          -- Case 3. C call with arguments of unboxed primitive type.
          --
          -- We have encountered a C call.  The actual call, including the `RealWorld#` token,
          -- will appear in the scrutinee.  Matching, of course, evaluates this, and GHC then
          -- pattern-matches the tuple of `RealWorld#` and result values in `conBinds` and
          -- reboxes the desired results in `e`.
          --
          -- We do not care about the state token, forcing evaluation or reboxing results.  This
          -- means we can delete the case statement entirely and just apply the appropriate
          -- category method to the arguments (which will be replaced by their boxed equivalents
          -- in `rebox`).  The generated body is thus the same as in case 1. once we have looked
          -- up the appropriate boxed function.
          --
          -- TODO(MP): The final argument to `f` should be the magic `RealWorld#` token, but
          -- trying to match on the last argument in a guard clause prevents us from ever
          -- hitting this case!  Figure out what was going wrong.  (Same goes for the resulting
          -- `_realWorld` token in the new bindings.)
          --
          -- TODO(MP): We could get slightly more safety by checking the `cUnit` in the table
          -- lookup as well.
          | [Plugins.Alt _alt [_realWorld, conBind] e] <- alts,
            (Plugins.Var f, arguments) <- Plugins.collectArgs sc,
            Just target <- extractCallSpecTarget $ Plugins.idDetails f,
            Plugins.StaticTarget _source functionName _cUnit True <- target,
            Just (Boxer _mod _name _dcs boxedFun, ccallArgTypes, ccallResultType) <-
              findCCallBoxer additionalBoxers makers dflags debug functionName ->
              fmap
                (rpoLabel "3" caseExpr)
                $ do
                  when (length arguments /= length ccallArgTypes + 1) $
                    lift . throwE . pure
                      . UnsupportedPrimOpApplication f arguments
                      $ pure ccallResultType
                  conBind' <- mkBoxedVarFrom "ccc_boxed_ccall" ccallResultType conBind
                  withNewBinding conBind conBind' $
                    newApp conBind'
                      <$> applyPrimOpFun boxedFun (List.zip ccallArgTypes $ List.init arguments)
                      <*\> replacePrimOps resultType e
          -- Case 2. Primitive op applied inside the scrutinee of a case expression.
          | Plugins.isPrimitiveType (Plugins.exprType sc) ->
              rpoLabel "2" caseExpr
                <$> do
                  bndr' <- mkBoxedVarFrom "ccc_boxed" newScTy bndr
                  withNewBinding bndr bndr' . fmap (newApp bndr' sc') $ case alts of
                    [Plugins.Alt Plugins.DEFAULT [] e] ->
                      -- If there is only one branch, the solution is easy.
                      replacePrimOps resultType e
                    _ -> do
                      -- There are multiple branches, so we must do a more complicated rewrite.
                      (acc, mbDefault) <-
                        traverseD (cleanAlt newScTy) alts
                          >>= foldrM (flip rewriteAlts) (id, Nothing)
                      maybe
                        (lift . throwE . pure $ UnexpectedMissingDefault caseExpr)
                        (pure . acc)
                        mbDefault
                      where
                        -- Step 1 is to rule out invalid situations (like `DataAlt`s) on this
                        -- primitive type.  We organize this way so we can report all such issues
                        -- at once.
                        --
                        -- Here we also replacePrimOps on the right-hand sides of all valid
                        -- branches.
                        cleanAlt t = \case
                          Plugins.Alt Plugins.DEFAULT _binds defExpr ->
                            Left <$> replacePrimOps resultType defExpr
                          Plugins.Alt (Plugins.LitAlt x) _binds litExpr ->
                            case x of
                              -- __TODO__: We've run into Core Lint complaints here when dealing
                              --           with negative literals (e.g., @-1# :: Int32@), but we've
                              --           managed to avoid them by interpreting operations where
                              --           they occur.
                              Plugins.LitNumber _ ->
                                Right . (Plugins.setLiteralType t x,)
                                  <$> replacePrimOps resultType litExpr
                              _ -> lift . throwE . pure $ UnsupportedPrimitiveLiteral x caseExpr
                          Plugins.Alt (Plugins.DataAlt dc) _binds _e ->
                            lift . throwE . pure $ UnsupportedPrimitiveDataAlt dc caseExpr
                        -- Step 2 is to transform situations like
                        --
                        --    case primScrut of primBinder {
                        --      __DEFAULT -> e0;
                        --      LitAlt lit1 -> e1;
                        --      LitAlt lit2 -> e2;
                        --    }
                        --
                        -- into
                        --
                        --    case (boxedScrut == lit1) of boolBinder1 {
                        --      False -> case (boxedScrut == lit2) of boolBinder2 {
                        --        False -> e0;
                        --        True -> e2;
                        --      True -> e1;
                        --    }
                        rewriteAlts (acc, mbDefault) = \case
                          Left defaultExpr ->
                            maybe
                              (pure (acc, Just defaultExpr))
                              (const . lift . throwE . pure $ UnexpectedDoubleDefault caseExpr)
                              mbDefault
                          Right (lit, litExpr) -> do
                            eqScrut <-
                              applyPrimOpFun
                                (mkEqual makers newScTy)
                                [(newScTy, Plugins.Lit lit), (newScTy, Plugins.Var bndr')]
                            eqBndr <-
                              mkBoxedVarFrom "ccc_boxed_compare" Plugins.boolTy bndr
                            pure
                              ( \k ->
                                  Plugins.Case
                                    eqScrut
                                    eqBndr
                                    (Plugins.exprType litExpr)
                                    [ Plugins.Alt (Plugins.DataAlt Plugins.falseDataCon) [] (acc k),
                                      Plugins.Alt (Plugins.DataAlt Plugins.trueDataCon) [] litExpr
                                    ],
                                mbDefault
                              )
          -- If we aren't dealing with any primitive types, we can simply go ahead and descend
          -- through this case-statement, taking care to provide the correct result type
          -- information.
          --
          -- TODO(MP): This is no guarantee!  But we should have already matched any cases that
          -- unbox variables, which is the main thing I can think of that this would otherwise
          -- catch by mistake.
          | not (Plugins.isPrimitiveType ty),
            not (Plugins.isPrimitiveType $ Plugins.varType bndr) ->
              fmap (rpoLabel "8" caseExpr) $
                Plugins.Case
                  <$> replacePrimOps (pure $ Plugins.varType bndr) sc
                  <*\> pure bndr
                  <*\> pure ty
                  <*\> traverseD
                    ( \(Plugins.Alt con bind expr) ->
                        Plugins.Alt con bind <$> replacePrimOps (pure ty) expr
                    )
                    alts
          -- TODO(MP): what error conditions should be caught here?
          | otherwise -> lift . throwE . pure $ UnsupportedPrimOpExpression "case" caseExpr
    -- Remove the application of a boxing constructor.
    reboxingApp@(Plugins.App (Plugins.Var f) e)
      | Just dc <- Plugins.isDataConId_maybe f,
        dc `elem` fmap snd boxers ->
          rpoLabel "9" reboxingApp <$> replacePrimOps (pure $ Plugins.dataConOrigResTy dc) e
    -- The cases below here all correspond to boring old traversals on hopefully boxed types.
    -- We cannot use something like `descendM` here, because we want to propagate result-type
    -- info if it's available.

    coerceExpr@(Plugins.Cast expr coercion) ->
      fmap (rpoLabel "10" coerceExpr) $
        flip Plugins.Cast coercion
          -- TODO(MP): how to make sure we get the right type out of the coercion?
          <$> replacePrimOps (pure . Plugins.pSnd $ Plugins.coercionKind coercion) expr
    -- This is hopefully a boxed variable.
    Plugins.Var v -> pure $ Plugins.Var v
    lit@(Plugins.Lit l)
      | Just ty <- resultType ->
          let dataCon
                | ty `Plugins.eqType` Plugins.doubleTy = pure Plugins.doubleDataCon
                | ty `Plugins.eqType` Plugins.floatTy = pure Plugins.floatDataCon
                | otherwise = do
                    tc <- Plugins.tyConAppTyCon_maybe ty
                    intCon <- List.find ((== tc) . intConstructorTyCon) intCons
                    pure $ intConstructorDataCon intCon
           in maybe
                -- When the dataCon is not found, it's usually because @resultType@ is @Integer@.
                -- In this case we set the type field of the `Plugins.LitNumber` to `resultType`.
                --
                -- TODO (ziyang): Can we add @Integer@ to @[IntConstructor]@?
                --
                -- TODO(MP): This is bogus!
                --
                -- The type field of a `Plugins.LitNumber` should always be one of the unboxed types
                -- according to the GHC documentation.  If we maintain this invariant, we can't
                -- continue searching for unboxed types or doing type deduction in the same way.
                -- Furthermore, we haven't encountered any Core Lint or other complaints from GHC
                -- about setting the type field to a boxed type.  We expect such literals to
                -- hopefully get caught in the top-level case, which applies `mkConst'` to them.
                -- I don't know what happens after that.
                --
                -- My best idea for how to resolve this situation nicely is to wrap
                -- `Plugins.isPrimitiveType` in our own logic which handles `Plugins.Literal` values.
                (pure . Plugins.Lit $ maybe l (`Plugins.setLiteralType` l) resultType)
                (pure . flip Plugins.mkCoreConApps [lit])
                dataCon
      | otherwise -> pure lit
    Plugins.Type t -> pure $ Plugins.Type t
    -- The strategy for applications and lambdas is a bit complex; see notes below.
    a@(Plugins.App f x)
      -- If we look ahead into the body of the lambda and find that the binder is ignored, we don't
      -- bother with the argument at all.  Lambda-handling will also see this case and remove the
      -- binder, just doing `replacePrimOps` in the body, so all we do is replace in `f`.  The
      -- reason we handle this situation separately is that we can avoid a scan of the body and all
      -- replacement in the argument.
      | Plugins.Lam binder body <- f,
        not $ binder `isFreeIn` body ->
          rpoLabel "11a" a <$> replacePrimOps resultType f
      -- If instead we find that this is a normal lambda (the binder is used), we do everything we
      -- can to work out the appropriate boxed types.  This requires looking ahead into the lambda
      -- just like the previous case.
      | Plugins.Lam binder body <- f ->
          rpoLabel "11b" a <$> do
            let predictedBinderBoxedType = deduceEBTFrom boxers binder $ universe body
            argument <- replacePrimOps predictedBinderBoxedType x
            let argTy = Plugins.exprType argument
                lamTy = mkFunTy argTy <$> resultType
            Plugins.App <$> replacePrimOps lamTy f <*\> pure argument
      -- To form the correct type for the `f` term, we need to figure out what the argument type is
      -- (by replacing in `x`, since `f` is not a `Lam`) and combine it with `resultType` using a
      -- function arrow.  To form the correct type for the `x` term, we need to figure out what the
      -- type of the replaced `f` term is and pick apart the function arrow to get the argument
      -- type.
      --
      -- Obviously we cannot do both of these at once, so for lack of any better ideas, we try
      -- replacing in `f` and `x` in both orders and only fail if neither ordering can deduce all
      -- the boxed types.
      | otherwise ->
          fmap (rpoLabel "11c" a) . Reader.ReaderT $ \binders -> do
            let fx, xf :: CategoryStack Plugins.CoreExpr
                fx = Reader.runReaderT fThenX binders
                xf = Reader.runReaderT xThenF binders
            fx `catchE` catchDeductionFailure xf
      where
        fThenX = do
          f' <- replacePrimOps Nothing f
          let argTy = fmap fst . Plugins.splitFunTy_maybe $ Plugins.exprType f'
          Plugins.App f' <$> replacePrimOps argTy x
        xThenF = do
          x' <- replacePrimOps Nothing x
          let funTy = mkFunTy (Plugins.exprType x') <$> resultType
          Plugins.App <$> replacePrimOps funTy f <*\> pure x'
        catchDeductionFailure retry es =
          if getAny $ foldMap (Any . isDeductionFailure) es
            then retry
            else throwE es
        isDeductionFailure = \case
          -- In principle, any deduction failure during replacement could be the result of not
          -- having a `resultType` for the first term we try, so any time we see
          -- `CannotDeduceBoxedTypeOfExpr` in the set of exceptions, we retry in the other order.
          CannotDeduceBoxedTypeOfExpr {} -> True
          -- Our check at the beginning of `replacePrimOps` may see us suggest an unboxed type if
          -- deduction has failed and our first attempt simply did not replace the term.  This is
          -- another reason to retry replacement in the opposite order.
          UnexpectedUnboxedType {} -> True
          _ -> False
    lam@(Plugins.Lam binder body)
      | not $ binder `isFreeIn` body ->
          rpoLabel "13a" lam . Plugins.Lam binder <$> replacePrimOps resTy body
      | Plugins.isPrimitiveType $ Plugins.varType binder ->
          rpoLabel "13b" lam <$> do
            bindTy <-
              maybe
                (lift . throwE . pure $ CannotDeduceBoxedTypeOfExpr (Plugins.Var binder) lam)
                pure
                bindTy'
            binder' <- mkBoxedVarFrom "ccc_boxed_lam" bindTy binder
            withNewBinding binder binder' $
              Plugins.Lam binder' <$> replacePrimOps resTy body
      | otherwise ->
          rpoLabel "13c" lam . Plugins.Lam binder <$> replacePrimOps resTy body
      where
        bt, resTy :: Maybe Plugins.Type
        (bt, resTy) = NonEmpty.unzip $ Plugins.splitFunTy_maybe =<< resultType
        predictedBinderBoxedType = deduceEBTFrom boxers binder $ universe body
        bindTy' = bt <|> predictedBinderBoxedType
    -- TODO(MP): Here we assume the types are already boxed.
    letExpr@(Plugins.Let (Plugins.NonRec binder definition) expression)
      | containsPrimitiveType $ Plugins.varType binder ->
          rpoLabel "12a" letExpr <$> do
            definition' <- replacePrimOps Nothing definition
            binder' <- mkBoxedVarFrom "ccc_boxed_let" (Plugins.exprType definition') binder
            withNewBinding binder binder' $
              Plugins.Let (Plugins.NonRec binder' definition')
                <$> replacePrimOps resultType expression
      | otherwise ->
          fmap (rpoLabel "12b" letExpr) $
            Plugins.Let
              <$> ( Plugins.NonRec binder
                      <$> replacePrimOps (pure $ Plugins.varType binder) definition
                  )
              <*\> replacePrimOps resultType expression
    -- Nothing to do.
    e -> lift . throwE . pure $ UnsupportedPrimOpExpression "replacePrimOps" e

mkFunTy :: Plugins.Type -> Plugins.Type -> Plugins.Type
mkFunTy argType retType = Plugins.mkTyConApp Plugins.funTyCon [argType, retType]

mkBoxedVarFrom :: String -> Plugins.Type -> Plugins.Var -> PrimOpT CategoryStack Plugins.Var
mkBoxedVarFrom pfx ty v = do
  ReplacePrimOpsContext
    { rpoCtxDebugPrint = dbg,
      rpoCtxGetUnique = getUnique,
      rpoCtxShouldTrace = debug
    } <-
    fst <$> Reader.ask
  maybeTraceWith debug (\x -> [fmt|mkBoxedVarFrom made: {dbg x} :: {dbg $ Plugins.varType x}|])
    . mkVar varName ty
    <$> lift getUnique
  where
    varName = [fmt|{pfx}_{getName v}|]
    errString =
      "<Categorifier.Core.Categorify: unboxing `case` transformation var>"
    getName = Plugins.occNameString . Plugins.nameOccName . Plugins.varName
    mkVar nm vt u =
      Plugins.mkLocalVar
        Plugins.VanillaId
        ( Plugins.mkInternalName u (Plugins.mkVarOcc nm) $
            Plugins.mkGeneralSrcSpan errString
        )
        vt
        Plugins.vanillaIdInfo

-- | Return the data constructor found if the given expression is an application of a boxing
-- constructor to an unboxed variable in an expression body; otherwise return `Nothing`.
matchBoxingApp :: [(Plugins.TyCon, Plugins.DataCon)] -> Plugins.CoreExpr -> Maybe Plugins.DataCon
matchBoxingApp boxers = \case
  Plugins.App (Plugins.Var b) _e
    | Just dc <- Plugins.isDataConId_maybe b,
      dc `elem` fmap snd boxers ->
        pure dc
  _ -> Nothing

-- | Return the original expression if it's an application of `toInteger`; otherwise return
-- `Nothing`.
matchToIntegerApp :: Plugins.CoreExpr -> Maybe Plugins.CoreExpr
matchToIntegerApp = \case
  a@(Plugins.App _ _)
    | (Plugins.Var ti, _args) <- Plugins.collectArgs a,
      Plugins.varName ti == Plugins.toIntegerName ->
        pure a
  _ -> Nothing

-- | Return the original expression if it's an application of @integerToInt@; otherwise return
-- `Nothing`.
matchIntegerToIntApp :: Plugins.CoreExpr -> Maybe Plugins.CoreExpr
matchIntegerToIntApp = \case
  a@(Plugins.App (Plugins.Var conv) _e)
    | Plugins.varName conv == Plugins.integerToIntName -> pure a
  _ -> Nothing

-- | If the given expression is the application of a @fromIntegral@ operation yielding a
-- floating-point type, then return that type; otherwise return `Nothing`.
matchFloatFromIntegralApp :: Plugins.CoreExpr -> Maybe Plugins.Type
matchFloatFromIntegralApp = \case
  Plugins.App (Plugins.Var conv) _num
    | Just toTy <- List.lookup (Plugins.varName conv) fromIntegralNames -> pure toTy
  _ -> Nothing

matchTagToEnumApp :: Plugins.CoreExpr -> Maybe Plugins.PrimOp
matchTagToEnumApp = \case
  Plugins.App (Plugins.collectArgs -> (Plugins.Var couldBeTagToEnum, _arg)) _arg'
    | Just t2e <- Plugins.isPrimOpId_maybe couldBeTagToEnum,
      t2e == Plugins.TagToEnumOp ->
        pure t2e
  _ -> Nothing

-- | Given a predicate function, return the first result found as a result of calling it on the
-- `Data.Generics.Uniplate.Data.universe` of the given expression, or `Nothing` if the predicate
-- never matches.
matchOnUniverse ::
  -- | Predicate function: return `Just` on a match
  (Plugins.CoreExpr -> Maybe a) ->
  -- | Expression to search
  Plugins.CoreExpr ->
  Maybe a
matchOnUniverse predicate = asum . fmap predicate . universe

deduceEBTFrom ::
  [(Plugins.TyCon, Plugins.DataCon)] ->
  Plugins.Var ->
  [Plugins.CoreExpr] ->
  Maybe Plugins.Type
deduceEBTFrom boxers binder expression =
  staticRules (Plugins.tyConAppTyCon_maybe $ Plugins.varType binder)
    <|> asum (fmap (scanForBinderEBTViaReboxing boxers binder) expression)
  where
    -- With floating-point numbers, the type of the unboxed variable maps unambiguously to an
    -- equivalent boxed type.  This means that some situations that succeed with floating-point
    -- types will fail with integral types, but in the floating-point cases, we can proceed without
    -- even looking through the expression.
    staticRules mbBinderTyCon
      | mbBinderTyCon == Just Plugins.doublePrimTyCon = pure Plugins.doubleTy
      | mbBinderTyCon == Just Plugins.floatPrimTyCon = pure Plugins.floatTy
      | otherwise = Nothing

scanForBinderEBTViaReboxing ::
  [(Plugins.TyCon, Plugins.DataCon)] ->
  Plugins.Var ->
  Plugins.CoreExpr ->
  Maybe Plugins.Type
scanForBinderEBTViaReboxing boxers binder = \case
  a@(Plugins.App _ _)
    | (Plugins.Var mbDcId, [Plugins.Var arg]) <- Plugins.collectArgs a,
      arg == binder,
      Just dc <- Plugins.isDataConId_maybe mbDcId,
      Just binderTy <- Plugins.mkTyConTy <$> lookup dc (fmap Tuple.swap boxers) ->
        pure binderTy
  _ -> Nothing

-- | Determines whether the given type is a primitive type, or a function type
-- whose argument types or result type contains a primitive type.
containsPrimitiveType :: Plugins.Type -> Bool
containsPrimitiveType ty
  | Plugins.isFunTy ty,
    (args, res) <- Plugins.splitFunTys ty =
      any containsPrimitiveType (res : args)
  | otherwise = Plugins.isPrimitiveType ty

replacePrimOpFunCall ::
  Plugins.CoreExpr ->
  Maybe Plugins.Type ->
  Plugins.Var ->
  [Plugins.CoreExpr] ->
  PrimOpT CategoryStack Plugins.CoreExpr
replacePrimOpFunCall = replaceFunCall deduceArgumentType deduceResultType rpoCtxPrimOpMap

-- | This is for rewriting function applications where more type info is needed in order to
-- determine which boxed function to apply.  Here we use the dodgy approach of first deducing
-- downwards to the arguments and then deducing upwards from the results of replacing the arguments.
--
-- If we have a result type already, this may be useful in deducing all the relevant types.  If we
-- haven't got one, we can still succeed if replacing primops / @NOINLINE@ functions in all the
-- argument expressions yields well-typed boxed expressions anyway.
replaceFunCall ::
  (mapType -> Plugins.Var -> Plugins.Type -> Maybe Plugins.Type) ->
  (mapType -> Plugins.Var -> Plugins.Type -> Maybe Plugins.Type) ->
  (ReplacePrimOpsContext -> mapType) ->
  Plugins.CoreExpr ->
  Maybe Plugins.Type ->
  Plugins.Var ->
  [Plugins.CoreExpr] ->
  PrimOpT CategoryStack Plugins.CoreExpr
replaceFunCall deduceArg deduceResult chooseMap outerExpr mbResultTy fid arguments = do
  ReplacePrimOpsContext
    { rpoCtxShouldTrace = debug,
      rpoCtxDebugPrint = dbg,
      rpoCtxMakers = makers,
      rpoCtxFindBoxedPrim = findBoxedPrim
    } <-
    fst <$> Reader.ask
  opMap <- chooseMap . fst <$> Reader.ask
  args <- traverseD (replacePrimOps $ deduceArg opMap fid =<< mbResultTy) arguments
  let argTypes = fmap Plugins.exprType args
      (primArgTypes, boxedArgTypes) = List.partition Plugins.isPrimitiveType argTypes
      stillHavePrims = not $ List.null primArgTypes
      accumWithTypes =
        foldr (\x acc -> [fmt|{dbg x} :: {dbg $ Plugins.exprType x}\n{acc}|]) ("" :: String)
      -- This seems like it should be handled elsewhere eventually.  Note that the second case may
      -- receive more than two arguments, which is not currently expected to work.
      apply fun = \case
        as@[_, _] -> flip Plugins.mkCoreApps as <$> lift (mkCurryH makers fun)
        as -> pure $ Plugins.mkCoreApps fun as
  realArgs <-
    if stillHavePrims
      then case boxedArgTypes of
        [] -> lift . throwE . pure $ UnsupportedPrimOpApplication fid args mbResultTy
        (boxedType : _) ->
          maybeTraceWith
            debug
            ( \new ->
                [fmt|Retried replacement with '{dbg boxedType}':
                     {accumWithTypes args}
                         -->
                     {accumWithTypes new}|]
            )
            <$> traverseD (replacePrimOps $ pure boxedType) arguments
      else pure args
  let realArgTypes = fmap Plugins.exprType realArgs
  resultType <-
    maybe
      (lift . throwE . pure $ CannotDeduceBoxedTypeOfExpr (Plugins.Var fid) outerExpr)
      pure
      $ mbResultTy <|> (deduceResult opMap fid =<< listToMaybe realArgTypes)
  case findBoxedPrim (fid, realArgTypes, resultType) of
    Just (Boxer _ _ _ boxedFun) -> lift boxedFun >>= flip apply realArgs
    Nothing ->
      lift . throwE . pure . UnsupportedPrimOpApplication fid realArgs $ pure resultType

replaceNoInlinePrimFunctionCall ::
  Plugins.CoreExpr ->
  Maybe Plugins.Type ->
  Plugins.Var ->
  [Plugins.CoreExpr] ->
  PrimOpT CategoryStack Plugins.CoreExpr
replaceNoInlinePrimFunctionCall =
  replaceFunCall deduceArgumentTypeNIP deduceResultTypeNIP rpoCtxNoInlinePrimFunctionmap

-- This is for rewriting function applications when you already know the relevant types and can
-- already form the boxed function expression (`boxedFun`) on your own.
applyPrimOpFun ::
  CategoryStack Plugins.CoreExpr ->
  [(Plugins.Type, Plugins.CoreExpr)] ->
  PrimOpT CategoryStack Plugins.CoreExpr
applyPrimOpFun boxedFun arguments = do
  f <- lift boxedFun
  args <- traverseD (uncurry replacePrimOps . first pure) arguments
  apply f args
  where
    apply ::
      Plugins.CoreExpr ->
      [Plugins.CoreExpr] ->
      PrimOpT CategoryStack Plugins.CoreExpr
    -- This seems like it should be handled elsewhere eventually.  Note that the second case may
    -- receive more than two arguments, which is not currently expected to work.
    apply fun = \case
      as@[_, _] -> do
        makers <- rpoCtxMakers . fst <$> Reader.ask
        flip Plugins.mkCoreApps as <$> lift (mkCurryH makers fun)
      as -> pure $ Plugins.mkCoreApps fun as

mkFloatingPrimOps :: Makers -> PrimOpMap
mkFloatingPrimOps makers =
  Map.fromListWith
    const
    [ ( Plugins.DoubleAddOp,
        pure (([dt, dt], dt), Boxer "GHC.Num" "+" [dd, dd] . mkPlus makers $ Plugins.mkTyConTy dt)
      ),
      ( Plugins.DoubleSubOp,
        pure (([dt, dt], dt), Boxer "GHC.Num" "-" [dd, dd] . mkMinus makers $ Plugins.mkTyConTy dt)
      ),
      ( Plugins.DoubleMulOp,
        pure (([dt, dt], dt), Boxer "GHC.Num" "*" [dd, dd] . mkTimes makers $ Plugins.mkTyConTy dt)
      ),
      ( Plugins.DoubleCosOp,
        pure (([dt], dt), Boxer "GHC.Float" "cos" [dd] . mkCos makers $ Plugins.mkTyConTy dt)
      ),
      ( Plugins.DoubleLogOp,
        pure (([dt], dt), Boxer "GHC.Float" "log" [dd] . mkLog makers $ Plugins.mkTyConTy dt)
      ),
      ( Plugins.DoubleSinOp,
        pure (([dt], dt), Boxer "GHC.Float" "sin" [dd] . mkSin makers $ Plugins.mkTyConTy dt)
      ),
      ( Plugins.DoubleDivOp,
        pure
          (([dt, dt], dt), Boxer "GHC.Real" "/" [dd, dd] . mkDivide makers $ Plugins.mkTyConTy dt)
      ),
      ( Plugins.DoubleExpOp,
        pure (([dt], dt), Boxer "GHC.Real" "exp" [dd] . mkExp makers $ Plugins.mkTyConTy dt)
      ),
      ( Plugins.DoubleSqrtOp,
        pure (([dt], dt), Boxer "GHC.Float" "sqrt" [dd] . mkSqrt makers $ Plugins.mkTyConTy dt)
      ),
      ( Plugins.DoubleNegOp,
        pure (([dt], dt), Boxer "GHC.Num" "negate" [dd] . mkNegate makers $ Plugins.mkTyConTy dt)
      ),
      ( Plugins.DoubleFabsOp,
        pure (([dt], dt), Boxer "GHC.Num" "abs" [dd] . mkAbs makers $ Plugins.mkTyConTy dt)
      ),
      ( Plugins.DoubleToFloatOp,
        pure (([dt], ft), Boxer "GHC.Float" "double2float" [dd] $ mkDoubleToFloat makers)
      ),
      ( Plugins.FloatToDoubleOp,
        pure (([ft], dt), Boxer "GHC.Float" "float2Double" [fd] $ mkFloatToDouble makers)
      ),
      ( Plugins.DoubleEqOp,
        pure
          ( ([dt, dt], Plugins.boolTyCon),
            Boxer "GHC.Classes" "==" [dd] . mkEqual makers $ Plugins.mkTyConTy dt
          )
      ),
      ( Plugins.DoubleLeOp,
        pure
          ( ([dt, dt], Plugins.boolTyCon),
            Boxer "GHC.Classes" "<=" [dd] . mkLE makers $ Plugins.mkTyConTy dt
          )
      ),
      ( Plugins.DoubleLtOp,
        pure
          ( ([dt, dt], Plugins.boolTyCon),
            Boxer "GHC.Classes" "<" [dd] . mkLT makers $ Plugins.mkTyConTy dt
          )
      ),
      ( Plugins.DoubleGeOp,
        pure
          ( ([dt, dt], Plugins.boolTyCon),
            Boxer "GHC.Classes" ">=" [dd] . mkGE makers $ Plugins.mkTyConTy dt
          )
      ),
      ( Plugins.DoubleGtOp,
        pure
          ( ([dt, dt], Plugins.boolTyCon),
            Boxer "GHC.Classes" ">" [dd] . mkGT makers $ Plugins.mkTyConTy dt
          )
      ),
      ( Plugins.DoubleNeOp,
        pure
          ( ([dt, dt], Plugins.boolTyCon),
            Boxer "GHC.Classes" "/=" [dd] . mkNotEqual makers $ Plugins.mkTyConTy dt
          )
      ),
      ( Plugins.FloatAddOp,
        pure
          (([ft, ft], ft), Boxer "GHC.Num" "+" [fd, fd] . mkPlus makers $ Plugins.mkTyConTy ft)
      ),
      ( Plugins.FloatSubOp,
        pure
          (([ft, ft], ft), Boxer "GHC.Num" "-" [fd, fd] . mkMinus makers $ Plugins.mkTyConTy ft)
      ),
      ( Plugins.FloatMulOp,
        pure
          (([ft, ft], ft), Boxer "GHC.Num" "*" [fd, fd] . mkTimes makers $ Plugins.mkTyConTy ft)
      ),
      ( Plugins.FloatCosOp,
        pure (([ft], ft), Boxer "GHC.Float" "cos" [fd] . mkCos makers $ Plugins.mkTyConTy ft)
      ),
      ( Plugins.FloatLogOp,
        pure (([ft], ft), Boxer "GHC.Float" "log" [fd] . mkLog makers $ Plugins.mkTyConTy ft)
      ),
      ( Plugins.FloatSinOp,
        pure (([ft], ft), Boxer "GHC.Float" "sin" [fd] . mkSin makers $ Plugins.mkTyConTy ft)
      ),
      ( Plugins.FloatDivOp,
        pure
          (([ft, ft], ft), Boxer "GHC.Real" "/" [fd, fd] . mkDivide makers $ Plugins.mkTyConTy ft)
      ),
      ( Plugins.FloatExpOp,
        pure (([ft], ft), Boxer "GHC.Real" "exp" [fd] . mkExp makers $ Plugins.mkTyConTy ft)
      ),
      ( Plugins.FloatSqrtOp,
        pure (([ft], ft), Boxer "GHC.Float" "sqrt" [fd] . mkSqrt makers $ Plugins.mkTyConTy ft)
      ),
      ( Plugins.FloatNegOp,
        pure (([ft], ft), Boxer "GHC.Num" "negate" [fd] . mkNegate makers $ Plugins.mkTyConTy ft)
      ),
      ( Plugins.FloatFabsOp,
        pure (([ft], ft), Boxer "GHC.Num" "abs" [fd] . mkAbs makers $ Plugins.mkTyConTy ft)
      ),
      ( Plugins.FloatEqOp,
        pure
          ( ([ft, ft], Plugins.boolTyCon),
            Boxer "GHC.Classes" "==" [fd] . mkEqual makers $ Plugins.mkTyConTy ft
          )
      ),
      ( Plugins.FloatLeOp,
        pure
          ( ([ft, ft], Plugins.boolTyCon),
            Boxer "GHC.Classes" "<=" [fd] . mkLE makers $ Plugins.mkTyConTy ft
          )
      ),
      ( Plugins.FloatLtOp,
        pure
          ( ([ft, ft], Plugins.boolTyCon),
            Boxer "GHC.Classes" "<" [fd] . mkLT makers $ Plugins.mkTyConTy ft
          )
      ),
      ( Plugins.FloatGeOp,
        pure
          ( ([ft, ft], Plugins.boolTyCon),
            Boxer "GHC.Classes" ">=" [fd] . mkGE makers $ Plugins.mkTyConTy ft
          )
      ),
      ( Plugins.FloatGtOp,
        pure
          ( ([ft, ft], Plugins.boolTyCon),
            Boxer "GHC.Classes" ">" [fd] . mkGT makers $ Plugins.mkTyConTy ft
          )
      ),
      ( Plugins.FloatNeOp,
        pure
          ( ([ft, ft], Plugins.boolTyCon),
            Boxer "GHC.Classes" "/=" [fd] . mkNotEqual makers $ Plugins.mkTyConTy ft
          )
      )
    ]
  where
    dt = Plugins.doubleTyCon
    ft = Plugins.floatTyCon
    dd = Plugins.doubleDataCon
    fd = Plugins.floatDataCon

mkWordPrimOps :: Makers -> Plugins.TyCon -> Plugins.DataCon -> PrimOpMap
mkWordPrimOps makers tc dc =
  Map.fromListWith
    const
    [ ( Plugins.WordAddOp,
        pure (([tc, tc], tc), Boxer "GHC.Num" "+" [dc, dc] . mkPlus makers $ Plugins.mkTyConTy tc)
      ),
      ( Plugins.WordSubOp,
        pure (([tc, tc], tc), Boxer "GHC.Num" "-" [dc, dc] . mkMinus makers $ Plugins.mkTyConTy tc)
      ),
      ( Plugins.WordMulOp,
        pure (([tc, tc], tc), Boxer "GHC.Num" "*" [dc, dc] . mkTimes makers $ Plugins.mkTyConTy tc)
      ),
      ( Plugins.WordQuotOp,
        pure
          (([tc, tc], tc), Boxer "GHC.Real" "quot" [dc, dc] . mkQuot makers $ Plugins.mkTyConTy tc)
      ),
      ( Plugins.WordRemOp,
        pure (([tc, tc], tc), Boxer "GHC.Real" "rem" [dc, dc] . mkRem makers $ Plugins.mkTyConTy tc)
      ),
      ( Plugins.WordEqOp,
        pure
          ( ([tc, tc], Plugins.boolTyCon),
            Boxer "GHC.Classes" "==" [dc] . mkEqual makers $ Plugins.mkTyConTy tc
          )
      ),
      ( Plugins.WordLeOp,
        pure
          ( ([tc, tc], Plugins.boolTyCon),
            Boxer "GHC.Classes" "<=" [dc] . mkLE makers $ Plugins.mkTyConTy tc
          )
      ),
      ( Plugins.WordLtOp,
        pure
          ( ([tc, tc], Plugins.boolTyCon),
            Boxer "GHC.Classes" "<" [dc] . mkLT makers $ Plugins.mkTyConTy tc
          )
      ),
      ( Plugins.WordGeOp,
        pure
          ( ([tc, tc], Plugins.boolTyCon),
            Boxer "GHC.Classes" ">=" [dc] . mkGE makers $ Plugins.mkTyConTy tc
          )
      ),
      ( Plugins.WordGtOp,
        pure
          ( ([tc, tc], Plugins.boolTyCon),
            Boxer "GHC.Classes" ">" [dc] . mkGT makers $ Plugins.mkTyConTy tc
          )
      ),
      ( Plugins.WordNeOp,
        pure
          ( ([tc, tc], Plugins.boolTyCon),
            Boxer "GHC.Classes" "/=" [dc] . mkNotEqual makers $ Plugins.mkTyConTy tc
          )
      ),
      ( Plugins.WordToDoubleOp,
        pure
          ( ([tc], Plugins.doubleTyCon),
            Boxer "GHC.Classes" "fromIntegral" [dc] $
              mkFromIntegral makers (Plugins.mkTyConTy tc) Plugins.doubleTy
          )
      ),
      ( Plugins.WordToFloatOp,
        pure
          ( ([tc], Plugins.floatTyCon),
            Boxer "GHC.Classes" "fromIntegral" [dc] $
              mkFromIntegral makers (Plugins.mkTyConTy tc) Plugins.floatTy
          )
      )
    ]

mkIntPrimOps :: Makers -> Plugins.TyCon -> Plugins.DataCon -> PrimOpMap
mkIntPrimOps makers tc dc =
  Map.fromListWith
    const
    [ ( Plugins.IntAddOp,
        pure (([tc, tc], tc), Boxer "GHC.Num" "+" [dc, dc] . mkPlus makers $ Plugins.mkTyConTy tc)
      ),
      ( Plugins.IntSubOp,
        pure (([tc, tc], tc), Boxer "GHC.Num" "-" [dc, dc] . mkMinus makers $ Plugins.mkTyConTy tc)
      ),
      ( Plugins.IntMulOp,
        pure (([tc, tc], tc), Boxer "GHC.Num" "*" [dc, dc] . mkTimes makers $ Plugins.mkTyConTy tc)
      ),
      ( Plugins.IntQuotOp,
        pure
          (([tc, tc], tc), Boxer "GHC.Real" "quot" [dc, dc] . mkQuot makers $ Plugins.mkTyConTy tc)
      ),
      ( Plugins.IntRemOp,
        pure (([tc, tc], tc), Boxer "GHC.Real" "rem" [dc, dc] . mkRem makers $ Plugins.mkTyConTy tc)
      ),
      ( Plugins.IntNegOp,
        pure (([tc], tc), Boxer "GHC.Num" "negate" [dc] . mkNegate makers $ Plugins.mkTyConTy tc)
      ),
      ( Plugins.IntEqOp,
        pure
          ( ([tc, tc], Plugins.boolTyCon),
            Boxer "GHC.Classes" "==" [dc] . mkEqual makers $ Plugins.mkTyConTy tc
          )
      ),
      ( Plugins.IntLeOp,
        pure
          ( ([tc, tc], Plugins.boolTyCon),
            Boxer "GHC.Classes" "<=" [dc] . mkLE makers $ Plugins.mkTyConTy tc
          )
      ),
      ( Plugins.IntLtOp,
        pure
          ( ([tc, tc], Plugins.boolTyCon),
            Boxer "GHC.Classes" "<" [dc] . mkLT makers $ Plugins.mkTyConTy tc
          )
      ),
      ( Plugins.IntGeOp,
        pure
          ( ([tc, tc], Plugins.boolTyCon),
            Boxer "GHC.Classes" ">=" [dc] . mkGE makers $ Plugins.mkTyConTy tc
          )
      ),
      ( Plugins.IntGtOp,
        pure
          ( ([tc, tc], Plugins.boolTyCon),
            Boxer "GHC.Classes" ">" [dc] . mkGT makers $ Plugins.mkTyConTy tc
          )
      ),
      ( Plugins.IntNeOp,
        pure
          ( ([tc, tc], Plugins.boolTyCon),
            Boxer "GHC.Classes" "/=" [dc] . mkNotEqual makers $ Plugins.mkTyConTy tc
          )
      ),
      ( Plugins.IntToDoubleOp,
        pure
          ( ([tc], Plugins.doubleTyCon),
            Boxer "GHC.Classes" "fromIntegral" [dc] $
              mkFromIntegral makers (Plugins.mkTyConTy tc) Plugins.doubleTy
          )
      ),
      ( Plugins.IntToFloatOp,
        pure
          ( ([tc], Plugins.floatTyCon),
            Boxer "GHC.Classes" "fromIntegral" [dc] $
              mkFromIntegral makers (Plugins.mkTyConTy tc) Plugins.floatTy
          )
      )
    ]

mkFpCCallBoxers ::
  Makers ->
  (Plugins.TyCon, Plugins.DataCon, String) ->
  [(Plugins.CLabelString, (Boxer, [Plugins.Type], Plugins.Type))]
mkFpCCallBoxers makers (tc, dc, suffix) =
  fmap
    (bimap (<> Plugins.mkFastString suffix) renameBoxer)
    [ ("pow", (Boxer "GHC.Float" "**" [dc, dc] $ mkPow makers ty, [ty, ty], ty)),
      ("exp", (Boxer "GHC.Real" "exp" [dc] $ mkExp makers ty, [ty], ty)),
      ("log", (Boxer "GHC.Float" "log" [dc] $ mkLog makers ty, [ty], ty)),
      ("sqrt", (Boxer "GHC.Float" "sqrt" [dc] $ mkSqrt makers ty, [ty], ty)),
      ("atan2", (Boxer "GHC.Float" "atan2" [dc, dc] $ mkArcTan2 makers ty, [ty, ty], ty)),
      ("sin", (Boxer "GHC.Real" "sin" [dc] $ mkSin makers ty, [ty], ty)),
      ("cos", (Boxer "GHC.Real" "cos" [dc] $ mkCos makers ty, [ty], ty)),
      ("tan", (Boxer "GHC.Real" "tan" [dc] $ mkTan makers ty, [ty], ty)),
      ("asin", (Boxer "GHC.Real" "asin" [dc] $ mkASin makers ty, [ty], ty)),
      ("acos", (Boxer "GHC.Real" "acos" [dc] $ mkACos makers ty, [ty], ty)),
      ("atan", (Boxer "GHC.Real" "atan" [dc] $ mkATan makers ty, [ty], ty)),
      ("sinh", (Boxer "GHC.Real" "sinh" [dc] $ mkSinh makers ty, [ty], ty)),
      ("cosh", (Boxer "GHC.Real" "cosh" [dc] $ mkCosh makers ty, [ty], ty)),
      ("tanh", (Boxer "GHC.Real" "tanh" [dc] $ mkTanh makers ty, [ty], ty)),
      ("asinh", (Boxer "GHC.Real" "asinh" [dc] $ mkASinh makers ty, [ty], ty)),
      ("acosh", (Boxer "GHC.Real" "acosh" [dc] $ mkACosh makers ty, [ty], ty)),
      ("atanh", (Boxer "GHC.Real" "atanh" [dc] $ mkATanh makers ty, [ty], ty))
    ]
  where
    ty = Plugins.mkTyConTy tc
    renameBoxer (Boxer m f d t, args, res) =
      (Boxer m [fmt|{f}{suffix}|] d t, args, res)

findCCallBoxer ::
  [(Plugins.CLabelString, (Boxer, [Plugins.Type], Plugins.Type))] ->
  Makers ->
  Plugins.DynFlags ->
  Bool ->
  Plugins.CLabelString ->
  Maybe (Boxer, [Plugins.Type], Plugins.Type)
findCCallBoxer additionalBoxers makers dflags debug x =
  lookup x table
    <|> maybeTraceWith
      debug
      (const [fmt|"findCCallBoxer: boxed equivalent for C function '{dbg x}' not found."|])
      Nothing
  where
    dbg = renderSDoc dflags . Plugins.ppr
    table =
      additionalBoxers
        <> mkFpCCallBoxers makers (Plugins.doubleTyCon, Plugins.doubleDataCon, mempty)
        <> mkFpCCallBoxers makers (Plugins.floatTyCon, Plugins.floatDataCon, "f")
        <> [ ( "isDoubleNegativeZero",
               ( Boxer "GHC.Float" "isDoubleNegativeZero" [Plugins.doubleDataCon]
                   . mkFPIsNegativeZero makers
                   $ Plugins.mkTyConTy Plugins.doubleTyCon,
                 [Plugins.doubleTy],
                 Plugins.boolTy
               )
             ),
             ( "isDoubleInfinite",
               ( Boxer "GHC.Float" "isDoubleInfinite" [Plugins.doubleDataCon]
                   . mkFPIsInfinite makers
                   $ Plugins.mkTyConTy Plugins.doubleTyCon,
                 [Plugins.doubleTy],
                 Plugins.boolTy
               )
             ),
             ( "isDoubleFinite",
               ( Boxer "GHC.Float" "isDoubleFinite" [Plugins.doubleDataCon] . mkFPIsFinite makers $
                   Plugins.mkTyConTy Plugins.doubleTyCon,
                 [Plugins.doubleTy],
                 Plugins.boolTy
               )
             ),
             ( "isDoubleNaN",
               ( Boxer "GHC.Float" "isDoubleNaN" [Plugins.doubleDataCon] . mkFPIsNaN makers $
                   Plugins.mkTyConTy Plugins.doubleTyCon,
                 [Plugins.doubleTy],
                 Plugins.boolTy
               )
             ),
             ( "isDoubleDenormalized",
               ( Boxer "GHC.Float" "isDoubleDenormalized" [Plugins.doubleDataCon]
                   . mkFPIsDenormal makers
                   $ Plugins.mkTyConTy Plugins.doubleTyCon,
                 [Plugins.doubleTy],
                 Plugins.boolTy
               )
             ),
             ( "isFloatNegativeZero",
               ( Boxer "GHC.Float" "isFloatNegativeZero" [Plugins.floatDataCon]
                   . mkFPIsNegativeZero makers
                   $ Plugins.mkTyConTy Plugins.floatTyCon,
                 [Plugins.floatTy],
                 Plugins.boolTy
               )
             ),
             ( "isFloatInfinite",
               ( Boxer "GHC.Float" "isFloatInfinite" [Plugins.floatDataCon]
                   . mkFPIsInfinite makers
                   $ Plugins.mkTyConTy Plugins.floatTyCon,
                 [Plugins.floatTy],
                 Plugins.boolTy
               )
             ),
             ( "isFloatFinite",
               ( Boxer "GHC.Float" "isFloatFinite" [Plugins.floatDataCon] . mkFPIsFinite makers $
                   Plugins.mkTyConTy Plugins.floatTyCon,
                 [Plugins.floatTy],
                 Plugins.boolTy
               )
             ),
             ( "isFloatNaN",
               ( Boxer "GHC.Float" "isFloatNaN" [Plugins.floatDataCon] . mkFPIsNaN makers $
                   Plugins.mkTyConTy Plugins.floatTyCon,
                 [Plugins.floatTy],
                 Plugins.boolTy
               )
             ),
             ( "isFloatDenormalized",
               ( Boxer "GHC.Float" "isFloatDenormalized" [Plugins.floatDataCon]
                   . mkFPIsDenormal makers
                   $ Plugins.mkTyConTy Plugins.floatTyCon,
                 [Plugins.floatTy],
                 Plugins.boolTy
               )
             )
           ]

mkNoInlinePrimIntFunctions :: Makers -> Plugins.TyCon -> Plugins.DataCon -> NoInlinePrimFunctionMap
mkNoInlinePrimIntFunctions makers tc dc =
  Map.fromListWith
    const
    -- TODO (#18): add `rem` and `quot`. There's unfortunately no `Plugins.remIntName` or
    -- `Plugins.quotIntName`, which makes it slightly more complicated.
    [ ( Plugins.modIntName,
        pure (([tc, tc], tc), Boxer "GHC.Real" "mod" [dc, dc] . mkMod makers $ Plugins.mkTyConTy tc)
      ),
      ( Plugins.divIntName,
        pure (([tc, tc], tc), Boxer "GHC.Real" "div" [dc, dc] . mkDiv makers $ Plugins.mkTyConTy tc)
      )
    ]

-- Enum comparison is handled slightly differently; we don't do type deduction or type-based lookup.

type EnumCmpMap = Map Plugins.PrimOp (Makers -> Plugins.Type -> CategoryStack Plugins.CoreExpr)

enumCmpMap :: EnumCmpMap
enumCmpMap =
  Map.fromListWith
    const
    [ (Plugins.IntEqOp, mkEqual),
      (Plugins.IntLeOp, mkLE),
      (Plugins.IntLtOp, mkLT),
      (Plugins.IntGeOp, mkGE),
      (Plugins.IntGtOp, mkGT),
      (Plugins.IntNeOp, mkNotEqual)
    ]

reboxEnumCmp ::
  Plugins.Var ->
  Plugins.Type ->
  Plugins.CoreExpr ->
  Plugins.CoreExpr ->
  PrimOpT CategoryStack Plugins.CoreExpr
reboxEnumCmp primOpVar argType enumA enumB = do
  opMaker <- maybe errorMsg pure $ getEnumCmpPrimOp primOpVar
  baseMakers <- rpoCtxMakers . fst <$> Reader.ask
  applyPrimOpFun (opMaker baseMakers argType) $ List.zip (repeat argType) [enumA, enumB]
  where
    getEnumCmpPrimOp = flip Map.lookup enumCmpMap <=< Plugins.isPrimOpId_maybe
    errorMsg =
      lift . throwE . pure . UnsupportedPrimOpExpression "reboxEnumCmp" $
        Plugins.mkCoreApps (Plugins.Var primOpVar) [enumA, enumB]

stripGetTag ::
  GetTagInfo ->
  Plugins.CoreExpr ->
  Maybe Plugins.CoreExpr
stripGetTag (GetTagInfo gtId) = go
  where
    go = \case
      app@Plugins.App {}
        | (Plugins.Var op, [Plugins.Type _ty, x]) <- Plugins.collectArgs app,
          op == gtId ->
            pure x
      -- Plugins.Cast expr _coercion -> go expr
      _ -> Nothing
