{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

-- | The high-level transformation between GHC's `Plugins.CoreExpr` and the abstract categorical
--   representation, as described in [Compiling to
--   Categories](http://conal.net/papers/compiling-to-categories/compiling-to-categories.pdf).
module Kitty.Plugin.Core.Categorize
  ( AutoInterpreter,
    categorize,
    applyTyAndPredArgs,
    isTypeOrPred,
    simplifyFun,
  )
where

import Control.Arrow (Arrow ((&&&)))
import Control.Monad ((<=<), when)
import Control.Monad.Extra (loopM, unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), catchE, runExceptT, throwE, withExceptT)
import Control.Monad.Trans.RWS.Strict (ask, gets, local, modify)
import CoreArity (etaExpand)
import CoreStats (exprSize)
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (bitraverse)
import Data.Bool (bool)
import Data.Functor ((<&>))
import Data.Functor.Alt ((<!>))
import Data.Functor.Transformer (tmap)
import Data.Generics.Uniplate.Data (transformBi, transformM, universeBi)
import Data.List.Extra (isPrefixOf, isSuffixOf, notNull, sortOn)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Traversable (for)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as Set
import Data.Tuple.Extra (first3, thd3)
import qualified ForeignCall as Plugins
import qualified GhcPlugins as Plugins
import Kitty.Duoidal (joinD, sequenceD, traverseD, (<*\>), (<=\<), (=<\<))
import qualified Kitty.Plugin.Core.Benchmark as Bench
import Kitty.Plugin.Core.MakerMap
  ( MakerMapFun,
    composeCat,
    curryCat,
    forkCat,
    handleAdditionalArgs,
    makeMaker1,
    makeMaker2,
    splitNameString,
  )
import Kitty.Plugin.Core.Makers (Makers (..), isCalledIn, isFreeIn)
import qualified Kitty.Plugin.Core.PrimOp as PrimOp
import Kitty.Plugin.Core.Simplify (Transformation (..), simplifyExpr)
import Kitty.Plugin.Core.Trace (WithIdInfo (..), maybeTraceWith, maybeTraceWithStack, renderSDoc)
import Kitty.Plugin.Core.Types
  ( AutoInterpreter,
    CategoricalFailure (..),
    CategoryStack,
    CategoryState (..),
    DictCacheEntry (..),
    DictionaryFailure (..),
    DictionaryStack,
  )
import Kitty.Plugin.Hierarchy (BaseIdentifiers (..), getLast, pattern Last)
import qualified Language.Haskell.TH as TH
import PrelNames (leftDataConName, rightDataConName)
import Prelude hiding (head)
import qualified TyCoRep
import qualified TysWiredIn
import qualified Unique

-- Need Uniplate for traversals on GHC-provided recursive types
{-# ANN module ("HLint: ignore Avoid restricted module" :: String) #-}

-- | This is named as a pun on `Kitty.Plugin.Categorize.expression`, as it's effectively the "real"
--   implementation of that pseudo-function.
categorize ::
  -- | Enable debugging
  Bool ->
  -- | Enable benchmarking
  Bool ->
  Plugins.DynFlags ->
  -- | Target category
  Plugins.Type ->
  (Plugins.Type -> DictionaryStack Plugins.CoreExpr) ->
  BaseIdentifiers ->
  Makers ->
  Makers ->
  AutoInterpreter ->
  MakerMapFun ->
  (Makers -> [(Plugins.CLabelString, (PrimOp.Boxer, [Plugins.Type], Plugins.Type))]) ->
  -- | The @a -> b@ parameter from `Kitty.Plugin.Categorize.expression`.
  Plugins.CoreExpr ->
  -- | The @c a b@ result of `Kitty.Plugin.Categorize.expression` (where @c@ represents the arrow of
  --   the target category).
  CategoryStack Plugins.CoreExpr
categorize
  debug
  bench
  dflags
  cat
  buildDictionary
  baseIdentifiers
  baseMakers
  makers
  tryAutoInterpret
  makerMapFun
  additionalBoxers
  fun = do
    res0 <-
      maybeTraceWith debug (const "---------- categorize ----------")
        . Bench.billToUninterruptible bench Bench.Categorize
        $ categorizeFun fun
    -- It seems GHC simplifier's memory usage is determined by the size of the largest top level
    -- bind. Therefore, we float some local let-binds out to the top level, which reduces the
    -- size of the largest top-level bind.
    --
    -- The `markBindNoInline` saves some work by preventing the simplifier from inlining these
    -- binds. The simplifier would otherwise attempt to inline some of them as part of
    -- `PreInlineUnconditionally` and `PostInlineUnconditionally`.
    (res1, fmap markBindNoInline -> binds) <- floatLetsOut res0
    (fmap markBindNoInline -> dictVarBinds) <-
      fmap (uncurry Plugins.NonRec . fst) . sortOn snd <$> getCreatedDictVars
    let res =
          maybeTraceWith debug (\res' -> "result size: " <> show (exprSize res'))
            . maybeTraceWith debug (thump "result")
            . Plugins.mkCoreLets dictVarBinds
            $ Plugins.mkCoreLets binds res1
    when bench Bench.displayTimes
    pure res
    where
      floatLetsOut :: Plugins.CoreExpr -> CategoryStack (Plugins.CoreExpr, [Plugins.CoreBind])
      floatLetsOut = go []
        where
          go ::
            [Plugins.CoreBind] ->
            Plugins.CoreExpr ->
            CategoryStack (Plugins.CoreExpr, [Plugins.CoreBind])
          go binds = \case
            Plugins.App f arg -> do
              (f', binds') <- go binds f
              first (Plugins.App f') <$> go binds' arg
            Plugins.Cast e co ->
              first (`Plugins.Cast` co) <$> go binds e
            Plugins.Case e b ty alts ->
              first (\e' -> Plugins.Case e' b ty alts) <$> go binds e
            Plugins.Let (Plugins.NonRec v rhs) e -> do
              v' <- uniquifyVarName v
              let e' = subst [(v, Plugins.Var v')] e
              second (Plugins.NonRec v' rhs :) <$> go binds e'
            -- In all other cases we stop descending. In particular, we must stop descending
            -- in the `Lam` case, because a `Let` under a `Lam` may refer to a lambda-bound
            -- var, and thus cannot be floated out. We could potentially float out a `Let`
            -- that doesn't refer to any lambda bound var (also called full-laziness), but
            -- we currently don't bother to do so.
            other -> pure (other, binds)
      funName = case fst (Plugins.collectArgs fun) of
        Plugins.Var v -> Just . Plugins.occNameString . Plugins.nameOccName $ Plugins.varName v
        _ -> Nothing
      -- "Since we are translating function-typed terms, we can assume that we have an explicit
      --  abstraction, /λ(x :: τ) → U/ for some term /U/; otherwise, simply η-expand." ⸻§3
      categorizeFun :: Plugins.CoreExpr -> CategoryStack Plugins.CoreExpr
      categorizeFun =
        maybeTraceWithStack debug (thump "fun") $ \case
          Plugins.Lam x u -> categorizeLambda x u
          -- `Plugins.Cast` at the top level is difficult. Within `categorizeLambda`, we can always
          -- compose the coercion, but here we need to apply the coercion to a function, and then
          -- categorize /that/. This currently only handles simple cases, where we can either ignore
          -- the coercion or separate it into coercions for the domain and codomain, which we can
          -- then compose.
          to@(Plugins.Cast from co) ->
            case co of
              TyCoRep.FunCo {} ->
                joinD $
                  ( \(a', b') (a, b) ->
                      joinD $
                        composeCat makers
                          <$> mkCoerce makers b' b
                          <*\> joinD
                            (composeCat makers <$> categorizeFun from <*\> mkCoerce makers a a')
                  )
                    <$> extractTypes from
                    <*\> extractTypes to
              TyCoRep.Refl _ -> categorizeFun from
              TyCoRep.TransCo inner outer ->
                categorizeFun $ Plugins.Cast (Plugins.Cast from inner) outer -- NON-INDUCTIVE
              _ -> throwE . pure $ UnsupportedCast from co
            where
              extractTypes expr =
                let eTy = Plugins.exprType expr
                 in maybe (throwE . pure . NotFunTy expr $ eTy) pure $
                      Plugins.splitFunTy_maybe eTy
          Plugins.Tick tickish expr -> Plugins.Tick tickish <$> categorizeFun expr
          -- __NB__: `etaExpand` can result in `Plugins.Cast` and `Plugins.Tick` in addition to the
          --         expected `Plugins.Lam`, so we handle those cases here recursively.
          e -> categorizeFun $ etaExpand 1 e -- NON-INDUCTIVE
      categorizeLambda = categorizeLambda' MakeConst

      categorizeLambda' ::
        MakeOrIgnoreConst ->
        Plugins.Var ->
        Plugins.CoreExpr ->
        CategoryStack Plugins.CoreExpr
      categorizeLambda' makeOrIgnoreConst name =
        maybeTraceWithStack debug (thump "lam" . Plugins.Lam name) $ \case
          Plugins.Coercion co -> throwE . pure . UnsupportedDependentType name $ Left co
          Plugins.Type ty -> throwE . pure . UnsupportedDependentType name $ pure ty
          -- "The remaining case is a constant as abstraction body, i.e., /λx → c/." ⸻§3
          body
            | MakeConst <- makeOrIgnoreConst,
              not (name `isFreeIn` body) ->
              ( maybe (tryMkConst name) (mkConstFun (Plugins.varType name) . fst)
                  . Plugins.splitFunTy_maybe
                  . Plugins.dropForAlls
                  $ Plugins.exprType body
              )
                body
                makers
          -- "First consider the case that the abstraction body is a variable. Since our terms are
          --  closed and well-typed, there is only one possible variable choice, so we must have the
          --  identity function on /τ/: /(λx → x) ≡ id :: τ → τ/." ⸻§3
          Plugins.Var y ->
            if name == y
              then mkId makers $ Plugins.varType name
              else categorizeLambda name =<\< mkInline (Plugins.Var y)
          -- `tagToEnum#` in enum comparisons
          --
          -- Typically we see `tagToEnum#` within some other primitive-related expression.
          -- But in some situations, we see @`tagToEnum#` (`==#` <some tag> <another tag>)@.
          --
          -- We have two options for detecting this situation:
          --
          --  - right after the `==` for the enum is inlined, when it's a nested series of `case`
          --    expressions to primitivise the enums around the comparison
          --
          --  - right before we try to inline `tagToEnum#` itself, when it's at the outside of the
          --    application
          --
          -- The advantage of the former is that we can keep all the primop stuff inside
          -- `case`-handling in this function, but the latter is preferable because at this
          -- point, the `let` and `case` bindings have been moved beneath `tagToEnum#` itself,
          -- so we can trigger primop replacement without having to look deeper than the top
          -- level of the expression.
          e@(Plugins.App _ _)
            | Just _t2e <- PrimOp.matchTagToEnumApp e ->
              handlePrimOps "applying `tagToEnum#'" name e $ Plugins.exprType e
          -- "Translating an application (as abstraction body) is a little more involved, involving
          --  the /Category/, /Cartesian/, and /Closed/ instances for functions" ⸻§3
          e@(Plugins.App (Plugins.collectArgs -> (head, args')) arg) ->
            let args = args' <> [arg]
             in case head of
                  Plugins.Var ident
                    | ident /= name ->
                      maybe
                        -- If the expression is a categorical operation, translate it directly.
                        (interpretVocabulary name e ident)
                        (categorizeDataCon name e)
                        (Plugins.isDataConId_maybe ident)
                        args
                  -- This handles `Cast from co` where `from` is a dictionary, and `co` is an
                  -- `AxiomInstCo`. Such a `Cast` mostly likely comes from single-method classes
                  -- (for multi-method classes you'd get the benign `$fFoo_$cfoo` stuff). The way
                  -- we deal with it is to inline `from` to get `Cast (Cast from' co') co`, where
                  -- `co` cancels `co'`, and `from'` has the right type (same type as
                  -- `$fFoo_$cfoo`). If the inlined expression is not in this form, we keep
                  -- inlining, until it is. Then we simply discard `co` and `co'`, and proceed
                  -- with `from'`.
                  Plugins.Cast from0 TyCoRep.AxiomInstCo {}
                    | Plugins.isPredTy (Plugins.exprType from0) -> do
                      inlined <- flip loopM from0 $ \from -> do
                        ( \case
                            Plugins.Cast from' _ ->
                              if Plugins.isPredTy (Plugins.exprType from')
                                then Left from'
                                else Right from'
                            other -> Left other
                          )
                          <$> (simplifyFun dflags [] =<\< mkInline from)
                      categorizeLambda name
                        =<\< simplifyFun dflags [] (Plugins.mkCoreApps inlined args)
                  -- Convert all the arguments of an application at once.
                  _
                    | let (tyArgs, otherArgs) = spanTypes args,
                      notNull tyArgs -> do
                      {-
                       handleExtraArgs can only handle term args, not type args. This is because
                       it uses mkApply, which basically creates @(a -> b, a) `k` b@. However,
                       to apply a type arg, e.g., apply @A@ to @forall a. Maybe a@, we need to
                       create @((a :: *) -> Maybe a, A) `k` Maybe A@. This doesn't align with
                       mkApply.

                       The approach taken here is to coerce the CoreExpr of type
                       @forall a. Maybe a@ into type @Maybe A@.
                       -}
                      headCoerced <-
                        Plugins.App
                          <$> mkCoerce
                            baseMakers
                            (Plugins.exprType head)
                            (Plugins.exprType (Plugins.mkTyApps head tyArgs))
                          <*\> pure head
                      handleExtraArgs makers name otherArgs =<\< categorizeLambda name headCoerced
                    | otherwise ->
                      -- If we are dealing with something like
                      --
                      -- @
                      --   (let ... in (let-binding, 0 or more times)
                      --    case ... of ... -> (single-alt case, 0 or more times)
                      --    \x y ... -> body) arg1 arg2 ...
                      -- @
                      --
                      -- then instead of simply categorizing `head` and each arg, we check if we
                      -- should perform any substitutions, i.e., substitute `arg1` for `x`, `arg2`
                      -- for `y`, etc.
                      --
                      -- This is not just an optimization, but is in fact a necessary step.
                      -- The `simplifyFun []` after inlining is supposed to perform beta-reductions.
                      -- However, sometimes a `case` expression with a single `DEFAULT` case may
                      -- prevent beta-reductions from being performed. As a result, here we may be
                      -- faced with something like
                      --
                      -- @
                      --    (case eq_sel ($p3(%,,%) ($d(%,,%)_aKHi `cast` <Co:25>)) of co_aKND
                      --       { __DEFAULT -> let ...
                      --                       in \x -> body
                      --       }) arg
                      -- @
                      --
                      -- i.e., `simplifyFun` failed to substitute `arg` for `x` here, due to the
                      -- existence of the `case`.
                      --
                      -- This could cause problems. For instance, if `arg` is a constant, then an
                      -- expression that mentions `arg` and doesn't depend on the input can be
                      -- categorized via `mkConst`. But if we try to categorize `x -> body`,
                      -- then since `x` is now an argument, anything in `body` that mentions `x`
                      -- cannot be categorized via `mkConst`. Therefore, in this case, we must
                      -- substitute `arg` for `x`.
                      let (xs, bndrs, body) = collectNestedBinders head
                          (newBody, remainingBndrs, remainingArgs) = substBndrs body bndrs args
                       in if length bndrs == length remainingBndrs
                            then handleExtraArgs makers name args =<\< categorizeLambda name head
                            else
                              categorizeLambda name . addLetsAndCases xs $
                                Plugins.mkCoreApps
                                  (Plugins.mkCoreLams remainingBndrs newBody)
                                  remainingArgs
          -- "If the body of an abstraction is an abstraction, we can curry a translation of the
          --  uncurried form:" ⸻§3
          Plugins.Lam name' body -> do
            let pair =
                  freshId
                    (Plugins.exprFreeVars body)
                    "pair"
                    (TysWiredIn.mkBoxedTupleTy [Plugins.varType name, Plugins.varType name'])
            sub <-
              traverseD
                sequenceD
                [(name, mkFst makers (Plugins.Var pair)), (name', mkSnd makers (Plugins.Var pair))]
            curryCat makers =<\< categorizeLambda pair (subst sub body)
          -- The @unsafeBinder@ and @unsafeAlts@ are unsafe because they are not necessarily unique.
          --
          -- The lack of uniqueness of @unsafeBinder@ can be observed by building
          -- //code_generation/generate:CalcTracking on 15465/10, where you'd see
          --
          -- @
          --    lam:
          --      \ (wild_aAtb :: Type1) (wild_aAtb :: Type2) -> ...
          -- @
          --
          -- which leads to a core lint error. To get a unique binder, use @withBinder@.
          --
          -- The lack of uniqueness of @unsafeAlts@ can be observed by building
          -- //code_generation/generate:BallFollower on 15833/2, where you'd see @x_azfm@
          -- and @y_azfn@ used repeatedly, causing shadowing.
          caseExpr@(Plugins.Case scrut (Plugins.zapIdOccInfo -> unsafeBinder) typ unsafeAlts) -> do
            alts <- for unsafeAlts $ \(altCon, unsafeBoundVars, rhs) -> do
              boundVars <- traverse uniquifyVarName unsafeBoundVars
              pure (altCon, boundVars, subst (zip unsafeBoundVars $ fmap Plugins.Var boundVars) rhs)

            let withBinder f = do
                  binder <-
                    -- If @unsafeBinder@ occurs in any of the @Alt@s, we don't bother making
                    -- a new unique binder, because if it is not unique, we don't know which
                    -- one the occurrence refers to.
                    if any (isFreeIn unsafeBinder . thd3) alts
                      then pure unsafeBinder
                      else uniquifyVarName unsafeBinder
                  categorizeLambda name . Plugins.Let (Plugins.NonRec binder scrut) =<\< f binder

            case alts of
              -- "For __case__ expressions, suppose the scrutinee expression has a product type"
              -- ⸻§3
              [(Plugins.DataAlt dc, [a, b], rhs)] | Plugins.isTupleDataCon dc ->
                withBinder $ \binder -> do
                  bindFst <- do
                    if a `isFreeIn` rhs
                      then Plugins.Let . Plugins.NonRec a <$> mkFst makers (Plugins.Var binder)
                      else pure id
                  bindSnd <-
                    if b `isFreeIn` rhs
                      then Plugins.Let . Plugins.NonRec b <$> mkSnd makers (Plugins.Var binder)
                      else pure id
                  pure $ bindFst (bindSnd rhs)
              -- "Distributive categories enable translation of definition by cases. Consider
              --  only __case__ over binary sums /a + b/ for now." ⸻§8
              [(Plugins.DataAlt left, [a], lrhs), (Plugins.DataAlt right, [b], rrhs)]
                | Plugins.dataConName left == leftDataConName
                    && Plugins.dataConName right == rightDataConName ->
                  withBinder $
                    mkEither makers (Plugins.Lam a lrhs) (Plugins.Lam b rrhs)
                      . Plugins.Var
              -- @if@ is represented in Core as a @case@ on `Bool`.
              [(Plugins.DataAlt false, [], rhsF), (Plugins.DataAlt true, [], rhsT)]
                | false == Plugins.falseDataCon && true == Plugins.trueDataCon ->
                  joinD $
                    composeCat makers
                      <$> mkIf makers typ
                      <*\> joinD
                        ( forkCat makers
                            <$> categorizeLambda name scrut
                            <*\> joinD
                              ( forkCat makers
                                  <$> categorizeLambda name rhsT
                                  <*\> categorizeLambda name rhsF
                              )
                        )
              -- @Data.Constraint.Dict@ contains a constraint, so it can't have a
              -- @HasRep@ instance. Here we handle it as a special case.
              [(Plugins.DataAlt dc, [v], rhs)]
                | isDictDataCon dc,
                  let predTy = Plugins.varType v,
                  Plugins.isPredTy predTy ->
                  let go [] = throwE . pure $ ConstraintNotFound scrut predTy
                      go (x : xs) =
                        findTypeInTuple makers predTy (Plugins.Var x) >>= \case
                          Just expr -> do
                            categorizeLambda name $ Plugins.Let (Plugins.NonRec v expr) rhs
                          Nothing -> go xs
                   in go (universeBi scrut)
              -- "One more transformation eliminates the unboxing __case__ scrutinees: transform
              --  an expression like “__case__ /a/ __of__ /I# x/ → ... /boxI x/ ...” to “__let__
              --  /x′/ = /a/ __in__ ... /x′/ ...”." ⸻§10.1
              --
              -- Here we are matching a case-statement that matches apart a boxed value into an
              -- unboxed (primitive) one, so that @_rhs@ contains some unboxed operations.  This
              -- type of destructuring bind will eventually be removed in case 3 of
              -- `PrimOp.checkForUnboxedVars` below.
              [(Plugins.DataAlt con, [_unboxedV], _rhs)]
                -- We look for the boxing constructor for the type that this case statement
                -- returns.
                | con `elem` fmap snd primConMap ->
                  handlePrimOps "unboxing" name (Plugins.Case scrut unsafeBinder typ alts) typ
              -- This case handles calls to `fromInteger` and `fromIntegral` at the top level.
              -- It is separate from the preceding case because it involves reboxing rather than
              -- unboxing a primitive (the unboxing of the argument typically occurs within the
              -- right-hand side of the case alternative); its contents must still be handled
              -- with `replacePrimOps`.
              [(Plugins.DEFAULT, [], rhs)]
                | Plugins.isCoVar unsafeBinder ->
                  if unsafeBinder `isFreeIn` rhs
                    then do
                      binder <- uniquifyVarName unsafeBinder
                      res <-
                        categorizeLambda name $
                          transformBi (\case v | v == unsafeBinder -> binder; other -> other) rhs
                      -- Here we are making a `Plugins.Case`, rather than a `Plugins.Let`
                      -- (as `withBinder` would do), because core lint complains
                      -- "bad `let` binding" when a let-binding has a coercion type.
                      pure $
                        Plugins.Case
                          scrut
                          binder
                          (Plugins.exprType res)
                          [(Plugins.DEFAULT, [], res)]
                    else categorizeLambda name rhs
                -- `frominteger`
                | Just toTy <- PrimOp.matchOnUniverse PrimOp.matchFloatFromIntegralApp scrut,
                  Just _dc <- PrimOp.matchOnUniverse (PrimOp.matchBoxingApp primConMap) rhs ->
                  -- This is the original case at the top level of `categorizeLambda`.
                  handlePrimOps
                    "fromInteger"
                    name
                    (Plugins.Case scrut unsafeBinder typ alts)
                    toTy
                -- `fromIntegral`
                | Just i2iApp <- PrimOp.matchOnUniverse PrimOp.matchIntegerToIntApp scrut,
                  Just _toIApp <- PrimOp.matchOnUniverse PrimOp.matchToIntegerApp i2iApp,
                  Just _dc <- PrimOp.matchOnUniverse (PrimOp.matchBoxingApp primConMap) rhs ->
                  -- This is the original case at the top level of `categorizeLambda`.
                  handlePrimOps
                    "fromIntegral"
                    name
                    (Plugins.Case scrut unsafeBinder typ alts)
                    typ
              -- Also need to handle the unit case.
              [(_, [], rhs)] -> withBinder $ \_binder -> pure rhs
              -- When the scrut's type is a constraint (e.g., `Num (C Double)`), we must
              -- specialize the whole case expression, because constraints don't have `HasRep`
              -- instances. This is achieved by simplifying it with `Inline` and `Rules`.
              [_]
                | Plugins.isPredTy (Plugins.varType unsafeBinder) ->
                  categorizeLambda name =<\< simplifyFun dflags [Inline, Rules] caseExpr
              -- "consider a __case__ expression /case scrut of { p1 → rhs1; ...; pn → rhsn }/,
              -- where (the scrutinee) /scrut/ has a non-standard type with a /HasRep/
              -- instance. Rewrite /scrut/ to /inline abst (repr scrut)/ (this time inlining
              -- /abst/ instead of /repr/).  GHC’s usual simplifications will then replace the
              -- __case__ over a non-standard type with a __case__ over a standard type or one
              -- closer to standard." ⸻§9
              -- __NB__: This case can pretty easily cause an infinite loop, so we should be
              --         very careful with handling the product and coproduct @case@ cases.
              _ -> do
                let bindTy =
                      maybeTraceWith
                        debug
                        ( \bt ->
                            "case> fallback:\nscrut:\t"
                              <> dbg scrut
                              <> "\nbinder:\t"
                              <> dbg unsafeBinder
                              <> "\nbinder type:\t"
                              <> dbg bt
                              <> "\ntype:\t"
                              <> dbg typ
                              <> "\nalts:\t"
                              <> dbg alts
                        )
                        -- scrut type sometimes differs from binder type, e.g.,
                        --
                        -- scrut type: @Vec ('S ('S ('S 'Z))) (PressureSensorStatus C)@
                        -- binder type: @Vec n1 (PressureSensorStatus C)@
                        --
                        -- In this case we can't use binder type.
                        $ Plugins.exprType scrut

                abst <- inlineHasRep =<\< mkAbst makers bindTy
                repr <- mkRepr makers bindTy
                -- NON-INDUCTIVE
                -- Here we expect `simplifyFun` to apply the `let`-substitution, case-of-case,
                -- and case-of-known-constructor transformations.
                categorizeLambda name <=\< simplifyFun dflags [CaseOfCase] $
                  Plugins.Case (Plugins.App abst (Plugins.App repr scrut)) unsafeBinder typ alts
          Plugins.Let bind expr -> case bind of
            Plugins.NonRec v rhs ->
              if not (name `isFreeIn` rhs)
                &&
                -- Don't float out join points, because doing so may cause errors like this:
                --
                -- ghc: panic! (the 'impossible' happened)
                --   (GHC version 8.10.1:
                --   GHC.StgToCmm.Env: variable not found
                --   $j_sdPJ
                not (Plugins.isJoinId v)
                then -- Float bindings outside of lambdas when possible. This is an optimization,
                --      but more importantly it prevents us from trying to inline type class
                --      dictionaries (which GHC does not want to do) by moving them outside the term
                --      being categorized. E.g.,
                --
                --    > categorize $ \x -> let $pIsPrimitive = ... in myFun $pIsPrimitive x
                --
                --      becomes
                --
                --    > let $pIsPrimitive = ... in categorize $ \x -> myFun $pIsPrimitive x
                --
                --      rather than
                --
                --    > categorize $ \x' -> (\(x, $pIsPrimitive) -> myFun $pIsPrimitive x) (x', ...)
                --
                --      This also stores the binding in case we need to look it up and categorize it
                --      later (e.g., in the case of join points).

                  fmap (Plugins.Let bind) . tmap (local (Map.insert v rhs)) $
                    categorizeLambda name expr
                else -- Either substitute the @rhs@ in @expr@, or rewrite as a lambda.
                -- Whether substituting or rewriting is determined by the number of occurrence of
                -- `v` in `expr`, which we can obtain from `OccInfo` or by whether `expr` consists
                -- only of projections.
                --
                -- NOTE: The rationale is as follows. @let@ bindings can always be desugared to an
                -- application of a lambda, which potentially involves fewer common subexpressions.
                -- However, because of the handling of nested lambdas from the paper, that can
                -- sometimes lead to an explosion in argument size, eventually resulting in
                -- additional curry/uncurry calls and slowing down the process of @buildDictionary@.
                -- So here we instead substitute the bound term in some cases, most notably when
                -- the term consists only of projections from @name@.

                -- TODO (SW-3488): currently the v's `OccInfo` is always `ManyOccs`, probably
                -- because we forget to `zapIdOccInfo` somewhere. If the `OccInfo` is accurate,
                -- we can obtain `isManyOccs` from it rather than manually counting.

                  let isManyOccs = case filter (== v) $ universeBi expr of
                        _ : _ : _ -> True
                        _ -> False
                   in if Plugins.isJoinId v
                        || not isManyOccs
                        || hasOnlyProjections (const True) rhs
                        || Plugins.isPredTy (Plugins.varType v)
                        || any isDictOrBarbiesDictTyCon (universeBi (Plugins.varType v))
                        || Plugins.isForAllTy (Plugins.varType v)
                        then
                          categorizeLambda name
                            =<\< bool
                              pure
                              -- If `v` has a polymorphic type, we run the simplifier
                              -- to apply the type argument(s).
                              (simplifyFun dflags [])
                              (Plugins.isForAllTy (Plugins.varType v))
                              (subst [(v, rhs)] expr)
                        else
                          categorizeLambda
                            name
                            (Plugins.App (Plugins.Lam v expr) rhs) -- NON-INDUCTIVE
            Plugins.Rec [(v, rhs)] -> do
              nonRec <- Plugins.NonRec v <$> unfix v [] rhs
              categorizeLambda name $ Plugins.Let nonRec expr
            Plugins.Rec binds -> throwE . pure $ UnsupportedMutuallyRecursiveLetBindings binds
          to@(Plugins.Cast from _) ->
            joinD $
              composeCat makers
                <$> mkCoerce makers (Plugins.exprType from) (Plugins.exprType to)
                <*\> categorizeLambda name from
          Plugins.Tick tickish body -> Plugins.Tick tickish <$> categorizeLambda name body
          -- This case is covered by "constant as abstraction body", but hard to convince GHC of
          -- that, so we duplicate the relevant logic here.
          Plugins.Lit lit -> mkConst' makers (Plugins.varType name) (Plugins.Lit lit)

      substBndrs ::
        Plugins.CoreExpr ->
        [Plugins.Var] ->
        [Plugins.CoreExpr] ->
        (Plugins.CoreExpr, [Plugins.Var], [Plugins.CoreExpr])
      substBndrs body (bndr : bndrs) (Plugins.Var arg : args) =
        -- We only perform the substitution if the arg is a `Plugins.Var`, because
        -- for non-Var args, we'd need to perform checks such as how many times
        -- `bndr` occurs in `body` to determine whether we should substitute, and
        -- the benefit does not make up for the overhead.
        substBndrs (subst [(bndr, Plugins.Var arg)] body) bndrs args
      substBndrs body bndrs args = (body, bndrs, args)

      categorizeDataCon ::
        Plugins.Var ->
        Plugins.CoreExpr ->
        Plugins.DataCon ->
        [Plugins.CoreExpr] ->
        CategoryStack Plugins.CoreExpr
      categorizeDataCon name e dc args =
        case (Plugins.getOccString dc, args) of
          ("(,)", Plugins.Type a : Plugins.Type b : rest) ->
            makeMaker2 makers (categorizeLambda name) e rest
              <=\< mkId makers
              $ Plugins.mkBoxedTupleTy [a, b]
          ("Left", Plugins.Type a : Plugins.Type b : rest) ->
            makeMaker1 makers (categorizeLambda name) rest =<\< mkInl makers a b
          ("Right", Plugins.Type a : Plugins.Type b : rest) ->
            makeMaker1 makers (categorizeLambda name) rest =<\< mkInr makers a b
          -- "Given a saturated constructor application /Con e1...en/, rewrite it to /abst (inline
          --  repr (Con e1...en))/, where /inline e/ tells GHC’s simplifier to inline the expression
          -- /e/." ⸻§9
          (_, _) -> do
            let nonTypeArgs = filter (not . Plugins.isTypeArg) args
                (binds, body) =
                  Plugins.collectBinders
                    (etaExpand (Plugins.dataConRepArity dc - length nonTypeArgs) e)
                bodyTy = Plugins.exprType body
            abst <- mkAbst makers bodyTy
            repr <- inlineHasRep =<\< mkRepr makers bodyTy
            -- NON-INDUCTIVE
            -- Here we expect `simplifyFun` to apply the `let`-substitution and
            -- case-of-known-constructor transformations.
            categorizeLambda name
              <=\< simplifyFun dflags [] . Plugins.mkLams binds . Plugins.App abst
              $ Plugins.App repr body
      -- `HasRep` is special to the plugin. We need to ensure the operations /don't/ inline in some
      -- cases and the must be /fully/ inlined in others. We wrap the methods in functions so we can
      -- prevent specialization, then here we inline twice -- the first inlines the function to the
      -- method, and the second inlines the method. This should work as long as the instances are
      -- defined correctly (which should be the case, since it's rare to have to define an instance
      -- that wouldn't be provided by `deriveHasRep`).
      inlineHasRep :: Plugins.CoreExpr -> CategoryStack Plugins.CoreExpr
      inlineHasRep = inlineCast <=\< mkInline <=\< mkInline
        where
          -- `inlineCast` is needed to deal with casts resulting from type family definitions. For
          -- example, when we inline `abst` for `KSum2 C () (TimedSensor (Msg MotorStatusMsg C) C)`,
          -- we expect to get something like
          --
          -- ```
          --   \a -> case a of (b, c) -> case c of (d, e) -> UnsafeSum2 b d e
          -- ```
          --
          -- But since we have the following type instance:
          --
          -- ```
          --   type instance Msg MotorStatusMsg f = CobsSensor (MotorStatusMsg f) f
          -- ```
          --
          -- Without `inlineCast`, what we get instead is
          --
          -- ```
          --   $fHasRepKSum2_$cabst `cast`
          --     (<Co:19> :: Rep (KSum2 C () (TimedSensor (CobsSensor (MotorStatusMsg C) C) C)) ->
          --                      KSum2 C () (TimedSensor (CobsSensor (MotorStatusMsg C) C) C)
          --             ~R# Rep (KSum2 C () (TimedSensor (Msg MotorStatusMsg C) C)) ->
          --                      KSum2 C () (TimedSensor (Msg MotorStatusMsg C) C)
          --     )
          -- ```
          --
          -- Because of this extra `cast`, `$fHasRepKSum2_$cabst` isn't inlined, causing
          -- infinite looping.
          --
          -- What `inlineCast` does is simply drop the `Coercion` and call `mkInline` again.
          -- This would cause core lint to complain (one doesn't simply drop `Coercion`s), but
          -- otherwise should be harmless.
          inlineCast :: Plugins.CoreExpr -> CategoryStack Plugins.CoreExpr
          inlineCast = \case
            Plugins.Cast e _ ->
              -- Here we try to inline `e`, and if `e` can't be inlined, just return it rather than
              -- throwing an error. Because for a type like `data Foo = Foo (C Word8)`, we have
              --
              -- ```
              --   $fHasRepFoo_$cabst = Foo `cast`
              --     (<Co:5> :: (C Word8 -> Foo) ~R# (Rep Foo -> Foo))
              -- ```
              --
              -- and since we can't inline `Foo`, we need to return it.
              mkInline' (const . pure) pure e
            other -> pure other
      -- This checks to see if the input is some application of an `Plugins.Id` that we know how to
      -- map directly to a categorical representation. See `findMaker` for the `Plugins.Id`s that we
      -- recognize here.
      interpretVocabulary ::
        Plugins.Var ->
        Plugins.CoreExpr ->
        Plugins.Var ->
        [Plugins.CoreExpr] ->
        CategoryStack Plugins.CoreExpr
      interpretVocabulary n expr name args =
        -- TODO: When doesn't a name have a module? What should we do in those cases?
        let (moduleName, varName) =
              first (fromMaybe "") . splitNameString . Plugins.varName $
                maybeTraceWith debug (thump "interpreting" . WithIdInfo) name
         in findMaker makers n name expr varName args moduleName

      handleExtraArgs m = handleAdditionalArgs m . categorizeLambda

      thump :: Plugins.Outputable a => Plugins.SDoc -> a -> String
      thump label term =
        renderSDoc dflags $
          Plugins.sep [label Plugins.<> ":", Plugins.nest 2 $ Plugins.ppr term]
      -- This is where we enumerate the applications of identifiers that have a categorical
      -- representation.
      --
      -- The general approach is this:
      -- 1. "apply" enough to get to a single morphism in the target category.
      --    - for most functions, this means do nothing, but ones like `fmap` and `either` take
      --      one or more morphisms in the target category before returning a morphism in the
      --      target category
      -- 2. call @maker/n/@ on it where /n/ is the number of parameters (curried in __Hask__, tupled
      --    in the target) in the final morphism.
      --    - e.g., @(`+`) :: a -> a -> a@ becomes @`plusV` :: c (a, a) a@, so you call @maker2@,
      --      not to be confused with @`fst` :: (a, b) -> a@ (becoming @`exlV` :: c (a, b) a@, which
      --      would call @maker1@.
      findMaker ::
        Makers ->
        -- | Lambda-bound var
        Plugins.Var ->
        -- | The var being interpreted
        Plugins.Var ->
        -- | The expression where the var being interpreted is the head
        Plugins.CoreExpr ->
        String ->
        [Plugins.CoreExpr] ->
        String ->
        CategoryStack Plugins.CoreExpr
      findMaker m@Makers {..} n target expr var args modu =
        fromMaybe
          ( -- If we can't find a matching case to interpret, we fall back through a few cases:
            --
            -- 1. try to interpret a specialized version
            -- 2. look for a separate categorization of the function
            -- 3. auto-interpret
            -- 4. inline the function
            maybe
              ( -- the checks whether we're categorizing this exact function. If so, we can't take
                -- advantage of `mkNative'` for separate categorization  because it'll loop.
                if funName /= Just var
                  then
                    withExceptT getLast $
                      withExceptT Last mkNative'
                        <!> withExceptT
                          Last
                          ( maybe
                              -- __FIXME__: This call to @`simplifyFun` []@ causes a lot of our
                              --            specialization woes. E.g., prior to this, we'll have an
                              --            expression containing `<*>`, `fmap`, etc., but after the
                              --            call (despite not passing `Rules`), we have
                              --           `$fApplicativeFoo_$c<*>`, `$fFunctorFoo_$cfmap`, etc.
                              --            However, if we /don't/ simplify here, then we get
                              --            complaints about missing dictionaries elsewhere.

                              (categorizeLambda n =<\< simplifyFun dflags [] =<\< mkInline expr)
                              (maker1 (dropWhile isTypeOrPred args))
                              =<\< tryAutoInterpret'
                          )
                  else
                    maybe
                      (categorizeLambda n =<\< simplifyFun dflags [] =<\< mkInline expr)
                      (maker1 (dropWhile isTypeOrPred args))
                      =<\< tryAutoInterpret'
              )
              pure
              =<\< interpretSpecialized
          )
          . (($ args) <=< Map.lookup (modu, var))
          $ Map.mapKeys (fromMaybe "" . TH.nameModule &&& TH.nameBase) makerMap
        where
          makerMap ::
            Map TH.Name ([Plugins.CoreExpr] -> Maybe (CategoryStack Plugins.CoreExpr))
          makerMap =
            makerMapFun
              dflags
              m
              n
              target
              expr
              cat
              var
              args
              modu
              categorizeFun
              (categorizeLambda n)

          maker1 = makeMaker1 m (categorizeLambda n)
          maker2 = makeMaker2 m (categorizeLambda n) expr

          mkNative' = do
            let tagTy = TyCoRep.LitTy . TyCoRep.StrTyLit . Plugins.mkFastString $ modu <> "." <> var
                f = fst $ applyTyAndPredArgs Plugins.Var (Plugins.Var target) args
            (argTy, resTy) <-
              maybe (throwE . pure $ NotFunTy f (Plugins.exprType f)) pure $
                Plugins.splitFunTy_maybe (Plugins.exprType f)
            maker1 (dropWhile isTypeOrPred args) =<\< mkNative tagTy argTy resTy

          interpretSpecialized :: CategoryStack (Maybe Plugins.CoreExpr)
          interpretSpecialized
            | "$fEq" `isPrefixOf` var = interpretEqSpecialized monoTy
            | "$fFoldable" `isPrefixOf` var = interpretFoldableSpecialized monoTy
            | "$fIntegral" `isPrefixOf` var = interpretIntegralSpecialized monoTy
            | "$fNum" `isPrefixOf` var = interpretNumSpecialized monoTy
            | "$fOrd" `isPrefixOf` var = interpretOrdSpecialized monoTy
            | otherwise = pure Nothing
            where
              monoTy =
                Plugins.exprType . fst $
                  applyTyAndPredArgs Plugins.Var (Plugins.Var target) args

          interpretEqSpecialized :: Plugins.Type -> CategoryStack (Maybe Plugins.CoreExpr)
          interpretEqSpecialized monoTy
            | rest <- dropWhile isTypeOrPred args,
              "$c==" `isSuffixOf` var = do
              ty <-
                maybe
                  (throwE . pure $ NotTyConApp "interpreting $c==" monoTy)
                  pure
                  (extractTypeFromFunTy [ArgTy] monoTy)
              pure <$> (maker2 rest =<\< mkEqual ty)
            | otherwise = pure Nothing

          interpretFoldableSpecialized :: Plugins.Type -> CategoryStack (Maybe Plugins.CoreExpr)
          interpretFoldableSpecialized monoTy
            | rest <- dropWhile isTypeOrPred args,
              "$cmaximum" `isSuffixOf` var = do
              (t, a) <-
                maybe
                  (throwE . pure $ NotTyConApp "interpreting $cmaximum" monoTy)
                  pure
                  (Plugins.splitAppTy_maybe =<< extractTypeFromFunTy [ArgTy] monoTy)
              pure <$> (maker1 rest =<\< mkMaximum t a)
            | rest <- dropWhile isTypeOrPred args,
              "$cminimum" `isSuffixOf` var = do
              (t, a) <-
                maybe
                  (throwE . pure $ NotTyConApp "interpreting $cminimum" monoTy)
                  pure
                  (Plugins.splitAppTy_maybe =<< extractTypeFromFunTy [ArgTy] monoTy)
              pure <$> (maker1 rest =<\< mkMinimum t a)
            | otherwise = pure Nothing

          interpretIntegralSpecialized :: Plugins.Type -> CategoryStack (Maybe Plugins.CoreExpr)
          interpretIntegralSpecialized monoTy
            | rest <- dropWhile isTypeOrPred args,
              "$cdiv" `isSuffixOf` var = do
              ty <-
                maybe
                  (throwE . pure $ NotTyConApp "interpreting $cdiv" monoTy)
                  pure
                  (extractTypeFromFunTy [ArgTy] monoTy)
              pure <$> (maker2 rest =<\< mkDiv ty)
            | rest <- dropWhile isTypeOrPred args,
              "$cmod" `isSuffixOf` var = do
              ty <-
                maybe
                  (throwE . pure $ NotTyConApp "interpreting $cmod" monoTy)
                  pure
                  (extractTypeFromFunTy [ArgTy] monoTy)
              pure <$> (maker2 rest =<\< mkMod ty)
            | otherwise = pure Nothing

          interpretNumSpecialized :: Plugins.Type -> CategoryStack (Maybe Plugins.CoreExpr)
          interpretNumSpecialized monoTy
            | rest <- dropWhile isTypeOrPred args,
              "$c+" `isSuffixOf` var = do
              ty <-
                maybe
                  (throwE . pure $ NotTyConApp "interpreting $c+" monoTy)
                  pure
                  (extractTypeFromFunTy [ArgTy] monoTy)
              pure <$> (maker2 rest =<\< mkPlus ty)
            | rest <- dropWhile isTypeOrPred args,
              "$cfromInteger" `isSuffixOf` var = do
              ty <-
                maybe
                  (throwE . pure $ NotTyConApp "interpreting $cfromInteger" monoTy)
                  pure
                  (extractTypeFromFunTy [ResTy] monoTy)
              pure <$> (maker1 rest =<\< mkFromInteger ty)
            | otherwise = pure Nothing

          interpretOrdSpecialized :: Plugins.Type -> CategoryStack (Maybe Plugins.CoreExpr)
          interpretOrdSpecialized monoTy
            | rest <- dropWhile isTypeOrPred args,
              "$c<" `isSuffixOf` var = do
              ty <-
                maybe
                  (throwE . pure $ NotTyConApp "interpreting $c<" monoTy)
                  pure
                  (extractTypeFromFunTy [ArgTy] monoTy)
              pure <$> (maker2 rest =<\< mkLT ty)
            | rest <- dropWhile isTypeOrPred args,
              "$c>" `isSuffixOf` var = do
              ty <-
                maybe
                  (throwE . pure $ NotTyConApp "interpreting $c>" monoTy)
                  pure
                  (extractTypeFromFunTy [ArgTy] monoTy)
              pure <$> (maker2 rest =<\< mkGT ty)
            | rest <- dropWhile isTypeOrPred args,
              "$c<=" `isSuffixOf` var = do
              ty <-
                maybe
                  (throwE . pure $ NotTyConApp "interpreting $c<=" monoTy)
                  pure
                  (extractTypeFromFunTy [ArgTy] monoTy)
              pure <$> (maker2 rest =<\< mkLE ty)
            | rest <- dropWhile isTypeOrPred args,
              "$c>=" `isSuffixOf` var = do
              ty <-
                maybe
                  (throwE . pure $ NotTyConApp "interpreting $c>=" monoTy)
                  pure
                  (extractTypeFromFunTy [ArgTy] monoTy)
              pure <$> (maker2 rest =<\< mkGE ty)
            | rest <- dropWhile isTypeOrPred args,
              "$cmax" `isSuffixOf` var = do
              ty <-
                maybe
                  (throwE . pure $ NotTyConApp "interpreting $cmax" monoTy)
                  pure
                  (extractTypeFromFunTy [ArgTy] monoTy)
              pure <$> (maker2 rest =<\< mkMax ty)
            | rest <- dropWhile isTypeOrPred args,
              "$cmin" `isSuffixOf` var = do
              ty <-
                maybe
                  (throwE . pure $ NotTyConApp "interpreting $cmin" monoTy)
                  pure
                  (extractTypeFromFunTy [ArgTy] monoTy)
              pure <$> (maker2 rest =<\< mkMin ty)
            | otherwise = pure Nothing

          tryAutoInterpret' =
            maybeTraceWith debug (const $ "Automatically interpreted " <> dbg target)
              . tryAutoInterpret
                buildDictionary
                cat
                (Plugins.exprType . fst $ applyTyAndPredArgs Plugins.Var (Plugins.Var target) args)
                target
              $ takeWhile isTypeOrPred args

      dbg :: Plugins.Outputable a => a -> String
      dbg = renderSDoc dflags . Plugins.ppr

      -- Try `mkConst'`. If the required `ConstCat` instance doesn't exist, retry
      -- `categorizeLambda'` with `IgnoreConst`.
      --
      -- The reason to do this is because sometimes we try to categorize things like
      -- `\name -> (x :: ManualGains (C Double -> C Double))`. Here the type of
      -- `x` doesn't have a `ConstCat` instance because it contains a function type, so
      -- we retry categorizing it normally rather than via `const`.
      tryMkConst name expr m =
        ExceptT $
          runExceptT (mkConst' m (Plugins.varType name) expr) >>= \case
            Right res -> pure (Right res)
            Left (CouldNotBuildDictionary _ _ (TypecheckFailure _ :| []) :| []) ->
              runExceptT $ categorizeLambda' IgnoreConst name expr
            Left otherErrs -> pure (Left otherErrs)

      -- §3: @constFun f = curry (f . exr)@
      mkConstFun ::
        Plugins.Type -> Plugins.Type -> Plugins.CoreExpr -> Makers -> CategoryStack Plugins.CoreExpr
      mkConstFun ignored a f m =
        curryCat m =<\< joinD (composeCat m <$> categorizeFun f <*\> mkExr m ignored a)
      primConMap = PrimOp.mkConMap $ baseIntConstructors baseIdentifiers
      -- Creates a new `Plugins.Id` of `Plugins.Type` that is unique in the `Plugins.VarSet` and is
      -- prefixed with `String`.
      freshId :: Plugins.VarSet -> String -> Plugins.Type -> Plugins.Id
      freshId used nm =
        Plugins.uniqAway (Plugins.mkInScopeSet used)
          . Plugins.mkSysLocal (Plugins.fsLit nm) (Unique.mkBuiltinUnique 17)

      mkConst' m a expr
        | Plugins.isPredTy (Plugins.exprType expr) =
          -- First try `ConstraintCat`. Target categories like `C.Cat` can't use `ConstCat`
          -- for constraints, because `ConstCat C.Cat a` requires `ToTargetOb a`, and there's no
          -- `ToTargetOb a` if `a` is a constraint.
          catchE
            (use mkConstraint)
            ( \case
                -- Target categories like `Hask` doesn't have `ConstraintCat` instance, because
                -- `Hask`'s kind is `* -> * -> *`. For such categories we need to use `ConstCat`.
                CouldNotBuildDictionary _ _ (TypecheckFailure _ :| []) :| [] -> use mkConst
                otherErrs -> throwE otherErrs
            )
        | otherwise = use mkConst
        where
          use mk = Plugins.App <$> mk m a (Plugins.exprType expr) <*\> pure expr

      -- This can only handle monomorphic recursion. The @tyArgs@ provided must match for each
      -- recursive call.
      unfix :: Plugins.Id -> [Plugins.Type] -> Plugins.CoreExpr -> CategoryStack Plugins.CoreExpr
      unfix name tyArgs expr = do
        let exprTyBinders = fst $ Plugins.collectTyBinders expr
            exprMono = Plugins.mkCoreApps expr (Plugins.Type <$> tyArgs)
            monoTy = Plugins.exprType exprMono
            recursive = freshId (Plugins.exprFreeVars expr) "rec" monoTy
            sameTyVar :: (Plugins.Var, Plugins.Type) -> Bool
            sameTyVar (var, TyCoRep.TyVarTy var') = var == var'
            sameTyVar _ = False
        unless (length exprTyBinders == length tyArgs) $
          throwE . pure $ InvalidUnfixTyArgs name exprTyBinders tyArgs
        newExpr <-
          -- I don't know why @subst@ doesn't do what we want, but this does. It uses
          -- `Plugins.substExpr` to apply (potentially multiple) substitutions over an
          -- expression. In theory, this does the same thing, but for our fixed-point
          -- rewrites, @subst@ doesn't result in our recursive references being replaced.
          -- Instead, we use `transform` to traverse the structure and replaces any matching
          -- `Plugin.Var` with our @new@ expression.
          transformM
            ( \case
                (Plugins.collectArgs -> (Plugins.Var f, spanTypes -> (recCallTyArgs, [])))
                  | f == name,
                    length recCallTyArgs == length exprTyBinders ->
                    if all sameTyVar (zip exprTyBinders recCallTyArgs)
                      then pure (Plugins.Var recursive)
                      else
                        throwE . pure $
                          UnsupportedPolymorphicRecursion name exprTyBinders recCallTyArgs
                other -> pure other
            )
            exprMono
        if name `isCalledIn` newExpr
          then throwE . pure $ FailureToUnfix name expr newExpr
          else pure . mkFixH makers monoTy $ Plugins.Lam recursive newExpr

      maybeUnfix ::
        Plugins.Id ->
        [Plugins.Type] ->
        Plugins.CoreExpr ->
        CategoryStack Plugins.CoreExpr
      maybeUnfix name tyArgs expr
        | name `isCalledIn` expr =
          ExceptT $
            runExceptT (unfix name tyArgs expr) <&> \case
              Right e -> Right e
              -- When `unfix` fails with `FailureToUnfix`, we proceed without unfixing, rather than
              -- throwing the error, because proceeding may eventually succeed, although it may also
              -- lead to looping.
              -- TODO: log the errors.
              Left (FailureToUnfix {} :| []) -> Right $ Plugins.mkTyApps expr tyArgs
              Left other -> Left other
        | otherwise = pure $ Plugins.mkTyApps expr tyArgs

      -- This is the behavior for the `Plugins.BuiltinRule` defined for `GHC.Exts.inline`. We
      -- duplicate it here rather than generating a call to `GHC.Exts.inline` and trampolining. This
      -- gives us very precise control over inlining.
      mkInline :: Plugins.CoreExpr -> CategoryStack Plugins.CoreExpr
      mkInline =
        mkInline'
          (\e unf -> throwE . pure . UninlinedExpr e $ Just unf)
          (throwE . pure . flip UninlinedExpr Nothing)

      mkInline' ::
        (Plugins.CoreExpr -> Plugins.Unfolding -> CategoryStack Plugins.CoreExpr) ->
        (Plugins.CoreExpr -> CategoryStack Plugins.CoreExpr) ->
        Plugins.CoreExpr ->
        CategoryStack Plugins.CoreExpr
      mkInline' onMissingUnfolding onNonVar expr = do
        lets <- lift ask
        createdDictVars <- Map.fromListWith const . fmap fst <$> getCreatedDictVars
        let varUnfoldingFun v = case Map.lookup v createdDictVars of
              Nothing -> Plugins.Var v
              Just e -> e
        go
          -- If we don't have an unfolding, try again after simplifying. We use `Rules` here
          -- because it's generally the case that we're missing an unfolding due to the
          -- `Plugins.Var` being a method, so we need it to specialize to allow it to be inlined.
          ( const
              -- If the simplified one isn't a `Plugins.App` of a `Plugins.Var`, at least we
              -- know the expression has changed, so we can continue trying to categorize it.
              . ( go onMissingUnfolding pure lets
                    . uncurry Plugins.mkCoreApps
                    <=\< bitraverse (simplifyFun dflags [Rules]) pure
                      . uncurry (applyTyAndPredArgs varUnfoldingFun)
                      . Plugins.collectArgs
                )
          )
          onNonVar
          lets
          expr
        where
          -- actually try to inline, accepting handlers for failure cases
          go onMissingUnfolding' onNonVar' lets = \case
            e@(Plugins.collectArgs -> (Plugins.Var f, args)) ->
              let unf =
                    Plugins.realIdUnfolding $ maybeTraceWith debug (thump "inlining" . WithIdInfo) f
                  (tyArgs, remainingArgs) = spanTypes args
               in maybe
                    (onMissingUnfolding' e unf)
                    (fmap (`Plugins.mkCoreApps` remainingArgs) . maybeUnfix f tyArgs)
                    $ Map.lookup f lets <!> Plugins.maybeUnfoldingTemplate unf
            e -> onNonVar' e

      handlePrimOps label name expr resultType = do
        boxedOps <-
          PrimOp.replace
            dflags
            debug
            baseMakers
            additionalBoxers
            baseIdentifiers
            getUnique
            resultType
            $ maybeTraceWith debug (\x -> "going primitive> " <> label <> ": " <> dbg x) expr
        _ <-
          -- __FIXME__: In some cases this check should not be performed; see SW-3416
          PrimOp.checkForUnboxedVars boxedOps $
            maybeTraceWith
              debug
              (\x -> "primitive> " <> label <> ": " <> dbg x)
              boxedOps
        categorizeLambda name boxedOps

data MakeOrIgnoreConst = MakeConst | IgnoreConst

-- | Returns the longest prefix of 'Plugin.Type's, and the rest.
spanTypes :: [Plugins.CoreExpr] -> ([Plugins.Type], [Plugins.CoreExpr])
spanTypes = \case
  Plugins.Type x : xs -> let (ys, zs) = spanTypes xs in (x : ys, zs)
  xs -> ([], xs)

getUnique :: CategoryStack Plugins.Unique
getUnique = lift $ do
  us <- gets csUniqSupply
  let (u, us') = Plugins.takeUniqFromSupply us
  modify $ \s -> s {csUniqSupply = us'}
  pure u

getNewUniqueSupply :: CategoryStack Plugins.UniqSupply
getNewUniqueSupply = lift $ do
  us <- gets csUniqSupply
  let (us', us'') = Plugins.splitUniqSupply us
  modify $ \s -> s {csUniqSupply = us'}
  pure us''

uniquifyVarName :: Plugins.Var -> CategoryStack Plugins.Var
uniquifyVarName v =
  Plugins.setVarName v . Plugins.setNameUnique (Plugins.varName v) <$> getUnique

-- This runs the GHC simplifier as minimally as possible. We want to be careful with this,
-- because it's a lot of magic that is out of our control. We should generally wrap the smallest
-- expression we can.
simplifyFun ::
  Plugins.DynFlags ->
  [Transformation] ->
  Plugins.CoreExpr ->
  CategoryStack Plugins.CoreExpr
simplifyFun dflags trans e0 = do
  -- Here we perform `subst` directly, rather than make let-bindings and let the
  -- `Plugins.Let` case handle them. The reason is that the latter doesn't perform some
  -- specializations that we want. For instance, there was a case where the latter failed to
  -- specialize `(.&&)` into `$fKAndC_$c.&&`.
  e <- Map.foldrWithKey' (curry (subst . pure)) e0 <$> lift ask
  uniqS <- getNewUniqueSupply
  Plugins.liftIO $ simplifyExpr dflags (Set.fromList trans) uniqS e

-- Replaces all occurences of each `Plugins.Id` with the corresponding `Plugins.CoreExpr`.
subst :: [(Plugins.Id, Plugins.CoreExpr)] -> Plugins.CoreExpr -> Plugins.CoreExpr
subst =
  Plugins.substExpr (Plugins.text "subst")
    . foldr add Plugins.emptySubst
    . filter (not . Plugins.isDeadBinder . fst)
  where
    add (v, new) sub = Plugins.extendIdSubst sub v new

isDictDataCon :: Plugins.DataCon -> Bool
isDictDataCon dc = modu == Just "Data.Constraint" && occ == "Dict"
  where
    (modu, occ) = splitNameString (Plugins.dataConName dc)

-- | To check whether the type is Dict from Data.Constraint or Barbies.Internal.Dicts.
--
-- TODO: Ideally, the check for Barbies.Internal.Dicts is not required since they are equal.
--       But in current implementation, this subt/no subst decision should happen earlier than
--       inlining representation, we need to deal with Barbies.Internal.Dicts. Later, we will
--       seek for a better treatment for such module re-exports in subst decision.
--       See SW-3418.
isDictOrBarbiesDictTyCon :: Plugins.TyCon -> Bool
isDictOrBarbiesDictTyCon tc =
  (modu == Just "Barbies.Internal.Dicts" || modu == Just "Data.Constraint") && occ == "Dict"
  where
    (modu, occ) = splitNameString (Plugins.tyConName tc)

-- | Find a target type in the given expr. If the expr's type equals the target
-- type, then we are done. Otherwise, if the expr has a boxed tuple type, we
-- descend into its components.
--
-- > findTupleInType
-- >   (Ord Double)
-- >   (expr :: ((Eq Double, Ord Double), Show Double))
-- >   == snd (fst expr)
findTypeInTuple ::
  Makers ->
  Plugins.Type ->
  Plugins.CoreExpr ->
  CategoryStack (Maybe Plugins.CoreExpr)
findTypeInTuple makers target (id &&& Plugins.exprType -> (expr, ty))
  | target `Plugins.eqType` ty = pure $ Just expr
  -- If expr's type is a 2-tuple, search in its first and second components.
  --
  -- __NB__: the default `AllB` types are always nested 2-tuples, so this
  -- works for default `AllB` types, as well as hand-written `AllB` types that
  -- are nested 2-tuples.
  --
  -- If we for some reason need an `AllB` type that is a `k`-tuple where `k > 2`,
  -- then we'd need to add the corresponding projection operations to `HaskOps`,
  -- (e.g., `fst3`, `snd3` and `thd3` for 3-tuples). Or, we can add conversion
  -- operations like `tuple3ToTuple2 :: forall a b c. (a, b, c) -> ((a, b), c)`.
  | Just (tycon, [_, _]) <- Plugins.splitTyConApp_maybe ty,
    Plugins.isBoxedTupleTyCon tycon =
    mkFst makers expr >>= findTypeInTuple makers target >>= \case
      Just res -> pure (Just res)
      Nothing -> mkSnd makers expr >>= findTypeInTuple makers target
  | otherwise = pure Nothing

-- | check if the expression is `proj . proj . proj ... . proj $ var` form.
hasOnlyProjections :: (Plugins.Var -> Bool) -> Plugins.CoreExpr -> Bool
hasOnlyProjections p = \case
  Plugins.Var v -> p v
  Plugins.App (Plugins.collectArgs -> (Plugins.Var head, [Plugins.Type _, Plugins.Type _])) arg ->
    isProjectionFun head && hasOnlyProjections p arg
  _ -> False
  where
    isProjectionFun :: Plugins.Id -> Bool
    isProjectionFun = (`elem` ["fst", "snd"]) . Plugins.occNameString . Plugins.occName

isTypeOrPred :: Plugins.CoreExpr -> Bool
isTypeOrPred expr = Plugins.isTypeArg expr || Plugins.isPredTy (Plugins.exprType expr)

-- | Given a function and a list of args, apply the type args and constraint args,
-- until we see an arg that is not a type or constraint arg.
applyTyAndPredArgs ::
  -- | What to do when we encounter a pred var. Sometimes a pred var needs to be
  -- replaced by its unfolding in order to be applied.
  (Plugins.Var -> Plugins.CoreExpr) ->
  Plugins.CoreExpr ->
  [Plugins.CoreExpr] ->
  (Plugins.CoreExpr, [Plugins.CoreExpr])
applyTyAndPredArgs varUnfoldingFun fun = first (Plugins.mkCoreApps fun . fmap f) . span isTypeOrPred
  where
    f :: Plugins.CoreExpr -> Plugins.CoreExpr
    f = \case
      Plugins.Var v -> varUnfoldingFun v
      other -> other

data Direction = ArgTy | ResTy

extractTypeFromFunTy :: [Direction] -> Plugins.Type -> Maybe Plugins.Type
extractTypeFromFunTy [] ty = pure ty
extractTypeFromFunTy (d : ds) ty = do
  (argTy, resTy) <- Plugins.splitFunTy_maybe ty
  case d of
    ArgTy -> extractTypeFromFunTy ds argTy
    ResTy -> extractTypeFromFunTy ds resTy

data LetOrCase
  = Let Plugins.CoreBind
  | SingleAltCase Plugins.CoreExpr Plugins.Var Plugins.Type Plugins.AltCon [Plugins.Var]

-- | Like `Plugins.collectBinders`, but sees through `Plugins.Let` and `Plugins.Case` with
-- a single `Plugins.Alt`. The first component of the result is the `Plugins.Let`s and the
-- `Plugins.Case`s that it sees through.
collectNestedBinders :: Plugins.CoreExpr -> ([LetOrCase], [Plugins.Var], Plugins.CoreExpr)
collectNestedBinders = \case
  Plugins.Let b e -> first3 (Let b :) (collectNestedBinders e)
  Plugins.Case scrut binder typ [(altCon, caseBinders, rhs)] ->
    first3 (SingleAltCase scrut binder typ altCon caseBinders :) (collectNestedBinders rhs)
  e -> let (vars, body) = Plugins.collectBinders e in ([], vars, body)

addLetsAndCases :: [LetOrCase] -> Plugins.CoreExpr -> Plugins.CoreExpr
addLetsAndCases = flip . foldr $ \x e -> case x of
  Let b -> Plugins.Let b e
  SingleAltCase scrut binder typ altCon caseBinders ->
    Plugins.Case scrut binder typ [(altCon, caseBinders, e)]

-- | Get the dictionary vars created during `Kitty.Plugin.Core.BuildDictionary.buildDictionary`
-- and their unfoldings.
getCreatedDictVars :: CategoryStack [((Plugins.Var, Plugins.CoreExpr), Int)]
getCreatedDictVars =
  mapMaybe
    ( \entry ->
        fmap ((dceVar entry, dceDict entry),) (dceIdx entry)
    )
    <$> lift (gets (Map.elems . csDictCache))

markNoInline :: Plugins.Var -> Plugins.Var
markNoInline v = Plugins.setIdInfo v $ Plugins.setInlinePragInfo (Plugins.idInfo v) inlineInfo
  where
    inlineInfo =
      (Plugins.inlinePragInfo $ Plugins.idInfo v)
        { Plugins.inl_act = Plugins.NeverActive
        }

markBindNoInline :: Plugins.CoreBind -> Plugins.CoreBind
markBindNoInline = \case
  Plugins.NonRec v rhs -> Plugins.NonRec (markNoInline v) rhs
  other -> other
