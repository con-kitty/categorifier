{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Categorifier.GHC.Core
  ( module Coercion,
    module CoreArity,
    module CoreFVs,
    module CoreMonad,
    module CoreStats,
    module CoreSubst,
    module CoreSyn,
    module CoreUnfold,
    module CoreUtils,
    module DataCon,
    module FamInstEnv,
    module MkCore,
    module OccurAnal,
    module Predicate,
    module SimplCore,
    module Simplify,
    module TyCoRep,
    module TyCon,
    module Type,
    pattern Alt,
    Transformation (..),
    Unpretty (..),
    WithType (..),
    funTy,
    invisFunArg,
    localId,
    properFunTy,
    simplifyExpr,
    simplifyExpr',
    splitFunTys,
    splitFunTy_maybe,
    substExpr,
  )
where

import qualified Categorifier.GHC.Builtin as Builtin
import qualified Categorifier.GHC.Driver as Driver
import qualified Categorifier.GHC.Types as Types
import Categorifier.GHC.Utils ((<+>))
import qualified Categorifier.GHC.Utils as Utils
import Control.Monad ((<=<))
import Data.Bifunctor (first)
import Data.Set (Set)
import qualified Data.Set as Set
#if MIN_VERSION_ghc(9, 0, 0)
#if MIN_VERSION_ghc(9, 2, 0)
import GHC.Core as CoreSyn hiding (Alt)
import qualified GHC.Core as CoreSyn
#else
import GHC.Core as CoreSyn
#endif
import GHC.Core.Coercion as Coercion hiding (substCo, substCoVarBndr)
import GHC.Core.DataCon as DataCon
import GHC.Core.FamInstEnv as FamInstEnv
import GHC.Core.FVs as CoreFVs
import GHC.Core.Make as MkCore
import GHC.Core.Opt.Arity as CoreArity
import GHC.Core.Opt.Monad as CoreMonad hiding (CaseOfCase)
import GHC.Core.Opt.OccurAnal as OccurAnal
import GHC.Core.Opt.Pipeline as SimplCore hiding (simplifyExpr)
import qualified GHC.Core.Opt.Pipeline as SimplCore
import GHC.Core.Opt.Simplify.Env as SimplEnv
import GHC.Core.Opt.Simplify.Monad as SimplMonad
import GHC.Core.Opt.Simplify as Simplify
import GHC.Core.Predicate as Predicate
import GHC.Core.Stats as CoreStats
import GHC.Core.Subst as CoreSubst hiding (substExpr)
import qualified GHC.Core.Subst as CoreSubst
import GHC.Core.TyCo.Rep as TyCoRep
import GHC.Core.TyCon as TyCon
import GHC.Core.Type as Type hiding
  ( extendCvSubst,
    extendTvSubst,
    extendTvSubstList,
    isInScope,
    splitFunTys,
    splitFunTy_maybe,
    substTy,
    substTyVarBndr,
  )
import qualified GHC.Core.Type as Type
import GHC.Core.Unfold as CoreUnfold
import GHC.Core.Utils as CoreUtils
import GHC.Plugins () -- to get the orphan @instance `MonadThings` `CoreM`@
#else
import Coercion hiding (substCo, substCoVarBndr)
import CoreArity
import CoreFVs
import CoreMonad hiding (CaseOfCase)
import CoreStats
import CoreSubst hiding (substExpr)
import qualified CoreSubst
import CoreSyn
import CoreUnfold
import CoreUtils
import DataCon
import FamInstEnv
import GhcPlugins () -- to get the orphan @instance `MonadThings` `CoreM`@
import MkCore
import OccurAnal
#if MIN_VERSION_ghc(8, 10, 0)
import Predicate
#else
import Id as Predicate hiding (mkSysLocal)
#endif
import SimplCore hiding (simplifyExpr)
import qualified SimplCore
import SimplEnv
import SimplMonad
import Simplify
import TyCoRep hiding
  ( extendCvSubst,
    extendTvSubst,
    extendTvSubstList,
    isInScope,
    substCo,
    substCoVarBndr,
    substTy,
    substTyVarBndr,
  )
import TyCon
import Type hiding
  ( extendCvSubst,
    extendTvSubst,
    extendTvSubstList,
    isInScope,
    splitFunTys,
    splitFunTy_maybe,
    substTy,
    substTyVarBndr,
  )
import qualified Type hiding
  ( extendCvSubst,
    extendTvSubst,
    extendTvSubstList,
    isInScope,
    substTy,
    substTyVarBndr,
  )
#endif

#if MIN_VERSION_ghc(9, 2, 0)
pattern Alt :: AltCon -> [b] -> Expr b -> CoreSyn.Alt b
pattern Alt x y z = CoreSyn.Alt x y z
#else
pattern Alt :: AltCon -> [b] -> Expr b -> (AltCon, [b], Expr b)
pattern Alt x y z = (x, y, z)
#endif
{-# COMPLETE Alt #-}

-- | This is the type for @(->)@ when you want to apply it to types of kind `Data.Kind.Type`. It's
--   easy to build this incorrectly and Core won't tell you that you have, so use this instead.
properFunTy :: Type
#if MIN_VERSION_ghc(9, 0, 0)
properFunTy = mkTyConApp funTyCon [Builtin.manyDataConTy, Builtin.liftedRepTy, Builtin.liftedRepTy]
#else
properFunTy = mkTyConApp funTyCon [Builtin.liftedRepTy, Builtin.liftedRepTy]
#endif

-- | Similar to `properFunTy`, but fully-applied, inferring the kind arguments from the kinds of the
--   provided types.
funTy :: Type -> Type -> Type
#if MIN_VERSION_ghc(9, 0, 0)
funTy a b = mkTyConApp funTyCon [Builtin.manyDataConTy, typeKind a, typeKind b, a, b]
#else
funTy a b = mkTyConApp funTyCon [typeKind a, typeKind b, a, b]
#endif

-- | Like `splitFunTy_maybe`, but only returns `Just` if the argument is invisible.
--
--    When applying `categorifyLambda` to `\(x :: X) -> ($fFoo :: Foo Bar)` where
--    `Foo` is a typeclass and `$fFoo` is its dictionary, the plugin would invoke
--    `mkConst' X (Foo Bar)`, which ends up applying `onDicts` to
--
--    ```
--    (ConstCat Hask (Foo Bar), Ok Hask X) => Foo Bar -> Hask X (Foo Bar)
--    ```
--
--    Here we need two dictionaries, not three, i.e., it should only proceed if the
--    arrow is "=>", not "->".
invisFunArg :: Type -> Maybe Type
#if MIN_VERSION_ghc(8, 10, 0)
invisFunArg = \case
  ty | Just ty' <- coreView ty -> invisFunArg ty'
#if MIN_VERSION_ghc(9, 0, 0)
  TyCoRep.FunTy InvisArg _ arg _ -> Just arg
#else
  TyCoRep.FunTy InvisArg arg _ -> Just arg
#endif
  _ -> Nothing
#else
invisFunArg =
  (\(arg, _) -> if isPredTy arg then pure arg else Nothing) <=< splitFunTy_maybe
#endif

-- | Make a unique identifier for a specified type, using a provided name.
localId :: InScopeEnv -> String -> Type -> Types.Id
#if MIN_VERSION_ghc(9, 0, 0)
localId (inScopeSet, _) str ty =
  Types.uniqAway inScopeSet $ Types.mkLocalId (Types.stringToName str) ty ty
#else
localId (inScopeSet, _) str = Types.uniqAway inScopeSet . Types.mkLocalId (Types.stringToName str)
#endif

-- | This is the simplifier we apply surgically to expressions that should be re-written before
--   continuing categorification.
--
--   These are passes that we have control of in the simplifier.
data Transformation = CaseOfCase | EtaExpand | Inline | Rules
  deriving (Eq, Ord)

simplifyExpr :: Driver.HscEnv -> CoreExpr -> IO CoreExpr
#if MIN_VERSION_ghc(9, 0, 0)
simplifyExpr = SimplCore.simplifyExpr
#else
simplifyExpr = SimplCore.simplifyExpr . Driver.hsc_dflags
#endif

-- | The GHC API doesn't give very much control over the simplifier. The only way to use it is to
--   trampoline out of your plugin pass to let an external `CoreToDo` simplify for you or to
--   call `simplifyExpr`, which doesn't give you even the same control that the `CoreToDo`
--   simplifier does.
--
--   Here we try to give more control over how the simplifier is applied, making it possible to at
--   least call `simplifyExpr'` with a `SimplMode`. We also try to provide access to specific
--   transformations so that Core plugins can do fine-grained simplification internally.
--
--  __NB__: Unfortunately, many of the useful operations aren't exposed in the API, so this
--          duplicates them as necessary, with comments explaining which are abstracted (so we can
--          try to keep them in sync with upstream changes).
--
-- | Like `simplifyExpr`, except it takes a `Set` of `Transformation`s to apply. This
--   allows us to control (as much as possible) how the simplifier is applied in each call.
--
--   No matter how we restrict it, this simplification applies a ton of transformations that are
--   hard to separate out (future work?). This one disables everything we can by default, then
--   allows us to turn on individual transformations. There are also some other transformations we
--   need that are implicit in here. E.g. specialization and case of known constructor.
simplifyExpr' ::
  Utils.Logger ->
  Driver.DynFlags ->
  Set Transformation ->
  Types.UniqSupply ->
  CoreExpr ->
  IO CoreExpr
#if MIN_VERSION_ghc(9, 2, 0)
simplifyExpr' logger dflags trans _ expr =
  fmap fst . initSmpl logger dflags emptyRuleEnv emptyFamInstEnvs (exprSize expr) $
    doSimplify
      1
      SimplMode
        { sm_case_case = CaseOfCase `Set.member` trans,
          sm_uf_opts = defaultUnfoldingOpts,
          sm_pre_inline = Inline `Set.member` trans,
          sm_logger = logger,
          sm_dflags = dflags,
          sm_eta_expand = EtaExpand `Set.member` trans,
          sm_inline = Inline `Set.member` trans,
          sm_names = ["categorify internal"],
          sm_phase = Types.Phase 1,
          sm_rules = Rules `Set.member` trans -- this improves specialisation
        }
      expr
#else
simplifyExpr' _ dflags trans uniqS expr =
  fmap fst . initSmpl dflags emptyRuleEnv emptyFamInstEnvs uniqS (exprSize expr) $
    doSimplify
      1
      SimplMode
        { sm_case_case = CaseOfCase `Set.member` trans,
          sm_dflags = dflags,
          sm_eta_expand = EtaExpand `Set.member` trans,
          sm_inline = Inline `Set.member` trans,
          sm_names = ["categorify internal"],
          sm_phase = Types.Phase 1,
          sm_rules = Rules `Set.member` trans -- this improves specialisation
        }
      expr
#endif

-- | Designed to be like `CoreDoSimplify`, but applicable to arbitrary `CoreExpr`s.
doSimplify :: Int -> SimplMode -> CoreExpr -> SimplM CoreExpr
doSimplify passCount mode expr =
  foldr (<=<) pure (replicate passCount $ simplExpr simplEnv . occurAnalyseExpr) expr
  where
    simplEnv' = mkSimplEnv mode
    simplEnv =
      -- __NB__: We have to extend the scope here so that any fresh Uniques don't conflict with vars
      --         free in @expr@. See https://gitlab.haskell.org/ghc/ghc/-/issues/21321 for details.
      simplEnv' {seInScope = Types.extendInScopeSetSet (seInScope simplEnv') $ exprFreeVars expr}

splitFunTys :: Type -> ([Type], Type)
#if MIN_VERSION_ghc(9, 0, 0)
splitFunTys = first (scaledThing <$>) . Type.splitFunTys
#else
splitFunTys = Type.splitFunTys
#endif

{-# ANN splitFunTy_maybe ("HLint: ignore Use camelCase" :: String) #-}
splitFunTy_maybe :: Type -> Maybe (Type, Type)
#if MIN_VERSION_ghc(9, 0, 0)
splitFunTy_maybe = fmap (\(_, b, c) -> (b, c)) . Type.splitFunTy_maybe
#else
splitFunTy_maybe = Type.splitFunTy_maybe
#endif

substExpr :: Subst -> CoreExpr -> CoreExpr
#if MIN_VERSION_ghc(9, 0, 0)
substExpr = CoreSubst.substExpr
#else
substExpr = CoreSubst.substExpr (Utils.text "categorifier")
#endif

-- | Generic wrapper to make a pretty printer that's less ... pretty (and more useful for people
--   looking at the code).
newtype Unpretty a = Unpretty a

instance Utils.Outputable (Unpretty Coercion) where
  ppr (Unpretty coercion) = case coercion of
#if MIN_VERSION_ghc(8, 8, 0)
    TyCoRep.Refl ty -> "Refl" <+> Utils.ppr ty
    TyCoRep.GRefl role ty mco -> "GRefl" <+> Utils.ppr role <+> Utils.ppr ty <+> nestedCo mco
#else
    TyCoRep.CoherenceCo co kCo -> "CoherenceCo" <+> nestedCo co <+> nestedCo kCo
    TyCoRep.Refl role ty -> "Refl" <+> Utils.ppr role <+> Utils.ppr ty
#endif
    TyCoRep.TyConAppCo role tyCon coes ->
      "TyConAppCo" <+> Utils.ppr role <+> Utils.ppr tyCon <+> Utils.ppr (Unpretty <$> coes)
    TyCoRep.AppCo co coN -> "AppCo" <+> nestedCo co <+> nestedCo coN
    TyCoRep.ForAllCo tyCoVar kCo co ->
      "ForAllCo" <+> Utils.ppr tyCoVar <+> nestedCo kCo <+> nestedCo co
#if MIN_VERSION_ghc(9, 0, 0)
    TyCoRep.FunCo role coN co co' ->
      "FunCo" <+> Utils.ppr role <+> nestedCo coN <+> nestedCo co <+> nestedCo co'
#else
    TyCoRep.FunCo role co co' -> "FunCo" <+> Utils.ppr role <+> nestedCo co <+> nestedCo co'
#endif
    TyCoRep.CoVarCo coVar -> "CoVarCo" <+> Utils.ppr coVar
    TyCoRep.AxiomInstCo coA brI coes ->
      "AxiomInstCo" <+> Utils.ppr coA <+> Utils.ppr brI <+> Utils.ppr (Unpretty <$> coes)
    TyCoRep.AxiomRuleCo coARule coes ->
      "AxiomRuleCo" <+> Utils.ppr coARule <+> Utils.ppr (Unpretty <$> coes)
    TyCoRep.UnivCo prov role ty ty' ->
      "UnivCo" <+> Utils.ppr prov <+> Utils.ppr role <+> Utils.ppr ty <+> Utils.ppr ty'
    TyCoRep.SymCo co -> "SymCo" <+> nestedCo co
    TyCoRep.TransCo co co' -> "TransCo" <+> nestedCo co <+> nestedCo co'
#if MIN_VERSION_ghc(8, 6, 0)
    TyCoRep.NthCo rule i co -> "NthCo" <+> Utils.ppr rule <+> Utils.ppr i <+> nestedCo co
#else
    TyCoRep.NthCo i co -> "NthCo" <+> Utils.ppr i <+> nestedCo co
#endif
    TyCoRep.LRCo lr coN -> "LRCo" <+> Utils.ppr lr <+> nestedCo coN
    TyCoRep.InstCo co coN -> "InstCo" <+> nestedCo co <+> nestedCo coN
    TyCoRep.KindCo co -> "KindCo" <+> nestedCo co
    TyCoRep.SubCo coN -> "SubCo" <+> nestedCo coN
    TyCoRep.HoleCo coH -> "HoleCo" <+> Utils.ppr coH
    where
      nestedCo :: Utils.Outputable (Unpretty a) => a -> Utils.SDoc
      nestedCo = Utils.parens . Utils.ppr . Unpretty

#if MIN_VERSION_ghc(8, 6, 0)
instance Utils.Outputable (Unpretty MCoercion) where
  ppr (Unpretty mco) = case mco of
    MRefl -> "MRefl"
    MCo co -> "MCo" <+> Utils.parens (Utils.ppr (Unpretty co))
#endif

newtype WithType = WithType CoreExpr

instance Utils.Outputable WithType where
  ppr (WithType e) = Utils.ppr e <+> Utils.dcolon <+> Utils.ppr (exprType e)
