{-# LANGUAGE CPP #-}

-- | The GHC API doesn't give very much control over the simplifier. The only way to use it is to
--   trampoline out of your plugin pass to let an external `Plugins.CoreToDo` simplify for you or to
--   call `SimplCore.simplifyExpr`, which doesn't give you even the same control that the
--  `Plugins.CoreToDo` simplifier does.
--
--   Here we try to give more control over how the simplifier is applied, making it possible to at
--   least call `simplifyExpr` with a `Plugins.SimplMode`. We also try to provide access to specific
--   transformations so that Core plugins can do fine-grained simplification internally.
--
--  __NB__: Unfortunately, many of the useful operations aren't exposed in the API, so this
--          duplicates them as necessary, with comments explaining which are abstracted (so we can
--          try to keep them in sync with upstream changes).
module Categorifier.Core.Simplify
  ( Transformation (..),
    simplifyExpr,
  )
where

import Control.Monad ((<=<))
import Data.Set (Set, member)
#if MIN_VERSION_ghc(9, 2, 0)
import qualified GHC.Core.Unfold as Core
import qualified GHC.Utils.Logger as Log
#endif
#if MIN_VERSION_ghc(9, 0, 0)
import GHC.Core.Stats (exprSize)
import GHC.Core.FamInstEnv (emptyFamInstEnvs)
import GHC.Core.Opt.OccurAnal (occurAnalyseExpr)
import GHC.Core.Opt.Simplify.Env (mkSimplEnv)
import GHC.Core.Opt.Simplify.Monad (SimplM, initSmpl)
import GHC.Core.Opt.Simplify (simplExpr)
import qualified GHC.Plugins as Plugins
#else
import CoreStats (exprSize)
import FamInstEnv (emptyFamInstEnvs)
import qualified GhcPlugins as Plugins
import OccurAnal (occurAnalyseExpr)
import SimplEnv (mkSimplEnv)
import SimplMonad (SimplM, initSmpl)
import Simplify (simplExpr)
#endif

-- | This is the simplifier we apply surgically to expressions that should be re-written before
--   continuing categorification.
--
--   These are passes that we have control of in the simplifier.
data Transformation = CaseOfCase | EtaExpand | Inline | Rules
  deriving (Eq, Ord)

-- | Like `SimplCore.simplifyExpr`, except it takes a `Set` of `Transformation`s to apply. This
--   allows us to control (as much as possible) how the simplifier is applied in each call.
--
--   No matter how we restrict it, this simplification applies a ton of transformations that are
--   hard to separate out (future work?). This one disables everything we can by default, then
--   allows us to turn on individual transformations. There are also some other transformations we
--   need that are implicit in here. E.g. specialization and case of known constructor.
#if MIN_VERSION_ghc(9, 2, 0)
simplifyExpr ::
  Log.Logger ->
  Plugins.DynFlags ->
  Set Transformation ->
  Plugins.CoreExpr ->
  IO Plugins.CoreExpr
simplifyExpr logger dflags trans expr =
  fmap fst . initSmpl logger dflags Plugins.emptyRuleEnv emptyFamInstEnvs (exprSize expr) $
    doSimplify
      1
      Plugins.SimplMode
        { Plugins.sm_case_case = CaseOfCase `member` trans,
          Plugins.sm_uf_opts = Core.defaultUnfoldingOpts,
          Plugins.sm_pre_inline = Inline `member` trans,
          Plugins.sm_logger = logger,
          Plugins.sm_dflags = dflags,
          Plugins.sm_eta_expand = EtaExpand `member` trans,
          Plugins.sm_inline = Inline `member` trans,
          Plugins.sm_names = ["categorify internal"],
          Plugins.sm_phase = Plugins.Phase 1,
          Plugins.sm_rules = Rules `member` trans -- this improves specialisation
        }
      expr
#else
simplifyExpr ::
  Plugins.DynFlags ->
  Set Transformation ->
  Plugins.UniqSupply ->
  Plugins.CoreExpr ->
  IO Plugins.CoreExpr
simplifyExpr dflags trans uniqS expr =
  fmap fst . initSmpl dflags Plugins.emptyRuleEnv emptyFamInstEnvs uniqS (exprSize expr) $
    doSimplify
      1
      Plugins.SimplMode
        { Plugins.sm_case_case = CaseOfCase `member` trans,
          Plugins.sm_dflags = dflags,
          Plugins.sm_eta_expand = EtaExpand `member` trans,
          Plugins.sm_inline = Inline `member` trans,
          Plugins.sm_names = ["categorify internal"],
          Plugins.sm_phase = Plugins.Phase 1,
          Plugins.sm_rules = Rules `member` trans -- this improves specialisation
        }
      expr
#endif

-- | Designed to be like `Plugins.CoreDoSimplify`, but applicable to arbitrary `Plugins.CoreExpr`s.
doSimplify :: Int -> Plugins.SimplMode -> Plugins.CoreExpr -> SimplM Plugins.CoreExpr
doSimplify passCount mode =
  foldr (<=<) pure . replicate passCount $ simplExpr (mkSimplEnv mode) . occurAnalyseExpr
