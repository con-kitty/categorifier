{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- -Wno-orphans is so we can add missing instances to `Bag.Bag`

-- |
-- Module      :  ConCat.BuildDictionary
-- Copyright   :  (c) 2016 Conal Elliott
-- License     :  BSD3
--
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
--
-- Adaptation of HERMIT's buildDictionaryT via ConCat's BuildDictonary
module Categorifier.Core.BuildDictionary (buildDictionary) where

import qualified Bag
import qualified Categorifier.Core.Benchmark as Bench
import Categorifier.Core.Trace (pprTrace')
import Categorifier.Core.Types
  ( CategoryState (..),
    DictCacheEntry (..),
    DictCacheKey,
    DictionaryFailure (..),
    DictionaryStack,
    writerT,
  )
import Categorifier.Duoidal ((<\*))
import qualified Constraint as Typechecker
import Control.Arrow (Arrow (..))
import Control.Monad ((<=<))
import Control.Monad.Extra (filterM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), except, throwE)
import Control.Monad.Trans.RWS.Strict (gets, modify)
import Data.Data (Data)
import Data.Foldable (traverse_)
import Data.Generics (everything, mkQ)
import Data.List.Extra (isPrefixOf)
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Data.Monoid (Any (..))
import DsBinds (dsEvBinds)
import DsMonad (initDsTc)
import ErrUtils (WarningMessages)
import Finder (FindResult (..), findExposedPackageModule)
import GhcPlugins ((<+>))
import qualified GhcPlugins as Plugins
import qualified Predicate as Core
import SimplCore (simplifyExpr)
import qualified TcErrors as Typechecker
import qualified TcEvidence as Typechecker
import qualified TcHsSyn as Typechecker
import qualified TcInteract as Typechecker
import qualified TcOrigin as Typechecker
import qualified TcRnDriver as Typechecker
import qualified TcRnMonad as Typechecker
import qualified TcSMonad as Typechecker
import qualified TcSimplify as Typechecker
import qualified UniqSet as NonDetSet
import Unique (mkUniqueGrimily)
import Yaya.Functor (hmap)

uniqSetToList :: Plugins.UniqSet a -> [a]
uniqSetToList = NonDetSet.nonDetEltsUniqSet

traceTcS' :: String -> Plugins.SDoc -> Typechecker.TcS ()
traceTcS' str doc = pprTrace' str doc (pure ())

traceTc' :: String -> Plugins.SDoc -> Typechecker.TcRn ()
traceTc' str doc = pprTrace' str doc (pure ())

-- | The result type is a slight improvement of the situation handed to us by
--  `Typechecker.runTcInteractive`, so not too much we can do about it other than hide it a bit and
--   try to keep it from leaking all over everything.
runTcRn ::
  Plugins.Outputable a =>
  Plugins.HscEnv ->
  Plugins.ModGuts ->
  Typechecker.TcRn a ->
  IO (Either (NonEmpty DictionaryFailure) a, WarningMessages)
runTcRn env0 guts m = do
  -- Remove hidden modules from dep_orphans
  orphans <-
    filterM (fmap isFound . flip (findExposedPackageModule env0) Nothing)
      . fmap Plugins.moduleName
      . Plugins.dep_orphs
      $ Plugins.mg_deps guts
  ((warns, errs), mr) <- Typechecker.runTcInteractive (env orphans) m
  pure (handleResult errs mr, warns)
  where
    isFound :: FindResult -> Bool
    isFound (Found _ _) = True
    isFound _ = False
    handleResult errors =
      maybe
        (Left . pure $ TypecheckFailure errors)
        (if Bag.isEmptyBag errors then pure else Left . pure . ErroneousTypecheckSuccess errors)
    imports0 = Plugins.ic_imports (Plugins.hsc_IC env0)
    env :: [Plugins.ModuleName] -> Plugins.HscEnv
    env extraModuleNames =
      env0
        { Plugins.hsc_IC =
            (Plugins.hsc_IC env0)
              { Plugins.ic_imports = fmap Plugins.IIModule extraModuleNames <> imports0,
                Plugins.ic_rn_gbl_env = Plugins.mg_rdr_env guts,
                Plugins.ic_instances = (Plugins.mg_insts guts, Plugins.mg_fam_insts guts)
              }
        }

-- | Build a dictionary for the given id.
buildDictionary' ::
  Plugins.VarSet -> Plugins.Id -> Typechecker.TcRn [Plugins.CoreBind]
buildDictionary' evIds evar = do
  bs <- do
    loc <- Typechecker.getCtLocM (Typechecker.GivenOrigin Typechecker.UnkSkol) Nothing
    let givens = Typechecker.mkGivens loc (uniqSetToList evIds)
        predTy = Plugins.varType evar
        nonC =
          Typechecker.mkNonCanonical $
            Typechecker.CtWanted
              { Typechecker.ctev_pred = predTy,
                Typechecker.ctev_dest = Typechecker.EvVarDest evar,
                Typechecker.ctev_nosh = Typechecker.WOnly,
                Typechecker.ctev_loc = loc
              }
        wCs = Typechecker.mkSimpleWC [Typechecker.cc_ev nonC]
    -- TODO: Make sure solveWanteds is the right function to call.
    traceTc' "buildDictionary': givens" (Plugins.ppr givens)
    (wCs', bnds0) <-
      second Typechecker.evBindMapBinds
        <$> Typechecker.runTcS
          ( do
              _ <- Typechecker.solveSimpleGivens givens
              traceTcS' "buildDictionary' back from solveSimpleGivens" Plugins.empty
              z <- Typechecker.solveWanteds wCs
              traceTcS' "buildDictionary' back from solveWanteds" (Plugins.ppr z)
              pure z
          )
    traceTc' "buildDictionary' back from runTcS" (Plugins.ppr bnds0)
    ez <- Typechecker.emptyZonkEnv
    -- Use the newly exported zonkEvBinds. <https://phabricator.haskell.org/D2088>
    (_env', bnds) <- Typechecker.zonkEvBinds ez bnds0
    -- traceTc "buildDictionary' wCs'" (Plugins.ppr wCs')
    traceTc' "buildDictionary' zonked" (Plugins.ppr bnds)
    Typechecker.reportAllUnsolved wCs'
    pure bnds
  initDsTc $ dsEvBinds bs

-- TODO: Richard Eisenberg: "use TcMType.newWanted to make your CtWanted. As it
-- stands, if predTy is an equality constraint, your CtWanted will be
-- ill-formed, as all equality constraints should have HoleDests, not
-- EvVarDests. Using TcMType.newWanted will simplify and improve your code."

-- | This attempts to build a dictionary representing a type class instance. The `CoreSyn.Type` is
--   the constraint to satisfy.
buildDictionary ::
  Plugins.HscEnv ->
  Plugins.DynFlags ->
  Plugins.ModGuts ->
  Plugins.InScopeEnv ->
  Plugins.Type ->
  DictionaryStack Plugins.CoreExpr
buildDictionary env dflags guts inScope goalTy =
  pprTrace' "\nbuildDictionary" (Plugins.ppr goalTy)
    . pprTrace'
      "buildDictionary in-scope evidence"
      (Plugins.ppr (WithType . Plugins.Var <$> uniqSetToList scopedDicts))
    -- TODO: replace the hardcoded @True@.
    . Bench.billTo True Bench.BuildDictionary
    $ getCachedDict goalTy >>= \case
      Just cachedDict -> pure cachedDict
      Nothing -> do
        dict <-
          hmap lift . reassemble
            <=< ExceptT . writerT . runTcRn env guts
            $ buildDictionary' scopedDicts binder
        cacheDict goalTy dict
        pure dict
  where
    binder = localId inScope name goalTy
    name = "cccDict"
    scopedDicts = Plugins.filterVarSet keepVar (Plugins.getInScopeVars (fst inScope))
    -- This /should/ return `True` when @v@'s an applicable instance related to our @goalTy@,
    -- however it has run into some issues. Here is a bit of reconstructed history:
    --
    --  * 05b2df0 - removed @&& not (isEmptyVarSet (tyCoVarsOfType goalTy `intersectVarSet`
    --              tyCoVarsOfType (Plugins.varType v)))@, because that was filtering out the
    --              instances in the module being compiled
    --
    --  * cee7466 - changed to @keepVar = const False@ because of an occasional
    --              "StgCmmEnv: variable not found" error. Included a comment, "See 2018-01-23
    --              journal notes". This change also included a commented out
    --              @&& not (isDeadBinder v)@, which presumably was hoped would eliminate the error,
    --              but apparently didn't work.
    --
    -- We've currently restored it to just keep /all/ evidence (per 05b2df0) and haven't come across
    -- StgCmmEnv errors yet, so maybe things have improved. If not, we'll try to identify the
    -- problem, as this is a useful feature to keep.
    keepVar v =
      let varName = Plugins.occNameString . Plugins.nameOccName $ Plugins.varName v
       in Core.isEvVar v
            &&
            -- Here we remove all the "cccDict" vars from the `inScope`. Why? Because when there
            -- are multiple functions (say `foo` and `bar`) being categorized in parallel, a
            -- dictionary var `cccDict_...` created during categorizing `foo` may show up in the
            -- `inScope` when categorizing `bar`. We don't want to use this var when building
            -- dictionaries for `bar`, because it is out of scope in the result of `bar`.
            not (name `isPrefixOf` varName)
            &&
            -- TODO (ziyang): I'm not quite sure why this is needed.
            not ("$d" `isPrefixOf` varName)
    reassemble :: [Plugins.CoreBind] -> ExceptT (NonEmpty DictionaryFailure) IO Plugins.CoreExpr
    reassemble =
      maybe
        (throwE (pure NoBindings))
        ( uncurry (<\*)
            . ( uncurry (<\*)
                  -- __NB__: The `simplifyExpr` here and the one in Conal's ConCat only differ in
                  --         terms of the `Plugins.CompilerPhase` they run in (this is
                  --        `Plugins.InitialPhase` vs @`Plugins.Phase` 0@ in Conal's. AFAICT, that
                  --         shouldn't matter, but if it does, come back here.
                  . ( lift . simplifyExpr dflags
                        &&& except . traverse_ (Left . pure . FreeIds) . nonEmpty . freeIdTys
                    )
                  . dict
                  &&& except
                    . traverse_ (Left . pure . CoercionHoles)
                    . nonEmpty
                    . NonEmpty.filter hasCoercionHole
              )
        )
        . nonEmpty
      where
        dict = \case
          -- Common case with single non-recursive let
          (Plugins.NonRec v e :| []) | binder == v -> e
          (h :| t) -> Plugins.mkCoreLets (h : t) (Plugins.varToCoreExpr binder)
        -- Sometimes buildDictionary' constructs bogus dictionaries with free
        -- identifiers. Hence check that freeIds is empty. Allow for free *type*
        -- variables, however, since there may be some in the given type as
        -- parameters. Alternatively, check that there are no free variables (val or
        -- type) in the resulting dictionary that were not in the original type.
        freeIds dictionary =
          Plugins.filterVarSet Plugins.isId (Plugins.exprFreeVars dictionary)
            `Plugins.minusVarSet` scopedDicts
        freeIdTys = fmap (id &&& Plugins.varType) . uniqSetToList . freeIds

hasCoercionHole :: Data t => t -> Bool
hasCoercionHole = getAny . everything (<>) (mkQ mempty (Any . isHole))
  where
    isHole :: Plugins.CoercionHole -> Bool
    isHole = const True

-- | Make a unique identifier for a specified type, using a provided name.
localId :: Plugins.InScopeEnv -> String -> Plugins.Type -> Plugins.Id
localId (inScopeSet, _) str =
  Plugins.uniqAway inScopeSet . Plugins.mkLocalId (stringToName str)

stringToName :: String -> Plugins.Name
stringToName str =
  Plugins.mkSystemVarName
    -- When mkUniqueGrimily's argument is negative, we see something like
    -- "Exception: Prelude.chr: bad argument: (-52)". Hence the abs.
    (mkUniqueGrimily (abs (fromIntegral (Plugins.hashString str))))
    (Plugins.mkFastString str)

cacheKey :: Plugins.Type -> DictCacheKey
cacheKey ty = modu <> "." <> Plugins.showSDocUnsafe (Plugins.ppr ty)
  where
    tyCon = fst $ Plugins.splitTyConApp ty
    name = Plugins.tyConName tyCon
    modu = maybe "" (Plugins.moduleNameString . Plugins.moduleName) (Plugins.nameModule_maybe name)

getCachedDict :: Plugins.Type -> DictionaryStack (Maybe Plugins.CoreExpr)
getCachedDict goalTy = do
  lift (gets csDictCache)
    >>= ( \case
            Just cached
              | Plugins.eqType goalTy (dceType cached) -> pure . Just $ Plugins.Var (dceVar cached)
            _ -> pure Nothing
        )
      . Map.lookup (cacheKey goalTy)

cacheDict :: Plugins.Type -> Plugins.CoreExpr -> DictionaryStack ()
cacheDict goalTy dict = lift . modify $ \(CategoryState uniqS idx cache) -> case dict of
  Plugins.Var v ->
    CategoryState uniqS idx $
      Map.insert (cacheKey goalTy) (DictCacheEntry goalTy v dict Nothing) cache
  _ ->
    let (u, uniqS') = Plugins.takeUniqFromSupply uniqS
        name =
          Plugins.mkInternalName u (Plugins.mkVarOcc "cccDict") $
            Plugins.mkGeneralSrcSpan "oops"
        v = Plugins.mkLocalVar (Plugins.DFunId False) name goalTy Plugins.vanillaIdInfo
     in CategoryState uniqS' (idx + 1) $
          Map.insert (cacheKey goalTy) (DictCacheEntry goalTy v dict (Just idx)) cache

-- Maybe place in a GHC utils module.

newtype WithType = WithType Plugins.CoreExpr

instance Plugins.Outputable WithType where
  ppr (WithType e) = Plugins.ppr e <+> Plugins.dcolon <+> Plugins.ppr (Plugins.exprType e)
