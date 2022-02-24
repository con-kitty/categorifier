{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

-- | This is the internal plumbing for the @Core@ pass of `Categorifier.plugin`.
module Categorifier.Core
  ( install,
  )
where

import Bag (isEmptyBag)
import qualified Categorifier.Categorify
import Categorifier.CommandLineOptions (OptionGroup (..))
import Categorifier.Common.IO.Exception (SomeException, handle, throwIOAsException)
import qualified Categorifier.Core.BuildDictionary as BuildDictionary
import Categorifier.Core.Categorify (categorify)
import qualified Categorifier.Core.ErrorHandling as Errors
import Categorifier.Core.MakerMap (MakerMapFun, baseMakerMapFun, combineMakerMapFuns)
import Categorifier.Core.Makers (Makers, haskMakers)
import qualified Categorifier.Core.PrimOp as PrimOp
import Categorifier.Core.Types
  ( AutoInterpreter,
    CategoricalFailure (..),
    CategoryStack,
    CategoryState (..),
    Lookup,
    MissingSymbol (..),
    neverAutoInterpret,
  )
import Categorifier.Duoidal (Parallel (..), traverseD, (=<\<))
import Categorifier.Hierarchy
  ( First (..),
    Hierarchy,
    baseHierarchy,
    concatOps,
    findId,
    findName,
    getBaseIdentifiers,
    properFunTy,
  )
import qualified Categorifier.TH as TH
import Control.Monad (unless, (<=<))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import Control.Monad.Trans.RWS.Strict (RWST (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Foldable (toList)
import Data.List (findIndex)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty.Extra (NonEmpty, nonEmpty, nubOrd)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid (Ap (..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified DynamicLoading as Dynamic
import ErrUtils (WarningMessages)
import qualified ForeignCall as Plugins
import qualified GHC.CString
import qualified GhcPlugins as Plugins
import qualified Language.Haskell.TH as TH
import PyF (fmt)
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)

-- We need @MagicHash@ to load some GHC-internal identifiers
{-# ANN module ("HLint: ignore Avoid restricted extensions" :: String) #-}

-- | The name of the pseudo-function that triggers the execution of our simplifier rule.
conversionFunction :: TH.Name
conversionFunction = 'Categorifier.Categorify.expression

-- | Our Core plugin does four things,
--
-- 1. run a simplifier to get as many calls to `Categorifier.Categorify.expression` into a
--    recognizable state,
-- 2. add the rules that rewrite `Categorifier.Categorify.expression` (this doesn't do any rewriting
--    itself, just allows later simplifiers to trigger the rewrites),
-- 3. run a simplifier that actually triggers the rewrite rules, and finally
-- 4. remove the rules we added so they don't end up in the module's interface file.
--
--   When possible, we use an existing simplifier in the incoming @CoreToDo@ list rather than adding
--   one of our own for step 3. When we do this, we install the rewrite rules immediately before the
--   simplifier we borrowed. This reduces the chance that other plugins can shadow or otherwise
--   affect our rules. The rules can be removed any time after the simplifier, but they /must/ be
--   removed.
install ::
  Map OptionGroup [Text] ->
  [Plugins.CoreToDo] ->
  Plugins.CoreM [Plugins.CoreToDo]
install opts todos = do
  dflags <- Plugins.getDynFlags
  convert <-
    either (throwIOAsException $ prettyMissingSymbols dflags) pure <=< runExceptT . getParallel $
      idFromTHName conversionFunction
  let allOurTodos = categorifyTodos <> [postsimplifier convert dflags, removeCategorify]
      categorifyTodos =
        [ presimplifier convert dflags,
          Plugins.CoreDoPluginPass ("add " <> Plugins.getOccString convert <> " rules") $
            addCategorifyRules dflags convert opts
        ]
      removeCategorify =
        Plugins.CoreDoPluginPass ("remove " <> Plugins.getOccString convert <> " rules") $
          removeBuiltinRules ruleNames
      -- __TODO__: extract these from `addCategorifyRules`
      ruleNames = [Plugins.getOccFS convert, Plugins.getOccFS convert <> " $"]
  pure
    . maybe
      -- no existing simplifier, so we use our own
      (allOurTodos <> todos)
      -- piggy-back on an existing simplifier rather than add our own
      ( \i ->
          let (before, after) = splitAt i todos
           in before <> categorifyTodos <> after <> [removeCategorify]
      )
    $ findIndex isSimplifier todos
  where
    isSimplifier = \case
      Plugins.CoreDoSimplify numIterations _ -> numIterations > 0
      _ -> False

removeBuiltinRules :: [Plugins.RuleName] -> Plugins.CorePluginPass
removeBuiltinRules names = pure . mapModGutsRules (filter (not . ruleMatches))
  where
    ruleMatches r = Plugins.isBuiltinRule r && Plugins.ru_name r `elem` names

-- | A simplifier run before we add our pass, to make sure calls to `categorify` are in the right
--   form to be successfully transformed.
--
--  __TODO__: We may be able to add this conditionally, based on whether another suitable
--            simplifier is already in the todos list. That could allow us to play more nicely
--            with other plugins.
presimplifier :: Plugins.Id -> Plugins.DynFlags -> Plugins.CoreToDo
presimplifier convert dflags =
  Plugins.CoreDoSimplify
    1
    Plugins.SimplMode
      { Plugins.sm_case_case = False,
        Plugins.sm_dflags = dflags,
        Plugins.sm_eta_expand = False,
        Plugins.sm_inline = True,
        Plugins.sm_names = ["pre-" <> Plugins.getOccString convert],
        Plugins.sm_phase = Plugins.Phase 1,
        Plugins.sm_rules = False
      }

-- | A simplifer run after we add our pass, to make sure `categorify` calls are expanded regardless
--   of what other passes exist.
--
--   This is the most minimal simplifier possible, as we try not to affect whatever else the user
--   wants, we only need a pass so that categorification is performed. If there is already a
--   simplifier in the pipeline, we should prefer that to adding our own.
postsimplifier :: Plugins.Id -> Plugins.DynFlags -> Plugins.CoreToDo
postsimplifier convert dflags =
  Plugins.CoreDoSimplify
    1
    Plugins.SimplMode
      { Plugins.sm_case_case = False,
        Plugins.sm_dflags = dflags,
        Plugins.sm_eta_expand = False,
        Plugins.sm_inline = False,
        Plugins.sm_names = [Plugins.getOccString convert <> " minimal"],
        Plugins.sm_phase = Plugins.InitialPhase,
        Plugins.sm_rules = False
      }

prettyMissingSymbols :: Plugins.DynFlags -> NonEmpty MissingSymbol -> String
prettyMissingSymbols dflags =
  ("some symbols couldn't be found while attempting to install the Categorifier plugin:" <>)
    . concat
    . nubOrd
    . fmap
      ( \case
          IncorrectType name ty ->
            [fmt|\n  - `{Plugins.showPpr dflags name}` was found, but didn't have type `{Plugins.showPpr dflags ty}`|]
          MissingDataCon modu name ->
            [fmt|\n  - data constructor {name} in {Plugins.moduleNameString modu}|]
          MissingId modu name ->
            [fmt|\n  - identifier {name} in {Plugins.moduleNameString modu}|]
          MissingName modu name ->
            [fmt|\n  - name {name} in {Plugins.moduleNameString modu}|]
          MissingTyCon modu name ->
            [fmt|\n  - type constructor {name} in {Plugins.moduleNameString modu}|]
      )

-- | A plugin pass that adds our `categorify` rule to the list of simplifier rules to be run.
--
--  __NB__: We're forced to fold our failures into `IO` here.
addCategorifyRules ::
  Plugins.DynFlags ->
  Plugins.Id ->
  Map OptionGroup [Text] ->
  Plugins.CorePluginPass
addCategorifyRules dflags convert opts guts =
  either (throwIOAsException $ prettyMissingSymbols dflags) (\r -> pure (mapModGutsRules (r <>) guts))
    =<< categorifyRules convert opts guts

-- | Given the arguments for a `Plugins.BuiltinRule`, this builds a list of rules that try to handle
--   partial application of the original rule. One caveat is that the provided `Plugins.RuleFun` has
--   to return `Nothing` on partial applications for this to work (e.g., it can't `Prelude.error`).
partialAppRules ::
  Plugins.Var ->
  Plugins.Name ->
  Int ->
  Plugins.RuleFun ->
  [Plugins.CoreRule]
partialAppRules app fn nargs try =
  let name = Plugins.getOccFS fn
   in [ Plugins.BuiltinRule
          { Plugins.ru_name = name,
            Plugins.ru_fn = fn,
            Plugins.ru_nargs = nargs,
            Plugins.ru_try = try
          },
        Plugins.BuiltinRule
          { Plugins.ru_name = name <> Plugins.fsLit " $",
            Plugins.ru_fn = Plugins.varName app,
            Plugins.ru_nargs = 5,
            Plugins.ru_try = \dflags inScope ident exprs ->
              if ident == app
                then applyApply (try dflags inScope) exprs
                else Nothing
          }
      ]
  where
    -- Turns an application of @v a b `$` c x y@ into an application of @v a b (c x y)@.
    applyApply :: (Plugins.Var -> [Plugins.CoreExpr] -> Maybe a) -> [Plugins.CoreExpr] -> Maybe a
    applyApply try' = \case
      Plugins.Type _levity
        : Plugins.Type _dom
        : Plugins.Type _cod
        : (splitApps -> (Plugins.Var v, args))
        : moreArgs ->
          if fn == Plugins.varName v
            then try' v (args <> moreArgs)
            else
              if app == v
                then applyApply try' (args <> moreArgs)
                else Nothing
      _ -> Nothing

-- | This generates an expression representing a @throw@. The first two arguments provide looked-up
--   identifiers and the rest match the arguments to `Categorifier.Categorify.expression`.
deferFailures ::
  -- | `GHC.Base.error`
  Plugins.Id ->
  -- | `GHC.CSTring.unpackCStringUtf8#`
  Plugins.Id ->
  -- | target category (@`Data.Kind.Type` -> `Data.Kind.Type` -> `Data.Kind.Type`@)
  Plugins.Type ->
  -- | domain (`Data.Kind.Type`)
  Plugins.Type ->
  -- | codomain (`Data.Kind.Type`)
  Plugins.Type ->
  -- | `GHC.Stack.CallStack`
  Plugins.CoreExpr ->
  Plugins.CoreExpr
deferFailures throw str cat a b calls =
  let convertFn = 'Categorifier.Categorify.expression
   in Plugins.App
        ( Plugins.App
            ( Plugins.mkTyApps
                (Plugins.Var throw)
                [Plugins.liftedRepTy, Plugins.mkAppTys cat [a, b]]
            )
            calls
        )
        . Plugins.App (Plugins.Var str)
        . Plugins.Lit
        $ Plugins.mkLitString
          [fmt|A call to `{TH.nameQualified convertFn}` failed to be eliminated by
the "Categorifier" plugin. But errors from the plugin have been deferred to runtime,
so you see this message instead of the actual compile-time failure. Compile
without `-fplugin-opt Categorifier:defer-failures` to see what actually went wrong.|]

-- | Convert a name from Template Haskell to Core. Template Haskell ensures the name is resolvable
--   when the plugin is compiled, which can help avoid failures when client code is compiled.
idFromTHName :: TH.Name -> Lookup Plugins.Id
idFromTHName name =
  maybe
    ( Parallel . lift $
        throwIOAsException
          (("This name should have been global, but has no module: " <>) . TH.nameBase)
          name
    )
    (flip findId $ TH.nameBase name)
    $ TH.nameModule name

nameFromTHName :: TH.Name -> Lookup Plugins.Name
nameFromTHName name =
  maybe
    ( Parallel . lift $
        throwIOAsException
          (("This name should have been global, but has no module: " <>) . TH.nameBase)
          name
    )
    (flip findName $ TH.nameBase name)
    $ TH.nameModule name

-- |
-- __TODO__: `Dynamic.getValueSafely` throws in many cases. Try to catch, accumulate, return in
--           `Either` (not that we can drop the IO regardless).
getDynamicValueSafely :: Plugins.HscEnv -> Plugins.Name -> Plugins.Type -> IO (Maybe a)
getDynamicValueSafely = Dynamic.getValueSafely

nameFromText :: Text -> Lookup Plugins.Name
nameFromText =
  uncurry findName
    . bimap (Text.unpack . Text.dropWhileEnd (== '.')) Text.unpack
    . Text.breakOnEnd "."

additionalBoxersTy :: Lookup Plugins.Type
additionalBoxersTy = Plugins.exprType . Plugins.Var <$> idFromTHName 'PrimOp.noAdditionalBoxers

autoInterpreterTy :: Lookup Plugins.Type
autoInterpreterTy = Plugins.exprType . Plugins.Var <$> idFromTHName 'neverAutoInterpret

hierarchyTy :: Lookup Plugins.Type
hierarchyTy = Plugins.exprType . Plugins.Var <$> idFromTHName 'baseHierarchy

makerMapTy :: Lookup Plugins.Type
makerMapTy = Plugins.exprType . Plugins.Var <$> idFromTHName 'baseMakerMapFun

-- | Wiring our function into a `Plugins.BuiltInRule` for the plugin system.
categorifyRules ::
  Plugins.Id ->
  Map OptionGroup [Text] ->
  Plugins.ModGuts ->
  Plugins.CoreM (Either (NonEmpty MissingSymbol) [Plugins.CoreRule])
categorifyRules convert opts guts =
  runExceptT $ do
    -- __TODO__: This @do@ block currently has monadic semantics, but it should have duoidal
    --           ones. It's a bit complicated to get it to applicative with manual duoidal handling
    --           (as we do elsewhere via explicit `Parallel` handling, but GHC 9.0 should make this
    --           easy to get right with @QualifiedDo@.
    apply <- getParallel $ idFromTHName '($)
    throw <- getParallel $ idFromTHName 'Prelude.error
    str <- getParallel $ idFromTHName 'GHC.CString.unpackCStringUtf8#
    additionalBoxersTy' <- getParallel additionalBoxersTy
    autoInterpreterTy' <- getParallel autoInterpreterTy
    hierarchyTy' <- getParallel hierarchyTy
    makerMapTy' <- getParallel makerMapTy
    hask <- getParallel concatOps
    let loadOptions def =
          getParallel
            . maybe (pure <$> nameFromTHName def) (traverse nameFromText)
            . (nonEmpty <=< flip Map.lookup opts)
        additionalBoxersOptions = loadOptions 'PrimOp.noAdditionalBoxers AdditionalBoxersOptions
        autoInterpreterOptions = loadOptions 'neverAutoInterpret AutoInterpreterOptions
        hierarchyOptions = loadOptions 'baseHierarchy HierarchyOptions
        makerMapOptions = loadOptions 'baseMakerMapFun MakerMapOptions
    hscEnv <- lift Plugins.getHscEnv
    let handleOptions ty =
          traverseD
            ( \opt ->
                maybe (throwE . pure $ IncorrectType opt ty) getParallel
                  <=< Plugins.liftIO
                  $ getDynamicValueSafely hscEnv opt ty
            )
        handleAdditionalBoxers ::
          NonEmpty Plugins.Name ->
          ExceptT
            (NonEmpty MissingSymbol)
            Plugins.CoreM
            (Makers -> [(Plugins.CLabelString, (PrimOp.Boxer, [Plugins.Type], Plugins.Type))])
        handleAdditionalBoxers =
          fmap (foldr1 (\f g a -> f a <> g a))
            . traverseD
              ( \opt ->
                  maybe (throwE . pure $ IncorrectType opt additionalBoxersTy') pure
                    <=< Plugins.liftIO
                    $ getDynamicValueSafely hscEnv opt additionalBoxersTy'
              )
        handleAutoInterpreter ::
          NonEmpty Plugins.Name -> ExceptT (NonEmpty MissingSymbol) Plugins.CoreM AutoInterpreter
        handleAutoInterpreter = fmap NE.last . handleOptions autoInterpreterTy'
        handleHierarchy ::
          NonEmpty Plugins.Name -> ExceptT (NonEmpty MissingSymbol) Plugins.CoreM (Hierarchy CategoryStack)
        handleHierarchy = fmap (getFirst . foldMap First) . handleOptions hierarchyTy'
        handleMakerMap ::
          NonEmpty Plugins.Name -> ExceptT (NonEmpty MissingSymbol) Plugins.CoreM MakerMapFun
        handleMakerMap =
          fmap (combineMakerMapFuns . toList)
            . traverseD
              ( \opt ->
                  maybe (throwE . pure $ IncorrectType opt makerMapTy') pure
                    <=< Plugins.liftIO
                    $ getDynamicValueSafely hscEnv opt makerMapTy'
              )
    let additionalBoxers' = handleAdditionalBoxers =<\< additionalBoxersOptions
        autoInterpreter = handleAutoInterpreter =<\< autoInterpreterOptions
        hierarchy = handleHierarchy =<\< hierarchyOptions
        makerMap = handleMakerMap =<\< makerMapOptions
    additionalBoxers <- additionalBoxers'
    tryAutoInterpret <- autoInterpreter
    h <- hierarchy
    bh <- getParallel $ combineHierarchies [baseHierarchy, Parallel hierarchy]
    makerMapFun <- makerMap
    baseIdentifiers <- getParallel getBaseIdentifiers
    uniqS <- lift Plugins.getUniqueSupplyM
    hierarchyOptions' <- hierarchyOptions
    pure $
      partialAppRules apply (Plugins.varName convert) 5 $
        \dflags inScope ident exprs ->
          let baseArrowMakers = haskMakers dflags inScope guts hscEnv hask bh properFunTy
           in if ident == convert
                then
                  unsafePerformIO $
                    applyCategorify
                      convert
                      hierarchyOptions'
                      ( if Map.member DeferFailuresOption opts
                          then pure $ deferFailures throw str
                          else Nothing
                      )
                      dflags
                      uniqS
                      ( \cat ->
                          categorify
                            (Map.member DebugOption opts)
                            (Map.member BenchmarkOption opts)
                            dflags
                            cat
                            (BuildDictionary.buildDictionary hscEnv dflags guts inScope)
                            baseIdentifiers
                            baseArrowMakers
                            (haskMakers dflags inScope guts hscEnv hask h cat)
                            tryAutoInterpret
                            makerMapFun
                            additionalBoxers
                      )
                      exprs
                else Nothing
  where
    combineHierarchies = getAp . fmap getFirst . foldMap (Ap . fmap First)

-- | Fold everything in our stack down to `IO`. The plugin system gives us basically no outlet for
--   errors or anything, so we need to process everything before we return. This does the "safe"
--   part of that, so all that's left is to perform the `IO` as late as possible.
runStack ::
  NonEmpty Plugins.Name ->
  Maybe Plugins.CoreExpr ->
  Plugins.DynFlags ->
  Plugins.UniqSupply ->
  Plugins.CoreExpr ->
  Plugins.CoreExpr ->
  (Plugins.CoreExpr -> CategoryStack Plugins.CoreExpr) ->
  IO Plugins.CoreExpr
runStack hierarchyOptions defer dflags uniqS calls f =
  handlePanic . deferException . deferLeft <=< printWarnings . runExceptT . ($ f)
  where
    deferException :: IO Plugins.CoreExpr -> IO Plugins.CoreExpr
    deferException =
      maybe
        id
        (handle . (const . pure :: Plugins.CoreExpr -> SomeException -> IO Plugins.CoreExpr))
        defer
    deferLeft :: Either (NonEmpty CategoricalFailure) Plugins.CoreExpr -> IO Plugins.CoreExpr
    deferLeft = either (maybe printFailure (const . pure) defer) pure
    handlePanic :: IO b -> IO b
    handlePanic = handle (throwIOAsException (Text.unpack . Errors.displayPanic dflags calls))
    printWarnings ::
      RWST (Map Plugins.Var Plugins.CoreExpr) WarningMessages CategoryState IO a ->
      IO a
    printWarnings wt = do
      (val, _newState, warns) <- runRWST wt Map.empty (CategoryState uniqS 0 mempty)
      unless (isEmptyBag warns) . hPutStrLn stderr . Text.unpack $ Errors.showWarnings dflags warns
      pure val
    printFailure :: NonEmpty CategoricalFailure -> IO a
    printFailure = throwIOAsException (Text.unpack . Errors.showFailures dflags hierarchyOptions f)

-- | __HIC SUNT DRACONES__
--
--   We use `unsafePerformIO` here because the build should fail if we can't eliminate a call to
--  `Categorifier.Categorify.expression` (otherwise, that call will simply fail itself at runtime).
--   Ideally this function would allow us to return errors in /some/ context, at /least/ `IO`, but
--   it until then, we're reduced to this.
--
--   The `Maybe` indicates whether or not the `Plugins.CoreExpr` was rewritten, but in our case, not
--   re-writing means the code is unusable at runtime, so we always return `Just` (assuming we
--   haven't failed in `IO`).
applyCategorify ::
  Plugins.Id ->
  NonEmpty Plugins.Name ->
  Maybe (Plugins.Type -> Plugins.Type -> Plugins.Type -> Plugins.CoreExpr -> Plugins.CoreExpr) ->
  Plugins.DynFlags ->
  Plugins.UniqSupply ->
  (Plugins.Type -> Plugins.CoreExpr -> CategoryStack Plugins.CoreExpr) ->
  [Plugins.CoreExpr] ->
  IO (Maybe Plugins.CoreExpr)
applyCategorify convert hierarchyOptions defer dflags uniqS f = \case
  (Plugins.Type cat : Plugins.Type a : Plugins.Type b : calls : function : extraArgs) ->
    if null extraArgs
      then
        pure
          <$> runStack
            hierarchyOptions
            ((\fn -> fn cat a b calls) <$> defer)
            dflags
            uniqS
            calls
            function
            (f cat)
      else
        throwIOAsException
          ( \args ->
              [fmt|
Categorifier: GHC somehow called `{Plugins.getOccString convert}` with too many
           arguments. This shouldn't have gotten past the typechecker:
           {Plugins.showPpr dflags args}
|]
          )
          extraArgs
  -- The next few cases are partial applications of `categorify` that we ignore and hope finish
  -- application later (see `applyApply`).
  [Plugins.Type _cat, Plugins.Type _a, Plugins.Type _b, _callStack] -> pure Nothing
  [Plugins.Type _cat, Plugins.Type _a, Plugins.Type _b] -> pure Nothing
  [Plugins.Type _cat, Plugins.Type _a] -> pure Nothing
  [Plugins.Type _cat] -> pure Nothing
  [] -> pure Nothing
  -- And ... if we can't identify this as a valid call to `categorify`, we error.
  args ->
    throwIOAsException
      ( \as ->
          [fmt|
Categorifier: GHC failed to invoke the categorify rule correctly. It was called with
           the following arguments:
           {Plugins.showPpr dflags as}
|]
      )
      args

-- | Expands repeated `Plugins.App` nodes, returning a "primary" expression and all the arguments
--   it's applied to. If the expression isn't an application, it still succeeds, returning the
--   original expression and an empty list (i.e., a nullary application).
--
--  __TODO__: This should already exist, right?
splitApps :: Plugins.CoreExpr -> (Plugins.CoreExpr, [Plugins.CoreExpr])
splitApps = \case
  Plugins.App h t -> second (<> [t]) (splitApps h)
  expr -> (expr, [])

-- | A utility function that treats `Plugins.ModGuts` as a `Functor` over the list of
--  `Plugins.CoreRules`.
mapModGutsRules :: ([Plugins.CoreRule] -> [Plugins.CoreRule]) -> Plugins.ModGuts -> Plugins.ModGuts
mapModGutsRules f mg = mg {Plugins.mg_rules = f (Plugins.mg_rules mg)}
