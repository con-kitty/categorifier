{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Conditional tracing of plugin components.
module Categorifier.Core.Trace
  ( renderSDoc,
    pprTrace',
    traceWith,
    maybeTraceWith,
    maybeTraceWithStack,
    takeLines,
    WithIdInfo (..),
    addIdInfo,
    Unpretty (..),
  )
where

import Categorifier.Core.Types (WithIdInfo (..))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Bifunctor (Bifunctor (..))
import Data.Bool (bool)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import qualified Data.List.Extra as List
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Set as Set
import Debug.Trace (trace, traceM)
#if MIN_VERSION_ghc(9, 0, 0)
import GHC.Plugins ((<+>))
import qualified GHC.Plugins as Plugins
import qualified GHC.Core.TyCo.Rep as TyCoRep
#else
import GhcPlugins ((<+>))
import qualified GhcPlugins as Plugins
import qualified TyCoRep
#endif
import PyF (fmt)
import System.IO.Unsafe (unsafePerformIO)
import System.Time.Extra (Seconds, offsetTime)

renderWithStyle :: Plugins.DynFlags -> Plugins.PrintUnqualified -> Plugins.SDoc -> String
#if MIN_VERSION_ghc(9, 0, 0)
renderWithStyle dflags =
  Plugins.renderWithStyle . Plugins.initSDocContext dflags . Plugins.mkDumpStyle
#else
renderWithStyle dflags qual sdoc =
  Plugins.renderWithStyle dflags sdoc (Plugins.mkDumpStyle dflags qual)
#endif

-- | Like 'Plugins.showSDoc', but qualifies some ambiguous names, and also shortens
-- large output.
renderSDoc :: Plugins.DynFlags -> Plugins.SDoc -> String
renderSDoc dflags sdoc = takeLines 20 20 $ renderWithStyle dflags qual sdoc
  where
    qual =
      Plugins.neverQualify
        { Plugins.queryQualifyName = \modu name ->
            if Plugins.occNameString name `Set.member` ambiguousNames
              then Plugins.NameQual (Plugins.moduleName modu)
              else Plugins.NameUnqual
        }
    ambiguousNames = Set.fromList ["Dict", "Rep"]

-- | Hacky eliminable tracing. Just switch the commented-out implementation to enable or disable
--   tracing.
pprTrace' :: String -> Plugins.SDoc -> a -> a
-- pprTrace' = Plugins.pprTrace
pprTrace' _ _ = id

-- | The important missing function from "Debug.Trace". It generalizes `Debug.Trace.traceShowId` to
--   take an arbitrary function rather than relying on `show`. E.g.,
--   >>> trace msg = traceWith (const msg)
--   >>> traceId = traceWith id
--   >>> traceShow x = traceWith (const (show x))
--   >>> traceShowId = traceWith show
traceWith :: (a -> String) -> a -> a
traceWith f a = trace (f a) a

maybeTraceWith :: Bool -> (a -> String) -> a -> a
maybeTraceWith = bool (const id) (traceWith . withTime)
  where
    withTime f a = unsafePerformIO $ do
      elapsed <- getElapsed
      pure [fmt|[{elapsed:.2}s] - {f a}|]
{-# INLINE maybeTraceWith #-}
{-# ANN maybeTraceWith ("HLint: ignore Missing NOINLINE pragma" :: String) #-}

maybeTraceWithStack :: MonadIO m => Bool -> (a -> String) -> (a -> m b) -> a -> m b
maybeTraceWithStack doTrace render act a =
  if doTrace
    then do
      (stepId, stack) <- liftIO $ readIORef stepInfoRef
      liftIO $ modifyIORef' stepInfoRef (bimap (+ 1) (stepId :))
      let parentId = fromMaybe (-1) (listToMaybe stack)
      elapsed <- liftIO getElapsed
      traceM [fmt|[{elapsed:.2}s] - step {show (stepId, parentId)}: {render a}|]
      res <- act a
      elapsed' <- liftIO getElapsed
      traceM [fmt|[{elapsed':.2}s] - completed step {stepId}, returning to step {parentId}|]
      liftIO . modifyIORef' stepInfoRef . second $ drop 1
      pure res
    else act a
{-# INLINE maybeTraceWithStack #-}

-- | Take @x@ lines at the beginning, and @y@ lines at the end of a string,
-- and replace everything in the middle (if any) with @"(...omitted k lines)"@.
--
-- No-op if either @x@ or @y@ is negative.
takeLines :: Int -> Int -> String -> String
takeLines x y s
  | x < 0 || y < 0 = s
  | otherwise = [fmt|{prefix}{omitted}{suffix}|]
  where
    (xs, (zs, ws)) = List.splitAtEnd y <$> List.splitAt x (List.lines s)
    prefix = List.intercalate "\n" xs
    omitted = if null zs then "" else [fmt|\n<...omitted {length zs} lines>|] :: String
    suffix =
      if null ws || y == 0
        then ""
        else (if x == 0 && null zs then "" else "\n") <> List.intercalate "\n" ws

-- | An `IORef` holding (incrementing step id, call stack).
stepInfoRef :: IORef (Int, [Int])
stepInfoRef = unsafePerformIO (newIORef (0, []))
{-# NOINLINE stepInfoRef #-}

getElapsed :: IO Seconds
getElapsed = unsafePerformIO offsetTime
{-# NOINLINE getElapsed #-}

addIdInfo :: Plugins.CoreExpr -> Plugins.Expr WithIdInfo
addIdInfo = \case
  Plugins.Var v -> Plugins.Var v
  Plugins.Lit l -> Plugins.Lit l
  Plugins.App e a -> Plugins.App (addIdInfo e) (addIdInfo a)
  Plugins.Lam b e -> Plugins.Lam (WithIdInfo b) (addIdInfo e)
  Plugins.Let b e -> Plugins.Let (addIdInfoBind b) (addIdInfo e)
  Plugins.Case scrut bind ty alts ->
    Plugins.Case (addIdInfo scrut) (WithIdInfo bind) ty $ fmap addIdInfoAlt alts
  Plugins.Cast e c -> Plugins.Cast (addIdInfo e) c
  Plugins.Tick tickish e -> Plugins.Tick tickish $ addIdInfo e
  Plugins.Type ty -> Plugins.Type ty
  Plugins.Coercion c -> Plugins.Coercion c
  where
    addIdInfoAlt (con, binds, expr) = (con, fmap WithIdInfo binds, addIdInfo expr)
    addIdInfoBind = \case
      Plugins.NonRec b e -> Plugins.NonRec (WithIdInfo b) $ addIdInfo e
      Plugins.Rec alts -> Plugins.Rec $ fmap (bimap WithIdInfo addIdInfo) alts

-- | Generic wrapper to make a pretty printer that's less ... pretty (and more useful for people
--   looking at the code).
newtype Unpretty a = Unpretty a

instance Plugins.Outputable (Unpretty Plugins.Coercion) where
  ppr (Unpretty coercion) = case coercion of
#if MIN_VERSION_ghc(8, 8, 0)
    TyCoRep.Refl ty -> "Refl" <+> Plugins.ppr ty
    TyCoRep.GRefl role ty mco -> "GRefl" <+> Plugins.ppr role <+> Plugins.ppr ty <+> nestedCo mco
#else
    TyCoRep.CoherenceCo co kCo -> "CoherenceCo" <+> nestedCo co <+> nestedCo kCo
    TyCoRep.Refl role ty -> "Refl" <+> Plugins.ppr role <+> Plugins.ppr ty
#endif
    TyCoRep.TyConAppCo role tyCon coes ->
      "TyConAppCo" <+> Plugins.ppr role <+> Plugins.ppr tyCon <+> Plugins.ppr (Unpretty <$> coes)
    TyCoRep.AppCo co coN -> "AppCo" <+> nestedCo co <+> nestedCo coN
    TyCoRep.ForAllCo tyCoVar kCo co ->
      "ForAllCo" <+> Plugins.ppr tyCoVar <+> nestedCo kCo <+> nestedCo co
#if MIN_VERSION_ghc(9, 0, 0)
    TyCoRep.FunCo role coN co co' ->
      "FunCo" <+> Plugins.ppr role <+> nestedCo coN <+> nestedCo co <+> nestedCo co'
#else
    TyCoRep.FunCo role co co' -> "FunCo" <+> Plugins.ppr role <+> nestedCo co <+> nestedCo co'
#endif
    TyCoRep.CoVarCo coVar -> "CoVarCo" <+> Plugins.ppr coVar
    TyCoRep.AxiomInstCo coA brI coes ->
      "AxiomInstCo" <+> Plugins.ppr coA <+> Plugins.ppr brI <+> Plugins.ppr (Unpretty <$> coes)
    TyCoRep.AxiomRuleCo coARule coes ->
      "AxiomRuleCo" <+> Plugins.ppr coARule <+> Plugins.ppr (Unpretty <$> coes)
    TyCoRep.UnivCo prov role ty ty' ->
      "UnivCo" <+> Plugins.ppr prov <+> Plugins.ppr role <+> Plugins.ppr ty <+> Plugins.ppr ty'
    TyCoRep.SymCo co -> "SymCo" <+> nestedCo co
    TyCoRep.TransCo co co' -> "TransCo" <+> nestedCo co <+> nestedCo co'
#if MIN_VERSION_ghc(8, 6, 0)
    TyCoRep.NthCo rule i co -> "NthCo" <+> Plugins.ppr rule <+> Plugins.ppr i <+> nestedCo co
#else
    TyCoRep.NthCo i co -> "NthCo" <+> Plugins.ppr i <+> nestedCo co
#endif
    TyCoRep.LRCo lr coN -> "LRCo" <+> Plugins.ppr lr <+> nestedCo coN
    TyCoRep.InstCo co coN -> "InstCo" <+> nestedCo co <+> nestedCo coN
    TyCoRep.KindCo co -> "KindCo" <+> nestedCo co
    TyCoRep.SubCo coN -> "SubCo" <+> nestedCo coN
    TyCoRep.HoleCo coH -> "HoleCo" <+> Plugins.ppr coH
    where
      nestedCo :: Plugins.Outputable (Unpretty a) => a -> Plugins.SDoc
      nestedCo = Plugins.parens . Plugins.ppr . Unpretty

#if MIN_VERSION_ghc(8, 6, 0)
instance Plugins.Outputable (Unpretty Plugins.MCoercion) where
  ppr (Unpretty mco) = case mco of
    Plugins.MRefl -> "MRefl"
    Plugins.MCo co -> "MCo" <+> Plugins.parens (Plugins.ppr (Unpretty co))
#endif
