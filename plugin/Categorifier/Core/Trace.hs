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
import GhcPlugins ((<+>))
import qualified GhcPlugins as Plugins
import PyF (fmt)
import System.IO.Unsafe (unsafePerformIO)
import System.Time.Extra (Seconds, offsetTime)
import qualified TyCoRep

-- | Like 'Plugins.showSDoc', but qualifies some ambiguous names, and also shortens
-- large output.
renderSDoc :: Plugins.DynFlags -> Plugins.SDoc -> String
renderSDoc dflags sdoc = takeLines 20 20 $ Plugins.renderWithStyle dflags sdoc style
  where
    style = Plugins.mkDumpStyle dflags qual
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
  | otherwise = prefix <> omitted <> suffix
  where
    (xs, ys) = List.splitAt x (List.lines s)
    (zs, ws) = List.splitAtEnd y ys
    prefix = List.intercalate "\n" xs
    omitted =
      if null zs
        then ""
        else
          (if x == 0 then "" else "\n")
            <> "<...omitted "
            <> show (length zs)
            <> " lines>"
    suffix =
      if null ws || y == 0
        then ""
        else
          (if x == 0 && null zs then "" else "\n")
            <> List.intercalate "\n" ws

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
    TyCoRep.Refl ty -> "Refl" <+> Plugins.ppr ty
    TyCoRep.GRefl role ty mco -> "GRefl" <+> Plugins.ppr role <+> Plugins.ppr ty <+> nestedCo mco
    TyCoRep.TyConAppCo role tyCon coes ->
      "TyConAppCo" <+> Plugins.ppr role <+> Plugins.ppr tyCon <+> Plugins.ppr (Unpretty <$> coes)
    TyCoRep.AppCo co coN -> "AppCo" <+> nestedCo co <+> nestedCo coN
    TyCoRep.ForAllCo tyCoVar kCo co ->
      "ForAllCo" <+> Plugins.ppr tyCoVar <+> nestedCo kCo <+> nestedCo co
    TyCoRep.FunCo role co co' -> "FunCo" <+> Plugins.ppr role <+> nestedCo co <+> nestedCo co'
    TyCoRep.CoVarCo coVar -> "CoVarCo" <+> Plugins.ppr coVar
    TyCoRep.AxiomInstCo coA brI coes ->
      "AxiomInstCo" <+> Plugins.ppr coA <+> Plugins.ppr brI <+> Plugins.ppr (Unpretty <$> coes)
    TyCoRep.AxiomRuleCo coARule coes ->
      "AxiomRuleCo" <+> Plugins.ppr coARule <+> Plugins.ppr (Unpretty <$> coes)
    TyCoRep.UnivCo prov role ty ty' ->
      "UnivCo" <+> Plugins.ppr prov <+> Plugins.ppr role <+> Plugins.ppr ty <+> Plugins.ppr ty'
    TyCoRep.SymCo co -> "SymCo" <+> nestedCo co
    TyCoRep.TransCo co co' -> "TransCo" <+> nestedCo co <+> nestedCo co'
    TyCoRep.NthCo rule i co -> "NthCo" <+> Plugins.ppr rule <+> Plugins.ppr i <+> nestedCo co
    TyCoRep.LRCo lr coN -> "LRCo" <+> Plugins.ppr lr <+> nestedCo coN
    TyCoRep.InstCo co coN -> "InstCo" <+> nestedCo co <+> nestedCo coN
    TyCoRep.KindCo co -> "KindCo" <+> nestedCo co
    TyCoRep.SubCo coN -> "SubCo" <+> nestedCo coN
    TyCoRep.HoleCo coH -> "HoleCo" <+> Plugins.ppr coH
    where
      nestedCo :: Plugins.Outputable (Unpretty a) => a -> Plugins.SDoc
      nestedCo = Plugins.parens . Plugins.ppr . Unpretty

instance Plugins.Outputable (Unpretty Plugins.MCoercion) where
  ppr (Unpretty mco) = case mco of
    Plugins.MRefl -> "MRefl"
    Plugins.MCo co -> "MCo" <+> Plugins.parens (Plugins.ppr (Unpretty co))
