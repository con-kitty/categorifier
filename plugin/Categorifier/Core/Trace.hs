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
    addIdInfo,
  )
where

import qualified Categorifier.GHC.Core as Plugins
import qualified Categorifier.GHC.Driver as Plugins
import qualified Categorifier.GHC.Types as Plugins
import qualified Categorifier.GHC.Unit as Plugins
import qualified Categorifier.GHC.Utils as Plugins
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Bifunctor (Bifunctor (..))
import Data.Bool (bool)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import qualified Data.List.Extra as List
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Set as Set
import Debug.Trace (trace, traceM)
import PyF (fmt)
import System.IO.Unsafe (unsafePerformIO)
import System.Time.Extra (Seconds, offsetTime)

-- | Like 'Plugins.showSDoc', but qualifies some ambiguous names, and also shortens
-- large output.
renderSDoc :: Plugins.DynFlags -> Plugins.SDoc -> String
renderSDoc dflags sdoc = takeLines 20 20 $ Plugins.renderWithStyle dflags qual sdoc
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

addIdInfo :: Plugins.CoreExpr -> Plugins.Expr Plugins.WithIdInfo
addIdInfo = \case
  Plugins.Var v -> Plugins.Var v
  Plugins.Lit l -> Plugins.Lit l
  Plugins.App e a -> Plugins.App (addIdInfo e) (addIdInfo a)
  Plugins.Lam b e -> Plugins.Lam (Plugins.WithIdInfo b) (addIdInfo e)
  Plugins.Let b e -> Plugins.Let (addIdInfoBind b) (addIdInfo e)
  Plugins.Case scrut bind ty alts ->
    Plugins.Case (addIdInfo scrut) (Plugins.WithIdInfo bind) ty $ fmap addIdInfoAlt alts
  Plugins.Cast e c -> Plugins.Cast (addIdInfo e) c
  Plugins.Tick tickish e -> Plugins.Tick tickish $ addIdInfo e
  Plugins.Type ty -> Plugins.Type ty
  Plugins.Coercion c -> Plugins.Coercion c
  where
    addIdInfoAlt (Plugins.Alt con binds expr) =
      Plugins.Alt con (fmap Plugins.WithIdInfo binds) (addIdInfo expr)
    addIdInfoBind = \case
      Plugins.NonRec b e -> Plugins.NonRec (Plugins.WithIdInfo b) $ addIdInfo e
      Plugins.Rec alts -> Plugins.Rec $ fmap (bimap Plugins.WithIdInfo addIdInfo) alts
