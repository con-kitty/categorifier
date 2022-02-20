{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module Kitty.Benchmark
  ( Benchmark (..),
    billTo,
    billToUninterruptible,
    displayTimes,
  )
where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Foldable (for_)
import Data.Functor (($>))
import Data.IORef.Extra (IORef, modifyIORef', readIORef, writeIORef')
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Debug.Trace (traceM)
import Kitty.Common.IO.Exception (evaluate)
import PyF (fmt)
import System.IO.Unsafe (unsafePerformIO)
import System.Time.Extra (Seconds, offsetTime)

data Benchmark a
  = Benchmark
      (Maybe (a, Seconds))
      -- ^ The (interruptible) account currently being billed
      (Map a Seconds)
      -- ^ Meters

getElapsed :: IO Seconds
getElapsed = unsafePerformIO offsetTime
{-# NOINLINE getElapsed #-}

addTime :: forall a. Ord a => Seconds -> a -> Map a Seconds -> Map a Seconds
addTime time = Map.alter (maybe (Just time) (Just . (+ time)))

-- | Switch to the given account, and return the old account
switchAccount :: forall a. Ord a => IORef (Benchmark a) -> Maybe a -> IO (Maybe a)
switchAccount ref mbNewAccount = do
  currentTime <- getElapsed
  Benchmark mbOldAccount oldMeters <- readIORef ref
  let meters = case mbOldAccount of
        Just (account, startTime) -> addTime (currentTime - startTime) account oldMeters
        Nothing -> oldMeters
  writeIORef' ref $ Benchmark (fmap (,currentTime) mbNewAccount) meters
  pure $ fst <$> mbOldAccount

-- | Bill to the given account. If the given action calls `billTo` or
-- `billToUninterruptible` on a sub-action, billing to the current account
--  will be suspended until the sub-action completes.
billTo :: forall a r m. (MonadIO m, Ord a) => Bool -> IORef (Benchmark a) -> a -> m r -> m r
billTo enableDebugging ref newAccount act
  | not enableDebugging = act
  | otherwise = do
      oldAccount <- liftIO $ switchAccount ref (Just newAccount)
      res <- act >>= liftIO . evaluate
      -- TODO: this should ideally be in `finally`, but `CategoryStack` is not `MonadUnliftIO`.
      liftIO (switchAccount ref oldAccount) $> res

-- | Like `billTo`, but keep billing to the given account even if the given
-- action calls `billTo` or `billToUninterruptible`.
billToUninterruptible ::
  forall a r m.
  (MonadIO m, Ord a) =>
  Bool ->
  IORef (Benchmark a) ->
  a ->
  m r ->
  m r
billToUninterruptible enableDebugging ref newAccount act
  | not enableDebugging = act
  | otherwise = do
      oldAccount <- liftIO $ switchAccount ref Nothing
      startTime <- liftIO getElapsed
      res <- act >>= liftIO . evaluate
      -- TODO: this should ideally be in `finally`, but `CategoryStack` is not `MonadUnliftIO`.
      endTime <- liftIO getElapsed
      liftIO $
        modifyIORef' ref $ \(Benchmark account meters) ->
          Benchmark account $ addTime (endTime - startTime) newAccount meters
      liftIO (switchAccount ref oldAccount) $> res

displayTimes :: forall a m. (MonadIO m, Show a) => IORef (Benchmark a) -> m ()
displayTimes ref = liftIO $ do
  traceM "============Benchmark============"
  Benchmark _ meters <- readIORef ref
  for_ (sortOn (negate . snd) $ Map.toList meters) $ \(account, time) ->
    traceM [fmt|{show account}: {time:.2}s|]
  traceM "========End of Benchmark========="
