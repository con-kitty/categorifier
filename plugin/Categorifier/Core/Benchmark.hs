module Categorifier.Core.Benchmark
  ( Account (..),
    billTo,
    billToUninterruptible,
    displayTimes,
  )
where

import qualified Categorifier.Benchmark as Benchmark
import Control.Monad.IO.Class (MonadIO (..))
import Data.IORef.Extra (IORef, newIORef)
import qualified Data.Map.Strict as Map
import System.IO.Unsafe (unsafePerformIO)

data Account
  = BuildDictionary
  | Categorify
  deriving (Eq, Ord, Show)

ref :: IORef (Benchmark.Benchmark Account)
ref = unsafePerformIO . newIORef $ Benchmark.Benchmark Nothing Map.empty
{-# NOINLINE ref #-}

billTo :: MonadIO m => Bool -> Account -> m r -> m r
billTo enableDebugging = Benchmark.billTo enableDebugging ref

billToUninterruptible :: MonadIO m => Bool -> Account -> m r -> m r
billToUninterruptible enableDebugging = Benchmark.billToUninterruptible enableDebugging ref

displayTimes :: MonadIO m => m ()
displayTimes = Benchmark.displayTimes ref
