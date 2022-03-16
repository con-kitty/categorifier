{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Create a GHC plugin that implements Conal Elliott's [Compiling to
--   Categories](http://conal.net/papers/compiling-to-categories/compiling-to-categories.pdf).
--
--   It replaces calls to `Categorifier.Categorify.expression` with the expression in the target
--   category.
module Categorifier
  ( plugin,
  )
where

import Categorifier.CommandLineOptions (OptionGroup, partitionOptions)
import Categorifier.Common.IO.Exception (throwIOAsException)
import qualified Categorifier.Core
import Control.Applicative (liftA2)
import Control.Monad (join)
import Data.Either.Validation (Validation (..))
import Data.Foldable (toList)
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified GhcPlugins
import PyF (fmt)

-- | The required plugin entry-point. See [the GHC User's Guide section on Compiler
--   Plugins](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/extending_ghc.html#compiler-plugins) [ ](DONTLINTLINELENGTH)
--   for more information.
plugin :: GhcPlugins.Plugin
#if MIN_VERSION_ghc(8, 6, 0)
plugin =
  GhcPlugins.defaultPlugin
    { GhcPlugins.installCoreToDos =
        \opts ->
          join
            . GhcPlugins.liftIO
            . liftA2 Categorifier.Core.install (partitionOptions' opts)
            . pure,
      GhcPlugins.pluginRecompile = GhcPlugins.purePlugin
    }
#else
plugin =
  GhcPlugins.defaultPlugin
    { GhcPlugins.installCoreToDos =
        \opts ->
          join
            . GhcPlugins.liftIO
            . liftA2 Categorifier.Core.install (partitionOptions' opts)
            . pure
    }
#endif

partitionOptions' :: [GhcPlugins.CommandLineOption] -> IO (Map OptionGroup [Text])
partitionOptions' opts =
  case partitionOptions opts of
    Success groups -> pure groups
    Failure errs ->
      throwIOAsException
        ( \badOpts ->
            [fmt|  The following option groups passed to the Categorifier GHC plugin were
  unrecognized:
- {Text.intercalate "\n- " $ toList badOpts}|]
        )
        errs
