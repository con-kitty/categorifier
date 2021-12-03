{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Create a GHC plugin that implements Conal Elliott's [Compiling to
--   Categories](http://conal.net/papers/compiling-to-categories/compiling-to-categories.pdf).
--
--   It replaces calls to `Kitty.Cat.Categorize.expression` with the expression in the target
--   category.
module Kitty.Plugin
  ( plugin,
  )
where

import Control.Applicative (liftA2)
import Control.Monad (join)
import Data.Either.Validation (Validation (..))
import Data.Foldable (toList)
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified GhcPlugins
import Kitty.Common.IO.Exception (throwIOAsException)
import Kitty.Plugin.CommandLineOptions (OptionGroup, partitionOptions)
import qualified Kitty.Plugin.Core
import PyF (fmt)

-- | The required plugin entry-point. See [the GHC User's Guide section on Compiler
--   Plugins](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/extending_ghc.html#compiler-plugins) [ ](DONTLINTLINELENGTH)
--   for more information.
plugin :: GhcPlugins.Plugin
plugin =
  GhcPlugins.defaultPlugin
    { GhcPlugins.installCoreToDos =
        \opts ->
          join
            . GhcPlugins.liftIO
            . liftA2 Kitty.Plugin.Core.install (partitionOptions' opts)
            . pure,
      GhcPlugins.pluginRecompile = GhcPlugins.purePlugin
    }

partitionOptions' :: [GhcPlugins.CommandLineOption] -> IO (Map OptionGroup [Text])
partitionOptions' opts =
  case partitionOptions opts of
    Success groups -> pure groups
    Failure errs ->
      throwIOAsException
        ( \badOpts ->
            [fmt|  The following option groups passed to the Kitty.Plugin GHC plugin were
  unrecognized:
- {Text.intercalate "\n- " $ toList badOpts}|]
        )
        errs
