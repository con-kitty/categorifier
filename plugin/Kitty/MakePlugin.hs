{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Create a GHC plugin that implements Conal Elliott's [Compiling to
--   Categories](http://conal.net/papers/compiling-to-categories/compiling-to-categories.pdf).
--
--   It replaces calls to `Kitty.Cat.Categorize.expression` with the expression in the target
--   category.
module Kitty.MakePlugin
  ( makePlugin,
    neverAutoInterpret,
  )
where

import Control.Applicative (liftA2)
import Control.Monad (join)
import Data.Either.Validation (Validation (..))
import Data.Foldable (toList)
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified ForeignCall as Plugins
import qualified GhcPlugins
import Kitty.Common.IO.Exception (throwIOAsException)
import Kitty.Plugin.CommandLineOptions (OptionGroup, partitionOptions)
import qualified Kitty.Plugin.Core
import Kitty.Plugin.Core.MakerMap (MakerMapFun)
import Kitty.Plugin.Core.Makers (Makers)
import qualified Kitty.Plugin.Core.PrimOp as PrimOp
import Kitty.Plugin.Core.Types (AutoInterpreter, CategoryStack)
import Kitty.Plugin.Hierarchy (Hierarchy, Lookup)
import PyF (fmt)

-- | What to use for the `AutoInterpreter` if you have no need to bypass the plugin.
neverAutoInterpret :: Lookup AutoInterpreter
neverAutoInterpret = pure $ \_ _ _ _ _ -> pure Nothing

-- | The required plugin entry-point. See [the GHC User's Guide section on Compiler
--   Plugins](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/extending_ghc.html#compiler-plugins) [ ](DONTLINTLINELENGTH)
--   for more information.
--
--   The plugin is (temporarily) parameterized to allow customization of the plugin. This should
--   eventually be replaced by dynamic loading of the desired values via command-line flags. But in
--   the mean time, each variation must be defined as a new plugin.
makePlugin ::
  -- | A mechanism to bypass the plugin, providing a mapping @(a -> b) -> cat a b@ for any special
  --   cases.
  Lookup AutoInterpreter ->
  -- | Extending support in the source language ... if you have operations that should map more
  --   directly than simply being inlined.
  MakerMapFun ->
  -- | Extending support for C operations.
  (Makers -> [(Plugins.CLabelString, (PrimOp.Boxer, [GhcPlugins.Type], GhcPlugins.Type))]) ->
  Lookup (Hierarchy CategoryStack) ->
  GhcPlugins.Plugin
makePlugin tryAutoInterpret makerMapFun additionalBoxers hierarchy =
  GhcPlugins.defaultPlugin
    { GhcPlugins.installCoreToDos =
        \opts ->
          join
            . GhcPlugins.liftIO
            . liftA2
              (Kitty.Plugin.Core.install tryAutoInterpret makerMapFun additionalBoxers hierarchy)
              (partitionOptions' opts)
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
