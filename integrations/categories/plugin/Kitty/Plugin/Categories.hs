module Kitty.Plugin.Categories (plugin) where

import qualified GhcPlugins
import Kitty.MakePlugin (makePlugin, neverAutoInterpret)
import Kitty.Plugin.Core.MakerMap (baseMakerMapFun)
import qualified Kitty.Plugin.Hierarchy.Categories as Categories

plugin :: GhcPlugins.Plugin
plugin = makePlugin neverAutoInterpret baseMakerMapFun (const []) Categories.hierarchy
