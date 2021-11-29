module Kitty.Plugin.Base (plugin) where

import qualified GhcPlugins
import Kitty.MakePlugin (makePlugin, neverAutoInterpret)
import Kitty.Plugin.Core.MakerMap (baseMakerMapFun)
import Kitty.Plugin.Hierarchy (baseHierarchy)

plugin :: GhcPlugins.Plugin
plugin = makePlugin neverAutoInterpret baseMakerMapFun (const []) baseHierarchy
