module Kitty.Plugin.ConCat.Class (plugin) where

import qualified GhcPlugins
import Kitty.MakePlugin (makePlugin, neverAutoInterpret)
import Kitty.Plugin.Core.MakerMap (baseMakerMapFun)
import qualified Kitty.Plugin.Hierarchy.ConCat as ConCat

plugin :: GhcPlugins.Plugin
plugin = makePlugin neverAutoInterpret baseMakerMapFun (const []) ConCat.classHierarchy
