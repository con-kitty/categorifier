module Kitty.Plugin (plugin) where

import qualified GhcPlugins
import Kitty.MakePlugin (makePlugin, neverAutoInterpret)
import Kitty.Plugin.Core.MakerMap (baseMakerMapFun)

plugin :: GhcPlugins.Plugin
plugin = makePlugin neverAutoInterpret baseMakerMapFun (const [])
