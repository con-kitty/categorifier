{-# LANGUAGE CPP #-}

-- | This module defines a plugin that is designed to not play well with other plugins. It helps
--   ensure that we're programming our plugins defensively and at least being informative when we
--   can't recover.
module Test.Chaos
  ( plugin,
  )
where

import qualified GhcPlugins as Plugins
import P

-- | This plugin simply deletes all the `Plugins.CoreToDo`s that exist. It should surface any bugs
--   that are caused by your plugin relying on effects from other passes that exist incidentally.
plugin :: Plugins.Plugin
#if MIN_VERSION_ghc(8, 6, 0)
plugin =
  Plugins.defaultPlugin
    { Plugins.installCoreToDos = \_opts _todos -> pure [],
      Plugins.pluginRecompile = Plugins.purePlugin
    }
#else
plugin = Plugins.defaultPlugin {Plugins.installCoreToDos = \_opts _todos -> pure []}
#endif
