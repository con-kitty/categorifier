{-# LANGUAGE CPP #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Categorifier.GHC.Driver
  ( module DynFlags,
    module HscTypes,
    module Outputable,
    module Plugins,
    defaultPurePlugin,
    pureDynflagsAndCorePlugin,
  )
where

#if MIN_VERSION_ghc(9, 0, 0)
#if MIN_VERSION_ghc(9, 2, 0)
import GHC.Driver.Env.Types as HscTypes
import GHC.Driver.Ppr as Outputable
#else
import GHC.Driver.Types as HscTypes hiding
  ( InteractiveContext (..),
    InteractiveImport (..),
    ModGuts (..),
  )
import GHC.Utils.Outputable as Outputable hiding (Outputable (..), renderWithStyle)
#endif
import GHC.Driver.Plugins as Plugins
import GHC.Driver.Session as DynFlags
#else
import DynFlags
import HscTypes hiding (InteractiveContext (..), InteractiveImport (..), ModGuts (..))
import Outputable hiding (Outputable (..), renderWithStyle)
import Plugins
#endif

-- | Like `defaultPlugin`, but specifies that the plugin is pure (when GHC allows).
defaultPurePlugin :: Plugin
#if MIN_VERSION_ghc(8, 6, 0)
defaultPurePlugin = defaultPlugin {pluginRecompile = purePlugin}
#else
defaultPurePlugin = defaultPlugin
#endif

-- | Builds a pure plugin that has both `DynFlags` and `Core` components. Prior to GHC 8.10, the
--  `DynFlags` component will be ignored and so the flags must be manually configured to match.
pureDynflagsAndCorePlugin ::
  ([CommandLineOption] -> DynFlags -> IO DynFlags) ->
  -- | @([CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo])@, but skipped to avoid import issues
  _ ->
  Plugin
#if MIN_VERSION_ghc(9, 2, 0)
pureDynflagsAndCorePlugin dynflags core =
  defaultPurePlugin
    { driverPlugin =
        \opts hsc -> (\newFlags -> hsc {hsc_dflags = newFlags}) <$> dynflags opts (hsc_dflags hsc),
      installCoreToDos = core
    }
#elif MIN_VERSION_ghc(8, 10, 0)
pureDynflagsAndCorePlugin dynflags core =
  defaultPurePlugin {dynflagsPlugin = dynflags, installCoreToDos = core}
#else
pureDynflagsAndCorePlugin _ core = defaultPurePlugin {installCoreToDos = core}
#endif
