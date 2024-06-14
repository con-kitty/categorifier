module Categorifier.DynFlags
  ( plugin,
  )
where

import qualified Categorifier.GHC.Driver as GHC

plugin :: GHC.DynFlags -> GHC.DynFlags
plugin = setUpDynFlags

-- | This sets up flags that allow the plugin to do what it needs to.
--
--   For a compiler that doesn't support this plugin (before GHC 8.10), the following GHC options
--   approximate it:
--
--  [@-fno-ignore-interface-pragmas@]: Ensures we can inline definitions that we don't know how to
--                                     interpret directly to the target category. This flag is also
--                                     implied by @-O@ and @-O2@.
setUpDynFlags :: GHC.DynFlags -> GHC.DynFlags
setUpDynFlags = GHC.unSetGeneralFlag' GHC.Opt_IgnoreInterfacePragmas
