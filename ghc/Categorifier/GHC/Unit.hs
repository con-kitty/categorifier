{-# LANGUAGE CPP #-}

module Categorifier.GHC.Unit
  ( module Finder,
    module HscTypes,
    module Module,
    module State,
    findExposedPackageModule,
  )
where

#if MIN_VERSION_ghc(9, 0, 0)
import qualified GHC.Data.FastString as FastString
import qualified GHC.Driver.Config.Finder as DriverConfig
import qualified GHC.Driver.Env as DriverEnv
import qualified GHC.Types.PkgQual as PkgQual
#if MIN_VERSION_ghc(9, 2, 0)
import GHC.Unit.Finder as Finder hiding (findExposedPackageModule)
import qualified GHC.Unit.Finder as Finder
import GHC.Unit.Module.Deps as Module
import GHC.Unit.Module.ModGuts as HscTypes
#else
import GHC.Driver.Finder as Finder hiding (findExposedPackageModule)
import qualified GHC.Driver.Finder as Finder
import GHC.Driver.Types as HscTypes (ModGuts (..))
#endif
import GHC.Unit.Module.Name as Module
import GHC.Unit.State as State
import GHC.Unit.Types as Module
#else
import Finder
import HscTypes (ModGuts (..))
import Module
#endif

findExposedPackageModule :: DriverEnv.HscEnv -> ModuleName -> Maybe FastString.FastString -> IO FindResult
#if MIN_VERSION_ghc(9, 4, 0)
findExposedPackageModule env mod_name pkg_name = do
  finderCache <- initFinderCache
  Finder.findExposedPackageModule
    finderCache
    (DriverConfig.initFinderOpts $ DriverEnv.hsc_dflags env)
    emptyUnitState
    mod_name
    (maybe PkgQual.NoPkgQual (PkgQual.OtherPkg . UnitId) pkg_name)
#else
findExposedPackageModule = Finder.findExposedPackageModule
#endif
