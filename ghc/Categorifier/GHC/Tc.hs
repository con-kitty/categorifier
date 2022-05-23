{-# LANGUAGE CPP #-}

module Categorifier.GHC.Tc
  ( module TcErrors,
    module TcEvidence,
    module TcHsSyn,
    module TcInteract,
    module TcRnDriver,
    module TcRnMonad,
    module TcRnTypes,
    module TcSMonad,
    module TcSimplify,
    module TcType,
    runTcInteractive,
  )
where

import qualified Categorifier.GHC.Driver as Driver
import qualified Categorifier.GHC.Types as Types
#if MIN_VERSION_ghc(9, 2, 0)
import Control.Arrow (Arrow (..))
#endif
#if MIN_VERSION_ghc(9, 0, 0)
import GHC.Tc.Errors as TcErrors
import GHC.Tc.Module as TcRnDriver hiding (runTcInteractive)
import qualified GHC.Tc.Module as TcRnDriver
import GHC.Tc.Solver as TcSimplify
import GHC.Tc.Solver.Interact as TcInteract
import GHC.Tc.Solver.Monad as TcSMonad (TcS, runTcS)
import GHC.Tc.Types as TcRnTypes
import GHC.Tc.Types.Constraint as TcRnTypes
import GHC.Tc.Types.Evidence as TcEvidence
import GHC.Tc.Types.Origin as TcType
import GHC.Tc.Utils.Monad as TcRnMonad
import GHC.Tc.Utils.Zonk as TcHsSyn
#else
#if MIN_VERSION_ghc(8, 10, 0)
import Constraint as TcRnTypes
import TcOrigin as TcType
#else
import TcRnTypes
import TcType
#endif
import TcErrors
import TcEvidence
import TcHsSyn
import TcInteract
import TcRnDriver hiding (runTcInteractive)
import qualified TcRnDriver
import TcRnMonad
import TcSMonad (TcS, runTcS)
import TcSimplify
#endif

runTcInteractive ::
  Driver.HscEnv -> TcRn a -> IO ((Types.ErrorMessages, Types.WarningMessages), Maybe a)
#if MIN_VERSION_ghc(9, 2, 0)
runTcInteractive env =
  fmap (first (Types.getErrorMessages &&& Types.getWarningMessages))
    . TcRnDriver.runTcInteractive env
#else
runTcInteractive = TcRnDriver.runTcInteractive
#endif
