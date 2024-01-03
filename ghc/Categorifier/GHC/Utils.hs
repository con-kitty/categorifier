{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}

module Categorifier.GHC.Utils
  ( module ErrUtils,
    module Outputable,
    module Panic,
    module Util,
    HasLogger,
    Logger,
    getLogger,
    pprMsgEnvelopeBagWithLoc,
    renderWithStyle,
  )
where

import qualified Categorifier.GHC.Data as Data
import qualified Categorifier.GHC.Driver as Driver
#if MIN_VERSION_ghc(9, 2, 0)
import qualified GHC.Utils.Logger as Logger
#endif
#if MIN_VERSION_ghc(9, 0, 0)
#if MIN_VERSION_ghc(9, 2, 0)
import GHC.Utils.Error as ErrUtils hiding (pprMsgEnvelopeBagWithLoc)
import qualified GHC.Utils.Error as ErrUtils
import GHC.Utils.Outputable as Outputable
#else
import GHC.Utils.Error as ErrUtils
import GHC.Utils.Outputable as Outputable hiding (renderWithStyle)
import qualified GHC.Utils.Outputable as Outputable
#endif
import GHC.Utils.Misc as Util
import GHC.Utils.Panic as Panic
#else
import ErrUtils
import Outputable hiding (renderWithStyle)
import qualified Outputable
import Panic
import Util
#endif

#if MIN_VERSION_ghc(9, 2, 0)
type HasLogger = Logger.HasLogger
#else
type HasLogger = Applicative
#endif

#if MIN_VERSION_ghc(9, 2, 0)
type Logger = Logger.Logger
#else
type Logger = ()
#endif

getLogger :: (HasLogger m) => m Logger
#if MIN_VERSION_ghc(9, 2, 0)
getLogger = Logger.getLogger
#else
getLogger = pure ()
#endif

#if MIN_VERSION_ghc(9, 2, 0)
pprMsgEnvelopeBagWithLoc :: Data.Bag (MsgEnvelope DecoratedSDoc) -> [Outputable.SDoc]
pprMsgEnvelopeBagWithLoc = ErrUtils.pprMsgEnvelopeBagWithLoc
#else
pprMsgEnvelopeBagWithLoc :: Data.Bag ErrMsg -> [Outputable.SDoc]
pprMsgEnvelopeBagWithLoc = pprErrMsgBagWithLoc
#endif

renderWithStyle :: Driver.DynFlags -> PrintUnqualified -> SDoc -> String
#if MIN_VERSION_ghc(9, 2, 0)
renderWithStyle dflags = renderWithContext . Driver.initSDocContext dflags . mkDumpStyle
#elif MIN_VERSION_ghc(9, 0, 0)
renderWithStyle dflags = Outputable.renderWithStyle . Driver.initSDocContext dflags . mkDumpStyle
#else
renderWithStyle dflags qual sdoc = Outputable.renderWithStyle dflags sdoc (mkDumpStyle dflags qual)
#endif
