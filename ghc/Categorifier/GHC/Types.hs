{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}

module Categorifier.GHC.Types
  ( module BasicTypes,
    module ErrUtils,
    module ForeignCall,
    module HscTypes,
    module Id,
    module IdInfo,
    module Literal,
    module Name,
    module RdrName,
    module SrcLoc,
    module UniqSet,
    module UniqSupply,
    module Unique,
    module Var,
    module VarEnv,
    module VarSet,
    pattern CCallSpec,
    pattern LitNumber,
    WithIdInfo (..),
    isEmptyMessages,
    mkLocalVar,
    mkSysLocal,
    setLiteralType,
    stringToName,
  )
where

import qualified Categorifier.GHC.Data as Data
import qualified Categorifier.GHC.Unit as Unit
import Categorifier.GHC.Utils ((<+>))
import qualified Categorifier.GHC.Utils as Utils
#if MIN_VERSION_ghc(9, 0, 0)
-- needed to avoid an import cycle
import qualified GHC.Core.TyCo.Rep as Core
#if MIN_VERSION_ghc(9, 4, 0)
import GHC.Driver.Errors.Types as ErrUtils
#else
import qualified GHC.Data.Bag as Bag
#endif
import GHC.Types.Basic as BasicTypes hiding (Inline)
import GHC.Types.ForeignCall as ForeignCall hiding (CCallSpec (..))
import qualified GHC.Types.ForeignCall as ForeignCall
#if MIN_VERSION_ghc(9, 2, 0)
#if MIN_VERSION_ghc(9, 4, 0)
import GHC.Types.Error as ErrUtils hiding (isEmptyMessages)
import qualified GHC.Types.Error as ErrUtils
#else
import GHC.Types.Error as ErrUtils
#endif
import GHC.Types.TyThing as HscTypes
#else
import GHC.Driver.Types as HscTypes hiding
  ( InteractiveContext (..),
    InteractiveImport (..),
    ModGuts (..),
  )
import GHC.Utils.Error as ErrUtils
#endif
import GHC.Types.Id as Id hiding (mkSysLocal)
import qualified GHC.Types.Id as Id
import GHC.Types.Id.Info as IdInfo
import GHC.Types.Literal as Literal hiding (LitNumber)
import qualified GHC.Types.Literal as Literal
import GHC.Types.Name as Name hiding (varName)
#if MIN_VERSION_ghc(9, 4, 0)
import GHC.Types.Name.Env as Name
#endif
import GHC.Types.Name.Reader as RdrName
import GHC.Types.SrcLoc as SrcLoc
import GHC.Types.Unique as Unique
import GHC.Types.Unique.Set as UniqSet
import GHC.Types.Unique.Supply as UniqSupply
import GHC.Types.Var as Var hiding (lazySetIdInfo, mkLocalVar, setIdExported, setIdNotExported)
import qualified GHC.Types.Var as Var
import GHC.Types.Var.Env as VarEnv
import GHC.Types.Var.Set as VarSet
#else
import BasicTypes hiding (Inline)
import qualified Categorifier.GHC.Driver as Driver
import ErrUtils
import HscTypes hiding (InteractiveContext (..), InteractiveImport (..), ModGuts (..))
import ForeignCall hiding (CCallSpec (..))
import qualified ForeignCall
import Id hiding (mkSysLocal)
import qualified Id
import IdInfo
import Literal hiding (LitNumber)
import qualified Literal
import Name hiding (varName)
import RdrName
import SrcLoc
-- needed to avoid an import cycle
import qualified TyCoRep as Core
import UniqSet
import UniqSupply
import Unique
import Var hiding (lazySetIdInfo, mkLocalVar, setIdExported, setIdNotExported)
import qualified Var
import VarEnv
import VarSet
#endif
import PyF (fmt)

pattern CCallSpec :: CCallTarget -> CCallConv -> Safety -> ForeignCall.CCallSpec
#if MIN_VERSION_ghc(8, 10, 7) && !MIN_VERSION_ghc(9, 0, 0)
pattern CCallSpec target conv safety <- ForeignCall.CCallSpec target conv safety _ _
#else
pattern CCallSpec target conv safety = ForeignCall.CCallSpec target conv safety
#endif

pattern LitNumber :: Integer -> Literal
#if MIN_VERSION_ghc(9, 0, 0)
pattern LitNumber n <- Literal.LitNumber _ n
#else
pattern LitNumber n <- Literal.LitNumber _ n _
#endif

#if MIN_VERSION_ghc(9, 4, 0)
isEmptyMessages :: Messages e -> Bool
isEmptyMessages = ErrUtils.isEmptyMessages
#else
isEmptyMessages :: Bag.Bag a -> Bool
isEmptyMessages = Bag.isEmptyBag
#endif

mkLocalVar :: IdDetails -> Name -> Core.Type -> IdInfo -> Id
#if MIN_VERSION_ghc(9, 0, 0)
mkLocalVar details name ty = Var.mkLocalVar details name ty ty
#else
mkLocalVar = Var.mkLocalVar
#endif

mkSysLocal :: Data.FastString -> Unique -> Core.Type -> Id
#if MIN_VERSION_ghc(9, 0, 0)
mkSysLocal fs uniq ty = Id.mkSysLocal fs uniq ty ty
#else
mkSysLocal = Id.mkSysLocal
#endif

setLiteralType :: Core.Type -> Literal -> Literal
#if MIN_VERSION_ghc(9, 0, 0)
#else
setLiteralType toType (Literal.LitNumber litNumTy litNumVal _oldType) =
  Literal.LitNumber litNumTy litNumVal toType
#endif
setLiteralType _ x = x

stringToName :: String -> Name
stringToName str =
  mkSystemVarName
    -- When mkUniqueGrimily's argument is negative, we see something like
    -- "Exception: Prelude.chr: bad argument: (-52)". Hence the abs.
    (mkUniqueGrimily (abs (fromIntegral (Utils.hashString str))))
    (Data.mkFastString str)

newtype WithIdInfo = WithIdInfo Id

#if MIN_VERSION_ghc(9, 0, 0)
instance Utils.Outputable WithIdInfo where
  -- I wanted the full IdInfo, but it's not `Utils.Outputable`
  ppr (WithIdInfo v) =
    Utils.sdocWithContext $ \ctx ->
      let ident =
            ( if Utils.sdocSuppressModulePrefixes ctx
                then id
                else
                  ( maybe
                      ""
                      (\m -> [fmt|{Unit.moduleNameString $ Unit.moduleName m}.|])
                      (nameModule_maybe $ varName v)
                      Utils.<>
                  )
            )
              $ Utils.ppr v
       in if Utils.sdocSuppressTypeSignatures ctx
            then ident
            else
              Utils.sep
                [ident, Utils.nest 2 $ Utils.dcolon <+> Utils.ppr (varType v)]
#else
instance Utils.Outputable WithIdInfo where
  -- I wanted the full IdInfo, but it's not `Utils.Outputable`
  ppr (WithIdInfo v) =
    Utils.sdocWithDynFlags $ \dflags ->
      let ident =
            ( if Driver.gopt Driver.Opt_SuppressModulePrefixes dflags
                then id
                else
                  ( maybe
                      ""
                      (\m -> [fmt|{Unit.moduleNameString $ Unit.moduleName m}.|])
                      (nameModule_maybe $ varName v)
                      Utils.<>
                  )
            )
              $ Utils.ppr v
       in if Driver.gopt Driver.Opt_SuppressTypeSignatures dflags
            then ident
            else
              Utils.sep
                [ident, Utils.nest 2 $ Utils.dcolon <+> Utils.ppr (varType v)]
#endif

instance Utils.OutputableBndr WithIdInfo where
  pprInfixOcc = Utils.ppr
  pprPrefixOcc = Utils.ppr
