{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Categorifier.GhcBignum.Integration
  ( makerMapFun,
    symbolLookup,
  )
where

import Categorifier.Core.MakerMap
  ( MakerMapFun,
    SymbolLookup (..),
    makeLookupMap,
    makeMaker1,
    makeMaker2,
  )
import Categorifier.Core.Makers (Makers (..))
import Categorifier.Core.Types (Lookup)
import Categorifier.Duoidal ((=<\<))
import qualified Categorifier.GHC.Core as Plugins
import qualified Data.Map as Map
import qualified GHC.Num.BigNat
import qualified GHC.Num.Integer
import qualified GHC.Num.Natural

symbolLookup :: Lookup SymbolLookup
symbolLookup =
  makeLookupMap
    [''GHC.Num.BigNat.BigNat, ''GHC.Num.Integer.Integer, ''GHC.Num.Natural.Natural]

makerMapFun :: MakerMapFun
makerMapFun
  symLookup
  _dflags
  _logger
  m@Makers {..}
  _n
  _target
  expr
  _cat
  _var
  _args
  _modu
  _categorifyFun
  categorifyLambda =
    makerMap
    where
      makerMap =
        Map.fromListWith
          const
          [ ( 'GHC.Num.Integer.integerEq,
              \rest -> do
                integerTyCon <- Map.lookup ''GHC.Num.Integer.Integer (tyConLookup symLookup)
                pure $ maker2 rest =<\< mkEqual (Plugins.mkTyConTy integerTyCon)
            ),
            ( 'GHC.Num.Integer.integerNe,
              \rest -> do
                integerTyCon <- Map.lookup ''GHC.Num.Integer.Integer (tyConLookup symLookup)
                pure $ maker2 rest =<\< mkNotEqual (Plugins.mkTyConTy integerTyCon)
            ),
            ( 'GHC.Num.Integer.integerGe,
              \rest -> do
                integerTyCon <- Map.lookup ''GHC.Num.Integer.Integer (tyConLookup symLookup)
                pure $ maker2 rest =<\< mkGE (Plugins.mkTyConTy integerTyCon)
            ),
            ( 'GHC.Num.Integer.integerGt,
              \rest -> do
                integerTyCon <- Map.lookup ''GHC.Num.Integer.Integer (tyConLookup symLookup)
                pure $ maker2 rest =<\< mkGT (Plugins.mkTyConTy integerTyCon)
            ),
            ( 'GHC.Num.Integer.integerLe,
              \rest -> do
                integerTyCon <- Map.lookup ''GHC.Num.Integer.Integer (tyConLookup symLookup)
                pure $ maker2 rest =<\< mkLE (Plugins.mkTyConTy integerTyCon)
            ),
            ( 'GHC.Num.Integer.integerLt,
              \rest -> do
                integerTyCon <- Map.lookup ''GHC.Num.Integer.Integer (tyConLookup symLookup)
                pure $ maker2 rest =<\< mkLT (Plugins.mkTyConTy integerTyCon)
            ),
            ( 'GHC.Num.Integer.integerCompare,
              \rest -> do
                integerTyCon <- Map.lookup ''GHC.Num.Integer.Integer (tyConLookup symLookup)
                pure $ maker2 rest =<\< mkCompare (Plugins.mkTyConTy integerTyCon)
            ),
            ( 'GHC.Num.Integer.integerAdd,
              \rest -> do
                integerTyCon <- Map.lookup ''GHC.Num.Integer.Integer (tyConLookup symLookup)
                pure $ maker2 rest =<\< mkPlus (Plugins.mkTyConTy integerTyCon)
            ),
            ( 'GHC.Num.Integer.integerSub,
              \rest -> do
                integerTyCon <- Map.lookup ''GHC.Num.Integer.Integer (tyConLookup symLookup)
                pure $ maker2 rest =<\< mkMinus (Plugins.mkTyConTy integerTyCon)
            ),
            ( 'GHC.Num.Integer.integerMul,
              \rest -> do
                integerTyCon <- Map.lookup ''GHC.Num.Integer.Integer (tyConLookup symLookup)
                pure $ maker2 rest =<\< mkTimes (Plugins.mkTyConTy integerTyCon)
            ),
            ( 'GHC.Num.Integer.integerNegate,
              \rest -> do
                integerTyCon <- Map.lookup ''GHC.Num.Integer.Integer (tyConLookup symLookup)
                pure $ maker1 rest =<\< mkNegate (Plugins.mkTyConTy integerTyCon)
            ),
            ( 'GHC.Num.Integer.integerAbs,
              \rest -> do
                integerTyCon <- Map.lookup ''GHC.Num.Integer.Integer (tyConLookup symLookup)
                pure $ maker1 rest =<\< mkAbs (Plugins.mkTyConTy integerTyCon)
            ),
            ( 'GHC.Num.Integer.integerSignum,
              \rest -> do
                integerTyCon <- Map.lookup ''GHC.Num.Integer.Integer (tyConLookup symLookup)
                pure $ maker1 rest =<\< mkSignum (Plugins.mkTyConTy integerTyCon)
            ),
            ( 'GHC.Num.Integer.integerQuot,
              \rest -> do
                integerTyCon <- Map.lookup ''GHC.Num.Integer.Integer (tyConLookup symLookup)
                pure $ maker2 rest =<\< mkQuot (Plugins.mkTyConTy integerTyCon)
            ),
            ( 'GHC.Num.Integer.integerRem,
              \rest -> do
                integerTyCon <- Map.lookup ''GHC.Num.Integer.Integer (tyConLookup symLookup)
                pure $ maker2 rest =<\< mkRem (Plugins.mkTyConTy integerTyCon)
            ),
            ( 'GHC.Num.Natural.naturalEq,
              \rest -> do
                naturalTyCon <- Map.lookup ''GHC.Num.Natural.Natural (tyConLookup symLookup)
                pure $ maker2 rest =<\< mkEqual (Plugins.mkTyConTy naturalTyCon)
            ),
            ( 'GHC.Num.Natural.naturalNe,
              \rest -> do
                naturalTyCon <- Map.lookup ''GHC.Num.Natural.Natural (tyConLookup symLookup)
                pure $ maker2 rest =<\< mkNotEqual (Plugins.mkTyConTy naturalTyCon)
            ),
            ( 'GHC.Num.Natural.naturalGe,
              \rest -> do
                naturalTyCon <- Map.lookup ''GHC.Num.Natural.Natural (tyConLookup symLookup)
                pure $ maker2 rest =<\< mkGE (Plugins.mkTyConTy naturalTyCon)
            ),
            ( 'GHC.Num.Natural.naturalGt,
              \rest -> do
                naturalTyCon <- Map.lookup ''GHC.Num.Natural.Natural (tyConLookup symLookup)
                pure $ maker2 rest =<\< mkGT (Plugins.mkTyConTy naturalTyCon)
            ),
            ( 'GHC.Num.Natural.naturalLe,
              \rest -> do
                naturalTyCon <- Map.lookup ''GHC.Num.Natural.Natural (tyConLookup symLookup)
                pure $ maker2 rest =<\< mkLE (Plugins.mkTyConTy naturalTyCon)
            ),
            ( 'GHC.Num.Natural.naturalLt,
              \rest -> do
                naturalTyCon <- Map.lookup ''GHC.Num.Natural.Natural (tyConLookup symLookup)
                pure $ maker2 rest =<\< mkLT (Plugins.mkTyConTy naturalTyCon)
            ),
            ( 'GHC.Num.Natural.naturalCompare,
              \rest -> do
                naturalTyCon <- Map.lookup ''GHC.Num.Natural.Natural (tyConLookup symLookup)
                pure $ maker2 rest =<\< mkCompare (Plugins.mkTyConTy naturalTyCon)
            ),
            ( 'GHC.Num.Natural.naturalAdd,
              \rest -> do
                naturalTyCon <- Map.lookup ''GHC.Num.Natural.Natural (tyConLookup symLookup)
                pure $ maker2 rest =<\< mkPlus (Plugins.mkTyConTy naturalTyCon)
            ),
            ( 'GHC.Num.Natural.naturalSubThrow,
              \rest -> do
                naturalTyCon <- Map.lookup ''GHC.Num.Natural.Natural (tyConLookup symLookup)
                pure $ maker2 rest =<\< mkMinus (Plugins.mkTyConTy naturalTyCon)
            ),
            ( 'GHC.Num.Natural.naturalSubUnsafe,
              \rest -> do
                naturalTyCon <- Map.lookup ''GHC.Num.Natural.Natural (tyConLookup symLookup)
                pure $ maker2 rest =<\< mkMinus (Plugins.mkTyConTy naturalTyCon)
            ),
            ( 'GHC.Num.Natural.naturalMul,
              \rest -> do
                naturalTyCon <- Map.lookup ''GHC.Num.Natural.Natural (tyConLookup symLookup)
                pure $ maker2 rest =<\< mkTimes (Plugins.mkTyConTy naturalTyCon)
            ),
            ( 'GHC.Num.Natural.naturalSignum,
              \rest -> do
                naturalTyCon <- Map.lookup ''GHC.Num.Natural.Natural (tyConLookup symLookup)
                pure $ maker1 rest =<\< mkSignum (Plugins.mkTyConTy naturalTyCon)
            ),
            ( 'GHC.Num.Natural.naturalNegate,
              \rest -> do
                naturalTyCon <- Map.lookup ''GHC.Num.Natural.Natural (tyConLookup symLookup)
                pure $ maker1 rest =<\< mkNegate (Plugins.mkTyConTy naturalTyCon)
            ),
            ( 'GHC.Num.Natural.naturalQuot,
              \rest -> do
                naturalTyCon <- Map.lookup ''GHC.Num.Natural.Natural (tyConLookup symLookup)
                pure $ maker2 rest =<\< mkQuot (Plugins.mkTyConTy naturalTyCon)
            ),
            ( 'GHC.Num.Natural.naturalRem,
              \rest -> do
                naturalTyCon <- Map.lookup ''GHC.Num.Natural.Natural (tyConLookup symLookup)
                pure $ maker2 rest =<\< mkRem (Plugins.mkTyConTy naturalTyCon)
            )
          ]
      maker1 = makeMaker1 m categorifyLambda
      maker2 = makeMaker2 m categorifyLambda expr
