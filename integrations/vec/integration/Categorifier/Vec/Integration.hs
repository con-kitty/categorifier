{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Categorifier.Vec.Integration
  ( symbolLookup,
    makerMapFun,
  )
where

import Categorifier.Core.MakerMap (MakerMapFun, SymbolLookup (..), applyEnrichedCat', makeMaker1, makeMaker2)
import Categorifier.Core.Makers (Makers (..))
import Categorifier.Core.Types (Lookup)
import Categorifier.Duoidal (joinD, (<*\>), (=<\<))
import qualified Categorifier.GHC.Builtin as Plugins
import qualified Categorifier.GHC.Core as Plugins
import qualified Categorifier.GHC.Types as Plugins
import Categorifier.Hierarchy (findTyCon)
import qualified Data.Map as Map
import qualified Data.Vec.Lazy

symbolLookup :: Lookup SymbolLookup
symbolLookup = do
  vec <- findTyCon "Data.Vec.Lazy" "Vec"
  pure $ SymbolLookup (Map.singleton ''Data.Vec.Lazy.Vec vec) mempty

makerMapFun :: MakerMapFun
makerMapFun
  lookup
  _dflags
  _logger
  m@Makers {..}
  n
  _target
  expr
  _cat
  _var
  _args
  _modu
  _categorifyFun
  categorifyLambda =
    Map.fromListWith
      const
      [ ( '(Data.Vec.Lazy.!),
          \case
            Plugins.Type n' : Plugins.Type a : rest -> do
              vec <- Map.lookup ''Data.Vec.Lazy.Vec (tyConLookup lookup)
              pure $ maker1 rest =<\< mkIndex (Plugins.mkTyConApp vec [n']) a
            _ -> Nothing
        ),
        ( 'Data.Vec.Lazy.bind,
          \case
            Plugins.Type n' : Plugins.Type a : Plugins.Type b : rest -> do
              vec <- Map.lookup ''Data.Vec.Lazy.Vec (tyConLookup lookup)
              pure $ maker2 rest =<\< mkBind (Plugins.mkTyConApp vec [n']) a b
            _ -> Nothing
        ),
        ( 'Data.Vec.Lazy.map,
          \case
            Plugins.Type a : Plugins.Type b : Plugins.Type n' : u : rest -> do
              -- from: (\n -> map {{u}}) :: n -> [a] -> [b]
              -- to:   curry (map (uncurry (categorifyLambda n {{u}})) . strength)
              --         :: n `k` ([a] -> [b])
              vec <- Map.lookup ''Data.Vec.Lazy.Vec (tyConLookup lookup)
              let f = Plugins.mkTyConApp vec [n']
              pure . joinD $
                applyEnriched' [u] rest
                  <$> mkMap f (Plugins.mkBoxedTupleTy [Plugins.varType n, a]) b
                  <*\> mkStrength f (Plugins.varType n) a
            _ -> Nothing
        ),
        ( 'Data.Vec.Lazy.sum,
          \case
            Plugins.Type a : Plugins.Type n' : _num : rest -> do
              vec <- Map.lookup ''Data.Vec.Lazy.Vec (tyConLookup lookup)
              pure $ maker1 rest =<\< mkSum (Plugins.mkTyConApp vec [n']) a
            _ -> Nothing
        ),
        ( 'Data.Vec.Lazy.tabulate,
          \case
            Plugins.Type n' : Plugins.Type a : _snati : rest -> do
              vec <- Map.lookup ''Data.Vec.Lazy.Vec (tyConLookup lookup)
              pure $ maker1 rest =<\< mkTabulate (Plugins.mkTyConApp vec [n']) a
            _ -> Nothing
        ),
        ( 'Data.Vec.Lazy.traverse,
          \case
            Plugins.Type n' : Plugins.Type f : Plugins.Type a : Plugins.Type b
              : _applicative
              : u
              : rest -> do
                vec <- Map.lookup ''Data.Vec.Lazy.Vec (tyConLookup lookup)
                let t = Plugins.mkTyConApp vec [n']
                pure . joinD $
                  applyEnriched' [u] rest
                    <$> mkTraverse t f (Plugins.mkBoxedTupleTy [Plugins.varType n, a]) b
                    <*\> mkStrength t (Plugins.varType n) a
            _ -> Nothing
        )
      ]
    where
      applyEnriched' = applyEnrichedCat' m categorifyLambda
      maker1 = makeMaker1 m categorifyLambda
      maker2 = makeMaker2 m categorifyLambda expr
