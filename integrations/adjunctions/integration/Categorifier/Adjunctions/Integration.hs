{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Categorifier.Adjunctions.Integration
  ( hierarchy,
    makerMapFun,
  )
where

import Categorifier.Core.MakerMap (MakerMapFun, baseMakerMapFun, makeMaker1)
import Categorifier.Core.Makers (Makers (..))
import Categorifier.Core.Types (CategoryStack, Lookup)
import Categorifier.Duoidal ((=<\<))
import Categorifier.Hierarchy
  ( Hierarchy (..),
    emptyHierarchy,
    findTyCon,
    funTy,
    identifier,
    mkMethodApps,
  )
import qualified Data.Functor.Rep
import qualified Data.Map as Map
import qualified GHC.Base
#if MIN_VERSION_ghc(9, 0, 0)
import qualified GHC.Plugins as Plugins
#else
import qualified GhcPlugins as Plugins
#endif

hierarchy :: Lookup (Hierarchy CategoryStack)
hierarchy = do
  kindexV <-
    pure <$> do
      arr <- identifier "Control.Arrow" "arr"
      op <- identifier "Data.Functor.Rep" "index"
      rep <- findTyCon "Data.Functor.Rep" "Rep"
      pure $ \onDict cat f a -> do
        let repfTy = Plugins.mkTyConApp rep [f]
        op' <- mkMethodApps onDict op [f] [a] []
        mkMethodApps onDict arr [cat] [Plugins.mkAppTy f a, funTy repfTy a] [op']
  ktabulateV <-
    pure <$> do
      arr <- identifier "Control.Arrow" "arr"
      op <- identifier "Data.Functor.Rep" "tabulate"
      rep <- findTyCon "Data.Functor.Rep" "Rep"
      pure $ \onDict cat f a -> do
        let repfTy = Plugins.mkTyConApp rep [f]
        op' <- mkMethodApps onDict op [f] [a] []
        mkMethodApps onDict arr [cat] [funTy repfTy a, Plugins.mkAppTy f a] [op']
  pure emptyHierarchy {indexV = kindexV, tabulateV = ktabulateV}

makerMapFun :: MakerMapFun
makerMapFun
  dflags
  m@Makers {..}
  n
  target
  expr
  cat
  var
  args
  modu
  categorifyFun
  categorifyLambda =
    Map.fromListWith
      const
      [ ( 'Data.Functor.Rep.apRep,
          \case
            f : a : b : representable : rest ->
              ($ (f : representable : a : b : rest)) =<< Map.lookup '(GHC.Base.<*>) baseMakerMap
            _ -> Nothing
        ),
        ( 'Data.Functor.Rep.bindRep,
          \case
            f : a : b : representable : rest ->
              ($ (f : representable : a : b : rest)) =<< Map.lookup '(GHC.Base.>>=) baseMakerMap
            _ -> Nothing
        ),
        ( 'Data.Functor.Rep.fmapRep,
          \case
            f : a : b : representable : rest ->
              ($ (f : representable : a : b : rest)) =<< Map.lookup 'GHC.Base.fmap baseMakerMap
            _ -> Nothing
        ),
        ( 'Data.Functor.Rep.index,
          \case
            Plugins.Type f : _representable : Plugins.Type a : rest ->
              pure $ maker1 rest =<\< mkIndex f a
            _ -> Nothing
        ),
        ( 'Data.Functor.Rep.liftR2,
          \case
            f : a : b : c : representable : rest ->
              ($ (f : representable : a : b : c : rest))
                =<< Map.lookup 'GHC.Base.liftA2 baseMakerMap
            _ -> Nothing
        ),
        ( 'Data.Functor.Rep.pureRep,
          \case
            f : a : representable : rest ->
              ($ (f : representable : a : rest)) =<< Map.lookup 'GHC.Base.pure baseMakerMap
            _ -> Nothing
        ),
        ( 'Data.Functor.Rep.tabulate,
          \case
            Plugins.Type f : _representable : Plugins.Type a : rest ->
              pure $ maker1 rest =<\< mkTabulate f a
            _ -> Nothing
        )
      ]
    where
      baseMakerMap =
        baseMakerMapFun
          dflags
          m
          n
          target
          expr
          cat
          var
          args
          modu
          categorifyFun
          categorifyLambda
      maker1 = makeMaker1 m categorifyLambda
