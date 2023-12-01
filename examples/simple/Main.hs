{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import qualified Categorifier.Categorify as Categorify
import Categorifier.Client (deriveHasRep)
import qualified Control.Arrow as Base
import Control.Categorical.Bifunctor
  ( Bifunctor (..),
    PFunctor (..),
    QFunctor (..),
  )
import Control.Category (Category (..))
import Control.Category.Associative (Associative (..))
import Control.Category.Braided (Braided (..), Symmetric)
import Control.Category.Cartesian (Cartesian (..))
import Control.Category.Cartesian.Closed (CCC (..))
import Control.Category.Monoidal (Monoidal (..))
import Control.Monad ((<=<))
import Control.Monad.State (State, runState)
import Control.Monad.State.Class (get, modify, put)
import Data.Kind (Type)

type Port = Int

genPort :: GraphM Port
genPort = do
  (o, comps) <- get
  put (o + 1, comps)
  pure o

data Ports :: Type -> Type where
  UnitP :: Ports ()
  BoolP :: Port -> Ports Bool
  IntP :: Port -> Ports Int
  PairP :: Ports a -> Ports b -> Ports (a, b)
  FunP :: Graph a b -> Ports (a -> b)

class GenPorts a where
  genPorts :: GraphM (Ports a)

instance GenPorts () where
  genPorts = pure UnitP

instance GenPorts Bool where
  genPorts = BoolP <$> genPort

instance GenPorts Int where
  genPorts = IntP <$> genPort

instance (GenPorts a, GenPorts b) => GenPorts (a, b) where
  genPorts = PairP <$> genPorts <*> genPorts

data Comp where
  Comp :: forall a b. String -> (Ports a) -> (Ports b) -> Comp

genComp :: GenPorts b => String -> Graph a b
genComp name = Graph $ \a -> do
  b <- genPorts
  modify (Base.second (Comp name a b :))
  pure b

type GraphM = State (Port, [Comp])

data Graph a b = Graph {runGraph :: Ports a -> GraphM (Ports b)}
{-# ANN Graph "Hlint: ignore Use newtype instead of data" #-}

deriveHasRep ''Graph

instance Category Graph where
  id = Graph pure
  Graph f . Graph g = Graph (f <=< g)

instance PFunctor (,) Graph Graph where
  first (Graph f) = Graph $ \(PairP a c) -> do
    b <- f a
    pure (PairP b c)

instance QFunctor (,) Graph Graph where
  second (Graph f) = Graph $ \(PairP c a) -> do
    b <- f a
    pure (PairP c b)

instance Bifunctor (,) Graph Graph Graph where
  bimap (Graph f) (Graph g) = Graph $ \(PairP a b) -> do
    c <- f a
    d <- g b
    pure (PairP c d)

instance Associative Graph (,) where
  associate = Graph $ \(PairP (PairP a b) c) -> pure (PairP a (PairP b c))
  disassociate = Graph $ \(PairP a (PairP b c)) -> pure (PairP (PairP a b) c)

instance Monoidal Graph (,) where
  type Id Graph (,) = ()
  idl = Graph $ \(PairP UnitP a) -> pure a
  idr = Graph $ \(PairP a UnitP) -> pure a
  coidl = Graph $ \a -> pure (PairP UnitP a)
  coidr = Graph $ \a -> pure (PairP a UnitP)

instance Braided Graph (,) where
  braid = Graph $ \(PairP a b) -> pure (PairP b a)

instance Symmetric Graph (,)

instance Cartesian Graph where
  type Product Graph = (,)
  fst = Graph $ \(PairP a _) -> pure a
  snd = Graph $ \(PairP _ b) -> pure b
  diag = Graph $ \a -> pure (PairP a a)
  Graph f &&& Graph g = Graph $ \a -> do
    b <- f a
    c <- g a
    pure (PairP b c)

instance CCC Graph where
  type Exp Graph = (->)
  curry :: Graph (a, b) c -> Graph a (b -> c)
  curry (Graph f) = Graph $ \a ->
    pure $
      FunP $
        Graph $
          \b ->
            f (PairP a b)

  uncurry :: Graph a (b -> c) -> Graph (a, b) c
  uncurry (Graph f) = Graph $
    \(PairP a b) -> do
      x <- f a
      case x of
        FunP (Graph g) -> g b

  apply :: Graph (a -> b, a) b
  apply = Graph $ \(PairP (FunP (Graph f)) a) -> f a

--instance (Num a, GenPorts a) => ConCat.NumCat Graph a where
--  negateC = genComp "negate"
--  addC = genComp "+"
--  subC = genComp "-"
--  mulC = genComp "×"
--  powIC = genComp "^"

square :: Int -> Int
square a = a * a

program :: Graph Int Int
program = Categorify.expression square

main :: IO ()
main = print $ case runState (runGraph program (IntP 5)) (1, []) of
  (IntP a, _) -> a