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
import qualified ConCat.Category as ConCat
import qualified Control.Arrow as Base
import Control.Monad ((<=<))
import Control.Monad.State (State, runState)
import Control.Monad.State.Class (get, modify, put)
import Data.Constraint
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

--instance (GenPorts a, GenPorts b) => GenPorts (a -> b) where
--  genPorts = do
--    a <- genPorts
--    b <- genPorts
--    pure $ FunP (Graph (Kleisli $ \x -> pure (runGraph a x, runGraph b x)))

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

instance ConCat.Category Graph where
  --  type Ok Graph = GenPorts
  id = Graph pure
  Graph f . Graph g = Graph (f <=< g)

instance ConCat.ProductCat Graph where
  exl = Graph $ \(PairP a _) -> pure a
  exr = Graph $ \(PairP _ b) -> pure b
  dup = Graph $ \a -> pure (PairP a a)

instance ConCat.TerminalCat Graph where
  it = Graph $ \UnitP -> pure UnitP

instance ConCat.OpCon (ConCat.Prod Graph) (ConCat.Sat GenPorts) where inOp = ConCat.Entail (Sub Dict)

--instance ConCat.OpCon (ConCat.Exp Graph) (ConCat.Sat GenPorts) where inOp = ConCat.Entail (Sub Dict)

instance ConCat.ClosedCat Graph where
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

instance (Num a, GenPorts a) => ConCat.NumCat Graph a where
  negateC = genComp "negate"
  addC = genComp "+"
  subC = genComp "-"
  mulC = genComp "×"
  powIC = genComp "^"

square :: Int -> Int
square a = a * a

program :: Graph Int Int
program = Categorify.expression square

main :: IO ()
main = print $ case runState (runGraph program (IntP 5)) (1, []) of
  (IntP a, _) -> a