{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Categorifier.Test.Categories.Instances (Hask (..), Term (..)) where

import Categorifier.Test.Data (Pair)
import Categorifier.Test.Hask (Hask (..))
import Categorifier.Test.Term (Term (..), binaryZero, unaryZero)
import qualified Control.Categorical.Bifunctor as Categories
import qualified Control.Categorical.Functor as Categories
import qualified Control.Category.Associative as Categories
import qualified Control.Category.Braided as Categories
import qualified Control.Category.Cartesian as Categories
import qualified Control.Category.Cartesian.Closed as Categories
import qualified Control.Category.Distributive as Categories
import qualified Control.Category.Monoidal as Categories

-- data

instance Categories.Functor Pair (->) (->) where
  fmap = fmap -- the rhs is from base.

-- Term

instance Categories.Functor f Term Term where
  fmap = unaryZero

instance Categories.PFunctor f Term Term where
  first = unaryZero

instance Categories.QFunctor f Term Term where
  second = unaryZero

instance Categories.Bifunctor t Term Term Term where
  bimap = binaryZero

instance Categories.Associative Term t where
  associate = ZeroId
  disassociate = ZeroId

instance Categories.Monoidal Term t where
  type Id Term t = ()
  idl = ZeroId
  idr = ZeroId
  coidl = ZeroId
  coidr = ZeroId

instance Categories.Braided Term t where
  braid = ZeroId

instance Categories.Symmetric Term t

instance Categories.Cartesian Term where
  type Product Term = (,)
  fst = ZeroId
  snd = ZeroId
  diag = ZeroId

instance Categories.CoCartesian Term where
  type Sum Term = Either
  inl = ZeroId
  inr = ZeroId
  codiag = ZeroId

instance Categories.CCC Term where
  type Exp Term = (->)
  apply = ZeroId
  curry = unaryZero
  uncurry = unaryZero

instance Categories.Distributive Term where
  distribute = ZeroId

-- Hask

instance Categories.Functor f (->) (->) => Categories.Functor f Hask Hask where
  fmap (Hask f) = Hask (Categories.fmap f)

instance Categories.PFunctor f (->) (->) => Categories.PFunctor f Hask Hask where
  first (Hask f) = Hask (Categories.first f)

instance Categories.QFunctor f (->) (->) => Categories.QFunctor f Hask Hask where
  second (Hask g) = Hask (Categories.second g)

instance Categories.Bifunctor t (->) (->) (->) => Categories.Bifunctor t Hask Hask Hask where
  bimap (Hask f) (Hask g) = Hask (Categories.bimap f g)

instance Categories.Associative (->) t => Categories.Associative Hask t where
  associate = Hask Categories.associate
  disassociate = Hask Categories.disassociate

instance Categories.Monoidal (->) t => Categories.Monoidal Hask t where
  type Id Hask t = Categories.Id (->) t
  idl = Hask Categories.idl
  idr = Hask Categories.idr
  coidl = Hask Categories.coidl
  coidr = Hask Categories.coidr

instance Categories.Braided (->) t => Categories.Braided Hask t where
  braid = Hask Categories.braid

instance Categories.Symmetric (->) t => Categories.Symmetric Hask t

instance Categories.Cartesian Hask where
  type Product Hask = (,)
  fst = Hask Categories.fst
  snd = Hask Categories.snd
  diag = Hask Categories.diag

instance Categories.CoCartesian Hask where
  type Sum Hask = Either
  inl = Hask Categories.inl
  inr = Hask Categories.inr
  codiag = Hask Categories.codiag

instance Categories.CCC Hask where
  type Exp Hask = (->)
  apply = Hask Categories.apply
  curry (Hask f) = Hask (Categories.curry f)
  uncurry (Hask f) = Hask (Categories.uncurry f)

instance Categories.Distributive Hask where
  distribute = Hask Categories.distribute
