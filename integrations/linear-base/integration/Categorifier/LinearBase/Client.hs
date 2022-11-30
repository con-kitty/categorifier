{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Categorifier.LinearBase.Client
  ( HasRep (..),
    deriveHasRep,
  )
where

import Categorifier.Client (HasRep (..), deriveHasRep)
import Control.Functor.Linear (Data, ReaderT, StateT)
import Control.Optics.Linear (Optic_)
import Data.Arity.Linear (Peano)
import Data.Array.Destination (DArray)
import qualified Data.Array.Polarized.Pull as Pull
import Data.HashMap.Mutable.Linear (HashMap)
import Data.Monoid.Linear (Endo, NonLinear)
import Data.Profunctor.Kleisli.Linear (CoKleisli, Kleisli)
import Data.Profunctor.Linear (Exchange, Market)
import Data.Replicator.Linear (Replicator)
import Data.Set.Mutable.Linear (Set)
import Data.Unrestricted.Linear (AsMovable, MovableMonoid, Ur, UrT)
import Data.V.Linear (V)
import Data.Vector.Mutable.Linear (Vector)
import Foreign.Marshal.Pure (Box, Pool)
import Prelude.Linear.Generically (Generically, Generically1)
import Streaming.Linear (Of, Stream)
import Streaming.Prelude.Linear (Either3)
import System.IO.Resource.Linear (RIO)
import System.IO.Resource.Linear.Internal (Resource)

deriveHasRep ''AsMovable
deriveHasRep ''Box
deriveHasRep ''CoKleisli
deriveHasRep ''DArray
deriveHasRep ''Data
deriveHasRep ''Either3
deriveHasRep ''Endo
deriveHasRep ''Exchange
deriveHasRep ''Generically
deriveHasRep ''Generically1
deriveHasRep ''HashMap
deriveHasRep ''Kleisli
deriveHasRep ''Market
deriveHasRep ''MovableMonoid
deriveHasRep ''NonLinear
deriveHasRep ''Of
deriveHasRep ''Optic_
deriveHasRep ''Peano
deriveHasRep ''Pool
deriveHasRep ''Pull.Array
deriveHasRep ''RIO
deriveHasRep ''ReaderT
deriveHasRep ''Replicator
deriveHasRep ''Resource
deriveHasRep ''Set
deriveHasRep ''StateT
deriveHasRep ''Stream
deriveHasRep ''Ur
deriveHasRep ''UrT
deriveHasRep ''V
deriveHasRep ''Vector
