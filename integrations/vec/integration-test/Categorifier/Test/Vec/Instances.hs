{-# OPTIONS_GHC -Wno-orphans #-}

module Categorifier.Test.Vec.Instances
  ( module Categorifier.Test.Hask,
    module Categorifier.Test.Term,
  )
where

import Categorifier.Test.Hask
import Categorifier.Test.Term
import Data.Pointed (Pointed (..))
import qualified Data.Type.Nat as Nat
import Data.Vec.Lazy (Vec)

instance (Nat.SNatI n) => Pointed (Vec n) where
  point = pure
