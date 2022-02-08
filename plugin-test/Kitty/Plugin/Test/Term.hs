
{-# LANGUAGE TypeFamilies #-}

module Kitty.Plugin.Test.Term
  ( Term (..),
    binaryZero,
    unaryZero,
  )
where

import qualified Control.Arrow as Base
import qualified Control.Category as Base
import Kitty.Plugin.Category (RepCat (..), UnsafeCoerceCat (..))
import qualified Kitty.Plugin.Client as Client

-- | The terminal object in __Cat__ -- a category with only a single object (up to unique
--   isomorphism), whose only arrow is its identity.
data Term a b = ZeroId
  deriving (Show)

unaryZero :: Term a b -> Term c d
unaryZero ZeroId = ZeroId

binaryZero :: Term a b -> Term c d -> Term e f
binaryZero ZeroId ZeroId = ZeroId

instance Base.Category Term where
  id = ZeroId

  (.) = binaryZero

instance Base.Arrow Term where
  arr _ = ZeroId

  first = unaryZero

instance Base.ArrowChoice Term where
  (+++) = binaryZero

instance Base.ArrowApply Term where
  app = ZeroId

instance Base.ArrowLoop Term where
  loop = unaryZero

instance (Client.HasRep a, r ~ Client.Rep a) => RepCat Term a r where
  abstC = ZeroId
  reprC = ZeroId

instance UnsafeCoerceCat Term a b where
  unsafeCoerceK = ZeroId
