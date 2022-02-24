module Test.WithInstance
  ( test,
  )
where

import qualified Categorifier.Categorify as Categorify
import ConCat.Category (Category (..))
import P hiding (id, (.))

-- | The terminal object in __Cat__ -- a category with only a single object (up to unique
--   isomorphism), whose only arrow is its identity.
data Term a b = ZeroId
  deriving (Show)

instance Category Term where
  id = ZeroId

  ZeroId . ZeroId = ZeroId

test :: Term Int Int
test = Categorify.expression id
