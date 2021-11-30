{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Plugin.Data
  ( One (..),
    Oneof (..),
    Pair (..),
  )
where

import qualified Control.Categorical.Functor as Categories
import Data.Distributive (Distributive (..))
import Data.Functor.Rep (Representable)
import GHC.Generics (Generic, Generic1)
import qualified Kitty.Plugin.Client as Client

-- For DeriveAnyClass
{-# ANN module "HLint: ignore Avoid restricted extensions" #-}

{-# ANN module "HLint: ignore Use newtype instead of data" #-}

-- | Same as @Identity@, except defined using @data@ rather than @newtype@. This makes it
-- easier to test some of the specialized methods, e.g., @$fRepresentableOne_$ctabulate@.
-- Using @Identity@ we'd get @$fRepresentableIdentity1@.
data One a = One a
  deriving
    ( Eq,
      Ord,
      Show,
      Functor,
      Traversable,
      Foldable,
      Generic,
      Generic1,
      Representable
    )

instance Distributive One where
  distribute = One . fmap (\(One a) -> a)

Client.deriveHasRep ''One

data Pair a = Pair a a
  deriving (Generic, Eq, Ord, Show, Functor)

instance Applicative Pair where
  pure x = Pair x x
  Pair f f' <*> Pair x x' = Pair (f x) (f' x')

instance Categories.Functor Pair (->) (->) where
  fmap = fmap -- the rhs is from base.

Client.deriveHasRep ''Pair

data Oneof a
  = Perhaps a
  | Or a
  deriving (Generic, Eq, Ord, Show, Functor)

Client.deriveHasRep ''Oneof
