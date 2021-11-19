{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | The only module that should be needed by code that is in the dependency graph of calls to
--  `Kitty.Plugin.Categorize.expression`. If the module actually /calls/
--  `Kitty.Plugin.Categorize.expression`, then it will also need to
--   @import qualified "Kitty.Plugin.Categorize" as Categorize@.
module Kitty.Plugin.Client
  ( HasRep (..),
    deriveHasRep,
  )
where

import Data.Functor.Compose (Compose (..))
import qualified Data.Functor.Product as Functor
import qualified Data.Functor.Sum as Functor
import Data.List.NonEmpty (NonEmpty)
import Data.Ratio (Ratio)
import qualified Data.Semigroup as Semigroup
import Kitty.Plugin.Client.Internal (HasRep (..), deriveHasRep)

deriveHasRep ''[]
deriveHasRep ''Compose
deriveHasRep ''NonEmpty
deriveHasRep ''Ordering
deriveHasRep ''Functor.Product
deriveHasRep ''Functor.Sum
deriveHasRep ''Ratio
deriveHasRep ''Semigroup.Max
deriveHasRep ''Semigroup.Min
