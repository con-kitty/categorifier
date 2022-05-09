{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Categorifier.Fin.Client
  ( HasRep (..),
    deriveHasRep,
  )
where

import Categorifier.Client (HasRep (..), deriveHasRep)
import Data.Fin (Fin)
import Data.Type.Nat (Nat (..), SNat (..))

deriveHasRep ''Fin

deriveHasRep ''Nat

deriveHasRep ''SNat
