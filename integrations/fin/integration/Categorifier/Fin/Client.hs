{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Categorifier.Fin.Client
  ( HasRep (..),
    Rep,
    deriveHasRep,
  )
where

import Categorifier.Client (HasRep (..), Rep, deriveHasRep)
import Data.Fin (Fin)
import Data.Type.Nat (Nat (..), SNat (..))

deriveHasRep ''Fin

deriveHasRep ''Nat

deriveHasRep ''SNat
