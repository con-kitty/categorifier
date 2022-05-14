{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Categorifier.Vec.Client
  ( HasRep (..),
    Rep,
    deriveHasRep,
  )
where

import Categorifier.Client (HasRep (..), Rep, deriveHasRep)
import Data.Vec.Lazy (Vec (..))

deriveHasRep ''Vec
