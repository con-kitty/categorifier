{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Categorifier.Vec.Client
  ( HasRep (..),
    deriveHasRep,
  )
where

import Categorifier.Client (HasRep (..), deriveHasRep)
import Data.Vec.Lazy (Vec (..))

deriveHasRep ''Vec
