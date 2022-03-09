{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This is intended to be used /instead/ of "ConCat.Syntactic" when using the "Categorifier"
--   plugin.
module Categorifier.ConCat.Examples.Syntactic
  ( module ConCat.Syntactic,
  )
where

import qualified Categorifier.Category as Categorifier
import qualified Categorifier.Client as Categorifier
import ConCat.Syntactic

instance
  (Categorifier.HasRep a, r ~ Categorifier.Rep a, T a, T r) =>
  Categorifier.RepCat Syn a r
  where
  abstC = app0' "abst"
  reprC = app0' "repr"

instance Categorifier.UnsafeCoerceCat Syn a b where
  unsafeCoerceK = app0' "unsafeCoerce"
