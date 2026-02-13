{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This is intended to be used /instead/ of "ConCat.Syntactic" when using the "Categorifier"
--   plugin.
--
--   It re-exports the original module, adding instances for classes required by "Categorifier"..
module Categorifier.ConCat.Examples.Syntactic
  ( module ConCat.Syntactic,
  )
where

import qualified Categorifier.Category as Categorifier
import qualified Categorifier.Client as Categorifier
import qualified ConCat.Category as ConCat
import ConCat.Syntactic

instance Categorifier.ReferenceCat Syn a b

instance (Categorifier.HasRep a, r ~ Categorifier.Rep a) => Categorifier.RepCat Syn a r where
  abstC = app0' "abst"
  reprC = app0' "repr"

instance Categorifier.UnsafeCoerceCat Syn a b where
  unsafeCoerceK = app0' "unsafeCoerce"

-- TODO: Move these instances upstream

instance (Functor f) => ConCat.Strong Syn f where
  strength = app0 "strength"

instance ConCat.TraversableCat Syn t f where
  sequenceAC = app0 "sequenceA"

instance ConCat.TracedCat Syn where
  trace = app1 "trace"
