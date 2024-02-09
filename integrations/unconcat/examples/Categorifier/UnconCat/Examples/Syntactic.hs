{-# OPTIONS_GHC -Wno-orphans #-}

-- | This is intended to be used /instead/ of "ConCat.Syntactic" when using the "Categorifier"
--   plugin with `Categorifier.Hierarchy.UnconCat.hierarchy`.
--
--   It re-exports the original module, adding instances for classes required by `Categorifier`.
module Categorifier.UnconCat.Examples.Syntactic
  ( module Categorifier.ConCat.Examples.Syntactic,
  )
where

import Categorifier.ConCat.Examples.Syntactic
import qualified Categorifier.UnconCat as Cat

instance Cat.Category Syn where
  id = app0 "id"
  (.) = app2 "."

instance Cat.AssociativePCat Syn where
  lassocP = app0 "lassocP"
  rassocP = app0 "rassocP"

instance Cat.ClosedCat Syn where
  apply = app0 "apply"
  curry = app1 "curry"
  uncurry = app1 "uncurry"

instance Cat.CoproductCat Syn where
  inl = app0 "inl"
  inr = app0 "inr"
  jam = app0 "jam"

instance Cat.MonoidalPCat Syn where
  (***) = app2 "***"
  first = app1 "first"
  second = app1 "second"

instance Cat.ProductCat Syn where
  exl = app0 "exl"
  exr = app0 "exr"
  dup = app0 "dup"
