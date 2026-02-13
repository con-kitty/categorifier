{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This is intended to be used /instead/ of "ConCat.Circuit" when using the "Categorifier"
--   plugin.
--
--   It re-exports the original module, adding instances for classes required by "Categorifier"..
module Categorifier.ConCat.Examples.Circuit
  ( module ConCat.Circuit,
  )
where

import qualified Categorifier.Category as Categorifier
import qualified Categorifier.Client as Categorifier
import qualified ConCat.Category as ConCat
import ConCat.Circuit ((:>))
import qualified ConCat.Rep as ConCat

instance Categorifier.ReferenceCat (:>) a b

instance
  ( Categorifier.HasRep a,
    r ~ Categorifier.Rep a,
    ConCat.Ok (:>) a,
    ConCat.Ok (:>) r,
    -- __NB__: This constraint is only because "ConCat.Circuit" doesn't export enough for us to
    --         define this instance directly.
    r ~ ConCat.Rep a
  ) =>
  Categorifier.RepCat (:>) a r
  where
  reprC = ConCat.reprC
  abstC = ConCat.abstC

instance (ConCat.CoerceCat (:>) a b) => Categorifier.UnsafeCoerceCat (:>) a b where
  unsafeCoerceK = ConCat.coerceC
