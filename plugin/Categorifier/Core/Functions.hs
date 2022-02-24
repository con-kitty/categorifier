-- | Various functions that are used internally for the plugin conversion.
--
--   The @INLINE@ pragmas on these are very delicate. We need to ensure that these functions aren't
--   accidentally inlined before or during categorification. The need to be either /explicitly/
--   inlined by the plugin or preserved until we can map them to morphisms in the target
--   category. We use @INLINE [0]@ instead of @NOINLINE@ so that the unfoldings are still created
--   for our manual inlining, but they should never persist long enough to be inlined by the
--   simplifier.
module Categorifier.Core.Functions
  ( abst,
    repr,
  )
where

import qualified Categorifier.Client as Client

-- | Lower `abst` from a method to a function, for inlining purposes.
abst :: Client.HasRep a => Client.Rep a -> a
abst = Client.abst
{-# INLINE [0] abst #-}

-- | Lower `repr` from a method to a function, for inlining purposes.
repr :: Client.HasRep a => a -> Client.Rep a
repr = Client.repr
{-# INLINE [0] repr #-}
