{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- | Plugin-specific type classes for your category. These are less generally categorical and more
--   tied to specific functionality in `GhcPlugins.CoreExpr` and/or the plugin.
module Kitty.Plugin.Category
  ( ReferenceCat (..),
    ForeignFunCallCat (..),
    NativeCat (..),
    RepCat (..),
    UnsafeCoerceCat (..),
  )
where

import GHC.TypeLits (Symbol)
import Kitty.Plugin.Client (HasRep (..))
import Unsafe.Coerce (unsafeCoerce)

-- | This lifts `HasRep` into the target category.
--
--  __NB__: This is slightly hierarchy-specific. E.g., the base hierarchy doesn't require an
--          instance of this, and while concat has its own version of this, we need to use ours
--          because it has to tie into our `HasRep`.
class (HasRep a, r ~ Rep a) => RepCat k a r where
  reprC :: a `k` r
  abstC :: r `k` a

instance (HasRep a, r ~ Rep a) => RepCat (->) a r where
  reprC = repr
  abstC = abst

-- | An interface for having something like function calls in your category. The default
--   implementation is basically a NOP, inlining any function calls, but if your category has some
--   way of handling abstraction, then you can provide a real implementation.
class ReferenceCat k a b where
  -- | The first parameter is a qualified Haskell identifier (although not necessarily one that
  --   exists in the source), and the input arrow is guaranteed to be the same as for any other call
  --   with the same first parameter. Be careful when mangling the identifier, to not introduce
  --   collisions.
  indirection :: String -> a `k` b -> a `k` b
  indirection = const id

-- | This allows a category to define a way to call functions that are defined externally
-- (as opposed to ReferenceCat which allows for internal function calls).
class ForeignFunCallCat k i a b where
  ffcall :: i -> Maybe (a -> b) -> a `k` b

-- | This class provides a backdoor for a user to provide a custom @findMaker@ entry
-- to interpret something that the user doesn't want to or cannot categorize.
--
-- We use @tag@ instead of @Proxy tag@ here, so that @findMaker@ doesn't have to
-- obtain the @Proxy@ tycon.
class NativeCat k (tag :: Symbol) a b where
  nativeK :: a `k` b

-- | This class is intended to lift `unsafeCoerce` into the target category @k@. This is needed for
--   converting various coercions.
--
--  __NB__: See https://github.com/conal/concat/issues/34 for more context on this.
class UnsafeCoerceCat k a b where
  unsafeCoerceK :: a `k` b

instance UnsafeCoerceCat (->) a b where
  unsafeCoerceK = unsafeCoerce
