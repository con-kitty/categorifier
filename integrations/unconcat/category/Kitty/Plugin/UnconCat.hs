{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE PolyKinds #-}

-- | This module contains some category classes similar to those in @ConCat.Category@,
-- but without the @Ok@ constraints. This can improve the performance significantly,
-- since we don't need to build the dictionaries for the @Ok@ constraints.
--
-- __NB__: `Category` itself comes from base, as the unconstrained version is the same.
module Kitty.Plugin.UnconCat
  ( Category (..),
    AssociativePCat (..),
    ClosedCat (..),
    CoproductCat (..),
    MonoidalPCat (..),
    MProductCat,
    ProductCat (..),
    Coprod,
    Exp,
    Prod,
    (&&&),
    apply2,
    compose2,
  )
where

import Control.Category (Category (..))
import qualified Control.Arrow as P
import qualified Prelude as P

type Coprod k = P.Either

type Exp k = (->)

type Prod k = (,)

class Category k => AssociativePCat k where
  lassocP :: forall a b c. Prod k a (Prod k b c) `k` Prod k (Prod k a b) c
  default lassocP ::
    forall a b c.
    MProductCat k =>
    Prod k a (Prod k b c) `k` Prod k (Prod k a b) c
  lassocP = second exl &&& (exr . exr)
  {-# INLINE lassocP #-}
  rassocP :: forall a b c. Prod k (Prod k a b) c `k` Prod k a (Prod k b c)
  default rassocP ::
    forall a b c.
    MProductCat k =>
    Prod k (Prod k a b) c `k` Prod k a (Prod k b c)
  rassocP = (exl . exl) &&& first exr
  {-# INLINE rassocP #-}

class ProductCat k => ClosedCat k where
  apply :: forall a b. Prod k (Exp k a b) a `k` b
  apply = uncurry id
  {-# INLINE apply #-}

  curry :: (Prod k a b `k` c) -> (a `k` Exp k b c)

  uncurry :: forall a b c. (a `k` Exp k b c) -> (Prod k a b `k` c)
  default uncurry ::
    forall a b c.
    MonoidalPCat k =>
    (a `k` Exp k b c) ->
    (Prod k a b `k` c)
  uncurry g = apply . first g
  {-# INLINE uncurry #-}
  {-# MINIMAL curry, (apply | uncurry) #-}

class Category k => CoproductCat k where
  inl :: forall a b. a `k` Coprod k a b
  inr :: forall a b. b `k` Coprod k a b
  jam :: forall a. Coprod k a a `k` a

class Category k => MonoidalPCat k where
  (***) :: forall a b c d. (a `k` c) -> (b `k` d) -> (Prod k a b `k` Prod k c d)
  first :: forall a a' b. (a `k` a') -> (Prod k a b `k` Prod k a' b)
  first = (*** id)
  {-# INLINE first #-}
  second :: forall a b b'. (b `k` b') -> (Prod k a b `k` Prod k a b')
  second = (id ***)
  {-# INLINE second #-}

type MProductCat k = (ProductCat k, MonoidalPCat k)

class Category k => ProductCat k where
  exl :: forall a b. Prod k a b `k` a
  exr :: forall a b. Prod k a b `k` b
  dup :: a `k` Prod k a a

(&&&) ::
  forall k a c d.
  MProductCat k =>
  (a `k` c) ->
  (a `k` d) ->
  (a `k` Prod k c d)
f &&& g = (f *** g) . dup
{-# INLINE (&&&) #-}

apply2 ::
  forall cat x a b.
  (ClosedCat cat, MonoidalPCat cat) =>
  cat x (a -> b) ->
  cat x a ->
  cat x b
apply2 f a = apply . (f &&& a)

compose2 ::
  forall cat x b c a.
  (ClosedCat cat, MonoidalPCat cat) =>
  cat x (b -> c) ->
  cat x (a -> b) ->
  cat x (a -> c)
compose2 f g = curry (uncurry f . (exl &&& uncurry g))

------------------------------------------------------------------------------

-- * (->) instances

instance AssociativePCat (->) where
  lassocP = \(a, (b, c)) -> ((a, b), c)
  rassocP = \((a, b), c) -> (a, (b, c))

instance ClosedCat (->) where
  apply = P.uncurry (P.$)
  curry = P.curry
  uncurry = P.uncurry

instance CoproductCat (->) where
  inl = P.Left
  inr = P.Right
  jam = P.id `P.either` P.id

instance MonoidalPCat (->) where
  (***) = (P.***)
  first = P.first
  second = P.second

instance ProductCat (->) where
  exl = P.fst
  exr = P.snd
  dup = \a -> (a, a)
