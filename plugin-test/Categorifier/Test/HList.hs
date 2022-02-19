{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Categorifier.Test.HList
  ( HList1 (..),
    HMap1 (..),
    append,
    appendMap,
    lowerWith,
    map1,
    zipWith,
    zipLowerWith,
    zipMapLowerWith,
  )
where

import Data.Kind (Type)
import Data.Nat (Nat (..))
import Data.Proxy (Proxy (..))
import Data.Vec.Lazy (Vec (..))
import Prelude hiding (lookup, zipWith)

-- | An @HList@ where each term is wrapped in the same functor.
--
-- - @`HList1` `Data.Functor.Identity.Identity` ~ Data.HList.HList.HList@
-- - @`HList1` (`Data.Functor.Const.Const` a) l ~ `Vec` (`Length` l) a@
data HList1 (f :: k -> Type) (l :: [k]) where
  HNil1 :: HList1 f '[]
  HCons1 :: f e -> HList1 f l -> HList1 f (e ': l)

type family Append (a :: [k]) (b :: [k]) :: [k] where
  Append '[] b = b
  Append (a ': as) b = a ': Append as b

type family Length (a :: [k]) :: Nat where
  Length '[] = 'Z
  Length (_h ': t) = 'S (Length t)

append :: HList1 f l -> HList1 f m -> HList1 f (Append l m)
append HNil1 = id
append (HCons1 h t) = HCons1 h . append t

lowerWith :: (forall a. f a -> c) -> HList1 f l -> Vec (Length l) c
lowerWith fn = \case
  HNil1 -> VNil
  (HCons1 h t) -> fn h ::: lowerWith fn t

map1 :: (forall a. f a -> g a) -> HList1 f l -> HList1 g l
map1 fn = \case
  HNil1 -> HNil1
  (HCons1 h t) -> HCons1 (fn h) $ map1 fn t

zipWith :: (forall a. f a -> g a -> h a) -> HList1 f l -> HList1 g l -> HList1 h l
zipWith _ HNil1 HNil1 = HNil1
zipWith fn (HCons1 hf tf) (HCons1 hg tg) = HCons1 (fn hf hg) $ zipWith fn tf tg

-- | Should be equivalent to
--   @(`lowerWith` `Data.Functor.Const.getConst` .) . `zipWith` (`Data.Functor.Const.Const` . fn)@,
--   but more efficient.
zipLowerWith :: (forall a. f a -> g a -> c) -> HList1 f l -> HList1 g l -> Vec (Length l) c
zipLowerWith _ HNil1 HNil1 = VNil
zipLowerWith fn (HCons1 hf tf) (HCons1 hg tg) = fn hf hg ::: zipLowerWith fn tf tg

data HMap1 (f :: v -> Type) (l :: [(k, v)]) where
  HEmpty1 :: HMap1 f '[]
  HInsert1 :: Proxy k -> f e -> HMap1 f l -> HMap1 f ('(k, e) ': l)

zipMapLowerWith :: (forall a. f a -> g a -> c) -> HMap1 f l -> HMap1 g l -> Vec (Length l) c
zipMapLowerWith _ HEmpty1 HEmpty1 = VNil
zipMapLowerWith f (HInsert1 Proxy fe t) (HInsert1 Proxy ge u) = (f fe ge) ::: zipMapLowerWith f t u

appendMap :: HMap1 f l -> HMap1 f m -> HMap1 f (Append l m)
appendMap HEmpty1 = id
appendMap (HInsert1 k v t) = HInsert1 k v . appendMap t
