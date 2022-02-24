{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TupleSections #-}

-- | Provides duoidal operations on functors. This lets us easily mix and match "parallel" and
--  "sequential" operations on structures that have multiple viable `Applicative` instances, like
--  `Either` (`Data.Either.Validation.Validation`) and `System.IO.IO`
--  (`Control.Concurrent.Async.Concurrently`).
--
--   So, for example, when using this, you should ignore the existence of
--  `Data.Either.Validation.Validation`, and always work in `Either` (and `ExceptT`), then, using
--   these operators instead of the usual `Applicative` and `Monad` operators, you will have
--   behavior that correctly mixes the accumulation of errors with the monadic "first failure"
--   semantics. This should lawfully always do what you want, without running into the
--  "`Applicative` semantics must match `Monad` semantics" problem.
--
--   Resources:
--
-- * https://ncatlab.org/nlab/show/duoidal+category
-- * https://blogs.ncl.ac.uk/andreymokhov/united-monoids/
module Categorifier.Duoidal
  ( Duoid,
    Parallel (..),
    Sequential (..),
    (<*\>),
    (<=\<),
    (=<\<),
    (<\*),
    (*\>),
    bisequenceD,
    bitraverseD,
    foldMapD,
    joinD,
    liftD2,
    pureD,
    sequenceD,
    traverseD,
  )
where

import Control.Applicative (liftA2)
import Control.Monad (join, liftM2, (<=<))
import Control.Monad.Trans.Except (ExceptT (..))
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.Monad.Trans.Writer.Strict (WriterT (..))
import Data.Bifunctor (bimap, first)
import Data.Bitraversable (Bitraversable, bisequence, bitraverse)
import Data.Functor.Compose (Compose (..))
import Data.Monoid (Ap (..))

newtype Parallel f a = Parallel {getParallel :: f a}

newtype Sequential f a = Sequential {getSequential :: f a}

instance Functor f => Functor (Parallel f) where
  fmap f = Parallel . fmap f . getParallel

instance Functor f => Functor (Sequential f) where
  fmap f = Sequential . fmap f . getSequential

-- | What this mostly does is give us `Applicative` operations that don't exactly match the `Monad`
--   ones, however they still relate via a set of laws.
type Duoid f = (Functor f, Applicative (Parallel f), Monad (Sequential f))

pureD :: Duoid f => a -> f a
pureD = getParallel . pure

liftD2 :: Duoid f => (a -> b -> c) -> f a -> f b -> f c
liftD2 f a b = getParallel (liftA2 f (Parallel a) (Parallel b))

(<*\>) :: Duoid f => f (a -> b) -> f a -> f b
f <*\> g = getParallel (Parallel f <*> Parallel g)

infixl 4 <*\>

joinD :: Duoid f => f (f a) -> f a
joinD = getSequential . join . Sequential . fmap Sequential

(=<\<) :: Duoid f => (a -> f b) -> f a -> f b
f =<\< a = getSequential (Sequential . f =<< Sequential a)

infixr 1 =<\<

(<=\<) :: Duoid f => (b -> f c) -> (a -> f b) -> a -> f c
f <=\< g = getSequential . (Sequential . f <=< Sequential . g)

infixr 1 <=\<

(*\>) :: Duoid f => f a -> f b -> f b
a *\> b = getParallel (Parallel a *> Parallel b)

(<\*) :: Duoid f => f a -> f b -> f a
a <\* b = getParallel (Parallel a <* Parallel b)

-- | `bisequence` over a `Duoid`.
bisequenceD :: (Bitraversable t, Duoid f) => t (f a) (f b) -> f (t a b)
bisequenceD = getParallel . bisequence . bimap Parallel Parallel

-- | `bitraverse` over a `Duoid`.
bitraverseD :: (Bitraversable t, Duoid f) => (a -> f c) -> (b -> f d) -> t a b -> f (t c d)
bitraverseD f g = getParallel . bitraverse (Parallel . f) (Parallel . g)

-- | `sequenceA` over a `Duoid`.
sequenceD :: (Traversable t, Duoid f) => t (f a) -> f (t a)
sequenceD = getParallel . traverse Parallel

-- | `traverse` over a `Duoid`.
traverseD :: (Traversable t, Duoid f) => (a -> f b) -> t a -> f (t b)
traverseD f = getParallel . traverse (Parallel . f)

foldMapD :: (Foldable t, Duoid f, Monoid b) => (a -> f b) -> t a -> f b
foldMapD f = getParallel . getAp . foldMap (Ap . Parallel . f)

-- INSTANCES

instance Semigroup e => Applicative (Parallel (Either e)) where
  pure = Parallel . pure

  liftA2 f (Parallel a) (Parallel b) =
    Parallel $ case (a, b) of
      (Left e, Left e') -> Left $ e <> e'
      (Left e, Right _) -> Left e
      (Right _, Left e') -> Left e'
      (Right x, Right y) -> Right $ f x y

instance Semigroup e => Applicative (Sequential (Either e)) where
  pure = Sequential . pure

  liftA2 f (Sequential a) (Sequential b) = Sequential (liftA2 f a b)

instance Semigroup e => Monad (Sequential (Either e)) where
  Sequential a >>= f = Sequential (a >>= getSequential . f)

instance (Semigroup e, Monad m) => Applicative (Parallel (ExceptT e m)) where
  pure = Parallel . pure

  liftA2 f (Parallel (ExceptT a)) (Parallel (ExceptT b)) =
    Parallel . ExceptT . fmap getParallel . getCompose $
      liftA2 f (Compose (fmap Parallel a)) (Compose (fmap Parallel b))

instance (Semigroup e, Monad m) => Applicative (Sequential (ExceptT e m)) where
  pure = Sequential . pure

  liftA2 f (Sequential a) (Sequential b) = Sequential (liftA2 f a b)

instance (Semigroup e, Monad m) => Monad (Sequential (ExceptT e m)) where
  Sequential a >>= f = Sequential (a >>= getSequential . f)

-- WriterT w m

-- | @w@ must be a commutative monoid.
instance (Monoid w, Applicative m) => Applicative (Parallel (WriterT w m)) where
  pure = Parallel . pure

  Parallel (WriterT f) <*> Parallel (WriterT v) =
    Parallel . WriterT $ liftA2 k f v
    where
      k (a, w) (b, w') = (a b, w <> w')

instance (Monoid w, Applicative m) => Applicative (Sequential (WriterT w m)) where
  pure = Sequential . pure

  Sequential (WriterT f) <*> Sequential (WriterT v) =
    Sequential . WriterT $ liftA2 k f v
    where
      k (a, w) (b, w') = (a b, w <> w')

instance (Monoid w, Monad m) => Monad (Sequential (WriterT w m)) where
  Sequential a >>= f = Sequential $ a >>= getSequential . f

-- (,)

-- | @a@ must be a commutative monoid.
instance Monoid a => Applicative (Parallel ((,) a)) where
  pure = Parallel . (mempty,)

  liftA2 f (Parallel (a0, b0)) (Parallel (a1, b1)) = Parallel (a0 <> a1, f b0 b1)

instance Monoid a => Applicative (Sequential ((,) a)) where
  pure = Sequential . (mempty,)

  liftA2 f (Sequential (a0, b0)) (Sequential (a1, b1)) = Sequential (a0 <> a1, f b0 b1)

instance Monoid a => Monad (Sequential ((,) a)) where
  Sequential (a, b) >>= f = Sequential . first (a <>) . getSequential $ f b

-- ReaderT r m

instance Duoid m => Applicative (Parallel (ReaderT r m)) where
  pure = Parallel . ReaderT . (getParallel .) . runReaderT . pure

  liftA2 f (Parallel (ReaderT r2ma)) (Parallel (ReaderT r2mb)) =
    Parallel . ReaderT $ \r -> liftD2 f (r2ma r) (r2mb r)

instance Duoid m => Applicative (Sequential (ReaderT r m)) where
  pure = Sequential . ReaderT . (getSequential .) . runReaderT . pure

  liftA2 f (Sequential (ReaderT r2ma)) (Sequential (ReaderT r2mb)) =
    Sequential . ReaderT $
      \r -> getSequential $ liftM2 f (Sequential $ r2ma r) (Sequential $ r2mb r)

instance Duoid m => Monad (Sequential (ReaderT r m)) where
  Sequential (ReaderT r2ma) >>= f =
    Sequential . ReaderT $
      \r -> flip (runReaderT . getSequential . f) r =<\< r2ma r
