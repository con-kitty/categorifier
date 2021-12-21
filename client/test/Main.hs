{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- | Tests various properties of `Client.HasRep` instances. This tries to cover all of
--  `Client.deriveHasRep`, so users of @kitty-cat-client@ shouldn't need to test their own derived
--   instances, but manual `Client.HasRep` instances /should/ be tested similar to the instances
--   here.
module Main (main) where

import Data.Constraint (Dict (..))
import Data.Functor.Compose (Compose (..))
import Data.Type.Nat (Nat (..))
import qualified Data.Type.Nat as Nat
import Data.Proxy (Proxy (..))
import qualified Kitty.Plugin.Client as Client
import Kitty.Plugin.Hedgehog (genFloating)
import qualified Hedgehog
import qualified Hedgehog.Gen
import qualified Hedgehog.Main as Hedgehog
import qualified Hedgehog.Range

-- | Ensures that for a type the `Client.HasRep` instance forms an ismorphism between it and its
--  `Client.Rep`.
iso ::
  forall a.
  (Client.HasRep a, Eq a, Show a, Eq (Client.Rep a), Show (Client.Rep a)) =>
  Hedgehog.Gen a ->
  Hedgehog.Gen (Client.Rep a) ->
  Hedgehog.Property
iso genA genRep = Hedgehog.property $ do
  a <- Hedgehog.forAll genA
  rep <- Hedgehog.forAll genRep
  Client.abstReturn (Proxy @a) (Hedgehog.===) rep
  Client.reprReturn (Hedgehog.===) a

-- | Exercises `Client.deriveHasRep` handling of products, sums, type parameters, and constraints.
data Foo a
  = Num a => Bar a Int String
  | Baz Double

deriving instance Eq a => Eq (Foo a)
deriving instance Show a => Show (Foo a)

Client.deriveHasRep ''Foo

genFoo :: Num a => Hedgehog.Gen a -> Hedgehog.Gen (Foo a)
genFoo a =
  Hedgehog.Gen.choice
    [ Bar
        <$> a
        <*> Hedgehog.Gen.enumBounded
        <*> Hedgehog.Gen.string (Hedgehog.Range.constant 0 100) Hedgehog.Gen.unicodeAll,
      Baz <$> genFloating
    ]

genRepFoo :: Num a => Hedgehog.Gen a -> Hedgehog.Gen (Client.Rep (Foo a))
genRepFoo a =
  Hedgehog.Gen.either
    ( (,)
        <$> ((Dict,) <$> a)
        <*> ( (,)
                <$> Hedgehog.Gen.enumBounded
                <*> Hedgehog.Gen.string (Hedgehog.Range.constant 0 100) Hedgehog.Gen.unicodeAll
            )
    )
    (genFloating)

prop_fooIso :: Hedgehog.Property
prop_fooIso =
  let a = genFloating @Double
   in iso (genFoo a) (genRepFoo a)

-- | Exercises `Client.deriveHasRep` handling of GADTs, including grouping of result types.
data AltVec n a where
  AVNil :: AltVec 'Z a
  ACons :: a -> AltVec n a -> AltVec ('S n) a
  BCons :: a -> AltVec n a -> AltVec ('S n) a

deriving instance Eq a => Eq (AltVec n a)
deriving instance Show a => Show (AltVec n a)

-- | This should create two instances, one for `'Z` and one for @`'S` n@, which need to be tested
--   separately.
Client.deriveHasRep ''AltVec

-- genAltVecZ :: Hedgehog.Gen (AltVec 'Z a)
-- genAltVecZ = pure AVNil

genRepAltVecZ :: Hedgehog.Gen (Client.Rep (AltVec 'Z a))
genRepAltVecZ = pure ()

prop_altVecZIso :: Hedgehog.Property
prop_altVecZIso =
  let a = Hedgehog.Gen.discard
   in iso (genAltVec @(Nat.FromGHC 0) @Double a) (genRepAltVecZ)

newtype Flipped f a (b :: Nat) = Flip { unflip :: f b a }

-- | Induction on 'Nat', functor form. Useful for computation.
--
induction1M
    :: (Monad t, Nat.SNatI n)
    => t (f 'Z a)
    -> (forall m. Nat.SNatI m => f m a -> t (f ('S m) a))
    -> t (f n a)
induction1M z f =
  fmap unflip . getCompose $
    Nat.induction (Compose $ fmap Flip z) (\(Compose x) -> Compose $ fmap Flip (f . unflip =<< x))
{-# INLINE induction1M #-}

genAltVec :: Nat.SNatI n => Hedgehog.Gen a -> Hedgehog.Gen (AltVec n a)
genAltVec a =
  induction1M
    (pure AVNil)
    (\prev -> Hedgehog.Gen.choice [ACons <$> a <*> pure prev, BCons <$> a <*> pure prev])

genRepAltVecS :: Nat.SNatI n => Hedgehog.Gen a -> Hedgehog.Gen (Client.Rep (AltVec ('S n) a))
genRepAltVecS a =
  Hedgehog.Gen.choice [Left <$> ((,) <$> a <*> genAltVec a), Right <$> ((,) <$> a <*> genAltVec a)]

prop_altVecSIso :: Hedgehog.Property
prop_altVecSIso =
  let a = genFloating @Double
   in iso (genAltVec @(Nat.FromGHC 10) a) (genRepAltVecS a)

-- | It's currently not supported to derive a working `Client.HasRep` for this type, so we test this
--   with manual instances.
data SomeExpr a where
  DubLit :: Double -> SomeExpr Double
  IntLit :: Int -> SomeExpr Int
  Add :: SomeExpr a -> SomeExpr a -> SomeExpr a

deriving instance Eq a => Eq (SomeExpr a)
deriving instance Show a => Show (SomeExpr a)

instance Client.HasRep (SomeExpr Double) where
  type Rep (SomeExpr Double) = Either Double (SomeExpr Double, SomeExpr Double)
  abst = \case
    Left d -> DubLit d
    Right (x, y) -> Add x y
  {-# INLINE abst #-}
  repr = \case
    DubLit d -> Left d
    Add x y -> Right (x, y)
  {-# INLINE repr #-}

genSomeExprDouble :: Hedgehog.Gen (SomeExpr Double)
genSomeExprDouble =
  Hedgehog.Gen.recursive
    Hedgehog.Gen.choice
    [DubLit <$> genFloating]
    [Add <$> genSomeExprDouble <*> genSomeExprDouble]

genRepSomeExprDouble :: Hedgehog.Gen (Client.Rep (SomeExpr Double))
genRepSomeExprDouble =
  Hedgehog.Gen.choice
    [Left <$> genFloating, Right <$> ((,) <$> genSomeExprDouble <*> genSomeExprDouble)]

prop_someExprDoubleIso :: Hedgehog.Property
prop_someExprDoubleIso = iso genSomeExprDouble genRepSomeExprDouble

instance Client.HasRep (SomeExpr Int) where
  type Rep (SomeExpr Int) = Either Int (SomeExpr Int, SomeExpr Int)
  abst = \case
    Left i -> IntLit i
    Right (x, y) -> Add x y
  {-# INLINE abst #-}
  repr = \case
    IntLit i -> Left i
    Add x y -> Right (x, y)
  {-# INLINE repr #-}

genSomeExprInt :: Hedgehog.Gen (SomeExpr Int)
genSomeExprInt =
  Hedgehog.Gen.recursive
    Hedgehog.Gen.choice
    [IntLit <$> Hedgehog.Gen.enumBounded]
    [Add <$> genSomeExprInt <*> genSomeExprInt]

genRepSomeExprInt :: Hedgehog.Gen (Client.Rep (SomeExpr Int))
genRepSomeExprInt =
  Hedgehog.Gen.choice
    [Left <$> Hedgehog.Gen.enumBounded, Right <$> ((,) <$> genSomeExprInt <*> genSomeExprInt)]

prop_someExprIntIso :: Hedgehog.Property
prop_someExprIntIso = iso genSomeExprInt genRepSomeExprInt

main :: IO ()
main = Hedgehog.defaultMain [Hedgehog.checkParallel $$(Hedgehog.discover)]
