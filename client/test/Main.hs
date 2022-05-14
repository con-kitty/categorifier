{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- | Tests various properties of `Client.HasRep` instances. This tries to cover all of
--  `Client.deriveHasRep`, so users of @categorifier-client@ shouldn't need to test their own derived
--   instances, but manual `Client.HasRep` instances /should/ be tested similar to the instances
--   here.
module Main (main) where

import qualified Categorifier.Client as Client
import Categorifier.Hedgehog (genFloating)
import Data.Constraint (Dict (..))
import Data.Functor.Compose (Compose (..))
import Data.Proxy (Proxy (..))
import Data.Type.Nat (Nat (..))
import qualified Data.Type.Nat as Nat
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
    genFloating

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
   in iso (genAltVec @(Nat.FromGHC 0) @Double a) genRepAltVecZ

newtype Flipped f a (b :: Nat) = Flip {unflip :: f b a}

-- | Induction on 'Nat', functor form. Useful for computation.
induction1M ::
  (Monad t, Nat.SNatI n) =>
  t (f 'Z a) ->
  (forall m. Nat.SNatI m => f m a -> t (f ('S m) a)) ->
  t (f n a)
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
--
--   In this case, @a@ can only be `Double` or `Int`, because those are the only base cases. So we
--   can fully define `Client.HasRep` with only two instances (and two `Client.Rep` instances).
data SomeExpr a where
  DubLit :: Double -> SomeExpr Double
  IntLit :: Int -> SomeExpr Int
  Add :: SomeExpr a -> SomeExpr a -> SomeExpr a

deriving instance Eq a => Eq (SomeExpr a)

deriving instance Show a => Show (SomeExpr a)

type instance Client.Rep (SomeExpr Double) = Either Double (SomeExpr Double, SomeExpr Double)

type instance Client.Rep (SomeExpr Int) = Either Int (SomeExpr Int, SomeExpr Int)

instance Client.HasRep (SomeExpr Double) where
  abst = \case
    Left d -> DubLit d
    Right (x, y) -> Add x y
  {-# INLINE abst #-}
  repr = \case
    DubLit d -> Left d
    Add x y -> Right (x, y)
  {-# INLINE repr #-}

instance Client.HasRep (SomeExpr Int) where
  abst = \case
    Left i -> IntLit i
    Right (x, y) -> Add x y
  {-# INLINE abst #-}
  repr = \case
    IntLit i -> Left i
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

-- | A slightly more complicated type than `SomeExpr`, allowing @a@ to be something other than one
--   of the concrete types used in the other constructor result types (`Double` and `Int`).
--
--   In this case, we need three `Client.HasRep` instances (`Double`, `Int`, and @a@). The instance
--   for @a@ has two extra requirements: 1. it needs to be @overlappable@ and 2. it needs an
--   equality constraint matching the general @`Client.Rep` a@ pattern. Then we need a `Client.Rep`
--   instance for each concrete type we want @a@ to be. Instances matching the concrete
--  `Client.HasRep` types (`Double` and `Int`) have to cover all the cases that can be specialized
--   that way, then the remaining cases (`Bool` and `Char`) need to match the pattern in the
--   equality constraint on the @`Client.HasRep` a@ instance. Also, if you don't know all the types
--   that are needed, downstream users can add their own
--   @type instance `Client.Rep` (`WeirdExpr` _)@ as necessary.
data WeirdExpr a where
  WeirdDub :: Double -> WeirdExpr Double
  WeirdInt :: Int -> WeirdExpr Int
  Empty :: WeirdExpr a
  Combine :: WeirdExpr a -> WeirdExpr a -> WeirdExpr a

deriving instance Eq a => Eq (WeirdExpr a)

deriving instance Show a => Show (WeirdExpr a)

type instance
  Client.Rep (WeirdExpr Double) =
    Either Double (Either () (WeirdExpr Double, WeirdExpr Double))

type instance Client.Rep (WeirdExpr Int) = Either Int (Either () (WeirdExpr Int, WeirdExpr Int))

-- | A simple helper synonym that you can export to insulate users from changes to the type.
type RepPattern a = Either () (WeirdExpr a, WeirdExpr a)

type instance Client.Rep (WeirdExpr Bool) = RepPattern Bool

type instance Client.Rep (WeirdExpr Char) = RepPattern Char

instance Client.HasRep (WeirdExpr Double) where
  abst = \case
    Left d -> WeirdDub d
    Right (Left ()) -> Empty
    Right (Right (x, y)) -> Combine x y
  {-# INLINE abst #-}
  repr = \case
    WeirdDub d -> Left d
    Empty -> Right (Left ())
    Combine x y -> Right (Right (x, y))
  {-# INLINE repr #-}

instance Client.HasRep (WeirdExpr Int) where
  abst = \case
    Left i -> WeirdInt i
    Right (Left ()) -> Empty
    Right (Right (x, y)) -> Combine x y
  {-# INLINE abst #-}
  repr = \case
    WeirdInt i -> Left i
    Empty -> Right (Left ())
    Combine x y -> Right (Right (x, y))
  {-# INLINE repr #-}

instance
  {-# OVERLAPPABLE #-}
  (Client.Rep (WeirdExpr a) ~ RepPattern a) =>
  Client.HasRep (WeirdExpr a)
  where
  abst = \case
    Left () -> Empty
    Right (x, y) -> Combine x y
  {-# INLINE abst #-}
  repr = \case
    Empty -> Left ()
    Combine x y -> Right (x, y)
  {-# INLINE repr #-}

genWeirdExprDouble :: Hedgehog.Gen (WeirdExpr Double)
genWeirdExprDouble =
  Hedgehog.Gen.recursive
    Hedgehog.Gen.choice
    [WeirdDub <$> genFloating, pure Empty]
    [Combine <$> genWeirdExprDouble <*> genWeirdExprDouble]

genRepWeirdExprDouble :: Hedgehog.Gen (Client.Rep (WeirdExpr Double))
genRepWeirdExprDouble =
  Hedgehog.Gen.choice
    [ Left <$> genFloating,
      pure . Right $ Left (),
      Right . Right <$> ((,) <$> genWeirdExprDouble <*> genWeirdExprDouble)
    ]

prop_weirdExprDoubleIso :: Hedgehog.Property
prop_weirdExprDoubleIso = iso genWeirdExprDouble genRepWeirdExprDouble

genWeirdExprInt :: Hedgehog.Gen (WeirdExpr Int)
genWeirdExprInt =
  Hedgehog.Gen.recursive
    Hedgehog.Gen.choice
    [WeirdInt <$> Hedgehog.Gen.enumBounded, pure Empty]
    [Combine <$> genWeirdExprInt <*> genWeirdExprInt]

genRepWeirdExprInt :: Hedgehog.Gen (Client.Rep (WeirdExpr Int))
genRepWeirdExprInt =
  Hedgehog.Gen.choice
    [ Left <$> Hedgehog.Gen.enumBounded,
      pure . Right $ Left (),
      Right . Right <$> ((,) <$> genWeirdExprInt <*> genWeirdExprInt)
    ]

prop_weirdExprIntIso :: Hedgehog.Property
prop_weirdExprIntIso = iso genWeirdExprInt genRepWeirdExprInt

genWeirdExprBool :: Hedgehog.Gen (WeirdExpr Bool)
genWeirdExprBool =
  Hedgehog.Gen.recursive
    Hedgehog.Gen.choice
    [pure Empty]
    [Combine <$> genWeirdExprBool <*> genWeirdExprBool]

genRepWeirdExprBool :: Hedgehog.Gen (Client.Rep (WeirdExpr Bool))
genRepWeirdExprBool =
  Hedgehog.Gen.choice [pure $ Left (), Right <$> ((,) <$> genWeirdExprBool <*> genWeirdExprBool)]

prop_weirdExprBoolIso :: Hedgehog.Property
prop_weirdExprBoolIso = iso genWeirdExprBool genRepWeirdExprBool

genWeirdExprChar :: Hedgehog.Gen (WeirdExpr Char)
genWeirdExprChar =
  Hedgehog.Gen.recursive
    Hedgehog.Gen.choice
    [pure Empty]
    [Combine <$> genWeirdExprChar <*> genWeirdExprChar]

genRepWeirdExprChar :: Hedgehog.Gen (Client.Rep (WeirdExpr Char))
genRepWeirdExprChar =
  Hedgehog.Gen.choice [pure $ Left (), Right <$> ((,) <$> genWeirdExprChar <*> genWeirdExprChar)]

prop_weirdExprCharIso :: Hedgehog.Property
prop_weirdExprCharIso = iso genWeirdExprChar genRepWeirdExprChar

main :: IO ()
main = Hedgehog.defaultMain [Hedgehog.checkParallel $$(Hedgehog.discover)]
