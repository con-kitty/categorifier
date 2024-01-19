{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
-- To avoid having to specify massive HList types.
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TupleSections #-}
-- To avoid having to specify massive HList types.
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Categorifier.Test.GhcBignum
  ( testTerms,
  )
where

import Categorifier.Test.HList (HMap1 (..))
import Categorifier.Test.TH (mkBinaryTestConfig, mkUnaryTestConfig)
import Categorifier.Test.Tests (TestTerms, insertTest)
import Data.Proxy (Proxy (..))
import GHC.Num.Integer (Integer)
import qualified GHC.Num.Integer
import GHC.Num.Natural (Natural)
import qualified GHC.Num.Natural

testTerms :: TestTerms _
testTerms =
  insertTest
    (Proxy @"EqualInteger")
    mkBinaryTestConfig
    (\() -> ([t|Integer|], [t|Integer -> Bool|]))
    [|GHC.Num.Integer.integerEq|]
    . insertTest
      (Proxy @"NotEqualInteger")
      mkBinaryTestConfig
      (\() -> ([t|Integer|], [t|Integer -> Bool|]))
      [|GHC.Num.Integer.integerNe|]
    . insertTest
      (Proxy @"GeInteger")
      mkBinaryTestConfig
      (\() -> ([t|Integer|], [t|Integer -> Bool|]))
      [|GHC.Num.Integer.integerGe|]
    . insertTest
      (Proxy @"GtInteger")
      mkBinaryTestConfig
      (\() -> ([t|Integer|], [t|Integer -> Bool|]))
      [|GHC.Num.Integer.integerGt|]
    . insertTest
      (Proxy @"LeInteger")
      mkBinaryTestConfig
      (\() -> ([t|Integer|], [t|Integer -> Bool|]))
      [|GHC.Num.Integer.integerLe|]
    . insertTest
      (Proxy @"LtInteger")
      mkBinaryTestConfig
      (\() -> ([t|Integer|], [t|Integer -> Bool|]))
      [|GHC.Num.Integer.integerLt|]
    . insertTest
      (Proxy @"CompareInteger")
      mkBinaryTestConfig
      (\() -> ([t|Integer|], [t|Integer -> Ordering|]))
      [|GHC.Num.Integer.integerCompare|]
    . insertTest
      (Proxy @"PlusInteger")
      mkBinaryTestConfig
      (\() -> ([t|Integer|], [t|Integer -> Integer|]))
      [|GHC.Num.Integer.integerAdd|]
    . insertTest
      (Proxy @"MinusInteger")
      mkBinaryTestConfig
      (\() -> ([t|Integer|], [t|Integer -> Integer|]))
      [|GHC.Num.Integer.integerSub|]
    . insertTest
      (Proxy @"TimesInteger")
      mkBinaryTestConfig
      (\() -> ([t|Integer|], [t|Integer -> Integer|]))
      [|GHC.Num.Integer.integerMul|]
    . insertTest
      (Proxy @"NegateInteger")
      mkUnaryTestConfig
      (\() -> ([t|Integer|], [t|Integer|]))
      [|GHC.Num.Integer.integerNegate|]
    . insertTest
      (Proxy @"AbsInteger")
      mkUnaryTestConfig
      (\() -> ([t|Integer|], [t|Integer|]))
      [|GHC.Num.Integer.integerNegate|]
    . insertTest
      (Proxy @"SignumInteger")
      mkUnaryTestConfig
      (\() -> ([t|Integer|], [t|Integer|]))
      [|GHC.Num.Integer.integerSignum|]
    . insertTest
      (Proxy @"QuotInteger")
      mkBinaryTestConfig
      (\() -> ([t|Integer|], [t|Integer -> Integer|]))
      [|GHC.Num.Integer.integerQuot|]
    . insertTest
      (Proxy @"RemInteger")
      mkBinaryTestConfig
      (\() -> ([t|Integer|], [t|Integer -> Integer|]))
      [|GHC.Num.Integer.integerRem|]
    . insertTest
      (Proxy @"EqualNatural")
      mkBinaryTestConfig
      (\() -> ([t|Natural|], [t|Natural -> Bool|]))
      [|GHC.Num.Natural.naturalEq|]
    . insertTest
      (Proxy @"NotEqualNatural")
      mkBinaryTestConfig
      (\() -> ([t|Natural|], [t|Natural -> Bool|]))
      [|GHC.Num.Natural.naturalNe|]
    . insertTest
      (Proxy @"GeNatural")
      mkBinaryTestConfig
      (\() -> ([t|Natural|], [t|Natural -> Bool|]))
      [|GHC.Num.Natural.naturalGe|]
    . insertTest
      (Proxy @"GtNatural")
      mkBinaryTestConfig
      (\() -> ([t|Natural|], [t|Natural -> Bool|]))
      [|GHC.Num.Natural.naturalGt|]
    . insertTest
      (Proxy @"LeNatural")
      mkBinaryTestConfig
      (\() -> ([t|Natural|], [t|Natural -> Bool|]))
      [|GHC.Num.Natural.naturalLe|]
    . insertTest
      (Proxy @"LtNatural")
      mkBinaryTestConfig
      (\() -> ([t|Natural|], [t|Natural -> Bool|]))
      [|GHC.Num.Natural.naturalLt|]
    . insertTest
      (Proxy @"CompareNatural")
      mkBinaryTestConfig
      (\() -> ([t|Natural|], [t|Natural -> Ordering|]))
      [|GHC.Num.Natural.naturalCompare|]
    . insertTest
      (Proxy @"PlusNatural")
      mkBinaryTestConfig
      (\() -> ([t|Natural|], [t|Natural -> Natural|]))
      [|GHC.Num.Natural.naturalAdd|]
    . insertTest
      (Proxy @"MinusNaturalThrow")
      mkBinaryTestConfig
      (\() -> ([t|Natural|], [t|Natural -> Natural|]))
      [|GHC.Num.Natural.naturalSubThrow|]
    . insertTest
      (Proxy @"MinusNaturalUnsafe")
      mkBinaryTestConfig
      (\() -> ([t|Natural|], [t|Natural -> Natural|]))
      [|GHC.Num.Natural.naturalSubUnsafe|]
    . insertTest
      (Proxy @"TimesNatural")
      mkBinaryTestConfig
      (\() -> ([t|Natural|], [t|Natural -> Natural|]))
      [|GHC.Num.Natural.naturalMul|]
    . insertTest
      (Proxy @"SignumNatural")
      mkUnaryTestConfig
      (\() -> ([t|Natural|], [t|Natural|]))
      [|GHC.Num.Natural.naturalSignum|]
    . insertTest
      (Proxy @"NegateNatural")
      mkUnaryTestConfig
      (\() -> ([t|Natural|], [t|Natural|]))
      [|GHC.Num.Natural.naturalNegate|]
    . insertTest
      (Proxy @"QuotNatural")
      mkBinaryTestConfig
      (\() -> ([t|Natural|], [t|Natural -> Natural|]))
      [|GHC.Num.Natural.naturalQuot|]
    . insertTest
      (Proxy @"RemNatural")
      mkBinaryTestConfig
      (\() -> ([t|Natural|], [t|Natural -> Natural|]))
      [|GHC.Num.Natural.naturalRem|]
    $ HEmpty1
