{-# LANGUAGE DataKinds #-}
-- To avoid having to specify massive HList types.
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
-- To avoid having to specify massive HList types.
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Categorifier.Test.Vec
  ( testTerms,
  )
where

import Categorifier.Test.HList (HMap1 (..))
import Categorifier.Test.TH (mkBinaryTestConfig, mkExprTest, mkUnaryTestConfig)
import Categorifier.Test.Tests (TestTerms, insertTest)
import Data.Proxy (Proxy (..))
import qualified Data.Type.Nat as Nat
import Data.Vec.Lazy (Vec (..))
import qualified Data.Vec.Lazy as Vec

testTerms :: TestTerms _
testTerms =
  insertTest
    (Proxy @"BindVec")
    mkBinaryTestConfig
    (\a -> ([t|Vec Nat.Nat9 $a|], [t|($a -> Vec Nat.Nat9 $a) -> Vec Nat.Nat9 $a|]))
    [|Vec.bind|]
    . insertTest
      (Proxy @"MapVec")
      mkUnaryTestConfig
      (\a -> ([t|Vec Nat.Nat9 $a|], [t|Vec Nat.Nat9 $a|]))
      [|Vec.map id|]
    . insertTest (Proxy @"SumVec") mkUnaryTestConfig (\a -> ([t|Vec Nat.Nat9 $a|], a)) [|Vec.sum|]
    . insertTest
      (Proxy @"TraverseVec")
      mkUnaryTestConfig
      (\(f, a) -> ([t|Vec Nat.Nat9 $a|], [t|$f (Vec Nat.Nat9 $a)|]))
      [|Vec.traverse pure|]
    $ HEmpty1
