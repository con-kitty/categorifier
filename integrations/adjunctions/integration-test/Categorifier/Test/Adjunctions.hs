-- To avoid having to specify massive HList types.
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
-- To avoid having to specify massive HList types.
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Categorifier.Test.Adjunctions
  ( testTerms,
  )
where

import Categorifier.Test.HList (HList1 (..))
import Categorifier.Test.TH (mkBinaryTestConfig, mkExprTest, mkUnaryTestConfig)
import Categorifier.Test.Tests (TestTerms)
import Data.Functor.Identity (Identity)
import qualified Data.Functor.Rep as Representable

testTerms :: TestTerms _
testTerms =
  HCons1
    ( mkExprTest
        (mkUnaryTestConfig "PureRep")
        (\a -> (a, [t|Identity $a|]))
        [|Representable.pureRep|]
    )
    . HCons1
      ( mkExprTest
          (mkUnaryTestConfig "FmapRep")
          (\a -> ([t|Identity $a|], [t|Identity $a|]))
          [|Representable.fmapRep id|]
      )
    . HCons1
      ( mkExprTest
          (mkUnaryTestConfig "ApRep")
          (\a -> ([t|Identity $a|], [t|Identity $a|]))
          [|Representable.apRep (Identity id)|]
      )
    . HCons1
      ( mkExprTest
          (mkBinaryTestConfig "BindRep")
          (\a -> ([t|Identity $a|], [t|($a -> Identity $a) -> Identity $a|]))
          [|Representable.bindRep|]
      )
    . HCons1
      ( mkExprTest
          (mkBinaryTestConfig "Index")
          (\(f, a) -> ([t|$f $a|], [t|Representable.Rep $f -> $a|]))
          [|Representable.index|]
      )
    . HCons1
      ( mkExprTest
          (mkUnaryTestConfig "Tabulate")
          (\(f, a) -> ([t|Representable.Rep $f -> $a|], [t|$f $a|]))
          [|Representable.tabulate|]
      )
    $ HNil1
