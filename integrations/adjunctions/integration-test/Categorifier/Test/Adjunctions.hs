{-# LANGUAGE DataKinds #-}
-- To avoid having to specify massive HList types.
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
-- To avoid having to specify massive HList types.
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Categorifier.Test.Adjunctions
  ( testTerms,
  )
where

import Categorifier.Test.HList (HMap1 (..))
import Categorifier.Test.TH (mkBinaryTestConfig, mkExprTest, mkUnaryTestConfig)
import Categorifier.Test.Tests (TestTerms, insertTest)
import Data.Functor.Identity (Identity (..))
import qualified Data.Functor.Rep as Representable
import Data.Proxy (Proxy (..))

testTerms :: TestTerms _
testTerms =
  insertTest
    (Proxy @"PureRep")
    mkUnaryTestConfig
    (\a -> (a, [t|Identity $a|]))
    [|Representable.pureRep|]
    . insertTest
      (Proxy @"FmapRep")
      mkUnaryTestConfig
      (\a -> ([t|Identity $a|], [t|Identity $a|]))
      [|Representable.fmapRep id|]
    . insertTest
      (Proxy @"ApRep")
      mkUnaryTestConfig
      (\a -> ([t|Identity $a|], [t|Identity $a|]))
      [|Representable.apRep (Identity id)|]
    . insertTest
      (Proxy @"BindRep")
      mkBinaryTestConfig
      (\a -> ([t|Identity $a|], [t|($a -> Identity $a) -> Identity $a|]))
      [|Representable.bindRep|]
    . insertTest
      (Proxy @"Index")
      mkBinaryTestConfig
      (\(f, a) -> ([t|$f $a|], [t|Representable.Rep $f -> $a|]))
      [|Representable.index|]
    . insertTest
      (Proxy @"Tabulate")
      mkUnaryTestConfig
      (\(f, a) -> ([t|Representable.Rep $f -> $a|], [t|$f $a|]))
      [|Representable.tabulate|]
    $ HEmpty1
