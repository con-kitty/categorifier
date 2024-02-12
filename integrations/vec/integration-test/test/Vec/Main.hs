{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
-- To avoid turning @if then else@ into `ifThenElse`.
{-# LANGUAGE NoRebindableSyntax #-}
-- To allow testing of individual properties (see plugin/README.md#dealing_with_failed_tests)
{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds #-}

-- | See @Test/Cat/ConCat/Main.hs@ for copious notes on the testing situation here.
module Main
  ( main,
  )
where

import Categorifier.Hedgehog (genFloating, genIntegralBounded)
import Categorifier.Test.ConCatExtensions.Instances (Hask (..), Term)
import Categorifier.Test.HList (HMap1 (..))
import Categorifier.Test.Tests
  ( TestCases (..),
    TestCategory (..),
    TestStrategy (..),
    mkTestTerms,
  )
import qualified Categorifier.Test.Vec as Vec
import Categorifier.Test.Vec.Instances ()
import Data.Bool (bool)
import Data.Functor.Identity (Identity (..))
import Data.Proxy (Proxy (..))
import qualified Data.Type.Nat as Nat
import Data.Vec.Lazy (Vec (..))
import qualified Data.Vec.Lazy as Vec
import GHC.Word (Word8)
-- To allow testing of individual properties (see plugin/README.md#dealing_with_failed_tests)
import qualified Hedgehog
import qualified Hedgehog.Main as Hedgehog (defaultMain)

-- For @NoRebindableSyntax@
{-# ANN module ("HLint: ignore Avoid restricted integration" :: String) #-}

mkTestTerms
  Vec.testTerms
  --             name   type      prefix  strategy
  [ TestCategory ''Term [t|Term|] "term" CheckCompileOnly,
    TestCategory ''(->) [t|(->)|] "plainArrow" $ ComputeFromInput [|id|],
    TestCategory ''Hask [t|Hask|] "hask" (ComputeFromInput [|runHask|])
  ]
  . HInsert1
    (Proxy @"BindVec")
    ( TestCases
        ( const
            [ ( [t|Double|],
                pure ([|fmap (,pure) . sequenceA $ pure genFloating|], [|show . fst|])
              )
            ]
        )
    )
  . HInsert1
    (Proxy @"IndexVec")
    ( TestCases
        ( const
            [ ( [t|Word8|],
                pure
                  ( [|(,) <$> sequenceA (Vec.tabulate $ const genIntegralBounded) <*> genIntegralBounded|],
                    [|show|]
                  )
              )
            ]
        )
    )
  . HInsert1
    (Proxy @"MapVec")
    (TestCases (const [([t|Double|], pure ([|sequenceA $ pure genFloating|], [|show|]))]))
  . HInsert1
    (Proxy @"SumVec")
    (TestCases (const [([t|Double|], pure ([|sequenceA $ pure genFloating|], [|show|]))]))
  . HInsert1
    (Proxy @"TabulateVec")
    ( TestCases
        ( const
            [ ( [t|Word8|],
                pure
                  ( [|fmap (Vec.!) . sequenceA $ pure genIntegralBounded|],
                    [|show . (<$> Vec.universe)|]
                  )
              )
            ]
        )
    )
  . HInsert1
    (Proxy @"TraverseVec")
    ( TestCases
        ( const
            [ ( ([t|Identity|], [t|Word8|]),
                pure ([|sequenceA (pure genIntegralBounded)|], [|show|])
              ),
              ( ([t|Vec Nat.Nat9|], [t|Word8|]),
                pure ([|sequenceA (pure genIntegralBounded)|], [|show|])
              )
            ]
        )
    )
  $ HEmpty1

main :: IO ()
main = Hedgehog.defaultMain allTestTerms
