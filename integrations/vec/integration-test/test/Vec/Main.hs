{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
-- To avoid turning @if then else@ into `ifThenElse`.
{-# LANGUAGE NoRebindableSyntax #-}

-- | See @Test/Cat/ConCat/Main.hs@ for copious notes on the testing situation here.
module Main
  ( main,
  )
where

import Categorifier.Hedgehog (genFloating)
import Categorifier.Test.ConCatExtensions.Instances (Hask (..), Term)
import Categorifier.Test.HList (HMap1 (..))
import Categorifier.Test.Tests
  ( TestCases (..),
    TestCategory (..),
    TestStrategy (..),
    builtinTestCategories,
    mkTestTerms,
  )
import qualified Categorifier.Test.Vec as Vec
import Categorifier.Test.Vec.Instances ()
import Data.Bool (bool)
import Data.Functor.Identity (Identity (..))
import Data.Proxy (Proxy (..))
import qualified Data.Type.Nat as Nat
import Data.Vec.Lazy (Vec (..))
import GHC.Word (Word8)
import qualified Hedgehog.Gen as Gen
import System.Exit (exitFailure, exitSuccess)

-- For @NoRebindableSyntax@
{-# ANN module ("HLint: ignore Avoid restricted integration" :: String) #-}

mkTestTerms
  Vec.testTerms
  --               name   type      prefix       strategy
  ( [ TestCategory ''Term [t|Term|] "term" CheckCompileOnly,
      TestCategory ''Hask [t|Hask|] "hask" (ComputeFromInput [|runHask|])
    ]
      <> builtinTestCategories
  )
  -- adjunctions
  . HInsert1
    (Proxy @"BindVec")
    ( TestCases
        ( const
            [ ( [t|Double|],
                pure ([|fmap (\x -> (x, pure)) . sequenceA $ pure genFloating|], [|show . fst|])
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
    (Proxy @"TraverseVec")
    ( TestCases
        ( const
            [ ( ([t|Identity|], [t|Word8|]),
                pure ([|sequenceA (pure Gen.enumBounded)|], [|show|])
              ),
              ( ([t|Vec Nat.Nat9|], [t|Word8|]),
                pure ([|sequenceA (pure Gen.enumBounded)|], [|show|])
              )
            ]
        )
    )
  $ HEmpty1

main :: IO ()
main = bool exitFailure exitSuccess . and =<< allTestTerms
