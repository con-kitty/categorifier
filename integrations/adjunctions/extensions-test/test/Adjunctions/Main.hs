{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
-- To avoid turning @if then else@ into `ifThenElse`.
{-# LANGUAGE NoRebindableSyntax #-}

-- | See @Test/Cat/ConCat/Main.hs@ for copious notes on the testing situation here.
module Main
  ( main,
  )
where

import Data.Bool (bool)
import Data.Functor.Identity (Identity (..))
import GHC.Int (Int64)
import GHC.Word (Word8)
import qualified Hedgehog.Gen as Gen
import Kitty.Plugin.Hedgehog (genFloating)
import qualified Kitty.Plugin.Test.Adjunctions as Adjunctions
import Kitty.Plugin.Test.Data (One (..))
import Kitty.Plugin.Test.HList (HList1 (..))
import Kitty.Plugin.Test.Categories.Instances (Hask (..), Term)
import Kitty.Plugin.Test.Tests
  ( TestCases (..),
    TestCategory (..),
    TestStrategy (..),
    mkTestTerms,
  )
import System.Exit (exitFailure, exitSuccess)

-- For @NoRebindableSyntax@
{-# ANN module ("HLint: ignore Avoid restricted extensions" :: String) #-}

mkTestTerms
  Adjunctions.testTerms
  --             name   type      prefix       strategy
  [ TestCategory ''Term [t|Term|] "term" CheckCompileOnly,
    TestCategory ''Hask [t|Hask|] "hask" (ComputeFromInput [|runHask|]),
    TestCategory ''(->) [t|(->)|] "plainArrow" (ComputeFromInput [|id|])
  ]
  -- adjunctions
  . HCons1 (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HCons1 (TestCases (const []))
  . HCons1 (TestCases (const [([t|Int64|], pure ([|Gen.enumBounded|], [|show|]))]))
  . HCons1
    ( TestCases
        (const [([t|Word8|], pure ([|(,pure) . Identity <$> Gen.enumBounded|], [|show . fst|]))])
    )
  . HCons1
    ( TestCases
        ( const
            [ ( ([t|Identity|], [t|Word8|]),
                pure ([|(,) <$> Gen.enumBounded <*> pure ()|], [|show|])
              ),
              ( ([t|One|], [t|Word8|]),
                pure ([|(,) <$> (One <$> Gen.enumBounded) <*> pure ()|], [|show|])
              )
            ]
        )
    )
  . HCons1
    ( TestCases
        ( const
            [ ( ([t|Identity|], [t|Word8|]),
                pure ([|const <$> Gen.enumBounded|], [|("\\() -> " <>) . show . ($ ())|])
              ),
              ( ([t|One|], [t|Word8|]),
                pure ([|const <$> Gen.enumBounded|], [|("\\() -> " <>) . show . ($ ())|])
              )
            ]
        )
    )
  $ HNil1

main :: IO ()
main = bool exitFailure exitSuccess . and =<< allTestTerms
