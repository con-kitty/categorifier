{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
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

import Categorifier.Hedgehog (genInteger, genNatural, genNaturalFrom)
import Categorifier.Test.ConCat.Instances ()
import Categorifier.Test.ConCatExtensions.Instances ()
import qualified Categorifier.Test.GhcBignum as GhcBignum
import Categorifier.Test.HList (HMap1 (..))
import Categorifier.Test.Hask (Hask (..))
import Categorifier.Test.Term (Term (..))
import Categorifier.Test.Tests
  ( TestCases (..),
    TestCategory (..),
    TestStrategy (..),
    mkTestTerms,
  )
import Data.Bool (bool)
import Data.Proxy (Proxy (..))
-- To allow testing of individual properties (see plugin/README.md#dealing_with_failed_tests)
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import System.Exit (exitFailure, exitSuccess)

-- For @NoRebindableSyntax@
{-# ANN module ("HLint: ignore Avoid restricted integration" :: String) #-}

mkTestTerms
  GhcBignum.testTerms
  --             name   type      prefix       strategy
  [ TestCategory ''Term [t|Term|] "term" CheckCompileOnly,
    TestCategory ''Hask [t|Hask|] "hask" $ ComputeFromInput [|runHask|],
    TestCategory ''(->) [t|(->)|] "plainArrow" $ ComputeFromInput [|id|]
  ]
  -- ghc-bignum
  . HInsert1
    (Proxy @"EqualInteger")
    (TestCases (const [((), pure ([|(,) <$> genInteger <*> genInteger|], [|show|]))]))
  . HInsert1
    (Proxy @"NotEqualInteger")
    (TestCases (const [((), pure ([|(,) <$> genInteger <*> genInteger|], [|show|]))]))
  . HInsert1
    (Proxy @"GeInteger")
    (TestCases (const [((), pure ([|(,) <$> genInteger <*> genInteger|], [|show|]))]))
  . HInsert1
    (Proxy @"GtInteger")
    (TestCases (const [((), pure ([|(,) <$> genInteger <*> genInteger|], [|show|]))]))
  . HInsert1
    (Proxy @"LeInteger")
    (TestCases (const [((), pure ([|(,) <$> genInteger <*> genInteger|], [|show|]))]))
  . HInsert1
    (Proxy @"LtInteger")
    (TestCases (const [((), pure ([|(,) <$> genInteger <*> genInteger|], [|show|]))]))
  . HInsert1
    (Proxy @"CompareInteger")
    (TestCases (const [((), pure ([|(,) <$> genInteger <*> genInteger|], [|show|]))]))
  . HInsert1
    (Proxy @"PlusInteger")
    (TestCases (const [((), pure ([|(,) <$> genInteger <*> genInteger|], [|show|]))]))
  . HInsert1
    (Proxy @"MinusInteger")
    (TestCases (const [((), pure ([|(,) <$> genInteger <*> genInteger|], [|show|]))]))
  . HInsert1
    (Proxy @"TimesInteger")
    (TestCases (const [((), pure ([|(,) <$> genInteger <*> genInteger|], [|show|]))]))
  . HInsert1
    (Proxy @"NegateInteger")
    (TestCases (const [((), pure ([|genInteger|], [|show|]))]))
  . HInsert1
    (Proxy @"AbsInteger")
    (TestCases (const [((), pure ([|genInteger|], [|show|]))]))
  . HInsert1
    (Proxy @"SignumInteger")
    (TestCases (const [((), pure ([|genInteger|], [|show|]))]))
  . HInsert1
    (Proxy @"QuotInteger")
    ( TestCases
        (const [((), pure ([|(,) <$> genInteger <*> Gen.filter (/= 0) genInteger|], [|show|]))])
    )
  . HInsert1
    (Proxy @"RemInteger")
    ( TestCases
        (const [((), pure ([|(,) <$> genInteger <*> Gen.filter (/= 0) genInteger|], [|show|]))])
    )
  . HInsert1
    (Proxy @"EqualNatural")
    (TestCases (const [((), pure ([|(,) <$> genNatural <*> genNatural|], [|show|]))]))
  . HInsert1
    (Proxy @"NotEqualNatural")
    (TestCases (const [((), pure ([|(,) <$> genNatural <*> genNatural|], [|show|]))]))
  . HInsert1
    (Proxy @"GeNatural")
    (TestCases (const [((), pure ([|(,) <$> genNatural <*> genNatural|], [|show|]))]))
  . HInsert1
    (Proxy @"GtNatural")
    (TestCases (const [((), pure ([|(,) <$> genNatural <*> genNatural|], [|show|]))]))
  . HInsert1
    (Proxy @"LeNatural")
    (TestCases (const [((), pure ([|(,) <$> genNatural <*> genNatural|], [|show|]))]))
  . HInsert1
    (Proxy @"LtNatural")
    (TestCases (const [((), pure ([|(,) <$> genNatural <*> genNatural|], [|show|]))]))
  . HInsert1
    (Proxy @"CompareNatural")
    (TestCases (const [((), pure ([|(,) <$> genNatural <*> genNatural|], [|show|]))]))
  . HInsert1
    (Proxy @"PlusNatural")
    (TestCases (const [((), pure ([|(,) <$> genNatural <*> genNatural|], [|show|]))]))
  . HInsert1
    (Proxy @"MinusNaturalThrow")
    ( TestCases
        ( const
            [ ( (),
                pure
                  ( [|(\subtrahend -> (,subtrahend) <$> genNaturalFrom subtrahend) =<< genNatural|],
                    [|show|]
                  )
              )
            ]
        )
    )
  . HInsert1
    (Proxy @"MinusNaturalUnsafe")
    ( TestCases
        ( const
            [ ( (),
                pure
                  ( [|(\subtrahend -> (,subtrahend) <$> genNaturalFrom subtrahend) =<< genNatural|],
                    [|show|]
                  )
              )
            ]
        )
    )
  . HInsert1
    (Proxy @"TimesNatural")
    (TestCases (const [((), pure ([|(,) <$> genNatural <*> genNatural|], [|show|]))]))
  . HInsert1
    (Proxy @"SignumNatural")
    (TestCases (const [((), pure ([|genNatural|], [|show|]))]))
  . HInsert1
    (Proxy @"NegateNatural")
    (TestCases (const [((), pure ([|Gen.constant 0|], [|show|]))]))
  . HInsert1
    (Proxy @"QuotNatural")
    (TestCases (const [((), pure ([|(,) <$> genNatural <*> genNaturalFrom 1|], [|show|]))]))
  . HInsert1
    (Proxy @"RemNatural")
    (TestCases (const [((), pure ([|(,) <$> genNatural <*> genNaturalFrom 1|], [|show|]))]))
  $ HEmpty1

main :: IO ()
main = bool exitFailure exitSuccess . and =<< allTestTerms
