{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
-- To avoid turning @if then else@ into `ifThenElse`.
{-# LANGUAGE NoRebindableSyntax #-}

-- | See @Test/Cat/ConCat/Main.hs@ for copious notes on the testing situation here.
module Main
  ( main,
  )
where

import Categorifier.Hedgehog (genFloating)
import qualified Categorifier.Test.Adjunctions as Adjunctions
import Categorifier.Test.Categories.Instances (Hask (..), Term)
import Categorifier.Test.Data (One (..))
import Categorifier.Test.HList (HMap1 (..))
import Categorifier.Test.Tests
  ( TestCases (..),
    TestCategory (..),
    TestStrategy (..),
    mkTestTerms,
  )
import Data.Bool (bool)
import Data.Functor.Identity (Identity (..))
import Data.Proxy (Proxy (..))
import GHC.Int (Int64)
import GHC.Word (Word8)
import qualified Hedgehog.Gen as Gen
import System.Exit (exitFailure, exitSuccess)

-- For @NoRebindableSyntax@
{-# ANN module ("HLint: ignore Avoid restricted integration" :: String) #-}

mkTestTerms
  Adjunctions.testTerms
  --             name   type      prefix  strategy
  [ TestCategory ''Term [t|Term|] "term" CheckCompileOnly,
    TestCategory ''(->) [t|(->)|] "plainArrow" $ ComputeFromInput [|id|],
    TestCategory ''Hask [t|Hask|] "hask" (ComputeFromInput [|runHask|])
  ]
  -- adjunctions
  . HInsert1 (Proxy @"PureRep") (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"FmapRep") (TestCases (const []))
  . HInsert1 (Proxy @"ApRep") (TestCases (const [([t|Int64|], pure ([|Gen.enumBounded|], [|show|]))]))
  . HInsert1
    (Proxy @"BindRep")
    ( TestCases
        ( const
            [ ( [t|Word8|],
                pure ([|(\x -> (x, pure)) . Identity <$> Gen.enumBounded|], [|show . fst|])
              )
            ]
        )
    )
  . HInsert1
    (Proxy @"Index")
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
  . HInsert1
    (Proxy @"Tabulate")
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
  $ HEmpty1

main :: IO ()
main = bool exitFailure exitSuccess . and =<< allTestTerms
