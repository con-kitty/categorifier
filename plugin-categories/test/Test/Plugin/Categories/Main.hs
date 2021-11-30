{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
-- To avoid turning @if then else@ into `ifThenElse`.
{-# LANGUAGE NoRebindableSyntax #-}

-- |
-- Module      :  Main
-- Copyright   :  (c) Kittyhawk 2020
-- License     :  AllRightsReversed
-- Maintainer  :  software@kittyhawk.aero
-- Stability   :  provisional
-- Portability :  GHC
--
-- See @Test/Cat/ConCat/Main.hs@ for copious notes on the testing situation here.
module Main
  ( main,
  )
where

import Control.Arrow (Arrow (..), ArrowChoice (..))
import Data.Bool (bool)
import Data.Functor.Identity (Identity (..))
import Data.Semigroup (Sum (..))
import GHC.Int (Int16, Int32, Int64, Int8)
import GHC.Word (Word16, Word32, Word64, Word8)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import System.Exit (exitFailure, exitSuccess)
import Test.Plugin.Data (One (..), Pair (..))
import Test.Plugin.HList (HList1 (..))
import Test.Plugin.Hask (Hask (..))
import Test.Plugin.Term (Term)
import Test.Plugin.Tests
  ( TestCases (..),
    TestCategory (..),
    TestStrategy (..),
    defaultTestTerms,
    genFloating,
    mkTestTerms,
    zerosafeUnsignedPrimitiveCases,
  )

-- For @NoRebindableSyntax@
{-# ANN module ("HLint: ignore Avoid restricted extensions" :: String) #-}

mkTestTerms
  defaultTestTerms
  --             name   type      prefix       strategy
  [ TestCategory ''Term [t|Term|] "term" CheckCompileOnly,
    TestCategory ''Hask [t|Hask|] "hask" (ComputeFromInput [|runHask|]),
    TestCategory ''(->) [t|(->)|] "plainArrow" (ComputeFromInput [|id|])
  ]
  -- core
  . HCons1 (TestCases (const [([t|Word8|], pure ([|Gen.enumBounded|], [|show|]))]))
  . HCons1 (TestCases (const [([t|Word8|], pure ([|Gen.enumBounded|], [|show|]))]))
  . HCons1
    ( TestCases
        ( const
            [ ( ([t|Int64|], [t|Word8|]),
                pure ([|(,) <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|])
              )
            ]
        )
    )
  . HCons1 (TestCases (const [([t|Word8|], pure ([|Gen.enumBounded|], [|show|]))]))
  . HCons1 (TestCases (const [([t|Word8|], pure ([|Gen.enumBounded|], [|show|]))]))
  . HCons1
    ( TestCases
        (const [([t|Word8|], pure ([|(,) <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|]))])
    )
  . HCons1
    ( TestCases
        (const [([t|Word8|], pure ([|(,) <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|]))])
    )
  . HCons1
    ( TestCases
        ( const
            [ ( [t|Word8|],
                pure
                  ( [|(,) <$> Gen.enumBounded <*> ((,) <$> Gen.enumBounded <*> Gen.enumBounded)|],
                    [|show|]
                  )
              )
            ]
        )
    )
  . HCons1 (TestCases (const [([t|Word8|], pure ([|Gen.enumBounded|], [|show|]))]))
  . HCons1
    ( TestCases
        ( const
            [ ( [t|Word8|],
                pure ([|Gen.choice [const <$> Gen.enumBounded, pure id]|], [|const "<function>"|])
              )
            ]
        )
    )
  . HCons1
    ( TestCases
        ( const
            [ ( [t|Int64|],
                pure
                  ([|(,) <$> Gen.bool <*> ((,) <$> Gen.enumBounded <*> Gen.enumBounded)|], [|show|])
              )
            ]
        )
    )
  -- plugin
  . HCons1
    ( TestCases
        (const [([t|Word8|], pure ([|(,) <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|]))])
    )
  . HCons1
    ( TestCases
        (const [([t|Word8|], pure ([|Pair <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|]))])
    )
  -- base
  . HCons1 (TestCases (const [([t|Word8|], pure ([|Gen.enumBounded|], [|show|]))]))
  . HCons1
    ( TestCases
        ( const
            [ ( ([t|Int64|], [t|Word8|]),
                pure ([|(,) <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|])
              )
            ]
        )
    )
  . HCons1
    ( TestCases
        ( const
            [ ( ([t|Word8|], [t|Word8|]),
                pure ([|(,) <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|])
              )
            ]
        )
    )
  . HCons1
    ( TestCases
        ( const
            [ ( ([t|Word8|], [t|Word8|], [t|Word8|]),
                pure
                  ( [|
                      (,)
                        <$> Gen.enumBounded
                        <*> ((,) <$> Gen.enumBounded <*> Gen.enumBounded)
                      |],
                    [|show|]
                  )
              )
            ]
        )
    )
  . HCons1
    ( TestCases
        ( const
            [ ( ([t|Word8|], [t|Word8|], [t|Word8|]),
                pure
                  ( [|(,) <$> Gen.enumBounded <*> ((,) <$> Gen.enumBounded <*> Gen.enumBounded)|],
                    [|show|]
                  )
              )
            ]
        )
    )
  . HCons1
    ( TestCases
        ( const
            [ ( ([t|Word8|], [t|Int64|]),
                pure ([|(,) <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|])
              )
            ]
        )
    )
  . HCons1 (TestCases (const [(([t|Int64|], [t|Word8|]), pure ([|Gen.enumBounded|], [|show|]))]))
  . HCons1
    ( TestCases
        ( const
            [ ( ([t|Int64|], [t|Word8|]),
                pure
                  ([|Gen.choice [Left <$> Gen.enumBounded, Right <$> Gen.enumBounded]|], [|show|])
              )
            ]
        )
    )
  . HCons1 (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HCons1
    ( TestCases
        ( const
            [ ( ([t|Int64|], [t|Word8|]),
                pure
                  ([|Gen.choice [Left <$> Gen.enumBounded, Right <$> Gen.enumBounded]|], [|show|])
              )
            ]
        )
    )
  . HCons1 (TestCases (const [([t|Word8|], pure ([|Gen.enumBounded|], [|show|]))]))
  . HCons1 (TestCases (const [([t|Word8|], pure ([|Gen.enumBounded|], [|show|]))]))
  . HCons1
    ( TestCases
        ( const
            [ ( [t|Bool|],
                pure
                  ( [|(,,) <$> Gen.bool <*> Gen.bool <*> Gen.bool|],
                    [|show|]
                  )
              ),
              ( [t|Word8|],
                pure
                  ( [|(,,) <$> Gen.enumBounded <*> Gen.enumBounded <*> Gen.bool|],
                    [|show|]
                  )
              ),
              ( [t|Word16|],
                pure
                  ( [|(,,) <$> Gen.enumBounded <*> Gen.enumBounded <*> Gen.bool|],
                    [|show|]
                  )
              ),
              ( [t|Word32|],
                pure
                  ( [|(,,) <$> Gen.enumBounded <*> Gen.enumBounded <*> Gen.bool|],
                    [|show|]
                  )
              ),
              ( [t|Word64|],
                pure
                  ( [|
                      (,,) <$> Gen.integral Range.linearBounded
                        <*> Gen.integral Range.linearBounded
                        <*> Gen.bool
                      |],
                    [|show|]
                  )
              ),
              ( [t|Int8|],
                pure
                  ( [|(,,) <$> Gen.enumBounded <*> Gen.enumBounded <*> Gen.bool|],
                    [|show|]
                  )
              ),
              ( [t|Int16|],
                pure
                  ( [|(,,) <$> Gen.enumBounded <*> Gen.enumBounded <*> Gen.bool|],
                    [|show|]
                  )
              ),
              ( [t|Int32|],
                pure
                  ( [|(,,) <$> Gen.enumBounded <*> Gen.enumBounded <*> Gen.bool|],
                    [|show|]
                  )
              ),
              ( [t|Int64|],
                pure
                  ( [|(,,) <$> Gen.enumBounded <*> Gen.enumBounded <*> Gen.bool|],
                    [|show|]
                  )
              ),
              ( [t|Float|],
                pure
                  ( [|(,,) <$> genFloating <*> genFloating <*> Gen.bool|],
                    [|show|]
                  )
              ),
              ( [t|Double|],
                pure
                  ( [|(,,) <$> genFloating <*> genFloating <*> Gen.bool|],
                    [|show|]
                  )
              )
            ]
        )
    )
  . HCons1
    (TestCases (const [([t|Double|], pure ([|(,) <$> genFloating <*> genFloating|], [|show|]))]))
  . HCons1 (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HCons1 (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HCons1 (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HCons1 (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HCons1 (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HCons1 (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HCons1 (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HCons1 (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HCons1 (TestCases (const [((), pure ([|genFloating|], [|show|]))]))
  . HCons1 (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HCons1 (TestCases (const [((), pure ([|genFloating|], [|show|]))]))
  . HCons1 (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HCons1 (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HCons1 (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HCons1 (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HCons1 (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HCons1 (TestCases (const [((), pure ([|genFloating|], [|show|]))]))
  . HCons1 (TestCases (const [((), pure ([|(,) <$> genFloating <*> genFloating|], [|show|]))]))
  . HCons1 (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HCons1 (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HCons1 (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HCons1 (TestCases (const [((), pure ([|genFloating|], [|show|]))]))
  . HCons1 (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HCons1 (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HCons1 (TestCases (const [((), pure ([|(,) <$> genFloating <*> genFloating|], [|show|]))]))
  . HCons1
    (TestCases (const [((), pure ([|(,) <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|]))]))
  . HCons1
    (TestCases (const [((), pure ([|(,) <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|]))]))
  . HCons1
    ( TestCases
        (const [([t|Int64|], pure ([|(,) <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|]))])
    )
  . HCons1
    ( TestCases
        (const [([t|Int64|], pure ([|(,) <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|]))])
    )
  . HCons1
    ( TestCases
        (const [([t|Int64|], pure ([|(,) <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|]))])
    )
  . HCons1
    ( TestCases
        (const [([t|Int64|], pure ([|(,) <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|]))])
    )
  . HCons1
    ( TestCases
        (const [([t|Int64|], pure ([|(,) <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|]))])
    )
  . HCons1
    ( TestCases
        (const [([t|Int64|], pure ([|(,) <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|]))])
    )
  . HCons1
    (TestCases (const [([t|Double|], pure ([|(,) <$> genFloating <*> genFloating|], [|show|]))]))
  . HCons1 (TestCases (const [((), pure ([|(,) <$> genFloating <*> genFloating|], [|show|]))]))
  . HCons1
    (TestCases (const [([t|Double|], pure ([|(,) <$> genFloating <*> genFloating|], [|show|]))]))
  . HCons1
    (TestCases (const [([t|Double|], pure ([|(,) <$> genFloating <*> genFloating|], [|show|]))]))
  . HCons1 (TestCases (const [((), pure ([|Gen.enumBounded|], [|show|]))]))
  . HCons1
    (TestCases (const [([t|Double|], pure ([|(,) <$> genFloating <*> genFloating|], [|show|]))]))
  . HCons1
    (TestCases (const [([t|Double|], pure ([|(,) <$> genFloating <*> genFloating|], [|show|]))]))
  . HCons1
    (TestCases (const [([t|Double|], pure ([|(,) <$> genFloating <*> genFloating|], [|show|]))]))
  . HCons1 (TestCases (const zerosafeUnsignedPrimitiveCases))
  . HCons1 (TestCases (const [(([t|Double|], [t|Float|]), pure ([|genFloating|], [|show|]))]))
  . HCons1 (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HCons1 (TestCases (const zerosafeUnsignedPrimitiveCases))
  . HCons1
    ( TestCases
        ( const
            [ ( [t|Word8|],
                pure
                  ([|(,) <$> Gen.enumBounded <*> Gen.integral (Range.linear 1 maxBound)|], [|show|])
              )
            ]
        )
    )
  . HCons1
    ( TestCases
        ( const
            [ ( [t|Word8|],
                pure
                  ([|(,) <$> Gen.enumBounded <*> Gen.integral (Range.linear 1 maxBound)|], [|show|])
              )
            ]
        )
    )
  . HCons1
    (TestCases (const [([t|Double|], pure ([|(,) <$> genFloating <*> genFloating|], [|show|]))]))
  . HCons1
    (TestCases (const [((), pure ([|(,) <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|]))]))
  . HCons1
    (TestCases (const [((), pure ([|(,) <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|]))]))
  . HCons1 (TestCases (const [])) -- no support for `atan2` in Categories
  . HCons1 (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HCons1 (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HCons1 (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HCons1 (TestCases (const []))
  . HCons1 (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HCons1
    ( TestCases
        ( const
            [ ( [t|Double|],
                pure
                  ( [|
                      Gen.integral $
                        Range.linearFrom
                          0
                          (toInteger (minBound :: Int64) - 1)
                          (toInteger (maxBound :: Int64) + 1)
                      |],
                    [|show|]
                  )
              )
            ]
        )
    )
  . HCons1
    ( TestCases
        (const [(([t|Int64|], [t|Double|]), pure ([|Gen.int64 Range.linearBounded|], [|show|]))])
    )
  . HCons1
    ( TestCases
        ( const
            [ ( [t|[Word8]|],
                pure
                  ( [|
                      (,)
                        <$> Gen.list (Range.linear 0 100) Gen.enumBounded
                        <*> Gen.list (Range.linear 0 100) Gen.enumBounded
                      |],
                    [|show|]
                  )
              )
            ]
        )
    )
  . HCons1
    ( TestCases
        ( const
            [ ( [t|[Word8]|],
                pure
                  ( [|
                      (,)
                        <$> Gen.list (Range.linear 0 100) Gen.enumBounded
                        <*> Gen.list (Range.linear 0 100) Gen.enumBounded
                      |],
                    [|show|]
                  )
              )
            ]
        )
    )
  . HCons1
    ( TestCases
        ( const
            [ ( [t|Word8|],
                pure
                  ( [|
                      (,)
                        <$> Gen.list (Range.linear 0 100) Gen.enumBounded
                        <*> Gen.list (Range.linear 0 100) Gen.enumBounded
                      |],
                    [|show|]
                  )
              )
            ]
        )
    )
  . HCons1 (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HCons1 (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HCons1 (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HCons1 (TestCases (const [])) -- no support for `error` in Categories
  . HCons1 (TestCases (const [(([t|Int64|], [t|Word8|]), pure ([|Gen.enumBounded|], [|show|]))]))
  . HCons1 (TestCases (const [(([t|Int64|], [t|Word8|]), pure ([|Gen.enumBounded|], [|show|]))]))
  . HCons1
    ( TestCases
        ( const
            [ ( [t|Word8|],
                pure
                  ( [|
                      do
                        x :: Word8 <- Gen.enumBounded
                        Gen.element [Left x, Right x]
                      |],
                    [|show|]
                  )
              )
            ]
        )
    )
  . HCons1
    ( TestCases
        ( const
            [ ( [t|Word8|],
                pure
                  ( [|
                      do
                        x :: Word8 <- Gen.enumBounded
                        Gen.element [Left x, Right x]
                      |],
                    [|show|]
                  )
              )
            ]
        )
    )
  . HCons1
    ( TestCases
        ( const
            [ ( ([t|Word8|], [t|Bool|]),
                pure ([|(,) <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|])
              )
            ]
        )
    )
  . HCons1
    ( TestCases
        ( const
            [ ( [t|Word8|],
                pure
                  ( [|
                      (,)
                        <$> Gen.element [const 42, id]
                        <*> (Pair <$> Gen.enumBounded <*> Gen.enumBounded)
                      |],
                    [|show . snd|]
                  )
              )
            ]
        )
    )
  . HCons1
    ( TestCases
        (const [([t|Word8|], pure ([|Pair <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|]))])
    )
  . HCons1
    ( TestCases
        ( const
            [ ( ([t|Pair|], [t|Word8|]),
                pure ([|Pair <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|])
              )
            ]
        )
    )
  . HCons1
    ( TestCases
        (const [([t|Word8|], pure ([|Pair <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|]))])
    )
  . HCons1 (TestCases (const []))
  . HCons1
    ( TestCases
        (const [([t|Word8|], pure ([|(,) <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|]))])
    )
  . HCons1
    ( TestCases
        ( const
            [([t|Word8|], pure ([|Gen.list (Range.exponential 1 1024) Gen.enumBounded|], [|show|]))]
        )
    )
  . HCons1 (TestCases (const [([t|Word8|], pure ([|Gen.enumBounded|], [|show|]))]))
  . HCons1
    ( TestCases
        ( const
            [ ( ([t|[]|], [t|Int64|]),
                pure ([|Gen.list (Range.linear 0 100) Gen.enumBounded|], [|show|])
              )
            ]
        )
    )
  . HCons1 (TestCases (const [([t|Int64|], pure ([|Gen.enumBounded|], [|show|]))]))
  . HCons1 (TestCases (const [])) -- no support for `liftA2` in Categories
  . HCons1
    ( TestCases
        (const [([t|Word8|], pure ([|(,pure) . Identity <$> Gen.enumBounded|], [|show . fst|]))])
    )
  . HCons1
    ( TestCases
        (const [([t|Word8|], pure ([|(,pure) . Identity <$> Gen.enumBounded|], [|show . fst|]))])
    )
  . HCons1
    ( TestCases
        ( const
            [ ( ([t|Word8|], [t|Bool|]),
                pure ([|(,) <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|])
              )
            ]
        )
    )
  . HCons1
    ( TestCases
        ( const
            [ ( ([t|Word8|], [t|Bool|]),
                pure ([|(,) <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|])
              )
            ]
        )
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
  . HCons1 (TestCases (const []))
  . HCons1 (TestCases (const []))
  . HCons1 (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HCons1 (TestCases (const [])) -- can only work with specialization
  . HCons1 (TestCases (const [])) -- can only work with specialization
  $ HNil1

main :: IO ()
main = bool exitFailure exitSuccess . and =<< allTestTerms
