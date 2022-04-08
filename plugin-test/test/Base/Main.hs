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

import Categorifier.Hedgehog (genFloating)
import Categorifier.Test.Data (Pair (..))
import Categorifier.Test.HList (HList1 (..))
import Categorifier.Test.Hask (Hask (..))
import Categorifier.Test.Term (Term)
import Categorifier.Test.Tests
  ( TestCases (..),
    TestCategory (..),
    TestStrategy (..),
    builtinTestCategories,
    defaultTestTerms,
    mkTestTerms,
  )
import Control.Applicative (liftA2)
import Control.Arrow (Arrow (..), ArrowChoice (..))
import Control.Monad ((<=<))
import Data.Bool (bool)
import Data.Either.Validation (Validation)
import Data.Semigroup (Sum (..))
import GHC.Int (Int64)
import GHC.Word (Word64, Word8)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import System.Exit (exitFailure, exitSuccess)

-- For @NoRebindableSyntax@
{-# ANN module ("HLint: ignore Avoid restricted integration" :: String) #-}

mkTestTerms
  defaultTestTerms
  --               name   type      prefix       strategy
  ( [ TestCategory ''Term [t|Term|] "term" CheckCompileOnly,
      TestCategory ''Hask [t|Hask|] "hask" (ComputeFromInput [|runHask|])
    ]
      <> builtinTestCategories
  )
  -- core
  . HCons1 (TestCases (const [([t|Word8|], pure ([|Gen.enumBounded|], [|show|]))]))
  . HCons1 (TestCases (const [([t|Word8|], pure ([|Gen.enumBounded|], [|show|]))]))
  . HCons1 (TestCases (const [(([t|Int64|], [t|Word8|]), Nothing)]))
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
  . HCons1 (TestCases (const [])) -- no support for `curry` in Base
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
        ( const
            [ ( [t|Word8|],
                pure ([|(,) <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|])
              )
            ]
        )
    )
  . HCons1
    ( TestCases
        ( const
            [ ( [t|Word8|],
                pure ([|Pair <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|])
              )
            ]
        )
    )
  -- base
  . HCons1 (TestCases (const [([t|Word8|], pure ([|Gen.enumBounded|], [|show|]))]))
  . HCons1 (TestCases (const [(([t|Int64|], [t|Word8|]), Nothing)]))
  . HCons1
    ( TestCases
        ( const
            [ ( ([t|Word8|], [t|Word8|]),
                pure ([|(,) <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|])
              )
            ]
        )
    )
  . HCons1 (TestCases (const [(([t|Word8|], [t|Word8|], [t|Word8|]), Nothing)]))
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
  . HCons1 (TestCases (const [(([t|Int64|], [t|Word8|]), Nothing)]))
  . HCons1 (TestCases (const [(([t|Int64|], [t|Word8|]), Nothing)]))
  . HCons1 (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HCons1 (TestCases (const [(([t|Int64|], [t|Word8|]), Nothing)]))
  . HCons1 (TestCases (const [([t|Word8|], pure ([|Gen.enumBounded|], [|show|]))]))
  . HCons1 (TestCases (const [([t|Word8|], Nothing)]))
  . HCons1 (TestCases (const [([t|Double|], Nothing)]))
  . HCons1 (TestCases (const [([t|Double|], Nothing)]))
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
  . HCons1 (TestCases (const [((), Nothing)]))
  . HCons1 (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HCons1 (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HCons1 (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HCons1 (TestCases (const [((), pure ([|genFloating|], [|show|]))]))
  . HCons1 (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HCons1 (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HCons1 (TestCases (const [((), Nothing)]))
  . HCons1 (TestCases (const [((), Nothing)]))
  . HCons1 (TestCases (const [((), Nothing)]))
  . HCons1 (TestCases (const [([t|Int64|], Nothing)]))
  . HCons1 (TestCases (const [([t|Int64|], Nothing)]))
  . HCons1 (TestCases (const [([t|Int64|], Nothing)]))
  . HCons1 (TestCases (const [([t|Int64|], Nothing)]))
  . HCons1 (TestCases (const [([t|Int64|], Nothing)]))
  . HCons1 (TestCases (const [([t|Int64|], Nothing)]))
  . HCons1
    ( TestCases
        ( \arrow ->
            if arrow /= ''Hask
              then [] -- Only `Hask` currently has `OrdCat'` instance
              else [([t|Double|], Nothing)]
        )
    )
  . HCons1 (TestCases (const [((), Nothing)]))
  . HCons1 (TestCases (const [([t|Double|], Nothing)]))
  . HCons1 (TestCases (const [([t|Double|], Nothing)]))
  . HCons1 (TestCases (const [((), pure ([|Gen.enumBounded|], [|show|]))]))
  . HCons1 (TestCases (const [([t|Int64|], Nothing)]))
  . HCons1 (TestCases (const [([t|Int64|], Nothing)]))
  . HCons1 (TestCases (const [([t|Int64|], Nothing)]))
  . HCons1 (TestCases (const [([t|Word8|], Nothing)]))
  . HCons1 (TestCases (const [(([t|Double|], [t|Float|]), pure ([|genFloating|], [|show|]))]))
  . HCons1 (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HCons1 (TestCases (const [([t|Word8|], Nothing)]))
  . HCons1 (TestCases (const [([t|Word64|], Nothing)]))
  . HCons1 (TestCases (const [([t|Word8|], Nothing)]))
  . HCons1 (TestCases (const [([t|Double|], Nothing)]))
  . HCons1 (TestCases (const [((), Nothing)]))
  . HCons1 (TestCases (const [((), Nothing)]))
  . HCons1 (TestCases (const [])) -- no `curry`
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
  . HCons1 (TestCases (const [([t|[Word8]|], Nothing)]))
  . HCons1 (TestCases (const [([t|[Word8]|], Nothing)]))
  . HCons1 (TestCases (const [([t|Word8|], Nothing)]))
  . HCons1 (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HCons1 (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HCons1
    ( TestCases
        ( \arrow ->
            if arrow `elem` [''(->), ''Hask]
              then [] -- we expect bottomC to raise an exception in these categories
              else
                [ ( [t|Word8|],
                    pure ([|Gen.string Range.linearBounded Gen.unicodeAll|], [|show|])
                  )
                ]
        )
    )
  . HCons1 (TestCases (const [(([t|Int64|], [t|Word8|]), pure ([|Gen.enumBounded|], [|show|]))]))
  . HCons1 (TestCases (const [(([t|Int64|], [t|Word8|]), pure ([|Gen.enumBounded|], [|show|]))]))
  . HCons1 (TestCases (const [([t|Word8|], Nothing)]))
  . HCons1 (TestCases (const [([t|Word8|], Nothing)]))
  . HCons1 (TestCases (const [(([t|Word8|], [t|Bool|]), Nothing)]))
  . HCons1 (TestCases (const [([t|Word8|], Nothing)]))
  . HCons1 (TestCases (const [([t|Word8|], Nothing)]))
  . HCons1 (TestCases (const [(([t|Pair|], [t|Word8|]), Nothing)]))
  . HCons1 (TestCases (const [([t|Word8|], Nothing)]))
  . HCons1 (TestCases (const [([t|Word8|], Nothing)]))
  . HCons1 (TestCases (const [([t|Word8|], Nothing)]))
  . HCons1 (TestCases (const [([t|Word8|], pure ([|Gen.enumBounded|], [|show|]))]))
  . HCons1 (TestCases (const [])) -- no curry
  . HCons1 (TestCases (const [(([t|Validation ()|], [t|Int64|], [t|Int64|]), Nothing)]))
  . HCons1 (TestCases (const [([t|Word8|], Nothing)])) -- no curry
  . HCons1 (TestCases (const [(([t|Word8|], [t|Bool|]), Nothing)]))
  . HCons1 (TestCases (const [(([t|Word8|], [t|Bool|]), Nothing)]))
  . HCons1 (TestCases (const []))
  . HCons1 (TestCases (const []))
  . HCons1 (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HCons1 (TestCases (const [])) -- can only work with specialization
  . HCons1 (TestCases (const [])) -- can only work with specialization
  . HCons1 (TestCases (const [([t|Int64|], pure ([|Gen.enumBounded|], [|show|]))]))
  . HCons1 (TestCases (const [([t|Int64|], pure ([|Gen.enumBounded|], [|show|]))]))
  $ HNil1

main :: IO ()
main = bool exitFailure exitSuccess . and =<< allTestTerms
