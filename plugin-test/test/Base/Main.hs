{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
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
import Categorifier.Test.Data (Pair (..))
import Categorifier.Test.HList (HMap1 (..))
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
import Data.Proxy (Proxy (..))
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
  . HInsert1 (Proxy @"LamId") (TestCases (const [([t|Word8|], pure ([|Gen.enumBounded|], [|show|]))]))
  . HInsert1 (Proxy @"ComposeLam") (TestCases (const [([t|Word8|], pure ([|Gen.enumBounded|], [|show|]))]))
  . HInsert1 (Proxy @"ConstLam") (TestCases (const [(([t|Int64|], [t|Word8|]), Nothing)]))
  . HInsert1 (Proxy @"ReturnLam") (TestCases (const [([t|Word8|], pure ([|Gen.enumBounded|], [|show|]))]))
  . HInsert1 (Proxy @"BuildTuple") (TestCases (const [([t|Word8|], pure ([|Gen.enumBounded|], [|show|]))]))
  . HInsert1
    (Proxy @"EliminateTupleFst")
    ( TestCases
        (const [([t|Word8|], pure ([|(,) <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|]))])
    )
  . HInsert1
    (Proxy @"EliminateTupleSnd")
    ( TestCases
        (const [([t|Word8|], pure ([|(,) <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|]))])
    )
  . HInsert1
    (Proxy @"EliminateNestedTuples")
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
  . HInsert1 (Proxy @"LocalFixedPoint") (TestCases (const [])) -- no support for `curry` in Base
  . HInsert1
    (Proxy @"ApplyArg")
    ( TestCases
        ( const
            [ ( [t|Word8|],
                pure ([|Gen.choice [const <$> Gen.enumBounded, pure id]|], [|const "<function>"|])
              )
            ]
        )
    )
  . HInsert1
    (Proxy @"If")
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
  . HInsert1
    (Proxy @"Abst")
    ( TestCases
        ( const
            [ ( [t|Word8|],
                pure ([|(,) <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|])
              )
            ]
        )
    )
  . HInsert1
    (Proxy @"Repr")
    ( TestCases
        ( const
            [ ( [t|Word8|],
                pure ([|Pair <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|])
              )
            ]
        )
    )
  -- base
  . HInsert1 (Proxy @"Id") (TestCases (const [([t|Word8|], pure ([|Gen.enumBounded|], [|show|]))]))
  . HInsert1 (Proxy @"Const") (TestCases (const [(([t|Int64|], [t|Word8|]), Nothing)]))
  . HInsert1
    (Proxy @"Snd")
    ( TestCases
        ( const
            [ ( ([t|Word8|], [t|Word8|]),
                pure ([|(,) <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|])
              )
            ]
        )
    )
  . HInsert1 (Proxy @"FstSnd") (TestCases (const [(([t|Word8|], [t|Word8|], [t|Word8|]), Nothing)]))
  . HInsert1
    (Proxy @"FstLet")
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
  . HInsert1
    (Proxy @"Swap")
    ( TestCases
        ( const
            [ ( ([t|Word8|], [t|Int64|]),
                pure ([|(,) <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|])
              )
            ]
        )
    )
  . HInsert1 (Proxy @"Fork") (TestCases (const [(([t|Int64|], [t|Word8|]), Nothing)]))
  . HInsert1 (Proxy @"Join") (TestCases (const [(([t|Int64|], [t|Word8|]), Nothing)]))
  . HInsert1 (Proxy @"Arr") (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"Either") (TestCases (const [(([t|Int64|], [t|Word8|]), Nothing)]))
  . HInsert1 (Proxy @"Coerce") (TestCases (const [([t|Word8|], pure ([|Gen.enumBounded|], [|show|]))]))
  . HInsert1 (Proxy @"ComposedCoerce") (TestCases (const [([t|Word8|], Nothing)]))
  . HInsert1 (Proxy @"Bool") (TestCases (const [([t|Double|], Nothing)]))
  . HInsert1 (Proxy @"Pow") (TestCases (const [([t|Double|], Nothing)]))
  . HInsert1 (Proxy @"Acos") (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"Acosh") (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"Asin") (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"Asinh") (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"Atan") (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"Atanh") (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"Cos") (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"Cosh") (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"Double2Float") (TestCases (const [((), pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"Exp") (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"Float2Double") (TestCases (const [((), pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"IsDenormalized") (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"IsInfinite") (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"IsNaN") (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"IsNegativeZero") (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"Log") (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"NegateDouble") (TestCases (const [((), pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"PlusDouble") (TestCases (const [((), Nothing)]))
  . HInsert1 (Proxy @"Sin") (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"Sinh") (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"Sqrt") (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"SqrtDouble") (TestCases (const [((), pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"Tan") (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"Tanh") (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"TimesDouble") (TestCases (const [((), Nothing)]))
  . HInsert1 (Proxy @"And") (TestCases (const [((), Nothing)]))
  . HInsert1 (Proxy @"Or") (TestCases (const [((), Nothing)]))
  . HInsert1 (Proxy @"Equal") (TestCases (const [([t|Int64|], Nothing)]))
  . HInsert1 (Proxy @"NotEqual") (TestCases (const [([t|Int64|], Nothing)]))
  . HInsert1 (Proxy @"Ge") (TestCases (const [([t|Int64|], Nothing)]))
  . HInsert1 (Proxy @"Gt") (TestCases (const [([t|Int64|], Nothing)]))
  . HInsert1 (Proxy @"Le") (TestCases (const [([t|Int64|], Nothing)]))
  . HInsert1 (Proxy @"Lt") (TestCases (const [([t|Int64|], Nothing)]))
  . HInsert1
    (Proxy @"Compare")
    ( TestCases
        ( \arrow ->
            if arrow /= ''Hask
              then [] -- Only `Hask` currently has `OrdCat'` instance
              else [([t|Double|], Nothing)]
        )
    )
  . HInsert1 (Proxy @"EqDouble") (TestCases (const [((), Nothing)]))
  . HInsert1 (Proxy @"Max") (TestCases (const [([t|Double|], Nothing)]))
  . HInsert1 (Proxy @"Min") (TestCases (const [([t|Double|], Nothing)]))
  . HInsert1 (Proxy @"Not") (TestCases (const [((), pure ([|Gen.enumBounded|], [|show|]))]))
  . HInsert1 (Proxy @"Plus") (TestCases (const [([t|Int64|], Nothing)]))
  . HInsert1 (Proxy @"Minus") (TestCases (const [([t|Int64|], Nothing)]))
  . HInsert1 (Proxy @"Times") (TestCases (const [([t|Int64|], Nothing)]))
  . HInsert1 (Proxy @"Quot") (TestCases (const [([t|Word8|], Nothing)]))
  . HInsert1
    (Proxy @"RealToFrac")
    (TestCases (const [(([t|Double|], [t|Float|]), pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"Recip") (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"Rem") (TestCases (const [([t|Word8|], Nothing)]))
  . HInsert1 (Proxy @"Div") (TestCases (const [([t|Word64|], Nothing)]))
  . HInsert1 (Proxy @"Mod") (TestCases (const [([t|Word8|], Nothing)]))
  . HInsert1 (Proxy @"Divide") (TestCases (const [([t|Double|], Nothing)]))
  . HInsert1 (Proxy @"EqWord8") (TestCases (const [((), Nothing)]))
  . HInsert1 (Proxy @"NeWord8") (TestCases (const [((), Nothing)]))
  . HInsert1 (Proxy @"Atan2") (TestCases (const [])) -- no `curry`
  . HInsert1 (Proxy @"Abs") (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"Negate") (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"Signum") (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"PowI") (TestCases (const []))
  . HInsert1 (Proxy @"PowInt") (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1
    (Proxy @"FromInteger")
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
  . HInsert1
    (Proxy @"FromIntegral")
    ( TestCases
        (const [(([t|Int64|], [t|Double|]), pure ([|Gen.int64 Range.linearBounded|], [|show|]))])
    )
  . HInsert1 (Proxy @"Append") (TestCases (const [([t|[Word8]|], Nothing)]))
  . HInsert1 (Proxy @"Mappend") (TestCases (const [([t|[Word8]|], Nothing)]))
  . HInsert1 (Proxy @"ListAppend") (TestCases (const [([t|Word8|], Nothing)]))
  . HInsert1 (Proxy @"Pure") (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"Return") (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1
    (Proxy @"Error")
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
  . HInsert1 (Proxy @"BuildLeft") (TestCases (const [(([t|Int64|], [t|Word8|]), pure ([|Gen.enumBounded|], [|show|]))]))
  . HInsert1 (Proxy @"BuildRight") (TestCases (const [(([t|Int64|], [t|Word8|]), pure ([|Gen.enumBounded|], [|show|]))]))
  . HInsert1 (Proxy @"EliminateEither") (TestCases (const [([t|Word8|], Nothing)]))
  . HInsert1 (Proxy @"EliminateEitherSwapped") (TestCases (const [([t|Word8|], Nothing)]))
  . HInsert1 (Proxy @"Apply") (TestCases (const [(([t|Word8|], [t|Bool|]), Nothing)]))
  . HInsert1 (Proxy @"BareFMap") (TestCases (const [([t|Word8|], Nothing)]))
  . HInsert1 (Proxy @"PartialFmap") (TestCases (const [([t|Word8|], Nothing)]))
  . HInsert1 (Proxy @"Fmap") (TestCases (const [(([t|Pair|], [t|Word8|]), Nothing)]))
  . HInsert1 (Proxy @"Fmap'") (TestCases (const [([t|Word8|], Nothing)]))
  . HInsert1 (Proxy @"ConstNot") (TestCases (const [([t|Word8|], Nothing)]))
  . HInsert1 (Proxy @"MapList") (TestCases (const [([t|Word8|], Nothing)]))
  . HInsert1 (Proxy @"Point") (TestCases (const [([t|Word8|], pure ([|Gen.enumBounded|], [|show|]))]))
  . HInsert1 (Proxy @"Ap") (TestCases (const [])) -- no curry
  . HInsert1 (Proxy @"LiftA2") (TestCases (const [(([t|Validation ()|], [t|Int64|], [t|Int64|]), Nothing)]))
  . HInsert1 (Proxy @"Bind") (TestCases (const [([t|Word8|], Nothing)])) -- no curry
  . HInsert1 (Proxy @"Curry") (TestCases (const [(([t|Word8|], [t|Bool|]), Nothing)]))
  . HInsert1 (Proxy @"Uncurry") (TestCases (const [(([t|Word8|], [t|Bool|]), Nothing)]))
  . HInsert1 (Proxy @"SequenceA") (TestCases (const []))
  . HInsert1 (Proxy @"Traverse") (TestCases (const []))
  . HInsert1 (Proxy @"UnsafeCoerce") (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"Sum") (TestCases (const [])) -- can only work with specialization
  . HInsert1 (Proxy @"ToList") (TestCases (const [])) -- can only work with specialization
  . HInsert1 (Proxy @"Even") (TestCases (const [([t|Int64|], pure ([|Gen.enumBounded|], [|show|]))]))
  . HInsert1 (Proxy @"Odd") (TestCases (const [([t|Int64|], pure ([|Gen.enumBounded|], [|show|]))]))
  $ HEmpty1

main :: IO ()
main = bool exitFailure exitSuccess . and =<< allTestTerms
