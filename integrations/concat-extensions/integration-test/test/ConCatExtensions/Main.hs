{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main
  ( main,
  )
where

import Categorifier.Hedgehog (genFloating)
import Categorifier.Test.ConCatExtensions.Instances (Hask (..), Term)
import Categorifier.Test.Data (Pair (..))
import Categorifier.Test.HList (HMap1 (..))
import Categorifier.Test.Tests
  ( TestCases (..),
    TestCategory (..),
    TestStrategy (..),
    builtinTestCategories,
    defaultTestTerms,
    mkTestTerms,
    zerosafeUnsignedPrimitiveCases,
  )
import Control.Arrow (Arrow (..), ArrowChoice (..))
import Data.Bool (bool)
import Data.Functor.Identity (Identity (..))
import Data.Proxy (Proxy (..))
import Data.Semigroup (Product (..), Sum (..))
import GHC.Int (Int16, Int32, Int64, Int8)
import GHC.Word (Word16, Word32, Word64, Word8)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Linear.V2 (V2 (..))
import System.Exit (exitFailure, exitSuccess)

-- |
--
--  __TODO__: This doesn't yet test against `Categorifier.Test.TotOrd.TotOrd` because a lot of tests need
--            to be disabled since that category isn't closed.
mkTestTerms
  defaultTestTerms
  --               name     type         prefix       strategy
  ( [ TestCategory ''Term [t|Term|] "term" CheckCompileOnly,
      TestCategory ''Hask [t|Hask|] "hask" (ComputeFromInput [|runHask|])
    ]
      <> builtinTestCategories
  )
  -- core
  . HInsert1 (Proxy @"LamId") (TestCases (const [([t|Word8|], pure ([|Gen.enumBounded|], [|show|]))]))
  . HInsert1 (Proxy @"ComposeLam") (TestCases (const [([t|Word8|], pure ([|Gen.enumBounded|], [|show|]))]))
  . HInsert1
    (Proxy @"ConstLam")
    ( TestCases
        ( const
            [ ( ([t|Int64|], [t|Word8|]),
                pure ([|(,) <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|])
              )
            ]
        )
    )
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
  . HInsert1 (Proxy @"LocalFixedPoint") (TestCases (const []))
  . HInsert1 (Proxy @"ApplyArg") (TestCases (const [])) -- no ToTargetOb (Word8 -> Word8) (Word8 -> Word8)
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
  . HInsert1
    (Proxy @"Const")
    ( TestCases
        ( const
            [ ( ([t|Int64|], [t|Word8|]),
                pure ([|(,) <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|])
              )
            ]
        )
    )
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
  . HInsert1
    (Proxy @"FstSnd")
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
  . HInsert1 (Proxy @"Fork") (TestCases (const [(([t|Int64|], [t|Word8|]), pure ([|Gen.enumBounded|], [|show|]))]))
  . HInsert1
    (Proxy @"Join")
    ( TestCases
        ( const
            [ ( ([t|Int64|], [t|Word8|]),
                pure
                  ([|Gen.choice [Left <$> Gen.enumBounded, Right <$> Gen.enumBounded]|], [|show|])
              )
            ]
        )
    )
  . HInsert1 (Proxy @"Arr") (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1
    (Proxy @"Either")
    ( TestCases
        ( const
            [ ( ([t|Int64|], [t|Word8|]),
                pure
                  ([|Gen.choice [Left <$> Gen.enumBounded, Right <$> Gen.enumBounded]|], [|show|])
              )
            ]
        )
    )
  . HInsert1 (Proxy @"Coerce") (TestCases (const [([t|Word8|], pure ([|Gen.enumBounded|], [|show|]))]))
  . HInsert1
    (Proxy @"ComposedCoerce")
    (TestCases (const [([t|Word8|], pure ([|Gen.enumBounded|], [|show|]))]))
  . HInsert1
    (Proxy @"Bool")
    ( TestCases
        ( const
            [ ([t|Bool|], pure ([|(,,) <$> Gen.bool <*> Gen.bool <*> Gen.bool|], [|show|])),
              ( [t|Word8|],
                pure ([|(,,) <$> Gen.enumBounded <*> Gen.enumBounded <*> Gen.bool|], [|show|])
              ),
              ( [t|Word16|],
                pure ([|(,,) <$> Gen.enumBounded <*> Gen.enumBounded <*> Gen.bool|], [|show|])
              ),
              ( [t|Word32|],
                pure ([|(,,) <$> Gen.enumBounded <*> Gen.enumBounded <*> Gen.bool|], [|show|])
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
                pure ([|(,,) <$> Gen.enumBounded <*> Gen.enumBounded <*> Gen.bool|], [|show|])
              ),
              ( [t|Int16|],
                pure ([|(,,) <$> Gen.enumBounded <*> Gen.enumBounded <*> Gen.bool|], [|show|])
              ),
              ( [t|Int32|],
                pure ([|(,,) <$> Gen.enumBounded <*> Gen.enumBounded <*> Gen.bool|], [|show|])
              ),
              ( [t|Int64|],
                pure ([|(,,) <$> Gen.enumBounded <*> Gen.enumBounded <*> Gen.bool|], [|show|])
              ),
              ( [t|Float|],
                pure ([|(,,) <$> genFloating <*> genFloating <*> Gen.bool|], [|show|])
              ),
              ( [t|Double|],
                pure ([|(,,) <$> genFloating <*> genFloating <*> Gen.bool|], [|show|])
              )
            ]
        )
    )
  . HInsert1
    (Proxy @"Pow")
    (TestCases (const [([t|Double|], pure ([|(,) <$> genFloating <*> genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"Acos") (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"Acosh") (TestCases (const [])) -- ACoshNotSupported
  . HInsert1 (Proxy @"Asin") (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"Asinh") (TestCases (const [])) -- ASinhNotSupported
  . HInsert1 (Proxy @"Atan") (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"Atanh") (TestCases (const [])) -- AtanhNotSupported
  . HInsert1 (Proxy @"Cos") (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"Cosh") (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"Double2Float") (TestCases (const [((), pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"Exp") (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"Float2Double") (TestCases (const [((), pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"IsDenormalized") (TestCases (const [])) -- IsDenormalKNotSupported
  . HInsert1 (Proxy @"IsInfinite") (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"IsNaN") (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"IsNegativeZero") (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"Log") (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"NegateDouble") (TestCases (const [((), pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"PlusDouble") (TestCases (const [((), pure ([|(,) <$> genFloating <*> genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"Sin") (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"Sinh") (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"Sqrt") (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"SqrtDouble") (TestCases (const [((), pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"Tan") (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"Tanh") (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"TimesDouble") (TestCases (const [((), pure ([|(,) <$> genFloating <*> genFloating|], [|show|]))]))
  . HInsert1
    (Proxy @"And")
    (TestCases (const [((), pure ([|(,) <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|]))]))
  . HInsert1
    (Proxy @"Or")
    (TestCases (const [((), pure ([|(,) <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|]))]))
  . HInsert1
    (Proxy @"Equal")
    ( TestCases
        (const [([t|Int64|], pure ([|(,) <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|]))])
    )
  . HInsert1
    (Proxy @"NotEqual")
    ( TestCases
        (const [([t|Int64|], pure ([|(,) <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|]))])
    )
  . HInsert1
    (Proxy @"Ge")
    ( TestCases
        (const [([t|Int64|], pure ([|(,) <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|]))])
    )
  . HInsert1
    (Proxy @"Gt")
    ( TestCases
        (const [([t|Int64|], pure ([|(,) <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|]))])
    )
  . HInsert1
    (Proxy @"Le")
    ( TestCases
        (const [([t|Int64|], pure ([|(,) <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|]))])
    )
  . HInsert1
    (Proxy @"Lt")
    ( TestCases
        (const [([t|Int64|], pure ([|(,) <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|]))])
    )
  . HInsert1 (Proxy @"Compare") (TestCases (const [])) -- no `OrdCat'` instance
  . HInsert1
    (Proxy @"EqDouble")
    (TestCases (const [((), pure ([|(,) <$> genFloating <*> genFloating|], [|show|]))]))
  . HInsert1
    (Proxy @"Max")
    (TestCases (const [([t|Double|], pure ([|(,) <$> genFloating <*> genFloating|], [|show|]))]))
  . HInsert1
    (Proxy @"Min")
    (TestCases (const [([t|Double|], pure ([|(,) <$> genFloating <*> genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"Not") (TestCases (const [((), pure ([|Gen.enumBounded|], [|show|]))]))
  . HInsert1
    (Proxy @"Plus")
    (TestCases (const [([t|Double|], pure ([|(,) <$> genFloating <*> genFloating|], [|show|]))]))
  . HInsert1
    (Proxy @"Minus")
    (TestCases (const [([t|Double|], pure ([|(,) <$> genFloating <*> genFloating|], [|show|]))]))
  . HInsert1
    (Proxy @"Times")
    (TestCases (const [([t|Double|], pure ([|(,) <$> genFloating <*> genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"Quot") (TestCases (const zerosafeUnsignedPrimitiveCases))
  . HInsert1
    (Proxy @"RealToFrac")
    (TestCases (const [(([t|Double|], [t|Float|]), pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"Recip") (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"Rem") (TestCases (const zerosafeUnsignedPrimitiveCases))
  . HInsert1
    (Proxy @"Div")
    ( TestCases
        ( const
            [ ( [t|Word8|],
                pure
                  ([|(,) <$> Gen.enumBounded <*> Gen.integral (Range.linear 1 maxBound)|], [|show|])
              )
            ]
        )
    )
  . HInsert1
    (Proxy @"Mod")
    ( TestCases
        ( const
            [ ( [t|Word8|],
                pure
                  ([|(,) <$> Gen.enumBounded <*> Gen.integral (Range.linear 1 maxBound)|], [|show|])
              )
            ]
        )
    )
  . HInsert1
    (Proxy @"Divide")
    (TestCases (const [([t|Double|], pure ([|(,) <$> genFloating <*> genFloating|], [|show|]))]))
  . HInsert1
    (Proxy @"EqWord8")
    (TestCases (const [((), pure ([|(,) <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|]))]))
  . HInsert1
    (Proxy @"NeWord8")
    (TestCases (const [((), pure ([|(,) <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|]))]))
  . HInsert1 (Proxy @"Atan2") (TestCases (const [])) -- `arctan2` differs from `atan2`
  . HInsert1 (Proxy @"Abs") (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"Negate") (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"Signum") (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"PowI") (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"PowInt") (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"FromInteger") (TestCases (const [])) -- Integer isn't an Expr
  . HInsert1
    (Proxy @"FromIntegral")
    ( TestCases
        (const [(([t|Int64|], [t|Double|]), pure ([|Gen.int64 Range.linearBounded|], [|show|]))])
    )
  . HInsert1
    (Proxy @"Append")
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
  . HInsert1
    (Proxy @"Mappend")
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
  . HInsert1
    (Proxy @"ListAppend")
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
  . HInsert1 (Proxy @"Pure") (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"Return") (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"Error") (TestCases (const [])) -- `String` is not an object in these categories
  . HInsert1
    (Proxy @"BuildLeft")
    (TestCases (const [(([t|Int64|], [t|Word8|]), pure ([|Gen.enumBounded|], [|show|]))]))
  . HInsert1
    (Proxy @"BuildRight")
    (TestCases (const [(([t|Int64|], [t|Word8|]), pure ([|Gen.enumBounded|], [|show|]))]))
  . HInsert1
    (Proxy @"EliminateEither")
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
  . HInsert1
    (Proxy @"EliminateEitherSwapped")
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
  . HInsert1
    (Proxy @"Apply")
    ( TestCases
        ( const
            [ ( ([t|Word8|], [t|Bool|]),
                pure ([|(,) <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|])
              )
            ]
        )
    )
  . HInsert1 (Proxy @"BareFMap") (TestCases (const [])) -- no ToTargetOb (Word8 -> Word8) (Word8 -> Word8)
  . HInsert1
    (Proxy @"PartialFmap")
    ( TestCases
        (const [([t|Word8|], pure ([|Pair <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|]))])
    )
  . HInsert1
    (Proxy @"Fmap")
    ( TestCases
        ( const
            [ ( ([t|Pair|], [t|Word8|]),
                pure ([|Pair <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|])
              ),
              -- Tests `$fAdditiveV2_$cfmap`.
              ( ([t|V2|], [t|Word8|]),
                pure ([|V2 <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|])
              )
            ]
        )
    )
  . HInsert1
    (Proxy @"Fmap'")
    ( TestCases
        (const [([t|Word8|], pure ([|Pair <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|]))])
    )
  . HInsert1
    (Proxy @"ConstNot")
    ( TestCases
        (const [([t|Word8|], pure ([|(,) <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|]))])
    )
  . HInsert1
    (Proxy @"MapList")
    ( TestCases
        ( const
            [([t|Word8|], pure ([|Gen.list (Range.exponential 1 1024) Gen.enumBounded|], [|show|]))]
        )
    )
  . HInsert1 (Proxy @"Point") (TestCases (const [([t|Word8|], pure ([|Gen.enumBounded|], [|show|]))]))
  . HInsert1
    (Proxy @"Ap")
    ( TestCases
        ( const
            [ ( ([t|[]|], [t|Int64|]),
                pure ([|Gen.list (Range.linear 0 100) Gen.enumBounded|], [|show|])
              ),
              -- Tests `$fAdditiveV2_$c<*>`.
              ( ([t|V2|], [t|Int64|]),
                pure ([|V2 <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|])
              )
            ]
        )
    )
  . HInsert1 (Proxy @"LiftA2") (TestCases (const [])) -- no ToTargetOb Validation ...
  . HInsert1
    (Proxy @"Bind")
    ( TestCases
        ( const
            [ ( [t|Word8|],
                pure ([|(\x -> (x, pure)) . Identity <$> Gen.enumBounded|], [|show . fst|])
              )
            ]
        )
    )
  . HInsert1
    (Proxy @"Curry")
    ( TestCases
        ( const
            [ ( ([t|Word8|], [t|Bool|]),
                pure ([|(,) <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|])
              )
            ]
        )
    )
  . HInsert1
    (Proxy @"Uncurry")
    ( TestCases
        ( const
            [ ( ([t|Word8|], [t|Bool|]),
                pure ([|(,) <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|])
              )
            ]
        )
    )
  . HInsert1
    (Proxy @"SequenceA")
    ( TestCases
        ( const
            [ ( ([t|Sum|], [t|Product|], [t|Word8|]),
                pure ([|Sum . Product <$> Gen.enumBounded|], [|show|])
              )
            ]
        )
    )
  . HInsert1
    (Proxy @"Traverse")
    ( TestCases
        ( const
            [(([t|Sum|], [t|Product|], [t|Word8|]), pure ([|Sum <$> Gen.enumBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"UnsafeCoerce")
    (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"Sum") (TestCases (const []))
  . HInsert1 (Proxy @"ToList") (TestCases (const []))
  . HInsert1
    (Proxy @"Even")
    (TestCases (const [([t|Int64|], pure ([|Gen.enumBounded|], [|show|]))]))
  . HInsert1 (Proxy @"Odd") (TestCases (const [([t|Int64|], pure ([|Gen.enumBounded|], [|show|]))]))
  $ HEmpty1

main :: IO ()
main = bool exitFailure exitSuccess . and =<< allTestTerms
