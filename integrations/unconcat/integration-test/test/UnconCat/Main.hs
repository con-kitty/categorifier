{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Main
  ( main,
  )
where

import Categorifier.Hedgehog (genFloating)
import Categorifier.Test.Data (Pair (..))
import Categorifier.Test.HList (HMap1 (..))
import Categorifier.Test.Tests
  ( TestCases (..),
    TestCategory (..),
    TestStrategy (..),
    builtinTestCategories,
    defaultTestTerms,
    mkTestTerms,
  )
import Categorifier.Test.UnconCat.Instances (Hask (..), Term)
import Control.Arrow (Arrow (..), ArrowChoice (..))
import Data.Bool (bool)
import Data.Proxy (Proxy (..))
import Data.Semigroup (Sum (..))
import GHC.Int (Int16, Int32, Int64, Int8)
import GHC.Word (Word16, Word32, Word64, Word8)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import System.Exit (exitFailure, exitSuccess)

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
  . HInsert1 (Proxy @"LocalFixedPoint") (TestCases (const [])) -- no support for fixed-points in ConCat hierarchy
  . HInsert1
    (Proxy @"ApplyArg")
    ( TestCases
        ( const
            [ ( [t|Word8|],
                pure
                  ([|Gen.choice [const <$> Gen.enumBounded, pure id]|], [|const "<function>"|])
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
            [([t|Word8|], pure ([|(,) <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"Repr")
    ( TestCases
        (const [([t|Word8|], pure ([|Pair <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|]))])
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
                      (,) <$> Gen.enumBounded <*> ((,) <$> Gen.enumBounded <*> Gen.enumBounded)
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
  . HInsert1
    (Proxy @"Fork")
    (TestCases (const [(([t|Int64|], [t|Word8|]), pure ([|Gen.enumBounded|], [|show|]))]))
  . HInsert1
    (Proxy @"Join")
    ( TestCases
        ( const
            [ ( ([t|Int64|], [t|Word8|]),
                pure
                  ( [|Gen.choice [Left <$> Gen.enumBounded, Right <$> Gen.enumBounded]|],
                    [|show|]
                  )
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
                  ( [|Gen.choice [Left <$> Gen.enumBounded, Right <$> Gen.enumBounded]|],
                    [|show|]
                  )
              )
            ]
        )
    )
  . HInsert1 (Proxy @"Coerce") (TestCases (const [([t|Word8|], pure ([|Gen.enumBounded|], [|show|]))]))
  . HInsert1 (Proxy @"ComposedCoerce") (TestCases (const [([t|Word8|], pure ([|Gen.enumBounded|], [|show|]))]))
  . HInsert1
    (Proxy @"Bool")
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
  . HInsert1 (Proxy @"Pow") (TestCases (const [])) -- no support for `**` in ConCat
  . HInsert1 (Proxy @"Acos") (TestCases (const [])) -- no support for `acos` in ConCat
  . HInsert1 (Proxy @"Acosh") (TestCases (const [])) -- no support for `acosh` in ConCat
  . HInsert1 (Proxy @"Asin") (TestCases (const [])) -- no support for `asin` in ConCat
  . HInsert1 (Proxy @"Asinh") (TestCases (const [])) -- no support for `asinh` in ConCat
  . HInsert1 (Proxy @"Atan") (TestCases (const [])) -- no support for `atan` in ConCat
  . HInsert1 (Proxy @"Atanh") (TestCases (const [])) -- no support for `atanh` in ConCat
  . HInsert1 (Proxy @"Cos") (TestCases (const [])) -- no support for `cos` in ConCat
  . HInsert1 (Proxy @"Cosh") (TestCases (const [])) -- no support for `cosh` in ConCat
  . HInsert1 (Proxy @"Double2Float") (TestCases (const [])) -- no support for `double2Float` in ConCat
  . HInsert1 (Proxy @"Exp") (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"Float2Double") (TestCases (const [])) -- no support for `float2Double` in ConCat
  . HInsert1 (Proxy @"IsDenormalized") (TestCases (const [])) -- no support for `isDenormalized` in ConCat
  . HInsert1 (Proxy @"IsInfinite") (TestCases (const [])) -- no support for `isInfinite` in ConCat
  . HInsert1 (Proxy @"IsNaN") (TestCases (const [])) -- no support for `isNaN` in ConCat
  . HInsert1 (Proxy @"IsNegativeZero") (TestCases (const [])) -- no support for `isNegativeZero` in ConCat
  . HInsert1 (Proxy @"Log") (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"NegateDouble") (TestCases (const [((), pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"PlusDouble") (TestCases (const [((), pure ([|(,) <$> genFloating <*> genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"Sin") (TestCases (const [])) -- no support for `sin` in ConCat
  . HInsert1 (Proxy @"Sinh") (TestCases (const [])) -- no support for `sinh` in ConCat
  . HInsert1 (Proxy @"Sqrt") (TestCases (const [])) -- no support for `sqrt` in ConCat
  . HInsert1 (Proxy @"SqrtDouble") (TestCases (const [])) -- no support for `sqrtDouble` in ConCat
  . HInsert1 (Proxy @"Tan") (TestCases (const [])) -- no support for `tan` in ConCat
  . HInsert1 (Proxy @"Tanh") (TestCases (const [])) -- no support for `tanh` in ConCat
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
  . HInsert1 (Proxy @"Compare") (TestCases (const [])) -- no support for `compare` in ConCat
  . HInsert1 (Proxy @"EqDouble") (TestCases (const [((), pure ([|(,) <$> genFloating <*> genFloating|], [|show|]))]))
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
  . HInsert1 (Proxy @"Quot") (TestCases (const [])) -- no support for `quot` in ConCat
  . HInsert1 (Proxy @"RealToFrac") (TestCases (const [])) -- no support for `realToFrac` in ConCat
  . HInsert1 (Proxy @"Recip") (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"Rem") (TestCases (const [])) -- no support for `rem` in ConCat
  . HInsert1
    (Proxy @"Div")
    ( TestCases
        ( const
            [ ( [t|Word8|],
                pure
                  ( [|(,) <$> Gen.enumBounded <*> Gen.integral (Range.linear 1 maxBound)|],
                    [|show|]
                  )
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
                  ( [|(,) <$> Gen.enumBounded <*> Gen.integral (Range.linear 1 maxBound)|],
                    [|show|]
                  )
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
  . HInsert1 (Proxy @"Atan2") (TestCases (const [])) -- no support for `atan2` in ConCat
  . HInsert1 (Proxy @"Abs") (TestCases (const [])) -- no support for `abs` in ConCat
  . HInsert1 (Proxy @"Negate") (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"Signum") (TestCases (const [])) -- no support for `signum` in ConCat
  . HInsert1 (Proxy @"PowI") (TestCases (const [])) -- ConCat only supports `Int` for `^`
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
  . HInsert1 (Proxy @"Append") (TestCases (const [])) -- no support for `<>` in ConCat
  . HInsert1 (Proxy @"Mappend") (TestCases (const [])) -- no support for `mappend` in ConCat
  . HInsert1 (Proxy @"ListAppend") (TestCases (const [])) -- no support for `++` in ConCat
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
  . HInsert1
    (Proxy @"BareFMap")
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
            [ ( [t|Word8|],
                pure ([|Gen.list (Range.exponential 1 1024) Gen.enumBounded|], [|show|])
              )
            ]
        )
    )
  . HInsert1 (Proxy @"Point") (TestCases (const [([t|Word8|], pure ([|Gen.enumBounded|], [|show|]))]))
  . HInsert1 (Proxy @"Ap") (TestCases (const [])) -- no support for `<*>` in ConCat
  . HInsert1 (Proxy @"LiftA2") (TestCases (const [])) -- no support for `liftA2` in ConCat
  . HInsert1 (Proxy @"Bind") (TestCases (const [])) -- no support for `>>=` in ConCat
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
  . HInsert1 (Proxy @"SequenceA") (TestCases (const [])) -- no support for `sequenceA` in ConCat
  . HInsert1 (Proxy @"Traverse") (TestCases (const [])) -- no support for `traverse` in ConCat
  . HInsert1 (Proxy @"UnsafeCoerce") (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"Sum") (TestCases (const [])) -- can only work with specialization
  . HInsert1 (Proxy @"ToList") (TestCases (const [])) -- can only work with specialization
  . HInsert1 (Proxy @"Even") (TestCases (const []))
  . HInsert1 (Proxy @"Odd") (TestCases (const []))
  $ HEmpty1

main :: IO ()
main = bool exitFailure exitSuccess . and =<< allTestTerms
