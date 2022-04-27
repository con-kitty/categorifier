{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
-- To avoid turning @if then else@ into `ifThenElse`.
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- |
-- Template Haskell is used to automate the generation of the same test cases for each category we
-- want to test.  See "Test.TH" for the generation of these cases.
--
-- Many properties are generated for things where, due to the way the plugin works, we doubt a
-- priori that some inputs may fail if any input succeeds (e.g. testing how 'id' is 'categorify'd).
-- They are written as properties anyway to simplify the TH implementation, which supports the
-- general case.
--
-- Things are unfortunately complex in the case of tests that are expected to fail:
--
--     * Hedgehog does not seem to have a way to mark a property as an expected failure.
--
--     * Tasty auto-discovery does not seem to have a way to mark a property as an expected failure.
--
--     * The same is true for tests that must be skipped because the plugin-generated code has some
--       terrifying property like non-termination or segfaulting.
--
--     * There are some tests that we expect not to work, but because the problem is in the plugin
--       itself or in the instances, we cannot even expect the test we would like to someday pass to
--       compile.
--
-- Not being able to mark expected failures means that we can't even generate TH splices for tests
-- that don't pass.  All this combined means that you should carefully read the comments in the TH
-- module to determine the status of anything that's not currently being tested and passing.
module Main
  ( main,
  )
where

import Categorifier.Hedgehog (genFloating)
import qualified Categorifier.Test.Adjunctions as Adjunctions
import Categorifier.Test.ConCat.Instances (Hask (..), Term)
import Categorifier.Test.Data (One (..), Pair (..))
import Categorifier.Test.HList (HMap1 (..))
import qualified Categorifier.Test.HList as HList
import Categorifier.Test.Tests
  ( TestCases (..),
    TestCategory (..),
    TestStrategy (..),
    builtinTestCategories,
    defaultTestTerms,
    mkTestTerms,
  )
import Categorifier.Test.TotOrd (TotOrd, runTotOrd)
import ConCat.Circuit ((:>))
import ConCat.Syntactic (Syn)
import Control.Arrow (Arrow (..), ArrowChoice (..))
import Data.Bool (bool)
import Data.Functor.Identity (Identity (..))
import Data.Proxy (Proxy (..))
import Data.Semigroup (Product (..), Sum (..))
import GHC.Int (Int16, Int32, Int64, Int8)
import GHC.Word (Word16, Word32, Word64, Word8)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import System.Exit (exitFailure, exitSuccess)

-- For @NoRebindableSyntax@
{-# ANN module ("HLint: ignore Avoid restricted integration" :: String) #-}

mkTestTerms
  (HList.appendMap defaultTestTerms Adjunctions.testTerms)
  --               name     type         prefix       strategy
  ( [ TestCategory ''Term [t|Term|] "term" CheckCompileOnly,
      TestCategory ''Hask [t|Hask|] "hask" (ComputeFromInput [|runHask|]),
      TestCategory ''TotOrd [t|TotOrd|] "totOrd" (ComputeFromInput [|runTotOrd|]),
      TestCategory ''(:>) [t|(:>)|] "circuit" CheckCompileOnly,
      TestCategory ''Syn [t|Syn|] "syn" CheckCompileOnly
    ]
      <> builtinTestCategories
  )
  -- core
  . HInsert1 (Proxy @"LamId") (TestCases (const [([t|Word8|], pure ([|Gen.enumBounded|], [|show|]))]))
  . HInsert1 (Proxy @"ComposeLam") (TestCases (const [([t|Word8|], pure ([|Gen.enumBounded|], [|show|]))]))
  . HInsert1
    (Proxy @"ConstLam")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else
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
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- no ClosedCat
              else
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
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else
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
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else
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
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [(([t|Int64|], [t|Word8|]), pure ([|Gen.enumBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"Join")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else
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
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else
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
  . HInsert1
    (Proxy @"ComposedCoerce")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [([t|Word8|], pure ([|Gen.enumBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"Bool")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else
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
  . HInsert1
    (Proxy @"PlusDouble")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genFloating <*> genFloating|], [|show|]))]
        )
    )
  . HInsert1 (Proxy @"Sin") (TestCases (const [])) -- no support for `sin` in ConCat
  . HInsert1 (Proxy @"Sinh") (TestCases (const [])) -- no support for `sinh` in ConCat
  . HInsert1 (Proxy @"Sqrt") (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"SqrtDouble") (TestCases (const [((), pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"Tan") (TestCases (const [])) -- no support for `tan` in ConCat
  . HInsert1 (Proxy @"Tanh") (TestCases (const [])) -- no support for `tanh` in ConCat
  . HInsert1
    (Proxy @"TimesDouble")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] --
              else [((), pure ([|(,) <$> genFloating <*> genFloating|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"And")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"Or")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"Equal")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [([t|Int64|], pure ([|(,) <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"NotEqual")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [([t|Int64|], pure ([|(,) <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"Ge")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [([t|Int64|], pure ([|(,) <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"Gt")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [([t|Int64|], pure ([|(,) <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"Le")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [([t|Int64|], pure ([|(,) <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"Lt")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [([t|Int64|], pure ([|(,) <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|]))]
        )
    )
  . HInsert1 (Proxy @"Compare") (TestCases (const [])) -- no support for `compare` in ConCat
  . HInsert1
    (Proxy @"EqDouble")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genFloating <*> genFloating|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"Max")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [([t|Double|], pure ([|(,) <$> genFloating <*> genFloating|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"Min")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [([t|Double|], pure ([|(,) <$> genFloating <*> genFloating|], [|show|]))]
        )
    )
  . HInsert1 (Proxy @"Not") (TestCases (const [((), pure ([|Gen.enumBounded|], [|show|]))]))
  . HInsert1
    (Proxy @"Plus")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [([t|Double|], pure ([|(,) <$> genFloating <*> genFloating|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"Minus")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [([t|Double|], pure ([|(,) <$> genFloating <*> genFloating|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"Times")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [([t|Double|], pure ([|(,) <$> genFloating <*> genFloating|], [|show|]))]
        )
    )
  . HInsert1 (Proxy @"Quot") (TestCases (const [])) -- no support for `quot` in ConCat
  . HInsert1 (Proxy @"RealToFrac") (TestCases (const [])) -- no support for `realToFrac` in ConCat
  . HInsert1 (Proxy @"Recip") (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"Rem") (TestCases (const [])) -- no support for `rem` in ConCat
  . HInsert1
    (Proxy @"Div")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else
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
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else
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
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [([t|Double|], pure ([|(,) <$> genFloating <*> genFloating|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"EqWord8")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"NeWord8")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|]))]
        )
    )
  . HInsert1 (Proxy @"Atan2") (TestCases (const [])) -- no support for `atan2` in ConCat
  . HInsert1 (Proxy @"Abs") (TestCases (const [])) -- no support for `abs` in ConCat
  . HInsert1 (Proxy @"Negate") (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"Signum") (TestCases (const [])) -- no support for `signum` in ConCat
  . HInsert1 (Proxy @"PowI") (TestCases (const [])) -- ConCat only supports `Int` for `^`
  . HInsert1
    (Proxy @"PowInt")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then []
              else [([t|Double|], pure ([|genFloating|], [|show|]))]
        )
    )
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
        -- __FIXME__: This works for ConCat_function, but not ConCat_class.
        -- ( \arrow ->
        --     if arrow `elem` [''(->), ''Hask, ''TotOrd]
        --       then [] -- we expect bottomC to raise an exception in these categories
        --       else
        --         [ ( [t|Word8|],
        --             pure ([|Gen.string Range.linearBounded Gen.unicodeAll|], [|show|])
        --           )
        --         ]
        -- )
        (const [])
    )
  . HInsert1 (Proxy @"BuildLeft") (TestCases (const [(([t|Int64|], [t|Word8|]), pure ([|Gen.enumBounded|], [|show|]))]))
  . HInsert1 (Proxy @"BuildRight") (TestCases (const [(([t|Int64|], [t|Word8|]), pure ([|Gen.enumBounded|], [|show|]))]))
  . HInsert1
    (Proxy @"EliminateEither")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd -- #19
              then []
              else
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
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else
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
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- no ClosedCat
              else
                [ ( ([t|Word8|], [t|Bool|]),
                    pure ([|(,) <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|])
                  )
                ]
        )
    )
  . HInsert1
    (Proxy @"BareFMap")
    ( TestCases
        ( \arrow ->
            if arrow
              `elem` [ ''Syn, -- no Strong
                       ''TotOrd -- no ClosedCat
                     ]
              then []
              else
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
        ( \arrow ->
            if arrow
              `elem` [ ''Syn, -- no Strong
                       ''TotOrd -- #19
                     ]
              then []
              else [([t|Word8|], pure ([|Pair <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"Fmap")
    ( TestCases
        ( \arrow ->
            if arrow
              `elem` [ ''Syn, -- no Strong
                       ''TotOrd -- #19
                     ]
              then []
              else
                [ ( ([t|Pair|], [t|Word8|]),
                    pure ([|Pair <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|])
                  )
                ]
        )
    )
  . HInsert1
    (Proxy @"Fmap'")
    ( TestCases
        ( \arrow ->
            if arrow
              `elem` [ ''Syn, -- no Strong
                       ''TotOrd -- #19
                     ]
              then []
              else [([t|Word8|], pure ([|Pair <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"ConstNot")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- no ClosedCat
              else [([t|Word8|], pure ([|(,) <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"MapList")
    ( TestCases
        ( \arrow ->
            if arrow
              `elem` [ ''Syn, -- no Strong
                       ''TotOrd -- #19
                     ]
              then []
              else
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
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- no ClosedCat
              else
                [ ( ([t|Word8|], [t|Bool|]),
                    pure ([|(,) <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|])
                  )
                ]
        )
    )
  . HInsert1
    (Proxy @"Uncurry")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- no ClosedCat
              else
                [ ( ([t|Word8|], [t|Bool|]),
                    pure ([|(,) <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|])
                  )
                ]
        )
    )
  . HInsert1
    (Proxy @"SequenceA")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- no ClosedCat
              else
                [ ( ([t|Sum|], [t|Product|], [t|Word8|]),
                    pure ([|Sum . Product <$> Gen.enumBounded|], [|show|])
                  )
                ]
        )
    )
  . HInsert1 (Proxy @"Traverse") (TestCases (const []))
  . HInsert1 (Proxy @"UnsafeCoerce") (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"Sum") (TestCases (const [])) -- can only work with specialization
  . HInsert1 (Proxy @"ToList") (TestCases (const [])) -- can only work with specialization
  . HInsert1 (Proxy @"Even") (TestCases (const []))
  . HInsert1 (Proxy @"Odd") (TestCases (const []))
  -- adjunctions
  . HInsert1 (Proxy @"PureRep") (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1
    (Proxy @"FmapRep")
    ( TestCases
        ( \arrow ->
            if arrow
              `elem` [ ''Syn, -- no Strong
                       ''TotOrd -- #19
                     ]
              then []
              else [([t|Word8|], pure ([|Gen.enumBounded|], [|show|]))]
        )
    )
  . HInsert1 (Proxy @"ApRep") (TestCases (const [])) -- no support for `apRep` in ConCat
  . HInsert1 (Proxy @"BindRep") (TestCases (const [])) -- no support for `bindRep` in ConCat
  . HInsert1
    (Proxy @"Index")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- no ClosedCat
              else
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
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- no ClosedCat
              else
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
