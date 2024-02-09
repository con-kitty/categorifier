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

import Categorifier.ConCat.Examples.Circuit ((:>))
import Categorifier.ConCat.Examples.Syntactic (Syn)
import Categorifier.Hedgehog (genFloating, genIntegralBounded)
import qualified Categorifier.Test.Adjunctions as Adjunctions
import Categorifier.Test.ConCat.Instances (Hask (..), Term)
import Categorifier.Test.Data (One (..), Pair (..))
import Categorifier.Test.HList (HMap1 (..))
import qualified Categorifier.Test.HList as HList
import Categorifier.Test.Tests
  ( TestCases (..),
    TestCategory (..),
    TestStrategy (..),
    defaultTestTerms,
    mkTestTerms,
  )
import Categorifier.Test.TotOrd (TotOrd, runTotOrd)
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
  (HList.appendMap Adjunctions.testTerms defaultTestTerms)
  --             name   type      prefix  strategy
  [ TestCategory ''Term [t|Term|] "term" CheckCompileOnly,
    TestCategory ''(->) [t|(->)|] "plainArrow" $ ComputeFromInput [|id|],
    TestCategory ''Hask [t|Hask|] "hask" (ComputeFromInput [|runHask|]),
    TestCategory ''TotOrd [t|TotOrd|] "totOrd" (ComputeFromInput [|runTotOrd|]),
    TestCategory ''(:>) [t|(:>)|] "circuit" CheckCompileOnly,
    TestCategory ''Syn [t|Syn|] "syn" CheckCompileOnly
  ]
  -- adjunctions
  . HInsert1 (Proxy @"PureRep") (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1
    (Proxy @"FmapRep")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd -- #19
              then []
              else [([t|Word8|], pure ([|genIntegralBounded|], [|show|]))]
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
                    pure ([|(,) <$> genIntegralBounded <*> pure ()|], [|show|])
                  ),
                  ( ([t|One|], [t|Word8|]),
                    pure ([|(,) <$> (One <$> genIntegralBounded) <*> pure ()|], [|show|])
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
                    pure ([|const <$> genIntegralBounded|], [|("\\() -> " <>) . show . ($ ())|])
                  ),
                  ( ([t|One|], [t|Word8|]),
                    pure ([|const <$> genIntegralBounded|], [|("\\() -> " <>) . show . ($ ())|])
                  )
                ]
        )
    )
  -- core
  . HInsert1 (Proxy @"LamId") (TestCases (const [([t|Word8|], pure ([|genIntegralBounded|], [|show|]))]))
  . HInsert1 (Proxy @"ComposeLam") (TestCases (const [([t|Word8|], pure ([|genIntegralBounded|], [|show|]))]))
  . HInsert1
    (Proxy @"ConstLam")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else
                [ ( ([t|Int64|], [t|Word8|]),
                    pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|])
                  )
                ]
        )
    )
  . HInsert1 (Proxy @"ReturnLam") (TestCases (const [([t|Word8|], pure ([|genIntegralBounded|], [|show|]))]))
  . HInsert1 (Proxy @"BuildTuple") (TestCases (const [([t|Word8|], pure ([|genIntegralBounded|], [|show|]))]))
  . HInsert1
    (Proxy @"EliminateTupleFst")
    ( TestCases
        (const [([t|Word8|], pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))])
    )
  . HInsert1
    (Proxy @"EliminateTupleSnd")
    ( TestCases
        (const [([t|Word8|], pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))])
    )
  . HInsert1
    (Proxy @"EliminateNestedTuples")
    ( TestCases
        ( const
            [ ( [t|Word8|],
                pure
                  ( [|(,) <$> genIntegralBounded <*> ((,) <$> genIntegralBounded <*> genIntegralBounded)|],
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
                      ([|Gen.choice [const <$> genIntegralBounded, pure id]|], [|const "<function>"|])
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
                  ([|(,) <$> Gen.bool <*> ((,) <$> genIntegralBounded <*> genIntegralBounded)|], [|show|])
              )
            ]
        )
    )
  -- plugin
  . HInsert1
    (Proxy @"Abst")
    ( TestCases
        ( const
            [([t|Word8|], pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"Repr")
    ( TestCases
        (const [([t|Word8|], pure ([|Pair <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))])
    )
  -- base
  . HInsert1 (Proxy @"Id") (TestCases (const [([t|Word8|], pure ([|genIntegralBounded|], [|show|]))]))
  . HInsert1
    (Proxy @"Const")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else
                [ ( ([t|Int64|], [t|Word8|]),
                    pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|])
                  )
                ]
        )
    )
  . HInsert1
    (Proxy @"Snd")
    ( TestCases
        ( const
            [ ( ([t|Word8|], [t|Word8|]),
                pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|])
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
                          (,) <$> genIntegralBounded <*> ((,) <$> genIntegralBounded <*> genIntegralBounded)
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
                  ( [|(,) <$> genIntegralBounded <*> ((,) <$> genIntegralBounded <*> genIntegralBounded)|],
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
                pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|])
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
              else [(([t|Int64|], [t|Word8|]), pure ([|genIntegralBounded|], [|show|]))]
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
                      ( [|Gen.choice [Left <$> genIntegralBounded, Right <$> genIntegralBounded]|],
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
                      ( [|Gen.choice [Left <$> genIntegralBounded, Right <$> genIntegralBounded]|],
                        [|show|]
                      )
                  )
                ]
        )
    )
  . HInsert1 (Proxy @"Coerce") (TestCases (const [([t|Word8|], pure ([|genIntegralBounded|], [|show|]))]))
  . HInsert1
    (Proxy @"ComposedCoerce")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [([t|Word8|], pure ([|genIntegralBounded|], [|show|]))]
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
                      ( [|(,,) <$> genIntegralBounded <*> genIntegralBounded <*> Gen.bool|],
                        [|show|]
                      )
                  ),
                  ( [t|Word16|],
                    pure
                      ( [|(,,) <$> genIntegralBounded <*> genIntegralBounded <*> Gen.bool|],
                        [|show|]
                      )
                  ),
                  ( [t|Word32|],
                    pure
                      ( [|(,,) <$> genIntegralBounded <*> genIntegralBounded <*> Gen.bool|],
                        [|show|]
                      )
                  ),
                  ( [t|Word64|],
                    pure
                      ( [|
                          (,,)
                            <$> genIntegralBounded
                            <*> genIntegralBounded
                            <*> Gen.bool
                          |],
                        [|show|]
                      )
                  ),
                  ( [t|Int8|],
                    pure ([|(,,) <$> genIntegralBounded <*> genIntegralBounded <*> Gen.bool|], [|show|])
                  ),
                  ( [t|Int16|],
                    pure ([|(,,) <$> genIntegralBounded <*> genIntegralBounded <*> Gen.bool|], [|show|])
                  ),
                  ( [t|Int32|],
                    pure ([|(,,) <$> genIntegralBounded <*> genIntegralBounded <*> Gen.bool|], [|show|])
                  ),
                  ( [t|Int64|],
                    pure ([|(,,) <$> genIntegralBounded <*> genIntegralBounded <*> Gen.bool|], [|show|])
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
  . HInsert1 (Proxy @"Acos") (TestCases (const [])) -- no support for `acos` in ConCat
  . HInsert1 (Proxy @"Acosh") (TestCases (const [])) -- no support for `acosh` in ConCat
  . HInsert1 (Proxy @"AcoshDouble") (TestCases (const [])) -- no support for `acoshDouble` in ConCat
  . HInsert1 (Proxy @"AcoshFloat") (TestCases (const [])) -- no support for `acoshFloat` in ConCat
  . HInsert1 (Proxy @"Asin") (TestCases (const [])) -- no support for `asin` in ConCat
  . HInsert1 (Proxy @"Asinh") (TestCases (const [])) -- no support for `asinh` in ConCat
  . HInsert1 (Proxy @"AsinhDouble") (TestCases (const [])) -- no support for `asinhDouble` in ConCat
  . HInsert1 (Proxy @"AsinhFloat") (TestCases (const [])) -- no support for `asinhFloat` in ConCat
  . HInsert1 (Proxy @"Atan") (TestCases (const [])) -- no support for `atan` in ConCat
  . HInsert1 (Proxy @"Atanh") (TestCases (const [])) -- no support for `atanh` in ConCat
  . HInsert1 (Proxy @"AtanhDouble") (TestCases (const [])) -- no support for `atanhDouble` in ConCat
  . HInsert1 (Proxy @"AtanhFloat") (TestCases (const [])) -- no support for `atanhFloat` in ConCat
  . HInsert1 (Proxy @"Cos") (TestCases (const [])) -- no support for `cos` in ConCat
  . HInsert1 (Proxy @"Cosh") (TestCases (const [])) -- no support for `cosh` in ConCat
  . HInsert1 (Proxy @"AcosDouble") (TestCases (const [])) -- no support for `acosDouble` in ConCat
  . HInsert1 (Proxy @"AsinDouble") (TestCases (const [])) -- no support for `asinDouble` in ConCat
  . HInsert1 (Proxy @"AtanDouble") (TestCases (const [])) -- no support for `atanDouble` in ConCat
  . HInsert1 (Proxy @"CosDouble") (TestCases (const [])) -- no support for `cosDouble` in ConCat
  . HInsert1 (Proxy @"CoshDouble") (TestCases (const [])) -- no support for `coshDouble` in ConCat
  . HInsert1 (Proxy @"AcosFloat") (TestCases (const [])) -- no support for `acosFloat` in ConCat
  . HInsert1 (Proxy @"AsinFloat") (TestCases (const [])) -- no support for `asinFloat` in ConCat
  . HInsert1 (Proxy @"AtanFloat") (TestCases (const [])) -- no support for `atanFloat` in ConCat
  . HInsert1 (Proxy @"CosFloat") (TestCases (const [])) -- no support for `cosFloat` in ConCat
  . HInsert1 (Proxy @"CoshFloat") (TestCases (const [])) -- no support for `coshFloat` in ConCat
  . HInsert1 (Proxy @"Double2Float") (TestCases (const [])) -- no support for `double2Float` in ConCat
  . HInsert1 (Proxy @"Exp") (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"Float2Double") (TestCases (const [])) -- no support for `float2Double` in ConCat
  . HInsert1 (Proxy @"IsDenormalized") (TestCases (const [])) -- no support for `isDenormalized` in ConCat
  . HInsert1 (Proxy @"IsInfinite") (TestCases (const [])) -- no support for `isInfinite` in ConCat
  . HInsert1 (Proxy @"IsNaN") (TestCases (const [])) -- no support for `isNaN` in ConCat
  . HInsert1 (Proxy @"IsNegativeZero") (TestCases (const [])) -- no support for `isNegativeZero` in ConCat
  . HInsert1 (Proxy @"Log") (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"LogDouble") (TestCases (const [((), pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"LogFloat") (TestCases (const [((), pure ([|genFloating|], [|show|]))]))
  . HInsert1
    (Proxy @"MinusDouble")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genFloating <*> genFloating|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"MinusFloat")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genFloating <*> genFloating|], [|show|]))]
        )
    )
  . HInsert1 (Proxy @"NegateDouble") (TestCases (const [((), pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"NegateFloat") (TestCases (const [((), pure ([|genFloating|], [|show|]))]))
  . HInsert1
    (Proxy @"PlusDouble")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genFloating <*> genFloating|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"PlusFloat")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genFloating <*> genFloating|], [|show|]))]
        )
    )
  . HInsert1 (Proxy @"Power") (TestCases (const [])) -- no support for `**` in ConCat
  . HInsert1 (Proxy @"PowerDouble") (TestCases (const [])) -- no support for `**` in ConCat
  . HInsert1 (Proxy @"PowerFloat") (TestCases (const [])) -- no support for `**` in ConCat
  . HInsert1 (Proxy @"Sin") (TestCases (const [])) -- no support for `sin` in ConCat
  . HInsert1 (Proxy @"Sinh") (TestCases (const [])) -- no support for `sinh` in ConCat
  . HInsert1 (Proxy @"SinDouble") (TestCases (const [])) -- no support for `sinDouble` in ConCat
  . HInsert1 (Proxy @"SinhDouble") (TestCases (const [])) -- no support for `sinhDouble` in ConCat
  . HInsert1 (Proxy @"SinFloat") (TestCases (const [])) -- no support for `sinFloat` in ConCat
  . HInsert1 (Proxy @"SinhFloat") (TestCases (const [])) -- no support for `sinhFloat` in ConCat
  . HInsert1 (Proxy @"Sqrt") (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"SqrtDouble") (TestCases (const [((), pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"SqrtFloat") (TestCases (const [((), pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"Tan") (TestCases (const [])) -- no support for `tan` in ConCat
  . HInsert1 (Proxy @"Tanh") (TestCases (const [])) -- no support for `tanh` in ConCat
  . HInsert1 (Proxy @"TanDouble") (TestCases (const [])) -- no support for `tanDouble` in ConCat
  . HInsert1 (Proxy @"TanhDouble") (TestCases (const [])) -- no support for `tanhDouble` in ConCat
  . HInsert1 (Proxy @"TanFloat") (TestCases (const [])) -- no support for `tanFloat` in ConCat
  . HInsert1 (Proxy @"TanhFloat") (TestCases (const [])) -- no support for `tanhFloat` in ConCat
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
    (Proxy @"TimesFloat")
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
              else [((), pure ([|(,) <$> Gen.bool <*> Gen.bool|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"Or")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> Gen.bool <*> Gen.bool|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"Equal")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [([t|Int64|], pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"NotEqual")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [([t|Int64|], pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"Ge")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [([t|Int64|], pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"Gt")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [([t|Int64|], pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"Le")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [([t|Int64|], pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"Lt")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [([t|Int64|], pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"EqualDouble")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genFloating <*> genFloating|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"GeDouble")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genFloating <*> genFloating|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"GtDouble")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genFloating <*> genFloating|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"LeDouble")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genFloating <*> genFloating|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"LtDouble")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genFloating <*> genFloating|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"EqualFloat")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genFloating <*> genFloating|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"GeFloat")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genFloating <*> genFloating|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"GtFloat")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genFloating <*> genFloating|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"LeFloat")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genFloating <*> genFloating|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"LtFloat")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genFloating <*> genFloating|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"EqualInt")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"NotEqualInt")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"GeInt")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"GtInt")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"LeInt")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"LtInt")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"EqualInt16")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"NotEqualInt16")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"GeInt16")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"GtInt16")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"LeInt16")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"LtInt16")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"EqualInt32")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"NotEqualInt32")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"GeInt32")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"GtInt32")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"LeInt32")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"LtInt32")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"EqualInt64")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"NotEqualInt64")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"GeInt64")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"GtInt64")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"LeInt64")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"LtInt64")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"EqualInt8")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"NotEqualInt8")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"GeInt8")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"GtInt8")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"LeInt8")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"LtInt8")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"EqualWord")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"NotEqualWord")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"GeWord")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"GtWord")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"LeWord")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"LtWord")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"EqualWord16")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"NotEqualWord16")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"GeWord16")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"GtWord16")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"LeWord16")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"LtWord16")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"EqualWord32")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"NotEqualWord32")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"GeWord32")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"GtWord32")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"LeWord32")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"LtWord32")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"EqualWord64")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"NotEqualWord64")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"GeWord64")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"GtWord64")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"LeWord64")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"LtWord64")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"EqualWord8")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"NotEqualWord8")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"GeWord8")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"GtWord8")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"LeWord8")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"LtWord8")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]
        )
    )
  . HInsert1 (Proxy @"Compare") (TestCases (const [])) -- no support for `compare` in ConCat
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
  . HInsert1 (Proxy @"Not") (TestCases (const [((), pure ([|Gen.bool|], [|show|]))]))
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
                      ( [|(,) <$> genIntegralBounded <*> Gen.integral (Range.linear 1 maxBound)|],
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
                      ( [|(,) <$> genIntegralBounded <*> Gen.integral (Range.linear 1 maxBound)|],
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
    (Proxy @"DivideDouble")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genFloating <*> genFloating|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"DivideFloat")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- #19
              else [((), pure ([|(,) <$> genFloating <*> genFloating|], [|show|]))]
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
  . HInsert1
    (Proxy @"Pure")
    ( TestCases
        ( const
            [ ([t|Double|], pure ([|genFloating|], [|show|])),
              ([t|Word8|], pure ([|genIntegralBounded|], [|show|]))
            ]
        )
    )
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
  . HInsert1 (Proxy @"BuildLeft") (TestCases (const [(([t|Int64|], [t|Word8|]), pure ([|genIntegralBounded|], [|show|]))]))
  . HInsert1 (Proxy @"BuildRight") (TestCases (const [(([t|Int64|], [t|Word8|]), pure ([|genIntegralBounded|], [|show|]))]))
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
                            x :: Word8 <- genIntegralBounded
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
                            x :: Word8 <- genIntegralBounded
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
                    pure ([|(,) <$> genIntegralBounded <*> Gen.bool|], [|show|])
                  )
                ]
        )
    )
  . HInsert1
    (Proxy @"BareFMap")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd -- no ClosedCat
              then []
              else
                [ ( [t|Word8|],
                    pure
                      ( [|
                          (,)
                            <$> Gen.element [const 42, id]
                            <*> (Pair <$> genIntegralBounded <*> genIntegralBounded)
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
            if arrow == ''TotOrd -- #19
              then []
              else [([t|Word8|], pure ([|Pair <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"Fmap")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd -- #19
              then []
              else
                [ ( ([t|Pair|], [t|Word8|]),
                    pure ([|Pair <$> genIntegralBounded <*> genIntegralBounded|], [|show|])
                  )
                ]
        )
    )
  . HInsert1
    (Proxy @"Fmap'")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd -- #19
              then []
              else [([t|Word8|], pure ([|Pair <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"ConstNot")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd
              then [] -- no ClosedCat
              else [([t|Word8|], pure ([|(,) <$> genIntegralBounded <*> Gen.bool|], [|show|]))]
        )
    )
  . HInsert1
    (Proxy @"MapList")
    ( TestCases
        ( \arrow ->
            if arrow == ''TotOrd -- #19
              then []
              else
                [ ( [t|Word8|],
                    pure ([|Gen.list (Range.exponential 1 1024) genIntegralBounded|], [|show|])
                  )
                ]
        )
    )
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
                    pure ([|(,) <$> genIntegralBounded <*> Gen.bool|], [|show|])
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
                    pure ([|(,) <$> genIntegralBounded <*> Gen.bool|], [|show|])
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
                    pure ([|Sum . Product <$> genIntegralBounded|], [|show|])
                  )
                ]
        )
    )
  . HInsert1 (Proxy @"Traverse") (TestCases (const []))
  . HInsert1 (Proxy @"UnsafeCoerce") (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"Sum") (TestCases (const [])) -- can only work with specialization
  . HInsert1 (Proxy @"SumList") (TestCases (const []))
  . HInsert1 (Proxy @"ToList") (TestCases (const [])) -- can only work with specialization
  . HInsert1 (Proxy @"Even") (TestCases (const []))
  . HInsert1 (Proxy @"Odd") (TestCases (const []))
  $ HEmpty1

main :: IO ()
main = bool exitFailure exitSuccess . and =<< allTestTerms
