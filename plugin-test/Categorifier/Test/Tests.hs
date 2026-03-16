{-# LANGUAGE DataKinds #-}
-- To avoid having to specify massive HList types.
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
-- To avoid having to specify massive HList types.
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

-- |
-- This module contains TH that generates terms to be invoked with the
-- 'Categorifier.Categorify.expression' plugin-runner. This is done so that new arrows can be tested
-- easily.
--
-- The HLint warnings for 'id' and 'const' are disabled because we want to test how the plugin
-- handles exactly what's written.
module Categorifier.Test.Tests
  ( TestTerms,
    insertTest,
    defaultTestTerms,
    coreTestTerms,
    pluginTestTerms,
    baseTestTerms,
    mkTestTerms,
    zerosafeUnsignedPrimitiveCases,
    noCategoricalRepresentation,
    unableToInline,
    TestCases (..),
    TestCategory (..),
    TestStrategy (..),
  )
where

import Categorifier.Client (Rep)
import Categorifier.Core.Functions (abst, repr)
import Categorifier.Test.Data (Pair (..))
import Categorifier.Test.HList (HMap1 (..))
import qualified Categorifier.Test.HList as HList
import Categorifier.Test.TH
  ( ExprTest,
    TestCases (..),
    TestCategory (..),
    TestConfig,
    TestStrategy (..),
    mkBinaryTestConfig,
    mkExprTest,
    mkTernaryTestConfig,
    mkTestTerms,
    mkUnaryTestConfig,
  )
import Control.Arrow (arr)
import Data.Coerce (coerce)
import qualified Data.Foldable
import Data.Functor.Identity (Identity (..))
import qualified Data.List
import Data.Proxy (Proxy (..))
import Data.Semigroup (Sum (..))
import Data.Tuple (swap)
import qualified GHC.Float
import GHC.Int (Int16, Int32, Int64, Int8)
import qualified GHC.Int
import GHC.TypeLits (KnownSymbol, symbolVal)
import GHC.Word (Word16, Word32, Word64, Word8)
import qualified GHC.Word
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Language.Haskell.TH (Exp, Q, Type)
import Unsafe.Coerce (unsafeCoerce)

-- For @PartialTypeSignatures@
{-# ANN module "HLint: ignore Avoid restricted extensions" #-}

-- For `Unsafe.Coerce`
{-# ANN module "HLint: ignore Avoid restricted module" #-}

noCategoricalRepresentation :: String -> Either String (Q Exp, Q Exp)
noCategoricalRepresentation operation =
  Left $
    "There is no categorical representation defined for `" <> operation <> "` when using the"

unableToInline :: String -> Either String (Q Exp, Q Exp)
unableToInline operation = Left $ "The Categorifier plugin was unable to inline " <> operation

-- * property sets

--   Combinations of property generators that are commonly desired when dealing with `C.Cat`.

zerosafeUnsignedPrimitiveCases :: [(Q Type, Either String (Q Exp, Q Exp))]
zerosafeUnsignedPrimitiveCases =
  [ ( [t|Word16|],
      pure ([|(,) <$> genIntegralBounded <*> Gen.integral (Range.linear 1 maxBound)|], [|show|])
    ),
    ( [t|Word32|],
      pure ([|(,) <$> genIntegralBounded <*> Gen.integral (Range.linear 1 maxBound)|], [|show|])
    ),
    ( [t|Word64|],
      pure
        ( [|(,) <$> genIntegralBounded <*> Gen.integral (Range.linear 1 maxBound)|],
          [|show|]
        )
    ),
    ( [t|Word8|],
      pure ([|(,) <$> genIntegralBounded <*> Gen.integral (Range.linear 1 maxBound)|], [|show|])
    )
  ]

-- | A helper to avoid duplicating the key when inserting a new test.
insertTest ::
  (KnownSymbol k) =>
  Proxy k ->
  (String -> TestCategory -> TestConfig) ->
  (a -> (Q Type, Q Type)) ->
  Q Exp ->
  HMap1 ExprTest l ->
  HMap1 ExprTest ('(k, a) ': l)
insertTest key config ty = HInsert1 key . mkExprTest (config $ symbolVal key) ty

-- | A list of type-parameterized expressions to test.
--
--   There should be a `TestTerms` value corresponding to each
--  `Categorifier.Core.MakerMap.MakerMapFun` value (plus `coreTestTerms` to cover all of the GHC
--   Core cases).
--
--  __NB__: Use of this type should take advantage of @PartialTypeSignatures@ to avoid having to
--          specify massive HList types (e.g., @`TestTerms` _@).
type TestTerms = HMap1 ExprTest

defaultTestTerms :: TestTerms _
defaultTestTerms = HList.appendMap coreTestTerms $ HList.appendMap pluginTestTerms baseTestTerms

{-# ANN coreTestTerms "HLint: ignore Collapse lambdas" #-}
{-# ANN coreTestTerms "HLint: ignore Use const" #-}
{-# ANN coreTestTerms "HLint: ignore Use fst" #-}
{-# ANN coreTestTerms "HLint: ignore Use id" #-}
{-# ANN coreTestTerms "HLint: ignore Use snd" #-}
coreTestTerms :: TestTerms _
coreTestTerms =
  insertTest (Proxy @"LamId") mkUnaryTestConfig (\a -> (a, a)) [|(\x -> x)|]
    . insertTest
      (Proxy @"ComposeLam")
      mkUnaryTestConfig
      (\a -> (a, a))
      [|((\f g x -> f (g x)) (\x -> x) (\x -> x))|]
    . insertTest
      (Proxy @"ConstLam")
      mkBinaryTestConfig
      (\(a, b) -> (a, [t|$b -> $a|]))
      [|(\x -> \_ -> x)|]
    . insertTest (Proxy @"ReturnLam") mkUnaryTestConfig (\a -> (a, a)) [|\y -> (\x -> \_ -> x) y y|]
    . insertTest (Proxy @"BuildTuple") mkUnaryTestConfig (\a -> (a, [t|($a, $a)|])) [|\x -> (x, x)|]
    . insertTest
      (Proxy @"EliminateTupleFst")
      mkUnaryTestConfig
      (\a -> ([t|($a, $a)|], a))
      [|\(x, _) -> x|]
    . insertTest
      (Proxy @"EliminateTupleSnd")
      mkUnaryTestConfig
      (\a -> ([t|($a, $a)|], a))
      [|\(_, x) -> x|]
    . insertTest
      (Proxy @"EliminateNestedTuples")
      mkUnaryTestConfig
      (\a -> ([t|($a, ($a, $a))|], a))
      [|\(_, (x, _)) -> x|]
    . insertTest
      (Proxy @"LocalFixedPoint")
      mkUnaryTestConfig
      (\a -> (a, a))
      [|let go x = if x > 0 then go (x - 1) else x in go|]
    . insertTest (Proxy @"ApplyArg") mkUnaryTestConfig (\a -> ([t|($a -> $a)|], a)) [|\f -> f 7|]
    . insertTest
      (Proxy @"If")
      mkUnaryTestConfig
      (\a -> ([t|(Bool, ($a, $a))|], a))
      [|\(cond, (tru, fls)) -> if cond then tru else fls|]
    $ HEmpty1

pluginTestTerms :: TestTerms _
pluginTestTerms =
  insertTest (Proxy @"Abst") mkUnaryTestConfig (\a -> ([t|Rep (Pair $a)|], [t|Pair $a|])) [|abst|]
    . insertTest
      (Proxy @"Repr")
      mkUnaryTestConfig
      (\a -> ([t|Pair $a|], [t|Rep (Pair $a)|]))
      [|repr|]
    $ HEmpty1

{-# ANN baseTestTerms "HLint: ignore Avoid lambda" #-}
{-# ANN baseTestTerms "HLint: ignore Redundant uncurry" #-}
{-# ANN baseTestTerms "HLint: ignore Traversable law" #-}
{-# ANN baseTestTerms "HLint: ignore Use <>" #-}
{-# ANN baseTestTerms "HLint: ignore Use String" #-}
{-# ANN baseTestTerms "HLint: ignore Use const" #-}
{-# ANN baseTestTerms "HLint: ignore Use fmap" #-}
{-# ANN baseTestTerms "HLint: ignore Use fromRight" #-}
{-# ANN baseTestTerms "HLint: ignore Use pure" #-}
{-# ANN baseTestTerms "HLint: ignore Use tuple-section" #-}

-- | These expressions roughly map to things that are directly handled in the plugin. Most of them
--   come from functions we interpret, but others are introduced by particular Core expressions.
--
--  __TODO__: Split this into @builtinTestTerms@ (for expressions handled directly by the
--           `Categorifier.Core.Categorify.categorify` logic) and @defaultTestTerms@ (for
--            expressions handled by the included @findMaker@ map, which may be omitted by plugin
--            users).
baseTestTerms :: TestTerms _
baseTestTerms =
  insertTest (Proxy @"Id") mkUnaryTestConfig (\a -> (a, a)) [|id|]
    . insertTest (Proxy @"Const") mkBinaryTestConfig (\(a, b) -> (a, [t|$b -> $a|])) [|const|]
    . insertTest (Proxy @"Snd") mkUnaryTestConfig (\(a, b) -> ([t|($a, $b)|], b)) [|snd|]
    . insertTest
      (Proxy @"FstSnd")
      mkUnaryTestConfig
      (\(a, b, c) -> ([t|($a, ($b, $c))|], b))
      [|fst . snd|]
    . insertTest
      (Proxy @"FstLet")
      mkUnaryTestConfig
      (\(a, b, c) -> ([t|($a, ($b, $c))|], b))
      [|\x -> let y = snd x in fst y|]
    . insertTest
      (Proxy @"Swap")
      mkUnaryTestConfig
      (\(a, b) -> ([t|($a, $b)|], [t|($b, $a)|]))
      [|swap|]
    . insertTest
      (Proxy @"Fork")
      mkUnaryTestConfig
      (\(a, b) -> (b, [t|($a, $b)|]))
      [|const 42 &&& id|]
    . insertTest
      (Proxy @"Join")
      mkUnaryTestConfig
      (\(a, b) -> ([t|Either $a $b|], b))
      [|const 2 ||| (`mod` 8)|]
    . insertTest (Proxy @"Arr") mkUnaryTestConfig (\a -> (a, a)) [|arr cos|]
    . insertTest
      (Proxy @"Either")
      mkUnaryTestConfig
      (\(a, b) -> ([t|Either $a $b|], b))
      [|either (const 42) id|]
    . insertTest (Proxy @"Coerce") mkUnaryTestConfig (\a -> (a, [t|Sum $a|])) [|coerce|]
    . insertTest
      (Proxy @"ComposedCoerce")
      mkUnaryTestConfig
      (\a -> (a, [t|Sum $a|]))
      [|fst . (coerce &&& id)|]
    -- `bool` needs to be fully applied in order to be interpreted
    . insertTest
      (Proxy @"Bool")
      mkTernaryTestConfig
      (\a -> (a, [t|$a -> Bool -> $a|]))
      [|\a b c -> bool a b c|]
    . insertTest (Proxy @"Acos") mkUnaryTestConfig (\a -> (a, a)) [|acos|]
    . insertTest (Proxy @"Acosh") mkUnaryTestConfig (\a -> (a, a)) [|acosh|]
    . insertTest
      (Proxy @"AcoshDouble")
      mkUnaryTestConfig
      (\() -> ([t|Double|], [t|Double|]))
      [|GHC.Float.acoshDouble|]
    . insertTest
      (Proxy @"AcoshFloat")
      mkUnaryTestConfig
      (\() -> ([t|Float|], [t|Float|]))
      [|GHC.Float.acoshFloat|]
    . insertTest (Proxy @"Asin") mkUnaryTestConfig (\a -> (a, a)) [|asin|]
    . insertTest (Proxy @"Asinh") mkUnaryTestConfig (\a -> (a, a)) [|asinh|]
    . insertTest
      (Proxy @"AsinhDouble")
      mkUnaryTestConfig
      (\() -> ([t|Double|], [t|Double|]))
      [|GHC.Float.asinhDouble|]
    . insertTest
      (Proxy @"AsinhFloat")
      mkUnaryTestConfig
      (\() -> ([t|Float|], [t|Float|]))
      [|GHC.Float.asinhFloat|]
    . insertTest (Proxy @"Atan") mkUnaryTestConfig (\a -> (a, a)) [|atan|]
    . insertTest (Proxy @"Atanh") mkUnaryTestConfig (\a -> (a, a)) [|atanh|]
    . insertTest
      (Proxy @"AtanhDouble")
      mkUnaryTestConfig
      (\() -> ([t|Double|], [t|Double|]))
      [|GHC.Float.atanhDouble|]
    . insertTest
      (Proxy @"AtanhFloat")
      mkUnaryTestConfig
      (\() -> ([t|Float|], [t|Float|]))
      [|GHC.Float.atanhFloat|]
    . insertTest (Proxy @"Cos") mkUnaryTestConfig (\a -> (a, a)) [|cos|]
    . insertTest (Proxy @"Cosh") mkUnaryTestConfig (\a -> (a, a)) [|cosh|]
    . insertTest
      (Proxy @"AcosDouble")
      mkUnaryTestConfig
      (\() -> ([t|Double|], [t|Double|]))
      [|GHC.Float.acosDouble|]
    . insertTest
      (Proxy @"AsinDouble")
      mkUnaryTestConfig
      (\() -> ([t|Double|], [t|Double|]))
      [|GHC.Float.asinDouble|]
    . insertTest
      (Proxy @"AtanDouble")
      mkUnaryTestConfig
      (\() -> ([t|Double|], [t|Double|]))
      [|GHC.Float.atanDouble|]
    . insertTest
      (Proxy @"CosDouble")
      mkUnaryTestConfig
      (\() -> ([t|Double|], [t|Double|]))
      [|GHC.Float.cosDouble|]
    . insertTest
      (Proxy @"CoshDouble")
      mkUnaryTestConfig
      (\() -> ([t|Double|], [t|Double|]))
      [|GHC.Float.coshDouble|]
    . insertTest
      (Proxy @"AcosFloat")
      mkUnaryTestConfig
      (\() -> ([t|Float|], [t|Float|]))
      [|GHC.Float.acosFloat|]
    . insertTest
      (Proxy @"AsinFloat")
      mkUnaryTestConfig
      (\() -> ([t|Float|], [t|Float|]))
      [|GHC.Float.asinFloat|]
    . insertTest
      (Proxy @"AtanFloat")
      mkUnaryTestConfig
      (\() -> ([t|Float|], [t|Float|]))
      [|GHC.Float.atanFloat|]
    . insertTest
      (Proxy @"CosFloat")
      mkUnaryTestConfig
      (\() -> ([t|Float|], [t|Float|]))
      [|GHC.Float.cosFloat|]
    . insertTest
      (Proxy @"CoshFloat")
      mkUnaryTestConfig
      (\() -> ([t|Float|], [t|Float|]))
      [|GHC.Float.coshFloat|]
    . insertTest
      (Proxy @"Double2Float")
      mkUnaryTestConfig
      (\() -> ([t|Double|], [t|Float|]))
      [|GHC.Float.double2Float|]
    . insertTest (Proxy @"Exp") mkUnaryTestConfig (\a -> (a, a)) [|exp|]
    . insertTest
      (Proxy @"Float2Double")
      mkUnaryTestConfig
      (\() -> ([t|Float|], [t|Double|]))
      [|GHC.Float.float2Double|]
    . insertTest
      (Proxy @"IsDenormalized")
      mkUnaryTestConfig
      (\a -> (a, [t|Bool|]))
      [|GHC.Float.isDenormalized|]
    . insertTest
      (Proxy @"IsInfinite")
      mkUnaryTestConfig
      (\a -> (a, [t|Bool|]))
      [|GHC.Float.isInfinite|]
    . insertTest (Proxy @"IsNaN") mkUnaryTestConfig (\a -> (a, [t|Bool|])) [|GHC.Float.isNaN|]
    . insertTest
      (Proxy @"IsNegativeZero")
      mkUnaryTestConfig
      (\a -> (a, [t|Bool|]))
      [|GHC.Float.isNegativeZero|]
    . insertTest (Proxy @"Log") mkUnaryTestConfig (\a -> (a, a)) [|log|]
    . insertTest
      (Proxy @"LogDouble")
      mkUnaryTestConfig
      (\() -> ([t|Double|], [t|Double|]))
      [|GHC.Float.logDouble|]
    . insertTest
      (Proxy @"LogFloat")
      mkUnaryTestConfig
      (\() -> ([t|Float|], [t|Float|]))
      [|GHC.Float.logFloat|]
    . insertTest
      (Proxy @"MinusDouble")
      mkBinaryTestConfig
      (\() -> ([t|Double|], [t|Double -> Double|]))
      [|GHC.Float.minusDouble|]
    . insertTest
      (Proxy @"MinusFloat")
      mkBinaryTestConfig
      (\() -> ([t|Float|], [t|Float -> Float|]))
      [|GHC.Float.minusFloat|]
    . insertTest
      (Proxy @"NegateDouble")
      mkUnaryTestConfig
      (\() -> ([t|Double|], [t|Double|]))
      [|GHC.Float.negateDouble|]
    . insertTest
      (Proxy @"NegateFloat")
      mkUnaryTestConfig
      (\() -> ([t|Float|], [t|Float|]))
      [|GHC.Float.negateFloat|]
    . insertTest
      (Proxy @"PlusDouble")
      mkBinaryTestConfig
      (\() -> ([t|Double|], [t|Double -> Double|]))
      [|GHC.Float.plusDouble|]
    . insertTest
      (Proxy @"PlusFloat")
      mkBinaryTestConfig
      (\() -> ([t|Float|], [t|Float -> Float|]))
      [|GHC.Float.plusFloat|]
    . insertTest (Proxy @"Power") mkBinaryTestConfig (\a -> (a, [t|$a -> $a|])) [|(**)|]
    . insertTest
      (Proxy @"PowerDouble")
      mkBinaryTestConfig
      (\() -> ([t|Double|], [t|Double -> Double|]))
      [|GHC.Float.powerDouble|]
    . insertTest
      (Proxy @"PowerFloat")
      mkBinaryTestConfig
      (\() -> ([t|Float|], [t|Float -> Float|]))
      [|GHC.Float.powerFloat|]
    . insertTest (Proxy @"Sin") mkUnaryTestConfig (\a -> (a, a)) [|sin|]
    . insertTest (Proxy @"Sinh") mkUnaryTestConfig (\a -> (a, a)) [|sinh|]
    . insertTest
      (Proxy @"SinDouble")
      mkUnaryTestConfig
      (\() -> ([t|Double|], [t|Double|]))
      [|GHC.Float.sinDouble|]
    . insertTest
      (Proxy @"SinhDouble")
      mkUnaryTestConfig
      (\() -> ([t|Double|], [t|Double|]))
      [|GHC.Float.sinhDouble|]
    . insertTest
      (Proxy @"SinFloat")
      mkUnaryTestConfig
      (\() -> ([t|Float|], [t|Float|]))
      [|GHC.Float.sinFloat|]
    . insertTest
      (Proxy @"SinhFloat")
      mkUnaryTestConfig
      (\() -> ([t|Float|], [t|Float|]))
      [|GHC.Float.sinhFloat|]
    . insertTest (Proxy @"Sqrt") mkUnaryTestConfig (\a -> (a, a)) [|sqrt|]
    . insertTest
      (Proxy @"SqrtDouble")
      mkUnaryTestConfig
      (\() -> ([t|Double|], [t|Double|]))
      [|GHC.Float.sqrtDouble|]
    . insertTest
      (Proxy @"SqrtFloat")
      mkUnaryTestConfig
      (\() -> ([t|Float|], [t|Float|]))
      [|GHC.Float.sqrtFloat|]
    . insertTest (Proxy @"Tan") mkUnaryTestConfig (\a -> (a, a)) [|tan|]
    . insertTest (Proxy @"Tanh") mkUnaryTestConfig (\a -> (a, a)) [|tanh|]
    . insertTest
      (Proxy @"TanDouble")
      mkUnaryTestConfig
      (\() -> ([t|Double|], [t|Double|]))
      [|GHC.Float.tanDouble|]
    . insertTest
      (Proxy @"TanhDouble")
      mkUnaryTestConfig
      (\() -> ([t|Double|], [t|Double|]))
      [|GHC.Float.tanhDouble|]
    . insertTest
      (Proxy @"TanFloat")
      mkUnaryTestConfig
      (\() -> ([t|Float|], [t|Float|]))
      [|GHC.Float.tanFloat|]
    . insertTest
      (Proxy @"TanhFloat")
      mkUnaryTestConfig
      (\() -> ([t|Float|], [t|Float|]))
      [|GHC.Float.tanhFloat|]
    . insertTest
      (Proxy @"TimesDouble")
      mkBinaryTestConfig
      (\() -> ([t|Double|], [t|Double -> Double|]))
      [|GHC.Float.timesDouble|]
    . insertTest
      (Proxy @"TimesFloat")
      mkBinaryTestConfig
      (\() -> ([t|Float|], [t|Float -> Float|]))
      [|GHC.Float.timesFloat|]
    . insertTest (Proxy @"And") mkBinaryTestConfig (\() -> ([t|Bool|], [t|Bool -> Bool|])) [|(&&)|]
    . insertTest (Proxy @"Or") mkBinaryTestConfig (\() -> ([t|Bool|], [t|Bool -> Bool|])) [|(||)|]
    . insertTest (Proxy @"Equal") mkBinaryTestConfig (\a -> (a, [t|$a -> Bool|])) [|(==)|]
    . insertTest (Proxy @"NotEqual") mkBinaryTestConfig (\a -> (a, [t|$a -> Bool|])) [|(/=)|]
    . insertTest (Proxy @"Ge") mkBinaryTestConfig (\a -> (a, [t|$a -> Bool|])) [|(>=)|]
    . insertTest (Proxy @"Gt") mkBinaryTestConfig (\a -> (a, [t|$a -> Bool|])) [|(>)|]
    . insertTest (Proxy @"Le") mkBinaryTestConfig (\a -> (a, [t|$a -> Bool|])) [|(<=)|]
    . insertTest (Proxy @"Lt") mkBinaryTestConfig (\a -> (a, [t|$a -> Bool|])) [|(<)|]
    . insertTest (Proxy @"EqualDouble") mkBinaryTestConfig (\() -> ([t|Double|], [t|Double -> Bool|])) [|GHC.Float.eqDouble|]
    . insertTest (Proxy @"GeDouble") mkBinaryTestConfig (\() -> ([t|Double|], [t|Double -> Bool|])) [|GHC.Float.geDouble|]
    . insertTest (Proxy @"GtDouble") mkBinaryTestConfig (\() -> ([t|Double|], [t|Double -> Bool|])) [|GHC.Float.gtDouble|]
    . insertTest (Proxy @"LeDouble") mkBinaryTestConfig (\() -> ([t|Double|], [t|Double -> Bool|])) [|GHC.Float.leDouble|]
    . insertTest (Proxy @"LtDouble") mkBinaryTestConfig (\() -> ([t|Double|], [t|Double -> Bool|])) [|GHC.Float.ltDouble|]
    . insertTest (Proxy @"EqualFloat") mkBinaryTestConfig (\() -> ([t|Float|], [t|Float -> Bool|])) [|GHC.Float.eqFloat|]
    . insertTest (Proxy @"GeFloat") mkBinaryTestConfig (\() -> ([t|Float|], [t|Float -> Bool|])) [|GHC.Float.geFloat|]
    . insertTest (Proxy @"GtFloat") mkBinaryTestConfig (\() -> ([t|Float|], [t|Float -> Bool|])) [|GHC.Float.gtFloat|]
    . insertTest (Proxy @"LeFloat") mkBinaryTestConfig (\() -> ([t|Float|], [t|Float -> Bool|])) [|GHC.Float.leFloat|]
    . insertTest (Proxy @"LtFloat") mkBinaryTestConfig (\() -> ([t|Float|], [t|Float -> Bool|])) [|GHC.Float.ltFloat|]
    . insertTest (Proxy @"EqualInt") mkBinaryTestConfig (\() -> ([t|Int|], [t|Int -> Bool|])) [|GHC.Int.eqInt|]
    . insertTest (Proxy @"NotEqualInt") mkBinaryTestConfig (\() -> ([t|Int|], [t|Int -> Bool|])) [|GHC.Int.neInt|]
    . insertTest (Proxy @"GeInt") mkBinaryTestConfig (\() -> ([t|Int|], [t|Int -> Bool|])) [|GHC.Int.geInt|]
    . insertTest (Proxy @"GtInt") mkBinaryTestConfig (\() -> ([t|Int|], [t|Int -> Bool|])) [|GHC.Int.gtInt|]
    . insertTest (Proxy @"LeInt") mkBinaryTestConfig (\() -> ([t|Int|], [t|Int -> Bool|])) [|GHC.Int.leInt|]
    . insertTest (Proxy @"LtInt") mkBinaryTestConfig (\() -> ([t|Int|], [t|Int -> Bool|])) [|GHC.Int.ltInt|]
    . insertTest (Proxy @"EqualInt16") mkBinaryTestConfig (\() -> ([t|Int16|], [t|Int16 -> Bool|])) [|GHC.Int.eqInt16|]
    . insertTest (Proxy @"NotEqualInt16") mkBinaryTestConfig (\() -> ([t|Int16|], [t|Int16 -> Bool|])) [|GHC.Int.neInt16|]
    . insertTest (Proxy @"GeInt16") mkBinaryTestConfig (\() -> ([t|Int16|], [t|Int16 -> Bool|])) [|GHC.Int.geInt16|]
    . insertTest (Proxy @"GtInt16") mkBinaryTestConfig (\() -> ([t|Int16|], [t|Int16 -> Bool|])) [|GHC.Int.gtInt16|]
    . insertTest (Proxy @"LeInt16") mkBinaryTestConfig (\() -> ([t|Int16|], [t|Int16 -> Bool|])) [|GHC.Int.leInt16|]
    . insertTest (Proxy @"LtInt16") mkBinaryTestConfig (\() -> ([t|Int16|], [t|Int16 -> Bool|])) [|GHC.Int.ltInt16|]
    . insertTest (Proxy @"EqualInt32") mkBinaryTestConfig (\() -> ([t|Int32|], [t|Int32 -> Bool|])) [|GHC.Int.eqInt32|]
    . insertTest (Proxy @"NotEqualInt32") mkBinaryTestConfig (\() -> ([t|Int32|], [t|Int32 -> Bool|])) [|GHC.Int.neInt32|]
    . insertTest (Proxy @"GeInt32") mkBinaryTestConfig (\() -> ([t|Int32|], [t|Int32 -> Bool|])) [|GHC.Int.geInt32|]
    . insertTest (Proxy @"GtInt32") mkBinaryTestConfig (\() -> ([t|Int32|], [t|Int32 -> Bool|])) [|GHC.Int.gtInt32|]
    . insertTest (Proxy @"LeInt32") mkBinaryTestConfig (\() -> ([t|Int32|], [t|Int32 -> Bool|])) [|GHC.Int.leInt32|]
    . insertTest (Proxy @"LtInt32") mkBinaryTestConfig (\() -> ([t|Int32|], [t|Int32 -> Bool|])) [|GHC.Int.ltInt32|]
    . insertTest (Proxy @"EqualInt64") mkBinaryTestConfig (\() -> ([t|Int64|], [t|Int64 -> Bool|])) [|GHC.Int.eqInt64|]
    . insertTest (Proxy @"NotEqualInt64") mkBinaryTestConfig (\() -> ([t|Int64|], [t|Int64 -> Bool|])) [|GHC.Int.neInt64|]
    . insertTest (Proxy @"GeInt64") mkBinaryTestConfig (\() -> ([t|Int64|], [t|Int64 -> Bool|])) [|GHC.Int.geInt64|]
    . insertTest (Proxy @"GtInt64") mkBinaryTestConfig (\() -> ([t|Int64|], [t|Int64 -> Bool|])) [|GHC.Int.gtInt64|]
    . insertTest (Proxy @"LeInt64") mkBinaryTestConfig (\() -> ([t|Int64|], [t|Int64 -> Bool|])) [|GHC.Int.leInt64|]
    . insertTest (Proxy @"LtInt64") mkBinaryTestConfig (\() -> ([t|Int64|], [t|Int64 -> Bool|])) [|GHC.Int.ltInt64|]
    . insertTest (Proxy @"EqualInt8") mkBinaryTestConfig (\() -> ([t|Int8|], [t|Int8 -> Bool|])) [|GHC.Int.eqInt8|]
    . insertTest (Proxy @"NotEqualInt8") mkBinaryTestConfig (\() -> ([t|Int8|], [t|Int8 -> Bool|])) [|GHC.Int.neInt8|]
    . insertTest (Proxy @"GeInt8") mkBinaryTestConfig (\() -> ([t|Int8|], [t|Int8 -> Bool|])) [|GHC.Int.geInt8|]
    . insertTest (Proxy @"GtInt8") mkBinaryTestConfig (\() -> ([t|Int8|], [t|Int8 -> Bool|])) [|GHC.Int.gtInt8|]
    . insertTest (Proxy @"LeInt8") mkBinaryTestConfig (\() -> ([t|Int8|], [t|Int8 -> Bool|])) [|GHC.Int.leInt8|]
    . insertTest (Proxy @"LtInt8") mkBinaryTestConfig (\() -> ([t|Int8|], [t|Int8 -> Bool|])) [|GHC.Int.ltInt8|]
    . insertTest (Proxy @"EqualWord") mkBinaryTestConfig (\() -> ([t|Word|], [t|Word -> Bool|])) [|GHC.Word.eqWord|]
    . insertTest (Proxy @"NotEqualWord") mkBinaryTestConfig (\() -> ([t|Word|], [t|Word -> Bool|])) [|GHC.Word.neWord|]
    . insertTest (Proxy @"GeWord") mkBinaryTestConfig (\() -> ([t|Word|], [t|Word -> Bool|])) [|GHC.Word.geWord|]
    . insertTest (Proxy @"GtWord") mkBinaryTestConfig (\() -> ([t|Word|], [t|Word -> Bool|])) [|GHC.Word.gtWord|]
    . insertTest (Proxy @"LeWord") mkBinaryTestConfig (\() -> ([t|Word|], [t|Word -> Bool|])) [|GHC.Word.leWord|]
    . insertTest (Proxy @"LtWord") mkBinaryTestConfig (\() -> ([t|Word|], [t|Word -> Bool|])) [|GHC.Word.ltWord|]
    . insertTest (Proxy @"EqualWord16") mkBinaryTestConfig (\() -> ([t|Word16|], [t|Word16 -> Bool|])) [|GHC.Word.eqWord16|]
    . insertTest (Proxy @"NotEqualWord16") mkBinaryTestConfig (\() -> ([t|Word16|], [t|Word16 -> Bool|])) [|GHC.Word.neWord16|]
    . insertTest (Proxy @"GeWord16") mkBinaryTestConfig (\() -> ([t|Word16|], [t|Word16 -> Bool|])) [|GHC.Word.geWord16|]
    . insertTest (Proxy @"GtWord16") mkBinaryTestConfig (\() -> ([t|Word16|], [t|Word16 -> Bool|])) [|GHC.Word.gtWord16|]
    . insertTest (Proxy @"LeWord16") mkBinaryTestConfig (\() -> ([t|Word16|], [t|Word16 -> Bool|])) [|GHC.Word.leWord16|]
    . insertTest (Proxy @"LtWord16") mkBinaryTestConfig (\() -> ([t|Word16|], [t|Word16 -> Bool|])) [|GHC.Word.ltWord16|]
    . insertTest (Proxy @"EqualWord32") mkBinaryTestConfig (\() -> ([t|Word32|], [t|Word32 -> Bool|])) [|GHC.Word.eqWord32|]
    . insertTest (Proxy @"NotEqualWord32") mkBinaryTestConfig (\() -> ([t|Word32|], [t|Word32 -> Bool|])) [|GHC.Word.neWord32|]
    . insertTest (Proxy @"GeWord32") mkBinaryTestConfig (\() -> ([t|Word32|], [t|Word32 -> Bool|])) [|GHC.Word.geWord32|]
    . insertTest (Proxy @"GtWord32") mkBinaryTestConfig (\() -> ([t|Word32|], [t|Word32 -> Bool|])) [|GHC.Word.gtWord32|]
    . insertTest (Proxy @"LeWord32") mkBinaryTestConfig (\() -> ([t|Word32|], [t|Word32 -> Bool|])) [|GHC.Word.leWord32|]
    . insertTest (Proxy @"LtWord32") mkBinaryTestConfig (\() -> ([t|Word32|], [t|Word32 -> Bool|])) [|GHC.Word.ltWord32|]
    . insertTest (Proxy @"EqualWord64") mkBinaryTestConfig (\() -> ([t|Word64|], [t|Word64 -> Bool|])) [|GHC.Word.eqWord64|]
    . insertTest (Proxy @"NotEqualWord64") mkBinaryTestConfig (\() -> ([t|Word64|], [t|Word64 -> Bool|])) [|GHC.Word.neWord64|]
    . insertTest (Proxy @"GeWord64") mkBinaryTestConfig (\() -> ([t|Word64|], [t|Word64 -> Bool|])) [|GHC.Word.geWord64|]
    . insertTest (Proxy @"GtWord64") mkBinaryTestConfig (\() -> ([t|Word64|], [t|Word64 -> Bool|])) [|GHC.Word.gtWord64|]
    . insertTest (Proxy @"LeWord64") mkBinaryTestConfig (\() -> ([t|Word64|], [t|Word64 -> Bool|])) [|GHC.Word.leWord64|]
    . insertTest (Proxy @"LtWord64") mkBinaryTestConfig (\() -> ([t|Word64|], [t|Word64 -> Bool|])) [|GHC.Word.ltWord64|]
    . insertTest (Proxy @"EqualWord8") mkBinaryTestConfig (\() -> ([t|Word8|], [t|Word8 -> Bool|])) [|GHC.Word.eqWord8|]
    . insertTest (Proxy @"NotEqualWord8") mkBinaryTestConfig (\() -> ([t|Word8|], [t|Word8 -> Bool|])) [|GHC.Word.neWord8|]
    . insertTest (Proxy @"GeWord8") mkBinaryTestConfig (\() -> ([t|Word8|], [t|Word8 -> Bool|])) [|GHC.Word.geWord8|]
    . insertTest (Proxy @"GtWord8") mkBinaryTestConfig (\() -> ([t|Word8|], [t|Word8 -> Bool|])) [|GHC.Word.gtWord8|]
    . insertTest (Proxy @"LeWord8") mkBinaryTestConfig (\() -> ([t|Word8|], [t|Word8 -> Bool|])) [|GHC.Word.leWord8|]
    . insertTest (Proxy @"LtWord8") mkBinaryTestConfig (\() -> ([t|Word8|], [t|Word8 -> Bool|])) [|GHC.Word.ltWord8|]
    . insertTest (Proxy @"Compare") mkBinaryTestConfig (\a -> (a, [t|$a -> Ordering|])) [|compare|]
    . insertTest (Proxy @"Max") mkBinaryTestConfig (\a -> (a, [t|$a -> $a|])) [|max|]
    . insertTest (Proxy @"Min") mkBinaryTestConfig (\a -> (a, [t|$a -> $a|])) [|min|]
    . insertTest (Proxy @"Not") mkUnaryTestConfig (\() -> ([t|Bool|], [t|Bool|])) [|not|]
    . insertTest (Proxy @"Plus") mkBinaryTestConfig (\a -> (a, [t|$a -> $a|])) [|(+)|]
    . insertTest (Proxy @"Minus") mkBinaryTestConfig (\a -> (a, [t|$a -> $a|])) [|(-)|]
    . insertTest (Proxy @"Times") mkBinaryTestConfig (\a -> (a, [t|$a -> $a|])) [|(*)|]
    . insertTest (Proxy @"Quot") mkBinaryTestConfig (\a -> (a, [t|$a -> $a|])) [|quot|]
    . insertTest (Proxy @"RealToFrac") mkUnaryTestConfig (\(a, b) -> (a, b)) [|realToFrac|]
    . insertTest (Proxy @"Recip") mkUnaryTestConfig (\a -> (a, a)) [|recip|]
    . insertTest (Proxy @"Rem") mkBinaryTestConfig (\a -> (a, [t|$a -> $a|])) [|rem|]
    . insertTest (Proxy @"Div") mkBinaryTestConfig (\a -> (a, [t|$a -> $a|])) [|div|]
    . insertTest (Proxy @"Mod") mkBinaryTestConfig (\a -> (a, [t|$a -> $a|])) [|mod|]
    . insertTest (Proxy @"Divide") mkBinaryTestConfig (\a -> (a, [t|$a -> $a|])) [|(/)|]
    . insertTest
      (Proxy @"DivideDouble")
      mkBinaryTestConfig
      (\() -> ([t|Double|], [t|Double -> Double|]))
      [|GHC.Float.divideDouble|]
    . insertTest
      (Proxy @"DivideFloat")
      mkBinaryTestConfig
      (\() -> ([t|Float|], [t|Float -> Float|]))
      [|GHC.Float.divideFloat|]
    . insertTest (Proxy @"Atan2") mkBinaryTestConfig (\a -> (a, [t|$a -> $a|])) [|GHC.Float.atan2|]
    . insertTest (Proxy @"Abs") mkUnaryTestConfig (\a -> (a, a)) [|abs|]
    . insertTest (Proxy @"Negate") mkUnaryTestConfig (\a -> (a, a)) [|negate|]
    . insertTest (Proxy @"Signum") mkUnaryTestConfig (\a -> (a, a)) [|signum|]
    . insertTest (Proxy @"PowI") mkUnaryTestConfig (\a -> (a, a)) [|(^ (3 :: Word8))|]
    . insertTest (Proxy @"PowInt") mkUnaryTestConfig (\a -> (a, a)) [|(^ (3 :: Int))|]
    . insertTest (Proxy @"FromInteger") mkUnaryTestConfig (\a -> ([t|Integer|], a)) [|fromInteger|]
    . insertTest (Proxy @"FromIntegral") mkUnaryTestConfig (\(a, b) -> (a, b)) [|fromIntegral|]
    . insertTest (Proxy @"Append") mkBinaryTestConfig (\a -> (a, [t|$a -> $a|])) [|(<>)|]
    . insertTest (Proxy @"Mappend") mkBinaryTestConfig (\a -> (a, [t|$a -> $a|])) [|mappend|]
    . insertTest
      (Proxy @"ListAppend")
      mkBinaryTestConfig
      (\a -> ([t|[$a]|], [t|[$a] -> [$a]|]))
      [|(++)|]
    . insertTest (Proxy @"Pure") mkUnaryTestConfig (\a -> (a, [t|Identity $a|])) [|pure|]
    . insertTest (Proxy @"Return") mkUnaryTestConfig (\a -> (a, [t|Identity $a|])) [|return|]
    . insertTest (Proxy @"Error") mkUnaryTestConfig (\a -> ([t|String|], a)) [|error|]
    . insertTest (Proxy @"BuildLeft") mkUnaryTestConfig (\(a, b) -> (a, [t|Either $a $b|])) [|Left|]
    . insertTest
      (Proxy @"BuildRight")
      mkUnaryTestConfig
      (\(a, b) -> (b, [t|Either $a $b|]))
      [|Right|]
    . insertTest
      (Proxy @"EliminateEither")
      mkUnaryTestConfig
      (\a -> ([t|Either $a $a|], a))
      [|
        \case
          Left x -> x
          Right y -> y
        |]
    . insertTest
      (Proxy @"EliminateEitherSwapped")
      mkUnaryTestConfig
      (\a -> ([t|Either $a $a|], a))
      [|
        \case
          Right y -> y
          Left x -> x
        |]
    . insertTest (Proxy @"Apply") mkBinaryTestConfig (\(a, b) -> (a, [t|$b -> $a|])) [|const|]
    . insertTest
      (Proxy @"BareFMap")
      mkUnaryTestConfig
      (\a -> ([t|($a -> $a, Pair $a)|], [t|Pair $a|]))
      [|uncurry fmap|]
    . insertTest
      (Proxy @"PartialFmap")
      mkUnaryTestConfig
      (\a -> ([t|Pair $a|], [t|Pair ($a, $a)|]))
      [|fmap . (id &&&) $ id|]
    . insertTest
      (Proxy @"Fmap")
      mkUnaryTestConfig
      (\(f, a) -> ([t|$f $a|], [t|$f ($a, $a)|]))
      [|fmap (id &&& id)|]
    . insertTest
      (Proxy @"Fmap'")
      mkUnaryTestConfig
      (\a -> ([t|Pair $a|], [t|Pair ($a, $a)|]))
      [|((id &&& id) <$>)|]
    . insertTest
      (Proxy @"ConstNot")
      mkBinaryTestConfig
      (\a -> (a, [t|Bool -> Bool|]))
      [|(\_ -> not)|]
    . insertTest
      (Proxy @"MapList")
      mkUnaryTestConfig
      (\a -> ([t|[$a]|], [t|[($a, $a)]|]))
      [|map (id &&& id)|]
    . insertTest
      (Proxy @"Ap")
      mkUnaryTestConfig
      (\(f, a) -> ([t|$f $a|], [t|$f $a|]))
      [|(pure id <*>)|]
    . insertTest
      (Proxy @"LiftA2")
      mkBinaryTestConfig
      (\(f, a, b) -> ([t|$f $a|], [t|$f $b -> $f $a|]))
      [|liftA2 const|]
    . insertTest
      (Proxy @"Bind")
      mkBinaryTestConfig
      (\a -> ([t|Identity $a|], [t|($a -> Identity $a) -> Identity $a|]))
      [|(>>=)|]
    . insertTest (Proxy @"Curry") mkBinaryTestConfig (\(a, b) -> (a, [t|$b -> $a|])) [|const|]
    . insertTest
      (Proxy @"Uncurry")
      mkUnaryTestConfig
      (\(a, b) -> ([t|($a, $b)|], a))
      [|uncurry (\x _ -> x)|]
    . insertTest
      (Proxy @"SequenceA")
      mkUnaryTestConfig
      (\(t, f, a) -> ([t|$t ($f $a)|], [t|$f ($t $a)|]))
      [|sequenceA|]
    . insertTest
      (Proxy @"Traverse")
      mkUnaryTestConfig
      (\(t, f, a) -> ([t|$t $a|], [t|$f ($t $a)|]))
      [|traverse pure|]
    . insertTest
      (Proxy @"UnsafeCoerce")
      mkUnaryTestConfig
      (\a -> (a, [t|Identity $a|]))
      [|unsafeCoerce|]
    . insertTest
      (Proxy @"Sum")
      mkUnaryTestConfig
      (\(t, a) -> ([t|$t $a|], [t|$a|]))
      [|Data.Foldable.sum|]
    . insertTest (Proxy @"SumList") mkUnaryTestConfig (\a -> ([t|[$a]|], [t|$a|])) [|Data.List.sum|]
    . insertTest
      (Proxy @"ToList")
      mkUnaryTestConfig
      (\(t, a) -> ([t|$t $a|], [t|[$a]|]))
      [|Data.Foldable.toList|]
    . insertTest (Proxy @"Even") mkUnaryTestConfig (\a -> (a, [t|Bool|])) [|even|]
    . insertTest (Proxy @"Odd") mkUnaryTestConfig (\a -> (a, [t|Bool|])) [|odd|]
    $ HEmpty1
