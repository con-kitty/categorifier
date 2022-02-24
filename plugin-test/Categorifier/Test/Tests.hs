-- To avoid having to specify massive HList types.
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
-- To avoid having to specify massive HList types.
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

-- |
-- This module contains TH that generates terms to be invoked with the
-- 'Categorifier.Categorify.expression' plugin-runner.  This is done so that new arrows can be tested
-- easily.
--
-- The HLint warnings for 'id' and 'const' are disabled because we want to test how the plugin
-- handles exactly what's written.
module Categorifier.Test.Tests
  ( TestTerms,
    defaultTestTerms,
    coreTestTerms,
    pluginTestTerms,
    baseTestTerms,
    mkTestTerms,
    zerosafeUnsignedPrimitiveCases,
    TestCases (..),
    TestCategory (..),
    TestStrategy (..),
  )
where

import Categorifier.Client (Rep)
import Categorifier.Core.Functions (abst, repr)
import Categorifier.Test.Data (Pair (..))
import Categorifier.Test.HList (HList1 (..))
import qualified Categorifier.Test.HList as HList
import Categorifier.Test.TH
  ( ExprTest,
    TestCases (..),
    TestCategory (..),
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
import Data.Semigroup (Sum (..))
import Data.Tuple (swap)
import qualified GHC.Classes
import qualified GHC.Float
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

-- * property sets

--   Combinations of property generators that are commonly desired when dealing with `C.Cat`.

zerosafeUnsignedPrimitiveCases :: [(Q Type, Maybe (Q Exp, Q Exp))]
zerosafeUnsignedPrimitiveCases =
  [ ( [t|Word16|],
      pure ([|(,) <$> Gen.enumBounded <*> Gen.integral (Range.linear 1 maxBound)|], [|show|])
    ),
    ( [t|Word32|],
      pure ([|(,) <$> Gen.enumBounded <*> Gen.integral (Range.linear 1 maxBound)|], [|show|])
    ),
    ( [t|Word64|],
      pure
        ( [|(,) <$> Gen.integral Range.linearBounded <*> Gen.integral (Range.linear 1 maxBound)|],
          [|show|]
        )
    ),
    ( [t|Word8|],
      pure ([|(,) <$> Gen.enumBounded <*> Gen.integral (Range.linear 1 maxBound)|], [|show|])
    )
  ]

-- | A list of type-parameterized expressions to test.
--
--   There should be a `TestTerms` value corresponding to each
--  `Categorifier.Core.MakerMap.MakerMapFun` value (plus `coreTestTerms` to cover all of the GHC
--   Core cases).
--
--  __NB__: Use of this type should take advantage of @PartialTypeSignatures@ to avoid having to
--          specify massive HList types (e.g., @`TestTerms` _@).
type TestTerms = HList1 ExprTest

defaultTestTerms :: TestTerms _
defaultTestTerms = HList.append coreTestTerms $ HList.append pluginTestTerms baseTestTerms

{-# ANN coreTestTerms "HLint: ignore Collapse lambdas" #-}
{-# ANN coreTestTerms "HLint: ignore Use const" #-}
{-# ANN coreTestTerms "HLint: ignore Use fst" #-}
{-# ANN coreTestTerms "HLint: ignore Use id" #-}
{-# ANN coreTestTerms "HLint: ignore Use snd" #-}
coreTestTerms :: TestTerms _
coreTestTerms =
  HCons1 (mkExprTest (mkUnaryTestConfig "LamId") (\a -> (a, a)) [|(\x -> x)|])
    . HCons1
      ( mkExprTest
          (mkUnaryTestConfig "ComposeLam")
          (\a -> (a, a))
          [|((\f g x -> f (g x)) (\x -> x) (\x -> x))|]
      )
    . HCons1
      ( mkExprTest
          (mkBinaryTestConfig "ConstLam")
          (\(a, b) -> (a, [t|$b -> $a|]))
          [|(\x -> \_ -> x)|]
      )
    . HCons1
      (mkExprTest (mkUnaryTestConfig "ReturnLam") (\a -> (a, a)) [|\y -> (\x -> \_ -> x) y y|])
    . HCons1
      (mkExprTest (mkUnaryTestConfig "BuildTuple") (\a -> (a, [t|($a, $a)|])) [|\x -> (x, x)|])
    . HCons1
      ( mkExprTest
          (mkUnaryTestConfig "EliminateTupleFst")
          (\a -> ([t|($a, $a)|], a))
          [|\(x, _) -> x|]
      )
    . HCons1
      ( mkExprTest
          (mkUnaryTestConfig "EliminateTupleSnd")
          (\a -> ([t|($a, $a)|], a))
          [|\(_, x) -> x|]
      )
    . HCons1
      ( mkExprTest
          (mkUnaryTestConfig "EliminateNestedTuples")
          (\a -> ([t|($a, ($a, $a))|], a))
          [|\(_, (x, _)) -> x|]
      )
    . HCons1
      ( mkExprTest
          (mkUnaryTestConfig "LocalFixedpoint")
          (\a -> (a, a))
          [|let go x = if x > 0 then go (x - 1) else x in go|]
      )
    . HCons1 (mkExprTest (mkUnaryTestConfig "ApplyArg") (\a -> ([t|($a -> $a)|], a)) [|\f -> f 7|])
    . HCons1
      ( mkExprTest
          (mkUnaryTestConfig "If")
          (\a -> ([t|(Bool, ($a, $a))|], a))
          [|\(cond, (tru, fls)) -> if cond then tru else fls|]
      )
    $ HNil1

pluginTestTerms :: TestTerms _
pluginTestTerms =
  HCons1
    (mkExprTest (mkUnaryTestConfig "Abst") (\a -> ([t|Rep (Pair $a)|], [t|Pair $a|])) [|abst|])
    . HCons1
      (mkExprTest (mkUnaryTestConfig "Repr") (\a -> ([t|Pair $a|], [t|Rep (Pair $a)|])) [|repr|])
    $ HNil1

{-# ANN baseTestTerms "HLint: ignore Avoid lambda" #-}
{-# ANN baseTestTerms "HLint: ignore Redundant uncurry" #-}
{-# ANN baseTestTerms "HLint: ignore Use <>" #-}
{-# ANN baseTestTerms "HLint: ignore Use String" #-}
{-# ANN baseTestTerms "HLint: ignore Use const" #-}
{-# ANN baseTestTerms "HLint: ignore Use fmap" #-}
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
  HCons1 (mkExprTest (mkUnaryTestConfig "Id") (\a -> (a, a)) [|id|])
    . HCons1
      (mkExprTest (mkBinaryTestConfig "Const") (\(a, b) -> (a, [t|$b -> $a|])) [|const|])
    . HCons1 (mkExprTest (mkUnaryTestConfig "Snd") (\(a, b) -> ([t|($a, $b)|], b)) [|snd|])
    . HCons1
      ( mkExprTest
          (mkUnaryTestConfig "FstSnd")
          (\(a, b, c) -> ([t|($a, ($b, $c))|], b))
          [|fst . snd|]
      )
    . HCons1
      ( mkExprTest
          (mkUnaryTestConfig "FstLet")
          (\(a, b, c) -> ([t|($a, ($b, $c))|], b))
          [|\x -> let y = snd x in fst y|]
      )
    . HCons1
      (mkExprTest (mkUnaryTestConfig "Swap") (\(a, b) -> ([t|($a, $b)|], [t|($b, $a)|])) [|swap|])
    . HCons1
      ( mkExprTest
          (mkUnaryTestConfig "Fork")
          (\(a, b) -> (b, [t|($a, $b)|]))
          [|const 42 &&& id|]
      )
    . HCons1
      ( mkExprTest
          (mkUnaryTestConfig "Join")
          (\(a, b) -> ([t|Either $a $b|], b))
          [|const 2 ||| (`mod` 8)|]
      )
    . HCons1 (mkExprTest (mkUnaryTestConfig "Arr") (\a -> (a, a)) [|arr cos|])
    . HCons1
      ( mkExprTest
          (mkUnaryTestConfig "Either")
          (\(a, b) -> ([t|Either $a $b|], b))
          [|either (const 42) id|]
      )
    . HCons1 (mkExprTest (mkUnaryTestConfig "Coerce") (\a -> (a, [t|Sum $a|])) [|coerce|])
    . HCons1
      ( mkExprTest
          (mkUnaryTestConfig "ComposedCoerce")
          (\a -> (a, [t|Sum $a|]))
          [|fst . (coerce &&& id)|]
      )
    -- `bool` needs to be fully applied in order to be interpreted
    . HCons1
      ( mkExprTest
          (mkTernaryTestConfig "Bool")
          (\a -> (a, [t|$a -> Bool -> $a|]))
          [|\a b c -> bool a b c|]
      )
    . HCons1 (mkExprTest (mkBinaryTestConfig "Pow") (\a -> (a, [t|$a -> $a|])) [|(**)|])
    . HCons1 (mkExprTest (mkUnaryTestConfig "Acos") (\a -> (a, a)) [|acos|])
    . HCons1 (mkExprTest (mkUnaryTestConfig "Acosh") (\a -> (a, a)) [|acosh|])
    . HCons1 (mkExprTest (mkUnaryTestConfig "Asin") (\a -> (a, a)) [|asin|])
    . HCons1 (mkExprTest (mkUnaryTestConfig "Asinh") (\a -> (a, a)) [|asinh|])
    . HCons1 (mkExprTest (mkUnaryTestConfig "Atan") (\a -> (a, a)) [|atan|])
    . HCons1 (mkExprTest (mkUnaryTestConfig "Atanh") (\a -> (a, a)) [|atanh|])
    . HCons1 (mkExprTest (mkUnaryTestConfig "Cos") (\a -> (a, a)) [|cos|])
    . HCons1 (mkExprTest (mkUnaryTestConfig "Cosh") (\a -> (a, a)) [|cosh|])
    . HCons1
      ( mkExprTest
          (mkUnaryTestConfig "Double2Float")
          (\() -> ([t|Double|], [t|Float|]))
          [|GHC.Float.double2Float|]
      )
    . HCons1 (mkExprTest (mkUnaryTestConfig "Exp") (\a -> (a, a)) [|exp|])
    . HCons1
      ( mkExprTest
          (mkUnaryTestConfig "Float2Double")
          (\() -> ([t|Float|], [t|Double|]))
          [|GHC.Float.float2Double|]
      )
    . HCons1
      ( mkExprTest
          (mkUnaryTestConfig "IsDenormalized")
          (\a -> (a, [t|Bool|]))
          [|GHC.Float.isDenormalized|]
      )
    . HCons1
      (mkExprTest (mkUnaryTestConfig "IsInfinite") (\a -> (a, [t|Bool|])) [|GHC.Float.isInfinite|])
    . HCons1 (mkExprTest (mkUnaryTestConfig "IsNaN") (\a -> (a, [t|Bool|])) [|GHC.Float.isNaN|])
    . HCons1
      ( mkExprTest
          (mkUnaryTestConfig "IsNegativeZero")
          (\a -> (a, [t|Bool|]))
          [|GHC.Float.isNegativeZero|]
      )
    . HCons1 (mkExprTest (mkUnaryTestConfig "Log") (\a -> (a, a)) [|log|])
    . HCons1
      ( mkExprTest
          (mkUnaryTestConfig "NegateDouble")
          (\() -> ([t|Double|], [t|Double|]))
          [|GHC.Float.negateDouble|]
      )
    . HCons1
      ( mkExprTest
          (mkBinaryTestConfig "PlusDouble")
          (\() -> ([t|Double|], [t|Double -> Double|]))
          [|GHC.Float.plusDouble|]
      )
    . HCons1 (mkExprTest (mkUnaryTestConfig "Sin") (\a -> (a, a)) [|sin|])
    . HCons1 (mkExprTest (mkUnaryTestConfig "Sinh") (\a -> (a, a)) [|sinh|])
    . HCons1 (mkExprTest (mkUnaryTestConfig "Sqrt") (\a -> (a, a)) [|sqrt|])
    . HCons1
      ( mkExprTest
          (mkUnaryTestConfig "SqrtDouble")
          (\() -> ([t|Double|], [t|Double|]))
          [|GHC.Float.sqrtDouble|]
      )
    . HCons1 (mkExprTest (mkUnaryTestConfig "Tan") (\a -> (a, a)) [|tan|])
    . HCons1 (mkExprTest (mkUnaryTestConfig "Tanh") (\a -> (a, a)) [|tanh|])
    . HCons1
      ( mkExprTest
          (mkBinaryTestConfig "TimesDouble")
          (\() -> ([t|Double|], [t|Double -> Double|]))
          [|GHC.Float.timesDouble|]
      )
    . HCons1
      (mkExprTest (mkBinaryTestConfig "And") (\() -> ([t|Bool|], [t|Bool -> Bool|])) [|(&&)|])
    . HCons1 (mkExprTest (mkBinaryTestConfig "Or") (\() -> ([t|Bool|], [t|Bool -> Bool|])) [|(||)|])
    . HCons1 (mkExprTest (mkBinaryTestConfig "Equal") (\a -> (a, [t|$a -> Bool|])) [|(==)|])
    . HCons1 (mkExprTest (mkBinaryTestConfig "NotEqual") (\a -> (a, [t|$a -> Bool|])) [|(/=)|])
    . HCons1 (mkExprTest (mkBinaryTestConfig "Ge") (\a -> (a, [t|$a -> Bool|])) [|(>=)|])
    . HCons1 (mkExprTest (mkBinaryTestConfig "Gt") (\a -> (a, [t|$a -> Bool|])) [|(>)|])
    . HCons1 (mkExprTest (mkBinaryTestConfig "Le") (\a -> (a, [t|$a -> Bool|])) [|(<=)|])
    . HCons1 (mkExprTest (mkBinaryTestConfig "Lt") (\a -> (a, [t|$a -> Bool|])) [|(<)|])
    . HCons1
      (mkExprTest (mkBinaryTestConfig "Compare") (\a -> (a, [t|$a -> Ordering|])) [|compare|])
    . HCons1
      ( mkExprTest
          (mkBinaryTestConfig "EqDouble")
          (\() -> ([t|Double|], [t|Double -> Bool|]))
          [|GHC.Classes.eqDouble|]
      )
    . HCons1 (mkExprTest (mkBinaryTestConfig "Max") (\a -> (a, [t|$a -> $a|])) [|max|])
    . HCons1 (mkExprTest (mkBinaryTestConfig "Min") (\a -> (a, [t|$a -> $a|])) [|min|])
    . HCons1 (mkExprTest (mkUnaryTestConfig "Not") (\() -> ([t|Bool|], [t|Bool|])) [|not|])
    . HCons1 (mkExprTest (mkBinaryTestConfig "Plus") (\a -> (a, [t|$a -> $a|])) [|(+)|])
    . HCons1 (mkExprTest (mkBinaryTestConfig "Minus") (\a -> (a, [t|$a -> $a|])) [|(-)|])
    . HCons1 (mkExprTest (mkBinaryTestConfig "Times") (\a -> (a, [t|$a -> $a|])) [|(*)|])
    . HCons1 (mkExprTest (mkBinaryTestConfig "Quot") (\a -> (a, [t|$a -> $a|])) [|quot|])
    . HCons1 (mkExprTest (mkUnaryTestConfig "RealToFrac") (\(a, b) -> (a, b)) [|realToFrac|])
    . HCons1 (mkExprTest (mkUnaryTestConfig "Recip") (\a -> (a, a)) [|recip|])
    . HCons1 (mkExprTest (mkBinaryTestConfig "Rem") (\a -> (a, [t|$a -> $a|])) [|rem|])
    . HCons1 (mkExprTest (mkBinaryTestConfig "Div") (\a -> (a, [t|$a -> $a|])) [|div|])
    . HCons1 (mkExprTest (mkBinaryTestConfig "Mod") (\a -> (a, [t|$a -> $a|])) [|mod|])
    . HCons1 (mkExprTest (mkBinaryTestConfig "Divide") (\a -> (a, [t|$a -> $a|])) [|(/)|])
    . HCons1
      ( mkExprTest
          (mkBinaryTestConfig "EqWord8")
          (\() -> ([t|Word8|], [t|Word8 -> Bool|]))
          [|GHC.Word.eqWord8|]
      )
    . HCons1
      ( mkExprTest
          (mkBinaryTestConfig "NeWord8")
          (\() -> ([t|Word8|], [t|Word8 -> Bool|]))
          [|GHC.Word.neWord8|]
      )
    . HCons1
      (mkExprTest (mkBinaryTestConfig "Atan2") (\a -> (a, [t|$a -> $a|])) [|GHC.Float.atan2|])
    . HCons1 (mkExprTest (mkUnaryTestConfig "Abs") (\a -> (a, a)) [|abs|])
    . HCons1 (mkExprTest (mkUnaryTestConfig "Negate") (\a -> (a, a)) [|negate|])
    . HCons1 (mkExprTest (mkUnaryTestConfig "Signum") (\a -> (a, a)) [|signum|])
    . HCons1 (mkExprTest (mkUnaryTestConfig "PowI") (\a -> (a, a)) [|(^ (3 :: Word8))|])
    . HCons1 (mkExprTest (mkUnaryTestConfig "PowInt") (\a -> (a, a)) [|(^ (3 :: Int))|])
    . HCons1
      (mkExprTest (mkUnaryTestConfig "FromInteger") (\a -> ([t|Integer|], a)) [|fromInteger|])
    . HCons1 (mkExprTest (mkUnaryTestConfig "FromIntegral") (\(a, b) -> (a, b)) [|fromIntegral|])
    . HCons1 (mkExprTest (mkBinaryTestConfig "Append") (\a -> (a, [t|$a -> $a|])) [|(<>)|])
    . HCons1 (mkExprTest (mkBinaryTestConfig "Mappend") (\a -> (a, [t|$a -> $a|])) [|mappend|])
    . HCons1
      (mkExprTest (mkBinaryTestConfig "ListAppend") (\a -> ([t|[$a]|], [t|[$a] -> [$a]|])) [|(++)|])
    . HCons1 (mkExprTest (mkUnaryTestConfig "Pure") (\a -> (a, [t|Identity $a|])) [|pure|])
    . HCons1 (mkExprTest (mkUnaryTestConfig "Return") (\a -> (a, [t|Identity $a|])) [|return|])
    . HCons1 (mkExprTest (mkUnaryTestConfig "Error") (\a -> ([t|String|], a)) [|error|])
    . HCons1
      (mkExprTest (mkUnaryTestConfig "BuildLeft") (\(a, b) -> (a, [t|Either $a $b|])) [|Left|])
    . HCons1
      (mkExprTest (mkUnaryTestConfig "BuildRight") (\(a, b) -> (b, [t|Either $a $b|])) [|Right|])
    . HCons1
      ( mkExprTest
          (mkUnaryTestConfig "EliminateEither")
          (\a -> ([t|Either $a $a|], a))
          [|
            \case
              Left x -> x
              Right y -> y
            |]
      )
    . HCons1
      ( mkExprTest
          (mkUnaryTestConfig "EliminateEitherSwapped")
          (\a -> ([t|Either $a $a|], a))
          [|
            \case
              Right y -> y
              Left x -> x
            |]
      )
    . HCons1 (mkExprTest (mkBinaryTestConfig "Apply") (\(a, b) -> (a, [t|$b -> $a|])) [|const|])
    . HCons1
      ( mkExprTest
          (mkUnaryTestConfig "BareFMap")
          (\a -> ([t|($a -> $a, Pair $a)|], [t|Pair $a|]))
          [|uncurry fmap|]
      )
    . HCons1
      ( mkExprTest
          (mkUnaryTestConfig "PartialFmap")
          (\a -> ([t|Pair $a|], [t|Pair ($a, $a)|]))
          [|fmap . (id &&&) $ id|]
      )
    . HCons1
      ( mkExprTest
          (mkUnaryTestConfig "Fmap")
          (\(f, a) -> ([t|$f $a|], [t|$f ($a, $a)|]))
          [|fmap (id &&& id)|]
      )
    . HCons1
      ( mkExprTest
          (mkUnaryTestConfig "Fmap'")
          (\a -> ([t|Pair $a|], [t|Pair ($a, $a)|]))
          [|((id &&& id) <$>)|]
      )
    . HCons1
      (mkExprTest (mkBinaryTestConfig "ConstNot") (\a -> (a, [t|Bool -> Bool|])) [|(\_ -> not)|])
    . HCons1
      ( mkExprTest
          (mkUnaryTestConfig "MapList")
          (\a -> ([t|[$a]|], [t|[($a, $a)]|]))
          [|map (id &&& id)|]
      )
    . HCons1 (mkExprTest (mkUnaryTestConfig "Point") (\a -> (a, [t|Identity $a|])) [|pure|])
    . HCons1
      (mkExprTest (mkUnaryTestConfig "Ap") (\(f, a) -> ([t|$f $a|], [t|$f $a|])) [|(pure id <*>)|])
    . HCons1
      ( mkExprTest
          (mkBinaryTestConfig "LiftA2")
          (\(f, a, b) -> ([t|$f $a|], [t|$f $b -> $f $a|]))
          [|liftA2 const|]
      )
    . HCons1
      ( mkExprTest
          (mkBinaryTestConfig "Bind")
          (\a -> ([t|Identity $a|], [t|($a -> Identity $a) -> Identity $a|]))
          [|(>>=)|]
      )
    . HCons1 (mkExprTest (mkBinaryTestConfig "Curry") (\(a, b) -> (a, [t|$b -> $a|])) [|const|])
    . HCons1
      ( mkExprTest
          (mkUnaryTestConfig "Uncurry")
          (\(a, b) -> ([t|($a, $b)|], a))
          [|uncurry (\x _ -> x)|]
      )
    . HCons1
      ( mkExprTest
          (mkUnaryTestConfig "SequenceA")
          (\(t, f, a) -> ([t|$t ($f $a)|], [t|$f ($t $a)|]))
          [|sequenceA|]
      )
    . HCons1
      ( mkExprTest
          (mkUnaryTestConfig "Traverse")
          (\(t, f, a) -> ([t|$t $a|], [t|$f ($t $a)|]))
          [|traverse pure|]
      )
    . HCons1
      (mkExprTest (mkUnaryTestConfig "UnsafeCoerce") (\a -> (a, [t|Identity $a|])) [|unsafeCoerce|])
    . HCons1
      ( mkExprTest
          (mkUnaryTestConfig "Sum")
          (\(t, a) -> ([t|$t $a|], [t|$a|]))
          [|Data.Foldable.sum|]
      )
    . HCons1
      ( mkExprTest
          (mkUnaryTestConfig "ToList")
          (\(t, a) -> ([t|$t $a|], [t|[$a]|]))
          [|Data.Foldable.toList|]
      )
    . HCons1 (mkExprTest (mkUnaryTestConfig "Even") (\a -> (a, [t|Bool|])) [|even|])
    . HCons1 (mkExprTest (mkUnaryTestConfig "Odd") (\a -> (a, [t|Bool|])) [|odd|])
    $ HNil1
