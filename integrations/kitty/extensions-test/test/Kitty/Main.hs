{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Main
  ( main,
  )
where

import Control.Arrow (Arrow (..), ArrowChoice (..))
import Data.Bool (bool)
import Data.Functor.Identity (Identity (..))
import Data.Semigroup (Product (..), Sum (..))
import GHC.Int (Int16, Int32, Int64, Int8)
import GHC.Word (Word16, Word32, Word64, Word8)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Kitty.Plugin.Test.Kitty.Instances (Hask (..), Term)
import Kitty.Plugin.Test.Data (One (..), Pair (..))
import Kitty.Plugin.Test.HList (HList1 (..))
import Kitty.Plugin.Test.Tests
  ( TestCases (..),
    TestCategory (..),
    TestStrategy (..),
    defaultTestTerms,
    genFloating,
    mkTestTerms,
    zerosafeUnsignedPrimitiveCases,
  )
import Linear.V2 (V2 (..))
import System.Exit (exitFailure, exitSuccess)

-- |
--
--  __TODO__: This doesn't yet test against `Kitty.Plugin.Test.TotOrd.TotOrd` because a lot of tests need
--            to be disabled since that category isn't closed.
mkTestTerms
  defaultTestTerms
  --             name     type         prefix       strategy
  [ TestCategory ''Term [t|Term|] "term" CheckCompileOnly,
    TestCategory ''(->) [t|(->)|] "plainArrow" (ComputeFromInput [|id|]),
    TestCategory ''Hask [t|Hask|] "hask" (ComputeFromInput [|runHask|])
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
  -- This test case was disabled for `C.Cat` in patchset 15416. `C.Cat` is not supposed to work for
  -- arbitrary recursive functions. Previously this test case passed for `C.Cat` because in the
  -- previous `IfCat Cat a` implementation, the recursion was guarded by the `CExprF.BranchF`
  -- constructor, so the non-termination only happened when generating the C code. The new `IfCat
  -- Cat a` implementation uses the `ExprF.Branch` constructor, which means the non-termination
  -- would happen when converting `Expr` to `CExpr`, i.e., during `Kitty.CExpr.Cat.Eval.eval`, and
  -- so this test case no longer passes.
  . HCons1 (TestCases (const []))
  . HCons1 (TestCases (const [])) -- no ToTargetOb (Word8 -> Word8) (Word8 -> Word8)
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
  . HCons1
    (TestCases (const [([t|Double|], pure ([|(,) <$> genFloating <*> genFloating|], [|show|]))]))
  . HCons1 (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HCons1 (TestCases (const [])) -- ACoshNotSupported
  . HCons1 (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HCons1 (TestCases (const [])) -- ASinhNotSupported
  . HCons1 (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HCons1 (TestCases (const [])) -- AtanhNotSupported
  . HCons1 (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HCons1 (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HCons1 (TestCases (const [((), pure ([|genFloating|], [|show|]))]))
  . HCons1 (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HCons1 (TestCases (const [((), pure ([|genFloating|], [|show|]))]))
  . HCons1 (TestCases (const [])) -- IsDenormalKNotSupported
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
  . HCons1 (TestCases (const [])) -- no `OrdCat'` instance
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
  . HCons1 (TestCases (const [])) -- `arctan2` differs from `atan2`
  . HCons1 (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HCons1 (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HCons1 (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HCons1 (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HCons1 (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HCons1 (TestCases (const [])) -- Integer isn't an Expr
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
  . HCons1 (TestCases (const [])) -- `String` is not an object in these categories
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
  . HCons1 (TestCases (const [])) -- no ToTargetOb (Word8 -> Word8) (Word8 -> Word8)
  . HCons1
    ( TestCases
        (const [([t|Word8|], pure ([|Pair <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|]))])
    )
  . HCons1
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
  . HCons1
    ( TestCases
        (const [([t|Word8|], pure ([|Pair <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|]))])
    )
  . HCons1
    ( TestCases
        ( const [([t|Word8|], pure ([|Gen.enumBounded|], [|show|]))]
        )
    )
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
              ),
              -- Tests `$fAdditiveV2_$c<*>`.
              ( ([t|V2|], [t|Int64|]),
                pure ([|V2 <$> Gen.enumBounded <*> Gen.enumBounded|], [|show|])
              )
            ]
        )
    )
  . HCons1 (TestCases (const [([t|Int64|], pure ([|Gen.enumBounded|], [|show|]))]))
  . HCons1 (TestCases (const [])) -- no ToTargetOb Validation ...
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
  . HCons1
    ( TestCases
        ( const
            [ ( ([t|Sum|], [t|Product|], [t|Word8|]),
                pure ([|Sum . Product <$> Gen.enumBounded|], [|show|])
              )
            ]
        )
    )
  . HCons1
    ( TestCases
        ( const
            [(([t|Sum|], [t|Product|], [t|Word8|]), pure ([|Sum <$> Gen.enumBounded|], [|show|]))]
        )
    )
  . HCons1 (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HCons1 (TestCases (const []))
  . HCons1 (TestCases (const []))
  $ HNil1

main :: IO ()
main = bool exitFailure exitSuccess . and =<< allTestTerms
