{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
-- To avoid turning @if then else@ into `ifThenElse`.
{-# LANGUAGE NoRebindableSyntax #-}

module Main
  ( main,
  )
where

import Data.Functor.Identity (Identity (..))
import Data.Semigroup (Sum (..))
import Data.String (String)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import P
import System.Exit (exitFailure, exitSuccess)
import System.IO (IO)
import Test.Data (One (..), Pair (..))
import Test.HList (HList1 (..))
import Test.Term (Term)
import Test.Tests
  ( TestCases (..),
    TestCategory (..),
    TestStrategy (..),
    defaultTestTerms,
    mkTestTerms,
  )

-- For @NoRebindableSyntax@
{-# ANN module ("HLint: ignore Avoid restricted extensions" :: String) #-}

-- The first stage of testing is just that the transformation happens at all (and doesn't fail at
-- build time). That only requires that the call to `Categorifier.Categorify.expression` happens in
-- a module imported somewhere in this binary. Referring to them in `main` is mostly just to not
-- have linters complain that we have an unused import.

mkTestTerms defaultTestTerms [TestCategory ''Term [t|Term|] "term" CheckCompileOnly]
  -- core
  . HCons1 (TestCases (const [([t|Word8|], pure ([|genIntegralBounded|], [|show|]))]))
  . HCons1 (TestCases (const [([t|Word8|], pure ([|genIntegralBounded|], [|show|]))]))
  . HCons1
    ( TestCases
        ( const
            [ ( ([t|Int64|], [t|Word8|]),
                pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|])
              )
            ]
        )
    )
  . HCons1 (TestCases (const [([t|Word8|], pure ([|genIntegralBounded|], [|show|]))]))
  . HCons1 (TestCases (const [([t|Word8|], pure ([|genIntegralBounded|], [|show|]))]))
  . HCons1
    ( TestCases
        (const [([t|Word8|], pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))])
    )
  . HCons1
    ( TestCases
        (const [([t|Word8|], pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))])
    )
  . HCons1
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
  . HCons1 (TestCases (const []))
  . HCons1
    ( TestCases
        ( const
            [ ( [t|Word8|],
                pure ([|Gen.choice [const <$> genIntegralBounded, pure id]|], [|const "<function>"|])
              )
            ]
        )
    )
  . HCons1
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
  . HCons1
    ( TestCases
        (const [([t|Word8|], pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))])
    )
  . HCons1
    ( TestCases
        (const [([t|Word8|], pure ([|Pair <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))])
    )
  -- base
  . HCons1 (TestCases (const [([t|Word8|], pure ([|genIntegralBounded|], [|show|]))]))
  . HCons1
    ( TestCases
        ( const
            [ ( ([t|Int64|], [t|Word8|]),
                pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|])
              )
            ]
        )
    )
  . HCons1
    ( TestCases
        ( const
            [ ( ([t|Word8|], [t|Word8|]),
                pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|])
              )
            ]
        )
    )
  . HCons1
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
  . HCons1
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
  . HCons1
    ( TestCases
        ( const
            [ ( ([t|Word8|], [t|Int64|]),
                pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|])
              )
            ]
        )
    )
  . HCons1 (TestCases (const [(([t|Int64|], [t|Word8|]), pure ([|genIntegralBounded|], [|show|]))]))
  . HCons1
    ( TestCases
        ( const
            [ ( ([t|Int64|], [t|Word8|]),
                pure
                  ([|Gen.choice [Left <$> genIntegralBounded, Right <$> genIntegralBounded]|], [|show|])
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
                  ([|Gen.choice [Left <$> genIntegralBounded, Right <$> genIntegralBounded]|], [|show|])
              )
            ]
        )
    )
  . HCons1 (TestCases (const [([t|Word8|], pure ([|genIntegralBounded|], [|show|]))]))
  . HCons1 (TestCases (const [([t|Word8|], pure ([|genIntegralBounded|], [|show|]))]))
  . HCons1
    ( TestCases
        ( const
            [ ( [t|Double|],
                pure ([|(,,) <$> genIntegralBounded <*> genFloating <*> genFloating|], [|show|])
              )
            ]
        )
    )
  . HCons1 (TestCases (const [])) -- no support for `**` in ConCat
  . HCons1 (TestCases (const [])) -- no support for `acos` in ConCat
  . HCons1 (TestCases (const [])) -- no support for `acosh` in ConCat
  . HCons1 (TestCases (const [])) -- no support for `asin` in ConCat
  . HCons1 (TestCases (const [])) -- no support for `asinh` in ConCat
  . HCons1 (TestCases (const [])) -- no support for `atan` in ConCat
  . HCons1 (TestCases (const [])) -- no support for `atanh` in ConCat
  . HCons1 (TestCases (const [])) -- no support for `cos` in ConCat
  . HCons1 (TestCases (const [])) -- no support for `cosh` in ConCat
  . HCons1 (TestCases (const [])) -- no support for `double2Float` in ConCat
  . HCons1 (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HCons1 (TestCases (const [])) -- no support for `float2Double` in ConCat
  . HCons1 (TestCases (const [])) -- no support for `isDenormalized` in ConCat
  . HCons1 (TestCases (const [])) -- no support for `isInfinite` in ConCat
  . HCons1 (TestCases (const [])) -- no support for `isNaN` in ConCat
  . HCons1 (TestCases (const [])) -- no support for `isNegativeZero` in ConCat
  . HCons1 (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HCons1 (TestCases (const [((), pure ([|genFloating|], [|show|]))]))
  . HCons1 (TestCases (const [((), pure ([|(,) <$> genFloating <*> genFloating|], [|show|]))]))
  . HCons1 (TestCases (const [])) -- no support for `sin` in ConCat
  . HCons1 (TestCases (const [])) -- no support for `sinh` in ConCat
  . HCons1 (TestCases (const [])) -- no support for `sqrt` in ConCat
  . HCons1 (TestCases (const [])) -- no support for `sqrtDouble` in ConCat
  . HCons1 (TestCases (const [])) -- no support for `tan` in ConCat
  . HCons1 (TestCases (const [])) -- no support for `tanh` in ConCat
  . HCons1 (TestCases (const [((), pure ([|(,) <$> genFloating <*> genFloating|], [|show|]))]))
  . HCons1
    (TestCases (const [((), pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]))
  . HCons1
    (TestCases (const [((), pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]))
  . HCons1
    ( TestCases
        (const [([t|Int64|], pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))])
    )
  . HCons1
    ( TestCases
        (const [([t|Int64|], pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))])
    )
  . HCons1
    ( TestCases
        (const [([t|Int64|], pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))])
    )
  . HCons1
    ( TestCases
        (const [([t|Int64|], pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))])
    )
  . HCons1
    ( TestCases
        (const [([t|Int64|], pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))])
    )
  . HCons1
    ( TestCases
        (const [([t|Int64|], pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))])
    )
  . HCons1 (TestCases (const [])) -- no support for `compare` in ConCat
  . HCons1 (TestCases (const [((), pure ([|(,) <$> genFloating <*> genFloating|], [|show|]))]))
  . HCons1
    (TestCases (const [([t|Double|], pure ([|(,) <$> genFloating <*> genFloating|], [|show|]))]))
  . HCons1
    (TestCases (const [([t|Double|], pure ([|(,) <$> genFloating <*> genFloating|], [|show|]))]))
  . HCons1 (TestCases (const [((), pure ([|genIntegralBounded|], [|show|]))]))
  . HCons1
    (TestCases (const [([t|Double|], pure ([|(,) <$> genFloating <*> genFloating|], [|show|]))]))
  . HCons1
    (TestCases (const [([t|Double|], pure ([|(,) <$> genFloating <*> genFloating|], [|show|]))]))
  . HCons1
    (TestCases (const [([t|Double|], pure ([|(,) <$> genFloating <*> genFloating|], [|show|]))]))
  . HCons1 (TestCases (const [])) -- no support for `quot` in ConCat
  . HCons1 (TestCases (const [])) -- no support for `realToFrac` in ConCat
  . HCons1 (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HCons1 (TestCases (const [])) -- no support for `rem` in ConCat
  . HCons1
    ( TestCases
        ( const
            [ ( [t|Word8|],
                pure
                  ([|(,) <$> genIntegralBounded <*> Gen.integral (Range.linear 1 maxBound)|], [|show|])
              )
            ]
        )
    )
  . HCons1
    ( TestCases
        ( const
            [ ( [t|Word8|],
                pure
                  ([|(,) <$> genIntegralBounded <*> Gen.integral (Range.linear 1 maxBound)|], [|show|])
              )
            ]
        )
    )
  . HCons1
    (TestCases (const [([t|Double|], pure ([|(,) <$> genFloating <*> genFloating|], [|show|]))]))
  . HCons1
    (TestCases (const [((), pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]))
  . HCons1
    (TestCases (const [((), pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))]))
  . HCons1 (TestCases (const [])) -- no support for `atan2` in ConCat
  . HCons1 (TestCases (const [])) -- no support for `abs` in ConCat
  . HCons1 (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HCons1 (TestCases (const [])) -- no support for `signum` in ConCat
  . HCons1 (TestCases (const [])) -- ConCat only supports `^` for `Int`
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
  . HCons1 (TestCases (const [])) -- no support for `<>` in ConCat
  . HCons1 (TestCases (const [])) -- no support for `mappend` in ConCat
  . HCons1 (TestCases (const [])) -- no support for `++` in ConCat
  . HCons1 (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HCons1 (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HCons1 (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HCons1
    ( TestCases
        (const [([t|Word8|], pure ([|Gen.string Range.linearBounded Gen.unicodeAll|], [|show|]))])
    )
  . HCons1 (TestCases (const [(([t|Int64|], [t|Word8|]), pure ([|genIntegralBounded|], [|show|]))]))
  . HCons1 (TestCases (const [(([t|Int64|], [t|Word8|]), pure ([|genIntegralBounded|], [|show|]))]))
  . HCons1
    ( TestCases
        ( const
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
  . HCons1
    ( TestCases
        ( const
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
  . HCons1
    ( TestCases
        ( const
            [ ( ([t|Word8|], [t|Bool|]),
                pure ([|(,) <$> genIntegralBounded <*> Gen.bool|], [|show|])
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
                        <*> (Pair <$> genIntegralBounded <*> genIntegralBounded)
                      |],
                    [|show . snd|]
                  )
              )
            ]
        )
    )
  . HCons1
    ( TestCases
        (const [([t|Word8|], pure ([|Pair <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))])
    )
  . HCons1
    ( TestCases
        ( const
            [ ( ([t|Pair|], [t|Word8|]),
                pure ([|Pair <$> genIntegralBounded <*> genIntegralBounded|], [|show|])
              )
            ]
        )
    )
  . HCons1
    ( TestCases
        (const [([t|Word8|], pure ([|Pair <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))])
    )
  . HCons1 (TestCases (const [([t|Word8|], pure ([|genIntegralBounded|], [|show|]))]))
  . HCons1
    ( TestCases
        (const [([t|Word8|], pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))])
    )
  . HCons1
    ( TestCases
        ( const
            [([t|Word8|], pure ([|Gen.list (Range.exponential 1 1024) genIntegralBounded|], [|show|]))]
        )
    )
  . HCons1 (TestCases (const [([t|Word8|], pure ([|genIntegralBounded|], [|show|]))]))
  . HCons1 (TestCases (const [])) -- no support for `<*>` in ConCat
  . HCons1 (TestCases (const [])) -- no support for `apRep` in ConCat
  . HCons1 (TestCases (const [])) -- no support for `liftA2` in ConCat
  . HCons1 (TestCases (const [])) -- no support for `>>=` in ConCat
  . HCons1 (TestCases (const [])) -- no support for `bindRep` in ConCat
  . HCons1
    ( TestCases
        ( const
            [ ( ([t|Word8|], [t|Bool|]),
                pure ([|(,) <$> genIntegralBounded <*> Gen.bool|], [|show|])
              )
            ]
        )
    )
  . HCons1
    ( TestCases
        ( const
            [ ( ([t|Word8|], [t|Bool|]),
                pure ([|(,) <$> genIntegralBounded <*> Gen.bool|], [|show|])
              )
            ]
        )
    )
  . HCons1
    ( TestCases
        ( const
            [ ( ([t|Identity|], [t|Word8|]),
                pure ([|(,) <$> genIntegralBounded <*> pure ()|], [|show|])
              ),
              ( ([t|One|], [t|Word8|]),
                pure ([|(,) <$> (One <$> genIntegralBounded) <*> pure ()|], [|show|])
              )
            ]
        )
    )
  . HCons1
    ( TestCases
        ( const
            [ ( ([t|Identity|], [t|Word8|]),
                pure ([|const <$> genIntegralBounded|], [|("\\() -> " <>) . show . ($ ())|])
              ),
              ( ([t|One|], [t|Word8|]),
                pure ([|const <$> genIntegralBounded|], [|("\\() -> " <>) . show . ($ ())|])
              )
            ]
        )
    )
  . HCons1 (TestCases (const [])) -- only Hask and C.Cat have TraversableCat instances
  . HCons1 (TestCases (const [])) -- only Hask and C.Cat have TraversableCat instances
  . HCons1 (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HCons1 (TestCases (const []))
  . HCons1 (TestCases (const []))
  . HCons1 (TestCases (const []))
  $ HNil1

main :: IO ()
main = bool exitFailure exitSuccess . and =<< allTestTerms
