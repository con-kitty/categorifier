{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
-- To avoid turning @if then else@ into `ifThenElse`.
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | See @Test/Cat/ConCat/Main.hs@ for copious notes on the testing situation here.
module Main
  ( main,
  )
where

import Categorifier.Hedgehog (genFloating, genIntegralBounded)
import Categorifier.Test.ConCat.Instances ()
import Categorifier.Test.ConCatExtensions.Instances ()
import Categorifier.Test.HList (HMap1 (..))
import Categorifier.Test.Hask (Hask (..))
import qualified Categorifier.Test.LinearBase as LinearBase
import Categorifier.Test.Term (Term (..))
import Categorifier.Test.Tests
  ( TestCases (..),
    TestCategory (..),
    TestStrategy (..),
    builtinTestCategories,
    mkTestTerms,
  )
import qualified Control.Functor.Linear
import Data.Bool (bool)
import qualified Data.Either.Linear
import Data.Functor.Identity (Identity (..))
import qualified Data.Functor.Linear
import Data.Pointed (Pointed (..))
import Data.Proxy (Proxy (..))
import qualified Data.Replicator.Linear
import qualified Data.V.Linear
import GHC.Int (Int64)
import GHC.TypeNats (KnownNat)
import GHC.Word (Word8)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Prelude.Linear
import System.Exit (exitFailure, exitSuccess)

-- For @NoRebindableSyntax@
{-# ANN module ("HLint: ignore Avoid restricted integration" :: String) #-}

instance Pointed Data.Replicator.Linear.Replicator where
  point = Data.Functor.Linear.pure

instance (KnownNat n) => Pointed (Data.V.Linear.V n) where
  point = Data.Functor.Linear.pure

mkTestTerms
  LinearBase.testTerms
  --               name   type      prefix       strategy
  ( [ TestCategory ''Term [t|Term|] "term" CheckCompileOnly,
      TestCategory ''Hask [t|Hask|] "hask" $ ComputeFromInput [|runHask|]
    ]
      <> builtinTestCategories
  )
  -- linear-base
  . HInsert1
    (Proxy @"LinearAbs")
    (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1
    (Proxy @"LinearAnd")
    (TestCases (const [((), pure ([|(,) <$> Gen.bool <*> Gen.bool|], [|show|]))]))
  . HInsert1
    (Proxy @"LinearAp")
    ( TestCases
        ( const
            [ ( ([t|Identity|], [t|Int64|], [t|Int64|]),
                pure
                  ( [|(,) <$> pure (Control.Functor.Linear.pure Prelude.Linear.id) <*> (pure <$> genIntegralBounded)|],
                    [|show . snd|]
                  )
              )
            ]
        )
    )
  . HInsert1
    (Proxy @"LinearApOpControl")
    ( TestCases
        ( const
            [ ( ([t|Identity|], [t|Int64|], [t|Int64|]),
                pure
                  ( [|(,) <$> pure (Control.Functor.Linear.pure Prelude.Linear.id) <*> (pure <$> genIntegralBounded)|],
                    [|show . snd|]
                  )
              )
            ]
        )
    )
  . HInsert1
    (Proxy @"LinearApOpData")
    ( TestCases
        ( const
            [ ( ([t|Data.V.Linear.V 9|], [t|Int64|], [t|Int64|]),
                pure
                  ( [|(,) <$> pure (Data.Functor.Linear.pure Prelude.Linear.id) <*> sequenceA (pure genIntegralBounded)|],
                    [|show . snd|]
                  )
              )
            ]
        )
    )
  . HInsert1
    (Proxy @"LinearApOpReplicator")
    ( TestCases
        ( \arrow ->
            if arrow `elem` [''Hask, ''(->)]
              then [] -- No @`Applicative` `Replicator`@
              else
                [ ( ([t|Int64|], [t|Int64|]),
                    pure ([|(,) <$> pure (Control.Functor.Linear.pure Prelude.Linear.id) <*> fmap pure genIntegralBounded|], [|show|])
                  )
                ]
        )
    )
  . HInsert1
    (Proxy @"LinearApOpV")
    ( TestCases
        ( const
            [ ( ([t|Int64|], [t|Int64|]),
                pure
                  ( [|(,) <$> pure (Data.Functor.Linear.pure Prelude.Linear.id) <*> sequenceA (pure genIntegralBounded)|],
                    [|show . snd|]
                  )
              )
            ]
        )
    )
  . HInsert1
    (Proxy @"LinearAppend")
    (TestCases (const []))
  -- top-level @AxiomInstCo@, avoidable with `class Empty` hack
  --   ( TestCases
  --       ( const
  --           [ ( [t|[Word8]|],
  --               pure
  --                 ( [|
  --                     (,)
  --                       <$> Gen.list (Range.linear 0 100) genIntegralBounded
  --                       <*> Gen.list (Range.linear 0 100) genIntegralBounded
  --                     |],
  --                   [|show|]
  --                 )
  --             )
  --           ]
  --       )
  --   )
  . HInsert1
    (Proxy @"LinearAppendList")
    ( TestCases
        ( const
            [ ( [t|Word8|],
                pure
                  ( [|
                      (,)
                        <$> Gen.list (Range.linear 0 100) genIntegralBounded
                        <*> Gen.list (Range.linear 0 100) genIntegralBounded
                      |],
                    [|show|]
                  )
              )
            ]
        )
    )
  . HInsert1
    (Proxy @"LinearApply")
    ( TestCases
        ( const
            [ ( ([t|Word8|], [t|Identity Word8|]),
                pure ([|(Control.Functor.Linear.pure,) <$> genIntegralBounded|], [|show . snd|])
              )
            ]
        )
    )
  . HInsert1
    (Proxy @"LinearBind")
    ( TestCases
        ( const
            [ ( ([t|Identity|], [t|Word8|], [t|Word8|]),
                pure ([|(\x -> (x, Control.Functor.Linear.pure)) . Identity <$> genIntegralBounded|], [|show . fst|])
              )
            ]
        )
    )
  . HInsert1
    (Proxy @"LinearCoerce")
    (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1
    (Proxy @"LinearCompare")
    (TestCases (const [([t|Double|], pure ([|(,) <$> genFloating <*> genFloating|], [|show|]))]))
  . HInsert1
    (Proxy @"LinearCompose")
    ( TestCases
        (const [(([t|Identity|], [t|Identity|], [t|Int64|]), pure ([|genIntegralBounded|], [|show|]))])
    )
  . HInsert1
    (Proxy @"LinearConst")
    ( TestCases
        ( const
            [ ( ([t|Int64|], [t|Word8|]),
                pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|])
              )
            ]
        )
    )
  . HInsert1
    (Proxy @"LinearCurry")
    ( TestCases
        ( const
            [ ( ([t|Word8|], [t|Bool|]),
                pure ([|(,) <$> genIntegralBounded <*> Gen.bool|], [|show . snd|])
              )
            ]
        )
    )
  . HInsert1
    (Proxy @"LinearEither")
    ( TestCases
        ( const
            [ ( [t|Int64|],
                pure
                  ([|Gen.choice [Left <$> genIntegralBounded, Right <$> genIntegralBounded]|], [|show|])
              )
            ]
        )
    )
  . HInsert1
    (Proxy @"LinearEqual")
    ( TestCases
        (const [([t|Int64|], pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))])
    )
  . HInsert1
    (Proxy @"LinearFmapControl")
    ( TestCases
        ( const
            [ ( ([t|Identity|], [t|Either Word8 Word8|], [t|Word8|]),
                pure
                  ( [|
                      (,)
                        <$> pure (Data.Either.Linear.either Prelude.Linear.id Prelude.Linear.id)
                        <*> (pure <$> Gen.choice [Left <$> genIntegralBounded, Right <$> genIntegralBounded])
                      |],
                    [|show . snd|]
                  )
              )
            ]
        )
    )
  . HInsert1
    (Proxy @"LinearFmapData")
    (TestCases (const []))
  -- specialized instance with no unfolding, avoidable with `class Empty` hack
  --   ( TestCases
  --       ( const
  --           [ ( ([t|[]|], [t|Either Word8 Word8|], [t|Word8|]),
  --               pure
  --                 ( [|
  --                     (,)
  --                       <$> pure (Data.Either.Linear.either Prelude.Linear.id Prelude.Linear.id)
  --                       <*> Gen.list (Range.exponential 1 1024) (Gen.choice [Left <$> genIntegralBounded, Right <$> genIntegralBounded])
  --                     |],
  --                   [|show . snd|]
  --                 )
  --             )
  --           ]
  --       )
  --   )
  . HInsert1
    (Proxy @"LinearFmapOpControl")
    ( TestCases
        ( const
            [ ( ([t|Identity|], [t|Either Word8 Word8|], [t|Word8|]),
                pure
                  ( [|
                      (,)
                        <$> pure (Data.Either.Linear.either Prelude.Linear.id Prelude.Linear.id)
                        <*> (pure <$> Gen.choice [Left <$> genIntegralBounded, Right <$> genIntegralBounded])
                      |],
                    [|show . snd|]
                  )
              )
            ]
        )
    )
  . HInsert1
    (Proxy @"LinearFmapOpData")
    ( TestCases
        ( const
            [ ( ([t|[]|], [t|Either Word8 Word8|], [t|Word8|]),
                pure
                  ( [|
                      (,)
                        <$> pure (Data.Either.Linear.either Prelude.Linear.id Prelude.Linear.id)
                        <*> Gen.list (Range.exponential 1 1024) (Gen.choice [Left <$> genIntegralBounded, Right <$> genIntegralBounded])
                      |],
                    [|show . snd|]
                  )
              )
            ]
        )
    )
  . HInsert1
    (Proxy @"LinearFromInteger")
    (TestCases (const []))
  -- top-level @AxiomInstCo@, avoidable with `class Empty` hack
  --   ( TestCases
  --       ( const
  --           [ ( [t|Double|],
  --               pure
  --                 ( [|
  --                     Gen.integral $
  --                       Range.linearFrom
  --                         0
  --                         (toInteger (minBound :: Int64) - 1)
  --                         (toInteger (maxBound :: Int64) + 1)
  --                     |],
  --                   [|show|]
  --                 )
  --             )
  --           ]
  --       )
  --   )
  . HInsert1
    (Proxy @"LinearFst")
    ( TestCases
        ( const
            [ ( ([t|Word8|], [t|Word8|]),
                pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|])
              )
            ]
        )
    )
  . HInsert1
    (Proxy @"LinearGe")
    ( TestCases
        (const [([t|Int64|], pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))])
    )
  . HInsert1
    (Proxy @"LinearGt")
    ( TestCases
        (const [([t|Int64|], pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))])
    )
  . HInsert1
    (Proxy @"LinearLe")
    ( TestCases
        (const [([t|Int64|], pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))])
    )
  . HInsert1
    (Proxy @"LinearLiftA2Control")
    (TestCases (const [(([t|Identity|], [t|Int64|], [t|Int64|]), pure ([|(,) <$> sequenceA (pure genIntegralBounded) <*> sequenceA (pure genIntegralBounded)|], [|show|]))]))
  . HInsert1
    (Proxy @"LinearLiftA2Data")
    (TestCases (const [(([t|Data.V.Linear.V 9|], [t|Int64|], [t|Int64|]), pure ([|(,) <$> sequenceA (pure genIntegralBounded) <*> sequenceA (pure genIntegralBounded)|], [|show|]))]))
  . HInsert1
    (Proxy @"LinearLt")
    ( TestCases
        (const [([t|Int64|], pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))])
    )
  . HInsert1
    (Proxy @"LinearMapList")
    ( TestCases
        ( const
            [ ( ([t|Either Word8 Word8|], [t|Word8|]),
                pure
                  ( [|
                      (,)
                        <$> pure (Data.Either.Linear.either Prelude.Linear.id Prelude.Linear.id)
                        <*> Gen.list (Range.exponential 1 1024) (Gen.choice [Left <$> genIntegralBounded, Right <$> genIntegralBounded])
                      |],
                    [|show . snd|]
                  )
              )
            ]
        )
    )
  . HInsert1 (Proxy @"LinearMapMutableArray") (TestCases (const [])) -- Can't extract an Array value
  . HInsert1
    (Proxy @"LinearMapMutableArrayUnlifted")
    (TestCases (const [])) -- No unlifted arguments
  . HInsert1
    (Proxy @"LinearMapReplicator")
    ( TestCases
        ( \arrow ->
            if arrow `elem` [''Hask, ''(->)]
              then [] -- No @`Functor` `Replicator`@
              else
                [ ( ([t|Either Word8 Word8|], [t|Word8|]),
                    pure
                      ( [|
                          (,)
                            <$> pure (Data.Either.Linear.either Prelude.Linear.id Prelude.Linear.id)
                            <*> (pure <$> Gen.choice [Left <$> genIntegralBounded, Right <$> genIntegralBounded])
                          |],
                        [|show|]
                      )
                  )
                ]
        )
    )
  . HInsert1
    (Proxy @"LinearMapV")
    ( TestCases
        ( const
            [ ( ([t|Either Word8 Word8|], [t|Word8|]),
                pure
                  ( [|
                      (,)
                        <$> pure (Data.Either.Linear.either Prelude.Linear.id Prelude.Linear.id)
                        <*> sequenceA (pure $ Gen.choice [Left <$> genIntegralBounded, Right <$> genIntegralBounded])
                      |],
                    [|show . snd|]
                  )
              )
            ]
        )
    )
  . HInsert1
    (Proxy @"LinearMappend")
    ( TestCases
        ( const
            [ ( [t|[Word8]|],
                pure
                  ( [|
                      (,)
                        <$> Gen.list (Range.linear 0 100) genIntegralBounded
                        <*> Gen.list (Range.linear 0 100) genIntegralBounded
                      |],
                    [|show|]
                  )
              )
            ]
        )
    )
  . HInsert1
    (Proxy @"LinearMax")
    ( TestCases
        ( const
            [ ([t|Double|], pure ([|(,) <$> genFloating <*> genFloating|], [|show|])),
              ([t|Int64|], pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))
            ]
        )
    )
  . HInsert1
    (Proxy @"LinearMin")
    ( TestCases
        ( const
            [ ([t|Double|], pure ([|(,) <$> genFloating <*> genFloating|], [|show|])),
              ([t|Int64|], pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))
            ]
        )
    )
  . HInsert1
    (Proxy @"LinearMinus")
    (TestCases (const [([t|Double|], pure ([|(,) <$> genFloating <*> genFloating|], [|show|]))]))
  . HInsert1
    (Proxy @"LinearNegate")
    (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1 (Proxy @"LinearNot") (TestCases (const [((), pure ([|Gen.bool|], [|show|]))]))
  . HInsert1
    (Proxy @"LinearNotEqual")
    ( TestCases
        (const [([t|Int64|], pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|]))])
    )
  . HInsert1
    (Proxy @"LinearOr")
    (TestCases (const [((), pure ([|(,) <$> Gen.bool <*> Gen.bool|], [|show|]))]))
  . HInsert1
    (Proxy @"LinearPlus")
    (TestCases (const []))
  -- specialized instance with no unfolding, avoidable with `class Empty` hack
  --   (TestCases (const [([t|Double|], pure ([|(,) <$> genFloating <*> genFloating|], [|show|]))]))
  . HInsert1
    (Proxy @"LinearPureControl")
    ( TestCases
        ( const
            [ (([t|Identity|], [t|Double|]), pure ([|genFloating|], [|show|])),
              (([t|Identity|], [t|Word8|]), pure ([|genIntegralBounded|], [|show|]))
            ]
        )
    )
  . HInsert1
    (Proxy @"LinearPureData")
    (TestCases (const []))
  -- ( TestCases
  --     ( const
  --         [ (([t|Data.V.Linear.V 9|], [t|Double|]), pure ([|genFloating|], [|show|])),
  --           (([t|Identity|], [t|Word8|]), pure ([|genIntegralBounded|], [|show|]))
  --         ]
  --     )
  -- )
  . HInsert1
    (Proxy @"LinearPureReplicator")
    ( TestCases
        ( \arrow ->
            if arrow `elem` [''Hask, ''(->)]
              then [] -- No `Eq` on `Replicator`
              else
                [ ([t|Double|], pure ([|genFloating|], [|show|])),
                  ([t|Word8|], pure ([|genIntegralBounded|], [|show|]))
                ]
        )
    )
  . HInsert1
    (Proxy @"LinearPureV")
    (TestCases (const []))
  -- ( TestCases
  --     ( const
  --         [ ([t|Double|], pure ([|genFloating|], [|show|])),
  --           ([t|Word8|], pure ([|genIntegralBounded|], [|show|]))
  --         ]
  --     )
  -- )
  . HInsert1
    (Proxy @"LinearReturn")
    (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1
    (Proxy @"LinearSequence")
    ( TestCases
        ( const
            [ ( ([t|Maybe|], [t|Identity|], [t|Word8|]),
                pure ([|Gen.maybe $ pure <$> genIntegralBounded|], [|show|])
              )
            ]
        )
    )
  . HInsert1
    (Proxy @"LinearSequenceA")
    ( TestCases
        ( const
            [ ( ([t|Maybe|], [t|Identity|], [t|Word8|]),
                pure ([|Gen.maybe $ pure <$> genIntegralBounded|], [|show|])
              )
            ]
        )
    )
  . HInsert1
    (Proxy @"LinearSignum")
    (TestCases (const [([t|Double|], pure ([|genFloating|], [|show|]))]))
  . HInsert1
    (Proxy @"LinearSnd")
    ( TestCases
        ( const
            [ ( ([t|Word8|], [t|Word8|]),
                pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|])
              )
            ]
        )
    )
  . HInsert1
    (Proxy @"LinearSumList")
    ( TestCases
        (const [([t|Int|], pure ([|Gen.list (Range.linear 0 100) genIntegralBounded|], [|show|]))])
    )
  . HInsert1
    (Proxy @"LinearSwap")
    ( TestCases
        ( const
            [ ( ([t|Word8|], [t|Int64|]),
                pure ([|(,) <$> genIntegralBounded <*> genIntegralBounded|], [|show|])
              )
            ]
        )
    )
  . HInsert1
    (Proxy @"LinearTimes")
    (TestCases (const []))
  -- specialized instance with no unfolding, avoidable with `class Empty` hack
  --   (TestCases (const [([t|Double|], pure ([|(,) <$> genFloating <*> genFloating|], [|show|]))]))
  . HInsert1
    (Proxy @"LinearTraverse")
    ( TestCases
        ( const
            [ ( ([t|Maybe|], [t|Identity|], [t|Word8|]),
                pure
                  ( [|(,) <$> pure Control.Functor.Linear.pure <*> (Gen.maybe genIntegralBounded)|],
                    [|show . snd|]
                  )
              )
            ]
        )
    )
  . HInsert1
    (Proxy @"LinearTraverseList")
    ( TestCases
        ( const
            [ ( ([t|Identity|], [t|Word8|]),
                pure
                  ( [|(,) <$> pure Control.Functor.Linear.pure <*> Gen.list (Range.linear 0 100) genIntegralBounded|],
                    [|show . snd|]
                  )
              )
            ]
        )
    )
  . HInsert1
    (Proxy @"LinearUncurry")
    ( TestCases
        ( const
            [ ( ([t|Word8|], [t|Bool|]),
                pure ([|(,) <$> genIntegralBounded <*> Gen.bool|], [|show|])
              )
            ]
        )
    )
  $ HEmpty1

main :: IO ()
main = bool exitFailure exitSuccess . and =<< allTestTerms
