{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
-- To avoid having to specify massive HList types.
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
-- To avoid having to specify massive HList types.
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Categorifier.Test.LinearBase
  ( testTerms,
  )
where

import Categorifier.Test.Data (Pair (..))
import Categorifier.Test.HList (HMap1 (..))
import Categorifier.Test.TH (mkBinaryTestConfig, mkExprTest, mkUnaryTestConfig)
import Categorifier.Test.Tests (TestTerms, insertTest)
import qualified Control.Functor.Linear
import qualified Data.Array.Mutable.Linear
import qualified Data.Array.Mutable.Unlifted.Linear
import qualified Data.Bool.Linear
import qualified Data.Either.Linear
import Data.Functor.Identity (Identity (..))
import qualified Data.Functor.Linear
import qualified Data.List.Linear
import qualified Data.Monoid.Linear
import qualified Data.Num.Linear
import qualified Data.Ord.Linear
import Data.Proxy (Proxy (..))
import qualified Data.Replicator.Linear
import qualified Data.Replicator.Linear.Internal
import qualified Data.Replicator.Linear.Internal.ReplicationStream
import Data.Semigroup (Sum (..))
import qualified Data.Tuple.Linear
import qualified Data.V.Linear
import qualified Data.V.Linear.Internal
import qualified Data.Vector as Vector
import GHC.Int (Int16, Int32, Int64, Int8)
import GHC.TypeLits (KnownNat)
import GHC.Word (Word16, Word32, Word64, Word8)
import qualified Prelude.Linear
import qualified Unsafe.Linear

testTerms :: TestTerms _
testTerms =
  insertTest (Proxy @"LinearAbs") mkUnaryTestConfig (\a -> (a, a)) [|Data.Num.Linear.abs|]
    . insertTest
      (Proxy @"LinearAnd")
      mkBinaryTestConfig
      (\() -> ([t|Bool|], [t|Bool %1 -> Bool|]))
      [|(Data.Bool.Linear.&&)|]
    . insertTest
      (Proxy @"LinearAp")
      mkBinaryTestConfig
      (\(f, a, b) -> ([t|$f ($a %1 -> $b)|], [t|$f $a %1 -> $f $b|]))
      [|Control.Functor.Linear.ap|]
    . insertTest
      (Proxy @"LinearApOpControl")
      mkBinaryTestConfig
      (\(f, a, b) -> ([t|$f ($a %1 -> $b)|], [t|$f $a %1 -> $f $b|]))
      [|(Control.Functor.Linear.<*>)|]
    . insertTest
      (Proxy @"LinearApOpData")
      mkBinaryTestConfig
      (\(f, a, b) -> ([t|$f ($a %1 -> $b)|], [t|$f $a %1 -> $f $b|]))
      [|(Data.Functor.Linear.<*>)|]
    . insertTest
      (Proxy @"LinearApOpReplicator")
      mkBinaryTestConfig
      (\(a, b) -> ([t|Data.Replicator.Linear.Replicator ($a %1 -> $b)|], [t|Data.Replicator.Linear.Replicator $a %1 -> Data.Replicator.Linear.Replicator $b|]))
      [|(Data.Replicator.Linear.<*>)|]
    . insertTest
      (Proxy @"LinearApOpV")
      mkBinaryTestConfig
      (\(a, b) -> ([t|Data.V.Linear.V 9 ($a %1 -> $b)|], [t|Data.V.Linear.V 9 $a %1 -> Data.V.Linear.V 9 $b|]))
      [|(Data.V.Linear.<*>)|]
    . insertTest
      (Proxy @"LinearAppend")
      mkBinaryTestConfig
      (\a -> (a, [t|$a %1 -> $a|]))
      [|(Data.Monoid.Linear.<>)|]
    . insertTest
      (Proxy @"LinearAppendList")
      mkBinaryTestConfig
      (\a -> ([t|[$a]|], [t|[$a] %1 -> [$a]|]))
      [|(Data.List.Linear.++)|]
    . insertTest
      (Proxy @"LinearApply")
      mkBinaryTestConfig
      (\(a, b) -> ([t|$a %1 -> $b|], [t|$a %1 -> $b|]))
      [|(Prelude.Linear.$)|]
    . insertTest
      (Proxy @"LinearBind")
      mkBinaryTestConfig
      (\(f, a, b) -> ([t|$f $a|], [t|($a %1 -> $f $b) %1 -> $f $b|]))
      [|(Control.Functor.Linear.>>=)|]
    . insertTest
      (Proxy @"LinearCoerce")
      mkUnaryTestConfig
      (\a -> (a, [t|Sum $a|]))
      [|Unsafe.Linear.coerce|]
    . insertTest
      (Proxy @"LinearCompare")
      mkBinaryTestConfig
      (\a -> (a, [t|$a %1 -> Ordering|]))
      [|Data.Ord.Linear.compare|]
    . insertTest
      (Proxy @"LinearCompose")
      mkUnaryTestConfig
      (\(f, g, a) -> (a, [t|$f ($g $a)|]))
      [|Control.Functor.Linear.pure Prelude.Linear.. Control.Functor.Linear.pure|]
    . insertTest
      (Proxy @"LinearConst")
      mkBinaryTestConfig
      (\(a, b) -> (a, [t|$b -> $a|]))
      [|Prelude.Linear.const|]
    . insertTest
      (Proxy @"LinearCurry")
      mkBinaryTestConfig
      (\(a, b) -> (a, [t|$b %1 -> $a|]))
      [|Data.Tuple.Linear.curry Data.Tuple.Linear.fst|]
    . insertTest
      (Proxy @"LinearEither")
      mkUnaryTestConfig
      (\a -> ([t|Either $a $a|], a))
      [|Data.Either.Linear.either Prelude.Linear.id Prelude.Linear.id|]
    . insertTest
      (Proxy @"LinearEqual")
      mkBinaryTestConfig
      (\a -> (a, [t|$a %1 -> Bool|]))
      [|(Data.Ord.Linear.==)|]
    . insertTest
      (Proxy @"LinearFmapControl")
      mkBinaryTestConfig
      (\(f, a, b) -> ([t|$a %1 -> $b|], [t|$f $a %1 -> $f $b|]))
      [|Control.Functor.Linear.fmap|]
    . insertTest
      (Proxy @"LinearFmapData")
      mkBinaryTestConfig
      (\(f, a, b) -> ([t|$a %1 -> $b|], [t|$f $a %1 -> $f $b|]))
      [|Data.Functor.Linear.fmap|]
    . insertTest
      (Proxy @"LinearFmapOpControl")
      mkBinaryTestConfig
      (\(f, a, b) -> ([t|$a %1 -> $b|], [t|$f $a %1 -> $f $b|]))
      [|(Control.Functor.Linear.<$>)|]
    . insertTest
      (Proxy @"LinearFmapOpData")
      mkBinaryTestConfig
      (\(f, a, b) -> ([t|$a %1 -> $b|], [t|$f $a %1 -> $f $b|]))
      [|(Data.Functor.Linear.<$>)|]
    . insertTest
      (Proxy @"LinearFromInteger")
      mkUnaryTestConfig
      ([t|Integer|],)
      [|Data.Num.Linear.fromInteger|]
    . insertTest
      (Proxy @"LinearFst")
      mkUnaryTestConfig
      (\(a, b) -> ([t|($a, $b)|], b))
      [|Data.Tuple.Linear.fst|]
    . insertTest
      (Proxy @"LinearGe")
      mkBinaryTestConfig
      (\a -> (a, [t|$a %1 -> Bool|]))
      [|(Data.Ord.Linear.>=)|]
    . insertTest
      (Proxy @"LinearGt")
      mkBinaryTestConfig
      (\a -> (a, [t|$a %1 -> Bool|]))
      [|(Data.Ord.Linear.>)|]
    . insertTest
      (Proxy @"LinearLe")
      mkBinaryTestConfig
      (\a -> (a, [t|$a %1 -> Bool|]))
      [|(Data.Ord.Linear.<=)|]
    . insertTest
      (Proxy @"LinearLiftA2Control")
      mkBinaryTestConfig
      (\(f, a, b) -> ([t|$f $a|], [t|$f $b %1 -> $f ($a, $b)|]))
      [|Control.Functor.Linear.liftA2 (,)|]
    . insertTest
      (Proxy @"LinearLiftA2Data")
      mkBinaryTestConfig
      (\(f, a, b) -> ([t|$f $a|], [t|$f $b %1 -> $f ($a, $b)|]))
      [|Data.Functor.Linear.liftA2 (,)|]
    . insertTest
      (Proxy @"LinearLt")
      mkBinaryTestConfig
      (\a -> (a, [t|$a %1 -> Bool|]))
      [|(Data.Ord.Linear.<)|]
    . insertTest
      (Proxy @"LinearMapList")
      mkBinaryTestConfig
      (\(a, b) -> ([t|$a %1 -> $b|], [t|[$a] %1 -> [$b]|]))
      [|Data.List.Linear.map|]
    . insertTest
      (Proxy @"LinearMapMutableArray")
      mkUnaryTestConfig
      (\a -> ([t|Data.Array.Mutable.Linear.Array $a|], [t|Data.Array.Mutable.Linear.Array ($a, $a)|]))
      [|Data.Array.Mutable.Linear.map (id &&& id)|]
    . insertTest
      (Proxy @"LinearMapMutableArrayUnlifted")
      mkUnaryTestConfig
      ( \a ->
          ( [t|Data.Array.Mutable.Unlifted.Linear.Array# $a|],
            [t|Data.Array.Mutable.Unlifted.Linear.Array# ($a, $a)|]
          )
      )
      [|Data.Array.Mutable.Unlifted.Linear.map (id &&& id)|]
    . insertTest
      (Proxy @"LinearMapReplicator")
      mkBinaryTestConfig
      (\(a, b) -> ([t|$a %1 -> $b|], [t|Data.Replicator.Linear.Replicator $a %1 -> Data.Replicator.Linear.Replicator $b|]))
      [|Data.Replicator.Linear.map|]
    . insertTest
      (Proxy @"LinearMapV")
      mkBinaryTestConfig
      (\(a, b) -> ([t|$a %1 -> $b|], [t|Data.V.Linear.V 9 $a %1 -> Data.V.Linear.V 9 $b|]))
      [|Data.V.Linear.map|]
    . insertTest
      (Proxy @"LinearMappend")
      mkBinaryTestConfig
      (\a -> (a, [t|$a %1 -> $a|]))
      [|Data.Monoid.Linear.mappend|]
    . insertTest
      (Proxy @"LinearMax")
      mkBinaryTestConfig
      (\a -> (a, [t|$a %1 -> $a|]))
      [|Data.Ord.Linear.max|]
    . insertTest
      (Proxy @"LinearMin")
      mkBinaryTestConfig
      (\a -> (a, [t|$a %1 -> $a|]))
      [|Data.Ord.Linear.min|]
    . insertTest
      (Proxy @"LinearMinus")
      mkBinaryTestConfig
      (\a -> (a, [t|$a %1 -> $a|]))
      [|(Data.Num.Linear.-)|]
    . insertTest (Proxy @"LinearNegate") mkUnaryTestConfig (\a -> (a, a)) [|Data.Num.Linear.negate|]
    . insertTest
      (Proxy @"LinearNot")
      mkUnaryTestConfig
      (\() -> ([t|Bool|], [t|Bool|]))
      [|Data.Bool.Linear.not|]
    . insertTest
      (Proxy @"LinearNotEqual")
      mkBinaryTestConfig
      (\a -> (a, [t|$a %1 -> Bool|]))
      [|(Data.Ord.Linear./=)|]
    . insertTest
      (Proxy @"LinearOr")
      mkBinaryTestConfig
      (\() -> ([t|Bool|], [t|Bool %1 -> Bool|]))
      [|(Data.Bool.Linear.||)|]
    . insertTest
      (Proxy @"LinearPlus")
      mkBinaryTestConfig
      (\a -> (a, [t|$a %1 -> $a|]))
      [|(Data.Num.Linear.+)|]
    . insertTest
      (Proxy @"LinearPureControl")
      mkUnaryTestConfig
      (\(f, a) -> (a, [t|$f $a|]))
      [|Control.Functor.Linear.pure|]
    . insertTest
      (Proxy @"LinearPureData")
      mkUnaryTestConfig
      (\(f, a) -> (a, [t|$f $a|]))
      [|Data.Functor.Linear.pure|]
    . insertTest
      (Proxy @"LinearPureReplicator")
      mkUnaryTestConfig
      (\a -> (a, [t|Data.Replicator.Linear.Replicator $a|]))
      [|Data.Replicator.Linear.pure|]
    . insertTest
      (Proxy @"LinearPureV")
      mkUnaryTestConfig
      (\a -> (a, [t|Data.V.Linear.V 9 $a|]))
      [|Data.V.Linear.pure|]
    . insertTest
      (Proxy @"LinearReturn")
      mkUnaryTestConfig
      (\a -> (a, [t|Identity $a|]))
      [|Control.Functor.Linear.return|]
    . insertTest
      (Proxy @"LinearSequence")
      mkUnaryTestConfig
      (\(t, f, a) -> ([t|$t ($f $a)|], [t|$f ($t $a)|]))
      [|Data.Functor.Linear.sequence|]
    . insertTest
      (Proxy @"LinearSequenceA")
      mkUnaryTestConfig
      (\(t, f, a) -> ([t|$t ($f $a)|], [t|$f ($t $a)|]))
      [|Data.Functor.Linear.sequenceA|]
    . insertTest (Proxy @"LinearSignum") mkUnaryTestConfig (\a -> (a, a)) [|Data.Num.Linear.signum|]
    . insertTest
      (Proxy @"LinearSnd")
      mkUnaryTestConfig
      (\(a, b) -> ([t|($a, $b)|], b))
      [|Data.Tuple.Linear.snd|]
    . insertTest
      (Proxy @"LinearSumList")
      mkUnaryTestConfig
      (\a -> ([t|[$a]|], [t|$a|]))
      [|Data.List.Linear.sum|]
    . insertTest
      (Proxy @"LinearSwap")
      mkUnaryTestConfig
      (\(a, b) -> ([t|($a, $b)|], [t|($b, $a)|]))
      [|Data.Tuple.Linear.swap|]
    . insertTest
      (Proxy @"LinearTimes")
      mkBinaryTestConfig
      (\a -> (a, [t|$a %1 -> $a|]))
      [|(Data.Num.Linear.*)|]
    . insertTest
      (Proxy @"LinearTraverse")
      mkBinaryTestConfig
      (\(t, f, a) -> ([t|$a %1 -> $f $a|], [t|$t $a %1 -> $f ($t $a)|]))
      [|Data.Functor.Linear.traverse|]
    . insertTest
      (Proxy @"LinearTraverseList")
      mkBinaryTestConfig
      (\(f, a) -> ([t|$a %1 -> $f $a|], [t|[$a] %1 -> $f [$a]|]))
      [|Data.List.Linear.traverse'|]
    . insertTest
      (Proxy @"LinearUncurry")
      mkUnaryTestConfig
      (\(a, b) -> ([t|($a, $b)|], a))
      [|Data.Tuple.Linear.uncurry const|]
    $ HEmpty1
