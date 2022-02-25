{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}

-- | Defines various mappings between categorical representations and the plugin, allowing us to
--   support transformations against different type class hierarchies.
module Categorifier.Hierarchy
  ( First (..),
    Last,
    pattern Last,
    getLast,
    HaskOps (..),
    Hierarchy (..),

    -- * concrete hierarchies
    baseHierarchy,
    emptyHierarchy,
    concatOps,

    -- * building hierarchies
    findDataCon,
    findId,
    findName,
    findTyCon,
    identifier,
    mkFunctionApps,
    mkMethodApps,
    mkMethodApps',

    -- * lookup of identifier info from @base@

    --
    -- Many things are not built into the compiler, but sometimes (especially in connection with
    -- @baseMakers@ / `Categorifier.Core.PrimOp.replace`), we need to recognize these things.  The
    -- `Lookup` monad run outside the call to `Categorifier.Core.Categorify.categorify` is where we
    -- can query the compiler for this information.
    BaseIdentifiers (..),
    getBaseIdentifiers,

    -- ** Fixed-size integer constructors
    IntConstructor (..),
    getIntegerConstructors,
    intConstructorToOpTyPair,
    intConstructorToBoxer,

    -- ** `GHC.Base.getTag`
    GetTagInfo (..),
    getGetTagInfo,

    -- * Other things
    properFunTy,
    funTy,
  )
where

import Categorifier.Core.Types (CategoryStack, Lookup, MissingSymbol (..))
import Categorifier.Duoidal (Parallel (..))
import Control.Applicative (Alternative (..))
import Control.Monad ((<=<))
import Control.Monad.Trans.Except (ExceptT (..))
import CoreMonad (CoreM, getHscEnv, liftIO)
import Data.Bits (FiniteBits (..))
import Data.Functor.Identity (Identity (..))
import Data.Maybe (fromMaybe)
import Data.Monoid (Dual (..))
import DynamicLoading (lookupRdrNameInModuleForPlugins)
import GHC (Id, ModuleName, Name, RdrName (..), mkModuleName)
import GhcPlugins (CoreExpr, Type)
import qualified GhcPlugins as Plugins
import HscTypes (lookupDataCon, lookupId, lookupTyCon)
import qualified PrimOp
import qualified Unique
import Prelude hiding (mod)

-- | This is the type for @(->)@ when you want to apply it to types of kind `Data.Kind.Type`. It's
--   easy to build this incorrectly and Core won't tell you that you have, so use this instead.
properFunTy :: Type
properFunTy =
  Plugins.mkTyConApp Plugins.funTyCon [Plugins.liftedRepDataConTy, Plugins.liftedRepDataConTy]

-- | Similar to `properFunTy`, but fully-applied, inferring the kind arguments from the kinds of the
--   provided types.
funTy :: Type -> Type -> Type
funTy a b = Plugins.mkTyConApp Plugins.funTyCon [Plugins.typeKind a, Plugins.typeKind b, a, b]

-- | These are operations that we need to use in __Hask__, rather than in the target category.
data HaskOps f = HaskOps
  { -- | @forall a. Rep a -> a@
    abstH :: (CoreExpr -> f CoreExpr) -> Type -> f CoreExpr,
    -- | @forall a b c. (a -> c) -> (b -> c) -> Either a b -> c@
    eitherH :: Type -> Type -> Type -> CoreExpr -> CoreExpr -> CoreExpr -> CoreExpr,
    -- | @forall cat i a b. i -> (a -> b) -> cat a b@
    ffcallH ::
      (CoreExpr -> f CoreExpr) ->
      Type ->
      Type ->
      Type ->
      Type ->
      CoreExpr ->
      CoreExpr ->
      f CoreExpr,
    -- | @forall a. (a -> a) -> a@
    fixH :: Type -> CoreExpr -> CoreExpr,
    -- | @forall a b. (a, b) -> a@
    fstH :: Type -> Type -> CoreExpr -> CoreExpr,
    -- | @forall cat a b. String -> cat a b -> cat a b@
    indirectionH ::
      (CoreExpr -> f CoreExpr) ->
      Type ->
      Type ->
      Type ->
      CoreExpr ->
      f CoreExpr,
    -- | @forall a. a -> Rep a@
    reprH :: (CoreExpr -> f CoreExpr) -> Type -> f CoreExpr,
    -- | @forall a b. (a, b) -> b@
    sndH :: Type -> Type -> CoreExpr -> CoreExpr,
    -- | @forall a b c. ((a, b) -> c) -> a -> b -> c@
    curryH :: Type -> Type -> Type -> CoreExpr -> CoreExpr
  }

concatOps :: Monad f => Lookup (HaskOps f)
concatOps = do
  abstH <- repOp "abst"
  eitherH <- do
    op <- identifier "Data.Either" "either"
    pure (\a b c f g e -> runIdentity $ mkFunctionApps Identity op [a, c, b] [f, g, e])
  ffcallH <- do
    op <- identifier "Categorifier.Category" "ffcall"
    pure $ \onDict cat ity a b i f ->
      mkMethodApps onDict op [cat, ity, a, b] [] [i, f]
  fixH <- do
    op <- identifier "Data.Function" "fix"
    pure (\t e -> runIdentity $ mkFunctionApps Identity op [t] [e])
  fstH <- do
    op <- identifier "Data.Tuple" "fst"
    pure (\a b e -> runIdentity $ mkFunctionApps Identity op [a, b] [e])
  indirectionH <- do
    op <- identifier "Categorifier.Category" "indirection"
    pure $ \onDict cat a b s ->
      mkMethodApps onDict op [Plugins.typeKind a, Plugins.typeKind b, cat, a, b] [] [s]
  reprH <- repOp "repr"
  sndH <- do
    op <- identifier "Data.Tuple" "snd"
    pure (\a b e -> runIdentity $ mkFunctionApps Identity op [a, b] [e])
  curryH <- do
    op <- identifier "Data.Tuple" "curry"
    pure (\a b c e -> runIdentity $ mkFunctionApps Identity op [a, b, c] [e])
  pure HaskOps {..}
  where
    repOp name = do
      op <- identifier "Categorifier.Core.Functions" name
      pure $ \onDict a -> mkFunctionApps onDict op [a] []

-- | This structure relates categorical concepts to operations in a particular library. Each field
--   can be either `Nothing` (if there is no way to model that operation in the library) or some
--   function that expects the correct set of type parameters. The resulting expression should match
--   the type of the operation in the comment for that field (with the type arguments fully
--   applied).
--
--  __TODO__: We should perhaps have the fields return @`Categorifier.Core.Makers.CategoryStack`
--           `CoreExpr`@ so implementations can fail to handle some cases.
data Hierarchy f = Hierarchy
  { -- | @forall cat a. cat a a@
    absV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> f CoreExpr),
    -- | @forall cat a. cat (Rep a) a@
    abstCV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> f CoreExpr),
    -- | @forall cat a. cat a a@
    acosV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> f CoreExpr),
    -- | @forall cat a. cat a a@
    acoshV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> f CoreExpr),
    -- | @forall cat. cat (Prod Bool Bool) Bool@
    andV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> f CoreExpr),
    -- | @forall cat f a b. cat (Prod (f (Exp a b)) (f a)) (f b)@
    apV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> Type -> Type -> f CoreExpr),
    -- | @forall cat a. Semigroup a. cat (Prod a a) a@
    appendV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> f CoreExpr),
    -- | @forall cat a b. cat (Prod (Exp a b) a) b@
    applyV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> Type -> f CoreExpr),
    -- | @forall cat x a b -> cat x (a -> b) -> cat x a -> cat x b@
    apply2V ::
      Maybe
        ( (CoreExpr -> f CoreExpr) ->
          Type ->
          Type ->
          Type ->
          Type ->
          CoreExpr ->
          CoreExpr ->
          f CoreExpr
        ),
    -- | @forall cat a. cat (Prod a a) a@
    arctan2V :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> f CoreExpr),
    -- | @forall cat a. cat a a@
    asinV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> f CoreExpr),
    -- | @forall cat a. cat a a@
    asinhV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> f CoreExpr),
    -- | @forall cat a. cat a a@
    atanV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> f CoreExpr),
    -- | @forall cat a. cat a a@
    atanhV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> f CoreExpr),
    -- | @forall cat m a b. cat (Prod (m a) (Exp a (m b)) (m b)@
    bindV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> Type -> Type -> f CoreExpr),
    -- | @forall cat a b. cat a b@
    bottomV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> Type -> f CoreExpr),
    -- | @forall cat from to. cat from to@
    coerceV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> Type -> f CoreExpr),
    -- | @forall cat a. cat (Prod a a) Ordering@
    compareV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> f CoreExpr),
    -- | @forall cat a b c. cat b c -> cat a b -> cat a c@
    composeV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> Type -> Type -> f CoreExpr),
    -- | @forall cat x b c a. cat x (b -> c) -> cat x (a -> b) -> cat x (a -> c)@
    compose2V ::
      Maybe
        ( (CoreExpr -> f CoreExpr) ->
          Type ->
          Type ->
          Type ->
          Type ->
          Type ->
          CoreExpr ->
          CoreExpr ->
          f CoreExpr
        ),
    -- | @forall cat a b. b -> cat a b@
    constV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> Type -> f CoreExpr),
    -- | @forall cat a b. b -> cat a b@
    constraintV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> Type -> f CoreExpr),
    -- | @forall cat a. cat a a@
    cosV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> f CoreExpr),
    -- | @forall cat a. cat a a@
    coshV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> f CoreExpr),
    -- | @forall cat a1 a2 b. cat (Prod a1 a2) b -> cat a1 (Exp a2 b)@
    curryV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> Type -> Type -> f CoreExpr),
    -- | @forall cat a b c. cat (Prod a (Coprod b c)) (Coprod (Prod a b) (Prod a c)) @
    distlV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> Type -> Type -> f CoreExpr),
    -- | @forall cat a. cat (Prod a a) a@
    divV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> f CoreExpr),
    -- | @forall cat a. cat (Prod a a) a@
    divideV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> f CoreExpr),
    -- | @forall cat. cat Double Float@
    doubleToFloatV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> f CoreExpr),
    -- | @forall cat a. cat (Prod a a) Bool@
    equalV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> f CoreExpr),
    -- | @forall cat a. cat a Bool@
    evenV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> f CoreExpr),
    -- | @forall cat a b. cat (Prod a b) a@
    exlV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> Type -> f CoreExpr),
    -- | @forall cat a. cat a a@
    expV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> f CoreExpr),
    -- | @forall cat a b. cat (Prod a b) b@
    exrV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> Type -> f CoreExpr),
    -- | @forall cat a x. cat (Prod a x) x -> cat a x@
    fixV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> Type -> f CoreExpr),
    -- | @forall cat. cat Float Double@
    floatToDoubleV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> f CoreExpr),
    -- | @forall cat a. cat (Prod a a) a@
    fmodV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> f CoreExpr),
    -- | @forall cat a b1 b2. cat a b1 -> cat a b2 -> cat a (Prod b1 b2)@
    forkV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> Type -> Type -> f CoreExpr),
    -- | @forall cat a. cat a Bool@
    fpIsNegativeZeroV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> f CoreExpr),
    -- | @forall cat a. cat a Bool@
    fpIsInfiniteV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> f CoreExpr),
    -- | @forall cat a. cat a Bool@
    fpIsFiniteV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> f CoreExpr),
    -- | @forall cat a. cat a Bool@
    fpIsNaNV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> f CoreExpr),
    -- | @forall cat a. cat a Bool@
    fpIsDenormalV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> f CoreExpr),
    -- | @forall cat a. cat Integer a@
    --
    --  __TODO__: This is simply a specialization of `fromIntegralV`, but it's difficult to look up
    --            type constructors outside of this context, so we make it part of the hierarchy,
    --            but we shouldn't have to.
    fromIntegerV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> f CoreExpr),
    -- | @forall cat a b. cat a b@
    fromIntegralV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> Type -> f CoreExpr),
    -- | @forall cat a. cat (Prod a a) Bool@
    geV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> f CoreExpr),
    -- | @forall cat a. cat (Prod a a) Bool@
    gtV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> f CoreExpr),
    -- | @forall cat a. cat a a@
    idV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> f CoreExpr),
    -- | @forall cat a. cat (Prod Bool (Prod a a)) a@
    ifV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> f CoreExpr),
    -- | @forall cat a b. cat a (Coprod a b)@
    inlV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> Type -> f CoreExpr),
    -- | @forall cat a b. cat b (Coprod a b)@
    inrV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> Type -> f CoreExpr),
    -- | @forall cat a1 a2 b. cat a1 b -> cat a2 b -> cat (Coprod a1 a2) c@
    joinV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> Type -> Type -> f CoreExpr),
    -- | @forall cat a b c. cat (Prod a (Prod b c)) (Prod (Prod a b) c)@
    lassocV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> Type -> Type -> f CoreExpr),
    -- | @forall cat a. cat (Prod a a) Bool@
    leV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> f CoreExpr),
    -- | @forall cat f a b c. cat (Prod a b) c -> cat (Prod (f a) (f b)) (f c)@
    liftA2V ::
      Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> Type -> Type -> Type -> f CoreExpr),
    -- | @forall cat a. cat a a@
    logV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> f CoreExpr),
    -- | @forall cat a. cat (Prod a a) Bool@
    ltV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> f CoreExpr),
    -- | @forall cat cat' f a b. cat a b -> cat' (f a) (f b)@
    --
    --  __NB__: This is not necessarily an endofunctor. It expects /two/ categories, which may
    --          differ.
    mapV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> Type -> Type -> Type -> f CoreExpr),
    -- | @forall cat a. Ord a. cat (Prod a a) a@
    maxV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> f CoreExpr),
    -- | @forall cat f a. cat (f a) a@
    maximumV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> Type -> f CoreExpr),
    -- | @forall cat a. Ord a. cat (Prod a a) a@
    minV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> f CoreExpr),
    -- | @forall cat f a. cat (f a) a@
    minimumV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> Type -> f CoreExpr),
    -- | @forall cat a. Semiring a. cat (Prod a a) a@
    minusV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> f CoreExpr),
    -- | @forall cat a. cat (Prod a a) a@
    modV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> f CoreExpr),
    -- | @forall cat tag a b. cat a b@
    nativeV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> Type -> Type -> f CoreExpr),
    -- | @forall cat a. Semiring a. cat a a@
    negateV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> f CoreExpr),
    -- | @forall cat f a. cat (f a) (Exp (Rep f) a)@
    indexV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> Type -> f CoreExpr),
    -- | @forall cat f a. cat (Exp (Rep f) a) (f a)@
    tabulateV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> Type -> f CoreExpr),
    -- | @forall cat. cat Bool Bool@
    notV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> f CoreExpr),
    -- | @forall cat a. cat (Prod a a) Bool@
    notEqualV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> f CoreExpr),
    -- | @forall cat a. cat a Bool@
    oddV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> f CoreExpr),
    -- | @forall cat. cat (Prod Bool Bool) Bool@
    orV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> f CoreExpr),
    -- | @forall cat a. Semiring a. cat (Prod a a) a@
    plusV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> f CoreExpr),
    -- | @forall cat f a. cat a (f a)
    pointV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> Type -> f CoreExpr),
    -- | @forall cat a. cat (Prod a a) a@
    powV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> f CoreExpr),
    -- | @forall cat a i. i -> cat a a@
    powIV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> Type -> CoreExpr -> f CoreExpr),
    -- | @forall cat a. cat (a, Int) a@
    powIntV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> f CoreExpr),
    -- | @forall cat a. cat (Prod a a) a@
    quotV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> f CoreExpr),
    -- | @forall cat a b c. cat (Prod (Prod a b) c) (Prod a (Prod b c))@
    rassocV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> Type -> Type -> f CoreExpr),
    -- | @forall cat a b. cat a b@
    realToFracV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> Type -> f CoreExpr),
    -- | @forall cat a. cat a a@
    recipV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> f CoreExpr),
    -- | @forall cat a. cat (Prod a a) a@
    remV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> f CoreExpr),
    -- | @forall cat a. cat a (Rep a) @
    reprCV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> f CoreExpr),
    -- | @forall cat t f a. cat (t (f a)) (f (t a))
    sequenceAV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> Type -> Type -> f CoreExpr),
    -- | @forall cat a. cat a a@
    signumV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> f CoreExpr),
    -- | @forall cat a. cat a a@
    sinV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> f CoreExpr),
    -- | @forall cat a. cat a a@
    sinhV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> f CoreExpr),
    -- | @forall cat a. cat a a@
    sqrtV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> f CoreExpr),
    -- | @forall cat f a b. cat (Prod a (f b)) (f (Prod a b))@
    strengthV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> Type -> Type -> f CoreExpr),
    -- | @forall cat a b. cat (Prod a b) (Prod b a)@
    swapV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> Type -> f CoreExpr),
    -- | @forall cat a. cat a a@
    tanV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> f CoreExpr),
    -- | @forall cat a. cat a a@
    tanhV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> f CoreExpr),
    -- | @forall cat a. Semiring a. cat (Prod a a) a@
    timesV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> f CoreExpr),
    -- | @forall cat t f a b. cat a (f b) -> cat (t a) (f (t b))@
    traverseV ::
      Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> Type -> Type -> Type -> f CoreExpr),
    -- | @forall cat a1 a2 b. cat a1 (Exp a2 b) -> cat (Prod a1 a2) b@
    uncurryV :: Maybe ((CoreExpr -> f CoreExpr) -> Type -> Type -> Type -> Type -> f CoreExpr)
  }

-- | Like `Data.Monoid.First`, but more general, as it isn't restricted to `Maybe`.
newtype First a = First {getFirst :: a}

-- | This default instance /always/ chooses the first argument. If your @a@ is a monoid (see
--   @`First` (`Maybe` a)@), or is somehow mergable (see @`First` (`Hierarchy` f)@), then it's
--   better to create an @overlapping@ instance.
instance {-# OVERLAPPABLE #-} Semigroup (First a) where
  (<>) = const

instance Semigroup (First (Maybe a)) where
  First a <> First b = First $ a <|> b

type Last a = Dual (First a)

pattern Last :: a -> Last a
pattern Last a = Dual (First a)

getLast :: Last a -> a
getLast = getFirst . getDual

instance Semigroup (First (Hierarchy f)) where
  First a <> First b =
    First $
      Hierarchy
        { absV = absV a <|> absV b,
          abstCV = abstCV a <|> abstCV b,
          acosV = acosV a <|> acosV b,
          acoshV = acoshV a <|> acoshV b,
          andV = andV a <|> andV b,
          apV = apV a <|> apV b,
          appendV = appendV a <|> appendV b,
          applyV = applyV a <|> applyV b,
          apply2V = apply2V a <|> apply2V b,
          arctan2V = arctan2V a <|> arctan2V b,
          asinV = asinV a <|> asinV b,
          asinhV = asinhV a <|> asinhV b,
          atanV = atanV a <|> atanV b,
          atanhV = atanhV a <|> atanhV b,
          bindV = bindV a <|> bindV b,
          bottomV = bottomV a <|> bottomV b,
          coerceV = coerceV a <|> coerceV b,
          compareV = compareV a <|> compareV b,
          composeV = composeV a <|> composeV b,
          compose2V = compose2V a <|> compose2V b,
          constV = constV a <|> constV b,
          constraintV = constraintV a <|> constraintV b,
          cosV = cosV a <|> cosV b,
          coshV = coshV a <|> coshV b,
          curryV = curryV a <|> curryV b,
          distlV = distlV a <|> distlV b,
          divV = divV a <|> divV b,
          divideV = divideV a <|> divideV b,
          doubleToFloatV = doubleToFloatV a <|> doubleToFloatV b,
          equalV = equalV a <|> equalV b,
          evenV = evenV a <|> evenV b,
          exlV = exlV a <|> exlV b,
          expV = expV a <|> expV b,
          exrV = exrV a <|> exrV b,
          fixV = fixV a <|> fixV b,
          floatToDoubleV = floatToDoubleV a <|> floatToDoubleV b,
          fmodV = fmodV a <|> fmodV b,
          forkV = forkV a <|> forkV b,
          fpIsNegativeZeroV = fpIsNegativeZeroV a <|> fpIsNegativeZeroV b,
          fpIsInfiniteV = fpIsInfiniteV a <|> fpIsInfiniteV b,
          fpIsFiniteV = fpIsFiniteV a <|> fpIsFiniteV b,
          fpIsNaNV = fpIsNaNV a <|> fpIsNaNV b,
          fpIsDenormalV = fpIsDenormalV a <|> fpIsDenormalV b,
          fromIntegerV = fromIntegerV a <|> fromIntegerV b,
          fromIntegralV = fromIntegralV a <|> fromIntegralV b,
          geV = geV a <|> geV b,
          gtV = gtV a <|> gtV b,
          idV = idV a <|> idV b,
          ifV = ifV a <|> ifV b,
          inlV = inlV a <|> inlV b,
          inrV = inrV a <|> inrV b,
          joinV = joinV a <|> joinV b,
          lassocV = lassocV a <|> lassocV b,
          leV = leV a <|> leV b,
          liftA2V = liftA2V a <|> liftA2V b,
          logV = logV a <|> logV b,
          ltV = ltV a <|> ltV b,
          mapV = mapV a <|> mapV b,
          maxV = maxV a <|> maxV b,
          maximumV = maximumV a <|> maximumV b,
          minV = minV a <|> minV b,
          minimumV = minimumV a <|> minimumV b,
          minusV = minusV a <|> minusV b,
          modV = modV a <|> modV b,
          nativeV = nativeV a <|> nativeV b,
          negateV = negateV a <|> negateV b,
          indexV = indexV a <|> indexV b,
          tabulateV = tabulateV a <|> tabulateV b,
          notV = notV a <|> notV b,
          notEqualV = notEqualV a <|> notEqualV b,
          oddV = oddV a <|> oddV b,
          orV = orV a <|> orV b,
          plusV = plusV a <|> plusV b,
          pointV = pointV a <|> pointV b,
          powV = powV a <|> powV b,
          powIV = powIV a <|> powIV b,
          powIntV = powIntV a <|> powIntV b,
          quotV = quotV a <|> quotV b,
          rassocV = rassocV a <|> rassocV b,
          realToFracV = realToFracV a <|> realToFracV b,
          recipV = recipV a <|> recipV b,
          remV = remV a <|> remV b,
          reprCV = reprCV a <|> reprCV b,
          sequenceAV = sequenceAV a <|> sequenceAV b,
          signumV = signumV a <|> signumV b,
          sinV = sinV a <|> sinV b,
          sinhV = sinhV a <|> sinhV b,
          sqrtV = sqrtV a <|> sqrtV b,
          strengthV = strengthV a <|> strengthV b,
          swapV = swapV a <|> swapV b,
          tanV = tanV a <|> tanV b,
          tanhV = tanhV a <|> tanhV b,
          timesV = timesV a <|> timesV b,
          traverseV = traverseV a <|> traverseV b,
          uncurryV = uncurryV a <|> uncurryV b
        }

instance Monoid (First (Hierarchy f)) where
  mempty = First emptyHierarchy

-- | If you are building a custom hierarchy, it is worth considering whether you want to populate
--   the record from `Hierarchy` or from `emptyHierarchy`. Most of the ones provided in this library
--   are built from `Hierarchy`, because it is easier to keep in sync with changes that way and we
--   can update them in lock-step. If you are defining a hierarchy within a specific application,
--   you probably want to do the same to ensure that changes in this library are caught. However, if
--   you are publishing a hierarchy in a separate library, building from `emptyHierarchy` is more
--   future-proof. As we add more operations, your library will continue to work, it just won't
--   support the new operations.
emptyHierarchy :: Hierarchy f
emptyHierarchy =
  Hierarchy
    { absV = Nothing,
      abstCV = Nothing,
      acosV = Nothing,
      acoshV = Nothing,
      andV = Nothing,
      apV = Nothing,
      appendV = Nothing,
      applyV = Nothing,
      apply2V = Nothing,
      arctan2V = Nothing,
      asinV = Nothing,
      asinhV = Nothing,
      atanV = Nothing,
      atanhV = Nothing,
      bindV = Nothing,
      bottomV = Nothing,
      coerceV = Nothing,
      compareV = Nothing,
      composeV = Nothing,
      compose2V = Nothing,
      constV = Nothing,
      constraintV = Nothing,
      cosV = Nothing,
      coshV = Nothing,
      curryV = Nothing,
      distlV = Nothing,
      divV = Nothing,
      divideV = Nothing,
      doubleToFloatV = Nothing,
      equalV = Nothing,
      evenV = Nothing,
      exlV = Nothing,
      expV = Nothing,
      exrV = Nothing,
      fixV = Nothing,
      floatToDoubleV = Nothing,
      fmodV = Nothing,
      forkV = Nothing,
      fpIsNegativeZeroV = Nothing,
      fpIsInfiniteV = Nothing,
      fpIsFiniteV = Nothing,
      fpIsNaNV = Nothing,
      fpIsDenormalV = Nothing,
      fromIntegerV = Nothing,
      fromIntegralV = Nothing,
      geV = Nothing,
      gtV = Nothing,
      idV = Nothing,
      ifV = Nothing,
      inlV = Nothing,
      inrV = Nothing,
      joinV = Nothing,
      lassocV = Nothing,
      leV = Nothing,
      liftA2V = Nothing,
      logV = Nothing,
      ltV = Nothing,
      mapV = Nothing,
      maxV = Nothing,
      maximumV = Nothing,
      minV = Nothing,
      minimumV = Nothing,
      minusV = Nothing,
      modV = Nothing,
      negateV = Nothing,
      indexV = Nothing,
      tabulateV = Nothing,
      nativeV = Nothing,
      notV = Nothing,
      notEqualV = Nothing,
      oddV = Nothing,
      orV = Nothing,
      plusV = Nothing,
      pointV = Nothing,
      powV = Nothing,
      powIV = Nothing,
      powIntV = Nothing,
      quotV = Nothing,
      rassocV = Nothing,
      realToFracV = Nothing,
      recipV = Nothing,
      remV = Nothing,
      reprCV = Nothing,
      sequenceAV = Nothing,
      signumV = Nothing,
      sinV = Nothing,
      sinhV = Nothing,
      sqrtV = Nothing,
      strengthV = Nothing,
      swapV = Nothing,
      tanV = Nothing,
      tanhV = Nothing,
      timesV = Nothing,
      traverseV = Nothing,
      uncurryV = Nothing
    }

lookupName :: ModuleName -> (String -> Plugins.OccName) -> String -> CoreM (Maybe Plugins.Name)
lookupName modu mkOcc str = do
  hscEnv <- getHscEnv
  liftIO . fmap (fmap fst) . lookupRdrNameInModuleForPlugins hscEnv modu . Unqual $ mkOcc str

-- __TODO__: This can throw in `lookupRdrNameInModuleForPlugins` if it can't find the module. We
--           should capture that.
lookupRdr ::
  ModuleName -> (String -> Plugins.OccName) -> (Name -> CoreM a) -> String -> CoreM (Maybe a)
lookupRdr modu mkOcc mkThing = traverse mkThing <=< lookupName modu mkOcc

findName :: String -> String -> Lookup Plugins.Name
findName modu str =
  let mod = mkModuleName modu
   in Parallel
        ( ExceptT
            (maybe (Left . pure $ MissingName mod str) pure <$> lookupName mod Plugins.mkVarOcc str)
        )

findDataCon :: String -> String -> Lookup Plugins.DataCon
findDataCon modu str =
  let mod = mkModuleName modu
   in Parallel
        ( ExceptT
            ( maybe (Left . pure $ MissingDataCon mod str) pure
                <$> lookupRdr mod Plugins.mkDataOcc lookupDataCon str
            )
        )

-- | A helper for building `Hierarchy`s, also useful for looking up other `Plugins.Id`s in the
--   appropriate context.
findId :: String -> String -> Lookup Id
findId modu str =
  let mod = mkModuleName modu
   in Parallel
        ( ExceptT
            ( maybe (Left . pure $ MissingId mod str) pure
                <$> lookupRdr mod Plugins.mkVarOcc lookupId str
            )
        )

findTyCon :: String -> String -> Lookup Plugins.TyCon
findTyCon modu str =
  let mod = mkModuleName modu
   in Parallel
        ( ExceptT
            ( maybe (Left . pure $ MissingTyCon mod str) pure
                <$> lookupRdr mod Plugins.mkTcOcc lookupTyCon str
            )
        )

identifier :: String -> String -> Lookup CoreExpr
identifier = (fmap Plugins.Var .) . findId

-- | Very much like `mkMethodApps`, but as a function is not a member of a type class, there are no
--   class parameters to apply (or class dictionary to resolve).
mkFunctionApps ::
  Functor f =>
  -- | The dictionary applicator
  (CoreExpr -> f CoreExpr) ->
  -- | The function
  CoreExpr ->
  -- | The type arguments
  [Type] ->
  -- | The term arguments
  [CoreExpr] ->
  f CoreExpr
mkFunctionApps onDict fn tys terms =
  fmap (`Plugins.mkCoreApps` terms) . onDict $ Plugins.mkTyApps fn tys

-- | Applies all of the arguments (types and terms) to a type class method. It's structured in a
--   particular way. E.g., given a class like
--
--   >>> class Foldable t where
--   >>>   foldMap :: Monoid m => (a -> m) -> t a -> m
--
--   we need to do a few things in the right order:
-- 1. apply the type parameters in the class declaration (@t@),
-- 2. resolve the dictionary for the type class itself (@Foldable t@),
-- 3. apply the type parameters on the method (@m@ and @a@),
-- 4. resolve any constraint dictionaries on the method (@`Monoid` m@), and finally
-- 5. apply the term parameters on the method (values with types @(a -> m)@ and @t a@).
--
--   This function automatically handles all the dictionary resolution (via the @onDict@ argument),
--   but uses the split in the `Type` lists to know when to do the resolution. So, a call to
--   @foldMap@ would look something like @`mkMethodApps` onDict [t] [m, a] [fn, ta]@.
--
--  __NB__: The types as printed in places like [Haddock](https://www.haskell.org/haddock/) and by
--         `Plugins.Outputable` are misleading. They will show you things like
--          @forall t m a. (Foldable t, `Monoid` m) => (a -> m) -> t a -> m@, which make
--          it /look/ like all the dictionary resolution happens after @t@, @m@, and @a@ are
--          applied. However, doing that can cause the plugin to segfault. Something like
--          @forall t. Foldable t => forall m a. `Monoid` m => (a -> m) -> t a -> m@ would be more
--          honest, but not what we currently get.
mkMethodApps ::
  Monad f =>
  -- | The dictionary applicator
  (CoreExpr -> f CoreExpr) ->
  -- | The type class method
  CoreExpr ->
  -- | The type arguments for the type class itself
  [Type] ->
  -- | The type arguments on the method
  [Type] ->
  -- | The term arguments
  [CoreExpr] ->
  f CoreExpr
mkMethodApps onDict = mkMethodApps' onDict onDict

-- | Like `mkMethodApps`, but takes separate dictionary applicators for class constraints
-- and method constraints.
mkMethodApps' ::
  Monad f =>
  -- | The dictionary applicator for class constraints
  (CoreExpr -> f CoreExpr) ->
  -- | The dictionary applicator for method constraints
  (CoreExpr -> f CoreExpr) ->
  -- | The type class method
  CoreExpr ->
  -- | The type arguments for the type class itself
  [Type] ->
  -- | The type arguments on the method
  [Type] ->
  -- | The term arguments
  [CoreExpr] ->
  f CoreExpr
mkMethodApps' onDictCls onDictMeth fn tc tys terms = do
  method <- onDictCls $ Plugins.mkTyApps fn tc
  mkFunctionApps onDictMeth method tys terms

-- | A hierarchy using only type classes available in @base@.
--
--  __TODO__: This could generalize `CategoryStack` to @`Monad` f => f@, but can't currently
--            dynamically load a parameterized type successfully.
baseHierarchy :: Lookup (Hierarchy CategoryStack)
baseHierarchy = do
  absV <- closedUnaryOp "Prelude" "abs"
  abstCV <-
    pure <$> do
      arr <- identifier "Control.Arrow" "arr"
      rep <- findTyCon "Categorifier.Client" "Rep"
      op <- identifier "Categorifier.Client" "abst"
      pure $ \onDict cat a ->
        mkMethodApps onDict arr [cat] [Plugins.mkTyConApp rep [a], a] . pure
          =<< mkMethodApps onDict op [a] [] []
  acosV <- closedUnaryOp "Numeric" "acos"
  acoshV <- closedUnaryOp "Numeric" "acosh"
  andV <-
    pure <$> do
      arr <- identifier "Control.Arrow" "arr"
      uncur <- identifier "Data.Tuple" "uncurry"
      op <- identifier "GHC.Classes" "&&"
      pure
        ( \onDict cat -> do
            op' <- mkFunctionApps onDict op [] []
            uncur' <-
              mkFunctionApps onDict uncur [Plugins.boolTy, Plugins.boolTy, Plugins.boolTy] [op']
            mkMethodApps
              onDict
              arr
              [cat]
              [Plugins.mkBoxedTupleTy [Plugins.boolTy, Plugins.boolTy], Plugins.boolTy]
              [uncur']
        )
  apV <-
    pure <$> do
      arr <- identifier "Control.Arrow" "arr"
      uncur <- identifier "Data.Tuple" "uncurry"
      op <- identifier "Control.Applicative" "<*>"
      pure
        ( \onDict cat f a b -> do
            let fa = Plugins.mkAppTy f a
                fb = Plugins.mkAppTy f b
                ffun = Plugins.mkAppTy f (Plugins.mkAppTys properFunTy [a, b])
            op' <- mkMethodApps onDict op [f] [a, b] []
            uncur' <- mkFunctionApps onDict uncur [ffun, fa, fb] [op']
            mkMethodApps onDict arr [cat] [Plugins.mkBoxedTupleTy [ffun, fa], fb] [uncur']
        )
  appendV <- closedBinaryOp "GHC.Base" "<>"
  applyV <-
    pure <$> do
      arr <- identifier "Control.Arrow" "arr"
      op <- identifier "Data.Function" "$"
      uncur <- identifier "Data.Tuple" "uncurry"
      pure $
        \onDict cat a b -> do
          let fun = Plugins.mkAppTys properFunTy [a, b]
          op' <- mkFunctionApps onDict op [Plugins.liftedRepTy, a, b] []
          uncur' <- mkFunctionApps onDict uncur [fun, a, b] [op']
          mkMethodApps onDict arr [cat] [Plugins.mkBoxedTupleTy [fun, a], b] [uncur']
  let apply2V = Nothing
  arctan2V <- closedBinaryOp "GHC.Float" "atan2"
  asinV <- closedUnaryOp "Numeric" "asin"
  asinhV <- closedUnaryOp "Numeric" "asinh"
  atanV <- closedUnaryOp "Numeric" "atan"
  atanhV <- closedUnaryOp "Numeric" "atanh"
  bindV <-
    pure <$> do
      arr <- identifier "Control.Arrow" "arr"
      op <- identifier "Control.Monad" ">>="
      uncur <- identifier "Data.Tuple" "uncurry"
      pure $
        \onDict cat m a b -> do
          let ma = Plugins.mkAppTy m a
              mb = Plugins.mkAppTy m b
              fun = Plugins.mkAppTys properFunTy [a, mb]
          op' <- mkMethodApps onDict op [m] [a, b] []
          uncur' <- mkFunctionApps onDict uncur [ma, fun, mb] [op']
          mkMethodApps onDict arr [cat] [Plugins.mkBoxedTupleTy [ma, fun], mb] [uncur']
  bottomV <-
    pure <$> do
      arr <- identifier "Control.Arrow" "arr"
      op <- identifier "GHC.Err" "undefined"
      pure $ \onDict cat a b ->
        mkMethodApps onDict arr [cat] [a, b] . pure
          =<< mkFunctionApps onDict op [Plugins.liftedRepDataConTy, funTy a b] []
  coerceV <-
    pure <$> do
      arr <- identifier "Control.Arrow" "arr"
      op <- identifier "Unsafe.Coerce" "unsafeCoerce"
      pure $ \onDict cat from to ->
        mkMethodApps onDict arr [cat] [from, to] . pure =<< mkFunctionApps onDict op [from, to] []
  compareV <- binaryOp (Just $ Plugins.mkTyConTy Plugins.orderingTyCon) "GHC.Classes" "compare"
  composeV <-
    pure <$> do
      fn <- identifier "Control.Category" "."
      pure $ \onDict cat a b c -> mkMethodApps onDict fn [Plugins.typeKind a, cat] [b, c, a] []
  let compose2V = Nothing
  constV <-
    pure <$> do
      arr <- identifier "Control.Arrow" "arr"
      op <- identifier "Data.Function" "const"
      pure $
        \onDict cat a b ->
          -- Builds a lambda expecting the expression we want to return before applying `arr`. I.e.,
          -- @\bang -> arr (const bang) :: b -> cat a b@.
          let v = Plugins.mkSysLocal (Plugins.fsLit "bang") (Unique.mkBuiltinUnique 17) b
           in Plugins.Lam v
                <$> ( mkMethodApps onDict arr [cat] [a, b] . pure
                        =<< mkFunctionApps onDict op [b, a] [Plugins.Var v]
                    )
  let constraintV = Nothing
  cosV <- closedUnaryOp "Numeric" "cos"
  coshV <- closedUnaryOp "Numeric" "cosh"
  let curryV = Nothing
  distlV <-
    pure <$> do
      arr <- identifier "Control.Arrow" "arr"
      op <- identifier "Categorifier.Core.Base" "distlB"
      eith <- findTyCon "Data.Either" "Either"
      pure $ \onDict cat a b c ->
        mkMethodApps
          onDict
          arr
          [cat]
          [ Plugins.mkBoxedTupleTy [a, Plugins.mkTyConApp eith [b, c]],
            Plugins.mkTyConApp eith [Plugins.mkBoxedTupleTy [a, b], Plugins.mkBoxedTupleTy [a, c]]
          ]
          . pure
          =<< mkFunctionApps onDict op [Plugins.mkTyConTy eith, a, b, c] []
  divV <- closedBinaryOp "GHC.Real" "div"
  divideV <- closedBinaryOp "Prelude" "/"
  doubleToFloatV <-
    pure <$> do
      arr <- identifier "Control.Arrow" "arr"
      op <- identifier "GHC.Float" "double2Float"
      pure (\onDict cat -> mkMethodApps onDict arr [cat] [Plugins.doubleTy, Plugins.floatTy] [op])
  equalV <- binaryRel "GHC.Classes" "=="
  evenV <- unaryRel "GHC.Real" "even"
  exlV <-
    pure <$> do
      arr <- identifier "Control.Arrow" "arr"
      op <- identifier "Data.Tuple" "fst"
      pure $ \onDict cat a b ->
        mkMethodApps onDict arr [cat] [Plugins.mkBoxedTupleTy [a, b], a] . pure
          =<< mkFunctionApps onDict op [a, b] []
  expV <- closedUnaryOp "Numeric" "exp"
  exrV <-
    pure <$> do
      arr <- identifier "Control.Arrow" "arr"
      op <- identifier "Data.Tuple" "snd"
      pure $ \onDict cat a b ->
        mkMethodApps onDict arr [cat] [Plugins.mkBoxedTupleTy [a, b], b] . pure
          =<< mkFunctionApps onDict op [a, b] []
  fixV <-
    pure <$> do
      fn <- identifier "Categorifier.Core.Base" "fixB"
      pure (\onDict cat a x -> mkFunctionApps onDict fn [cat, a, x] [])
  floatToDoubleV <-
    pure <$> do
      arr <- identifier "Control.Arrow" "arr"
      op <- identifier "GHC.Float" "float2Double"
      pure (\onDict cat -> mkMethodApps onDict arr [cat] [Plugins.floatTy, Plugins.doubleTy] [op])
  let fmodV = Nothing
  forkV <-
    pure <$> do
      fn <- identifier "Control.Arrow" "&&&"
      pure (\onDict cat a b c -> mkMethodApps onDict fn [cat] [a, b, c] [])
  fpIsNegativeZeroV <- unaryRel "GHC.Float" "isNegativeZero"
  fpIsInfiniteV <- unaryRel "GHC.Float" "isInfinite"
  let fpIsFiniteV = Nothing
  fpIsNaNV <- unaryRel "GHC.Float" "isNaN"
  fpIsDenormalV <- unaryRel "GHC.Float" "isDenormalized"
  fromIntegerV <-
    pure <$> do
      arr <- identifier "Control.Arrow" "arr"
      op <- identifier "Prelude" "fromInteger"
      int <- findTyCon "Prelude" "Integer"
      pure $ \onDict cat a ->
        mkMethodApps onDict arr [cat] [Plugins.mkTyConTy int, a] . pure
          =<< mkFunctionApps onDict op [a] []
  fromIntegralV <-
    pure <$> do
      arr <- identifier "Control.Arrow" "arr"
      op <- identifier "GHC.Real" "fromIntegral"
      pure $ \onDict cat a b ->
        mkMethodApps onDict arr [cat] [a, b] . pure =<< mkFunctionApps onDict op [a, b] []
  geV <- binaryRel "GHC.Classes" ">="
  gtV <- binaryRel "GHC.Classes" ">"
  idV <-
    pure <$> do
      fn <- identifier "Control.Category" "id"
      pure (\onDict cat a -> mkMethodApps onDict fn [Plugins.typeKind a, cat] [a] [])
  ifV <-
    pure <$> do
      arr <- identifier "Control.Arrow" "arr"
      op <- identifier "Categorifier.Core.Base" "ifThenElseB"
      pure $ \onDict cat a ->
        mkMethodApps
          onDict
          arr
          [cat]
          [Plugins.mkBoxedTupleTy [Plugins.boolTy, Plugins.mkBoxedTupleTy [a, a]], a]
          . pure
          =<< mkFunctionApps onDict op [a] []
  inlV <- eitherOp "Left" const
  inrV <- eitherOp "Right" (\_ x -> x)
  joinV <-
    pure <$> do
      fn <- identifier "Control.Arrow" "|||"
      pure (\onDict cat a1 a2 b -> mkMethodApps onDict fn [cat] [a1, b, a2] [])
  lassocV <-
    pure <$> do
      arr <- identifier "Control.Arrow" "arr"
      op <- identifier "Categorifier.Core.Base" "lassocB"
      pure $ \onDict cat a b c ->
        mkMethodApps
          onDict
          arr
          [cat]
          [ Plugins.mkBoxedTupleTy [a, Plugins.mkBoxedTupleTy [b, c]],
            Plugins.mkBoxedTupleTy [Plugins.mkBoxedTupleTy [a, b], c]
          ]
          . pure
          =<< mkFunctionApps onDict op [a, b, c] []
  leV <- binaryRel "GHC.Classes" "<="
  let liftA2V = Nothing
  logV <- closedUnaryOp "Numeric" "log"
  ltV <- binaryRel "GHC.Classes" "<"
  let mapV = Nothing
  maxV <- closedBinaryOp "Prelude" "max"
  let maximumV = Nothing
  minV <- closedBinaryOp "Prelude" "min"
  let minimumV = Nothing
  minusV <- closedBinaryOp "Prelude" "-"
  modV <- closedBinaryOp "GHC.Real" "mod"
  let nativeV = Nothing
  negateV <- closedUnaryOp "Prelude" "negate"
  let indexV = Nothing
  realToFracV <-
    pure <$> do
      arr <- identifier "Control.Arrow" "arr"
      op <- identifier "GHC.Real" "realToFrac"
      pure $ \onDict cat a b ->
        mkMethodApps onDict arr [cat] [a, b] . pure =<< mkFunctionApps onDict op [a, b] []
  let tabulateV = Nothing
  notV <-
    pure <$> do
      arr <- identifier "Control.Arrow" "arr"
      op <- identifier "Data.Bool" "not"
      pure (\onDict cat -> mkMethodApps onDict arr [cat] [Plugins.boolTy, Plugins.boolTy] [op])
  notEqualV <- binaryRel "GHC.Classes" "/="
  oddV <- unaryRel "GHC.Real" "odd"
  orV <-
    pure <$> do
      arr <- identifier "Control.Arrow" "arr"
      uncur <- identifier "Data.Tuple" "uncurry"
      op <- identifier "GHC.Classes" "||"
      pure
        ( \onDict cat -> do
            op' <- mkFunctionApps onDict op [] []
            uncur' <-
              mkFunctionApps onDict uncur [Plugins.boolTy, Plugins.boolTy, Plugins.boolTy] [op']
            mkMethodApps
              onDict
              arr
              [cat]
              [Plugins.mkBoxedTupleTy [Plugins.boolTy, Plugins.boolTy], Plugins.boolTy]
              [uncur']
        )
  plusV <- closedBinaryOp "Prelude" "+"
  pointV <-
    pure <$> do
      arr <- identifier "Control.Arrow" "arr"
      op <- identifier "Control.Applicative" "pure"
      pure $ \onDict cat f a ->
        mkMethodApps onDict arr [cat] [a, Plugins.mkAppTy f a] . pure
          =<< mkMethodApps onDict op [f] [a] []
  powV <- closedBinaryOp "Numeric" "**"
  let powIV = Nothing
  powIntV <-
    pure <$> do
      arr <- identifier "Control.Arrow" "arr"
      uncur <- identifier "Data.Tuple" "uncurry"
      op <- identifier "GHC.Real" "^"
      pure $ \onDict cat a -> do
        op' <- mkFunctionApps onDict op [a, Plugins.intTy] []
        uncur' <- mkFunctionApps onDict uncur [a, Plugins.intTy, a] [op']
        mkMethodApps onDict arr [cat] [Plugins.mkBoxedTupleTy [a, Plugins.intTy], a] [uncur']
  quotV <- closedBinaryOp "GHC.Real" "quot"
  rassocV <-
    pure <$> do
      arr <- identifier "Control.Arrow" "arr"
      op <- identifier "Categorifier.Core.Base" "rassocB"
      pure $ \onDict cat a b c ->
        mkMethodApps
          onDict
          arr
          [cat]
          [ Plugins.mkBoxedTupleTy [Plugins.mkBoxedTupleTy [a, b], c],
            Plugins.mkBoxedTupleTy [a, Plugins.mkBoxedTupleTy [b, c]]
          ]
          . pure
          =<< mkFunctionApps onDict op [a, b, c] []
  recipV <- closedUnaryOp "Prelude" "recip"
  remV <- closedBinaryOp "GHC.Real" "rem"
  reprCV <-
    pure <$> do
      arr <- identifier "Control.Arrow" "arr"
      rep <- findTyCon "Categorifier.Client" "Rep"
      op <- identifier "Categorifier.Client" "repr"
      pure $ \onDict cat a ->
        mkMethodApps onDict arr [cat] [a, Plugins.mkTyConApp rep [a]] . pure
          =<< mkMethodApps onDict op [a] [] []
  let sequenceAV = Nothing
  signumV <- closedUnaryOp "Prelude" "signum"
  sinV <- closedUnaryOp "Numeric" "sin"
  sinhV <- closedUnaryOp "Numeric" "sinh"
  sqrtV <- closedUnaryOp "Numeric" "sqrt"
  strengthV <-
    pure <$> do
      arr <- identifier "Control.Arrow" "arr"
      op <- identifier "Categorifier.Core.Base" "strengthB"
      pure $ \onDict cat f a b ->
        mkMethodApps
          onDict
          arr
          [cat]
          [ Plugins.mkBoxedTupleTy [a, Plugins.mkAppTy f b],
            Plugins.mkAppTy f (Plugins.mkBoxedTupleTy [a, b])
          ]
          . pure
          =<< mkFunctionApps onDict op [f, a, b] []
  swapV <-
    pure <$> do
      arr <- identifier "Control.Arrow" "arr"
      op <- identifier "Data.Tuple" "swap"
      pure $ \onDict cat a b ->
        mkMethodApps onDict arr [cat] [Plugins.mkBoxedTupleTy [a, b], Plugins.mkBoxedTupleTy [b, a]]
          . pure
          =<< mkFunctionApps onDict op [a, b] []
  tanV <- closedUnaryOp "Numeric" "tan"
  tanhV <- closedUnaryOp "Numeric" "tanh"
  timesV <- closedBinaryOp "Prelude" "*"
  let traverseV = Nothing
  uncurryV <-
    pure <$> do
      fn <- identifier "Categorifier.Core.Base" "uncurryB"
      pure (\onDict cat a b c -> mkFunctionApps onDict fn [cat, a, b, c] [])
  pure Hierarchy {..}
  where
    unaryRel = unaryOp (Just Plugins.boolTy)
    binaryRel = binaryOp (Just Plugins.boolTy)
    closedUnaryOp = unaryOp Nothing
    closedBinaryOp = binaryOp Nothing
    unaryOp mbResTy modu ident =
      pure <$> do
        arr <- identifier "Control.Arrow" "arr"
        op <- identifier modu ident
        pure $ \onDict cat a -> do
          let resTy = fromMaybe a mbResTy
          mkMethodApps onDict arr [cat] [a, resTy] . pure
            =<< mkMethodApps onDict op [a] [] []
    binaryOp mbResTy modu ident =
      pure <$> do
        arr <- identifier "Control.Arrow" "arr"
        uncur <- identifier "Data.Tuple" "uncurry"
        op <- identifier modu ident
        pure $ \onDict cat a -> do
          let resTy = fromMaybe a mbResTy
          op' <- mkMethodApps onDict op [a] [] []
          uncur' <- mkFunctionApps onDict uncur [a, a, resTy] [op']
          mkMethodApps onDict arr [cat] [Plugins.mkBoxedTupleTy [a, a], resTy] [uncur']
    eitherOp opName select =
      pure <$> do
        arr <- identifier "Control.Arrow" "arr"
        eith <- findTyCon "Data.Either" "Either"
        op <- Plugins.Var . Plugins.dataConWorkId <$> findDataCon "Data.Either" opName
        pure $ \onDict cat a b ->
          mkMethodApps onDict arr [cat] [select a b, Plugins.mkTyConApp eith [a, b]] . pure
            =<< mkFunctionApps onDict op [a, b] []

data IntConstructor = IntConstructor
  { intConstructorSize :: Integer,
    intConstructorSigned :: Bool,
    intConstructorTyCon :: Plugins.TyCon,
    intConstructorDataCon :: Plugins.DataCon,
    intConstructorNarrowOp :: Maybe PrimOp.PrimOp
  }

intConstructorToOpTyPair ::
  IntConstructor ->
  Maybe (PrimOp.PrimOp, Plugins.TyCon)
intConstructorToOpTyPair IntConstructor {..} =
  (,intConstructorTyCon) <$> intConstructorNarrowOp

intConstructorToBoxer ::
  IntConstructor ->
  (Plugins.TyCon, Plugins.DataCon)
intConstructorToBoxer IntConstructor {..} =
  (intConstructorTyCon, intConstructorDataCon)

getIntegerConstructors :: Lookup [IntConstructor]
getIntegerConstructors = traverse mkIntCons ints
  where
    mkIntCons ::
      (Integer, Bool, String, String, String, Maybe PrimOp.PrimOp) ->
      Lookup IntConstructor
    mkIntCons (i, s, m, t, d, n) =
      (\tc dc -> IntConstructor i s tc dc n) <$> findTyCon m t <*> findDataCon m d
    ints =
      -- (size, is signed, module, boxed type, boxing data con, narrowing op)
      [ (8, True, "GHC.Int", "Int8", "I8#", pure PrimOp.Narrow8IntOp),
        (16, True, "GHC.Int", "Int16", "I16#", pure PrimOp.Narrow16IntOp),
        (32, True, "GHC.Int", "Int32", "I32#", pure PrimOp.Narrow32IntOp),
        (64, True, "GHC.Int", "Int64", "I64#", Nothing),
        (toInteger $ finiteBitSize (0 :: Int), True, "GHC.Int", "Int", "I#", Nothing),
        (8, False, "GHC.Word", "Word8", "W8#", pure PrimOp.Narrow8WordOp),
        (16, False, "GHC.Word", "Word16", "W16#", pure PrimOp.Narrow16WordOp),
        (32, False, "GHC.Word", "Word32", "W32#", pure PrimOp.Narrow32WordOp),
        (64, False, "GHC.Word", "Word64", "W64#", Nothing),
        (toInteger $ finiteBitSize (0 :: Word), False, "GHC.Word", "Word", "W#", Nothing)
      ]

newtype GetTagInfo = GetTagInfo
  { getTagId :: Id
  }

getGetTagInfo :: Lookup GetTagInfo
getGetTagInfo = GetTagInfo <$> findId "GHC.Base" "getTag"

data BaseIdentifiers = BaseIdentifiers
  { baseIntConstructors :: [IntConstructor],
    baseGetTag :: GetTagInfo
  }

getBaseIdentifiers :: Lookup BaseIdentifiers
getBaseIdentifiers = do
  baseIntConstructors <- getIntegerConstructors
  baseGetTag <- getGetTagInfo
  pure BaseIdentifiers {..}
