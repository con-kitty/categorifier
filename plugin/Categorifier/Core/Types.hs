{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TupleSections #-}

module Categorifier.Core.Types
  ( AutoInterpreter,
    CategoryStack,
    DictionaryStack,
    CategoryState (..),
    DictCache,
    DictCacheKey,
    DictCacheEntry (..),
    CategoricalFailure (..),
    Lookup,
    MissingSymbol (..),
    DictionaryFailure (..),
    liftDictionaryStack,
    neverAutoInterpret,
    writerT,
  )
where

import Categorifier.Duoidal (Parallel)
import qualified Categorifier.GHC.Core as Plugins
import qualified Categorifier.GHC.Types as Plugins
import qualified Categorifier.GHC.Unit as Plugins
import qualified Categorifier.GHC.Utils as Plugins
import Control.Monad.Trans.Except (ExceptT (..), mapExceptT)
import Control.Monad.Trans.RWS.Strict (RWST (..), withRWST)
import Data.Bifunctor (Bifunctor (..))
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)

type CategoryStack =
  ExceptT
    (NonEmpty CategoricalFailure)
    (RWST (Map Plugins.Var Plugins.CoreExpr) Plugins.WarningMessages CategoryState IO)

type DictionaryStack =
  ExceptT
    (NonEmpty DictionaryFailure)
    (RWST () Plugins.WarningMessages CategoryState IO)

liftDictionaryStack :: Plugins.Type -> Plugins.CoreExpr -> DictionaryStack a -> CategoryStack a
liftDictionaryStack ty expr =
  mapExceptT (fmap (first (pure . CouldNotBuildDictionary ty expr)) . withRWST (const ((),)))

data CategoryState = CategoryState
  { csUniqSupply :: Plugins.UniqSupply,
    -- An incrementing index, used to populate `dceIdx`.
    csIdx :: Int,
    csDictCache :: DictCache
  }

data MissingSymbol
  = IncorrectType Plugins.Name Plugins.Type
  | MissingDataCon Plugins.ModuleName String
  | MissingId Plugins.ModuleName String
  | MissingName Plugins.ModuleName String
  | MissingTyCon Plugins.ModuleName String

-- | This type lets us perform everything in `Plugins.CoreM` while tracking failures properly. It
--   uses `Parallel` explicitly rather than relying on the `Categorifier.Duoid` operations because
--   we want to take advantage of @do@ notation for building up our records.
type Lookup = Parallel (ExceptT (NonEmpty MissingSymbol) Plugins.CoreM)

-- | A mechanism to bypass the plugin, providing a mapping @(a -> b) -> cat a b@ for any special
--   cases.
type AutoInterpreter =
  (Plugins.Type -> DictionaryStack Plugins.CoreExpr) ->
  Plugins.Type ->
  Plugins.Type ->
  Plugins.Id ->
  [Plugins.CoreExpr] ->
  CategoryStack (Maybe Plugins.CoreExpr)

-- | What to use for the `AutoInterpreter` if you have no need to bypass the plugin.
neverAutoInterpret :: Lookup AutoInterpreter
neverAutoInterpret = pure $ \_ _ _ _ _ -> pure Nothing

type DictCache = Map DictCacheKey DictCacheEntry

type DictCacheKey = String

data DictCacheEntry = DictCacheEntry
  { -- | goalTy
    dceType :: Plugins.Type,
    dceVar :: Plugins.Var,
    -- | The actual dictionary
    dceDict :: Plugins.CoreExpr,
    -- | Index of the created dict var, used for topological sort. Contains a value
    -- if `dceVar` is created during `Categorifier.Core.BuildDictionary.buildDictionary`.
    -- A var with a smaller index is created earler, and thus may be referred to in
    -- the unfolding of a var with a bigger index.
    dceIdx :: Maybe Int
  }

-- | Various ways in which the plugin can fail to transform a term.
data CategoricalFailure
  = BareUnboxedVar Plugins.Var (Plugins.Expr Plugins.WithIdInfo)
  | CouldNotBuildDictionary Plugins.Type Plugins.CoreExpr (NonEmpty DictionaryFailure)
  | FailureToUnfix Plugins.Id Plugins.CoreExpr Plugins.CoreExpr
  | InvalidUnfixTyArgs Plugins.Id [Plugins.Var] [Plugins.Type]
  | -- | The class hierarchy that was used has no mapping for some required operation.
    MissingCategoricalRepresentation String
  | NotEnoughTypeArgs String Plugins.CoreExpr Plugins.Type [Plugins.Type]
  | NotFunTy Plugins.CoreExpr Plugins.Type
  | NotTyConApp String Plugins.Type
  | TypeMismatch String Plugins.Type Plugins.Type
  | UninlinedExpr Plugins.CoreExpr (Maybe Plugins.Unfolding)
  | UnsupportedCast Plugins.CoreExpr Plugins.Coercion
  | UnsupportedDependentType Plugins.Var (Either Plugins.Coercion Plugins.Type)
  | UnsupportedMutuallyRecursiveLetBindings [(Plugins.CoreBndr, Plugins.CoreExpr)]
  | UnsupportedPolymorphicRecursion Plugins.Id [Plugins.Var] [Plugins.Type]
  | UnexpectedUnboxedType String Plugins.Type Plugins.CoreExpr
  | CannotDeduceBoxedTypeOfBinder Plugins.Var Plugins.CoreExpr Plugins.CoreExpr
  | UnsupportedPrimitiveDataAlt Plugins.DataCon Plugins.CoreExpr
  | UnexpectedMissingDefault Plugins.CoreExpr
  | UnexpectedDoubleDefault Plugins.CoreExpr
  | UnsupportedPrimitiveLiteral Plugins.Literal Plugins.CoreExpr
  | CannotDeduceBoxedTypeOfExpr Plugins.CoreExpr Plugins.CoreExpr
  | UnsupportedPrimOpApplication Plugins.Var [Plugins.CoreExpr] (Maybe Plugins.Type)
  | UnsupportedPrimOpExpression String Plugins.CoreExpr

data DictionaryFailure
  = TypecheckFailure Plugins.ErrorMessages
  | -- | Typechecking ostensibly succeeded, but also returned errors. Not sure if this is possible,
    --   but the types allow for it. Here we treat it as a failure in order to at least diagnose the
    --   problem.
    forall r. Plugins.Outputable r => ErroneousTypecheckSuccess Plugins.ErrorMessages r
  | NoBindings
  | CoercionHoles (NonEmpty (Plugins.Bind Plugins.CoreBndr))
  | FreeIds (NonEmpty (Plugins.Id, Plugins.Kind))

-- | Construct a writer computation from a (result, output) pair inside the monad.
writerT :: Functor m => m (a, w) -> RWST r w s m a
writerT x = RWST (\_ s -> uncurry (,s,) <$> x)
