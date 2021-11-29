{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Kitty.Plugin.Core.Types
  ( AutoInterpreter,
    CategoryStack,
    DictionaryStack,
    CategoryState (..),
    DictCache,
    DictCacheKey,
    DictCacheEntry (..),
    CategoricalFailure (..),
    WithIdInfo (..),
    DictionaryFailure (..),
    writerT,
  )
where

import Control.Monad.Trans.Except (ExceptT (..))
import Control.Monad.Trans.RWS.Strict (RWST (..))
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import ErrUtils (ErrorMessages, WarningMessages)
import qualified GhcPlugins as Plugins

type CategoryStack =
  ExceptT
    (NonEmpty CategoricalFailure)
    (RWST (Map Plugins.Var Plugins.CoreExpr) WarningMessages CategoryState IO)

type DictionaryStack =
  ExceptT
    (NonEmpty DictionaryFailure)
    (RWST () WarningMessages CategoryState IO)

data CategoryState = CategoryState
  { csUniqSupply :: Plugins.UniqSupply,
    -- An incrementing index, used to populate `dceIdx`.
    csIdx :: Int,
    csDictCache :: DictCache
  }

type AutoInterpreter =
  (Plugins.Type -> DictionaryStack Plugins.CoreExpr) ->
  Plugins.Type ->
  Plugins.Type ->
  Plugins.Id ->
  [Plugins.CoreExpr] ->
  CategoryStack (Maybe Plugins.CoreExpr)

type DictCache = Map DictCacheKey DictCacheEntry

type DictCacheKey = String

data DictCacheEntry = DictCacheEntry
  { -- | goalTy
    dceType :: Plugins.Type,
    dceVar :: Plugins.Var,
    -- | The actual dictionary
    dceDict :: Plugins.CoreExpr,
    -- | Index of the created dict var, used for topological sort. Contains a value
    -- if `dceVar` is created during `Kitty.Plugin.Core.BuildDictionary.buildDictionary`.
    -- A var with a smaller index is created earler, and thus may be referred to in
    -- the unfolding of a var with a bigger index.
    dceIdx :: Maybe Int
  }

-- | Various ways in which the plugin can fail to transform a term.
data CategoricalFailure
  = BareUnboxedVar Plugins.Var (Plugins.Expr WithIdInfo)
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
  | ConstraintNotFound Plugins.CoreExpr Plugins.Type
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

newtype WithIdInfo = WithIdInfo Plugins.Id

instance Plugins.Outputable WithIdInfo where
  -- I wanted the full IdInfo, but it's not Outputtable
  ppr (WithIdInfo v) =
    Plugins.sdocWithDynFlags $ \dflags ->
      let ident =
            ( if Plugins.gopt Plugins.Opt_SuppressModulePrefixes dflags
                then id
                else
                  ( maybe
                      ""
                      (\m -> Plugins.text $ Plugins.moduleNameString (Plugins.moduleName m) <> ".")
                      (Plugins.nameModule_maybe $ Plugins.varName v)
                      Plugins.<>
                  )
            )
              $ Plugins.ppr v
       in if Plugins.gopt Plugins.Opt_SuppressTypeSignatures dflags
            then ident
            else
              Plugins.sep
                [ident, Plugins.nest 2 $ Plugins.dcolon Plugins.<+> Plugins.ppr (Plugins.varType v)]

instance Plugins.OutputableBndr WithIdInfo where
  pprInfixOcc = Plugins.ppr
  pprPrefixOcc = Plugins.ppr

data DictionaryFailure
  = TypecheckFailure ErrorMessages
  | -- | Typechecking ostensibly succeeded, but also returned errors. Not sure if this is possible,
    --   but the types allow for it. Here we treat it as a failure in order to at least diagnose the
    --   problem.
    forall r. Plugins.Outputable r => ErroneousTypecheckSuccess ErrorMessages r
  | NoBindings
  | CoercionHoles (NonEmpty (Plugins.Bind Plugins.CoreBndr))
  | FreeIds (NonEmpty (Plugins.Id, Plugins.Kind))

-- | Construct a writer computation from a (result, output) pair inside the monad.
writerT :: Functor m => m (a, w) -> RWST r w s m a
writerT x = RWST (\_ s -> uncurry (,s,) <$> x)
