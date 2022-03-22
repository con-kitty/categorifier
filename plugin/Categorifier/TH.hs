{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Functions that should be part of @template-haskell@, but aren't.
module Categorifier.TH
  ( nameQualified,
    specializeT,
  )
where

import qualified Categorifier.Common.IO.Exception as Exception
import Control.Monad ((<=<))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy (StateT (..), evalStateT, get, modify)
import Data.Align (alignWith)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.These (These (..))
import qualified Language.Haskell.TH as TH
import PyF (fmt)

-- | It generates the fully-qualified name.
nameQualified :: TH.Name -> String
nameQualified name =
  maybe (TH.nameBase name) (\modu -> [fmt|{modu}.{TH.nameBase name}|]) $ TH.nameModule name

data SpecializationFailure
  = NotAParameterizedType
  | TooManySpecializers (NonEmpty (Maybe TH.Type))

prettySpecializationFailure :: SpecializationFailure -> String
prettySpecializationFailure = \case
  NotAParameterizedType -> "Attempted to specialize a type that doesn't have any type variables."
  TooManySpecializers extras ->
    [fmt|Specialized a parameterized type, but had leftover specializers: {show extras}|]

-- | It specializes a `TH.ForallT` or `TH.ForallVisT`.
specializeT ::
  -- | The type to specialize.
  TH.TypeQ ->
  -- | The types to specialize each parameter to. `Nothing` means to skip a paramater, leaving it
  --   unspecialized.
  [Maybe TH.TypeQ] ->
  TH.TypeQ
specializeT typ typs = do
  baseTy <- typ
  case baseTy of
    TH.ForallT bs ctx t -> flip evalStateT mempty $ do
      -- Like `TH.ForallVisT`, but also has to specialize the context and remove any constraints
      -- that have no vars left.
      (vars, newType) <- applySubsts t bs typs
      if null vars && null ctx
        then pure newType
        else TH.ForallT vars <$> fmap (filter hasVarT) (traverse go ctx) <*> pure newType
#if MIN_VERSION_template_haskell(2, 16, 0)
    TH.ForallVisT bs t -> flip evalStateT mempty $ do
      -- Add substitutions for as many args as we can. Leave the tail of the binders alone.
      (vars, newType) <- applySubsts t bs typs
      pure $
        if null vars
          then newType
          else TH.ForallVisT vars newType
#endif
    _ ->
      if null typs
        then typ
        else liftIO $ Exception.throwIOAsException prettySpecializationFailure NotAParameterizedType
  where
    nameOfBinder :: TH.TyVarBndr -> TH.Name
    nameOfBinder = \case
      TH.PlainTV n -> n
      TH.KindedTV n _ -> n
    go :: TH.Type -> StateT (Map TH.Name TH.TypeQ) TH.Q TH.Type
    go = \case
#if MIN_VERSION_template_haskell(2, 16, 0)
      TH.ForallVisT bs t -> TH.ForallVisT bs <$> go t
#endif
#if MIN_VERSION_template_haskell(2, 15, 0)
      TH.AppKindT t k -> TH.AppKindT <$> go t <*> pure k
      TH.ImplicitParamT s t -> TH.ImplicitParamT s <$> go t
#endif
      TH.ForallT bs ctx t -> TH.ForallT bs <$> fmap (filter hasVarT) (traverse go ctx) <*> go t
      TH.AppT a b -> TH.AppT <$> go a <*> go b
      TH.SigT t k -> TH.SigT <$> go t <*> pure k
      TH.VarT n ->
        -- If @n@ is substitutable, do so, otherwise leave the `TH.VarT` alone.
        lift . fromMaybe (pure $ TH.VarT n) . Map.lookup n =<< get
      TH.ConT n -> pure $ TH.ConT n
      TH.PromotedT n -> pure $ TH.ConT n
      TH.InfixT a n b -> TH.InfixT <$> go a <*> pure n <*> go b
      TH.UInfixT a n b -> TH.UInfixT <$> go a <*> pure n <*> go b
      TH.ParensT t -> TH.ParensT <$> go t
      TH.TupleT i -> pure $ TH.TupleT i
      TH.UnboxedTupleT i -> pure $ TH.UnboxedTupleT i
      TH.UnboxedSumT a -> pure $ TH.UnboxedSumT a
      TH.ArrowT -> pure TH.ArrowT
      TH.EqualityT -> pure TH.EqualityT
      TH.ListT -> pure TH.ListT
      TH.PromotedTupleT i -> pure $ TH.PromotedTupleT i
      TH.PromotedNilT -> pure TH.PromotedNilT
      TH.PromotedConsT -> pure TH.PromotedConsT
      TH.StarT -> pure TH.StarT
      TH.ConstraintT -> pure TH.ConstraintT
      TH.LitT l -> pure $ TH.LitT l
      TH.WildCardT -> pure TH.WildCardT
    applySubsts t bs args =
      let (varsM, substsM, remainingArgsM) =
            unzip3 $
              alignWith
                ( \case
                    This v -> (Just v, Nothing, Nothing)
                    That a -> (Nothing, Nothing, Just a)
                    These v Nothing -> (Just v, Nothing, Nothing)
                    These v (Just a) -> (Nothing, Just (nameOfBinder v, a), Nothing)
                )
                bs
                args
          vars = catMaybes varsM
          substs = Map.fromListWith const $ catMaybes substsM
          remainingArgs = catMaybes remainingArgsM
       in maybe
            ( do
                modify (substs <>)
                sequenceA (vars, go t)
            )
            ( liftIO
                . Exception.throwIOAsException prettySpecializationFailure
                . TooManySpecializers
                <=< lift . traverse sequenceA
            )
            $ nonEmpty remainingArgs

hasVarT :: TH.Type -> Bool
#if MIN_VERSION_template_haskell(2, 16, 0)
hasVarT (TH.ForallVisT _ _) = True
#endif
#if MIN_VERSION_template_haskell(2, 15, 0)
hasVarT (TH.AppKindT t _) = hasVarT t
hasVarT (TH.ImplicitParamT _ t) = hasVarT t
#endif
hasVarT TH.ForallT {} = True
hasVarT (TH.AppT a b) = hasVarT a || hasVarT b
hasVarT (TH.SigT t _) = hasVarT t
hasVarT (TH.VarT _) = True
hasVarT (TH.ConT _) = False
hasVarT (TH.PromotedT _) = False
hasVarT (TH.InfixT a _ b) = hasVarT a || hasVarT b
hasVarT (TH.UInfixT a _ b) = hasVarT a || hasVarT b
hasVarT (TH.ParensT t) = hasVarT t
hasVarT (TH.TupleT _) = False
hasVarT (TH.UnboxedTupleT _) = False
hasVarT (TH.UnboxedSumT _) = False
hasVarT TH.ArrowT = False
hasVarT TH.EqualityT = False
hasVarT TH.ListT = False
hasVarT (TH.PromotedTupleT _) = False
hasVarT TH.PromotedNilT = False
hasVarT TH.PromotedConsT = False
hasVarT TH.StarT = False
hasVarT TH.ConstraintT = False
hasVarT (TH.LitT _) = False
hasVarT TH.WildCardT = False
