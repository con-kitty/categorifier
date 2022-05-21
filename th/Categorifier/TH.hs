{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Functions that should be part of @template-haskell@, but aren't.
--
--   This also re-exports all of "Language.Haskell.TH", replacing some identifiers with ones that
--   work across more versions.
module Categorifier.TH
  ( module Language.Haskell.TH,
    TyVarBndr,
    pattern PlainTV,
    pattern KindedTV,
    alphaEquiv,
    alphaRename,
    conP,
    nameQualified,
    reifyType,
    specializeT,
    splitTy,
    tyVarBndrName,
  )
where

import qualified Categorifier.Common.IO.Exception as Exception
import Categorifier.Duoidal (Parallel (..), traverseD, (<*\>))
import Categorifier.Duoidal.Either (noteAccum)
import Control.Monad ((<=<))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy (StateT (..), evalStateT, get, modify)
import Data.Align (alignWith)
import Data.Bifunctor (Bifunctor (..))
import Data.Bool (bool)
import Data.Foldable (foldl')
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.These (These (..))
import Language.Haskell.TH hiding (TyVarBndr (..), conP, reifyType)
import qualified Language.Haskell.TH as TH
import PyF (fmt)

#if MIN_VERSION_template_haskell(2, 17, 0)
type TyVarBndr flag = TH.TyVarBndr flag

pattern PlainTV :: TH.Name -> TyVarBndr flag
pattern PlainTV n <- TH.PlainTV n _

pattern KindedTV :: TH.Name -> TH.Kind -> TyVarBndr flag
pattern KindedTV n k <- TH.KindedTV n _ k
#else
type TyVarBndr _flag = TH.TyVarBndr

pattern PlainTV :: TH.Name -> TyVarBndr flag
pattern PlainTV n = TH.PlainTV n

pattern KindedTV :: TH.Name -> TH.Kind -> TyVarBndr flag
pattern KindedTV n k = TH.KindedTV n k
#endif

{-# COMPLETE PlainTV, KindedTV #-}

conP :: TH.Name -> [TH.Pat] -> TH.Pat
#if MIN_VERSION_template_haskell(2, 18, 0)
conP n = TH.ConP n []
#else
conP = TH.ConP
#endif

reifyType :: TH.Name -> TH.TypeQ
#if MIN_VERSION_template_haskell(2, 16, 0)
reifyType = TH.reifyType
#else
reifyType =
  (\case
      TH.ClassOpI _ ty _ -> pure ty
      TH.DataConI _ ty _ -> pure ty
      TH.VarI _ ty _ -> pure ty
      _ -> fail "Tried to reify the type of a term that isn't a function"
  )
    <=< TH.reify
#endif

tyVarBndrName :: TyVarBndr flag -> TH.Name
tyVarBndrName (KindedTV n _) = n
tyVarBndrName (PlainTV n) = n

-- | It generates the fully-qualified name.
nameQualified :: TH.Name -> String
nameQualified name =
  maybe (TH.nameBase name) (\modu -> [fmt|{modu}.{TH.nameBase name}|]) $ TH.nameModule name

-- | Checks if two types are alpha equivalent. If so, it returns a mapping between the variable
--   names, usable in `alphaRename`.
--
--  __TODO__: Currently just ignores var names, but should eventually keep track of them.
alphaEquiv :: TH.Type -> TH.Type -> Maybe [(TH.Name, TH.Name)]
alphaEquiv = curry alphaEquiv'

alphaEquiv' :: (TH.Type, TH.Type) -> Maybe [(TH.Name, TH.Name)]
#if MIN_VERSION_template_haskell(2, 17, 0)
alphaEquiv' (TH.MulArrowT, TH.MulArrowT) = pure []
#endif
#if MIN_VERSION_template_haskell(2, 16, 0)
alphaEquiv' (TH.ForallVisT b t, TH.ForallVisT b' t') =
  -- __TODO__: Ensure that the kinds of @b@ match, and that those names are added to the map
  --          (order of @b@ matters)
  bool Nothing (alphaEquiv t t') $ length b == length b'
#endif
alphaEquiv' ty = case ty of
  (TH.ForallT b c t, TH.ForallT b' c' t') ->
    -- __TODO__: Ensure that the kinds of @b@ match, and that those names are added to the map
    --          (order of @b@ matters)
    bool Nothing (alphaEquiv t t') $ length b == length b' && c == c'
  (TH.AppKindT t k, TH.AppKindT t' k') -> (<>) <$> alphaEquiv t t' <*> alphaEquiv k k'
  (TH.AppT c e, TH.AppT c' e') -> (<>) <$> alphaEquiv c c' <*> alphaEquiv e e'
  (TH.SigT t k, TH.SigT t' k') -> (<>) <$> alphaEquiv t t' <*> alphaEquiv k k'
  (TH.VarT n, TH.VarT n') ->
    -- __TODO__: If neither has been seen before, add them both with the same index, otherwise they
    --           must already have the same index to be the same
    pure [(n, n')]
  (TH.ConT n, TH.ConT n') -> bool Nothing (pure []) $ n == n'
  (TH.PromotedT n, TH.PromotedT n') -> bool Nothing (pure []) $ n == n'
  (TH.ImplicitParamT s t, TH.ImplicitParamT s' t') -> bool Nothing (alphaEquiv t t') $ s == s'
  (TH.InfixT t n u, TH.InfixT t' n' u') ->
    bool Nothing ((<>) <$> alphaEquiv t t' <*> alphaEquiv u u') $ n == n'
  (TH.UInfixT t n u, TH.UInfixT t' n' u') ->
    bool Nothing ((<>) <$> alphaEquiv t t' <*> alphaEquiv u u') $ n == n'
  (TH.ParensT t, TH.ParensT t') -> alphaEquiv t t'
  (TH.TupleT i, TH.TupleT i') -> bool Nothing (pure []) $ i == i'
  (TH.UnboxedTupleT i, TH.UnboxedTupleT i') -> bool Nothing (pure []) $ i == i'
  (TH.UnboxedSumT i, TH.UnboxedSumT i') -> bool Nothing (pure []) $ i == i'
  (TH.ArrowT, TH.ArrowT) -> pure []
  (TH.EqualityT, TH.EqualityT) -> pure []
  (TH.ListT, TH.ListT) -> pure []
  (TH.PromotedTupleT i, TH.PromotedTupleT i') -> bool Nothing (pure []) $ i == i'
  (TH.PromotedNilT, TH.PromotedNilT) -> pure []
  (TH.PromotedConsT, TH.PromotedConsT) -> pure []
  (TH.StarT, TH.StarT) -> pure []
  (TH.ConstraintT, TH.ConstraintT) -> pure []
  (TH.LitT t, TH.LitT t') -> bool Nothing (pure []) $ t == t'
  (TH.WildCardT, TH.WildCardT) -> pure []
  (_, _) -> Nothing -- any structural mismatch is not equivalent

-- | Renames the varibles in a type according to some equivalence mapping.
alphaRename :: [(TH.Name, TH.Name)] -> TH.Type -> Either (NonEmpty TH.Name) TH.Type
alphaRename mapping = first NE.nub . alphaRename' mapping

alphaRename' :: [(TH.Name, TH.Name)] -> TH.Type -> Either (NonEmpty TH.Name) TH.Type
#if MIN_VERSION_template_haskell(2, 17, 0)
alphaRename' _ TH.MulArrowT = pure TH.MulArrowT
#endif
#if MIN_VERSION_template_haskell(2, 16, 0)
alphaRename' m (TH.ForallVisT b t) = TH.ForallVisT b <$> alphaRename' m t
#endif
alphaRename' m (TH.ForallT b c t) =
  TH.ForallT b <$> traverseD (alphaRename' m) c <*\> alphaRename' m t
alphaRename' m (TH.AppKindT t k) = TH.AppKindT <$> alphaRename' m t <*\> alphaRename' m k
alphaRename' m (TH.AppT c e) = TH.AppT <$> alphaRename' m c <*\> alphaRename' m e
alphaRename' m (TH.SigT t k) = TH.SigT <$> alphaRename' m t <*\> alphaRename' m k
alphaRename' m (TH.VarT n) = fmap TH.VarT . getParallel $ noteAccum (flip lookup m) n
alphaRename' _ (TH.ConT n) = pure $ TH.ConT n
alphaRename' _ (TH.PromotedT n) = pure $ TH.PromotedT n
alphaRename' m (TH.ImplicitParamT s t) = TH.ImplicitParamT s <$> alphaRename' m t
alphaRename' m (TH.InfixT t n t') =
  TH.InfixT <$> alphaRename' m t <*\> pure n <*\> alphaRename' m t'
alphaRename' m (TH.UInfixT t n t') =
  TH.UInfixT <$> alphaRename' m t <*\> pure n <*\> alphaRename' m t'
alphaRename' m (TH.ParensT t) = TH.ParensT <$> alphaRename' m t
alphaRename' _ (TH.TupleT i) = pure $ TH.TupleT i
alphaRename' _ (TH.UnboxedTupleT i) = pure $ TH.UnboxedTupleT i
alphaRename' _ (TH.UnboxedSumT i) = pure $ TH.UnboxedSumT i
alphaRename' _ TH.ArrowT = pure TH.ArrowT
alphaRename' _ TH.EqualityT = pure TH.EqualityT
alphaRename' _ TH.ListT = pure TH.ListT
alphaRename' _ (TH.PromotedTupleT i) = pure $ TH.PromotedTupleT i
alphaRename' _ TH.PromotedNilT = pure TH.PromotedNilT
alphaRename' _ TH.PromotedConsT = pure TH.PromotedConsT
alphaRename' _ TH.StarT = pure TH.StarT
alphaRename' _ TH.ConstraintT = pure TH.ConstraintT
alphaRename' _ (TH.LitT l) = pure $ TH.LitT l
alphaRename' _ TH.WildCardT = pure TH.WildCardT

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
specializeT typ typs = flip toplevelSpecializeT typs =<< typ

toplevelSpecializeT :: TH.Type -> [Maybe TH.TypeQ] -> TH.TypeQ
toplevelSpecializeT (TH.ForallT bs ctx t) typs =
  flip evalStateT mempty $ do
    -- Like `TH.ForallVisT`, but also has to specialize the context and remove any constraints
    -- that have no vars left.
    (vars, newType) <- applySubsts t bs typs
    if null vars && null ctx
      then pure newType
      else TH.ForallT vars <$> fmap (filter hasVarT) (traverse specializeT' ctx) <*> pure newType
#if MIN_VERSION_template_haskell(2, 16, 0)
toplevelSpecializeT (TH.ForallVisT bs t) typs =
  flip evalStateT mempty $ do
    -- Add substitutions for as many args as we can. Leave the tail of the binders alone.
    (vars, newType) <- applySubsts t bs typs
    pure $
      if null vars
        then newType
        else TH.ForallVisT vars newType
#endif
toplevelSpecializeT baseTy typs =
  if null typs
    then pure baseTy
    else liftIO $ Exception.throwIOAsException prettySpecializationFailure NotAParameterizedType

specializeT' :: TH.Type -> StateT (Map TH.Name TH.TypeQ) TH.Q TH.Type
#if MIN_VERSION_template_haskell(2, 17, 0)
specializeT' TH.MulArrowT = pure TH.MulArrowT
#endif
#if MIN_VERSION_template_haskell(2, 16, 0)
specializeT' (TH.ForallVisT bs t) = TH.ForallVisT bs <$> specializeT' t
#endif
specializeT' (TH.ForallT bs ctx t) =
  TH.ForallT bs <$> fmap (filter hasVarT) (traverse specializeT' ctx) <*> specializeT' t
specializeT' (TH.AppKindT t k) = TH.AppKindT <$> specializeT' t <*> pure k
specializeT' (TH.AppT a b) = TH.AppT <$> specializeT' a <*> specializeT' b
specializeT' (TH.SigT t k) = TH.SigT <$> specializeT' t <*> pure k
-- If @n@ is substitutable, do so, otherwise leave the `TH.VarT` alone.
specializeT' (TH.VarT n) = lift . fromMaybe (pure $ TH.VarT n) . Map.lookup n =<< get
specializeT' (TH.ConT n) = pure $ TH.ConT n
specializeT' (TH.PromotedT n) = pure $ TH.ConT n
specializeT' (TH.ImplicitParamT s t) = TH.ImplicitParamT s <$> specializeT' t
specializeT' (TH.InfixT a n b) = TH.InfixT <$> specializeT' a <*> pure n <*> specializeT' b
specializeT' (TH.UInfixT a n b) = TH.UInfixT <$> specializeT' a <*> pure n <*> specializeT' b
specializeT' (TH.ParensT t) = TH.ParensT <$> specializeT' t
specializeT' (TH.TupleT i) = pure $ TH.TupleT i
specializeT' (TH.UnboxedTupleT i) = pure $ TH.UnboxedTupleT i
specializeT' (TH.UnboxedSumT a) = pure $ TH.UnboxedSumT a
specializeT' TH.ArrowT = pure TH.ArrowT
specializeT' TH.EqualityT = pure TH.EqualityT
specializeT' TH.ListT = pure TH.ListT
specializeT' (TH.PromotedTupleT i) = pure $ TH.PromotedTupleT i
specializeT' TH.PromotedNilT = pure TH.PromotedNilT
specializeT' TH.PromotedConsT = pure TH.PromotedConsT
specializeT' TH.StarT = pure TH.StarT
specializeT' TH.ConstraintT = pure TH.ConstraintT
specializeT' (TH.LitT l) = pure $ TH.LitT l
specializeT' TH.WildCardT = pure TH.WildCardT

applySubsts ::
  TH.Type ->
  [TyVarBndr flag] ->
  [Maybe TH.TypeQ] ->
  StateT (Map TH.Name TH.TypeQ) TH.Q ([TyVarBndr flag], TH.Type)
applySubsts t bs args =
  let (varsM, substsM, remainingArgsM) =
        unzip3 $
          alignWith
            ( \case
                This v -> (Just v, Nothing, Nothing)
                That a -> (Nothing, Nothing, Just a)
                These v Nothing -> (Just v, Nothing, Nothing)
                These v (Just a) -> (Nothing, Just (tyVarBndrName v, a), Nothing)
            )
            bs
            args
      vars = catMaybes varsM
      substs = Map.fromListWith const $ catMaybes substsM
      remainingArgs = catMaybes remainingArgsM
   in maybe
        ( do
            modify (substs <>)
            sequenceA (vars, specializeT' t)
        )
        ( liftIO
            . Exception.throwIOAsException prettySpecializationFailure
            . TooManySpecializers
            <=< lift . traverse sequenceA
        )
        $ nonEmpty remainingArgs

hasVarT :: TH.Type -> Bool
#if MIN_VERSION_template_haskell(2, 17, 0)
hasVarT TH.MulArrowT = False
#endif
#if MIN_VERSION_template_haskell(2, 16, 0)
hasVarT (TH.ForallVisT _ _) = True
#endif
hasVarT TH.ForallT {} = True
hasVarT (TH.AppKindT t _) = hasVarT t
hasVarT (TH.AppT a b) = hasVarT a || hasVarT b
hasVarT (TH.SigT t _) = hasVarT t
hasVarT (TH.VarT _) = True
hasVarT (TH.ConT _) = False
hasVarT (TH.PromotedT _) = False
hasVarT (TH.ImplicitParamT _ t) = hasVarT t
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

#if MIN_VERSION_template_haskell(2, 17, 0)
splitTy :: TH.Type -> TH.Q (([TyVarBndr TH.Specificity], TH.Cxt), (TH.Type, TH.Type))
#else
splitTy :: TH.Type -> TH.Q (([TyVarBndr ()], TH.Cxt), (TH.Type, TH.Type))
#endif
splitTy (TH.AppT (TH.AppT TH.ArrowT inp) outp) = pure (mempty, (inp, outp))
splitTy (TH.ForallT vs ctx t) = first ((vs, ctx) <>) <$> splitTy t
#if MIN_VERSION_template_haskell(2, 16, 0)
splitTy (TH.ForallVisT _ t) = splitTy t
#endif
splitTy typ = Exception.throwIOAsException (("unsupported type " <>) . show) typ
