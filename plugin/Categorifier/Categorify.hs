{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | These are the operations to use with the "Categorifier" plugin, which trigger conversion from
--  `(->)` to some target category.
--
--  __NB__: This module is expected to be imported qualified.
module Categorifier.Categorify
  ( UnconvertedCall (..),
    expression,
    function,
    functionAs,
    separately,
    separatelyAs,
  )
where

import Categorifier.Category (NativeCat (..))
-- The instances from "Categorifier.Client" aren't used in this module, but must be in scope for
-- much of categorification to succeed.
import Categorifier.Client ()
import Categorifier.Common.IO.Exception (Exception)
import qualified Categorifier.Common.IO.Exception as Exception
import qualified Categorifier.TH as TH
import Control.Applicative (liftA2)
import Control.Arrow (Arrow (..))
#if !MIN_VERSION_template_haskell(2, 16, 0)
import Control.Monad ((<=<))
#endif
import Data.Maybe (fromMaybe)
import GHC.Stack (CallStack, HasCallStack, callStack, prettyCallStack)
import qualified Language.Haskell.TH as TH
import PyF (fmt)

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

-- | The name of the module for the plugin.
pluginModule :: String
pluginModule =
  -- This would ideally be more like @`TH.moduleName` 'Categorifier.plugin@, but that would cause a
  -- circular dependency, so we just hardcode the `String` and try to be careful.
  "Categorifier"

-- | This is a pseudo-function that represents a functor from __Hask__ to some target category
--  __C__. It should be replaced at compile-time by `Categorifier.plugin`. If it's not replaced, it
--   will always `Exception.impureThrow` an `UnconvertedCall` `Exception`.
--
--   @c@ is the type of arrows in the target category (__C__). It's perhaps surprising that there
--   are no constraints on this type parameter, but here we don't even know which type class
--   hierarchy will be used to constrain it, let alone whether we'll need a @Category@ or a
--   @CartesianClosedCategory@ or what. The categorical transformation will identify the needed
--   classes and ensure they're satisified.
--
--  __NB__: While it may be possible to categorify a particular function using a fairly weak set of
--          constraints, different optimizations may change the exact transformation that
--          happens. We currently make no guarantee that we find the minimal requirements, nor that
--          the conversion will be the same regardless of optimizations. Finally, the plugin doesn't
--          attempt to do any more inlining or other simplification than is strictly
--          necessary. However, it can often generate trivially-simplifiable expressions (like
--          @curry . uncurry@). It's the responsibility of the hierarchy-providing library to
--          provide @rules@ that will reduce that to @id@, and the responsibility of the user to
--          compile with flags that enable those rules.
expression :: forall c a b. HasCallStack => (a -> b) -> a `c` b
expression f = Exception.impureThrow $ UnconvertedCall f callStack
{-# NOINLINE expression #-}

-- | An exception thrown at runtime if `Categorifier.plugin` either isn't available, or couldn't
--   compile away a call to `expression`.
data UnconvertedCall = forall a b. UnconvertedCall (a -> b) CallStack

-- | Defined because it's required by the `Exception` instance, this is not a standard `Show`.
instance Show UnconvertedCall where
  show (UnconvertedCall _ calls) =
    let functionName = TH.nameQualified 'expression
     in [fmt|error: {pluginModule} failed to eliminate a call to `{functionName}`.
  | This should only be possible if the module mentioned above was compiled
  | without the {pluginModule} plugin enabled. Ensure that you're configuring
  | it properly for your build process. E.g., passing `-fplugin={pluginModule}`
  | to GHC directly.
  |
  | It's also possible that some other plugin that you've enabled has interfered
  | with this one. If you've enabled other plugins, try permuting the order of
  | the `-fplugin` flags. (GHC installs the plugins in the /reverse/ order that
  | `-fplugin` flags are provided on the command line.)

{prettyCallStack calls}|]

instance Exception UnconvertedCall

#if MIN_VERSION_template_haskell(2, 17, 0)
type TyVarBndr = TH.TyVarBndr TH.Specificity
#else
type TyVarBndr = TH.TyVarBndr
#endif

splitTy :: TH.Type -> TH.Q (([TyVarBndr], TH.Cxt), (TH.Type, TH.Type))
splitTy (TH.AppT (TH.AppT TH.ArrowT inp) outp) = pure (mempty, (inp, outp))
splitTy (TH.ForallT vs ctx t) = first ((vs, ctx) <>) <$> splitTy t
#if MIN_VERSION_template_haskell(2, 16, 0)
splitTy (TH.ForallVisT _ t) = splitTy t
#endif
splitTy typ = Exception.throwIOAsException (("unsupported type " <>) . show) typ

generateResultName ::
  TH.Name ->
  -- | The target category type
  TH.TypeQ ->
  -- | A list of types for specializing the type of the provided `TH.Name`
  [Maybe TH.TypeQ] ->
  TH.Q String
generateResultName name _k _tys = pure [fmt|wrap_{TH.nameBase name}|]

-- | Shorthand for `expression` when you're applying it to a named function. Makes it more robust
--   against types changing.
function ::
  -- | The name of the function being categorified
  TH.Name ->
  -- | The target category type
  TH.TypeQ ->
  -- | A list of types for specializing the type of the provided `TH.Name`
  [Maybe TH.TypeQ] ->
  TH.DecsQ
function name k tys = do
  newName <- generateResultName name k tys
  functionAs newName name k tys

-- | Like `function`, but allows you to choose an explict name for the categorified result.
--
--   One reason to use this is that it keeps names more stable. If the name of the function to
--   categorify changes, `function` would force you to update every caller of the categorified
--   result, but with `functionAs`, those call sites can be left untouched.
functionAs ::
  -- | The name to use for the categorified result
  String ->
  -- | The name of the function being categorified
  TH.Name ->
  -- | The target category type
  TH.TypeQ ->
  -- | A list of types for specializing the type of the provided `TH.Name`
  [Maybe TH.TypeQ] ->
  TH.DecsQ
functionAs newName oldName k tys = do
  ((vs, ctx), (input, output)) <- splitTy =<< TH.specializeT (reifyType oldName) tys
  functionAs' (TH.mkName newName) oldName vs ctx k input output

functionAs' ::
  TH.Name -> TH.Name -> [TyVarBndr] -> TH.Cxt -> TH.TypeQ -> TH.Type -> TH.Type -> TH.DecsQ
functionAs' newName oldName _vs ctx k input output =
  sequenceA
    [ TH.sigD newName $ TH.forallT [] (pure ctx) [t|$k $(pure input) $(pure output)|],
      TH.funD newName [TH.clause [] (TH.normalB [|expression $(TH.varE oldName)|]) []]
    ]

-- | Generates a `NativeCat` instance that allows us to categorify this function separately from a
--   larger expression that calls it.
--
--   This also exposes the same name that `function` does, to avoid categorifying the same
--   definition multiple times.
--
--  __NB__: THis currently requires the type to specialize any constrained vars.
separately ::
  -- | The name of the function being categorified
  TH.Name ->
  -- | The target category type
  TH.TypeQ ->
  -- | A list of types for specializing the type of the provided `TH.Name`
  [Maybe TH.TypeQ] ->
  TH.DecsQ
separately name k tys = do
  newName <- generateResultName name k tys
  separatelyAs newName name k tys

-- | Like `separately`, but allows you to choose an explict name for the categorified result.
separatelyAs ::
  -- | The name to use for the categorified result
  String ->
  -- | The name of the function being categorified
  TH.Name ->
  -- | The target category type
  TH.TypeQ ->
  -- | A list of types for specializing the type of the provided `TH.Name`
  [Maybe TH.TypeQ] ->
  TH.DecsQ
separatelyAs newName oldName k tys = do
  ((vs, ctx), (input, output)) <- splitTy =<< TH.specializeT (reifyType oldName) tys
  -- __TODO__: Fail if there's no module, because the name isn't global.
  let (modu, base) = (fromMaybe "" . TH.nameModule &&& TH.nameBase) oldName
      newName' = TH.mkName newName
  liftA2
    (<>)
    (functionAs' newName' oldName vs ctx k input output)
    ( pure
        <$> TH.instanceD
          (pure ctx)
          [t|
            NativeCat
              $k
              $(TH.litT $ TH.strTyLit [fmt|{modu}.{base}|])
              $(pure input)
              $(pure output)
            |]
          [TH.funD 'nativeK [TH.clause [] (TH.NormalB <$> TH.varE newName') []]]
    )
