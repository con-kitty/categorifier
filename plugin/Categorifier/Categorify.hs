{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
#if MIN_VERSION_GLASGOW_HASKELL(9, 0, 0, 0)
{-# LANGUAGE LinearTypes #-}
#endif
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | These are the operations to use with the "Categorifier" plugin, which trigger conversion from
--   @->@ to some target category.
--
--  __NB__: This module is expected to be imported qualified.
module Categorifier.Categorify
  ( UnconvertedCall (..),
    expression,
    function,
    functionAs,
    functionOnly,
    functionOnlyAs,
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
import Data.Maybe (fromMaybe)
import GHC.Stack (CallStack, HasCallStack, callStack, prettyCallStack)
import PyF (fmt)

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
#if MIN_VERSION_GLASGOW_HASKELL(9, 0, 0, 0)
expression :: forall q c a b. HasCallStack => (a %q -> b) -> a `c` b
#else
expression :: forall c a b. HasCallStack => (a -> b) -> a `c` b
#endif
expression f = Exception.impureThrow $ UnconvertedCall f callStack
{-# NOINLINE expression #-}

-- | An exception thrown at runtime if `Categorifier.plugin` either isn't available, or couldn't
--   compile away a call to `expression`.
#if MIN_VERSION_GLASGOW_HASKELL(9, 0, 0, 0)
data UnconvertedCall = forall q a b. UnconvertedCall (a %q -> b) CallStack
#else
data UnconvertedCall = forall a b. UnconvertedCall (a -> b) CallStack
#endif

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

generateResultName ::
  TH.Name ->
  -- | The target category type
  TH.TypeQ ->
  -- | A list of types for specializing the type of the provided `TH.Name`
  [Maybe TH.TypeQ] ->
  TH.Q String
generateResultName name _k _tys = pure [fmt|wrap_{TH.nameBase name}|]

-- | Like `function`, but doesn't introduce a `NativeCat` instance. The tradeoffs are that this
--   doesn't produce orphan instances or require @DataKinds@, however it means that categorified
--   functions which contain calls to functions categorified with `functionOnly` won't be able to
--   take advantage of this categorification and will instead re-categorify the function.
functionOnly ::
  -- | The name of the function being categorified
  TH.Name ->
  -- | The target category type
  TH.TypeQ ->
  -- | A list of types for specializing the type of the provided `TH.Name`
  [Maybe TH.TypeQ] ->
  TH.DecsQ
functionOnly name k tys = do
  newName <- generateResultName name k tys
  functionOnlyAs newName name k tys

-- | Like `functionAs`, but with the same tradeoffs as `functionOnly` relative to `function`.
functionOnlyAs ::
  -- | The name to use for the categorified result
  String ->
  -- | The name of the function being categorified
  TH.Name ->
  -- | The target category type
  TH.TypeQ ->
  -- | A list of types for specializing the type of the provided `TH.Name`
  [Maybe TH.TypeQ] ->
  TH.DecsQ
functionOnlyAs newName oldName k tys = do
  ((vs, ctx), (input, output)) <- TH.splitTy =<< TH.specializeT (TH.reifyType oldName) tys
  functionOnlyAs' (TH.mkName newName) oldName vs ctx k input output

functionOnlyAs' ::
  TH.Name -> TH.Name -> [TH.TyVarBndr flag] -> TH.Cxt -> TH.TypeQ -> TH.Type -> TH.Type -> TH.DecsQ
functionOnlyAs' newName oldName _vs ctx k input output =
  sequenceA
    [ TH.sigD newName $ TH.forallT [] (pure ctx) [t|$k $(pure input) $(pure output)|],
      TH.funD newName [TH.clause [] (TH.normalB [e|expression $(TH.varE oldName)|]) []]
    ]

-- | Use instead of `expression` when you're applying it to a named function. Makes it more robust
--   against types changing and generates a `NativeCat` instance that allows us to categorify this
--   function separately from a larger expression that calls it, which allows us to re-use the
--   categorified result rather than re-categorfying at each call-site.
--
--  __NB__: This currently requires the type to specialize any constrained vars.
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
  ((vs, ctx), (input, output)) <- TH.splitTy =<< TH.specializeT (TH.reifyType oldName) tys
  -- __TODO__: Fail if there's no module, because the name isn't global.
  let (modu, base) = (fromMaybe "" . TH.nameModule &&& TH.nameBase) oldName
      newName' = TH.mkName newName
      originalName = [fmt|{modu}.{base}|]
  liftA2
    (<>)
    (functionOnlyAs' newName' oldName vs ctx k input output)
    ( pure
        <$> TH.instanceD
          (pure ctx)
          [t|
            NativeCat
              $k
              $(TH.litT $ TH.strTyLit originalName)
              $(pure input)
              $(pure output)
            |]
          [TH.funD 'nativeK [TH.clause [] (TH.NormalB <$> TH.varE newName') []]]
    )
