{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

-- |
module Kitty.Plugin.Core.ErrorHandling
  ( displayPanic,
    showFailures,
    showWarnings,
  )
where

import qualified Data.ByteString.Char8 as BS
import Data.Foldable (fold, toList)
import Data.List.NonEmpty.Extra (NonEmpty, intersperse)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import ErrUtils (ErrorMessages, WarningMessages, pprErrMsgBagWithLoc)
import GHC.Stack (CallStack, SrcLoc (..), fromCallSiteList, prettyCallStack)
import qualified GhcPlugins as Plugins
import Kitty.Common.IO.Exception (displayException)
import Kitty.Plugin.Core.Trace (Unpretty (..), WithIdInfo, renderSDoc)
import Kitty.Plugin.Core.Types (CategoricalFailure (..), DictionaryFailure (..))
import Numeric.Natural (Natural)
import Panic (GhcException (..))
import PyF (fmt)

-- | Deduplicates a structure, maintaining a count for each element.
--
--  __TODO__: Once we have good source locations, we should accumulate those here instead of simply
--            counting.
countOccurances :: (Foldable t, Ord a) => t a -> Map a Natural
countOccurances = Map.fromListWith (+) . fmap (,1 :: Natural) . toList

-- | GHC gives us a `Bag.Bag` of random `Outputable.SDoc` (text) errors when something fails. This
--   tries to format them nicely and rephrase them when possible.
formatTcErrs :: Plugins.DynFlags -> Int -> ErrorMessages -> Text
formatTcErrs dflags _indent =
  Text.intercalate "\n"
    . fmap (Text.pack . renderSDoc dflags . (Plugins.text "-" Plugins.<+>) . rephraseErrMsg)
    . pprErrMsgBagWithLoc
  where
    -- TODO: This should use regex to match and replace error messages that we can report more
    --       effectively for plugin users.
    rephraseErrMsg = id

-- | Attempts to pull a `CallStack` out of a `Plugins.CoreExpr` that represents one.
--
--  __NB__: This is quick and dirty, and could possibly cause something that's not a `CallStack` to
--          become one or fail to parse a correct `CallStack`.
runtimeCallStack :: Plugins.CoreExpr -> Maybe CallStack
runtimeCallStack = \case
  Plugins.Cast stack _ -> fromCallSiteList <$> decodeStack stack
  _ -> Nothing
  where
    decodeStack = \case
      Plugins.App (Plugins.App _pushCallStack frame) rest ->
        (:) <$> decodeFrame frame <*> decodeStack rest
      Plugins.Var _emptyCallStack -> pure []
      _ -> Nothing
    decodeFrame = \case
      Plugins.App
        (Plugins.App _tuple (Plugins.App _ (Plugins.Lit (Plugins.LitString function))))
        ( Plugins.collectArgs ->
            ( _srcLoc,
              [ Plugins.App _ (Plugins.Lit (Plugins.LitString package)),
                Plugins.App _ (Plugins.Lit (Plugins.LitString modu)),
                Plugins.App _ (Plugins.Lit (Plugins.LitString file)),
                Plugins.App _ (Plugins.Lit (Plugins.LitNumber _ startLine _)),
                Plugins.App _ (Plugins.Lit (Plugins.LitNumber _ startCol _)),
                Plugins.App _ (Plugins.Lit (Plugins.LitNumber _ endLine _)),
                Plugins.App _ (Plugins.Lit (Plugins.LitNumber _ endCol _))
                ]
              )
          ) ->
          pure
            ( BS.unpack function,
              SrcLoc
                (BS.unpack package)
                (BS.unpack modu)
                (BS.unpack file)
                (fromInteger startLine)
                (fromInteger startCol)
                (fromInteger endLine)
                (fromInteger endCol)
            )
      _ -> Nothing

-- |
displayPanic :: Plugins.DynFlags -> Plugins.CoreExpr -> GhcException -> Text
displayPanic dflags calls = \case
  Panic s -> pretty (Text.pack s) Nothing
  PprPanic s doc -> pretty (Text.pack s) (Just doc)
  x -> Text.pack $ displayException x
  where
    pretty = prettyPanic dflags calls

prettyPanic :: Plugins.DynFlags -> Plugins.CoreExpr -> Text -> Maybe Plugins.SDoc -> Text
prettyPanic dflags calls msg stack =
  [fmt|The categorize plugin should never panic. However, it is difficult to avoid the
myriad panics that exist in the GHC API. You just found one. Sorry about that.
Please report the following information to the categorize plugin maintainers:
- panicking function: {msg}
- additional context: {maybe "" (renderSDoc dflags) stack}
- call site {formattedStack}|]
  where
    formattedStack =
      maybe
        ("(failed to parse runtime stack, so dumping AST):\n" <> Plugins.showPpr dflags calls)
        prettyCallStack
        (runtimeCallStack calls)

showWarnings :: Plugins.DynFlags -> WarningMessages -> Text
showWarnings dflags warns =
  [fmt|warnings during categorization:
{renderSDoc dflags . Plugins.vcat $ pprErrMsgBagWithLoc warns}|]

showFailures ::
  Plugins.DynFlags -> [Plugins.Name] -> Plugins.CoreExpr -> NonEmpty CategoricalFailure -> Text
showFailures dflags hierarchyOptions f =
  ( [fmt|Kitty.Plugin failed to categorize the following expression:
{Plugins.showPpr dflags f}|]
      <>
  )
    . Map.foldrWithKey
      ( \msg cnt acc ->
          "\n  - "
            <> msg
            <> "\n    (seen "
            <> Text.pack (show cnt)
            <> (if cnt == 1 then " time)" else " times)")
            <> acc
      )
      ""
    . countOccurances
    -- We unfortunately have to `show` before de-duplicating, because there are no reasonable
    -- instances on `CoreExpr` and similar types.
    . fmap (showFailure dflags hierarchyOptions)

showFailure :: Plugins.DynFlags -> [Plugins.Name] -> CategoricalFailure -> Text
showFailure dflags hierarchyOptions = \case
  BareUnboxedVar var expr ->
    [fmt|Found an unboxed variable ({showP var} :: {showP $ Plugins.varType var})
    that isn't wrapped in a boxing constructor in the following expression. This
    is likely a bug in the Kitty.Plugin plugin.
    {showW expr}|]
  CouldNotBuildDictionary goalTy expr errs ->
    [fmt|couldn't build dictionary for constraint

{showP goalTy}

    required by {showE expr}.|]
      <> foldMap
        ( ("\n    - " <>)
            . ( \case
                  TypecheckFailure tcErrs ->
                    [fmt|If the following errors refer to a missing instance (particularly for
     `HasRep`), it's likely that you need to define it.
      {formatTcErrs dflags 6 tcErrs}|]
                  ErroneousTypecheckSuccess tcErrs result ->
                    [fmt|typechecking appeared to succeed (returning {showP result}), but
      also returned the following errors:
      {formatTcErrs dflags 6 tcErrs}|]
                  NoBindings -> "no bindings"
                  CoercionHoles holes -> [fmt|coercion holes: {foldMap showP holes}|]
                  FreeIds ids ->
                    "free ids: "
                      <> fold
                        (intersperse ", " (fmap (\(i, t) -> showP i <> " :: " <> showP t) ids))
              )
        )
        errs
  FailureToUnfix name expr newExpr ->
    [fmt|Kitty.Plugin failed to unfix recursive function `{showP name}`.
    Original body of `{showP name}`:

    {showE expr}

    New body of `{showP name}`:

    {showE newExpr}

    The new body still contains `{showP name}`. Please file an issue against the plugin.|]
  InvalidUnfixTyArgs name tyBinders tyArgs ->
    [fmt|The Kitty.Plugin plugin attempted to unfix recursive function `{showP name}`
    with {length tyArgs} type args: `{showP tyArgs}`. However, `{showP name}`
    only has {length tyBinders} type binders: `{showP tyBinders}`.

    Please file an issue against the plugin.|]
  MissingCategoricalRepresentation name ->
    [fmt|There is no categorical representation defined for "{name} when using the
    following hierarchies: {showP hierarchyOptions}". You can try using a
    different category hierarchy, or modify the existing hierarchy definition to
    support additional operations.|]
  NotEnoughTypeArgs loc expr ty args ->
    [fmt|in {loc}, the type of

    {showE expr}

    does not have enough type arguments. Its type is
    {showP ty}.
    Expected 2 type arguments, but got {showP args}.|]
  NotFunTy expr ty ->
    [fmt|the type of

    {showE expr}

    is not a function type. Its type is
    {showP ty}|]
  NotTyConApp loc ty ->
    [fmt|in {loc}, type

    {showP ty}

    is not a TyCon app.|]
  TypeMismatch kind a b ->
    [fmt|the types {showP a} and {showP b} do not match in {kind}|]
  UninlinedExpr e unf ->
    [fmt|The Kitty.Plugin plugin was unable to inline {showE e}.|]
      <> maybe
        [fmt|   That's because the expression wasn't an application of an identifier. This is
    very likely an internal bug in Kitty.Plugin.|]
        ( \case
            Plugins.NoUnfolding ->
              [fmt|
    There is no unfolding available for the identifier. It's possible that the
    identifier is an unspecialized type class method (methods never have
    unfoldings), a name used for `RebindableSyntax`, or that the unfolding
    somehow didn't get from the definition point to the module that called
   `categorize`. There are a few things you can check. Is the definition in the
    base library? If so, report this as a bug against the plugin. Does the
    definition expose an unfolding? The module containing the definition to be
    inlined should be compiled with `-fno-omit-interface-pragmas` (this is
    implied by `-O`). That _may_ be enough, but if not, try adding an
   `inlinable` pragma to the definition or compiling with
   `-fexpose-all-unfoldings` to make _every_ operation inlinable. It's also
    important that the module containing the call to `categorize` is compiled
    with `-fno-ignore-interface-pragmas` (also implied by `-O`). If the
    unfolding that's missing is for `$j` (GHC-internal join points), you may
    need to bump `-funfolding-creation-threshold` on the modules you're
    depending on. If there is still no unfolding available, please file an issue
    against the plugin.|]
            Plugins.BootUnfolding ->
              [fmt|
    The identifier is defined in an hi-boot file, so can't be inlined.|]
            Plugins.OtherCon _ ->
              [fmt|
    No idea -- this is the `OtherCon` case.|]
            Plugins.DFunUnfolding {} ->
              [fmt|
    This should have been an inlinable dictionary function, definitely a bug in
    Kitty.Plugin.|]
            Plugins.CoreUnfolding {} ->
              [fmt|
    This should have been an inlinable core unfolding, definitely a bug in
    Kitty.Plugin.|]
        )
        unf
  ConstraintNotFound expr constraint ->
    [fmt|Kitty.Plugin encountered a `Data.Constraint.Dict`, but was unable to
    find a `CoreExpr` of type `{showP constraint}` in the following expression:

    {showE expr}

    The desired type is usually contained in the first value arg of `baddDicts`
    (i.e., `AllB c b`). Currently we only support 2-tuples for `AllB`, so
    make sure all `AllB` definitions use nested 2-tuples only.
    |]
  UnsupportedCast expr co ->
    [fmt|Kitty.Plugin can't apply the coercion `{showP $ Unpretty co}` to the expression
    {showP expr} :: {showP $ Plugins.exprType expr}|]
  UnsupportedDependentType name ty ->
    [fmt|Kitty.Plugin has no support for dependent types but one was encountered:
    \\{showP name} -> {either showP showP ty}|]
  UnsupportedMutuallyRecursiveLetBindings binds ->
    [fmt|Kitty.Plugin doesn't support mutually-recursive let bindings, but it found some:
    {showP binds}|]
  UnsupportedPolymorphicRecursion name tyBinders tyArgs ->
    [fmt|Kitty.Plugin doesn't support polymorphically-recursive functions, but it found one:
    {showP name}. Its type arguments are: `{showP tyBinders}`, but in one of the
    recursive calls, the type arguments are: `{showP tyArgs}`.|]
  UnexpectedUnboxedType lbl ty expr ->
    [fmt|Kitty.Plugin encountered an unboxed type at '{lbl}':
       expression: {showP expr} :: {showP ty}|]
  CannotDeduceBoxedTypeOfBinder bndr sc rhs ->
    [fmt|Kitty.Plugin couldn't deduce a boxed equivalent type:
                       unboxed binder: {showP bndr} :: {showP $ Plugins.varType bndr}
                            scrutinee: {showP sc}
        corresponding right-hand side: {showP rhs}|]
  CannotDeduceBoxedTypeOfExpr expr ctx ->
    [fmt|Kitty.Plugin couldn't deduce a boxed equivalent type for:
         unboxed expression: {showP expr} :: {showP $ Plugins.exprType expr}
                    context: {showP ctx}|]
  UnsupportedPrimitiveDataAlt expr con ->
    [fmt|Kitty.Plugin encountered a data-constructor match in a primitive expression:
        constructor: {showP con}
         expression: {showP expr}|]
  UnexpectedMissingDefault expr ->
    [fmt|Kitty.Plugin encountered a primitive `case`-expression with no `DEFAULT` branch:
        {showP expr}
        This is likely a problem introduced by Kitty.Plugin itself.|]
  UnexpectedDoubleDefault expr ->
    [fmt|Kitty.Plugin encountered a primitive `case`-expression with no `DEFAULT` branch:
        {showP expr}
        This violates an important GHC invariant and was likely caused by Kitty.Plugin itself.|]
  UnsupportedPrimitiveLiteral lit expr ->
    [fmt|Kitty.Plugin encountered a literal of a primitive type it can't handle:
        {showP lit}
        within
        {showP expr}|]
  UnsupportedPrimOpApplication var args boxedType ->
    let argsWithSigs =
          Text.unlines $ fmap (\e -> showP e <> " :: " <> showP (Plugins.exprType e)) args
     in [fmt|Kitty.Plugin encountered a primop application it can't handle:
         op: {showP var}
       args: {argsWithSigs}
result type: {showP boxedType}|]
  UnsupportedPrimOpExpression label expr ->
    [fmt|Kitty.Plugin encountered a primop-related expression it can't handle at '{label}':
        {showP expr}|]
  where
    showE :: Plugins.CoreExpr -> Text
    showE = showP
    showP :: Plugins.Outputable a => a -> Text
    showP = Text.pack . Plugins.showPpr dflags
    showW :: Plugins.Expr WithIdInfo -> Text
    showW = showP
