{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

-- | The plumbing for converting the category-agnostic representation to some specific type class
--   hierarchy.
module Categorifier.Core.Makers
  ( Makers (..),
    haskMakers,
    extract2TypeArgs,
    getMorphismType,
    isFreeIn,
    isCalledIn,
  )
where

import Categorifier.Core.BuildDictionary (buildDictionary)
import Categorifier.Core.Types (CategoricalFailure (..), CategoryStack)
import Categorifier.Hierarchy (HaskOps (..), Hierarchy (..))
import Control.Monad ((<=<))
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import Control.Monad.Trans.RWS.Strict (withRWST)
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (Bitraversable (..))
import Data.Either.Extra (maybeToEither)
import Data.Generics.Uniplate.Data (universeBi)
import qualified GhcPlugins as Plugins
#if MIN_VERSION_ghc(8, 10, 0)
import qualified TyCoRep
#endif

-- Need Uniplate for traversals on GHC-provided recursive types
{-# ANN module ("HLint: ignore Avoid restricted module" :: String) #-}

data Makers = Makers
  { mkAbs :: Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkAbstC :: Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkACos :: Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkACosh :: Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkAnd :: CategoryStack Plugins.CoreExpr,
    mkAp :: Plugins.Type -> Plugins.Type -> Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkAppend :: Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkApply :: Plugins.Type -> Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkArcTan2 :: Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkASin :: Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkASinh :: Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkATan :: Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkATanh :: Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkBind :: Plugins.Type -> Plugins.Type -> Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkBottom :: Plugins.Type -> Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkCoerce :: Plugins.Type -> Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkCompare :: Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkCompose :: Plugins.Type -> Plugins.Type -> Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkConst :: Plugins.Type -> Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkConstraint :: Plugins.Type -> Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkCos :: Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkCosh :: Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkCurry :: Plugins.Type -> Plugins.Type -> Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkDistl :: Plugins.Type -> Plugins.Type -> Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkDiv :: Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkDivide :: Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkDoubleToFloat :: CategoryStack Plugins.CoreExpr,
    mkEqual :: Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkEven :: Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkExl :: Plugins.Type -> Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkExp :: Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkExr :: Plugins.Type -> Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkFix :: Plugins.Type -> Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkFloatToDouble :: CategoryStack Plugins.CoreExpr,
    mkFmod :: Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkFork :: Plugins.Type -> Plugins.Type -> Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkFPIsNegativeZero :: Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkFPIsInfinite :: Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkFPIsFinite :: Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkFPIsNaN :: Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkFPIsDenormal :: Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkFromInteger :: Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkFromIntegral :: Plugins.Type -> Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkGE :: Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkGT :: Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkId :: Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkIf :: Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkIndex :: Plugins.Type -> Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkInl :: Plugins.Type -> Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkInr :: Plugins.Type -> Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkJoin :: Plugins.Type -> Plugins.Type -> Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkLAssoc :: Plugins.Type -> Plugins.Type -> Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkLE :: Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkLT :: Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkLiftA2 ::
      Plugins.Type ->
      Plugins.Type ->
      Plugins.Type ->
      Plugins.Type ->
      CategoryStack Plugins.CoreExpr,
    mkLog :: Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkMap :: Plugins.Type -> Plugins.Type -> Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkMax :: Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkMaximum :: Plugins.Type -> Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkMin :: Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkMinimum :: Plugins.Type -> Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkMinus :: Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkMod :: Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkNative :: Plugins.Type -> Plugins.Type -> Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkNegate :: Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkNot :: CategoryStack Plugins.CoreExpr,
    mkNotEqual :: Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkOdd :: Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkOr :: CategoryStack Plugins.CoreExpr,
    mkPlus :: Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkPoint :: Plugins.Type -> Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkPow :: Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkPowI :: Plugins.Type -> Plugins.Type -> Plugins.CoreExpr -> CategoryStack Plugins.CoreExpr,
    mkPowInt :: Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkQuot :: Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkRAssoc :: Plugins.Type -> Plugins.Type -> Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkRealToFrac :: Plugins.Type -> Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkRecip :: Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkRem :: Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkReprC :: Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkSequenceA :: Plugins.Type -> Plugins.Type -> Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkSignum :: Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkSin :: Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkSinh :: Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkSqrt :: Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkStrength :: Plugins.Type -> Plugins.Type -> Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkSwap :: Plugins.Type -> Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkTan :: Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkTanh :: Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkTabulate :: Plugins.Type -> Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkTimes :: Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkTraverse ::
      Plugins.Type ->
      Plugins.Type ->
      Plugins.Type ->
      Plugins.Type ->
      CategoryStack Plugins.CoreExpr,
    mkUncurry :: Plugins.Type -> Plugins.Type -> Plugins.Type -> CategoryStack Plugins.CoreExpr,
    -- TODO: These are in Hask, probably makes sense to move them to a separate structure.
    mkAbst :: Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkApply2 ::
      Maybe
        ( Plugins.Type ->
          Plugins.Type ->
          Plugins.Type ->
          Plugins.CoreExpr ->
          Plugins.CoreExpr ->
          CategoryStack Plugins.CoreExpr
        ),
    mkCompose2 ::
      Maybe
        ( Plugins.Type ->
          Plugins.Type ->
          Plugins.Type ->
          Plugins.Type ->
          Plugins.CoreExpr ->
          Plugins.CoreExpr ->
          CategoryStack Plugins.CoreExpr
        ),
    mkEither ::
      Plugins.CoreExpr ->
      Plugins.CoreExpr ->
      Plugins.CoreExpr ->
      CategoryStack Plugins.CoreExpr,
    mkFfcall ::
      Plugins.Type ->
      Plugins.Type ->
      Plugins.Type ->
      Plugins.CoreExpr ->
      Plugins.CoreExpr ->
      CategoryStack Plugins.CoreExpr,
    mkFixH :: Plugins.Type -> Plugins.CoreExpr -> Plugins.CoreExpr,
    mkFst :: Plugins.CoreExpr -> CategoryStack Plugins.CoreExpr,
    mkIndirection ::
      Plugins.Type ->
      Plugins.Type ->
      Plugins.CoreExpr ->
      CategoryStack Plugins.CoreExpr,
    mkRepr :: Plugins.Type -> CategoryStack Plugins.CoreExpr,
    mkSnd :: Plugins.CoreExpr -> CategoryStack Plugins.CoreExpr,
    mkCurryH :: Plugins.CoreExpr -> CategoryStack Plugins.CoreExpr
  }

haskMakers ::
  Plugins.DynFlags ->
  Plugins.InScopeEnv ->
  Plugins.ModGuts ->
  Plugins.HscEnv ->
  HaskOps CategoryStack ->
  Hierarchy CategoryStack ->
  Plugins.Type ->
  Makers
haskMakers dflags inScope guts hscEnv HaskOps {..} Hierarchy {..} cat =
  Makers
    { mkAbs = tys1 "abs" absV,
      mkAbstC = tys1 "abstC" abstCV,
      mkACos = tys1 "acos" acosV,
      mkACosh = tys1 "acosh" acoshV,
      mkAnd = tys0 "&&" andV,
      mkAp = tys3 "ap" apV,
      mkAppend = tys1 "(<>)" appendV,
      mkApply = tys2 "apply" applyV,
      mkArcTan2 = tys1 "arctan2" arctan2V,
      mkASin = tys1 "asin" asinV,
      mkASinh = tys1 "asinh" asinhV,
      mkATan = tys1 "atan" atanV,
      mkATanh = tys1 "atanh" atanhV,
      mkBind = tys3 ">>=" bindV,
      mkBottom = tys2 "bottom" bottomV,
      mkCoerce = tys2 "coerce" coerceV,
      mkCompare = tys1 "compare" compareV,
      mkCompose = tys3 "(.)" composeV,
      mkConst = tys2 "const" constV,
      mkConstraint = tys2 "constraintK" constraintV,
      mkCos = tys1 "cos" cosV,
      mkCosh = tys1 "cosh" coshV,
      mkCurry = tys3 "curry" curryV,
      mkDistl = tys3 "distl" distlV,
      mkDiv = tys1 "div" divV,
      mkDivide = tys1 "(/)" divideV,
      mkDoubleToFloat = tys0 "doubleToFloatK" doubleToFloatV,
      mkEqual = tys1 "==" equalV,
      mkEven = tys1 "even" evenV,
      mkExl = tys2 "exl" exlV,
      mkExp = tys1 "exp" expV,
      mkExr = tys2 "exr" exrV,
      mkFix = tys2 "fix" fixV,
      mkFloatToDouble = tys0 "floatToDoubleK" floatToDoubleV,
      mkFmod = tys1 "fmod" fmodV,
      mkFork = tys3 "(&&&)" forkV,
      mkFPIsNegativeZero = tys1 "isNegativeZeroK" fpIsNegativeZeroV,
      mkFPIsInfinite = tys1 "isInfiniteK" fpIsInfiniteV,
      mkFPIsFinite = tys1 "isFiniteK" fpIsFiniteV,
      mkFPIsNaN = tys1 "isNaNK" fpIsNaNV,
      mkFPIsDenormal = tys1 "isDenormalK" fpIsDenormalV,
      mkFromInteger = tys1 "fromInteger" fromIntegerV,
      mkFromIntegral = tys2 "fromIntegral" fromIntegralV,
      mkGE = tys1 ">=" geV,
      mkGT = tys1 ">" gtV,
      mkId = tys1 "id" idV,
      mkIf = tys1 "if" ifV,
      mkIndex = tys2 "index" indexV,
      mkInl = tys2 "inl" inlV,
      mkInr = tys2 "inr" inrV,
      mkJoin = tys3 "(|||)" joinV,
      mkLAssoc = tys3 "lassoc" lassocV,
      mkLE = tys1 "<=" leV,
      mkLT = tys1 "<" ltV,
      mkLiftA2 = tys4 "liftA2" liftA2V,
      mkLog = tys1 "log" logV,
      -- This one doesn't use @tys@, because it passes the @cat@ twice.
      mkMap = \f a b -> doFn "map" mapV (\fn -> fn onDicts cat cat f a b),
      mkMax = tys1 "max" maxV,
      mkMaximum = tys2 "maximum" maximumV,
      mkMin = tys1 "min" minV,
      mkMinimum = tys2 "minimum" minimumV,
      mkMinus = tys1 "(-)" minusV,
      mkMod = tys1 "mod" modV,
      mkNative = tys3 "native" nativeV,
      mkNegate = tys1 "negate" negateV,
      mkNot = tys0 "not" notV,
      mkNotEqual = tys1 "/=" notEqualV,
      mkOdd = tys1 "odd" oddV,
      mkOr = tys0 "||" orV,
      mkPlus = tys1 "(+)" plusV,
      mkPoint = tys2 "point" pointV,
      mkPow = tys1 "pow" powV,
      mkPowI = tys3 "^" powIV,
      mkPowInt = tys1 "^ @_ @Int" powIntV,
      mkQuot = tys1 "quot" quotV,
      mkRAssoc = tys3 "rassoc" rassocV,
      mkRealToFrac = tys2 "realToFrac" realToFracV,
      mkRecip = tys1 "recip" recipV,
      mkRem = tys1 "rem" remV,
      mkReprC = tys1 "reprC" reprCV,
      mkSequenceA = tys3 "sequenceA" sequenceAV,
      mkSignum = tys1 "signum" signumV,
      mkSin = tys1 "sin" sinV,
      mkSinh = tys1 "sinh" sinhV,
      mkSqrt = tys1 "sqrt" sqrtV,
      mkStrength = tys3 "strength" strengthV,
      mkSwap = tys2 "swap" swapV,
      mkTabulate = tys2 "tabulate" tabulateV,
      mkTan = tys1 "tan" tanV,
      mkTanh = tys1 "tanh" tanhV,
      mkTimes = tys1 "(*)" timesV,
      mkTraverse = tys4 "traverse" traverseV,
      mkUncurry = tys3 "uncurry" uncurryV,
      mkAbst = abstH onDicts,
      mkApply2 = fmap (\fn -> fn onDicts cat) apply2V,
      mkCompose2 = fmap (\fn -> fn onDicts cat) compose2V,
      mkEither = \f g e -> do
        (a, c) <- getMorphismType f
        (b, c') <- getMorphismType g
        if c `Plugins.eqType` c'
          then pure $ eitherH a b c f g e
          else throwE . pure $ TypeMismatch "either" c c',
      mkFfcall = ffcallH onDicts cat,
      mkFixH = fixH,
      mkFst = \e -> do
        let eTy = Plugins.exprType e
        (a, b) <-
          either (throwE . pure . NotEnoughTypeArgs "mkFst" e eTy) pure (extract2TypeArgs eTy)
        pure $ fstH a b e,
      mkIndirection = indirectionH onDicts cat,
      mkRepr = reprH onDicts,
      mkSnd = \e -> do
        let eTy = Plugins.exprType e
        (a, b) <-
          either (throwE . pure . NotEnoughTypeArgs "mkSnd" e eTy) pure (extract2TypeArgs eTy)
        pure $ sndH a b e,
      mkCurryH = \e -> do
        let eTy = Plugins.exprType e
        ((a1, a2), b) <-
          either
            (throwE . pure . NotEnoughTypeArgs "mkCurryH" e eTy)
            pure
            (bitraverse extract2TypeArgs pure =<< extract2TypeArgs eTy)
        pure $ curryH a1 a2 b e
    }
  where
    tys0 label ident = doFn label ident (\fn -> fn onDicts cat)
    tys1 label ident a = doFn label ident (\fn -> fn onDicts cat a)
    tys2 label ident a b = doFn label ident (\fn -> fn onDicts cat a b)
    tys3 label ident a b c = doFn label ident (\fn -> fn onDicts cat a b c)
    tys4 label ident a b c d = doFn label ident (\fn -> fn onDicts cat a b c d)
    doFn ::
      String -> Maybe a -> (a -> CategoryStack Plugins.CoreExpr) -> CategoryStack Plugins.CoreExpr
    doFn name op apply = maybe (throwE . pure $ MissingCategoricalRepresentation name) apply op
    -- Attempts to apply the provided `Plugins.CoreExpr` to as many type class instances as are
    -- required.
    onDicts :: Plugins.CoreExpr -> CategoryStack Plugins.CoreExpr
    onDicts e =
      case invisFunArg (Plugins.exprType e) of
        Just ty
          | isPredTy' ty ->
              -- Find the wanted type class instance in the environment, apply `e` to it,
              -- and continue
              onDicts
                <=< ExceptT
                  . withRWST (const ((),))
                  . fmap (bimap (pure . CouldNotBuildDictionary ty e) (Plugins.App e))
                  . runExceptT
                  . buildDictionary hscEnv dflags guts inScope
                $ ty
        _ -> pure e

    {-
    Like `Plugins.splitFunTy_maybe`, but only returns `Just` if the argument is invisible.

    When applying `categorifyLambda` to `\(x :: X) -> ($fFoo :: Foo Bar)` where
    `Foo` is a typeclass and `$fFoo` is its dictionary, the plugin would invoke
    `mkConst' X (Foo Bar)`, which ends up applying `onDicts` to

    ```
    (ConstCat Hask (Foo Bar), Ok Hask X) => Foo Bar -> Hask X (Foo Bar)
    ```

    Here we need two dictionaries, not three, i.e., it should only proceed if the
    arrow is "=>", not "->".
    -}
    invisFunArg :: Plugins.Type -> Maybe Plugins.Type
#if MIN_VERSION_ghc(8, 10, 0)
    invisFunArg = \case
      ty | Just ty' <- Plugins.coreView ty -> invisFunArg ty'
      TyCoRep.FunTy Plugins.InvisArg arg _ -> Just arg
      _ -> Nothing
#else
    invisFunArg =
      (\(arg, _) -> if Plugins.isPredTy arg then pure arg else Nothing) <=< Plugins.splitFunTy_maybe
#endif

    isPredTy' :: Plugins.Type -> Bool
    isPredTy' ty = Plugins.isPredTy ty || others ty
      where
        others (Plugins.splitTyConApp_maybe -> Just (tc, tys)) =
          -- The first half of the arguments are representation types ('PtrRepLifted)
          Plugins.isUnboxedTupleTyCon tc && all isPredTy' (drop (length tys `div` 2) tys)
        others _ = False

extract2TypeArgs :: Plugins.Type -> Either [Plugins.Type] (Plugins.Type, Plugins.Type)
extract2TypeArgs ty = do
  (rest, arg2) <- maybeToEither [] (Plugins.splitAppTy_maybe ty)
  (_, arg1) <- maybeToEither [arg2] (Plugins.splitAppTy_maybe rest)
  pure (arg1, arg2)

getMorphismType :: Plugins.CoreExpr -> CategoryStack (Plugins.Type, Plugins.Type)
getMorphismType f =
  let ty = Plugins.exprType f
   in either (throwE . pure . NotEnoughTypeArgs "getMorphismType" f ty) pure (extract2TypeArgs ty)

isFreeIn :: Plugins.Var -> Plugins.CoreExpr -> Bool
v `isFreeIn` e = v `Plugins.elemVarSet` Plugins.exprFreeVars e

-- | Similar to 'isFreeIn', this identifies whether an identifier is referenced in the expr,
--   regardless of whether it's a free variable or not.
isCalledIn :: Plugins.Var -> Plugins.CoreExpr -> Bool
isCalledIn v = elem v . universeBi
