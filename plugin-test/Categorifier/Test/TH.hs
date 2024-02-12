{-# LANGUAGE CPP #-}
#if MIN_VERSION_GLASGOW_HASKELL(9, 0, 0, 0)
{-# LANGUAGE LinearTypes #-}
#endif
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

-- |
-- This module contains TH that generates terms to be invoked with the
-- 'Categorifier.Categorify.expression' plugin-runner. This is done so that new arrows can be tested
-- easily.
--
-- The HLint warnings for 'id' and 'const' are disabled because we want to test how the plugin
-- handles exactly what's written.
module Categorifier.Test.TH
  ( mkExprTest,
    mkTestTerms,
    mkBinaryTestConfig,
    mkTernaryTestConfig,
    mkUnaryTestConfig,
    ExprTest (..),
    TestCases (..),
    TestCategory (..),
    TestConfig,
    TestStrategy (..),
  )
where

import qualified Categorifier.Categorify as Categorify
import Categorifier.Common.IO.Exception (SomeException, displayException, evaluate, try)
import Categorifier.Hedgehog (floatingEq)
import Categorifier.Test.HList (HMap1 (..), zipMapLowerWith)
import Control.Applicative (liftA2)
import Control.Monad (join, (<=<))
import Data.Bifunctor (Bifunctor (..))
import Data.Char (toLower)
import Data.Foldable (toList)
import Data.List (isInfixOf)
import Data.Maybe (mapMaybe)
import Data.Tuple.Extra (uncurry3)
import qualified Hedgehog
import Language.Haskell.TH (Dec, Exp, Name, Q, Type)
import qualified Language.Haskell.TH as TH
import PyF (fmt)

#if MIN_VERSION_GLASGOW_HASKELL(9, 0, 0, 0)
-- | The one from linear-base doesn't work, because it's not parametric, but specialized to @%1@.
forget :: forall {q} a b. (a %q -> b) %1 -> a -> b
forget f a = f a
#endif

data TestCategory = TestCategory
  { arrName :: Name,
    arrTy :: Q Type,
    fnPrefix :: String,
    strategy :: TestStrategy
  }

mkSafeFunName :: String -> Name
mkSafeFunName = TH.mkName . go
  where
    go [] = []
    go (x : xs) = toLower x : xs

mkFunNameFromArrow :: TestCategory -> String -> Name
mkFunNameFromArrow cat = mkSafeFunName . (fnPrefix cat <>)

-- | How should we test this function?
data TestStrategy
  = -- | If it compiles, the test should be considered a pass
    CheckCompileOnly
  | -- | Compute the expected output from the input and verify that running the function gives the
    --   correct value. The expression here should have the type @k a b -> a %q -> b@
    ComputeFromInput (Q Exp)

data TestConfig = TestConfig
  { _testConfigArrowType :: TestCategory,
    _testConfigFunName :: String,
    -- | How do we want to call the function under test?  This is used for multiple things:
    --
    --     * handling any processing of the arrow under test, e.g. @runHask@
    --
    --     * currying the function under test so it can be used with a tuple of
    --       hedgehog-generated inputs
    _testConfigPostProcess :: Q Exp
  }

-- | Provide test configuration information for the specified arrow and unary function.
mkUnaryTestConfig :: String -> TestCategory -> TestConfig
mkUnaryTestConfig funName' arrowTy = TestConfig arrowTy funName' [|id|]

-- | Provide test configuration information for the specified arrow and binary function.
mkBinaryTestConfig :: String -> TestCategory -> TestConfig
#if MIN_VERSION_GLASGOW_HASKELL(9, 0, 0, 0)
mkBinaryTestConfig funName' arrowTy =
  TestConfig arrowTy funName' [|uncurry . fmap (forget forget) . forget forget|]
#else
mkBinaryTestConfig funName' arrowTy = TestConfig arrowTy funName' [|uncurry|]
#endif

-- | Provide test configuration information for the specified arrow and ternary function.
mkTernaryTestConfig :: String -> TestCategory -> TestConfig
mkTernaryTestConfig funName' arrowTy = TestConfig arrowTy funName' [|uncurry3|]

mkPropName :: Int -> Name -> Name
mkPropName i = TH.mkName . (\nm -> [fmt|hprop_{nm}{show i}|]) . TH.nameBase

mkPropLabel :: Int -> Name -> String
mkPropLabel i = (<> show i) . TH.nameBase

-- | Create a TH splice defining a Hedgehog property test of the given function.  This should be
-- automatically found and run by tasty.
expectMatch :: Q Exp -> Q Exp -> Q Exp -> Int -> TestConfig -> Q Type -> (String, Name, Q [Dec])
expectMatch gen display calcExpected i (TestConfig arrowTy funName' post) testTy =
  ( mkPropLabel i funName,
    propName,
    (:) <$> typeSig <*> [d|$(TH.varP propName) = Hedgehog.property $(propBody $ strategy arrowTy)|]
  )
  where
    funName = arrowTy `mkFunNameFromArrow` funName'
    typeSig = TH.sigD propName [t|Hedgehog.Property|]
    propName = mkPropName i funName
    propBody (ComputeFromInput arrowCallExpr) =
      [e|
        do
          input <- Hedgehog.forAllWith $display $gen
          let retval = $post ($arrowCallExpr (Categorify.expression $calcExpected :: $testTy)) input
              expected = $post $calcExpected input
          floatingEq retval expected
        |]
    propBody CheckCompileOnly =
      [e|
        do
          _ <- pure (Categorify.expression $calcExpected :: $testTy)
          Hedgehog.success
        |]

-- | Create a TH splice defining a Hedgehog property test of the given function. The property test
--   will succeed only if there was a build failure with a message that contains the provided
--  `String`.
expectBuildFailure :: String -> Q Exp -> Int -> TestConfig -> Q Type -> (String, Name, Q [Dec])
expectBuildFailure partialMessage calcExpected i (TestConfig arrowTy funName' _) testTy =
  ( mkPropLabel i funName,
    propName,
    (:)
      <$> typeSig
      <*> [d|
        $(TH.varP propName) =
          Hedgehog.property
            ( either
                (Hedgehog.diff partialMessage isInfixOf . displayException @SomeException)
                (const Hedgehog.failure)
                <=< Hedgehog.evalIO . try
                $ evaluate (Categorify.expression $calcExpected :: $testTy)
            )
        |]
  )
  where
    funName = arrowTy `mkFunNameFromArrow` funName'
    typeSig = TH.sigD propName [t|Hedgehog.Property|]
    propName = mkPropName i funName

mkTopLevelPair :: TestCategory -> [(String, Name)] -> (Name, Q Exp)
mkTopLevelPair arrowTy names =
  ( arrowLabel,
    [e|
      Hedgehog.checkParallel $
        Hedgehog.Group
          $(nameBaseLiteral $ arrName arrowTy)
          $(TH.listE namePairs)
      |]
  )
  where
    arrowLabel = mkFunNameFromArrow arrowTy "TopLevel"
    namePairs :: [Q Exp]
    namePairs = (\(name, prop) -> [e|($(nameBaseLit name), $(TH.varE prop))|]) <$> names
    nameBaseLiteral = TH.litE . TH.stringL . TH.nameBase
    nameBaseLit = TH.litE . TH.stringL

mkTopLevelTestTarget :: Name -> Q Exp -> Q [Dec]
mkTopLevelTestTarget arrowLabel testExpr =
  (:)
    <$> TH.sigD arrowLabel [t|IO Bool|]
    <*> [d|$(TH.varP arrowLabel) = $testExpr|]

mkTestType :: Q Type -> Q Type -> Q Type -> Q Type
mkTestType arr input output = [t|$arr $input $output|]

-- | Given an arrow `Name`, return a list of properties to construct. Each consists of the specific
--   types for specializing the parametric type above, followed by an optional pair of generator and
--   display function. If it's `Left`, it takes a `String` that must a substring of the error
--   message. If the list is empty don't run the test at all on that arrow.
newtype TestCases a = TestCases {getTestCases :: Name -> [(a, Either String (Q Exp, Q Exp))]}

-- | This is a function that eventually returns "named definitions" (a named definition is a pair of
--   a `Name` and a @`Q` [`Dec`]@ containing a definition with that name. The result is a pair of a
--   named definition for the expression to test and a list of named definitions for the properties
--   to test it with.
newtype ExprTest a = ExprTest
  {getExprTest :: TestCases a -> TestCategory -> Maybe [(String, Name, Q [Dec])]}

-- | Provides @allTestTerms :: [`IO` `Bool`]@ to comprehensively test various categories.
mkTestTerms ::
  -- | The expressions to test. If you are using the plugin without extension, then
  --  `Test.Tests.defaultTestTerms` should cover all possible expressions.
  HMap1 ExprTest l ->
  -- | All of the categories to run the tests against.
  [TestCategory] ->
  -- | The test cases to run for each expression. It's recommended to have /at least/ one test case
  --   per hierarchy instance. `TestCases` takes the category as an argument, so you can
  --   conditionalize tests per category, without having to duplicate the bulk of them.
  --
  --  __TODO__: Instead of defining test cases per expression, define them in a `Data.Map.Map` keyed
  --            off the classes for the particular hierarchy. This ties them more directly to the
  --            instances as defined, reducing the amount of code needed and making it easier to
  --            verify coverage.
  HMap1 TestCases l ->
  Q [Dec]
mkTestTerms testTerms arrows testCases =
  ( uncurry (liftA2 (<>))
      . bimap
        ( \labels ->
            let emptyList = [|[]|]
             in [d|
                  allTestTerms :: [IO Bool]
                  allTestTerms =
                    $(foldr (TH.appE . TH.appE (TH.conE '(:)) . TH.varE) emptyList labels)
                  |]
        )
        (pure . join)
      . unzip
  )
    =<< traverse
      ( \arrowTy -> do
          let (names, funDecls') =
                fmap (fmap join . sequenceA)
                  . unzip
                  . join
                  . mapMaybe (fmap (fmap groupNames) . selectCase arrowTy)
                  . toList
                  $ zipMapLowerWith getExprTest testTerms testCases
          funDecls <- funDecls'
          let (label, expr) = mkTopLevelPair arrowTy names
          topTestDecls <- mkTopLevelTestTarget label expr
          pure (label, funDecls <> topTestDecls)
      )
      arrows
  where
    selectCase arrowTy term =
      term arrowTy
    groupNames (propLabel, propName, prop) = ((propLabel, propName), prop)

mkExprTest ::
  -- | A name to use for the test, wrapped in a call that sets up the arity properly.
  (TestCategory -> TestConfig) ->
  -- | The parametric type of the expression to test. @a@ should be some tuple of @`Q` `Type`@,
  --   returning the pair of input and output types.
  (a -> (Q Type, Q Type)) ->
  -- | The expression to test.
  Q Exp ->
  ExprTest a
mkExprTest testName idxTy calcExpected = ExprTest $ \props arrowTy ->
  let testConfig = testName arrowTy
   in pure
        . zipWith
          ( \i (testTys, testGen) ->
              either
                expectBuildFailure
                (uncurry expectMatch)
                testGen
                calcExpected
                i
                testConfig
                . uncurry (mkTestType $ arrTy arrowTy)
                $ idxTy testTys
          )
          [0 ..]
        $ getTestCases props (arrName arrowTy)
