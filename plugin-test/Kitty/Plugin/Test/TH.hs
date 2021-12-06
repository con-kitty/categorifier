{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

-- |
-- This module contains TH that generates terms to be invoked with the
-- 'Kitty.Plugin.Categorize.expression' plugin-runner. This is done so that new arrows can be tested
-- easily.
--
-- The HLint warnings for 'id' and 'const' are disabled because we want to test how the plugin
-- handles exactly what's written.
module Kitty.Plugin.Test.TH
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

import Control.Applicative (liftA2)
import Control.Monad (join)
import Data.Bifunctor (Bifunctor (..))
import Data.Char (toLower)
import Data.Foldable (toList)
import Data.Maybe (mapMaybe)
import Data.Tuple.Extra (uncurry3)
import GHC.Stack (HasCallStack, withFrozenCallStack)
import qualified Hedgehog
import Kitty.Common.IO.Exception (SomeException, evaluate, try)
import qualified Kitty.Plugin.Categorize as Categorize
import Kitty.Plugin.Test.HList (HList1 (..), zipLowerWith)
import Language.Haskell.TH (Dec, Exp, Name, Q, Type)
import qualified Language.Haskell.TH as TH

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
  | -- | Compute the expected output from the input and verify that running the
    -- function gives the correct value.
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
mkBinaryTestConfig funName' arrowTy = TestConfig arrowTy funName' [|uncurry|]

-- | Provide test configuration information for the specified arrow and ternary function.
mkTernaryTestConfig :: String -> TestCategory -> TestConfig
mkTernaryTestConfig funName' arrowTy = TestConfig arrowTy funName' [|uncurry3|]

mkPropName :: Int -> Name -> Name
mkPropName i = TH.mkName . (\nm -> "hprop_" <> nm <> show i) . TH.nameBase

mkPropLabel :: Int -> Name -> String
mkPropLabel i = (<> show i) . TH.nameBase

-- | A variant on `Hedgehog.===` that identifies NaNs as equals. It still works for non-FP types.
floatingEq :: (Hedgehog.MonadTest m, Eq a, Show a, HasCallStack) => a -> a -> m ()
floatingEq x y = withFrozenCallStack $ Hedgehog.diff x eq y
  where
    eq x' y' = x' /= x' && y' /= y' || x' == y'

-- | Create a TH splice defining a Hedgehog property test of the given function.  This should be
-- automatically found and run by tasty.
expectMatch :: Q Exp -> Q Exp -> Q Exp -> Int -> TestConfig -> Q Type -> (String, Name, Q [Dec])
expectMatch display gen calcExpected i (TestConfig arrowTy funName' post) testTy =
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
          let retval = $post ($arrowCallExpr (Categorize.expression $calcExpected :: $testTy)) input
              expected = $post $calcExpected input
          floatingEq retval expected
        |]
    propBody CheckCompileOnly =
      [e|
        do
          _ <- pure (Categorize.expression $calcExpected :: $testTy)
          Hedgehog.success
        |]

-- | Right now this simply indicates that the test failed to build in _some_ way. In future, we
--   should check the specific failure that occurred, so changes in failure cases also break tests.
expectBuildFailure :: Q Exp -> Int -> TestConfig -> Q Type -> (String, Name, Q [Dec])
expectBuildFailure calcExpected i (TestConfig arrowTy funName' _) testTy =
  ( mkPropLabel i funName,
    propName,
    (:)
      <$> typeSig
      <*> [d|
        $(TH.varP propName) =
          Hedgehog.property
            ( either
                (const Hedgehog.success :: SomeException -> Hedgehog.PropertyT IO ())
                (const Hedgehog.failure)
                <=< Hedgehog.evalIO . try
                $ evaluate (Categorize.expression $calcExpected :: $testTy)
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
    [|
      Hedgehog.checkSequential $
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
--   display function. If it's `Nothing`, that means only check that it compiles. If the list is
--   empty don't run the test at all on that arrow.
newtype TestCases a = TestCases {getTestCases :: Name -> [(a, Maybe (Q Exp, Q Exp))]}

-- | This is a function that eventually returns "named definitions" (a named definition is a pair of
--   a `Name` and a @`Q` [`Dec`]@ containing a definition with that name. The result is a pair of a
--   named definition for the expression to test and a list of named definitions for the properties
--   to test it with.
newtype ExprTest a = ExprTest
  {getExprTest :: TestCases a -> TestCategory -> Maybe [(String, Name, Q [Dec])]}

-- | Provides @allTestTerms :: `IO` [`Bool`]@ to comprehensively test various categories.
mkTestTerms ::
  -- | The expressions to test. If you are using the plugin without extension, then
  --  `Test.Tests.defaultTestTerms` should cover all possible expressions.
  HList1 ExprTest l ->
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
  HList1 TestCases l ->
  Q [Dec]
mkTestTerms testTerms arrows testCases =
  ( uncurry (liftA2 (<>))
      . bimap
        ( \labels ->
            [d|
              allTestTerms :: IO [Bool]
              allTestTerms =
                sequenceA $(foldr (TH.appE . TH.appE (TH.conE '(:)) . TH.varE) [|[]|] labels)
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
                  $ zipLowerWith getExprTest testTerms testCases
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
   in pure $
        zipWith
          ( \i (testTys, testGen) ->
              maybe
                expectBuildFailure
                (\(gen, showExp) -> expectMatch showExp gen)
                testGen
                calcExpected
                i
                testConfig
                . uncurry (mkTestType $ arrTy arrowTy)
                $ idxTy testTys
          )
          [0 ..]
          $ getTestCases props (arrName arrowTy)
