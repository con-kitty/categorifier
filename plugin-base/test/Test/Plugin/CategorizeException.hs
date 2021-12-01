{-# LANGUAGE TemplateHaskellQuotes #-}

module Test.Plugin.CategorizeException
  ( hprop_correctException,
  )
where

import Control.Monad ((<=<))
import Data.Maybe (fromMaybe)
import GHC.Stack (SrcLoc (..), getCallStack)
import qualified Hedgehog
import Kitty.Common.IO.Exception (evaluate, try)
import qualified Kitty.Plugin.Categorize as Categorize
import qualified Language.Haskell.TH as TH

hprop_correctException :: Hedgehog.Property
hprop_correctException =
  Hedgehog.property $
    either
      -- __NB__: Ensures we get a `CallStack` that is useful to a user.
      ( \(Categorize.UnconvertedCall _ calls) ->
          -- __NB__: The `srcLocPackage` varies depending on the build target, so we effectively
          --         ignore it.
          fmap (fmap (\frame -> frame {srcLocPackage = ""})) (getCallStack calls)
            Hedgehog.=== [ ( TH.nameBase 'Categorize.expression,
                             SrcLoc
                               { srcLocPackage = "",
                                 srcLocModule =
                                   fromMaybe "" $ TH.nameModule 'hprop_correctException,
                                 srcLocFile =
                                   "code_generation/plugin/Test/Plugin/CategorizeException.hs",
                                 srcLocStartLine = 41,
                                 srcLocStartCol = 9,
                                 srcLocEndLine = 41,
                                 srcLocEndCol = 33
                               }
                           )
                         ]
      )
      (const Hedgehog.failure)
      <=< Hedgehog.evalIO . try . evaluate
      $ Categorize.expression id
