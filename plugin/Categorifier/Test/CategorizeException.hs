{-# LANGUAGE TemplateHaskellQuotes #-}

module Test.CategorifyException
  ( hprop_correctException,
  )
where

import qualified Categorifier.Categorify as Categorify
import Categorifier.Common.IO.Exception (evaluate, try)
import GHC.Stack (SrcLoc (..), getCallStack)
import qualified Hedgehog
import qualified Language.Haskell.TH as TH
import P

hprop_correctException :: Hedgehog.Property
hprop_correctException =
  Hedgehog.property $
    either
      -- __NB__: Ensures we get a `CallStack` that is useful to a user.
      ( \(Categorify.UnconvertedCall _ calls) ->
          -- __NB__: The `srcLocPackage` varies depending on the build target, so we effectively
          --         ignore it.
          fmap (fmap (\frame -> frame {srcLocPackage = ""})) (getCallStack calls)
            Hedgehog.=== [ ( TH.nameBase 'Categorify.expression,
                             SrcLoc
                               { srcLocPackage = "",
                                 srcLocModule =
                                   fromMaybe "" $ TH.nameModule 'hprop_correctException,
                                 srcLocFile =
                                   "code_generation/plugin/Test/Plugin/CategorifyException.hs",
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
      $ Categorify.expression id
