module Main
  ( main,
  )
where

import Test.Plugin.WithInstance (test)

main :: IO ()
main = print test
