module Main
  ( main,
  )
where

import P ()
import System.IO (IO, print)
import Test.WithInstance (test)

main :: IO ()
main = print test
