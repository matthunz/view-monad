module Main where

import Data.IntMap (fromList)
import ViewMonad

main :: IO ()
main = do
  let tree = fromList [(0, Text "Hello")]
  let x = foldTree (\_ _ a -> a + 1) 0 0 tree
  print x
