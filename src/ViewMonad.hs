module ViewMonad (someFunc) where

import Data.IntMap (IntMap)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Node = Component Int | Element String [Int] | Text String

data Tree = Tree (IntMap Node)
