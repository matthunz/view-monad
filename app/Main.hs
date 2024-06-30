module Main where

import qualified ViewMonad (someFunc)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  ViewMonad.someFunc
