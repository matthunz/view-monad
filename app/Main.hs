{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Control.Monad.IO.Class (MonadIO, liftIO)
import ViewMonad

data Counter = Counter
  { _counter :: State Int,
    _output :: Memo Int Int
  }

makeLenses ''Counter

app :: (MonadIO m) => View m
app = componentV (Counter mkState mkMemo) $ do
  (count, setCount) <- useState counter 0

  liftScope $ setCount (count + 1)

  liftIO $ print count

  return []

main :: IO ()
main = do
  (ui, _, updates) <- buildUI app mkUI
  let ui' = foldr updateUI ui updates
  (ui'', updates2) <- rebuildUI 0 ui'
  let ui''' = foldr updateUI ui'' updates2
  _ <- rebuildUI 0 ui'''
  return ()
