{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Control.Monad.IO.Class (MonadIO, liftIO)
import ViewMonad

data Counter = Counter
  { _countHook :: UseState Int,
    _countHook' :: UseMemo Int Int,
    _effectHook :: UseEffect Int
  }

makeLenses ''Counter

counter :: (MonadIO m) => View m
counter = componentV (Counter mkState mkMemo mkEffect) $ do
  (count, setCount) <- useState countHook 0

  count' <- useMemo countHook' count $ \c -> pure $ c * 2

  useEffect effectHook count $ \c -> setCount $ c + 1

  useUnmount . liftIO $ print "Unmounted!"

  liftIO $ print count'

  return []

app :: (MonadIO m) => View m
app = componentV () $ return [counter, counter]

main :: IO ()
main = do
  (i, updates, ui) <- buildUI app mkUI
  let ui' = foldr updateUI ui updates
  (i', updates2, ui'') <- rebuildUI i ui'
  let ui''' = foldr updateUI ui'' updates2
  _ <- rebuildUI i' ui'''
  return ()
