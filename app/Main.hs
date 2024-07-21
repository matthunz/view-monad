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

app :: (MonadIO m) => View m
app = componentV (Counter mkState mkMemo mkEffect) $ do
  (count, setCount) <- useState countHook 0

  count' <- useMemo countHook' count $ \c -> pure $ c * 2

  useEffect effectHook count $ \c -> setCount (c + 1)

  liftIO $ print count'

  return []

main :: IO ()
main = do
  (_, updates, ui) <- buildUI app mkUI
  let ui' = foldr updateUI ui updates
  (updates2, ui'') <- rebuildUI 0 ui'
  let ui''' = foldr updateUI ui'' updates2
  _ <- rebuildUI 0 ui'''
  return ()
