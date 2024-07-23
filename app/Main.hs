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
  (i, updates, effs, ui1) <- buildUI app mkUI
  let ui2 = foldr updateUI ui1 updates
  effUpdates <- concat <$> mapM (\s -> snd <$> runScope s i) effs
  let ui3 = foldr updateUI ui2 effUpdates

  (i', updates2, effs2, ui4) <- rebuildUI i ui3
  let ui5 = foldr updateUI ui4 updates2
  effUpdates2 <- concat <$> mapM (\s -> snd <$> runScope s i) effs2
  let ui6 = foldr updateUI ui5 effUpdates2

  _ <- rebuildUI i' ui6
  return ()
