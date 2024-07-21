{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe (fromMaybe)
import ViewMonad

data Counter = Counter
  { _counter :: State Int,
    _output :: Memo Int Int
  }

makeLenses ''Counter

app :: (MonadIO m) => View m
app = componentV (Counter mkState mkMemo) $ do
  (count, setCount) <- useState counter 0

  liftIO $ print count

  return []

main :: IO ()
main = do
  _ <- buildUI app mkUI
  return ()

{-
data Counter = Counter
  { _counter :: State Int,
    _output :: Memo Int Int
  }

makeLenses ''Counter

app :: (Monad m) => Html m
app = component_ (Counter mkState mkMemo) $ do
  (count, setCount) <- useState counter 0

  count' <- useMemo output count $ \x -> pure $ x * 2

  return $
    div_
      []
      [ text_ $ "Useful files: " ++ show count',
        button_ [on_ "click" $ setCount (count + 1)] [text_ "Clone repo!"],
        button_ [on_ "click" $ setCount (count - 1)] [text_ "Download meme!"]
      ]

main :: IO ()
main = do
  (mutations, _, vdom) <- buildHtml app mkVirtualDom
  print mutations
  print vdom

  let button = fromMaybe (error "TODO") $ find "button" (root vdom)
  vdom' <- click button
  (mutations2, _, vdom'') <- rebuildHtml 0 vdom'
  print mutations2

  let button' = fromMaybe (error "TODO") $ find "button" (root vdom'')
  vdom''' <- click button'
  (mutations3, _, _) <- rebuildHtml 0 vdom'''
  print mutations3
-}