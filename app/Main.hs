{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Data.Maybe (fromMaybe)
import ViewMonad

data Counter = Counter
  { _counter :: Int,
    _output :: Memo Int Int
  }

makeLenses ''Counter

app :: (Monad m) => Html m
app = component_ (Counter 0 memo) $ do
  (count, setCount) <- useState counter

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
  (ms, updates, vdom) <- buildHtml app mkVirtualDom
  print ms
  print vdom

  let button = fromMaybe (error "TODO") $ find "button" (root vdom)
  vdom' <- click button
  (mutations, updates, vdom'') <- rebuildHtml 0 vdom'
  print mutations
