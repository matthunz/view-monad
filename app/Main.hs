{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Data.Maybe (fromMaybe)
import ViewMonad

data Counter = Counter
  { _count :: Int
  }

makeLenses ''Counter

app :: (Monad m) => Html m
app = component_ (Counter 0) $ do
  (count, setCount) <- useState count

  return $
    div_
      []
      [ text_ $ "Useful files: " ++ show count,
        button_ [on_ "click" $ setCount (count + 1)] [text_ "Clone repo!"],
        button_ [on_ "click" $ setCount (count - 1)] [text_ "Download meme!"]
      ]

main :: IO ()
main = do
  (ms, vdom) <- buildHtml app mkVirtualDom
  print ms
  print vdom

  let button = fromMaybe (error "TODO") $ find "button" (root vdom)
  (mutations, vdom') <- rebuildHtml 0 (click button)
  print mutations

  let button' = fromMaybe (error "TODO") $ find "button" (root vdom')
  (mutations2, _) <- rebuildHtml 0 (click button')
  print mutations2
