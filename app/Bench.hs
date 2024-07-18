{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Criterion.Main
import Data.Maybe (fromMaybe)
import ViewMonad

data Counter = Counter
  { _counter :: Int
  }

makeLenses ''Counter

app :: (Monad m) => Html m
app = component_ (Counter 0) $ do
  (count, setCount) <- useState counter

  return $
    div_
      []
      [ text_ $ "Useful files: " ++ show count,
        button_ [on_ "click" $ setCount (count + 1)] [text_ "Clone repo!"],
        button_ [on_ "click" $ setCount (count - 1)] [text_ "Download meme!"]
      ]

run :: VirtualDom IO -> IO (VirtualDom IO)
run vdom = do
  (_, _, vdom') <- buildHtml app vdom
  loop 10000 vdom'

loop :: Int -> VirtualDom IO -> IO (VirtualDom IO)
loop 0 v = pure v
loop n vdom = do
  vdom' <- handle 4 "onclick" vdom
  (mutations, _, vdom'') <- rebuildHtml 0 vdom'
  loop (n - 1) vdom''

main :: IO ()
main =
  defaultMain
    [ bgroup
        "run"
        [ bench "5" $ whnf run mkVirtualDom
        ]
    ]
