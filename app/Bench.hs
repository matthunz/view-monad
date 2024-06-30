module Main where

import Criterion.Main
import ViewMonad

app :: Html IO
app = component_ $ do
  (x, setX) <- useState (0 :: Int)

  return $
    div_
      []
      [ text_ $ "Useful files: " ++ show x,
        button_ [on_ "click" $ setX (x + 1)] [text_ "Clone repo!"],
        button_ [on_ "click" $ setX (x - 1)] [text_ "Download meme!"]
      ]

run :: VirtualDom IO -> IO (VirtualDom IO)
run vdom = do
  (_, vdom') <- buildHtml app vdom
  loop 10000 vdom'

loop :: Int -> VirtualDom IO -> IO (VirtualDom IO)
loop 0 v = pure v
loop n vdom = do
  let vdom' = handle 4 "onclick" vdom
  (mutations, vdom'') <- rebuildHtml 0 vdom'
  loop (n - 1) vdom''

main :: IO ()
main =
  defaultMain
    [ bgroup
        "run"
        [ bench "5" $ whnf run mkVirtualDom
        ]
    ]
